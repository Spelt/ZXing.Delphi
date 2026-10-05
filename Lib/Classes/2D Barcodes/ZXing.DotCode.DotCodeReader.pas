{
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *      http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.

  * The DotCode reader for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net
  * have none. DotCode has no finder pattern: the dots themselves are found
  * (the blobs of the image of about the same size), then their grid.
}

unit ZXing.DotCode.DotCodeReader;

interface

uses
  System.Types,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap,
  ZXing.Common.BitMatrix;

type
  /// <summary>
  /// Reads DotCode symbols: dots on a checkerboard grid, in any direction,
  /// mirrored too.
  /// </summary>
  TDotCodeReader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

implementation

uses
  ZXing.ResultPoint,
  ZXing.Common.Blobs,
  ZXing.DotCode.Decoder;

const
  // the least coherence of the directions to the nearest neighbours (times
  // 4; 1: all the same) of a grid of dots
  MIN_COHERENCE = 0.5;

type
  TPointD = record
    X, Y: Double;
  end;

  /// <summary>A projective map (homography, row major, H[8] = 1) from grid
  /// places (column, row) to the image.</summary>
  TGridMap = record
    H: array [0 .. 8] of Double;
    function Map(c, r: Double): TPointD;
    /// <summary>The grid place of an image point; false when the map is
    /// singular there.</summary>
    function Unmap(x, y: Double; out c, r: Double): Boolean;
  end;

function TGridMap.Map(c, r: Double): TPointD;
begin
  var w := H[6] * c + H[7] * r + H[8];
  if (Abs(w) < 1E-12) then
    w := 1E-12;
  Result.X := (H[0] * c + H[1] * r + H[2]) / w;
  Result.Y := (H[3] * c + H[4] * r + H[5]) / w;
end;

function TGridMap.Unmap(x, y: Double; out c, r: Double): Boolean;
begin
  // the inverse (the adjugate; the scale does not matter)
  var i0 := H[4] * H[8] - H[5] * H[7];
  var i1 := H[2] * H[7] - H[1] * H[8];
  var i2 := H[1] * H[5] - H[2] * H[4];
  var i3 := H[5] * H[6] - H[3] * H[8];
  var i4 := H[0] * H[8] - H[2] * H[6];
  var i5 := H[2] * H[3] - H[0] * H[5];
  var i6 := H[3] * H[7] - H[4] * H[6];
  var i7 := H[1] * H[6] - H[0] * H[7];
  var i8 := H[0] * H[4] - H[1] * H[3];
  var w := i6 * x + i7 * y + i8;
  Result := Abs(w) > 1E-12;
  if not Result then
    exit;
  c := (i0 * x + i1 * y + i2) / w;
  r := (i3 * x + i4 * y + i5) / w;
end;
type
  /// <summary>Blobs in square cells: the blobs near a point quickly.
  /// </summary>
  TBlobCells = class
  private
    FSize, FMinX, FMinY: Double;
    FColumns, FRows: Integer;
    // the indexes (of ids) by cell, from FStarts[cell] on
    FOrder, FStarts: TArray<Integer>;
    FFound: TArray<Integer>;
  public
    /// <summary>The blobs ids[0 .. count - 1] (of blobs) in cells of (at
    /// least) size.</summary>
    constructor Create(const blobs: TArray<TBlob>; const ids: TArray<Integer>;
      count: Integer; size: Double);
    /// <summary>The number of indexes (of ids) in the cells within distance
    /// of (x, y) (also some farther away), into Found.</summary>
    function Near(x, y, distance: Double): Integer;
    property Found: TArray<Integer> read FFound;
  end;

constructor TBlobCells.Create(const blobs: TArray<TBlob>;
  const ids: TArray<Integer>; count: Integer; size: Double);
begin
  FMinX := MaxDouble;
  FMinY := MaxDouble;
  var maxX := -MaxDouble;
  var maxY := -MaxDouble;
  for var i := 0 to count - 1 do
  begin
    FMinX := Min(FMinX, blobs[ids[i]].X);
    FMinY := Min(FMinY, blobs[ids[i]].Y);
    maxX := Max(maxX, blobs[ids[i]].X);
    maxY := Max(maxY, blobs[ids[i]].Y);
  end;
  // (not many more cells than blobs)
  FSize := Max(size, 1);
  repeat
    FColumns := Trunc((maxX - FMinX) / FSize) + 1;
    FRows := Trunc((maxY - FMinY) / FSize) + 1;
    if (Int64(FColumns) * FRows <= 8 * count + 64) then
      break;
    FSize := 2 * FSize;
  until false;
  // the indexes sorted by cell (counting)
  var cells: TArray<Integer>;
  SetLength(cells, count);
  SetLength(FStarts, FColumns * FRows + 1);
  for var c := 0 to High(FStarts) do
    FStarts[c] := 0;
  for var i := 0 to count - 1 do
  begin
    cells[i] := Trunc((blobs[ids[i]].Y - FMinY) / FSize) * FColumns +
      Trunc((blobs[ids[i]].X - FMinX) / FSize);
    Inc(FStarts[cells[i] + 1]);
  end;
  for var c := 1 to High(FStarts) do
    Inc(FStarts[c], FStarts[c - 1]);
  var next := Copy(FStarts);
  SetLength(FOrder, count);
  for var i := 0 to count - 1 do
  begin
    FOrder[next[cells[i]]] := i;
    Inc(next[cells[i]]);
  end;
  SetLength(FFound, count);
end;

function TBlobCells.Near(x, y, distance: Double): Integer;
begin
  Result := 0;
  var c1 := Max(0, Floor((x - distance - FMinX) / FSize));
  var c2 := Min(FColumns - 1, Floor((x + distance - FMinX) / FSize));
  var r1 := Max(0, Floor((y - distance - FMinY) / FSize));
  var r2 := Min(FRows - 1, Floor((y + distance - FMinY) / FSize));
  for var r := r1 to r2 do
    for var k := FStarts[r * FColumns + c1] to
      FStarts[r * FColumns + c2 + 1] - 1 do
    begin
      FFound[Result] := FOrder[k];
      Inc(Result);
    end;
end;

/// <summary>The groups of blobs about as large as size, each near the next
/// (within reach): the candidates of symbols.</summary>
function GroupBlobs(const blobs: TArray<TBlob>; size: Integer;
  reach: Double): TArray<TArray<Integer>>;
begin
  Result := [];
  // the blobs of about this size
  var ids: TArray<Integer>;
  SetLength(ids, Length(blobs));
  var count := 0;
  for var i := 0 to High(blobs) do
    if (blobs[i].Size >= size * 0.6) and (blobs[i].Size <= size * 1.6 + 1) then
    begin
      ids[count] := i;
      Inc(count);
    end;
  if (count < 20) then
    exit;
  var cells := TBlobCells.Create(blobs, ids, count, reach);
  try
    var found := cells.Found;
    var seen: TArray<Boolean>;
    SetLength(seen, count);
    for var i := 0 to count - 1 do
      seen[i] := false;
    var members: TArray<Integer>;
    SetLength(members, count);
    for var start := 0 to count - 1 do
    begin
      if seen[start] then
        continue;
      members[0] := start;
      var n := 1;
      seen[start] := true;
      var k := 0;
      while (k < n) do
      begin
        var b := blobs[ids[members[k]]];
        for var m := 0 to cells.Near(b.X, b.Y, reach) - 1 do
        begin
          var j := found[m];
          if not seen[j] and (Sqr(blobs[ids[j]].X - b.X) +
            Sqr(blobs[ids[j]].Y - b.Y) <= Sqr(reach)) then
          begin
            seen[j] := true;
            members[n] := j;
            Inc(n);
          end;
        end;
        Inc(k);
      end;
      if (n >= 20) then
      begin
        var group: TArray<Integer>;
        SetLength(group, n);
        for var m := 0 to n - 1 do
          group[m] := ids[members[m]];
        Result := Result + [group];
      end;
    end;
  finally
    cells.Free;
  end;
end;

type
  /// <summary>The normal equations of at most 8 unknowns (each row the
  /// coefficients, then the value).</summary>
  TNormalEquations = array [0 .. 7, 0 .. 8] of Double;

/// <summary>Adds the equation (the coefficients of the n unknowns, then the
/// value) to the normal equations a.</summary>
procedure AddEquation(var a: TNormalEquations; const e: array of Double;
  n: Integer);
begin
  for var p := 0 to n - 1 do
    if (e[p] <> 0) then
      for var q := 0 to n - 1 do
        a[p, q] := a[p, q] + e[p] * e[q];
  for var p := 0 to n - 1 do
    a[p, 8] := a[p, 8] + e[p] * e[n];
end;

/// <summary>The solution of the normal equations a of n unknowns (Gauss with
/// pivoting); false when singular.</summary>
function SolveNormal(var a: TNormalEquations; n: Integer;
  out solution: array of Double): Boolean;
begin
  Result := false;
  for var col := 0 to n - 1 do
  begin
    var pivot := col;
    for var p := col + 1 to n - 1 do
      if (Abs(a[p, col]) > Abs(a[pivot, col])) then
        pivot := p;
    if (Abs(a[pivot, col]) < 1E-9) then
      exit;
    if (pivot <> col) then
      for var q := 0 to 8 do
      begin
        var t := a[col, q];
        a[col, q] := a[pivot, q];
        a[pivot, q] := t;
      end;
    for var p := col + 1 to n - 1 do
    begin
      var f := a[p, col] / a[col, col];
      if (f <> 0) then
      begin
        for var q := col to n - 1 do
          a[p, q] := a[p, q] - f * a[col, q];
        a[p, 8] := a[p, 8] - f * a[col, 8];
      end;
    end;
  end;
  for var p := n - 1 downto 0 do
  begin
    var v := a[p, 8];
    for var q := p + 1 to n - 1 do
      v := v - a[p, q] * solution[q];
    solution[p] := v / a[p, p];
  end;
  Result := true;
end;

/// <summary>The map (projective, else affine) fitted to the placed blobs
/// of a group (least squares; the image coordinates about (mx, my) in
/// units of scale); false with fewer than minimum blobs or singular.
/// </summary>
function FitMap(const blobs: TArray<TBlob>; const group: TArray<Integer>;
  const places: TArray<TPoint>; const placed: TArray<Boolean>;
  projective: Boolean; mx, my, scale: Double; minimum: Integer;
  out map: TGridMap): Boolean;
begin
  Result := false;
  var unknowns := 6;
  if projective then
    unknowns := 8;
  var a: TNormalEquations;
  FillChar(a, SizeOf(a), 0);
  var used := 0;
  var ex: array [0 .. 8] of Double;
  var ey: array [0 .. 8] of Double;
  for var i := 0 to High(group) do
    if placed[i] then
    begin
      var c: Double := places[i].X;
      var r: Double := places[i].Y;
      var x := (blobs[group[i]].X - mx) / scale;
      var y := (blobs[group[i]].Y - my) / scale;
      FillChar(ex, SizeOf(ex), 0);
      FillChar(ey, SizeOf(ey), 0);
      ex[0] := c;
      ex[1] := r;
      ex[2] := 1;
      ey[3] := c;
      ey[4] := r;
      ey[5] := 1;
      if projective then
      begin
        ex[6] := -c * x;
        ex[7] := -r * x;
        ey[6] := -c * y;
        ey[7] := -r * y;
      end;
      ex[unknowns] := x;
      ey[unknowns] := y;
      AddEquation(a, ex, unknowns);
      AddEquation(a, ey, unknowns);
      Inc(used);
    end;
  if (used < minimum) then
    exit;
  var u: array [0 .. 7] of Double;
  if not SolveNormal(a, unknowns, u) then
    exit;
  var g6 := 0.0;
  var g7 := 0.0;
  if projective then
  begin
    g6 := u[6];
    g7 := u[7];
  end;
  // (back to image coordinates)
  map.H[0] := scale * u[0] + mx * g6;
  map.H[1] := scale * u[1] + mx * g7;
  map.H[2] := scale * u[2] + mx;
  map.H[3] := scale * u[3] + my * g6;
  map.H[4] := scale * u[4] + my * g7;
  map.H[5] := scale * u[5] + my;
  map.H[6] := g6;
  map.H[7] := g7;
  map.H[8] := 1;
  Result := true;
end;

/// <summary>The grid of the blobs of a group (each within reach of the
/// next): the direction of the grid from the directions to the nearest
/// neighbours (diagonals of the grid), the places from neighbour to
/// neighbour, then a projective map fitted to them; false when the blobs
/// are no grid.</summary>
function FitGrid(const blobs: TArray<TBlob>; const group: TArray<Integer>;
  reach: Double; out map: TGridMap; out places: TArray<TPoint>): Boolean;
begin
  Result := false;
  var n := Length(group);
  var cells := TBlobCells.Create(blobs, group, n, reach);
  try
    var found := cells.Found;
    // the nearest neighbours of (at most about 200 of) the blobs: distance
    // and direction (modulo 90 degrees)
    var stride := n div 200 + 1;
    var distances: TArray<Double>;
    SetLength(distances, (n + stride - 1) div stride);
    var sampled := 0;
    var sumCos := 0.0;
    var sumSin := 0.0;
    var s := 0;
    while (s < n) do
    begin
      var a := blobs[group[s]];
      var best := Sqr(reach);
      var bestDx := reach;
      var bestDy := 0.0;
      for var m := 0 to cells.Near(a.X, a.Y, reach) - 1 do
      begin
        var j := found[m];
        if (j <> s) then
        begin
          var dx := blobs[group[j]].X - a.X;
          var dy := blobs[group[j]].Y - a.Y;
          var d := Sqr(dx) + Sqr(dy);
          if (d < best) then
          begin
            best := d;
            bestDx := dx;
            bestDy := dy;
          end;
        end;
      end;
      distances[sampled] := Sqrt(best);
      Inc(sampled);
      // the angle times 4: the 4 diagonals the same
      var angle := 4 * ArcTan2(bestDy, bestDx);
      sumCos := sumCos + Cos(angle);
      sumSin := sumSin + Sin(angle);
      Inc(s, stride);
    end;
    // (most nearest neighbours in about one direction: the grid)
    if (Sqrt(Sqr(sumCos) + Sqr(sumSin)) < MIN_COHERENCE * sampled) then
      exit;
    TArray.Sort<Double>(distances);
    var diagonal := distances[sampled div 2];
    if (diagonal < 2) then
      exit;
    // the diagonal direction, then the grid direction 45 degrees away
    var theta := ArcTan2(sumSin, sumCos) / 4 - Pi / 4;
    var pitch := diagonal / Sqrt(2);
    var ex: TPointD;
    ex.X := Cos(theta) * pitch;
    ex.Y := Sin(theta) * pitch;
    var ey: TPointD;
    ey.X := -ex.Y;
    ey.Y := ex.X;

    // the places from neighbour to neighbour (breadth first)
    SetLength(places, n);
    var placed: TArray<Boolean>;
    SetLength(placed, n);
    for var p := 0 to n - 1 do
      placed[p] := false;
    // (from the blob nearest to the middle)
    var mx := 0.0;
    var my := 0.0;
    for var b in group do
    begin
      mx := mx + blobs[b].X;
      my := my + blobs[b].Y;
    end;
    mx := mx / n;
    my := my / n;
    var first := 0;
    for var p := 1 to n - 1 do
      if (Sqr(blobs[group[p]].X - mx) + Sqr(blobs[group[p]].Y - my) <
        Sqr(blobs[group[first]].X - mx) + Sqr(blobs[group[first]].Y - my)) then
        first := p;
    places[first] := TPoint.Create(0, 0);
    placed[first] := true;
    var queue: TArray<Integer>;
    SetLength(queue, n);
    queue[0] := first;
    var count := 1;
    var k := 0;
    var step := 2.6 * pitch;
    var far := 6 * pitch;
    var det := ex.X * ey.Y - ex.Y * ey.X;
    var grown: Boolean;
    var wrongParity := 0;
    repeat
      while (k < count) do
      begin
        var a := queue[k];
        Inc(k);
        var ba := blobs[group[a]];
        for var m := 0 to cells.Near(ba.X, ba.Y, step) - 1 do
        begin
          var j := found[m];
          if not placed[j] then
          begin
            var dx := blobs[group[j]].X - ba.X;
            var dy := blobs[group[j]].Y - ba.Y;
            if (Sqr(dx) + Sqr(dy) > Sqr(step)) then
              continue;
            var c := (ey.Y * dx - ey.X * dy) / det;
            var r := (ex.X * dy - ex.Y * dx) / det;
            places[j] := TPoint.Create(places[a].X + Round(c),
              places[a].Y + Round(r));
            placed[j] := true;
            queue[count] := j;
            Inc(count);
            // (a grid of dots: all on one parity, else no grid)
            if Odd(places[j].X + places[j].Y) then
              Inc(wrongParity);
            if (count >= 30) and (5 * wrongParity > count) then
              exit;
          end;
        end;
      end;
      // stopped at a gap in the dots: on with the blobs a few places away,
      // from the map of the places so far
      grown := false;
      if (count < n) and FitMap(blobs, group, places, placed, count >= 40,
        mx, my, pitch, 6, map) then
        for var j := 0 to n - 1 do
          if not placed[j] then
          begin
            var bj := blobs[group[j]];
            var nearby := false;
            for var m := 0 to cells.Near(bj.X, bj.Y, far) - 1 do
            begin
              var a := found[m];
              if placed[a] and (Sqr(bj.X - blobs[group[a]].X) +
                Sqr(bj.Y - blobs[group[a]].Y) <= Sqr(far)) then
              begin
                nearby := true;
                break;
              end;
            end;
            var c, r: Double;
            if not nearby or not map.Unmap(bj.X, bj.Y, c, r) or
              (Abs(c - Round(c)) > 0.3) or (Abs(r - Round(r)) > 0.3) then
              continue;
            places[j] := TPoint.Create(Round(c), Round(r));
            placed[j] := true;
            queue[count] := j;
            Inc(count);
            grown := true;
            // (a grid of dots: all on one parity, else no grid)
            if Odd(places[j].X + places[j].Y) then
              Inc(wrongParity);
            if (count >= 30) and (5 * wrongParity > count) then
              exit;
          end;
    until not grown;

    // the map fitted (projective: the perspective of a photo), then the
    // places again from it
    for var pass := 0 to 2 do
    begin
      if not FitMap(blobs, group, places, placed, true, mx, my, pitch, 20,
        map) then
        exit;
      for var i := 0 to n - 1 do
      begin
        var c, r: Double;
        if not map.Unmap(blobs[group[i]].X, blobs[group[i]].Y, c, r) then
          exit;
        places[i] := TPoint.Create(Round(c), Round(r));
        placed[i] := (Abs(c - Round(c)) < 0.35) and (Abs(r - Round(r)) < 0.35);
      end;
    end;
    // most blobs on the grid, on one parity
    var onGrid := 0;
    var even := 0;
    for var i := 0 to n - 1 do
      if placed[i] then
      begin
        Inc(onGrid);
        if not Odd(places[i].X + places[i].Y) then
          Inc(even);
      end;
    Result := (onGrid >= 0.8 * n) and ((even >= 0.9 * onGrid) or
      (even <= 0.1 * onGrid));
  finally
    cells.Free;
  end;
end;

/// <summary>Decodes the dots of the grid (the image sampled at the places
/// of columns c1 to c2 and rows r1 to r2), in the 8 directions; false when
/// none decodes.</summary>
function DecodeGrid(image: TBitMatrix; const map: TGridMap;
  c1, c2, r1, r2: Integer; out decoded: TDotCodeResult;
  out corners: TArray<TPointD>): Boolean;
begin
  Result := false;
  var w := c2 - c1 + 1;
  var h := r2 - r1 + 1;
  if (w < 5) or (h < 5) or not Odd(w + h) then
    exit;
  // a quiet zone: (almost) no dots in the 2 places around (else a part of
  // a larger symbol, or noise)
  var ring := 0;
  var darkRing := 0;
  for var r := r1 - 2 to r2 + 2 do
    for var c := c1 - 2 to c2 + 2 do
      if (c < c1) or (c > c2) or (r < r1) or (r > r2) then
      begin
        Inc(ring);
        var p := map.Map(c, r);
        var x := Floor(p.X);
        var y := Floor(p.Y);
        if (x >= 0) and (y >= 0) and (x < image.Width) and
          (y < image.Height) and image[x, y] then
          Inc(darkRing);
      end;
  if (darkRing > ring div 10) then
    exit;
  // the dots: dark at the place (or next to it)
  var grid: TArray<Boolean>;
  SetLength(grid, w * h);
  for var r := 0 to h - 1 do
    for var c := 0 to w - 1 do
    begin
      var p := map.Map(c1 + c, r1 + r);
      var x := Floor(p.X);
      var y := Floor(p.Y);
      var dark := false;
      if (x >= 0) and (y >= 0) and (x < image.Width) and (y < image.Height) then
        dark := image[x, y];
      grid[r * w + c] := dark;
    end;
  // the dots of a DotCode: on about 55% of the places of one parity (the
  // codewords 5 of 9), none on the others
  var counts := [0, 0];
  for var r := 0 to h - 1 do
    for var c := 0 to w - 1 do
      if grid[r * w + c] then
        Inc(counts[(c1 + c + r1 + r) and 1]);
  var most := Max(counts[0], counts[1]);
  if (most < 0.4 * (w * h div 2)) or (most > 0.75 * (w * h div 2 + 1)) or
    (Min(counts[0], counts[1]) > most div 10) then
    exit;
  // 8 directions: turned 0, 90, 180, 270 degrees, mirrored or not
  for var t := 0 to 7 do
  begin
    var tw := w;
    var th := h;
    if Odd(t) then
    begin
      tw := h;
      th := w;
    end;
    var dots: TArray<Boolean>;
    SetLength(dots, tw * th);
    var any := false;
    for var r := 0 to th - 1 do
      for var c := 0 to tw - 1 do
      begin
        // the place in the grid of (c, r) of the direction
        var gc, gr: Integer;
        case t mod 4 of
          0:
            begin
              gc := c;
              gr := r;
            end;
          1:
            begin
              gc := r;
              gr := tw - 1 - c;
            end;
          2:
            begin
              gc := tw - 1 - c;
              gr := th - 1 - r;
            end;
        else
          begin
            gc := th - 1 - r;
            gr := c;
          end;
        end;
        if (t >= 4) then
          gc := w - 1 - gc;
        dots[r * tw + c] := grid[gr * w + gc];
        if dots[r * tw + c] and Odd(c + r) then
          any := true;
      end;
    // (dots on the odd places: not this direction)
    if any then
      continue;
    // (the direction with the fewest errors: a small symbol can also
    // decode in a wrong one)
    var attempt: TDotCodeResult;
    if DecodeDotCode(dots, tw, th, attempt) and (not Result or
      (attempt.Errors < decoded.Errors)) then
    begin
      Result := true;
      decoded := attempt;
      corners := [map.Map(c1, r1), map.Map(c2, r1), map.Map(c2, r2),
        map.Map(c1, r2)];
      if (decoded.Errors = 0) then
        exit;
    end;
  end;
end;

{ TDotCodeReader }

function TDotCodeReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TDotCodeReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
  var results := TList<TReadResult>.Create;
  try
    decodeMultiple(image, hints, results, 1);
    if (results.Count > 0) then
      Result := results.Extract(results[0]);
  finally
    for var r in results do
      r.Free;
    results.Free;
  end;
end;

procedure TDotCodeReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var matrix := image.BlackMatrix;
  var blobs := FindBlobs(matrix, Max(matrix.Width, matrix.Height) div 20);
  if (Length(blobs) < 20) then
    exit;
  // the sizes of the dots: the sizes of the blobs seen most
  var sizes := TDictionary<Integer, Integer>.Create;
  try
    for var b in blobs do
    begin
      var count: Integer;
      if not sizes.TryGetValue(b.Size, count) then
        count := 0;
      sizes.AddOrSetValue(b.Size, count + 1);
    end;
    var tried: TArray<Integer> := [];
    for var attempt := 1 to 3 do
    begin
      var size := -1;
      var most := 0;
      for var pair in sizes do
      begin
        var near := false;
        for var t in tried do
          if (Abs(pair.Key - t) <= Max(1, t div 3)) then
            near := true;
        // (dots of at least 3 pixels)
        if not near and (pair.Key >= 3) and (pair.Value > most) then
        begin
          most := pair.Value;
          size := pair.Key;
        end;
      end;
      if (size < 0) or (most < 5) then
        break;
      tried := tried + [size];
      // the dots at most 2 places apart (about 3 to 5 sizes)
      var reach := 5.0 * size + 2;
      for var group in GroupBlobs(blobs, size, reach) do
      begin
        var map: TGridMap;
        var places: TArray<TPoint>;
        if not FitGrid(blobs, group, reach, map, places) then
          continue;
        var c1 := MaxInt;
        var c2 := -MaxInt;
        var r1 := MaxInt;
        var r2 := -MaxInt;
        for var p in places do
        begin
          c1 := Min(c1, p.X);
          c2 := Max(c2, p.X);
          r1 := Min(r1, p.Y);
          r2 := Max(r2, p.Y);
        end;
        var decoded: TDotCodeResult;
        var corners: TArray<TPointD>;
        // the edge rows or columns can be empty: the grid also widened by
        // an empty row or column on one or two sides
        var found := false;
        for var e := 0 to 15 do
        begin
          var grow := Ord(e and 1 <> 0) + Ord(e and 2 <> 0) +
            Ord(e and 4 <> 0) + Ord(e and 8 <> 0);
          if (grow > 2) then
            continue;
          // (the one with the fewest errors)
          var candidate: TDotCodeResult;
          var candidateCorners: TArray<TPointD>;
          if DecodeGrid(matrix, map, c1 - Ord(e and 1 <> 0),
            c2 + Ord(e and 2 <> 0), r1 - Ord(e and 4 <> 0),
            r2 + Ord(e and 8 <> 0), candidate, candidateCorners) and
            (not found or (candidate.Errors < decoded.Errors)) then
          begin
            found := true;
            decoded := candidate;
            corners := candidateCorners;
            if (decoded.Errors = 0) then
              break;
          end;
        end;
        if not found then
          continue;
        var points: TArray<IResultPoint>;
        for var p in corners do
          points := points + [TResultPointHelpers.CreateResultPoint(p.X, p.Y)];
        var r := TReadResult.Create(decoded.Text, nil, points,
          TBarcodeFormat.DOTCODE);
        // ISO/IEC 15424: ]J0 DotCode, ]J1 GS1 DotCode
        if decoded.GS1 then
          r.SymbologyIdentifier := ']J1'
        else
          r.SymbologyIdentifier := ']J0';
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
        if ResultsFull(results, maxCount) then
          exit;
      end;
    end;
  finally
    sizes.Free;
  end;
end;

procedure TDotCodeReader.reset;
begin
  // do nothing
end;

end.
