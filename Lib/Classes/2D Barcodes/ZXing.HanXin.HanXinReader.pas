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

  * The Han Xin Code reader for ZXing.Delphi: zxing-cpp, ZXing Java and
  * ZXing.Net have none. The 4 finder patterns each have a core of 3 x 3
  * modules, alone in the image (a light ring around it), with 2 thin
  * L-shaped bars on 2 sides: the cores are found as blobs, their thin sides
  * by rays, and the symbol from 2 to 4 of them.
}

unit ZXing.HanXin.HanXinReader;

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
  /// Reads Han Xin Code symbols: in any direction, mirrored and in
  /// perspective too.
  /// </summary>
  THanXinReader = class(TInterfacedObject, IReader, IMultipleReader)
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
  ZXing.HanXin.Decoder;

type
  TPointD = record
    X, Y: Double;
    class function Create(x, y: Double): TPointD; static;
  end;

  /// <summary>The core of a finder pattern: its centre, the module size and
  /// the directions (unit vectors) to its 2 thin sides, Q 90 degrees
  /// clockwise (in the image) from P.</summary>
  TCore = record
    Center: TPointD;
    Module: Double;
    P, Q: TPointD;
  end;

  /// <summary>The finder patterns: top left, top right, bottom right, bottom
  /// left.</summary>
  TCorner = (cnTopLeft, cnTopRight, cnBottomRight, cnBottomLeft);

  /// <summary>A projective map (homography, row major, H[8] = 1) from
  /// module coordinates (column, row) to the image.</summary>
  TModuleMap = record
    H: array [0 .. 8] of Double;
    function Map(c, r: Double): TPointD;
  end;

class function TPointD.Create(x, y: Double): TPointD;
begin
  Result.X := x;
  Result.Y := y;
end;

function TModuleMap.Map(c, r: Double): TPointD;
begin
  var w := H[6] * c + H[7] * r + H[8];
  if (Abs(w) < 1E-12) then
    w := 1E-12;
  Result.X := (H[0] * c + H[1] * r + H[2]) / w;
  Result.Y := (H[3] * c + H[4] * r + H[5]) / w;
end;

function Dot(const a, b: TPointD): Double; inline;
begin
  Result := a.X * b.X + a.Y * b.Y;
end;

function Dark(image: TBitMatrix; x, y: Double): Boolean;
begin
  var ix := Floor(x);
  var iy := Floor(y);
  Result := (ix >= 0) and (iy >= 0) and (ix < image.Width) and
    (iy < image.Height) and image[ix, iy];
end;

/// <summary>The centre of the core of a finder pattern in module coordinates
/// (s: the size of the symbol - 11).</summary>
function CorePosition(corner: TCorner; s: Double): TPointD;
begin
  case corner of
    cnTopLeft:
      Result := TPointD.Create(5.5, 5.5);
    cnTopRight:
      Result := TPointD.Create(5.5 + s, 5.5);
    cnBottomRight:
      Result := TPointD.Create(5.5 + s, 5.5 + s);
  else
    Result := TPointD.Create(1.5, 9.5 + s);
  end;
end;

/// <summary>The directions of the columns (x) and rows (y) of the symbol
/// from the thin sides of the core of a finder pattern.</summary>
procedure CoreAxes(const core: TCore; corner: TCorner; out x, y: TPointD);
begin
  case corner of
    cnTopLeft:
      begin
        x := TPointD.Create(-core.P.X, -core.P.Y);
        y := TPointD.Create(-core.Q.X, -core.Q.Y);
      end;
    cnBottomRight:
      begin
        x := core.P;
        y := core.Q;
      end;
  else
    // top right and bottom left: the same pattern
    begin
      x := core.Q;
      y := TPointD.Create(-core.P.X, -core.P.Y);
    end;
  end;
end;

/// <summary>Whether the image along the direction (angle) from the centre
/// is a thin side of a finder pattern: dark, light, dark, light, dark,
/// light at 1 to 6 modules (the bars as wide as the core: also 1 module to
/// either side); the number of the 18 places that are.</summary>
function ThinScore(image: TBitMatrix; const center: TPointD;
  module, angle: Double): Integer;
begin
  Result := 0;
  var dx := Cos(angle) * module;
  var dy := Sin(angle) * module;
  for var t := 1 to 6 do
    for var side := -1 to 1 do
      if (Dark(image, center.X + t * dx - side * dy,
        center.Y + t * dy + side * dx) = Odd(t)) then
        Inc(Result);
end;

/// <summary>The core of a finder pattern at the blob: dark in the middle,
/// light around it, 2 thin sides 90 degrees apart; false when it is none.
/// </summary>
function ProbeCore(image: TBitMatrix; const blob: TBlob;
  out core: TCore): Boolean;
const
  STEPS = 120;
begin
  Result := false;
  // 3 x 3 modules
  var module := Sqrt(blob.Count) / 3;
  if (module < 1.5) then
    exit;
  var center := TPointD.Create(blob.X, blob.Y);
  // (quick: a solid core, dark in the middle and at 0.75 modules around)
  if not Dark(image, center.X, center.Y) then
    exit;
  var lightCore := 0;
  for var k := 0 to 7 do
    if not Dark(image, center.X + 0.75 * module * Cos(k * Pi / 4),
      center.Y + 0.75 * module * Sin(k * Pi / 4)) then
      Inc(lightCore);
  if (lightCore > 1) then
    exit;
  // (quick: light around the core, at 2.2 modules in 16 directions; the
  // corners of the core reach 2.1 modules)
  var darkRing := 0;
  for var k := 0 to 15 do
  begin
    var a := k * Pi / 8;
    if Dark(image, center.X + 2.2 * module * Cos(a),
      center.Y + 2.2 * module * Sin(a)) then
      Inc(darkRing);
  end;
  if (darkRing > 4) then
    exit;
  // (quick: 2 thin sides 90 degrees apart, every 15 degrees)
  var coarse: array [0 .. 23] of Integer;
  for var k := 0 to 23 do
    coarse[k] := ThinScore(image, center, module, k * Pi / 12);
  var likely := false;
  for var k := 0 to 23 do
    if (coarse[k] + coarse[(k + 6) mod 24] >= 26) then
      likely := true;
  if not likely then
    exit;
  // the direction of the corner of the 2 thin sides (45 degrees from each;
  // the module somewhat smaller or larger along them: perspective)
  var scores: array [0 .. STEPS - 1] of Integer;
  for var k := 0 to STEPS - 1 do
  begin
    scores[k] := 0;
    for var scale in [1.0, 0.9, 1.1] do
      scores[k] := Max(scores[k], ThinScore(image, center, scale * module,
        2 * Pi * k / STEPS));
  end;
  var best := -1;
  var bestScore := 0;
  for var k := 0 to STEPS - 1 do
  begin
    var score := scores[(k + STEPS - STEPS div 8) mod STEPS] +
      scores[(k + STEPS div 8) mod STEPS];
    if (score > bestScore) then
    begin
      bestScore := score;
      best := k;
    end;
  end;
  if (bestScore < 33) then
    exit;
  // (the middle of the directions with that score)
  var first := best;
  var last := best;
  while (scores[(last + 1 + STEPS - STEPS div 8) mod STEPS] +
    scores[(last + 1 + STEPS div 8) mod STEPS] = bestScore) and
    (last - first < STEPS div 4) do
    Inc(last);
  while (scores[(first - 1 + 2 * STEPS - STEPS div 8) mod STEPS] +
    scores[(first - 1 + STEPS + STEPS div 8) mod STEPS] = bestScore) and
    (last - first < STEPS div 4) do
    Dec(first);
  var corner := 2 * Pi * (first + last) / 2 / STEPS;
  core.Center := center;
  core.P := TPointD.Create(Cos(corner - Pi / 4), Sin(corner - Pi / 4));
  core.Q := TPointD.Create(Cos(corner + Pi / 4), Sin(corner + Pi / 4));
  // the module size from the outer edges of the thin bars (5.5 modules)
  var sum := 0.0;
  var n := 0;
  for var side in [core.P, core.Q] do
  begin
    var edge := -1.0;
    var t := 4.5 * module;
    while (t < 7 * module) do
    begin
      if Dark(image, center.X + t * side.X, center.Y + t * side.Y) then
        edge := t;
      t := t + 0.25;
    end;
    if (edge > 0) then
    begin
      sum := sum + (edge + 0.25) / 5.5;
      Inc(n);
    end;
  end;
  core.Module := module;
  if (n > 0) then
    core.Module := sum / n;
  Result := true;
end;

/// <summary>The cores of the finder patterns in the image.</summary>
function FindCores(image: TBitMatrix): TArray<TCore>;
begin
  Result := [];
  for var blob in FindBlobs(image, Max(image.Width, image.Height) div 4) do
  begin
    if (blob.Count < 20) then
      continue;
    var core: TCore;
    if ProbeCore(image, blob, core) then
      Result := Result + [core];
  end;
end;

/// <summary>The solution of the n x n equations a (each row the
/// coefficients, then the value); false when singular.</summary>
function Solve(var a: TArray<TArray<Double>>; n: Integer;
  out solution: TArray<Double>): Boolean;
begin
  Result := false;
  for var col := 0 to n - 1 do
  begin
    var pivot := col;
    for var p := col + 1 to n - 1 do
      if (Abs(a[p, col]) > Abs(a[pivot, col])) then
        pivot := p;
    if (Abs(a[pivot, col]) < 1E-12) then
      exit;
    var t := a[col];
    a[col] := a[pivot];
    a[pivot] := t;
    for var p := col + 1 to n - 1 do
    begin
      var f := a[p, col] / a[col, col];
      for var q := col to n do
        a[p, q] := a[p, q] - f * a[col, q];
    end;
  end;
  SetLength(solution, n);
  for var p := n - 1 downto 0 do
  begin
    var v := a[p, n];
    for var q := p + 1 to n - 1 do
      v := v - a[p, q] * solution[q];
    solution[p] := v / a[p, p];
  end;
  Result := true;
end;

/// <summary>The map of the module coordinates to the image points:
/// projective with 4 points, affine with 3; false when it can not be
/// made.</summary>
function MakeMap(const modules, points: TArray<TPointD>;
  out map: TModuleMap): Boolean;
begin
  var n := Length(modules);
  var unknowns := 8;
  if (n < 4) then
    unknowns := 6;
  var a: TArray<TArray<Double>>;
  SetLength(a, unknowns);
  for var p := 0 to unknowns - 1 do
  begin
    SetLength(a[p], unknowns + 1);
    for var q := 0 to unknowns do
      a[p, q] := 0;
  end;
  // the least squares normal equations of x (c, r, 1, -cx, -rx) and y
  for var i := 0 to n - 1 do
    for var axis := 0 to 1 do
    begin
      var e: array [0 .. 8] of Double;
      for var q := 0 to 8 do
        e[q] := 0;
      var c := modules[i].X;
      var r := modules[i].Y;
      var v := points[i].X;
      if (axis = 1) then
        v := points[i].Y;
      e[3 * axis] := c;
      e[3 * axis + 1] := r;
      e[3 * axis + 2] := 1;
      if (unknowns = 8) then
      begin
        e[6] := -c * v;
        e[7] := -r * v;
      end;
      e[unknowns] := v;
      for var p := 0 to unknowns - 1 do
        for var q := 0 to unknowns do
          a[p, q] := a[p, q] + e[p] * e[q];
    end;
  var u: TArray<Double>;
  Result := Solve(a, unknowns, u);
  if not Result then
    exit;
  for var k := 0 to 5 do
    map.H[k] := u[k];
  map.H[6] := 0;
  map.H[7] := 0;
  if (unknowns = 8) then
  begin
    map.H[6] := u[6];
    map.H[7] := u[7];
  end;
  map.H[8] := 1;
end;

/// <summary>The map corrected by the known modules (the finder and
/// alignment patterns): around points every 6 modules the shift (up to a
/// module) with which most of them are right, a projective map fitted to
/// those points; twice.</summary>
procedure RefineMap(image: TBitMatrix; var map: TModuleMap; version: Integer);
const
  RADIUS = 5;
  STEP = 6;
begin
  var size := HanXinSize(version);
  var pattern := HanXinFunctionPattern(version);
  for var pass := 1 to 2 do
  begin
    var mods: TArray<TPointD> := [];
    var pts: TArray<TPointD> := [];
    var ay := 2;
    while (ay < size) do
    begin
      var ax := 2;
      while (ax < size) do
      begin
        // the known modules around
        var cs, rs: TArray<Integer>;
        var darks: TArray<Boolean>;
        cs := [];
        rs := [];
        darks := [];
        for var r := Max(0, ay - RADIUS) to Min(size - 1, ay + RADIUS) do
          for var c := Max(0, ax - RADIUS) to Min(size - 1, ax + RADIUS) do
          begin
            var index := r * size + c;
            if (pattern[index] <> 0) then
            begin
              cs := cs + [c];
              rs := rs + [r];
              darks := darks + [pattern[index] = 2];
            end;
          end;
        if (Length(cs) >= 15) then
        begin
          var bestScore := -1;
          var bestDx := 0.0;
          var bestDy := 0.0;
          for var sy := -4 to 4 do
            for var sx := -4 to 4 do
            begin
              var score := 0;
              for var i := 0 to High(cs) do
              begin
                var p := map.Map(cs[i] + 0.5 + sx / 4, rs[i] + 0.5 + sy / 4);
                if (Dark(image, p.X, p.Y) = darks[i]) then
                  Inc(score);
              end;
              // (the smallest shift of the best)
              if (score > bestScore) or (score = bestScore) and
                (Abs(sx) + Abs(sy) < 4 * (Abs(bestDx) + Abs(bestDy))) then
              begin
                bestScore := score;
                bestDx := sx / 4;
                bestDy := sy / 4;
              end;
            end;
          if (bestScore >= 0.9 * Length(cs)) then
          begin
            mods := mods + [TPointD.Create(ax + 0.5, ay + 0.5)];
            pts := pts + [map.Map(ax + 0.5 + bestDx, ay + 0.5 + bestDy)];
          end;
        end;
        Inc(ax, STEP);
      end;
      Inc(ay, STEP);
    end;
    var refined: TModuleMap;
    if (Length(mods) >= 6) and MakeMap(mods, pts, refined) then
      map := refined;
  end;
end;

/// <summary>Samples and decodes the symbol of version at the map (refined
/// when it decodes so); false when it does not decode (hint: the version of
/// its function information when that is another one, else 0).</summary>
function SampleAndDecode(image: TBitMatrix; var map: TModuleMap;
  version: Integer; out decoded: THanXinResult; out hint: Integer): Boolean;

  function Sample(const at: TModuleMap): TArray<Boolean>;
  begin
    var size := HanXinSize(version);
    SetLength(Result, size * size);
    for var r := 0 to size - 1 do
      for var c := 0 to size - 1 do
      begin
        var p := at.Map(c + 0.5, r + 0.5);
        Result[r * size + c] := Dark(image, p.X, p.Y);
      end;
  end;

begin
  Result := false;
  hint := 0;
  var size := HanXinSize(version);
  // the version of the function information first (it is near the finder
  // patterns), then the map refined for it
  var v, level, mask: Integer;
  var at := map;
  if not ReadHanXinFunctionInfo(
    function(x, y: Integer): Boolean
    begin
      var p := at.Map(x + 0.5, y + 0.5);
      Result := Dark(image, p.X, p.Y);
    end, size, v, level, mask) then
    exit;
  if (v <> version) then
  begin
    hint := v;
    exit;
  end;
  var refined := map;
  RefineMap(image, refined, version);
  if DecodeHanXin(Sample(refined), size, decoded) then
  begin
    map := refined;
    exit(true);
  end;
  Result := DecodeHanXin(Sample(map), size, decoded);
end;

/// <summary>The symbols of 2 or more of the cores (not used yet): 2 cores as
/// 2 corners give the symbol (its axes, module size and size), then the
/// other corners are looked for; true when the results are full.</summary>
function FindSymbols(matrix: TBitMatrix; const cores: TArray<TCore>;
  var used: TArray<Boolean>; results: TList<TReadResult>;
  maxCount: Integer): Boolean;
begin
  Result := false;
  var n := Length(cores);
  for var a := 0 to n - 1 do
    for var b := 0 to n - 1 do
    begin
      if (a = b) or used[a] or used[b] then
        continue;
      var ca := cores[a];
      var cb := cores[b];
      if (ca.Module > 1.6 * cb.Module) or (cb.Module > 1.6 * ca.Module) then
        continue;
      var module := (ca.Module + cb.Module) / 2;
      for var ta := Low(TCorner) to High(TCorner) do
        for var tb := Low(TCorner) to High(TCorner) do
        begin
          if (tb <= ta) or used[a] or used[b] then
            continue;
          var xa, ya, xb, yb: TPointD;
          CoreAxes(ca, ta, xa, ya);
          CoreAxes(cb, tb, xb, yb);
          if (Dot(xa, xb) < 0.94) or (Dot(ya, yb) < 0.94) then
            continue;
          var x := TPointD.Create((xa.X + xb.X) / 2, (xa.Y + xb.Y) / 2);
          var y := TPointD.Create((ya.X + yb.X) / 2, (ya.Y + yb.Y) / 2);
          // the offset in modules: d0 + s d1
          var d := TPointD.Create(cb.Center.X - ca.Center.X,
            cb.Center.Y - ca.Center.Y);
          var dm := TPointD.Create(Dot(d, x) / module, Dot(d, y) / module);
          var p0 := CorePosition(ta, 0);
          var p1 := CorePosition(ta, 1);
          var q0 := CorePosition(tb, 0);
          var q1 := CorePosition(tb, 1);
          var d0 := TPointD.Create(q0.X - p0.X, q0.Y - p0.Y);
          var d1 := TPointD.Create(q1.X - p1.X - d0.X, q1.Y - p1.Y - d0.Y);
          var s := Dot(TPointD.Create(dm.X - d0.X, dm.Y - d0.Y), d1) /
            Dot(d1, d1);
          if (s < 11) or (s > 180) then
            continue;
          var residual := Sqrt(Sqr(d0.X + s * d1.X - dm.X) +
            Sqr(d0.Y + s * d1.Y - dm.Y));
          if (residual > 0.15 * s + 2) then
            continue;
          // the versions near: size = s + 11 = 2 v + 21
          var guess := Round((s - 10) / 2);
          var versions: TArray<Integer> := [guess, guess - 1, guess + 1,
            guess - 2, guess + 2];
          var vi := 0;
          while (vi < Length(versions)) do
          begin
            var version := versions[vi];
            Inc(vi);
            if (version < 1) or (version > 84) then
              continue;
            var sv := HanXinSize(version) - 11;
            // the cores of all 4 corners where the 2 predict them (the
            // module size of this version from the 2)
            var mods: TArray<TPointD> := [];
            var pts: TArray<TPointD> := [];
            var found: TArray<Integer> := [];
            var pa := CorePosition(ta, sv);
            var pb := CorePosition(tb, sv);
            var mv := Sqrt(Sqr(d.X) + Sqr(d.Y)) / Sqrt(Sqr(pb.X - pa.X) +
              Sqr(pb.Y - pa.Y));
            for var corner := Low(TCorner) to High(TCorner) do
            begin
              var pc := CorePosition(corner, sv);
              var predicted := TPointD.Create(ca.Center.X + mv *
                ((pc.X - pa.X) * x.X + (pc.Y - pa.Y) * y.X),
                ca.Center.Y + mv * ((pc.X - pa.X) * x.Y + (pc.Y - pa.Y) *
                y.Y));
              var nearest := -1;
              // (farther away less sure: perspective)
              var distance := Sqr(mv * (4 + 0.12 * Sqrt(Sqr(pc.X - pa.X) +
                Sqr(pc.Y - pa.Y))));
              for var k := 0 to n - 1 do
              begin
                var dd := Sqr(cores[k].Center.X - predicted.X) +
                  Sqr(cores[k].Center.Y - predicted.Y);
                if (dd < distance) then
                begin
                  var xk, yk: TPointD;
                  CoreAxes(cores[k], corner, xk, yk);
                  if (Dot(xk, x) > 0.8) and (Dot(yk, y) > 0.8) then
                  begin
                    distance := dd;
                    nearest := k;
                  end;
                end;
              end;
              if (nearest >= 0) then
              begin
                mods := mods + [pc];
                pts := pts + [cores[nearest].Center];
                found := found + [nearest];
              end;
            end;
            if (Length(mods) < 2) then
              continue;
            // 2 cores: a third point square to them (by the axes)
            if (Length(mods) = 2) then
            begin
              var px := -(mods[1].Y - mods[0].Y);
              var py := mods[1].X - mods[0].X;
              mods := mods + [TPointD.Create(mods[0].X + px, mods[0].Y + py)];
              pts := pts + [TPointD.Create(pts[0].X + mv * (px * x.X +
                py * y.X), pts[0].Y + mv * (px * x.Y + py * y.Y))];
            end;
            var map: TModuleMap;
            if not MakeMap(mods, pts, map) then
              continue;
            var decoded: THanXinResult;
            var hint: Integer;
            if not SampleAndDecode(matrix, map, version, decoded, hint) then
            begin
              // (the version of the function information next)
              var known := false;
              for var tried in versions do
                if (tried = hint) then
                  known := true;
              if (hint > 0) and not known then
                Insert(hint, versions, vi);
              continue;
            end;
            var size := HanXinSize(version);
            var corners := [map.Map(0, 0), map.Map(size, 0),
              map.Map(size, size), map.Map(0, size)];
            var points: TArray<IResultPoint> := [];
            for var p in corners do
              points := points + [TResultPointHelpers.CreateResultPoint(p.X,
                p.Y)];
            var r := TReadResult.Create(decoded.Text, nil, points,
              TBarcodeFormat.HAN_XIN);
            // ISO/IEC 15424: ]h0 Han Xin Code
            r.SymbologyIdentifier := ']h0';
            if ContainsResult(results, r) then
              r.Free
            else
              results.Add(r);
            for var k in found do
              used[k] := true;
            if ResultsFull(results, maxCount) then
              exit(true);
            break;
          end;
          if used[a] then
            break;
        end;
    end;
end;

{ THanXinReader }

function THanXinReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function THanXinReader.decode(const image: TBinaryBitmap;
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


procedure THanXinReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var matrix := image.BlackMatrix;
  var found := FindCores(matrix);
  var used: TArray<Boolean>;
  SetLength(used, Length(found));
  for var i := 0 to High(used) do
    used[i] := false;
  // mirrored: the thin sides the other way around (the map then mirrors)
  for var mirrored := false to true do
  begin
    var cores := Copy(found);
    if mirrored then
      for var i := 0 to High(cores) do
      begin
        var p := cores[i].P;
        cores[i].P := cores[i].Q;
        cores[i].Q := p;
      end;
    if FindSymbols(matrix, cores, used, results, maxCount) then
      exit;
  end;
end;


procedure THanXinReader.reset;
begin
  // do nothing
end;

end.
