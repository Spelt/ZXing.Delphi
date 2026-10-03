unit ZXing.Common.LocalGrid;

{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2020, 2026 Axel Waggershauser
  *
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

  * Ported from zxing-cpp (LocalGrid.h/.cpp and GridSampler.cpp): locates
  * patterns (like the alignment patterns between the data regions of a Data
  * Matrix symbol) in the local neighbourhood of their expected position, and
  * samples a grid piecewise between those located points. This corrects
  * symbols that are not flat, e.g. printed on a curved surface.
  *
  * Callers run with the floating point exceptions masked, see
  * TFloatExceptionsMasked.
}

interface

uses
  System.Types,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry;

type
  /// <summary>Directions as string: l(eft), r(ight), u(p) and d(own).</summary>
  TGridDirections = string;

  /// <summary>
  /// A grid of modules around a given module position, estimated from a
  /// global module-to-pixel transformation and then adjusted to the local
  /// position and size of the modules in the image.
  /// </summary>
  TLocalGrid = class
  private
    FImage: TBitMatrix;
    FMod2Pix: TPerspectiveTransformF;
    FDim, FCenter: TPoint;
    FOrigin, FStepX, FStepY: TPointD;

    procedure AdjustOriginAndStep(var step: TPointD; radius: Integer;
      const offsets: TArray<TPointD>);
    /// <summary>-1 outside the image, 0 white, 1 black.</summary>
    function Get(const p: TPointD): Integer;
    /// <summary>Whether there is value v at p or a quarter module before or
    /// after p, or p is outside the image.</summary>
    function FindValue(const p, dir: TPoint; v: Integer): Boolean;
    function IsPatternAt(const p: TPoint; radius: Integer;
      const timingStart: TPoint; const timingDirs: TArray<TPoint>;
      const blackStart: TPoint; const blackDirs: TArray<TPoint>;
      const whiteStart: TPoint; const whiteDirs: TArray<TPoint>): Boolean;
    /// <summary>Whether there is a cross of timing patterns (alternating
    /// black and white, black or white at the center) at module p, up to
    /// radius modules in 4 directions, with at most errorThreshold wrong
    /// modules. Near the edge of the grid the arms continue on the other side.
    /// </summary>
    function IsTimingPatternCross(const p: TPoint; isBlack: Boolean;
      radius: Integer; errorThreshold: Integer = 0): Boolean;
  public
    /// <summary>mod2Pix: the global module (grid) to image (pixel)
    /// transformation; dim: the size of the grid in modules.</summary>
    constructor Create(image: TBitMatrix; const mod2Pix: TPerspectiveTransformF;
      const dim: TPoint);

    /// <summary>Positions the grid at module p, adjusted to the image around
    /// p + offset (allows to work near the border of the symbol).</summary>
    function At(const p: TPoint; const offset: TPoint): TLocalGrid;
    function GetPos(const p: TPointD): TPointD; overload;
    function GetPos(const p: TPoint): TPointD; overload;

    /// <summary>
    /// Looks in a spiral around the grid center for a pattern of timing
    /// modules (alternating black and white, starting black at timingStart)
    /// in timingDirs, black modules from blackStart in blackDirs and white
    /// modules from whiteStart in whiteDirs, up to radius modules. Returns
    /// true and the (adjusted) pixel position of the pattern when found.
    /// </summary>
    function FindPattern(radius: Integer; const timingStart: TPoint;
      const timingDirs: TGridDirections; const blackStart: TPoint;
      const blackDirs: TGridDirections; const whiteStart: TPoint;
      const whiteDirs: TGridDirections; out position: TPointD): Boolean;
    /// <summary>
    /// Looks in a spiral around the grid center for a cross of timing
    /// patterns (like the reference grid of an Aztec Code) of radius modules;
    /// true and the (adjusted) pixel position of its center when found.
    /// </summary>
    function FindTimingPatternCross(isBlack: Boolean; radius: Integer;
      out position: TPointD): Boolean;
  end;

  /// <summary>A region of interest of the grid (x0 to x1 and y0 to y1,
  /// exclusive) with its own module to pixel transformation.</summary>
  TGridROI = record
    x0, x1, y0, y1: Integer;
    mod2Pix: TPerspectiveTransformF;
  end;

/// <summary>Samples the module centers of every region of interest; nil when
/// a transformation is invalid or a point lies outside the image.</summary>
function SampleGridROIs(image: TBitMatrix; width, height: Integer;
  const rois: TArray<TGridROI>; dx: Double = 0; dy: Double = 0): TBitMatrix;

/// <summary>
/// Samples a grid piecewise between alignment points: apP holds the pixel
/// position of the module at (apMX[x], apMY[y]) at index y * Length(apMX) + x,
/// and found[] whether it was located. Missing points are projected with
/// mod2Pix, which works if the symbol is flat. Returns nil when a point lies
/// outside the image.
/// </summary>
function SampleGridAligned(image: TBitMatrix; width, height: Integer;
  const mod2Pix: TPerspectiveTransformF; apP: TArray<TPointD>;
  const found: TArray<Boolean>; const apMX, apMY: TArray<Integer>;
  dx: Double = 0; dy: Double = 0): TBitMatrix;

implementation

uses
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults;

const
  VALUE_INVALID = -1;
  VALUE_WHITE = 0;
  VALUE_BLACK = 1;

var
  // the points of a spiral with radius 3 around (0, 0), see zxing-cpp's
  // Spiral; built once at startup
  Spiral3: TArray<TPoint>;

procedure BuildSpiral(radius: Integer);
begin
  Spiral3 := [Point(0, 0)];
  for var r := 1 to radius do
  begin
    for var k := 0 to 2 * r - 1 do // right -> down
      Spiral3 := Spiral3 + [Point(r, -(r - 1) + k)];
    for var k := 0 to 2 * r - 1 do // bottom -> left
      Spiral3 := Spiral3 + [Point((r - 1) - k, r)];
    for var k := 0 to 2 * r - 1 do // left -> up
      Spiral3 := Spiral3 + [Point(-r, (r - 1) - k)];
    for var k := 0 to 2 * r - 1 do // top -> right
      Spiral3 := Spiral3 + [Point(-(r - 1) + k, -r)];
  end;
end;

function ToDirections(const dirs: TGridDirections): TArray<TPoint>;
begin
  Result := nil;
  for var c in dirs do
    case c of
      'l':
        Result := Result + [Point(-1, 0)];
      'r':
        Result := Result + [Point(1, 0)];
      'u':
        Result := Result + [Point(0, -1)];
      'd':
        Result := Result + [Point(0, 1)];
    end;
end;

function PointOf(const p: TPoint): TPointD; inline;
begin
  Result := PointD(p.X, p.Y);
end;

function CenteredOf(x, y: Integer): TPointD; inline;
begin
  Result := PointD(x + 0.5, y + 0.5);
end;

/// <summary>std::round: halfway cases away from zero.</summary>
function RoundAway(d: Double): Double;
begin
  if (d >= 0) then
    Result := Floor(d + 0.5)
  else
    Result := -Floor(-d + 0.5);
end;

/// <summary>Steps from p in direction d (Bresenham) to the next pixel with
/// another value, or just outside the image, within range steps. Returns the
/// number of steps, or 0 when there is no edge within range
/// (BitMatrixCursorF::stepToEdge).</summary>
function StepToEdge(image: TBitMatrix; const p, d: TPointD;
  range: Integer): Integer;

  function valueAt(const q: TPointD): Integer;
  begin
    if IsInImage(image, q) then
      Result := Ord(BlackAtPoint(image, q))
    else
      Result := VALUE_INVALID;
  end;

begin
  var steps := 0;
  var lv := valueAt(p);
  var found := false;
  while (not found) and ((range = 0) or (steps < range)) and
    (lv <> VALUE_INVALID) do
  begin
    Inc(steps);
    var v := valueAt(p + steps * d);
    if (lv <> v) then
    begin
      lv := v;
      found := true;
    end;
  end;
  if found then
    Result := steps
  else
    Result := 0;
end;

/// <summary>The average of the largest cluster of values, where a cluster
/// holds values that are within threshold of each other.</summary>
function ClusterAvg(values: TArray<Double>; threshold: Double): Double;
begin
  TArray.Sort<Double>(values);
  var bestStart := 0;
  var bestLen := 0;
  var start := 0;
  for var fin := 0 to High(values) do
  begin
    while (start < fin) and (values[fin] - values[start] >= threshold) do
      Inc(start);
    if (fin - start + 1 > bestLen) then
    begin
      bestLen := fin - start + 1;
      bestStart := start;
    end;
  end;
  var sum: Double := 0;
  for var i := bestStart to bestStart + bestLen - 1 do
    sum := sum + values[i];
  Result := sum / bestLen;
end;

{ TLocalGrid }

constructor TLocalGrid.Create(image: TBitMatrix;
  const mod2Pix: TPerspectiveTransformF; const dim: TPoint);
begin
  inherited Create;
  FImage := image;
  FMod2Pix := mod2Pix;
  FDim := dim;
end;

function TLocalGrid.GetPos(const p: TPointD): TPointD;
begin
  Result := FOrigin + p.X * FStepX + p.Y * FStepY;
end;

function TLocalGrid.GetPos(const p: TPoint): TPointD;
begin
  Result := GetPos(PointOf(p));
end;

function TLocalGrid.Get(const p: TPointD): Integer;
begin
  var q := GetPos(p);
  if IsInImage(FImage, q) then
    Result := Ord(BlackAtPoint(FImage, q))
  else
    Result := VALUE_INVALID;
end;

function TLocalGrid.FindValue(const p, dir: TPoint; v: Integer): Boolean;
begin
  var d := PointD(IfThen(dir.X <> 0, 0.25, 0.0), IfThen(dir.Y <> 0, 0.25, 0.0));
  var pd := PointOf(p);
  Result := not IsInImage(FImage, GetPos(p)) or (Get(pd) = v) or
    (Get(pd + d) = v) or (Get(pd - d) = v);
end;

procedure TLocalGrid.AdjustOriginAndStep(var step: TPointD; radius: Integer;
  const offsets: TArray<TPointD>);
type
  TDistMod = record
    dist, modSize: Double;
  end;

  function nearestHalfResidual(n, s: Double): Double;
  begin
    Result := n - (RoundAway((n - s / 2) / s) * s + s / 2);
  end;

begin
  var modSize: Double := PointLength(step);
  var limit: Integer := Trunc(6 * modSize);
  var dir := BresenhamDirection(step);
  // module size in terms of steps in the given direction
  modSize := modSize / PointLength(dir);

  var distMod := TList<TDistMod>.Create;
  try
    for var r := 0 to radius do
      for var offset in offsets do
      begin
        var start := FOrigin + r * offset;
        var startC := Centered(start);
        var stepsPos := StepToEdge(FImage, startC, dir, limit);
        var stepsNeg := StepToEdge(FImage, startC, -dir, limit);
        // -0.5 because the center of the pixel is at .5, .5
        var distPos: Double := stepsPos - Dot(start - startC, dir) - 0.5;
        var distNeg: Double := stepsNeg - Dot(start - startC, -dir) - 0.5;
        var item: TDistMod;
        if (stepsPos <> 0) and (stepsNeg <> 0) then
        begin
          var blockSize := stepsPos + stepsNeg - 1;
          var localModSize: Double := blockSize /
            Max(1.0, RoundAway(blockSize / modSize));
          item.dist := distPos;
          if (blockSize / modSize < 5) then
            item.modSize := localModSize
          else
            item.modSize := 0;
          distMod.Add(item);
        end
        else if (stepsPos <> 0) then
        begin
          item.dist := distPos;
          item.modSize := 0;
          distMod.Add(item);
        end
        else if (stepsNeg <> 0) then
        begin
          item.dist := -distNeg;
          item.modSize := 0;
          distMod.Add(item);
        end;
        if (r = 0) then
          break; // only do the center point once
      end;

    // calculate the average local module size from the points where we found
    // one...
    var localModSize: Double := 0;
    var n := 0;
    for var item in distMod do
      if (item.modSize > 0) then
      begin
        localModSize := localModSize + item.modSize;
        Inc(n);
      end;
    if (n = 0) then
      exit;
    localModSize := localModSize / n;

    // ... and use it for the points where we didn't find one
    var d: TArray<Double>;
    SetLength(d, distMod.Count);
    for var i := 0 to distMod.Count - 1 do
    begin
      var item := distMod[i];
      if (item.modSize = 0) then
        item.modSize := localModSize;
      if (item.dist < 0) then
        d[i] := -nearestHalfResidual(-item.dist, item.modSize)
      else
        d[i] := nearestHalfResidual(item.dist, item.modSize);
    end;

    FOrigin := FOrigin + ClusterAvg(d, modSize / 2) * dir;

    // if we found local module sizes for each point (hopefully at the timing
    // pattern crosses), we update the step size
    if (n = distMod.Count) and (Abs(localModSize - modSize) > modSize * 0.1)
    then
      step := (localModSize / modSize) * step;
  finally
    distMod.Free;
  end;
end;

function TLocalGrid.At(const p: TPoint; const offset: TPoint): TLocalGrid;
begin
  FCenter := p;
  var c := CenteredOf(p.X, p.Y);
  FOrigin := FMod2Pix.Map(c);
  FStepX := FMod2Pix.Map(c + PointD(1, 0)) - FOrigin;
  FStepY := FMod2Pix.Map(c + PointD(0, 1)) - FOrigin;

  // works better for Data Matrix (especially near the symbol edges)
  var offsets: TArray<TPointD> := [-FStepX, -FStepY, FStepX, FStepY];

  // evaluate the image at origin + offset
  FOrigin := GetPos(offset);
  for var i := 0 to 1 do
  begin
    AdjustOriginAndStep(FStepX, 2, offsets);
    AdjustOriginAndStep(FStepY, 2, offsets);
  end;
  FOrigin := GetPos(Point(-offset.X, -offset.Y));
  Result := Self;
end;

function TLocalGrid.IsPatternAt(const p: TPoint; radius: Integer;
  const timingStart: TPoint; const timingDirs: TArray<TPoint>;
  const blackStart: TPoint; const blackDirs: TArray<TPoint>;
  const whiteStart: TPoint; const whiteDirs: TArray<TPoint>): Boolean;
begin
  for var r := 0 to radius do
  begin
    for var d in timingDirs do
      if not FindValue(Point(p.X + timingStart.X + r * d.X, p.Y + timingStart.Y
        + r * d.Y), d, Ord(Odd(r))) then
        exit(false);
    for var d in blackDirs do
      if not FindValue(Point(p.X + blackStart.X + r * d.X, p.Y + blackStart.Y +
        r * d.Y), d, VALUE_BLACK) then
        exit(false);
    for var d in whiteDirs do
      if not FindValue(Point(p.X + whiteStart.X + r * d.X, p.Y + whiteStart.Y +
        r * d.Y), d, VALUE_WHITE) then
        exit(false);
  end;
  Result := true;
end;

function TLocalGrid.FindPattern(radius: Integer; const timingStart: TPoint;
  const timingDirs: TGridDirections; const blackStart: TPoint;
  const blackDirs: TGridDirections; const whiteStart: TPoint;
  const whiteDirs: TGridDirections; out position: TPointD): Boolean;
begin
  var tDirs := ToDirections(timingDirs);
  var bDirs := ToDirections(blackDirs);
  var wDirs := ToDirections(whiteDirs);

  for var p in Spiral3 do
    if IsPatternAt(p, radius, timingStart, tDirs, blackStart, bDirs,
      whiteStart, wDirs) then
    begin
      var original := FOrigin;
      FOrigin := GetPos(p);
      // adjust origin and step with full radius and only in the direction of
      // the timing pattern
      var stepsX, stepsY: TArray<TPointD>;
      for var d in tDirs do
        if (d.Y = 0) then
          stepsX := stepsX + [d.X * FStepX]
        else
          stepsY := stepsY + [d.Y * FStepY];
      if (stepsX <> nil) then
        AdjustOriginAndStep(FStepX, radius, stepsX);
      if (stepsY <> nil) then
        AdjustOriginAndStep(FStepY, radius, stepsY);
      if ((stepsX <> nil) or (stepsY <> nil)) and
        not IsPatternAt(Point(0, 0), radius, timingStart, tDirs, blackStart,
        bDirs, whiteStart, wDirs) then
        // pattern lost after adjusting for the timing pattern, revert
        FOrigin := original;
      position := FOrigin;
      exit(true);
    end;
  Result := false;
end;

function TLocalGrid.IsTimingPatternCross(const p: TPoint; isBlack: Boolean;
  radius, errorThreshold: Integer): Boolean;

  // near the edge of the grid: continue on the other side of the center
  function wrapOffset(center, offset, dim: Integer): Integer;
  begin
    var pos := center + offset;
    if (pos < 0) then
      Result := radius - pos
    else if (pos >= dim) then
      Result := -(radius + (pos - dim) + 1)
    else
      Result := offset;
  end;

var
  errors: Integer;

  procedure check(x, y: Integer);
  begin
    x := wrapOffset(FCenter.X, x, FDim.X);
    y := wrapOffset(FCenter.Y, y, FDim.Y);
    var d := Point(x, y);
    // black on even distances when the center is black (like zxing-cpp,
    // also for negative distances)
    var black: Boolean;
    if isBlack then
      black := ((x + y) mod 2 = 0)
    else
      black := ((x + y) mod 2 = 1);
    if not FindValue(Point(p.X + d.X, p.Y + d.Y), d, Ord(black)) then
      Inc(errors);
  end;

begin
  errors := 0;
  for var r := 0 to radius do
  begin
    check(-r, 0);
    check(r, 0);
    check(0, -r);
    check(0, r);
    if (errors > errorThreshold) then
      exit(false);
  end;
  Result := (errors <= errorThreshold);
end;

function TLocalGrid.FindTimingPatternCross(isBlack: Boolean; radius: Integer;
  out position: TPointD): Boolean;
begin
  for var p in Spiral3 do
    // a cross candidate at p with half the radius
    if IsTimingPatternCross(p, isBlack, radius div 2) then
    begin
      var original := FOrigin;
      FOrigin := GetPos(p);
      // adjust origin and step with the full radius, only in the direction
      // of the timing patterns
      AdjustOriginAndStep(FStepX, radius, [-FStepX, FStepX]);
      AdjustOriginAndStep(FStepY, radius, [-FStepY, FStepY]);

      // check again with the full radius, aligned to the timing patterns
      if IsTimingPatternCross(Point(0, 0), isBlack, radius) then
      begin
        position := FOrigin;
        exit(true);
      end;
      FOrigin := original;
    end;
  Result := false;
end;

{ grid sampling }

function SampleGridROIs(image: TBitMatrix; width, height: Integer;
  const rois: TArray<TGridROI>; dx, dy: Double): TBitMatrix;

  function isInside(const mod2Pix: TPerspectiveTransformF; x, y: Integer)
    : Boolean;
  begin
    Result := IsInImage(image, mod2Pix.Map(CenteredOf(x, y)));
  end;

begin
  Result := nil;
  if (width <= 0) or (height <= 0) then
    exit;
  // precheck the corners of every roi to bail out early if the grid is
  // "obviously" not completely inside the image
  for var roi in rois do
    if not roi.mod2Pix.IsValid or not isInside(roi.mod2Pix, roi.x0, roi.y0) or
      not isInside(roi.mod2Pix, roi.x1 - 1, roi.y0) or
      not isInside(roi.mod2Pix, roi.x1 - 1, roi.y1 - 1) or
      not isInside(roi.mod2Pix, roi.x0, roi.y1 - 1) then
      exit;

  Result := TBitMatrix.Create(width, height);
  for var roi in rois do
    for var y := roi.y0 to roi.y1 - 1 do
      for var x := roi.x0 to roi.x1 - 1 do
      begin
        var q := roi.mod2Pix.Map(CenteredOf(x, y)) + PointD(dx, dy);
        // even when all corners are inside, an inner point can be outside due
        // to numerical instability (see zxing-cpp #563)
        if not IsInImage(image, q) then
        begin
          Result.Free;
          exit(nil);
        end;
        if BlackAtPoint(image, q) then
          Result[x, y] := true;
      end;
end;

function SampleGridAligned(image: TBitMatrix; width, height: Integer;
  const mod2Pix: TPerspectiveTransformF; apP: TArray<TPointD>;
  const found: TArray<Boolean>; const apMX, apMY: TArray<Integer>;
  dx, dy: Double): TBitMatrix;
begin
  var nx := System.Length(apMX);
  var w := nx - 1;
  var h := System.Length(apMY) - 1;

  // fill the alignment points that were not found by a projection based on
  // the global mod2Pix
  for var y := 0 to h do
    for var x := 0 to w do
      if not found[y * nx + x] then
        apP[y * nx + x] := mod2Pix.Map(CenteredOf(apMX[x], apMY[y]));

  // one region of interest between every 4 neighbouring alignment points
  var rois: TArray<TGridROI>;
  SetLength(rois, w * h);
  for var y := 0 to h - 1 do
    for var x := 0 to w - 1 do
    begin
      var x0 := apMX[x];
      var x1 := apMX[x + 1];
      var y0 := apMY[y];
      var y1 := apMY[y + 1];
      var roi: TGridROI;
      if (x = 0) then
        roi.x0 := 0
      else
        roi.x0 := x0;
      if (x = w - 1) then
        roi.x1 := width
      else
        roi.x1 := x1;
      if (y = 0) then
        roi.y0 := 0
      else
        roi.y0 := y0;
      if (y = h - 1) then
        roi.y1 := height
      else
        roi.y1 := y1;
      var src: TQuadrilateralF;
      src[0] := PointD(x0 + 0.5, y0 + 0.5);
      src[1] := PointD(x1 + 0.5, y0 + 0.5);
      src[2] := PointD(x1 + 0.5, y1 + 0.5);
      src[3] := PointD(x0 + 0.5, y1 + 0.5);
      var dst: TQuadrilateralF;
      dst[0] := apP[y * nx + x];
      dst[1] := apP[y * nx + x + 1];
      dst[2] := apP[(y + 1) * nx + x + 1];
      dst[3] := apP[(y + 1) * nx + x];
      roi.mod2Pix := TPerspectiveTransformF.Create(src, dst);
      rois[y * w + x] := roi;
    end;

  Result := SampleGridROIs(image, width, height, rois, dx, dy);
end;

initialization
  BuildSpiral(3);

end.
