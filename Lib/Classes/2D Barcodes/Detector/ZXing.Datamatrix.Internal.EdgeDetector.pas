unit ZXing.Datamatrix.Internal.EdgeDetector;

{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
  * Copyright 2017 Axel Waggershauser
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

  * Port of the 'new' Data Matrix detector of zxing-cpp (DMDetector.cpp,
  * DetectNew and Scan), by Axel Waggershauser. It works completely different
  * from the 'old' WhiteRectangle detector:
  *
  * - scan lines through the image; every black to white edge is a start point
  * - trace the solid 'L' (left and bottom border) pixel by pixel, then the
  *   dotted timing pattern at the top and right, jumping over white modules
  * - fit lines through the 4 traced borders and intersect them: the corners
  *   have sub-pixel precision, at every rotation and under perspective
  * - the dimension follows from the gaps along the whole timing border
  *
  * It works with about 2 pixels per module and with a quiet zone of only one
  * module. Not ported yet: LocalGrid (local refinement per data region) and
  * the timing pattern correction for curved symbols.
}

interface

uses
  System.SysUtils,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry,
  ZXing.ResultPoint;

type
  /// <summary>Gets every candidate grid: the sampled bits (freed by the
  /// detector after the call) and its corners top left, bottom left, bottom
  /// right, top right. Return true to stop the detection.</summary>
  TDataMatrixCandidate = reference to function(bits: TBitMatrix;
    const points: TArray<IResultPoint>): Boolean;

/// <summary>
/// Looks for Data Matrix symbols by tracing edges. Without tryHarder only the
/// center lines are scanned, with it parallel lines every 16 pixels. Without
/// tryRotate only the scan direction to the left is used.
/// Returns true when onCandidate stopped the detection.
/// </summary>
function DetectDataMatrixByEdges(image: TBitMatrix; tryHarder, tryRotate: Boolean;
  const onCandidate: TDataMatrixCandidate): Boolean;

/// <summary>Detects a code in a "pure" image: an unrotated, unskewed code with
/// only a white border around it (zxing-cpp's DetectPure). Also handles codes
/// of 1 pixel per module, which the edge tracer can not find. Returns the
/// sampled bits (to be freed by the caller), or nil when the image is not
/// pure.</summary>
function DetectDataMatrixPure(image: TBitMatrix;
  out points: TArray<IResultPoint>): TBitMatrix;

implementation

uses
  System.Math,
  System.Generics.Collections,
  ZXing.Datamatrix.Internal.Version;

type
  /// <summary>Remembers where a trace already passed by, so a later trace does
  /// not do the same work twice (tryHarder only).</summary>
  THistory = class
  private
    FWidth: Integer;
    FStates: TArray<ShortInt>;
  public
    constructor Create(width, height: Integer);
    procedure Clear;
    function Get(x, y: Integer): ShortInt; inline;
    procedure SetState(x, y: Integer; state: ShortInt); inline;
  end;

  TDMRegressionLine = class(TRegressionLine)
  public
    procedure Reverse;
    /// <summary>Number of 2 module steps (black + white) between beg and
    /// fin, from the gaps between the traced points.</summary>
    function Modules(const beg, fin: TPointD): Double;
    function TruncateIfLShape: Boolean;
  end;

  TStepResult = (srFound, srOpenEnd, srClosedEnd);

  /// <summary>Cursor that walks along black/white edges (BitMatrixCursorF +
  /// EdgeTracer of zxing-cpp). A record, so assigning makes a copy.</summary>
  TEdgeTracer = record
  public
    img: TBitMatrix;
    p: TPointD; // current position
    d: TPointD; // current direction
    history: THistory;
    state: ShortInt;

    class function Create(image: TBitMatrix; const pos, dir: TPointD)
      : TEdgeTracer; static;
    procedure SetDirection(const dir: TPointD); inline;
    function IsInAt(const q: TPointD): Boolean; inline;
    function IsIn: Boolean; inline;
    function BlackAt(const q: TPointD): Boolean; inline;
    function WhiteAt(const q: TPointD): Boolean; inline;
    function IsWhite: Boolean; inline;
    function Front: TPointD; inline;
    function Back: TPointD; inline;
    function Left: TPointD; inline;
    function Right: TPointD; inline;
    procedure TurnRight; inline;
    function Step(s: Double = 1): Boolean; inline;

    function TraceStep(dEdge: TPointD; maxStepSize: Integer;
      goodDirection: Boolean): TStepResult;
    function UpdateDirectionFromOrigin(const origin: TPointD): Boolean;
    function UpdateDirectionFromLine(line: TRegressionLine): Boolean;
    function UpdateDirectionFromLineCentroid(line: TRegressionLine): Boolean;
    function TraceLine(const dEdge: TPointD; line: TRegressionLine): Boolean;
    function TraceGaps(const dEdge: TPointD; line: TRegressionLine;
      maxStepSize: Integer; finishLine: TRegressionLine;
      minDist: Double): Boolean;
    function TraceCorner(dir: TPointD; out corner: TPointD): Boolean;
    function MoveToNextWhiteAfterBlack: Boolean;
  end;

  TLines = array [0 .. 3] of TDMRegressionLine;

{ THistory }

constructor THistory.Create(width, height: Integer);
begin
  inherited Create;
  FWidth := width;
  SetLength(FStates, width * height);
end;

procedure THistory.Clear;
begin
  if (Length(FStates) > 0) then
    FillChar(FStates[0], Length(FStates), 0);
end;

function THistory.Get(x, y: Integer): ShortInt;
begin
  Result := FStates[y * FWidth + x];
end;

procedure THistory.SetState(x, y: Integer; state: ShortInt);
begin
  FStates[y * FWidth + x] := state;
end;

{ TDMRegressionLine }

procedure TDMRegressionLine.Reverse;
var
  i: Integer;
  t: TPointD;
begin
  for i := 0 to FCount div 2 - 1 do
  begin
    t := FPoints[i];
    FPoints[i] := FPoints[FCount - 1 - i];
    FPoints[FCount - 1 - i] := t;
  end;
end;

function AverageOfPositive(const values: TArray<Double>): Double;
var
  v, sum: Double;
  num: Integer;
begin
  sum := 0;
  num := 0;
  for v in values do
    if (v > 0) then
    begin
      sum := sum + v;
      Inc(num);
    end;
  Result := sum / num;
end;

function TDMRegressionLine.Modules(const beg, fin: TPointD): Double;
var
  gapSizes, modSizes: TArray<Double>;
  i, n, numMod: Integer;
  unitPixelDist, sumFront, sumBack, dist, lineLength, meanModSize, minV,
    maxV: Double;

  procedure addModSize(value: Double);
  begin
    if (numMod = System.Length(modSizes)) then
      SetLength(modSizes, numMod * 2 + 8);
    modSizes[numMod] := value;
    Inc(numMod);
  end;

begin
  // re-evaluate and filter out all points too far away. required for the
  // gapSizes calculation.
  Evaluate(1.2, true);

  // the distance between the points projected onto the regression line
  n := FCount;
  SetLength(gapSizes, Max(n - 1, 0));
  for i := 1 to n - 1 do
    gapSizes[i - 1] := PointDistance(Project(FPoints[i]), Project(FPoints[i - 1]));

  // the (expected average) distance of two adjacent pixels
  unitPixelDist := PointLength(BresenhamDirection(Back - Front));

  // the width of 2 modules (first black pixel to first black pixel)
  sumFront := PointDistance(beg, Project(Front)) - unitPixelDist;
  sumBack := 0; // (last black pixel to last black pixel)
  numMod := 0;
  for dist in gapSizes do
  begin
    if (dist > 1.9 * unitPixelDist) then
    begin
      addModSize(sumBack);
      sumBack := 0;
    end;
    sumFront := sumFront + dist;
    sumBack := sumBack + dist;
    if (dist > 1.9 * unitPixelDist) then
    begin
      addModSize(sumFront);
      sumFront := 0;
    end;
  end;
  if (numMod = 0) then
    exit(0);
  addModSize(sumFront + PointDistance(fin, Project(Back)));
  SetLength(modSizes, numMod);
  modSizes[0] := 0; // the first element is an invalid sumBack value
  lineLength := PointDistance(beg, fin) - unitPixelDist;

  minV := modSizes[1];
  maxV := modSizes[1];
  for i := 2 to numMod - 1 do
  begin
    minV := Min(minV, modSizes[i]);
    maxV := Max(maxV, modSizes[i]);
  end;
  meanModSize := AverageOfPositive(modSizes);

  if (maxV > 2 * minV) then
  begin
    for i := 1 to numMod - 3 do
    begin
      if (modSizes[i] > 0) and (modSizes[i] + modSizes[i + 2] < meanModSize * 1.4)
      then
      begin
        modSizes[i] := modSizes[i] + modSizes[i + 2];
        modSizes[i + 2] := 0;
      end
      else if (modSizes[i] > meanModSize * 1.6) then
        modSizes[i] := 0;
    end;
    meanModSize := AverageOfPositive(modSizes);
  end;

  Result := lineLength / meanModSize;
end;

function TDMRegressionLine.TruncateIfLShape: Boolean;
var
  lineAB: TRegressionLine;
  i, maxIndex: Integer;
  maxD, dist, lenL, lenB: Double;
begin
  Result := false;
  if (FCount < 16) then
    exit;
  lineAB := TRegressionLine.Create(Front, Back);
  try
    if (lineAB.Distance(FPoints[FCount div 2]) < 5) then
      exit;

    maxIndex := 0;
    maxD := 0.0;
    for i := 0 to FCount - 1 do
    begin
      dist := lineAB.Distance(FPoints[i]);
      if (dist > maxD) then
      begin
        maxIndex := i;
        maxD := dist;
      end;
    end;
  finally
    lineAB.Free;
  end;

  lenL := PointDistance(Front, FPoints[maxIndex]) - 1;
  lenB := PointDistance(FPoints[maxIndex], Back) - 1;
  if (maxD < Min(lenL, lenB) / 2) then
    exit;

  SetDirectionInward(Back - FPoints[maxIndex]);
  FCount := Max(maxIndex - 1, 0);
  Result := true;
end;

{ TEdgeTracer }

class function TEdgeTracer.Create(image: TBitMatrix; const pos, dir: TPointD)
  : TEdgeTracer;
begin
  Result.img := image;
  Result.p := pos;
  Result.SetDirection(dir);
  Result.history := nil;
  Result.state := 0;
end;

procedure TEdgeTracer.SetDirection(const dir: TPointD);
begin
  d := BresenhamDirection(dir);
end;

function TEdgeTracer.IsInAt(const q: TPointD): Boolean;
begin
  Result := IsInImage(img, q);
end;

function TEdgeTracer.IsIn: Boolean;
begin
  Result := IsInImage(img, p);
end;

function TEdgeTracer.BlackAt(const q: TPointD): Boolean;
begin
  Result := IsInImage(img, q) and BlackAtPoint(img, q);
end;

function TEdgeTracer.WhiteAt(const q: TPointD): Boolean;
begin
  Result := IsInImage(img, q) and not BlackAtPoint(img, q);
end;

function TEdgeTracer.IsWhite: Boolean;
begin
  Result := WhiteAt(p);
end;

function TEdgeTracer.Front: TPointD;
begin
  Result := d;
end;

function TEdgeTracer.Back: TPointD;
begin
  Result := -d;
end;

function TEdgeTracer.Left: TPointD;
begin
  Result := LeftOf(d);
end;

function TEdgeTracer.Right: TPointD;
begin
  Result := RightOf(d);
end;

procedure TEdgeTracer.TurnRight;
begin
  d := Right;
end;

function TEdgeTracer.Step(s: Double): Boolean;
begin
  p := p + s * d;
  Result := IsIn;
end;

function TEdgeTracer.TraceStep(dEdge: TPointD; maxStepSize: Integer;
  goodDirection: Boolean): TStepResult;
var
  breadth, maxBreadth, stepNr, i, j, offset: Integer;
  pEdge: TPointD;
begin
  dEdge := MainDirection(dEdge);
  if (maxStepSize = 1) then
    maxBreadth := 2
  else if goodDirection then
    maxBreadth := 1
  else
    maxBreadth := 3;

  for breadth := 1 to maxBreadth do
    for stepNr := 1 to maxStepSize do
      for i := 0 to 2 * (stepNr div 4 + 1) * breadth do
      begin
        if Odd(i) then
          offset := (i + 1) div 2
        else
          offset := -(i div 2);
        pEdge := p + stepNr * d + offset * dEdge;

        if not BlackAt(pEdge + dEdge) then
          continue;

        // found black pixel -> go 'outward' until we hit the b/w border
        j := 0;
        while (j < Max(maxStepSize, 3)) and IsInAt(pEdge) do
        begin
          if WhiteAt(pEdge) then
          begin
            p := Centered(pEdge);

            if (history <> nil) and (maxStepSize = 1) then
            begin
              if (history.Get(PixelX(p), PixelY(p)) = state) then
                exit(srClosedEnd);
              history.SetState(PixelX(p), PixelY(p), state);
            end;

            exit(srFound);
          end;
          pEdge := pEdge - dEdge;
          if BlackAt(pEdge - d) then
            pEdge := pEdge - d;
          Inc(j);
        end;
        // no valid b/w border found within reasonable range
        exit(srClosedEnd);
      end;
  Result := srOpenEnd;
end;

function TEdgeTracer.UpdateDirectionFromOrigin(const origin: TPointD): Boolean;
var
  oldD: TPointD;
begin
  oldD := d;
  SetDirection(p - origin);
  // if the new direction is pointing "backward", i.e. angle(new, old) > 90
  // deg -> break
  if (Dot(d, oldD) < 0) then
    exit(false);
  // make sure d stays in the same quadrant to prevent an infinite loop
  if (Abs(d.X) = Abs(d.Y)) then
    d := MainDirection(oldD) + 0.99 * (d - MainDirection(oldD))
  else if (MainDirection(d) <> MainDirection(oldD)) then
    d := MainDirection(oldD) + 0.99 * MainDirection(d);
  Result := true;
end;

function TEdgeTracer.UpdateDirectionFromLine(line: TRegressionLine): Boolean;
begin
  Result := line.Evaluate(1.5) and
    UpdateDirectionFromOrigin(p - line.Project(p) + line.Front);
end;

function TEdgeTracer.UpdateDirectionFromLineCentroid
  (line: TRegressionLine): Boolean;
begin
  // a faster, less accurate version of the above without the line evaluation
  Result := UpdateDirectionFromOrigin(line.Centroid);
end;

function TEdgeTracer.TraceLine(const dEdge: TPointD;
  line: TRegressionLine): Boolean;
var
  stepResult: TStepResult;
begin
  line.SetDirectionInward(dEdge);
  repeat
    line.Add(p);
    if (line.Count mod 50 = 10) and not UpdateDirectionFromLineCentroid(line)
    then
      exit(false);
    stepResult := TraceStep(dEdge, 1, line.IsValid);
    if (stepResult <> srFound) then
      exit((stepResult = srOpenEnd) and (line.Count > 1) and
        UpdateDirectionFromLineCentroid(line));
  until false;
end;

function TEdgeTracer.TraceGaps(const dEdge: TPointD; line: TRegressionLine;
  maxStepSize: Integer; finishLine: TRegressionLine; minDist: Double): Boolean;
var
  gaps, steps, maxStepsPerGap: Integer;
  lastP, np, curStep: TPointD;
  stepLengthInMainDir: Double;
  stepResult: TStepResult;
  finishValid, samePosition: Boolean;
begin
  line.SetDirectionInward(dEdge);
  gaps := 0;
  steps := 0;
  maxStepsPerGap := maxStepSize;
  lastP := PointD(0, 0);
  finishValid := (finishLine <> nil) and finishLine.IsValid;
  repeat
    // detect an endless loop (lack of progress)
    samePosition := (p = lastP);
    lastP := p;
    if samePosition then
      exit(false);
    if (gaps = 0) then
    begin
      if (steps > 2 * maxStepsPerGap) then
        exit(false);
    end
    else if (steps > (gaps + 1) * maxStepsPerGap) then
      exit(false);
    Inc(steps);

    // if we drifted too far outside of the code, break
    if line.IsValid and (line.SignedDistance(p) < -5) and
      (not line.Evaluate or (line.SignedDistance(p) < -5)) then
      exit(false);

    // if we are drifting towards the inside of the code, pull the current
    // position back out onto the line
    if line.IsValid and (line.SignedDistance(p) > 3) then
    begin
      // The current direction d and the line we are tracing are supposed to
      // be roughly parallel. Break if the angle between them is greater than
      // 45 deg.
      if (Abs(Dot(Normalized(d), line.Normal)) > 0.7) then
        exit(false);

      // re-evaluate line with all the points up to here before projecting
      if not line.Evaluate(1.5) then
        exit(false);

      np := line.Project(p);
      // make sure we are making progress even when back-projecting
      while (PointDistance(np, line.Project(line.Back)) < 1) do
        np := np + d;
      p := Centered(np);
    end
    else
    begin
      if (line.Count = 0) then
      begin
        curStep := PointD(0, 0);
        stepLengthInMainDir := 0;
      end
      else
      begin
        curStep := p - line.Back;
        stepLengthInMainDir := Dot(MainDirection(d), curStep);
      end;
      line.Add(p);

      if (stepLengthInMainDir > 1) or (MaxAbsComponent(curStep) >= 2) then
      begin
        Inc(gaps);
        if (gaps >= 2) or (line.Count > 5) then
        begin
          if not UpdateDirectionFromLine(line) then
            exit(false);
          // check if the first half of the top-line trace is complete.
          // the minimum code size is 10x10 -> every code has at least 4 gaps
          if (minDist <> 0) and (gaps >= 4) and
            (PointDistance(p, line.Front) > minDist) then
          begin
            // undo the last insert, it will be inserted again after the
            // restart
            line.PopBack;
            exit(true);
          end;
        end;
      end
      else if (gaps = 0) and (line.Count >= 2 * maxStepSize) then
        exit(false); // no point in following a line that has no gaps
    end;

    if finishValid then
      maxStepSize := Min(maxStepSize, Trunc(finishLine.SignedDistance(p)));

    stepResult := TraceStep(dEdge, maxStepSize, line.IsValid);

    if (stepResult <> srFound) then
      // we are successful iff we found an open end across a valid finishLine
      exit((stepResult = srOpenEnd) and finishValid and
        (Trunc(finishLine.SignedDistance(p)) <= maxStepSize + 1));
  until false;
end;

function TEdgeTracer.TraceCorner(dir: TPointD; out corner: TPointD): Boolean;
var
  t: TPointD;
begin
  Step;
  corner := p;
  t := d;
  d := dir;
  dir := t;
  TraceStep(-1 * dir, 2, false);
  Result := IsInAt(corner) and IsIn;
end;

function TEdgeTracer.MoveToNextWhiteAfterBlack: Boolean;
var
  px, py, dx, dy, stepsToBorder, maxStepsX, maxStepsY, steps: Integer;

  // FastEdgeToEdgeCounter of zxing-cpp: steps to the next pixel with another
  // value, or just outside the image when there is none
  function stepToNextEdge: Integer;
  var
    n: Integer;
    v: Boolean;
  begin
    if (stepsToBorder < 0) then
      exit(0);
    v := img[px, py];
    n := 0;
    repeat
      Inc(n);
      if (n > stepsToBorder) then
        break;
    until img[px + n * dx, py + n * dy] <> v;
    px := px + n * dx;
    py := py + n * dy;
    stepsToBorder := stepsToBorder - n;
    Result := n;
  end;

begin
  px := PixelX(p);
  py := PixelY(p);
  dx := Round(d.X);
  dy := Round(d.Y);
  if (dx = 0) then
    maxStepsX := MaxInt
  else if (dx > 0) then
    maxStepsX := img.Width - 1 - px
  else
    maxStepsX := px;
  if (dy = 0) then
    maxStepsY := MaxInt
  else if (dy > 0) then
    maxStepsY := img.Height - 1 - py
  else
    maxStepsY := py;
  stepsToBorder := Min(maxStepsX, maxStepsY);

  steps := stepToNextEdge;
  if (steps = 0) then
    exit(false);
  Step(steps);
  if IsWhite then
    exit(true);

  steps := stepToNextEdge;
  if (steps = 0) then
    exit(false);
  Result := Step(steps);
end;

{ Scan }

function MovedTowardsBy(const a, b1, b2: TPointD; dist: Double): TPointD;
begin
  Result := a + dist * Normalized(Normalized(b1 - a) + Normalized(b2 - a));
end;

procedure SplitDouble(d: Double; out i: Integer; out f: Double);
begin
  // like std::isnormal: not zero, subnormal, infinite or NaN
  if IsNan(d) or IsInfinite(d) or (Abs(d) < MinDouble) then
  begin
    i := 0;
    f := Infinity;
  end
  else
  begin
    // std::lround: halfway cases away from zero
    if (d >= 0) then
      i := Trunc(d + 0.5)
    else
      i := -Trunc(-d + 0.5);
    f := Abs(d - i);
  end;
end;

/// <summary>Samples the module centers; nil when the grid does not lie
/// completely in the image.</summary>
function SampleGrid(image: TBitMatrix; width, height: Integer;
  const mod2Pix: TPerspectiveTransformF): TBitMatrix;

  function isInside(x, y: Integer): Boolean;
  begin
    Result := IsInImage(image, mod2Pix.Map(PointD(x + 0.5, y + 0.5)));
  end;

var
  x, y: Integer;
  q: TPointD;
begin
  Result := nil;
  if (width <= 0) or (height <= 0) or not mod2Pix.IsValid then
    exit;
  // bail out early if the grid is obviously not completely inside the image
  if not isInside(0, 0) or not isInside(width - 1, 0) or
    not isInside(width - 1, height - 1) or not isInside(0, height - 1) then
    exit;

  Result := TBitMatrix.Create(width, height);
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
    begin
      q := mod2Pix.Map(PointD(x + 0.5, y + 0.5));
      // even when all corners are inside, an inner point can be outside due
      // to numerical instability (see zxing-cpp #563)
      if not IsInImage(image, q) then
      begin
        FreeAndNil(Result);
        exit;
      end;
      if BlackAtPoint(image, q) then
        Result[x, y] := true;
    end;
end;

function CornerPoint(const mod2Pix: TPerspectiveTransformF; x, y: Integer)
  : IResultPoint;
var
  q: TPointD;
begin
  q := mod2Pix.Map(PointD(x, y));
  Result := TResultPointHelpers.CreateResultPoint(Trunc(q.X + 0.5),
    Trunc(q.Y + 0.5));
end;

/// <summary>Follows the start tracer to every next black to white edge and
/// tries to trace a Data Matrix symbol from there. Returns true when
/// onCandidate stopped the detection.</summary>
function Scan(var startTracer: TEdgeTracer; const lines: TLines;
  const onCandidate: TDataMatrixCandidate): Boolean;
var
  t, tlTracer: TEdgeTracer;
  lineL, lineB, lineR, lineT: TDMRegressionLine;
  l: TDMRegressionLine;
  tl, bl, br, tr, up, right: TPointD;
  lenL, lenB, lenT, lenR, fracT, fracR: Double;
  maxStepSize, dimT, dimR: Integer;
  version, versionT, versionR: TVersion;
  sourcePoints: TQuadrilateralF;
  mod2Pix: TPerspectiveTransformF;
  bits: TBitMatrix;
  points: TArray<IResultPoint>;
begin
  Result := false;
  lineL := lines[0];
  lineB := lines[1];
  lineR := lines[2];
  lineT := lines[3];

  while startTracer.MoveToNextWhiteAfterBlack do
  begin
    for l in lines do
      l.Reset;

    t := startTracer;

    // follow left leg upwards
    t.TurnRight;
    t.state := 1;
    if not t.TraceLine(t.Right, lineL) then
      continue;
    if not t.TraceCorner(t.Right, tl) then
      continue;
    lineL.Reverse;
    tlTracer := t;

    // follow left leg downwards
    t := startTracer;
    t.state := 1;
    t.SetDirection(tlTracer.Right);
    if not t.TraceLine(t.Left, lineL) then
      continue;

    // check if lineL is L-shaped -> truncate the lower leg and set t to just
    // before the corner
    if lineL.TruncateIfLShape then
      t.p := lineL.Back;
    t.UpdateDirectionFromOrigin(tl);
    up := t.Back;
    if not t.TraceCorner(t.Left, bl) then
      continue;

    // follow bottom leg right
    t.state := 2;
    if not t.TraceLine(t.Left, lineB) then
      continue;
    t.UpdateDirectionFromOrigin(bl);
    right := t.Front;
    if not t.TraceCorner(t.Left, br) then
      continue;

    lenL := PointDistance(tl, bl) - 1;
    lenB := PointDistance(bl, br) - 1;
    if not ((lenL >= 8) and (lenB >= 10) and (lenB >= lenL / 4) and
      (lenB <= lenL * 18)) then
      continue;

    // datamatrix bottom dim is at least 10
    maxStepSize := Trunc(lenB / 5 + 1);

    // at this point we found a plausible L-shape and are now looking for the
    // b/w pattern at the top and right: follow top row right 'half way' (at
    // least 4 gaps), see TraceGaps
    tlTracer.SetDirection(right);
    if not tlTracer.TraceGaps(tlTracer.Right, lineT, maxStepSize, nil, lenB / 2)
    then
      continue;

    maxStepSize := Min(lineT.Length div 3, Trunc(lenL / 5)) * 2;

    // follow up until we reach the top line
    t.SetDirection(up);
    t.state := 3;
    if not t.TraceGaps(t.Left, lineR, maxStepSize, lineT, 0) then
      continue;
    if not t.TraceCorner(t.Left, tr) then
      continue;

    lenT := PointDistance(tl, tr) - 1;
    lenR := PointDistance(tr, br) - 1;

    if not ((Abs(lenT - lenB) / lenB < 0.5) and (Abs(lenR - lenL) / lenL < 0.5)
      and (lineT.Count >= 5) and (lineR.Count >= 5)) then
      continue;

    // continue top row right until we cross the right line
    if not tlTracer.TraceGaps(tlTracer.Right, lineT, maxStepSize, lineR, 0) then
      continue;

    for l in lines do
      l.Evaluate(1.0);

    // find the bounding box corners of the code with sub-pixel precision by
    // intersecting the 4 border lines
    bl := Intersect(lineB, lineL);
    tl := Intersect(lineT, lineL);
    tr := Intersect(lineT, lineR);
    br := Intersect(lineB, lineR);

    SplitDouble(lineT.Modules(tl, tr), dimT, fracT);
    SplitDouble(lineR.Modules(br, tr), dimR, fracR);

    // the dimension is 2x the number of black/white transitions
    dimT := dimT * 2;
    dimR := dimR * 2;

    version := TVersion.getVersionForDimensions(dimR, dimT);

    // if we have an invalid dimension but it is almost square (all valid
    // rectangular symbols differ in their dimension by at least 10), we try
    // to parse it by assuming a square. If only one leads to a valid version,
    // we use that, otherwise the dimension that is closer to an integral value.
    if (version = nil) and (Abs(dimT - dimR) < 10) then
    begin
      versionT := TVersion.getVersionForDimensions(dimT, dimT);
      versionR := TVersion.getVersionForDimensions(dimR, dimR);
      if ((versionT <> nil) xor (versionR <> nil)) then
      begin
        if (versionT <> nil) then
          dimR := dimT
        else
          dimT := dimR;
      end
      else if (fracR < fracT) then
        dimT := dimR
      else
        dimR := dimT;
      version := TVersion.getVersionForDimensions(dimR, dimT);
    end;
    if (version = nil) then
      continue;

    // shrink shape by half a pixel to go from center of white pixel outside
    // of code to the edge between white and black
    sourcePoints[0] := MovedTowardsBy(tl, tr, bl, 0.5);
    // move the tr point a little less because the jagged top and right line
    // tend to be statistically slightly inclined toward the center anyway.
    sourcePoints[1] := MovedTowardsBy(tr, br, tl, 0.3);
    sourcePoints[2] := MovedTowardsBy(br, bl, tr, 0.5);
    sourcePoints[3] := MovedTowardsBy(bl, tl, br, 0.5);

    mod2Pix := TPerspectiveTransformF.Create(RectangleF(dimT, dimR, 0),
      sourcePoints);

    bits := SampleGrid(startTracer.img, dimT, dimR, mod2Pix);
    if (bits = nil) then
      continue;
    try
      points := TArray<IResultPoint>.Create(CornerPoint(mod2Pix, 0, 0),
        CornerPoint(mod2Pix, 0, dimR), CornerPoint(mod2Pix, dimT, dimR),
        CornerPoint(mod2Pix, dimT, 0));
      if onCandidate(bits, points) then
        exit(true);
    finally
      bits.Free;
    end;
  end;
end;

/// <summary>The smallest rectangle containing all black pixels, as in
/// zxing-cpp's BitMatrix::findBoundingBox.</summary>
function FindBoundingBox(image: TBitMatrix; out left, top, width,
  height: Integer; minSize: Integer): Boolean;
var
  topLeft, bottomRight: TArray<Integer>;
  x, y, right, bottom: Integer;
begin
  Result := false;
  topLeft := image.getTopLeftOnBit;
  bottomRight := image.getBottomRightOnBit;
  if (topLeft = nil) or (bottomRight = nil) then
    exit;
  left := topLeft[0];
  top := topLeft[1];
  right := bottomRight[0];
  bottom := bottomRight[1];
  if (bottom - top + 1 < minSize) then
    exit;

  for y := top to bottom do
  begin
    for x := 0 to left - 1 do
      if image[x, y] then
      begin
        left := x;
        break;
      end;
    for x := image.Width - 1 downto right + 1 do
      if image[x, y] then
      begin
        right := x;
        break;
      end;
  end;

  width := right - left + 1;
  height := bottom - top + 1;
  Result := (width >= minSize) and (height >= minSize);
end;

/// <summary>Counts the color changes when stepping at most range pixels from
/// (x, y) in direction (dx, dy), and moves (x, y) along (BitMatrixCursor's
/// countEdges).</summary>
function CountEdges(image: TBitMatrix; var x, y: Integer; dx, dy,
  range: Integer): Integer;

  function valueAt(px, py: Integer): Integer;
  begin
    if (px < 0) or (px >= image.Width) or (py < 0) or (py >= image.Height) then
      Result := -1
    else
      Result := Ord(image[px, py]);
  end;

var
  steps, lv, v: Integer;
  found: Boolean;
begin
  Result := 0;
  while (range > 0) do
  begin
    // step to the next edge, within range
    steps := 0;
    found := false;
    lv := valueAt(x, y);
    while (not found) and (steps < range) and (lv <> -1) do
    begin
      Inc(steps);
      v := valueAt(x + steps * dx, y + steps * dy);
      if (v <> lv) then
      begin
        lv := v;
        found := true;
      end;
    end;
    Inc(x, steps * dx);
    Inc(y, steps * dy);
    if not found then
      break;
    Dec(range, steps);
    Inc(Result);
  end;
end;

function DetectDataMatrixPure(image: TBitMatrix; out points: TArray<IResultPoint>)
  : TBitMatrix;
var
  left, top, width, height, x, y, dimR, dimT: Integer;
  modSizeX, modSizeY, modSize, px, py: Double;
begin
  Result := nil;
  if not FindBoundingBox(image, left, top, width, height, 8) then
    exit;

  // walk around the code counter-clockwise from the top left: the left and
  // bottom side are solid, the right and top side have the timing pattern
  x := left;
  y := top;
  if (CountEdges(image, x, y, 0, 1, height - 1) <> 0) then
    exit;
  if (CountEdges(image, x, y, 1, 0, width - 1) <> 0) then
    exit;
  dimR := CountEdges(image, x, y, 0, -1, height - 1) + 1;
  dimT := CountEdges(image, x, y, -1, 0, width - 1) + 1;

  modSizeX := width / dimT;
  modSizeY := height / dimR;
  modSize := (modSizeX + modSizeY) / 2;

  px := left + modSizeX / 2 + (dimT - 1) * modSize;
  py := top + modSizeY / 2 + (dimR - 1) * modSize;
  if Odd(dimT) or Odd(dimR) or (dimT < 10) or (dimT > 144) or (dimR < 8) or
    (dimR > 144) or (Abs(modSizeX - modSizeY) > 1) or
    not IsInImage(image, PointD(px, py)) then
    exit;

  // now just read off the bits (this is a crop + subsample)
  Result := TBitMatrix.Create(dimT, dimR);
  for y := 0 to dimR - 1 do
  begin
    py := top + modSizeY / 2 + y * modSize;
    for x := 0 to dimT - 1 do
      if image[Trunc(left + modSizeX / 2 + x * modSize), Trunc(py)] then
        Result[x, y] := true;
  end;

  points := TArray<IResultPoint>.Create(
    TResultPointHelpers.CreateResultPoint(left, top),
    TResultPointHelpers.CreateResultPoint(left, top + height),
    TResultPointHelpers.CreateResultPoint(left + width, top + height),
    TResultPointHelpers.CreateResultPoint(left + width, top));
end;

function DetectDataMatrixByEdges(image: TBitMatrix; tryHarder, tryRotate: Boolean;
  const onCandidate: TDataMatrixCandidate): Boolean;
const
  // minimum realistic size in pixel: 8 modules x 2 pixels per module
  MIN_SYMBOL_SIZE = 8 * 2;
  DIRECTIONS: array [0 .. 3, 0 .. 1] of Integer = ((-1, 0), (1, 0), (0, -1),
    (0, 1));
var
  history: THistory;
  lines: TLines;
  k, i, cx, cy, offset: Integer;
  dir, startPos: TPointD;
  tracer: TEdgeTracer;
  floatMask: TFloatExceptionsMasked; // masked until this function returns
  pureBits: TBitMatrix;
  purePoints: TArray<IResultPoint>;
begin
  Result := false;
  if (image = nil) then
    exit;

  // first the very fast pure path, also because the edge tracing generally
  // fails on pure symbols with a module size of 1 pixel
  pureBits := DetectDataMatrixPure(image, purePoints);
  if (pureBits <> nil) then
    try
      if onCandidate(pureBits, purePoints) then
        exit(true);
    finally
      pureBits.Free;
    end;

  history := nil;
  for k := 0 to 3 do
    lines[k] := nil;
  try
    // a history to remember where the tracing already passed by, to prevent
    // a later trace from doing the same work twice
    if tryHarder then
      history := THistory.Create(image.Width, image.Height);
    for k := 0 to 3 do
      lines[k] := TDMRegressionLine.Create;

    for k := 0 to 3 do
    begin
      dir := PointD(DIRECTIONS[k, 0], DIRECTIONS[k, 1]);
      cx := image.Width div 2;
      cy := image.Height div 2;
      // start at the image border opposite to dir, a bit inside
      startPos := Centered(PointD(cx - cx * DIRECTIONS[k, 0] + (MIN_SYMBOL_SIZE div
        2) * DIRECTIONS[k, 0], cy - cy * DIRECTIONS[k, 1] + (MIN_SYMBOL_SIZE div
        2) * DIRECTIONS[k, 1]));

      if (history <> nil) then
        history.Clear;

      i := 1;
      while true do
      begin
        tracer := TEdgeTracer.Create(image, startPos, dir);
        // alternate lines left and right of the center line
        offset := (i div 2) * MIN_SYMBOL_SIZE;
        if Odd(i) then
          offset := -offset;
        tracer.p := tracer.p + offset * tracer.Right;
        if tryHarder then
          tracer.history := history;

        if not tracer.IsIn then
          break;

        if Scan(tracer, lines, onCandidate) then
          exit(true);

        if not tryHarder then
          break; // only test center lines
        Inc(i);
      end;

      if not tryRotate then
        break; // only test left direction
    end;
  finally
    for k := 0 to 3 do
      lines[k].Free;
    history.Free;
  end;
end;

end.
