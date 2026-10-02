unit ZXing.Common.ConcentricFinder;

{
  * Copyright 2020 Axel Waggershauser
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

  * Ported from zxing-cpp (ConcentricFinder.h/.cpp): locates the center and
  * corners of concentric patterns, like the finder and alignment patterns
  * of a QR Code, with sub-pixel precision.
  *
  * Callers run with the floating point exceptions masked, see
  * TFloatExceptionsMasked.
}

interface

uses
  System.Types,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry,
  ZXing.Common.BitMatrixCursor;

type
  /// <summary>The center of a concentric pattern and its size (width) in
  /// pixels. Two patterns are the same when their centers are.</summary>
  TConcentricPattern = record
    p: TPointD;
    size: Double;
    class function Create(const center: TPointD; size: Double)
      : TConcentricPattern; static;
    function SameAs(const other: TConcentricPattern): Boolean;
  end;

/// <summary>Reads the 5 widths of a symmetric pattern around the cursor
/// position (forwards and backwards), at most range pixels.</summary>
function ReadSymmetricPattern5(var cur: TBitMatrixCursorF; range: Integer;
  out pattern: TArray<Integer>): Boolean;

/// <summary>The width of the symmetric pattern around the cursor in its
/// direction when it matches pattern, otherwise 0. With updatePosition the
/// cursor moves to the center of the pattern.</summary>
function CheckSymmetricPattern(var cur: TBitMatrixCursorI;
  const pattern: array of Integer; e2e: Boolean; range: Integer;
  updatePosition: Boolean): Integer;

/// <summary>The center and size of the nth ring around center (negative:
/// its inner edge).</summary>
function CenterOfRing(image: TBitMatrix; const center: TPoint;
  width, nth: Integer; requireCircle: Boolean;
  out res: TConcentricPattern): Boolean;

function FinetuneConcentricPatternCenter(image: TBitMatrix;
  const center: TPointD; width, finderPatternSize: Integer;
  out res: TConcentricPattern): Boolean;

/// <summary>The corners of the square ring lineIndex around center.
/// </summary>
function FitSquareToPoints(image: TBitMatrix; const center: TPointD;
  width, lineIndex: Integer; backup: Boolean;
  out res: TQuadrilateralF): Boolean;

/// <summary>The corners between the ring ringIndex and the next one around
/// center.</summary>
function FindConcentricPatternCorners(image: TBitMatrix;
  const center: TPointD; width, ringIndex: Integer;
  out res: TQuadrilateralF): Boolean;

/// <summary>Locates a concentric pattern (like the 1:1:3:1:1 finder pattern
/// of a QR Code) around center in all 4 directions and fine-tunes its
/// center.</summary>
function LocateConcentricPattern(image: TBitMatrix;
  const pattern: array of Integer; e2e: Boolean; const center: TPointD;
  width: Integer; out res: TConcentricPattern): Boolean;

implementation

uses
  System.Math,
  ZXing.Common.Pattern;

{ TConcentricPattern }

class function TConcentricPattern.Create(const center: TPointD; size: Double)
  : TConcentricPattern;
begin
  Result.p := center;
  Result.size := size;
end;

function TConcentricPattern.SameAs(const other: TConcentricPattern): Boolean;
begin
  Result := (p.X = other.p.X) and (p.Y = other.p.Y);
end;

function DistanceI(const a, b: TPoint): Double; inline;
begin
  Result := Sqrt(Sqr(Double(a.X - b.X)) + Sqr(Double(a.Y - b.Y)));
end;

/// <summary>bresenhamDirection of an integer point: truncating division by
/// the largest component.</summary>
function BresenhamDirectionI(const d: TPoint): TPoint;
begin
  var m := Max(Abs(d.X), Abs(d.Y));
  Result := PointI(d.X div m, d.Y div m);
end;

function ReadSymmetricPattern5(var cur: TBitMatrixCursorF; range: Integer;
  out pattern: TArray<Integer>): Boolean;
var
  res: TArray<Integer>;
  cuo: TBitMatrixCursorF;
  rangeLeft: Integer;

  function next(var c: TBitMatrixCursorF; i: Integer): Integer;
  begin
    Result := c.StepToEdge(1, rangeLeft);
    Inc(res[2 + i], Result);
    if (rangeLeft <> 0) then
      Dec(rangeLeft, Result);
  end;

begin
  Result := false;
  SetLength(res, 5);
  rangeLeft := range;
  cuo := cur.TurnedBack;
  for var i := 0 to 2 do
    if (next(cur, i) = 0) or (next(cuo, -i) = 0) then
      exit;
  // the starting pixel has been counted twice, fix this
  Dec(res[2]);
  pattern := res;
  Result := true;
end;

function CheckSymmetricPattern(var cur: TBitMatrixCursorI;
  const pattern: array of Integer; e2e: Boolean; range: Integer;
  updatePosition: Boolean): Integer;
var
  res: TArray<Integer>;
  curFwd, curBwd: TFastEdgeToEdgeCounter;
begin
  Result := 0;
  var n := System.Length(pattern);
  var s2 := n div 2;
  curFwd := TFastEdgeToEdgeCounter.Create(cur);
  curBwd := TFastEdgeToEdgeCounter.Create(cur.TurnedBack);

  var centerFwd := curFwd.StepToNextEdge(range);
  if (centerFwd = 0) then
    exit;
  var centerBwd := curBwd.StepToNextEdge(range);
  if (centerBwd = 0) then
    exit;

  SetLength(res, n);
  // -1 because the starting pixel is counted twice
  res[s2] := centerFwd + centerBwd - 1;
  Dec(range, res[s2]);

  for var i := 1 to s2 do
  begin
    var v := curFwd.StepToNextEdge(range);
    res[s2 + i] := v;
    Dec(range, v);
    if (v = 0) then
      exit;
    v := curBwd.StepToNextEdge(range);
    res[s2 - i] := v;
    Dec(range, v);
    if (v = 0) then
      exit;
  end;

  if (IsPattern(res, pattern, e2e) = 0) then
    exit;

  if updatePosition then
    cur.Step(res[s2] div 2 - (centerBwd - 1));

  for var v in res do
    Inc(Result, v);
end;

function AverageEdgePixels(cur: TBitMatrixCursorI; range, numOfEdges: Integer;
  out res: TConcentricPattern): Boolean;
begin
  Result := false;
  var sum := PointD(0, 0);
  var totalSteps := 0;
  for var i := 0 to numOfEdges - 1 do
  begin
    var steps := cur.StepToEdge(1, range - totalSteps);
    if (steps = 0) then
      exit;
    Inc(totalSteps, steps);
    var b := cur.Back;
    sum := sum + CenteredI(cur.p) + CenteredI(PointI(cur.p.X + b.X,
      cur.p.Y + b.Y));
  end;
  res := TConcentricPattern.Create(sum / (2 * numOfEdges), totalSteps);
  Result := true;
end;

function CenterOfDoubleCross(image: TBitMatrix; const center: TPoint;
  width, numOfEdges: Integer; out res: TConcentricPattern): Boolean;
const
  DIRS: array [0 .. 3, 0 .. 1] of Integer = ((0, 1), (1, 0), (1, 1), (1, -1));
begin
  Result := false;
  var sumP := PointD(0, 0);
  var sumS: Double := 0;
  for var k := 0 to 3 do
  begin
    var avr1, avr2: TConcentricPattern;
    if not AverageEdgePixels(TBitMatrixCursorI.Create(image, center,
      PointI(DIRS[k, 0], DIRS[k, 1])), width * 3 div 5, numOfEdges, avr1) or
      not AverageEdgePixels(TBitMatrixCursorI.Create(image, center,
      PointI(-DIRS[k, 0], -DIRS[k, 1])), width * 3 div 5, numOfEdges, avr2) then
      exit;
    var m: Double := Min(avr1.size, avr2.size);
    var mm: Double := Max(avr1.size, avr2.size);
    // only accept if the two legs are very similar in size
    if (mm > 1.2 * m + 1) then
      exit;
    sumP := sumP + avr1.p + avr2.p;
    sumS := sumS + avr1.size + avr2.size;
  end;
  res := TConcentricPattern.Create(sumP / 8, 2 * sumS / 8);
  Result := true;
end;

function CenterOfRing(image: TBitMatrix; const center: TPoint;
  width, nth: Integer; requireCircle: Boolean;
  out res: TConcentricPattern): Boolean;
begin
  Result := false;
  // upper limit for circumference of the ring
  var maxN := 4 * (width + 1) * 3 div 2;
  var maxR: Integer;
  if requireCircle then
    maxR := width + 1
  else
    maxR := 2 * width;
  var inner := nth < 0;
  nth := Abs(nth);

  var cur := TBitMatrixCursorI.Create(image, center, PointI(1, 0));
  if (cur.StepToEdge(nth, maxR, inner) = 0) then
    exit;
  // move clock wise and keep edge on the right/left depending on backup
  cur.TurnRight;
  var edgeDir: Integer;
  if inner then
    edgeDir := DIR_LEFT
  else
    edgeDir := DIR_RIGHT;

  var neighbourMask: Cardinal := 0;
  var start := cur.p;
  var sumP := PointD(0, 0);
  var sumR: Double := 0;
  var nP := 0;
  repeat
    sumP := sumP + CenteredI(cur.p);
    Inc(nP);

    // find out if we come full circle around the center. 8 bits have to be
    // set in the end.
    var bd := BresenhamDirectionI(PointI(cur.p.X - center.X,
      cur.p.Y - center.Y));
    neighbourMask := neighbourMask or (Cardinal(1) shl (4 + bd.X + 3 * bd.Y));

    if not cur.StepAlongEdge(edgeDir) then
      exit;

    // add/subtract 0.5 to go from pixel center to pixel edge
    var r: Double := DistanceI(cur.p, center);
    if inner then
      r := r + 0.5
    else
      r := r - 0.5;
    sumR := sumR + r;

    if (r > maxR) or ((center.X = cur.p.X) and (center.Y = cur.p.Y)) or
      (nP > maxN) then
      exit;
  until (cur.p.X = start.X) and (cur.p.Y = start.Y);

  if requireCircle and (neighbourMask <> $1EF) then // 0b111101111
    exit;

  var meanP := sumP / nP;
  var meanR: Double := sumR / nP;
  // C is the number of edge pixels per unit length, for a perfect circle
  // C = 2*pi ~ 6.28, for a square C = 8
  var c: Double := nP / meanR;
  // normalized distance between estimated and calculated center
  var centerMove: Double := PointDistance(meanP, PointD(center.X, center.Y))
    / width;

  // C > 12 means that the edge is very irregular, centerMove > 0.5 means the
  // center moved more than half the estimated width of the ring
  if requireCircle and ((c > 12) or (centerMove > 0.5)) then
    exit;

  // a square with mean radius r has a width of approx. 1.74 * r
  res := TConcentricPattern.Create(meanP, 1.74 * meanR);
  Result := true;
end;

function CenterOfRings(image: TBitMatrix; const center: TPointD;
  range, numOfRings: Integer; out res: TConcentricPattern): Boolean;
begin
  Result := false;
  var n := 1;
  var size: Double := 0;
  var sum := center;
  for var i := 2 to numOfRings do
  begin
    var c: TConcentricPattern;
    if not CenterOfRing(image, ToPointI(center), range, i, true, c) then
    begin
      if (n = 1) then
        exit
      else
        break;
    end
    else if (PointDistance(c.p, center) > range div numOfRings div 2) then
      exit;
    sum := sum + c.p;
    size := c.size;
    Inc(n);
  end;
  if (n <> numOfRings) then
    size := 0;
  res := TConcentricPattern.Create(sum / n, size);
  Result := true;
end;

function CollectRingPoints(image: TBitMatrix; const center: TPointD;
  width, edgeIndex: Integer; backup: Boolean): TArray<TPointD>;
begin
  Result := nil;
  var centerI := ToPointI(center);
  // upper limit for circumference of the ring
  var maxN := 4 * (width + 1) * 3 div 2;
  var maxR := width * 2;
  var cur := TBitMatrixCursorI.Create(image, centerI, PointI(1, 0));
  if (cur.StepToEdge(edgeIndex, maxR, backup) = 0) then
    exit;
  // move clock wise and keep edge on the right/left depending on backup
  cur.TurnRight;
  var edgeDir: Integer;
  if backup then
    edgeDir := DIR_LEFT
  else
    edgeDir := DIR_RIGHT;

  var neighbourMask: Cardinal := 0;
  var start := cur.p;
  var points: TArray<TPointD>;
  var count := 0;
  SetLength(points, maxN div 2 + 1);
  repeat
    if (count = System.Length(points)) then
      SetLength(points, count * 2);
    points[count] := CenteredI(cur.p);
    Inc(count);

    // find out if we come full circle around the center. 8 bits have to be
    // set in the end.
    var bd := BresenhamDirectionI(PointI(cur.p.X - centerI.X,
      cur.p.Y - centerI.Y));
    neighbourMask := neighbourMask or (Cardinal(1) shl (4 + bd.X + 3 * bd.Y));

    if not cur.StepAlongEdge(edgeDir) then
      exit;

    if (DistanceI(cur.p, centerI) > maxR) or
      ((centerI.X = cur.p.X) and (centerI.Y = cur.p.Y)) or (count > maxN) then
      exit;
  until (cur.p.X = start.X) and (cur.p.Y = start.Y);

  if (neighbourMask <> $1EF) then // 0b111101111
    exit;

  SetLength(points, count);
  Result := points;
end;

/// <summary>A regression line through points first to last - 1 (zxing-cpp's
/// RegressionLine(begin, end)); invalid when there are none.</summary>
function LineThrough(const points: TArray<TPointD>; first, last: Integer)
  : TRegressionLine;
begin
  Result := TRegressionLine.Create;
  for var i := first to last - 1 do
    Result.Add(points[i]);
  Result.Evaluate;
end;

function FitQuadrilateralToPoints(const center: TPointD;
  var points: TArray<TPointD>; out res: TQuadrilateralF): Boolean;
var
  corners: array [0 .. 3] of Integer;
  lines: array [0 .. 3] of TRegressionLine;
  beg, fin: array [0 .. 3] of Integer;

  function dist2Center(i: Integer): Double;
  begin
    Result := PointDistance(points[i], center);
  end;

begin
  Result := false;
  var n := System.Length(points);

  // the first smallest and the last largest distance, like
  // std::minmax_element
  var minIdx := 0;
  var maxIdx := 0;
  for var i := 1 to n - 1 do
  begin
    if (dist2Center(i) < dist2Center(minIdx)) then
      minIdx := i;
    if not (dist2Center(i) < dist2Center(maxIdx)) then
      maxIdx := i;
  end;

  // check if points are on a circle: for a square the min/max ratio is 0.7,
  // for a circle it is 1
  if (dist2Center(minIdx) / dist2Center(maxIdx) > 0.85) then
    exit;

  // rotate points such that the first one is the furthest away from the
  // center (hence, a corner)
  var rotated: TArray<TPointD>;
  SetLength(rotated, n);
  for var i := 0 to n - 1 do
    rotated[i] := points[(maxIdx + i) mod n];
  points := rotated;

  corners[0] := 0;
  // find the opposite corner by looking for the farthest point near the
  // opposite point
  corners[2] := n * 3 div 8;
  for var i := n * 3 div 8 + 1 to n * 5 div 8 - 1 do
    if (dist2Center(i) > dist2Center(corners[2])) then
      corners[2] := i;

  // find the two in between corners by looking for the points farthest from
  // the long diagonal
  var diagonal := TRegressionLine.Create(points[corners[0]],
    points[corners[2]]);
  try
    corners[1] := n div 8;
    for var i := n div 8 + 1 to n * 3 div 8 - 1 do
      if (diagonal.Distance(points[i]) > diagonal.Distance(points[corners[1]]))
      then
        corners[1] := i;
    corners[3] := n * 5 div 8;
    for var i := n * 5 div 8 + 1 to n * 7 div 8 - 1 do
      if (diagonal.Distance(points[i]) > diagonal.Distance(points[corners[3]]))
      then
        corners[3] := i;
  finally
    diagonal.Free;
  end;

  for var i := 0 to 3 do
  begin
    beg[i] := corners[i] + 1;
    if (i < 3) then
      fin[i] := corners[i + 1]
    else
      fin[i] := n;
  end;

  for var i := 0 to 3 do
    lines[i] := nil;
  try
    for var i := 0 to 3 do
      lines[i] := LineThrough(points, beg[i], fin[i]);
    for var i := 0 to 3 do
      if not lines[i].IsValid then
        exit;

    // check if all points belonging to each line segment are sufficiently
    // close to that line
    for var i := 0 to 3 do
    begin
      var len := fin[i] - beg[i];
      for var j := beg[i] to fin[i] - 1 do
        if (len > 3) and (lines[i].Distance(points[j]) > Max(1.0, Min(8.0,
          len / 8.0))) then
          exit;
    end;

    for var i := 0 to 3 do
      res[i] := Intersect(lines[i], lines[(i + 1) mod 4]);
    Result := true;
  finally
    for var i := 0 to 3 do
      lines[i].Free;
  end;
end;

function QuadrilateralIsPlausibleSquare(const q: TQuadrilateralF;
  lineIndex: Integer): Boolean;
begin
  var m: Double := PointDistance(q[0], q[3]);
  var mm: Double := m;
  for var i := 1 to 3 do
  begin
    var d: Double := PointDistance(q[i - 1], q[i]);
    m := Min(m, d);
    mm := Max(mm, d);
  end;
  Result := (m >= lineIndex * 2) and (m > mm / 3);
end;

function FitSquareToPoints(image: TBitMatrix; const center: TPointD;
  width, lineIndex: Integer; backup: Boolean;
  out res: TQuadrilateralF): Boolean;
begin
  Result := false;
  var points := CollectRingPoints(image, center, width, lineIndex, backup);
  if (points = nil) then
    exit;
  if not FitQuadrilateralToPoints(center, points, res) then
    exit;
  Result := QuadrilateralIsPlausibleSquare(res, lineIndex - Ord(backup));
end;

/// <summary>The average of two quadrilaterals, rotated such that the two
/// first points are closest to each other.</summary>
function Blend(const a, b: TQuadrilateralF): TQuadrilateralF;
begin
  var offset := 0;
  for var i := 1 to 3 do
    if (PointDistance(b[i], a[0]) < PointDistance(b[offset], a[0])) then
      offset := i;
  for var i := 0 to 3 do
    Result[i] := (a[i] + b[(i + offset) mod 4]) / 2;
end;

function FindConcentricPatternCorners(image: TBitMatrix;
  const center: TPointD; width, ringIndex: Integer;
  out res: TQuadrilateralF): Boolean;
begin
  Result := false;
  var innerCorners, outerCorners: TQuadrilateralF;
  if not FitSquareToPoints(image, center, width, ringIndex, false,
    innerCorners) then
    exit;
  if not FitSquareToPoints(image, center, width, ringIndex + 1, true,
    outerCorners) then
    exit;
  res := Blend(innerCorners, outerCorners);
  Result := true;
end;

function FinetuneConcentricPatternCenter(image: TBitMatrix;
  const center: TPointD; width, finderPatternSize: Integer;
  out res: TConcentricPattern): Boolean;
begin
  Result := false;
  // make sure we have at least one path of white around the center
  var res1: TConcentricPattern;
  if CenterOfRing(image, ToPointI(center), width * 2 div 3, 1, true, res1) and
    IsInImage(image, res1.p) and BlackAtPoint(image, res1.p) then
  begin
    // and then either at least one more ring around that
    var res2: TConcentricPattern;
    if CenterOfRings(image, res1.p, width, finderPatternSize div 2, res2) and
      IsInImage(image, res2.p) and BlackAtPoint(image, res2.p) then
    begin
      // CenterOfRings only estimates the radius of the white ring
      res2.size := res2.size * 7 / 5;
      res := res2;
      exit(true);
    end;
    // or the center can be approximated by a double cross
    if CenterOfDoubleCross(image, ToPointI(res1.p), width,
      finderPatternSize div 2 + 1, res2) and IsInImage(image, res2.p) and
      BlackAtPoint(image, res2.p) then
    begin
      res := res2;
      exit(true);
    end;
  end;
end;

function LocateConcentricPattern(image: TBitMatrix;
  const pattern: array of Integer; e2e: Boolean; const center: TPointD;
  width: Integer; out res: TConcentricPattern): Boolean;
const
  DIRS: array [0 .. 3, 0 .. 1] of Integer = ((0, 1), (1, 0), (1, 1), (1, -1));
begin
  Result := false;
  var cur := TBitMatrixCursorI.Create(image, ToPointI(center), PointI(0, 0));
  var range := width * 2;
  var minSpread := image.Width;
  var maxSpread := 0;
  var maxError := 0;
  for var k := 0 to 3 do
  begin
    cur.d := PointI(DIRS[k, 0], DIRS[k, 1]);
    // horizontal and vertical with e2e and moving to the center, the
    // diagonals relaxed and without moving
    var spread: Integer;
    if (k < 2) then
      spread := CheckSymmetricPattern(cur, pattern, e2e, range, true)
    else
      spread := CheckSymmetricPattern(cur, pattern, true, range, false);
    if (spread <> 0) then
    begin
      minSpread := Min(minSpread, spread);
      maxSpread := Max(maxSpread, spread);
    end
    else
    begin
      Dec(maxError);
      if (maxError < 0) then
        exit;
    end;
  end;

  if (maxSpread > 5 * minSpread) then
    exit;

  if FinetuneConcentricPatternCenter(image, PointD(cur.p.X, cur.p.Y), width,
    System.Length(pattern), res) then
  begin
    if (res.size = 0) then
      res.size := (maxSpread + minSpread) div 2;
    Result := true;
  end;
end;

end.
