unit ZXing.Common.Geometry;

{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2017, 2020 Axel Waggershauser
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

  * Ported from zxing-cpp (Point.h, RegressionLine.h, Quadrilateral.h and
  * PerspectiveTransform.cpp) for the edge tracing Data Matrix detector.
  *
  * Like in zxing-cpp, a RegressionLine can hold NaN values (an invalid line)
  * and calculations can produce NaN or infinity. Callers run with the
  * floating point exceptions masked, see TFloatExceptionsMasked.
}

interface

uses
  System.SysUtils,
  System.Math,
  ZXing.Common.BitMatrix;

type
  TPointD = record
    X, Y: Double;
    class function Create(const AX, AY: Double): TPointD; static; inline;
    class operator Add(const a, b: TPointD): TPointD; inline;
    class operator Subtract(const a, b: TPointD): TPointD; inline;
    class operator Negative(const a: TPointD): TPointD; inline;
    class operator Multiply(const s: Double; const a: TPointD): TPointD; inline;
    class operator Divide(const a: TPointD; const d: Double): TPointD; inline;
    class operator Equal(const a, b: TPointD): Boolean; inline;
    class operator NotEqual(const a, b: TPointD): Boolean; inline;
  end;

  TQuadrilateralF = array [0 .. 3] of TPointD;

function PointD(const x, y: Double): TPointD; inline;
function Dot(const a, b: TPointD): Double; inline;
function Cross(const a, b: TPointD): Double; inline;
function PointLength(const p: TPointD): Double; inline;
function PointDistance(const a, b: TPointD): Double; inline;
function MaxAbsComponent(const p: TPointD): Double; inline;
function Normalized(const d: TPointD): TPointD; inline;
/// <summary>Direction with the largest component 1: one step in it moves
/// one pixel along the main axis (Bresenham).</summary>
function BresenhamDirection(const d: TPointD): TPointD; inline;
/// <summary>The larger of the two components, the other one 0.</summary>
function MainDirection(const d: TPointD): TPointD; inline;
function RightOf(const d: TPointD): TPointD; inline;
function LeftOf(const d: TPointD): TPointD; inline;
/// <summary>Center of the pixel the point is in.</summary>
function Centered(const p: TPointD): TPointD; inline;
/// <summary>Pixel coordinate as in C++ (int) conversion: truncated.</summary>
function PixelX(const p: TPointD): Integer; inline;
function PixelY(const p: TPointD): Integer; inline;

function IsInImage(const image: TBitMatrix; const p: TPointD): Boolean; inline;
/// <summary>Only for points inside the image.</summary>
function BlackAtPoint(const image: TBitMatrix; const p: TPointD): Boolean; inline;

/// <summary>(margin, margin) to (width - margin, height - margin), clockwise
/// from the top left.</summary>
function RectangleF(width, height: Integer; margin: Double = 0): TQuadrilateralF;
function IsConvex(const q: TQuadrilateralF): Boolean;

type
  /// <summary>
  /// Line fitted through a list of points (total least squares). The normal
  /// points to the inside, set with SetDirectionInward before adding points.
  /// </summary>
  TRegressionLine = class
  protected
    FPoints: TArray<TPointD>;
    FCount: Integer;
    FDirectionInward: TPointD;
    a, b, c: Double;
    function EvaluatePoints(const points: TArray<TPointD>; count: Integer): Boolean;
  public
    constructor Create; overload;
    /// <summary>Line through the two points.</summary>
    constructor Create(const p1, p2: TPointD); overload;

    procedure Reset;
    procedure Add(const p: TPointD);
    procedure PopBack;
    procedure SetDirectionInward(const d: TPointD);
    /// <summary>Fits the line. With maxSignedDist > 0 the points further
    /// inside than maxSignedDist or further outside than 2 * maxSignedDist
    /// are left out (and removed when updatePoints).</summary>
    function Evaluate(maxSignedDist: Double = -1;
      updatePoints: Boolean = false): Boolean;

    function IsValid: Boolean; inline;
    function Normal: TPointD; inline;
    function SignedDistance(const p: TPointD): Double; inline;
    function Distance(const p: TPointD): Double; inline;
    function Project(const p: TPointD): TPointD; inline;
    function Centroid: TPointD;
    /// <summary>Distance between first and last point, truncated.</summary>
    function Length: Integer;
    function Point(i: Integer): TPointD; inline;
    function Front: TPointD; inline;
    function Back: TPointD; inline;
    property Count: Integer read FCount;
  end;

function Intersect(l1, l2: TRegressionLine): TPointD;

type
  /// <summary>Projective transformation, maps grid (module) coordinates to
  /// image (pixel) coordinates.</summary>
  TPerspectiveTransformF = record
  private
    a11, a12, a13, a21, a22, a23, a31, a32, a33: Double;
    class function Make(a11, a21, a31, a12, a22, a32, a13, a23,
      a33: Double): TPerspectiveTransformF; static;
    class function UnitSquareTo(const q: TQuadrilateralF)
      : TPerspectiveTransformF; static;
    function Inverse: TPerspectiveTransformF;
    function Times(const other: TPerspectiveTransformF): TPerspectiveTransformF;
  public
    /// <summary>Transformation from src to dst; invalid when one of them is
    /// not convex.</summary>
    class function Create(const src, dst: TQuadrilateralF)
      : TPerspectiveTransformF; static;
    function Map(const p: TPointD): TPointD;
    function IsValid: Boolean;
  end;

  /// <summary>Masks the floating point exceptions while it lives (NaN and
  /// infinity are valid intermediate values in the geometry code). Use as
  /// local variable: Init at the start, Restore in a finally block.</summary>
  TFloatExceptionsMasked = record
  private
    FOldMask: TArithmeticExceptionMask;
  public
    procedure Init;
    procedure Restore;
  end;

implementation

{ TPointD }

class function TPointD.Create(const AX, AY: Double): TPointD;
begin
  Result.X := AX;
  Result.Y := AY;
end;

class operator TPointD.Add(const a, b: TPointD): TPointD;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

class operator TPointD.Subtract(const a, b: TPointD): TPointD;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

class operator TPointD.Negative(const a: TPointD): TPointD;
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
end;

class operator TPointD.Multiply(const s: Double; const a: TPointD): TPointD;
begin
  Result.X := s * a.X;
  Result.Y := s * a.Y;
end;

class operator TPointD.Divide(const a: TPointD; const d: Double): TPointD;
begin
  Result.X := a.X / d;
  Result.Y := a.Y / d;
end;

class operator TPointD.Equal(const a, b: TPointD): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y);
end;

class operator TPointD.NotEqual(const a, b: TPointD): Boolean;
begin
  Result := not (a = b);
end;

function PointD(const x, y: Double): TPointD;
begin
  Result.X := x;
  Result.Y := y;
end;

function Dot(const a, b: TPointD): Double;
begin
  Result := a.X * b.X + a.Y * b.Y;
end;

function Cross(const a, b: TPointD): Double;
begin
  Result := a.X * b.Y - b.X * a.Y;
end;

function PointLength(const p: TPointD): Double;
begin
  Result := Sqrt(Dot(p, p));
end;

function PointDistance(const a, b: TPointD): Double;
begin
  Result := PointLength(a - b);
end;

function MaxAbsComponent(const p: TPointD): Double;
begin
  Result := Max(Abs(p.X), Abs(p.Y));
end;

function Normalized(const d: TPointD): TPointD;
begin
  Result := d / PointLength(d);
end;

function BresenhamDirection(const d: TPointD): TPointD;
begin
  Result := d / MaxAbsComponent(d);
end;

function MainDirection(const d: TPointD): TPointD;
begin
  if (Abs(d.X) > Abs(d.Y)) then
    Result := PointD(d.X, 0)
  else
    Result := PointD(0, d.Y);
end;

function RightOf(const d: TPointD): TPointD;
begin
  Result := PointD(-d.Y, d.X);
end;

function LeftOf(const d: TPointD): TPointD;
begin
  Result := PointD(d.Y, -d.X);
end;

function Centered(const p: TPointD): TPointD;
begin
  Result := PointD(Floor(p.X) + 0.5, Floor(p.Y) + 0.5);
end;

function PixelX(const p: TPointD): Integer;
begin
  Result := Trunc(p.X);
end;

function PixelY(const p: TPointD): Integer;
begin
  Result := Trunc(p.Y);
end;

function IsInImage(const image: TBitMatrix; const p: TPointD): Boolean;
begin
  // also false for NaN
  Result := (p.X >= 0) and (p.X < image.Width) and (p.Y >= 0) and
    (p.Y < image.Height);
end;

function BlackAtPoint(const image: TBitMatrix; const p: TPointD): Boolean;
begin
  Result := image[Trunc(p.X), Trunc(p.Y)];
end;

function RectangleF(width, height: Integer; margin: Double): TQuadrilateralF;
begin
  Result[0] := PointD(margin, margin);
  Result[1] := PointD(width - margin, margin);
  Result[2] := PointD(width - margin, height - margin);
  Result[3] := PointD(margin, height - margin);
end;

function IsConvex(const q: TQuadrilateralF): Boolean;
var
  i: Integer;
  sign: Boolean;
  cp, m, mx: Double;
begin
  sign := false;
  m := Infinity;
  mx := 0;
  for i := 0 to 3 do
  begin
    cp := Cross(q[(i + 2) mod 4] - q[(i + 1) mod 4], q[i] - q[(i + 1) mod 4]);
    m := Min(m, Abs(cp));
    mx := Max(mx, Abs(cp));
    if (i = 0) then
      sign := cp > 0
    else if (sign <> (cp > 0)) then
      exit(false);
  end;
  // Being convex is not enough to prevent a numerical instability where one
  // corner is almost in line with two others (see zxing-cpp).
  Result := mx / m < 4.0;
end;

{ TRegressionLine }

constructor TRegressionLine.Create;
begin
  inherited Create;
  SetLength(FPoints, 16);
  Reset;
end;

constructor TRegressionLine.Create(const p1, p2: TPointD);
var
  points: TArray<TPointD>;
begin
  inherited Create;
  a := NaN;
  b := NaN;
  c := NaN;
  SetLength(points, 2);
  points[0] := p1;
  points[1] := p2;
  EvaluatePoints(points, 2);
end;

procedure TRegressionLine.Reset;
begin
  FCount := 0;
  FDirectionInward := PointD(0, 0);
  a := NaN;
  b := NaN;
  c := NaN;
end;

procedure TRegressionLine.Add(const p: TPointD);
begin
  if (FCount = System.Length(FPoints)) then
    SetLength(FPoints, FCount * 2 + 16);
  FPoints[FCount] := p;
  Inc(FCount);
  if (FCount = 1) then
    c := Dot(Normal, p);
end;

procedure TRegressionLine.PopBack;
begin
  if (FCount > 0) then
    Dec(FCount);
end;

procedure TRegressionLine.SetDirectionInward(const d: TPointD);
begin
  FDirectionInward := Normalized(d);
end;

function TRegressionLine.EvaluatePoints(const points: TArray<TPointD>;
  count: Integer): Boolean;
var
  i: Integer;
  mean, d: TPointD;
  sumXX, sumYY, sumXY, l: Double;
begin
  mean := PointD(0, 0);
  for i := 0 to count - 1 do
    mean := mean + points[i];
  mean := mean / count;
  sumXX := 0;
  sumYY := 0;
  sumXY := 0;
  for i := 0 to count - 1 do
  begin
    d := points[i] - mean;
    sumXX := sumXX + d.X * d.X;
    sumYY := sumYY + d.Y * d.Y;
    sumXY := sumXY + d.X * d.Y;
  end;
  if (sumYY >= sumXX) then
  begin
    l := Sqrt(sumYY * sumYY + sumXY * sumXY);
    a := +sumYY / l;
    b := -sumXY / l;
  end
  else
  begin
    l := Sqrt(sumXX * sumXX + sumXY * sumXY);
    a := +sumXY / l;
    b := -sumXX / l;
  end;
  if (Dot(FDirectionInward, Normal) < 0) then
  begin
    a := -a;
    b := -b;
  end;
  c := Dot(Normal, mean);
  // angle between original and new direction is at most 60 degree
  Result := Dot(FDirectionInward, Normal) > 0.5;
end;

function TRegressionLine.Evaluate(maxSignedDist: Double;
  updatePoints: Boolean): Boolean;
var
  points: TArray<TPointD>;
  count, oldCount, i, j: Integer;
  sd: Double;
begin
  Result := EvaluatePoints(FPoints, FCount);
  if (maxSignedDist > 0) then
  begin
    points := Copy(FPoints, 0, FCount);
    count := FCount;
    while true do
    begin
      oldCount := count;
      // remove points that are further 'inside' than maxSignedDist or further
      // 'outside' than 2 x maxSignedDist
      j := 0;
      for i := 0 to count - 1 do
      begin
        sd := SignedDistance(points[i]);
        if not ((sd > maxSignedDist) or (sd < -2 * maxSignedDist)) then
        begin
          points[j] := points[i];
          Inc(j);
        end;
      end;
      count := j;
      // if we threw away too many points, something is off with the line
      if (count < oldCount div 2) or (count < 2) then
        exit(false);
      if (oldCount = count) then
        break;
      Result := EvaluatePoints(points, count);
    end;

    if updatePoints then
    begin
      FPoints := points;
      FCount := count;
    end;
  end;
end;

function TRegressionLine.IsValid: Boolean;
begin
  Result := not IsNan(a);
end;

function TRegressionLine.Normal: TPointD;
begin
  if IsValid then
    Result := PointD(a, b)
  else
    Result := FDirectionInward;
end;

function TRegressionLine.SignedDistance(const p: TPointD): Double;
begin
  Result := Dot(Normal, p) - c;
end;

function TRegressionLine.Distance(const p: TPointD): Double;
begin
  Result := Abs(SignedDistance(p));
end;

function TRegressionLine.Project(const p: TPointD): TPointD;
begin
  Result := p - SignedDistance(p) * Normal;
end;

function TRegressionLine.Centroid: TPointD;
var
  i: Integer;
begin
  Result := PointD(0, 0);
  for i := 0 to FCount - 1 do
    Result := Result + FPoints[i];
  Result := Result / FCount;
end;

function TRegressionLine.Length: Integer;
begin
  if (FCount >= 2) then
    Result := Trunc(PointDistance(FPoints[0], FPoints[FCount - 1]))
  else
    Result := 0;
end;

function TRegressionLine.Point(i: Integer): TPointD;
begin
  Result := FPoints[i];
end;

function TRegressionLine.Front: TPointD;
begin
  Result := FPoints[0];
end;

function TRegressionLine.Back: TPointD;
begin
  Result := FPoints[FCount - 1];
end;

function Intersect(l1, l2: TRegressionLine): TPointD;
var
  d: Double;
begin
  d := l1.a * l2.b - l1.b * l2.a;
  Result.X := (l1.c * l2.b - l1.b * l2.c) / d;
  Result.Y := (l1.a * l2.c - l1.c * l2.a) / d;
end;

{ TPerspectiveTransformF }

class function TPerspectiveTransformF.Make(a11, a21, a31, a12, a22, a32, a13,
  a23, a33: Double): TPerspectiveTransformF;
begin
  Result.a11 := a11;
  Result.a12 := a12;
  Result.a13 := a13;
  Result.a21 := a21;
  Result.a22 := a22;
  Result.a23 := a23;
  Result.a31 := a31;
  Result.a32 := a32;
  Result.a33 := a33;
end;

function TPerspectiveTransformF.Inverse: TPerspectiveTransformF;
begin
  // Here, the adjoint serves as the inverse
  Result := Make(a22 * a33 - a23 * a32, a23 * a31 - a21 * a33,
    a21 * a32 - a22 * a31, a13 * a32 - a12 * a33, a11 * a33 - a13 * a31,
    a12 * a31 - a11 * a32, a12 * a23 - a13 * a22, a13 * a21 - a11 * a23,
    a11 * a22 - a12 * a21);
end;

function TPerspectiveTransformF.Times(const other: TPerspectiveTransformF)
  : TPerspectiveTransformF;
begin
  Result := Make(a11 * other.a11 + a21 * other.a12 + a31 * other.a13,
    a11 * other.a21 + a21 * other.a22 + a31 * other.a23,
    a11 * other.a31 + a21 * other.a32 + a31 * other.a33,
    a12 * other.a11 + a22 * other.a12 + a32 * other.a13,
    a12 * other.a21 + a22 * other.a22 + a32 * other.a23,
    a12 * other.a31 + a22 * other.a32 + a32 * other.a33,
    a13 * other.a11 + a23 * other.a12 + a33 * other.a13,
    a13 * other.a21 + a23 * other.a22 + a33 * other.a23,
    a13 * other.a31 + a23 * other.a32 + a33 * other.a33);
end;

class function TPerspectiveTransformF.UnitSquareTo(const q: TQuadrilateralF)
  : TPerspectiveTransformF;
var
  d1, d2, d3: TPointD;
  denominator, a13, a23: Double;
begin
  d3 := q[0] - q[1] + q[2] - q[3];
  if (d3 = PointD(0, 0)) then
    // Affine
    Result := Make(q[1].X - q[0].X, q[2].X - q[1].X, q[0].X, q[1].Y - q[0].Y,
      q[2].Y - q[1].Y, q[0].Y, 0, 0, 1)
  else
  begin
    d1 := q[1] - q[2];
    d2 := q[3] - q[2];
    denominator := Cross(d1, d2);
    a13 := Cross(d3, d2) / denominator;
    a23 := Cross(d1, d3) / denominator;
    Result := Make(q[1].X - q[0].X + a13 * q[1].X, q[3].X - q[0].X + a23 *
      q[3].X, q[0].X, q[1].Y - q[0].Y + a13 * q[1].Y, q[3].Y - q[0].Y + a23 *
      q[3].Y, q[0].Y, a13, a23, 1);
  end;
end;

class function TPerspectiveTransformF.Create(const src, dst: TQuadrilateralF)
  : TPerspectiveTransformF;
begin
  Result := Make(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN);
  if not IsConvex(src) or not IsConvex(dst) then
    exit;
  Result := UnitSquareTo(dst).Times(UnitSquareTo(src).Inverse);
end;

function TPerspectiveTransformF.Map(const p: TPointD): TPointD;
var
  denominator: Double;
begin
  denominator := a13 * p.X + a23 * p.Y + a33;
  Result.X := (a11 * p.X + a21 * p.Y + a31) / denominator;
  Result.Y := (a12 * p.X + a22 * p.Y + a32) / denominator;
end;

function TPerspectiveTransformF.IsValid: Boolean;
begin
  Result := not IsNan(a33);
end;

{ TFloatExceptionsMasked }

procedure TFloatExceptionsMasked.Init;
begin
  FOldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
end;

procedure TFloatExceptionsMasked.Restore;
begin
  SetExceptionMask(FOldMask);
end;

end.
