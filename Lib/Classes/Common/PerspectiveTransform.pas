unit PerspectiveTransform;

{
  * Copyright 2008 ZXing authors
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

  * Implemented by E. Spelt for Delphi
}
interface

type

  TPerspectiveTransform = class sealed

  private
    a11: Single;
    a12: Single;
    a13: Single;
    a21: Single;
    a22: Single;
    a23: Single;
    a31: Single;
    a32: Single;
    a33: Single;

    function buildAdjoint: TPerspectiveTransform;
    function times(other: TPerspectiveTransform): TPerspectiveTransform;

  public
    constructor Create(a11: Single; a21: Single; a31: Single; a12: Single;
      a22: Single; a32: Single; a13: Single; a23: Single; a33: Single);

    class function quadrilateralToQuadrilateral(x0: Single; y0: Single;
      x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single;
      x0p: Single; y0p: Single; x1p: Single; y1p: Single; x2p: Single;
      y2p: Single; x3p: Single; y3p: Single): TPerspectiveTransform; static;

    class function quadrilateralToSquare(x0: Single; y0: Single; x1: Single;
      y1: Single; x2: Single; y2: Single; x3: Single; y3: Single)
      : TPerspectiveTransform; static;

    class function squareToQuadrilateral(x0: Single; y0: Single; x1: Single;
      y1: Single; x2: Single; y2: Single; x3: Single; y3: Single)
      : TPerspectiveTransform; static;

    procedure transformPoints(points: TArray<Single>); overload;
    procedure transformPoints(xValues: TArray<Single>;
      yValues: TArray<Single>); overload;
  end;

implementation

constructor TPerspectiveTransform.Create(a11: Single; a21: Single; a31: Single;
  a12: Single; a22: Single; a32: Single; a13: Single; a23: Single; a33: Single);
begin
  self.a11 := a11;
  self.a12 := a12;
  self.a13 := a13;
  self.a21 := a21;
  self.a22 := a22;
  self.a23 := a23;
  self.a31 := a31;
  self.a32 := a32;
  self.a33 := a33
end;

function TPerspectiveTransform.buildAdjoint: TPerspectiveTransform;
begin
  Result := TPerspectiveTransform.Create
    (((self.a22 * self.a33) - (self.a23 * self.a32)),
    ((self.a23 * self.a31) - (self.a21 * self.a33)),
    ((self.a21 * self.a32) - (self.a22 * self.a31)),
    ((self.a13 * self.a32) - (self.a12 * self.a33)),
    ((self.a11 * self.a33) - (self.a13 * self.a31)),
    ((self.a12 * self.a31) - (self.a11 * self.a32)),
    ((self.a12 * self.a23) - (self.a13 * self.a22)),
    ((self.a13 * self.a21) - (self.a11 * self.a23)),
    ((self.a11 * self.a22) - (self.a12 * self.a21)))
end;

class function TPerspectiveTransform.quadrilateralToQuadrilateral(x0, y0, x1,
  y1, x2, y2, x3, y3, x0p, y0p, x1p, y1p, x2p, y2p, x3p, y3p: Single)
  : TPerspectiveTransform;
var
  qToS: TPerspectiveTransform;
begin
  qToS := TPerspectiveTransform.quadrilateralToSquare(x0, y0, x1, y1, x2,
    y2, x3, y3);

  Result := TPerspectiveTransform.squareToQuadrilateral(x0p, y0p, x1p, y1p, x2p,
    y2p, x3p, y3p).times(qToS)
end;

class function TPerspectiveTransform.quadrilateralToSquare(x0: Single;
  y0: Single; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single;
  y3: Single): TPerspectiveTransform;
begin
  Result := TPerspectiveTransform.squareToQuadrilateral(x0, y0, x1, y1, x2, y2,
    x3, y3).buildAdjoint
end;

class function TPerspectiveTransform.squareToQuadrilateral(x0: Single;
  y0: Single; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single;
  y3: Single): TPerspectiveTransform;
var
  dx1, dx2, dx3, dy1, dy2, dy3, a13, a23, denominator: Single;
begin
  dx3 := (((x0 - x1) + x2) - x3);
  dy3 := (((y0 - y1) + y2) - y3);
  if ((dx3 = 0) and (dy3 = 0)) then
  begin
    Result := TPerspectiveTransform.Create((x1 - x0), (x2 - x1), x0, (y1 - y0),
      (y2 - y1), y0, 0, 0, 1);
    exit
  end;
  dx1 := (x1 - x2);
  dx2 := (x3 - x2);
  dy1 := (y1 - y2);
  dy2 := (y3 - y2);
  denominator := ((dx1 * dy2) - (dx2 * dy1));
  a13 := (((dx3 * dy2) - (dx2 * dy3)) / denominator);
  a23 := (((dx1 * dy3) - (dx3 * dy1)) / denominator);
  begin
    Result := TPerspectiveTransform.Create(((x1 - x0) + (a13 * x1)),
      ((x3 - x0) + (a23 * x3)), x0, ((y1 - y0) + (a13 * y1)),
      ((y3 - y0) + (a23 * y3)), y0, a13, a23, 1);
    exit
  end
end;

function TPerspectiveTransform.times(other: TPerspectiveTransform)
  : TPerspectiveTransform;
begin
  Result := TPerspectiveTransform.Create
    ((((self.a11 * other.a11) + (self.a21 * other.a12)) + (self.a31 * other.a13)
    ), (((self.a11 * other.a21) + (self.a21 * other.a22)) +
    (self.a31 * other.a23)), (((self.a11 * other.a31) + (self.a21 * other.a32))
    + (self.a31 * other.a33)), (((self.a12 * other.a11) + (self.a22 * other.a12)
    ) + (self.a32 * other.a13)),
    (((self.a12 * other.a21) + (self.a22 * other.a22)) + (self.a32 * other.a23)
    ), (((self.a12 * other.a31) + (self.a22 * other.a32)) +
    (self.a32 * other.a33)), (((self.a13 * other.a11) + (self.a23 * other.a12))
    + (self.a33 * other.a13)), (((self.a13 * other.a21) + (self.a23 * other.a22)
    ) + (self.a33 * other.a23)),
    (((self.a13 * other.a31) + (self.a23 * other.a32)) +
    (self.a33 * other.a33)))
end;

procedure TPerspectiveTransform.transformPoints(points: TArray<Single>);
var
  max, i: Integer;
  x, y, denominator: Single;
begin
  max := Length(points);
  a11 := self.a11;
  a12 := self.a12;
  a13 := self.a13;
  a21 := self.a21;
  a22 := self.a22;
  a23 := self.a23;
  a31 := self.a31;
  a32 := self.a32;
  a33 := self.a33;
  i := 0;
  while ((i < max)) do
  begin
    x := points[i];
    y := points[(i + 1)];
    denominator := (((a13 * x) + (a23 * y)) + a33);
    points[i] := ((((a11 * x) + (a21 * y)) + a31) / denominator);
    points[(i + 1)] := ((((a12 * x) + (a22 * y)) + a32) / denominator);
    inc(i, 2)
  end
end;

procedure TPerspectiveTransform.transformPoints(xValues: TArray<Single>;
  yValues: TArray<Single>);
var
  n, i: Integer;
  x, y, denominator: Single;

begin
  n := Length(xValues);
  i := 0;
  while ((i < n)) do
  begin
    x := xValues[i];
    y := yValues[i];
    denominator := (((self.a13 * x) + (self.a23 * y)) + self.a33);
    xValues[i] := ((((self.a11 * x) + (self.a21 * y)) + self.a31) /
      denominator);
    yValues[i] := ((((self.a12 * x) + (self.a22 * y)) + self.a32) /
      denominator);
    inc(i)
  end
end;

end.
