{
  * Copyright 2007 ZXing authors
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

  * Original Author: Sean Owen
  * Ported from ZXING Java Source: www.Redivivus.in (suraj.supekar@redivivus.in)
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Common.PerspectiveTransform;

interface

uses
  System.SysUtils;

type
  /// <summary> <p>This class implements a perspective transform in two dimensions. Given four source and four
  /// destination points, it will compute the transformation implied between them. The code is based
  /// directly upon section 3.4.2 of George Wolberg's "Digital Image Warping"; see pages 54-56.</p>
  /// </summary>
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

    function buildAdjoint(): TPerspectiveTransform;
    function times(const other: TPerspectiveTransform): TPerspectiveTransform;
  public
    constructor Create(const a11, a21, a31, a12, a22, a32, a13,
      a23, a33: Single);

    class function quadrilateralToQuadrilateral(const x0, y0, x1, y1, x2, y2,
      x3, y3, x0p, y0p, x1p, y1p, x2p, y2p,
      x3p, y3p: Single): TPerspectiveTransform; static;

    class function quadrilateralToSquare(x0: Single; y0: Single; x1: Single;
      y1: Single; x2: Single; y2: Single; x3: Single; y3: Single)
      : TPerspectiveTransform; static;

    class function squareToQuadrilateral(const x0, y0, x1, y1, x2, y2,
      x3, y3: Single): TPerspectiveTransform; static;

    procedure transformPoints(var points: TArray<Single>); overload;

    /// <summary>Convenience method, not optimized for performance. </summary>
    procedure transformPoints(const xValues, yValues: TArray<Single>); overload;
  end;

implementation

constructor TPerspectiveTransform.Create(const a11, a21, a31, a12, a22, a32,
  a13, a23, a33: Single);
begin
  Self.a11 := a11;
  Self.a12 := a12;
  Self.a13 := a13;
  Self.a21 := a21;
  Self.a22 := a22;
  Self.a23 := a23;
  Self.a31 := a31;
  Self.a32 := a32;
  Self.a33 := a33;
end;

class function TPerspectiveTransform.quadrilateralToQuadrilateral(const x0, y0,
  x1, y1, x2, y2, x3, y3, x0p, y0p, x1p, y1p, x2p, y2p,
  x3p, y3p: Single): TPerspectiveTransform;
var
  qToS,
  SToQ : TPerspectiveTransform;
begin
  qToS := nil;
  SToQ := nil;
  try
    qToS := TPerspectiveTransform.quadrilateralToSquare(x0, y0, x1, y1, x2,
      y2, x3, y3);
    SToQ := TPerspectiveTransform.squareToQuadrilateral(x0p, y0p, x1p, y1p, x2p,
      y2p, x3p, y3p);

    Result := SToQ.times(qToS);
  finally

    //if (SToQ <> nil)
    //then
       SToQ.Free;

    //if (qToS <> nil)
    //then
       qToS.Free;
  end;
end;

class function TPerspectiveTransform.quadrilateralToSquare(x0: Single;
  y0: Single; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single;
  y3: Single): TPerspectiveTransform;
var
  SToQ: TPerspectiveTransform;
begin
  SToQ := nil;
  // Here, the adjoint serves as the inverse:
  try
    SToQ := TPerspectiveTransform.squareToQuadrilateral(x0, y0, x1, y1, x2,
      y2, x3, y3);

    Result := SToQ.buildAdjoint;
  finally
    //if (SToQ <> nil)
    //then
       SToQ.Free;
  end;
end;

class function TPerspectiveTransform.squareToQuadrilateral(const x0, y0,
  x1, y1, x2, y2, x3, y3: Single): TPerspectiveTransform;
var
  dx1, dx2,
  dx3, dy1,
  dy2, dy3,
  a13, a23,
  denominator: Single;
begin
  dx3 := (((x0 - x1) + x2) - x3);
  dy3 := (((y0 - y1) + y2) - y3);

  if ((dx3 = 0) and (dy3 = 0)) then
  begin
    Result := TPerspectiveTransform.Create((x1 - x0), (x2 - x1), x0, (y1 - y0),
      (y2 - y1), y0, 0, 0, 1);
  end else
  begin
    dx1 := (x1 - x2);
    dx2 := (x3 - x2);
    dy1 := (y1 - y2);
    dy2 := (y3 - y2);
    denominator := ((dx1 * dy2) - (dx2 * dy1));
    if denominator = 0 then
    begin
      a13 := 0;
      a23 := 0;
    end
    else
    begin
      a13 := (((dx3 * dy2) - (dx2 * dy3)) / denominator);
      a23 := (((dx1 * dy3) - (dx3 * dy1)) / denominator);
    end;
    Result := TPerspectiveTransform.Create(((x1 - x0) + (a13 * x1)),
      ((x3 - x0) + (a23 * x3)), x0, ((y1 - y0) + (a13 * y1)),
      ((y3 - y0) + (a23 * y3)), y0, a13, a23, 1);
  end;
end;

procedure TPerspectiveTransform.transformPoints(var points: TArray<Single>);
var
  max, i: Integer;
  x, y,
  denominator: Single;
begin


  a11 := Self.a11;
  a12 := Self.a12;
  a13 := Self.a13;
  a21 := Self.a21;
  a22 := Self.a22;
  a23 := Self.a23;
  a31 := Self.a31;
  a32 := Self.a32;
  a33 := Self.a33;
  i := 0;
  max := Length(points) - 1;
  while ((i < max)) do
  begin
    x := points[i];
    y := points[(i + 1)];
    denominator := (((a13 * x) + (a23 * y)) + a33);
    points[i] := ((((a11 * x) + (a21 * y)) + a31) / denominator);
    points[(i + 1)] := ((((a12 * x) + (a22 * y)) + a32) / denominator);
    Inc(i, 2);
  end
end;

procedure TPerspectiveTransform.transformPoints(const xValues,
  yValues: TArray<Single>);
var
  n, i: Integer;
  x, y, denominator: Single;
begin
  n := Length(xValues);
  for i := 0 to Pred(n) do
  begin
    x := xValues[i];
    y := yValues[i];
    denominator := (((self.a13 * x) + (self.a23 * y)) + self.a33);
    xValues[i] := ((((self.a11 * x) + (self.a21 * y)) + self.a31) /
      denominator);
    yValues[i] := ((((self.a12 * x) + (self.a22 * y)) + self.a32) /
      denominator);
  end
end;

function TPerspectiveTransform.buildAdjoint(): TPerspectiveTransform;
begin
  // Adjoint is the transpose of the cofactor matrix:
  Result := TPerspectiveTransform.Create(a22 * a33 - a23 * a32,
                                         a23 * a31 - a21 * a33,
                                         a21 * a32 - a22 * a31,
                                         a13 * a32 - a12 * a33,
                                         a11 * a33 - a13 * a31,
                                         a12 * a31 - a11 * a32,
                                         a12 * a23 - a13 * a22,
                                         a13 * a21 - a11 * a23,
                                         a11 * a22 - a12 * a21);
end;

function TPerspectiveTransform.times(
  const other: TPerspectiveTransform): TPerspectiveTransform;
begin
  Result := TPerspectiveTransform.Create(a11 * other.a11 + a21 * other.a12 + a31 * other.a13,
                                         a11 * other.a21 + a21 * other.a22 + a31 * other.a23,
                                         a11 * other.a31 + a21 * other.a32 + a31 * other.a33,
                                         a12 * other.a11 + a22 * other.a12 + a32 * other.a13,
                                         a12 * other.a21 + a22 * other.a22 + a32 * other.a23,
                                         a12 * other.a31 + a22 * other.a32 + a32 * other.a33,
                                         a13 * other.a11 + a23 * other.a12 + a33 * other.a13,
                                         a13 * other.a21 + a23 * other.a22 + a33 * other.a23,
                                         a13 * other.a31 + a23 * other.a32 + a33 * other.a33);
end;

end.
