unit ResultPoint;

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

uses SysUtils, MathUtils;

type

  byteArray = array [0 .. 3] of byte;

  TResultPoint = class
  private
    Fx, Fy: single;
    bytesX, bytesY: byteArray;
    FToString: string;
    class function crossProductZ(pointA, pointB, pointC: TResultPoint)
      : single; static;

  public
    constructor Create(pX, pY: single);
    function Equals(other: TObject): Boolean; override;
    function GetHashCode(): Integer; override;
    function ToString(): String; override;
    class procedure OrderBestPatterns(patterns: TArray<TResultPoint>); static;
    class function Distance(pattern1, pattern2: TResultPoint): single; static;

    property x: single read Fx write Fx;
    property y: single read Fy write Fy;
  end;

  /// <summary> Callback which is invoked when a possible result point (significant
  /// point in the barcode image such as a corner) is found.
  ///
  /// </summary>
  /// <seealso cref="DecodeHintType.NEED_RESULT_POINT_CALLBACK">
  /// </seealso>
type

  TResultPointCallback = procedure(point: TResultPoint) of Object;
  // The method- pointer

implementation

{ TResultPoint }

constructor TResultPoint.Create(pX, pY: single);

begin
  Fx := pX;
  Fy := pY;
  bytesX := byteArray(pX);
  bytesY := byteArray(pY);
end;

class function TResultPoint.crossProductZ(pointA, pointB,
  pointC: TResultPoint): single;
var
  bX, bY: single;
begin
  bX := pointB.x;
  bY := pointB.y;
  result := ((pointC.x - bX) * (pointA.y - bY)) -
    ((pointC.y - bY) * (pointA.x - bX));
end;

class function TResultPoint.Distance(pattern1, pattern2: TResultPoint): single;
begin
  result := TMathUtils.Distance(pattern1.x, pattern1.y, pattern2.x, pattern2.y);
end;

function TResultPoint.Equals(other: TObject): Boolean;
var
  otherPoint: TResultPoint;
begin

  otherPoint := other as TResultPoint;
  if (otherPoint = nil) then
  begin
    result := false;
    Exit;
  end;

  result := ((otherPoint.x = Fx) and (otherPoint.y = Fy))

end;

function TResultPoint.GetHashCode: Integer;
begin
  result := 31 * ((bytesX[0] shl 24) + (bytesX[1] shl 16) + (bytesX[2] shl 8) +
    bytesX[3]) + (bytesY[0] shl 24) + (bytesY[1] shl 16) + (bytesY[2] shl 8) +
    bytesY[3];
end;

class procedure TResultPoint.OrderBestPatterns(patterns: TArray<TResultPoint>);
var
  zeroOneDistance, oneTwoDistance, zeroTwoDistance: single;
  pointA, pointB, pointC, temp: TResultPoint;

begin
  // Find distances between pattern centers
  zeroOneDistance := Distance(patterns[0], patterns[1]);
  oneTwoDistance := Distance(patterns[1], patterns[2]);
  zeroTwoDistance := Distance(patterns[0], patterns[2]);

  // Assume one closest to other two is B; A and C will just be guesses at first
  if ((oneTwoDistance >= zeroOneDistance) and
    (oneTwoDistance >= zeroTwoDistance)) then
  begin
    pointB := patterns[0];
    pointA := patterns[1];
    pointC := patterns[2];
  end
  else if ((zeroTwoDistance >= oneTwoDistance) and
    (zeroTwoDistance >= zeroOneDistance)) then
  begin
    pointB := patterns[1];
    pointA := patterns[0];
    pointC := patterns[2];
  end
  else
  begin
    pointB := patterns[2];
    pointA := patterns[0];
    pointC := patterns[1];
  end;

  // Use cross product to figure out whether A and C are correct or flipped.
  // This asks whether BC x BA has a positive z component, which is the arrangement
  // we want for A, B, C. If it's negative, then we've got it flipped around and
  // should swap A and C.
  if (crossProductZ(pointA, pointB, pointC) < 0) then
  begin
    temp := pointA;
    pointA := pointC;
    pointC := temp;
  end;

  patterns[0] := pointA;
  patterns[1] := pointB;
  patterns[2] := pointC;
end;

function TResultPoint.ToString: String;
begin
  if (FToString = '') then
  begin
    FToString := Format('(%g),(%g)', [Fx, Fy]);
  end;

  result := FToString;
end;

end.
