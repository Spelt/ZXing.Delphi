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

  byteArray = array of byte;

  IResultPoint = interface
    ['{224BDE0F-391E-4F23-AB91-AA3696FB8C9E}']
    // property accessors (must be functions and procedures, for interfaces)
    function GetX: single;
    function GetY: single;
    procedure SetX(const Value: single);
    procedure SetY(const Value: single);

    function Equals(other: IResultPoint): Boolean;
    function GetHashCode(): Integer;
    function ToString(): String;

    property x: single read GetX write SetX;
    property y: single read GetY write SetY;
  end;

  /// <summary> Callback which is invoked when a possible result point (significant
  /// point in the barcode image such as a corner) is found.
  ///
  /// </summary>
  /// <seealso cref="DecodeHintType.NEED_RESULT_POINT_CALLBACK">
  /// </seealso>

type
  TResultPointCallback = procedure(point: IResultPoint) of Object;
  // The method- pointer

  TResultPointHelpers = class
  private
     class function crossProductZ(const pointA, pointB, pointC: IResultPoint): single;
  public
     class function CreateResultPoint  (const pX, pY: single):IResultPoint;
     class procedure OrderBestPatterns(patterns: TArray<IResultPoint>); static;
     class function Distance(const pattern1, pattern2: IResultPoint): single; static;
  end;



implementation
uses ResultPointImpl;

{ TResultPoint }


class function TResultPointHelpers.CreateResultPoint  (const pX, pY: single):IResultPoint;
begin
    result := ResultPointImpl.NewResultPoint(px,py);
end;

class function TResultPointHelpers.crossProductZ(const pointA, pointB,  pointC: IResultPoint): single;
var
  bX, bY: single;
begin
  bX := pointB.x;
  bY := pointB.y;
  result := ((pointC.x - bX) * (pointA.y - bY)) -
            ((pointC.y - bY) * (pointA.x - bX));
end;

class function TResultPointHelpers.Distance(const pattern1, pattern2: IResultPoint): single;
begin
  result := TMathUtils.Distance(pattern1.x, pattern1.y, pattern2.x, pattern2.y);
end;

class procedure TResultPointHelpers.OrderBestPatterns(patterns: TArray<IResultPoint>);
var
  zeroOneDistance, oneTwoDistance, zeroTwoDistance: single;
  pointA, pointB, pointC, temp: IResultPoint;

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


end.
