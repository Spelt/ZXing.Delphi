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

  * Original Author: Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.ResultPoint;

interface

uses
  System.SysUtils,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary>
  /// To mimic "garbage collection" (or ARC) with old-gen compilers we have to use
  /// interfaces. IResultPoint is the interface that maps TResultPoint which is now
  /// implemented as a TInterfacedObject descendant which supports automatic deallocation
  /// based on reference counting of interface variables.
  /// See ZXing.ResultPointImplementation to see the actual implementation of this interface.
  /// since we are using automatic reference counting, we don't need the Clone method any more.
  /// </summary>

  IResultPoint = interface
     ['{1E60EDAB-0387-440C-9D8E-4E0CC772C820}']
    // property accessors, for interfaces, MUST be functions or procedures (interfaces cannot contain data)
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    function GetX: Single;
    function GetY: Single;

    function Equals(other: TObject): Boolean;
    function GetHashCode(): Integer;
    function ToString(): String;

    property x: Single read GetX write SetX;
    property y: Single read GetY write SetY;
  end;

  /// <summary>
  /// in order to implement TResultPoint as an interface all static methods
  /// have been moved in this static class.
  /// this class contains also the CreateResultPoint method that must be used
  /// in place of the actual class constructor
  /// </summary>
  TResultPointHelpers = class
    protected
       /// <summary>
       /// Returns the z component of the cross product between vectors BC and BA.
       /// </summary>
       class function crossProductZ(const pointA, pointB, pointC: IResultPoint): Single; static;
    public
       /// <summary>
       /// Initializes a new instance of <see cref="TResultPoint"/> and returns
       /// its <see cref="IResultPoint"/> interface
       /// </summary>
       class function CreateResultPoint:IResultPoint; overload;
       /// <summary>
       /// Initializes a new instance of  <see cref="TResultPoint"/> and returns
       /// its <see cref="IResultPoint"/> interface
       /// </summary>
       /// <param name="x">The x.</param>
       /// <param name="y">The y.</param>
       class function CreateResultPoint(const pX, pY: Single):IResultPoint; overload;
       /// <summary>
       /// Orders an array of three ResultPoints in an order [A,B,C] such that AB is less than AC and
       /// BC is less than AC and the angle between BC and BA is less than 180 degrees.
       /// </summary>
       /// <param name="patterns">array of three <see cref="IResultPoint" /> to order</param>
       class procedure orderBestPatterns(const patterns : TArray<IResultPoint>); static;
       /// <summary>
       /// calculates the distance between two points
       /// </summary>
       /// <param name="pattern1">first pattern</param>
       /// <param name="pattern2">second pattern</param>
       /// <returns>
       /// distance between two points
       /// </returns>
       class function distance(const pattern1, pattern2: IResultPoint) : Single; static;
  end;



  /// <summary> Callback which is invoked when a possible result point (significant
  /// point in the barcode image such as a corner) is found.
  ///
  /// </summary>
  /// <seealso cref="TDecodeHintType.NEED_RESULT_POINT_CALLBACK">
  /// </seealso>
  TResultPointCallback = procedure(const point: IResultPoint) of object;

  TResultPointEventObject = class(TObject)
  private
    FResultPointCallback: TResultPointCallback;
  public
    constructor Create(const AEvent: TResultPointCallback);

    property Event: TResultPointCallback read FResultPointCallback;
  end;

implementation
uses ZXing.ResultPointImplementation;

class function TResultPointHelpers.CreateResultPoint:IResultPoint;
begin
   result := ZXing.ResultPointImplementation.NewResultPoint;
end;

class function TResultPointHelpers.CreateResultPoint(const pX, pY: Single):IResultPoint;
begin
   result := ZXing.ResultPointImplementation.NewResultPoint(px,py);
end;


class function TResultPointHelpers.crossProductZ(const pointA, pointB,
  pointC: IResultPoint): Single;
var
  bX, bY: Single;
begin
  bX := pointB.x;
  bY := pointB.y;
  Result := ((pointC.x - bX) * (pointA.y - bY)) -
    ((pointC.y - bY) * (pointA.x - bX));
end;


class function TResultPointHelpers.distance(const pattern1,
  pattern2: IResultPoint): Single;
begin
  Result := TMathUtils.distance(pattern1.x, pattern1.y, pattern2.x, pattern2.y);
end;


class procedure TResultPointHelpers.orderBestPatterns(const patterns
  : TArray<IResultPoint>);
var
  zeroOneDistance, oneTwoDistance, zeroTwoDistance: Single;
  pointA, pointB, pointC, temp: IResultPoint;
begin
  // Find distances between pattern centers
  zeroOneDistance := distance(patterns[0], patterns[1]);
  oneTwoDistance := distance(patterns[1], patterns[2]);
  zeroTwoDistance := distance(patterns[0], patterns[2]);

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


{ TResultPointEventObject }

constructor TResultPointEventObject.Create(const AEvent: TResultPointCallback);
begin
  Self.FResultPointCallback := AEvent;
end;


end.
