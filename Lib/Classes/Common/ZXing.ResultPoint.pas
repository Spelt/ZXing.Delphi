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
  /// Encapsulates a point of interest in an image containing a barcode. Typically, this
  /// would be the location of a finder pattern or the corner of the barcode, for example.
  /// </summary>
  TResultPoint = class
  private
    type
      TSingleArray = array[0..Pred(SizeOf(Single))] of Byte;
    var
      Fx, Fy : Single;
      bytesX,
      bytesY : TSingleArray;
      FToString : String;
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
  protected
    class function crossProductZ(const pointA, pointB,
      pointC: TResultPoint): Single; static;
  public
    constructor Create; overload;
    constructor Create(const pX, pY: Single); overload;
    destructor Destroy; override;

    function Equals(other: TObject): Boolean; override;
    function GetHashCode(): Integer; override;
    function ToString(): String; override;

    class procedure orderBestPatterns(const patterns: TArray<TResultPoint>); static;
    class function distance(const pattern1, pattern2: TResultPoint): Single; static;

    property x: Single read Fx write SetX;
    property y: Single read Fy write SetY;
  end;

  /// <summary> Callback which is invoked when a possible result point (significant
  /// point in the barcode image such as a corner) is found.
  ///
  /// </summary>
  /// <seealso cref="TDecodeHintType.NEED_RESULT_POINT_CALLBACK">
  /// </seealso>
  TResultPointCallback = procedure(const point: TResultPoint) of object;
  TResultPointEventObject = class(TObject)
  private
    FResultPointCallback : TResultPointCallback;
  public
    constructor Create(const AEvent: TResultPointCallback);

    property Event : TResultPointCallback read FResultPointCallback;
  end;


implementation

{ TResultPoint }

/// <summary>
/// Initializes a new instance of the <see cref="TResultPoint"/> class.
/// </summary>
constructor TResultPoint.Create;
begin
  inherited;
end;

// <summary>
/// Initializes a new instance of the <see cref="TResultPoint"/> class.
/// </summary>
/// <param name="x">The x.</param>
/// <param name="y">The y.</param>
constructor TResultPoint.Create(const pX, pY: Single);
begin
  inherited Create;

  Fx := pX;
  Fy := pY;
  bytesX := TSingleArray(pX);
  bytesY := TSingleArray(pY);
end;

destructor TResultPoint.Destroy;
var n:Single;
begin
  n:=0;
  bytesX := TSingleArray(n);
  bytesY := TSingleArray(n);
  inherited;
end;

procedure TResultPoint.SetX(const AValue: Single);
begin
  if (AValue <> Fx) then
  begin
    Fx := AValue;
    bytesX := TSingleArray(Fx);
  end;
end;

procedure TResultPoint.SetY(const AValue: Single);
begin
  if (AValue <> Fy) then
  begin
    Fy := AValue;
    bytesY := TSingleArray(Fy);
  end;
end;

/// <summary>
/// Returns the z component of the cross product between vectors BC and BA.
/// </summary>
class function TResultPoint.crossProductZ(const pointA, pointB,
  pointC: TResultPoint): Single;
var
  bX, bY: single;
begin
  bX := pointB.x;
  bY := pointB.y;
  Result := ((pointC.x - bX) * (pointA.y - bY)) -
            ((pointC.y - bY) * (pointA.x - bX));
end;

/// <summary>
/// calculates the distance between two points
/// </summary>
/// <param name="pattern1">first pattern</param>
/// <param name="pattern2">second pattern</param>
/// <returns>
/// distance between two points
/// </returns>
class function TResultPoint.distance(const pattern1,
  pattern2: TResultPoint): Single;
begin
  Result := TMathUtils.distance(pattern1.x,
              pattern1.y, pattern2.x, pattern2.y);
end;

/// <summary>
/// Determines whether the specified <see cref="System.TObject"/> is equal to this instance.
/// </summary>
/// <param name="other">The <see cref="System.TObject"/> to compare with this instance.</param>
/// <returns>
///   <c>true</c> if the specified <see cref="System.TObject"/> is equal to this instance; otherwise, <c>false</c>.
/// </returns>
function TResultPoint.Equals(other: TObject): Boolean;
var
  otherPoint: TResultPoint;
begin
  otherPoint := other as TResultPoint;
  if (otherPoint = nil)
  then
     Result := false
  else
     Result := ((otherPoint.x = Fx) and (otherPoint.y = Fy));
end;

/// <summary>
/// Returns a hash code for this instance.
/// </summary>
/// <returns>
/// A hash code for this instance, suitable for use in hashing algorithms and data structures like a hash table.
/// </returns>
function TResultPoint.GetHashCode: Integer;
begin
  Result := 31 * ((bytesX[0] shl 24) + (bytesX[1] shl 16) + (bytesX[2] shl 8) + bytesX[3]) +
                  (bytesY[0] shl 24) + (bytesY[1] shl 16) + (bytesY[2] shl 8) + bytesY[3];
end;

/// <summary>
/// Orders an array of three ResultPoints in an order [A,B,C] such that AB is less than AC and
/// BC is less than AC and the angle between BC and BA is less than 180 degrees.
/// </summary>
/// <param name="patterns">array of three <see cref="TResultPoint" /> to order</param>
class procedure TResultPoint.orderBestPatterns(
  const patterns: TArray<TResultPoint>);
var
  zeroOneDistance,
  oneTwoDistance,
  zeroTwoDistance: Single;
  pointA,
  pointB,
  pointC,
  temp: TResultPoint;
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

/// <summary>
/// Returns a <see cref="System.String"/> that represents this instance.
/// </summary>
/// <returns>
/// A <see cref="System.String"/> that represents this instance.
/// </returns>
function TResultPoint.ToString: String;
begin
  if (FToString = '')
  then
     FToString := Format('(%g),(%g)', [Fx, Fy]);
  Result := FToString;
end;

{ TResultPointEventObject }

constructor TResultPointEventObject.Create(const AEvent: TResultPointCallback);
begin
  Self.FResultPointCallback := AEvent;
end;

end.