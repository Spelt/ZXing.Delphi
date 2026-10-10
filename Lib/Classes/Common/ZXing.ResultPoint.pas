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
  /// A point of interest in the image containing a barcode: the location of
  /// a finder pattern, a corner of the symbol, the ends of a 1D code. A
  /// record (a value): nothing to free, no reference counting. A default
  /// (zeroed) record is "no point", IsNone, where nil was used before; the
  /// points of a result that are not known are such points.
  /// </summary>
  TResultPoint = record
  private
    FX, FY: Single;
    // 0 no point, 1 a point; an Integer so that the record has no padding
    // bytes (a TDictionary with points as keys compares the bytes)
    FState: Integer;
  public
    class function Create(const x, y: Single): TResultPoint; static; inline;
    /// <summary>No point (nil before).</summary>
    class function None: TResultPoint; static; inline;
    function IsNone: Boolean; inline;
    function HasValue: Boolean; inline;
    /// <summary>The same point (or both no point).</summary>
    function Equals(const other: TResultPoint): Boolean;
    function ToString: string;
    class operator Equal(const a, b: TResultPoint): Boolean;
    class operator NotEqual(const a, b: TResultPoint): Boolean;

    property x: Single read FX write FX;
    property y: Single read FY write FY;
  end;

  /// <summary>The name of the interface of before, for existing code: a
  /// result point is a record now.</summary>
  IResultPoint = TResultPoint;

  /// <summary>
  /// The static methods for result points, and the constructor of before.
  /// </summary>
  TResultPointHelpers = class
    public
       /// <summary>
       /// Returns the z component of the cross product between vectors BC and BA.
       /// </summary>
       class function crossProductZ(const pointA, pointB, pointC: TResultPoint): Single; static;
       /// <summary>The point 0, 0 (as before).</summary>
       class function CreateResultPoint: TResultPoint; overload;
       /// <summary>The point x, y.</summary>
       class function CreateResultPoint(const pX, pY: Single): TResultPoint; overload;
       /// <summary>
       /// Orders an array of three ResultPoints in an order [A,B,C] such that AB is less than AC and
       /// BC is less than AC and the angle between BC and BA is less than 180 degrees.
       /// </summary>
       /// <param name="patterns">array of three points to order</param>
       class procedure orderBestPatterns(const patterns: TArray<TResultPoint>); static;
       /// <summary>
       /// calculates the distance between two points
       /// </summary>
       class function distance(const pattern1, pattern2: TResultPoint): Single; static;
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
    FResultPointCallback: TResultPointCallback;
  public
    constructor Create(const AEvent: TResultPointCallback);

    property Event: TResultPointCallback read FResultPointCallback;
  end;

implementation

{ TResultPoint }

class function TResultPoint.Create(const x, y: Single): TResultPoint;
begin
  Result.FX := x;
  Result.FY := y;
  Result.FState := 1;
end;

class function TResultPoint.None: TResultPoint;
begin
  Result.FX := 0;
  Result.FY := 0;
  Result.FState := 0;
end;

function TResultPoint.IsNone: Boolean;
begin
  Result := (FState = 0);
end;

function TResultPoint.HasValue: Boolean;
begin
  Result := (FState <> 0);
end;

function TResultPoint.Equals(const other: TResultPoint): Boolean;
begin
  if (FState = 0) or (other.FState = 0) then
    Result := (FState = other.FState)
  else
    Result := (FX = other.FX) and (FY = other.FY);
end;

function TResultPoint.ToString: string;
begin
  if (FState = 0) then
    Result := '(none)'
  else
    Result := Format('(%g),(%g)', [FX, FY]);
end;

class operator TResultPoint.Equal(const a, b: TResultPoint): Boolean;
begin
  Result := a.Equals(b);
end;

class operator TResultPoint.NotEqual(const a, b: TResultPoint): Boolean;
begin
  Result := not a.Equals(b);
end;

{ TResultPointHelpers }

class function TResultPointHelpers.CreateResultPoint: TResultPoint;
begin
  Result := TResultPoint.Create(0, 0);
end;

class function TResultPointHelpers.CreateResultPoint(const pX, pY: Single): TResultPoint;
begin
  Result := TResultPoint.Create(pX, pY);
end;

class function TResultPointHelpers.crossProductZ(const pointA, pointB,
  pointC: TResultPoint): Single;
var
  bX, bY: Single;
begin
  bX := pointB.x;
  bY := pointB.y;
  Result := ((pointC.x - bX) * (pointA.y - bY)) -
    ((pointC.y - bY) * (pointA.x - bX));
end;

class function TResultPointHelpers.distance(const pattern1,
  pattern2: TResultPoint): Single;
begin
  Result := TMathUtils.distance(pattern1.x, pattern1.y, pattern2.x, pattern2.y);
end;

class procedure TResultPointHelpers.orderBestPatterns(const patterns
  : TArray<TResultPoint>);
var
  zeroOneDistance, oneTwoDistance, zeroTwoDistance: Single;
  pointA, pointB, pointC, temp: TResultPoint;
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
