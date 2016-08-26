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
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.FinderPattern;

interface

uses
  ZXing.ResultPoint;

type

  /// <summary>
  ///  As we did for ResultPoint, we use an interfaced object to implement automatic deallocation
  /// of TAligmnentPattern instances
  /// the actual implementation of this interface is in unit ZXing.QrCode.Internal.FinderPatternImplementation
  /// </summary>
  IFinderPattern = interface(IResultPoint)
    ['{EF95BB2F-8863-4A83-840E-699779592EC0}']
    function GetCount:integer;
    procedure SetCount(value:integer);
    /// <summary> <p>Determines if this finder pattern "about equals" a finder pattern at the stated
    /// position and size -- meaning, it is at nearly the same center with nearly the same size.</p>
    /// </summary>
    function aboutEquals(const moduleSize, i, j: Single): Boolean;

    /// <summary>
    /// Combines this object's current estimate of a finder pattern position and module size
    /// with a new estimate. It returns a new {@code FinderPattern} containing a weighted average
    /// based on count.
    /// </summary>
    /// <param name="i">The i.</param>
    /// <param name="j">The j.</param>
    /// <param name="newModuleSize">New size of the module.</param>
    /// <returns></returns>
    function combineEstimate(const i, j, newModuleSize: Single): IFinderPattern;

    /// <summary>
    /// Gets the size of the estimated module.
    /// </summary>
    /// <value>
    /// The size of the estimated module.
    /// </value>
    function estimatedModuleSize : Single;
    property count : Integer read GetCount  write SetCount;
  end;


  /// <summary>
  ///  contains all static methods for using IFinderPattern instances
  /// </summary>
  TFinderPatternHelpers= class(TResultPointHelpers)
    class function  CreateFinderPattern(const posX, posY, estimatedModuleSize: Single;
      const count: Integer):IFinderPattern; overload;
    class function CreateFinderPattern(const posX, posY, estimatedModuleSize: Single):IFinderPattern; overload;

    class procedure orderBestPatterns(const patterns: TArray<IFinderPattern>); static;

  end;

implementation
uses ZXing.QrCode.Internal.FinderPatternImplementation;

class function  TFinderPatternHelpers.CreateFinderPattern(const posX, posY, estimatedModuleSize: Single; const count: Integer):IFinderPattern;
begin
   result :=  ZXing.QrCode.Internal.FinderPatternImplementation.NewFinderPattern(posX, posY, estimatedModuleSize ,count);
end;

class function TFinderPatternHelpers.CreateFinderPattern(const posX, posY, estimatedModuleSize: Single):IFinderPattern;
begin
   result :=  ZXing.QrCode.Internal.FinderPatternImplementation.NewFinderPattern(posX, posY, estimatedModuleSize);
end;



class procedure TFinderPatternHelpers.orderBestPatterns( const patterns: TArray<IFinderPattern>);
var
  zeroOneDistance,
  oneTwoDistance,
  zeroTwoDistance: Single;
  pointA,
  pointB,
  pointC,
  temp: IFinderPattern;
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
  if (TResultPointHelpers.crossProductZ(pointA, pointB, pointC) < 0) then
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
