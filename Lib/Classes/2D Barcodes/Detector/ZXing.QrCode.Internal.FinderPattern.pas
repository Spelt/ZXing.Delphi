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
  /// A finder pattern of the QR Code detector of the Java original: its
  /// center, estimated module size and the number of times it was seen. An
  /// interfaced object, so that the lists of the finder hold and free them.
  /// The actual implementation is in unit ZXing.QrCode.Internal.FinderPatternImplementation.
  /// </summary>
  IFinderPattern = interface
    ['{EF95BB2F-8863-4A83-840E-699779592EC0}']
    function GetX: Single;
    function GetY: Single;
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
    /// <summary>The center as a result point.</summary>
    function AsResultPoint: TResultPoint;
    property count : Integer read GetCount  write SetCount;
    property X: Single read GetX;
    property Y: Single read GetY;
  end;


  /// <summary>
  ///  contains all static methods for using IFinderPattern instances
  /// </summary>
  TFinderPatternHelpers= class
    class function  CreateFinderPattern(const posX, posY, estimatedModuleSize: Single;
      const count: Integer):IFinderPattern; overload;
    class function CreateFinderPattern(const posX, posY, estimatedModuleSize: Single):IFinderPattern; overload;

    /// <summary>Like TResultPointHelpers.orderBestPatterns, for three finder
    /// patterns.</summary>
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
  points: TArray<TResultPoint>;
  ordered: TArray<IFinderPattern>;
begin
  // the centers ordered as points, then the patterns in that order
  points := [patterns[0].AsResultPoint, patterns[1].AsResultPoint,
    patterns[2].AsResultPoint];
  TResultPointHelpers.orderBestPatterns(points);
  SetLength(ordered, 3);
  for var i := 0 to 2 do
    for var j := 0 to 2 do
      if (ordered[i] = nil) and (patterns[j].X = points[i].x) and
        (patterns[j].Y = points[i].y) then
      begin
        ordered[i] := patterns[j];
        break;
      end;
  // (the same point twice: keep what is there)
  for var i := 0 to 2 do
    if (ordered[i] = nil) then
      exit;
  for var i := 0 to 2 do
    patterns[i] := ordered[i];
end;

end.
