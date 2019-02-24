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

unit ZXing.QrCode.Internal.FinderPatternImplementation;

interface
uses
  ZXing.QrCode.Internal.FinderPattern;


function NewFinderPattern(const posX, posY, estimatedModuleSize: Single; const count: Integer):IFinderPattern; overload;
function NewFinderPattern(const posX, posY, estimatedModuleSize: Single):IFinderPattern; overload;


implementation
uses ZXing.ResultPoint,ZXing.ResultPointImplementation;

{ TFinderPattern }


type
  /// <summary>
  /// <p>Encapsulates a finder pattern, which are the three square patterns found in
  /// the corners of QR Codes. It also encapsulates a count of similar finder patterns,
  /// as a convenience to the finder's bookkeeping.</p>
  /// </summary>
  TFinderPattern = class(TResultPoint,IResultPoint,IFinderPattern)
  private
    Fcount: Integer;
    FestimatedModuleSize: Single;

    function GetCount:integer;
    procedure SetCount(value:integer);

    constructor Create(const posX, posY, estimatedModuleSize: Single;
      const count: Integer); overload;
    constructor Create(const posX, posY, estimatedModuleSize: Single); overload;

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
    property count : Integer read GetCount write SetCount;
  end;

constructor TFinderPattern.Create(const posX, posY,
  estimatedModuleSize: Single; const count: Integer);
begin
  inherited Create(posX, posY);

  FestimatedModuleSize := estimatedModuleSize;
  Fcount := count;
end;

constructor TFinderPattern.Create(const posX, posY,
  estimatedModuleSize: Single);
begin
  Self.Create(posX, posY, estimatedModuleSize, 1);
end;

function TFinderPattern.estimatedModuleSize: Single;
begin
  result := FestimatedModuleSize;
end;

function TFinderPattern.GetCount: integer;
begin
   result := FCount;
end;

function TFinderPattern.aboutEquals(const moduleSize, i, j: Single): Boolean;
var
  moduleSizeDiff : Single;
begin
  Result := false;

  if ((Abs(i - y) <= moduleSize) and (Abs(j - x) <= moduleSize)) then
  begin
    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);

    Result := ((moduleSizeDiff <= 1) or
      (moduleSizeDiff <= self.estimatedModuleSize));
  end;
end;

procedure TFinderPattern.SetCount(value: integer);
begin
   FCount := value;
end;

function TFinderPattern.combineEstimate(const i, j,
  newModuleSize: Single): IFinderPattern;
var
  combinedCount: Integer;
  combinedX, combinedY: Single;
begin
  combinedCount := (Self.count + 1);
  combinedX := ((Self.count * x) + j) / combinedCount;
  combinedY := ((Self.count * y) + i) / combinedCount;
  Result := TFinderPattern.Create(combinedX, combinedY,
    (((Self.count * Self.estimatedModuleSize) + newModuleSize) / combinedCount),
    combinedCount)
end;


function NewFinderPattern(const posX, posY, estimatedModuleSize: Single; const count: Integer):IFinderPattern;
begin
   result := TFinderPattern.Create( posX, posY, estimatedModuleSize,count);
end;

function NewFinderPattern(const posX, posY, estimatedModuleSize: Single):IFinderPattern;
begin
   result := TFinderPattern.Create( posX, posY, estimatedModuleSize);
end;


end.
