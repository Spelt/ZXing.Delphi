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
  * Delphi Implementation by E.Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.AlignmentPatternImplementation;


interface

uses
  ZXing.QrCode.Internal.AlignmentPattern,
  ZXing.ResultPoint,
  ZXing.ResultPointImplementation;

// this is the function that actually creates IAlignmentPattern instances.
// the actual implementation of the interface is totally hidden to all other units,
// since even its declaration is in the implementation section of this unit
function NewAlignmentPattern(const posX, posY, estimatedModuleSize: Single):IAlignmentPattern;


implementation

{ TAlignmentPattern }


type
  /// <summary> <p>Encapsulates an alignment pattern, which are the smaller square patterns found in
  /// all but the simplest QR Codes.</p>
  ///
  /// </summary>
  TAlignmentPattern = class sealed(TResultPoint,IResultPoint,IAlignmentPattern)
  private
    estimatedModuleSize: Single;
    constructor Create(const posX, posY, estimatedModuleSize: Single);

    /// <summary> <p>Determines if this alignment pattern "about equals" an alignment pattern at the stated
    /// position and size -- meaning, it is at nearly the same center with nearly the same size.</p>
    /// </summary>
    function aboutEquals(const moduleSize, i, j: Single): Boolean;

    /// <summary>
    /// Combines this object's current estimate of a finder pattern position and module size
    /// with a new estimate. It returns a new {@code FinderPattern} containing an average of the two.
    /// </summary>
    /// <param name="i">The i.</param>
    /// <param name="j">The j.</param>
    /// <param name="newModuleSize">New size of the module.</param>
    /// <returns></returns>
    function combineEstimate(const i, j,
      newModuleSize: Single): IAlignmentPattern;
  end;


constructor TAlignmentPattern.Create(const posX, posY,
  estimatedModuleSize: Single);
begin
  inherited Create(posX, posY);

  Self.estimatedModuleSize := estimatedModuleSize;
end;

function TAlignmentPattern.aboutEquals(const moduleSize, i, j: Single): Boolean;
var
  moduleSizeDiff: Single;
begin
  if ((Abs(i - Self.y) <= moduleSize) and
      (Abs(j - Self.x) <= moduleSize)) then
  begin
    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);
    Result := ((moduleSizeDiff <= 1) or
               (moduleSizeDiff <= self.estimatedModuleSize));
  end else Result := false;
end;

function TAlignmentPattern.combineEstimate(const i, j,
  newModuleSize: Single): IAlignmentPattern;
var
  combinedX,
  combinedY,
  combinedModuleSize : Single;
begin
  combinedX := (self.x + j) / 2.0;
  combinedY := (self.y + i) / 2.0;
  combinedModuleSize := (estimatedModuleSize + newModuleSize) / 2.0;
  Result := TAlignmentPattern.Create(combinedX, combinedY, combinedModuleSize);
end;




function NewAlignmentPattern(const posX, posY, estimatedModuleSize: Single):IAlignmentPattern;
begin
   result := TAlignmentPattern.Create(posX,posY,estimatedModuleSize);
end;

end.
