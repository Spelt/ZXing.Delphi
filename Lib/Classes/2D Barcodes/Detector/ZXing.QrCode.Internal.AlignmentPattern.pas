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

unit ZXing.QrCode.Internal.AlignmentPattern;
interface
uses ZXing.ResultPoint;

type
  /// <summary>
  ///  As we did for ResultPoint, we use an interfaced object to implement automatic deallocation
  /// of TAligmnentPattern instances
  /// the actual implementation of this interface is in unit ZXing.QrCode.Internal.AlignmentPatternImplementation
  /// </summary>
  IAlignmentPattern = interface(IResultPoint)
     ['{8C0A5D01-9620-42B6-BE3E-9C40717B5B38}']
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
    function combineEstimate(const i, j, newModuleSize: Single): IAlignmentPattern;
  end;


  /// <summary>
  ///  contains all static methods for using IAlignmentPatterns instances
  /// </summary>
  TAlignmentPatternHelpers = class
    /// <summary>
    ///  IAlignmentPattern instances must be obtained by calling this function
    /// </summary>
    class function CreateAlignmentPattern(const posX, posY, estimatedModuleSize: Single):IAlignmentPattern;
  end;

implementation
uses ZXing.QrCode.Internal.AlignmentPatternImplementation;

{ TAlignmentPatternHelpers }
class function TAlignmentPatternHelpers.CreateAlignmentPattern(const posX, posY, estimatedModuleSize: Single):IAlignmentPattern;
begin
    result := ZXing.QrCode.Internal.AlignmentPatternImplementation.NewAlignmentPattern(posX,posY,estimatedModuleSize);
end;

end.
