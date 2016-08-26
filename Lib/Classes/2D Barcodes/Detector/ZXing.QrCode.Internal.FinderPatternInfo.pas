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

unit ZXing.QrCode.Internal.FinderPatternInfo;

interface

uses 
  ZXing.QrCode.Internal.FinderPattern;

type
  /// <summary>
  /// <p>Encapsulates information about finder patterns in an image, including the location of
  /// the three finder patterns, and their estimated module size.</p>
  /// </summary>
  TFinderPatternInfo = class sealed
  private
    FbottomLeft: IFinderPattern;
    FtopLeft: IFinderPattern;
    FtopRight: IFinderPattern;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="FinderPatternInfo"/> class.
    /// </summary>
    /// <param name="patternCenters">The pattern centers.</param>
    constructor Create(const patternCenters: TArray<IFinderPattern>);

    /// <summary>
    /// Gets the bottom left.
    /// </summary>
    property bottomLeft : IFinderPattern read FbottomLeft;

    /// <summary>
    /// Gets the top left.
    /// </summary>
    property topLeft : IFinderPattern read FtopLeft;

    /// <summary>
    /// Gets the top right.
    /// </summary>
    property topRight : IFinderPattern read FtopRight;
  end;

implementation

constructor TFinderPatternInfo.Create(const patternCenters: TArray<IFinderPattern>);
begin
  FbottomLeft := patternCenters[0];
  FtopLeft := patternCenters[1];
  FtopRight := patternCenters[2]
end;

end.
