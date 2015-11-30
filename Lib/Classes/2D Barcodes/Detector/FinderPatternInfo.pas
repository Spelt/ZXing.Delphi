unit FinderPatternInfo;

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

uses FinderPattern;

type

  TFinderPatternInfo = class sealed
  public
  var
    bottomLeft: TFinderPattern;
    topLeft: TFinderPattern;
    topRight: TFinderPattern;
    constructor Create(patternCenters: TArray<TFinderPattern>);
  end;

implementation

constructor TFinderPatternInfo.Create(patternCenters: TArray<TFinderPattern>);
begin
  self.bottomLeft := patternCenters[0];
  self.topLeft := patternCenters[1];
  self.topRight := patternCenters[2]
end;

end.
