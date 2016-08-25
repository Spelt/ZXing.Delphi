unit FinderPattern;

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

uses Resultpoint;

type
  IFinderPattern = interface(IResultPoint)
    ['{8F123EA3-5A12-4C25-BFD1-49C6DB24D9C1}']
    function aboutEquals(const moduleSize,i,j: Single): boolean;
    function combineEstimate(const i,j,newModuleSize: Single): IFinderPattern;
    function Count:integer;
    function EstimatedModuleSize: Single;
  end;

  TFinderPatternHelpers = class
    class function CreateFinderPattern(const posX,posY,estimatedModuleSize: Single):IFinderPattern; overload;
    class function CreateFinderPattern(const posX,posY,estimatedModuleSize: Single; const count :integer):IFinderPattern; overload;
  end;


implementation
uses FinderPatternImpl;

class function TFinderPatternHelpers.CreateFinderPattern(const posX,posY,estimatedModuleSize: Single):IFinderPattern;
begin
   result := FinderPatternImpl.NewFinderPattern(posX,posY,estimatedModuleSize);
end;

class function TFinderPatternHelpers.CreateFinderPattern(const posX,posY,estimatedModuleSize: Single; const count :integer):IFinderPattern;
begin
   result := FinderPatternImpl.NewFinderPattern(posX,posY,estimatedModuleSize,count);
end;

end.
