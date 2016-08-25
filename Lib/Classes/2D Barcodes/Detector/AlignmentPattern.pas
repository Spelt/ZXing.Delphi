unit AlignmentPattern;

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

  IAlignmentPattern = interface(IResultPoint)
    ['{B75C2D62-002F-4DAD-AE99-2E5FB8CFAB52}']
     function aboutEquals(const moduleSize,i,j: Single): boolean;
     function combineEstimate(const i,j,newModuleSize: Single):IAlignmentPattern;
  end;



  TAlignmentPatternHelpers = class
     class function CreateAlignmentPattern(const posX,posY,estimatedModuleSize: Single):IAlignmentPattern;
  end;


implementation
uses AlignmentPatternImpl;

class function TAlignmentPatternHelpers.CreateAlignmentPattern(const posX,posY,estimatedModuleSize: Single):IAlignmentPattern;
begin
  result := AlignmentPatternImpl.NewAlignmentPattern(posx,posy,estimatedModuleSize)
end;


end.
