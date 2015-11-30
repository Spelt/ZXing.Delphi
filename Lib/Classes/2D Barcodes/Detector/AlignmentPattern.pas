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

  TAlignmentPattern = class sealed(TResultpoint)
  private
    estimatedModuleSize: Single;
  public
    constructor Create(posX: Single; posY: Single; estimatedModuleSize: Single);
    function aboutEquals(moduleSize: Single; i: Single; j: Single): boolean;
    function combineEstimate(i: Single; j: Single; newModuleSize: Single)
      : TAlignmentPattern;
  end;

implementation

constructor TAlignmentPattern.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single);
begin
  inherited Create(posX, posY);
  self.estimatedModuleSize := estimatedModuleSize;
end;

function TAlignmentPattern.aboutEquals(moduleSize: Single; i: Single;
  j: Single): boolean;

var
  moduleSizeDiff: Single;

begin
  if ((Abs(i - self.y) <= moduleSize) and (Abs(j - self.x) <= moduleSize)) then
  begin
    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);
    begin
      Result := ((moduleSizeDiff <= 1) or
        (moduleSizeDiff <= self.estimatedModuleSize));
      exit
    end
  end;

  Result := false;
end;

function TAlignmentPattern.combineEstimate(i: Single; j: Single;
  newModuleSize: Single): TAlignmentPattern;
var
  combinedX, combinedY: Single;
begin
  combinedX := ((self.x + j) / 2);
  combinedY := ((self.y + i) / 2);
  Result := TAlignmentPattern.Create(combinedX, combinedY,
    ((self.estimatedModuleSize + newModuleSize) / 2))
end;

end.
