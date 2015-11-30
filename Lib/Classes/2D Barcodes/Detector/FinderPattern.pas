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
  TFinderPattern = class(TResultpoint)
  public
    constructor Create(posX: Single; posY: Single;
      estimatedModuleSize: Single); overload;
    constructor Create(posX: Single; posY: Single; estimatedModuleSize: Single;
      count: Integer); overload;
    function aboutEquals(moduleSize: Single; i: Single; j: Single): boolean;
    function combineEstimate(i: Single; j: Single; newModuleSize: Single)
      : TFinderPattern;

  var
    count: Integer;
    estimatedModuleSize: Single;
  end;

implementation

constructor TFinderPattern.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single);
begin
  inherited Create(posX, posY);
  self.estimatedModuleSize := estimatedModuleSize;
  self.count := 1
end;

constructor TFinderPattern.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single; count: Integer);
begin
  inherited Create(posX, posY);
  self.estimatedModuleSize := estimatedModuleSize;
  self.count := count
end;

function TFinderPattern.aboutEquals(moduleSize: Single; i: Single;
  j: Single): boolean;
var
  moduleSizeDiff, x, y: Single;

begin
  x := self.x;
  y := self.y;
  if ((Abs(i - self.y) <= moduleSize) and (Abs(j - self.x) <= moduleSize)) then
  begin

    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);

    Result := ((moduleSizeDiff <= 1) or
      (moduleSizeDiff <= self.estimatedModuleSize));
    exit
  end;

  Result := false;

end;

function TFinderPattern.combineEstimate(i: Single; j: Single;
  newModuleSize: Single): TFinderPattern;
var
  combinedCount: Integer;
  combinedX, combinedY: Single;

begin
  combinedCount := (self.count + 1);
  combinedX := ((self.count * self.x) + j) / combinedCount;
  combinedY := ((self.count * self.y) + i) / combinedCount;
  Result := TFinderPattern.Create(combinedX, combinedY,
    (((self.count * self.estimatedModuleSize) + newModuleSize) / combinedCount),
    combinedCount)
end;

end.
