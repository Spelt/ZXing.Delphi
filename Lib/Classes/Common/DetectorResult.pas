unit DetectorResult;

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

uses SysUtils, BitMatrix, Resultpoint;

type
  TDetectorResult = class

    FBits: TBitmatrix;
    FPoints: TArray<IResultPoint>;
  private
    function get_Bits: TBitmatrix;
    function get_Points: TArray<IResultPoint>;

  public
    constructor Create(bits: TBitmatrix; points: TArray<IResultPoint>);
    destructor Destroy; override;
    property bits: TBitmatrix read get_Bits;
    property points: TArray<IResultPoint> read get_Points;
  end;

implementation

constructor TDetectorResult.Create(bits: TBitmatrix;
  points: TArray<IResultPoint>);
begin
  FBits := bits;
  FPoints := points
end;

destructor TDetectorResult.Destroy;
begin
  FPoints := nil;
  FreeAndNil(FBits);
  inherited;
end;

function TDetectorResult.get_Bits: TBitmatrix;
begin
  result := FBits;
end;

function TDetectorResult.get_Points: TArray<IResultPoint>;
begin
  result := FPoints;
end;

end.
