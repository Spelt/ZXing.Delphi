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
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Common.DetectorResult;

interface

uses
  System.SysUtils,
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint;

type
  /// <summary> <p>Encapsulates the result of detecting a barcode in an image. This includes the raw
  /// matrix of black/white pixels corresponding to the barcode, and possibly points of interest
  /// in the image, like the location of finder patterns or corners of the barcode in the image.</p>
  /// </summary>
  TDetectorResult = class
  private
    Fbits: TBitmatrix;
    Fpoints: TArray<IResultPoint>;
  public
    constructor Create(const bits: TBitmatrix; const points: TArray<IResultPoint>);
    destructor Destroy; override;

    property bits: TBitmatrix read Fbits;
    property points: TArray<IResultPoint> read Fpoints;
  end;

implementation

constructor TDetectorResult.Create(const bits: TBitmatrix;
  const points: TArray<IResultPoint>);
begin
  Fbits := bits;
  Fpoints := points;
end;

destructor TDetectorResult.Destroy;
begin
  FPoints := nil;
  FBits.Free;
  inherited;
end;

end.
