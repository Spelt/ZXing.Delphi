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

unit ZXing.BinaryBitmap;

interface

uses
  System.SysUtils,
  ZXing.Binarizer,
  ZXing.LuminanceSource,
  ZXing.Common.BitArray,
  ZXing.Common.BitMatrix;

type
  TBinaryBitmap = class
  private
    Binarizer: TBinarizer;
    Matrix: TBitMatrix;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetBlackMatrix: TBitMatrix;
  public
    constructor Create(Binarizer: TBinarizer);
    destructor Destroy(); override;
    /// <summary>
    /// Converts one row of luminance data to 1 bit data. May actually do the conversion, or return
    /// cached data. Callers should assume this method is expensive and call it as seldom as possible.
    /// This method is intended for decoding 1D barcodes and may choose to apply sharpening.
    /// </summary>
    /// <param name="y">The row to fetch, which must be in [0, bitmap height).</param>
    /// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
    /// If used, the Binarizer will call BitArray.clear(). Always use the returned object.
    /// </param>
    /// <returns> The array of bits for this row (true means black).</returns>
    function getBlackRow(y: Integer; row: IBitArray): IBitArray;
    function RotateSupported: Boolean;
    function rotateCounterClockwise(): TBinaryBitmap;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property BlackMatrix: TBitMatrix read GetBlackMatrix;
  end;

implementation

{ TBinaryBitmap }

constructor TBinaryBitmap.Create(Binarizer: TBinarizer);
begin

  if (Binarizer = nil) then
  begin
    raise EArgumentException.Create('Binarizer must be non-null.');
  end;

  Self.Binarizer := Binarizer;

end;

destructor TBinaryBitmap.Destroy;
begin
  if Assigned(Matrix) then
    FreeAndNil(Matrix);

  //FreeAndNil(Binarizer);
  inherited;
end;

function TBinaryBitmap.GetBlackMatrix: TBitMatrix;
begin
  if (Matrix = nil) then
  begin
    Matrix := Binarizer.BlackMatrix();
  end;

  result := Matrix;

end;

function TBinaryBitmap.getBlackRow(y: Integer; row: IBitArray): IBitArray;
begin
  result := Binarizer.getBlackRow(y, row);
end;

function TBinaryBitmap.GetHeight: Integer;
begin
  result := Binarizer.Height;
end;

function TBinaryBitmap.GetWidth: Integer;
begin
  result := Binarizer.Width;
end;

function TBinaryBitmap.rotateCounterClockwise: TBinaryBitmap;
var
  newSource: TLuminanceSource;
begin
  newSource := Binarizer.LuminanceSource.rotateCounterClockwise();
  result := TBinaryBitmap.Create(Binarizer.createBinarizer(newSource));
end;

function TBinaryBitmap.RotateSupported: Boolean;
begin
  result := Binarizer.LuminanceSource.RotateSupported();
end;

end.
