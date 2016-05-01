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

  * Original Author: bbrown@google.com (Brian Brown)  
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.DataMatrixReader;

interface

uses
  System.SysUtils, 
  System.Generics.Collections, 
  ZXing.Common.Detector.MathUtils,
  Math,
  ZXing.Common.BitArray,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  DecodeHintType,
  DecoderResult,
  ZXing.Common.DetectorResult,
  ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitMatrix,
  BinaryBitmap,
  ZXing.Datamatrix.Internal.Decoder,
  ZXing.Datamatrix.Internal.Detector;

type
  /// <summary>
  /// This implementation can detect and decode Data Matrix codes in an image.
  /// </summary>
  TDataMatrixReader = class(TInterfacedObject, IReader)  
  private
    FDecoder: TDataMatrixDecoder;
    NO_POINTS: TArray<TResultPoint>;

    /// <summary>
    /// This method detects a code in a "pure" image -- that is, pure monochrome image
    /// which contains only an unrotated, unskewed, image of a code, with some white border
    /// around it. This is a specialized method that works exceptionally fast in this special
    /// case.
    ///
    /// <seealso cref="ZXing.QrCode.QRCodeReader.extractPureBits(TBitMatrix)" />
    /// </summary>
    class function extractPureBits(const image: TBitMatrix): TBitMatrix; static;

    class function moduleSize(const leftTopBlack: TArray<Integer>;
      const image: TBitMatrix; var modulesize: Integer): Boolean; static;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// This implementation can detect and decode Data Matrix codes in an image.
    /// </summary>
    function decode(const image: TBinaryBitmap): TReadResult; overload;

    function decode(const image: TBinaryBitmap;
	    hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    procedure reset;
  end;
  
   
implementation

{ TDataMatrixReader }

constructor TDataMatrixReader.Create;
begin
  inherited;
  FDecoder := TDataMatrixDecoder.Create;
  NO_POINTS := TArray<TResultPoint>.Create();
end;

destructor TDataMatrixReader.Destroy;
begin
  NO_POINTS := nil;
  FDecoder.Free;
  inherited;
end;

function TDataMatrixReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TDataMatrixReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  matrixDetector: TDataMatrixDetector;
  decoderResult: TDecoderResult;
  points: TArray<TResultPoint>;
  bits: TBitMatrix;
  detectorResult: TDetectorResult;
  byteSegments: TList<TArray<Byte>>;
begin
  Result := nil;

  if ((hints <> nil) and
       hints.ContainsKey(TDecodeHintType.PURE_BARCODE)) then
  begin
    bits := TDataMatrixReader.extractPureBits(image.BlackMatrix);
    if Assigned(bits) then
    begin
      DecoderResult := FDecoder.decode(bits);
      points := NO_POINTS;
      bits.Free;
    end else exit;
  end else
  begin
    matrixDetector := TDataMatrixDetector.Create(image.BlackMatrix);
    try
      detectorResult := matrixDetector.detect();
    finally
      matrixDetector.Free;
    end;

    if (detectorResult = nil)
    then
       exit;

    decoderResult := FDecoder.decode(detectorResult.Bits);
    points := detectorResult.Points;
  end;

  if (decoderResult = nil)
  then
     exit;

  Result := TReadResult.Create(decoderResult.Text, decoderResult.RawBytes,
              points, TBarcodeFormat.DATA_MATRIX);
  byteSegments := decoderResult.ByteSegments;
  if (byteSegments <> nil) 
  then
     Result.putMetadata(TResultMetadataType.BYTE_SEGMENTS, byteSegments);

  if (Length(decoderResult.ECLevel) <> 0)
  then
     Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL,
       TStringObject.Create(decoderResult.ECLevel));
end;

class function TDataMatrixReader.extractPureBits(const image: TBitMatrix): TBitMatrix;
var
  moduleSize: Integer;
  leftTopBlack,
  rightBottomBlack : TArray<Integer>;
  top, 
  bottom,
  left,
  right : Integer;
  matrixWidth,
  matrixHeight : Integer;
  nudge: Integer;
  bits: TBitMatrix;
  x, y,
  iOffset : Integer;
begin
  Result := nil;

  leftTopBlack := image.getTopLeftOnBit;
  rightBottomBlack := image.getBottomRightOnBit;
  if ((leftTopBlack = nil) or
      (rightBottomBlack = nil))
  then
     exit;

  if (not TDataMatrixReader.moduleSize(leftTopBlack, image, moduleSize))
  then
     exit;
  
  top := leftTopBlack[1];
  bottom := rightBottomBlack[1];
  left := leftTopBlack[0];
  right := rightBottomBlack[0];
  
  matrixWidth := ((right - left + 1) div moduleSize);
  matrixHeight := ((bottom - top + 1) div moduleSize);
  if ((matrixWidth <= 0) or
      (matrixHeight <= 0))
  then
     exit;
  
  // Push in the "border" by half the module width so that we start
  // sampling in the middle of the module. Just in case the image is a
  // little off, this will help recover.
  nudge := moduleSize shr 1;
  Inc(top, nudge);
  Inc(left, nudge);
  
  // Now just read off the bits
  bits := TBitMatrix.Create(matrixWidth, matrixHeight);
  for y := 0 to Pred(matrixHeight) do
  begin
    iOffset := (top + (y * moduleSize));
    for x := 0 to Pred(matrixWidth) do
    begin
      if (image[(left + (x * moduleSize)), iOffset])
      then
         bits[x, y] := true;
    end;
  end;
  
  Result := bits;
end;	

class function TDataMatrixReader.moduleSize(const leftTopBlack: TArray<Integer>;
  const image: TBitMatrix; var modulesize: Integer): Boolean;
var
  width,
  x, y: Integer;
begin
  Result := false;

  width := image.Width;
  x := leftTopBlack[0];
  y := leftTopBlack[1];

  while ((x < width) and image[x, y]) do
    Inc(x);

  if (x = width) then
  begin
    modulesize := 0;
    exit;
  end;
  
  modulesize := (x - leftTopBlack[0]);
  if (modulesize = 0)
  then
     exit;
  
  Result := true;
end;

procedure TDataMatrixReader.reset;
begin
  // do nothing
end;	


end.

 
 
