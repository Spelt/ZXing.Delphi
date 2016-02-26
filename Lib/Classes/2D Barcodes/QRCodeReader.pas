unit QRCodeReader;

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

uses
  SysUtils, Generics.Collections, Math, BitArray, ReadResult, Reader,
  DecodeHintType, ResultPoint, BarcodeFormat, BinaryBitmap, QRDecoder,
  Bitmatrix,
  DecoderResult, ResultMetadataType, DetectorResult, QRCodeDecoderMetaData,
  Detector, DecodedBitStreamParser;

type

  TQRCodeReader = class(TInterfacedObject, IReader)
  private
    // Fields
    Decoder: TQRDecoder;
    NO_POINTS: TArray<TResultPoint>;

    class function extractPureBits(image: TBitMatrix): TBitMatrix; static;
    class function moduleSize(leftTopBlack: TArray<Integer>; image: TBitMatrix;
      var msize: Single): boolean; static;

  protected
    function getDecoder: TQRDecoder;
  public
    constructor Create();
    function decode(image: TBinaryBitmap): TReadResult; overload;
    function decode(image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;

    procedure reset;

  end;

implementation

{ TQRCodeReader }

function TQRCodeReader.decode(image: TBinaryBitmap): TReadResult;
begin
  Result := self.decode(image, nil)
end;

constructor TQRCodeReader.Create;
begin
  inherited;

  NO_POINTS := TArray<TResultPoint>.Create();
end;

function TQRCodeReader.decode(image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  DecoderResult: TDecoderResult;
  Detector: TDetector;
  points: TArray<TResultPoint>;
  bits: TBitMatrix;
  DetectorResult: TDetectorResult;
  data: TQRCodeDecoderMetaData;
  byteSegments: TList<TArray<Byte>>;
  ecLevel: string;

begin
  Detector := nil;

  if ((image = nil) or (image.BlackMatrix = nil)) then
  begin
    Result := nil;
    exit
  end;

  if ((hints <> nil) and hints.ContainsKey(TDecodeHintType.PURE_BARCODE)) then
  begin
    bits := TQRCodeReader.extractPureBits(image.BlackMatrix);
    if (bits = nil) then
    begin
      Result := nil;
      exit
    end;

    Decoder := TQRDecoder.Create;
    DecoderResult := Decoder.decode(bits, hints);
    points := NO_POINTS

  end
  else
  begin
    Detector := TDetector.Create(image.BlackMatrix);
    DetectorResult := Detector.Detect(hints);
    try

      if (DetectorResult = nil) then
      begin
        FreeAndNil(Detector);
        exit(nil);
      end;

      Decoder := TQRDecoder.Create;
      try
        DecoderResult := Decoder.decode(DetectorResult.bits, hints);
        points := DetectorResult.points;
      finally
        FreeAndNil(Decoder);
      end;

    finally
      FreeAndNil(DetectorResult);
    end;

  end;

  if (DecoderResult = nil) then
  begin
    if assigned(Detector) then
      FreeAndNil(Detector);
    exit(nil)
  end;

  try

    data := TQRCodeDecoderMetaData(DecoderResult.Other);

    if (data <> nil) then
      data.applyMirroredCorrection(points);

    Result := TReadResult.Create(DecoderResult.Text, DecoderResult.RawBytes,
      points, BarcodeFormat.QR_CODE);

    byteSegments := DecoderResult.byteSegments;

    if (byteSegments <> nil) then
      Result.putMetadata(TResultMetadataType.BYTE_SEGMENTS, byteSegments);

    ecLevel := DecoderResult.ecLevel;

    if (length(ecLevel) = 0) then
      Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL,
        TObject(ecLevel));

    if (DecoderResult.StructuredAppend) then
    begin
      Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_SEQUENCE,
        TObject(DecoderResult.StructuredAppendSequenceNumber));
      Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_PARITY,
        TObject(DecoderResult.StructuredAppendParity))
    end;

  finally
    FreeAndNil(DecoderResult);

    if assigned(Detector) then
      FreeAndNil(Detector);
  end;

end;

class function TQRCodeReader.extractPureBits(image: TBitMatrix): TBitMatrix;
begin
  raise ENotImplemented.Create('not supported');
end;

function TQRCodeReader.getDecoder: TQRDecoder;
begin
  Result := self.Decoder
end;

class function TQRCodeReader.moduleSize(leftTopBlack: TArray<Integer>;
  image: TBitMatrix; var msize: Single): boolean;
begin
  raise ENotImplemented.Create('not supported');
end;

procedure TQRCodeReader.reset;
begin

end;

end.
