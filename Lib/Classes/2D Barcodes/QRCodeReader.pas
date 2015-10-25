unit QRCodeReader;

interface

uses
  SysUtils, Generics.Collections, Math, BitArray, ReadResult, Reader,
  DecodeHintType, ResultPoint, BarcodeFormat, BinaryBitmap, QRDecoder, Bitmatrixx,
  DecoderResult, ResultMetadataType, DetectorResult, QRCodeDecoderMetaData, Detector;

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
  points: TArray<TResultPoint>;
  bits: TBitMatrix;
  detectorResult : TDetectorResult;
  data: TQRCodeDecoderMetaData;
  byteSegments: TList<TArray<Byte>>;
  ecLevel: string;

begin

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
    detectorResult := TDetector.Create(image.BlackMatrix).Detect(hints);
    if (detectorResult = nil) then
    begin
      Result := nil;
      exit
    end;

    Decoder := TQRDecoder.Create;
    DecoderResult := Decoder.decode(detectorResult.bits, hints);
    points := detectorResult.points;

  end;

  if (DecoderResult = nil) then
  begin
    Result := nil;
    exit
  end;

  data := TQRCodeDecoderMetaData(DecoderResult.Other);

  if (data <> nil) then
    data.applyMirroredCorrection(points);

  Result := Result.Create(DecoderResult.Text, DecoderResult.RawBytes, points,
    BarcodeFormat.QR_CODE);

  byteSegments := DecoderResult.byteSegments;

  if (byteSegments <> nil) then
    Result.putMetadata(TResultMetadataType.BYTE_SEGMENTS, byteSegments);

  ecLevel := DecoderResult.ecLevel;
 {
  if (length(ecLevel) = 0) then
    Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL, ecLevel);

  if (DecoderResult.StructuredAppend) then
  begin
    Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_SEQUENCE,
      DecoderResult.StructuredAppendSequenceNumber);
    Result.putMetadata(TResultMetadataType.STRUCTURED_APPEND_PARITY,
      DecoderResult.StructuredAppendParity)
  end;
  }

  Result := Result;
  exit
end;

class function TQRCodeReader.extractPureBits(image: TBitMatrix): TBitMatrix;
begin

end;

function TQRCodeReader.getDecoder: TQRDecoder;
begin
  Result := self.Decoder
end;

class function TQRCodeReader.moduleSize(leftTopBlack: TArray<Integer>;
  image: TBitMatrix; var msize: Single): boolean;
begin

end;

procedure TQRCodeReader.reset;
begin

end;

end.
