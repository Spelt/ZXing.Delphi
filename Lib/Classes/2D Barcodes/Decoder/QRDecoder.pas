unit QRDecoder;

interface

uses SysUtils, Generics.Collections, DecodeHintType, BitMatrixx,
  BitmatrixParser, DecoderResult, ReedSolomonDecoder, GenericGF;

type

  TQRDecoder = class

  private
    rsDecoder: TReedSolomonDecoder;

    function correctErrors(codewordBytes: TArray<Byte>;
      numDataCodewords: Integer): boolean;

  public

    constructor Create;
    function decode(parser: TBitMatrixParser;
      hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult; overload;

    function decode(image: TArray<TArray<boolean>>;
      hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult; overload;
    function decode(bits: TBitMatrix;
      hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult; overload;

  end;

implementation

{ TDecoder }

function TQRDecoder.correctErrors(codewordBytes: TArray<Byte>;
  numDataCodewords: Integer): boolean;
begin

end;

constructor TQRDecoder.Create;
begin
  rsDecoder := TReedSolomonDecoder.Create(TGenericGF.QR_CODE_FIELD_256);
end;

function TQRDecoder.decode(parser: TBitMatrixParser;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

function TQRDecoder.decode(bits: TBitMatrix;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

function TQRDecoder.decode(image: TArray<TArray<boolean>>;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

end.
