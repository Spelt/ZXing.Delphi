unit Decoder;

interface

uses SysUtils, Generics.Collections, DecodeHintType, BitMatrixx, BitmatrixParser, DecoderResult;

type

  TDecoder = class

  private
   // rsDecoder: TReedSolomonDecoder;

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

function TDecoder.correctErrors(codewordBytes: TArray<Byte>;
  numDataCodewords: Integer): boolean;
begin

end;

constructor TDecoder.Create;
begin

end;

function TDecoder.decode(parser: TBitMatrixParser;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

function TDecoder.decode(bits: TBitMatrix;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

function TDecoder.decode(image: TArray<TArray<boolean>>;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

end.
