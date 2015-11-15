unit DecodedBitStreamParser;

interface

uses BitSource, SysUtils, DecodeHintType, Generics.Collections,
  ErrorCorrectionLevel, version, DecoderResult, CharacterSetECI;

type

  TDecodedBitStreamParser = class abstract

  private

    class var ALPHANUMERIC_CHARS: TArray<Char>;

  const
    GB2312_SUBSET: Integer = 1;

    constructor Create;
    class function decodeAlphanumericSegment(bits: TBitSource;
      result: TStringBuilder; count: Integer; fc1InEffect: boolean)
      : boolean; static;

    class function decodeByteSegment(bits: TBitSource; result: TStringBuilder;
      count: Integer; currentCharacterSetECI: TCharacterSetECI;
      byteSegments: TList<TArray<Byte>>;
      hints: TDictionary<TDecodeHintType, TObject>): boolean; static;

    class function decodeHanziSegment(bits: TBitSource; result: TStringBuilder;
      count: Integer): boolean; static;

    class function decodeKanjiSegment(bits: TBitSource; result: TStringBuilder;
      count: Integer): boolean; static;

    class function decodeNumericSegment(bits: TBitSource;
      result: TStringBuilder; count: Integer): boolean; static;
    class function parseECIValue(bits: TBitSource): Integer; static;
    class function toAlphaNumericChar(value: Integer): Char; static;

  public
    class function decode(bytes: TArray<Byte>; version: TVersion;
      ecLevel: TErrorCorrectionLevel;
      hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult; static;

  end;

implementation

{ TDecodedBitStreamParser }

constructor TDecodedBitStreamParser.Create;
begin

end;

class function TDecodedBitStreamParser.decode(bytes: TArray<Byte>;
  version: TVersion; ecLevel: TErrorCorrectionLevel;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
begin

end;

class function TDecodedBitStreamParser.decodeAlphanumericSegment(
  bits: TBitSource; result: TStringBuilder; count: Integer;
  fc1InEffect: boolean): boolean;
begin

end;

class function TDecodedBitStreamParser.decodeByteSegment(bits: TBitSource;
  result: TStringBuilder; count: Integer;
  currentCharacterSetECI: TCharacterSetECI; byteSegments: TList<TArray<Byte>>;
  hints: TDictionary<TDecodeHintType, TObject>): boolean;
begin

end;

class function TDecodedBitStreamParser.decodeHanziSegment(bits: TBitSource;
  result: TStringBuilder; count: Integer): boolean;
begin

end;

class function TDecodedBitStreamParser.decodeKanjiSegment(bits: TBitSource;
  result: TStringBuilder; count: Integer): boolean;
begin

end;

class function TDecodedBitStreamParser.decodeNumericSegment(bits: TBitSource;
  result: TStringBuilder; count: Integer): boolean;
begin

end;

class function TDecodedBitStreamParser.parseECIValue(bits: TBitSource): Integer;
begin

end;

class function TDecodedBitStreamParser.toAlphaNumericChar(value: Integer): Char;
begin

end;

end.
