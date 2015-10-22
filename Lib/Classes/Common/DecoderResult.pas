unit DecoderResult;

interface

uses SysUtils, Generics.Collections;

type

  TDecoderResult = class
  private
    function GetStructuredAppend: boolean;
  public
  var
    ByteSegments: TList<TArray<Byte>>;
    ECLevel: string;
    Erasures: Integer;
    ErrorsCorrected: Integer;
    Other: TObject;
    RawBytes: TArray<Byte>;
    StructuredAppendParity: Integer;
    StructuredAppendSequenceNumber: Integer;
    Text: string;

    constructor Create(RawBytes: TArray<Byte>; Text: string;
      ByteSegments: TList<TArray<Byte>>; ECLevel: string); overload;
    constructor Create(RawBytes: TArray<Byte>; Text: string;
      ByteSegments: TList<TArray<Byte>>; ECLevel: string; saSequence: Integer;
      saParity: Integer); overload;

    property StructuredAppend: boolean read GetStructuredAppend;

  end;

implementation

{ TDecoderResult }

constructor TDecoderResult.Create(RawBytes: TArray<Byte>; Text: string;
  ByteSegments: TList<TArray<Byte>>; ECLevel: string);
begin
  self.Create(RawBytes, Text, ByteSegments, ECLevel, -1, -1);
end;

constructor TDecoderResult.Create(RawBytes: TArray<Byte>; Text: string;
  ByteSegments: TList<TArray<Byte>>; ECLevel: string;
  saSequence, saParity: Integer);
begin
  if ((RawBytes = nil) and (Text = '')) then
    raise EArgumentException.Create('Text or rawbytes cannot be nil or empty');
  self.RawBytes := RawBytes;
  self.Text := Text;
  self.ByteSegments := ByteSegments;
  self.ECLevel := ECLevel;
  self.StructuredAppendParity := saParity;
  self.StructuredAppendSequenceNumber := saSequence
end;

function TDecoderResult.GetStructuredAppend: boolean;
begin
  result := (StructuredAppendParity >= 0) and
    (StructuredAppendSequenceNumber >= 0);
end;

end.
