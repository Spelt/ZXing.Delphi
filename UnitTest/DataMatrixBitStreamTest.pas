unit DataMatrixBitStreamTest;

{
  * Tests for the Data Matrix bit stream parser with hand made codewords
  * (ISO 16022): ASCII values are written as value + 1, upper shift (235)
  * adds 128 to the next value, 241 is an ECI, 236/237 are the 05/06 macros.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDataMatrixBitStreamTest = class(TObject)
  public
    [Test]
    procedure PlainTextIsUnchanged;
    [Test]
    procedure Macro05;
    [Test]
    procedure Macro06;
    [Test]
    procedure ECIChangesCharacterSet;
    [Test]
    procedure ECIUtf8;
    [Test]
    procedure UnknownECIKeepsText;
  end;

implementation

uses
  System.SysUtils,
  ZXing.DecoderResult,
  ZXing.Datamatrix.Internal.DecodedBitStreamParser;

function DecodeCodewords(const codewords: array of Byte): string;
var
  bytes: TArray<Byte>;
  i: Integer;
  r: TDecoderResult;
begin
  SetLength(bytes, Length(codewords));
  for i := 0 to High(codewords) do
    bytes[i] := codewords[i];
  r := TDecodedBitStreamParser.decode(bytes);
  try
    Assert.IsNotNull(r, 'Nil result');
    Result := r.Text;
  finally
    r.Free;
  end;
end;

procedure TDataMatrixBitStreamTest.PlainTextIsUnchanged;
begin
  // 'A' 'B', '12' as digit pair (130 + 12), pad
  Assert.AreEqual('AB12', DecodeCodewords([66, 67, 142, 129]));
end;

procedure TDataMatrixBitStreamTest.Macro05;
begin
  Assert.AreEqual('[)>'#30'05'#29'AB'#30#4, DecodeCodewords([236, 66, 67]));
end;

procedure TDataMatrixBitStreamTest.Macro06;
begin
  Assert.AreEqual('[)>'#30'06'#29'A'#30#4, DecodeCodewords([237, 66]));
end;

procedure TDataMatrixBitStreamTest.ECIChangesCharacterSet;
begin
  // like datamatrix-1/eci.png of zxing-cpp: byte B6 in the default
  // character set (ISO-8859-1, a pilcrow), then ECI 7 (ISO-8859-5) and
  // byte B6 again, which is a Cyrillic Zhe there
  Assert.AreEqual(#$00B6#$0416, DecodeCodewords([235, 55, 241, 8, 235, 55]));
end;

procedure TDataMatrixBitStreamTest.ECIUtf8;
begin
  // ECI 26 (UTF-8), bytes C3 A9: e with acute accent
  Assert.AreEqual(#$00E9, DecodeCodewords([241, 27, 235, 68, 235, 42]));
end;

procedure TDataMatrixBitStreamTest.UnknownECIKeepsText;
begin
  // ECI 899 (two codewords: 131, 11) is not a known character set: the
  // ECI codewords disappear, the text stays
  Assert.AreEqual('A', DecodeCodewords([241, 131, 11, 66]));
end;

initialization

TDUnitX.RegisterTestFixture(TDataMatrixBitStreamTest);

end.
