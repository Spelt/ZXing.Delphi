unit AztecTest;

{
  * Tests for the Aztec reader: compact and full range symbols, a mirrored
  * symbol, GS1 (FNC1), Structured Append, ECI and Aztec Runes.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TAztecTest = class(TObject)
  public
    [Test]
    procedure Compact;
    [Test]
    procedure FullRange;
    [Test]
    procedure Mirrored;
    [Test]
    procedure GS1;
    [Test]
    procedure StructuredAppend;
    [Test]
    procedure MixedECIs;
    [Test]
    procedure Rune;
    [Test]
    procedure PureBarcode;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  VCL.Graphics,
{$ENDIF}
  Benchmark.Images,
  ZXing.ScanManager,
  ZXing.BarcodeFormat,
  ZXing.DecodeHintType,
  ZXing.ReadResult,
  ZXing.ResultMetadataType;

function ImagePath(const fileName: string): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\zxing-cpp\aztec-1\'
    + fileName;
end;

/// <summary>Scans the image with the hints (freed by the scan manager);
/// the caller frees the result.</summary>
function Scan(const fileName: string;
  hints: TDictionary<TDecodeHintType, TObject> = nil): TReadResult;
begin
  var bmp := LoadImage(ImagePath(fileName));
  var scanManager := TScanManager.Create(TBarcodeFormat.AZTEC, hints);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

function IntegerMeta(r: TReadResult; t: TResultMetadataType): Integer;
begin
  Result := -1;
  var meta: IMetaData;
  var value: IIntegerMetadata;
  if (r.ResultMetaData <> nil) and r.ResultMetaData.TryGetValue(t, meta) and
    Supports(meta, IIntegerMetadata, value) then
    Result := value.Value;
end;

procedure TAztecTest.Compact;
begin
  var r := Scan('7.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.AZTEC), Ord(r.BarcodeFormat));
    Assert.AreEqual('Code 2D!', r.Text);
    Assert.AreEqual(']z0', r.SymbologyIdentifier);
    Assert.AreEqual(4, Length(r.Position));
  finally
    r.Free;
  end;
end;

procedure TAztecTest.FullRange;
begin
  // 151 x 151 modules, with the reference grid
  var r := Scan('lorem-151x151.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.IsTrue(r.Text.StartsWith('In ut magna vel mauris malesuada dictum.'),
      r.Text);
  finally
    r.Free;
  end;
end;

procedure TAztecTest.Mirrored;
begin
  var r := Scan('abc-mirrored.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('abcdefghijklmnopqrstuvwxyz', r.Text);
    Assert.IsTrue(r.IsMirrored, 'not mirrored');
  finally
    r.Free;
  end;
end;

procedure TAztecTest.GS1;
begin
  // the leading FNC1 is removed, the next ones are GS characters
  var r := Scan('gs1-figure-4.15.1-2-31x31.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('01095040000591012112345678p901' + #29 + '101234567p' +
      #29 + '171411208200http://www.gs1.org/demo/', r.Text);
    Assert.AreEqual(']z1', r.SymbologyIdentifier);
    Assert.IsTrue(r.IsGS1, 'not GS1');
    Assert.AreEqual('(01)09504000059101(21)12345678p901(10)1234567p(17)141120' +
      '(8200)http://www.gs1.org/demo/', r.GS1HRI);
  finally
    r.Free;
  end;
end;

procedure TAztecTest.StructuredAppend;
begin
  // symbol 4 of 7: the Structured Append header is not in the text
  var r := Scan('Z1-sequence4of7.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('3456', r.Text);
    Assert.AreEqual(']z6', r.SymbologyIdentifier);
    // index 3 (0 based) in the high nibble, count - 1 in the low one
    Assert.AreEqual((3 shl 4) or 6,
      IntegerMeta(r, TResultMetadataType.STRUCTURED_APPEND_SEQUENCE));
  finally
    r.Free;
  end;
end;

procedure TAztecTest.MixedECIs;
const
  EXPECTED: array [0 .. 28] of Byte = ($E1, $A1, $A1, $A1, $A1, $A1, $83, $65,
    $93, $5F, $00, $A1, $F0, $90, $8C, $B6, $F9, $D5, $F9, $A1, $E9, $B0, $A1,
    $B0, $A2, $F7, $FE, $A1, $A2);
begin
  // several character sets in one symbol: the bytes of the content, and the
  // text of the first parts (ISO-8859-1, -3, -5)
  var r := Scan('mixed-ecis-41x41.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Length(EXPECTED), Length(r.RawBytes), 'length');
    for var i := 0 to High(EXPECTED) do
      Assert.AreEqual(EXPECTED[i], r.RawBytes[i], 'byte ' + IntToStr(i));
    Assert.IsTrue(r.Text.StartsWith(#$E1#$A1#$126#$401), r.Text);
    Assert.AreEqual(']z3', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TAztecTest.Rune;
begin
  // the value of an Aztec Rune, as 3 digits
  var r := Scan('rune-223.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('223', r.Text);
    Assert.AreEqual(']zC', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TAztecTest.PureBarcode;
begin
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.PURE_BARCODE, nil);
  var r := Scan('hello.png', hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('hello', r.Text);
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TAztecTest);

end.
