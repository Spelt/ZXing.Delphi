unit MaxiCodeTest;

{
  * Tests for the MaxiCode reader (symbols that fill the image).
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxiCodeTest = class(TObject)
  public
    [Test]
    procedure StructuredCarrierMessage;
    [Test]
    procedure StandardSymbol;
    [Test]
    procedure FullECCSequence;
    [Test]
    procedure MixedECIs;
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
  ZXing.ReadResult,
  ZXing.ResultMetadataType;

function Scan(const fileName: string): TReadResult;
begin
  var bmp := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\images\zxing-cpp\maxicode-1\' + fileName);
  var scanManager := TScanManager.Create(TBarcodeFormat.MAXICODE, nil);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

function Meta(r: TReadResult; t: TResultMetadataType): IMetaData;
begin
  Result := nil;
  if (r.ResultMetaData <> nil) then
    r.ResultMetaData.TryGetValue(t, Result);
end;

procedure TMaxiCodeTest.StructuredCarrierMessage;
begin
  // mode 2: the numeric postcode, country and service class in front
  var r := Scan('MODE2.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.MAXICODE), Ord(r.BarcodeFormat));
    Assert.AreEqual('[)>'#30'01'#29'96123450000'#29'222'#29'111'#29'MODE2',
      r.Text);
    Assert.AreEqual(']U1', r.SymbologyIdentifier);
    Assert.AreEqual('2', (Meta(r, TResultMetadataType.ERROR_CORRECTION_LEVEL)
      as IStringMetadata).Value);
  finally
    r.Free;
  end;
end;

procedure TMaxiCodeTest.StandardSymbol;
begin
  var r := Scan('Wikipedia.webp');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('Wikipedia, the free encyclopedia', r.Text);
    Assert.AreEqual(']U0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TMaxiCodeTest.FullECCSequence;
begin
  // mode 5, symbol 2 of 3
  var r := Scan('mode5-sequence2of3.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('12345678901234567890123456789012345678901234567890' +
      '12345678901234567890123456789012345678901234567890123456789' +
      '01', r.Text);
    var seq: IIntegerMetadata;
    Assert.IsTrue(Supports(Meta(r,
      TResultMetadataType.STRUCTURED_APPEND_SEQUENCE), IIntegerMetadata, seq));
    // index 1 in the high nibble, count - 1 in the low one
    Assert.AreEqual((1 shl 4) or 2, seq.Value);
  finally
    r.Free;
  end;
end;

procedure TMaxiCodeTest.MixedECIs;
const
  EXPECTED: array [0 .. 27] of Byte = ($E1, $A1, $A1, $A1, $A1, $A1, $83, $65,
    $93, $5F, $00, $A1, $F0, $90, $8C, $B6, $F9, $D5, $F9, $A1, $E9, $B0, $A1,
    $B0, $A2, $F7, $FE, $A1);
begin
  var r := Scan('mode4-mixed-ecis.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Length(EXPECTED), Length(r.RawBytes), 'length');
    for var i := 0 to High(EXPECTED) do
      Assert.AreEqual(EXPECTED[i], r.RawBytes[i], 'byte ' + IntToStr(i));
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TMaxiCodeTest);

end.
