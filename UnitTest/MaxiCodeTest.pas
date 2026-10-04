unit MaxiCodeTest;

{
  * Tests for the MaxiCode reader: symbols that fill the image, and symbols
  * found by their bullseye (in a photo, rotated).
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
    [Test]
    procedure PhotoOfLabel;
    [Test]
    procedure SymbolInImage;
    [Test]
    procedure SymbolInImageRotated;
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

/// <summary>Scans the image in the test images, turned quarterTurns times
/// 90 degrees.</summary>
function ScanImage(const fileName: string; quarterTurns: Integer = 0)
  : TReadResult;
begin
  var bmp := LoadImage(ExtractFileDir(ParamStr(0)) + '\..\..\images\' +
    fileName);
  if (quarterTurns <> 0) then
  begin
    var rotated := RotateImage(bmp, quarterTurns);
    if (rotated <> bmp) then
    begin
      bmp.Free;
      bmp := rotated;
    end;
  end;
  var scanManager := TScanManager.Create(TBarcodeFormat.MAXICODE, nil);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

function Scan(const fileName: string): TReadResult;
begin
  Result := ScanImage('zxing-cpp\maxicode-1\' + fileName);
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

procedure TMaxiCodeTest.PhotoOfLabel;
begin
  // a photo of a shipping label: found by the bullseye, in perspective
  var r := ScanImage('zxing-cpp\maxicode-2\03.webp');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('[)>'#30'01'#29'96100110000'#29'840'#29'001'#29 +
      '1Z40411757'#29'UPSN'#29'661907'#29'100'#29#29'20/24'#29'20'#29'N'#29#29
      + 'NEW YORK'#29'NY'#30#4, r.Text);
    Assert.AreEqual(4, Length(r.ResultPoints));
  finally
    r.Free;
  end;
end;

procedure TMaxiCodeTest.SymbolInImage;
begin
  // a symbol on a white square in a photo
  var r := ScanImage('MaxiCode-in-photo-1.webp');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('200 by Rick Asley', r.Text);
    // the corners of the symbol, about (178, 107) and (277, 203)
    Assert.AreEqual(178.0, r.ResultPoints[0].x, 3.0);
    Assert.AreEqual(107.0, r.ResultPoints[0].y, 3.0);
    Assert.AreEqual(277.0, r.ResultPoints[2].x, 3.0);
    Assert.AreEqual(203.0, r.ResultPoints[2].y, 3.0);
  finally
    r.Free;
  end;
end;

procedure TMaxiCodeTest.SymbolInImageRotated;
begin
  var r := ScanImage('MaxiCode-in-photo-2.webp', 1);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('400 by Rick Asley', r.Text);
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TMaxiCodeTest);

end.
