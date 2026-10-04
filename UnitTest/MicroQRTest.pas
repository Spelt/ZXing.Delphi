unit MicroQRTest;

{
  * Tests for the Micro QR Code and rMQR Code reader.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMicroQRTest = class(TObject)
  public
    [Test]
    procedure M1Numeric;
    [Test]
    procedure M2Alphanumeric;
    [Test]
    procedure M3Kanji;
    [Test]
    procedure M4Binary;
    [Test]
    procedure PureMicroQR;
    [Test]
    procedure RMQRSmall;
    [Test]
    procedure RMQRLarge;
    [Test]
    procedure PureRMQR;
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
  ZXing.ReadResult;

function ImagePath(const fileName: string): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\zxing-cpp\' +
    fileName;
end;

/// <summary>Scans the image with the hints (freed by the scan manager);
/// the caller frees the result.</summary>
function Scan(const fileName: string; format: TBarcodeFormat;
  hints: TDictionary<TDecodeHintType, TObject> = nil): TReadResult;
begin
  var bmp := LoadImage(ImagePath(fileName));
  var scanManager := TScanManager.Create(format, hints);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

function PureHints: TDictionary<TDecodeHintType, TObject>;
begin
  Result := TDictionary<TDecodeHintType, TObject>.Create;
  Result.Add(TDecodeHintType.PURE_BARCODE, nil);
end;

procedure CheckRead(const fileName: string; format: TBarcodeFormat;
  const text: string; hints: TDictionary<TDecodeHintType, TObject> = nil);
begin
  var r := Scan(fileName, format, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(format), Ord(r.BarcodeFormat));
    Assert.AreEqual(text, r.Text);
    Assert.AreEqual(']Q1', r.SymbologyIdentifier);
    Assert.AreEqual(4, Length(r.Position));
  finally
    r.Free;
  end;
end;

procedure TMicroQRTest.M1Numeric;
begin
  CheckRead('microqrcode-1\M1-Numeric.png', TBarcodeFormat.MICRO_QR_CODE,
    '12345');
end;

procedure TMicroQRTest.M2Alphanumeric;
begin
  CheckRead('microqrcode-1\M2-Alpha.png', TBarcodeFormat.MICRO_QR_CODE, 'ABC');
end;

procedure TMicroQRTest.M3Kanji;
begin
  CheckRead('microqrcode-1\M3-Kanji.png', TBarcodeFormat.MICRO_QR_CODE,
    #$8AA0);
end;

procedure TMicroQRTest.M4Binary;
begin
  // the bytes are UTF-8
  CheckRead('microqrcode-1\M4-Binary.png', TBarcodeFormat.MICRO_QR_CODE,
    '!"'#$00A7'$%&/()=?`');
end;

procedure TMicroQRTest.PureMicroQR;
begin
  CheckRead('microqrcode-1\M2-Alpha.png', TBarcodeFormat.MICRO_QR_CODE, 'ABC',
    PureHints);
end;

procedure TMicroQRTest.RMQRSmall;
begin
  CheckRead('rmqrcode-1\R7x43-H.png', TBarcodeFormat.RMQR_CODE, ',,');
end;

procedure TMicroQRTest.RMQRLarge;
begin
  var r := Scan('rmqrcode-1\R17x139.png', TBarcodeFormat.RMQR_CODE);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.RMQR_CODE), Ord(r.BarcodeFormat));
    Assert.IsTrue(r.Text.StartsWith('3.14159265358979'), r.Text);
  finally
    r.Free;
  end;
end;

procedure TMicroQRTest.PureRMQR;
begin
  CheckRead('rmqrcode-1\R7x43-H.png', TBarcodeFormat.RMQR_CODE, ',,',
    PureHints);
end;

initialization

TDUnitX.RegisterTestFixture(TMicroQRTest);

end.
