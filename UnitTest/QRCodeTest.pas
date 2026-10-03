unit QRCodeTest;

{
  * Tests for the QR Code decoder: QR Code model 1.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TQRCodeTest = class(TObject)
  public
    [Test]
    procedure Model1;
    [Test]
    procedure Model1Issue940;
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
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\' + fileName;
end;

/// <summary>Scans the image without hints; the caller frees the result.
/// </summary>
function Scan(const fileName: string): TReadResult;
begin
  var bmp := LoadImage(ImagePath(fileName));
  var scanManager := TScanManager.Create(TBarcodeFormat.QR_CODE, nil);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

procedure TQRCodeTest.Model1;
begin
  // a QR Code model 1 (ISO 18004:2000 annex M), level M
  var r := Scan('zxing-cpp\qrcode-1\qr-model-1.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.QR_CODE), Ord(r.BarcodeFormat));
    Assert.AreEqual('QR Code Model 1 ', r.Text);
    Assert.AreEqual(']Q0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TQRCodeTest.Model1Issue940;
begin
  // the sample of zxing-cpp issue 940: also a model 1 code
  var r := Scan('zxing-cpp\qrcode-2\#940.png');
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('N023X-3431800-V034PG03X-3436010-A1125032918310706',
      r.Text);
    Assert.AreEqual(']Q0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TQRCodeTest);

end.
