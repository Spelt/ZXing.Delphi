unit QRCodeTest;

{
  * Tests for the QR Code decoder: QR Code model 1, Kanji segments.
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
    [Test]
    procedure KanjiSegment;
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
  ZXing.QrCode.Internal.DecodedBitStreamParser,
  ZXing.QrCode.Internal.Version,
  ZXing.QrCode.Internal.ErrorCorrectionLevel;

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

procedure TQRCodeTest.KanjiSegment;
begin
  // Kanji mode (1000), 1 character (8 bits in version 1), the 13 bits of
  // Shift_JIS $935F: ($935F - $8140) = $121F, $12 * $C0 + $1F = 3487
  // (0110110011111), the terminator (0000) and 3 padding bits:
  // 1000 0000 | 0001 0110 | 1100 1111 | 1000 0000
  var bytes: TArray<Byte> := [$80, $16, $CF, $80];
  var r := TDecodedBitStreamParser.decode(bytes, TVersion.getVersionForNumber(1),
    TErrorCorrectionLevel.L, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    // U+70B9, the kanji for 'point'
    Assert.AreEqual(string(Char($70B9)), r.Text);
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TQRCodeTest);

end.
