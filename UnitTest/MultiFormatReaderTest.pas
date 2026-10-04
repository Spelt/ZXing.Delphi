unit MultiFormatReaderTest;

{
  * Tests for TMultiFormatReader used directly, without TScanManager.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMultiFormatReaderTest = class(TObject)
  public
    /// <summary>decode(image) without hints, twice: it used to raise an
    /// access violation and to leak the readers of the previous call.</summary>
    [Test]
    procedure DecodeWithoutHints;
    /// <summary>Auto reads the formats that were added later too.</summary>
    [Test]
    procedure AutoReadsAllFormats;
  end;

implementation

uses
  System.SysUtils,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  Vcl.Graphics,
{$ENDIF}
  ZXing.LuminanceSource,
  ZXing.RGBLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager,
  Benchmark.Images;

procedure TMultiFormatReaderTest.DecodeWithoutHints;
var
  bitmap: TBitmap;
  source: TLuminanceSource;
  binarizer: THybridBinarizer;
  image: TBinaryBitmap;
  reader: TMultiFormatReader;
  r: TReadResult;
  i: Integer;
begin
  bitmap := LoadImage(ExtractFileDir(ParamStr(0)) + '\..\..\Images\q33.png');
  source := TRGBLuminanceSource.CreateFromBitmap(bitmap, bitmap.Width,
    bitmap.Height);
  binarizer := THybridBinarizer.Create(source);
  image := TBinaryBitmap.Create(binarizer);
  reader := TMultiFormatReader.Create;
  try
    for i := 1 to 2 do
    begin
      r := reader.decode(image);
      try
        Assert.IsNotNull(r, 'Nil result');
        Assert.AreEqual(TBarcodeFormat.QR_CODE, r.BarcodeFormat);
        Assert.IsTrue(r.Text.StartsWith('Never gonna give you up'));
      finally
        r.Free;
      end;
    end;
  finally
    reader.Free;
    image.Free;
    binarizer.Free;
    source.Free;
    bitmap.Free;
  end;
end;

procedure TMultiFormatReaderTest.AutoReadsAllFormats;
type
  TSample = record
    FileName: string;
    Format: TBarcodeFormat;
    Text: string;
  end;
const
  SAMPLES: array [0 .. 12] of TSample = (
    (FileName: 'dxfilmedge-1\2.png'; Format: TBarcodeFormat.DX_FILM_EDGE;
    Text: '80-11/23'),
    (FileName: 'maxicode-1\MODE5.png'; Format: TBarcodeFormat.MAXICODE;
    Text: ''),
    (FileName: 'microqrcode-1\M2-Alpha.png';
    Format: TBarcodeFormat.MICRO_QR_CODE; Text: 'ABC'),
    (FileName: 'rmqrcode-1\R7x43-H.png'; Format: TBarcodeFormat.RMQR_CODE;
    Text: ',,'),
    (FileName: 'codabar-1\01.webp'; Format: TBarcodeFormat.CODABAR;
    Text: '1234567890'),
    (FileName: 'telepen-1\telepen-alpha-2.png'; Format: TBarcodeFormat.TELEPEN;
    Text: 'TELEPEN'),
    (FileName: 'aztec-1\7.png'; Format: TBarcodeFormat.AZTEC;
    Text: 'Code 2D!'),
    (FileName: 'databarOmni-1\1.png'; Format: TBarcodeFormat.RSS_14;
    Text: '0104412345678909'),
    (FileName: 'databarExp-3\1.png'; Format: TBarcodeFormat.RSS_EXPANDED;
    Text: '0190012345678908310301223315991231'),
    (FileName: 'databarLtd-1\00.png'; Format: TBarcodeFormat.RSS_LIMITED;
    Text: '0100000000000000'),
    (FileName: 'pdf417-1\01.png'; Format: TBarcodeFormat.PDF_417;
    Text: 'This is PDF417'),
    (FileName: 'micropdf417-1\MPDF-0.png';
    Format: TBarcodeFormat.MICRO_PDF417; Text: 'I have the best words.'),
    (FileName: 'qrcode-1\1.png'; Format: TBarcodeFormat.QR_CODE; Text: ''));
begin
  for var s in SAMPLES do
  begin
    var bitmap := LoadImage(ExtractFileDir(ParamStr(0)) +
      '\..\..\Images\zxing-cpp\' + s.FileName);
    var scanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
    try
      var r := scanManager.Scan(bitmap);
      try
        Assert.IsNotNull(r, 'Nil result: ' + s.FileName);
        Assert.AreEqual(Ord(s.Format), Ord(r.BarcodeFormat), s.FileName);
        if (s.Text <> '') then
          Assert.AreEqual(s.Text, r.Text, s.FileName);
      finally
        r.Free;
      end;
    finally
      scanManager.Free;
      bitmap.Free;
    end;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TMultiFormatReaderTest);

end.
