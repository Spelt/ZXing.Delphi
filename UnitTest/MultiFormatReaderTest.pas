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

initialization

TDUnitX.RegisterTestFixture(TMultiFormatReaderTest);

end.
