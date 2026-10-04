unit PDF417Test;

{
  * Tests for the PDF417 and MicroPDF417 readers: text, ECI, Reader
  * Initialisation, Macro PDF417 (PDF417_EXTRA_METADATA), the pure detector,
  * the Macro 06 header of MicroPDF417 and Compact PDF417.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPDF417Test = class(TObject)
  public
    [Test]
    procedure Basic;
    [Test]
    procedure PureBarcode;
    [Test]
    procedure MixedECIs;
    [Test]
    procedure ReaderInit;
    [Test]
    procedure MacroPDF417;
    [Test]
    procedure MicroPDF417;
    [Test]
    procedure MicroPDF417Macro06;
    [Test]
    procedure CompactPDF417;
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
  ZXing.ResultMetadataType,
  ZXing.PDF417.ResultMetadata;

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

function Extra(r: TReadResult): IPDF417ResultMetadata;
begin
  Result := nil;
  var meta: IMetaData;
  if (r.ResultMetaData <> nil) and r.ResultMetaData.TryGetValue
    (TResultMetadataType.PDF417_EXTRA_METADATA, meta) then
    Supports(meta, IPDF417ResultMetadata, Result);
end;

procedure TPDF417Test.Basic;
begin
  var r := Scan('pdf417-1\01.png', TBarcodeFormat.PDF_417);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.PDF_417), Ord(r.BarcodeFormat));
    Assert.AreEqual('This is PDF417', r.Text);
    Assert.AreEqual(']L2', r.SymbologyIdentifier);
    Assert.AreEqual(4, Length(r.Position));
    var md := Extra(r);
    Assert.IsNotNull(md, 'no PDF417 metadata');
    Assert.AreEqual(-1, md.SegmentIndex);
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.PureBarcode;
begin
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.PURE_BARCODE, nil);
  var r := Scan('pdf417-1\01.png', TBarcodeFormat.PDF_417, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('This is PDF417', r.Text);
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.MixedECIs;
begin
  // several character sets in one symbol: ]L1
  var r := Scan('pdf417-1\mixed-ecis.png', TBarcodeFormat.PDF_417);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('AB'#$70B9#$8317#$30C6#$9F44#$8180#$8D67#$03B1#$0452#$0179,
      r.Text);
    Assert.AreEqual(']L1', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.ReaderInit;
begin
  var r := Scan('pdf417-1\reader-init.png', TBarcodeFormat.PDF_417);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('$I', r.Text);
    Assert.IsTrue(Extra(r).ReaderInit, 'no reader initialisation');
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.MacroPDF417;
begin
  // a sequence of one symbol, with binary data
  var r := Scan('pdf417-2\FileId.png', TBarcodeFormat.PDF_417);
  try
    Assert.IsNotNull(r, ' Nil result ');
    var md := Extra(r);
    Assert.IsNotNull(md, 'no PDF417 metadata');
    Assert.AreEqual('099046122105112', md.FileId);
    Assert.AreEqual(0, md.SegmentIndex);
    Assert.IsTrue(md.IsLastSegment, 'not the last segment');
    Assert.AreEqual(1, md.SegmentCount);
    Assert.AreEqual(Byte($78), r.RawBytes[0]);
    Assert.AreEqual(Byte($88), r.RawBytes[High(r.RawBytes)]);
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.MicroPDF417;
begin
  var r := Scan('micropdf417-1\MPDF-0.png', TBarcodeFormat.MICRO_PDF417);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.MICRO_PDF417), Ord(r.BarcodeFormat));
    Assert.AreEqual('I have the best words.', r.Text);
    Assert.AreEqual(4, Length(r.Position));
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.MicroPDF417Macro06;
begin
  // the ISO 15434 header and trailer of Macro 06
  var r := Scan('micropdf417-1\#1159.webp', TBarcodeFormat.MICRO_PDF417);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('[)>'#30'06'#29'SMA3326324154'#29'1P3TL68105AA01'#29'11P'#29 +
      'E'#13#9'^'#30#4, r.Text);
  finally
    r.Free;
  end;
end;

procedure TPDF417Test.CompactPDF417;
const
  // made of pdf417-1\01.png and 02.png: the right row indicator and the
  // stop pattern replaced by the stop line of one module
  FILES: array [0 .. 1] of string = ('..\PDF417-compact-1.png',
    '..\PDF417-compact-2.png');
  TEXTS: array [0 .. 1] of string = ('This is PDF417', '12345678');
begin
  for var i := 0 to 1 do
    for var format in [TBarcodeFormat.PDF_417, TBarcodeFormat.Auto] do
    begin
      var r := Scan(FILES[i], format);
      try
        Assert.IsNotNull(r, ' Nil result ' + FILES[i]);
        Assert.AreEqual(Ord(TBarcodeFormat.PDF_417), Ord(r.BarcodeFormat));
        Assert.AreEqual(TEXTS[i], r.Text);
      finally
        r.Free;
      end;
    end;
end;

initialization

TDUnitX.RegisterTestFixture(TPDF417Test);

end.
