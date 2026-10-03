unit OneDTest;

{
  * Tests for the 1D readers: the hints ALLOWED_EAN_EXTENSIONS and
  * ALLOWED_LENGTHS, and Code 128 FNC4 (extended characters) in code set A.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOneDTest = class(TObject)
  public
    [Test]
    procedure RequiredEANAddOn;
    [Test]
    procedure RequiredEANAddOnMissing;
    [Test]
    procedure AllowedExtensionsAsArray;
    [Test]
    procedure ITFAllowedLengths;
    [Test]
    procedure Code128FNC4CodeSetA;
    [Test]
    procedure BitArraySetRange;
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
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.OneD.Code128Reader;

type
  // access to the protected decodePattern
  TCode128Access = class(TCode128Reader);

function ImagePath(const fileName: string): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\' + fileName;
end;

/// <summary>Scans the image with the hints (freed by the scan manager);
/// the caller frees the result.</summary>
function Scan(const fileName: string; format: TBarcodeFormat;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
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

function Extension(r: TReadResult): string;
begin
  Result := '';
  var meta: IMetaData;
  var ext: IStringMetadata;
  if (r.ResultMetaData <> nil) and r.ResultMetaData.TryGetValue
    (TResultMetadataType.UPC_EAN_EXTENSION, meta) and
    Supports(meta, IStringMetadata, ext) then
    Result := ext.Value;
end;

procedure TOneDTest.RequiredEANAddOn;
begin
  // an EAN-13 with a 5 digit add-on; the scan manager frees the hint value
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS,
    TIntegerArrayHint.Create([2, 5]));
  var r := Scan('zxing-cpp\ean13-ext-1\1.png', TBarcodeFormat.EAN_13, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('9780735200449', r.Text);
    Assert.AreEqual('51299', Extension(r));
    Assert.AreEqual(']E3', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.RequiredEANAddOnMissing;
begin
  // an EAN-13 without add-on: no result when one is required
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS,
    TIntegerArrayHint.Create([2, 5]));
  var r := Scan('zxing-cpp\ean13-1\01.webp', TBarcodeFormat.EAN_13, hints);
  try
    Assert.IsNull(r, 'result without add-on');
  finally
    r.Free;
  end;

  // without the hint it is read
  r := Scan('zxing-cpp\ean13-1\01.webp', TBarcodeFormat.EAN_13, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('9780596008574', r.Text);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.AllowedExtensionsAsArray;
begin
  // as in older versions: an array cast to TObject, kept alive by the
  // caller; the scan manager must not free it
  var lengths: TArray<Integer> := [5];
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS, TObject(Pointer(lengths)));
  var r := Scan('zxing-cpp\ean13-ext-1\1.png', TBarcodeFormat.EAN_13, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('51299', Extension(r));
  finally
    r.Free;
  end;
  Assert.AreEqual(5, lengths[0]);
end;

procedure TOneDTest.ITFAllowedLengths;
begin
  // an ITF of 10 digits
  var r := Scan('zxing-cpp\itf-1\13.webp', TBarcodeFormat.ITF, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(10, r.Text.Length);
  finally
    r.Free;
  end;

  // only 8 or 12 digits allowed
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_LENGTHS, TIntegerArrayHint.Create([8, 12]));
  r := Scan('zxing-cpp\itf-1\13.webp', TBarcodeFormat.ITF, hints);
  try
    Assert.IsNull(r, 'result of a length that is not allowed');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.Code128FNC4CodeSetA;
const
  // start A, FNC4, 'A' (33, with FNC4: 'A' + 128), 'B' (34), checksum
  // (103 + 1 * 101 + 2 * 33 + 3 * 34) mod 103 = 63, stop
  CODES: array [0 .. 5] of array [0 .. 5] of Integer = ((2, 1, 1, 4, 1, 2),
    (3, 1, 1, 1, 4, 1), (1, 1, 1, 3, 2, 3), (1, 3, 1, 1, 2, 3),
    (1, 1, 1, 2, 2, 4), (2, 3, 3, 1, 1, 1));
  MODULE = 3;
  QUIET = 40;
begin
  // the row: quiet zone, the codes, the termination bar of the stop code
  // (2 modules) and a quiet zone
  var widths: TArray<Integer> := [];
  for var c := 0 to High(CODES) do
    for var w in CODES[c] do
      widths := widths + [w];
  widths := widths + [2];
  var total := 0;
  for var w in widths do
    Inc(total, w * MODULE);
  var row := TBitArrayHelpers.CreateBitArray(total + 2 * QUIET);
  var x := QUIET;
  for var i := 0 to High(widths) do
  begin
    if not Odd(i) then
      row.setRange(x, x + widths[i] * MODULE);
    Inc(x, widths[i] * MODULE);
  end;

  // 'A' is 32 + 33, with FNC4 32 + 33 + 128
  var expected := Char(32 + 33 + 128) + 'B';

  var reader := TCode128Reader.Create;
  try
    // the decoder of before
    var r := reader.decodeRow(0, row, nil);
    try
      Assert.IsNotNull(r, ' Nil result (decodeRow) ');
      Assert.AreEqual(expected, r.Text);
    finally
      r.Free;
    end;

    // the decoder of zxing-cpp
    var bars: TPatternRow;
    GetPatternRow(row, row.Size, bars);
    var view := TPatternView.Create(bars);
    r := TCode128Access(reader).decodePattern(0, view, nil);
    try
      Assert.IsNotNull(r, ' Nil result (decodePattern) ');
      Assert.AreEqual(expected, r.Text);
    finally
      r.Free;
    end;
  finally
    reader.Free;
  end;
end;

procedure TOneDTest.BitArraySetRange;
begin
  // within one word, over word boundaries, and empty
  var row := TBitArrayHelpers.CreateBitArray(100);
  row.setRange(3, 7);
  row.setRange(30, 70);
  row.setRange(80, 80);
  for var i := 0 to 99 do
    Assert.AreEqual((i >= 3) and (i < 7) or (i >= 30) and (i < 70), row[i],
      'bit ' + IntToStr(i));
end;

initialization

TDUnitX.RegisterTestFixture(TOneDTest);

end.
