unit ReadResultTest;

{
  * Tests for the extra properties of TReadResult: Position, Orientation,
  * IsInverted, IsMirrored and SymbologyIdentifier.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TReadResultTest = class(TObject)
  public
    [Test]
    procedure QRCodePositionAndOrientation;
    [Test]
    procedure DataMatrixSymbologyIdentifier;
    [Test]
    procedure InvertedGS1DataMatrix;
    [Test]
    procedure MirroredDataMatrix;
    [Test]
    procedure Code128SymbologyAndPosition;
    [Test]
    procedure ScanAllFindsAllQRCodes;
    [Test]
    procedure ScanAllMixedFormats;
    [Test]
    procedure ScanAllMaxCount;
    [Test]
    procedure ScanAllStackedCode128;
    [Test]
    procedure ScanAllWithVerticalCode128;
    [Test]
    procedure GS1HRIFromElementStrings;
    [Test]
    procedure GS1HRIOfDataMatrix;
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
  ZXing.GS1;

function ImagePath(const fileName: string): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\' + fileName;
end;

/// <summary>Scans the image rotated by quarterTurns * 90 degrees; nil when
/// nothing is found. The caller frees the result.</summary>
function Scan(const fileName: string; format: TBarcodeFormat;
  quarterTurns: Integer = 0; inversion: Boolean = false): TReadResult;
begin
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  if inversion then
    hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);
  var original := LoadImage(ImagePath(fileName));
  var bmp := RotateImage(original, quarterTurns);
  var scanManager := TScanManager.Create(format, hints);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    if (bmp <> original) then
      bmp.Free;
    original.Free;
  end;
end;

procedure TReadResultTest.QRCodePositionAndOrientation;
begin
  for var turns := 0 to 3 do
  begin
    var r := Scan('zxing-cpp\qrcode-1\1.png', TBarcodeFormat.QR_CODE, turns);
    try
      Assert.IsNotNull(r, 'nil result at ' + IntToStr(turns * 90));
      Assert.AreEqual(Integer(turns * 90), r.Orientation, 'orientation');
      Assert.AreEqual(4, Integer(Length(r.Position)), 'position');
      Assert.AreEqual(']Q1', r.SymbologyIdentifier);
      Assert.IsFalse(r.IsInverted);
      Assert.IsFalse(r.IsMirrored);
      // the top left corner lies left of the top right one, as read
      if (turns = 0) then
        Assert.IsTrue(r.Position[0].x < r.Position[1].x);
    finally
      r.Free;
    end;
  end;
end;

procedure TReadResultTest.DataMatrixSymbologyIdentifier;
begin
  var r := Scan('dmc1.png', TBarcodeFormat.DATA_MATRIX);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(']d1', r.SymbologyIdentifier);
    Assert.AreEqual(0, r.Orientation);
    Assert.AreEqual(4, Integer(Length(r.Position)));
  finally
    r.Free;
  end;
end;

procedure TReadResultTest.InvertedGS1DataMatrix;
begin
  // a GS1 dot-peen code, light on dark
  var r := Scan('dm-inverted.jpg', TBarcodeFormat.DATA_MATRIX, 0, true);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(']d2', r.SymbologyIdentifier);
    Assert.IsTrue(r.IsInverted);
  finally
    r.Free;
  end;
end;

procedure TReadResultTest.MirroredDataMatrix;
begin
  var r := Scan('zxing-cpp\datamatrix-1\mirrored.png',
    TBarcodeFormat.DATA_MATRIX);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.IsTrue(r.IsMirrored);
  finally
    r.Free;
  end;
end;

procedure TReadResultTest.Code128SymbologyAndPosition;
begin
  var r := Scan('Code128.png', TBarcodeFormat.CODE_128);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(']C0', r.SymbologyIdentifier);
    Assert.AreEqual(4, Integer(Length(r.Position)));
    Assert.AreEqual(0, r.Orientation);
  finally
    r.Free;
  end;
end;

/// <summary>All barcodes in the image; the caller frees the list.</summary>
function ScanAll(const fileName: string; format: TBarcodeFormat;
  maxCount: Integer = 0): TObjectList<TReadResult>;
begin
  var bmp := LoadImage(ImagePath(fileName));
  var scanManager := TScanManager.Create(format, nil);
  try
    Result := scanManager.ScanAll(bmp, maxCount);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

procedure TReadResultTest.ScanAllFindsAllQRCodes;
begin
  // 4 QR Codes of a structured append sequence
  var list := ScanAll('zxing-cpp\qrcode-2\StructApp.webp', TBarcodeFormat.Auto);
  try
    Assert.AreEqual(4, list.Count);
    for var r in list do
      Assert.AreEqual(Ord(TBarcodeFormat.QR_CODE), Ord(r.BarcodeFormat));
  finally
    list.Free;
  end;
end;

procedure TReadResultTest.ScanAllMixedFormats;
begin
  // a QR Code and an EAN-13 (also read as UPC-A, but only once)
  var list := ScanAll('zxing-cpp\multi-1\ean+qr.png', TBarcodeFormat.Auto);
  try
    Assert.AreEqual(2, list.Count);
    var qr := 0;
    var ean := 0;
    for var r in list do
      if (r.BarcodeFormat = TBarcodeFormat.QR_CODE) then
      begin
        Inc(qr);
        Assert.AreEqual('www.airtable.com/jobs', r.Text);
      end
      else
      begin
        Inc(ean);
        Assert.IsTrue(r.Text.EndsWith('31415926531'), r.Text);
      end;
    Assert.AreEqual(1, qr);
    Assert.AreEqual(1, ean);
  finally
    list.Free;
  end;
end;

procedure TReadResultTest.ScanAllMaxCount;
begin
  var list := ScanAll('zxing-cpp\qrcode-2\StructApp.webp',
    TBarcodeFormat.QR_CODE, 2);
  try
    Assert.AreEqual(2, list.Count);
  finally
    list.Free;
  end;
end;

procedure TReadResultTest.ScanAllStackedCode128;
begin
  // two different Code 128 close to each other, one above the other
  var list := ScanAll('two-code128-stacked.png', TBarcodeFormat.Auto);
  try
    Assert.AreEqual(2, list.Count);
    var texts := '';
    for var r in list do
    begin
      Assert.AreEqual(Ord(TBarcodeFormat.CODE_128), Ord(r.BarcodeFormat));
      texts := texts + '[' + r.Text + ']';
    end;
    Assert.IsTrue(texts.Contains('[1234567]'), texts);
    Assert.IsTrue(texts.Contains('[Code 128]'), texts);
  finally
    list.Free;
  end;
end;

procedure TReadResultTest.ScanAllWithVerticalCode128;
begin
  // a horizontal and a vertical Code 128, a QR Code and a Data Matrix; the
  // vertical one is found in the rotated image (TRY_HARDER), also when the
  // horizontal one was found already
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.TRY_HARDER, nil);
  var bmp := LoadImage(ImagePath('scanall-sheet.png'));
  var scanManager := TScanManager.Create(TBarcodeFormat.Auto, hints);
  var list := scanManager.ScanAll(bmp);
  try
    Assert.AreEqual(4, list.Count);
    var texts := '';
    for var r in list do
      texts := texts + '[' + r.Text + ']';
    Assert.IsTrue(texts.Contains('[123123]'), texts);
    Assert.IsTrue(texts.Contains('[Code 128]'), texts);
    Assert.IsTrue(texts.Contains('[QR-never-gonna-give-you-up]'), texts);
    // GS1: the text starts with a GS character
    Assert.IsTrue(texts.Contains('0104150034194612]'), texts);
  finally
    list.Free;
    scanManager.Free;
    bmp.Free;
  end;
end;

procedure TReadResultTest.GS1HRIFromElementStrings;
begin
  // fixed length (01), variable length (21) ended by GS, then (17) and (10)
  Assert.AreEqual('(01)05909990329717(21)1039(17)270331(10)AB123',
    HRIFromGS1('0105909990329717211039' + #29 + '1727033110AB123'));
  // an AI of 4 digits (net weight with 3 decimals)
  Assert.AreEqual('(3103)001234', HRIFromGS1('3103001234'));
  // too short for (01) and an unknown AI (05)
  Assert.AreEqual('', HRIFromGS1('01059099903297'));
  Assert.AreEqual('', HRIFromGS1('0512345'));
end;

procedure TReadResultTest.GS1HRIOfDataMatrix;
begin
  // the GS1 dot-peen code of a medicine package
  var r := Scan('dm-inverted.jpg', TBarcodeFormat.DATA_MATRIX, 0, true);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.IsTrue(r.IsGS1);
    Assert.AreEqual('(01)05909990329717(21)1039520876635(17)270430(10)NE76571',
      r.GS1HRI);
  finally
    r.Free;
  end;

  // not GS1
  r := Scan('dmc1.png', TBarcodeFormat.DATA_MATRIX);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.IsFalse(r.IsGS1);
    Assert.AreEqual('', r.GS1HRI);
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TReadResultTest);

end.
