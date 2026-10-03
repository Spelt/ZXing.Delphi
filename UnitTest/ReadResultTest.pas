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

initialization

TDUnitX.RegisterTestFixture(TReadResultTest);

end.
