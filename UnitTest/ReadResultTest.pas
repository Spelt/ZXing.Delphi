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

initialization

TDUnitX.RegisterTestFixture(TReadResultTest);

end.
