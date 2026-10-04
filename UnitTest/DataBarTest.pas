unit DataBarTest;

{
  * Tests for the GS1 DataBar readers: DataBar (omnidirectional and
  * stacked), DataBar Limited and DataBar Expanded (also stacked).
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDataBarTest = class(TObject)
  public
    [Test]
    procedure Omnidirectional;
    [Test]
    procedure Stacked;
    [Test]
    procedure Limited;
    [Test]
    procedure Expanded;
    [Test]
    procedure ExpandedVariableLength;
    [Test]
    procedure ExpandedStacked;
    [Test]
    procedure OtherFormatNotRead;
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
  ZXing.ReadResult;

function ImagePath(const fileName: string): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\zxing-cpp\' +
    fileName;
end;

/// <summary>Scans the image without hints; the caller frees the result.
/// </summary>
function Scan(const fileName: string; format: TBarcodeFormat): TReadResult;
begin
  var bmp := LoadImage(ImagePath(fileName));
  var scanManager := TScanManager.Create(format, nil);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

procedure TDataBarTest.Omnidirectional;
begin
  var r := Scan('databarOmni-1\1.png', TBarcodeFormat.RSS_14);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.RSS_14), Ord(r.BarcodeFormat));
    Assert.AreEqual('0104412345678909', r.Text);
    Assert.AreEqual(']e0', r.SymbologyIdentifier);
    Assert.IsTrue(r.IsGS1, 'not GS1');
    Assert.AreEqual('(01)04412345678909', r.GS1HRI);
  finally
    r.Free;
  end;
end;

procedure TDataBarTest.Stacked;
begin
  // the two pairs on different rows: the position has 4 corners
  var r := Scan('databarStk-1\2.png', TBarcodeFormat.RSS_14);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('0100821935106427', r.Text);
    var p := r.Position;
    Assert.AreEqual(4, Length(p));
    Assert.IsTrue(p[3].Y > p[0].Y, 'bottom not below top');
  finally
    r.Free;
  end;
end;

procedure TDataBarTest.Limited;
begin
  var r := Scan('databarLtd-1\00.png', TBarcodeFormat.RSS_LIMITED);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.RSS_LIMITED), Ord(r.BarcodeFormat));
    Assert.AreEqual('0100000000000000', r.Text);
    Assert.AreEqual(']e0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TDataBarTest.Expanded;
begin
  // GTIN, weight (3103) and date (15)
  var r := Scan('databarExp-3\1.png', TBarcodeFormat.RSS_EXPANDED);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.RSS_EXPANDED), Ord(r.BarcodeFormat));
    Assert.AreEqual('0190012345678908310301223315991231', r.Text);
    Assert.AreEqual('(01)90012345678908(3103)012233(15)991231', r.GS1HRI);
  finally
    r.Free;
  end;
end;

procedure TDataBarTest.ExpandedVariableLength;
begin
  // variable length fields end with a GS
  var r := Scan('databarExp-1\10.png', TBarcodeFormat.RSS_EXPANDED);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('01988987654321061599123131030017501012A' + #29 + '422123' +
      #29 + '21123456' + #29 + '423012345678901', r.Text);
  finally
    r.Free;
  end;
end;

procedure TDataBarTest.ExpandedStacked;
begin
  var r := Scan('databarExpStk-1\1.png', TBarcodeFormat.RSS_EXPANDED);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('0190012345678908310301223315991231', r.Text);
  finally
    r.Free;
  end;
end;

procedure TDataBarTest.OtherFormatNotRead;
begin
  // a DataBar Expanded is not read as DataBar or DataBar Limited
  var r := Scan('databarExp-3\1.png', TBarcodeFormat.RSS_14);
  try
    Assert.IsNull(r, 'read as DataBar');
  finally
    r.Free;
  end;
  r := Scan('databarExp-3\1.png', TBarcodeFormat.RSS_LIMITED);
  try
    Assert.IsNull(r, 'read as DataBar Limited');
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TDataBarTest);

end.
