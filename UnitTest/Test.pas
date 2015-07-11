unit Test;

interface

uses
  DUnitX.TestFramework, SysUtils, ScanManager, BarcodeFormat, ReadResult,
  FMX.Types, FMX.Graphics, FMX.Objects;

type

  [TestFixture]
  TZXingDelphiTest = class(TObject)
  public
    function GetImage(Filename: string): TBitmap;
    function Decode(Filename: String; CodeFormat: TBarcodeFormat): TReadResult;

    [Test]
    procedure Code128();

    [Test]
    procedure Code93();

  end;

implementation


procedure TZXingDelphiTest.Code128();
var
  result: TReadResult;
begin
  result := Decode('Code128.png', BarcodeFormat.CODE_128);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('1234567'), 'Code 128 result Text Incorrect: ' + result.Text);
end;

procedure TZXingDelphiTest.Code93();
var
  result: TReadResult;
begin
  result := Decode('Code93-1.png', BarcodeFormat.CODE_93);
  Assert.IsNotNull(result, ' nil result ');
  Assert.IsTrue(result.Text.Equals('THIS IS CODE93'), 'Code 93 - 1 result Text Incorrect: ' + result.Text);

	result := Decode('Code93-2.png', BarcodeFormat.CODE_93);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('ABC CODE93-2'), 'Code 93 - 2 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-3.png', BarcodeFormat.CODE_93);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('ABC CODE96'), 'Code 93 - 3 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-3.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('ABC CODE96'), 'Auto Code 93 - 3 result Text Incorrect: ' + result.Text);
end;



////////////////////////////////////////////////////////////////////////////////
//// Helpers below                                                         /////
////////////////////////////////////////////////////////////////////////////////

function TZXingDelphiTest.Decode(Filename: String; CodeFormat: TBarcodeFormat): TReadResult;
var bmp :TBitmap;
    scanManager : TScanManager;
begin

  bmp:=GetImage(Filename);
  scanmanager := TScanManager.Create(CodeFormat,nil);
  result := scanManager.Scan(bmp);

end;

function TZXingDelphiTest.GetImage(Filename: string): TBitmap;
var
  img: TImage;
  fs:string;

begin
  img := TImage.Create(nil);
  result := nil;
  try

    fs := ExtractFileDir(ParamStr(0)) + '\..\..\images\' + Filename;
    img.Bitmap.LoadFromFile(fs);
    result := TBitmap.Create;
    result.Assign(img.Bitmap);

  finally
    img.Free;
  end;
end;


(* TBarcodeFormat.CODE_128
  Result Decode(string file, BarcodeFormat? format = null, KeyValuePair<DecodeHintType, object>[] additionalHints = null)
  {
  var r = GetReader(format, additionalHints);

  var i = GetImage(file);

  var result = r.decode(i); // decode(i);

  return result;
  }





*)

initialization

TDUnitX.RegisterTestFixture(TZXingDelphiTest);

end.
