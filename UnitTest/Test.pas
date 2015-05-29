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

  end;

implementation


procedure TZXingDelphiTest.Code128();
var
  result: TReadResult;
begin
  result := Decode('Code128.png', BarcodeFormat.CODE_128);
  Assert.IsNotNull(result, ' NULL result ');
  Assert.IsTrue(result.Text.Equals('1234567'), 'result Text Incorrect: ' + result.Text);
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
