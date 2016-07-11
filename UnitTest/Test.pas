unit Test;
{
  * Copyright 2015 E Spelt for only the test files
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *      http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.

  * Delphi Implementation by E. Spelt and K. Gossens
}

interface

uses
  DUnitX.TestFramework,
  FMX.Types,
  FMX.Graphics,
  FMX.Objects,
  SysUtils,
  ScanManager,
  ZXing.BarcodeFormat,
  ZXing.ReadResult;

type
  [TestFixture]
  TZXingDelphiTest = class(TObject)

  public

    function GetImage(Filename: string): TBitmap;
    function Decode(Filename: String; CodeFormat: TBarcodeFormat): TReadResult;

    [Test]
    procedure AllQRCode;

    [Test]
    procedure AllDataMatrixCode();

    [Test]
    procedure AllCode128();

    [Test]
    procedure AllCode93();

    [Test]
    procedure AllCodeITF;

    [Test]
    procedure AutoTypes();

  end;

implementation

procedure TZXingDelphiTest.AllQRCode();
var
  result: TReadResult;
begin
  try
    result := Decode('qrcode.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://google.com'),
      'QR code result Text Incorrect: ' + result.Text);

    // From here a test set from: http://datagenetics.com/blog/november12013/index.html
    // Please take a look of what QR can do for you
    // NOTE: some test are expected to fail and are setup as such.

    // !Cancelled for test. Does not work with zxing.net either.
    // Rotation does not work.
    // result := Decode('q3.png', TBarcodeFormat.QR_CODE);
    // Assert.IsNotNull(result, ' Nil result ');
    // Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    // 'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q33.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('Never gonna give you up, ' + #$0A +
                                     'Never gonna let you down ' + #$0A +
                                     'Never gonna run around and desert you ' + #$0A +
                                     'Never gonna make you cry, ' + #$0A +
                                     'Never gonna say goodbye ' + #$0A +
                                     'Never gonna tell a lie and hurt you'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q1.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q2.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q2q.png', TBarcodeFormat.QR_CODE); // rotate 90 degrees
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q2m.png', TBarcodeFormat.QR_CODE); // rotate 120 degrees
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q4.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q5.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    // fails on example website but does work!
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q6.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q7.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q8.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q9.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, ' Should be nil result. Missing possition block ');

    result := Decode('q10.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q11.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, ' The code should not scan ');

    result := Decode('q12.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q13.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q14.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q15.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q16.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q17.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, ' Should not scan ');

    result := Decode('q18.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q21.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q22.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q23.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, ' to dizzy to scan');

    result := Decode('q25.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, 'Should not scan');

    result := Decode('q28.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q29.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, 'Should not scan');

    result := Decode('q30.png', TBarcodeFormat.QR_CODE);
    Assert.IsNull(result, 'Should not be scanned');

    result := Decode('q31.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q32.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('qr-a1.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('a1'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('qr-1a.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('1a'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('qr-12.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('12'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('QRHiddenInBottom.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('big QR.png', TBarcodeFormat.QR_CODE);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Contains('Version 40 QR Code can contain up to 1852 chars.'),
       'QR code result Text Incorrect: ' + result.Text);



  finally
    FreeAndNil(result);
  end;
end;

procedure TZXingDelphiTest.AllDataMatrixCode();
var
  result: TReadResult;
begin
  try

    Result := Decode('DatamatrixHiddenInBottom.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://www.2D-IDent.com'),
      'DataMatrix code result Text Incorrect: ' + result.Text);
//    //exit;

    result := Decode('dmc1.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://www.2D-IDent.com'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    // WRONG Encoding: How we can get the correct encoding (umlaut) here... :(
    {result := Decode('dmc2.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('Beispiel für Wikipedia'),
      'DataMatrix code result Text Incorrect: ' + result.Text);}

    result := Decode('dmc3.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('Wikipédia, l''encyclopédie libre'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    // Not working yet
    {result := Decode('dmc4.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('??'),
      'DataMatrix code result Text Incorrect: ' + result.Text);}

    result := Decode('dmc5.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('Pause Hi-Tech' + #$0A +
                                     'Tech tips for the non-geek' + #$0A +
                                     'http://www.pausehitech.com'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    result := Decode('dmc6.bmp', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('12345678'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    result := Decode('dmc7.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('DataMatrix'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    result := Decode('dmc8.jpg', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://www.labeljoy.com'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    result := Decode('dmc9.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123Test123Test123Test123Test123' +
                                     'Test123'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    Result := Decode('dmc8.jpg', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://www.labeljoy.com'),
      'DataMatrix code result Text Incorrect: ' + result.Text);

    result := Decode('Code128.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNull(result, ' Should be Nil result ');








  finally
    FreeAndNil(result);
  end;
end;

procedure TZXingDelphiTest.AllCode128();
var
  result: TReadResult;
begin
  try

    result := Decode('Code128.png', TBarcodeFormat.CODE_128);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('1234567'),
      'Code 128 result Text Incorrect: ' + result.Text);

    result := Decode('Code128red.png', TBarcodeFormat.CODE_128);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('1234567'),
      'Code 128 result Text Incorrect: ' + result.Text);


    result := Decode('Code128HiddenInBottom.png', TBarcodeFormat.CODE_128);
    Assert.IsNotNull(result, ' Nil result: Code128HiddenInBottom');
    Assert.IsTrue(result.Text.Equals('1234567'),
      'Code 128 result Text Incorrect: ' + result.Text);

    result := Decode('Code128HiddenInTop.png', TBarcodeFormat.CODE_128);
      Assert.IsNotNull(result, ' Nil result: Code128HiddenInTop');
      Assert.IsTrue(result.Text.Equals('1234567'),
        'Code 128 result Text Incorrect: ' + result.Text);

  finally
    FreeAndNil(result);
  end;

end;

procedure TZXingDelphiTest.AllCode93();
var
  result: TReadResult;
begin
  try
    result := Decode('Code93-1.png', TBarcodeFormat.CODE_93);
    Assert.IsNotNull(result, ' nil result ');
    Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
      'Code 93 - 1 result Text Incorrect: ' + result.Text);

    result := Decode('Code93-2.png', TBarcodeFormat.CODE_93);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('ABC CODE93-2'),
      'Code 93 - 2 result Text Incorrect: ' + result.Text);

    result := Decode('Code93-3.png', TBarcodeFormat.CODE_93);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('ABC CODE96'),
      'Code 93 - 3 result Text Incorrect: ' + result.Text);

    result := Decode('Code93-3.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('ABC CODE96'),
      'Auto Code 93 - 3 result Text Incorrect: ' + result.Text);
  finally
    FreeAndNil(result);
  end;
end;

procedure TZXingDelphiTest.AllCodeITF();
var
  result: TReadResult;
begin
  result := Decode('q4.png', TBarcodeFormat.ITF);
  try
    Assert.IsNull(result, ' Should be nil result ');

    result := Decode('ITF-1.png', TBarcodeFormat.ITF);
    Assert.IsNotNull(result, ' nil result ');
    Assert.IsTrue(result.Text.Equals('55867492279103'),
      'Code ITF - 1 result Text Incorrect: ' + result.Text);

    result := Decode('ITF-2.png', TBarcodeFormat.ITF);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('04601234567893'),
      'ITF - 2 result Text Incorrect: ' + result.Text);

    result := Decode('ITF-3.png', TBarcodeFormat.ITF);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('12345678900098'),
      'ITF - 3 result Text Incorrect: ' + result.Text);

    result := Decode('ITF-4.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('32145678900098'),
      'ITF - 4 result Text Incorrect: ' + result.Text);

    result := Decode('ITF-5.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('77745678900093'),
      'ITF - 5 result Text Incorrect: ' + result.Text);
  finally
    FreeAndNil(result);
  end;
end;

procedure TZXingDelphiTest.AutoTypes;
var
  result: TReadResult;
begin
  try
    result := Decode('Code128.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('1234567'),
      'Code 128 result Text Incorrect: ' + result.Text);

    result := Decode('Code93-1.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' nil result ');
    Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
      'Code 93 - 1 result Text Incorrect: ' + result.Text);

    result := Decode('Code128.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('1234567'),
      'Code 128 result Text Incorrect: ' + result.Text);

    result := Decode('q4.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('q2.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('Code93-1.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' nil result ');
    Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
      'Code 93 - 1 result Text Incorrect: ' + result.Text);

    result := Decode('Code93-1.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' nil result ');
    Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
      'Code 93 - 1 result Text Incorrect: ' + result.Text);

    result := Decode('q2.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
      'QR code result Text Incorrect: ' + result.Text);

    result := Decode('ITF-2.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('04601234567893'),
      'ITF - 2 result Text Incorrect: ' + result.Text);

    result := Decode('ITF-3.png', TBarcodeFormat.Auto);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('12345678900098'),
      'ITF - 3 result Text Incorrect: ' + result.Text);

    result := Decode('dmc7.png', TBarcodeFormat.DATA_MATRIX);
    Assert.IsNotNull(result, ' Nil result ');
    Assert.IsTrue(result.Text.Equals('DataMatrix'),
      'DataMatrix code result Text Incorrect: ' + result.Text);


  finally
    FreeAndNil(result);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//// Helpers below                                                         /////
////////////////////////////////////////////////////////////////////////////////

function TZXingDelphiTest.Decode(Filename: String; CodeFormat: TBarcodeFormat)
  : TReadResult;
var
  bmp: TBitmap;
  ScanManager: TScanManager;
begin
  bmp := GetImage(Filename);
  try
    ScanManager := TScanManager.Create(CodeFormat, nil);
    result := ScanManager.Scan(bmp);
  finally
    FreeAndNil(bmp);
    FreeAndNil(ScanManager);
  end;
end;

function TZXingDelphiTest.GetImage(Filename: string): TBitmap;
var
  img: TImage;
  fs: string;

begin
  img := TImage.Create(nil);
  try
    fs := ExtractFileDir(ParamStr(0)) + '\..\..\images\' + Filename;
    img.Bitmap.LoadFromFile(fs);
    result := TBitmap.Create;
    result.Assign(img.Bitmap);
  finally
    img.Free;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TZXingDelphiTest);
end.
