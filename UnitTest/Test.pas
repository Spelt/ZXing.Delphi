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

  * Implemented by E. Spelt for Delphi
}

interface

uses
  DUnitX.TestFramework, SysUtils, ScanManager, BarcodeFormat, ReadResult,
  FMX.Types, FMX.Graphics, FMX.Objects;

type

  [TestFixture]
  TZXingDelphiTest = class(TObject)
  private

  public
    function GetImage(Filename: string): TBitmap;
    function Decode(Filename: String; CodeFormat: TBarcodeFormat): TReadResult;

    [Test]
    procedure AllQRCode;

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

  result := Decode('qrcode.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://google.com'),
    'QR code result Text Incorrect: ' + result.Text);

  // From here a test set from: http://datagenetics.com/blog/november12013/index.html
  // Please take a look of what QR can do for you
  // NOTE: some test are expected to fail and are setup as such.

  // !Cancelled for test. Does not work with zxing.net either.
  // Rotation does not work.
  // result := Decode('q3.png', BarcodeFormat.QR_CODE);
  // Assert.IsNotNull(result, ' Nil result ');
  // Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
  // 'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q33.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Contains('Never gonna give you up'),
    'QR code result Text Incorrect: ' + result.Text);
  Assert.IsTrue(result.Text.Contains('Never gonna tell a lie and hurt you'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q1.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q2.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q2q.png', BarcodeFormat.QR_CODE); // rotate 90 degrees
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q2m.png', BarcodeFormat.QR_CODE); // rotate 120 degrees
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q4.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q5.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  // fails on example website but does work!
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q6.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q7.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q8.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q9.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, ' Should be nil result. Missing possition block ');

  result := Decode('q10.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q11.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, ' The code should not scan ');

  result := Decode('q12.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q13.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q14.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q15.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q16.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q17.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, ' Should not scan ');

  result := Decode('q18.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q21.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q22.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q23.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, ' to dizzy to scan');

  result := Decode('q25.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, 'Should not scan');

  result := Decode('q28.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q29.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, 'Should not scan');

  result := Decode('q30.png', BarcodeFormat.QR_CODE);
  Assert.IsNull(result, 'Should not be scanned');

  result := Decode('q31.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q32.png', BarcodeFormat.QR_CODE);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

end;

procedure TZXingDelphiTest.AllCode128();
var
  result: TReadResult;
begin
  result := Decode('Code128.png', BarcodeFormat.CODE_128);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('1234567'),
    'Code 128 result Text Incorrect: ' + result.Text);
end;

procedure TZXingDelphiTest.AllCode93();
var
  result: TReadResult;
begin
  result := Decode('Code93-1.png', BarcodeFormat.CODE_93);
  Assert.IsNotNull(result, ' nil result ');
  Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
    'Code 93 - 1 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-2.png', BarcodeFormat.CODE_93);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('ABC CODE93-2'),
    'Code 93 - 2 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-3.png', BarcodeFormat.CODE_93);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('ABC CODE96'),
    'Code 93 - 3 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-3.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('ABC CODE96'),
    'Auto Code 93 - 3 result Text Incorrect: ' + result.Text);
end;

procedure TZXingDelphiTest.AllCodeITF();
var
  result: TReadResult;
begin
  result := Decode('ITF-1.png', BarcodeFormat.ITF);
  Assert.IsNotNull(result, ' nil result ');
  Assert.IsTrue(result.Text.Equals('55867492279103'),
    'Code ITF - 1 result Text Incorrect: ' + result.Text);

  result := Decode('ITF-2.png', BarcodeFormat.ITF);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('04601234567893'),
    'ITF - 2 result Text Incorrect: ' + result.Text);

  result := Decode('ITF-3.png', BarcodeFormat.ITF);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('12345678900098'),
    'ITF - 3 result Text Incorrect: ' + result.Text);

  result := Decode('ITF-4.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('32145678900098'),
    'ITF - 4 result Text Incorrect: ' + result.Text);

  result := Decode('ITF-5.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('77745678900093'),
    'ITF - 5 result Text Incorrect: ' + result.Text);

end;

procedure TZXingDelphiTest.AutoTypes;
var
  result: TReadResult;
begin
  // Test different types in a random sequence.

  result := Decode('Code128.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('1234567'),
    'Code 128 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-1.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' nil result ');
  Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
    'Code 93 - 1 result Text Incorrect: ' + result.Text);

  result := Decode('Code128.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('1234567'),
    'Code 128 result Text Incorrect: ' + result.Text);

  result := Decode('q4.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('q2.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('Code93-1.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' nil result ');
  Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
    'Code 93 - 1 result Text Incorrect: ' + result.Text);

  result := Decode('Code93-1.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' nil result ');
  Assert.IsTrue(result.Text.Equals('THIS IS CODE93'),
    'Code 93 - 1 result Text Incorrect: ' + result.Text);

  result := Decode('q2.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
    'QR code result Text Incorrect: ' + result.Text);

  result := Decode('ITF-2.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('04601234567893'),
    'ITF - 2 result Text Incorrect: ' + result.Text);

  result := Decode('ITF-3.png', BarcodeFormat.Auto);
  Assert.IsNotNull(result, ' Nil result ');
  Assert.IsTrue(result.Text.Equals('12345678900098'),
    'ITF - 3 result Text Incorrect: ' + result.Text);


end;

/// /////////////////////////////////////////////////////////////////////////////
/// / Helpers below                                                         /////
/// /////////////////////////////////////////////////////////////////////////////

function TZXingDelphiTest.Decode(Filename: String; CodeFormat: TBarcodeFormat)
  : TReadResult;
var
  bmp: TBitmap;
  ScanManager: TScanManager;
begin

  bmp := GetImage(Filename);
  ScanManager := TScanManager.Create(CodeFormat, nil);
  result := ScanManager.Scan(bmp);

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
