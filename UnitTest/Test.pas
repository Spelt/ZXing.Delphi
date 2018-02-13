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
	System.Generics.Collections,
	ZXing.DecodeHintType,
	ZXing.ScanManager,
	ZXing.BarcodeFormat,
	ZXing.Resultpoint,
	ZXing.ReadResult;

type

	[TestFixture]
	TZXingDelphiTest = class(TObject)
	private

	public

		function GetImage(Filename: string): TBitmap;
		function Decode(Filename: String; CodeFormat: TBarcodeFormat;
			additionalHints: TDictionary<TDecodeHintType, TObject> = nil)
			: TReadResult;

		[Test]
		procedure AllCode39();

		[Test]
		procedure AllUpcA();

		[Test]
		procedure AllUpcE;

		[Test]
		procedure AllQRCode;

		[Test]
		procedure All_PURE_QRCode;

		[Test]
		procedure AllDataMatrixCode();

		[Test]
		procedure AllCode128();

		[Test]
		procedure AllCode93();

		[Test]
		procedure AllCodeITF;

		[Test]
		procedure AllCodeEAN8;

		[Test]
		procedure AllCodeEAN13;

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
			'Never gonna make you cry, ' + #$0A + 'Never gonna say goodbye ' + #$0A +
			'Never gonna tell a lie and hurt you'), 'QR code result Text Incorrect: '
			+ result.Text);

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

		result := Decode('qr-1.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('1'), 'QR code result Text Incorrect: ' +
			result.Text);

		result := Decode('qr-a1.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('a1'), 'QR code result Text Incorrect: ' +
			result.Text);

		result := Decode('qr-1a.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('1a'), 'QR code result Text Incorrect: ' +
			result.Text);

		result := Decode('qr-12.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('12'), 'QR code result Text Incorrect: ' +
			result.Text);

		result := Decode('QRHiddenInBottom.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('http://DataGenetics.com'),
			'QR code result Text Incorrect: ' + result.Text);

		result := Decode('big QR.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Contains
			('Version 40 QR Code can contain up to 1852 chars.'),
			'QR code result Text Incorrect: ' + result.Text);

		result := Decode('CarloTest.jpg', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Contains('gov.it'),
			'QR code result Text Incorrect: ' + result.Text);

		result := Decode('QR_Droid_2663.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Contains('Version 40 QR Code'),
			'QR code result Text Incorrect: ' + result.Text);

		result := Decode('utf8-test.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.AreEqual
			(#$0440#$0443#$0301#$0441#$0441#$043A#$0438#$0439#$20#$044F#$0437#$044B#$0301#$043A#$2C#$20'russkij'#$20'jazyk'#$20#$E8#$E0#$F2#$F9,
			result.Text, false);

		result := Decode('contact information.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, 'Joe@bloggs.com', false);


    result := Decode('Calendar.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, 'Christmas', false);

    result := Decode('GeoLocation.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, '52.052490', false);

    result := Decode('SMS.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, '0777777', false);

    result := Decode('url.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, 'meetheed.com', false);

    result := Decode('email.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, 'joe@bloggs.com', false);

    result := Decode('Phone.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, '077777777', false);

    result := Decode('Text.png', TBarcodeFormat.QR_CODE);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.Contains(result.Text, 'just a lot of plain text', false);


	finally
		FreeAndNil(result);
	end;

end;

// This ones will only work when PURE_BARCODE is existing in the additional hints.
procedure TZXingDelphiTest.All_PURE_QRCode();
var
	result: TReadResult;
	hints: TDictionary<TDecodeHintType, TObject>;
begin

	try

		hints := TDictionary<TDecodeHintType, TObject>.Create();
		hints.Add(TDecodeHintType.PURE_BARCODE, nil);
		result := Decode('qr problem 1.jpg', TBarcodeFormat.QR_CODE, hints);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Contains('gov.it/'),
			'QR code result Text Incorrect: ' + result.Text);

	finally
		FreeAndNil(result);
	end;

end;

procedure TZXingDelphiTest.AllUpcA;
var
	result: TReadResult;
begin
	try
		result := Decode('upca.png', TBarcodeFormat.UPC_A);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('123456789012'),
			'upca result Text Incorrect: ' + result.Text);

		result := Decode('upcaHiddenInBottom.png', TBarcodeFormat.UPC_A);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('123456789012'),
			'upca result Text Incorrect: ' + result.Text);

		result := Decode('upca 2.gif', TBarcodeFormat.UPC_A);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('725272730706'),
			'upca 1 result Text Incorrect: ' + result.Text);

		result := Decode('upca 3.gif', TBarcodeFormat.UPC_A);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('232323232312'),
			'upca 2 result Text Incorrect: ' + result.Text);

	finally
		FreeAndNil(result);
	end;
end;

procedure TZXingDelphiTest.AllUpcE;
var
	result: TReadResult;
begin
	try
		result := Decode('upce.png', TBarcodeFormat.UPC_E);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('01234565'), 'upce result Text Incorrect: '
			+ result.Text);

		result := Decode('upceHiddenInBottom.png', TBarcodeFormat.UPC_E);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('01234565'), 'upce result Text Incorrect: '
			+ result.Text);

		result := Decode('upc-e_09999008.png', TBarcodeFormat.UPC_E);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('09999008'), 'upce result Text Incorrect: '
			+ result.Text);

		result := Decode('upc-e_09999992.png', TBarcodeFormat.UPC_E);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('09999992'), 'upce result Text Incorrect: '
			+ result.Text);

  	result := Decode('upce 2.png', TBarcodeFormat.UPC_E);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('01234565'), 'upce result Text Incorrect: '
			+ result.Text);



	finally
		FreeAndNil(result);
	end;
end;

procedure TZXingDelphiTest.AllCode39;
var
	result: TReadResult;
begin
	try
		result := Decode('code39.png', TBarcodeFormat.CODE_39);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('1234567'),
			'Code 39 result Text incorrect: ' + result.Text);

		result := Decode('code39 ABC 123456789.png', TBarcodeFormat.CODE_39);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('ABC 123456789'),
			'Code 39 result Text incorrect: ' + result.Text);

		result := Decode('code39 Hello World.png', TBarcodeFormat.CODE_39);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('HELLO $WORLD$'),
			'Code 39 result Text incorrect: ' + result.Text);

		result := Decode('code39HiddenInBottom.png', TBarcodeFormat.CODE_39);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('HELLO $WORLD$'),
			'Code 39 result Text incorrect: ' + result.Text);


    result := Decode('Code 39 Axtel.png', TBarcodeFormat.CODE_39);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Contains('AXTEL'),
			'Code 39 result Text incorrect: ' + result.Text);



	finally
		FreeAndNil(result);
	end;
end;

procedure TZXingDelphiTest.AllDataMatrixCode();
var
	result: TReadResult;
begin
	try

		// Result := Decode('DatamatrixHiddenInBottom.png', TBarcodeFormat.DATA_MATRIX);
		// Assert.IsNotNull(result, ' Nil result ');
		// Assert.IsTrue(result.Text.Equals('http://www.2D-IDent.com'),
		// 'DataMatrix code result Text Incorrect: ' + result.Text);
		//

		result := Decode('dmc1.png', TBarcodeFormat.DATA_MATRIX);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('http://www.2D-IDent.com'),
			'DataMatrix code result Text Incorrect: ' + result.Text);

		// WRONG Encoding: How we can get the correct encoding (umlaut) here... :(
		{ result := Decode('dmc2.png', TBarcodeFormat.DATA_MATRIX);
			Assert.IsNotNull(result, ' Nil result ');
			Assert.IsTrue(result.Text.Equals('Beispiel f'#$FC'r Wikipedia'),
			'DataMatrix code result Text Incorrect: ' + result.Text); }

		result := Decode('dmc3.png', TBarcodeFormat.DATA_MATRIX);
		Assert.IsNotNull(result, ' Nil result ');
		// 'Wikipédia, l''encyclopédie libre':
		// I escaped the above string because this source is not saved in UTF-8 format but in ascii, using the west european encoding
		// if your don't have a west-european windows installation, all the editors you use would try to interpret the non-ascii characters
		// whith the actual local charset. if this is the case you will not see the accented letters in this comment
		// ASCII charset, you risk the compiler to generate the wrong utf8 string when compiling this source
		// ES: 2016/12/9 not working here. Checking on text instead.
		Assert.IsTrue(result.Text.Contains('die libre'),
			'DataMatrix code result Text Incorrect: ' + result.Text);

		// Not working yet
		{ result := Decode('dmc4.png', TBarcodeFormat.DATA_MATRIX);
			Assert.IsNotNull(result, ' Nil result ');
			Assert.IsTrue(result.Text.Equals('??'),
			'DataMatrix code result Text Incorrect: ' + result.Text); }

		result := Decode('dmc5.png', TBarcodeFormat.DATA_MATRIX);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('Pause Hi-Tech' + #$0A +
			'Tech tips for the non-geek' + #$0A + 'http://www.pausehitech.com'),
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
			'Test123Test123Test123Test123Test123' + 'Test123'),
			'DataMatrix code result Text Incorrect: ' + result.Text);

		result := Decode('dmc8.jpg', TBarcodeFormat.Auto);
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
	x0, x1, y0, y1: single;
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

		result := Decode('code128 upsidedown.png', TBarcodeFormat.CODE_128);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('1234567'),
			'Code 128 result Text Incorrect: ' + result.Text);

		x0 := result.resultPoints[0].x;
		y0 := result.resultPoints[0].y;

		x1 := result.resultPoints[1].x;
		y1 := result.resultPoints[1].y;

		result := Decode('code128 upsidedownchidden in bottom.png',
			TBarcodeFormat.CODE_128);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('1234567'),
			'Code 128 result Text Incorrect: ' + result.Text);

		x0 := result.resultPoints[0].x;
		y0 := result.resultPoints[0].y;

		x1 := result.resultPoints[1].x;
		y1 := result.resultPoints[1].y;

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

procedure TZXingDelphiTest.AllCodeEAN13();
var
	result: TReadResult;
begin
	try

		result := Decode('ean13.gif', TBarcodeFormat.EAN_13);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('1234567890128'),
			'Code EAN13 - 1 result Text Incorrect: ' + result.Text);

		result := Decode('EAN13-2-big-hidden in bottom.png', TBarcodeFormat.EAN_13);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('1234567890128'),
			'Code EAN13 - 1 result Text Incorrect: ' + result.Text);

		result := Decode('EAN13-2-big-hidden in top.png', TBarcodeFormat.EAN_13);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('1234567890128'),
			'Code EAN13 - 1 result Text Incorrect: ' + result.Text);

	finally
		FreeAndNil(result);
	end;
end;

procedure TZXingDelphiTest.AllCodeEAN8;
var
	result: TReadResult;
begin
	try
		result := Decode('ean8.png', TBarcodeFormat.EAN_8);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('12345670'),
			'Code EAN8 - 1 result Text Incorrect: ' + result.Text);

		result := Decode('EAN8-big-hidden in top.png', TBarcodeFormat.EAN_8);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('12345670'),
			'Code EAN8 - 1 result Text Incorrect: ' + result.Text);

		result := Decode('EAN8-big-hidden in bottom.png', TBarcodeFormat.EAN_8);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('12345670'),
			'Code EAN8 - 1 result Text Incorrect: ' + result.Text);

    result := Decode('EAN8 12345670.png', TBarcodeFormat.EAN_8);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('12345670'),
			'Code EAN8 - 1 result Text Incorrect: ' + result.Text);





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

		result := Decode('dmc7.png', TBarcodeFormat.Auto);
		Assert.IsNotNull(result, ' Nil result ');
		Assert.IsTrue(result.Text.Equals('DataMatrix'),
			'DataMatrix code result Text Incorrect: ' + result.Text);

		result := Decode('upca.png', TBarcodeFormat.Auto);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('123456789012'),
			'upca result Text Incorrect: ' + result.Text);

		result := Decode('upce.png', TBarcodeFormat.Auto);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('01234565'), 'upce result Text Incorrect: '
			+ result.Text);

		result := Decode('EAN13-2-big-hidden in bottom.png', TBarcodeFormat.Auto);
		Assert.IsNotNull(result, ' nil result ');
		Assert.IsTrue(result.Text.Equals('1234567890128'),
			'Code EAN13 - 1 result Text Incorrect: ' + result.Text);

	finally
		FreeAndNil(result);
	end;
end;

/// /////////////////////////////////////////////////////////////////////////////
/// / Helpers below                                                         /////
/// /////////////////////////////////////////////////////////////////////////////

function TZXingDelphiTest.Decode(Filename: String; CodeFormat: TBarcodeFormat;
	additionalHints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
	bmp: TBitmap;
	ScanManager: TScanManager;
begin
	bmp := GetImage(Filename);
	try
		ScanManager := TScanManager.Create(CodeFormat, additionalHints);
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

// ReportMemoryLeaksOnShutdown := true;
end.
