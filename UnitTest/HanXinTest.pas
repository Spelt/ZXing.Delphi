unit HanXinTest;

{
  * Tests for Han Xin Code: the symbols of the tests of zint decoded from
  * their modules, drawn (turned, in perspective, mirrored, damaged) and
  * read, and the examples of Wikimedia Commons.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  THanXinTest = class(TObject)
  public
    [Test]
    procedure ZintVectors;
    [Test]
    procedure Drawn;
    [Test]
    procedure Perspective;
    [Test]
    procedure Mirrored;
    [Test]
    procedure Damaged;
    [Test]
    procedure Wikimedia;
    [Test]
    procedure NoHanXin;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.DecodeHintType,
  ZXing.ReadResult,
  ZXing.RGBLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  VCL.Graphics,
{$ENDIF}
  Benchmark.Images,
  ZXing.ScanManager,
  ZXing.HanXin.Decoder;

type
  THanXinVector = record
    ECI: Integer;
    Text: string;
    Size: Integer;
    Modules: TArray<Boolean>;
  end;

var
  Vectors: TArray<THanXinVector>;

function ImagesDir: string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\';
end;

/// <summary>The symbols of zint (hanxin\zint-vectors.txt), read once.
/// </summary>
function ZintSymbols: TArray<THanXinVector>;
begin
  if (Vectors = nil) then
  begin
    var lines := TStringList.Create;
    try
      lines.LoadFromFile(ImagesDir + 'hanxin\zint-vectors.txt');
      for var line in lines do
      begin
        var f := line.Split([#9]);
        var v: THanXinVector;
        v.ECI := StrToInt(f[0]);
        var bytes: TBytes;
        SetLength(bytes, Length(f[1]) div 2);
        for var i := 0 to High(bytes) do
          bytes[i] := StrToInt('$' + Copy(f[1], 2 * i + 1, 2));
        v.Text := TEncoding.UTF8.GetString(bytes);
        v.Size := StrToInt(f[2]);
        SetLength(v.Modules, v.Size * v.Size);
        for var r := 0 to v.Size - 1 do
          for var c := 0 to v.Size - 1 do
            v.Modules[r * v.Size + c] := StrToInt('$' + f[3 + r].Chars[c div 4])
              and (8 shr (c mod 4)) <> 0;
        Vectors := Vectors + [v];
      end;
    finally
      lines.Free;
    end;
  end;
  Result := Vectors;
end;

/// <summary>Whether the text of the vector can be compared: not in a
/// character set that Windows does not have (ISO-8859-10, -14 and -16).
/// </summary>
function Comparable(const v: THanXinVector): Boolean;
begin
  Result := (v.ECI <> 12) and (v.ECI <> 16) and (v.ECI <> 18);
end;

/// <summary>Reads the symbol drawn (pitch pixels per module, turned angle
/// degrees, in perspective: the scale along x from 1 - tilt to 1 + tilt,
/// mirrored), asked for as Han Xin Code; the caller frees the result.
/// </summary>
function ReadDrawn(const v: THanXinVector; angle, pitch: Double;
  tilt: Double = 0; mirrored: Boolean = false): TReadResult;
begin
  var n := v.Size;
  var size := Ceil((n + 6) * pitch * 1.5);
  var pixels: TArray<Byte>;
  SetLength(pixels, size * size);
  var a := angle * Pi / 180;
  // each pixel: its module (the inverse of the drawing)
  for var y := 0 to size - 1 do
    for var x := 0 to size - 1 do
    begin
      var px := x + 0.5 - size / 2;
      var py := y + 0.5 - size / 2;
      var g := 1 + tilt * px / (size / 2);
      var ux := px / g;
      var uy := py / g;
      var c := (ux * Cos(a) + uy * Sin(a)) / pitch + n / 2;
      var r := (-ux * Sin(a) + uy * Cos(a)) / pitch + n / 2;
      if mirrored then
      begin
        var t := c;
        c := r;
        r := t;
      end;
      if (c >= 0) and (r >= 0) and (c < n) and (r < n) and
        v.Modules[Floor(r) * n + Floor(c)] then
        pixels[y * size + x] := 0
      else
        pixels[y * size + x] := 255;
    end;
  var source := TRGBLuminanceSource.Create(pixels, size, size,
    TBitmapFormat.Gray8);
  var binarizer := THybridBinarizer.Create(source);
  var image := TBinaryBitmap.Create(binarizer);
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  var list := TList<TBarcodeFormat>.Create;
  var reader := TMultiFormatReader.Create;
  try
    list.Add(TBarcodeFormat.HAN_XIN);
    hints.Add(TDecodeHintType.POSSIBLE_FORMATS, list);
    reader.Hints := hints;
    Result := reader.decode(image, true);
  finally
    reader.Free;
    list.Free;
    hints.Free;
    image.Free;
    binarizer.Free;
    source.Free;
  end;
end;

/// <summary>Checks the result of vector index (and frees it).</summary>
procedure CheckDrawn(index: Integer; r: TReadResult; const what: string);
begin
  try
    var name := Format('vector %d %s', [index, what]);
    Assert.IsNotNull(r, ' Nil result ' + name);
    Assert.AreEqual(Ord(TBarcodeFormat.HAN_XIN), Ord(r.BarcodeFormat), name);
    Assert.AreEqual(ZintSymbols[index].Text, r.Text, name);
    Assert.AreEqual(']h0', r.SymbologyIdentifier, name);
  finally
    r.Free;
  end;
end;

/// <summary>Scans the image as format; the caller frees the result.
/// </summary>
function ScanImage(const name: string; format: TBarcodeFormat): TReadResult;
begin
  var bmp := LoadImage(ImagesDir + name);
  try
    var scanManager := TScanManager.Create(format, nil);
    try
      Result := scanManager.Scan(bmp);
    finally
      scanManager.Free;
    end;
  finally
    bmp.Free;
  end;
end;

{ THanXinTest }

procedure THanXinTest.ZintVectors;
begin
  var symbols := ZintSymbols;
  Assert.AreEqual(81, Length(symbols));
  for var i := 0 to High(symbols) do
  begin
    var decoded: THanXinResult;
    Assert.IsTrue(DecodeHanXin(symbols[i].Modules, symbols[i].Size, decoded),
      'vector ' + IntToStr(i));
    Assert.AreEqual(0, decoded.Errors, 'errors vector ' + IntToStr(i));
    if Comparable(symbols[i]) then
      Assert.AreEqual(symbols[i].Text, decoded.Text, 'vector ' + IntToStr(i));
  end;
end;

procedure THanXinTest.Drawn;
begin
  var symbols := ZintSymbols;
  // all vectors, turned
  for var i := 0 to High(symbols) do
    if Comparable(symbols[i]) then
      for var angle in [0, 30, 135, 250] do
        CheckDrawn(i, ReadDrawn(symbols[i], angle, 3),
          'angle ' + IntToStr(angle));
  // small and larger modules
  for var i := 0 to High(symbols) do
    if Comparable(symbols[i]) then
      for var pitch in [2, 6] do
        CheckDrawn(i, ReadDrawn(symbols[i], 17, pitch),
          'pitch ' + IntToStr(pitch));
end;

procedure THanXinTest.Perspective;
begin
  var symbols := ZintSymbols;
  for var i := 0 to High(symbols) do
    if Comparable(symbols[i]) then
      for var tilt in [-0.15, 0.1, 0.15] do
        CheckDrawn(i, ReadDrawn(symbols[i], 20, 3, tilt),
          'tilt ' + FloatToStr(tilt));
end;

procedure THanXinTest.Mirrored;
begin
  var symbols := ZintSymbols;
  for var i := 0 to High(symbols) do
    if Comparable(symbols[i]) then
      for var angle in [0, 37] do
        CheckDrawn(i, ReadDrawn(symbols[i], angle, 3, 0.1, true),
          'mirrored angle ' + IntToStr(angle));
end;

procedure THanXinTest.Damaged;
begin
  // modules wrong (the check codewords correct them): from version 4 (29
  // x 29) on
  var symbols := ZintSymbols;
  for var i := 0 to High(symbols) do
    if (symbols[i].Size >= 29) and Comparable(symbols[i]) then
    begin
      var size := symbols[i].Size;
      var modules := Copy(symbols[i].Modules);
      // 4 modules, away from the finder patterns and the function
      // information
      for var k := 1 to 4 do
      begin
        var r := 10 + k * (size - 21) div 5;
        var c := size - 11 - k * (size - 21) div 5;
        modules[r * size + c] := not modules[r * size + c];
      end;
      var decoded: THanXinResult;
      Assert.IsTrue(DecodeHanXin(modules, size, decoded),
        'vector ' + IntToStr(i));
      Assert.AreEqual(symbols[i].Text, decoded.Text, 'vector ' + IntToStr(i));
      Assert.IsTrue(decoded.Errors > 0, 'errors vector ' + IntToStr(i));
    end;
end;

procedure THanXinTest.Wikimedia;
const
  SAMPLES: array [0 .. 3, 0 .. 1] of string = (('hanxin-v01-wikimedia.png',
    'Aspose.BarCode'), ('hanxin-v04-wikimedia.png',
    'Aspose.BarCode for 1D & 2D barcodes'), ('hanxin-v22-wikimedia.png',
    'Aspose.BarCode is a powerful development library to generate & ' +
    'recognize 1D & 2D barcodes. Developers can easily add barcode ' +
    'generation and scanning functionality to their applications.'),
    ('hanxin-v22-wikimedia-tilted.jpg', 'Aspose.BarCode is a powerful ' +
    'development library to generate & recognize 1D & 2D barcodes. ' +
    'Developers can easily add barcode generation and scanning ' +
    'functionality to their applications.'));
begin
  for var s := 0 to High(SAMPLES) do
  begin
    var r := ScanImage('hanxin\' + SAMPLES[s, 0], TBarcodeFormat.HAN_XIN);
    try
      Assert.IsNotNull(r, ' Nil result ' + SAMPLES[s, 0]);
      Assert.AreEqual(Ord(TBarcodeFormat.HAN_XIN), Ord(r.BarcodeFormat));
      Assert.AreEqual(SAMPLES[s, 1], r.Text, SAMPLES[s, 0]);
    finally
      r.Free;
    end;
  end;
  // version 84
  var r := ScanImage('hanxin\hanxin-v84-wikimedia.png', TBarcodeFormat.HAN_XIN);
  try
    Assert.IsNotNull(r, ' Nil result version 84');
    Assert.IsTrue(r.Text.StartsWith('Aspose.BarCode for .NET supports ' +
      'multiple features'), r.Text);
    Assert.IsTrue(r.Text.Contains('- Image rotation at any angle'), r.Text);
    Assert.IsTrue(r.Text.TrimRight.EndsWith('provided in System ' +
      'Requirements.'), r.Text);
  finally
    r.Free;
  end;
end;

procedure THanXinTest.NoHanXin;
begin
  // other symbols and text: no Han Xin Code
  for var name in ['QR_Droid_2663.png', 'Text.png', 'datamatrix.png',
    'aztec.png', 'Calendar.png', 'MaxiCode-in-photo-1.webp',
    'dotcode\dotcode-57x60-wikimedia.png'] do
  begin
    var r := ScanImage(name, TBarcodeFormat.HAN_XIN);
    try
      Assert.IsNull(r, name);
    finally
      r.Free;
    end;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(THanXinTest);

end.
