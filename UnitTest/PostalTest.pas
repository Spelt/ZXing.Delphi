unit PostalTest;

{
  * Tests for the postal barcodes: KIX, RM4SCC, POSTNET and PLANET drawn (as
  * zint), also upside down and vertical, and the images of ZXing.Net
  * (Intelligent Mail Barcode, POSTNET).
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TPostalTest = class(TObject)
  public
    [Test]
    procedure KIX;
    [Test]
    procedure KIXUpsideDownAndVertical;
    [Test]
    procedure RM4SCC;
    [Test]
    procedure RM4SCCWrongCheckCharacter;
    [Test]
    procedure Postnet;
    [Test]
    procedure Planet;
    [Test]
    procedure PostnetPhoto;
    [Test]
    procedure IMbSamples;
    [Test]
    procedure NotInAuto;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
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
  ZXing.RGBLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader;

const
  RM4_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  // the bars of the RM4SCC and KIX characters (as zint): 0 full,
  // 1 ascender, 2 descender, 3 tracker
  RM4KIX: array [0 .. 35, 0 .. 3] of Byte = ((3, 3, 0, 0), (3, 2, 1, 0),
    (3, 2, 0, 1), (2, 3, 1, 0), (2, 3, 0, 1), (2, 2, 1, 1), (3, 1, 2, 0),
    (3, 0, 3, 0), (3, 0, 2, 1), (2, 1, 3, 0), (2, 1, 2, 1), (2, 0, 3, 1),
    (3, 1, 0, 2), (3, 0, 1, 2), (3, 0, 0, 3), (2, 1, 1, 2), (2, 1, 0, 3),
    (2, 0, 1, 3), (1, 3, 2, 0), (1, 2, 3, 0), (1, 2, 2, 1), (0, 3, 3, 0),
    (0, 3, 2, 1), (0, 2, 3, 1), (1, 3, 0, 2), (1, 2, 1, 2), (1, 2, 0, 3),
    (0, 3, 1, 2), (0, 3, 0, 3), (0, 2, 1, 3), (1, 1, 2, 2), (1, 0, 3, 2),
    (1, 0, 2, 3), (0, 1, 3, 2), (0, 1, 2, 3), (0, 0, 3, 3));

/// <summary>The bars of a KIX (or, with start and stop and the check
/// character, an RM4SCC) of text, as zint.</summary>
function RM4Bars(const text: string; rm4scc: Boolean;
  checkOffset: Integer = 0): TArray<Byte>;
begin
  Result := [];
  if rm4scc then
    Result := [1];
  var top := 0;
  var bottom := 0;
  for var ch in text do
  begin
    var p := RM4_CHARS.IndexOf(ch);
    Result := Result + [RM4KIX[p, 0], RM4KIX[p, 1], RM4KIX[p, 2],
      RM4KIX[p, 3]];
    Inc(top, (p div 6 + 1) mod 6);
    Inc(bottom, (p mod 6 + 1) mod 6);
  end;
  if rm4scc then
  begin
    var row := top mod 6 - 1;
    var column := bottom mod 6 - 1;
    if (row = -1) then
      row := 5;
    if (column = -1) then
      column := 5;
    var c := (6 * row + column + checkOffset) mod 36;
    Result := Result + [RM4KIX[c, 0], RM4KIX[c, 1], RM4KIX[c, 2],
      RM4KIX[c, 3], 0];
  end;
end;

/// <summary>The bars of a POSTNET or PLANET of digits (as zint): tall 1
/// (ascender), short 3 (tracker), with the check digit (+ checkOffset).
/// </summary>
function PostnetBars(const digits: string; planet: Boolean;
  checkOffset: Integer = 0): TArray<Byte>;
const
  TALL: array [0 .. 9, 0 .. 4] of Byte = ((1, 1, 0, 0, 0), (0, 0, 0, 1, 1),
    (0, 0, 1, 0, 1), (0, 0, 1, 1, 0), (0, 1, 0, 0, 1), (0, 1, 0, 1, 0),
    (0, 1, 1, 0, 0), (1, 0, 0, 0, 1), (1, 0, 0, 1, 0), (1, 0, 1, 0, 0));
begin
  Result := [1];
  var sum := 0;
  var all := digits;
  for var c in digits do
    Inc(sum, Ord(c) - Ord('0'));
  all := all + Chr(Ord('0') + ((10 - sum mod 10) mod 10 + checkOffset) mod 10);
  for var c in all do
    for var k := 0 to 4 do
      if ((TALL[Ord(c) - Ord('0'), k] = 1) xor planet) then
        Result := Result + [1]
      else
        Result := Result + [3];
  Result := Result + [1];
end;

/// <summary>Reads the bars drawn in an image (a bar and a space of 3 pixels,
/// ascender, tracker and descender parts of 9, 6 and 9 pixels), turned
/// upside down and/or vertical, with the format; the caller frees the
/// result.</summary>
function ReadBars(const bars: TArray<Byte>; format: TBarcodeFormat;
  upsideDown: Boolean = false; vertical: Boolean = false): TReadResult;
const
  MODULE = 3;
  QUIET = 30;
  ASC = 3;
  TRK = 2;
  DESC = 3;
begin
  var length := 2 * QUIET + (2 * System.Length(bars) - 1) * MODULE;
  var height := 2 * QUIET + (ASC + TRK + DESC) * MODULE;
  var pixels: TArray<Byte>;
  SetLength(pixels, length * height);
  FillChar(pixels[0], System.Length(pixels), 255);
  for var i := 0 to High(bars) do
  begin
    var top := QUIET + ASC * MODULE;
    var bottom := QUIET + (ASC + TRK) * MODULE;
    if (bars[i] in [0, 1]) then
      top := QUIET;
    if (bars[i] in [0, 2]) then
      bottom := QUIET + (ASC + TRK + DESC) * MODULE;
    for var y := top to bottom - 1 do
      for var x := QUIET + 2 * i * MODULE to QUIET + (2 * i + 1) * MODULE - 1 do
      begin
        var px := x;
        var py := y;
        if upsideDown then
        begin
          px := length - 1 - x;
          py := height - 1 - y;
        end;
        if vertical then
          pixels[px * height + (height - 1 - py)] := 0
        else
          pixels[py * length + px] := 0;
      end;
  end;
  var w := length;
  var h := height;
  if vertical then
  begin
    w := height;
    h := length;
  end;

  var source := TRGBLuminanceSource.Create(pixels, w, h, TBitmapFormat.Gray8);
  var binarizer := THybridBinarizer.Create(source);
  var image := TBinaryBitmap.Create(binarizer);
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  var list := TList<TBarcodeFormat>.Create;
  var reader := TMultiFormatReader.Create;
  try
    if (format <> TBarcodeFormat.Auto) then
    begin
      list.Add(format);
      hints.Add(TDecodeHintType.POSSIBLE_FORMATS, list);
    end;
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

procedure TPostalTest.KIX;
begin
  var r := ReadBars(RM4Bars('2500GG30250', false), TBarcodeFormat.KIX);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.KIX), Ord(r.BarcodeFormat));
    Assert.AreEqual('2500GG30250', r.Text);
  finally
    r.Free;
  end;
end;

procedure TPostalTest.KIXUpsideDownAndVertical;
begin
  // KIX has no start or stop: the postcode in front tells the direction
  for var upsideDown in [false, true] do
    for var vertical in [false, true] do
    begin
      var r := ReadBars(RM4Bars('1231FZ13XHS', false), TBarcodeFormat.KIX,
        upsideDown, vertical);
      try
        Assert.IsNotNull(r, Format(' Nil result %s %s',
          [BoolToStr(upsideDown, true), BoolToStr(vertical, true)]));
        Assert.AreEqual('1231FZ13XHS', r.Text);
      finally
        r.Free;
      end;
    end;
end;

procedure TPostalTest.RM4SCC;
begin
  for var upsideDown in [false, true] do
  begin
    var r := ReadBars(RM4Bars('LE28HS9Z', true), TBarcodeFormat.RM4SCC,
      upsideDown);
    try
      Assert.IsNotNull(r, ' Nil result ');
      Assert.AreEqual(Ord(TBarcodeFormat.RM4SCC), Ord(r.BarcodeFormat));
      // without the check character
      Assert.AreEqual('LE28HS9Z', r.Text);
    finally
      r.Free;
    end;
  end;
end;

procedure TPostalTest.RM4SCCWrongCheckCharacter;
begin
  var r := ReadBars(RM4Bars('LE28HS9Z', true, 1), TBarcodeFormat.RM4SCC);
  try
    Assert.IsNull(r, 'RM4SCC with a wrong check character');
  finally
    r.Free;
  end;
end;

procedure TPostalTest.Postnet;
begin
  for var digits in ['94306', '943061234', '94306123456'] do
    for var upsideDown in [false, true] do
    begin
      var r := ReadBars(PostnetBars(digits, false), TBarcodeFormat.POSTNET,
        upsideDown, upsideDown);
      try
        Assert.IsNotNull(r, ' Nil result ' + digits);
        Assert.AreEqual(Ord(TBarcodeFormat.POSTNET), Ord(r.BarcodeFormat));
        // without the check digit
        Assert.AreEqual(digits, r.Text);
      finally
        r.Free;
      end;
    end;

  var r := ReadBars(PostnetBars('94306', false, 1), TBarcodeFormat.POSTNET);
  try
    Assert.IsNull(r, 'POSTNET with a wrong check digit');
  finally
    r.Free;
  end;
end;

procedure TPostalTest.Planet;
begin
  var r := ReadBars(PostnetBars('40123456789', true), TBarcodeFormat.PLANET);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.PLANET), Ord(r.BarcodeFormat));
    Assert.AreEqual('40123456789', r.Text);
  finally
    r.Free;
  end;

  // a PLANET is no POSTNET
  r := ReadBars(PostnetBars('40123456789', true), TBarcodeFormat.POSTNET);
  try
    Assert.IsNull(r, 'PLANET read as POSTNET');
  finally
    r.Free;
  end;
end;

procedure TPostalTest.PostnetPhoto;
begin
  // the POSTNET below the IMb in a photo of ZXing.Net (ZIP+4 94704-9844)
  var bmp := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\images\zxing-net\imb-1\10.jpg');
  var scanManager := TScanManager.Create(TBarcodeFormat.POSTNET, nil);
  try
    var r := scanManager.Scan(bmp);
    try
      Assert.IsNotNull(r, ' Nil result ');
      Assert.AreEqual('947049844', r.Text);
    finally
      r.Free;
    end;
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

procedure TPostalTest.IMbSamples;
begin
  // the Intelligent Mail Barcode images of ZXing.Net (it reads 1 of them
  // without TRY_HARDER): at least 8, the texts as expected
  var read := 0;
  for var name in ['01.png', '02.png', '03.png', '04.png', '05.gif', '06.png',
    '07.png', '08.jpg', '09.png', '10.jpg'] do
  begin
    var path := ExtractFileDir(ParamStr(0)) + '\..\..\images\zxing-net\imb-1\';
    var expected := Trim(TFile.ReadAllText(path + ChangeFileExt(name,
      '.txt')));
    var bmp := LoadImage(path + name);
    var scanManager := TScanManager.Create(TBarcodeFormat.IMB, nil);
    try
      var r := scanManager.Scan(bmp);
      try
        if (r <> nil) then
        begin
          Assert.AreEqual(expected, r.Text, name);
          Assert.AreEqual(Ord(TBarcodeFormat.IMB), Ord(r.BarcodeFormat));
          Inc(read);
        end;
      finally
        r.Free;
      end;
    finally
      scanManager.Free;
      bmp.Free;
    end;
  end;
  Assert.IsTrue(read >= 8, IntToStr(read) + ' of 10 read');
end;

procedure TPostalTest.NotInAuto;
begin
  var r := ReadBars(RM4Bars('2500GG30250', false), TBarcodeFormat.Auto);
  try
    Assert.IsNull(r, 'KIX read in Auto');
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TPostalTest);

end.
