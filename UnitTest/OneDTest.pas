unit OneDTest;

{
  * Tests for the 1D readers: the hints ALLOWED_EAN_EXTENSIONS and
  * ALLOWED_LENGTHS, and Code 128 FNC4 (extended characters) in code set A.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TOneDTest = class(TObject)
  public
    [Test]
    procedure RequiredEANAddOn;
    [Test]
    procedure RequiredEANAddOnMissing;
    [Test]
    procedure AllowedExtensionsAsArray;
    [Test]
    procedure ITFAllowedLengths;
    [Test]
    procedure Code128FNC4CodeSetA;
    [Test]
    procedure BitArraySetRange;
    [Test]
    procedure Codabar;
    [Test]
    procedure TelepenAlpha;
    [Test]
    procedure TelepenNumeric;
    [Test]
    procedure DXFilmEdge;
    [Test]
    procedure DXFilmEdgeWithFrameNumber;
    [Test]
    procedure Code32;
    [Test]
    procedure PZN;
    [Test]
    procedure MSI;
    [Test]
    procedure Plessey;
    [Test]
    procedure Pharmacode;
    [Test]
    procedure NotInAuto;
    [Test]
    procedure MSISamples;
    [Test]
    procedure Code11;
    [Test]
    procedure Code2of5;
    [Test]
    procedure PharmacodeTwoTrack;
    [Test]
    procedure ZintVectors;
    [Test]
    procedure KoreaPost;
    [Test]
    procedure FIM;
    [Test]
    procedure DeutschePost;
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
  ZXing.ResultMetadataType,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.OneD.Code128Reader,
  ZXing.RGBLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader;

type
  // access to the protected decodePattern
  TCode128Access = class(TCode128Reader);

function ImagePath(const fileName: string): string;
begin
  Result := ExtractFileDir(ParamStr(0)) + '\..\..\images\' + fileName;
end;

/// <summary>Scans the image with the hints (freed by the scan manager);
/// the caller frees the result.</summary>
function Scan(const fileName: string; format: TBarcodeFormat;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  var bmp := LoadImage(ImagePath(fileName));
  var scanManager := TScanManager.Create(format, hints);
  try
    Result := scanManager.Scan(bmp);
  finally
    scanManager.Free;
    bmp.Free;
  end;
end;

function Extension(r: TReadResult): string;
begin
  Result := '';
  var meta: IMetaData;
  var ext: IStringMetadata;
  if (r.ResultMetaData <> nil) and r.ResultMetaData.TryGetValue
    (TResultMetadataType.UPC_EAN_EXTENSION, meta) and
    Supports(meta, IStringMetadata, ext) then
    Result := ext.Value;
end;

procedure TOneDTest.RequiredEANAddOn;
begin
  // an EAN-13 with a 5 digit add-on; the scan manager frees the hint value
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS,
    TIntegerArrayHint.Create([2, 5]));
  var r := Scan('zxing-cpp\ean13-ext-1\1.png', TBarcodeFormat.EAN_13, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('9780735200449', r.Text);
    Assert.AreEqual('51299', Extension(r));
    Assert.AreEqual(']E3', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.RequiredEANAddOnMissing;
begin
  // an EAN-13 without add-on: no result when one is required
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS,
    TIntegerArrayHint.Create([2, 5]));
  var r := Scan('zxing-cpp\ean13-1\01.webp', TBarcodeFormat.EAN_13, hints);
  try
    Assert.IsNull(r, 'result without add-on');
  finally
    r.Free;
  end;

  // without the hint it is read
  r := Scan('zxing-cpp\ean13-1\01.webp', TBarcodeFormat.EAN_13, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('9780596008574', r.Text);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.AllowedExtensionsAsArray;
begin
  // as in older versions: an array cast to TObject, kept alive by the
  // caller; the scan manager must not free it
  var lengths: TArray<Integer> := [5];
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS, TObject(Pointer(lengths)));
  var r := Scan('zxing-cpp\ean13-ext-1\1.png', TBarcodeFormat.EAN_13, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('51299', Extension(r));
  finally
    r.Free;
  end;
  Assert.AreEqual(5, lengths[0]);
end;

procedure TOneDTest.ITFAllowedLengths;
begin
  // an ITF of 10 digits
  var r := Scan('zxing-cpp\itf-1\13.webp', TBarcodeFormat.ITF, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(10, r.Text.Length);
  finally
    r.Free;
  end;

  // only 8 or 12 digits allowed
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.ALLOWED_LENGTHS, TIntegerArrayHint.Create([8, 12]));
  r := Scan('zxing-cpp\itf-1\13.webp', TBarcodeFormat.ITF, hints);
  try
    Assert.IsNull(r, 'result of a length that is not allowed');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.Code128FNC4CodeSetA;
const
  // start A, FNC4, 'A' (33, with FNC4: 'A' + 128), 'B' (34), checksum
  // (103 + 1 * 101 + 2 * 33 + 3 * 34) mod 103 = 63, stop
  CODES: array [0 .. 5] of array [0 .. 5] of Integer = ((2, 1, 1, 4, 1, 2),
    (3, 1, 1, 1, 4, 1), (1, 1, 1, 3, 2, 3), (1, 3, 1, 1, 2, 3),
    (1, 1, 1, 2, 2, 4), (2, 3, 3, 1, 1, 1));
  MODULE = 3;
  QUIET = 40;
begin
  // the row: quiet zone, the codes, the termination bar of the stop code
  // (2 modules) and a quiet zone
  var widths: TArray<Integer> := [];
  for var c := 0 to High(CODES) do
    for var w in CODES[c] do
      widths := widths + [w];
  widths := widths + [2];
  var total := 0;
  for var w in widths do
    Inc(total, w * MODULE);
  var row := TBitArrayHelpers.CreateBitArray(total + 2 * QUIET);
  var x := QUIET;
  for var i := 0 to High(widths) do
  begin
    if not Odd(i) then
      row.setRange(x, x + widths[i] * MODULE);
    Inc(x, widths[i] * MODULE);
  end;

  // 'A' is 32 + 33, with FNC4 32 + 33 + 128
  var expected := Char(32 + 33 + 128) + 'B';

  var reader := TCode128Reader.Create;
  try
    // the decoder of before
    var r := reader.decodeRow(0, row, nil);
    try
      Assert.IsNotNull(r, ' Nil result (decodeRow) ');
      Assert.AreEqual(expected, r.Text);
    finally
      r.Free;
    end;

    // the decoder of zxing-cpp
    var bars: TPatternRow;
    GetPatternRow(row, row.Size, bars);
    var view := TPatternView.Create(bars);
    r := TCode128Access(reader).decodePattern(0, view, nil);
    try
      Assert.IsNotNull(r, ' Nil result (decodePattern) ');
      Assert.AreEqual(expected, r.Text);
    finally
      r.Free;
    end;
  finally
    reader.Free;
  end;
end;

procedure TOneDTest.BitArraySetRange;
begin
  // within one word, over word boundaries, and empty
  var row := TBitArrayHelpers.CreateBitArray(100);
  row.setRange(3, 7);
  row.setRange(30, 70);
  row.setRange(80, 80);
  for var i := 0 to 99 do
    Assert.AreEqual((i >= 3) and (i < 7) or (i >= 30) and (i < 70), row[i],
      'bit ' + IntToStr(i));
end;

procedure TOneDTest.Codabar;
begin
  // without the start and stop characters, like Java
  var r := Scan('zxing-cpp\codabar-1\01.webp', TBarcodeFormat.CODABAR, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('1234567890', r.Text);
    Assert.AreEqual(']F0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;

  // with them
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  hints.Add(TDecodeHintType.RETURN_CODABAR_START_END, nil);
  r := Scan('zxing-cpp\codabar-1\01.webp', TBarcodeFormat.CODABAR, hints);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('A1234567890A', r.Text);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.TelepenAlpha;
begin
  var r := Scan('zxing-cpp\telepen-1\telepen-alpha-2.png',
    TBarcodeFormat.TELEPEN, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('TELEPEN', r.Text);
    Assert.AreEqual(']B0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.TelepenNumeric;
begin
  var r := Scan('zxing-cpp\telepen-1\telepen-numeric-1.png',
    TBarcodeFormat.TELEPEN, nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('01234567', r.Text);
    Assert.AreEqual(']B1', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.DXFilmEdge;
begin
  // product number 10, generation 3
  var r := Scan('zxing-cpp\dxfilmedge-1\3.webp', TBarcodeFormat.DX_FILM_EDGE,
    nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('10-3', r.Text);
    Assert.AreEqual(']XF', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.DXFilmEdgeWithFrameNumber;
begin
  var r := Scan('zxing-cpp\dxfilmedge-1\2.png', TBarcodeFormat.DX_FILM_EDGE,
    nil);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual('80-11/23', r.Text);
  finally
    r.Free;
  end;
end;

/// <summary>Reads the bars and spaces of widths (in modules, starting with a
/// bar), drawn in an image with quiet zones, with the formats (and the hint
/// ASSUME_MSI_CHECK_DIGIT); the caller frees the result.</summary>
function ReadBars(const widths: TArray<Integer>;
  const formats: array of TBarcodeFormat; msiCheckDigit: Boolean = false)
  : TReadResult;
const
  MODULE = 2;
  QUIET = 20;
  HEIGHT = 40;
begin
  var width := 2 * QUIET * MODULE;
  for var w in widths do
    Inc(width, w * MODULE);
  var row: TArray<Byte>;
  SetLength(row, width);
  FillChar(row[0], width, 255);
  var x := QUIET * MODULE;
  for var i := 0 to High(widths) do
  begin
    if not Odd(i) then
      FillChar(row[x], widths[i] * MODULE, 0);
    Inc(x, widths[i] * MODULE);
  end;
  var pixels: TArray<Byte>;
  SetLength(pixels, width * HEIGHT);
  for var y := 0 to HEIGHT - 1 do
    Move(row[0], pixels[y * width], width);

  var source := TRGBLuminanceSource.Create(pixels, width, HEIGHT,
    TBitmapFormat.Gray8);
  var binarizer := THybridBinarizer.Create(source);
  var image := TBinaryBitmap.Create(binarizer);
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  var list := TList<TBarcodeFormat>.Create;
  var reader := TMultiFormatReader.Create;
  try
    list.AddRange(formats);
    if (list.Count > 0) then
      hints.Add(TDecodeHintType.POSSIBLE_FORMATS, list);
    if msiCheckDigit then
      hints.Add(TDecodeHintType.ASSUME_MSI_CHECK_DIGIT, nil);
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

/// <summary>Reads a Code 39 of chars (without the start and stop
/// characters) with the formats; the caller frees the result.</summary>
function ReadCode39(const chars: string;
  const formats: array of TBarcodeFormat): TReadResult;
const
  ALPHABET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%*';
  ENCODINGS: array [0 .. 43] of Integer = ($034, $121, $061, $160, $031, $130,
    $070, $025, $124, $064, $109, $049, $148, $019, $118, $058, $00D, $10C,
    $04C, $01C, $103, $043, $142, $013, $112, $052, $007, $106, $046, $016,
    $181, $0C1, $1C0, $091, $190, $0D0, $085, $184, $0C4, $0A8, $0A2, $08A,
    $02A, $094);
begin
  // narrow 1, wide 3, a narrow space between the characters
  var widths: TArray<Integer> := [];
  for var c in '*' + chars + '*' do
  begin
    if (Length(widths) > 0) then
      widths := widths + [1];
    var pattern := ENCODINGS[ALPHABET.IndexOf(c)];
    for var j := 8 downto 0 do
      if (pattern shr j) and 1 = 1 then
        widths := widths + [3]
      else
        widths := widths + [1];
  end;
  Result := ReadBars(widths, formats);
end;

procedure TOneDTest.Code32;
const
  // 6 characters base 32 of 012345676 (the last digit the check digit)
  TABELLA = '0123456789BCDFGHJKLMNPQRSTUVWXYZ';
begin
  var value := 12345676;
  var chars := '';
  for var i := 1 to 6 do
  begin
    chars := TABELLA.Chars[value mod 32] + chars;
    value := value div 32;
  end;

  var r := ReadCode39(chars, [TBarcodeFormat.CODE_32]);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.CODE_32), Ord(r.BarcodeFormat));
    Assert.AreEqual('A012345676', r.Text);
  finally
    r.Free;
  end;

  // in Auto as before: Code 39
  r := ReadCode39(chars, []);
  try
    Assert.IsNotNull(r, ' Nil result (Auto) ');
    Assert.AreEqual(Ord(TBarcodeFormat.CODE_39), Ord(r.BarcodeFormat));
    Assert.AreEqual(chars, r.Text);
  finally
    r.Free;
  end;

  // a wrong check digit (012345675): Code 39, not Code 32
  value := 12345675;
  chars := '';
  for var i := 1 to 6 do
  begin
    chars := TABELLA.Chars[value mod 32] + chars;
    value := value div 32;
  end;
  r := ReadCode39(chars, [TBarcodeFormat.CODE_32, TBarcodeFormat.CODE_39]);
  try
    Assert.IsNotNull(r, ' Nil result (wrong check digit) ');
    Assert.AreEqual(Ord(TBarcodeFormat.CODE_39), Ord(r.BarcodeFormat));
  finally
    r.Free;
  end;

  // only Code 32 asked for: no other Code 39
  r := ReadCode39('HELLO', [TBarcodeFormat.CODE_32]);
  try
    Assert.IsNull(r, 'Code 39 when only Code 32 is asked for');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.PZN;
begin
  // 1 * 1 + 2 * 2 + ... + 7 * 7 = 140, modulo 11: 8
  var r := ReadCode39('-12345678', [TBarcodeFormat.PZN]);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.PZN), Ord(r.BarcodeFormat));
    Assert.AreEqual('-12345678', r.Text);
  finally
    r.Free;
  end;

  // a wrong check digit
  r := ReadCode39('-12345679', [TBarcodeFormat.PZN]);
  try
    Assert.IsNull(r, 'PZN with a wrong check digit');
  finally
    r.Free;
  end;

  // in Auto as before: Code 39
  r := ReadCode39('-12345678', []);
  try
    Assert.IsNotNull(r, ' Nil result (Auto) ');
    Assert.AreEqual(Ord(TBarcodeFormat.CODE_39), Ord(r.BarcodeFormat));
  finally
    r.Free;
  end;
end;

/// <summary>The bars and spaces of an MSI of digits (as zint): start wide
/// bar, narrow space; a bit 1 wide bar, narrow space, a bit 0 narrow bar,
/// wide space (1:2), the highest bit first; stop narrow, wide, narrow.
/// </summary>
function MSIWidths(const digits: string): TArray<Integer>;
begin
  Result := [2, 1];
  for var c in digits do
    for var b := 3 downto 0 do
      if ((Ord(c) - Ord('0')) shr b) and 1 = 1 then
        Result := Result + [2, 1]
      else
        Result := Result + [1, 2];
  Result := Result + [1, 2, 1];
end;

/// <summary>The bars and spaces of a Plessey of hexadecimal digits (as
/// zint), with its CRC (or a wrong one).</summary>
function PlesseyWidths(const digits: string; wrongCRC: Boolean = false)
  : TArray<Integer>;
const
  POLYNOMIAL: array [0 .. 8] of Integer = (1, 1, 1, 1, 0, 1, 0, 0, 1);
begin
  var bits: TArray<Integer>;
  SetLength(bits, 4 * Length(digits) + 8);
  for var i := 0 to Length(digits) - 1 do
  begin
    var value := StrToInt('$' + digits.Chars[i]);
    for var b := 0 to 3 do
      bits[4 * i + b] := (value shr b) and 1;
  end;
  // the CRC: the remainder of the polynomial division
  var check := Copy(bits);
  for var i := 0 to 4 * Length(digits) - 1 do
    if (check[i] = 1) then
      for var j := 0 to 8 do
        check[i + j] := check[i + j] xor POLYNOMIAL[j];
  for var i := 0 to 7 do
    bits[4 * Length(digits) + i] := check[4 * Length(digits) + i];
  if wrongCRC then
    bits[High(bits)] := 1 - bits[High(bits)];

  Result := [3, 1, 3, 1, 1, 3, 3, 1];
  for var bit in bits do
    if (bit = 1) then
      Result := Result + [3, 1]
    else
      Result := Result + [1, 3];
  Result := Result + [3, 3, 1, 3, 1, 1, 3, 1, 3];
end;

/// <summary>The bars and spaces of a Pharmacode of value (as zint): narrow
/// bars 1, wide bars 3, spaces 2.</summary>
function PharmacodeWidths(value: Integer): TArray<Integer>;
begin
  // the bars from right to left
  var bars: TArray<Integer> := [];
  repeat
    if Odd(value) then
    begin
      bars := [1] + bars;
      value := (value - 1) div 2;
    end
    else
    begin
      bars := [3] + bars;
      value := (value - 2) div 2;
    end;
  until (value = 0);
  Result := [];
  for var i := 0 to High(bars) do
  begin
    if (i > 0) then
      Result := Result + [2];
    Result := Result + [bars[i]];
  end;
end;

procedure TOneDTest.MSI;
begin
  var r := ReadBars(MSIWidths('1234567'), [TBarcodeFormat.MSI]);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.MSI), Ord(r.BarcodeFormat));
    Assert.AreEqual('1234567', r.Text);
    Assert.AreEqual(']M0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;

  // with the modulo 10 check digit: 1234 has 4
  r := ReadBars(MSIWidths('12344'), [TBarcodeFormat.MSI], true);
  try
    Assert.IsNotNull(r, ' Nil result (check digit) ');
    Assert.AreEqual('12344', r.Text);
    Assert.AreEqual(']M1', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
  r := ReadBars(MSIWidths('12345'), [TBarcodeFormat.MSI], true);
  try
    Assert.IsNull(r, 'MSI with a wrong check digit');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.Plessey;
begin
  var r := ReadBars(PlesseyWidths('12AB09F'), [TBarcodeFormat.PLESSEY]);
  try
    Assert.IsNotNull(r, ' Nil result ');
    Assert.AreEqual(Ord(TBarcodeFormat.PLESSEY), Ord(r.BarcodeFormat));
    Assert.AreEqual('12AB09F', r.Text);
  finally
    r.Free;
  end;

  r := ReadBars(PlesseyWidths('12AB09F', true), [TBarcodeFormat.PLESSEY]);
  try
    Assert.IsNull(r, 'Plessey with a wrong CRC');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.Pharmacode;
begin
  for var value in [3, 4, 1234, 131070] do
  begin
    var r := ReadBars(PharmacodeWidths(value), [TBarcodeFormat.PHARMA_CODE]);
    try
      Assert.IsNotNull(r, ' Nil result ' + IntToStr(value));
      Assert.AreEqual(Ord(TBarcodeFormat.PHARMA_CODE), Ord(r.BarcodeFormat));
      Assert.AreEqual(IntToStr(value), r.Text);
    finally
      r.Free;
    end;
  end;
end;

/// <summary>The bars and spaces of modules ('1' bar, '0' space; the spaces
/// at the ends are left out), as the encode tests of zint write them.</summary>
function ModuleWidths(const symbol: string): TArray<Integer>;
begin
  var modules := symbol.Trim(['0']);
  Result := [];
  var i := 1;
  while (i <= Length(modules)) do
  begin
    var j := i;
    while (j < Length(modules)) and (modules[j + 1] = modules[i]) do
      Inc(j);
    Result := Result + [j - i + 1];
    i := j + 1;
  end;
end;

/// <summary>The widths reversed (the barcode turned 180 degrees).</summary>
function Reversed(const widths: TArray<Integer>): TArray<Integer>;
begin
  SetLength(Result, Length(widths));
  for var i := 0 to High(widths) do
    Result[High(widths) - i] := widths[i];
end;

const
  CODE11_CHARS = '0123456789-';

/// <summary>The modulo 11 check digit of Code 11 data, the weights from the
/// right 1 up to maxWeight (as zint).</summary>
function Code11Check(const data: string; maxWeight: Integer): Char;
begin
  var sum := 0;
  var weight := 1;
  for var i := Length(data) downto 1 do
  begin
    Inc(sum, weight * CODE11_CHARS.IndexOf(data[i]));
    Inc(weight);
    if (weight > maxWeight) then
      weight := 1;
  end;
  Result := CODE11_CHARS.Chars[sum mod 11];
end;

/// <summary>The bars and spaces of a Code 11 of data with checks check
/// digits (C, K) as zint, the wide ones wide modules.</summary>
function Code11Widths(const data: string; checks: Integer;
  wide: Integer = 2): TArray<Integer>;
const
  // wide 1, the last one the start and stop
  PATTERNS: array [0 .. 11] of string = ('00001', '10001', '01001', '11000',
    '00101', '10100', '01100', '00011', '10010', '10000', '00100', '00110');
begin
  var text := data;
  if (checks >= 1) then
    text := text + Code11Check(text, 10);
  if (checks = 2) then
    text := text + Code11Check(text, 9);
  var chars: TArray<Integer> := [11];
  for var c in text do
    chars := chars + [CODE11_CHARS.IndexOf(c)];
  chars := chars + [11];
  Result := [];
  for var k := 0 to High(chars) do
  begin
    for var e := 1 to 5 do
      if (PATTERNS[chars[k]][e] = '1') then
        Result := Result + [wide]
      else
        Result := Result + [1];
    // the narrow space between the characters
    if (k < High(chars)) then
      Result := Result + [1];
  end;
end;

/// <summary>The GS1 (modulo 10) check digit of digits.</summary>
function GS1Check(const digits: string): Char;
begin
  var sum := 0;
  var weight := 3;
  for var i := Length(digits) downto 1 do
  begin
    Inc(sum, weight * (Ord(digits[i]) - Ord('0')));
    weight := 4 - weight;
  end;
  Result := Chr(Ord('0') + (10 - sum mod 10) mod 10);
end;

/// <summary>The bars and spaces of a Code 2 of 5 variant (format) of
/// digits as zint, the wide ones wide modules (the Matrix start and stop
/// bar one more).</summary>
function C25Widths(format: TBarcodeFormat; const digits: string;
  wide: Integer = 3): TArray<Integer>;
const
  DIGIT_PATTERNS: array [0 .. 9] of string = ('00110', '10001', '01001',
    '11000', '00101', '10100', '01100', '00011', '10010', '01010');
begin
  var inBars := (format = TBarcodeFormat.INDUSTRIAL_2_OF_5) or
    (format = TBarcodeFormat.IATA_2_OF_5);
  if (format = TBarcodeFormat.INDUSTRIAL_2_OF_5) then
    Result := [wide, 1, wide, 1, 1, 1]
  else if (format = TBarcodeFormat.MATRIX_2_OF_5) then
    Result := [wide + 1, 1, 1, 1, 1, 1]
  else
    Result := [1, 1, 1, 1];
  for var c in digits do
  begin
    var pattern := DIGIT_PATTERNS[Ord(c) - Ord('0')];
    for var e := 1 to 5 do
    begin
      var w := 1;
      if (pattern[e] = '1') then
        w := wide;
      Result := Result + [w];
      // the digits in the bars: a narrow space after each bar
      if inBars then
        Result := Result + [1];
    end;
    if not inBars then
      Result := Result + [1];
  end;
  if (format = TBarcodeFormat.INDUSTRIAL_2_OF_5) then
    Result := Result + [wide, 1, 1, 1, wide]
  else if (format = TBarcodeFormat.MATRIX_2_OF_5) then
    Result := Result + [wide + 1, 1, 1, 1, 1]
  else
    Result := Result + [wide, 1, 1];
end;

/// <summary>The two tracks of a Pharmacode two-track of value (as zint):
/// the top track and the bottom track, '1' a bar module.</summary>
function TwoTrackModules(value: Integer): TArray<string>;
begin
  var digits := '';
  repeat
    var d := value mod 3;
    if (d = 0) then
      d := 3;
    digits := Chr(Ord('0') + d) + digits;
    value := (value - d) div 3;
  until (value = 0);
  var top := '';
  var bottom := '';
  for var i := 1 to Length(digits) do
  begin
    if (i > 1) then
    begin
      top := top + '0';
      bottom := bottom + '0';
    end;
    if (digits[i] = '2') or (digits[i] = '3') then
      top := top + '1'
    else
      top := top + '0';
    if (digits[i] = '1') or (digits[i] = '3') then
      bottom := bottom + '1'
    else
      bottom := bottom + '0';
  end;
  Result := [top, bottom];
end;

/// <summary>Reads a Pharmacode two-track of the tracks (top, bottom; 3
/// pixels per module, track pixels per track) with the formats, turned 180
/// degrees (upsideDown) or 90 (vertical, the top track on the left); the
/// caller frees the result.</summary>
function ReadTwoTrack(const tracks: TArray<string>;
  const formats: array of TBarcodeFormat; upsideDown: Boolean = false;
  vertical: Boolean = false; track: Integer = 12): TReadResult;
const
  MODULE = 3;
  QUIET = 15;
begin
  var modules := Length(tracks[0]);
  var w := (modules + 2 * QUIET) * MODULE;
  var h := 2 * TRACK + 2 * QUIET * MODULE;
  var pixels: TArray<Byte>;
  SetLength(pixels, w * h);
  FillChar(pixels[0], w * h, 255);
  for var t := 0 to 1 do
    for var m := 0 to modules - 1 do
      if (tracks[t].Chars[m] = '1') then
        for var y := 0 to TRACK - 1 do
          for var x := 0 to MODULE - 1 do
          begin
            var px := (QUIET + m) * MODULE + x;
            var py := QUIET * MODULE + t * TRACK + y;
            if upsideDown then
            begin
              px := w - 1 - px;
              py := h - 1 - py;
            end;
            pixels[py * w + px] := 0;
          end;
  if vertical then
  begin
    // transposed: the top track on the left, read top to bottom
    var turned: TArray<Byte>;
    SetLength(turned, w * h);
    for var y := 0 to h - 1 do
      for var x := 0 to w - 1 do
        turned[x * h + y] := pixels[y * w + x];
    pixels := turned;
    var t := w;
    w := h;
    h := t;
  end;

  var source := TRGBLuminanceSource.Create(pixels, w, h, TBitmapFormat.Gray8);
  var binarizer := THybridBinarizer.Create(source);
  var image := TBinaryBitmap.Create(binarizer);
  var hints := TDictionary<TDecodeHintType, TObject>.Create;
  var list := TList<TBarcodeFormat>.Create;
  var reader := TMultiFormatReader.Create;
  try
    list.AddRange(formats);
    if (list.Count > 0) then
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

/// <summary>The bars and spaces of a Korea Post of a postal code of 6
/// digits (as zint: the last digit first, then the check digit, plus
/// checkOffset).</summary>
function KoreaPostWidths(const code: string; checkOffset: Integer = 0)
  : TArray<Integer>;
const
  // bar, space, ... of the digits (0: no bar)
  TABLE: array [0 .. 9] of string = ('1313150613', '0713131313', '0417131313',
    '1506131313', '0413171313', '17171313', '1315061313', '0413131713',
    '17131713', '13171713');
begin
  var sum := 0;
  for var c in code do
    Inc(sum, Ord(c) - Ord('0'));
  var check := ((10 - sum mod 10) mod 10 + checkOffset) mod 10;
  var digits := '';
  for var i := 6 downto 1 do
    digits := digits + code[i];
  digits := digits + Chr(Ord('0') + check);
  var modules := '';
  for var d in digits do
  begin
    var entry := TABLE[Ord(d) - Ord('0')];
    for var k := 1 to Length(entry) do
      modules := modules + StringOfChar(Chr(Ord('0') + Ord(Odd(k))),
        Ord(entry[k]) - Ord('0'));
  end;
  // (the spaces at the ends are the quiet zones)
  Result := ModuleWidths(modules.Trim(['0']));
end;

/// <summary>The bars and spaces of an ITF of digits (an even number, wide 3
/// times narrow).</summary>
function ITFWidths(const digits: string): TArray<Integer>;
const
  DIGIT_PATTERNS: array [0 .. 9] of string = ('00110', '10001', '01001',
    '11000', '00101', '10100', '01100', '00011', '10010', '01010');
begin
  Result := [1, 1, 1, 1];
  var i := 1;
  while (i < Length(digits)) do
  begin
    var bars := DIGIT_PATTERNS[Ord(digits[i]) - Ord('0')];
    var spaces := DIGIT_PATTERNS[Ord(digits[i + 1]) - Ord('0')];
    for var k := 1 to 5 do
      Result := Result + [1 + 2 * Ord(bars[k] = '1'),
        1 + 2 * Ord(spaces[k] = '1')];
    Inc(i, 2);
  end;
  Result := Result + [3, 1, 1];
end;

/// <summary>The Deutsche Post check digit of digits (the weights from the
/// right 4, 9, 4, ...).</summary>
function DPCheck(const digits: string): Char;
begin
  var sum := 0;
  var weight := 4;
  for var i := Length(digits) downto 1 do
  begin
    Inc(sum, weight * (Ord(digits[i]) - Ord('0')));
    weight := 13 - weight;
  end;
  Result := Chr(Ord('0') + (10 - sum mod 10) mod 10);
end;

procedure TOneDTest.NotInAuto;
begin
  // MSI, Plessey, Pharmacode, Code 11 and the 2 of 5 variants are only
  // read when asked for
  for var widths in [MSIWidths('1234567'), PlesseyWidths('12AB09F'),
    PharmacodeWidths(1234), Code11Widths('123-45', 2),
    C25Widths(TBarcodeFormat.INDUSTRIAL_2_OF_5, '87654321'),
    C25Widths(TBarcodeFormat.IATA_2_OF_5, '87654321'),
    C25Widths(TBarcodeFormat.MATRIX_2_OF_5, '87654321'),
    C25Widths(TBarcodeFormat.DATALOGIC_2_OF_5, '87654321')] do
  begin
    var r := ReadBars(widths, []);
    try
      Assert.IsNull(r, 'read in Auto');
    finally
      r.Free;
    end;
  end;
  var r := ReadTwoTrack(TwoTrackModules(29876543), []);
  try
    Assert.IsNull(r, 'Pharmacode two-track read in Auto');
  finally
    r.Free;
  end;
  r := ReadBars(KoreaPostWidths('123456'), []);
  try
    Assert.IsNull(r, 'Korea Post read in Auto');
  finally
    r.Free;
  end;
  r := ReadTwoTrack(['10100010001000101', '10100010001000101'], [],
    false, false, 20);
  try
    Assert.IsNull(r, 'FIM read in Auto');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.MSISamples;
begin
  // the MSI images of ZXing.Net: like ZXing.Net at least 5 of the 6, also
  // turned 180 degrees, and nothing wrong
  for var turns in [0, 2] do
  begin
    var read := 0;
    for var i := 1 to 6 do
    begin
      var name := Format('zxing-net\msi-1\%.2d', [i]);
      var expected := Trim(TFile.ReadAllText(ImagePath(name + '.txt')));
      var bmp := LoadImage(ImagePath(name + '.png'));
      if (turns <> 0) then
      begin
        var rotated := RotateImage(bmp, turns);
        if (rotated <> bmp) then
        begin
          bmp.Free;
          bmp := rotated;
        end;
      end;
      var scanManager := TScanManager.Create(TBarcodeFormat.MSI, nil);
      try
        var r := scanManager.Scan(bmp);
        try
          if (r <> nil) then
          begin
            Assert.AreEqual(expected, r.Text, name);
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
    Assert.IsTrue(read >= 5, Format('%d of 6 read, turned %d', [read,
      90 * turns]));
  end;
end;

/// <summary>Reads widths with format and checks the text (expected '':
/// nothing read).</summary>
procedure CheckRead(const widths: TArray<Integer>; format: TBarcodeFormat;
  const expected, what: string);
begin
  var r := ReadBars(widths, [format]);
  try
    if (expected = '') then
      Assert.IsNull(r, what)
    else
    begin
      Assert.IsNotNull(r, ' Nil result ' + what);
      Assert.AreEqual(Ord(format), Ord(r.BarcodeFormat), what);
      Assert.AreEqual(expected, r.Text, what);
    end;
  finally
    r.Free;
  end;
end;

procedure TOneDTest.Code11;
begin
  // all characters, 2 and 1 check digits, wide 2 and 3 times narrow, also
  // upside down
  for var wide in [2, 3] do
    for var upsideDown in [false, true] do
    begin
      var widths := Code11Widths('0123456789-', 2, wide);
      if upsideDown then
        widths := Reversed(widths);
      CheckRead(widths, TBarcodeFormat.CODE_11, '0123456789-' +
        Code11Check('0123456789-', 10) + Code11Check('0123456789-' +
        Code11Check('0123456789-', 10), 9), 'Code 11 wide ' + IntToStr(wide));
    end;
  var r := ReadBars(Code11Widths('123-45', 1), [TBarcodeFormat.CODE_11]);
  try
    Assert.IsNotNull(r, ' Nil result (1 check digit) ');
    Assert.AreEqual('123-455', r.Text);
    Assert.AreEqual(']H0', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
  r := ReadBars(Code11Widths('123-45', 2), [TBarcodeFormat.CODE_11]);
  try
    Assert.IsNotNull(r, ' Nil result (2 check digits) ');
    Assert.AreEqual('123-4552', r.Text);
    Assert.AreEqual(']H1', r.SymbologyIdentifier);
  finally
    r.Free;
  end;
  // without (or with a wrong) check digit: nothing
  CheckRead(Code11Widths('123-45', 0), TBarcodeFormat.CODE_11, '',
    'Code 11 without check digit');
  CheckRead(Code11Widths('123-456', 0), TBarcodeFormat.CODE_11, '',
    'Code 11 with a wrong check digit');
end;

procedure TOneDTest.Code2of5;
begin
  // the variants, wide 2 and 3 times narrow, also upside down; another
  // variant does not read them
  var formats: TArray<TBarcodeFormat> := [TBarcodeFormat.INDUSTRIAL_2_OF_5,
    TBarcodeFormat.IATA_2_OF_5, TBarcodeFormat.MATRIX_2_OF_5,
    TBarcodeFormat.DATALOGIC_2_OF_5];
  for var f := 0 to High(formats) do
    for var wide in [2, 3] do
      for var upsideDown in [false, true] do
      begin
        var widths := C25Widths(formats[f], '01234567890', wide);
        if upsideDown then
          widths := Reversed(widths);
        CheckRead(widths, formats[f], '01234567890', 'variant ' +
          IntToStr(f) + ' wide ' + IntToStr(wide));
        CheckRead(widths, formats[(f + 1) mod 4], '', 'variant ' +
          IntToStr(f) + ' read as ' + IntToStr((f + 1) mod 4));
      end;
end;

procedure TOneDTest.PharmacodeTwoTrack;
begin
  for var value in [14, 16, 1234, 29876543] do
    for var vertical in [false, true] do
    begin
      var r := ReadTwoTrack(TwoTrackModules(value),
        [TBarcodeFormat.PHARMA_CODE_TWO_TRACK], false, vertical);
      try
        Assert.IsNotNull(r, ' Nil result ' + IntToStr(value));
        Assert.AreEqual(Ord(TBarcodeFormat.PHARMA_CODE_TWO_TRACK),
          Ord(r.BarcodeFormat));
        Assert.AreEqual(IntToStr(value), r.Text);
      finally
        r.Free;
      end;
    end;
  // turned 180 degrees: read in the other direction with the tracks
  // swapped (1 bottom becomes 2 top), a different value
  var tracks := TwoTrackModules(29876543);
  var value: Int64 := 0;
  var m := Length(tracks[0]) - 1;
  while (m >= 0) do
  begin
    var digit := Ord(tracks[0].Chars[m] = '1') + 2 * Ord(tracks[1].Chars[m] =
      '1');
    value := 3 * value + digit;
    Dec(m, 2);
  end;
  var r := ReadTwoTrack(tracks, [TBarcodeFormat.PHARMA_CODE_TWO_TRACK], true);
  try
    Assert.IsNotNull(r, ' Nil result (upside down) ');
    Assert.AreEqual(IntToStr(value), r.Text);
  finally
    r.Free;
  end;
end;

procedure TOneDTest.ZintVectors;
const
  // the top track, the bottom track, the value
  TWO_TRACK_VECTORS: array [0 .. 1, 0 .. 2] of string =
    (('1010101010101010101010101010101', '1010101010101010101010101010101',
    '64570080'), ('0010100010001010001010001000101',
    '1000101010100000100000101010000', '29876543'));
begin
  // the encode tests of zint (backend/tests, verified against TEC-IT)
  CheckRead(ModuleWidths(
    '101100101101011010010110110010101011010101101101101101011011' +
    '010100101101011001'),
    TBarcodeFormat.CODE_11, '123-4552', 'Code 11 123-45 1');
  CheckRead(ModuleWidths(
    '10110010110101011001010101101010110101011001'),
    TBarcodeFormat.CODE_11, '93--', 'Code 11 93 2');
  CheckRead(ModuleWidths(
    '101100101101011010010110110010101011010101101101101101011011' +
    '010100101101011001'),
    TBarcodeFormat.CODE_11, '123-4552', 'Code 11 123-455 3');
  CheckRead(ModuleWidths(
    '101100101101011010010110110010101011010101101101101101011011' +
    '010100101101011001'),
    TBarcodeFormat.CODE_11, '123-4552', 'Code 11 123-4552 4');
  CheckRead(ModuleWidths(
    '101100101101011010010110110010101011010101101101101101011011' +
    '0101011001'),
    TBarcodeFormat.CODE_11, '123-455', 'Code 11 123-45 5');
  CheckRead(ModuleWidths(
    '101100101101011010010110110010101011010101101101101101010110' +
    '01'),
    TBarcodeFormat.CODE_11, '', 'Code 11 123-45 6');
  CheckRead(ModuleWidths(
    '111101010111010001010100011101000111010111011101010111011101' +
    '1100010101000101110111010111011110101'),
    TBarcodeFormat.MATRIX_2_OF_5,
    '87654321',
    'C25STANDARD 87654321');
  CheckRead(ModuleWidths(
    '111101010111010001010100011101000111010111011101010111011101' +
    '11000101010001011101110101110100010111011110101'),
    TBarcodeFormat.MATRIX_2_OF_5,
    '87654321' + GS1Check('87654321'),
    'C25STANDARD 87654321 check digit');
  CheckRead(ModuleWidths(
    '111101010111010111010001011101110001010101110111011101110101' +
    '000111010101000111011101000101000100010101110001011110101'),
    TBarcodeFormat.MATRIX_2_OF_5,
    '1234567890',
    'C25STANDARD 1234567890');
  CheckRead(ModuleWidths(
    '101011101010111010101010111011101011101110101011101011101010' +
    '101011101011101110111010101010111010101110111010101011101110' +
    '1'),
    TBarcodeFormat.IATA_2_OF_5,
    '87654321',
    'C25IATA 87654321');
  CheckRead(ModuleWidths(
    '101011101010111010101010111011101011101110101011101011101010' +
    '101011101011101110111010101010111010101110111010101011101011' +
    '101010111011101'),
    TBarcodeFormat.IATA_2_OF_5,
    '87654321' + GS1Check('87654321'),
    'C25IATA 87654321 check digit');
  CheckRead(ModuleWidths(
    '101011101000101010001110100011101011101110101011101110111000' +
    '10101000101110111010111011101'),
    TBarcodeFormat.DATALOGIC_2_OF_5,
    '87654321',
    'C25LOGIC 87654321');
  CheckRead(ModuleWidths(
    '101011101000101010001110100011101011101110101011101110111000' +
    '101010001011101110101110100010111011101'),
    TBarcodeFormat.DATALOGIC_2_OF_5,
    '87654321' + GS1Check('87654321'),
    'C25LOGIC 87654321 check digit');
  CheckRead(ModuleWidths(
    '111011101011101010111010101010111011101011101110101011101011' +
    '101010101011101011101110111010101010111010101110111010101011' +
    '10111010111'),
    TBarcodeFormat.INDUSTRIAL_2_OF_5,
    '87654321',
    'C25IND 87654321');
  CheckRead(ModuleWidths(
    '111011101011101010111010101010111011101011101110101011101011' +
    '101010101011101011101110111010101010111010101110111010101011' +
    '1010111010101110111010111'),
    TBarcodeFormat.INDUSTRIAL_2_OF_5,
    '87654321' + GS1Check('87654321'),
    'C25IND 87654321 check digit');
  CheckRead(ModuleWidths(
    '111011101011101010101110101110101011101110111010101010101110' +
    '101110111010111010101011101110101010101011101110111010101110' +
    '101011101011101010101110111010111010111'),
    TBarcodeFormat.INDUSTRIAL_2_OF_5,
    '1234567890',
    'C25IND 1234567890');

  // Pharmacode two-track (the top track first)
  for var t := 0 to High(TWO_TRACK_VECTORS) do
  begin
    var r := ReadTwoTrack([TWO_TRACK_VECTORS[t, 0], TWO_TRACK_VECTORS[t, 1]],
      [TBarcodeFormat.PHARMA_CODE_TWO_TRACK]);
    try
      // (64570080: full bars only, they can be 12 as well)
      if (TWO_TRACK_VECTORS[t, 2] = '64570080') then
        Assert.IsNull(r, '64570080')
      else
      begin
        Assert.IsNotNull(r, ' Nil result ' + TWO_TRACK_VECTORS[t, 2]);
        Assert.AreEqual(TWO_TRACK_VECTORS[t, 2], r.Text);
      end;
    finally
      r.Free;
    end;
  end;
end;

procedure TOneDTest.KoreaPost;
begin
  // the check digit is checked, not in the text; also upside down
  for var code in ['123456', '000000', '111111', '999999', '505050'] do
    for var upsideDown in [false, true] do
    begin
      var widths := KoreaPostWidths(code);
      if upsideDown then
        widths := Reversed(widths);
      CheckRead(widths, TBarcodeFormat.KOREA_POST, code, 'Korea Post ' + code);
    end;
  CheckRead(KoreaPostWidths('123456', 1), TBarcodeFormat.KOREA_POST, '',
    'Korea Post with a wrong check digit');
  // the encode tests of zint (verified against TEC-IT)
  CheckRead(ModuleWidths(
    '100010001000000000001000100000000000100010001000000010000000' +
    '100010001000100010001000000000001000000000010001000100010001' +
    '00010001000000000001000000010001000000010001000'),
    TBarcodeFormat.KOREA_POST, '010230',
    'Korea Post 010230 (zint)');
  CheckRead(ModuleWidths(
    '000010001000100000001000100000001000000010001000000010001000' +
    '000010001000100000000000100010001000000010000000100010001000' +
    '100010000000100000001000100010001000000000001000'),
    TBarcodeFormat.KOREA_POST, '923457',
    'Korea Post 923457 (zint)');
end;

procedure TOneDTest.FIM;
const
  // the modules of FIM A to E (as zint; C and E its encode tests)
  MODULES: array [0 .. 4] of string = ('10100000100000101',
    '10001010001010001', '10100010001000101', '10101000100010101',
    '10001000000010001');
begin
  // bars of 40 pixels, 3 per module: horizontal and vertical
  for var i := 0 to 4 do
    for var vertical in [false, true] do
    begin
      var r := ReadTwoTrack([MODULES[i], MODULES[i]], [TBarcodeFormat.FIM],
        false, vertical, 20);
      try
        Assert.IsNotNull(r, ' Nil result FIM ' + Chr(Ord('A') + i));
        Assert.AreEqual(Ord(TBarcodeFormat.FIM), Ord(r.BarcodeFormat));
        Assert.AreEqual(string(Chr(Ord('A') + i)), r.Text);
      finally
        r.Free;
      end;
    end;
  // bars not 10 times as high as wide (8 pixels): not
  var r := ReadTwoTrack([MODULES[2], MODULES[2]], [TBarcodeFormat.FIM],
    false, false, 4);
  try
    Assert.IsNull(r, 'FIM of low bars');
  finally
    r.Free;
  end;
end;

procedure TOneDTest.DeutschePost;
begin
  // Leitcode and Identcode, also the examples of the DIALOGPOST SCHWER
  // brochure and of de.wikipedia.org
  for var code in ['0000087654321', '2045703000360', '5082300702800'] do
    CheckRead(ITFWidths(code + DPCheck(code)), TBarcodeFormat.DP_LEITCODE,
      code + DPCheck(code), 'Leitcode ' + code);
  for var code in ['00087654321', '80420000001', '39601313414'] do
    CheckRead(ITFWidths(code + DPCheck(code)), TBarcodeFormat.DP_IDENTCODE,
      code + DPCheck(code), 'Identcode ' + code);
  // the encode tests of zint (verified against TEC-IT)
  CheckRead(ModuleWidths(
    '101010101110001110001010101110001110001010001011101110001010' +
    '100010001110111011101011100010100011101110001010100011101000' +
    '100010111011101'),
    TBarcodeFormat.DP_LEITCODE, '0000087654321' + DPCheck('0000087654321'),
    'Leitcode 0000087654321 (zint)');
  CheckRead(ModuleWidths(
    '101010111010001000111010001011100010111010101000111000111011' +
    '101110100010001010101110001110001011101110001000101010001011' +
    '100011101011101'),
    TBarcodeFormat.DP_LEITCODE, '2045703000360' + DPCheck('2045703000360'),
    'Leitcode 2045703000360 (zint)');
  CheckRead(ModuleWidths(
    '101011101011100010001011101000101110100011101110100010001010' +
    '101110111000100010100011101110100011101010001110001010001011' +
    '100011101011101'),
    TBarcodeFormat.DP_LEITCODE, '5082300702800' + DPCheck('5082300702800'),
    'Leitcode 5082300702800 (zint)');
  CheckRead(ModuleWidths(
    '101010101110001110001010001011101110001010100010001110111011' +
    '101011100010100011101110001010100011101000100010111011101'),
    TBarcodeFormat.DP_IDENTCODE, '00087654321' + DPCheck('00087654321'),
    'Identcode 00087654321 (zint)');
  CheckRead(ModuleWidths(
    '101011101010001110001010100011101011100010101110001110001010' +
    '101110001110001010101110001110001011101010001000111011101'),
    TBarcodeFormat.DP_IDENTCODE, '80420000001' + DPCheck('80420000001'),
    'Identcode 80420000001 (zint)');
  CheckRead(ModuleWidths(
    '101011101110001010001010111011100010001011100010001010111011' +
    '100010001010111010001011101011100010101110001000111011101'),
    TBarcodeFormat.DP_IDENTCODE, '39601313414' + DPCheck('39601313414'),
    'Identcode 39601313414 (zint)');
  // a wrong check digit, another length: not; Auto: ITF
  CheckRead(ITFWidths('20457030003601'), TBarcodeFormat.DP_LEITCODE, '',
    'Leitcode with a wrong check digit');
  CheckRead(ITFWidths('20457030003606'), TBarcodeFormat.DP_IDENTCODE, '',
    'Leitcode as Identcode');
  var r := ReadBars(ITFWidths('20457030003606'), []);
  try
    Assert.IsNotNull(r, ' Nil result (Auto) ');
    Assert.AreEqual(Ord(TBarcodeFormat.ITF), Ord(r.BarcodeFormat));
  finally
    r.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TOneDTest);

end.
