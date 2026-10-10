unit PostalTest;

{
  * Tests for the postal barcodes: KIX, RM4SCC, POSTNET, PLANET and Japan Post
  * drawn (as zint), also upside down and vertical, and the images of
  * ZXing.Net (Intelligent Mail Barcode, POSTNET).
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
    procedure JapanPost;
    [Test]
    procedure AustraliaPost;
    [Test]
    procedure Mailmark;
    [Test]
    procedure ZintVectors;
    [Test]
    procedure IMbSamples;
    [Test]
    procedure PostalSamples;
    [Test]
    procedure CEPNet;
    [Test]
    procedure NotInAuto;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.Math,
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
  ZXing.MultiFormatReader,
  ZXing.Postal.FourStateDetector,
  ZXing.Postal.Mailmark,
  ZXing.Postal.AustraliaPost,
  ZXing.Postal.PostalReader;

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

/// <summary>The bars of a Japan Post of text (as zint), with the check
/// character (+ checkOffset).</summary>
function JapanBars(const text: string; checkOffset: Integer = 0)
  : TArray<Byte>;
const
  CHARS = '1234567890-abcdefgh';
  CHECK_CHARS = '0123456789-abcdefgh';
  BARS: array [0 .. 18, 0 .. 2] of Byte = ((0, 0, 3), (0, 2, 1), (2, 0, 1),
    (0, 1, 2), (0, 3, 0), (2, 1, 0), (1, 0, 2), (1, 2, 0), (3, 0, 0),
    (0, 3, 3), (3, 0, 3), (2, 1, 3), (2, 3, 1), (1, 2, 3), (3, 2, 1),
    (1, 3, 2), (3, 1, 2), (3, 3, 0), (0, 0, 0));
begin
  // letters: a, b or c and a digit; padding d up to 20
  var symbols := '';
  for var c in text do
    if CharInSet(c, ['0' .. '9', '-']) then
      symbols := symbols + c
    else if (c <= 'J') then
      symbols := symbols + 'a' + Chr(Ord(c) - Ord('A') + Ord('0'))
    else if (c <= 'T') then
      symbols := symbols + 'b' + Chr(Ord(c) - Ord('K') + Ord('0'))
    else
      symbols := symbols + 'c' + Chr(Ord(c) - Ord('U') + Ord('0'));
  while (Length(symbols) < 20) do
    symbols := symbols + 'd';
  var sum := 0;
  for var c in symbols do
    Inc(sum, CHECK_CHARS.IndexOf(c));
  symbols := symbols + CHECK_CHARS.Chars[((19 - sum mod 19) mod 19 +
    checkOffset) mod 19];
  Result := [0, 2];
  for var c in symbols do
  begin
    var k := CHARS.IndexOf(c);
    Result := Result + [BARS[k, 0], BARS[k, 1], BARS[k, 2]];
  end;
  Result := Result + [2, 0];
end;

/// <summary>The bars of an Australia Post barcode of the format control code
/// and the data (DPID and customer information, as zint: digits with the N
/// table, else characters with the C table).</summary>
function AustraliaBars(const fcc, data: string; n: Integer): TArray<Byte>;
const
  N_TABLE: array [0 .. 9, 0 .. 1] of Byte = ((0, 0), (0, 1), (0, 2), (1, 0),
    (1, 1), (1, 2), (2, 0), (2, 1), (2, 2), (3, 0));
  GDSET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz #';
  C_TABLE: array [0 .. 63, 0 .. 2] of Byte = ((2, 2, 2), (3, 0, 0), (3, 0, 1),
    (3, 0, 2), (3, 1, 0), (3, 1, 1), (3, 1, 2), (3, 2, 0), (3, 2, 1),
    (3, 2, 2), (0, 0, 0), (0, 0, 1), (0, 0, 2), (0, 1, 0), (0, 1, 1),
    (0, 1, 2), (0, 2, 0), (0, 2, 1), (0, 2, 2), (1, 0, 0), (1, 0, 1),
    (1, 0, 2), (1, 1, 0), (1, 1, 1), (1, 1, 2), (1, 2, 0), (1, 2, 1),
    (1, 2, 2), (2, 0, 0), (2, 0, 1), (2, 0, 2), (2, 1, 0), (2, 1, 1),
    (2, 1, 2), (2, 2, 0), (2, 2, 1), (0, 2, 3), (0, 3, 0), (0, 3, 1),
    (0, 3, 2), (0, 3, 3), (1, 0, 3), (1, 1, 3), (1, 2, 3), (1, 3, 0),
    (1, 3, 1), (1, 3, 2), (1, 3, 3), (2, 0, 3), (2, 1, 3), (2, 2, 3),
    (2, 3, 0), (2, 3, 1), (2, 3, 2), (2, 3, 3), (3, 0, 3), (3, 1, 3),
    (3, 2, 3), (3, 3, 0), (3, 3, 1), (3, 3, 2), (3, 3, 3), (0, 0, 3),
    (0, 1, 3));
begin
  var bars: TArray<Byte> := [1, 3];
  for var c in fcc + Copy(data, 1, 8) do
    bars := bars + [N_TABLE[Ord(c) - Ord('0'), 0],
      N_TABLE[Ord(c) - Ord('0'), 1]];
  var info := Copy(data, 9, MaxInt);
  var digits := true;
  for var c in info do
    digits := digits and CharInSet(c, ['0' .. '9']);
  for var c in info do
    if digits then
      bars := bars + [N_TABLE[Ord(c) - Ord('0'), 0],
        N_TABLE[Ord(c) - Ord('0'), 1]]
    else
    begin
      var k := GDSET.IndexOf(c);
      bars := bars + [C_TABLE[k, 0], C_TABLE[k, 1], C_TABLE[k, 2]];
    end;
  while (Length(bars) < n - 14) do
    bars := bars + [3];

  // Reed-Solomon in GF(64) (x^6 + x + 1), the generator (x - a)..(x - a^4)
  var exp: array [0 .. 127] of Integer;
  var log: array [0 .. 63] of Integer;
  var v := 1;
  for var i := 0 to 62 do
  begin
    exp[i] := v;
    exp[i + 63] := v;
    log[v] := i;
    v := v shl 1;
    if (v and 64 <> 0) then
      v := v xor $43;
  end;
  var generator: TArray<Integer> := [1];
  for var i := 1 to 4 do
  begin
    // times (x + a^i)
    var next: TArray<Integer>;
    SetLength(next, Length(generator) + 1);
    for var k := 0 to High(generator) do
    begin
      next[k] := next[k] xor generator[k];
      if (generator[k] <> 0) then
        next[k + 1] := next[k + 1] xor exp[log[generator[k]] + i];
    end;
    generator := next;
  end;
  // the remainder of data * x^4 (the highest coefficient first)
  var count := (Length(bars) - 2) div 3;
  var remainder: TArray<Integer>;
  SetLength(remainder, count + 4);
  for var i := 0 to count - 1 do
    remainder[i] := 16 * bars[2 + 3 * i] + 4 * bars[3 + 3 * i] +
      bars[4 + 3 * i];
  for var i := 0 to count - 1 do
  begin
    var c := remainder[i];
    if (c <> 0) then
      for var k := 1 to 4 do
        if (generator[k] <> 0) then
          remainder[i + k] := remainder[i + k] xor
            exp[log[c] + log[generator[k]]];
  end;
  // the error correction symbols, the highest first
  for var k := count to count + 3 do
    bars := bars + [remainder[k] shr 4, (remainder[k] shr 2) and 3,
      remainder[k] and 3];
  Result := bars + [1, 3];
end;

/// <summary>The bars of a Mailmark 4-state barcode of text (22 characters
/// barcode C, 26 barcode L; as zint).</summary>
function MailmarkBars(const text: string): TArray<Byte>;
const
  SET_A = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  SET_L = 'ABDEFGHJLNPQRSTUWXYZ';
  FORMATS: array [1 .. 6] of string = ('ANANLLNLS', 'AANNLLNLS', 'AANNNLLNL',
    'AANANLLNL', 'ANNLLNLSS', 'ANNNLLNLS');
  STARTS: array [1 .. 6] of UInt64 = (1, 5408000001, 10816000001,
    64896000001, 205504000001, 205712000001);
  SYMBOLS_ODD: array [0 .. 31] of Byte = ($01, $02, $04, $07, $08, $0B,
    $0D, $0E, $10, $13, $15, $16, $19, $1A, $1C, $1F, $20, $23, $25, $26,
    $29, $2A, $2C, $2F, $31, $32, $34, $37, $38, $3B, $3D, $3E);
  SYMBOLS_EVEN: array [0 .. 29] of Byte = ($03, $05, $06, $09, $0A, $0C,
    $0F, $11, $12, $14, $17, $18, $1B, $1D, $1E, $21, $22, $24, $27, $28,
    $2B, $2D, $2E, $30, $33, $35, $36, $39, $3A, $3C);
  GROUPS_C: array [0 .. 21] of Byte = (3, 5, 7, 11, 13, 14, 16, 17, 19, 0, 1,
    2, 4, 6, 8, 9, 10, 12, 15, 18, 20, 21);
  GROUPS_L: array [0 .. 25] of Byte = (2, 5, 7, 8, 13, 14, 15, 16, 21, 22, 23,
    0, 1, 3, 4, 6, 9, 10, 11, 12, 17, 18, 19, 20, 24, 25);
begin
  var n := Length(text);
  var barcodeC := (n = 22);
  var postcode := Copy(text, n - 8, 9);
  // the value of the postcode
  var value: UInt64 := 0;
  if (postcode <> 'XY11     ') then
    for var t := 1 to 6 do
    begin
      var fits := true;
      var v: UInt64 := 0;
      for var i := 0 to 8 do
        case FORMATS[t].Chars[i] of
          'A':
            begin
              fits := fits and (SET_A.IndexOf(postcode.Chars[i]) >= 0);
              v := v * 26 + UInt64(Max(SET_A.IndexOf(postcode.Chars[i]), 0));
            end;
          'L':
            begin
              fits := fits and (SET_L.IndexOf(postcode.Chars[i]) >= 0);
              v := v * 20 + UInt64(Max(SET_L.IndexOf(postcode.Chars[i]), 0));
            end;
          'N':
            begin
              fits := fits and CharInSet(postcode.Chars[i], ['0' .. '9']);
              v := v * 10 + UInt64(Max(Ord(postcode.Chars[i]) - Ord('0'), 0));
            end;
        else
          fits := fits and (postcode.Chars[i] = ' ');
        end;
      if fits then
      begin
        value := v + STARTS[t];
        break;
      end;
    end;

  // the consolidated data value (128 bits)
  var cdv: TPostalNumber;
  FillChar(cdv, SizeOf(cdv), 0);
  cdv.W[1] := Cardinal(value shr 32);
  cdv.W[0] := Cardinal(value);
  cdv.MulAdd(100000000, StrToInt(Copy(text, n - 16, 8)));
  if barcodeC then
    cdv.MulAdd(100, StrToInt(Copy(text, 4, 2)))
  else
    cdv.MulAdd(1000000, StrToInt(Copy(text, 4, 6)));
  cdv.MulAdd(15, '0123456789ABCDE'.IndexOf(text[3]));
  cdv.MulAdd(5, Ord(text[1]) - Ord('0'));
  cdv.MulAdd(4, Ord(text[2]) - Ord('1'));

  var step := 10;
  var count := 19;
  var checkCount := 7;
  if barcodeC then
  begin
    step := 8;
    count := 16;
    checkCount := 6;
  end;
  var numbers: TArray<Integer>;
  SetLength(numbers, count + checkCount);
  for var j := count - 1 downto step + 1 do
    numbers[j] := cdv.DivMod(32);
  for var j := step downto 0 do
    numbers[j] := cdv.DivMod(30);

  // Reed-Solomon in GF(32) (x^5 + x^2 + 1), generator (x - a)..(x - a^k)
  var exp: array [0 .. 61] of Integer;
  var log: array [0 .. 31] of Integer;
  var e := 1;
  for var i := 0 to 30 do
  begin
    exp[i] := e;
    exp[i + 31] := e;
    log[e] := i;
    e := e shl 1;
    if (e and 32 <> 0) then
      e := e xor $25;
  end;
  var generator: TArray<Integer> := [1];
  for var i := 1 to checkCount do
  begin
    var next: TArray<Integer>;
    SetLength(next, Length(generator) + 1);
    for var k := 0 to High(generator) do
    begin
      next[k] := next[k] xor generator[k];
      if (generator[k] <> 0) then
        next[k + 1] := next[k + 1] xor exp[log[generator[k]] + i];
    end;
    generator := next;
  end;
  var remainder := Copy(numbers);
  for var i := 0 to count - 1 do
    if (remainder[i] <> 0) then
      for var k := 1 to checkCount do
        if (generator[k] <> 0) then
          remainder[i + k] := remainder[i + k] xor
            exp[log[remainder[i]] + log[generator[k]]];
  // the check numbers, the highest first
  for var k := 0 to checkCount - 1 do
    numbers[count + k] := remainder[count + k];

  // the symbols in their extender groups, then the bars
  var extender: TArray<Integer>;
  SetLength(extender, n);
  for var i := 0 to n - 1 do
  begin
    var symbol: Integer;
    if (i <= step) then
      symbol := SYMBOLS_EVEN[numbers[i]]
    else
      symbol := SYMBOLS_ODD[numbers[i]];
    if barcodeC then
      extender[GROUPS_C[i]] := symbol
    else
      extender[GROUPS_L[i]] := symbol;
  end;
  Result := [];
  for var i := 0 to n - 1 do
    for var j := 0 to 2 do
    begin
      var bits := (extender[i] shl j) and $24;
      var state: Byte := 3;
      // which side is the ascender alternates
      if (bits = $24) then
        state := 0
      else if (bits = $20) and Odd(i) or (bits = $04) and not Odd(i) then
        state := 2
      else if (bits <> 0) then
        state := 1;
      Result := Result + [state];
    end;
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
  // half of the symbol in the image (the bars up to its edge, once read as
  // 1231FZ): no quiet zone, not read (it has no start or stop)
  var bmp := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\images\postal\KIX half 1231FZ13XHS.png');
  try
    for var format in [TBarcodeFormat.KIX, TBarcodeFormat.All] do
    begin
      var scanManager := TScanManager.Create(format, nil);
      try
        r := scanManager.Scan(bmp);
        try
          if (r <> nil) then
            Assert.Fail('half read: ' + r.Text);
        finally
          r.Free;
        end;
      finally
        scanManager.Free;
      end;
    end;
  finally
    bmp.Free;
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

procedure TPostalTest.JapanPost;
begin
  // a postal code and an address with letters
  for var text in ['15400233-16-4-205', '0600804-1-1-ABC'] do
    for var upsideDown in [false, true] do
    begin
      var r := ReadBars(JapanBars(text), TBarcodeFormat.JAPAN_POST, upsideDown,
        upsideDown);
      try
        Assert.IsNotNull(r, ' Nil result ' + text);
        Assert.AreEqual(Ord(TBarcodeFormat.JAPAN_POST), Ord(r.BarcodeFormat));
        Assert.AreEqual(text, r.Text);
      finally
        r.Free;
      end;
    end;

  var r := ReadBars(JapanBars('15400233-16-4-205', 1),
    TBarcodeFormat.JAPAN_POST);
  try
    Assert.IsNull(r, 'Japan Post with a wrong check character');
  finally
    r.Free;
  end;
end;

procedure TPostalTest.AustraliaPost;
const
  // Standard Customer Barcode, Customer Barcode 2 (digits) and 3
  // (characters): FCC, DPID, customer information, bars
  TESTS: array [0 .. 2, 0 .. 3] of string = (('11', '39987520', '', '37'),
    ('59', '32211324', '12345678', '52'),
    ('62', '39987520', 'AB12 #cd', '67'));
begin
  for var t := 0 to 2 do
  begin
    var test := TESTS[t];
    var bars := AustraliaBars(test[0], test[1] + test[2], StrToInt(test[3]));
    Assert.AreEqual(StrToInt(test[3]), Length(bars), 'bars');
    for var upsideDown in [false, true] do
    begin
      var r := ReadBars(bars, TBarcodeFormat.AUSTRALIA_POST, upsideDown,
        upsideDown);
      try
        Assert.IsNotNull(r, ' Nil result ' + test[0]);
        Assert.AreEqual(Ord(TBarcodeFormat.AUSTRALIA_POST),
          Ord(r.BarcodeFormat));
        Assert.AreEqual(test[0] + test[1] + test[2], r.Text);
      finally
        r.Free;
      end;
    end;
  end;

  // 2 symbols wrong are corrected
  var bars := AustraliaBars('11', '39987520', 37);
  bars[5] := (bars[5] + 1) mod 4;
  bars[20] := (bars[20] + 2) mod 4;
  var r := ReadBars(bars, TBarcodeFormat.AUSTRALIA_POST);
  try
    Assert.IsNotNull(r, ' Nil result (corrected) ');
    Assert.AreEqual('1139987520', r.Text);
  finally
    r.Free;
  end;
end;

procedure TPostalTest.Mailmark;
begin
  // barcode C and L, postcodes of type 2 and 1 and international
  for var text in ['1100123456789BS12AB3D ', '21B12345698765432B1A2DE3F ',
    '41E9900000001XY11     '] do
    for var upsideDown in [false, true] do
    begin
      var r := ReadBars(MailmarkBars(text), TBarcodeFormat.MAILMARK_4STATE,
        upsideDown, upsideDown);
      try
        Assert.IsNotNull(r, ' Nil result ' + text);
        Assert.AreEqual(Ord(TBarcodeFormat.MAILMARK_4STATE),
          Ord(r.BarcodeFormat));
        Assert.AreEqual(text, r.Text);
      finally
        r.Free;
      end;
    end;

  // 3 bars wrong are corrected
  var bars := MailmarkBars('1100123456789BS12AB3D ');
  bars[4] := (bars[4] + 1) mod 4;
  bars[30] := (bars[30] + 2) mod 4;
  bars[60] := (bars[60] + 3) mod 4;
  var r := ReadBars(bars, TBarcodeFormat.MAILMARK_4STATE);
  try
    Assert.IsNotNull(r, ' Nil result (corrected) ');
    Assert.AreEqual('1100123456789BS12AB3D ', r.Text);
  finally
    r.Free;
  end;
end;

/// <summary>The states of bars written as F, A, D and T.</summary>
function States(const bars: string): TArray<Byte>;
begin
  SetLength(Result, Length(bars));
  for var i := 1 to Length(bars) do
    Result[i - 1] := Pos(bars[i], 'FADT') - 1;
end;

procedure TPostalTest.ZintVectors;
begin
  // test vectors of zint (backend/tests): Mailmark barcode C and L
  Assert.AreEqual('1100000000000XY11     ', DecodeMailmark(States(
    'TTDTTATTDTAATTDTAATTDTAATTDTTDDAATAADDATAATDDFAFTDDTAADDDTAAFDFAFF')));
  Assert.AreEqual('21B2254800659JW5O9QA6Y', DecodeMailmark(States(
    'DAATATTTADTAATTFADDDDTTFTFDDDDFFDFDAFTADDTFFTDDATADTTFATTDAFDTFDDA')));
  Assert.AreEqual('41038422416563762EF61AH8T ', DecodeMailmark(States(
    'DTTFATTDDTATTTATFTDFFFTFDFDAFTTTADTTFDTFDDDTDFDDFTFAADTFDTDTDTFAATAFDD' +
    'TAATTDTT')));
  // Australia Post (the first two verified by zint against TEC-IT)
  Assert.AreEqual('1196184209', DecodeAustraliaPost(States(
    'ATFAFATFDFFADDAAFDFFTFTDFFTDADADTADAT')));
  Assert.AreEqual('1139549554', DecodeAustraliaPost(States(
    'ATFAFAAFTFADAATFADADAATTADAFATAATDDAT')));
  Assert.AreEqual('5956439111ABA 9', DecodeAustraliaPost(States(
    'ATADTFADDFAAAFTFFAFAFAFFFFFAFFFFFTTDDTTAFADAFFFTTAAT')));
  Assert.AreEqual('6232211324123456789012345', DecodeAustraliaPost(States(
    'ATDFFDAFFDFDFAFAAFFDAAFAFDAFAAADDFDADDTFFFFAFDAFAAADTADDATFAFFFTFAT')));
end;

procedure TPostalTest.CEPNet;
begin
  // POSTNET of 8 digits; not as POSTNET, and a POSTNET not as CEPNet
  for var upsideDown in [false, true] do
  begin
    var r := ReadBars(PostnetBars('12345678', false), TBarcodeFormat.CEPNET,
      upsideDown, upsideDown);
    try
      Assert.IsNotNull(r, ' Nil result ');
      Assert.AreEqual(Ord(TBarcodeFormat.CEPNET), Ord(r.BarcodeFormat));
      Assert.AreEqual('12345678', r.Text);
    finally
      r.Free;
    end;
  end;
  var r := ReadBars(PostnetBars('12345678', false), TBarcodeFormat.POSTNET);
  try
    Assert.IsNull(r, 'CEPNet read as POSTNET');
  finally
    r.Free;
  end;
  r := ReadBars(PostnetBars('123456789', false), TBarcodeFormat.CEPNET);
  try
    Assert.IsNull(r, 'POSTNET read as CEPNet');
  finally
    r.Free;
  end;
  // with All (POSTNET asked for too): each one as its own format
  r := ReadBars(PostnetBars('12345678', false), TBarcodeFormat.All);
  try
    Assert.IsNotNull(r, ' Nil result (All) ');
    Assert.AreEqual(Ord(TBarcodeFormat.CEPNET), Ord(r.BarcodeFormat));
    Assert.AreEqual('12345678', r.Text);
  finally
    r.Free;
  end;
  r := ReadBars(PostnetBars('123456789', false), TBarcodeFormat.All);
  try
    Assert.IsNotNull(r, ' Nil result (All, POSTNET) ');
    Assert.AreEqual(Ord(TBarcodeFormat.POSTNET), Ord(r.BarcodeFormat));
    Assert.AreEqual('123456789', r.Text);
  finally
    r.Free;
  end;
  // the encode tests of zint (the figures 8 and 10 of the guide of Correios)
  Assert.AreEqual('12345678', DecodePostnet(States(
    'ATTTAATTATATTAATTATTATATATTAATTATTTAATTATTATTAA'), false, true));
  Assert.AreEqual('36400000', DecodePostnet(States(
    'ATTAATTAATTTATTAAATTTAATTTAATTTAATTTAATTTATTTAA'), false, true));
end;

procedure TPostalTest.IMbSamples;
begin
  // the Intelligent Mail Barcode images of ZXing.Net (it reads 1 of them
  // without TRY_HARDER): at least 9, the texts as expected
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
  Assert.IsTrue(read >= 9, IntToStr(read) + ' of 10 read');
end;

procedure TPostalTest.PostalSamples;
const
  // the folders of the formats (images named format_number_text, '-' in the
  // text written as '_'): png the barcode alone, jpg on a photo, turned up
  // to 10 degrees; CEPNet: the figure of the guide of Correios (01) and the
  // example of the barcode guide of Seagull Scientific (02)
  FOLDERS: array [0 .. 8] of string = ('AustraliaPost', 'IMb', 'JapanPost',
    'KIX', 'Mailmark', 'PLANET', 'POSTNET', 'RM4SCC', 'CEPNet');
  FORMATS: array [0 .. 8] of TBarcodeFormat = (TBarcodeFormat.AUSTRALIA_POST,
    TBarcodeFormat.IMB, TBarcodeFormat.JAPAN_POST, TBarcodeFormat.KIX,
    TBarcodeFormat.MAILMARK_4STATE, TBarcodeFormat.PLANET,
    TBarcodeFormat.POSTNET, TBarcodeFormat.RM4SCC, TBarcodeFormat.CEPNET);
begin
  for var f := 0 to High(FOLDERS) do
    for var name in TDirectory.GetFiles(ExtractFileDir(ParamStr(0)) +
      '\..\..\images\postal\' + FOLDERS[f], '*.*') do
    begin
      var base := TPath.GetFileNameWithoutExtension(name);
      // (the text: after the second _)
      base := base.Substring(base.IndexOf('_') + 1);
      var expected := base.Substring(base.IndexOf('_') + 1).Replace('_', '-');
      // (JapanPost_10: bar 65 is drawn as a line of 1 pixel, unreadable)
      if (expected = '9800811') then
        continue;
      // the customer information TEST has the bars of the digits 634621:
      // the bars do not tell which (digits taken, like zint encodes them)
      if (expected = '6212345678TEST') then
        expected := '6212345678634621';
      var bmp := LoadImage(name);
      var scanManager := TScanManager.Create(FORMATS[f], nil);
      try
        var r := scanManager.Scan(bmp);
        try
          Assert.IsNotNull(r, ' Nil result ' + name);
          Assert.AreEqual(expected, r.Text, name);
          Assert.AreEqual(Ord(FORMATS[f]), Ord(r.BarcodeFormat), name);
        finally
          r.Free;
        end;
      finally
        scanManager.Free;
        bmp.Free;
      end;
    end;
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
