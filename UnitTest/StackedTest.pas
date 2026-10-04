unit StackedTest;

{
  * Tests for the stacked barcodes: Codablock F drawn from the encode tests
  * of zint, also upside down and vertical.
}

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TStackedTest = class(TObject)
  public
    [Test]
    procedure CodablockFZintVectors;
    [Test]
    procedure CodablockFTurned;
    [Test]
    procedure CodablockFIncomplete;
    [Test]
    procedure CodablockFWikipedia;
    [Test]
    procedure Code16KZintVectors;
    [Test]
    procedure Code16KModes;
    [Test]
    procedure Code16KWikipedia;
  end;

implementation

uses
  System.SysUtils,
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
  ZXing.ScanManager;

const
  // the encode tests of zint (backend/tests/test_codablock.c): the text,
  // the rows (';' between them)
  CODABLOCK_VECTORS: array [0 .. 10, 0 .. 1] of string = (
    ('AAAAAAA',
    '110100001001011110111010010110000101000110001010001100010100' +
    '01100010100011000110110011001100011101011;110100001001011110' +
    '111011000100100101000110001010001100010100011000101110111101' +
    '00100111101100011101011;110100001001011110111010110011100101' +
    '110111101011110111011000010100110111011101111001010011000111' +
    '01011'),
    ('AAAAAAAAAA',
    '110100001001011110111010010110000101000110001010001100010100' +
    '01100010100011000110110011001100011101011;110100001001011110' +
    '111011000100100101000110001010001100010100011000101000110001' +
    '11101000101100011101011;110100001001011110111010110011100101' +
    '000110001010001100011101100100100101100001110001011011000111' +
    '01011'),
    ('AAAAAAAAAAA',
    '110100001001011110111010010000110101000110001010001100010100' +
    '01100010100011000110011001101100011101011;110100001001011110' +
    '111011000100100101000110001010001100010100011000101000110001' +
    '11101000101100011101011;110100001001011110111010110011100101' +
    '000110001010001100010100011000101110111101001111010011000111' +
    '01011;110100001001011110111010011011100101110111101011110111' +
    '01110101100011101100100110010111001100011101011'),
    ('AAAAAAAAAAAAAA',
    '110100001001011110111010010000110101000110001010001100010100' +
    '01100010100011000110011001101100011101011;110100001001011110' +
    '111011000100100101000110001010001100010100011000101000110001' +
    '11101000101100011101011;110100001001011110111010110011100101' +
    '000110001010001100010100011000101000110001011110111011000111' +
    '01011;110100001001011110111010011011100101000110001010001100' +
    '01011110100010111011000100110000101100011101011'),
    ('AAAAAAAAAAAAAAA',
    '110100001001011110111010000101100101000110001010001100010100' +
    '01100010100011000100100011001100011101011;110100001001011110' +
    '111011000100100101000110001010001100010100011000101000110001' +
    '11101000101100011101011;110100001001011110111010110011100101' +
    '000110001010001100010100011000101000110001011110111011000111' +
    '01011;110100001001011110111010011011100101000110001010001100' +
    '01010001100010111011110111101001001100011101011;110100001001' +
    '011110111010011001110101110111101011110111010001100010101111' +
    '01000110001010001100011101011'),
    ('AAAAAAAAAAAAAAA',
    '110100001001011110111010100001100101000110001010001100010100' +
    '011000101000110001010001100010100011000101000110001010001100' +
    '010100011000110001000101100011101011;11010000100101111011101' +
    '100010010010100011000101000110001010001100010100011000101000' +
    '110001010001100010111011110111011110101101100011011100010110' +
    '1100011101011'),
    ('AAAAAAAAAAAAAAAA',
    '110100001001011110111010000101100101000110001010001100010100' +
    '01100010100011000100100011001100011101011;110100001001011110' +
    '111011000100100101000110001010001100010100011000101000110001' +
    '11101000101100011101011;110100001001011110111010110011100101' +
    '000110001010001100010100011000101000110001011110111011000111' +
    '01011;110100001001011110111010011011100101000110001010001100' +
    '01010001100010100011000111101011101100011101011;110100001001' +
    '011110111010011001110101110111101011110111010111000110100011' +
    '00010100011101101100011101011'),
    ('AAAAAAAAAAAAAAAAAAAAAAAAA',
    '110100001001011110111010000100110101000110001010001100010100' +
    '0110001010001100010100011000110110001101100011101011;1101000' +
    '010010111101110110001001001010001100010100011000101000110001' +
    '010001100010100011000110010011101100011101011;11010000100101' +
    '111011101011001110010100011000101000110001010001100010100011' +
    '00010100011000110011101001100011101011;110100001001011110111' +
    '010011011100101000110001010001100010100011000101000110001010' +
    '0011000111010011001100011101011;1101000010010111101110100110' +
    '011101010001100010100011000101000110001010001100010100011000' +
    '111001001101100011101011;11010000100101111011101011100110010' +
    '111011110101111011101011101111011101000110101000011001100010' +
    '10001100011101011'),
    ('CODABLOCK F 34567890123456789010040digit',
    '110100001001011110111010010000110100010001101000111011010110' +
    '001000101000110001000101100010001101110100011101101000100011' +
    '0110110011001100011101011;1101000010010111101110110001001001' +
    '011000111011011001100100011000101101100110010111011110100010' +
    '110001110001011011000010100101100111001100011101011;11010000' +
    '100101110111101000110111011011110110101100111001000101100011' +
    '100010110110000101001101111011011001000100100100011001000110' +
    '00101100011101011;110100001001011110111010011011100100111011' +
    '001000010011010000110100100110100001000011010010011110100110' +
    '1110111010111000110110010000101100011101011'),
    ('CODABLOCK F Symbology',
    '110100001001011110111010010110000100010001101000111011010110' +
    '001000101000110001000101100010001101110100011101101000100011' +
    '0111010111101100011101011;1101000010010111101110110001001001' +
    '011000111011011001100100011000101101100110011011101000110110' +
    '111101111011101010010000110100100111101100011101011;11010000' +
    '100101111011101011001110010001111010110010100001000111101010' +
    '011010000110110111101011101111010000110010110111011101010011' +
    '11001100011101011'),
    (' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    '[\]^_`abcdefghijklmnopqrstuvwxyz{|}~',
    '110100001001011110111010000110100110110011001100110110011001' +
    '100110100100110001001000110010001001100100110010001001100010' +
    '010001100100101100011101100011101011;11010000100101111011101' +
    '100010010011001001000110010001001100010010010110011100100110' +
    '111001001100111010111001100100111011001001110011010000110010' +
    '1100011101011;1101000010010111011110100011011101110110111010' +
    '111011000100001011001101101111010111101110111001001101110110' +
    '01001110011010011100110010100001001101100011101011;110100001' +
    '001011110111010011011100110110110001101100011011000110110101' +
    '000110001000101100010001000110101100010001000110100010001100' +
    '010111100010101100011101011;11010000100101111011101001100111' +
    '011010001000110001010001100010001010110111000101100011101000' +
    '110111010111011000101110001101000111011010011011100110001110' +
    '1011;1101000010010111101110101110011001110111011011010001110' +
    '110001011101101110100011011100010110111011101110101100011101' +
    '00011011100010110100001011001100011101011;110100001001011110' +
    '111011100100110111011010001110110001011100011010111011110101' +
    '100100001011110001010101001100001010000110010010110000100011' +
    '000101100011101011;11010000100101111011101110110010010010000' +
    '110100001011001000010011010110010000101100001001001101000010' +
    '0110000101000011010010000110010101011110001100011101011;1101' +
    '000010010111101110111001101001100001001011001010000111101110' +
    '101100001010010001111010101001111001001011110010010011110101' +
    '11100100101100011101100011101011;110100001001011110111011100' +
    '110010100111101001001111001011110100100111100101001111001001' +
    '011011011110110111101101111011011010101111000111101010001100' +
    '011101011;11010000100101111011101101101100010100011110100010' +
    '111101011101111010111101110101110111101011110111010111011110' +
    '1011100011011101101110101001100001100011101011'));

  // the Code 128 characters (6 widths)
  C128_PATTERNS: array [0 .. 105] of string = (
    '212222', '222122', '222221', '121223', '121322', '131222', '122213',
    '122312', '132212', '221213', '221312', '231212', '112232', '122132',
    '122231', '113222', '123122', '123221', '223211', '221132', '221231',
    '213212', '223112', '312131', '311222', '321122', '321221', '312212',
    '322112', '322211', '212123', '212321', '232121', '111323', '131123',
    '131321', '112313', '132113', '132311', '211313', '231113', '231311',
    '112133', '112331', '132131', '113123', '113321', '133121', '313121',
    '211331', '231131', '213113', '213311', '213131', '311123', '311321',
    '331121', '312113', '312311', '332111', '314111', '221411', '431111',
    '111224', '111422', '121124', '121421', '141122', '141221', '112214',
    '112412', '122114', '122411', '142112', '142211', '241211', '221114',
    '413111', '241112', '134111', '111242', '121142', '121241', '114212',
    '124112', '124211', '411212', '421112', '421211', '212141', '214121',
    '412121', '111143', '111341', '131141', '114113', '114311', '411113',
    '411311', '113141', '114131', '311141', '411131', '211412', '211214',
    '211232');
  // the Code 16K start and stop patterns, and the ones of the rows
  C16K_START_STOP: array [0 .. 7] of string = ('3211', '2221', '2122',
    '1411', '1132', '1231', '1114', '3112');
  C16K_START_OF_ROW: array [0 .. 15] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 0,
    1, 2, 3, 4, 5, 6, 7);
  C16K_STOP_OF_ROW: array [0 .. 15] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 4,
    5, 6, 7, 0, 1, 2, 3);
  // the encode tests of zint (backend/tests/test_code16k.c): the text, the
  // rows (';' between them)
  CODE16K_VECTORS: array [0 .. 4, 0 .. 1] of string = (
    ('ab0123456789',
    '111001010110011011101101001111011011110010011001001100010010' +
    '0010001101;1100110101000100111011110100110010010000100110100' +
    '011010010001110011001'),
    ('www.wikipedia.de',
    '111001010100011001100001101011000011010110000110101101100110' +
    '0010001101;1100110100001101011011110010110011110110101111001' +
    '011010110000110011001;11011001010011011110111101100101111001' +
    '01101101001111011001100010010011;100001010111101100101001101' +
    '1110010111101101100001011010001001110111101'),
    ('12345678901234567890123456789012',
    '111001010110001001101001100011011101001110001110100100111101' +
    '0110001101;1100110100100001001010011000110111010011100011101' +
    '001001111010110011001;11011001001000010010100110001101110100' +
    '11100011101001001111010110010011;100001010010000100101001100' +
    '0110010111101100001011101000111010010111101'),
    ('12345678901234567890123456789012',
    '111001010001001000101001100011011101001110001110100100111101' +
    '0110001101;1100110100100001001010011000110111010011100011101' +
    '001001111010110011001;11011001001000010010100110001101110100' +
    '11100011101001001111010110010011;100001010010000100101001100' +
    '0110010111101100101111011001011110110111101;1011100100101111' +
    '011001011110110010111101101000010001011110011010100011'),
    ('12345678901234567890123456789012',
    '111001010100001000101001100011011101001110001110100100111101' +
    '0110001101;1100110100100001001010011000110111010011100011101' +
    '001001111010110011001;11011001001000010010100110001101110100' +
    '11100011101001001111010110010011;100001010010000100101001100' +
    '0110010111101100101111011001011110110111101;1011100100101111' +
    '011001011110110010111101100101111011001011110110100011;10011' +
    '101001011110110010111101100101111011001011110110010111101101' +
    '10001;101000010010111101100101111011001011110110010111101100' +
    '1011110110101111;1110100100101111011001011110110010111101100' +
    '101111011001011110110001011;11100101001011110110010111101100' +
    '10111101100101111011001011110110100011;110011010010111101100' +
    '1011110110010111101100101111011001011110110110001;1101100100' +
    '101111011001011110110010111101100101111011001011110110101111' +
    ';10000101001011110110010111101100101111011001011110110010111' +
    '10110001011;101110010010111101100101111011001011110110010111' +
    '1011001011110110001101;1001110100101111011001011110110010111' +
    '101100101111011001011110110011001;10100001001011110110010111' +
    '10110010111101100101111011001011110110010011;111010010010111' +
    '1011001011110110010111101100101110001001110010010111101'));

/// <summary>The rows of a Code 16K of text in mode (0 A, 1 B, 2 C: as zint,
/// without changes of code set), at least minRows rows.</summary>
function Code16KRows(const text: string; mode: Integer;
  minRows: Integer = 2; checkOffset: Integer = 0): TArray<string>;
begin
  var values: TArray<Integer> := [0];
  var i := 1;
  while (i <= Length(text)) do
  begin
    var c := Ord(text[i]);
    case mode of
      0:
        if (c < 32) then
          values := values + [c + 64]
        else
          values := values + [c - 32];
      1:
        values := values + [c - 32];
      2:
        begin
          values := values + [10 * (c - Ord('0')) + Ord(text[i + 1]) -
            Ord('0')];
          Inc(i);
        end;
    end;
    Inc(i);
  end;
  // pads (103) to whole rows of 5 with the 2 check characters
  while ((Length(values) + 2) mod 5 <> 0) or (Length(values) + 2 < 5 * minRows)
  do
    values := values + [103];
  var rows := (Length(values) + 2) div 5;
  values[0] := 7 * (rows - 2) + mode;
  var first := 0;
  var second := 0;
  for var k := 0 to High(values) do
  begin
    Inc(first, (k + 2) * values[k]);
    Inc(second, (k + 1) * values[k]);
  end;
  first := first mod 107;
  second := (second + first * (Length(values) + 1) + checkOffset) mod 107;
  values := values + [first, second];
  SetLength(Result, rows);
  for var r := 0 to rows - 1 do
  begin
    var widths := C16K_START_STOP[C16K_START_OF_ROW[r]] + '1';
    for var c := 0 to 4 do
      widths := widths + C128_PATTERNS[values[5 * r + c]];
    widths := widths + C16K_START_STOP[C16K_STOP_OF_ROW[r]];
    var modules := '';
    for var k := 1 to Length(widths) do
      modules := modules + StringOfChar(Chr(Ord('0') + Ord(Odd(k))),
        Ord(widths[k]) - Ord('0'));
    Result[r] := modules;
  end;
end;

/// <summary>Reads the rows of a stacked symbol ('1' a dark module; 2 pixels
/// per module, 10 modules per row, a line of 1 module between the rows and
/// above and below them, as zint draws them) with the formats (none: Auto),
/// turned 180 degrees (upsideDown) or 90 (vertical, with TRY_HARDER); the
/// caller frees the result.</summary>
function ReadStacked(const rows: TArray<string>;
  const formats: array of TBarcodeFormat; upsideDown: Boolean = false;
  vertical: Boolean = false): TReadResult;
const
  MODULE = 2;
  ROW_HEIGHT = 10;
  QUIET = 12;
begin
  var modules := Length(rows[0]);
  var w := (modules + 2 * QUIET) * MODULE;
  var h := (Length(rows) * (ROW_HEIGHT + 1) + 1 + 2 * QUIET) * MODULE;
  var pixels: TArray<Byte>;
  SetLength(pixels, w * h);
  FillChar(pixels[0], w * h, 255);
  var setModule: TProc<Integer, Integer> :=
    procedure(mx, my: Integer)
    begin
      for var y := 0 to MODULE - 1 do
        for var x := 0 to MODULE - 1 do
        begin
          var px := (QUIET + mx) * MODULE + x;
          var py := (QUIET + my) * MODULE + y;
          if upsideDown then
          begin
            px := w - 1 - px;
            py := h - 1 - py;
          end;
          pixels[py * w + px] := 0;
        end;
    end;
  for var r := 0 to High(rows) do
  begin
    var top := r * (ROW_HEIGHT + 1) + 1;
    for var m := 0 to modules - 1 do
      if (rows[r].Chars[m] = '1') then
        for var my := top to top + ROW_HEIGHT - 1 do
          setModule(m, my);
  end;
  // the lines between the rows, above and below them
  for var r := 0 to Length(rows) do
    for var m := 0 to modules - 1 do
      setModule(m, r * (ROW_HEIGHT + 1));
  if vertical then
  begin
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
    // vertical: with TRY_HARDER, like the 1D codes
    if vertical then
      hints.Add(TDecodeHintType.TRY_HARDER, nil);
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

{ TStackedTest }

procedure TStackedTest.CodablockFZintVectors;
begin
  // asked for and in Auto (not as Code 128)
  for var v := 0 to High(CODABLOCK_VECTORS) do
    for var auto in [false, true] do
    begin
      var rows := CODABLOCK_VECTORS[v, 1].Split([';']);
      var r: TReadResult;
      if auto then
        r := ReadStacked(rows, [])
      else
        r := ReadStacked(rows, [TBarcodeFormat.CODABLOCK_F]);
      try
        Assert.IsNotNull(r, ' Nil result ' + CODABLOCK_VECTORS[v, 0]);
        Assert.AreEqual(Ord(TBarcodeFormat.CODABLOCK_F), Ord(r.BarcodeFormat),
          CODABLOCK_VECTORS[v, 0]);
        Assert.AreEqual(CODABLOCK_VECTORS[v, 0], r.Text);
        Assert.AreEqual(']O4', r.SymbologyIdentifier);
      finally
        r.Free;
      end;
    end;
end;

procedure TStackedTest.CodablockFTurned;
begin
  var rows := CODABLOCK_VECTORS[9, 1].Split([';']);
  for var turn := 1 to 3 do
  begin
    var r := ReadStacked(rows, [TBarcodeFormat.CODABLOCK_F], turn <> 2,
      turn >= 2);
    try
      Assert.IsNotNull(r, ' Nil result turn ' + IntToStr(turn));
      Assert.AreEqual(CODABLOCK_VECTORS[9, 0], r.Text);
    finally
      r.Free;
    end;
  end;
end;

procedure TStackedTest.CodablockFIncomplete;
begin
  // without its last row: not (it has K1 and K2)
  var rows := CODABLOCK_VECTORS[9, 1].Split([';']);
  SetLength(rows, Length(rows) - 1);
  var r := ReadStacked(rows, [TBarcodeFormat.CODABLOCK_F]);
  try
    Assert.IsNull(r, 'Codablock F without its last row');
  finally
    r.Free;
  end;
end;

procedure TStackedTest.CodablockFWikipedia;
begin
  // the example of Wikipedia (Wikimedia Commons, public domain), asked for
  // and in Auto
  var bmp := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\images\stacked\codablock-f-wikipedia.png');
  try
    for var format in [TBarcodeFormat.CODABLOCK_F, TBarcodeFormat.Auto] do
    begin
      var scanManager := TScanManager.Create(format, nil);
      try
        var r := scanManager.Scan(bmp);
        try
          Assert.IsNotNull(r, ' Nil result ');
          Assert.AreEqual(Ord(TBarcodeFormat.CODABLOCK_F),
            Ord(r.BarcodeFormat));
          Assert.AreEqual('Codablock-F Example', r.Text);
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

procedure TStackedTest.Code16KZintVectors;
begin
  // asked for and in Auto
  for var v := 0 to High(CODE16K_VECTORS) do
    for var auto in [false, true] do
    begin
      var rows := CODE16K_VECTORS[v, 1].Split([';']);
      var r: TReadResult;
      if auto then
        r := ReadStacked(rows, [])
      else
        r := ReadStacked(rows, [TBarcodeFormat.CODE_16K]);
      try
        Assert.IsNotNull(r, ' Nil result ' + CODE16K_VECTORS[v, 0]);
        Assert.AreEqual(Ord(TBarcodeFormat.CODE_16K), Ord(r.BarcodeFormat),
          CODE16K_VECTORS[v, 0]);
        Assert.AreEqual(CODE16K_VECTORS[v, 0], r.Text);
        Assert.AreEqual(']K0', r.SymbologyIdentifier);
      finally
        r.Free;
      end;
    end;
end;

procedure TStackedTest.Code16KModes;
const
  // mode, turn (0 upside down, 1 no, 2 vertical), rows
  TESTS: array [0 .. 3, 0 .. 2] of Integer = ((0, 0, 2), (1, 1, 2), (2, 2, 2),
    (1, 1, 16));
begin
  // the modes A (control characters), B and C, also upside down and
  // vertical, and 16 rows
  for var t := 0 to High(TESTS) do
  begin
    var mode := TESTS[t, 0];
    var text := 'Code 16K, ZXing.Delphi!';
    if (mode = 0) then
      text := 'TAB'#9'LINE'#10'END'
    else if (mode = 2) then
      text := '0123456789012345678901234567890123456789';
    var rows := Code16KRows(text, mode, TESTS[t, 2]);
    var r := ReadStacked(rows, [TBarcodeFormat.CODE_16K], TESTS[t, 1] = 0,
      TESTS[t, 1] = 2);
    try
      Assert.IsNotNull(r, ' Nil result mode ' + IntToStr(mode));
      Assert.AreEqual(text, r.Text);
    finally
      r.Free;
    end;
  end;
  // a wrong check character: nothing
  var r := ReadStacked(Code16KRows('Code 16K', 1, 2, 1),
    [TBarcodeFormat.CODE_16K]);
  try
    Assert.IsNull(r, 'Code 16K with a wrong check character');
  finally
    r.Free;
  end;
end;

procedure TStackedTest.Code16KWikipedia;
begin
  // the example of Wikipedia (Wikimedia Commons, Barcodat GmbH, copyrighted
  // free use), asked for and in Auto
  var bmp := LoadImage(ExtractFileDir(ParamStr(0)) +
    '\..\..\images\stacked\code16k-wikipedia.png');
  try
    for var format in [TBarcodeFormat.CODE_16K, TBarcodeFormat.Auto] do
    begin
      var scanManager := TScanManager.Create(format, nil);
      try
        var r := scanManager.Scan(bmp);
        try
          Assert.IsNotNull(r, ' Nil result ');
          Assert.AreEqual(Ord(TBarcodeFormat.CODE_16K), Ord(r.BarcodeFormat));
          Assert.AreEqual('www.wikipedia.de', r.Text);
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

initialization

TDUnitX.RegisterTestFixture(TStackedTest);

end.
