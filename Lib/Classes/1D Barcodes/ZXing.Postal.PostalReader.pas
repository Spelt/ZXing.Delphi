{
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

  * Reader of the 4-state postal barcodes for ZXing.Delphi (zxing-cpp and
  * ZXing Java have none). The encodings as in zint (postal.c).
}

unit ZXing.Postal.PostalReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap,
  ZXing.Common.BitMatrix,
  ZXing.Postal.FourStateDetector,
  ZXing.Postal.IMb,
  ZXing.Postal.AustraliaPost,
  ZXing.Postal.Mailmark;

type
  /// <summary>
  /// Reads the postal barcodes of formats (KIX, RM4SCC, IMB, POSTNET,
  /// PLANET, JAPAN_POST, AUSTRALIA_POST, MAILMARK_4STATE): rows of bars that
  /// differ in height, horizontal or vertical, also upside down, slanted and
  /// in perspective.
  /// </summary>
  TPostalReader = class(TInterfacedObject, IReader, IMultipleReader)
  private
    FFormats: TArray<TBarcodeFormat>;
    function Wants(format: TBarcodeFormat): Boolean;
    /// <summary>The text of the bars in one of the formats asked for (only
    /// the ones with a strong check: strongOnly); '' when none fits.
    /// </summary>
    function DecodeStates(const states: TArray<Byte>; strongOnly: Boolean;
      out format: TBarcodeFormat): string;
  public
    constructor Create(const formats: array of TBarcodeFormat);
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>The postal barcode formats.</summary>
function IsPostalFormat(format: TBarcodeFormat): Boolean;

/// <summary>KIX (PostNL): the characters of the bars (4 per character),
/// starting with a postcode; '' when they are none.</summary>
function DecodeKIX(const states: TArray<Byte>): string;
/// <summary>RM4SCC (Royal Mail 4-State Customer Code): the characters
/// between the start and the stop bar, the check character checked and
/// removed; '' when they are none.</summary>
function DecodeRM4SCC(const states: TArray<Byte>): string;
/// <summary>POSTNET or PLANET (USPS, tall and short bars): the digits, the
/// check digit checked and removed; '' when they are none.</summary>
function DecodePostnet(const states: TArray<Byte>; planet: Boolean): string;
/// <summary>Japan Post (Kasutama): the postal code and the address, the check
/// character checked; '' when the bars are none.</summary>
function DecodeJapanPost(const states: TArray<Byte>): string;

implementation

uses
  System.Math,
  ZXing.ResultPoint;

const
  // the characters of RM4SCC and KIX: 0-9 A-Z
  RM4_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  // their bars (0 full, 1 ascender, 2 descender, 3 tracker)
  RM4KIX: array [0 .. 35, 0 .. 3] of Byte = ((3, 3, 0, 0), (3, 2, 1, 0),
    (3, 2, 0, 1), (2, 3, 1, 0), (2, 3, 0, 1), (2, 2, 1, 1), (3, 1, 2, 0),
    (3, 0, 3, 0), (3, 0, 2, 1), (2, 1, 3, 0), (2, 1, 2, 1), (2, 0, 3, 1),
    (3, 1, 0, 2), (3, 0, 1, 2), (3, 0, 0, 3), (2, 1, 1, 2), (2, 1, 0, 3),
    (2, 0, 1, 3), (1, 3, 2, 0), (1, 2, 3, 0), (1, 2, 2, 1), (0, 3, 3, 0),
    (0, 3, 2, 1), (0, 2, 3, 1), (1, 3, 0, 2), (1, 2, 1, 2), (1, 2, 0, 3),
    (0, 3, 1, 2), (0, 3, 0, 3), (0, 2, 1, 3), (1, 1, 2, 2), (1, 0, 3, 2),
    (1, 0, 2, 3), (0, 1, 3, 2), (0, 1, 2, 3), (0, 0, 3, 3));
  // KIX: at least a postcode (6 characters), at most 18
  KIX_MIN_CHARS = 6;
  KIX_MAX_CHARS = 18;
  RM4SCC_MIN_CHARS = 4;

function IsPostalFormat(format: TBarcodeFormat): Boolean;
begin
  Result := (format = TBarcodeFormat.KIX) or (format = TBarcodeFormat.RM4SCC)
    or (format = TBarcodeFormat.IMB) or (format = TBarcodeFormat.POSTNET) or
    (format = TBarcodeFormat.PLANET) or
    (format = TBarcodeFormat.JAPAN_POST) or
    (format = TBarcodeFormat.AUSTRALIA_POST) or
    (format = TBarcodeFormat.MAILMARK_4STATE);
end;

/// <summary>The index of the RM4SCC/KIX character of the 4 bars from
/// states[first]; -1 when there is none.</summary>
function RM4Char(const states: TArray<Byte>; first: Integer): Integer;
begin
  for var c := 0 to 35 do
    if (states[first] = RM4KIX[c, 0]) and (states[first + 1] = RM4KIX[c, 1])
      and (states[first + 2] = RM4KIX[c, 2]) and
      (states[first + 3] = RM4KIX[c, 3]) then
      exit(c);
  Result := -1;
end;

function DecodeKIX(const states: TArray<Byte>): string;
begin
  Result := '';
  var n := Length(states);
  if (n mod 4 <> 0) or (n div 4 < KIX_MIN_CHARS) or (n div 4 > KIX_MAX_CHARS)
  then
    exit;
  for var i := 0 to n div 4 - 1 do
  begin
    var c := RM4Char(states, 4 * i);
    if (c < 0) then
      exit('');
    Result := Result + RM4_CHARS.Chars[c];
  end;
  // a Dutch postcode in front (4 digits, 2 letters): there is no start or
  // stop, a KIX read upside down is valid as well
  for var i := 0 to 5 do
    if (i < 4) <> CharInSet(Result.Chars[i], ['0' .. '9']) then
      exit('');
end;

function DecodeRM4SCC(const states: TArray<Byte>): string;
begin
  Result := '';
  // start (ascender), characters, check character, stop (full); at least
  // RM4SCC_MIN_CHARS characters (a postcode), the check character is weak
  var n := Length(states);
  if (n < 2 + 4 * (RM4SCC_MIN_CHARS + 1)) or ((n - 2) mod 4 <> 0) or
    (states[0] <> BAR_ASCENDER) or (states[n - 1] <> BAR_FULL) then
    exit;
  var count := (n - 2) div 4;
  var top := 0;
  var bottom := 0;
  for var i := 0 to count - 1 do
  begin
    var c := RM4Char(states, 1 + 4 * i);
    if (c < 0) then
      exit('');
    if (i < count - 1) then
    begin
      Result := Result + RM4_CHARS.Chars[c];
      // the check character: the row and column of the sums of the
      // upper and lower halves (as zint's CheckCharTopBottom)
      Inc(top, (c div 6 + 1) mod 6);
      Inc(bottom, (c mod 6 + 1) mod 6);
    end
    else
    begin
      var row := top mod 6 - 1;
      var column := bottom mod 6 - 1;
      if (row = -1) then
        row := 5;
      if (column = -1) then
        column := 5;
      if (c <> 6 * row + column) then
        exit('');
    end;
  end;
end;

function DecodePostnet(const states: TArray<Byte>; planet: Boolean): string;
const
  // the tall bars of the digits (POSTNET; PLANET the other way around)
  DIGITS: array [0 .. 9, 0 .. 4] of Byte = ((1, 1, 0, 0, 0), (0, 0, 0, 1, 1),
    (0, 0, 1, 0, 1), (0, 0, 1, 1, 0), (0, 1, 0, 0, 1), (0, 1, 0, 1, 0),
    (0, 1, 1, 0, 0), (1, 0, 0, 0, 1), (1, 0, 0, 1, 0), (1, 0, 1, 0, 0));
begin
  Result := '';
  // only tall (ascender) and short (tracker) bars: frame bars, digits of 5
  // bars, a check digit
  var n := Length(states);
  if (n < 2) or ((n - 2) mod 5 <> 0) then
    exit;
  for var state in states do
    if (state <> BAR_ASCENDER) and (state <> BAR_TRACKER) then
      exit;
  if (states[0] <> BAR_ASCENDER) or (states[n - 1] <> BAR_ASCENDER) then
    exit;
  var count := (n - 2) div 5;
  // the lengths of the standard (5, 9 or 11 digits POSTNET, 11 or 13
  // PLANET) with the check digit
  if planet and (count <> 12) and (count <> 14) or not planet and
    (count <> 6) and (count <> 10) and (count <> 12) then
    exit;
  var sum := 0;
  for var i := 0 to count - 1 do
  begin
    var digit := -1;
    for var d := 0 to 9 do
    begin
      var same := true;
      for var k := 0 to 4 do
        if ((states[1 + 5 * i + k] = BAR_ASCENDER) <> ((DIGITS[d, k] = 1) xor
          planet)) then
          same := false;
      if same then
        digit := d;
    end;
    if (digit < 0) then
      exit('');
    Inc(sum, digit);
    if (i < count - 1) then
      Result := Result + Chr(Ord('0') + digit);
  end;
  if (sum mod 10 <> 0) then
    Result := '';
end;

function DecodeJapanPost(const states: TArray<Byte>): string;
const
  // the symbol characters and their bars (as zint)
  CHARS = '1234567890-abcdefgh';
  // the values of the characters for the check character
  CHECK_CHARS = '0123456789-abcdefgh';
  BARS: array [0 .. 18, 0 .. 2] of Byte = ((0, 0, 3), (0, 2, 1), (2, 0, 1),
    (0, 1, 2), (0, 3, 0), (2, 1, 0), (1, 0, 2), (1, 2, 0), (3, 0, 0),
    (0, 3, 3), (3, 0, 3), (2, 1, 3), (2, 3, 1), (1, 2, 3), (3, 2, 1),
    (1, 3, 2), (3, 1, 2), (3, 3, 0), (0, 0, 0));
  JAPAN_BARS = 67;
begin
  Result := '';
  // start (full, descender), 20 characters, check character, stop
  // (descender, full)
  if (Length(states) <> JAPAN_BARS) or (states[0] <> BAR_FULL) or
    (states[1] <> BAR_DESCENDER) or (states[JAPAN_BARS - 2] <> BAR_DESCENDER)
    or (states[JAPAN_BARS - 1] <> BAR_FULL) then
    exit;
  var symbols := '';
  var sum := 0;
  for var i := 0 to 20 do
  begin
    var c := -1;
    for var k := 0 to 18 do
      if (states[2 + 3 * i] = BARS[k, 0]) and
        (states[3 + 3 * i] = BARS[k, 1]) and
        (states[4 + 3 * i] = BARS[k, 2]) then
        c := k;
    if (c < 0) then
      exit;
    var value := CHECK_CHARS.IndexOf(CHARS.Chars[c]);
    if (i < 20) then
    begin
      symbols := symbols + CHARS.Chars[c];
      Inc(sum, value);
    end
    else if ((19 - sum mod 19) mod 19 <> value) then
      exit;
  end;

  // digits and '-'; a, b and c with a digit: A-J, K-T and U-Z; d padding
  var i := 0;
  while (i < Length(symbols)) do
  begin
    var c := symbols.Chars[i];
    if CharInSet(c, ['0' .. '9', '-']) then
      Result := Result + c
    else if CharInSet(c, ['a' .. 'c']) and (i + 1 < Length(symbols)) and
      CharInSet(symbols.Chars[i + 1], ['0' .. '9']) then
    begin
      var letter := Ord('A') + 10 * (Ord(c) - Ord('a')) +
        Ord(symbols.Chars[i + 1]) - Ord('0');
      if (letter > Ord('Z')) then
        exit('');
      Result := Result + Chr(letter);
      Inc(i);
    end
    else if (c = 'd') then
    begin
      // only padding after it
      for var k := i to Length(symbols) - 1 do
        if (symbols.Chars[k] <> 'd') then
          exit('');
      break;
    end
    else
      exit('');
    Inc(i);
  end;
end;

/// <summary>The image turned: rows become columns.</summary>
function Transposed(image: TBitMatrix): TBitMatrix;
begin
  Result := TBitMatrix.Create(image.Height, image.Width);
  for var y := 0 to image.Height - 1 do
    for var x := 0 to image.Width - 1 do
      if image[x, y] then
        Result[y, x] := true;
end;

{ TPostalReader }

constructor TPostalReader.Create(const formats: array of TBarcodeFormat);
begin
  inherited Create;
  for var f in formats do
    if IsPostalFormat(f) then
      FFormats := FFormats + [f];
end;

function TPostalReader.Wants(format: TBarcodeFormat): Boolean;
begin
  for var f in FFormats do
    if (f = format) then
      exit(true);
  Result := false;
end;

function TPostalReader.DecodeStates(const states: TArray<Byte>;
  strongOnly: Boolean; out format: TBarcodeFormat): string;
begin
  Result := '';
  // RM4SCC first: its bars without start and stop could be a KIX
  if not strongOnly and Wants(TBarcodeFormat.RM4SCC) then
  begin
    Result := DecodeRM4SCC(states);
    format := TBarcodeFormat.RM4SCC;
  end;
  if (Result = '') and Wants(TBarcodeFormat.MAILMARK_4STATE) then
  begin
    Result := DecodeMailmark(states);
    format := TBarcodeFormat.MAILMARK_4STATE;
  end;
  if (Result = '') and Wants(TBarcodeFormat.AUSTRALIA_POST) then
  begin
    Result := DecodeAustraliaPost(states);
    format := TBarcodeFormat.AUSTRALIA_POST;
  end;
  if not strongOnly and (Result = '') and Wants(TBarcodeFormat.JAPAN_POST)
  then
  begin
    Result := DecodeJapanPost(states);
    format := TBarcodeFormat.JAPAN_POST;
  end;
  if not strongOnly and (Result = '') and Wants(TBarcodeFormat.POSTNET) then
  begin
    Result := DecodePostnet(states, false);
    format := TBarcodeFormat.POSTNET;
  end;
  if not strongOnly and (Result = '') and Wants(TBarcodeFormat.PLANET) then
  begin
    Result := DecodePostnet(states, true);
    format := TBarcodeFormat.PLANET;
  end;
  if (Result = '') and Wants(TBarcodeFormat.IMB) then
  begin
    Result := DecodeIMb(states);
    format := TBarcodeFormat.IMB;
  end;
  if not strongOnly and (Result = '') and Wants(TBarcodeFormat.KIX) then
  begin
    Result := DecodeKIX(states);
    format := TBarcodeFormat.KIX;
  end;
end;

function TPostalReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TPostalReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
  var results := TList<TReadResult>.Create;
  try
    decodeMultiple(image, hints, results, 1);
    if (results.Count > 0) then
      Result := results.Extract(results[0]);
  finally
    for var r in results do
      r.Free;
    results.Free;
  end;
end;

procedure TPostalReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
const
  // the fewest bars of a symbol (an RM4SCC of 1 character)
  MIN_BARS = 10;
  // the most uncertain decisions that are changed (all combinations)
  MAX_FLIPS = 8;
begin
  if (Length(FFormats) = 0) or (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  // the tracker can be only a few pixels high
  var rowStep := 2;
  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.TRY_HARDER) then
    rowStep := 1;

  // horizontal symbols, then vertical ones in the image turned
  for var vertical in [false, true] do
  begin
    var matrix := image.BlackMatrix;
    if vertical then
      matrix := Transposed(matrix);
    try
      for var symbol in DetectPostalBars(matrix, rowStep, MIN_BARS) do
      begin
        if ResultsFull(results, maxCount) then
          exit;
        var format: TBarcodeFormat;
        // as found, or turned 180 degrees; vertical (transposed) mirrored or
        // reversed
        var text := DecodeStates(symbol.Variant(false, vertical), false,
          format);
        var turned := (text = '');
        if turned then
          text := DecodeStates(symbol.Variant(true, not vertical), false,
            format);
        // then with the least certain bars changed, for the formats with a
        // strong check
        var flipCount := Min(Length(symbol.Uncertain), MAX_FLIPS);
        var flips: Cardinal := 1;
        while (text = '') and (flips < Cardinal(1 shl flipCount)) do
        begin
          turned := false;
          text := DecodeStates(symbol.Variant(false, vertical, flips), true,
            format);
          if (text = '') then
          begin
            turned := true;
            text := DecodeStates(symbol.Variant(true, not vertical, flips),
              true, format);
          end;
          Inc(flips);
        end;
        if (text = '') then
          continue;
        var p1x := symbol.FirstX;
        var p1y := symbol.FirstY;
        var p2x := symbol.LastX;
        var p2y := symbol.LastY;
        if turned then
        begin
          p1x := symbol.LastX;
          p1y := symbol.LastY;
          p2x := symbol.FirstX;
          p2y := symbol.FirstY;
        end;
        if vertical then
        begin
          var t := p1x;
          p1x := p1y;
          p1y := t;
          t := p2x;
          p2x := p2y;
          p2y := t;
        end;
        var r := TReadResult.Create(text, nil,
          [TResultPointHelpers.CreateResultPoint(p1x, p1y),
          TResultPointHelpers.CreateResultPoint(p2x, p2y)], format);
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
      end;
    finally
      if vertical then
        matrix.Free;
    end;
  end;
end;

procedure TPostalReader.reset;
begin
  // do nothing
end;

end.
