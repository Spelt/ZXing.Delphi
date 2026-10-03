{
  * Copyright 2010 ZXing authors
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

  * Original Authors: Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.OneD.Code93Reader;

interface

uses
  System.SysUtils,
  System.Types,
  System.Generics.Collections,
  Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary>
  /// <p>Decodes Code 93 barcodes.</p>
  /// <see cref="TCode39Reader" />
  /// </summary>
  TCode93Reader = class sealed(TOneDReader)
  private
    counters: TArray<Integer>;
    decodeRowResult: TStringBuilder;

    function checkChecksums(pResult: TStringBuilder): boolean;
    function checkOneChecksum(pResult: TStringBuilder;
      checkPosition, weightMax: Integer): boolean;
    function decodeExtended(encoded: TStringBuilder): string;

    function findAsteriskPattern(row: IBitArray): TArray<Integer>;
    function patternToChar(pattern: Integer; var c: Char): boolean;
    function toPattern(counters: TArray<Integer>): Integer;

  const
    ALPHABET_STRING = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%abcd*';
    CHARACTER_ENCODINGS: TIntegerDynArray = [
      $114, $148, $144, $142, $128, $124, $122, $150, $112, $10A, // 0-9
      $1A8, $1A4, $1A2, $194, $192, $18A, $168, $164, $162, $134, // A-J
      $11A, $158, $14C, $146, $12C, $116, $1B4, $1B2, $1AC, $1A6, // K-T
      $196, $19A, $16C, $166, $136, $13A, $12E, $1D4, $1D2, $1CA, // U-$
      $16E, $176, $1AE, $126, $1DA, $1D6, $132, $15E];            // /-*
    ASTERISK_ENCODING = $15E;

    /// <summary>The character of the 6 bars and spaces of view by their
    /// edge to edge widths, #0 when none.</summary>
    class function DecodeChar(const view: TPatternView): Char; static;
  protected
    /// <summary>The decoder of zxing-cpp: the start character with a quiet
    /// zone of half a character, the characters by their edge to edge
    /// widths, the termination bar and the quiet zone behind the stop
    /// character. The text is made as decodeRow does (check characters,
    /// full ASCII).</summary>
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

{ Code93Reader }

const
  CHAR_LEN = 6; // 3 bars and 3 spaces
  CHAR_MODS = 9;
  ASTERISK_INDEX = 47;

var
  // the edge to edge widths (in modules) of the characters of
  // CHARACTER_ENCODINGS
  CharE2E: TArray<TArray<Integer>>;

procedure InitE2E(const encodings: array of Integer);
begin
  SetLength(CharE2E, Length(encodings));
  for var k := 0 to High(encodings) do
  begin
    // the widths of the bars and spaces of the 9 modules (from the left,
    // starting with a bar)
    var widths: TArray<Integer>;
    var bit := 8;
    while (bit >= 0) do
    begin
      var black := (encodings[k] shr bit) and 1;
      var w := 0;
      while (bit >= 0) and ((encodings[k] shr bit) and 1 = black) do
      begin
        Inc(w);
        Dec(bit);
      end;
      widths := widths + [w];
    end;
    SetLength(CharE2E[k], CHAR_LEN - 2);
    for var i := 0 to CHAR_LEN - 3 do
      CharE2E[k][i] := widths[i] + widths[i + 1];
  end;
end;

/// <summary>The index of the character of view by its edge to edge widths;
/// -1 when none.</summary>
function E2EIndex(const view: TPatternView): Integer;
begin
  Result := -1;
  var moduleSize: Double := view.Sum(CHAR_LEN) / CHAR_MODS;
  if not(moduleSize > 0) then
    exit;
  var e2e: array [0 .. CHAR_LEN - 3] of Integer;
  for var i := 0 to CHAR_LEN - 3 do
    e2e[i] := Trunc((view[i] + view[i + 1]) / moduleSize + 0.5);
  for var k := 0 to High(CharE2E) do
    if (CharE2E[k][0] = e2e[0]) and (CharE2E[k][1] = e2e[1]) and
      (CharE2E[k][2] = e2e[2]) and (CharE2E[k][3] = e2e[3]) then
      exit(k);
end;

class function TCode93Reader.DecodeChar(const view: TPatternView): Char;
begin
  if (CharE2E = nil) then
    InitE2E(CHARACTER_ENCODINGS);
  var k := E2EIndex(view);
  if (k < 0) then
    Result := #0
  else
    Result := ALPHABET_STRING.Chars[k];
end;

function TCode93Reader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TCode93Reader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  // start, stop, 2 check characters and 1 character
  MIN_CHAR_COUNT = 5;
  // the quiet zone is half a character
  QUIET_ZONE_SCALE = 0.5;
begin
  Result := nil;
  if (CharE2E = nil) then
    InitE2E(CHARACTER_ENCODINGS);

  // the start character: 1-1-1-1 with a quiet zone, then 4-1, then its edge
  // to edge widths
  next := FindLeftGuard(next, CHAR_LEN, MIN_CHAR_COUNT * CHAR_LEN,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      Result := (IsPattern(window, [1, 1, 1, 1], false, spaceInPixel,
        QUIET_ZONE_SCALE * 12) <> 0) and (window[4] > 3 * window[5] - 2) and
        (E2EIndex(window) = ASTERISK_INDEX);
    end);
  if not next.IsValid then
    exit;
  var startView := next;

  var chars := TStringBuilder.Create;
  try
    var c: Char;
    repeat
      // the remaining width
      if not next.SkipSymbol then
        exit;
      c := DecodeChar(next);
      if (c = #0) then
        exit;
      chars.Append(c);
    until (c = '*');
    // without the stop character
    chars.Length := chars.Length - 1;
    if (chars.Length < MIN_CHAR_COUNT - 2) then
      exit;

    // the termination bar (not wider than about 2 modules) and the quiet
    // zone
    var stopView := next;
    next := next.SubView(0, CHAR_LEN + 1);
    if not next.IsValid or (next[CHAR_LEN] > next.Sum(CHAR_LEN) div 4) or
      not next.HasQuietZoneAfter(QUIET_ZONE_SCALE) then
      exit;

    // the same as decodeRow: both check characters, then full ASCII
    if not checkChecksums(chars) then
      exit;
    chars.Length := chars.Length - 2;
    var text := decodeExtended(chars);
    if (text = '') then
      exit;

    // the middle of the start and stop characters
    Result := TReadResult.Create(text, nil,
      [TResultPointHelpers.CreateResultPoint(startView.PixelsInFront +
      startView.Sum / 2, rowNumber), TResultPointHelpers.CreateResultPoint
      (stopView.PixelsInFront + stopView.Sum / 2, rowNumber)],
      TBarcodeFormat.CODE_93);
    Result.SymbologyIdentifier := ']G0';
  finally
    chars.Free;
  end;
end;

constructor TCode93Reader.Create;
begin
  counters := TArray<Integer>.Create();
  SetLength(counters, 6);
  decodeRowResult := TStringBuilder.Create();
end;

destructor TCode93Reader.Destroy;
begin
  counters := nil;
  FreeAndNil(decodeRowResult);
  inherited;
end;

function TCode93Reader.checkChecksums(pResult: TStringBuilder): boolean;
var
  length: Integer;
begin
  length := pResult.length;
  if (not checkOneChecksum(pResult, (length - 2), 20)) then
  begin
    Result := false;
    exit
  end;
  if (not checkOneChecksum(pResult, (length - 1), 15)) then
  begin
    Result := false;
    exit
  end;
  begin
    Result := true;
    exit
  end
end;

function TCode93Reader.checkOneChecksum(pResult: TStringBuilder;
  checkPosition, weightMax: Integer): boolean;
var
  weight, total, i: Integer;
begin
  weight := 1;
  total := 0;
  i := (checkPosition - 1);
  while ((i >= 0)) do
  begin

    inc(total, (weight * ALPHABET_STRING.IndexOf(pResult.Chars[i])));

    inc(weight);

    if (weight > weightMax) then
      weight := 1;

    dec(i)
  end;

  if (pResult.Chars[checkPosition] <> ALPHABET_STRING[(total mod $2F) + 1])
  then
  begin
    Result := false;
    exit
  end;
  begin
    Result := true;
    exit
  end
end;

function TCode93Reader.decodeExtended(encoded: TStringBuilder): string;
var
  next, c, decodedChar: Char;
  length, i: Integer;
  decoded: TStringBuilder;
begin
  // The shift characters a, b, c and d (written as ($), (%), (/) and (+)
  // on the label) combine with the next character into one Full ASCII
  // character, see the Code 93 Full ASCII table.
  Result := '';
  length := encoded.length;
  decoded := TStringBuilder.Create(length);
  try
    i := 0;
    while (i < length) do
    begin
      c := encoded.Chars[i];
      if ((c >= 'a') and (c <= 'd')) then
      begin
        if (i >= length - 1) then
          exit;
        next := encoded.Chars[i + 1];
        case c of
          'd':
            // +A to +Z map to a to z
            if ((next >= 'A') and (next <= 'Z')) then
              decodedChar := Char(ord(next) + 32)
            else
              exit;
          'a':
            // $A to $Z map to control codes SOH to SUB
            if ((next >= 'A') and (next <= 'Z')) then
              decodedChar := Char(ord(next) - 64)
            else
              exit;
          'b':
            if ((next >= 'A') and (next <= 'E')) then
              decodedChar := Char(ord(next) - 38) // ESC FS GS RS US
            else if ((next >= 'F') and (next <= 'J')) then
              decodedChar := Char(ord(next) - 11) // ; < = > ?
            else if ((next >= 'K') and (next <= 'O')) then
              decodedChar := Char(ord(next) + 16) // [ \ ] ^ _
            else if ((next >= 'P') and (next <= 'T')) then
              decodedChar := Char(ord(next) + 43) // { | } ~ DEL
            else if (next = 'U') then
              decodedChar := #0
            else if (next = 'V') then
              decodedChar := '@'
            else if (next = 'W') then
              decodedChar := '`'
            else if ((next >= 'X') and (next <= 'Z')) then
              decodedChar := #127 // DEL
            else
              exit;
        else // 'c'
          // /A to /O map to ! to , and /Z maps to :
          if ((next >= 'A') and (next <= 'O')) then
            decodedChar := Char(ord(next) - 32)
          else if (next = 'Z') then
            decodedChar := ':'
          else
            exit;
        end;
        decoded.Append(decodedChar);
        // two characters were read
        Inc(i, 2);
      end
      else
      begin
        decoded.Append(c);
        Inc(i);
      end;
    end;

    Result := decoded.ToString;

  finally
    FreeAndNil(decoded);
  end;

end;

function TCode93Reader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  decodedChar: Char;
  lastStart: Integer;
  index, nextStart, counter, aEnd, pattern, lastPatternSize: Integer;
  start: TArray<Integer>;
  resultString: String;
  Left, Right: Single;
  resultPointCallback: TResultPointCallback;
  obj: TObject;
  resultPoints: TArray<IResultPoint>;
  resultPointLeft, resultPointRight: IResultPoint;
begin
  for index := 0 to length(counters) - 1 do
    counters[index] := 0;

  decodeRowResult.length := 0;

  start := self.findAsteriskPattern(row);
  if (start = nil) then
  begin
    Result := nil;
    exit
  end;
  nextStart := row.getNextSet(start[1]);
  aEnd := row.Size;
  repeat
    if (not recordPattern(row, nextStart, self.counters)) then
    begin
      Result := nil;
      exit
    end;
    pattern := toPattern(self.counters);
    if (pattern < 0) then
    begin
      Result := nil;
      exit
    end;
    if (not patternToChar(pattern, decodedChar)) then
    begin
      Result := nil;
      exit
    end;

    self.decodeRowResult.Append(decodedChar);
    lastStart := nextStart;

    for counter in self.counters do
    begin
      inc(nextStart, counter)
    end;
    nextStart := row.getNextSet(nextStart)

  until (decodedChar = '*');

  self.decodeRowResult.Remove((self.decodeRowResult.length - 1), 1);

  lastPatternSize := 0;

  for counter in self.counters do
  begin
    inc(lastPatternSize, counter)
  end;

  if (not((nextStart <> aEnd) and row[nextStart])) then
  begin
    Result := nil;
    exit
  end;

  if (self.decodeRowResult.length < 2) then
  begin
    Result := nil;
    exit
  end;

  if (not checkChecksums(self.decodeRowResult)) then
  begin
    Result := nil;
    exit
  end;

  self.decodeRowResult.length := self.decodeRowResult.length - 2;

  resultString := decodeExtended(self.decodeRowResult);
  if (resultString = '') then
  begin
    Result := nil;
    exit
  end;

  Left := (start[1] + start[0]) div 2;
  Right := lastStart + lastPatternSize / 2;

  resultPointLeft := TResultPointHelpers.CreateResultPoint(Left, rowNumber);
  resultPointRight := TResultPointHelpers.CreateResultPoint(Right, rowNumber);
  resultPoints := [resultPointLeft, resultPointRight];

  resultPointCallback := nil;
  // it is a local variable: it doesn't get NIL as default value, and the following ifs do not assign a value for all possible cases

  if ((hints = nil) or
    (not hints.ContainsKey(TDecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
    resultPointCallback := nil
  else
  begin
    obj := hints[TDecodeHintType.NEED_RESULT_POINT_CALLBACK];
    if (obj is TResultPointEventObject) then
      resultPointCallback := TResultPointEventObject(obj).Event;
  end;

  if Assigned(resultPointCallback) then
  begin
    resultPointCallback(resultPointLeft);
    resultPointCallback(resultPointRight);
  end;

  Result := TReadResult.Create(resultString, nil, resultPoints,
    TBarcodeFormat.CODE_93);
  Result.SymbologyIdentifier := ']G0';
end;

function TCode93Reader.findAsteriskPattern(row: IBitArray): TArray<Integer>;
var
  i, l, patternStart, patternLength, width, counterPosition, rowOffset,
    index: Integer;
  isWhite: boolean;

begin
  width := row.Size;
  rowOffset := row.getNextSet(0);
  index := 0;
  while (index < length(self.counters)) do
  begin
    self.counters[index] := 0;
    inc(index)
  end;

  counterPosition := 0;
  patternStart := rowOffset;
  isWhite := false;
  patternLength := length(counters);

  for i := rowOffset to width - 1 do
  begin

    if (row[i] xor isWhite) then
    begin
      inc(counters[counterPosition])
    end
    else
    begin
      if (counterPosition = (patternLength - 1)) then
      begin

        if (toPattern(counters) = ASTERISK_ENCODING)
        then
        begin
          Result := TArray<Integer>.Create(patternStart, i);
          exit
        end;

        inc(patternStart, (counters[0] + counters[1]));

        for l := 2 to patternLength - 2 do
        begin
          counters[l - 2] := counters[l];
        end;

        counters[(patternLength - 2)] := 0;
        counters[(patternLength - 1)] := 0;
        dec(counterPosition)

      end
      else
        inc(counterPosition);

      counters[counterPosition] := 1;
      isWhite := not isWhite
    end;
  end;

  Result := nil;

end;

function TCode93Reader.patternToChar(pattern: Integer; var c: Char): boolean;
var
  i: Integer;
begin
  i := 0;
  while (i < length(CHARACTER_ENCODINGS)) do
  begin
    if (CHARACTER_ENCODINGS[i] = pattern) then
    begin
      c := ALPHABET_STRING[i + 1];
      begin
        Result := true;
        exit
      end
    end;
    inc(i)
  end;
  c := '*';
  Result := false;
end;

function TCode93Reader.toPattern(counters: TArray<Integer>): Integer;
var
  counter, max, sum, pattern, i, j, scaledShifted, scaledUnshifted: Integer;
begin
  max := length(counters);
  sum := 0;

  for counter in counters do
  begin
    inc(sum, counter)
  end;

  pattern := 0;
  i := 0;
  while ((i < max)) do
  begin

    scaledShifted := (((counters[i] shl TOneDReader.INTEGER_MATH_SHIFT) *
      9) div sum);

    scaledUnshifted := TMathUtils.Asr(scaledShifted,
      TOneDReader.INTEGER_MATH_SHIFT);

    if ((scaledShifted and $FF) > $7F) then
      inc(scaledUnshifted);

    if ((scaledUnshifted < 1) or (scaledUnshifted > 4)) then
    begin
      Result := -1;
      exit
    end;

    if ((i and 1) = 0) then
    begin
      j := 0;

      while ((j < scaledUnshifted)) do
      begin
        pattern := ((pattern shl 1) or 1);
        inc(j)
      end
    end
    else
      pattern := (pattern shl scaledUnshifted);

    inc(i)
  end;

  Result := pattern;

end;

end.
