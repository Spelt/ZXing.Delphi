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

  * Code 11 (USD-8) for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net
  * have no reader. The patterns and the check digits as in the encoder of
  * zint (code11.c).
}

unit ZXing.OneD.Code11Reader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes Code 11 barcodes: the digits and '-', each 3 bars and 2 spaces
  /// of which 1 or 2 are wide, between a start and a stop. The last
  /// character must be the modulo 11 check digit C, or the last two C and K
  /// (as zint encodes them); they stay in the text. Only read when asked
  /// for (not in Auto).
  /// </summary>
  TCode11Reader = class(TOneDReader)
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

/// <summary>The number of check digits at the end of text (Code 11
/// characters): 2 (C and K valid), 1 (C valid) or 0.</summary>
function Code11CheckDigits(const text: string): Integer;

implementation

const
  CHARS = '0123456789-';
  // the bars and spaces of the characters (wide 1), the last one the start
  // and the stop
  PATTERNS: array [0 .. 11] of string = ('00001', '10001', '01001', '11000',
    '00101', '10100', '01100', '00011', '10010', '10000', '00100', '00110');
  START_STOP = 11;
  // the quiet zones in narrow modules (the spec requires 10)
  QUIET_ZONE = 5;
  // at least this many characters, the check digits included
  MIN_CHARS = 3;

/// <summary>The modulo 11 check digit of the first count characters of
/// text, the weights from the right 1 up to maxWeight.</summary>
function CheckDigit(const text: string; count, maxWeight: Integer): Integer;
begin
  var sum := 0;
  var weight := 1;
  for var i := count downto 1 do
  begin
    Inc(sum, weight * CHARS.IndexOf(text[i]));
    Inc(weight);
    if (weight > maxWeight) then
      weight := 1;
  end;
  Result := sum mod 11;
end;

function Code11CheckDigits(const text: string): Integer;
begin
  var n := Length(text);
  Result := 0;
  if (n >= 3) and (CHARS.IndexOf(text[n - 1]) = CheckDigit(text, n - 2, 10))
    and (CHARS.IndexOf(text[n]) = CheckDigit(text, n - 1, 9)) then
    Result := 2
  else if (n >= 2) and (CHARS.IndexOf(text[n]) = CheckDigit(text, n - 1, 10))
  then
    Result := 1;
end;

/// <summary>The character of the 5 bars and spaces of view (index in
/// PATTERNS) and the threshold between narrow and wide; -1 when they are
/// none.</summary>
function Classify(const view: TPatternView; out threshold: Double): Integer;
begin
  Result := -1;
  var narrow := view[0];
  var wide := view[0];
  for var i := 1 to 4 do
  begin
    if (view[i] < narrow) then
      narrow := view[i];
    if (view[i] > wide) then
      wide := view[i];
  end;
  // wide about 2 to 3 times narrow
  if (wide < 1.5 * narrow) or (wide > 4 * narrow + 1) then
    exit;
  threshold := (narrow + wide) / 2;
  var pattern := '';
  for var i := 0 to 4 do
    if (view[i] > threshold) then
      pattern := pattern + '1'
    else
      pattern := pattern + '0';
  for var c := 0 to High(PATTERNS) do
    if (PATTERNS[c] = pattern) then
      exit(c);
end;

{ TCode11Reader }

function TCode11Reader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TCode11Reader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TCode11Reader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;

  // the start, a narrow space behind it and a quiet zone in front
  next := FindLeftGuard(next, 6, 6 * (MIN_CHARS + 1) + 5,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      var threshold: Double;
      Result := (Classify(window, threshold) = START_STOP) and
        (window[5] < threshold) and
        (spaceInPixel >= QUIET_ZONE * window.Sum(5) / 7);
    end);
  if not next.IsValid then
    exit;
  var xStart := next.PixelsInFront;
  // the width of a narrow module (the start is 7 modules)
  var module := next.Sum(5) / 7;

  var text := '';
  repeat
    if not next.Shift(6) or not next.IsValid(5) then
      exit;
    var threshold: Double;
    var c := Classify(next, threshold);
    if (c < 0) then
      exit;
    // the narrow module about the one before (no wide ones taken as narrow)
    var width := next.Sum(5);
    var modules := 7;
    if (Pos('1', PATTERNS[c], Pos('1', PATTERNS[c]) + 1) = 0) then
      modules := 6;
    if (width < 0.6 * modules * module) or (width > 1.6 * modules * module)
    then
      exit;
    module := (module + width / modules) / 2;
    if (c = START_STOP) then
    begin
      // the stop: a quiet zone behind it (or the end of the row)
      next := next.SubView(0, 5);
      if not next.IsAtLastBar and (next[5] < QUIET_ZONE * module) then
        exit;
      break;
    end;
    // a narrow space behind the character
    if not next.IsValid(6) or (next[5] >= threshold) then
      exit;
    text := text + CHARS.Chars[c];
  until false;

  if (Length(text) < MIN_CHARS) then
    exit;
  var checkDigits := Code11CheckDigits(text);
  if (checkDigits = 0) then
    exit;

  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(text, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.CODE_11);
  // ISO/IEC 15424: ]H0 one check digit, ]H1 two check digits, validated and
  // transmitted
  if (checkDigits = 2) then
    Result.SymbologyIdentifier := ']H1'
  else
    Result.SymbologyIdentifier := ']H0';
end;

end.
