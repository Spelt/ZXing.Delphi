{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
  * Copyright 2020 Axel Waggershauser
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

  * Ported from zxing-cpp (ODCodabarReader.cpp).
}

unit ZXing.OneD.CodabarReader;

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
  /// Decodes Codabar barcodes. Like the Java version the start and stop
  /// characters (A to D) are not in the text, unless the hint
  /// RETURN_CODABAR_START_END is set.
  /// </summary>
  TCodabarReader = class(TOneDReader)
  private
    /// <summary>The character of the 7 bars and spaces of view, #0 when
    /// none.</summary>
    class function DecodeChar(const view: TPatternView): Char; static;
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

implementation

const
  ALPHABET: string = '0123456789-$:/.+ABCD';
  // the patterns of wide (1) and narrow (0) bars and spaces of the
  // characters of ALPHABET
  CHARACTER_ENCODINGS: array [0 .. 19] of Integer = ($03, $06, $09, $60, $12,
    $42, $21, $24, $30, $48, $0C, $18, $45, $51, $54, $15, $1A, $29, $0B, $0E);

  // each character has 4 bars and 3 spaces
  CHAR_LEN = 7;
  // the quiet zone is half a character
  QUIET_ZONE_SCALE = 0.5;

function IsStartOrStop(c: Char): Boolean;
begin
  Result := (c >= 'A') and (c <= 'D');
end;

/// <summary>The character of the 7 bars and spaces of view, #0 when none.
/// </summary>
class function TCodabarReader.DecodeChar(const view: TPatternView): Char;
begin
  Result := #0;
  var pattern := NarrowWideBitPattern(view);
  if (pattern < 0) then
    exit;
  for var i := 0 to High(CHARACTER_ENCODINGS) do
    if (CHARACTER_ENCODINGS[i] = pattern) then
      exit(ALPHABET.Chars[i]);
end;

{ TCodabarReader }

function TCodabarReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TCodabarReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TCodabarReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  // start, stop and at least 2 characters: fewer give too many false
  // positives
  MIN_CHAR_COUNT = 4;
begin
  Result := nil;

  // the start characters ABCD all start with a narrow bar and have a wide
  // space in the middle and either on the first or the third position:
  // not completely specific but fast, the character itself is checked
  // below
  next := FindLeftGuard(next, CHAR_LEN, MIN_CHAR_COUNT * CHAR_LEN,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      Result := (spaceInPixel > window[0] * 4) and (window[0] < window[3]) and
        (window[1] + window[5] > window[3]) and
        (spaceInPixel > window.Sum * QUIET_ZONE_SCALE);
    end);
  if not next.IsValid then
    exit;

  var startView := next;
  // the spec says 1 narrow space, half a character is about 4
  var maxInterCharacterSpace := next.Sum div 2;

  var txt := '';
  var c := DecodeChar(next);
  if not IsStartOrStop(c) then
    exit;
  txt := txt + c;

  repeat
    // the remaining width and the space between the characters
    if not next.SkipSymbol or not next.SkipSingle(maxInterCharacterSpace) then
      exit;
    c := DecodeChar(next);
    if (c = #0) then
      exit;
    txt := txt + c;
  until IsStartOrStop(c);

  // the length and the white space behind the stop character
  if (Length(txt) < MIN_CHAR_COUNT) or
    not next.HasQuietZoneAfter(QUIET_ZONE_SCALE) then
    exit;

  // without the start and stop characters, like Java
  if (hints = nil) or not hints.ContainsKey
    (TDecodeHintType.RETURN_CODABAR_START_END) then
    txt := Copy(txt, 2, Length(txt) - 2);

  // the middle of the start and stop characters
  Result := TReadResult.Create(txt, nil,
    [TResultPointHelpers.CreateResultPoint(startView.PixelsInFront +
    startView.Sum / 2, rowNumber), TResultPointHelpers.CreateResultPoint
    (next.PixelsInFront + next.Sum / 2, rowNumber)], TBarcodeFormat.CODABAR);
  // ISO/IEC 15424:2008 4.4.9 (no check digit)
  Result.SymbologyIdentifier := ']F0';
end;

end.
