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

  * Korea Post barcode for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net
  * have no reader. The encoding as in zint (postal.c).
}

unit ZXing.OneD.KoreaPostReader;

interface

uses
  System.SysUtils,
  System.Math,
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
  /// Decodes the Korea Post barcode: a postal code of 6 digits and a check
  /// digit (checked, behind it in the text, as printed below the bars),
  /// each digit 4 narrow bars at their own places, the last digit first.
  /// Only read when asked for (not in Auto).
  /// </summary>
  TKoreaPostReader = class(TOneDReader)
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

/// <summary>The postal code and the check digit of the places of the bars
/// (in modules from the first one, 28 bars); '' when they are none or
/// the check digit is wrong.
/// </summary>
function DecodeKoreaPost(const places: TArray<Integer>): string;

implementation

const
  DIGIT_COUNT = 7;
  BARS = 4 * DIGIT_COUNT;
  // the places of the 4 bars of the digits in modules and their widths (as
  // zint: the 1 is 23 modules wide, the others 24); the last one the 1 as
  // other encoders draw it: a module further, its bars on the places of the
  // other digits (the next digit as far from its bars)
  PATTERNS = 11;
  DIGIT_PLACES: array [0 .. PATTERNS - 1, 0 .. 3] of Integer = ((0, 4, 8, 20),
    (7, 11, 15, 19), (4, 12, 16, 20), (0, 12, 16, 20), (4, 8, 16, 20),
    (0, 8, 16, 20), (0, 4, 16, 20), (4, 8, 12, 20), (0, 8, 12, 20),
    (0, 4, 12, 20), (8, 12, 16, 20));
  DIGIT_WIDTHS: array [0 .. PATTERNS - 1] of Integer = (24, 23, 24, 24, 24,
    24, 24, 24, 24, 24, 24);
  DIGIT_VALUES: array [0 .. PATTERNS - 1] of Integer = (0, 1, 2, 3, 4, 5, 6,
    7, 8, 9, 1);
  // the quiet zones in modules: wider than the narrowest spaces (3; the
  // widest are 11: the 28 bars and the check digit tell the symbol)
  QUIET_ZONE = 4;

/// <summary>The digits of the bars from bar on, the digit there starting at
/// module start (depth first: the places of different digits overlap).
/// </summary>
function DecodeDigits(const places: TArray<Integer>; bar, start: Integer;
  var digits: string): Boolean;
begin
  if (bar = BARS) then
    exit(true);
  for var d := 0 to PATTERNS - 1 do
  begin
    var fits := true;
    for var j := 0 to 3 do
      if (places[bar + j] <> start + DIGIT_PLACES[d, j]) then
      begin
        fits := false;
        break;
      end;
    if fits then
    begin
      digits := digits + Chr(Ord('0') + DIGIT_VALUES[d]);
      if DecodeDigits(places, bar + 4, start + DIGIT_WIDTHS[d], digits) then
        exit(true);
      SetLength(digits, Length(digits) - 1);
    end;
  end;
  Result := false;
end;

function DecodeKoreaPost(const places: TArray<Integer>): string;
begin
  Result := '';
  if (Length(places) <> BARS) then
    exit;
  // the first digit: its first bar at the first place
  var digits := '';
  var found := false;
  for var d := 0 to PATTERNS - 1 do
    if not found and DecodeDigits(places, 0, places[0] - DIGIT_PLACES[d, 0],
      digits) then
      found := true;
  if not found then
    exit;
  // the last digit of the postal code first, then the check digit
  var sum := 0;
  for var i := 1 to DIGIT_COUNT - 1 do
  begin
    Result := digits[i] + Result;
    Inc(sum, Ord(digits[i]) - Ord('0'));
  end;
  if ((10 - sum mod 10) mod 10 <> Ord(digits[DIGIT_COUNT]) - Ord('0')) then
    exit('');
  // the check digit in the text too (as printed below the bars)
  Result := Result + digits[DIGIT_COUNT];
end;

/// <summary>The places of the bars of view (28 bars and the spaces between
/// them) in modules from the first one; nil when the bars are not narrow
/// and the same or the places not whole modules.</summary>
function BarPlaces(const view: TPatternView; out module: Double)
  : TArray<Integer>;
begin
  Result := nil;
  // the centers of the bars
  var centers: TArray<Double>;
  SetLength(centers, BARS);
  var x := 0.0;
  var minBar := MaxInt;
  var maxBar := 0;
  for var i := 0 to BARS - 1 do
  begin
    centers[i] := x + view[2 * i] / 2;
    x := x + view[2 * i];
    if (i < BARS - 1) then
      x := x + view[2 * i + 1];
    minBar := Min(minBar, view[2 * i]);
    maxBar := Max(maxBar, view[2 * i]);
  end;
  if (maxBar > 2 * minBar + 1) then
    exit;
  // the module: the smallest distances between bars are 3 or 4 modules
  var minGap := MaxDouble;
  for var i := 1 to BARS - 1 do
    minGap := Min(minGap, centers[i] - centers[i - 1]);
  var sum := 0.0;
  var count := 0;
  for var i := 1 to BARS - 1 do
    if (centers[i] - centers[i - 1] <= 1.15 * minGap) then
    begin
      sum := sum + centers[i] - centers[i - 1];
      Inc(count);
    end;
  module := sum / count / 4;
  // the places, then the module of the whole symbol and the places again
  SetLength(Result, BARS);
  for var pass := 0 to 1 do
  begin
    Result[0] := 0;
    for var i := 1 to BARS - 1 do
    begin
      var gap := (centers[i] - centers[i - 1]) / module;
      if (Abs(gap - Round(gap)) > 0.4) or (Round(gap) < 3) then
        exit(nil);
      Result[i] := Result[i - 1] + Round(gap);
    end;
    module := (centers[BARS - 1] - centers[0]) / Result[BARS - 1];
  end;
  // the bars narrow (1 module)
  if (maxBar > 2.5 * module) then
    exit(nil);
end;

{ TKoreaPostReader }

function TKoreaPostReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TKoreaPostReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TKoreaPostReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
  var text := '';
  // 28 bars between quiet zones
  next := FindLeftGuard(next, 2 * BARS - 1, 2 * BARS - 1,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      Result := false;
      var module: Double;
      var places := BarPlaces(window, module);
      if (places = nil) or (spaceInPixel < QUIET_ZONE * module) or
        not window.IsAtLastBar and (window[2 * BARS - 1] < QUIET_ZONE * module)
      then
        exit;
      text := DecodeKoreaPost(places);
      Result := (text <> '');
    end);
  if not next.IsValid then
    exit;

  var xStart := next.PixelsInFront;
  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(text, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.KOREA_POST);
end;

end.
