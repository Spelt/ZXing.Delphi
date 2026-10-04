{
  * Copyright 2013 ZXing.Net authors
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

  * MSI (Modified Plessey), after the MSIReader of ZXing.Net (zxing-cpp and
  * ZXing Java have none), as a decoder of the bars and spaces of a row like
  * the other 1D readers. The patterns as in zint (plessey.c).
}

unit ZXing.OneD.MSIReader;

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
  /// Decodes MSI barcodes: digits of 4 bits, each a bar and a space of
  /// which one is wide (1:2). With the hint ASSUME_MSI_CHECK_DIGIT the last
  /// digit must be a modulo 10 (Luhn) check digit; it stays in the text,
  /// like ZXing.Net. MSI has no other check against false positives, so it
  /// is only read when asked for (not in Auto).
  /// </summary>
  TMSIReader = class(TOneDReader)
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

/// <summary>Whether the last digit of digits is its modulo 10 (Luhn) check
/// digit.</summary>
function IsMSICheckDigitValid(const digits: string): Boolean;

implementation

function IsMSICheckDigitValid(const digits: string): Boolean;
const
  // the digit doubled and its digits summed
  DOUBLED: array [0 .. 9] of Integer = (0, 2, 4, 6, 8, 1, 3, 5, 7, 9);
begin
  // from the right, the check digit not doubled, the one before it doubled
  var sum := 0;
  var isDoubled := false;
  for var i := Length(digits) downto 1 do
  begin
    var d := Ord(digits[i]) - Ord('0');
    if isDoubled then
      Inc(sum, DOUBLED[d])
    else
      Inc(sum, d);
    isDoubled := not isDoubled;
  end;
  Result := (Length(digits) > 1) and (sum mod 10 = 0);
end;

{ TMSIReader }

function TMSIReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TMSIReader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
end;

function TMSIReader.decodePattern(rowNumber: Integer; var next: TPatternView;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
const
  // at least 3 digits against false positives, like ZXing.Net
  MIN_DIGITS = 3;
  // the quiet zones in modules (the spec requires 12)
  QUIET_ZONE = 6;
  // the stop: narrow bar, wide space, narrow bar
  STOP: array [0 .. 2] of Integer = (1, 2, 1);
begin
  Result := nil;

  // the start: a wide bar and a narrow space, with a quiet zone in front
  next := FindLeftGuard(next, 2, 2 + 8 * MIN_DIGITS + 3,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      var bar := window[0];
      var space := window[1];
      Result := (bar >= 1.4 * space) and (bar <= 3.5 * space) and
        (spaceInPixel >= QUIET_ZONE * (bar + space) / 3);
    end);
  if not next.IsValid then
    exit;
  var xStart := next.PixelsInFront;
  // a bar and a space are always 3 modules wide
  var pairWidth: Double := next.Sum;

  var digits := '';
  repeat
    next.SkipPair;
    if not next.IsValid(3) then
      exit;
    if IsRightGuard(next.SubView(0, 3), STOP, QUIET_ZONE) then
      break;
    // a digit: 4 bits, a wide bar 1 and a wide space 0, the highest first
    var value := 0;
    for var b := 0 to 3 do
    begin
      if (b > 0) and not next.SkipPair then
        exit;
      if not next.IsValid(2) then
        exit;
      var bar := next[0];
      var space := next[1];
      var width := bar + space;
      if (width < 0.6 * pairWidth) or (width > 1.5 * pairWidth) or
        (bar = space) then
        exit;
      value := 2 * value + Ord(bar > space);
      pairWidth := (3 * pairWidth + width) / 4;
    end;
    if (value > 9) then
      exit;
    digits := digits + Chr(Ord('0') + value);
  until false;

  if (Length(digits) < MIN_DIGITS) then
    exit;
  var checkDigit := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.ASSUME_MSI_CHECK_DIGIT);
  if checkDigit and not IsMSICheckDigitValid(digits) then
    exit;

  next := next.SubView(0, 3);
  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(digits, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.MSI);
  // ISO/IEC 15424: ]M0 no check digit, ]M1 modulo 10 check digit
  // validated and transmitted
  if checkDigit then
    Result.SymbologyIdentifier := ']M1'
  else
    Result.SymbologyIdentifier := ']M0';
end;

end.
