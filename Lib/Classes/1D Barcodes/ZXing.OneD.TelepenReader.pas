{
  * Copyright 2026 Axel Waggershauser
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

  * Ported from zxing-cpp (ODTelepenReader.cpp), based on the AIM Europe USS
  * Telepen (1991) specification and ISO/IEC 15424:2025.
}

unit ZXing.OneD.TelepenReader;

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
  /// Decodes Telepen barcodes: full ASCII, compressed numeric and the
  /// combinations of both. The symbology identifier tells which one: ]B0
  /// ASCII, ]B1 numeric, ]B2 numeric then ASCII, ]B4 ASCII then numeric.
  /// </summary>
  TTelepenReader = class(TOneDReader)
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
  // the start patterns: 1 "full ASCII", 2 "compressed numeric (+ full
  // ASCII)", 3 "full ASCII + compressed numeric"
  START_PATTERNS: array [1 .. 3, 0 .. 11] of Integer = (
    (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3),
    (1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3),
    (1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3));
  END_PATTERNS: array [1 .. 3, 0 .. 10] of Integer = (
    (3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    (3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1),
    (3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1));
  DLE = #16; // the shift between ASCII and numeric

/// <summary>Appends the n bits of value (its highest bit first) to bits,
/// which has count bits; the first bit is the lowest one.</summary>
procedure AppendBits(var bits, count: Integer; value, n: Integer);
begin
  for var b := n - 1 downto 0 do
  begin
    if ((value shr b) and 1 = 1) then
      bits := bits or (1 shl count);
    Inc(count);
  end;
end;

/// <summary>The digits of the compressed numeric codewords; '' when one is
/// not valid.</summary>
function DecodeNumeric(const encoded: string): string;
begin
  Result := '';
  for var c in encoded do
  begin
    var codeword := Ord(c);
    if (codeword < 16) or (codeword = 127) then
      // control characters
      Result := Result + c
    else if (codeword >= 17) and (codeword < 27) then
      // a digit and X
      Result := Result + Chr(Ord('0') + codeword - 17) + 'X'
    else if (codeword >= 27) and (codeword < 127) then
      // 2 digits
      Result := Result + Format('%.2d', [codeword - 27])
    else
      // the shift (16) can not appear twice
      exit('');
  end;
end;

{ TTelepenReader }

function TTelepenReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TTelepenReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TTelepenReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  MIN_CHAR_COUNT = 1;
  MIN_QUIET_ZONE = 5; // the spec requires 10
  MIN_CHAR_LENGTH = 16 div 3;
begin
  Result := nil;

  // the 6 narrow bars and spaces in front of each start pattern
  next := FindLeftGuard(next, 2 * 12 + MIN_CHAR_COUNT * MIN_CHAR_LENGTH,
    [1, 1, 1, 1, 1, 1], MIN_QUIET_ZONE, true);
  if not next.IsValid then
    exit;

  var startChar := 1;
  while (startChar < 4) do
  begin
    if (IsPattern(next, START_PATTERNS[startChar], true, next.SpaceInFront,
      MIN_QUIET_ZONE) <> 0) then
      break;
    Inc(startChar);
  end;
  if (startChar = 4) then
    exit;

  var xStart := next.PixelsInFront;
  next := next.SubView(0, 12);

  // the threshold between narrow and wide from the start pattern
  var threshold := NarrowWideThreshold(next);
  if not threshold.IsValid then
    exit;

  next := next.SubView(12, 11);
  var raw := '';
  while next.IsValid and not IsRightGuard(next, END_PATTERNS[startChar],
    MIN_QUIET_ZONE, true) do
  begin
    // 8 bits from pairs of a bar and a space; the first bit is the lowest
    var bits := 0;
    var count := 0;
    var inBlock := false;
    var wSum := TBarAndSpace.Create(0, 0);
    var wNum := TBarAndSpace.Create(0, 0);
    var nSum := TBarAndSpace.Create(0, 0);
    var nNum := TBarAndSpace.Create(0, 0);

    while next.IsValid and (count < 8) do
    begin
      if (next[0] > threshold[0] * 3) or (next[1] > threshold[1] * 3) then
        exit;
      var wideBar := next[0] > threshold[0];
      var wideSpace := next[1] > threshold[1];
      if not wideBar and not wideSpace then
        // narrow bar, narrow space
        AppendBits(bits, count, 1, 1)
      else if not wideBar and wideSpace then
      begin
        // narrow bar, wide space: trailing 10 or leading 01
        if inBlock then
          AppendBits(bits, count, 2, 2)
        else
          AppendBits(bits, count, 1, 2);
        inBlock := not inBlock;
      end
      else if wideBar and not wideSpace then
        // wide bar, narrow space
        AppendBits(bits, count, 0, 2)
      else
        // wide bar, wide space
        AppendBits(bits, count, 2, 3);

      for var i := 0 to 1 do
        if (next[i] > threshold[i]) then
        begin
          wSum[i] := wSum[i] + next[i];
          wNum[i] := wNum[i] + 1;
        end
        else
        begin
          nSum[i] := nSum[i] + next[i];
          nNum[i] := nNum[i] + 1;
        end;

      next.SkipPair;
    end;

    // 8 bits with an even number of 0 bits
    if (count <> 8) then
      exit;
    var zeros := 0;
    for var b := 0 to 7 do
      if (bits shr b) and 1 = 0 then
        Inc(zeros);
    if Odd(zeros) then
      exit;
    // without the parity bit
    raw := raw + Chr(bits and $7F);

    // the new thresholds from the widths of this character
    for var i := 0 to 1 do
      if (wNum[i] > 0) and (nNum[i] > 0) then
        threshold[i] := ((wSum[i] + wNum[i] div 2) div wNum[i] +
          (nSum[i] + nNum[i] div 2) div nNum[i]) div 2
      else if (nNum[i] > 0) then
        threshold[i] := (2 * nSum[i] + nNum[i] div 2) div nNum[i];
  end;

  if (Length(raw) < MIN_CHAR_COUNT + 1) or not IsRightGuard(next,
    END_PATTERNS[startChar], MIN_QUIET_ZONE, true) then
    exit;

  // the last character is the checksum
  var txt := Copy(raw, 1, Length(raw) - 1);
  var sum := 0;
  for var c in txt do
    Inc(sum, Ord(c));
  if ((127 - sum mod 127) mod 127 <> Ord(raw[Length(raw)])) then
    exit;

  // start 1 with compressed numeric data in the wild: at most one shift
  // that is not the first character, characters below 32 but none of the
  // ones (below 16) that compressed numeric does not use for digits
  if (startChar = 1) then
  begin
    var shifts := 0;
    var below32 := false;
    var below16 := false;
    for var c in txt do
    begin
      if (c = DLE) then
        Inc(shifts);
      if (Ord(c) < 32) then
        below32 := true;
      if (Ord(c) < 16) then
        below16 := true;
    end;
    if (shifts <= 1) and (txt[1] <> DLE) and below32 and not below16 then
      startChar := 2;
  end;

  var shift := Pos(DLE, txt);
  var modifier: Char;
  if (startChar = 1) then
    modifier := '0'
  else if (startChar = 2) and (shift <> 1) then
  begin
    // compressed numeric, then full ASCII after a shift
    var num: string;
    var alpha := '';
    if (shift = 0) then
      num := DecodeNumeric(txt)
    else
    begin
      num := DecodeNumeric(Copy(txt, 1, shift - 1));
      alpha := Copy(txt, shift + 1, MaxInt);
    end;
    if (num = '') then
      exit;
    txt := num + alpha;
    if (shift = 0) then
      modifier := '1'
    else
      modifier := '2';
  end
  else if (startChar = 3) and (shift <> 1) then
  begin
    // full ASCII, then compressed numeric after a shift
    if (shift <> 0) then
    begin
      var num := DecodeNumeric(Copy(txt, shift + 1, MaxInt));
      if (num = '') then
        exit;
      txt := Copy(txt, 1, shift - 1) + num;
      modifier := '4';
    end
    else
      modifier := '0';
  end
  else
    // a shift as first character is not allowed
    exit;

  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(txt, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.TELEPEN);
  Result.SymbologyIdentifier := ']B' + modifier;
end;

end.
