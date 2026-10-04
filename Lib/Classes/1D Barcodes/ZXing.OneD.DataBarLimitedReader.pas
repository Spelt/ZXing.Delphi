{
  * Copyright 2024 Axel Waggershauser
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

  * Ported from zxing-cpp (ODDataBarLimitedReader.cpp).
}

unit ZXing.OneD.DataBarLimitedReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.OneD.DataBarCommon,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes GS1 DataBar Limited (ISO/IEC 24724). The text is the GTIN with
  /// AI 01, like '0100000000000000'; SymbologyIdentifier ]e0.
  /// </summary>
  TDataBarLimitedReader = class(TOneDReader)
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

uses
  System.Math;

const
  CHAR_LEN = 14;
  SYMBOL_LEN = 1 + 3 * CHAR_LEN + 2;

  // the 89 check characters, as bits (bars are 1)
  CHECK_CHAR_BITS: array [0 .. 88] of string = (
    '101010101011100010', '101010101001110010', '101010101000111010', '101010100101110010', '101010100100111010',
    '101010100010111010', '101010010101110010', '101010010100111010', '101010010010111010', '101010001010111010',
    '101001010101110010', '101001010100111010', '101001010010111010', '101001001010111010', '101000101010111010',
    '100101010101110010', '100101010100111010', '100101010010111010', '100101001010111010', '100100101010111010',
    '100010101010111010', '101010101101100010', '101010101100110010', '101010101100011010', '101010100110110010',
    '101010100110011010', '101010100011011010', '101010010110110010', '101010010110011010', '101010010011011010',
    '101010001011011010', '101001010110110010', '101001010110011010', '101001010011011010', '101001001011011010',
    '101000101011011010', '100101010110110010', '100101010110011010', '100101010011011010', '100101001011011010',
    '100100101011011010', '100010101011011010', '101010101110100010', '101010101110010010', '101010100111010010',
    '101001010111010010', '100101010111010010', '101010110101100010', '101010110100110010', '101010110100011010',
    '101010110010110010', '101001011010110010', '101001011010011010', '101001011001011010', '101001001101011010',
    '101000101101011010', '100101011010110010', '100101011010011010', '100100101101011010', '101011010101100010',
    '101011010100110010', '101011010100011010', '101011010010110010', '101011010010011010', '101011001010110010',
    '100101101010110010', '100101101010011010', '100101101001011010', '100101100101011010', '100100110101011010',
    '100010110101011010', '101101010101100010', '101101010100110010', '101101010100011010', '101101010010110010',
    '101101010010011010', '101101010001011010', '101101001010110010', '101101001010011010', '101100101010110010',
    '110101010100110010', '110101010100011010', '110101010010110010', '110101010010011010', '110101010001011010',
    '110101001010011010', '110101001001011010', '110100101010011010', '110101010110010010');

var
  CheckChars: array [0 .. 88] of Integer;

procedure InitCheckChars;
begin
  for var i := 0 to High(CHECK_CHAR_BITS) do
  begin
    var v := 0;
    for var c in CHECK_CHAR_BITS[i] do
      v := (v shl 1) or (Ord(c) - Ord('0'));
    CheckChars[i] := v;
  end;
end;

function ReadDataCharacter(const view: TPatternView): TDataBarCharacter;
const
  G_SUM: array [0 .. 6] of Integer = (0, 183064, 820064, 1000776, 1491021,
    1979845, 1996939);
  T_EVEN: array [0 .. 6] of Integer = (28, 728, 6454, 203, 2408, 1, 16632);
  ODD_SUM: array [0 .. 6] of Integer = (17, 13, 9, 15, 11, 19, 7);
  ODD_WIDEST: array [0 .. 6] of Integer = (6, 5, 3, 5, 4, 8, 1);
begin
  var pattern := NormalizedPatternFromE2E(view, CHAR_LEN, 26);

  var checksum := 0;
  for var i := High(pattern) downto 0 do
    checksum := 3 * checksum + pattern[i];

  var oddPattern, evnPattern: array [0 .. 6] of Integer;
  var oddSum := 0;
  for var i := 0 to CHAR_LEN - 1 do
    if Odd(i) then
      evnPattern[i div 2] := pattern[i]
    else
    begin
      oddPattern[i div 2] := pattern[i];
      Inc(oddSum, pattern[i]);
    end;

  var group := -1;
  for var i := 0 to High(ODD_SUM) do
    if (ODD_SUM[i] = oddSum) then
    begin
      group := i;
      break;
    end;
  if (group = -1) then
    exit(TDataBarCharacter.Invalid);

  var oddWidest := ODD_WIDEST[group];
  var evnWidest := 9 - oddWidest;
  var vOdd := GetValue(oddPattern, oddWidest, false);
  var vEvn := GetValue(evnPattern, evnWidest, true);
  Result := TDataBarCharacter.Create(vOdd * T_EVEN[group] + vEvn +
    G_SUM[group], checksum);
end;

function ConstructText(const left, right: TDataBarCharacter): string;
begin
  var symVal := 2013571 * Int64(left.Value) + right.Value;
  // strip the 2D linkage flag (GS1 Composite) if any (ISO/IEC 24724:2011
  // section 6.2.3)
  if (symVal >= 2015133531096) then
    Dec(symVal, 2015133531096);
  var txt := Format('%.13d', [symVal]);
  Result := '01' + txt + GTINCheckDigit(txt);
end;

function Has26to18Ratio(v26, v18: Integer): Boolean;
begin
  Result := (v26 + 1.5 * v26 / 26 > v18 / 18 * 26) and
    (v26 - 1.5 * v26 / 26 < v18 / 18 * 26);
end;

{ TDataBarLimitedReader }

function TDataBarLimitedReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TDataBarLimitedReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TDataBarLimitedReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;

  // the symbol at the start of v: left guard bar, 3 characters (left,
  // check, right) and the right guard
  function readSymbol(const v: TPatternView): TReadResult;
  begin
    Result := nil;
    if not IsGuard(v[27], v[43]) then
      exit;
    var spaceSize := (v[27] + v[43]) div 2;
    if (not v.IsAtFirstBar and (v[-1] < spaceSize)) or
      (not v.IsAtLastBar and (v[SYMBOL_LEN] < 4 * spaceSize)) then
      exit;
    var mBar := Min(v[0], Min(v[28], v[44]));
    var maxBar := Max(v[0], Max(v[28], v[44]));
    if (maxBar > mBar * 4 div 3 + 1) then
      exit;

    var leftView := v.SubView(1 + 0 * CHAR_LEN, CHAR_LEN);
    var checkView := v.SubView(1 + 1 * CHAR_LEN, CHAR_LEN);
    var rightView := v.SubView(1 + 2 * CHAR_LEN, CHAR_LEN);
    var leftWidth := leftView.Sum;
    var checkWidth := checkView.Sum;
    var rightWidth := rightView.Sum;
    if not Has26to18Ratio(leftWidth, checkWidth) or
      not Has26to18Ratio(rightWidth, checkWidth) then
      exit;

    var modSize: Double := (leftWidth + checkWidth + rightWidth) /
      (26 + 18 + 26);
    if (not v.IsAtFirstBar and (v[-1] < modSize)) or
      (not v.IsAtLastBar and (v[SYMBOL_LEN] < 5 * modSize)) then
      exit;

    var checkCharPattern := PatternToInt
      (NormalizedPatternFromE2E(checkView, CHAR_LEN, 18));
    var checksum := -1;
    for var i := 0 to High(CheckChars) do
      if (CheckChars[i] = checkCharPattern) then
      begin
        checksum := i;
        break;
      end;
    if (checksum = -1) then
      exit;

    var left := ReadDataCharacter(leftView);
    var right := ReadDataCharacter(rightView);
    if not left.IsValid or not right.IsValid or
      ((left.Checksum + 20 * right.Checksum) mod 89 <> checksum) then
      exit;

    Result := TReadResult.Create(ConstructText(left, right), nil,
      [TResultPointHelpers.CreateResultPoint(v.PixelsInFront, rowNumber),
      TResultPointHelpers.CreateResultPoint(v.PixelsTillEnd, rowNumber)],
      TBarcodeFormat.RSS_LIMITED);
    Result.SymbologyIdentifier := ']e0';
  end;

begin
  // zxing-cpp starts at the view 2 elements in front and shifts by 2 first
  next := next.SubView(0, SYMBOL_LEN);
  var valid := next.IsValid;
  while valid do
  begin
    Result := readSymbol(next);
    if (Result <> nil) then
      exit;
    valid := next.Shift(2);
  end;

  // guarantee progress (see the loop in decodePatternRow)
  next := TPatternView.Empty;
  Result := nil;
end;

initialization

InitCheckChars;

end.
