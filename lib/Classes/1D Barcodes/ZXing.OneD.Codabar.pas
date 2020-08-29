unit ZXing.OneD.Codabar;
{
  * Copyright 2008 ZXing authors
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
  * Implemented by E. Spelt for Delphi
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat,
  ZXing.Common.Detector.MathUtils;

type
  TCodaBarReader = class sealed(TOneDReader)
  private
    counterLength: Integer;
    counters: TArray<Integer>;
    decodeRowResult: TStringBuilder;

  const
    ALPHABET_STRING: string = '0123456789-$:/.+ABCD';

  const
    MIN_CHARACTER_LENGTH: Integer = 3;

    class var CHARACTER_ENCODINGS: TArray<Integer>;
    class var MAX_ACCEPTABLE: Integer;
    class var PADDING: Integer;
    class var STARTEND_ENCODING: TArray<Char>;
    class var ALPHABET: TArray<Char>;

    class function arrayContains(aArray: TArray<Char>; key: Char)
      : boolean; static;

    procedure counterAppend(e: Integer);
    function findStartPattern: Integer;
    function setCounters(row: IBitArray): boolean;
    function toNarrowWidePattern(position: Integer): Integer;
    function validatePattern(start: Integer): boolean;

  public
    constructor Create;
    destructor Destroy; override;
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

constructor TCodaBarReader.Create;
begin
  inherited;
  decodeRowResult := TStringBuilder.Create(20);
  SetLength(counters, 80);
  counterLength := 0;

  MAX_ACCEPTABLE := (TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 2);
  PADDING := Round(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 1.5);
  ALPHABET := '0123456789-$:/.+ABCD'.ToCharArray;
  CHARACTER_ENCODINGS := TArray<Integer>.Create(
        3, 6, 9, $60, $12, $42, $21, $24, $30, $48, 12, $18, $45, $51, $54, $15,
        $1a, $29, 11, 14);
  STARTEND_ENCODING := TArray<Char>.Create('A', 'B', 'C', 'D' );
end;

destructor TCodaBarReader.Destroy;
begin
  decodeRowResult.Free;
  inherited;
end;

class function TCodaBarReader.arrayContains(aArray: TArray<Char>;
  key: Char): boolean;
var
  i: Integer;
begin
  result := false;
  if (aArray <> nil) then
  begin
    i := 0;
    while i < Length(aArray) do
    begin
      if aArray[i] = key then
      begin
        result := true;
        exit
      end;
      inc(i)
    end
  end;
end;

procedure TCodaBarReader.counterAppend(e: Integer);
var
  temp: TArray<Integer>;
begin
  self.counters[self.counterLength] := e;
  inc(self.counterLength);
  if self.counterLength >= Length(self.counters) then
  begin
    SetLength(temp, self.counterLength * 2);
    Move(self.counters, temp, self.counterLength);
    self.counters := temp
  end
end;

function TCodaBarReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;

var
  index, charOffset, startOffset, nextStart, trailingWhitespace,
    lastPatternSize, i, runningCount, left, right: Integer;
  startchar, endchar: Char;

begin
  index := 0;
  while index < Length(self.counters) do
  begin
    self.counters[index] := 0;
    inc(index)
  end;

  if (not self.setCounters(row)) then
  begin
    result := nil;
    exit
  end;

  startOffset := self.findStartPattern;
  if (startOffset < 0) then
  begin
    result := nil;
    exit
  end;

  nextStart := startOffset;
  self.decodeRowResult.Length := 0;
  repeat
    charOffset := self.toNarrowWidePattern(nextStart);
    if (charOffset = -1) then
    begin
      result := nil;
      exit
    end;
    self.decodeRowResult.Append(Char(charOffset));
    inc(nextStart, 8)
  until (((self.decodeRowResult.Length > 1) and

    TCodaBarReader.arrayContains(TCodaBarReader.STARTEND_ENCODING,
    TCodaBarReader.ALPHABET[charOffset])) or (nextStart >= self.counterLength));

  trailingWhitespace := self.counters[(nextStart - 1)];
  lastPatternSize := 0;
  i := -8;
  while ((i < -1)) do
  begin
    inc(lastPatternSize, self.counters[(nextStart + i)]);
    inc(i)
  end;

  if ((nextStart < self.counterLength) and
    (trailingWhitespace < (lastPatternSize div 2))) then
  begin
    result := nil;
    exit
  end;

  if (not self.validatePattern(startOffset)) then
  begin
    result := nil;
    exit
  end;

  i := 0;
  while ((i < self.decodeRowResult.Length)) do
  begin
    // self.decodeRowResult.Chars[i] := ALPHABET[self.decodeRowResult[i]];
    inc(i)
  end;

  startchar := self.decodeRowResult.Chars[0];
  if (not TCodaBarReader.arrayContains(TCodaBarReader.STARTEND_ENCODING,
    startchar)) then
  begin
    result := nil;
    exit
  end;

  endchar := self.decodeRowResult.Chars[(self.decodeRowResult.Length - 1)];
  if (not TCodaBarReader.arrayContains(TCodaBarReader.STARTEND_ENCODING,
    endchar))
  then
  begin
    result := nil;
    exit
  end;

  if (self.decodeRowResult.Length <= 3) then
  begin
    result := nil;
    exit
  end;

  // if (not SupportClass.GetValue<boolean>(hints,
  // DecodeHintType.RETURN_CODABAR_START_END, false)) then
  // begin
  // self.decodeRowResult.Remove((self.decodeRowResult.Length - 1), 1);
  // self.decodeRowResult.Remove(0, 1)
  // end;

  runningCount := 0;
  i := 0;
  while ((i < startOffset)) do
  begin
    inc(runningCount, self.counters[i]);
    inc(i)
  end;

  left := runningCount;
  i := startOffset;
  while ((i < (nextStart - 1))) do
  begin
    inc(runningCount, self.counters[i]);
    inc(i)
  end;

  right := runningCount;

  // resultPointCallback := SupportClass.GetValue<resultPointCallback>(hints,
  // DecodeHintType.NEED_RESULT_POINT_CALLBACK, nil);
  //
  // if (resultPointCallback <> nil) then
  // begin
  // resultPointCallback.Invoke(ResultPoint.Create(left, (rowNumber as Single)));
  // resultPointCallback.Invoke(ResultPoint.Create(right, (rowNumber as Single)))
  // end;

  // Result := Result.Create(self.decodeRowResult.ToString, nil,
  // New(array [2] of ResultPoint,
  // ((ResultPoint.Create(left, (rowNumber as Single)),
  // ResultPoint.Create(right, (rowNumber as Single))))),
  // BarcodeFormat.Codabar);
  result := nil;

end;

function TCodaBarReader.findStartPattern: Integer;
var
  i, charOffset, patternSize, j: Integer;
begin
  i := 1;
  while ((i < self.counterLength)) do
  begin
    charOffset := self.toNarrowWidePattern(i);
    if ((charOffset <> -1) and TCodaBarReader.arrayContains
      (TCodaBarReader.STARTEND_ENCODING, TCodaBarReader.ALPHABET[charOffset]))
    then
    begin
      patternSize := 0;
      j := i;
      while ((j < (i + 7))) do
      begin
        inc(patternSize, self.counters[j]);
        inc(j)
      end;

      if ((i = 1) or (self.counters[(i - 1)] >= (patternSize div 2))) then
      begin
        result := i;
        exit
      end
    end;
    inc(i, 2)
  end;

  result := -1;
end;

function TCodaBarReader.setCounters(row: IBitArray): boolean;
var
  last, i, count: Integer;
  isWhite: boolean;
begin
  self.counterLength := 0;
  i := row.getNextUnset(0);
  last := row.Size;
  if (i >= last) then
  begin
    result := false;
    exit
  end;

  isWhite := true;
  count := 0;
  while ((i < last)) do
  begin
    if (row[i] xor isWhite) then
      inc(count)
    else
    begin
      self.counterAppend(count);
      count := 1;
      isWhite := not isWhite
    end;
    inc(i)
  end;

  self.counterAppend(count);
  result := true;
end;

function TCodaBarReader.toNarrowWidePattern(position: Integer): Integer;
var
  last, maxbar, minBar, j, currentCounter, thresholdBar, maxSpace, minSpace,
    thresholdSpace, bitmask, pattern, i, threshold: Integer;
  theCounters: TArray<Integer>;
begin
  last := (position + 7);
  if (last < self.counterLength) then
  begin
    theCounters := self.counters;
    maxbar := 0;
    minBar := $7FFFFFFF;
    j := position;
    while ((j < last)) do
    begin
      currentCounter := theCounters[j];
      if (currentCounter < minBar) then
        minBar := currentCounter;
      if (currentCounter > maxbar) then
        maxbar := currentCounter;
      inc(j, 2)
    end;
    thresholdBar := ((minBar + maxbar) div 2);
    maxSpace := 0;
    minSpace := $7FFFFFFF;
    j := (position + 1);
    while ((j < last)) do
    begin
      currentCounter := theCounters[j];
      if (currentCounter < minSpace) then
        minSpace := currentCounter;
      if (currentCounter > maxSpace) then
        maxSpace := currentCounter;
      inc(j, 2)
    end;
    thresholdSpace := ((minSpace + maxSpace) div 2);
    bitmask := $80;
    pattern := 0;
    i := 0;
    while ((i < 7)) do
    begin

      if (i and 1) = 0 then
        threshold := thresholdBar
      else
        threshold := thresholdSpace;

      bitmask := TMathUtils.Asr(bitmask, 1);
      if (theCounters[(position + i)] > threshold) then
        pattern := (pattern or bitmask);
      inc(i)
    end;

    i := 0;
    while i < Length(TCodaBarReader.CHARACTER_ENCODINGS) do
    begin
      if (TCodaBarReader.CHARACTER_ENCODINGS[i] = pattern) then
      begin
        result := i;
        exit
      end;
      inc(i)
    end
  end;

  result := -1;
end;

function TCodaBarReader.validatePattern(start: Integer): boolean;
var
  pos, Size, i, pattern, last, j, category: Integer;
  sizes, counts, maxes, mins: TArray<Integer>;
label Label_0117, Label_0021;

begin
  SetLength(sizes, 4);
  SetLength(counts, 4);
  last := (self.decodeRowResult.Length - 1);
  pos := start;
  i := 0;

Label_0021:

  // pattern := CHARACTER_ENCODINGS[self.decodeRowResult[i]];
  j := 6;
  while ((j >= 0)) do
  begin
    category := ((j and 1) + ((pattern and 1) * 2));
    inc(sizes[category], self.counters[(pos + j)]);
    inc(counts[category]);
    pattern := (pattern shr 1);
    dec(j)
  end;

  if (i < last) then
  begin
    inc(pos, 8);
    inc(i);
    goto Label_0021
  end;

  SetLength(maxes, 4);
  SetLength(mins, 4);

  i := 0;
  while ((i < 2)) do
  begin
    mins[i] := 0;
    mins[(i + 2)] :=
      ((((sizes[i] shl (TOneDReader.INTEGER_MATH_SHIFT and $1F)) div counts[i]) +
      ((sizes[(i + 2)] shl (TOneDReader.INTEGER_MATH_SHIFT and $1F)) div counts
      [(i + 2)])) shr 1);

    maxes[i] := mins[(i + 2)];
    maxes[(i + 2)] := (((sizes[(i + 2)] * MAX_ACCEPTABLE) +
      PADDING) div counts[(i + 2)]);
    inc(i)
  end;
  pos := start;
  i := 0;

Label_0117:
  // pattern := CHARACTER_ENCODINGS[self.decodeRowResult.Chars[i]];
  j := 6;
  while ((j >= 0)) do
  begin
    category := ((j and 1) + ((pattern and 1) * 2));
    Size := (self.counters[(pos + j)] shl INTEGER_MATH_SHIFT);
    if ((Size < mins[category]) or (Size > maxes[category])) then
    begin
      result := false;
      exit
    end;
    pattern := (pattern shr 1);
    dec(j)
  end;

  if (i < last) then
  begin
    inc(pos, 8);
    inc(i);
    goto Label_0117
  end;

  result := true;
end;

end.
