unit ITFReader;
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

  * Implemented by E. Spelt for Delphi
}

interface

uses
  SysUtils, Generics.Collections, Math, OneDReader, BitArray, ReadResult,
  DecodeHintType, ResultPoint, BarcodeFormat, Helpers;

type
  TArrInt = TArray<Integer>;
  TMultiIntegerArray = TArray<TArrInt>;

  TITFReader = class(TOneDReader)
  const
    LARGEST_DEFAULT_ALLOWED_LENGTH: Integer = 14;
    N: Integer = 1;
    W: Integer = 3;
  private

    class var PATTERNS: TMultiIntegerArray;
    class var END_PATTERN_REVERSED: TArray<Integer>;
    class var MAX_AVG_VARIANCE: Integer;
    class var MAX_INDIVIDUAL_VARIANCE: Integer;
    class var DEFAULT_ALLOWED_LENGTHS: TArray<Integer>;

  class var
    START_PATTERN: TArray<Integer>;
    narrowLineWidth: Integer;

    class function decodeDigit(counters: TArray<Integer>;
      out bestMatch: Integer): boolean; static;
    function decodeEnd(row: TBitArray): TArray<Integer>;
    class function skipWhiteSpace(row: TBitArray): Integer; static;
    class function findGuardPattern(row: TBitArray; rowOffset: Integer;
      pattern: TArray<Integer>): TArray<Integer>; static;
    function validateQuietZone(row: TBitArray; startPattern: Integer): boolean;
    function decodeStart(row: TBitArray): TArray<Integer>;
    class function decodeMiddle(row: TBitArray;
      payloadStart, payloadEnd: Integer; SBResult: TStringBuilder)
      : boolean; static;
    class procedure ClassInit();
  public
    function DecodeRow(rowNumber: Integer; row: TBitArray;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;
  end;

implementation

uses
  MathUtils;

{ TITFReader }

class procedure TITFReader.ClassInit();
begin
  MAX_AVG_VARIANCE := Floor(PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.42);
  MAX_INDIVIDUAL_VARIANCE := Floor(PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.78);

  DEFAULT_ALLOWED_LENGTHS := [6, 8, 10, 12, 14];

  START_PATTERN := [N, N, N, N];
  END_PATTERN_REVERSED := [N, N, W];

  PATTERNS := [[N, N, W, W, N], // 0
  [W, N, N, N, W], // 1
  [N, W, N, N, W], // 2
  [W, W, N, N, N], // 3
  [N, N, W, N, W], // 4
  [W, N, W, N, N], // 5
  [N, W, W, N, N], // 6
  [N, N, N, W, W], // 7
  [W, N, N, W, N], // 8
  [N, W, N, W, N]]; // 9
end;

function TITFReader.DecodeRow(rowNumber: Integer; row: TBitArray;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  allowedLength: Integer;
  startRange: TArray<Integer>;
  endRange: TArray<Integer>;
  stringResult: string;
  allowedLengths: TArray<Integer>;
  maxAllowedLength: Integer;
  len: Integer;
  lengthOK: boolean;
  SBResult: TStringBuilder;
  resultPoints: TArray<TResultPoint>;
  resultPointLeft, resultPointRight: TResultPoint;

begin
  startRange := decodeStart(row);
  if (startRange = nil) then
  begin
    Exit(nil);
  end;

  endRange := self.decodeEnd(row);
  if (endRange = nil) then
  begin
    Exit(nil);
  end;

  SBResult := TStringBuilder.Create(20);
  if (not decodeMiddle(row, startRange[1], endRange[0], SBResult)) then
  begin
    FreeAndNil(sbResult);
    Exit(nil);
  end;
  stringResult := SBResult.ToString();
  FreeAndNil(sbResult);

  allowedLengths := nil;
  maxAllowedLength := LARGEST_DEFAULT_ALLOWED_LENGTH;
  if ((hints <> nil) and hints.ContainsKey(DecodeHintType.ALLOWED_LENGTHS)) then
  begin
    allowedLengths := TArrInt(hints[DecodeHintType.ALLOWED_LENGTHS]);
    maxAllowedLength := 0
  end;

  if (allowedLengths = nil) then
  begin
    allowedLengths := DEFAULT_ALLOWED_LENGTHS;
    maxAllowedLength := LARGEST_DEFAULT_ALLOWED_LENGTH;
  end;

  len := Length(stringResult);
  lengthOK := (len > LARGEST_DEFAULT_ALLOWED_LENGTH);
  if (not lengthOK) then
  begin

    for allowedLength in allowedLengths do
    begin

      if (len = allowedLength) then
      begin
        lengthOK := true;
        break;
      end;

      if (allowedLength > maxAllowedLength) then
      begin
        maxAllowedLength := allowedLength;
      end;

    end;

    if ((not lengthOK) and (len > maxAllowedLength)) then
    begin
      lengthOK := true;
    end;

    if (not lengthOK) then
    begin
      Exit(nil);
    end

  end;

  resultPointLeft := TResultPoint.Create(startRange[1], rowNumber);
  resultPointRight := TResultPoint.Create(endRange[0], rowNumber);
  resultPoints := [resultPointLeft, resultPointRight];

  Result := TReadResult.Create(stringResult, nil, resultPoints,
    BarcodeFormat.ITF);

end;

class function TITFReader.decodeDigit(counters: TArray<Integer>;
  out bestMatch: Integer): boolean;
var
  bestVariance: Integer;
  max: Integer;
  i: Integer;
  pattern: TArrInt;
  variance: Integer;

begin
  bestVariance := MAX_AVG_VARIANCE;
  bestMatch := -1;
  max := Length(PATTERNS);
  i := 0;
  while ((i < max)) do
  begin
    pattern := PATTERNS[i];

    variance := PatternMatchVariance(counters, pattern,
      MAX_INDIVIDUAL_VARIANCE);
    if (variance < bestVariance) then
    begin
      bestVariance := variance;
      bestMatch := i
    end;

    inc(i)
  end;

  Result := (bestMatch >= 0);

end;

function TITFReader.decodeStart(row: TBitArray): TArray<Integer>;
var
  endStart: Integer;
  startPattern: TArray<Integer>;
begin
  endStart := skipWhiteSpace(row);
  if (endStart < 0) then
  begin
    Exit(nil);
  end;

  startPattern := findGuardPattern(row, endStart, START_PATTERN);
  if (startPattern = nil) then
  begin
    Exit(nil);
  end;

  narrowLineWidth := TMathUtils.Asr(startPattern[1] - startPattern[0], 2);
  if (not validateQuietZone(row, startPattern[0])) then
  begin
    Exit(nil);
  end;

  Result := startPattern;
end;

class function TITFReader.decodeMiddle(row: TBitArray;
  payloadStart, payloadEnd: Integer; SBResult: TStringBuilder): boolean;
var
  bestMatch: Integer;
  counterDigit: Integer;
  counterDigitPair: TArray<Integer>;
  counterBlack: TArray<Integer>;
  counterWhite: TArray<Integer>;
  k: Integer;
  twoK: Integer;

begin
  SetLength(counterDigitPair, 10);
  SetLength(counterBlack, 5);
  SetLength(counterWhite, 5);

  while ((payloadStart < payloadEnd)) do
  begin
    if (not RecordPattern(row, payloadStart, counterDigitPair)) then
    begin
      Exit(false);
    end;

    k := 0;
    while ((k < 5)) do
    begin
      twoK := (k shl 1);
      counterBlack[k] := counterDigitPair[twoK];
      counterWhite[k] := counterDigitPair[(twoK + 1)];
      inc(k)
    end;

    if (not decodeDigit(counterBlack, bestMatch)) then // @(bestMatch))) then
    begin
      Exit(false);
    end;
    SBResult.Append(char($30 + bestMatch));

    if (not decodeDigit(counterWhite, bestMatch)) then // @(bestMatch))) then
    begin
      Exit(false);
    end;

    SBResult.Append(char($30 + bestMatch));

    for counterDigit in counterDigitPair do
    begin
      inc(payloadStart, counterDigit);
    end
  end;

  Result := true;
end;

function TITFReader.decodeEnd(row: TBitArray): TArray<Integer>;
var
  endStart: Integer;
  endPattern: TArray<Integer>;
  temp: Integer;
begin
  row.reverse;
  endStart := skipWhiteSpace(row);
  if (endStart < 0) then
  begin
    Exit(nil);
  end;

  // !!
  endPattern := findGuardPattern(row, endStart, END_PATTERN_REVERSED);
  if (endPattern = nil) then
  begin
    row.reverse;
    Exit(nil);
  end;

  if (not validateQuietZone(row, endPattern[0])) then
  begin
    row.reverse;
    Exit(nil);
  end;

  temp := endPattern[0];
  endPattern[0] := row.Size - endPattern[1];
  endPattern[1] := row.Size - temp;
  row.reverse;

  Result := endPattern;
end;

class function TITFReader.findGuardPattern(row: TBitArray; rowOffset: Integer;
  pattern: TArray<Integer>): TArray<Integer>;
var
  patternLength: Integer;
  counters: TArray<Integer>;
  width: Integer;
  isWhite: boolean;
  counterPosition: Integer;
  patternStart: Integer;
  x: Integer;
  l: Integer;
begin
  patternLength := Length(pattern);
  counters := TArray<Integer>.Create();
  SetLength(counters, patternLength);
  width := row.Size;
  isWhite := false;

  counterPosition := 0;
  patternStart := rowOffset;
  x := rowOffset;

  while ((x < width)) do
  begin

    if (row[x] xor isWhite) then
    begin
      inc(counters[counterPosition]);
    end
    else
    begin

      if (counterPosition = (patternLength - 1)) then
      begin

        if (PatternMatchVariance(counters, pattern, MAX_INDIVIDUAL_VARIANCE) <
          MAX_AVG_VARIANCE) then
        begin
          Result := [patternStart, x];
          Exit(Result);
        end;
        inc(patternStart, (counters[0] + counters[1]));

        counters := TArray.CopyInSameArray(counters, 2, patternLength - 2);

        counters[(patternLength - 2)] := 0;
        counters[(patternLength - 1)] := 0;
        dec(counterPosition);

      end
      else
      begin
        inc(counterPosition);
      end;

      counters[counterPosition] := 1;
      isWhite := not isWhite

    end;

    inc(x);
  end;

  Result := nil;

end;

class function TITFReader.skipWhiteSpace(row: TBitArray): Integer;
var
  width: Integer;
  endStart: Integer;
begin
  width := row.Size;
  endStart := row.getNextSet(0);
  if (endStart = width) then
  begin
    Result := -1;
  end
  else
  begin
    Result := endStart;
  end

end;

function TITFReader.validateQuietZone(row: TBitArray;
  startPattern: Integer): boolean;
var
  quietCount: Integer;
  i: Integer;
begin
  quietCount := (self.narrowLineWidth * 10);
  if (quietCount >= startPattern) then
  begin
    quietCount := startPattern;
  end;

  i := (startPattern - 1);
  while (((quietCount > 0) and (i >= 0))) do
  begin
    if (row[i]) then
    begin
      break;
    end;
    dec(quietCount);
    dec(i)
  end;
  if (quietCount <> 0) then
  begin
    Exit(false);
  end;

  Result := true;

end;

initialization

TITFReader.ClassInit();

end.
