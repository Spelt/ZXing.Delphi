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
  DecodeHintType, ResultPoint, BarcodeFormat;

type
  TArrInt = TArray<Integer>;
  TMultiIntegerArray = TArray<TArrInt>;

type
  TITFReader = class(TOneDReader)
  const
    LARGEST_DEFAULT_ALLOWED_LENGTH = 14;
    N = 1;
    W = 3;
  private
    PATTERNS: TMultiIntegerArray;
    END_PATTERN_REVERSED: TArray<Integer>;
    MAX_AVG_VARIANCE: Integer;
    MAX_INDIVIDUAL_VARIANCE: Integer;
    DEFAULT_ALLOWED_LENGTHS: TArray<Integer>;
    START_PATTERN: TArray<Integer>;

    narrowLineWidth: Integer;

    function decodeDigit(counters: TArray<Integer>;
      out bestMatch: Integer): boolean;
    function decodeEnd(row: TBitArray): TArray<Integer>;
    function skipWhiteSpace(row: TBitArray): Integer;
    function findGuardPattern(row: TBitArray; rowOffset: Integer;
      pattern: TArray<Integer>): TArray<Integer>;
    function validateQuietZone(row: TBitArray; startPattern: Integer): boolean;
    function decodeStart(row: TBitArray): TArray<Integer>;
    function decodeMiddle(row: TBitArray; payloadStart: Integer;
      payloadEnd: Integer; out resultString: TReadResult): boolean;
  public
    constructor Create;
    function DecodeRow(rowNumber: Integer; row: TBitArray;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;
  end;

implementation

uses
  MathUtils;

{ TITFReader }

constructor TITFReader.Create;
begin
  inherited;

  narrowLineWidth := -1;
  MAX_AVG_VARIANCE :=
    Floor(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.42);
  MAX_INDIVIDUAL_VARIANCE :=
    Floor(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.78);
  END_PATTERN_REVERSED := [1, 1, 3];
  DEFAULT_ALLOWED_LENGTHS := [6, 8, 10, 12, 14];
  START_PATTERN := [1, 1, 1, 1];
  PATTERNS := [[1, 1, 3, 3, 1], [3, 1, 1, 1, 3], [1, 3, 1, 1, 3],
    [3, 3, 1, 1, 1], [1, 1, 3, 1, 3], [3, 1, 3, 1, 1], [1, 3, 3, 1, 1],
    [1, 1, 1, 3, 3], [3, 1, 1, 3, 1], [1, 3, 1, 3, 1]];
end;

function TITFReader.decodeDigit(counters: TArray<Integer>;
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

    variance := TOneDReader.patternMatchVariance(counters, pattern,
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

  endPattern := findGuardPattern(row, endStart, END_PATTERN_REVERSED);
  if (endPattern = nil) then
  begin
    row.reverse;
    begin
      Exit(nil);
    end
  end;

  if (not self.validateQuietZone(row, endPattern[0])) then
  begin
    row.reverse;
    begin
      Exit(nil);
    end
  end;

  temp := endPattern[0];
  endPattern[0] := (row.Size - endPattern[1]);
  endPattern[1] := (row.Size - temp);
  row.reverse;

  Result := endPattern;

end;

function TITFReader.decodeMiddle(row: TBitArray;
  payloadStart, payloadEnd: Integer; out resultString: TReadResult): boolean;
var
  bestMatch: Integer;
  counterDigit: Integer;
  counterDigitPair: TArray<Integer>;
  counterBlack: TArray<Integer>;
  counterWhite: TArray<Integer>;
  k: Integer;
  twoK: Integer;
  aString: string;
begin
  SetLength(counterDigitPair, 10);
  SetLength(counterBlack, 5);
  SetLength(counterWhite, 5);

  aString := '';

  while ((payloadStart < payloadEnd)) do
  begin
    if (not TOneDReader.recordPattern(row, payloadStart, counterDigitPair)) then
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

    aString := aString + IntToStr(bestMatch); // ((($30 + bestMatch) as Char));
    if (not decodeDigit(counterWhite, bestMatch)) then // @(bestMatch))) then
    begin
      Exit(false);
    end;
    aString := aString + IntToStr(bestMatch); // ((($30 + bestMatch) as Char));

    for counterDigit in counterDigitPair do
    begin
      inc(payloadStart, counterDigit);
    end
  end;

  Result := true;
  resultString := TReadResult.Create(aString, nil, nil, TBarcodeFormat.ITF);

end;

function TITFReader.DecodeRow(rowNumber: Integer; row: TBitArray;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  allowedLength: Integer;
  startRange: TArray<Integer>;
  endRange: TArray<Integer>;
  aResult: string;
  allowedLengths: TArray<Integer>;
  maxAllowedLength: Integer;
  ilength: Integer;
  lengthOK: boolean;
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

  // result := StringBuilder.Create(20);
  if (not decodeMiddle(row, startRange[1], endRange[0], Result)) then
  begin
    Exit(nil);
  end;
  aResult := Result.Text;
  allowedLengths := nil;
  maxAllowedLength := 14;
  if ((hints <> nil) and hints.ContainsKey(DecodeHintType.ALLOWED_LENGTHS)) then
  begin
    allowedLengths := TArrInt(hints[DecodeHintType.ALLOWED_LENGTHS]);
    maxAllowedLength := 0
  end;
  if (allowedLengths = nil) then
  begin
    allowedLengths := DEFAULT_ALLOWED_LENGTHS;
    maxAllowedLength := 14
  end;

  ilength := Length(aResult);
  lengthOK := (ilength > 14);
  if (not lengthOK) then
  begin
    for allowedLength in allowedLengths do
    begin
      if (ilength = allowedLength) then
      begin
        lengthOK := true;
        break;
      end;
      if (allowedLength > maxAllowedLength) then
      begin
        maxAllowedLength := allowedLength;
      end;
    end;
    if (not lengthOK and (ilength > maxAllowedLength)) then
    begin
      lengthOK := true;
    end;
    if (not lengthOK) then
    begin
      Exit(nil);
    end
  end;

  {
    resultPointCallback :=  (if ((hints = nil) or not hints.ContainsKey(DecodeHintType.NEED_RESULT_POINT_CALLBACK)) then nil else (hints.Item[DecodeHintType.NEED_RESULT_POINT_CALLBACK] as ResultPointCallback));
    if (resultPointCallback <> nil) then
    begin
    resultPointCallback.Invoke(ResultPoint.Create((startRange[1] as Single), (rowNumber as Single)));
    resultPointCallback.Invoke(ResultPoint.Create((endRange[0] as Single), (rowNumber as Single)))
    end;
    begin
    Result := Result.Create(resultString, nil, New(array[2] of ResultPoint, ( ( ResultPoint.Create((startRange[1] as Single), (rowNumber as Single)), ResultPoint.Create((endRange[0] as Single), (rowNumber as Single)) ) )), BarcodeFormat.ITF);
    exit
    end }

  Result := TReadResult.Create(aResult, nil,
    // New(array[2] of ResultPoint, ( ( ResultPoint.Create((startRange[1] as Single), (rowNumber as Single)), ResultPoint.Create((endRange[0] as Single), (rowNumber as Single)) ) )),
    nil, BarcodeFormat.ITF);

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
  // narrowLineWidth := ((startPattern[1] - startPattern[0]) shr 2);
  if (not self.validateQuietZone(row, startPattern[0])) then
  begin
    Exit(nil);
  end;

  Result := startPattern;

end;

function TITFReader.findGuardPattern(row: TBitArray; rowOffset: Integer;
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
  Result := nil;

  patternLength := Length(pattern);
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
      if (Length(counters) > counterPosition) then
        inc(counters[counterPosition]);
    end
    else
    begin
      if (counterPosition = (patternLength - 1)) then
      begin
        if (TOneDReader.patternMatchVariance(counters, pattern,
          MAX_INDIVIDUAL_VARIANCE) < MAX_AVG_VARIANCE) then
        begin
          Result := [patternStart, x];
          Exit(Result);
        end;

        if (Length(counters) > 1) then
        begin
          inc(patternStart, (counters[0] + counters[1]));
        end;

        counters := Copy(counters, 2, (patternLength - 2));

        if (Length(counters) >= patternLength - 2) then
        begin
          counters[(patternLength - 2)] := 0;
          counters[(patternLength - 1)] := 0;
        end;

        dec(counterPosition);
      end
      else
      begin
        inc(counterPosition);
      end;

      if (Length(counters) > counterPosition) then
      begin
        counters[counterPosition] := 1;
      end;

      isWhite := not isWhite
    end;
    inc(x)
  end;

end;

function TITFReader.skipWhiteSpace(row: TBitArray): Integer;
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

end.
