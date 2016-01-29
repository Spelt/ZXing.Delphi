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

    narrowLineWidth: integer;

    function decodeDigit(counters: TArray<Integer>; out bestMatch: Integer): boolean;
    function decodeEnd(row: TBitArray): TArray<Integer>;
    function skipWhiteSpace(row: TBitArray): Integer;
    function findGuardPattern(row: TBitArray; rowOffset: Integer; pattern: TArray<Integer>): TArray<Integer>;
    function validateQuietZone(row: TBitArray; startPattern: Integer): boolean;
    function decodeStart(row: TBitArray): TArray<Integer>;
    function decodeMiddle(row: TBitArray; payloadStart: Integer; payloadEnd: Integer; out resultString: TReadResult): boolean;
  public
    constructor Create;  override;
    function DecodeRow(rowNumber: Integer; row: TBitArray; hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;
 end;



 implementation
 uses
  MathUtils;
 
           

  {
    // Methods




    


    function ITFReader.skipWhiteSpace(row: BitArray): Integer;
    begin
        width := row.Size;
        endStart := row.getNextSet(0);
        if (endStart = width) then
            begin
                Result := -1;
                exit
            end;
        begin
            Result := endStart;
            exit
        end
    end;

     }

{ TITFReader }

constructor TITFReader.Create;
begin
  inherited;

  narrowLineWidth := -1;
  MAX_AVG_VARIANCE := Floor(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.42);
  MAX_INDIVIDUAL_VARIANCE := Floor(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.78);
  END_PATTERN_REVERSED := [1, 1, 3];
  DEFAULT_ALLOWED_LENGTHS := [6, 8, 10, 12, 14];
  START_PATTERN := [1, 1, 1, 1];
  PATTERNS :=
      [[1, 1, 3, 3, 1],
       [3, 1, 1, 1, 3],
       [1, 3, 1, 1, 3],
       [3, 3, 1, 1, 1],
       [1, 1, 3, 1, 3],
       [3, 1, 3, 1, 1],
       [1, 3, 3, 1, 1],
       [1, 1, 1, 3, 3],
       [3, 1, 1, 3, 1],
       [1, 3, 1, 3, 1]];
end;

function TITFReader.decodeDigit(counters: TArray<Integer>; out bestMatch: Integer): boolean;
var
  bestVariance: integer;
  max: integer;
  i: integer;
  pattern: TArrInt;
  variance: integer;
begin
  bestVariance := MAX_AVG_VARIANCE;
  bestMatch := -1;
  max := Length(PATTERNS);
  i := 0;
  while ((i < max)) do
  begin
    pattern := PATTERNS[i];

    variance := TOneDReader.patternMatchVariance(counters, pattern, MAX_INDIVIDUAL_VARIANCE);
    if (variance < bestVariance) then
    begin
        bestVariance := variance;
        bestMatch := i
    end;
    inc(i)
  end;

  Result := (bestMatch >= 0);
  
{function ITFReader.decodeDigit(counters: Integer[]; [Out] var bestMatch: Integer): boolean;
    begin
        bestVariance := ITFReader.MAX_AVG_VARIANCE;
        bestMatch := -1;
        max := ITFReader.PATTERNS.Length;
        i := 0;
        while ((i < max)) do
        begin
            pattern := ITFReader.PATTERNS[i];
            variance := OneDReader.patternMatchVariance(counters, pattern, ITFReader.MAX_INDIVIDUAL_VARIANCE);
            if (variance < bestVariance) then
            begin
                bestVariance := variance;
                bestMatch := i
            end;
            inc(i)
        end;
        begin
            Result := (bestMatch >= 0);
            exit
        end
    end;}
end;

function TITFReader.decodeEnd(row: TBitArray): TArray<Integer>;
var
  endStart: integer;
  endPattern: TArray<integer>;
  temp: integer;
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
{
function ITFReader.decodeEnd(row: BitArray): Integer[];
    begin
        row.reverse;
        endStart := ITFReader.skipWhiteSpace(row);
        if (endStart < 0) then
            begin
                Result := nil;
                exit
            end;
        endPattern := ITFReader.findGuardPattern(row, endStart, ITFReader.END_PATTERN_REVERSED);
        if (endPattern = nil) then
        begin
            row.reverse;
            begin
                Result := nil;
                exit
            end
        end;
        if (not self.validateQuietZone(row, endPattern[0])) then
        begin
            row.reverse;
            begin
                Result := nil;
                exit
            end
        end;
        temp := endPattern[0];
        endPattern[0] := (row.Size - endPattern[1]);
        endPattern[1] := (row.Size - temp);
        row.reverse;
        begin
            Result := endPattern;
            exit
        end
    end;}
end;

function TITFReader.decodeMiddle(row: TBitArray; payloadStart,
  payloadEnd: Integer; out resultString: TReadResult): boolean;
var
  bestMatch: Integer;
  counterDigit: Integer;
  counterDigitPair: TArray<integer>;
  counterBlack: TArray<integer>;
  counterWhite: TArray<integer>;
  k: integer;
  twoK: integer;
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
    if (not decodeDigit(counterBlack, bestMatch)) then //@(bestMatch))) then
    begin
      Exit(False);
    end;

    aString := aString + IntToStr(bestMatch); //((($30 + bestMatch) as Char));
    if (not decodeDigit(counterWhite, bestMatch)) then //@(bestMatch))) then
    begin
      Exit(False);
    end;
    aString := aString + IntToStr(bestMatch); //((($30 + bestMatch) as Char));

    for counterDigit in counterDigitPair do
    begin
      inc(payloadStart, counterDigit);
    end
  end;

  Result := true;
  resultString := TReadResult.Create(aString, nil, nil, TBarcodeFormat.ITF);
  
{function ITFReader.decodeMiddle(row: BitArray; payloadStart: Integer; payloadEnd: Integer; resultString: StringBuilder): boolean;
    var
        bestMatch: Integer;
        counterDigit: Integer;
    begin
        counterDigitPair := New(array[10] of Integer);
        counterBlack := New(array[5] of Integer);
        counterWhite := New(array[5] of Integer);
        while ((payloadStart < payloadEnd)) do
        begin
            if (not OneDReader.recordPattern(row, payloadStart, counterDigitPair)) then
                begin
                    Result := false;
                    exit
                end;
            k := 0;
            while ((k < 5)) do
            begin
                twoK := (k shl 1);
                counterBlack[k] := counterDigitPair[twoK];
                counterWhite[k] := counterDigitPair[(twoK + 1)];
                inc(k)
            end;
            if (not ITFReader.decodeDigit(counterBlack, @(bestMatch))) then
                begin
                    Result := false;
                    exit
                end;
            resultString.Append((($30 + bestMatch) as Char));
            if (not ITFReader.decodeDigit(counterWhite, @(bestMatch))) then
                begin
                    Result := false;
                    exit
                end;
            resultString.Append((($30 + bestMatch) as Char));

            for counterDigit in counterDigitPair do
            begin
                inc(payloadStart, counterDigit)
            end
        end;
        begin
            Result := true;
            exit
        end
    end;
}
end;

function TITFReader.decodeRow(rowNumber: Integer; row: TBitArray;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  allowedLength: Integer;
  startRange: TArray<integer>;
  endRange: TArray<Integer>;
  aResult: string;
  allowedLengths: TArray<Integer>;
  maxAllowedLength: integer;
  ilength: integer;
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

  //result := StringBuilder.Create(20);
  if (not decodeMiddle(row, startRange[1], endRange[0], result)) then
  begin
    Exit(nil);
  end;
  aResult := result.Text;
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
  end       }

  Result := TReadResult.Create(aResult, nil,
    //New(array[2] of ResultPoint, ( ( ResultPoint.Create((startRange[1] as Single), (rowNumber as Single)), ResultPoint.Create((endRange[0] as Single), (rowNumber as Single)) ) )),
    nil,
    BarcodeFormat.ITF);

{function ITFReader.decodeRow(rowNumber: Integer; row: BitArray; hints: IDictionary<DecodeHintType; TObject>): Result;
    var
        allowedLength: Integer;
    begin
        startRange := self.decodeStart(row);
        if (startRange = nil) then
            begin
                Result := nil;
                exit
            end;
        endRange := self.decodeEnd(row);
        if (endRange = nil) then
            begin
                Result := nil;
                exit
            end;
        result := StringBuilder.Create(20);
        if (not ITFReader.decodeMiddle(row, startRange[1], endRange[0], result)) then
            begin
                Result := nil;
                exit
            end;
        resultString := result.ToString;
        allowedLengths := nil;
        maxAllowedLength := 14;
        if ((hints <> nil) and hints.ContainsKey(DecodeHintType.ALLOWED_LENGTHS)) then
        begin
            allowedLengths := (hints.Item[DecodeHintType.ALLOWED_LENGTHS] as Integer[]);
            maxAllowedLength := 0
        end;
        if (allowedLengths = nil) then
        begin
            allowedLengths := ITFReader.DEFAULT_ALLOWED_LENGTHS;
            maxAllowedLength := 14
        end;
        length := resultString.Length;
        lengthOK := (length > 14);
        if (not lengthOK) then
        begin

            for allowedLength in allowedLengths do
            begin
                if (length = allowedLength) then
                begin
                    lengthOK := true;
                    break;

                end;
                if (allowedLength > maxAllowedLength) then
                    maxAllowedLength := allowedLength
                end;
            if (not lengthOK and (length > maxAllowedLength)) then
                lengthOK := true;
            if (not lengthOK) then
                begin
                    Result := nil;
                    exit
                end
            end;
        resultPointCallback :=  (if ((hints = nil) or not hints.ContainsKey(DecodeHintType.NEED_RESULT_POINT_CALLBACK)) then nil else (hints.Item[DecodeHintType.NEED_RESULT_POINT_CALLBACK] as ResultPointCallback));
        if (resultPointCallback <> nil) then
        begin
            resultPointCallback.Invoke(ResultPoint.Create((startRange[1] as Single), (rowNumber as Single)));
            resultPointCallback.Invoke(ResultPoint.Create((endRange[0] as Single), (rowNumber as Single)))
        end;
        begin
            Result := Result.Create(resultString, nil, New(array[2] of ResultPoint, ( ( ResultPoint.Create((startRange[1] as Single), (rowNumber as Single)), ResultPoint.Create((endRange[0] as Single), (rowNumber as Single)) ) )), BarcodeFormat.ITF);
            exit
        end
    end;}
end;

function TITFReader.decodeStart(row: TBitArray): TArray<Integer>;
var
  endStart: integer;
  startPattern: TArray<integer>;
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
  //narrowLineWidth := ((startPattern[1] - startPattern[0]) shr 2);
  if (not self.validateQuietZone(row, startPattern[0])) then
  begin
    Exit(nil);
  end;

  Result := startPattern;
 {

    function ITFReader.decodeStart(row: BitArray): Integer[];
    begin
        endStart := ITFReader.skipWhiteSpace(row);
        if (endStart < 0) then
            begin
                Result := nil;
                exit
            end;
        startPattern := ITFReader.findGuardPattern(row, endStart, ITFReader.START_PATTERN);
        if (startPattern = nil) then
            begin
                Result := nil;
                exit
            end;
        self.narrowLineWidth := ((startPattern[1] - startPattern[0]) shr 2);
        if (not self.validateQuietZone(row, startPattern[0])) then
            begin
                Result := nil;
                exit
            end;
        begin
            Result := startPattern;
            exit
        end
    end;
}
end;



function TITFReader.findGuardPattern(row: TBitArray; rowOffset: Integer;
  pattern: TArray<Integer>): TArray<Integer>;
var
  patternLength: integer;
  counters: TArray<integer>;
  width: integer;
  isWhite: boolean;
  counterPosition: integer;
  patternStart: integer;
  x: integer;
  l: integer;
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
        inc(counters[counterPosition])
    else
    begin
      if (counterPosition = (patternLength - 1)) then
      begin
        if (TOneDReader.patternMatchVariance(counters, pattern, MAX_INDIVIDUAL_VARIANCE) < MAX_AVG_VARIANCE) then
        begin
          Result := [patternStart, x];
          Exit(Result);
        end;
        inc(patternStart, (counters[0] + counters[1]));

        {counters := Copy(counters, 2, (patternLength - 2));

        SetLength(counters, patternLength);   }
        for l := 2 to patternLength - 2 do
        begin
          counters[l - 2] := counters[l];
        end;

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
    inc(x)
  end;


{
    function ITFReader.findGuardPattern(row: BitArray; rowOffset: Integer; pattern: Integer[]): Integer[];
    begin
        patternLength := pattern.Length;
        counters := New(array[patternLength] of Integer);
        width := row.Size;
        isWhite := false;
        counterPosition := 0;
        patternStart := rowOffset;
        x := rowOffset;
        while ((x < width)) do
        begin
            if (row.Item[x] xor isWhite) then
                inc(counters[counterPosition])
            else
            begin
                if (counterPosition = (patternLength - 1)) then
                begin
                    if (OneDReader.patternMatchVariance(counters, pattern, ITFReader.MAX_INDIVIDUAL_VARIANCE) < ITFReader.MAX_AVG_VARIANCE) then
                        begin
                            Result := New(array[2] of Integer, ( ( patternStart, x ) ));
                            exit
                        end;
                    inc(patternStart, (counters[0] + counters[1]));
                    Array.Copy(counters, 2, counters, 0, (patternLength - 2));
                    counters[(patternLength - 2)] := 0;
                    counters[(patternLength - 1)] := 0;
                    dec(counterPosition)
                end
                else
                    inc(counterPosition);
                counters[counterPosition] := 1;
                isWhite := not isWhite
            end;
            inc(x)
        end;
        begin
            Result := nil;
            exit
        end
    end;}
end;

function TITFReader.skipWhiteSpace(row: TBitArray): Integer;
var
  width: integer;
  endStart: integer;
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
{
 function ITFReader.skipWhiteSpace(row: BitArray): Integer;
    begin
        width := row.Size;
        endStart := row.getNextSet(0);
        if (endStart = width) then
            begin
                Result := -1;
                exit
            end;
        begin
            Result := endStart;
            exit
        end
    end;}
end;

function TITFReader.validateQuietZone(row: TBitArray; startPattern: Integer): boolean;
var
  quietCount: integer;
  i: integer;
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
    Exit(False);
  end;

  Result := true;

    {
     function ITFReader.validateQuietZone(row: BitArray; startPattern: Integer): boolean;
    begin
        quietCount := (self.narrowLineWidth * 10);
        quietCount :=   (if (quietCount < startPattern) then quietCount else startPattern);
        i := (startPattern - 1);
        while (((quietCount > 0) and (i >= 0))) do
        begin
            if (row.Item[i]) then
                break;
                ;
            dec(quietCount);
            dec(i)
        end;
        if (quietCount <> 0) then
            begin
                Result := false;
                exit
            end;
        begin
            Result := true;
            exit
        end
    end;}
end;

end.


