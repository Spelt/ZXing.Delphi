unit ZXing.OneD.Code39Reader;
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
  * Implemented by Nano103 and E. Spelt for Delphi
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

  TCode39Reader = class sealed(TOneDReader)

  private
    class var ALPHABET: TArray<Char>;
    class function decodeExtended(encoded: string): string; static;

    function findAsteriskPattern(row: IBitArray): TArray<Integer>;
    class function patternToChar(pattern: Integer; var c: Char)
      : boolean; static;
    class function toNarrowWidePattern(counters: TArray<Integer>)
      : Integer; static;

  const
    ALPHABET_STRING: string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%';
    class var ASTERISK_ENCODING: Integer;

  class var
    CHARACTER_ENCODINGS: TArray<Integer>;
    counters: TArray<Integer>;
    decodeRowResult: TStringBuilder;
    usingCheckDigit: boolean;
    extendedMode: boolean;
  public
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;

    constructor Create(AUsingCheckDigit, AExtendedMode: boolean);
    destructor Destroy(); override;

  end;

implementation

{ Code93Reader }

constructor TCode39Reader.Create(AUsingCheckDigit, AExtendedMode: boolean);
begin
  ALPHABET := ALPHABET_STRING.ToCharArray;
  CHARACTER_ENCODINGS := TArray<Integer>.Create($034, $121, $061, $160, $031,
    $130, $070, $025, $124, $064, // 0-9
    $109, $049, $148, $019, $118, $058, $00D, $10C, $04C, $01C, // A-J
    $103, $043, $142, $013, $112, $052, $007, $106, $046, $016, // K-T
    $181, $0C1, $1C0, $091, $190, $0D0, $085, $184, $0C4, $094, // U-*
    $0A8, $0A2, $08A, $02A // $-%
    );

  ASTERISK_ENCODING := TCode39Reader.CHARACTER_ENCODINGS[$27];

  counters := TArray<Integer>.Create();
  SetLength(counters, 9);
  decodeRowResult := TStringBuilder.Create();
  usingCheckDigit := AUsingCheckDigit;
  extendedMode := AExtendedMode;

end;

destructor TCode39Reader.Destroy;
begin
  ALPHABET := nil;
  CHARACTER_ENCODINGS := nil;
  counters := nil;
  FreeAndNil(decodeRowResult);
  inherited;
end;

class function TCode39Reader.decodeExtended(encoded: string): string;
var
  next, c, decodedChar: Char;
  length, i: Integer;
  decoded: TStringBuilder;

begin
  length := encoded.length;
  decoded := TStringBuilder.Create(length);
  try

    i := 0;
    while i < length do
    begin
      c := encoded.Chars[i];
      if (((c = '+') or (c = '$')) or ((c = '%') or (c = '/'))) then
      begin
        if ((i + 1) >= encoded.length) then
        begin
          Result := '';
          exit
        end;
        next := encoded.Chars[(i + 1)];
        decodedChar := Char(#0);
        case c of
          '+':
            begin
              if ((next >= 'A') and (next <= 'Z')) then
              begin
                decodedChar := Char(ord(next) + 32);
              end
              else
              begin
                exit('');
              end
            end;
          '$':
            begin
              if ((next >= 'A') and (next <= 'Z')) then
              begin
                decodedChar := Char(ord(next) - 64);
              end
              else
              begin
                exit('');
              end
            end;
          '%':
            begin
              if ((next >= 'A') and (next <= 'E')) then
                decodedChar := Char(ord(next) - 38)
              else if ((next >= 'F') and (next <= 'W')) then
                decodedChar := Char(ord(next) - 11)
              else
              begin
                exit('');
              end;

            end;
          '/':
            begin
              if ((next >= 'A') and (next <= 'O')) then
              begin
                decodedChar := Char(ord(next) - 32);
              end
              else if (next = 'Z') then
              begin
                decodedChar := ':';
              end
              else
                exit('');
            end;

        end;

        decoded.Append(decodedChar);
        Inc(i);
      end
      else
        decoded.Append(c);

      Inc(i);
    end; // end loop/while

    Result := decoded.ToString;
  finally
    decoded.Free;
  end;

end;

function TCode39Reader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  decodedChar: Char;
  lastStart: Integer;
  index, nextStart, counter, aEnd, pattern, lastPatternSize: Integer;
  start: TArray<Integer>;
  resultString: String;
  Left, Right: Single;
  resultPoints: TArray<IResultPoint>;
  resultPointLeft, resultPointRight: IResultPoint;
  whiteSpaceAfterEnd: Integer;
  i, max, total: Integer;
begin
  for index := 0 to length(counters) - 1 do
  begin
    counters[index] := 0;
  end;

  decodeRowResult.length := 0;

  start := self.findAsteriskPattern(row);
  if (start = nil) then
  begin
    Result := nil;
    exit
  end;
  nextStart := row.getNextSet(start[1]);
  aEnd := row.Size;
  repeat
    if (not TOneDReader.recordPattern(row, nextStart, self.counters)) then
    begin
      Result := nil;
      exit
    end;

    pattern := TCode39Reader.toNarrowWidePattern(self.counters);
    if (pattern < 0) then
    begin
      Result := nil;
      exit
    end;

    if (not TCode39Reader.patternToChar(pattern, decodedChar)) then
    begin
      Result := nil;
      exit
    end;

    self.decodeRowResult.Append(decodedChar);
    lastStart := nextStart;

    for counter in self.counters do
    begin
      Inc(nextStart, counter)
    end;

    nextStart := row.getNextSet(nextStart)

  until (decodedChar = '*');

  self.decodeRowResult.Remove((self.decodeRowResult.length - 1), 1);

  lastPatternSize := 0;

  for counter in self.counters do
  begin
    Inc(lastPatternSize, counter)
  end;

  whiteSpaceAfterEnd := nextStart - lastStart - lastPatternSize;

  if (nextStart <> aEnd) and ((whiteSpaceAfterEnd shl 1) < lastPatternSize) then
  begin
    Result := nil;
    exit
  end;

  if (usingCheckDigit) then
  begin
    max := self.decodeRowResult.length - 1;
    total := 0;
    for i := 0 to max - 1 do
    begin
      Inc(total, ALPHABET_STRING.indexOf(decodeRowResult.Chars[i]));
    end;

    if (self.decodeRowResult.Chars[max] <> ALPHABET_STRING[total mod 43]) then
    begin
      Result := nil;
      exit
    end;

    self.decodeRowResult.Remove(max, self.decodeRowResult.length);
  end;

  if (self.decodeRowResult.length = 0) then
  begin
    Result := nil;
    exit
  end;

  if extendedMode then
    resultString := TCode39Reader.decodeExtended
      (self.decodeRowResult.ToString())
  else
    resultString := decodeRowResult.ToString();

  if (resultString = '') then
  begin
    Result := nil;
    exit
  end;

  Left := (start[1] + start[0]) / 2;
  Right := lastStart + (lastPatternSize / 2);

  resultPointLeft := TResultPointHelpers.CreateResultPoint(Left, rowNumber);
  resultPointRight := TResultPointHelpers.CreateResultPoint(Right, rowNumber);
  resultPoints := [resultPointLeft, resultPointRight];

  Result := TReadResult.Create(resultString, nil, resultPoints,
    TBarcodeFormat.CODE_39);

end;

function TCode39Reader.findAsteriskPattern(row: IBitArray): TArray<Integer>;
var
  i, l, patternStart, patternLength, width, counterPosition, rowOffset,
    index: Integer;
  isWhite: boolean;

begin
  width := row.Size;
  rowOffset := row.getNextSet(0);
  index := 0;
  while (index < length(self.counters)) do
  begin
    self.counters[index] := 0;
    Inc(index)
  end;

  counterPosition := 0;
  patternStart := rowOffset;
  isWhite := False;
  patternLength := length(counters);

  for i := rowOffset to width - 1 do
  begin

    if (row[i] xor isWhite) then
    begin
      Inc(counters[counterPosition])
    end
    else
    begin
      if (counterPosition = (patternLength - 1)) then
      begin

        if (TCode39Reader.toNarrowWidePattern(counters)
          = TCode39Reader.ASTERISK_ENCODING) then
        begin
          Result := TArray<Integer>.Create(patternStart, i);
          exit
        end;

        Inc(patternStart, (counters[0] + counters[1]));

        for l := 2 to patternLength - 2 do
        begin
          counters[l - 2] := counters[l];
        end;

        counters[(patternLength - 2)] := 0;
        counters[(patternLength - 1)] := 0;
        dec(counterPosition)

      end
      else
        Inc(counterPosition);

      counters[counterPosition] := 1;
      isWhite := not isWhite
    end;
  end;

  Result := nil;

end;

class function TCode39Reader.patternToChar(pattern: Integer;
  var c: Char): boolean;
var
  i: Integer;
begin
  i := 0;
  while (i < length(TCode39Reader.CHARACTER_ENCODINGS)) do
  begin
    if (TCode39Reader.CHARACTER_ENCODINGS[i] = pattern) then
    begin
      c := TCode39Reader.ALPHABET[i];
      begin
        Result := true;
        exit
      end
    end;
    Inc(i)
  end;
  c := '*';
  Result := False;
end;

class function TCode39Reader.toNarrowWidePattern
  (counters: TArray<Integer>): Integer;
var
  numCounters: Integer;
  maxNarrowCounter: Integer;
  wideCounters: Integer;
  minCounter: Integer;
  counter: Integer;
  totalWideCountersWidth: Integer;
  pattern: Integer;
  i: Integer;
begin
  numCounters := length(counters);
  maxNarrowCounter := 0;

  repeat
    minCounter := High(Integer);
    for counter in counters do
    begin
      if (counter < minCounter) and (counter > maxNarrowCounter) then
        minCounter := counter;
    end;

    maxNarrowCounter := minCounter;
    wideCounters := 0;
    totalWideCountersWidth := 0;
    pattern := 0;
    for i := 0 to numCounters - 1 do
    begin
      counter := counters[i];
      if (counter > maxNarrowCounter) then
      begin
        pattern := pattern or (1 shl (numCounters - 1 - i));
        Inc(wideCounters);
        Inc(totalWideCountersWidth, counter);
      end;
    end;
    if (wideCounters = 3) then
    begin
      // Found 3 wide counters, but are they close enough in width?
      // We can perform a cheap, conservative check to see if any individual
      // counter is more than 1.5 times the average:
      i := 0;
      while (i < numCounters) and (wideCounters > 0) do
      begin
        counter := counters[i];
        if (counter > maxNarrowCounter) then
        begin
          dec(wideCounters);
          // totalWideCountersWidth = 3 * average, so this checks if counter >= 3/2 * average
          if ((counter * 2) >= totalWideCountersWidth) then
          begin
            Result := -1;
            exit;
          end;
        end;
        Inc(i);
      end;
      Result := pattern;
      exit;
    end;
  until (wideCounters <= 3);
  Result := -1;
end;

end.
