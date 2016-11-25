unit Code39Reader;

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
  DecodeHintType, ResultPoint, BarcodeFormat, MathUtils;

type

  TCode39Reader = class sealed(TOneDReader)

  private
    class var ALPHABET: TArray<Char>;
    class function decodeExtended(encoded: TStringBuilder): string; static;

    function findAsteriskPattern(row: TBitArray): TArray<Integer>;
    class function patternToChar(pattern: Integer; var c: Char)
      : boolean; static;
    class function toNarrowWidePattern(counters: TArray<Integer>): Integer;static;
  const
    ALPHABET_STRING
      : string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%';
    class var ASTERISK_ENCODING: Integer;

  class var
    CHARACTER_ENCODINGS: TArray<Integer>;
    counters: TArray<Integer>;
    decodeRowResult: TStringBuilder;
    usingCheckDigit : boolean;
    extendedMode : boolean;
  public
    function DecodeRow(rowNumber: Integer; row: TBitArray;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;
    constructor Create( AUsingCheckDigit, AExtendedMode:boolean);
    destructor Destroy(); override;

  end;

implementation

{ Code93Reader }

constructor TCode39Reader.Create( AUsingCheckDigit, AExtendedMode:boolean);
begin
  ALPHABET := ALPHABET_STRING.ToCharArray;
  CHARACTER_ENCODINGS := TArray<Integer>.Create($034, $121, $061, $160, $031, $130, $070, $025, $124, $064, // 0-9
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

class function TCode39Reader.decodeExtended(encoded: TStringBuilder): string;
var
  next, c: Char;
  decodedChar: Char;
  length, i: Integer;
  decoded: TStringBuilder;

label Label_0179, Label_0169, Label_014A;

begin
  length := encoded.length;
  decoded := TStringBuilder.Create(length);
  try
    i := 0;
    next := Char(0);
    decodedChar := Char(0);

    while ((i < length)) do
    begin

      c := encoded.Chars[i];

      if ((c <> '+') and (c <> '$') and
          (c <> '%') and (c <> '/')) then
        goto Label_0179;

      if (i < (length - 1)) then
      begin

        next := encoded.Chars[(i + 1)];
        decodedChar := #0;

        case c of
          '$':
            begin
              if ((next < 'A') or (next > 'Z')) then
              begin
                Result := '';
                exit
              end;
              decodedChar := Char(ord(next) - ord('@'));
              goto Label_0169
            end;
          '%':
            begin
              if ((next < 'A') or (next > 'W')) then
                break;;
              if next<='E' then
                decodedChar := Char(ord(next) - ord('&'))
              else
                decodedChar := Char(ord(next) - 11);
              goto Label_0169
            end;
          '/':
            begin
              if ((next < 'A') or (next > 'O')) then
                goto Label_014A;
              decodedChar := Char(ord(next) - ord(' '));
              goto Label_0169
            end;
          '+':
            begin
              if ((next < 'A') or (next > 'Z')) then
              begin
                Result := '';
                exit
              end;
              decodedChar := Char(ord(next) + ord(' '));
              goto Label_0169
            end;
        else
          begin
            goto Label_0169
          end;
        end;
        if ((next >= 'F') and (next <= 'W')) then
        begin
          decodedChar := Char(ord(next) - ord(''));
          goto Label_0169
        end
      end;
      begin
        Result := '';
        exit
      end;

    Label_014A:
      if (next = 'Z') then
        decodedChar := ':'
      else
      begin
        Result := '';
        exit
      end;

    Label_0169:
      decoded.Append(decodedChar);
      inc(i);
      continue;

    Label_0179:
      decoded.Append(c);
      inc(i)
    end;

    Result := decoded.ToString;

  finally
    FreeAndNil(decoded);
  end;

end;

function TCode39Reader.DecodeRow(rowNumber: Integer; row: TBitArray;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  decodedChar: Char;
  lastStart: Integer;
  index, nextStart, counter, aEnd, pattern, lastPatternSize: Integer;
  start: TArray<Integer>;
  resultString: String;
  Left, Right: Single;
  // resultPointCallback: TResultPointCallback;
  resultPoints: TArray<TResultPoint>;
  resultPointLeft, resultPointRight: TResultPoint;
  whiteSpaceAfterEnd : integer;
  i, max, total : integer;
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
      inc(nextStart, counter)
    end;
    nextStart := row.getNextSet(nextStart)

  until (decodedChar = '*');

  self.decodeRowResult.Remove((self.decodeRowResult.length - 1), 1);

  lastPatternSize := 0;

  for counter in self.counters do
  begin
    inc(lastPatternSize, counter)
  end;

  whiteSpaceAfterEnd := nextStart - lastStart - lastPatternSize;

  // TODO : fix and enable this check
  {if (nextStart <> aEnd) and (whiteSpaceAfterEnd * 2 < lastPatternSize) then
  begin
    Result := nil;
    exit
  end;}

  if (usingCheckDigit) then
  begin
    max := self.decodeRowResult.length - 1;
    total := 0;
    for i := 0 to max-1 do
    begin
      inc(total, ALPHABET_STRING.indexOf(decodeRowResult.chars[i]));
    end;
    if (self.decodeRowResult.chars[max] <> ALPHABET_STRING[total mod 43]) then
    begin
      Result := nil;
      exit
    end;
    self.decodeRowResult.Remove(max,self.decodeRowResult.Length);
  end;

  if (self.decodeRowResult.length = 0) then
  begin
    Result := nil;
    exit
  end;

{  if extendedMode then}    // TODO : only extended mode
    resultString := TCode39Reader.decodeExtended(self.decodeRowResult);
{  else
    resultString := result.toString();}

  if (resultString = '') then
  begin
    Result := nil;
    exit
  end;

  Left := (start[1] + start[0]) div 2;
  Right := (lastStart + lastPatternSize) div 2;

  resultPointLeft := TResultPoint.Create(Left, rowNumber);
  resultPointRight := TResultPoint.Create(Right, rowNumber);
  resultPoints := [resultPointLeft, resultPointRight];

  Result := TReadResult.Create(resultString, nil, resultPoints,
    BarcodeFormat.CODE_39);

end;

function TCode39Reader.findAsteriskPattern(row: TBitArray): TArray<Integer>;
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
    inc(index)
  end;

  counterPosition := 0;
  patternStart := rowOffset;
  isWhite := false;
  patternLength := length(counters);

  for i := rowOffset to width - 1 do
  begin

    if (row[i] xor isWhite) then
    begin
      inc(counters[counterPosition])
    end
    else
    begin
      if (counterPosition = (patternLength - 1)) then
      begin

        if (TCode39Reader.toNarrowWidePattern(counters) = TCode39Reader.ASTERISK_ENCODING)
        then
        begin
          Result := TArray<Integer>.Create(patternStart, i);
          exit
        end;

        inc(patternStart, (counters[0] + counters[1]));

        for l := 2 to patternLength - 2 do
        begin
          counters[l - 2] := counters[l];
        end;

        counters[(patternLength - 2)] := 0;
        counters[(patternLength - 1)] := 0;
        dec(counterPosition)

      end
      else
        inc(counterPosition);

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
    inc(i)
  end;
  c := '*';
  Result := false;
end;

class function TCode39Reader.toNarrowWidePattern(counters: TArray<Integer>): Integer;
var
  numCounters : integer;
  maxNarrowCounter : integer;
  wideCounters : integer;
  minCounter : integer;
  counter : integer;
  totalWideCountersWidth : integer;
  pattern : integer;
  i : integer;
begin
  numCounters := length(counters);
  maxNarrowCounter := 0;

  repeat
    minCounter := High(Integer);
    for  counter in counters do
    begin
      if (counter < minCounter) and(counter > maxNarrowCounter) then
        minCounter := counter;
    end;

    maxNarrowCounter := minCounter;
    wideCounters := 0;
    totalWideCountersWidth := 0;
    pattern := 0;
    for i := 0 to numCounters-1 do
    begin
        counter := counters[i];
        if (counter > maxNarrowCounter) then
        begin
          pattern := pattern or (1 shl (numCounters - 1 - i));
          inc(wideCounters);
          inc(totalWideCountersWidth, counter);
        end;
    end;
    if (wideCounters = 3) then
    begin
        // Found 3 wide counters, but are they close enough in width?
        // We can perform a cheap, conservative check to see if any individual
        // counter is more than 1.5 times the average:
        i := 0;
        while (i < numCounters) and (wideCounters > 0)do
        begin
          counter := counters[i];
          if (counter > maxNarrowCounter) then
          begin
            Dec(wideCounters);
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
