{
  * Copyright 2010 ZXing authors
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
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.OneD.Code93Reader;

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
  /// <summary>
  /// <p>Decodes Code 93 barcodes.</p>
  /// <see cref="TCode39Reader" />
  /// </summary>
  TCode93Reader = class sealed(TOneDReader)
  private
    class var ALPHABET: TArray<Char>;
    class function checkChecksums(pResult: TStringBuilder): boolean; static;
    class function checkOneChecksum(pResult: TStringBuilder;
      checkPosition, weightMax: Integer): boolean;
    class function decodeExtended(encoded: TStringBuilder): string; static;

    function findAsteriskPattern(row: IBitArray): TArray<Integer>;
    class function patternToChar(pattern: Integer; var c: Char)
      : boolean; static;
    class function toPattern(counters: TArray<Integer>): Integer; static;

  const
    ALPHABET_STRING
      : string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%abcd*';
    class var ASTERISK_ENCODING: Integer;

  class var
    CHARACTER_ENCODINGS: TArray<Integer>;
    counters: TArray<Integer>;
    decodeRowResult: TStringBuilder;

  public
    constructor Create;
    destructor Destroy; override;

    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

{ Code93Reader }

constructor TCode93Reader.Create;
begin
  ALPHABET := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%abcd*'.ToCharArray;
  CHARACTER_ENCODINGS := TArray<Integer>.Create($114, $148, $144, $142, $128,
    $124, 290, $150, $112, $10A, $1A8, 420, $1A2, $194, $192, $18A, 360, $164,
    $162, $134, $11A, $158, $14C, $146, 300, $116, $1B4, $1B2, $1AC, $1A6, $196,
    410, $16C, $166, 310, $13A, $12E, $1D4, $1D2, $1CA, $16E, $176, 430, $126,
    $1DA, 470, $132, 350);

  ASTERISK_ENCODING := TCode93Reader.CHARACTER_ENCODINGS[$2F];

  counters := TArray<Integer>.Create();
  SetLength(counters, 6);
  decodeRowResult := TStringBuilder.Create();
end;

destructor TCode93Reader.Destroy;
begin
  ALPHABET := nil;
  CHARACTER_ENCODINGS := nil;
  counters := nil;
  FreeAndNil(decodeRowResult);
  inherited;
end;

class function TCode93Reader.checkChecksums(pResult: TStringBuilder): boolean;
var
  length: Integer;
begin
  length := pResult.length;
  if (not TCode93Reader.checkOneChecksum(pResult, (length - 2), 20)) then
  begin
    Result := false;
    exit
  end;
  if (not TCode93Reader.checkOneChecksum(pResult, (length - 1), 15)) then
  begin
    Result := false;
    exit
  end;
  begin
    Result := true;
    exit
  end
end;

class function TCode93Reader.checkOneChecksum(pResult: TStringBuilder;
  checkPosition, weightMax: Integer): boolean;
var
  weight, total, i: Integer;
begin
  weight := 1;
  total := 0;
  i := (checkPosition - 1);
  while ((i >= 0)) do
  begin

    inc(total, (weight * ALPHABET_STRING.IndexOf(pResult.Chars[i])));

    inc(weight);

    if (weight > weightMax) then
      weight := 1;

    dec(i)
  end;

  if (pResult.Chars[checkPosition] <> TCode93Reader.ALPHABET[(total mod $2F)])
  then
  begin
    Result := false;
    exit
  end;
  begin
    Result := true;
    exit
  end
end;

class function TCode93Reader.decodeExtended(encoded: TStringBuilder): string;
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
    c := Char(0);
    while ((i < length)) do
    begin

      c := encoded.Chars[i];

      if ((c < 'a') or (c > 'd')) then
        goto Label_0179;

      if (i < (length - 1)) then
      begin

        next := encoded.Chars[(i + 1)];
        decodedChar := #0;

        case c of
          'a':
            begin
              if ((next < 'A') or (next > 'Z')) then
              begin
                Result := '';
                exit
              end;
              decodedChar := Char(ord(next) - ord('@'));
              goto Label_0169
            end;
          'b':
            begin
              if ((next < 'A') or (next > 'E')) then
                break;;
              decodedChar := Char(ord(next) - ord('&'));
              goto Label_0169
            end;
          'c':
            begin
              if ((next < 'A') or (next > 'O')) then
                goto Label_014A;
              decodedChar := Char(ord(next) - ord(' '));
              goto Label_0169
            end;
          'd':
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

function TCode93Reader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  decodedChar: Char;
  lastStart: Integer;
  index, nextStart, counter, aEnd, pattern, lastPatternSize: Integer;
  start: TArray<Integer>;
  resultString: String;
  Left, Right: Single;
  resultPointCallback: TResultPointCallback;
  obj: TObject;
  resultPoints: TArray<IResultPoint>;
  resultPointLeft, resultPointRight: IResultPoint;
begin
  for index := 0 to length(counters) - 1 do
    counters[index] := 0;

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
    pattern := TCode93Reader.toPattern(self.counters);
    if (pattern < 0) then
    begin
      Result := nil;
      exit
    end;
    if (not TCode93Reader.patternToChar(pattern, decodedChar)) then
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

  if (not((nextStart <> aEnd) and row[nextStart])) then
  begin
    Result := nil;
    exit
  end;

  if (self.decodeRowResult.length < 2) then
  begin
    Result := nil;
    exit
  end;

  if (not TCode93Reader.checkChecksums(self.decodeRowResult)) then
  begin
    Result := nil;
    exit
  end;

  self.decodeRowResult.length := self.decodeRowResult.length - 2;

  resultString := TCode93Reader.decodeExtended(self.decodeRowResult);
  if (resultString = '') then
  begin
    Result := nil;
    exit
  end;

  Left := (start[1] + start[0]) div 2;
  Right := (lastStart + lastPatternSize) div 2;

  resultPointLeft := TResultPointHelpers.CreateResultPoint(Left, rowNumber);
  resultPointRight := TResultPointHelpers.CreateResultPoint(Right, rowNumber);
  resultPoints := [resultPointLeft, resultPointRight];

  resultPointCallback := nil;
  // it is a local variable: it doesn't get NIL as default value, and the following ifs do not assign a value for all possible cases

  if ((hints = nil) or
    (not hints.ContainsKey(TDecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
    resultPointCallback := nil
  else
  begin
    obj := hints[TDecodeHintType.NEED_RESULT_POINT_CALLBACK];
    if (obj is TResultPointEventObject) then
      resultPointCallback := TResultPointEventObject(obj).Event;
  end;

  if Assigned(resultPointCallback) then
  begin
    resultPointCallback(resultPointLeft);
    resultPointCallback(resultPointRight);
  end;

  Result := TReadResult.Create(resultString, nil, resultPoints,
    TBarcodeFormat.CODE_39);
end;

function TCode93Reader.findAsteriskPattern(row: IBitArray): TArray<Integer>;
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

        if (TCode93Reader.toPattern(counters) = TCode93Reader.ASTERISK_ENCODING)
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

class function TCode93Reader.patternToChar(pattern: Integer;
  var c: Char): boolean;
var
  i: Integer;
begin
  i := 0;
  while (i < length(TCode93Reader.CHARACTER_ENCODINGS)) do
  begin
    if (TCode93Reader.CHARACTER_ENCODINGS[i] = pattern) then
    begin
      c := TCode93Reader.ALPHABET[i];
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

class function TCode93Reader.toPattern(counters: TArray<Integer>): Integer;
var
  counter, max, sum, pattern, i, j, scaledShifted, scaledUnshifted: Integer;
begin
  max := length(counters);
  sum := 0;

  for counter in counters do
  begin
    inc(sum, counter)
  end;

  pattern := 0;
  i := 0;
  while ((i < max)) do
  begin

    scaledShifted := (((counters[i] shl TOneDReader.INTEGER_MATH_SHIFT) *
      9) div sum);

    scaledUnshifted := TMathUtils.Asr(scaledShifted,
      TOneDReader.INTEGER_MATH_SHIFT);

    if ((scaledShifted and $FF) > $7F) then
      inc(scaledUnshifted);

    if ((scaledUnshifted < 1) or (scaledUnshifted > 4)) then
    begin
      Result := -1;
      exit
    end;

    if ((i and 1) = 0) then
    begin
      j := 0;

      while ((j < scaledUnshifted)) do
      begin
        pattern := ((pattern shl 1) or 1);
        inc(j)
      end
    end
    else
      pattern := (pattern shl scaledUnshifted);

    inc(i)
  end;

  Result := pattern;

end;

end.
