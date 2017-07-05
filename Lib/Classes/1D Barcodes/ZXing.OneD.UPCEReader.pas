unit ZXing.OneD.UPCEReader;
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
  * Delphi Implementation by E. Spelt
}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat,
  ZXing.Helpers,
  ZXing.OneD.EAN13Reader,
  ZXing.OneD.UPCEANReader;

type

  T2DIntArray = TArray<TArray<Integer>>;

  TUPCEReader = class(TUPCEANReader)
  private
    class var decodeMiddleCounters: TArray<Integer>;
    class var MIDDLE_END_PATTERN: TArray<Integer>;
    class var NUMSYS_AND_CHECK_DIGIT_PATTERNS: T2DIntArray;

    class procedure DoInitialize();
    class procedure DoFinalize();
    class function determineNumSysAndCheckDigit(resultString: TStringBuilder;
      lgPatternFound: Integer): boolean; static;

  protected

    class function checkChecksum(const s: string): boolean; override;
    class function decodeEnd(const row: IBitArray; const endStart: Integer)
      : TArray<Integer>; override;

  public
    class function DecodeMiddle(const row: IBitArray;
      const startRange: TArray<Integer>; const res: TStringBuilder)
      : Integer; override;

  public
    class function convertUPCEtoUPCA(upce: string): string; static;

    function BarcodeFormat: TBarcodeFormat; override;
  end;

implementation

{ TUPCEReader }

class function TUPCEReader.determineNumSysAndCheckDigit
  (resultString: TStringBuilder; lgPatternFound: Integer): boolean;
var
  numSys, d: Integer;
begin
  numSys := 0;
  while ((numSys <= 1)) do
  begin
    d := 0;
    while ((d < 10)) do
    begin
      if (lgPatternFound = TUPCEReader.NUMSYS_AND_CHECK_DIGIT_PATTERNS
        [numSys][d]) then
      begin
        resultString.Insert(0, Char($30 + numSys) );
        resultString.Append(char($30 + d));
        begin
          Result := true;
          exit
        end
      end;
      inc(d)
    end;
    inc(numSys)
  end;

  Result := false;
end;

class function TUPCEReader.checkChecksum(const s: string): boolean;
begin
  Result := inherited checkChecksum(TUPCEReader.convertUPCEtoUPCA(s))
end;

class function TUPCEReader.convertUPCEtoUPCA(upce: string): string;
var
  upceChars: string;
  res: TStringBuilder;
  lastChar: char;
begin
  upceChars := upce.Substring(1, 6);
  res := TStringBuilder.Create(12);
  res.Append(upce.Chars[0]);
  lastChar := upceChars.Chars[5];
  case lastChar of
    '0', '1', '2':
      begin
        res.Append(upceChars, 0, 2);
        res.Append(lastChar);
        res.Append('0000');
        res.Append(upceChars, 2, 3);
      end;
    '3':
      begin
        res.Append(upceChars, 0, 3);
        res.Append('00000');
        res.Append(upceChars, 3, 2);
      end;
    '4':
      begin
        res.Append(upceChars, 0, 4);
        res.Append('00000');
        res.Append(upceChars.Chars[4]);
      end;
  else
    begin
      res.Append(upceChars, 0, 5);
      res.Append('0000');
      res.Append(lastChar);
    end;
  end;

  res.Append(upce.Chars[7]);
  Result := res.ToString;
  res.Free;
end;

class function TUPCEReader.decodeEnd(const row: IBitArray;
  const endStart: Integer): TArray<Integer>;
begin
  Result := TUPCEANReader.findGuardPattern(row, endStart, true,
    TUPCEReader.MIDDLE_END_PATTERN);
end;

class function TUPCEReader.DecodeMiddle(const row: IBitArray;
  const startRange: TArray<Integer>; const res: TStringBuilder): Integer;
var
  bestMatch: Integer;
  counter, ending, lgPatternFound, rowOffset, x: Integer;
  counters: TArray<Integer>;
begin

  counters := self.decodeMiddleCounters;
  counters[0] := 0;
  counters[1] := 0;
  counters[2] := 0;
  counters[3] := 0;
  ending := row.Size;
  rowOffset := startRange[1];
  lgPatternFound := 0;
  x := 0;

  while ((x < 6) and (rowOffset < ending)) do
  begin
    if (not TUPCEANReader.decodeDigit(row, counters, rowOffset,
      TUPCEANReader.L_AND_G_PATTERNS, bestMatch)) then
    begin
      exit(-1);
    end;

    res.Append(Char(ord('0') + bestMatch mod 10));

    for counter in counters do
    begin
      inc(rowOffset, counter)
    end;

    if (bestMatch >= 10) then
      lgPatternFound := lgPatternFound or (1 shl (5 - x));

    inc(x)
  end;

  if (not TUPCEReader.determineNumSysAndCheckDigit(res, lgPatternFound)) then
  begin
    exit(-1);
  end;

  Result := rowOffset;
end;

function TUPCEReader.BarcodeFormat: TBarcodeFormat;
begin
  Result := TBarcodeFormat.UPC_E;
end;

class procedure TUPCEReader.DoInitialize;
begin
  MIDDLE_END_PATTERN := TArray<Integer>.Create(1, 1, 1, 1, 1, 1);
  NUMSYS_AND_CHECK_DIGIT_PATTERNS :=
    T2DIntArray.Create(TArray<Integer>.Create($38, $34, 50, $31, $2C, $26, $23,
    $2A, $29, $25), TArray<Integer>.Create(7, 11, 13, 14, $13, $19, $1C, $15,
    $16, $1A));
  decodeMiddleCounters := TArray<Integer>.Create(0,0,0,0);
end;

class procedure TUPCEReader.DoFinalize;
begin
  MIDDLE_END_PATTERN := nil;
  NUMSYS_AND_CHECK_DIGIT_PATTERNS := nil;
  decodeMiddleCounters := nil;
end;

initialization

TUPCEReader.DoInitialize;

finalization

TUPCEReader.DoFinalize;

end.
