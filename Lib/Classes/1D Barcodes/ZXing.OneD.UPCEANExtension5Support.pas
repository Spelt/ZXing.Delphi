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

  * Delphi Implementation by K. Gossens
}

unit ZXing.OneD.UPCEANExtension5Support;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  ZXing.Reader,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.BarcodeFormat,
  ZXing.DecodeHintType,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitArray,
  ZXing.Common.Detector.MathUtils;

type
  (**
   * @see TUPCEANExtension2Support
   *)
  TUPCEANExtension5Support = class sealed
  private
    function decodeMiddle(const row: IBitArray;
      const startRange: TArray<Integer>;
      const resultString: TStringBuilder): Integer;

    function extensionChecksum(const s: string): Integer;

    function determineCheckDigit(const lgPatternFound: Integer; var checkDigit: Integer): Boolean;

    /// <summary>
    /// Parses the extension string.
    /// </summary>
    /// <param name="raw">raw content of extension</param>
    /// <returns>formatted interpretation of raw content as a {@link TMap} mapping
    /// one {@link TResultMetadataType} to appropriate value, or {@code nil} if not known</returns>
    function parseExtensionString(const raw: string): TResultMetadata;

    function parseExtension5String(const raw: String): String;
  public
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const extensionStartRange: TArray<Integer>): TReadResult;
  end;

implementation

uses
  ZXing.OneD.UPCEANReader;

{ TUPCEANExtension2Support }

function TUPCEANExtension5Support.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const extensionStartRange: TArray<Integer>): TReadResult;
var
  res : TStringBuilder;
  ending : Integer;
  resultString : String;
  extensionData : TResultMetadata;
  resultPoints : TArray<IResultPoint>;
  extensionResult : TReadResult;
begin
  Result := nil;

  res := TStringBuilder.Create;
  try
    res.Length := 0;
    ending := decodeMiddle(row, extensionStartRange, res);
    if (ending < 0) then
      exit;

    resultString := res.ToString;
    extensionData := parseExtensionString(resultString);
    resultPoints := TArray<IResultPoint>.Create(
      TResultPointHelpers.CreateResultPoint((extensionStartRange[0] + extensionStartRange[1]) div 2, rowNumber),
      TResultPointHelpers.CreateResultPoint(ending, rowNumber));

    extensionResult := TReadResult.Create(resultString, nil, resultPoints, TBarcodeFormat.UPC_EAN_EXTENSION);
    if (extensionData <> nil) then
    begin
      extensionResult.putAllMetadata(extensionData);
      FreeAndNil(extensionData);
    end;
    Result := extensionResult;
  finally
    res.Free;
  end;

end;

function TUPCEANExtension5Support.decodeMiddle(const row: IBitArray;
  const startRange: TArray<Integer>; const resultString: TStringBuilder): Integer;
var
  bestMatch: Integer;
  counter: Integer;
  ending: Integer;
  counters : TArray<Integer>;
  rowOffset, x,
  lgPatternFound,
  checkDigit : Integer;
begin
  Result := -1;

  counters := [0, 0, 0, 0];
  ending := row.Size;
  rowOffset := startRange[1];

  lgPatternFound := 0;

  x := 0;
  while (((x < 5) and (rowOffset < ending))) do
  begin
    if (not TUPCEANReader.decodeDigit(row, counters, rowOffset, TUPCEANReader.L_PATTERNS + TUPCEANReader.G_PATTERNS, bestMatch)) then
      exit;

    resultString.Append(IntToStr(bestMatch mod 10));

    for counter in counters do
      Inc(rowOffset, counter);

    if (bestMatch >= 10) then
      lgPatternFound := lgPatternFound or (1 shl (4 - x));

    if (x <> 4) then
    begin
      // Read off separator if not last
      rowOffset := row.getNextSet(rowOffset);
      rowOffset := row.getNextUnset(rowOffset)
    end;
    Inc(x);
  end;

  if (resultString.Length <> 5) then
    exit;

  if (not determineCheckDigit(lgPatternFound, checkDigit)) then
    exit;

  if (extensionChecksum(resultString.ToString) <> checkDigit) then
    exit;

  Result := rowOffset;
end;

function TUPCEANExtension5Support.extensionChecksum(
  const s: String): Integer;
var
  len,
  sum, i : Integer;
begin
  len := Length(s);
  sum := 0;
  i := (len - 2);
  while ((i >= 0)) do
  begin
    Inc(sum, StrToIntDef(s.Chars[i], 0));
    Dec(i, 2);
  end;
  sum := (sum * 3);
  i := (len - 1);
  while ((i >= 0)) do
  begin
    Inc(sum, StrToIntDef(s.Chars[i], 0));
    Dec(i, 2);
  end;
  sum := (sum * 3);

  Result := (sum mod 10);
end;

function TUPCEANExtension5Support.determineCheckDigit(
  const lgPatternFound: Integer; var checkDigit: Integer): Boolean;
const
  CHECK_DIGIT_ENCODINGS: TArray<Integer> =
    [$18, $14, $12, $11, $0C,
     $06, $03, $0A, $09, $05];
var
  i: Integer;
begin
  Result := false;

  for i := 0 to Length(CHECK_DIGIT_ENCODINGS) - 1 do
  begin
    Result := lgPatternFound = CHECK_DIGIT_ENCODINGS[i];
    if Result then
    begin
      checkDigit := i;
      break;
    end;
  end;
end;

function TUPCEANExtension5Support.parseExtensionString(
  const raw: String): TResultMetadata;
var
  value: String;
  dictionary1: TResultMetadata;
begin
  Result := nil;
  if (Length(raw) <> 5) then
    exit;

  value := parseExtension5String(raw);
//
//  if (Length(value) = 0)
//  then
//     Result := nil;

  dictionary1 := TResultMetadata.Create();
  dictionary1.Add(ZXing.ResultMetadataType.SUGGESTED_PRICE, TResultMetadata.CreateStringMetadata(value));

  Result := dictionary1;
end;

function TUPCEANExtension5Support.parseExtension5String(const raw: String): String;
var
  currency,
  unitsString,
  hundredthsString : string;
  rawAmount,
  hundredths : Integer;
begin
  Result := '';

  case raw[1] of
    '0' : currency := '£';
    '5' : currency := '$';
    '9' : begin
            // Reference: http://www.jollytech.com
            if ('90000'.Equals(raw)) then
              // No suggested retail price
              exit;
            if ('99991'.Equals(raw)) then
              // Complementary
              Result := '0.00';
            if ('99990'.Equals(raw)) then
              Result := 'Used';
            // Otherwise... unknown currency?
            currency := '';
          end;
    else currency := '';
  end;

  rawAmount := Integer.Parse(raw.Substring(1));
  unitsString := ((rawAmount div 100)).ToString;
  hundredths := (rawAmount mod 100);
  if (hundredths < 10) then
     hundredthsString := '0' + hundredths.ToString
  else
     hundredthsString := hundredths.ToString;

  Result := currency + unitsString + '.' + hundredthsString;
end;

end.