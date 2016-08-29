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

unit ZXing.OneD.UPCEANExtension2Support;

interface

uses 
  System.SysUtils, 
  System.Generics.Collections, 
  System.Math,
  ZXing.Reader,
  ZXing.BarcodeFormat,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitArray;

type
  /// <summary>
  /// @see UPCEANExtension5Support
  /// </summary>
  TUPCEANExtension2Support = class sealed
  private
    class var
      decodeMiddleCounters : TArray<Integer>;
      decodeRowStringBuffer : TStringBuilder;

    class procedure InitializeClass; static;
    class procedure FinalizeClass; static;

    class function decodeMiddle(const row: IBitArray;
      const startRange: TArray<Integer>;
      const resultString: TStringBuilder): Integer;

    /// <summary>
    /// Parses the extension string.
    /// </summary>
    /// <param name="raw">raw content of extension</param>
    /// <returns>formatted interpretation of raw content as a {@link TMap} mapping
    class function parseExtensionString(const raw: String): TResultMetadata; static;
  public
    class function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const extensionStartRange: TArray<Integer>): TReadResult;
  end;

implementation

uses
  ZXing.OneD.UPCEANReader;

{ TUPCEANExtension2Support }

class procedure TUPCEANExtension2Support.InitializeClass;
begin
  decodeMiddleCounters := TArray<Integer>.Create(0, 0, 0, 0);
  decodeRowStringBuffer := TStringBuilder.Create;
end;

class procedure TUPCEANExtension2Support.FinalizeClass;
begin
  decodeMiddleCounters := nil;
  decodeRowStringBuffer.Free;
end;

class function TUPCEANExtension2Support.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const extensionStartRange: TArray<Integer>): TReadResult;
var
  res : TStringBuilder;
  ending : Integer;
  resultString : String;
  extensionResult : TReadResult;
  extensionData : TResultMetadata;
  resultPoints : TArray<IResultPoint>;
begin
  Result := nil;

  res := decodeRowStringBuffer;
  res.Length := 0;
  ending := decodeMiddle(row, extensionStartRange, res);
  if (ending < 0)
  then
     exit;

  resultString := Result.ToString;
  extensionData := TUPCEANExtension2Support.parseExtensionString(resultString);

  resultPoints := TArray<IResultPoint>.Create(
    TResultPointHelpers.CreateResultPoint((extensionStartRange[0] + extensionStartRange[1]) div 2, rowNumber),
    TResultPointHelpers.CreateResultPoint(ending, rowNumber));

  extensionResult := TReadResult.Create(resultString, nil, resultPoints, TBarcodeFormat.UPC_EAN_EXTENSION);
  if (extensionData <> nil)
  then
     extensionResult.putAllMetadata(extensionData);

  Result := extensionResult;
end;

class function TUPCEANExtension2Support.decodeMiddle(const row: IBitArray;
  const startRange: TArray<Integer>; const resultString: TStringBuilder): Integer;
var
  bestMatch: Integer;
  counter: Integer;
  ending: Integer;
  counters : TArray<Integer>;
  rowOffset, x,
  checkParity: Integer;
begin
  Result := -1;

  counters := decodeMiddleCounters;
  counters[0] := 0;
  counters[1] := 0;
  counters[2] := 0;
  counters[3] := 0;
  ending := row.Size;
  rowOffset := startRange[1];
  checkParity := 0;
  x := 0;
  while ((x < 2) and (rowOffset < ending)) do
  begin
    if (not TUPCEANReader.decodeDigit(row, counters, rowOffset, TUPCEANReader.L_AND_G_PATTERNS, bestMatch))
    then
       exit;

    resultString.Append('0' + IntToStr(bestMatch mod 10));

    for counter in counters do
      Inc(rowOffset, counter);

    if (bestMatch >= 10)
    then
       checkParity := checkParity or (1 shl (1 - x));

    if (x <> 1) then
    begin
      // Read off separator if not last
      rowOffset := row.getNextSet(rowOffset);
      rowOffset := row.getNextUnset(rowOffset);
    end;
    Inc(x);
  end;

  if (resultString.Length <> 2)
  then
     exit;

  if ((Integer.Parse(resultString.ToString) mod 4) <> checkParity)
  then
     exit;

  Result := rowOffset;
end;

class function TUPCEANExtension2Support.parseExtensionString(const raw: String): TResultMetadata;
var
  dictionary1: TResultMetadata;
begin
  Result := nil;

  if (Length(raw) <> 2)
  then
     exit;

  dictionary1 := TResultMetadata.Create;
  dictionary1[ZXing.ResultMetadataType.ISSUE_NUMBER] := TResultMetaData.CreateStringMetadata(raw);
  Result := dictionary1;
end;

initialization
  TUPCEANExtension2Support.InitializeClass;
finalization
  TUPCEANExtension2Support.FinalizeClass;
end.
