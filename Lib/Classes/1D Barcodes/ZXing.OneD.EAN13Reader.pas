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

  * Original Authors: dswitkin@google.com (Daniel Switkin), Sean Owen and
  *                   alasdair@google.com (Alasdair Mackintosh)
  * Delphi Implementation by K. Gossens, E. Spelt
}

unit ZXing.OneD.EAN13Reader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.OneD.UPCEANReader,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// <p>Implements decoding of the EAN-13 format.</p>
  /// </summary>
  TEAN13Reader = class(TUPCEANReader)
  private
    // For an EAN-13 barcode, the first digit is represented by the parities used
    // to encode the next six digits, according to the table below. For example,
    // if the barcode is 5 123456 789012 then the value of the first digit is
    // signified by using odd for '1', even for '2', even for '3', odd for '4',
    // odd for '5', and even for '6'. See http://en.wikipedia.org/wiki/EAN-13
    //
    // Parity of next 6 digits
    // Digit   0     1     2     3     4     5
    // 0    Odd   Odd   Odd   Odd   Odd   Odd
    // 1    Odd   Odd   Even  Odd   Even  Even
    // 2    Odd   Odd   Even  Even  Odd   Even
    // 3    Odd   Odd   Even  Even  Even  Odd
    // 4    Odd   Even  Odd   Odd   Even  Even
    // 5    Odd   Even  Even  Odd   Odd   Even
    // 6    Odd   Even  Even  Even  Odd   Odd
    // 7    Odd   Even  Odd   Even  Odd   Even
    // 8    Odd   Even  Odd   Even  Even  Odd
    // 9    Odd   Even  Even  Odd   Even  Odd
    //
    // Note that the encoding for '0' uses the same parity as a UPC barcode. Hence
    // a UPC barcode can be converted to an EAN-13 barcode by prepending a 0.
    //
    // The encoding is represented by the following array, which is a bit pattern
    // using Odd = 0 and Even = 1. For example, 5 is represented by:
    //
    // Odd Even Even Odd Odd Even
    // in binary:
    // 0    1    1   0   0    1   == 0x19
    //
  class var
    FIRST_DIGIT_ENCODINGS: TArray<Integer>;
    decodeMiddleCounters: TArray<Integer>;

    /// <summary>
    /// Based on pattern of odd-even ('L' and 'G') patterns used to encoded the explicitly-encoded
    /// digits in a barcode, determines the implicitly encoded first digit and adds it to the
    /// result string.
    /// </summary>
    /// <param name="resultString">string to insert decoded first digit into</param>
    /// <param name="lgPatternFound">int whose bits indicates the pattern of odd/even L/G patterns used to</param>
    /// encode digits
    /// <return>-1 if first digit cannot be determined</return>
    class function determineFirstDigit(const resultString: TStringBuilder;
      const lgPatternFound: Integer): Boolean; static;

    class procedure InitializeClass; static;
    class procedure FinalizeClass; static;
  protected
    public

      /// <summary>
    /// Subclasses override this to decode the portion of a barcode between the start
    /// and end guard patterns.
    /// </summary>
    /// <param name="row">row of black/white values to search</param>
    /// <param name="startRange">start/end offset of start guard pattern</param>
    /// <param name="resultString"><see cref="StringBuilder"/>to append decoded chars to</param>
    /// <returns>
    /// horizontal offset of first pixel after the "middle" that was decoded or -1 if decoding could not complete successfully
    /// </returns>
    class function DecodeMiddle(const row: IBitArray;
      const startRange: TArray<Integer>; const resultString: TStringBuilder)
      : Integer; override;



    /// <summary>
    /// Get the format of this decoder.
    /// <returns>The 1D format.</returns>
    /// </summary>
    function BarcodeFormat: TBarcodeFormat; override;
  end;

implementation

{ TEAN13Reader }

class function TEAN13Reader.DecodeMiddle(const row: IBitArray;
  const startRange: TArray<Integer>;
  const resultString: TStringBuilder): Integer;
var
  bestMatch: Integer;
  counter: Integer;
  ending: Integer;
  counters, middleRange: TArray<Integer>;
  rowOffset, x, lgPatternFound: Integer;
begin
  Result := -1;
  counters := decodeMiddleCounters;
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
    if (not decodeDigit(row, counters, rowOffset, L_AND_G_PATTERNS, bestMatch))
    then
      exit;
    resultString.Append( IntToStr ( bestMatch mod 10) );
    for counter in counters do
      Inc(rowOffset, counter);
    if (bestMatch >= 10) then
      lgPatternFound := lgPatternFound or (1 shl (5 - x));
    Inc(x);
  end;

  if (not determineFirstDigit(resultString, lgPatternFound)) then
    exit;

  middleRange := findGuardPattern(row, rowOffset, true, MIDDLE_PATTERN);
  if (middleRange = nil) then
    exit;
  rowOffset := middleRange[1];

  x := 0;
  while ((x < 6) and (rowOffset < ending)) do
  begin
    if (not decodeDigit(row, counters, rowOffset, L_PATTERNS, bestMatch)) then
      exit;
    resultString.Append(IntToStr(bestMatch));
    for counter in counters do
      Inc(rowOffset, counter);
    Inc(x);
  end;

  Result := rowOffset;
end;

function TEAN13Reader.BarcodeFormat: TBarcodeFormat;
begin
  Result := TBarcodeFormat.EAN_13;
end;

class function TEAN13Reader.determineFirstDigit(const resultString
  : TStringBuilder; const lgPatternFound: Integer): Boolean;
var
  d: Integer;
begin
  Result := false;
  for d := 0 to Pred(10) do
  begin
    if (lgPatternFound = FIRST_DIGIT_ENCODINGS[d]) then
    begin
      resultString.Insert(0,  IntToStr(d));
      Result := true;
      break;
    end;
  end;
end;

class procedure TEAN13Reader.InitializeClass;
begin
  FIRST_DIGIT_ENCODINGS := TArray<Integer>.Create($00, $0B, $0D, $0E, $13, $19,
    $1C, $15, $16, $1A);
  SetLength(decodeMiddleCounters, 4);
end;

class procedure TEAN13Reader.FinalizeClass;
begin
  FIRST_DIGIT_ENCODINGS := nil;
  decodeMiddleCounters := nil;
end;

initialization

TEAN13Reader.InitializeClass;

finalization

TEAN13Reader.FinalizeClass;

end.
