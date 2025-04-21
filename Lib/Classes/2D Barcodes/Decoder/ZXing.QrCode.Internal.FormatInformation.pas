{
  * Copyright 2007 ZXing authors
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

  * Original Author: Sean Owen
  * Ported from ZXING Java Source: www.Redivivus.in (suraj.supekar@redivivus.in)
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.FormatInformation;

interface

uses
  Generics.Collections,
  ZXing.QrCode.Internal.ErrorCorrectionLevel,
  Classes,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary> <p>Encapsulates a QR Code's format information, including the data mask used and
  /// error correction level.</p>
  ///
  /// </summary>
  /// <seealso cref="TDataMask">
  /// </seealso>
  /// <seealso cref="TErrorCorrectionLevel">
  /// </seealso>
  TFormatInformation = class sealed
  private
    FErrorCorrectionLevel: TErrorCorrectionLevel;
    FDataMask: Byte;

    class function doDecodeFormatInformation(const maskedFormatInfo1,
      maskedFormatInfo2: Integer): TFormatInformation; static;
  public
    constructor Create(const formatInfo: Integer);

    function Equals(o: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    class function numBitsDiffering(a, b: Integer): Integer; static;

    /// <summary>
    /// Decodes the format information.
    /// </summary>
    /// <param name="maskedFormatInfo1">format info indicator, with mask still applied</param>
    /// <param name="maskedFormatInfo2">The masked format info2.</param>
    /// <returns>
    /// information about the format it specifies, or <code>null</code>
    /// if doesn't seem to match any known pattern
    /// </returns>
    class function decodeFormatInformation(const maskedFormatInfo1,
      maskedFormatInfo2: Integer): TFormatInformation; static;

    property ErrorCorrectionLevel: TErrorCorrectionLevel
      read FErrorCorrectionLevel;
    property DataMask: Byte read FDataMask;
  end;

implementation

{ TFormatInformation }

constructor TFormatInformation.Create(const formatInfo: Integer);
begin
  // Bits 3,4
  FErrorCorrectionLevel := TErrorCorrectionLevel.forBits
    (TMathUtils.Asr(formatInfo, 3) and $03);
  // Bottom 3 bits
  FDataMask := Byte(formatInfo and 7);
end;

class function TFormatInformation.decodeFormatInformation(
  const maskedFormatInfo1, maskedFormatInfo2: Integer): TFormatInformation;
const
  FORMAT_INFO_MASK_QR: Integer = $5412;
var
  formatInfo: TFormatInformation;
begin
  formatInfo := doDecodeFormatInformation(maskedFormatInfo1, maskedFormatInfo2);

  if (formatInfo <> nil) then
    Result := formatInfo
  else
    // Should return null, but, some QR codes apparently
    // do not mask this info. Try again by actually masking the pattern
    // first
    Result := TFormatInformation.doDecodeFormatInformation
      ((maskedFormatInfo1 xor FORMAT_INFO_MASK_QR),
      (maskedFormatInfo2 xor FORMAT_INFO_MASK_QR));
end;

class function TFormatInformation.doDecodeFormatInformation
  (const maskedFormatInfo1, maskedFormatInfo2: Integer): TFormatInformation;
const
  /// <summary> See ISO 18004:2006, Annex C, Table C.1</summary>
  FORMAT_INFO_DECODE_LOOKUP: TArray<Integer> = [
    $5412, $5125, $5E7C, $5B4B, $45F9, $40CE, $4F97, $4AA0,
    $77C4, $72F3, $7DAA, $789D, $662F, $6318, $6C41, $6976,
    $1689, $13BE, $1CE7, $19D0, $0762, $0255, $0D0C, $083B,
    $355F, $3068, $3F31, $3A06, $24B4, $2183, $2EDA, $2BED
  ];
var
  i: Integer;
  bestDifference, bestFormatInfo, bitsDifference, targetInfo: Integer;
begin
  Result := nil;

  // Find the int in FORMAT_INFO_DECODE_LOOKUP with fewest bits differing
  bestDifference := High(Integer);
  bestFormatInfo := 0;

  for i := 0 to Length(FORMAT_INFO_DECODE_LOOKUP) - 1 do
  begin
    targetInfo := FORMAT_INFO_DECODE_LOOKUP[i];
    if ((targetInfo = maskedFormatInfo1) or (targetInfo = maskedFormatInfo2))
    then
    begin
      // Found an exact match
      Result := TFormatInformation.Create(i);
      exit;
    end;

    bitsDifference := numBitsDiffering(maskedFormatInfo1, targetInfo);
    if (bitsDifference < bestDifference) then
    begin
      bestFormatInfo := i;
      bestDifference := bitsDifference;
    end;

    if (maskedFormatInfo1 <> maskedFormatInfo2) then
    begin
      // also try the other option
      bitsDifference := numBitsDiffering(maskedFormatInfo2, targetInfo);
      if (bitsDifference < bestDifference) then
      begin
        bestFormatInfo := i;
        bestDifference := bitsDifference;
      end;
    end;
  end;
  // Hamming distance of the 32 masked codes is 7, by construction, so <= 3 bits
  // differing means we found a match
  if (bestDifference <= 3) then
  begin
    Result := TFormatInformation.Create(bestFormatInfo);
    exit;
  end;
end;

function TFormatInformation.Equals(o: TObject): Boolean;
var
  other: TFormatInformation;
begin
  Result := false;
  if (not(o is TFormatInformation)) then
    exit;

  other := TFormatInformation(o);
  Result := ((FErrorCorrectionLevel = other.ErrorCorrectionLevel) and
    (FDataMask = other.DataMask));
end;

function TFormatInformation.GetHashCode: Integer;
begin
  Result := ((FErrorCorrectionLevel.ordinal shl 3) or FDataMask)
end;

class function TFormatInformation.numBitsDiffering(a, b: Integer): Integer;
const
  /// <summary> Offset i holds the number of 1 bits in the binary representation of i</summary>
  BITS_SET_IN_HALF_BYTE: TArray<Integer> = [0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4];
begin
  a := (a xor b); // a now has a 1 bit exactly where its bit differs with b's
  // Count bits set quickly with a series of lookups:
  Result := BITS_SET_IN_HALF_BYTE[(a and $0F)] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 4) and $0F] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 8) and $0F] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 12) and $0F] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 16) and $0F] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 20) and $0F] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 24) and $0F] +
            BITS_SET_IN_HALF_BYTE[TMathUtils.Asr(UInt32(a), 28) and $0F];
end;

end.
