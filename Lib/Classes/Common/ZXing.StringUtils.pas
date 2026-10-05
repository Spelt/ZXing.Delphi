unit ZXing.StringUtils;

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

uses SysUtils, Generics.Collections, ZXing.DecodeHintType;

type

  TStringUtils = class abstract
  const
    UTF8 = 'UTF-8';
    EUC_JP = 'EUC-JP';
    ISO88591 = 'ISO-8859-1';
    class procedure ClassInit;
    class var ASSUME_SHIFT_JIS: boolean;
    class var PLATFORM_DEFAULT_ENCODING: string;
  public
    class var GB2312: string;
    class var SHIFT_JIS: string;
    class function guessEncoding(bytes: TArray<Byte>;
      hints: TDictionary<TDecodeHintType, TObject>): string; static;
    /// <summary>The text of the bytes in the character set (as
    /// TEncoding.GetEncoding has them; also ISO-8859-10, -11, -14 and -16
    /// where the platform does not have them, and UTF-32BE and LE); false
    /// when the character set is unknown.</summary>
    class function DecodeBytes(const bytes: TArray<Byte>;
      const encodingName: string; out text: string): Boolean; static;

  end;

implementation

class procedure TStringUtils.ClassInit;
begin
  PLATFORM_DEFAULT_ENCODING := 'ISO-8859-1'; // 'ISO 8859-15';
  SHIFT_JIS := 'SJIS';
  GB2312 := 'GB2312';

  ASSUME_SHIFT_JIS := (string.Compare(TStringUtils.SHIFT_JIS,
    TStringUtils.PLATFORM_DEFAULT_ENCODING, true) = 0) or
    (string.Compare('EUC-JP', TStringUtils.PLATFORM_DEFAULT_ENCODING,
    true) = 0);

end;

class function TStringUtils.guessEncoding(bytes: TArray<Byte>;
  hints: TDictionary<TDecodeHintType, TObject>): string;

var
  characterSet: string;
  utf8BytesLeft, utf2BytesChars, utf3BytesChars, utf4BytesChars, sjisBytesLeft,
    sjisKatakanaChars, sjisCurKatakanaWordLength, sjisCurDoubleBytesWordLength,
    sjisMaxKatakanaWordLength, sjisMaxDoubleBytesWordLength, isoHighOther, len,
    i, value: Integer;
  utf8bom, canBeISO88591, canBeShiftJIS, canBeUTF8: boolean;

begin
  if ((hints <> nil) and hints.ContainsKey(ZXing.DecodeHintType.CHARACTER_SET)) then
  begin
    characterSet := string(hints[ZXing.DecodeHintType.CHARACTER_SET]);
    if (characterSet <> '') then
    begin
      Result := characterSet;
      exit
    end
  end;

  len := length(bytes);
  canBeISO88591 := true;
  canBeShiftJIS := true;
  canBeUTF8 := true;
  utf8BytesLeft := 0;
  utf2BytesChars := 0;
  utf3BytesChars := 0;
  utf4BytesChars := 0;
  sjisBytesLeft := 0;
  sjisKatakanaChars := 0;
  sjisCurKatakanaWordLength := 0;
  sjisCurDoubleBytesWordLength := 0;
  sjisMaxKatakanaWordLength := 0;
  sjisMaxDoubleBytesWordLength := 0;
  isoHighOther := 0;

  utf8bom := ((((length(bytes) > 3) and (bytes[0] = $EF)) and (bytes[1] = $BB))
    and (bytes[2] = $BF));

  i := 0;
  while (((i < len) and ((canBeISO88591 or canBeShiftJIS) or canBeUTF8))) do
  begin
    value := (bytes[i] and $FF);
    if (canBeUTF8) then
      if (utf8BytesLeft > 0) then
        if ((value and $80) = 0) then
          canBeUTF8 := false
        else
          dec(utf8BytesLeft)
      else if ((value and $80) <> 0) then
        if ((value and $40) = 0) then
          canBeUTF8 := false
        else
        begin
          inc(utf8BytesLeft);
          if ((value and $20) = 0) then
            inc(utf2BytesChars)
          else
          begin
            inc(utf8BytesLeft);
            if ((value and $10) = 0) then
              inc(utf3BytesChars)
            else
            begin
              inc(utf8BytesLeft);
              if ((value and 8) = 0) then
                inc(utf4BytesChars)
              else
                canBeUTF8 := false
            end
          end
        end;

    if (canBeISO88591) then
      if ((value > $7F) and (value < 160)) then
        canBeISO88591 := false
      else if ((value > $9F) and (((value < $C0) or (value = $D7)) or
        (value = $F7))) then
        inc(isoHighOther);

    if (canBeShiftJIS) then
      if (sjisBytesLeft > 0) then
        if (((value < $40) or (value = $7F)) or (value > $FC)) then
          canBeShiftJIS := false
        else
          dec(sjisBytesLeft)
      else if (((value = $80) or (value = 160)) or (value > $EF)) then
        canBeShiftJIS := false
      else if ((value > 160) and (value < $E0)) then
      begin
        inc(sjisKatakanaChars);
        sjisCurDoubleBytesWordLength := 0;
        inc(sjisCurKatakanaWordLength);
        if (sjisCurKatakanaWordLength > sjisMaxKatakanaWordLength) then
          sjisMaxKatakanaWordLength := sjisCurKatakanaWordLength
      end
      else if (value > $7F) then
      begin
        inc(sjisBytesLeft);
        sjisCurKatakanaWordLength := 0;
        inc(sjisCurDoubleBytesWordLength);
        if (sjisCurDoubleBytesWordLength > sjisMaxDoubleBytesWordLength) then
          sjisMaxDoubleBytesWordLength := sjisCurDoubleBytesWordLength
      end
      else
      begin
        sjisCurKatakanaWordLength := 0;
        sjisCurDoubleBytesWordLength := 0
      end;
    inc(i)
  end;

  if (canBeUTF8 and (utf8BytesLeft > 0)) then
    canBeUTF8 := false;

  if (canBeShiftJIS and (sjisBytesLeft > 0)) then
    canBeShiftJIS := false;

  if (canBeUTF8 and (utf8bom or (((utf2BytesChars + utf3BytesChars) +
    utf4BytesChars) > 0))) then
  begin
    Result := 'UTF-8';
    exit
  end;

  if (canBeShiftJIS and ((TStringUtils.ASSUME_SHIFT_JIS or
    (sjisMaxKatakanaWordLength >= 3)) or (sjisMaxDoubleBytesWordLength >= 3)))
  then
  begin
    Result := TStringUtils.SHIFT_JIS;
    exit
  end;

  if (canBeISO88591 and canBeShiftJIS) then
  begin

    if (((sjisMaxKatakanaWordLength = 2) and (sjisKatakanaChars = 2)) or
      ((isoHighOther * 10) >= len)) then
      Result := TStringUtils.SHIFT_JIS
    else
      Result := 'ISO-8859-1';
    exit
  end;

  if (canBeISO88591) then
  begin
    Result := 'ISO-8859-1';
    exit
  end;

  if (canBeShiftJIS) then
  begin
    Result := TStringUtils.SHIFT_JIS;
    exit
  end;

  if (canBeUTF8) then
  begin
    Result := 'UTF-8';
    exit
  end;

  Result := TStringUtils.PLATFORM_DEFAULT_ENCODING;

end;

const
  // the ISO-8859 character sets that not every platform has: the
  // characters of $A0 to $FF (below them as ISO-8859-1; $FFFD: none)
  ISO_8859_10: array [$A0 .. $FF] of Word = (
    $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7, $013B, $0110,
    $0160, $0166, $017D, $00AD, $016A, $014A, $00B0, $0105, $0113, $0123,
    $012B, $0129, $0137, $00B7, $013C, $0111, $0161, $0167, $017E, $2015,
    $016B, $014B, $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
    $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF, $00D0, $0145,
    $014C, $00D3, $00D4, $00D5, $00D6, $0168, $00D8, $0172, $00DA, $00DB,
    $00DC, $00DD, $00DE, $00DF, $0101, $00E1, $00E2, $00E3, $00E4, $00E5,
    $00E6, $012F, $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,
    $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169, $00F8, $0173,
    $00FA, $00FB, $00FC, $00FD, $00FE, $0138);
  ISO_8859_11: array [$A0 .. $FF] of Word = (
    $00A0, $0E01, $0E02, $0E03, $0E04, $0E05, $0E06, $0E07, $0E08, $0E09,
    $0E0A, $0E0B, $0E0C, $0E0D, $0E0E, $0E0F, $0E10, $0E11, $0E12, $0E13,
    $0E14, $0E15, $0E16, $0E17, $0E18, $0E19, $0E1A, $0E1B, $0E1C, $0E1D,
    $0E1E, $0E1F, $0E20, $0E21, $0E22, $0E23, $0E24, $0E25, $0E26, $0E27,
    $0E28, $0E29, $0E2A, $0E2B, $0E2C, $0E2D, $0E2E, $0E2F, $0E30, $0E31,
    $0E32, $0E33, $0E34, $0E35, $0E36, $0E37, $0E38, $0E39, $0E3A, $FFFD,
    $FFFD, $FFFD, $FFFD, $0E3F, $0E40, $0E41, $0E42, $0E43, $0E44, $0E45,
    $0E46, $0E47, $0E48, $0E49, $0E4A, $0E4B, $0E4C, $0E4D, $0E4E, $0E4F,
    $0E50, $0E51, $0E52, $0E53, $0E54, $0E55, $0E56, $0E57, $0E58, $0E59,
    $0E5A, $0E5B, $FFFD, $FFFD, $FFFD, $FFFD);
  ISO_8859_14: array [$A0 .. $FF] of Word = (
    $00A0, $1E02, $1E03, $00A3, $010A, $010B, $1E0A, $00A7, $1E80, $00A9,
    $1E82, $1E0B, $1EF2, $00AD, $00AE, $0178, $1E1E, $1E1F, $0120, $0121,
    $1E40, $1E41, $00B6, $1E56, $1E81, $1E57, $1E83, $1E60, $1EF3, $1E84,
    $1E85, $1E61, $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF, $0174, $00D1,
    $00D2, $00D3, $00D4, $00D5, $00D6, $1E6A, $00D8, $00D9, $00DA, $00DB,
    $00DC, $00DD, $0176, $00DF, $00E0, $00E1, $00E2, $00E3, $00E4, $00E5,
    $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0175, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $1E6B, $00F8, $00F9,
    $00FA, $00FB, $00FC, $00FD, $0177, $00FF);
  ISO_8859_16: array [$A0 .. $FF] of Word = (
    $00A0, $0104, $0105, $0141, $20AC, $201E, $0160, $00A7, $0161, $00A9,
    $0218, $00AB, $0179, $00AD, $017A, $017B, $00B0, $00B1, $010C, $0142,
    $017D, $201D, $00B6, $00B7, $017E, $010D, $0219, $00BB, $0152, $0153,
    $0178, $017C, $00C0, $00C1, $00C2, $0102, $00C4, $0106, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF, $0110, $0143,
    $00D2, $00D3, $00D4, $0150, $00D6, $015A, $0170, $00D9, $00DA, $00DB,
    $00DC, $0118, $021A, $00DF, $00E0, $00E1, $00E2, $0103, $00E4, $0107,
    $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0111, $0144, $00F2, $00F3, $00F4, $0151, $00F6, $015B, $0171, $00F9,
    $00FA, $00FB, $00FC, $0119, $021B, $00FF);

class function TStringUtils.DecodeBytes(const bytes: TArray<Byte>;
  const encodingName: string; out text: string): Boolean;
begin
  text := '';
  Result := true;
  // UTF-32 (TEncoding does not have it)
  if SameText(encodingName, 'UTF-32BE') or
    SameText(encodingName, 'UTF-32LE') then
  begin
    var bigEndian := SameText(encodingName, 'UTF-32BE');
    for var p := 0 to Length(bytes) div 4 - 1 do
    begin
      var c: Cardinal;
      if bigEndian then
        c := Cardinal(bytes[4 * p]) shl 24 or Cardinal(bytes[4 * p + 1]) shl 16
          or Cardinal(bytes[4 * p + 2]) shl 8 or bytes[4 * p + 3]
      else
        c := Cardinal(bytes[4 * p + 3]) shl 24 or
          Cardinal(bytes[4 * p + 2]) shl 16 or Cardinal(bytes[4 * p + 1]) shl 8
          or bytes[4 * p];
      if (c < $10000) then
        text := text + Char(c)
      else if (c <= $10FFFF) then
        // a surrogate pair
        text := text + Char($D800 + (c - $10000) shr 10) +
          Char($DC00 + (c - $10000) and $3FF);
    end;
    exit;
  end;
  var encoding: TEncoding := nil;
  try
    encoding := TEncoding.GetEncoding(encodingName);
  except
    encoding := nil;
  end;
  if (encoding <> nil) then
  begin
    try
      try
        text := encoding.GetString(bytes);
      except
        Result := false;
      end;
    finally
      encoding.Free;
    end;
    exit;
  end;
  // the tables of the ISO-8859 character sets the platform does not have
  var table: PWord := nil;
  if SameText(encodingName, 'ISO-8859-10') then
    table := @ISO_8859_10[$A0]
  else if SameText(encodingName, 'ISO-8859-11') then
    table := @ISO_8859_11[$A0]
  else if SameText(encodingName, 'ISO-8859-14') then
    table := @ISO_8859_14[$A0]
  else if SameText(encodingName, 'ISO-8859-16') then
    table := @ISO_8859_16[$A0];
  if (table = nil) then
    exit(false);
  SetLength(text, Length(bytes));
  for var i := 0 to High(bytes) do
    if (bytes[i] < $A0) then
      text[i + 1] := Char(bytes[i])
    else
      text[i + 1] := Char(PWord(PByte(table) + 2 * (bytes[i] - $A0))^);
end;

Initialization

TStringUtils.ClassInit;

end.
