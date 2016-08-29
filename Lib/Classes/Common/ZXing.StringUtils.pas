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

Initialization

TStringUtils.ClassInit;

end.
