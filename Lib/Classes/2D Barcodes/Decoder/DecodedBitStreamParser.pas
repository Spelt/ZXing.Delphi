unit DecodedBitStreamParser;

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

uses BitSource, SysUtils, DecodeHintType, Generics.Collections,
  ErrorCorrectionLevel, version, DecoderResult, CharacterSetECI, StringUtils,
  Mode, MathUtils;

type

  TDecodedBitStreamParser = class abstract

  private

    class var ALPHANUMERIC_CHARS: TArray<Char>;
    class procedure InitializeClass;

  const
    GB2312_SUBSET: Integer = 1;

    class function decodeAlphanumericSegment(bits: TBitSource;
      res: TStringBuilder; count: Integer; fc1InEffect: boolean)
      : boolean; static;

    class function decodeByteSegment(bits: TBitSource; res: TStringBuilder;
      count: Integer; currentCharacterSetECI: TCharacterSetECI;
      byteSegments: TList<TArray<Byte>>;
      hints: TDictionary<TDecodeHintType, TObject>): boolean; static;

    class function decodeHanziSegment(bits: TBitSource; res: TStringBuilder;
      count: Integer): boolean; static;

    class function decodeKanjiSegment(bits: TBitSource; res: TStringBuilder;
      count: Integer): boolean; static;

    class function decodeNumericSegment(bits: TBitSource; res: TStringBuilder;
      count: Integer): boolean; static;
    class function parseECIValue(bits: TBitSource): Integer; static;
    class function toAlphaNumericChar(value: Integer): Char; static;

  public
    class function decode(bytes: TArray<Byte>; version: TVersion;
      ecLevel: TErrorCorrectionLevel;
      hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult; static;

  end;

implementation

{ TDecodedBitStreamParser }

class procedure TDecodedBitStreamParser.InitializeClass;
begin
  ALPHANUMERIC_CHARS := TArray<Char>.Create('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ' ',
    '$', '%', '*', '+', '-', '.', '/', ':');
end;

class function TDecodedBitStreamParser.decode(bytes: TArray<Byte>;
  version: TVersion; ecLevel: TErrorCorrectionLevel;
  hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
var
  Mode: TMode;
  bits: TBitSource;
  byteSegments: TList<TArray<Byte>>;
  fc1InEffect: boolean;
  symbolSequence, parityData, count, countHanzi, subSet: Integer;
  currentCharacterSetECI: TCharacterSetECI;
  ecstring: String;
  res: TStringBuilder;

begin
  bits := TBitSource.Create(bytes);
  res := TStringBuilder.Create(50);
  byteSegments := TList < TArray < Byte >>.Create();
  byteSegments.Capacity := 1;
  symbolSequence := -1;
  parityData := -1;
  Mode := nil;

  try
    currentCharacterSetECI := nil;
    fc1InEffect := false;
    repeat

      if (bits.available < 4) then
      begin
        Mode := TMode.TERMINATOR;
      end
      else
      begin

        try
          Mode := TMode.forBits(bits.readBits(4))
        except
          on exception1: EArgumentException do
          begin
            result := nil;
            exit;
          end;
        end;

      end;

      if (Mode <> TMode.TERMINATOR) then
      begin

        if ((Mode = TMode.FNC1_FIRST_POSITION) or
          (Mode = TMode.FNC1_SECOND_POSITION)) then
        begin
          fc1InEffect := true;
        end
        else if (Mode = TMode.STRUCTURED_APPEND) then
        begin
          if (bits.available() < 16) then
          begin
            result := nil;
            exit
          end;

          symbolSequence := bits.readBits(8);
          parityData := bits.readBits(8);
        end
        else if (Mode = TMode.ECI) then
        begin
          currentCharacterSetECI := TCharacterSetECI.getCharacterSetECIByValue
            (TDecodedBitStreamParser.parseECIValue(bits));

          if (currentCharacterSetECI = nil) then
          begin
            result := nil;
            exit
          end

        end
        else
        begin
          if (Mode = TMode.HANZI) then
          begin
            subSet := bits.readBits(4);
            countHanzi := bits.readBits(Mode.getCharacterCountBits(version));
            if (subSet = 1) then
            begin
              if (not TDecodedBitStreamParser.decodeHanziSegment(bits, res,
                countHanzi)) then
              begin
                result := nil;
                exit
              end
            end;
          end
          else
          begin
            count := bits.readBits(Mode.getCharacterCountBits(version));
            if (Mode = TMode.NUMERIC) then
            begin
              if (not TDecodedBitStreamParser.decodeNumericSegment(bits, res,
                count)) then
              begin
                result := nil;
                exit
              end;
            end
            else if (Mode = TMode.ALPHANUMERIC) then
            begin
              if (not TDecodedBitStreamParser.decodeAlphanumericSegment(bits,
                res, count, fc1InEffect)) then
              begin
                result := nil;
                exit
              end;
            end
            else if (Mode = TMode.BYTE) then
            begin
              if (not TDecodedBitStreamParser.decodeByteSegment(bits, res,
                count, currentCharacterSetECI, byteSegments, hints)) then
              begin
                result := nil;
                exit;
              end
            end
            else if (Mode <> TMode.KANJI) then
            begin
              if (not TDecodedBitStreamParser.decodeKanjiSegment(bits, res,
                count)) then
              begin
                result := nil;
                exit
              end
            end
            else
            begin
              result := nil;
              exit
            end
          end
        end
      end
      until (Mode = TMode.TERMINATOR)

    except
      on exception2: EArgumentException do
      begin
        result := nil;
        exit
      end
    end;

    if (byteSegments.count = 0) then
      byteSegments := nil;

    if (ecLevel = nil) then
      ecstring := ''
    else
      ecstring := ecLevel.toString();

    result := TDecoderResult.Create(bytes, res.toString.Replace('#13#10', '#10')
      .Replace('#10', #13), byteSegments, ecstring, symbolSequence, parityData);

  end;

  class function TDecodedBitStreamParser.decodeAlphanumericSegment
    (bits: TBitSource; res: TStringBuilder; count: Integer;
    fc1InEffect: boolean): boolean;
  var
    start, i, nextTwoCharsBits: Integer;
    charArray: TArray<Char>;
  begin

    start := res.Length;
    while ((count > 1)) do
    begin
      if (bits.available < 11) then
      begin
        result := false;
        exit
      end;

      nextTwoCharsBits := bits.readBits(11);
      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        ((nextTwoCharsBits div $2D)));
      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        ((nextTwoCharsBits mod $2D)));
      dec(count, 2)

    end;

    if (count = 1) then
    begin
      if (bits.available < 6) then
      begin
        result := false;
        exit
      end;

      res.Append(TDecodedBitStreamParser.toAlphaNumericChar(bits.readBits(6)))

    end;

    if (fc1InEffect) then
    begin

      i := start;
      while ((i < res.Length)) do
      begin
        if (res.Chars[i] = '%') then
        begin
          if ((i < (res.Length - 1)) and (res.Chars[(i + 1)] = '%')) then
            res.Remove((i + 1), 1)
          else
          begin
            res.Remove(i, 1);
            charArray := TArray<Char>.Create(' ');
            res.Insert(i, charArray);
          end;
        end;
        inc(i)
      end;

    end;

    result := true;

  end;

  class function TDecodedBitStreamParser.decodeByteSegment(bits: TBitSource;
    res: TStringBuilder; count: Integer;
    currentCharacterSetECI: TCharacterSetECI; byteSegments: TList<TArray<Byte>>;
    hints: TDictionary<TDecodeHintType, TObject>): boolean;
  var
    encoding: string;
    readBytes: TArray<Byte>;
    i: Integer;

  begin

    if ((count shl 3) > bits.available) then
    begin
      result := false;
      exit
    end;

    readBytes := TArray<Byte>.Create();
    SetLength(readBytes, count);
    i := 0;
    while ((i < count)) do
    begin
      readBytes[i] := Byte(bits.readBits(8));
      inc(i)
    end;

    if (currentCharacterSetECI = nil) then
      encoding := TStringUtils.guessEncoding(readBytes, hints)
    else
      encoding := currentCharacterSetECI.EncodingName;

    try
      res.Append(Tencoding.GetEncoding(encoding).GetString(readBytes, 0,
        Length(readBytes)))
    except
      on exception1: Exception do
      begin
        result := false;
        exit
      end
    end;

    byteSegments.Add(readBytes);
    result := true;
  end;

  class function TDecodedBitStreamParser.decodeHanziSegment(bits: TBitSource;
    res: TStringBuilder; count: Integer): boolean;
  var
    buffer: TArray<Byte>;
    offset, twoBytes, assembledTwoBytes: Integer;

  begin

    if ((count * 13) > bits.available) then
    begin
      result := false;
      exit
    end;

    buffer := TArray<Byte>.Create();
    SetLength(buffer, 2 * count);
    offset := 0;

    while ((count > 0)) do
    begin
      twoBytes := bits.readBits(13);
      assembledTwoBytes := (((twoBytes div $60) shl 8) or (twoBytes mod $60));

      if (assembledTwoBytes < $3BF) then
        inc(assembledTwoBytes, $A1A1)
      else
        inc(assembledTwoBytes, $A6A1);

      buffer[offset] := Byte(TMathUtils.Asr(assembledTwoBytes, 8) and $FF);
      buffer[(offset + 1)] := Byte(assembledTwoBytes and $FF);
      inc(offset, 2);
      dec(count)
    end;

    try
      res.Append(Tencoding.GetEncoding(TStringUtils.GB2312).GetString(buffer, 0,
        Length(buffer)))
    except
      on exception1: Exception do
      begin
        result := false;
        exit
      end
    end;

    result := true;
  end;

  class function TDecodedBitStreamParser.decodeKanjiSegment(bits: TBitSource;
    res: TStringBuilder; count: Integer): boolean;
  var
    buffer: TArray<Byte>;
    twoBytes, offset, assembledTwoBytes: Integer;

  begin

    if ((count * 13) > bits.available) then
    begin
      result := false;
      exit
    end;

    buffer := TArray<Byte>.Create();
    SetLength(buffer, 2 * count);

    offset := 0;

    while ((count > 0)) do
    begin
      twoBytes := bits.readBits(13);
      assembledTwoBytes := (((twoBytes div $C0) shl 8) or (twoBytes mod $C0));
      if (assembledTwoBytes < $1F00) then
        inc(assembledTwoBytes, $8140)
      else
        inc(assembledTwoBytes, $C140);
      buffer[offset] := Byte(TMathUtils.Asr(assembledTwoBytes, 8));
      buffer[(offset + 1)] := Byte(assembledTwoBytes);
      inc(offset, 2);
      dec(count)
    end;

    try
      res.Append(Tencoding.GetEncoding(TStringUtils.SHIFT_JIS).GetString(buffer,
        0, Length(buffer)))
    except
      on exception1: Exception do
      begin
        result := false;
        exit
      end
    end;

    result := true;

  end;

  class function TDecodedBitStreamParser.decodeNumericSegment(bits: TBitSource;
    res: TStringBuilder; count: Integer): boolean;
  var
    threeDigitsBits, twoDigitsBits, digitBits: Integer;
  begin

    while ((count >= 3)) do
    begin

      if (bits.available < 10) then
      begin
        result := false;
        exit
      end;

      threeDigitsBits := bits.readBits(10);
      if (threeDigitsBits >= $3E8) then
      begin
        result := false;
        exit
      end;

      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        ((threeDigitsBits div 100)));
      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        (((threeDigitsBits div 10) mod 10)));
      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        ((threeDigitsBits mod 10)));

      dec(count, 3)
    end;

    if (count = 2) then
    begin
      if (bits.available < 7) then
      begin
        result := false;
        exit
      end;

      twoDigitsBits := bits.readBits(7);
      if (twoDigitsBits >= 100) then
      begin
        result := false;
        exit
      end;

      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        ((twoDigitsBits div 10)));

      res.Append(TDecodedBitStreamParser.toAlphaNumericChar
        ((twoDigitsBits mod 10)))

    end
    else if (count = 1) then
    begin

      if (bits.available < 4) then
      begin
        result := false;
        exit
      end;

      digitBits := bits.readBits(4);
      if (digitBits >= 10) then
      begin
        result := false;
        exit
      end;

      res.Append(TDecodedBitStreamParser.toAlphaNumericChar(digitBits))
    end;

    result := true;

  end;

  class function TDecodedBitStreamParser.parseECIValue
    (bits: TBitSource): Integer;
  var
    firstByte, secondByte, SecondThirdBytes: Integer;
  begin

    firstByte := bits.readBits(8);

    if ((firstByte and $80) = 0) then
    begin
      result := (firstByte and $7F);
      exit
    end;

    if ((firstByte and $C0) = $80) then
    begin
      secondByte := bits.readBits(8);
      begin
        result := (((firstByte and $3F) shl 8) or secondByte);
        exit
      end
    end;

    if ((firstByte and $E0) <> $C0) then
      raise EArgumentException.Create('Bad ECI bits starting with byte ' +
        firstByte.toString());

    SecondThirdBytes := bits.readBits($10);
    result := (((firstByte and $1F) shl $10) or SecondThirdBytes);
  end;

  class function TDecodedBitStreamParser.toAlphaNumericChar
    (value: Integer): Char;
  begin
    if (value >= Length(TDecodedBitStreamParser.ALPHANUMERIC_CHARS)) then
      raise Exception.Create('Format exception');
    begin
      result := TDecodedBitStreamParser.ALPHANUMERIC_CHARS[value];
      exit
    end

  end;

Initialization

TDecodedBitStreamParser.InitializeClass;

end.
