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
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.DecodedBitStreamParser;

interface

uses 
  ZXing.BitSource,
  SysUtils, 
  ZXing.DecodeHintType,
  Generics.Collections,
  ZXing.QrCode.Internal.ErrorCorrectionLevel,
  ZXing.QrCode.Internal.Version,
  ZXing.DecoderResult,
  ZXing.CharacterSetECI,
  ZXing.StringUtils,
  ZXing.ByteSegments,
  ZXing.QrCode.Internal.Mode,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary> <p>QR Codes can encode text as bits in one of several modes, and can use multiple modes
  /// in one QR Code. This class decodes the bits back into text.</p>
  ///
  /// <p>See ISO 18004:2006, 6.4.3 - 6.4.7</p>
  /// </summary>
  TDecodedBitStreamParser = class abstract
  private
    /// <summary>
    /// See ISO 18004:2006, 6.4.4 Table 5
    /// </summary>
    class var ALPHANUMERIC_CHARS: TArray<Char>;

    const
      GB2312_SUBSET: Integer = 1;

    class procedure InitializeClass;
    class procedure FinalizeClass;

    class function decodeAlphanumericSegment(const bits: TBitSource;
      const res: TStringBuilder; count: Integer;
      const fc1InEffect: Boolean): Boolean; static;

    class function decodeByteSegment(const bits: TBitSource;
      const res: TStringBuilder; count: Integer;
      const currentCharacterSetECI: TCharacterSetECI;
      const byteSegments: IByteSegments;
      const hints: TDictionary<TDecodeHintType, TObject>): Boolean; static;

    /// <summary>
    /// See specification GBT 18284-2000
    /// </summary>
    /// <param name="bits">The bits.</param>
    /// <param name="result">The result.</param>
    /// <param name="count">The count.</param>
    /// <returns></returns>
    class function decodeHanziSegment(const bits: TBitSource;
      const res: TStringBuilder; count: Integer): Boolean; static;

    class function decodeKanjiSegment(const bits: TBitSource;
      const res: TStringBuilder; count: Integer): boolean; static;

    class function decodeNumericSegment(const bits: TBitSource;
      const res: TStringBuilder; count: Integer): Boolean; static;

    class function parseECIValue(const bits: TBitSource): Integer; static;
    class function toAlphaNumericChar(const value: Integer): Char; static;
  public
    class function decode(const bytes: TArray<Byte>; const version: TVersion;
      const ecLevel: TErrorCorrectionLevel;
      const hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult; static;
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

class procedure TDecodedBitStreamParser.FinalizeClass;
begin
  ALPHANUMERIC_CHARS := nil;
end;

class function TDecodedBitStreamParser.decode(const bytes: TArray<Byte>;
  const version: TVersion; const ecLevel: TErrorCorrectionLevel;
  const hints: TDictionary<TDecodeHintType, TObject>): TDecoderResult;
var
  Mode: TMode;
  bits: TBitSource;
  byteSegments: IByteSegments;
  fc1InEffect: Boolean;
  symbolSequence,
  parityData,
  count, value,
  countHanzi, subSet: Integer;
  currentCharacterSetECI: TCharacterSetECI;
  ecstring,s: String;
  res: TStringBuilder;
begin
  Result := nil;

  bits := TBitSource.Create(bytes);
  res := TStringBuilder.Create(50);
  byteSegments := ByteSegmentsCreate();
  byteSegments.Capacity := 1;
  symbolSequence := -1;
  parityData := -1;
  try
    try
      currentCharacterSetECI := nil;
      fc1InEffect := false;
      repeat
        // While still another segment to read...
        if (bits.available < 4)
        then
           // OK, assume we're done. Really, a TERMINATOR mode should have been recorded here
           Mode := TMode.TERMINATOR
        else
        begin
          try
            Mode := TMode.forBits(bits.readBits(4)); // mode is encoded by 4 bits
          except
            on E: EArgumentException do
              exit;
          end;
        end;

        if (Mode <> TMode.TERMINATOR) then
        begin
          if ((Mode = TMode.FNC1_FIRST_POSITION) or
              (Mode = TMode.FNC1_SECOND_POSITION)) then
          begin
            // We do little with FNC1 except alter the parsed result a bit according to the spec
            fc1InEffect := true;
          end else
          begin
            if (Mode = TMode.STRUCTURED_APPEND) then
            begin
              if (bits.available() < 16)
              then
                 exit;
              // not really supported; but sequence number and parity is added later to the result metadata
              // Read next 8 bits (symbol sequence #) and 8 bits (parity data), then continue
              symbolSequence := bits.readBits(8);
              parityData := bits.readBits(8);
            end else
            begin
              if (Mode = TMode.ECI) then
              begin
                // Count doesn't apply to ECI
                value := parseECIValue(bits);
                currentCharacterSetECI := TCharacterSetECI.getCharacterSetECIByValue(value);
                if (currentCharacterSetECI = nil)
                then
                   exit;
              end else
              begin
                // First handle Hanzi mode which does not start with character count
                if (Mode = TMode.HANZI) then
                begin
                  // chinese mode contains a sub set indicator right after mode indicator
                  subSet := bits.readBits(4);
                  countHanzi := bits.readBits(Mode.getCharacterCountBits(version));
                  if (subSet = GB2312_SUBSET) then
                  begin
                    if (not TDecodedBitStreamParser.decodeHanziSegment(bits,
                      res, countHanzi))
                    then
                       exit;
                  end;
                end else
                begin
                  // "Normal" QR code modes:
                  // How many characters will follow, encoded in this mode?
                  count := bits.readBits(Mode.getCharacterCountBits(version));
                  if (Mode = TMode.NUMERIC) then
                  begin
                    if (not TDecodedBitStreamParser.decodeNumericSegment(bits,
                      res, count))
                    then
                       exit;
                  end else
                  begin
                    if (Mode = TMode.ALPHANUMERIC) then
                    begin
                      if (not TDecodedBitStreamParser.
                        decodeAlphanumericSegment(bits, res, count, fc1InEffect))
                      then
                         exit;
                    end else
                    begin
                      if (Mode = TMode.BYTE) then
                      begin
                        if (not TDecodedBitStreamParser.
                          decodeByteSegment(bits, res, count,
                          currentCharacterSetECI, byteSegments, hints))
                        then
                           exit;
                      end else
                      begin
                        if (Mode <> TMode.KANJI) then
                        begin
                          if (not TDecodedBitStreamParser.
                            decodeKanjiSegment(bits, res, count))
                          then
                             exit;
                        end else exit;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        until (Mode = TMode.TERMINATOR)
      except
        on E: EArgumentException do
          exit;
      end;

      if (byteSegments.count = 0)
      then
         byteSegments := nil;

      if (ecLevel = nil)
      then
         ecstring := ''
      else
         ecstring := ecLevel.toString();

      s:= res.toString.Replace('#13#10', '#10').Replace('#10', #13);
      Result := TDecoderResult.Create(bytes, s, byteSegments, ecstring, symbolSequence,
        parityData);
    finally
     // FreeAndNil(byteSegments);

      res.Clear();

      FreeAndNil(res);
      FreeAndNil(bits);
    end;

  end;

class function TDecodedBitStreamParser.decodeAlphanumericSegment(
  const bits: TBitSource; const res: TStringBuilder; count: Integer;
  const fc1InEffect: Boolean): Boolean;
var
  start, i, nextTwoCharsBits: Integer;
  charArray: TArray<Char>;
begin
  Result := false;
  // Read two characters at a time
  start := res.Length;
  while ((count > 1)) do
  begin
    if (bits.available < 11)
    then
       exit;

    nextTwoCharsBits := bits.readBits(11);
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      ((nextTwoCharsBits div 45)));
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      ((nextTwoCharsBits mod 45)));
    Dec(count, 2);
  end;

  if (count = 1) then
  begin
    // special case: one character left
    if (bits.available < 6)
    then
       exit;
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar(bits.readBits(6)))
  end;

  // See section 6.4.8.1, 6.4.8.2
  if (fc1InEffect) then
  begin
    // We need to massage the result a bit if in an FNC1 mode:
    for i := start to Pred(res.Length) do
    begin
      if (res.Chars[i] = '%') then
      begin
        if ((i < (res.Length - 1)) and (res.Chars[(i + 1)] = '%'))
        then
           // %% is rendered as %
           res.Remove((i + 1), 1)
        else
        begin
          // In alpha mode, % should be converted to FNC1 separator 0x1D
          res.Remove(i, 1);
          charArray := TArray<Char>.Create(Char($1D));
          res.Insert(i, charArray);
        end;
      end;
    end;
  end;

  Result := true;
end;

class function TDecodedBitStreamParser.decodeByteSegment(const bits: TBitSource;
  const res: TStringBuilder; count: Integer;
  const currentCharacterSetECI: TCharacterSetECI;
  const byteSegments: IByteSegments;
  const hints: TDictionary<TDecodeHintType, TObject>): Boolean;
var
  Enc:TEncoding;
  encodingS, s: string;
  readBytes: TArray<Byte>;
  i: Integer;
begin
  Result := false;
  // Don't crash trying to read more bits than we have available.
  if ((count shl 3) > bits.available)
  then
     exit;

  readBytes := TArray<Byte>.Create();
  SetLength(readBytes, count);
  for i := 0 to Pred(count) do
    readBytes[i] := Byte(bits.readBits(8));

  if (currentCharacterSetECI = nil)
  then
     // The spec isn't clear on this mode; see
     // section 6.4.5: t does not say which encoding to assuming
     // upon decoding. I have seen ISO-8859-1 used as well as
     // Shift_JIS -- without anything like an ECI designator to
     // give a hint.
     encodingS := TStringUtils.guessEncoding(readBytes, hints)
  else
     encodingS := currentCharacterSetECI.EncodingName;

  try
    enc := TEncoding.GetEncoding(encodingS);
    try
      s := enc.GetString(readBytes, 0, Length(readBytes));
      res.Append(s);
    finally
      FreeAndNil(enc);
    end;
  except
    on E: Exception do
      exit;
  end;

  byteSegments.Add(readBytes);
  result := true;
end;

class function TDecodedBitStreamParser.decodeHanziSegment(
  const bits: TBitSource; const res: TStringBuilder; count: Integer): Boolean;
var
  buffer: TArray<Byte>;
  offset, twoBytes,
  assembledTwoBytes: Integer;
begin
  Result := false;
  // Don't crash trying to read more bits than we have available.
  if ((count * 13) > bits.available)
  then
     exit;

  // Each character will require 2 bytes. Read the characters as 2-byte pairs
  // and decode as GB2312 afterwards
  buffer := TArray<Byte>.Create();
  SetLength(buffer, 2 * count);
  offset := 0;

  while ((count > 0)) do
  begin
    // Each 13 bits encodes a 2-byte character
    twoBytes := bits.readBits(13);
    assembledTwoBytes := (((twoBytes div $60) shl 8) or (twoBytes mod $60));
    if (assembledTwoBytes < $00A00)
    then
       // In the 0xA1A1 to 0xAAFE range
       Inc(assembledTwoBytes, $0A1A1)
    else
       // In the 0xB0A1 to 0xFAFE range
       Inc(assembledTwoBytes, $0A6A1);

    buffer[offset] := Byte(TMathUtils.Asr(assembledTwoBytes, 8) and $FF);
    buffer[(offset + 1)] := Byte(assembledTwoBytes and $FF);
    Inc(offset, 2);
    Dec(count);
  end;

  try
    res.Append(Tencoding.GetEncoding(TStringUtils.GB2312).GetString(buffer, 0,
      Length(buffer)))
  except
    on E: Exception do
      exit;
 end;
 Result := true;
end;

class function TDecodedBitStreamParser.decodeKanjiSegment(
  const bits: TBitSource; const res: TStringBuilder; count: Integer): Boolean;
var
  buffer: TArray<Byte>;
  twoBytes, offset,
  assembledTwoBytes: Integer;
begin
  Result := false;
  // Don't crash trying to read more bits than we have available.
  if ((count * 13) > bits.available)
  then
     exit;

  // Each character will require 2 bytes. Read the characters as 2-byte pairs
  // and decode as Shift_JIS afterwards
  buffer := TArray<Byte>.Create();
  SetLength(buffer, 2 * count);
  offset := 0;

  while ((count > 0)) do
  begin
    // Each 13 bits encodes a 2-byte character
    twoBytes := bits.readBits(13);
    assembledTwoBytes := (((twoBytes div $0C0) shl 8) or (twoBytes mod $0C0));
    if (assembledTwoBytes < $1F00)
    then
       // In the 0x8140 to 0x9FFC range
       Inc(assembledTwoBytes, $08140)
    else
       Inc(assembledTwoBytes, $0C140);
    buffer[offset] := Byte( TMathUtils.Asr(assembledTwoBytes,8));
    buffer[(offset + 1)] := Byte(assembledTwoBytes);
    Inc(offset, 2);
    Dec(count);
  end;

  try
    res.Append(Tencoding.GetEncoding(TStringUtils.SHIFT_JIS).GetString(buffer,
      0, Length(buffer)))
  except
    on E: Exception do
      exit;
  end;
  Result := true;
end;

class function TDecodedBitStreamParser.decodeNumericSegment(
  const bits: TBitSource; const res: TStringBuilder; count: Integer): Boolean;
var
  threeDigitsBits,
  twoDigitsBits,
  digitBits: Integer;
begin
  Result := false;
  // Read three digits at a time
  while ((count >= 3)) do
  begin
    if (bits.available < 10)
    then
       exit;
    threeDigitsBits := bits.readBits(10);
    if (threeDigitsBits >= 1000)
    then
       exit;
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      ((threeDigitsBits div 100)));
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      (((threeDigitsBits div 10) mod 10)));
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      ((threeDigitsBits mod 10)));
    Dec(count, 3)
  end;

  if (count = 2) then
  begin
    // Two digits left over to read, encoded in 7 bits
    if (bits.available < 7)
    then
       exit;

    twoDigitsBits := bits.readBits(7);
    if (twoDigitsBits >= 100)
    then
       exit;

    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      ((twoDigitsBits div 10)));
    res.Append(TDecodedBitStreamParser.toAlphaNumericChar
      ((twoDigitsBits mod 10)));
  end else
  begin
    if (count = 1) then
    begin
      // One digit left over to read
      if (bits.available < 4)
      then
         exit;

      digitBits := bits.readBits(4);
      if (digitBits >= 10)
      then
         exit;

      res.Append(TDecodedBitStreamParser.toAlphaNumericChar(digitBits))
    end;
  end;

  Result := true;
end;

class function TDecodedBitStreamParser.parseECIValue(
  const bits: TBitSource): Integer;
var
  firstByte,
  secondByte,
  SecondThirdBytes: Integer;
begin
  firstByte := bits.readBits(8);

  if ((firstByte and $80) = 0) then
  begin
    // just one byte
    Result := (firstByte and $7F);
    exit;
  end;

  if ((firstByte and $C0) = $80) then
  begin
    // two bytes
    secondByte := bits.readBits(8);
    Result := (((firstByte and $3F) shl 8) or secondByte);
    exit;
  end;

  if ((firstByte and $E0) = $C0) then
  begin
    // three bytes
    SecondThirdBytes := bits.readBits(16);
    Result := (((firstByte and $1F) shl 16) or SecondThirdBytes);
    exit;
  end;

  raise EArgumentException.Create('Bad ECI bits starting with byte ' +
     firstByte.toString());
end;

class function TDecodedBitStreamParser.toAlphaNumericChar(
  const value: Integer): Char;
begin
  if (value >= Length(ALPHANUMERIC_CHARS))
  then
     raise Exception.Create('Format exception');

  Result := ALPHANUMERIC_CHARS[value];
end;

initialization
  TDecodedBitStreamParser.InitializeClass;
finalization
  TDecodedBitStreamParser.FinalizeClass;
end.
