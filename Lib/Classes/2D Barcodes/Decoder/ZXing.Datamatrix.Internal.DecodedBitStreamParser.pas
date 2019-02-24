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

  * Original Authors: bbrown@google.com (Brian Brown) and Sean Owen
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.Internal.DecodedBitStreamParser;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.DecoderResult,
  ZXing.ByteSegments,
  ZXing.BitSource;

type
  /// <summary>
  /// <p>Data Matrix Codes can encode text as bits in one of several modes, and can use multiple modes
  /// in one Data Matrix Code. This class decodes the bits back into text.</p>
  ///
  /// <p>See ISO 16022:2006, 5.2.1 - 5.2.9.2</p>
  /// </summary>
  TDecodedBitStreamParser = class abstract
  type
    TMode = (PAD_ENCODE, // Not really a mode
      ASCII_ENCODE, C40_ENCODE, TEXT_ENCODE, ANSIX12_ENCODE, EDIFACT_ENCODE,
      BASE256_ENCODE);
  private
    class var C40_BASIC_SET_CHARS, C40_SHIFT2_SET_CHARS, TEXT_BASIC_SET_CHARS,
      TEXT_SHIFT2_SET_CHARS, TEXT_SHIFT3_SET_CHARS: TArray<Char>;

    class procedure InitializeClass;
    class procedure FinalizeClass;

    class function decodeC40Segment(bits: TBitSource;
      res: TStringBuilder): boolean;
    class function decodeTextSegment(bits: TBitSource;
      res: TStringBuilder): boolean;
    class function decodeAnsiX12Segment(bits: TBitSource;
      res: TStringBuilder): boolean;
    class function decodeEdifactSegment(bits: TBitSource;
      res: TStringBuilder): boolean;
    class function decodeBase256Segment(bits: TBitSource; res: TStringBuilder;
      byteSegments:IByteSegments ): boolean;
    class function decodeAsciiSegment(bits: TBitSource; res: TStringBuilder;
      resultTrailer: TStringBuilder; var mode: TMode): boolean;
    class procedure parseTwoBytes(firstByte: Integer; secondByte: Integer;
      result: TArray<Integer>);
    class function unrandomize255State(randomizedBase256Codeword: Integer;
      base256CodewordPosition: Integer): Integer;
  public
    class function decode(bytes: TArray<Byte>): TDecoderResult;
  end;

implementation

{ TDecodedBitStreamParser }

class function TDecodedBitStreamParser.decode(bytes: TArray<Byte>)
  : TDecoderResult;
var
  bits: TBitSource;
  res, resultTrailer: TStringBuilder;
  byteSegments: IByteSegments;
  mode: TMode;

begin
  bits := TBitSource.Create(bytes);
  res := TStringBuilder.Create(100);
  resultTrailer := TStringBuilder.Create(0);
  byteSegments :=  ByteSegmentsCreate;

  try

    byteSegments.Add(TArray<Byte>.Create()); // TODO (KG): Validation
    mode := TMode.ASCII_ENCODE;
    while ((mode <> TMode.PAD_ENCODE) and (bits.available() > 0)) do
    begin
      if (mode = TMode.ASCII_ENCODE) then
      begin
        if (not TDecodedBitStreamParser.decodeAsciiSegment(bits, res,
          resultTrailer, mode)) then
             result := nil;  // this line was totally missing. I assume the correct thing is to set result = nil.
      end
      else
      begin
        case mode of
          TMode.C40_ENCODE:
            begin
              if (not TDecodedBitStreamParser.decodeC40Segment(bits, res)) then
              begin
                result := nil;
                exit;
              end;
            end;
          TMode.TEXT_ENCODE:
            begin
              if (not TDecodedBitStreamParser.decodeTextSegment(bits, res)) then
              begin
                result := nil;
                exit;
              end;
            end;
          TMode.ANSIX12_ENCODE:
            begin
              if (not TDecodedBitStreamParser.decodeAnsiX12Segment(bits, res))
              then
              begin
                result := nil;
                exit;
              end;
            end;
          TMode.EDIFACT_ENCODE:
            begin
              if (not TDecodedBitStreamParser.decodeEdifactSegment(bits, res))
              then
              begin
                result := nil;
                exit;
              end;
            end;
          TMode.BASE256_ENCODE:
            begin
              if (not TDecodedBitStreamParser.decodeBase256Segment(bits, res,
                    byteSegments)) then
              begin
                result := nil;
                exit;
              end;
            end;
        else
          begin
            result := nil;
            exit;
          end;
        end;

        mode := TMode.ASCII_ENCODE;
      end;
    end;

    if (resultTrailer.Length > 0) then
      res.Append(resultTrailer.ToString);

    if (byteSegments.Count = 0) then
      result := TDecoderResult.Create(bytes, res.ToString, nil, '')
    else
      result := TDecoderResult.Create(bytes, res.ToString, byteSegments, '');

  finally
    bits.Free;
    res.Free;
    resultTrailer.Free;
//
//    for i := 0 to byteSegments.Count - 1 do
//    begin
//
//    end;
//
//    byteSegments.Free;
  end;

end;

/// <summary>
/// See ISO 16022:2006, 5.2.7
/// </summary>
class function TDecodedBitStreamParser.decodeAnsiX12Segment(bits: TBitSource;
  res: TStringBuilder): boolean;
var
  i: Integer;
  cValues: TArray<Integer>;
  firstByte: Integer;
  cValue: Integer;
begin
  // Three ANSI X12 values are encoded in a 16-bit value as
  // (1600 * C1) + (40 * C2) + C3 + 1

  cValues := TArray<Integer>.Create(0, 0, 0);
  while (bits.available() > 0) do
  begin
    // If there is only one byte left then it will be encoded as ASCII
    if (bits.available = 8) then
    begin
      result := true;
      exit;
    end;
    firstByte := bits.readBits(8);
    if (firstByte = 254) then
    begin
      // Unlatch codeword
      result := true;
      exit;
    end;

    TDecodedBitStreamParser.parseTwoBytes(firstByte, bits.readBits(8), cValues);

    for i := 0 to 2 do
    begin
      cValue := cValues[i];
      case cValue of
        0:
          begin
            // X12 segment terminator <CR>
            res.Append(#13);
          end;
        1:
          begin
            // X12 segment separator *
            res.Append('*');
          end;
        2:
          begin
            // X12 sub-element separator >
            res.Append('>');
          end;
        3:
          begin
            // space
            res.Append(' ');
          end;
      else
        begin
          if (cValue < 14) then
            // 0 - 9
            res.Append(Char(cValue + 44))
          else if (cValue < 40) then
            // A - Z
            res.Append(Char(cValue + 51))
          else
          begin
            result := false;
            exit;
          end;
        end;
      end;
    end;
  end;;

  result := true;
end;

/// <summary>
/// See ISO 16022:2006, 5.2.3 and Annex C, Table C.2
/// </summary>
class function TDecodedBitStreamParser.decodeAsciiSegment(bits: TBitSource;
  res: TStringBuilder; resultTrailer: TStringBuilder; var mode: TMode): boolean;
var
  oneByte: Integer;
  upperShift: boolean;
  value: Byte;
begin
  upperShift := false;
  mode := TMode.ASCII_ENCODE;

  while (bits.available() > 0) do
  begin
    oneByte := bits.readBits(8);
    if (oneByte = 0) then
    begin
      result := false;
      exit;
    end
    else
    begin
      if (oneByte <= 128) then
      begin
        // ASCII data (ASCII value + 1)
        if (upperShift) then
        begin
          Inc(oneByte, 128);
          // upperShift := false;
        end;
        res.Append(Char(oneByte - 1));
        mode := TMode.ASCII_ENCODE;

        result := true;
        exit;
      end
      else
      begin
        if (oneByte = 129) then
        begin
          // Pad
          mode := TMode.PAD_ENCODE;
          result := true;
          exit;
        end
        else
        begin
          if (oneByte <= 229) then
          begin
            // 2-digit data 00-99 (Numeric Value + 130)
            value := (oneByte - 130);
            if (value < 10) then
              // pad with '0' for single digit values
              res.Append('0');
            res.Append(value);
          end
          else
          begin
            if (oneByte = 230) then
            begin
              // Latch to C40 encodation
              mode := TMode.C40_ENCODE;
              result := true;
              exit;
            end
            else
            begin
              if (oneByte = 231) then
              begin
                // Latch to Base 256 encodation
                mode := TMode.BASE256_ENCODE;
                result := true;
                exit;
              end
              else
              begin
                if (oneByte = 232) then
                begin
                  // FNC1
                  res.Append(Char(29)); // translate as ASCII 29
                end
                else
                begin
                  if ((oneByte = 233) or (oneByte = 234)) then
                  begin
                    // Structured Append, Reader Programming
                    // Ignore these symbols for now
                    // throw EReaderException.Instance;
                  end
                  else
                  begin
                    if (oneByte = 235) then
                    begin
                      // Upper Shift (shift to Extended ASCII)
                      upperShift := true;
                    end
                    else
                    begin
                      if (oneByte = 236) then
                      begin
                        // 05 Macro
                        res.Append('[)>' + #$001E05 + #$001D);
                        res.Insert(0, #$001E + #$0004);
                      end
                      else
                      begin
                        if (oneByte = 237) then
                        begin
                          // 06 Macro
                          res.Append('[)>' + #$001E06 + #$001D);
                          res.Insert(0, #$001E + #$0004);
                        end
                        else
                        begin
                          if (oneByte = 238) then
                          begin
                            // Latch to ANSI X12 encodation
                            mode := TMode.ANSIX12_ENCODE;
                            result := true;
                            exit;
                          end
                          else
                          begin
                            if (oneByte = 239) then
                            begin
                              // Latch to Text encodation
                              mode := TMode.TEXT_ENCODE;
                              result := true;
                              exit;
                            end
                            else
                            begin
                              if (oneByte = 240) then
                              begin
                                // ECI Character
                                // TODO: I think we need to support ECI
                                // throw TReaderException.Instance;
                                // Ignore this symbol for now
                              end
                              else
                              begin
                                  // Not to be used in ASCII encodation
                                  // ... but work around encoders that end with 254, latch back to ASCII
                                  if ((oneByte <> 254) or (bits.available() <> 0)) then
                                  begin
                                    result := false;
                                    exit;
                                  end;

                              end;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  mode := TMode.ASCII_ENCODE;
  result := true;
end;

/// <summary>
/// See ISO 16022:2006, 5.2.9 and Annex B, B.2
/// </summary>
class function TDecodedBitStreamParser.decodeBase256Segment(bits: TBitSource;
  res: TStringBuilder; byteSegments: IByteSegments ): boolean;
var
  i, Count, codewordPosition, d1: Integer;
  bytes: TArray<Byte>;
begin
  // Figure out how long the Base 256 Segment is.
  codewordPosition := (1 + bits.ByteOffset); // position is 1-indexed
  d1 := TDecodedBitStreamParser.unrandomize255State(bits.readBits(8),
    codewordPosition);
  Inc(codewordPosition);
  if (d1 = 0) then
    // Read the remainder of the symbol
    Count := (bits.available div 8)
  else if (d1 < 250) then
    Count := d1
  else
  begin
    Count := ((250 * (d1 - 249)) + TDecodedBitStreamParser.unrandomize255State
      (bits.readBits(8), codewordPosition));
    Inc(codewordPosition);
  end;

  // We're seeing NegativeArraySizeException errors from users.
  if (Count < 0) then
  begin
    result := false;
    exit;
  end;

  bytes := TArray<Byte>.Create();
  SetLength(bytes, Count);

  for i := 0 to Pred(Count) do
  begin
    // Have seen this particular error in the wild, such as at
    // http://www.bcgen.com/demo/IDAutomationStreamingDataMatrix.aspx?MODE=3&D=Fred&PFMT=3&PT=F&X=0.3&O=0&LM=0.2
    if (bits.available() < 8) then
    begin
      result := false;
      exit;
    end;
    bytes[i] := Byte(TDecodedBitStreamParser.unrandomize255State
      (bits.readBits(8), codewordPosition));
    Inc(codewordPosition);
  end;
  byteSegments.Add(bytes);
  try
    res.Append(TEncoding.GetEncoding('ISO-8859-1').GetString(bytes))
  except
    on uee: Exception do
      raise EInvalidOpException.Create
        ('Platform does not support required encoding: ' + uee.Message)
  end;

  result := true;
end;

/// <summary>
/// See ISO 16022:2006, 5.2.5 and Annex C, Table C.1
/// </summary>
class function TDecodedBitStreamParser.decodeC40Segment(bits: TBitSource;
  res: TStringBuilder): boolean;
var
  i: Integer;
  c40char: Char;
  cValues: TArray<Integer>;
  upperShift: boolean;
  shift: Integer;
  firstByte: Byte;
  cValue: Integer;
begin
  // Three C40 values are encoded in a 16-bit value as
  // (1600 * C1) + (40 * C2) + C3 + 1
  // TODO: The Upper Shift with C40 doesn't work in the 4 value scenario all the time
  upperShift := false;

  cValues := TArray<Integer>.Create(0, 0, 0);
  shift := 0;

  while (bits.available() > 0) do
  begin
    // If there is only one byte left then it will be encoded as ASCII
    if (bits.available() = 8) then
    begin
      result := true;
      exit;
    end;
    firstByte := bits.readBits(8);
    if (firstByte = 254) then
    begin
      // Unlatch codeword
      result := true;
      exit;
    end;

    TDecodedBitStreamParser.parseTwoBytes(firstByte, bits.readBits(8), cValues);

    for i := 0 to 2 do
    begin
      cValue := cValues[i];
      case shift of
        0:
          begin
            if (cValue < 3) then
              shift := (cValue + 1)
            else
            begin
              if (cValue < Length(C40_BASIC_SET_CHARS)) then
              begin
                c40char := C40_BASIC_SET_CHARS[cValue];
                if (upperShift) then
                begin
                  res.Append(Char(c40char) + Char(128));
                  upperShift := false;
                end
                else
                  res.Append(c40char);
              end
              else
              begin
                result := false;
                exit;
              end;
            end;
          end;
        1:
          begin
            if (upperShift) then
            begin
              res.Append(Char(cValue) + Char(128));
              upperShift := false;
            end
            else
              res.Append(Char(cValue));
            shift := 0;
          end;
        2:
          begin
            if (cValue < Length(C40_SHIFT2_SET_CHARS)) then
            begin
              c40char := C40_SHIFT2_SET_CHARS[cValue];
              if (upperShift) then
              begin
                res.Append(Char(c40char) + Char(128));
                upperShift := false;
              end
              else
                res.Append(c40char);
            end
            else
            begin
              if (cValue = 27) then
                // FNC1
                res.Append(Char(29)) // translate as ASCII 29
              else if (cValue = 30) then
                // Upper Shift
                upperShift := true
              else
              begin
                result := false;
                exit;
              end;
            end;
            shift := 0;
          end;
        3:
          begin
            if (upperShift) then
            begin
              res.Append(Char(cValue + 254));
              upperShift := false;
            end
            else
              res.Append(Char(cValue + 96));
            shift := 0;
          end;
      else
        begin
          result := false;
          exit;
        end;
      end;
    end;
  end;

  result := true;
end;

/// <summary>
/// See ISO 16022:2006, 5.2.8 and Annex C Table C.3
/// </summary>
class function TDecodedBitStreamParser.decodeEdifactSegment(bits: TBitSource;
  res: TStringBuilder): boolean;
var
  i: Integer;
  edifactValue: Integer;
  bitsLeft: Integer;
begin
  while (bits.available() > 0) do
  begin
    // If there is only two or less bytes left then it will be encoded as ASCII
    if (bits.available() <= 16) then
    begin
      result := true;
      exit;
    end;

    for i := 0 to 3 do
    begin
      edifactValue := bits.readBits(6);

      // Check for the unlatch character
      if (edifactValue = $1F) then
      begin
        // 011111
        // Read rest of byte, which should be 0, and stop
        bitsLeft := (8 - bits.BitOffset);
        if (not bitsLeft = 8) then
          bits.readBits(bitsLeft);

        result := true;
        exit;
      end;

      if ((edifactValue and $20) = 0) then
      begin
        // no 1 in the leading (6th) bit
        edifactValue := (edifactValue or $40);
        // Add a leading 01 to the 6 bit binary value
      end;
      res.Append(Char(edifactValue));
    end;
  end;

  result := true;
end;

/// <summary>
/// See ISO 16022:2006, 5.2.6 and Annex C, Table C.2
/// </summary>
class function TDecodedBitStreamParser.decodeTextSegment(bits: TBitSource;
  res: TStringBuilder): boolean;
var
  textChar: Char;
  cValues: TArray<Integer>;
  upperShift: boolean;
  i, shift: Integer;
  firstByte: Integer;
  cValue: Integer;
begin
  // Three Text values are encoded in a 16-bit value as
  // (1600 * C1) + (40 * C2) + C3 + 1
  // TODO: The Upper Shift with Text doesn't work in the 4 value scenario all the time
  upperShift := false;

  cValues := TArray<Integer>.Create(0, 0, 0);
  shift := 0;

  while (bits.available() > 0) do
  begin
    // If there is only one byte left then it will be encoded as ASCII
    if (bits.available() = 8) then
    begin
      result := true;
      exit;
    end;

    firstByte := bits.readBits(8);
    if (firstByte = 254) then
    begin
      // Unlatch codeword
      result := true;
      exit;
    end;

    TDecodedBitStreamParser.parseTwoBytes(firstByte, bits.readBits(8), cValues);

    for i := 0 to 2 do
    begin
      cValue := cValues[i];

      case (shift) of
        0:
          begin
            if (cValue < 3) then
              shift := (cValue + 1)
            else
            begin
              if (cValue < Length(TEXT_BASIC_SET_CHARS)) then
              begin
                textChar := TEXT_BASIC_SET_CHARS[cValue];
                if (upperShift) then
                begin
                  res.Append(Char(textChar) + Char(128));
                  upperShift := false;
                end
                else
                  res.Append(textChar);
              end
              else
              begin
                result := false;
                exit;
              end;
            end;
          end;
        1:
          begin
            if (upperShift) then
            begin
              res.Append(Char(cValue + 128));
              upperShift := false;
            end
            else
              res.Append(Char(cValue));
            shift := 0;
          end;
        2:
          begin
            // Shift 2 for Text is the same encoding as C40
            if (cValue < Length(TEXT_SHIFT2_SET_CHARS)) then
            begin
              textChar := TEXT_SHIFT2_SET_CHARS[cValue];
              if (upperShift) then
              begin
                res.Append(Char(textChar) + Char(128));
                upperShift := false;
              end
              else
                res.Append(textChar);
            end
            else
            begin
              if (cValue = 27) then
                // FNC1
                res.Append(Char(29)) // translate as ASCII 29
              else if (cValue = 30) then
                // Upper Shift
                upperShift := true
              else
              begin
                result := false;
                exit;
              end;
            end;
            shift := 0;
          end;
        3:
          begin
            if (cValue < Length(TEXT_SHIFT3_SET_CHARS)) then
            begin
              textChar := TEXT_SHIFT3_SET_CHARS[cValue];
              if (upperShift) then
              begin
                res.Append(Char(textChar) + Char(128));
                upperShift := false;
              end
              else
                res.Append(textChar);
              shift := 0;
            end
            else
            begin
              result := false;
              exit;
            end;
          end;
      else
        begin
          result := false;
          exit;
        end;
      end;
    end;
  end;

  result := true;
end;

class procedure TDecodedBitStreamParser.parseTwoBytes(firstByte: Integer;
  secondByte: Integer; result: TArray<Integer>);
var
  fullBitValue, temp: Integer;
begin
  fullBitValue := (((firstByte shl 8) + secondByte) - 1);
  temp := (fullBitValue div $640);
  result[0] := temp;
  dec(fullBitValue, (temp * $640));
  temp := (fullBitValue div 40);
  result[1] := temp;
  result[2] := (fullBitValue - (temp * 40))
end;

/// <summary>
/// See ISO 16022:2006, Annex B, B.2
/// </summary>
class function TDecodedBitStreamParser.unrandomize255State
  (randomizedBase256Codeword: Integer;
  base256CodewordPosition: Integer): Integer;
var
  pseudoRandomNumber: Integer;
  tempVariable: Integer;
begin
  pseudoRandomNumber := (((149 * base256CodewordPosition) mod 255) + 1);
  tempVariable := (randomizedBase256Codeword - pseudoRandomNumber);
  if (tempVariable >= 0) then
    result := tempVariable
  else
    result := (tempVariable + 256);
end;

class procedure TDecodedBitStreamParser.InitializeClass;
begin
  /// <summary>
  /// See ISO 16022:2006, Annex C Table C.1
  /// The C40 Basic Character Set (*'s used for placeholders for the shift values)
  /// </summary>
  C40_BASIC_SET_CHARS := TArray<Char>.Create('*', '*', '*', ' ', '0', '1', '2',
    '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z');

  C40_SHIFT2_SET_CHARS := TArray<Char>.Create('!', '"', '#', '$', '%', '&',
    '''', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?',
    '@', '[', '\', ']', '^', '_');

  /// <summary>
  /// See ISO 16022:2006, Annex C Table C.2
  /// The Text Basic Character Set (*'s used for placeholders for the shift values)
  /// </summary>
  TEXT_BASIC_SET_CHARS := TArray<Char>.Create('*', '*', '*', ' ', '0', '1', '2',
    '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z');

  // Shift 2 for Text is the same encoding as C40
  TEXT_SHIFT2_SET_CHARS := C40_SHIFT2_SET_CHARS;

  TEXT_SHIFT3_SET_CHARS := TArray<Char>.Create('`', 'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '{', '|', '}', '~', '');
end;

class procedure TDecodedBitStreamParser.FinalizeClass;
begin
  C40_BASIC_SET_CHARS := nil;
  C40_SHIFT2_SET_CHARS := nil;
  TEXT_BASIC_SET_CHARS := nil;
  TEXT_SHIFT2_SET_CHARS := nil;
  TEXT_SHIFT3_SET_CHARS := nil;
end;

initialization

TDecodedBitStreamParser.InitializeClass;

finalization

TDecodedBitStreamParser.FinalizeClass;

end.
