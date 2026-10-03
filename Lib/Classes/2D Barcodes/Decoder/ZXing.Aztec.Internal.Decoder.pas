unit ZXing.Aztec.Internal.Decoder;

{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
  * Copyright 2022 Axel Waggershauser
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

  * Ported from zxing-cpp (AZDecoder.cpp): reads the bits of the layers,
  * corrects them (Reed-Solomon) and decodes the text (ISO/IEC 24778:2008).
}

interface

uses
  ZXing.DecoderResult,
  ZXing.Aztec.Internal.Detector;

/// <summary>
/// Decodes the detected symbol; nil when it can not be read, with error
/// 'Checksum' (too many errors) or 'Format' (data that can not be
/// interpreted).
/// </summary>
function DecodeAztec(detected: TAztecDetectorResult;
  out error: string): TDecoderResult;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.CharacterSetECI,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder;

type
  TAztecTable = (tUpper, tLower, tMixed, tDigit, tPunct, tBinary);

  /// <summary>Raised when the bit stream ends too early.</summary>
  ETruncated = class(Exception);

  /// <summary>Reads bits (highest first) from a bit array.</summary>
  TBitReader = record
    Bits: TArray<Boolean>;
    Pos: Integer;
    function Available: Integer;
    function ReadBits(n: Integer): Integer;
  end;

  /// <summary>The bytes of the text with the character set (ECI) of each
  /// part.</summary>
  TContent = record
    Bytes: TBytes;
    // the start of each part and its character set (ECI value, -1 for the
    // default)
    Starts: TArray<Integer>;
    ECIs: TArray<Integer>;
    HasECI: Boolean;
    procedure Append(b: Byte); overload;
    procedure Append(const s: string); overload;
    procedure SwitchEncoding(eci: Integer);
    procedure Erase(index, count: Integer);
    function Text: string;
  end;

const
  // 'CTRL_xy': x the table (Upper, Lower, Mixed, Punct, Digit, Binary), y
  // Latch or Shift
  UPPER_TABLE: array [0 .. 31] of string = ('CTRL_PS', ' ', 'A', 'B', 'C', 'D',
    'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'CTRL_LL', 'CTRL_ML', 'CTRL_DL',
    'CTRL_BS');
  LOWER_TABLE: array [0 .. 31] of string = ('CTRL_PS', ' ', 'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'CTRL_US', 'CTRL_ML', 'CTRL_DL',
    'CTRL_BS');
  MIXED_TABLE: array [0 .. 31] of string = ('CTRL_PS', ' ', #1, #2, #3, #4, #5,
    #6, #7, #8, #9, #10, #11, #12, #13, #27, #28, #29, #30, #31, '@', '\', '^',
    '_', '`', '|', '~', #127, 'CTRL_LL', 'CTRL_UL', 'CTRL_PL', 'CTRL_BS');
  PUNCT_TABLE: array [0 .. 31] of string = ('FLGN', #13, #13#10, '. ', ', ',
    ': ', '!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.',
    '/', ':', ';', '<', '=', '>', '?', '[', ']', '{', '}', 'CTRL_UL');
  DIGIT_TABLE: array [0 .. 15] of string = ('CTRL_PS', ' ', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', ',', '.', 'CTRL_UL', 'CTRL_US');

{ TBitReader }

function TBitReader.Available: Integer;
begin
  Result := Length(Bits) - Pos;
end;

function TBitReader.ReadBits(n: Integer): Integer;
begin
  if (n > Available) then
    raise ETruncated.Create('Truncated bit stream');
  Result := 0;
  for var i := 0 to n - 1 do
  begin
    Result := (Result shl 1) or Ord(Bits[Pos]);
    Inc(Pos);
  end;
end;

{ TContent }

procedure TContent.Append(b: Byte);
begin
  Bytes := Bytes + [b];
end;

procedure TContent.Append(const s: string);
begin
  for var c in s do
    Append(Byte(Ord(c)));
end;

procedure TContent.SwitchEncoding(eci: Integer);
begin
  HasECI := true;
  Starts := Starts + [Length(Bytes)];
  ECIs := ECIs + [eci];
end;

procedure TContent.Erase(index, count: Integer);
begin
  Delete(Bytes, index, count);
  // (only used at the start, before any ECI part)
  for var i := 0 to High(Starts) do
    if (Starts[i] > index) then
      Starts[i] := Max(index, Starts[i] - count);
end;

function TContent.Text: string;
begin
  Result := '';
  // the parts: the default character set (ISO-8859-1) up to the first ECI
  var allStarts: TArray<Integer> := [0] + Starts;
  var allECIs: TArray<Integer> := [-1] + ECIs;
  for var i := 0 to High(allStarts) do
  begin
    var from := allStarts[i];
    var till := Length(Bytes);
    if (i < High(allStarts)) then
      till := allStarts[i + 1];
    if (till <= from) then
      continue;
    var part := Copy(Bytes, from, till - from);
    var encodingName := 'ISO-8859-1';
    if (allECIs[i] >= 0) then
    begin
      var charset := TCharacterSetECI.getCharacterSetECIByValue(allECIs[i]);
      if (charset <> nil) then
        encodingName := charset.EncodingName;
    end;
    var encoding: TEncoding := nil;
    try
      try
        encoding := TEncoding.GetEncoding(encodingName);
      except
        encoding := nil;
      end;
      if (encoding <> nil) then
        Result := Result + encoding.GetString(part)
      else
        // the bytes as Latin-1 characters
        for var b in part do
          Result := Result + Char(b);
    finally
      encoding.Free;
    end;
  end;
end;

{ decoding }

function TotalBitsInLayer(layers: Integer; compact: Boolean): Integer;
begin
  if compact then
    Result := (88 + 16 * layers) * layers
  else
    Result := (112 + 16 * layers) * layers;
end;

/// <summary>The bits of the layers, from the outside in.</summary>
function ExtractBits(detected: TAztecDetectorResult): TArray<Boolean>;
begin
  var compact := detected.Compact;
  var layers := detected.NbLayers;
  // not including the reference grid lines
  var baseMatrixSize := layers * 4 + 11;
  if not compact then
    baseMatrixSize := layers * 4 + 14;
  var map: TArray<Integer>;
  SetLength(map, baseMatrixSize);

  if compact then
    for var i := 0 to baseMatrixSize - 1 do
      map[i] := i
  else
  begin
    var matrixSize := baseMatrixSize + 1 + 2 *
      ((baseMatrixSize div 2 - 1) div 15);
    var origCenter := baseMatrixSize div 2;
    var center := matrixSize div 2;
    for var i := 0 to origCenter - 1 do
    begin
      var newOffset := i + i div 15;
      map[origCenter - i - 1] := center - newOffset - 1;
      map[origCenter + i] := center + newOffset + 1;
    end;
  end;

  var matrix := detected.Bits;
  SetLength(Result, TotalBitsInLayer(layers, compact));
  var rowOffset := 0;
  for var i := 0 to layers - 1 do
  begin
    var rowSize := (layers - i) * 4 + 12;
    if compact then
      rowSize := (layers - i) * 4 + 9;
    // the top left of this layer is (low, low), the bottom right (high,
    // high) (not including the reference grid lines)
    var low := i * 2;
    var high := baseMatrixSize - 1 - low;
    // two columns of 2 x rowSize and two rows of rowSize x 2
    for var j := 0 to rowSize - 1 do
    begin
      var colOffset := j * 2;
      for var k := 0 to 1 do
      begin
        // left column
        Result[rowOffset + 0 * rowSize + colOffset + k] :=
          matrix[map[low + k], map[low + j]];
        // bottom row
        Result[rowOffset + 2 * rowSize + colOffset + k] :=
          matrix[map[low + j], map[high - k]];
        // right column
        Result[rowOffset + 4 * rowSize + colOffset + k] :=
          matrix[map[high - k], map[high - j]];
        // top row
        Result[rowOffset + 6 * rowSize + colOffset + k] :=
          matrix[map[high - j], map[low + k]];
      end;
    end;
    Inc(rowOffset, rowSize * 8);
  end;
end;

/// <summary>The error corrected and unstuffed data bits; false with error
/// when it fails.</summary>
function CorrectBits(detected: TAztecDetectorResult;
  const rawbits: TArray<Boolean>; out corrected: TArray<Boolean>;
  out ecLevel: Integer; out error: string): Boolean;
begin
  Result := false;
  corrected := nil;
  var codewordSize: Integer;
  var field: TGenericGF;
  if (detected.NbLayers <= 2) then
  begin
    codewordSize := 6;
    field := TGenericGF.AZTEC_DATA_6;
  end
  else if (detected.NbLayers <= 8) then
  begin
    codewordSize := 8;
    field := TGenericGF.AZTEC_DATA_8;
  end
  else if (detected.NbLayers <= 22) then
  begin
    codewordSize := 10;
    field := TGenericGF.AZTEC_DATA_10;
  end
  else
  begin
    codewordSize := 12;
    field := TGenericGF.AZTEC_DATA_12;
  end;

  var numCodewords := Length(rawbits) div codewordSize;
  var numDataCodewords := detected.NbDataBlocks;
  var numECCodewords := numCodewords - numDataCodewords;
  if (numCodewords < numDataCodewords) then
  begin
    error := 'Format';
    exit;
  end;

  // the codewords (the first bits that do not make a codeword are skipped)
  var offset := Length(rawbits) mod codewordSize;
  var dataWords: TArray<Integer>;
  SetLength(dataWords, numCodewords);
  for var i := 0 to numCodewords - 1 do
  begin
    var w := 0;
    for var b := 0 to codewordSize - 1 do
      w := (w shl 1) or Ord(rawbits[offset + i * codewordSize + b]);
    dataWords[i] := w;
  end;

  var rs := TReedSolomonDecoder.Create(field);
  try
    if not rs.decode(dataWords, numECCodewords) then
    begin
      error := 'Checksum';
      exit;
    end;
  finally
    rs.Free;
  end;

  // unstuffing
  var bits := TList<Boolean>.Create;
  try
    var allOnes := (1 shl codewordSize) - 1;
    for var i := 0 to numDataCodewords - 1 do
    begin
      var dataWord := dataWords[i];
      if (dataWord = 0) or (dataWord = allOnes) then
      begin
        error := 'Format';
        exit;
      end
      else if (dataWord = 1) then
        // the next codewordSize - 1 bits are all 0
        for var b := 1 to codewordSize - 1 do
          bits.Add(false)
      else if (dataWord = allOnes - 1) then
        // or all 1
        for var b := 1 to codewordSize - 1 do
          bits.Add(true)
      else
        for var b := codewordSize - 1 downto 0 do
          bits.Add((dataWord shr b) and 1 = 1);
    end;
    corrected := bits.ToArray;
  finally
    bits.Free;
  end;

  ecLevel := numECCodewords * 100 div numCodewords;
  Result := true;
end;

function GetTable(c: Char): TAztecTable;
begin
  case c of
    'L':
      Result := tLower;
    'P':
      Result := tPunct;
    'M':
      Result := tMixed;
    'D':
      Result := tDigit;
    'B':
      Result := tBinary;
  else
    Result := tUpper;
  end;
end;

function GetCharacter(table: TAztecTable; code: Integer): string;
begin
  case table of
    tUpper:
      Result := UPPER_TABLE[code];
    tLower:
      Result := LOWER_TABLE[code];
    tMixed:
      Result := MIXED_TABLE[code];
    tPunct:
      Result := PUNCT_TABLE[code];
    tDigit:
      Result := DIGIT_TABLE[code];
  else
    Result := '';
  end;
end;

/// <summary>ISO/IEC 24778:2008 section 7: the bytes of the text, the
/// character set changes (ECI) and whether there is an FNC1.</summary>
procedure DecodeContent(const bits: TArray<Boolean>; var res: TContent;
  out haveFNC1: Boolean);
begin
  var latchTable := tUpper; // the table most recently latched to
  var shiftTable := tUpper; // the table for the next read
  var reader: TBitReader;
  reader.Bits := bits;
  reader.Pos := 0;
  haveFNC1 := false;

  // see ISO/IEC 24778:2008 7.3.1.2 about padding bits
  while true do
  begin
    var minBits := 5;
    if (shiftTable = tDigit) then
      minBits := 4;
    if (reader.Available < minBits) then
      break;

    if (shiftTable = tBinary) then
    begin
      // padding bits
      if (reader.Available <= 6) then
        break;
      var length := reader.ReadBits(5);
      if (length = 0) then
        length := reader.ReadBits(11) + 31;
      for var i := 0 to length - 1 do
        res.Append(Byte(reader.ReadBits(8)));
      // back to the table of before
      shiftTable := latchTable;
    end
    else
    begin
      var code := reader.ReadBits(minBits);
      var str := GetCharacter(shiftTable, code);
      if str.StartsWith('CTRL_') then
      begin
        // a table change; ISO/IEC 24778:2008 prescribes ending a shift
        // sequence in the mode from which it was invoked, also when that
        // is a shift (zxing-cpp issue 642)
        latchTable := shiftTable;
        shiftTable := GetTable(str.Chars[5]);
        if (str.Chars[6] = 'L') then
          latchTable := shiftTable;
      end
      else if (str = 'FLGN') then
      begin
        var flg := reader.ReadBits(3);
        if (flg = 0) then
        begin
          // FNC1 (may be removed at the end, at the first or second
          // position)
          haveFNC1 := true;
          res.Append(29);
        end
        else if (flg <= 6) then
        begin
          // FLG(1) to FLG(6): ECI of flg digits (ISO/IEC 24778:2008 10.1)
          var eci := 0;
          for var i := 1 to flg do
            eci := 10 * eci + reader.ReadBits(4) - 2;
          res.SwitchEncoding(eci);
        end;
        // FLG(7) is invalid
        shiftTable := latchTable;
      end
      else
      begin
        res.Append(str);
        // back to the table of before
        shiftTable := latchTable;
      end;
    end;
  end;
end;

function IsUpper(b: Byte): Boolean;
begin
  Result := (b >= Ord('A')) and (b <= Ord('Z'));
end;

function IsAlpha(b: Byte): Boolean;
begin
  Result := IsUpper(b) or ((b >= Ord('a')) and (b <= Ord('z')));
end;

function IsDigit(b: Byte): Boolean;
begin
  Result := (b >= Ord('0')) and (b <= Ord('9'));
end;

function DecodeBits(const bits: TArray<Boolean>; ecLevel: Integer;
  out error: string): TDecoderResult;
begin
  Result := nil;
  var res: TContent;
  res.HasECI := false;
  var haveFNC1: Boolean;
  try
    DecodeContent(bits, res, haveFNC1);
  except
    on ETruncated do
    begin
      error := 'Format';
      exit;
    end;
  end;
  if (Length(res.Bytes) = 0) then
  begin
    error := 'Format';
    exit;
  end;

  // Structured Append (ISO/IEC 24778:2008 section 8): 4 words of 5 bits,
  // beginning with ML UL, ending with index and count
  var saIndex := -1;
  var saCount := 0;
  if (Length(bits) > 20) then
  begin
    var reader: TBitReader;
    reader.Bits := bits;
    reader.Pos := 0;
    // latch to MIXED (from UPPER) and back to UPPER (from MIXED)
    if (reader.ReadBits(5) = 29) and (reader.ReadBits(5) = 29) then
    begin
      var i := 0;
      var ok := true;
      // a space delimited id
      if (res.Bytes[0] = Ord(' ')) then
      begin
        i := 1;
        while (i < Length(res.Bytes)) and (res.Bytes[i] <> Ord(' ')) do
          Inc(i);
        if (i >= Length(res.Bytes)) then
          ok := false
        else
          Inc(i);
      end;
      if ok and (i + 1 < Length(res.Bytes)) and IsUpper(res.Bytes[i]) and
        IsUpper(res.Bytes[i + 1]) then
      begin
        saIndex := res.Bytes[i] - Ord('A');
        saCount := res.Bytes[i + 1] - Ord('A') + 1;
        // information that makes no sense: count unknown
        if (saCount = 1) or (saCount <= saIndex) then
          saCount := 0;
        res.Erase(0, i + 2);
      end;
    end;
  end;
  if (Length(res.Bytes) = 0) then
  begin
    error := 'Format';
    exit;
  end;

  // ISO/IEC 15424: ]z0, ]z1 FNC1 first (GS1), ]z2 FNC1 after the AIM
  // application indicator, +3 with ECI, +6 with Structured Append
  var modifier := 0;
  if haveFNC1 then
  begin
    if (res.Bytes[0] = 29) then
    begin
      modifier := 1;
      res.Erase(0, 1);
    end
    else if (Length(res.Bytes) > 1) and IsAlpha(res.Bytes[0]) and
      (res.Bytes[1] = 29) then
    begin
      // the application indicator (a letter) stays in the text
      modifier := 2;
      res.Erase(1, 1);
    end
    else if (Length(res.Bytes) > 2) and IsDigit(res.Bytes[0]) and
      IsDigit(res.Bytes[1]) and (res.Bytes[2] = 29) then
    begin
      // the application indicator (2 digits) stays in the text
      modifier := 2;
      res.Erase(2, 1);
    end;
  end;
  if res.HasECI then
    Inc(modifier, 3);
  if (saIndex <> -1) then
    Inc(modifier, 6);

  var saSequence := -1;
  if (saIndex <> -1) then
    saSequence := (saIndex shl 4) or Max(saCount - 1, 0);
  Result := TDecoderResult.Create(res.Bytes, res.Text, nil,
    IntToStr(ecLevel) + '%', saSequence, -1);
  Result.SymbologyIdentifier := ']z' + Char(Ord('0') + modifier);
end;

function DecodeAztec(detected: TAztecDetectorResult;
  out error: string): TDecoderResult;
begin
  error := '';
  // an Aztec Rune: its value, as 3 digits
  if (detected.NbLayers = 0) then
  begin
    Result := TDecoderResult.Create(nil, Format('%.3d', [detected.RuneValue]),
      nil, '');
    Result.SymbologyIdentifier := ']zC';
    exit;
  end;

  Result := nil;
  var corrected: TArray<Boolean>;
  var ecLevel: Integer;
  if not CorrectBits(detected, ExtractBits(detected), corrected, ecLevel,
    error) then
    exit;
  Result := DecodeBits(corrected, ecLevel, error);
end;

end.
