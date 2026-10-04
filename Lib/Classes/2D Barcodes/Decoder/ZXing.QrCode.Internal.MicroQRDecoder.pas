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

  * Ported from zxing-cpp (QRDecoder.cpp, QRBitMatrixParser.cpp,
  * QRFormatInformation.cpp, QRVersion.cpp, QRCodecMode.cpp,
  * QRDataBlock.cpp, QRDataMask.h): the decoder of Micro QR Codes (ISO/IEC
  * 18004:2015) and rMQR Codes (ISO/IEC 23941:2022), apart from the QR Code
  * decoder so that that one stays as it is.
}

unit ZXing.QrCode.Internal.MicroQRDecoder;

interface

uses
  ZXing.Common.BitMatrix,
  ZXing.DecoderResult;

type
  TMicroQRType = (mqtMicro, mqtRMQR);

  /// <summary>The format information of a Micro QR or rMQR Code.</summary>
  TMicroQRFormat = record
    HammingDistance: Integer;
    // the version number (1 to 4 Micro, 1 to 32 rMQR)
    Version: Integer;
    // 0 L, 1 M, 2 Q, 3 H
    ECLevel: Integer;
    DataMask: Integer;
    IsMirrored: Boolean;
    function IsValid: Boolean;
  end;

const
  // the sizes of the rMQR versions (width x height)
  RMQR_WIDTHS: array [0 .. 31] of Integer = (43, 59, 77, 99, 139, 43, 59, 77,
    99, 139, 27, 43, 59, 77, 99, 139, 27, 43, 59, 77, 99, 139, 43, 59, 77, 99,
    139, 43, 59, 77, 99, 139);
  RMQR_HEIGHTS: array [0 .. 31] of Integer = (7, 7, 7, 7, 7, 9, 9, 9, 9, 9, 11,
    11, 11, 11, 11, 11, 13, 13, 13, 13, 13, 13, 15, 15, 15, 15, 15, 17, 17, 17,
    17, 17);

/// <summary>The format information of a Micro QR Code (15 bits, mask still
/// applied).</summary>
function DecodeMQRFormat(formatInfoBits: Cardinal): TMicroQRFormat;
/// <summary>The format information of an rMQR Code (18 bits each, mask
/// still applied; bits2 0 when there is only the first).</summary>
function DecodeRMQRFormat(bits1, bits2: Cardinal): TMicroQRFormat;

/// <summary>The rMQR version (1 to 32) of a size; 0 when there is none.
/// </summary>
function RMQRVersionOfSize(width, height: Integer): Integer;
function IsValidMicroSize(dimension: Integer): Boolean;

/// <summary>Decodes the modules of a Micro QR Code (11 to 17 square) or an
/// rMQR Code (rectangular). nil when that fails, with error 'Checksum' or
/// 'Format'.</summary>
function DecodeMicroQR(bits: TBitMatrix; out error: string): TDecoderResult;

implementation

uses
  System.SysUtils,
  System.Math,
  ZXing.Common.ECIContent,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder;

const
  FORMAT_INFO_MASK_MODEL2 = $5412;
  FORMAT_INFO_MASK_MICRO = $4445;
  FORMAT_INFO_MASK_RMQR = $1FAB2; // finder pattern side
  FORMAT_INFO_MASK_RMQR_SUB = $20A7B; // finder sub pattern side

  // ISO 18004:2015 annex C, table C.1
  MODEL2_MASKED_PATTERNS: array [0 .. 31] of Cardinal = ($5412, $5125, $5E7C,
    $5B4B, $45F9, $40CE, $4F97, $4AA0, $77C4, $72F3, $7DAA, $789D, $662F,
    $6318, $6C41, $6976, $1689, $13BE, $1CE7, $19D0, $0762, $0255, $0D0C,
    $083B, $355F, $3068, $3F31, $3A06, $24B4, $2183, $2EDA, $2BED);

  // ISO/IEC 23941:2022 annex C, table C.1: finder pattern side
  RMQR_MASKED_PATTERNS: array [0 .. 63] of Cardinal = ($1FAB2, $1E597, $1DBDD,
    $1C4F8, $1B86C, $1A749, $19903, $18626, $17F0E, $1602B, $15E61, $14144,
    $13DD0, $122F5, $11CBF, $1039A, $0F1CA, $0EEEF, $0D0A5, $0CF80, $0B314,
    $0AC31, $0927B, $08D5E, $07476, $06B53, $05519, $04A3C, $036A8, $0298D,
    $017C7, $008E2, $3F367, $3EC42, $3D208, $3CD2D, $3B1B9, $3AE9C, $390D6,
    $38FF3, $376DB, $369FE, $357B4, $34891, $33405, $32B20, $3156A, $30A4F,
    $2F81F, $2E73A, $2D970, $2C655, $2BAC1, $2A5E4, $29BAE, $2848B, $27DA3,
    $26286, $25CCC, $243E9, $23F7D, $22058, $21E12, $20137);
  // finder sub pattern side
  RMQR_MASKED_PATTERNS_SUB: array [0 .. 63] of Cardinal = ($20A7B, $2155E,
    $22B14, $23431, $248A5, $25780, $269CA, $276EF, $28FC7, $290E2, $2AEA8,
    $2B18D, $2CD19, $2D23C, $2EC76, $2F353, $30103, $31E26, $3206C, $33F49,
    $343DD, $35CF8, $362B2, $37D97, $384BF, $39B9A, $3A5D0, $3BAF5, $3C661,
    $3D944, $3E70E, $3F82B, $003AE, $01C8B, $022C1, $03DE4, $04170, $05E55,
    $0601F, $07F3A, $08612, $09937, $0A77D, $0B858, $0C4CC, $0DBE9, $0E5A3,
    $0FA86, $108D6, $117F3, $129B9, $1369C, $14A08, $1552D, $16B67, $17442,
    $18D6A, $1924F, $1AC05, $1B320, $1CFB4, $1D091, $1EEDB, $1F1FE);

type
  /// <summary>The error correction blocks of a version and level: the error
  /// correction codewords per block, and count1 blocks of data1 data
  /// codewords and count2 of data2.</summary>
  TECBlocks = record
    CodewordsPerBlock, Count1, Data1, Count2, Data2: Integer;
    function NumBlocks: Integer;
    function TotalCodewords: Integer;
  end;

  TCodecMode = (cmTerminator, cmNumeric, cmAlphanumeric, cmByte, cmKanji,
    cmFNC1First, cmFNC1Second, cmECI);

const
  // ISO 18004:2006 6.5.1 table 9: M1 to M4, levels L, M, Q
  MICRO_EC_BLOCKS: array [1 .. 4, 0 .. 2] of TECBlocks = (
    ((CodewordsPerBlock: 2; Count1: 1; Data1: 3; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 0; Count1: 0; Data1: 0; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 0; Count1: 0; Data1: 0; Count2: 0; Data2: 0)),
    ((CodewordsPerBlock: 5; Count1: 1; Data1: 5; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 6; Count1: 1; Data1: 4; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 0; Count1: 0; Data1: 0; Count2: 0; Data2: 0)),
    ((CodewordsPerBlock: 6; Count1: 1; Data1: 11; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 8; Count1: 1; Data1: 9; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 0; Count1: 0; Data1: 0; Count2: 0; Data2: 0)),
    ((CodewordsPerBlock: 8; Count1: 1; Data1: 16; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 10; Count1: 1; Data1: 14; Count2: 0; Data2: 0),
    (CodewordsPerBlock: 14; Count1: 1; Data1: 10; Count2: 0; Data2: 0)));

  // ISO/IEC 23941:2022 7.5.1 table 8: R1 to R32, levels M and H
  RMQR_EC_BLOCKS: array [1 .. 32, 0 .. 1, 0 .. 4] of Integer = (
    ((7, 1, 6, 0, 0), (10, 1, 3, 0, 0)), ((9, 1, 12, 0, 0), (14, 1, 7, 0, 0)),
    ((12, 1, 20, 0, 0), (22, 1, 10, 0, 0)), ((16, 1, 28, 0, 0),
    (30, 1, 14, 0, 0)), ((24, 1, 44, 0, 0), (22, 2, 12, 0, 0)),
    ((9, 1, 12, 0, 0), (14, 1, 7, 0, 0)), ((12, 1, 21, 0, 0), (22, 1, 11, 0, 0)),
    ((18, 1, 31, 0, 0), (16, 1, 8, 1, 9)), ((24, 1, 42, 0, 0),
    (22, 2, 11, 0, 0)), ((18, 1, 31, 1, 32), (22, 3, 11, 0, 0)),
    ((8, 1, 7, 0, 0), (10, 1, 5, 0, 0)), ((12, 1, 19, 0, 0), (20, 1, 11, 0, 0)),
    ((16, 1, 31, 0, 0), (16, 1, 7, 1, 8)), ((24, 1, 43, 0, 0),
    (22, 1, 11, 1, 12)), ((16, 1, 28, 1, 29), (30, 1, 14, 1, 15)),
    ((24, 2, 42, 0, 0), (30, 3, 14, 0, 0)), ((9, 1, 12, 0, 0),
    (14, 1, 7, 0, 0)), ((14, 1, 27, 0, 0), (28, 1, 13, 0, 0)),
    ((22, 1, 38, 0, 0), (20, 2, 10, 0, 0)), ((16, 1, 26, 1, 27),
    (28, 1, 14, 1, 15)), ((20, 1, 36, 1, 37), (26, 1, 11, 2, 12)),
    ((20, 2, 35, 1, 36), (28, 2, 13, 2, 14)), ((18, 1, 33, 0, 0),
    (18, 1, 7, 1, 8)), ((26, 1, 48, 0, 0), (24, 2, 13, 0, 0)),
    ((18, 1, 33, 1, 34), (24, 2, 10, 1, 11)), ((24, 2, 44, 0, 0),
    (22, 4, 12, 0, 0)), ((24, 2, 42, 1, 43), (26, 1, 13, 4, 14)),
    ((22, 1, 39, 0, 0), (20, 1, 10, 1, 11)), ((16, 2, 28, 0, 0),
    (30, 2, 14, 0, 0)), ((22, 2, 39, 0, 0), (28, 1, 12, 2, 13)),
    ((20, 2, 33, 1, 34), (26, 4, 14, 0, 0)), ((20, 4, 38, 0, 0),
    (26, 2, 12, 4, 13)));


  // ISO/IEC 23941:2022 7.4.1 table 3: the bits of the character counts
  RMQR_NUMERIC_BITS: array [0 .. 31] of Integer = (4, 5, 6, 7, 7, 5, 6, 7, 7,
    8, 4, 6, 7, 7, 8, 8, 5, 6, 7, 7, 8, 8, 7, 7, 8, 8, 9, 7, 8, 8, 8, 9);
  RMQR_ALPHANUM_BITS: array [0 .. 31] of Integer = (3, 5, 5, 6, 6, 5, 5, 6, 6,
    7, 4, 5, 6, 6, 7, 7, 5, 6, 6, 7, 7, 8, 6, 7, 7, 7, 8, 6, 7, 7, 8, 8);
  RMQR_BYTE_BITS: array [0 .. 31] of Integer = (3, 4, 5, 5, 6, 4, 5, 5, 6, 6,
    3, 5, 5, 6, 6, 7, 4, 5, 6, 6, 7, 7, 6, 6, 7, 7, 7, 6, 6, 7, 7, 8);
  RMQR_KANJI_BITS: array [0 .. 31] of Integer = (2, 3, 4, 5, 5, 3, 4, 5, 5, 6,
    2, 4, 5, 5, 6, 6, 3, 5, 5, 6, 6, 7, 5, 5, 6, 6, 7, 5, 6, 6, 6, 7);

  ALPHANUMERIC_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';
  EC_LEVEL_NAMES: array [0 .. 3] of string = ('L', 'M', 'Q', 'H');

  ECI_ISO8859_1 = 3;
  ECI_SHIFT_JIS = 20;

{ TMicroQRFormat }

function TMicroQRFormat.IsValid: Boolean;
begin
  // the Hamming distance of the masked codes is 7 (8 for rMQR): at most 3
  // bits differing is a match
  Result := (HammingDistance <= 3);
end;

{ TECBlocks }

function TECBlocks.NumBlocks: Integer;
begin
  Result := Count1 + Count2;
end;

function TECBlocks.TotalCodewords: Integer;
begin
  Result := Count1 * (Data1 + CodewordsPerBlock) + Count2 *
    (Data2 + CodewordsPerBlock);
end;

function RMQRBlocks(version, ecLevel: Integer): TECBlocks;
begin
  // (only M and H)
  var k := 0;
  if (ecLevel = 3) then
    k := 1;
  Result.CodewordsPerBlock := RMQR_EC_BLOCKS[version, k, 0];
  Result.Count1 := RMQR_EC_BLOCKS[version, k, 1];
  Result.Data1 := RMQR_EC_BLOCKS[version, k, 2];
  Result.Count2 := RMQR_EC_BLOCKS[version, k, 3];
  Result.Data2 := RMQR_EC_BLOCKS[version, k, 4];
end;

function RMQRAlignmentCenters(width: Integer): TArray<Integer>;
begin
  case width of
    43:
      Result := [21];
    59:
      Result := [19, 39];
    77:
      Result := [25, 51];
    99:
      Result := [23, 49, 75];
    139:
      Result := [27, 55, 83, 111];
  else
    Result := nil;
  end;
end;

function PopCount(v: Cardinal): Integer;
begin
  Result := 0;
  while (v <> 0) do
  begin
    Inc(Result, v and 1);
    v := v shr 1;
  end;
end;

function MirrorBits(bits: Cardinal): Cardinal;
begin
  // the lowest 15 bits reversed
  Result := 0;
  for var i := 0 to 14 do
    if ((bits shr i) and 1 = 1) then
      Result := Result or (1 shl (14 - i));
end;

function RMQRVersionOfSize(width, height: Integer): Integer;
begin
  for var i := 0 to High(RMQR_WIDTHS) do
    if (RMQR_WIDTHS[i] = width) and (RMQR_HEIGHTS[i] = height) then
      exit(i + 1);
  Result := 0;
end;

function IsValidMicroSize(dimension: Integer): Boolean;
begin
  Result := (dimension >= 11) and (dimension <= 17) and Odd(dimension);
end;

{ format information }

function DecodeMQRFormat(formatInfoBits: Cardinal): TMicroQRFormat;
const
  BITS_TO_VERSION: array [0 .. 7] of Integer = (1, 2, 2, 3, 3, 4, 4, 4);
  // L L M L M L M Q
  BITS_TO_LEVEL: array [0 .. 7] of Integer = (0, 0, 1, 0, 1, 0, 1, 2);
begin
  Result := Default(TMicroQRFormat);
  Result.HammingDistance := 255;
  var data: Cardinal := 255;
  var bitsIndex := 0;
  var bitsList: array [0 .. 1] of Cardinal;
  bitsList[0] := formatInfoBits;
  bitsList[1] := MirrorBits(formatInfoBits);
  for var bi := 0 to 1 do
    for var p in MODEL2_MASKED_PATTERNS do
    begin
      // 'unmask' the pattern: the 5 data bits and 10 error correction bits
      var pattern := p xor FORMAT_INFO_MASK_MODEL2;
      var dist := PopCount((bitsList[bi] xor FORMAT_INFO_MASK_MICRO) xor
        pattern);
      if (dist < Result.HammingDistance) then
      begin
        Result.HammingDistance := dist;
        data := pattern shr 10;
        bitsIndex := bi;
      end;
    end;
  // bits 2 to 4: error correction level and version, 0 and 1: mask
  Result.ECLevel := BITS_TO_LEVEL[(data shr 2) and 7];
  Result.DataMask := data and 3;
  Result.Version := BITS_TO_VERSION[(data shr 2) and 7];
  Result.IsMirrored := (bitsIndex = 1);
end;

function DecodeRMQRFormat(bits1, bits2: Cardinal): TMicroQRFormat;
var
  data: Cardinal;

  procedure best(bits: Cardinal; const patterns: array of Cardinal;
    mask: Cardinal; var res: TMicroQRFormat);
  begin
    for var p in patterns do
    begin
      // 'unmask' the pattern: the 6 data bits and 12 error correction bits
      var pattern := p xor mask;
      var dist := PopCount((bits xor mask) xor pattern);
      if (dist < res.HammingDistance) then
      begin
        res.HammingDistance := dist;
        data := pattern shr 12;
      end;
    end;
  end;

begin
  Result := Default(TMicroQRFormat);
  Result.HammingDistance := 255;
  data := 0;
  best(bits1, RMQR_MASKED_PATTERNS, FORMAT_INFO_MASK_RMQR, Result);
  if (bits2 <> 0) then
    best(bits2, RMQR_MASKED_PATTERNS_SUB, FORMAT_INFO_MASK_RMQR_SUB, Result);
  // bit 5: error correction level (M or H), bits 0 to 4: version
  if ((data shr 5) and 1 = 1) then
    Result.ECLevel := 3
  else
    Result.ECLevel := 1;
  Result.DataMask := 4; // ((y / 2) + (x / 3)) mod 2 = 0
  Result.Version := (data and $1F) + 1;
  Result.IsMirrored := false;
end;

{ reading the codewords }

function GetBit(bits: TBitMatrix; x, y: Integer; mirrored: Boolean): Boolean;
begin
  if mirrored then
    Result := bits[y, x]
  else
    Result := bits[x, y];
end;

function GetDataMaskBit(maskIndex, x, y: Integer; isMicro: Boolean): Boolean;
const
  MICRO_TO_QR: array [0 .. 3] of Integer = (1, 4, 6, 7);
begin
  if isMicro then
    maskIndex := MICRO_TO_QR[maskIndex and 3];
  case maskIndex of
    0:
      Result := (y + x) mod 2 = 0;
    1:
      Result := y mod 2 = 0;
    2:
      Result := x mod 3 = 0;
    3:
      Result := (y + x) mod 3 = 0;
    4:
      Result := ((y div 2) + (x div 3)) mod 2 = 0;
    5:
      Result := (y * x) mod 6 = 0;
    6:
      Result := ((y * x) mod 6) < 3;
  else
    Result := (y + x + ((y * x) mod 3)) mod 2 = 0;
  end;
end;

procedure SetRegion(m: TBitMatrix; left, top, width, height: Integer);
begin
  for var y := top to top + height - 1 do
    for var x := left to left + width - 1 do
      if (x >= 0) and (y >= 0) and (x < m.Width) and (y < m.Height) then
        m[x, y] := true;
end;

/// <summary>The function patterns of a Micro QR Code (ISO 18004:2006 annex
/// E).</summary>
function MicroFunctionPattern(dimension: Integer): TBitMatrix;
begin
  Result := TBitMatrix.Create(dimension, dimension);
  // the finder pattern, separator and format
  SetRegion(Result, 0, 0, 9, 9);
  // the timing patterns
  SetRegion(Result, 9, 0, dimension - 9, 1);
  SetRegion(Result, 0, 9, 1, dimension - 9);
end;

function RMQRFunctionPattern(width, height: Integer): TBitMatrix;
begin
  Result := TBitMatrix.Create(width, height);
  // the edge timing patterns
  SetRegion(Result, 0, 0, width, 1);
  SetRegion(Result, 0, height - 1, width, 1);
  SetRegion(Result, 0, 1, 1, height - 2);
  SetRegion(Result, width - 1, 1, 1, height - 2);
  // the vertical timing and alignment patterns
  for var cx in RMQRAlignmentCenters(width) do
  begin
    SetRegion(Result, cx - 1, 1, 3, 2);
    SetRegion(Result, cx - 1, height - 3, 3, 2);
    SetRegion(Result, cx, 3, 1, height - 6);
  end;
  // the top left finder pattern and separator (R7: the finder pattern flush
  // with the bottom edge) and format
  SetRegion(Result, 1, 1, 8 - 1, 8 - 1 - Ord(height = 7));
  SetRegion(Result, 8, 1, 3, 5);
  SetRegion(Result, 11, 1, 1, 3);
  // the bottom right finder sub pattern and format
  SetRegion(Result, width - 5, height - 5, 5 - 1, 5 - 1);
  SetRegion(Result, width - 8, height - 6, 3, 5);
  SetRegion(Result, width - 5, height - 6, 3, 1);
  // the top right and bottom left corner finders
  Result[width - 2, 1] := true;
  if (height > 9) then
    Result[1, height - 2] := true;
end;

function ReadMQRCodewords(bits: TBitMatrix; version, ecLevel: Integer;
  const fi: TMicroQRFormat; total: Integer): TBytes;
begin
  Result := nil;
  var dimension := bits.Height;
  var functionPattern := MicroFunctionPattern(dimension);
  try
    // D3 of M1, D11 of M3-L and D9 of M3-M is a 2x2 block of 4 modules
    // (ISO 18004:2006 6.7.3)
    var hasD4mBlock := Odd(version);
    var d4mBlockIndex := 9;
    if (version = 1) then
      d4mBlockIndex := 3
    else if (ecLevel = 0) then
      d4mBlockIndex := 11;

    var currentByte := 0;
    var readingUp := true;
    var bitsRead := 0;
    var x := dimension - 1;
    while (x > 0) do
    begin
      for var row := 0 to dimension - 1 do
      begin
        var y := row;
        if readingUp then
          y := dimension - 1 - row;
        for var col := 0 to 1 do
        begin
          var xx := x - col;
          if not functionPattern[xx, y] then
          begin
            currentByte := ((currentByte shl 1) or
              Ord(GetDataMaskBit(fi.DataMask, xx, y, true) <>
              GetBit(bits, xx, y, fi.IsMirrored))) and $FF;
            Inc(bitsRead);
            // a whole byte; early for the 2x2 data block
            if (bitsRead = 8) or (bitsRead = 4) and hasD4mBlock and
              (Length(Result) = d4mBlockIndex - 1) then
            begin
              Result := Result + [Byte(currentByte)];
              currentByte := 0;
              bitsRead := 0;
            end;
          end;
        end;
      end;
      readingUp := not readingUp;
      Dec(x, 2);
    end;
  finally
    functionPattern.Free;
  end;
  if (Length(Result) <> total) then
    Result := nil;
end;

function ReadRMQRCodewords(bits: TBitMatrix; const fi: TMicroQRFormat;
  total: Integer): TBytes;
begin
  Result := nil;
  var width := bits.Width;
  var height := bits.Height;
  var functionPattern := RMQRFunctionPattern(width, height);
  try
    var currentByte := 0;
    var readingUp := true;
    var bitsRead := 0;
    // (the right edge is skipped)
    var x := width - 1 - 1;
    while (x > 0) do
    begin
      for var row := 0 to height - 1 do
      begin
        var y := row;
        if readingUp then
          y := height - 1 - row;
        for var col := 0 to 1 do
        begin
          var xx := x - col;
          if not functionPattern[xx, y] then
          begin
            currentByte := ((currentByte shl 1) or
              Ord(GetDataMaskBit(fi.DataMask, xx, y, false) <>
              GetBit(bits, xx, y, fi.IsMirrored))) and $FF;
            Inc(bitsRead);
            if (bitsRead mod 8 = 0) then
            begin
              Result := Result + [Byte(currentByte)];
              currentByte := 0;
            end;
          end;
        end;
      end;
      readingUp := not readingUp;
      Dec(x, 2);
    end;
  finally
    functionPattern.Free;
  end;
  if (Length(Result) <> total) then
    Result := nil;
end;

{ the bit stream }

type
  EMicroQRFormat = class(Exception);

  TBitReader = record
    Bytes: TBytes;
    Pos: Integer; // in bits
    function Available: Integer;
    function PeekBits(n: Integer): Integer;
    function ReadBits(n: Integer): Integer;
  end;

function TBitReader.Available: Integer;
begin
  Result := 8 * Length(Bytes) - Pos;
end;

function TBitReader.PeekBits(n: Integer): Integer;
begin
  if (n > Available) then
    raise EMicroQRFormat.Create('Truncated bit stream');
  Result := 0;
  for var i := Pos to Pos + n - 1 do
    Result := (Result shl 1) or ((Bytes[i shr 3] shr (7 - (i and 7))) and 1);
end;

function TBitReader.ReadBits(n: Integer): Integer;
begin
  Result := PeekBits(n);
  Inc(Pos, n);
end;

function CodecModeForBits(bits: Integer; t: TMicroQRType): TCodecMode;
const
  MICRO_MODES: array [0 .. 3] of TCodecMode = (cmNumeric, cmAlphanumeric,
    cmByte, cmKanji);
  RMQR_MODES: array [0 .. 7] of TCodecMode = (cmTerminator, cmNumeric,
    cmAlphanumeric, cmByte, cmKanji, cmFNC1First, cmFNC1Second, cmECI);
begin
  if (t = mqtMicro) then
  begin
    if (bits < 0) or (bits > 3) then
      raise EMicroQRFormat.Create('Invalid codec mode');
    Result := MICRO_MODES[bits];
  end
  else
  begin
    if (bits < 0) or (bits > 7) then
      raise EMicroQRFormat.Create('Invalid codec mode');
    Result := RMQR_MODES[bits];
  end;
end;

function CharacterCountBits(mode: TCodecMode; t: TMicroQRType;
  version: Integer): Integer;
const
  MICRO_NUMERIC: array [1 .. 4] of Integer = (3, 4, 5, 6);
  MICRO_ALPHANUM: array [2 .. 4] of Integer = (3, 4, 5);
  MICRO_BYTE: array [3 .. 4] of Integer = (4, 5);
  MICRO_KANJI: array [3 .. 4] of Integer = (3, 4);
begin
  Result := 0;
  if (t = mqtMicro) then
    case mode of
      cmNumeric:
        Result := MICRO_NUMERIC[version];
      cmAlphanumeric:
        if (version >= 2) then
          Result := MICRO_ALPHANUM[version];
      cmByte:
        if (version >= 3) then
          Result := MICRO_BYTE[version];
      cmKanji:
        if (version >= 3) then
          Result := MICRO_KANJI[version];
    end
  else
    case mode of
      cmNumeric:
        Result := RMQR_NUMERIC_BITS[version - 1];
      cmAlphanumeric:
        Result := RMQR_ALPHANUM_BITS[version - 1];
      cmByte:
        Result := RMQR_BYTE_BITS[version - 1];
      cmKanji:
        Result := RMQR_KANJI_BITS[version - 1];
    end;
end;

function ParseECIValue(var bits: TBitReader): Integer;
begin
  var firstByte := bits.ReadBits(8);
  if ((firstByte and $80) = 0) then
    exit(firstByte and $7F);
  if ((firstByte and $C0) = $80) then
    exit(((firstByte and $3F) shl 8) or bits.ReadBits(8));
  if ((firstByte and $E0) = $C0) then
    exit(((firstByte and $1F) shl 16) or bits.ReadBits(16));
  raise EMicroQRFormat.Create('Invalid ECI value');
end;

function DecodeBitStream(const bytes: TBytes; t: TMicroQRType;
  version, ecLevel: Integer; out error: string): TDecoderResult;
begin
  Result := nil;
  var bits: TBitReader;
  bits.Bytes := bytes;
  bits.Pos := 0;
  var res := TECIContent.Create;
  var modifier := 1;
  var aiFlag := false;
  var modeBitLength := 3; // rMQR
  var terminatorBitLength := 3;
  if (t = mqtMicro) then
  begin
    modeBitLength := version - 1;
    terminatorBitLength := version * 2 + 1;
  end;

  try
    while true do
    begin
      // the end: no bits left or the (shorter) terminator
      var bitsAvailable := Min(bits.Available, terminatorBitLength);
      if (bitsAvailable = 0) or (bits.PeekBits(bitsAvailable) = 0) then
        break;

      var mode := cmNumeric; // M1 is always numeric (no mode bits)
      if (modeBitLength > 0) then
        mode := CodecModeForBits(bits.ReadBits(modeBitLength), t);

      case mode of
        cmFNC1First:
          begin
            modifier := 3;
            aiFlag := true;
          end;
        cmFNC1Second:
          begin
            if not res.IsEmpty then
              raise EMicroQRFormat.Create('AIM application indicator');
            modifier := 5;
            // ISO/IEC 18004:2015 7.4.8.3: '00' to '99' or 'A' to 'z'
            var appInd := bits.ReadBits(8);
            if (appInd < 100) then
              res.Append(Format('%.2d', [appInd]))
            else if (appInd >= 165) and (appInd <= 190) or (appInd >= 197) and
              (appInd <= 222) then
              res.Append(Byte(appInd - 100))
            else
              raise EMicroQRFormat.Create('Invalid AIM application indicator');
            aiFlag := true;
          end;
        cmECI:
          res.SwitchEncoding(ParseECIValue(bits));
        cmTerminator:
          break;
      else
        begin
          var count := bits.ReadBits(CharacterCountBits(mode, t, version));
          case mode of
            cmNumeric:
              begin
                res.SwitchCharset(ECI_ISO8859_1);
                while (count > 0) do
                begin
                  var n := Min(count, 3);
                  // 4, 7 or 10 bits for 1, 2 or 3 digits
                  var digits := bits.ReadBits(1 + 3 * n);
                  res.Append(Format('%.*d', [n, digits]));
                  Dec(count, n);
                end;
              end;
            cmAlphanumeric:
              begin
                var buffer := '';
                while (count > 1) do
                begin
                  var v := bits.ReadBits(11);
                  if (v div 45 >= 45) then
                    raise EMicroQRFormat.Create('Invalid alphanumeric');
                  buffer := buffer + ALPHANUMERIC_CHARS[v div 45 + 1] +
                    ALPHANUMERIC_CHARS[v mod 45 + 1];
                  Dec(count, 2);
                end;
                if (count = 1) then
                begin
                  var v := bits.ReadBits(6);
                  if (v >= 45) then
                    raise EMicroQRFormat.Create('Invalid alphanumeric');
                  buffer := buffer + ALPHANUMERIC_CHARS[v + 1];
                end;
                // FNC1 mode: %% is %, % is the separator GS (6.4.8.1)
                if aiFlag then
                begin
                  var s := '';
                  var i := 1;
                  while (i <= Length(buffer)) do
                  begin
                    if (buffer[i] = '%') then
                    begin
                      if (i < Length(buffer)) and (buffer[i + 1] = '%') then
                      begin
                        s := s + '%';
                        Inc(i);
                      end
                      else
                        s := s + #29;
                    end
                    else
                      s := s + buffer[i];
                    Inc(i);
                  end;
                  buffer := s;
                end;
                res.SwitchCharset(ECI_ISO8859_1);
                res.Append(buffer);
              end;
            cmByte:
              begin
                res.SwitchCharset(-1);
                for var i := 1 to count do
                  res.Append(Byte(bits.ReadBits(8)));
              end;
            cmKanji:
              begin
                res.SwitchCharset(ECI_SHIFT_JIS);
                for var i := 1 to count do
                begin
                  // 13 bits per 2 byte character
                  var twoBytes := bits.ReadBits(13);
                  var assembled := ((twoBytes div $0C0) shl 8) or
                    (twoBytes mod $0C0);
                  if (assembled < $01F00) then
                    Inc(assembled, $08140)
                  else
                    Inc(assembled, $0C140);
                  res.Append(Byte(assembled shr 8));
                  res.Append(Byte(assembled));
                end;
              end;
          else
            raise EMicroQRFormat.Create('Invalid codec mode');
          end;
        end;
      end;
    end;
  except
    on Exception do
    begin
      error := 'Format';
      exit;
    end;
  end;

  if res.HasECI then
    Inc(modifier);
  Result := TDecoderResult.Create(res.Bytes, res.Text, nil,
    EC_LEVEL_NAMES[ecLevel]);
  Result.SymbologyIdentifier := ']Q' + Char(Ord('0') + modifier);
end;

function DecodeMicroQR(bits: TBitMatrix; out error: string): TDecoderResult;
begin
  Result := nil;
  error := 'Format';
  var width := bits.Width;
  var height := bits.Height;
  var t: TMicroQRType;
  var fi: TMicroQRFormat;
  var version: Integer;
  var blocks: TECBlocks;

  if (width = height) and IsValidMicroSize(width) then
  begin
    t := mqtMicro;
    // the top left format information
    var formatInfoBits: Cardinal := 0;
    for var x := 1 to 8 do
      formatInfoBits := (formatInfoBits shl 1) or Cardinal(Ord(bits[x, 8]));
    for var y := 7 downto 1 do
      formatInfoBits := (formatInfoBits shl 1) or Cardinal(Ord(bits[8, y]));
    fi := DecodeMQRFormat(formatInfoBits);
    version := (width - 9) div 2;
    // (the version from the size, like zxing-cpp)
    if not fi.IsValid then
      exit;
    blocks := MICRO_EC_BLOCKS[version, Min(fi.ECLevel, 2)];
  end
  else if (width <> height) and (RMQRVersionOfSize(width, height) > 0) then
  begin
    t := mqtRMQR;
    // the top left format information
    var bits1: Cardinal := 0;
    for var y := 3 downto 1 do
      bits1 := (bits1 shl 1) or Cardinal(Ord(bits[11, y]));
    for var x := 10 downto 8 do
      for var y := 5 downto 1 do
        bits1 := (bits1 shl 1) or Cardinal(Ord(bits[x, y]));
    // the bottom right format information
    var bits2: Cardinal := 0;
    for var x := 3 to 5 do
      bits2 := (bits2 shl 1) or Cardinal(Ord(bits[width - x, height - 6]));
    for var x := 6 to 8 do
      for var y := 2 to 6 do
        bits2 := (bits2 shl 1) or Cardinal(Ord(bits[width - x, height - y]));
    fi := DecodeRMQRFormat(bits1, bits2);
    version := RMQRVersionOfSize(width, height);
    // (the version from the size, like zxing-cpp)
    if not fi.IsValid then
      exit;
    blocks := RMQRBlocks(version, fi.ECLevel);
  end
  else
    exit;

  if (blocks.NumBlocks = 0) then
    exit;
  var total := blocks.TotalCodewords;
  var codewords: TBytes;
  if (t = mqtMicro) then
    codewords := ReadMQRCodewords(bits, version, fi.ECLevel, fi, total)
  else
    codewords := ReadRMQRCodewords(bits, fi, total);
  if (codewords = nil) then
    exit;

  // the data blocks (interleaved; the last ones may be 1 longer)
  var numBlocks := blocks.NumBlocks;
  var blockData: TArray<Integer>;
  var blockCodewords: TArray<TArray<Integer>>;
  SetLength(blockData, numBlocks);
  SetLength(blockCodewords, numBlocks);
  for var j := 0 to numBlocks - 1 do
  begin
    if (j < blocks.Count1) then
      blockData[j] := blocks.Data1
    else
      blockData[j] := blocks.Data2;
    SetLength(blockCodewords[j], blockData[j] + blocks.CodewordsPerBlock);
  end;
  var shorterTotal := Length(blockCodewords[0]);
  var longerStart := numBlocks - 1;
  while (longerStart >= 0) and (Length(blockCodewords[longerStart]) <>
    shorterTotal) do
    Dec(longerStart);
  Inc(longerStart);
  var shorterData := shorterTotal - blocks.CodewordsPerBlock;
  var offset := 0;
  for var i := 0 to shorterData - 1 do
    for var j := 0 to numBlocks - 1 do
    begin
      blockCodewords[j][i] := codewords[offset];
      Inc(offset);
    end;
  for var j := longerStart to numBlocks - 1 do
  begin
    blockCodewords[j][shorterData] := codewords[offset];
    Inc(offset);
  end;
  for var i := shorterData to shorterTotal - 1 do
    for var j := 0 to numBlocks - 1 do
    begin
      var iOffset := i;
      if (j >= longerStart) then
        iOffset := i + 1;
      blockCodewords[j][iOffset] := codewords[offset];
      Inc(offset);
    end;

  // error correction
  var data: TBytes := nil;
  var rs := TReedSolomonDecoder.Create(TGenericGF.QR_CODE_FIELD_256);
  try
    for var j := 0 to numBlocks - 1 do
    begin
      if not rs.decode(blockCodewords[j], Length(blockCodewords[j]) -
        blockData[j]) then
      begin
        error := 'Checksum';
        exit;
      end;
      for var i := 0 to blockData[j] - 1 do
        data := data + [Byte(blockCodewords[j][i])];
    end;
  finally
    rs.Free;
  end;

  Result := DecodeBitStream(data, t, version, fi.ECLevel, error);
  if (Result <> nil) then
  begin
    Result.IsMirrored := fi.IsMirrored;
    error := '';
  end;
end;

end.
