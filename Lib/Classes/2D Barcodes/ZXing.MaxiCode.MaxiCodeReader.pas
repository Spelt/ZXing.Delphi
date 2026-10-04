{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
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

  * Ported from zxing-cpp (MCReader.cpp, MCBitMatrixParser.cpp,
  * MCDecoder.cpp), originally by mike32767 and Manuel Kasten: MaxiCode
  * (ISO/IEC 16023). Like zxing-cpp only symbols that fill the image
  * ("pure", unrotated) are read: there is no detector yet.
}

unit ZXing.MaxiCode.MaxiCodeReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.DecoderResult,
  ZXing.BinaryBitmap,
  ZXing.Common.BitMatrix;

type
  /// <summary>
  /// Reads MaxiCode symbols that fill the image (unrotated).
  /// </summary>
  TMaxiCodeReader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>Decodes the 30 x 33 modules of a MaxiCode (odd rows shifted
/// half a module to the right); nil with error 'Checksum' or 'Format'.
/// </summary>
function DecodeMaxiCode(bits: TBitMatrix; out error: string): TDecoderResult;

implementation

uses
  System.Math,
  ZXing.ResultPoint,
  ZXing.ResultMetadataType,
  ZXing.Common.ECIContent,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder;

const
  MATRIX_WIDTH = 30;
  MATRIX_HEIGHT = 33;

  // the bit of each module (-1 to -3: no data)
  BITNR: array [0 .. MATRIX_HEIGHT - 1, 0 .. MATRIX_WIDTH - 1] of Integer = (
    (121,120,127,126,133,132,139,138,145,144,151,150,157,156,163,162,169,168,175,174,181,180,187,186,193,192,199,198, -2, -2),
    (123,122,129,128,135,134,141,140,147,146,153,152,159,158,165,164,171,170,177,176,183,182,189,188,195,194,201,200,816, -3),
    (125,124,131,130,137,136,143,142,149,148,155,154,161,160,167,166,173,172,179,178,185,184,191,190,197,196,203,202,818,817),
    (283,282,277,276,271,270,265,264,259,258,253,252,247,246,241,240,235,234,229,228,223,222,217,216,211,210,205,204,819, -3),
    (285,284,279,278,273,272,267,266,261,260,255,254,249,248,243,242,237,236,231,230,225,224,219,218,213,212,207,206,821,820),
    (287,286,281,280,275,274,269,268,263,262,257,256,251,250,245,244,239,238,233,232,227,226,221,220,215,214,209,208,822, -3),
    (289,288,295,294,301,300,307,306,313,312,319,318,325,324,331,330,337,336,343,342,349,348,355,354,361,360,367,366,824,823),
    (291,290,297,296,303,302,309,308,315,314,321,320,327,326,333,332,339,338,345,344,351,350,357,356,363,362,369,368,825, -3),
    (293,292,299,298,305,304,311,310,317,316,323,322,329,328,335,334,341,340,347,346,353,352,359,358,365,364,371,370,827,826),
    (409,408,403,402,397,396,391,390, 79, 78, -2, -2, 13, 12, 37, 36,  2, -1, 44, 43,109,108,385,384,379,378,373,372,828, -3),
    (411,410,405,404,399,398,393,392, 81, 80, 40, -2, 15, 14, 39, 38,  3, -1, -1, 45,111,110,387,386,381,380,375,374,830,829),
    (413,412,407,406,401,400,395,394, 83, 82, 41, -3, -3, -3, -3, -3,  5,  4, 47, 46,113,112,389,388,383,382,377,376,831, -3),
    (415,414,421,420,427,426,103,102, 55, 54, 16, -3, -3, -3, -3, -3, -3, -3, 20, 19, 85, 84,433,432,439,438,445,444,833,832),
    (417,416,423,422,429,428,105,104, 57, 56, -3, -3, -3, -3, -3, -3, -3, -3, 22, 21, 87, 86,435,434,441,440,447,446,834, -3),
    (419,418,425,424,431,430,107,106, 59, 58, -3, -3, -3, -3, -3, -3, -3, -3, -3, 23, 89, 88,437,436,443,442,449,448,836,835),
    (481,480,475,474,469,468, 48, -2, 30, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,  0, 53, 52,463,462,457,456,451,450,837, -3),
    (483,482,477,476,471,470, 49, -1, -2, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -2, -1,465,464,459,458,453,452,839,838),
    (485,484,479,478,473,472, 51, 50, 31, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,  1, -2, 42,467,466,461,460,455,454,840, -3),
    (487,486,493,492,499,498, 97, 96, 61, 60, -3, -3, -3, -3, -3, -3, -3, -3, -3, 26, 91, 90,505,504,511,510,517,516,842,841),
    (489,488,495,494,501,500, 99, 98, 63, 62, -3, -3, -3, -3, -3, -3, -3, -3, 28, 27, 93, 92,507,506,513,512,519,518,843, -3),
    (491,490,497,496,503,502,101,100, 65, 64, 17, -3, -3, -3, -3, -3, -3, -3, 18, 29, 95, 94,509,508,515,514,521,520,845,844),
    (559,558,553,552,547,546,541,540, 73, 72, 32, -3, -3, -3, -3, -3, -3, 10, 67, 66,115,114,535,534,529,528,523,522,846, -3),
    (561,560,555,554,549,548,543,542, 75, 74, -2, -1,  7,  6, 35, 34, 11, -2, 69, 68,117,116,537,536,531,530,525,524,848,847),
    (563,562,557,556,551,550,545,544, 77, 76, -2, 33,  9,  8, 25, 24, -1, -2, 71, 70,119,118,539,538,533,532,527,526,849, -3),
    (565,564,571,570,577,576,583,582,589,588,595,594,601,600,607,606,613,612,619,618,625,624,631,630,637,636,643,642,851,850),
    (567,566,573,572,579,578,585,584,591,590,597,596,603,602,609,608,615,614,621,620,627,626,633,632,639,638,645,644,852, -3),
    (569,568,575,574,581,580,587,586,593,592,599,598,605,604,611,610,617,616,623,622,629,628,635,634,641,640,647,646,854,853),
    (727,726,721,720,715,714,709,708,703,702,697,696,691,690,685,684,679,678,673,672,667,666,661,660,655,654,649,648,855, -3),
    (729,728,723,722,717,716,711,710,705,704,699,698,693,692,687,686,681,680,675,674,669,668,663,662,657,656,651,650,857,856),
    (731,730,725,724,719,718,713,712,707,706,701,700,695,694,689,688,683,682,677,676,671,670,665,664,659,658,653,652,858, -3),
    (733,732,739,738,745,744,751,750,757,756,763,762,769,768,775,774,781,780,787,786,793,792,799,798,805,804,811,810,860,859),
    (735,734,741,740,747,746,753,752,759,758,765,764,771,770,777,776,783,782,789,788,795,794,801,800,807,806,813,812,861, -3),
    (737,736,743,742,749,748,755,754,761,760,767,766,773,772,779,778,785,784,791,790,797,796,803,802,809,808,815,814,863,862)
  );

  CORRECT_ALL = 0;
  CORRECT_EVEN = 1;
  CORRECT_ODD = 2;

  SHI0 = $100;
  SHI1 = $101;
  SHI2 = $102;
  SHI3 = $103;
  SHI4 = $104;
  TWSA = $105; // two shift A
  TRSA = $106; // three shift A
  LCHA = $107; // latch A
  LCHB = $108; // latch B
  LOCK = $109;
  ECI = $10A;
  NS = $10B;
  PAD = $10C;

  FS = $1C;
  GS = $1D;
  RS = $1E;

  CHARSETS: array [0 .. 4, 0 .. $3F] of Integer = (
    ( // set 0 (A)
    13, Ord('A'), Ord('B'), Ord('C'), Ord('D'), Ord('E'), Ord('F'), Ord('G'),
    Ord('H'), Ord('I'), Ord('J'), Ord('K'), Ord('L'), Ord('M'), Ord('N'),
    Ord('O'), Ord('P'), Ord('Q'), Ord('R'), Ord('S'), Ord('T'), Ord('U'),
    Ord('V'), Ord('W'), Ord('X'), Ord('Y'), Ord('Z'), ECI, FS, GS, RS, NS,
    Ord(' '), PAD, Ord('"'), Ord('#'), Ord('$'), Ord('%'), Ord('&'), Ord(''''),
    Ord('('), Ord(')'), Ord('*'), Ord('+'), Ord(','), Ord('-'), Ord('.'),
    Ord('/'), Ord('0'), Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5'),
    Ord('6'), Ord('7'), Ord('8'), Ord('9'), Ord(':'), SHI1, SHI2, SHI3, SHI4,
    LCHB),
    ( // set 1 (B)
    Ord('`'), Ord('a'), Ord('b'), Ord('c'), Ord('d'), Ord('e'), Ord('f'),
    Ord('g'), Ord('h'), Ord('i'), Ord('j'), Ord('k'), Ord('l'), Ord('m'),
    Ord('n'), Ord('o'), Ord('p'), Ord('q'), Ord('r'), Ord('s'), Ord('t'),
    Ord('u'), Ord('v'), Ord('w'), Ord('x'), Ord('y'), Ord('z'), ECI, FS, GS,
    RS, NS, Ord('{'), PAD, Ord('}'), Ord('~'), $7F, Ord(';'), Ord('<'),
    Ord('='), Ord('>'), Ord('?'), Ord('['), Ord('\'), Ord(']'), Ord('^'),
    Ord('_'), Ord(' '), Ord(','), Ord('.'), Ord('/'), Ord(':'), Ord('@'),
    Ord('!'), Ord('|'), PAD, TWSA, TRSA, PAD, SHI0, SHI2, SHI3, SHI4, LCHA),
    ( // set 2 (C)
    $C0, $C1, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $CB, $CC, $CD, $CE,
    $CF, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA, ECI, FS, GS,
    RS, NS, $DB, $DC, $DD, $DE, $DF, $AA, $AC, $B1, $B2, $B3, $B5, $B9, $BA,
    $BC, $BD, $BE, $80, $81, $82, $83, $84, $85, $86, $87, $88, $89, LCHA,
    $20, LOCK, SHI3, SHI4, LCHB),
    ( // set 3 (D)
    $E0, $E1, $E2, $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA, $EB, $EC, $ED, $EE,
    $EF, $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA, ECI, FS, GS,
    RS, NS, $FB, $FC, $FD, $FE, $FF, $A1, $A8, $AB, $AF, $B0, $B4, $B7, $B8,
    $BB, $BF, $8A, $8B, $8C, $8D, $8E, $8F, $90, $91, $92, $93, $94, LCHA,
    $20, SHI2, LOCK, SHI4, LCHB),
    ( // set 4 (E)
    $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
    $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, ECI, PAD, PAD,
    $1B, NS, FS, GS, RS, $1F, $9F, $A0, $A2, $A3, $A4, $A5, $A6, $A7, $A9,
    $AD, $AE, $B6, $95, $96, $97, $98, $99, $9A, $9B, $9C, $9D, $9E, LCHA,
    $20, SHI2, SHI3, LOCK, LCHB));

type
  EMaxiCodeFormat = class(Exception);

function ReadCodewords(image: TBitMatrix): TBytes;
begin
  SetLength(Result, 144);
  for var i := 0 to High(Result) do
    Result[i] := 0;
  for var y := 0 to image.Height - 1 do
    for var x := 0 to image.Width - 1 do
    begin
      var bit := BITNR[y, x];
      if (bit >= 0) and image[x, y] then
        Result[bit div 6] := Result[bit div 6] or (1 shl (5 - (bit mod 6)));
    end;
end;

function CorrectErrors(var codewordBytes: TBytes; start, dataCodewords,
  ecCodewords, mode: Integer): Boolean;
begin
  var codewords := dataCodewords + ecCodewords;
  // in the EVEN or ODD mode only half of the codewords
  var divisor := 1;
  if (mode <> CORRECT_ALL) then
    divisor := 2;

  var ints: TArray<Integer>;
  SetLength(ints, codewords div divisor);
  for var i := 0 to High(ints) do
    ints[i] := 0;
  for var i := 0 to codewords - 1 do
    if (mode = CORRECT_ALL) or (i mod 2 = mode - 1) then
      ints[i div divisor] := codewordBytes[i + start];

  var rs := TReedSolomonDecoder.Create(TGenericGF.MAXICODE_FIELD_64);
  try
    if not rs.decode(ints, ecCodewords div divisor) then
      exit(false);
  finally
    rs.Free;
  end;

  // only the data codewords are copied back
  for var i := 0 to dataCodewords - 1 do
    if (mode = CORRECT_ALL) or (i mod 2 = mode - 1) then
      codewordBytes[i + start] := Byte(ints[i div divisor]);
  Result := true;
end;

function GetBit(bit: Integer; const bytes: TBytes): Integer;
begin
  Dec(bit);
  if (bytes[bit div 6] and (1 shl (5 - (bit mod 6))) = 0) then
    Result := 0
  else
    Result := 1;
end;

function GetInt(const bytes: TBytes; const x: array of Integer): Cardinal;
begin
  Result := 0;
  var len := Length(x);
  for var i := 0 to len - 1 do
    Inc(Result, Cardinal(GetBit(x[i], bytes)) shl (len - i - 1));
end;

function GetPostCode2(const bytes: TBytes): string;
begin
  var val := GetInt(bytes, [33, 34, 35, 36, 25, 26, 27, 28, 29, 30, 19, 20, 21,
    22, 23, 24, 13, 14, 15, 16, 17, 18, 7, 8, 9, 10, 11, 12, 1, 2]);
  var len := Integer(Min(GetInt(bytes, [39, 40, 41, 42, 31, 32]), 9));
  // padded or truncated to the length
  Result := Copy(Format('%.*d', [len, val]), 1, len);
end;

function GetPostCode3(const bytes: TBytes): string;

  function c(const x: array of Integer): Char;
  begin
    Result := Char(CHARSETS[0, GetInt(bytes, x)]);
  end;

begin
  Result := c([39, 40, 41, 42, 31, 32]) + c([33, 34, 35, 36, 25, 26]) +
    c([27, 28, 29, 30, 19, 20]) + c([21, 22, 23, 24, 13, 14]) +
    c([15, 16, 17, 18, 7, 8]) + c([9, 10, 11, 12, 1, 2]);
end;

/// <summary>ISO/IEC 16023:2000 section 4.6 table 3.</summary>
function ParseECIValue(const bytes: TBytes; var i: Integer): Integer;
begin
  Inc(i);
  if (i >= Length(bytes)) then
    raise EMaxiCodeFormat.Create('Truncated');
  var firstByte := bytes[i];
  if ((firstByte and $20) = 0) then
    exit(firstByte);
  Inc(i);
  if (i >= Length(bytes)) then
    raise EMaxiCodeFormat.Create('Truncated');
  var secondByte := bytes[i];
  if ((firstByte and $10) = 0) then
    exit(((firstByte and $0F) shl 6) or secondByte);
  Inc(i);
  if (i >= Length(bytes)) then
    raise EMaxiCodeFormat.Create('Truncated');
  var thirdByte := bytes[i];
  if ((firstByte and $08) = 0) then
    exit(((firstByte and $07) shl 12) or (secondByte shl 6) or thirdByte);
  Inc(i);
  if (i >= Length(bytes)) then
    raise EMaxiCodeFormat.Create('Truncated');
  Result := ((firstByte and $03) shl 18) or (secondByte shl 12) or
    (thirdByte shl 6) or bytes[i];
end;

procedure GetMessage(const bytes: TBytes; start, len: Integer;
  var res: TECIContent; var saIndex, saCount: Integer);
begin
  var shift := -1;
  var cset := 0;
  var lastset := 0;
  var i := start;
  while (i < start + len) do
  begin
    var c := CHARSETS[cset, bytes[i] and $3F];
    case c of
      LCHA:
        begin
          cset := 0;
          shift := -1;
        end;
      LCHB:
        begin
          cset := 1;
          shift := -1;
        end;
      SHI0, SHI1, SHI2, SHI3, SHI4:
        begin
          lastset := cset;
          cset := c - SHI0;
          shift := 1;
        end;
      TWSA:
        begin
          lastset := cset;
          cset := 0;
          shift := 2;
        end;
      TRSA:
        begin
          lastset := cset;
          cset := 0;
          shift := 3;
        end;
      NS:
        begin
          if (i + 5 >= Length(bytes)) then
            raise EMaxiCodeFormat.Create('Truncated');
          var v := (bytes[i + 1] shl 24) + (bytes[i + 2] shl 18) +
            (bytes[i + 3] shl 12) + (bytes[i + 4] shl 6) + bytes[i + 5];
          res.Append(Format('%.9d', [v]));
          Inc(i, 5);
        end;
      LOCK:
        shift := -1;
      ECI:
        res.SwitchEncoding(ParseECIValue(bytes, i));
      PAD:
        begin
          // ISO/IEC 16023:2000 section 4.9.1 table 5: Structured Append
          if (i = start) then
          begin
            Inc(i);
            var b := bytes[i];
            saIndex := (b shr 3) and 7;
            saCount := (b and 7) + 1;
            // information that makes no sense: count unknown
            if (saCount = 1) or (saCount <= saIndex) then
              saCount := 0;
          end;
          shift := -1;
        end;
    else
      res.Append(Byte(c));
    end;

    if (shift = 0) then
      cset := lastset;
    Dec(shift);
    Inc(i);
  end;
end;

function DecodeBytes(const bytes: TBytes; mode: Integer;
  out error: string): TDecoderResult;
begin
  Result := nil;
  var res := TECIContent.Create('ISO-8859-1');
  var saIndex := -1;
  var saCount := 0;
  try
    case mode of
      2, 3:
        begin
          var postcode: string;
          if (mode = 2) then
            postcode := GetPostCode2(bytes)
          else
            postcode := GetPostCode3(bytes);
          var country := Format('%.3d', [Min(GetInt(bytes, [53, 54, 43, 44,
            45, 46, 47, 48, 37, 38]), 999)]);
          var service := Format('%.3d', [Min(GetInt(bytes, [55, 56, 57, 58,
            59, 60, 49, 50, 51, 52]), 999)]);
          GetMessage(bytes, 10, 84, res, saIndex, saCount);
          // behind '[)>' RS '01' GS when there
          var header: TBytes := [Ord('['), Ord(')'), Ord('>'), RS, Ord('0'),
            Ord('1'), GS];
          var pos := 0;
          if (Length(res.Bytes) >= 9) and CompareMem(@res.Bytes[0], @header[0],
            Length(header)) then
            pos := 9;
          res.Insert(pos, postcode + Char(GS) + country + Char(GS) + service +
            Char(GS));
        end;
      4, 6:
        GetMessage(bytes, 1, 93, res, saIndex, saCount);
      5:
        GetMessage(bytes, 1, 77, res, saIndex, saCount);
    end;
  except
    on Exception do
    begin
      error := 'Format';
      exit;
    end;
  end;

  var saSequence := -1;
  if (saIndex >= 0) then
    saSequence := (saIndex shl 4) or Max(saCount - 1, 0);
  Result := TDecoderResult.Create(res.Bytes, res.Text, nil, IntToStr(mode),
    saSequence, -1);
  // ISO/IEC 15424: ]U1 structured carrier message, ]U0 others, +2 with ECI
  var modifier := 0;
  if (mode = 2) or (mode = 3) then
    modifier := 1;
  if res.HasECI then
    Inc(modifier, 2);
  Result.SymbologyIdentifier := ']U' + Char(Ord('0') + modifier);
end;

function DecodeMaxiCode(bits: TBitMatrix; out error: string): TDecoderResult;
begin
  Result := nil;
  error := 'Checksum';
  var codewords := ReadCodewords(bits);
  if not CorrectErrors(codewords, 0, 10, 10, CORRECT_ALL) then
    exit;

  var mode := codewords[0] and $0F;
  var datawords: TBytes;
  case mode of
    // 2, 3 Structured Carrier Message (numeric or alphanumeric postcode),
    // 4 Standard Symbol, 6 Reader Programming
    2, 3, 4, 6:
      begin
        if not CorrectErrors(codewords, 20, 84, 40, CORRECT_EVEN) or
          not CorrectErrors(codewords, 20, 84, 40, CORRECT_ODD) then
          exit;
        SetLength(datawords, 94);
      end;
    // 5 Full ECC
    5:
      begin
        if not CorrectErrors(codewords, 20, 68, 56, CORRECT_EVEN) or
          not CorrectErrors(codewords, 20, 68, 56, CORRECT_ODD) then
          exit;
        SetLength(datawords, 78);
      end;
  else
    begin
      error := 'Format';
      exit;
    end;
  end;

  for var i := 0 to 9 do
    datawords[i] := codewords[i];
  for var i := 10 to High(datawords) do
    datawords[i] := codewords[i + 10];
  error := '';
  Result := DecodeBytes(datawords, mode, error);
end;

/// <summary>The modules of a symbol that fills the image; nil when the
/// image is too small.</summary>
function ExtractPureBits(image: TBitMatrix; out left, top, width,
  height: Integer): TBitMatrix;
begin
  Result := nil;
  if not image.findBoundingBox(left, top, width, height, MATRIX_WIDTH) then
    exit;
  Result := TBitMatrix.Create(MATRIX_WIDTH, MATRIX_HEIGHT);
  for var y := 0 to MATRIX_HEIGHT - 1 do
  begin
    var iy := top + (y * height + height div 2) div MATRIX_HEIGHT;
    for var x := 0 to MATRIX_WIDTH - 1 do
    begin
      var ix := left + (x * width + width div 2 + (y and 1) * width div 2)
        div MATRIX_WIDTH;
      if (ix < image.Width) and (iy < image.Height) and image[ix, iy] then
        Result[x, y] := true;
    end;
  end;
end;

{ TMaxiCodeReader }

function TMaxiCodeReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TMaxiCodeReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
  var results := TList<TReadResult>.Create;
  try
    decodeMultiple(image, hints, results, 1);
    if (results.Count > 0) then
      Result := results.Extract(results[0]);
  finally
    for var r in results do
      r.Free;
    results.Free;
  end;
end;

procedure TMaxiCodeReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var left, top, width, height: Integer;
  var bits := ExtractPureBits(image.BlackMatrix, left, top, width, height);
  if (bits = nil) then
    exit;
  try
    // (no checksum errors are returned: without a detector there is no
    // check that this is a MaxiCode at all)
    var error: string;
    var decoded := DecodeMaxiCode(bits, error);
    if (decoded = nil) then
      exit;
    try
      var position: TArray<IResultPoint> :=
        [TResultPointHelpers.CreateResultPoint(left, top),
        TResultPointHelpers.CreateResultPoint(left + width - 1, top),
        TResultPointHelpers.CreateResultPoint(left + width - 1,
        top + height - 1), TResultPointHelpers.CreateResultPoint(left,
        top + height - 1)];
      var r := TReadResult.Create(decoded.Text, decoded.RawBytes, position,
        TBarcodeFormat.MAXICODE);
      r.Position := Copy(position);
      r.SymbologyIdentifier := decoded.SymbologyIdentifier;
      r.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL,
        TResultMetaData.CreateStringMetadata(decoded.ECLevel));
      if decoded.StructuredAppend or (decoded.StructuredAppendSequenceNumber
        >= 0) then
        r.putMetadata(TResultMetadataType.STRUCTURED_APPEND_SEQUENCE,
          TResultMetaData.CreateIntegerMetadata
          (decoded.StructuredAppendSequenceNumber));
      if ContainsResult(results, r) then
        r.Free
      else
        results.Add(r);
    finally
      decoded.Free;
    end;
  finally
    bits.Free;
  end;
end;

procedure TMaxiCodeReader.reset;
begin
  // do nothing
end;

end.
