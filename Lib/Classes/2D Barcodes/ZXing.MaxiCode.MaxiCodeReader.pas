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
  * (ISO/IEC 16023). zxing-cpp only reads symbols that fill the image
  * ("pure", unrotated); the detector for symbols anywhere in the image
  * (rotated, skewed, in perspective) is not in zxing-cpp.
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
  /// Reads MaxiCode symbols: those that fill the image, and others by
  /// their bullseye.
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
  System.Types,
  System.Generics.Defaults,
  ZXing.ResultPoint,
  ZXing.ResultMetadataType,
  ZXing.Common.ECIContent,
  ZXing.Common.Geometry,
  ZXing.Common.Pattern,
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

{ Detector (not in zxing-cpp): the bullseye in the center gives the position
  and the skew of the symbol, the orientation modules around it the rotation
  and the scale. }

const
  // the radius of the middle of the outer dark ring of the bullseye, in
  // modules (the distance between the centers of two modules in a row); it
  // differs a few percent between generators, the inner rings more
  RING3_RADIUS = 4.1;
  // the orientation modules: row, column, dark (in BITNR -2 and -1)
  ORIENTATION: array [0 .. 17] of record Row, Col: Integer; Dark: Boolean;
    end = ((Row: 9; Col: 10; Dark: true), (Row: 9; Col: 11; Dark: true),
    (Row: 10; Col: 11; Dark: true), (Row: 15; Col: 7; Dark: true),
    (Row: 16; Col: 8; Dark: true), (Row: 16; Col: 20; Dark: true),
    (Row: 17; Col: 20; Dark: true), (Row: 22; Col: 10; Dark: true),
    (Row: 22; Col: 17; Dark: true), (Row: 23; Col: 10; Dark: true),
    (Row: 23; Col: 17; Dark: true), (Row: 9; Col: 17; Dark: false),
    (Row: 10; Col: 17; Dark: false), (Row: 10; Col: 18; Dark: false),
    (Row: 16; Col: 7; Dark: false), (Row: 16; Col: 21; Dark: false),
    (Row: 22; Col: 11; Dark: false), (Row: 23; Col: 16; Dark: false));
  MIN_ORIENTATION_SCORE = 17;
  // the distance between rows relative to the distance between modules in a
  // row: sqrt(3)/2 for a perfect hexagonal grid, a bit more in practice; 1
  // when the bullseye is drawn as an ellipse (some generators)
  ROW_DISTANCES: array [0 .. 1] of Double = (0.88, 1);
  // the coarse search of the orientation: scales and angles (degrees)
  MIN_SCALE = 0.88;
  SCALE_STEP = 0.06;
  SCALE_COUNT = 5;
  ANGLE_STEP = 3;
  // then decoding at the best orientations
  MAX_ORIENTATIONS = 4;

type
  // a system of at most 8 linear equations: the coefficients, then the
  // constant
  TLinearSystem = array [0 .. 7, 0 .. 8] of Double;

  TMatrix2 = record
    A, B, C, D: Double; // (A B; C D)
    function Apply(const p: TPointD): TPointD;
    class operator Multiply(const m, n: TMatrix2): TMatrix2;
  end;

  TRingEdges = array [0 .. 4] of Double;

  TMCGeometry = record
    Center: TPointD;
    // the circle of the bullseye (in modules) to its ellipse in the image,
    // without rotation
    Shape: TMatrix2;
    /// <summary>The grid (in modules, from the bullseye) to the image.
    /// </summary>
    function Transform(angle, scale: Double): TMatrix2;
  end;

  TOrientation = record
    Score: Integer;
    Angle, Scale, RowDistance: Double;
  end;

  /// <summary>Perspective transform (u, v) to (x, y): x = (H0 u + H1 v + H2)
  /// / w, y = (H3 u + H4 v + H5) / w with w = H6 u + H7 v + 1.</summary>
  THomography = record
    H: array [0 .. 7] of Double;
    function Map(u, v: Double): TPointD;
    /// <summary>Least squares fit of the points src to the points dst.
    /// </summary>
    class function Fit(const src, dst: TArray<TPointD>;
      out homography: THomography): Boolean; static;
  end;

  /// <summary>The grid of the modules in the image: points in modules from
  /// the bullseye (y in rows) to the image.</summary>
  TMCGrid = record
    Homography: THomography;
    class function Create(const geometry: TMCGeometry;
      const orientation: TOrientation): TMCGrid; static;
    function Map(gx, gy: Double): TPointD;
    /// <summary>The image point of the center of the module x, y, moved by
    /// dx, dy modules.</summary>
    function ModulePoint(x, y: Integer; dx, dy: Double): TPointD;
    /// <summary>The color at ModulePoint; false when it is outside the
    /// image.</summary>
    function IsModuleDark(image: TBitMatrix; x, y: Integer; dx, dy: Double;
      out dark: Boolean): Boolean;
  end;

function TMatrix2.Apply(const p: TPointD): TPointD;
begin
  Result := PointD(A * p.X + B * p.Y, C * p.X + D * p.Y);
end;

class operator TMatrix2.Multiply(const m, n: TMatrix2): TMatrix2;
begin
  Result.A := m.A * n.A + m.B * n.C;
  Result.B := m.A * n.B + m.B * n.D;
  Result.C := m.C * n.A + m.D * n.C;
  Result.D := m.C * n.B + m.D * n.D;
end;

function TMCGeometry.Transform(angle, scale: Double): TMatrix2;
begin
  var rotation: TMatrix2;
  rotation.A := scale * Cos(angle);
  rotation.B := -scale * Sin(angle);
  rotation.C := -rotation.B;
  rotation.D := rotation.A;
  Result := Shape * rotation;
end;

function IsDarkXY(image: TBitMatrix; x, y: Double; out dark: Boolean)
  : Boolean; inline;
begin
  Result := (x >= 0) and (y >= 0) and (x < image.Width) and
    (y < image.Height);
  if Result then
    dark := image[Trunc(x), Trunc(y)];
end;

function IsDarkAt(image: TBitMatrix; const p: TPointD;
  out dark: Boolean): Boolean; inline;
begin
  Result := IsDarkXY(image, p.X, p.Y, dark);
end;

/// <summary>Runs[i .. i + 10]: the 3 dark rings and the light center of a
/// bullseye (about 1:1:1:1:1:1-3:1:1:1:1:1); the outer ring may touch other
/// modules.</summary>
function IsBullseyePattern(const runs: TPatternRow; i: Integer): Boolean;
begin
  Result := false;
  // the size of the rings from the inner 4 at both sides
  var left := runs[i + 1] + runs[i + 2] + runs[i + 3] + runs[i + 4];
  var right := runs[i + 6] + runs[i + 7] + runs[i + 8] + runs[i + 9];
  var moduleSize := (left + right) / 8;
  if (moduleSize < 1) then
    exit;
  for var k := 0 to 10 do
    if (k <> 5) and ((runs[i + k] < 0.3 * moduleSize) or (runs[i + k] > 2 *
      moduleSize) and (k <> 0) and (k <> 10)) then
      exit;
  // left and right about the same
  if (Abs(left - right) > 0.3 * Max(left, right)) then
    exit;
  // the center wider than the light rings (not stripes), with a pixel
  // margin for small symbols
  var center := runs[i + 5];
  var light := (runs[i + 1] + runs[i + 3] + runs[i + 7] + runs[i + 9]) / 4;
  Result := (center + 1 >= 1.2 * light) and (center <= 5 * moduleSize);
end;

/// <summary>From the light center of a bullseye in the direction dir: the
/// distances of the first 5 edges of the 3 dark rings, in steps of step
/// pixels; false when there are not 5 edges before maxDistance.</summary>
function RingEdges(image: TBitMatrix; const center, dir: TPointD;
  maxDistance, step: Double; out edges: TRingEdges): Boolean;
begin
  Result := false;
  // no ring or gap is longer than about a third of the bullseye
  var maxRun := 0.45 * maxDistance;
  var dark := false;
  var count := 0;
  var t := 0.0;
  var last := 0.0;
  while (count < Length(edges)) do
  begin
    t := t + step;
    if (t > maxDistance) or (t - last > maxRun) then
      exit;
    var d: Boolean;
    if not IsDarkXY(image, center.X + t * dir.X, center.Y + t * dir.Y, d) then
      exit;
    if (d <> dark) then
    begin
      last := t - step / 2;
      edges[count] := last;
      Inc(count);
      dark := d;
    end;
  end;
  Result := true;
end;

/// <summary>The radii of the middles of the outer 2 dark rings; the outer
/// one from its inside edge (its outside often touches other modules);
/// false when they are not at about the expected ratio.</summary>
function RingMiddles(const edges: TRingEdges; out r2, r3: Double): Boolean;
begin
  var r1 := (edges[0] + edges[1]) / 2;
  r2 := (edges[2] + edges[3]) / 2;
  r3 := edges[4] + (edges[3] - edges[2]) / 2;
  Result := (r1 > 0) and (r2 > 1.2 * r1) and (r3 > 1.25 * r2) and
    (r3 < 1.9 * r2);
end;

/// <summary>The center of the bullseye along a line through p in the
/// direction dir, and its radius (about the outside of the outer ring).
/// </summary>
function CenterOnLine(image: TBitMatrix; const p, dir: TPointD;
  maxDistance: Double; out center: TPointD; out radius: Double): Boolean;
var
  e1, e2: TRingEdges;
begin
  var a2, a3, b2, b3: Double;
  Result := RingEdges(image, p, dir, maxDistance, 1, e1) and
    RingEdges(image, p, -dir, maxDistance, 1, e2) and RingMiddles(e1, a2, a3) and
    RingMiddles(e2, b2, b3);
  if not Result then
    exit;
  // the middles of the rings must be about symmetric
  var r3 := (a3 + b3) / 2;
  radius := r3 * 1.1;
  Result := Abs(a3 - b3) < 0.3 * r3;
  center := p + ((a2 - b2 + a3 - b3) / 4) * dir;
end;

/// <summary>Adds the equation v . x = value, as least squares, to the normal
/// equations of n unknowns.</summary>
procedure AddEquation(var system: TLinearSystem; n: Integer;
  const v: array of Double; value: Double);
begin
  for var i := 0 to n - 1 do
  begin
    for var j := 0 to n - 1 do
      system[i, j] := system[i, j] + v[i] * v[j];
    system[i, n] := system[i, n] + v[i] * value;
  end;
end;

/// <summary>Solves the n equations (Gauss-Jordan with partial pivoting);
/// false when they have no unique solution.</summary>
function SolveLinear(var system: TLinearSystem; n: Integer;
  out x: array of Double): Boolean;
begin
  Result := false;
  for var col := 0 to n - 1 do
  begin
    var pivot := col;
    for var r := col + 1 to n - 1 do
      if (Abs(system[r, col]) > Abs(system[pivot, col])) then
        pivot := r;
    if (Abs(system[pivot, col]) < 1E-12) then
      exit;
    if (pivot <> col) then
      for var k := 0 to n do
      begin
        var t := system[col, k];
        system[col, k] := system[pivot, k];
        system[pivot, k] := t;
      end;
    for var r := 0 to n - 1 do
      if (r <> col) then
      begin
        var f := system[r, col] / system[col, col];
        for var k := col to n do
          system[r, k] := system[r, k] - f * system[col, k];
      end;
  end;
  for var i := 0 to n - 1 do
    x[i] := system[i, n] / system[i, i];
  Result := true;
end;

/// <summary>Least squares fit of the ellipse a x^2 + b xy + c y^2 + d x + e y
/// = 1 through points relative to origin: its center and the matrix M with
/// (p - center)' M (p - center) = 1.</summary>
function FitEllipse(const points: TArray<TPointD>; const origin: TPointD;
  out center: TPointD; out m: TMatrix2): Boolean;
var
  system: TLinearSystem;
  x: array [0 .. 4] of Double;
begin
  Result := false;
  if (Length(points) < 8) then
    exit;
  FillChar(system, SizeOf(system), 0);
  for var p in points do
  begin
    var q := p - origin;
    AddEquation(system, 5, [q.X * q.X, q.X * q.Y, q.Y * q.Y, q.X, q.Y], 1);
  end;
  if not SolveLinear(system, 5, x) then
    exit;
  var a := x[0];
  var b := x[1];
  var c := x[2];
  var d := x[3];
  var e := x[4];
  var det := 4 * a * c - b * b;
  if (det <= 0) then
    exit;
  var x0 := (b * e - 2 * c * d) / det;
  var y0 := (b * d - 2 * a * e) / det;
  var k := 1 - (a * x0 * x0 + b * x0 * y0 + c * y0 * y0 + d * x0 + e * y0);
  if (k <= 0) then
    exit;
  center := origin + PointD(x0, y0);
  m.A := a / k;
  m.B := b / (2 * k);
  m.C := m.B;
  m.D := c / k;
  Result := m.A > 0;
end;

/// <summary>FitEllipse, then again without the points that are off by more
/// than a few percent (rays through a gap between the pixels of a ring).
/// </summary>
function FitEllipseRobust(const points: TArray<TPointD>;
  const origin: TPointD; out center: TPointD; out m: TMatrix2): Boolean;
const
  MAX_DEVIATION = 0.08;
begin
  Result := FitEllipse(points, origin, center, m);
  if not Result then
    exit;
  var good: TArray<TPointD> := [];
  for var p in points do
  begin
    var q := p - center;
    var d := Sqrt(Abs(Dot(q, m.Apply(q))));
    if (Abs(d - 1) <= MAX_DEVIATION) then
      good := good + [p];
  end;
  if (Length(good) < Length(points)) and (2 * Length(good) >= Length(points))
  then
    Result := FitEllipse(good, origin, center, m);
end;

/// <summary>The ellipse through the middles of the outer 2 rings of the
/// bullseye around center (of about radius pixels), along rays in steps of
/// step pixels: its center, its matrix and the ratio of the rings.</summary>
function FitRings(image: TBitMatrix; const center: TPointD;
  radius: Double; rays: Integer; step: Double; out fitted: TPointD;
  out m: TMatrix2; out ratio: Double): Boolean;
var
  edges: TRingEdges;
  dirs: TArray<TPointD>;
  radii2, radii3, ratios: TArray<Double>;
begin
  Result := false;
  SetLength(dirs, rays);
  SetLength(radii2, rays);
  SetLength(radii3, rays);
  SetLength(ratios, rays);
  var count := 0;
  for var i := 0 to rays - 1 do
  begin
    var dir := PointD(Cos(2 * Pi * i / rays), Sin(2 * Pi * i / rays));
    if RingEdges(image, center, dir, 2 * radius, step, edges) and
      RingMiddles(edges, radii2[count], radii3[count]) then
    begin
      dirs[count] := dir;
      ratios[count] := radii3[count] / radii2[count];
      Inc(count);
    end;
  end;
  // at least half of the rays
  if (count < rays div 2) then
    exit;
  // the middle ring scaled to the outer one with the median ratio
  TArray.Sort<Double>(ratios, TComparer<Double>.Default, 0, count);
  ratio := ratios[count div 2];
  var points: TArray<TPointD>;
  SetLength(points, 2 * count);
  for var i := 0 to count - 1 do
  begin
    points[2 * i] := center + radii3[i] * dirs[i];
    points[2 * i + 1] := center + (ratio * radii2[i]) * dirs[i];
  end;
  Result := FitEllipseRobust(points, center, fitted, m) and
    (PointDistance(fitted, center) <= radius / 2);
end;

/// <summary>The circle of the bullseye in modules to the ellipse m in the
/// image.</summary>
function EllipseShape(const m: TMatrix2; out shape: TMatrix2): Boolean;
begin
  // M = S' S with S = sqrt(M) maps the ellipse to the unit circle, so the
  // circle of radius RING3_RADIUS to the image is (RING3_RADIUS * S)^-1
  var det := m.A * m.D - m.B * m.C;
  var s := Sqrt(det);
  var t := Sqrt(m.A + m.D + 2 * s);
  var sq: TMatrix2;
  sq.A := (m.A + s) / t;
  sq.B := m.B / t;
  sq.C := m.C / t;
  sq.D := (m.D + s) / t;
  var sqDet := (sq.A * sq.D - sq.B * sq.C) * RING3_RADIUS;
  Result := sqDet > 0;
  if not Result then
    exit;
  shape.A := sq.D / sqDet;
  shape.B := -sq.B / sqDet;
  shape.C := -sq.C / sqDet;
  shape.D := sq.A / sqDet;
end;

/// <summary>Whether the rings of the bullseye and the gaps between them are
/// where expected (ratio: of the outer 2 rings) at at least minFit of the
/// points.</summary>
function RingsFit(image: TBitMatrix; const geometry: TMCGeometry;
  ratio: Double; rays: Integer; minFit: Double): Boolean;
begin
  var r2 := RING3_RADIUS / ratio;
  var circles: TArray<Double> := [RING3_RADIUS, r2,
    (RING3_RADIUS + r2) / 2, r2 - (RING3_RADIUS - r2) / 2];
  var same := 0;
  for var i := 0 to rays - 1 do
  begin
    var dir := PointD(Cos(2 * Pi * i / rays), Sin(2 * Pi * i / rays));
    for var k := 0 to 3 do
    begin
      var dark: Boolean;
      if IsDarkAt(image, geometry.Center + geometry.Shape.Apply(circles[k] *
        dir), dark) and (dark = (k < 2)) then
        Inc(same);
    end;
  end;
  Result := same >= minFit * 4 * rays;
end;

/// <summary>The geometry of the bullseye around center (of about radius
/// pixels): the ellipse through the middles of its outer rings.</summary>
function FitBullseye(image: TBitMatrix; center: TPointD; radius: Double;
  out geometry: TMCGeometry): Boolean;
const
  // first a quick check, then more precise
  QUICK_RAYS = 16;
  RAYS = 36;
  MAX_PASSES = 4;
begin
  Result := false;
  var m: TMatrix2;
  var ratio: Double;
  if not FitRings(image, center, radius, QUICK_RAYS, 0.5, geometry.Center, m,
    ratio) or not EllipseShape(m, geometry.Shape) or
    not RingsFit(image, geometry, ratio, QUICK_RAYS, 0.7) then
    exit;

  // again from the fitted center until that stays about the same
  center := geometry.Center;
  for var pass := 1 to MAX_PASSES do
  begin
    var fitted: TPointD;
    if not FitRings(image, center, radius, RAYS, 0.5, fitted, m, ratio) then
      exit;
    var moved := PointDistance(fitted, center);
    center := fitted;
    if (moved < 0.5) then
      break;
  end;
  geometry.Center := center;
  Result := EllipseShape(m, geometry.Shape) and RingsFit(image, geometry,
    ratio, RAYS, 0.8);
end;

/// <summary>How many orientation modules are as expected.</summary>
function OrientationScore(image: TBitMatrix; const geometry: TMCGeometry;
  const transform: TMatrix2; rowDistance: Double): Integer;
begin
  Result := 0;
  for var o in ORIENTATION do
  begin
    // as geometry.Point, faster
    var gx := o.Col + 0.5 * (o.Row and 1) - 14;
    var gy := (o.Row - 16) * rowDistance;
    var dark: Boolean;
    if IsDarkXY(image, geometry.Center.X + transform.A * gx + transform.B *
      gy, geometry.Center.Y + transform.C * gx + transform.D * gy, dark) and
      (dark = o.Dark) then
      Inc(Result);
  end;
end;

{ THomography }

function THomography.Map(u, v: Double): TPointD;
begin
  var w := H[6] * u + H[7] * v + 1;
  Result := PointD((H[0] * u + H[1] * v + H[2]) / w,
    (H[3] * u + H[4] * v + H[5]) / w);
end;

class function THomography.Fit(const src, dst: TArray<TPointD>;
  out homography: THomography): Boolean;
var
  system: TLinearSystem;
  x: array [0 .. 7] of Double;
begin
  Result := false;
  if (Length(src) < 8) then
    exit;
  // dst relative to its mean and scaled, for precision
  var mean := PointD(0, 0);
  for var p in dst do
    mean := mean + p;
  mean := mean / Length(dst);
  var scale := 0.0;
  for var p in dst do
    scale := scale + PointDistance(p, mean);
  scale := scale / Length(dst);
  if (scale <= 0) then
    exit;
  FillChar(system, SizeOf(system), 0);
  for var i := 0 to High(src) do
  begin
    var u := src[i].X;
    var v := src[i].Y;
    var d := (dst[i] - mean) / scale;
    AddEquation(system, 8, [u, v, 1, 0, 0, 0, -u * d.X, -v * d.X], d.X);
    AddEquation(system, 8, [0, 0, 0, u, v, 1, -u * d.Y, -v * d.Y], d.Y);
  end;
  if not SolveLinear(system, 8, x) then
    exit;
  // back to the image: x = scale * x' + mean.X
  homography.H[0] := scale * x[0] + mean.X * x[6];
  homography.H[1] := scale * x[1] + mean.X * x[7];
  homography.H[2] := scale * x[2] + mean.X;
  homography.H[3] := scale * x[3] + mean.Y * x[6];
  homography.H[4] := scale * x[4] + mean.Y * x[7];
  homography.H[5] := scale * x[5] + mean.Y;
  homography.H[6] := x[6];
  homography.H[7] := x[7];
  Result := true;
end;

{ TMCGrid }

class function TMCGrid.Create(const geometry: TMCGeometry;
  const orientation: TOrientation): TMCGrid;
begin
  var transform := geometry.Transform(orientation.Angle * Pi / 180,
    orientation.Scale);
  var rowDistance := orientation.RowDistance;
  Result.Homography.H[0] := transform.A;
  Result.Homography.H[1] := transform.B * rowDistance;
  Result.Homography.H[2] := geometry.Center.X;
  Result.Homography.H[3] := transform.C;
  Result.Homography.H[4] := transform.D * rowDistance;
  Result.Homography.H[5] := geometry.Center.Y;
  Result.Homography.H[6] := 0;
  Result.Homography.H[7] := 0;
end;

function TMCGrid.Map(gx, gy: Double): TPointD;
begin
  Result := Homography.Map(gx, gy);
end;

function TMCGrid.ModulePoint(x, y: Integer; dx, dy: Double): TPointD;
begin
  // the bullseye is at module 14 of row 16, odd rows half a module to the
  // right
  Result := Map(x + 0.5 * (y and 1) - 14 + dx, y - 16 + dy);
end;

function TMCGrid.IsModuleDark(image: TBitMatrix; x, y: Integer; dx, dy: Double;
  out dark: Boolean): Boolean;
begin
  // as ModulePoint, faster
  var u := x + 0.5 * (y and 1) - 14 + dx;
  var v := y - 16 + dy;
  var w := Homography.H[6] * u + Homography.H[7] * v + 1;
  Result := IsDarkXY(image, (Homography.H[0] * u + Homography.H[1] * v +
    Homography.H[2]) / w, (Homography.H[3] * u + Homography.H[4] * v +
    Homography.H[5]) / w, dark);
end;

/// <summary>The modules of the symbol; nil when a module is outside the
/// image.</summary>
function SampleSymbol(image: TBitMatrix; const grid: TMCGrid): TBitMatrix;
begin
  Result := TBitMatrix.Create(MATRIX_WIDTH, MATRIX_HEIGHT);
  for var y := 0 to MATRIX_HEIGHT - 1 do
    for var x := 0 to MATRIX_WIDTH - 1 do
    begin
      var dark: Boolean;
      if not grid.IsModuleDark(image, x, y, 0, 0, dark) then
      begin
        FreeAndNil(Result);
        exit;
      end;
      if dark then
        Result[x, y] := true;
    end;
end;

/// <summary>Whether the module x, y is a module (not in the bullseye) up to
/// radius modules from the bullseye.</summary>
function IsModuleInRadius(x, y: Integer; radius: Double): Boolean;
begin
  Result := (x >= 0) and (x < MATRIX_WIDTH) and (y >= 0) and
    (y < MATRIX_HEIGHT) and (BITNR[y, x] <> -3) and
    (Sqr(x + 0.5 * (y and 1) - 14) + Sqr((y - 16) * 0.87) <= Sqr(radius));
end;

/// <summary>How well the grid fits the modules up to radius modules from the
/// bullseye: the part of the points around their centers with the color of
/// the center (about 0.5 at random, 1 when perfect).</summary>
function GridFit(image: TBitMatrix; const grid: TMCGrid;
  radius: Double): Double;
const
  SHIFT = 0.3;
  OFFSETS: array [0 .. 3, 0 .. 1] of Double = ((-SHIFT, 0), (SHIFT, 0),
    (0, -SHIFT), (0, SHIFT));
begin
  var same := 0;
  var count := 0;
  for var y := 0 to MATRIX_HEIGHT - 1 do
    for var x := 0 to MATRIX_WIDTH - 1 do
    begin
      if not IsModuleInRadius(x, y, radius) then
        continue;
      var dark, d: Boolean;
      if not grid.IsModuleDark(image, x, y, 0, 0, dark) then
        continue;
      for var k := 0 to 3 do
      begin
        Inc(count);
        if grid.IsModuleDark(image, x, y, OFFSETS[k, 0], OFFSETS[k, 1], d)
          and (d = dark) then
          Inc(same);
      end;
    end;
  Result := 0;
  if (count > 0) then
    Result := same / count;
end;

/// <summary>Where the grid point between the neighboring modules a and b
/// should be: on the edge between them when they differ in color, measured
/// along the line through their centers (the other direction as it is);
/// false when there is no single edge.</summary>
function EdgeBetween(image: TBitMatrix; const grid: TMCGrid;
  xa, ya, xb, yb: Integer; out gridPoint, imagePoint: TPointD): Boolean;
begin
  Result := false;
  var pa := grid.ModulePoint(xa, ya, 0, 0);
  var pb := grid.ModulePoint(xb, yb, 0, 0);
  var darkA, darkB: Boolean;
  if not IsDarkAt(image, pa, darkA) or not IsDarkAt(image, pb, darkB) or
    (darkA = darkB) then
    exit;
  var distance := PointDistance(pa, pb);
  if (distance < 2) then
    exit;
  // in steps of about a pixel
  var steps := Max(4, Ceil(distance));
  var dx := (pb.X - pa.X) / steps;
  var dy := (pb.Y - pa.Y) / steps;
  var edge := -1.0;
  var dark := darkA;
  for var k := 1 to steps - 1 do
  begin
    var d: Boolean;
    IsDarkXY(image, pa.X + k * dx, pa.Y + k * dy, d);
    if (d <> dark) then
    begin
      // only one edge
      if (edge >= 0) then
        exit;
      edge := (k - 0.5) / steps;
      dark := d;
    end;
  end;
  if (edge < 0.2) or (edge > 0.8) then
    exit;
  gridPoint := PointD((xa + 0.5 * (ya and 1) + xb + 0.5 * (yb and 1)) / 2 -
    14, (ya + yb) / 2 - 16);
  var middle := grid.Map(gridPoint.X, gridPoint.Y);
  var direction := (pb - pa) / distance;
  imagePoint := middle + Dot(pa + edge * (pb - pa) - middle, direction) *
    direction;
  Result := true;
end;

/// <summary>Fits the grid to the edges between the modules (perspective),
/// first near the bullseye, then further out.</summary>
procedure RefineGrid(image: TBitMatrix; var grid: TMCGrid);
const
  RADII: array [0 .. 9] of Double = (7, 8, 9, 11, 13, 15, 18, 22, 30, 30);
  MIN_EDGES = 20;
var
  src, dst: TArray<TPointD>;
begin
  // at most 3 edges per module
  SetLength(src, 3 * MATRIX_WIDTH * MATRIX_HEIGHT);
  SetLength(dst, Length(src));
  for var radius in RADII do
  begin
    var count := 0;
    for var y := 0 to MATRIX_HEIGHT - 1 do
      for var x := 0 to MATRIX_WIDTH - 1 do
      begin
        if not IsModuleInRadius(x, y, radius) then
          continue;
        // the neighbors to the right and in the next row
        var next := x - 1 + (y and 1);
        for var k := 0 to 2 do
        begin
          var nx := next + k - 1;
          var ny := y + 1;
          if (k = 0) then
          begin
            nx := x + 1;
            ny := y;
          end;
          if IsModuleInRadius(nx, ny, radius) and EdgeBetween(image, grid, x,
            y, nx, ny, src[count], dst[count]) then
            Inc(count);
        end;
      end;
    var fitted: THomography;
    if (count < MIN_EDGES) or not THomography.Fit(Copy(src, 0, count),
      Copy(dst, 0, count), fitted) then
      exit;
    grid.Homography := fitted;
  end;
end;

/// <summary>Whether p (of radius) is near one of the centers: closer than
/// factor times the smaller radius.</summary>
function IsNear(const centers: TArray<TPointD>; const radii: TArray<Double>;
  const p: TPointD; radius, factor: Double): Boolean;
begin
  for var k := 0 to High(centers) do
    if (PointDistance(centers[k], p) < factor * Min(radii[k], radius)) then
      exit(true);
  Result := false;
end;

/// <summary>The bullseye around p (found in a row, of about size pixels,
/// the light center centerRun pixels wide): vertical, then horizontal again
/// through the center, and the diagonals.</summary>
function CheckBullseye(image: TBitMatrix; const p: TPointD;
  size, centerRun: Integer; out center: TPointD; out radius: Double): Boolean;
begin
  Result := false;
  // first quick: the light center about as high as wide (the bullseye can
  // be an ellipse)
  var x := Trunc(p.X);
  var y := Trunc(p.Y);
  var maxHeight := 4 * centerRun + 2;
  var top := y;
  while (top > 0) and (y - top < maxHeight) and not image[x, top - 1] do
    Dec(top);
  var bottom := y;
  while (bottom < image.Height - 1) and (bottom - top < maxHeight) and
    not image[x, bottom + 1] do
    Inc(bottom);
  var height := bottom - top + 1;
  if (height > maxHeight) or (4 * height < centerRun) then
    exit;

  var c, c2: TPointD;
  var r1, r2, r3, r4: Double;
  Result := CenterOnLine(image, p, PointD(0, 1), size, c, r1) and
    CenterOnLine(image, c, PointD(1, 0), size, center, r2) and
    CenterOnLine(image, center, PointD(Sqrt(0.5), Sqrt(0.5)), size, c2, r3)
    and CenterOnLine(image, center, PointD(Sqrt(0.5), -Sqrt(0.5)), size, c2,
    r4) and (MaxValue([r1, r2, r3, r4]) < 3 * MinValue([r1, r2, r3, r4]));
  radius := MaxValue([r1, r2, r3, r4]);
end;

/// <summary>The centers and radii of bullseyes in the image.</summary>
procedure FindBullseyes(image: TBitMatrix; rowStep: Integer;
  var centers: TArray<TPointD>; var radii: TArray<Double>);
var
  runs: TPatternRow;
begin
  var y := rowStep div 2;
  while (y < image.Height) do
  begin
    GetPatternRow(image, y, runs);
    // the outer ring at a dark run: odd index
    var i := 1;
    var x := runs[0];
    while (i + 10 < Length(runs)) do
    begin
      if IsBullseyePattern(runs, i) then
      begin
        var p := PointD(x + runs[i] + runs[i + 1] + runs[i + 2] + runs[i + 3]
          + runs[i + 4] + runs[i + 5] / 2, y + 0.5);
        var size := 0;
        for var k := 0 to 10 do
          Inc(size, runs[i + k]);
        // (not again for one found in a row before)
        var center: TPointD;
        var radius: Double;
        if not IsNear(centers, radii, p, size / 2, 0.5) and
          CheckBullseye(image, p, size, runs[i + 5], center, radius) and
          not IsNear(centers, radii, center, radius, 0.3) then
        begin
          centers := centers + [center];
          radii := radii + [radius];
        end;
      end;
      Inc(x, runs[i] + runs[i + 1]);
      Inc(i, 2);
    end;
    Inc(y, rowStep);
  end;
end;

/// <summary>Decodes the symbol on the grid; nil when that fails.</summary>
function DecodeGrid(image: TBitMatrix; const grid: TMCGrid;
  out position: TArray<IResultPoint>): TDecoderResult;
begin
  Result := nil;
  var bits := SampleSymbol(image, grid);
  if (bits = nil) then
    exit;
  try
    var error: string;
    Result := DecodeMaxiCode(bits, error);
  finally
    bits.Free;
  end;
  if (Result = nil) then
    exit;
  // the corners of the symbol; the bullseye is half a module left of the
  // middle
  var w := MATRIX_WIDTH / 2;
  var h := MATRIX_HEIGHT / 2;
  position := [];
  for var corner in [PointD(0.5 - w, -h), PointD(0.5 + w, -h),
    PointD(0.5 + w, h), PointD(0.5 - w, h)] do
  begin
    var p := grid.Map(corner.X, corner.Y);
    position := position + [TResultPointHelpers.CreateResultPoint(p.X, p.Y)];
  end;
end;

/// <summary>How well the grid at the orientation fits the modules near the
/// bullseye.</summary>
function FitNearBullseye(image: TBitMatrix; const geometry: TMCGeometry;
  const orientation: TOrientation): Double;
const
  RADIUS = 9;
begin
  Result := GridFit(image, TMCGrid.Create(geometry, orientation), RADIUS);
end;

/// <summary>Decodes the symbol at about the orientation, if needed with a
/// grid fitted to the modules; nil when that fails.</summary>
function DecodeAtOrientation(image: TBitMatrix; const geometry: TMCGeometry;
  const orientation: TOrientation;
  out position: TArray<IResultPoint>): TDecoderResult;
const
  // the fine search of the orientation near the bullseye
  FINE_ANGLE_STEP = 0.5;
  FINE_SCALE_STEP = 0.01;
  FINE_ROW_DISTANCE_STEP = 0.02;
  // the fit near the bullseye of a symbol (about 0.5 at random)
  MIN_FINE_FIT = 0.85;
begin
  Result := nil;
  // the best fit near the bullseye: the angle, the scale, the distance of
  // the rows and the angle again
  var best := orientation;
  var bestFit := FitNearBullseye(image, geometry, best);
  for var parameter := 0 to 3 do
  begin
    var base := best;
    for var i := -4 to 4 do
    begin
      var o := base;
      case parameter of
        0, 3:
          o.Angle := base.Angle + i * FINE_ANGLE_STEP;
        1:
          o.Scale := base.Scale + i * FINE_SCALE_STEP;
        2:
          o.RowDistance := base.RowDistance + i * FINE_ROW_DISTANCE_STEP;
      end;
      var fit := FitNearBullseye(image, geometry, o);
      if (fit > bestFit) then
      begin
        best := o;
        bestFit := fit;
      end;
    end;
  end;
  if (bestFit < MIN_FINE_FIT) then
    exit;

  var grid := TMCGrid.Create(geometry, best);
  Result := DecodeGrid(image, grid, position);
  if (Result = nil) then
  begin
    RefineGrid(image, grid);
    Result := DecodeGrid(image, grid, position);
  end;
end;

/// <summary>Decodes the symbol around the bullseye; nil when that fails.
/// </summary>
function DecodeAtBullseye(image: TBitMatrix; const center: TPointD;
  radius: Double; out position: TArray<IResultPoint>): TDecoderResult;
begin
  Result := nil;
  var geometry: TMCGeometry;
  if not FitBullseye(image, center, radius, geometry) then
    exit;

  // the orientations (rotation, scale) with the best scores of the
  // orientation modules
  var found: TArray<TOrientation> := [];
  for var rowDistance in ROW_DISTANCES do
    for var s := 0 to SCALE_COUNT - 1 do
    begin
      var a := 0;
      while (a < 360) do
      begin
        var o: TOrientation;
        o.Angle := a;
        o.Scale := MIN_SCALE + s * SCALE_STEP;
        o.RowDistance := rowDistance;
        o.Score := OrientationScore(image, geometry,
          geometry.Transform(o.Angle * Pi / 180, o.Scale), rowDistance);
        if (o.Score >= MIN_ORIENTATION_SCORE) then
          found := found + [o];
        Inc(a, ANGLE_STEP);
      end;
    end;

  // the best first, not close to one tried before
  var tried: TArray<TOrientation> := [];
  while (Length(tried) < MAX_ORIENTATIONS) do
  begin
    var best := -1;
    for var i := 0 to High(found) do
      if (best < 0) or (found[i].Score > found[best].Score) then
        best := i;
    if (best < 0) then
      exit;
    var o := found[best];
    Delete(found, best, 1);
    var close := false;
    for var t in tried do
      if (t.RowDistance = o.RowDistance) and
        (Abs(t.Scale - o.Scale) <= SCALE_STEP + 0.001) and
        (Abs(180 - Abs(Abs(t.Angle - o.Angle) - 180)) <= ANGLE_STEP) then
        close := true;
    if close then
      continue;
    tried := tried + [o];
    Result := DecodeAtOrientation(image, geometry, o, position);
    if (Result <> nil) then
      exit;
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

/// <summary>Adds the result of decoded at position, unless it is already
/// there.</summary>
procedure AddResult(decoded: TDecoderResult;
  const position: TArray<IResultPoint>; results: TList<TReadResult>);
begin
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
end;

procedure TMaxiCodeReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var matrix := image.BlackMatrix;

  // first as a symbol that fills the image
  // (no checksum errors are returned: there is no check that this is a
  // MaxiCode at all)
  var error: string;
  var decoded: TDecoderResult := nil;
  var left, top, width, height: Integer;
  var bits := ExtractPureBits(matrix, left, top, width, height);
  if (bits <> nil) then
    try
      decoded := DecodeMaxiCode(bits, error);
    finally
      bits.Free;
    end;
  if (decoded <> nil) then
  begin
    try
      AddResult(decoded, [TResultPointHelpers.CreateResultPoint(left, top),
        TResultPointHelpers.CreateResultPoint(left + width - 1, top),
        TResultPointHelpers.CreateResultPoint(left + width - 1,
        top + height - 1), TResultPointHelpers.CreateResultPoint(left,
        top + height - 1)], results);
    finally
      decoded.Free;
    end;
    exit;
  end;
  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.PURE_BARCODE) then
    exit;

  // then at the bullseyes in the image
  var rowStep := 3;
  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.TRY_HARDER) then
    rowStep := 2;
  var centers: TArray<TPointD> := [];
  var radii: TArray<Double> := [];
  FindBullseyes(matrix, rowStep, centers, radii);
  // (not again inside a symbol found: the bullseye is about a quarter of it)
  var symbols: TArray<TPointD> := [];
  var symbolRadii: TArray<Double> := [];
  for var i := 0 to High(centers) do
  begin
    if ResultsFull(results, maxCount) then
      exit;
    if IsNear(symbols, symbolRadii, centers[i], 4 * radii[i], 1) then
      continue;
    var position: TArray<IResultPoint>;
    decoded := DecodeAtBullseye(matrix, centers[i], radii[i], position);
    if (decoded <> nil) then
      try
        AddResult(decoded, position, results);
        symbols := symbols + [centers[i]];
        symbolRadii := symbolRadii + [4 * radii[i]];
      finally
        decoded.Free;
      end;
  end;
end;

procedure TMaxiCodeReader.reset;
begin
  // do nothing
end;

end.
