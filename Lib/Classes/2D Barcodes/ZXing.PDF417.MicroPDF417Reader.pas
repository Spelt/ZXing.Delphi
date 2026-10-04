{
  * Copyright 2026 Axel Waggershauser
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

  * Ported from zxing-cpp (MicroPDFReader.cpp and PDF417.cpp): finds
  * MicroPDF417 symbols (ISO/IEC 24728:2006) by their left row address
  * patterns, determines the number of columns and the symbol size from the
  * row address patterns and reads the codewords with a cursor.
}

unit ZXing.PDF417.MicroPDF417Reader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap;

type
  /// <summary>
  /// Detects and decodes MicroPDF417 symbols, also several in an image.
  /// </summary>
  TMicroPDF417Reader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    /// <summary>When FineBottom is 0 or more: the rows from FineTop to
    /// FineBottom only, every 2nd (the 2D component of a GS1 Composite, rows
    /// of 2 modules), not turned.</summary>
    FineTop, FineBottom: Integer;
    constructor Create;
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    /// <summary>All MicroPDF417 symbols in the image, see IMultipleReader.
    /// </summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>The index (1 to 52) of the row address pattern of the 6 widths
/// (10 modules), a left or right one or a center one; 0 when it is none.
/// </summary>
function MicroPDF417RAPIndex(const widths: array of Integer;
  center: Boolean): Integer;

implementation

uses
  System.Types,
  System.Math,
  ZXing.ResultPoint,
  ZXing.DecoderResult,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry,
  ZXing.Common.BitMatrixCursor,
  ZXing.Common.Pattern,
  ZXing.PDF417.ResultMetadata,
  ZXing.PDF417.Internal.CodewordDecoder,
  ZXing.PDF417.Internal.DecodedBitStreamParser,
  ZXing.PDF417.Internal.Detector,
  ZXing.PDF417.PDF417Reader;

const
  // tolerance of a pattern size in modules
  MS_THR = 2;

  // the left and right row address patterns (6 elements, 10 modules)
  LRRAPS: array [0 .. 51, 0 .. 5] of Integer = (
    (2, 2, 1, 3, 1, 1), (3, 1, 1, 3, 1, 1), (3, 1, 2, 2, 1, 1),
    (2, 2, 2, 2, 1, 1), (2, 1, 3, 2, 1, 1), (2, 1, 4, 1, 1, 1),
    (2, 2, 3, 1, 1, 1), (3, 1, 3, 1, 1, 1), (3, 2, 2, 1, 1, 1),
    (4, 1, 2, 1, 1, 1), (4, 2, 1, 1, 1, 1), (3, 3, 1, 1, 1, 1),
    (2, 4, 1, 1, 1, 1), (2, 3, 2, 1, 1, 1), (2, 3, 1, 2, 1, 1),
    (3, 2, 1, 2, 1, 1), (4, 1, 1, 2, 1, 1), (4, 1, 1, 1, 2, 1),
    (4, 1, 1, 1, 1, 2), (3, 2, 1, 1, 1, 2), (3, 1, 2, 1, 1, 2),
    (3, 1, 1, 2, 1, 2), (3, 1, 1, 2, 2, 1), (3, 1, 1, 1, 3, 1),
    (3, 1, 1, 1, 2, 2), (3, 1, 1, 1, 1, 3), (2, 2, 1, 1, 1, 3),
    (2, 2, 1, 1, 2, 2), (2, 2, 1, 1, 3, 1), (2, 2, 1, 2, 2, 1),
    (2, 2, 2, 1, 2, 1), (3, 1, 2, 1, 2, 1), (3, 2, 1, 1, 2, 1),
    (2, 3, 1, 1, 2, 1), (2, 3, 1, 1, 1, 2), (2, 2, 2, 1, 1, 2),
    (2, 1, 3, 1, 1, 2), (2, 1, 2, 2, 1, 2), (2, 1, 2, 2, 2, 1),
    (2, 1, 2, 1, 3, 1), (2, 1, 2, 1, 2, 2), (2, 1, 2, 1, 1, 3),
    (2, 1, 1, 2, 1, 3), (2, 1, 1, 1, 2, 3), (2, 1, 1, 1, 3, 2),
    (2, 1, 1, 1, 4, 1), (2, 1, 1, 2, 3, 1), (2, 1, 1, 2, 2, 2),
    (2, 1, 1, 3, 1, 2), (2, 1, 1, 3, 2, 1), (2, 1, 1, 4, 1, 1),
    (2, 1, 2, 3, 1, 1));

  // the center row address patterns
  CRAPS: array [0 .. 51, 0 .. 5] of Integer = (
    (1, 1, 2, 2, 3, 1), (1, 2, 1, 2, 3, 1), (1, 2, 2, 1, 3, 1),
    (1, 3, 1, 1, 3, 1), (1, 3, 1, 2, 2, 1), (1, 3, 2, 1, 2, 1),
    (1, 4, 1, 1, 2, 1), (1, 4, 1, 2, 1, 1), (1, 4, 2, 1, 1, 1),
    (1, 3, 3, 1, 1, 1), (1, 3, 2, 2, 1, 1), (1, 3, 1, 3, 1, 1),
    (1, 2, 2, 3, 1, 1), (1, 2, 3, 2, 1, 1), (1, 2, 4, 1, 1, 1),
    (1, 1, 5, 1, 1, 1), (1, 1, 4, 2, 1, 1), (1, 1, 4, 1, 2, 1),
    (1, 2, 3, 1, 2, 1), (1, 2, 3, 1, 1, 2), (1, 2, 2, 2, 1, 2),
    (1, 2, 2, 2, 2, 1), (1, 2, 1, 3, 2, 1), (1, 2, 1, 4, 1, 1),
    (1, 1, 2, 4, 1, 1), (1, 1, 3, 3, 1, 1), (1, 1, 3, 2, 2, 1),
    (1, 1, 3, 2, 1, 2), (1, 1, 3, 1, 2, 2), (1, 2, 2, 1, 2, 2),
    (1, 3, 1, 1, 2, 2), (1, 3, 1, 1, 1, 3), (1, 2, 2, 1, 1, 3),
    (1, 1, 3, 1, 1, 3), (1, 1, 2, 2, 1, 3), (1, 1, 2, 2, 2, 2),
    (1, 1, 2, 3, 1, 2), (1, 1, 2, 3, 2, 1), (1, 1, 1, 4, 2, 1),
    (1, 1, 1, 3, 3, 1), (1, 1, 1, 3, 2, 2), (1, 1, 1, 2, 3, 2),
    (1, 1, 1, 2, 2, 3), (1, 1, 1, 1, 3, 3), (1, 1, 1, 1, 2, 4),
    (1, 1, 1, 2, 1, 4), (1, 1, 2, 1, 1, 4), (1, 2, 1, 1, 1, 4),
    (1, 2, 1, 1, 2, 3), (1, 2, 1, 1, 3, 2), (1, 1, 2, 1, 3, 2),
    (1, 1, 2, 1, 4, 1));

type
  TRAP = (rapL, rapC, rapR);

  /// <summary>A codeword read with a cursor, with how often it was seen and
  /// the sum of its left and right ends.</summary>
  TMCodeword = record
    Codeword, Cluster, Count: Integer;
    Left, Right: TPointD;
    class function None: TMCodeword; static;
    function IsValid: Boolean;
    function LeftPos: TPointD;
    function RightPos: TPointD;
  end;

  /// <summary>A cursor with the module size.</summary>
  TModuleCursor = record
    Cur: TBitMatrixCursorF;
    Ms: Double;
    class function Create(image: TBitMatrix; const p, d: TPointD;
      ms: Double): TModuleCursor; static;
  end;

  TLRAP = record
    X, Y, Idx, Width: Integer;
  end;

  TCluster = TList<TLRAP>;

  TRAPPair = record
    First, Second, Offset, Family: Integer;
    class function Create(f, s: Integer): TRAPPair; static;
    function IsValid: Boolean;
  end;

  TSymbolInfo = record
    NCols, NRows, NECCs, RotFam, StartRow, RowB, RowE: Integer;
    function NCWs: Integer;
    function LastRow: Integer;
    function Width: Integer;
    function Height: Integer;
    function IsValid: Boolean;
  end;

const
  // tables 1, 10, 11, 12 of ISO/IEC 24728:2006; the first is a dummy
  SYMBOLS: array [0 .. 34] of TSymbolInfo = (
    (NCols: 0; NRows: 0; NECCs: 0; RotFam: 0; StartRow: 0; RowB: 0; RowE: 0),
    (NCols: 1; NRows: 11; NECCs: 7; RotFam: 8; StartRow: 1; RowB: 1; RowE: 8),
    (NCols: 1; NRows: 14; NECCs: 7; RotFam: 0; StartRow: 8; RowB: 8; RowE: 18),
    (NCols: 1; NRows: 17; NECCs: 7; RotFam: 0; StartRow: 36; RowB: 39; RowE: 52),
    (NCols: 1; NRows: 20; NECCs: 8; RotFam: 0; StartRow: 19; RowB: 22; RowE: 35),
    (NCols: 1; NRows: 24; NECCs: 8; RotFam: 8; StartRow: 9; RowB: 12; RowE: 24),
    (NCols: 1; NRows: 28; NECCs: 8; RotFam: 8; StartRow: 25; RowB: 33; RowE: 52),
    (NCols: 2; NRows: 8; NECCs: 8; RotFam: 0; StartRow: 1; RowB: 1; RowE: 7),
    (NCols: 2; NRows: 11; NECCs: 9; RotFam: 8; StartRow: 1; RowB: 1; RowE: 8),
    (NCols: 2; NRows: 14; NECCs: 9; RotFam: 0; StartRow: 8; RowB: 9; RowE: 18),
    (NCols: 2; NRows: 17; NECCs: 10; RotFam: 0; StartRow: 36; RowB: 39; RowE: 52),
    (NCols: 2; NRows: 20; NECCs: 11; RotFam: 0; StartRow: 19; RowB: 22; RowE: 35),
    (NCols: 2; NRows: 23; NECCs: 13; RotFam: 8; StartRow: 9; RowB: 12; RowE: 26),
    (NCols: 2; NRows: 26; NECCs: 15; RotFam: 8; StartRow: 27; RowB: 32; RowE: 52),
    (NCols: 3; NRows: 6; NECCs: 12; RotFam: 0; StartRow: 1; RowB: 1; RowE: 6),
    (NCols: 3; NRows: 8; NECCs: 14; RotFam: 0; StartRow: 7; RowB: 7; RowE: 14),
    (NCols: 3; NRows: 10; NECCs: 16; RotFam: 0; StartRow: 15; RowB: 15; RowE: 24),
    (NCols: 3; NRows: 12; NECCs: 18; RotFam: 0; StartRow: 25; RowB: 25; RowE: 36),
    (NCols: 3; NRows: 15; NECCs: 21; RotFam: 0; StartRow: 37; RowB: 37; RowE: 51),
    (NCols: 3; NRows: 20; NECCs: 26; RotFam: 16; StartRow: 1; RowB: 1; RowE: 14),
    (NCols: 3; NRows: 26; NECCs: 32; RotFam: 8; StartRow: 1; RowB: 1; RowE: 20),
    (NCols: 3; NRows: 32; NECCs: 38; RotFam: 8; StartRow: 21; RowB: 27; RowE: 52),
    (NCols: 3; NRows: 38; NECCs: 44; RotFam: 16; StartRow: 15; RowB: 21; RowE: 52),
    (NCols: 3; NRows: 44; NECCs: 50; RotFam: 24; StartRow: 1; RowB: 1; RowE: 44),
    (NCols: 4; NRows: 4; NECCs: 8; RotFam: 24; StartRow: 47; RowB: 47; RowE: 50),
    (NCols: 4; NRows: 6; NECCs: 12; RotFam: 0; StartRow: 1; RowB: 1; RowE: 6),
    (NCols: 4; NRows: 8; NECCs: 14; RotFam: 0; StartRow: 7; RowB: 7; RowE: 14),
    (NCols: 4; NRows: 10; NECCs: 16; RotFam: 0; StartRow: 15; RowB: 15; RowE: 24),
    (NCols: 4; NRows: 12; NECCs: 18; RotFam: 0; StartRow: 25; RowB: 25; RowE: 36),
    (NCols: 4; NRows: 15; NECCs: 21; RotFam: 0; StartRow: 37; RowB: 37; RowE: 51),
    (NCols: 4; NRows: 20; NECCs: 26; RotFam: 16; StartRow: 1; RowB: 1; RowE: 14),
    (NCols: 4; NRows: 26; NECCs: 32; RotFam: 8; StartRow: 1; RowB: 1; RowE: 20),
    (NCols: 4; NRows: 32; NECCs: 38; RotFam: 8; StartRow: 21; RowB: 27; RowE: 52),
    (NCols: 4; NRows: 38; NECCs: 44; RotFam: 16; StartRow: 15; RowB: 21; RowE: 52),
    (NCols: 4; NRows: 44; NECCs: 50; RotFam: 24; StartRow: 1; RowB: 1; RowE: 44));

var
  // the patterns as the int of their normalized e2e widths
  LRRAPInts, CRAPInts: array [0 .. 51] of Integer;

{ helpers }

/// <summary>The widths as bits, bars as 1 (zxing-cpp's ToInt).</summary>
function ToIntPattern(const a: array of Integer): Integer;
begin
  Result := 0;
  for var i := 0 to High(a) do
  begin
    if (a[i] < 0) or (a[i] > 31) then
      exit(-1);
    Result := Result shl a[i];
    if not Odd(i) then
      Result := Result or ((1 shl a[i]) - 1);
  end;
end;

/// <summary>The len - 1 (retLen) edge to similar edge widths of the first
/// len widths, in modules of mods modules in total.</summary>
function NormalizedE2E(const widths: array of Integer; len, mods,
  retLen: Integer): TArray<Integer>;
begin
  SetLength(Result, retLen);
  for var i := 0 to retLen - 1 do
    Result[i] := 0;
  var sum := 0;
  for var i := 0 to len - 1 do
    Inc(sum, widths[i]);
  var moduleSize: Double := sum / mods;
  if not (moduleSize > 0) then
    exit;
  for var i := 0 to retLen - 1 do
    Result[i] := Trunc((widths[i] + widths[i + 1]) / moduleSize + 0.5);
end;

function ViewWidths(const view: TPatternView; n: Integer): TArray<Integer>;
begin
  SetLength(Result, n);
  for var i := 0 to n - 1 do
    Result[i] := view[i];
end;

/// <summary>zxing-cpp's NormalizedPattern for 8 widths of 17 modules: all 0
/// when they do not fit.</summary>
function NormalizedPattern817(const pattern: TArray<Integer>): TArray<Integer>;
const
  LEN = 8;
  SUM = 17;
begin
  SetLength(Result, LEN);
  for var i := 0 to LEN - 1 do
    Result[i] := 0;
  var total := 0;
  for var i := 0 to LEN - 1 do
    Inc(total, pattern[i]);
  var moduleSize: Double := total / SUM;
  if not (moduleSize > 0) then
    exit;
  var err := SUM;
  var rs: array [0 .. LEN - 1] of Double;
  var res: TArray<Integer>;
  SetLength(res, LEN);
  for var i := 0 to LEN - 1 do
  begin
    var v := pattern[i] / moduleSize;
    res[i] := Trunc(v + 0.5);
    rs[i] := v - res[i];
    Dec(err, res[i]);
  end;
  if (Abs(err) > 1) then
    exit;
  if (err <> 0) then
  begin
    var mi := 0;
    for var i := 1 to LEN - 1 do
      if (err > 0) and (rs[i] > rs[mi]) or (err < 0) and (rs[i] < rs[mi]) then
        mi := i;
    Inc(res[mi], err);
  end;
  Result := res;
end;

procedure InitRAPInts;
begin
  for var i := 0 to 51 do
  begin
    LRRAPInts[i] := ToIntPattern(NormalizedE2E(LRRAPS[i], 6, 10, 5));
    CRAPInts[i] := ToIntPattern(NormalizedE2E(CRAPS[i], 6, 10, 5));
  end;
end;

function RAPIndex(v: Integer; t: TRAP): Integer;
begin
  if (v = 0) then
    exit(0);
  for var i := 0 to 51 do
    if (t = rapC) and (CRAPInts[i] = v) or (t <> rapC) and (LRRAPInts[i] = v)
    then
      exit(i + 1);
  Result := 0;
end;

function RAPCluster(idx: Integer): Integer;
begin
  Result := ((idx - 1) mod 3) * 3;
end;

{ TMCodeword }

class function TMCodeword.None: TMCodeword;
begin
  Result.Codeword := -1;
  Result.Cluster := -1;
  Result.Count := 0;
  Result.Left := PointD(0, 0);
  Result.Right := PointD(0, 0);
end;

function TMCodeword.IsValid: Boolean;
begin
  Result := (Codeword <> -1) and (Cluster mod 3 = 0);
end;

function TMCodeword.LeftPos: TPointD;
begin
  Result := Left / Count;
end;

function TMCodeword.RightPos: TPointD;
begin
  Result := Right / Count;
end;

{ TModuleCursor }

class function TModuleCursor.Create(image: TBitMatrix; const p, d: TPointD;
  ms: Double): TModuleCursor;
begin
  Result.Cur := TBitMatrixCursorF.Create(image, p, d);
  Result.Ms := ms;
end;

function MicroPDF417RAPIndex(const widths: array of Integer;
  center: Boolean): Integer;
begin
  var t := rapL;
  if center then
    t := rapC;
  Result := RAPIndex(ToIntPattern(NormalizedE2E(widths, 6, 10, 5)), t);
end;

{ TRAPPair }

class function TRAPPair.Create(f, s: Integer): TRAPPair;
begin
  Result.First := f;
  Result.Second := s;
  var diff := s - f;
  if (diff < -4) then
    Inc(diff, 52);
  Result.Family := (diff + 4) div 8 * 8;
  Result.Offset := diff - Result.Family;
end;

function TRAPPair.IsValid: Boolean;
begin
  Result := (First <> 0) and (Second <> 0) and (Family <= 24) and
    (Abs(Offset) <= 3);
end;

{ TSymbolInfo }

function TSymbolInfo.NCWs: Integer;
begin
  Result := NCols * NRows;
end;

function TSymbolInfo.LastRow: Integer;
begin
  Result := StartRow + NRows - 1;
end;

function TSymbolInfo.Width: Integer;
begin
  Result := 21 + NCols * 17 + Ord(NCols > 2) * 10;
end;

function TSymbolInfo.Height: Integer;
begin
  Result := NRows * 2;
end;

function TSymbolInfo.IsValid: Boolean;
begin
  Result := (NCols > 0) and (NRows > 0);
end;

{ reading codewords and row address patterns }

function ReadCodewordRaw(var mc: TModuleCursor): TMCodeword;
begin
  var start := mc.Cur.p;
  var pattern := mc.Cur.ReadPatternFromBlack(8, Trunc(mc.Ms / 2),
    Trunc(mc.Ms * (17 + MS_THR)), Trunc(mc.Ms * (17 - MS_THR)));
  var np := NormalizedPattern817(pattern);
  Result.Cluster := (np[0] - np[2] + np[4] - np[6] + 9) mod 9;
  var e2e := NormalizedE2E(pattern, 8, 17, 6);
  var ne2ep: TCodewordE2E;
  for var i := 0 to 5 do
    ne2ep[i] := e2e[i];
  Result.Codeword := GetCodewordFromE2E(ne2ep);
  Result.Count := 1;
  Result.Left := start;
  Result.Right := mc.Cur.p;
end;

function ReadCodeword(var mc: TModuleCursor;
  expectedCluster: Integer): TMCodeword;
begin
  var start := mc;
  Result := ReadCodewordRaw(mc);
  if not Result.IsValid or (Result.Cluster <> expectedCluster) then
    for var side := 0 to 1 do
    begin
      var offset := start.Cur.Left;
      if (side = 1) then
        offset := start.Cur.Right;
      var alt := start;
      alt.Cur.p := alt.Cur.p + (start.Ms / 2) * offset;
      var cwAlt := ReadCodewordRaw(alt);
      if cwAlt.IsValid then
        if not Result.IsValid or (cwAlt.Cluster = expectedCluster) then
        begin
          Result := cwAlt;
          if (cwAlt.Cluster = expectedCluster) then
            break;
        end;
    end;

  if Result.IsValid then
    mc.Ms := Dot(mc.Cur.p - start.Cur.p, MainDirection(mc.Cur.d)) / 17
  else
  begin
    mc := start;
    // the closest white edge near the expected position
    if mc.Cur.Step(17 * mc.Ms) and (mc.Cur.IsBlack or
      (mc.Cur.EdgeAtFront = CURSOR_INVALID)) then
    begin
      var back := mc.Cur;
      var front := mc.Cur;
      var step := 0;
      while (step < mc.Ms * MS_THR) do
      begin
        if back.Step(-1) and back.IsWhite and
          (back.EdgeAtFront <> CURSOR_INVALID) then
        begin
          mc.Cur := back;
          break;
        end;
        if front.Step(1) and front.IsWhite and
          (front.EdgeAtFront <> CURSOR_INVALID) then
        begin
          mc.Cur := front;
          break;
        end;
        Inc(step);
      end;
    end;
  end;
end;

function SkipCodeword(var mc: TModuleCursor): Boolean;
begin
  var min := Trunc(mc.Ms * (17 - MS_THR));
  var max := Trunc(mc.Ms * (17 + MS_THR));
  var steps := mc.Cur.StepToEdge(8, max);
  var totalSteps := steps;
  while (totalSteps < min) and (steps <> 0) do
  begin
    steps := mc.Cur.StepToEdge(2, max - totalSteps);
    Inc(totalSteps, steps);
  end;
  mc.Ms := totalSteps / 17;
  Result := (totalSteps >= min) and (max > 0);
end;

function ReadRAP(var mc: TModuleCursor; t: TRAP): Integer;
begin
  var pattern := mc.Cur.ReadPatternFromBlack(6, Trunc(mc.Ms * 1),
    Trunc(mc.Ms * (10 + MS_THR)), Trunc(mc.Ms * (10 - MS_THR)));
  Result := RAPIndex(ToIntPattern(NormalizedE2E(pattern, 6, 10, 5)), t);
  if (Result <> 0) then
  begin
    var sum := 0;
    for var w in pattern do
      Inc(sum, w);
    mc.Ms := sum / 10;
    if (t = rapR) then
    begin
      var msThr := Trunc(mc.Ms * 1.5 + 1);
      if (mc.Cur.StepToEdge(1, msThr) = 0) then
        Result := 0
      else
      begin
        var c := mc.Cur;
        if (c.StepToEdge(1, msThr) <> 0) and c.IsIn then
          Result := 0;
      end;
    end;
  end;
end;

function IsLRAP(const view: TPatternView): Integer;
begin
  Result := 0;
  var l := view.Sum(6);
  var r := view.SubView(6, 0).Sum(8);
  // nominally l:r is 10:17, accepted from 10:13 to 10:22 (about 45 degrees)
  if (l < 10) or (r < 17) or (l * 20 < r * 10) or (l * 14 > r * 10) or
    (not view.IsAtFirstBar and (view[-1] < l div 10)) then
    exit;
  var m := view[0];
  var mx := m;
  for var i := 1 to 5 do
  begin
    m := Min(m, view[i]);
    mx := Max(mx, view[i]);
  end;
  // the first bar is nominally >= 2 * m, mx nominally <= 4 * m
  if (view[0] < m * 3 div 2) or (mx > m * 6) or (view[5] > 5 * m) then
    exit;
  Result := RAPIndex(ToIntPattern(NormalizedE2E(ViewWidths(view, 6), 6, 10,
    5)), rapL);
end;

/// <summary>The first left row address pattern (with the codeword behind
/// it) in view; an invalid view when there is none.</summary>
function FindLRAP(const view: TPatternView; out idx: Integer): TPatternView;
const
  MIN_SIZE = 6 + 8; // 1 column
begin
  idx := 0;
  var window := view.SubView(0, 6 + 8);
  var ending := view.Data + view.Size - MIN_SIZE;
  while (window.Data < ending) do
  begin
    var i := IsLRAP(window);
    if (i <> 0) then
    begin
      idx := i;
      exit(window);
    end;
    window.SkipPair;
  end;
  Result := TPatternView.Empty;
end;

type
  TSegment = record
    Idx: Integer;
    Lraps: TArray<TLRAP>;
  end;

/// <summary>Cleans up the left row address patterns of a cluster: false
/// when the cluster is to be removed, else lraps holds one (center) point
/// per row.</summary>
function CleanCluster(lraps: TCluster): Boolean;
begin
  Result := false;
  var segs := TList<TSegment>.Create;
  try
    var seg: TSegment;
    seg.Idx := lraps[0].Idx;
    seg.Lraps := nil;
    segs.Add(seg);
    for var p in lraps do
    begin
      if (p.Idx <> segs.Last.Idx) then
      begin
        seg.Idx := p.Idx;
        seg.Lraps := nil;
        segs.Add(seg);
      end;
      var last := segs.Last;
      last.Lraps := last.Lraps + [p];
      segs[segs.Count - 1] := last;
    end;

    // noise: streaks less than half as long as the longest
    var maxLen := 0;
    for var s in segs do
      maxLen := Max(maxLen, Length(s.Lraps));
    for var i := segs.Count - 1 downto 0 do
      if (Length(segs[i].Lraps) < maxLen div 2) then
        segs.Delete(i);

    // duplicates (after removing small ones)
    for var i := segs.Count - 1 downto 1 do
      if (segs[i].Idx = segs[i - 1].Idx) then
        segs.Delete(i);

    // not monotonic or too far away at the front and the back
    while (segs.Count > 1) do
    begin
      var diff := segs[1].Idx - segs[0].Idx;
      if (diff < 0) or (diff > 3) then
        segs.Delete(0)
      else
        break;
    end;
    while (segs.Count > 1) do
    begin
      var diff := segs[segs.Count - 2].Idx - segs[segs.Count - 1].Idx;
      if (diff > 0) or (diff < -3) then
        segs.Delete(segs.Count - 1)
      else
        break;
    end;

    // too small, too spread out or not monotonic
    if (segs.Count < 3) or (Abs(segs.Last.Idx - segs.First.Idx) > 3 *
      segs.Count) then
      exit;
    for var i := 1 to segs.Count - 1 do
      if (segs[i].Idx < segs[i - 1].Idx) then
        exit;

    // the center point of each streak
    lraps.Clear;
    for var s in segs do
      lraps.Add(s.Lraps[Length(s.Lraps) div 2]);
    Result := true;
  finally
    segs.Free;
  end;
end;

function FindCandidates(image: TBitMatrix; tryHarder, reversed: Boolean;
  fineTop: Integer = 0; fineBottom: Integer = -1): TObjectList<TCluster>;
const
  // the shortest MicroPDF417 has 4 rows
  MIN_CLUSTER_SIZE = 4;
begin
  Result := TObjectList<TCluster>.Create(true);
  var height := image.Height;
  var width := image.Width;
  if (height < 4) or (width < 27) then
    exit;

  // the tallest MicroPDF417 has 44 rows, the shortest is 8 pixels high
  var margin := height div 4;
  var skip := Max((height - 2 * margin) div 32, 8);
  if tryHarder then
  begin
    margin := Min(8, height div 44);
    skip := 8;
  end;

  var lastY := height - margin;
  if (fineBottom >= 0) then
  begin
    skip := 2;
    margin := fineTop;
    lastY := fineBottom + 1;
    if reversed then
    begin
      margin := height - 1 - fineBottom;
      lastY := height - fineTop;
    end;
  end;
  var row: TPatternRow;
  var y := margin;
  while (y < lastY) do
  begin
    var imageY := y;
    if reversed then
      imageY := height - 1 - y;
    GetPatternRow(image, imageY, row);
    if reversed then
    begin
      var n := Length(row);
      for var i := 0 to n div 2 - 1 do
      begin
        var t := row[i];
        row[i] := row[n - 1 - i];
        row[n - 1 - i] := t;
      end;
    end;

    var next := TPatternView.Create(row);
    while true do
    begin
      var idx: Integer;
      next := FindLRAP(next, idx);
      if not next.IsValid then
        break;
      var p: TLRAP;
      p.X := next.PixelsInFront;
      p.Y := y;
      p.Idx := idx;
      p.Width := next.Sum;

      // attach to an existing cluster close by in x and y; remove short
      // stale clusters; else a new cluster
      var attached := false;
      var c := 0;
      while (c < Result.Count) do
      begin
        var cluster := Result[c];
        var last := cluster.Last;
        var dx := p.X - last.X;
        var dy := p.Y - last.Y;
        if (dy <= 3 * skip) and (Abs(dx) <= Max(dy, 2)) then
        begin
          cluster.Add(p);
          attached := true;
          break;
        end
        else if (dy > 2 * skip) and (cluster.Count < MIN_CLUSTER_SIZE) then
          Result.Delete(c)
        else
          Inc(c);
      end;
      if not attached then
      begin
        var cluster := TCluster.Create;
        cluster.Add(p);
        Result.Add(cluster);
      end;

      next.SkipPair;
      next.Extend;
    end;
    Inc(y, skip);
  end;

  for var c := Result.Count - 1 downto 0 do
    if (Result[c].Count < MIN_CLUSTER_SIZE) or not CleanCluster(Result[c]) then
      Result.Delete(c);

  if reversed then
    for var cluster in Result do
      for var i := 0 to cluster.Count - 1 do
      begin
        var p := cluster[i];
        p.X := width - 1 - p.X;
        p.Y := height - 1 - p.Y;
        cluster[i] := p;
      end;
end;

function CenteredLRAP(const p: TLRAP): TPointD;
begin
  Result := PointD(p.X + 0.5, p.Y + 0.5);
end;

function DetermineNumCols(var start: TModuleCursor; lraps: TCluster): Integer;
begin
  var colHist: array [0 .. 15] of Integer;
  var offsets: array [0 .. 15] of Integer;
  for var i := 0 to 15 do
  begin
    colHist[i] := 0;
    offsets[i] := 0;
  end;

  var histMax: TFunc<Integer> := function: Integer
    begin
      Result := 0;
      for var v in colHist do
        Result := Max(Result, v);
    end;

  for var s := 0 to 1 do
  begin
    if (histMax() >= 3) then
      break;
    for var p in lraps do
    begin
      var cur := start;
      var tmp := cur;
      // with 1 module offset when there are not enough LRAPs
      cur.Cur.p := CenteredLRAP(p) + (s * cur.Ms) * cur.Cur.Right;

      var pair := TRAPPair.Create(ReadRAP(cur, rapL), 0);
      if (pair.First = 0) or not SkipCodeword(cur) then
        continue;

      var checkRAP: TFunc<TRAP, Integer, Boolean> := function(rap: TRAP; colI: Integer): Boolean
        begin
          Dec(colI);
          tmp := cur;
          pair := TRAPPair.Create(pair.First, ReadRAP(tmp, rap));
          if pair.IsValid then
          begin
            Inc(colHist[colI * 4 + pair.Family div 8]);
            Inc(offsets[colI * 4 + pair.Family div 8], pair.Offset);
          end;
          Result := pair.IsValid;
        end;

      checkRAP(rapR, 1);
      if checkRAP(rapC, 3) then
      begin
        pair.First := pair.Second;
        cur := tmp;
        if SkipCodeword(cur) and SkipCodeword(cur) and checkRAP(rapR, 3) then
          continue;
      end;
      if SkipCodeword(cur) then
      begin
        checkRAP(rapR, 2);
        if checkRAP(rapC, 4) then
        begin
          pair.First := pair.Second;
          cur := tmp;
          if SkipCodeword(cur) and SkipCodeword(cur) and checkRAP(rapR, 4) then
            continue;
        end;
      end;
    end;
  end;

  var nCol := 0;
  for var i := 1 to 15 do
    if (colHist[i] > colHist[nCol]) then
      nCol := i;

  if (colHist[nCol] <> 0) then
  begin
    var offset: Double := offsets[nCol] / colHist[nCol];
    start.Cur.d := BresenhamDirection((10 + 17 * 2) * start.Cur.d -
      (2 * offset) * start.Cur.Right);
  end;
  Result := nCol div 4 + 1;
end;

type
  TCodewordMatrix = TArray<TMCodeword>; // nCols x 53, row major

function DetermineSymbolInfo(const cwMat: TCodewordMatrix; nCols: Integer;
  const rotFamHist: array of Integer): TSymbolInfo;
begin
  Result := SYMBOLS[0];
  var maxI := 0;
  var total := 0;
  for var i := 0 to High(rotFamHist) do
  begin
    Inc(total, rotFamHist[i]);
    if (rotFamHist[i] > rotFamHist[maxI]) then
      maxI := i;
  end;
  var rotFam := -1;
  if (rotFamHist[maxI] <> 0) and (rotFamHist[maxI] > total div 2) then
    rotFam := maxI * 8;

  var numCWs := 0;
  var countSum := 0;
  for var e in cwMat do
    if (e.Count > 0) then
    begin
      Inc(numCWs);
      Inc(countSum, e.Count);
    end;
  if (numCWs = 0) then
    exit;

  var matHeight := Length(cwMat) div nCols;
  var sightsPerRow: TArray<Integer>;
  SetLength(sightsPerRow, matHeight);
  for var y := 0 to matHeight - 1 do
  begin
    sightsPerRow[y] := 0;
    if (y >= 1) then
      for var x := 0 to nCols - 1 do
        Inc(sightsPerRow[y], cwMat[y * nCols + x].Count);
  end;

  var minError := MaxInt;
  var meanCount: Single := countSum / numCWs;
  for var s in SYMBOLS do
  begin
    if (s.NCols <> nCols) or ((rotFam <> -1) and (s.RotFam <> rotFam)) then
      continue;
    // all inside filled and outside empty gives 0
    var error := Trunc(s.NCWs * meanCount);
    for var y := 1 to matHeight - 1 do
    begin
      var isOutside := (y < s.StartRow) or (y > s.LastRow);
      if isOutside then
        Inc(error, sightsPerRow[y])
      else
        Dec(error, sightsPerRow[y]);
    end;
    if (error < minError) then
    begin
      minError := error;
      Result := s;
    end;
  end;
end;

function ScanCandidate(image: TBitMatrix; lraps: TCluster;
  hints: TDictionary<TDecodeHintType, TObject>;
  out position: TArray<IResultPoint>; out error: string;
  out extra: IPDF417ResultMetadata): TDecoderResult;
begin
  Result := nil;
  error := '';
  extra := nil;
  position := nil;

  var inward := PointD(1, 0);
  if not (lraps.Last.Y > lraps.First.Y) then
    inward := PointD(-1, 0);
  var lineL := TRegressionLine.Create;
  var lineR := TRegressionLine.Create;
  try
    lineL.SetDirectionInward(inward);
    lineR.SetDirectionInward(inward);
    for var p in lraps do
    begin
      lineL.Add(PointD(p.X, p.Y));
      lineR.Add(PointD(p.X, p.Y) + p.Width * inward);
    end;
    lineL.Evaluate(2, true);
    lineR.Evaluate(2, true);
    if not lineL.IsValid or not lineR.IsValid then
      exit;
    var down := BresenhamDirection(RightOf(lineL.Normal));
    var startCur := TModuleCursor.Create(image, CenteredLRAP(lraps.First),
      BresenhamDirection(lineR.Normal), lraps.First.Width / (10 + 17));
    startCur.Cur.Step(-1);
    // the module size with the angle between the x axis and the normal
    startCur.Ms := startCur.Ms * 1 / (1 + Min(Sqr(startCur.Cur.d.X),
      Sqr(startCur.Cur.d.Y)));

    var nCols := DetermineNumCols(startCur, lraps);
    if (nCols = 0) then
      exit;

    var failedTries := 0;
    while (failedTries < 10) and IsInImage(image, startCur.Cur.p - down) do
    begin
      startCur.Cur.p := startCur.Cur.p - down;
      var cur := startCur;
      if (ReadRAP(cur, rapL) = 0) then
        Inc(failedTries);
    end;
    startCur.Cur.p := startCur.Cur.p + failedTries * down;

    // all codeword sightings of each cell, and the votes for the rotation
    // family of the row address patterns
    var histMat: TArray<TArray<TMCodeword>>;
    SetLength(histMat, nCols * 53);
    var rotFamHist: array [0 .. 3] of Integer;
    for var i := 0 to 3 do
      rotFamHist[i] := 0;
    var pos: array [0 .. 3] of TPointD;
    var havePos0 := false;
    for var i := 0 to 3 do
      pos[i] := PointD(0, 0);

    var checkRAP: TFunc<Integer, Integer, Boolean> := function(li, ri: Integer): Boolean
      begin
        var rap := TRAPPair.Create(li, ri);
        if not rap.IsValid then
          exit(false);
        Inc(rotFamHist[rap.Family div 8]);
        failedTries := 0;
        Result := true;
      end;

    var lastLrap := PointD(lraps.Last.X, lraps.Last.Y);
    failedTries := 1;
    while IsInImage(image, startCur.Cur.p + down) and
      ((Dot(lastLrap - startCur.Cur.p, down) >= 0) or
      (failedTries < 10 * startCur.Ms)) do
    begin
      var cur := startCur;
      var li := ReadRAP(cur, rapL);
      if (li <> 0) then
      begin
        startCur.Ms := cur.Ms;
        var cw: array [0 .. 3] of TMCodeword;
        for var i := 0 to 3 do
          cw[i] := TMCodeword.None;
        var ci := 0;
        var cluster := RAPCluster(li);

        cw[0] := ReadCodeword(cur, cluster);
        if (nCols = 2) then
          cw[1] := ReadCodeword(cur, cluster)
        else if (nCols = 3) then
        begin
          ci := ReadRAP(cur, rapC);
          if checkRAP(li, ci) then
            cluster := RAPCluster(ci);
          cw[1] := ReadCodeword(cur, cluster);
          cw[2] := ReadCodeword(cur, cluster);
        end
        else if (nCols = 4) then
        begin
          cw[1] := ReadCodeword(cur, cluster);
          ci := ReadRAP(cur, rapC);
          if checkRAP(li, ci) then
            cluster := RAPCluster(ci);
          cw[2] := ReadCodeword(cur, cluster);
          cw[3] := ReadCodeword(cur, cluster);
        end;
        var ri := ReadRAP(cur, rapR);

        if (nCols <= 2) then
          checkRAP(li, ri)
        else
        begin
          checkRAP(li, ci);
          checkRAP(ci, ri);
        end;

        if (ci <> 0) or (ri <> 0) or cw[0].IsValid or cw[1].IsValid or
          cw[2].IsValid or cw[3].IsValid then
        begin
          // a crude approximation of the position as fallback
          if not havePos0 then
          begin
            pos[0] := startCur.Cur.p;
            pos[1] := cur.Cur.p;
            havePos0 := true;
          end
          else
          begin
            pos[2] := cur.Cur.p;
            pos[3] := startCur.Cur.p;
          end;
        end;

        var rowCluster := RAPCluster(li);
        for var x := 0 to nCols - 1 do
          if cw[x].IsValid then
          begin
            var o := ((cw[x].Cluster - rowCluster + 9) div 3) mod 3;
            if (o = 2) then
              o := -1;
            rowCluster := cw[x].Cluster;
            Inc(li, o);
            if (li < 1) or (li > 52) then
              continue;
            var cell := histMat[li * nCols + x];
            var found := false;
            for var k := 0 to High(cell) do
              if (cell[k].Codeword = cw[x].Codeword) then
              begin
                Inc(cell[k].Count);
                cell[k].Left := cell[k].Left + cw[x].Left;
                cell[k].Right := cell[k].Right + cw[x].Right;
                found := true;
                break;
              end;
            if not found then
              cell := cell + [cw[x]];
            histMat[li * nCols + x] := cell;
          end;
      end;
      startCur.Cur.p := startCur.Cur.p + down;
      Inc(failedTries);
    end;

    // the codeword of each cell: the one seen most often (when unique)
    var cwMat: TCodewordMatrix;
    SetLength(cwMat, nCols * 53);
    for var i := 0 to High(cwMat) do
    begin
      var hist := histMat[i];
      cwMat[i] := TMCodeword.None;
      if (Length(hist) = 1) then
        cwMat[i] := hist[0]
      else if (Length(hist) > 1) then
      begin
        var best := 0;
        for var k := 1 to High(hist) do
          if (hist[k].Count > hist[best].Count) then
            best := k;
        var unique := true;
        for var k := 0 to High(hist) do
          if (k <> best) and (hist[k].Count >= hist[best].Count) then
            unique := false;
        if unique then
          cwMat[i] := hist[best];
      end;
    end;

    var si := DetermineSymbolInfo(cwMat, nCols, rotFamHist);
    if not si.IsValid then
      exit;

    // the first element is the number of codewords: MicroPDF417 does not
    // encode it, 0 lets VerifyCodewordCount set it
    var codewords: TArray<Integer>;
    SetLength(codewords, si.NCWs + 1);
    codewords[0] := 0;
    for var i := 0 to si.NCWs - 1 do
    begin
      var index := si.StartRow * nCols + i;
      if (index < Length(cwMat)) then
        codewords[i + 1] := cwMat[index].Codeword
      else
        codewords[i + 1] := -1;
    end;

    var erasures: TArray<Integer> := nil;
    for var i := 0 to High(codewords) do
      if (codewords[i] = -1) then
        erasures := erasures + [i];
    if (Length(erasures) > si.NECCs) then
      exit;

    Result := DecodePDF417Codewords(codewords, si.NECCs, erasures, true,
      error, extra);

    // the corners from a perspective transform of codewords seen near the
    // corners
    var cellAt: TFunc<Integer, Integer, TMCodeword> := function(x, y: Integer): TMCodeword
      begin
        if (x < 0) or (x >= nCols) or (y < 0) or (y >= 53) then
          exit(TMCodeword.None);
        Result := cwMat[y * nCols + x];
      end;
    var closestCorner: TFunc<Integer, Integer, Integer, Integer, TPoint> := function(cx, cy, dx, dy: Integer): TPoint
      begin
        for var x := 0 to si.NCols div 2 do
          for var y := 0 to si.NRows div 2 - 1 do
            if (cellAt(cx + x * dx, cy + y * dy).Count > 1) then
              exit(Point(x * dx, y * dy));
        Result := Point(0, 0);
      end;
    var tlI := closestCorner(0, si.StartRow, 1, 1);
    var trI := closestCorner(si.NCols - 1, si.StartRow, -1, 1);
    var brI := closestCorner(si.NCols - 1, si.LastRow, -1, -1);
    var blI := closestCorner(0, si.LastRow, 1, -1);

    var src, dst: TQuadrilateralF;
    src[0] := PointD(10 + tlI.X * 17, 1 + tlI.Y * 2);
    src[1] := PointD(si.Width - 11 + trI.X * 17, 1 + trI.Y * 2);
    src[2] := PointD(si.Width - 11 + brI.X * 17, si.Height - 1 + brI.Y * 2);
    src[3] := PointD(10 + blI.X * 17, si.Height - 1 + blI.Y * 2);
    var cTL := cellAt(tlI.X, si.StartRow + tlI.Y);
    var cTR := cellAt(si.NCols - 1 + trI.X, si.StartRow + trI.Y);
    var cBR := cellAt(si.NCols - 1 + brI.X, si.LastRow + brI.Y);
    var cBL := cellAt(blI.X, si.LastRow + blI.Y);
    var transformed := false;
    if (cTL.Count > 0) and (cTR.Count > 0) and (cBR.Count > 0) and
      (cBL.Count > 0) then
    begin
      dst[0] := cTL.LeftPos;
      dst[1] := cTR.RightPos;
      dst[2] := cBR.RightPos;
      dst[3] := cBL.LeftPos;
      var mod2pix := TPerspectiveTransformF.Create(src, dst);
      if mod2pix.IsValid then
      begin
        pos[0] := mod2pix.Map(PointD(0, 0));
        pos[1] := mod2pix.Map(PointD(si.Width, 0));
        pos[2] := mod2pix.Map(PointD(si.Width, si.Height));
        pos[3] := mod2pix.Map(PointD(0, si.Height));
        transformed := true;
      end;
    end;
    if transformed or havePos0 then
    begin
      SetLength(position, 4);
      for var i := 0 to 3 do
        position[i] := TResultPointHelpers.CreateResultPoint(pos[i].X,
          pos[i].Y);
    end;
  finally
    lineL.Free;
    lineR.Free;
  end;
end;

{ TMicroPDF417Reader }

function TMicroPDF417Reader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TMicroPDF417Reader.decode(const image: TBinaryBitmap;
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

procedure TMicroPDF417Reader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var isPure := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.PURE_BARCODE);
  var tryHarder := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.TRY_HARDER);
  var tryRotate := tryHarder and not isPure and (FineBottom < 0);

  for var rotate90 := 0 to Ord(tryRotate) do
  begin
    var binImg := image.BlackMatrix;
    if (rotate90 = 1) then
      binImg := RotatedBitMatrix90(binImg);
    try
      for var reversed in [false, true] do
      begin
        var candidates := FindCandidates(binImg, tryHarder, reversed, FineTop,
          FineBottom);
        try
          for var lraps in candidates do
          begin
            var position: TArray<IResultPoint>;
            var error: string;
            var extra: IPDF417ResultMetadata;
            var decoded := ScanCandidate(binImg, lraps, hints, position,
              error, extra);
            if (rotate90 = 1) then
              for var i := 0 to High(position) do
                position[i] := TResultPointHelpers.CreateResultPoint
                  (binImg.Height - 1 - position[i].Y, position[i].X);
            if (decoded = nil) then
            begin
              if (error <> '') and (Length(position) = 4) then
                AddFailedResult(hints, error, TBarcodeFormat.MICRO_PDF417,
                  position, position);
              continue;
            end;
            try
              // (no position: the left row address patterns)
              if (Length(position) <> 4) then
              begin
                var f := lraps.First;
                var l := lraps.Last;
                position := [TResultPointHelpers.CreateResultPoint(f.X, f.Y),
                  TResultPointHelpers.CreateResultPoint(f.X + f.Width, f.Y),
                  TResultPointHelpers.CreateResultPoint(l.X + l.Width, l.Y),
                  TResultPointHelpers.CreateResultPoint(l.X, l.Y)];
              end;
              var r := CreatePDF417Result(decoded, extra, position,
                TBarcodeFormat.MICRO_PDF417);
              if ContainsResult(results, r) then
                r.Free
              else
                results.Add(r);
            finally
              decoded.Free;
            end;
            if ResultsFull(results, maxCount) then
              exit;
          end;
        finally
          candidates.Free;
        end;
      end;
    finally
      if (rotate90 = 1) then
        binImg.Free;
    end;
  end;
end;

constructor TMicroPDF417Reader.Create;
begin
  inherited Create;
  FineTop := 0;
  FineBottom := -1;
end;

procedure TMicroPDF417Reader.reset;
begin
  // do nothing
end;

initialization

InitRAPInts;

end.
