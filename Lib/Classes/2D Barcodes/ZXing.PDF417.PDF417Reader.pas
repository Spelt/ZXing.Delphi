{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
  * Copyright 2020 Axel Waggershauser
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

  * Ported from zxing-cpp (PDFReader.cpp).
}

unit ZXing.PDF417.PDF417Reader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.DecoderResult,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.BinaryBitmap,
  ZXing.PDF417.ResultMetadata;

type
  /// <summary>
  /// Detects and decodes PDF417 symbols, also several in an image and
  /// Macro PDF417 (Structured Append, see PDF417_EXTRA_METADATA).
  /// </summary>
  TPDF417Reader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    /// <summary>All PDF417 symbols in the image, see IMultipleReader.
    /// </summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>A result of a PDF417 or MicroPDF417 symbol.</summary>
function CreatePDF417Result(decoderResult: TDecoderResult;
  const extra: IPDF417ResultMetadata; const position: TArray<IResultPoint>;
  format: TBarcodeFormat): TReadResult;

implementation

uses
  System.Math,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry,
  ZXing.Common.BitMatrixCursor,
  ZXing.Common.Pattern,
  ZXing.PDF417.Internal.CodewordDecoder,
  ZXing.PDF417.Internal.DecodedBitStreamParser,
  ZXing.PDF417.Internal.ScanningDecoder,
  ZXing.PDF417.Internal.Detector;

const
  MODULES_IN_STOP_PATTERN = 18;
  START_PATTERN: array [0 .. 7] of Integer = (8, 1, 1, 1, 1, 1, 1, 3);
  // a missing corner (top or bottom right) from the one on its left
  ORIGINS: array [0 .. 3] of Integer = (0, 0, 3, 3);

function CreatePDF417Result(decoderResult: TDecoderResult;
  const extra: IPDF417ResultMetadata; const position: TArray<IResultPoint>;
  format: TBarcodeFormat): TReadResult;
begin
  Result := TReadResult.Create(decoderResult.Text, decoderResult.RawBytes,
    position, format);
  // (a copy: the points and the position are mapped separately)
  Result.Position := Copy(position);
  Result.SymbologyIdentifier := decoderResult.SymbologyIdentifier;
  if (Length(decoderResult.ECLevel) <> 0) then
    Result.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL,
      TResultMetaData.CreateStringMetadata(decoderResult.ECLevel));
  if (extra <> nil) then
    Result.putMetadata(TResultMetadataType.PDF417_EXTRA_METADATA, extra);
end;

function GetMinWidth(const p1, p2: IResultPoint): Integer;
begin
  // (divided to prevent an integer overflow)
  if (p1 = nil) or (p2 = nil) then
    exit(MaxInt div MODULES_IN_CODEWORD);
  Result := Abs(Trunc(p1.X) - Trunc(p2.X));
end;

function GetMinCodewordWidth(const p: TPDF417Vertices): Integer;
begin
  Result := Min(Min(GetMinWidth(p[0], p[4]), GetMinWidth(p[6], p[2]) *
    MODULES_IN_CODEWORD div MODULES_IN_STOP_PATTERN),
    Min(GetMinWidth(p[1], p[5]), GetMinWidth(p[7], p[3]) * MODULES_IN_CODEWORD
    div MODULES_IN_STOP_PATTERN));
end;

function GetMaxWidth(const p1, p2: IResultPoint): Integer;
begin
  if (p1 = nil) or (p2 = nil) then
    exit(0);
  Result := Abs(Trunc(p1.X) - Trunc(p2.X));
end;

function GetMaxCodewordWidth(const p: TPDF417Vertices): Integer;
begin
  Result := Max(Max(GetMaxWidth(p[0], p[4]), GetMaxWidth(p[6], p[2]) *
    MODULES_IN_CODEWORD div MODULES_IN_STOP_PATTERN),
    Max(GetMaxWidth(p[1], p[5]), GetMaxWidth(p[7], p[3]) * MODULES_IN_CODEWORD
    div MODULES_IN_STOP_PATTERN));
end;

/// <summary>The symbols found by the detector of start and stop patterns.
/// </summary>
procedure DoDecode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; multiple, tryRotate: Boolean;
  results: TList<TReadResult>; maxCount: Integer);
begin
  var detectorResult := DetectPDF417(image.BlackMatrix, multiple, tryRotate,
    function: TBitMatrix
    begin
      Result := image.BlackMatrixRotated90;
    end);
  if (detectorResult = nil) then
    exit;
  try
    var bits := detectorResult.Bits;
    var rotation := detectorResult.Rotation;

    // a point of the rotated image in the image
    var rotate := function(x, y: Integer): IResultPoint
      begin
        case rotation of
          90:
            Result := TResultPointHelpers.CreateResultPoint(bits.Height - y - 1, x);
          180:
            Result := TResultPointHelpers.CreateResultPoint(bits.Width - x - 1,
              bits.Height - y - 1);
          270:
            Result := TResultPointHelpers.CreateResultPoint(y, bits.Width - x - 1);
        else
          Result := TResultPointHelpers.CreateResultPoint(x, y);
        end;
      end;

    for var vertices in detectorResult.Points do
    begin
      var points := vertices;
      var error: string;
      var extra: IPDF417ResultMetadata;
      var approxSymbolWidth: Integer;
      var decoded := DecodePDF417Scanning(bits, points[4], points[5],
        points[6], points[7], GetMinCodewordWidth(points),
        GetMaxCodewordWidth(points), error, extra, approxSymbolWidth);

      // the corners: estimated from the width when the stop pattern was
      // not found
      var point := function(i: Integer): IResultPoint
        begin
          if (points[i] <> nil) or (i < 2) or (approxSymbolWidth < 0) then
          begin
            if (points[i] = nil) then
              exit(nil);
            exit(rotate(Trunc(points[i].X), Trunc(points[i].Y)));
          end;
          var p := rotate(Trunc(points[i - 2].X) + approxSymbolWidth,
            Trunc(points[i - 2].Y));
          Result := TResultPointHelpers.CreateResultPoint
            (EnsureRange(Trunc(p.X), 0, image.Width - 1),
            EnsureRange(Trunc(p.Y), 0, image.Height - 1));
        end;
      var position: TArray<IResultPoint> := [point(0), point(2), point(3),
        point(1)];

      if (decoded = nil) then
      begin
        if (error <> '') and (position[0] <> nil) and (position[3] <> nil) then
        begin
          // (the corners of the start pattern when the right ones are
          // missing)
          var failed := position;
          for var k := 0 to 3 do
            if (failed[k] = nil) then
              failed[k] := failed[ORIGINS[k]];
          AddFailedResult(hints, error, TBarcodeFormat.PDF_417, failed, failed);
        end;
        continue;
      end;
      try
        for var k := 0 to 3 do
          if (position[k] = nil) then
            position[k] := position[ORIGINS[k]];
        var r := CreatePDF417Result(decoded, extra, position,
          TBarcodeFormat.PDF_417);
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
      finally
        decoded.Free;
      end;
      if not multiple or ResultsFull(results, maxCount) then
        exit;
    end;
  finally
    detectorResult.Free;
  end;
end;

{ the pure symbol detector of zxing-cpp }

type
  TPureCodeWord = record
    Cluster, Code: Integer;
  end;

  TSymbolInfo = record
    Width, Height: Integer;
    NRows, NCols, FirstRow, LastRow: Integer;
    ECLevel: Integer;
    ColWidth: Integer;
    RowHeight: Single;
    function IsValid: Boolean;
  end;

function TSymbolInfo.IsValid: Boolean;
begin
  Result := (NRows >= 3) and (NCols >= 1) and (ECLevel <> -1);
end;

/// <summary>The widths of 8 elements of 17 modules normalized (zxing-cpp's
/// NormalizedPattern): nil when they do not fit.</summary>
function NormalizedPattern17(const pattern: TArray<Integer>): TArray<Integer>;
const
  LEN = 8;
  SUM = 17;
begin
  Result := nil;
  var total := 0;
  for var i := 0 to LEN - 1 do
    Inc(total, pattern[i]);
  var moduleSize: Double := total / SUM;
  if not (moduleSize > 0) then
    exit;
  var err := SUM;
  var rs: array [0 .. LEN - 1] of Double;
  SetLength(Result, LEN);
  for var i := 0 to LEN - 1 do
  begin
    var v := pattern[i] / moduleSize;
    Result[i] := Trunc(v + 0.5);
    rs[i] := v - Result[i];
    Dec(err, Result[i]);
  end;
  if (Abs(err) > 1) then
    exit(nil);
  if (err <> 0) then
  begin
    var mi := 0;
    for var i := 1 to LEN - 1 do
      if (err > 0) and (rs[i] > rs[mi]) or (err < 0) and (rs[i] < rs[mi]) then
        mi := i;
    Inc(Result[mi], err);
  end;
end;

function WidthsToSymbol(const np: TArray<Integer>): Integer;
begin
  Result := 0;
  for var i := 0 to High(np) do
  begin
    if (np[i] < 0) or (np[i] > 31) then
      exit(-1);
    Result := Result shl np[i];
    if not Odd(i) then
      Result := Result or ((1 shl np[i]) - 1);
  end;
end;

function ReadCodeWord(var cur: TBitMatrixCursorF;
  expectedCluster: Integer = -1): TPureCodeWord;

  function readCw(var c: TBitMatrixCursorF): TPureCodeWord;
  begin
    Result.Cluster := -1;
    Result.Code := -1;
    var np := NormalizedPattern17(c.ReadPattern(8));
    if (np = nil) then
      exit;
    Result.Cluster := (np[0] - np[2] + np[4] - np[6] + 9) mod 9;
    if (expectedCluster = -1) or (Result.Cluster = expectedCluster) then
      Result.Code := GetCodeword(WidthsToSymbol(np));
  end;

begin
  var curBackup := cur;
  Result := readCw(cur);
  if (Result.Code = -1) then
    for var offset in [0, 1] do
    begin
      var o := curBackup.Left;
      if (offset = 1) then
        o := curBackup.Right;
      var curAlt := curBackup.MovedBy(o);
      // (the first or last image row)
      if not curAlt.IsIn then
        continue;
      var cwAlt := readCw(curAlt);
      if (cwAlt.Code <> -1) then
      begin
        cur := curAlt;
        exit(cwAlt);
      end;
    end;
end;

function RowOf(const rowIndicator: TPureCodeWord): Integer;
begin
  Result := (rowIndicator.Code div 30) * 3 + rowIndicator.Cluster div 3;
end;

function ReadSymbolInfo(topCur: TBitMatrixCursorF; const rowSkip: TPointD;
  colWidth, width, height: Integer): TSymbolInfo;
begin
  Result := Default(TSymbolInfo);
  Result.Width := width;
  Result.Height := height;
  Result.FirstRow := -1;
  Result.LastRow := -1;
  Result.ECLevel := -1;
  Result.ColWidth := colWidth;
  var clusterMask := 0;
  var rows0 := 0;
  var rows1 := 0;

  topCur.p := topCur.p + 0.5 * rowSkip;
  var startCur := topCur;
  while (clusterMask <> 7) and (MaxAbsComponent(topCur.p - startCur.p) <
    height div 2) do
  begin
    var cur := startCur;
    startCur.p := startCur.p + rowSkip;
    if IsPattern(cur.ReadPatternFromBlack(8, 1, colWidth + 2), START_PATTERN,
      false) = 0 then
      break;
    var cw := ReadCodeWord(cur);
    if (cw.Code = -1) then
      continue;
    if (Result.FirstRow = -1) then
      Result.FirstRow := RowOf(cw);
    case cw.Cluster of
      0:
        rows0 := cw.Code mod 30;
      3:
        begin
          rows1 := cw.Code mod 3;
          Result.ECLevel := (cw.Code mod 30) div 3;
        end;
      6:
        Result.NCols := (cw.Code mod 30) + 1;
    else
      continue;
    end;
    clusterMask := clusterMask or (1 shl (cw.Cluster div 3));
  end;
  if ((clusterMask and 3) = 3) then
    Result.NRows := 3 * rows0 + rows1 + 1;
end;

function DetectSymbol(const topCur: TBitMatrixCursorF; width, height: Integer)
  : TSymbolInfo;
begin
  Result := Default(TSymbolInfo);
  Result.ECLevel := -1;
  var c := topCur.MovedBy((height div 2) * topCur.Right);
  var pat := c.ReadPatternFromBlack(8, 1, width div 3);
  if (IsPattern(pat, START_PATTERN, false) = 0) then
    exit;
  var colWidth := 0;
  for var w in pat do
    Inc(colWidth, w);
  var rowSkip := Max(colWidth / 17, 1) * BresenhamDirection(topCur.Right);
  var botCur := topCur.MovedBy((height - 1) * topCur.Right);

  var topSI := ReadSymbolInfo(topCur, rowSkip, colWidth, width, height);
  var botSI := ReadSymbolInfo(botCur, -rowSkip, colWidth, width, height);

  Result := topSI;
  Result.LastRow := botSI.FirstRow;
  Result.RowHeight := height / (Abs(Result.LastRow - Result.FirstRow) + 1);
  // something fishy with the number of columns (aliasing): from the width
  if (topSI.NCols <> botSI.NCols) then
    Result.NCols := (width + Result.ColWidth div 2) div Result.ColWidth - 4;
end;

function ReadCodeWords(topCur: TBitMatrixCursorF; info: TSymbolInfo)
  : TArray<Integer>;
begin
  var rowSkip := topCur.Right;
  if (info.FirstRow > info.LastRow) then
  begin
    topCur.p := topCur.p + (info.Height - 1) * rowSkip;
    rowSkip := -rowSkip;
    var t := info.FirstRow;
    info.FirstRow := info.LastRow;
    info.LastRow := t;
  end;

  var maxColWidth := info.ColWidth * 3 div 2;
  SetLength(Result, info.NRows * info.NCols);
  for var i := 0 to High(Result) do
    Result[i] := -1;
  for var row := info.FirstRow to Min(info.NRows, info.LastRow + 1) - 1 do
  begin
    var cluster := (row mod 3) * 3;
    var cur := topCur.MovedBy(Trunc((row - info.FirstRow + 0.5) *
      info.RowHeight) * rowSkip);
    // the start pattern
    cur.StepToEdge(8 + Ord(cur.IsWhite), maxColWidth);
    // the left row indicator
    ReadCodeWord(cur, cluster);
    var col := 0;
    while (col < info.NCols) and cur.IsIn do
    begin
      var cw := ReadCodeWord(cur, cluster);
      Result[row * info.NCols + col] := cw.Code;
      Inc(col);
    end;
  end;
end;

/// <summary>A pure symbol (filling the image); nil with error when it can
/// not be read.</summary>
function DecodePure(const image: TBinaryBitmap; out error: string)
  : TReadResult;
begin
  Result := nil;
  error := '';
  var matrix := image.BlackMatrix;
  if (matrix = nil) then
    exit;
  var left, top, width, height: Integer;
  if not matrix.findBoundingBox(left, top, width, height, 9) or
    ((width < 3 * 17) and (height < 3 * 17)) then
    exit;

  var cur := TBitMatrixCursorF.Create(matrix, CenteredI(PointI(left, top)),
    PointD(1, 0));
  var info: TSymbolInfo;
  // all 4 orientations
  for var a := 0 to 3 do
  begin
    info := DetectSymbol(cur, width, height);
    if info.IsValid then
      break;
    cur.Step(width - 1);
    cur.TurnRight;
    var t := width;
    width := height;
    height := t;
  end;
  if not info.IsValid then
    exit;

  var codeWords := ReadCodeWords(cur, info);
  var extra: IPDF417ResultMetadata;
  var decoded := DecodePDF417Codewords(codeWords, NumECCodewords(info.ECLevel),
    nil, false, error, extra);
  if (decoded = nil) then
    exit;
  try
    // the bounding box (of the image without rotation)
    var bbW := width;
    var bbH := height;
    if (cur.d.X = 0) then
    begin
      bbW := height;
      bbH := width;
    end;
    Result := CreatePDF417Result(decoded, extra,
      [TResultPointHelpers.CreateResultPoint(left, top),
      TResultPointHelpers.CreateResultPoint(left + bbW - 1, top),
      TResultPointHelpers.CreateResultPoint(left + bbW - 1, top + bbH - 1),
      TResultPointHelpers.CreateResultPoint(left, top + bbH - 1)],
      TBarcodeFormat.PDF_417);
  finally
    decoded.Free;
  end;
end;

{ TPDF417Reader }

function TPDF417Reader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TPDF417Reader.decode(const image: TBinaryBitmap;
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

procedure TPDF417Reader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.PURE_BARCODE) then
  begin
    var error: string;
    var r := DecodePure(image, error);
    if (r <> nil) then
    begin
      if ContainsResult(results, r) then
        r.Free
      else
        results.Add(r);
      exit;
    end;
    // a checksum error: the detector of start and stop patterns (better
    // for 'aliased' images)
    if (error <> 'Checksum') then
      exit;
  end;
  // also rotated by 90 degrees with TRY_HARDER, and for a pure symbol with
  // a checksum error (like zxing-cpp, where tryRotate is on in pure mode)
  var tryRotate := (hints <> nil) and
    (hints.ContainsKey(TDecodeHintType.TRY_HARDER) or
    hints.ContainsKey(TDecodeHintType.PURE_BARCODE));
  DoDecode(image, hints, true, tryRotate, results, maxCount);
end;

procedure TPDF417Reader.reset;
begin
  // do nothing
end;

end.
