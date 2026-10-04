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

  * Ported from zxing-cpp (QRReader.cpp and DetectPureMQR, DetectPureRMQR,
  * SampleMQR and SampleRMQR of QRDetector.cpp): Micro QR Codes and rMQR
  * Codes, found by their finder pattern.
}

unit ZXing.QrCode.MicroQRCodeReader;

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
  /// Detects and decodes Micro QR Codes (MICRO_QR_CODE) and rMQR Codes
  /// (RMQR_CODE), also several in an image.
  /// </summary>
  TMicroQRCodeReader = class(TInterfacedObject, IReader, IMultipleReader)
  private
    FMicro, FRMQR: Boolean;
  public
    constructor Create(micro: Boolean = true; rmqr: Boolean = true);
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    /// <summary>All Micro QR and rMQR Codes in the image, see
    /// IMultipleReader.</summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

implementation

uses
  System.Types,
  System.Math,
  ZXing.ResultPoint,
  ZXing.DecoderResult,
  ZXing.ResultMetadataType,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry,
  ZXing.Common.BitMatrixCursor,
  ZXing.Common.Pattern,
  ZXing.Common.ConcentricFinder,
  ZXing.Common.LocalGrid,
  ZXing.QrCode.Internal.ConcentricDetector,
  ZXing.QrCode.Internal.MicroQRDecoder;

const
  PATTERN: array [0 .. 4] of Integer = (1, 1, 3, 1, 1);
  SUBPATTERN: array [0 .. 3] of Integer = (1, 1, 1, 1);
  TIMINGPATTERN: array [0 .. 9] of Integer = (1, 1, 1, 1, 1, 1, 1, 1, 1, 1);

type
  /// <summary>A sampled symbol and its corners (top left, top right,
  /// bottom right, bottom left).</summary>
  TSampled = record
    Bits: TBitMatrix;
    Position: TArray<IResultPoint>;
  end;

function ResultPointOf(const p: TPointD): IResultPoint;
begin
  Result := TResultPointHelpers.CreateResultPoint(p.X, p.Y);
end;

function Centered(const p: TPoint): TPointD;
begin
  Result := PointD(p.X + 0.5, p.Y + 0.5);
end;

function RotatedCorners(const q: TQuadrilateralF; n: Integer): TQuadrilateralF;
begin
  for var i := 0 to 3 do
    Result[i] := q[(i + n + 4) mod 4];
end;

function QuadCenter(const q: TQuadrilateralF): TPointD;
begin
  Result := (q[0] + q[1] + q[2] + q[3]) / 4;
end;

function Black(image: TBitMatrix; const p: TPointD): Boolean;
begin
  Result := IsInImage(image, p) and BlackAtPoint(image, p);
end;

function Sum(const a: array of Integer): Integer;
begin
  Result := 0;
  for var v in a do
    Inc(Result, v);
end;

function PositionOf(const pt: TPerspectiveTransformF; w, h: Integer)
  : TArray<IResultPoint>;
begin
  Result := [ResultPointOf(pt.Map(PointD(0, 0))),
    ResultPointOf(pt.Map(PointD(w, 0))), ResultPointOf(pt.Map(PointD(w, h))),
    ResultPointOf(pt.Map(PointD(0, h)))];
end;

function SampleGrid(image: TBitMatrix; w, h: Integer;
  const pt: TPerspectiveTransformF): TSampled;
begin
  Result.Bits := nil;
  Result.Position := nil;
  if not pt.IsValid then
    exit;
  var rois: TArray<TGridROI>;
  SetLength(rois, 1);
  rois[0].x0 := 0;
  rois[0].x1 := w;
  rois[0].y0 := 0;
  rois[0].y1 := h;
  rois[0].mod2Pix := pt;
  Result.Bits := SampleGridROIs(image, w, h, rois);
  if (Result.Bits <> nil) then
    Result.Position := PositionOf(pt, w, h);
end;

/// <summary>A crop and subsample of the image.</summary>
function Deflate(image: TBitMatrix; w, h: Integer; top, left,
  subSampling: Single): TBitMatrix;
begin
  Result := TBitMatrix.Create(w, h);
  for var y := 0 to h - 1 do
  begin
    var yOffset := top + y * subSampling;
    for var x := 0 to w - 1 do
      if Black(image, PointD(left + x * subSampling, yOffset)) then
        Result[x, y] := true;
  end;
end;

function PurePosition(left, top, width, height: Integer)
  : TArray<IResultPoint>;
begin
  Result := [TResultPointHelpers.CreateResultPoint(left, top),
    TResultPointHelpers.CreateResultPoint(left + width - 1, top),
    TResultPointHelpers.CreateResultPoint(left + width - 1, top + height - 1),
    TResultPointHelpers.CreateResultPoint(left, top + height - 1)];
end;

{ pure symbols }

function DetectPureMQR(image: TBitMatrix): TSampled;
const
  MIN_MODULES = 11;
begin
  Result.Bits := nil;
  var left, top, width, height: Integer;
  if not image.findBoundingBox(left, top, width, height, MIN_MODULES) or
    (Abs(width - height) > 1) then
    exit;
  var cur := TBitMatrixCursorI.Create(image, Point(left, top), Point(1, 1));
  var diagonal := cur.ReadPatternFromBlack(5, 1);
  if (diagonal = nil) or (IsPattern(diagonal, PATTERN, false) = 0) then
    exit;
  var moduleSize: Single := Sum(diagonal) / 7;
  var dimension := Round(width / moduleSize);
  if not IsValidMicroSize(dimension) or not IsInImage(image,
    PointD(left + moduleSize / 2 + (dimension - 1) * moduleSize,
    top + moduleSize / 2 + (dimension - 1) * moduleSize)) then
    exit;
  Result.Bits := Deflate(image, dimension, dimension, top + moduleSize / 2,
    left + moduleSize / 2, moduleSize);
  Result.Position := PurePosition(left, top, width, height);
end;

function DetectPureRMQR(image: TBitMatrix): TSampled;
const
  MIN_MODULES = 7;
begin
  Result.Bits := nil;
  var left, top, width, height: Integer;
  if not image.findBoundingBox(left, top, width, height, MIN_MODULES) or
    (height >= width) then
    exit;
  var tl := Point(left, top);
  var tr := Point(left + width - 1, top);
  var bl := Point(left, top + height - 1);
  var br := Point(left + width - 1, top + height - 1);

  var cur := TBitMatrixCursorI.Create(image, tl, Point(1, 1));
  var diagonal := cur.ReadPatternFromBlack(5, 1);
  if (diagonal = nil) or (IsPattern(diagonal, PATTERN, false) = 0) then
    exit;
  // the finder sub pattern
  cur := TBitMatrixCursorI.Create(image, br, Point(-1, -1));
  var subdiagonal := cur.ReadPatternFromBlack(4, 1);
  if (subdiagonal = nil) or (IsPattern(subdiagonal, SUBPATTERN, false) = 0)
  then
    exit;

  var moduleSize: Single := Sum(diagonal) + Sum(subdiagonal);
  // the horizontal timing patterns
  var starts: array [0 .. 3] of TPoint;
  var dirs: array [0 .. 3] of TPoint;
  starts[0] := tr;
  dirs[0] := Point(-1, 0);
  starts[1] := bl;
  dirs[1] := Point(1, 0);
  starts[2] := tl;
  dirs[2] := Point(1, 0);
  starts[3] := br;
  dirs[3] := Point(-1, 0);
  for var k := 0 to 3 do
  begin
    cur := TBitMatrixCursorI.Create(image, starts[k], dirs[k]);
    // skip the corner / finder / sub pattern edge
    cur.StepToEdge(2 + Ord(cur.IsWhite));
    var timing := cur.ReadPattern(10);
    if (IsPattern(timing, TIMINGPATTERN, false) = 0) then
      exit;
    moduleSize := moduleSize + Sum(timing);
  end;

  // the finder pattern, sub pattern and 4 timing patterns
  moduleSize := moduleSize / (7 + 4 + 4 * 10);
  var dimW := Round(width / moduleSize);
  var dimH := Round(height / moduleSize);
  if (RMQRVersionOfSize(dimW, dimH) = 0) then
    exit;
  Result.Bits := Deflate(image, dimW, dimH, top + moduleSize / 2,
    left + moduleSize / 2, moduleSize);
  Result.Position := PurePosition(left, top, width, height);
end;

{ symbols found by a finder pattern }

function SampleMQR(image: TBitMatrix; const fp: TConcentricPattern): TSampled;
const
  FORMAT_INFO_COORDS: array [0 .. 16, 0 .. 1] of Integer = ((0, 8), (1, 8),
    (2, 8), (3, 8), (4, 8), (5, 8), (6, 8), (7, 8), (8, 8), (8, 7), (8, 6),
    (8, 5), (8, 4), (8, 3), (8, 2), (8, 1), (8, 0));
begin
  Result.Bits := nil;
  var fpQuad: TQuadrilateralF;
  if not FindConcentricPatternCorners(image, fp.p, Trunc(fp.size), 2, fpQuad)
  then
    exit;
  var srcQuad := RectangleF(7, 7, 0.5);

  var bestFI: TMicroQRFormat;
  bestFI.HammingDistance := 255;
  var bestPT: TPerspectiveTransformF;
  for var i := 0 to 3 do
  begin
    var mod2Pix := TPerspectiveTransformF.Create(srcQuad,
      RotatedCorners(fpQuad, i));
    if not mod2Pix.IsValid then
      continue;
    var coord: TFunc<Integer, TPointD> := function(k: Integer): TPointD
      begin
        Result := mod2Pix.Map(Centered(Point(FORMAT_INFO_COORDS[k, 0],
          FORMAT_INFO_COORDS[k, 1])));
      end;
    // both innermost timing pattern modules
    if not Black(image, coord(0)) or not IsInImage(image, coord(8)) or
      not Black(image, coord(16)) then
      continue;

    var formatInfoBits: Cardinal := 0;
    for var k := 1 to 15 do
      formatInfoBits := (formatInfoBits shl 1) or Cardinal(Ord(Black(image, coord(k))));
    var fi := DecodeMQRFormat(formatInfoBits);
    if (fi.HammingDistance < bestFI.HammingDistance) then
    begin
      bestFI := fi;
      bestPT := mod2Pix;
    end;
  end;
  if not bestFI.IsValid then
    exit;

  var dim := 9 + 2 * bestFI.Version;
  // not the corner of a QR Code: at most 1/3 black pixels in the quiet zone
  // (about 1/2 in a QR Code)
  var blackPixels := 0;
  for var i := 0 to dim - 1 do
    Inc(blackPixels, Ord(Black(image, bestPT.Map(Centered(Point(i, dim))))) +
      Ord(Black(image, bestPT.Map(Centered(Point(dim, i))))));
  if (blackPixels > 2 * dim div 3) then
    exit;

  var grid := TLocalGrid.Create(image, bestPT, Point(dim, dim));
  try
    var tr, bl, br: TPointD;
    grid.At(Point(dim - 1, 0), Point(-2, 1));
    var haveTR := grid.FindPattern(3, Point(1, 0), 'l', Point(0, 0), '',
      Point(1, -1), 'ld', tr);
    grid.At(Point(0, dim - 1), Point(1, -2));
    var haveBL := grid.FindPattern(3, Point(0, 1), 'u', Point(0, 0), '',
      Point(-1, 1), 'ur', bl);
    grid.At(Point(dim - 1, dim - 1), Point(-3, -3));
    var haveBR := grid.FindCorner(5, Point(1, 1), br);
    if haveTR and haveBL and haveBR then
    begin
      var dst: TQuadrilateralF;
      dst[0] := bestPT.Map(PointD(0.5, 0.5));
      dst[1] := tr;
      dst[2] := br;
      dst[3] := bl;
      var pt := TPerspectiveTransformF.Create(RectangleF(dim, dim, 0.5), dst);
      if pt.IsValid then
        bestPT := pt;
    end;
  finally
    grid.Free;
  end;
  Result := SampleGrid(image, dim, dim, bestPT);
end;

function SampleRMQR(image: TBitMatrix; const fp: TConcentricPattern)
  : TSampled;
const
  FORMAT_INFO_EDGE_COORDS: array [0 .. 3, 0 .. 1] of Integer = ((8, 0), (9, 0),
    (10, 0), (11, 0));
  FORMAT_INFO_COORDS: array [0 .. 17, 0 .. 1] of Integer = ((11, 3), (11, 2),
    (11, 1), (10, 5), (10, 4), (10, 3), (10, 2), (10, 1), (9, 5), (9, 4),
    (9, 3), (9, 2), (9, 1), (8, 5), (8, 4), (8, 3), (8, 2), (8, 1));
begin
  Result.Bits := nil;
  var fpQuad: TQuadrilateralF;
  if not FindConcentricPatternCorners(image, fp.p, Trunc(fp.size), 2, fpQuad)
  then
    exit;
  var srcQuad := RectangleF(7, 7, 0.5);

  var bestFI: TMicroQRFormat;
  bestFI.HammingDistance := 255;
  var bestPT: TPerspectiveTransformF;
  for var i := 0 to 3 do
  begin
    var mod2Pix := TPerspectiveTransformF.Create(srcQuad,
      RotatedCorners(fpQuad, i));
    if not mod2Pix.IsValid then
      continue;
    // the timing pattern modules of the top edge
    var ok := true;
    for var k := 0 to 3 do
    begin
      var p := mod2Pix.Map(Centered(Point(FORMAT_INFO_EDGE_COORDS[k, 0],
        FORMAT_INFO_EDGE_COORDS[k, 1])));
      if not IsInImage(image, p) or (BlackAtPoint(image, p) <> not Odd(k)) then
      begin
        ok := false;
        break;
      end;
    end;
    if not ok then
      continue;

    var formatInfoBits: Cardinal := 0;
    for var k := 0 to 17 do
      formatInfoBits := (formatInfoBits shl 1) or
        Cardinal(Ord(Black(image, mod2Pix.Map(Centered(Point(FORMAT_INFO_COORDS
        [k, 0], FORMAT_INFO_COORDS[k, 1]))))));
    var fi := DecodeRMQRFormat(formatInfoBits, 0);
    if (fi.HammingDistance < bestFI.HammingDistance) then
    begin
      bestFI := fi;
      bestPT := mod2Pix;
    end;
  end;
  if not bestFI.IsValid or (bestFI.Version < 1) or (bestFI.Version > 32) then
    exit;

  var dimX := RMQR_WIDTHS[bestFI.Version - 1];
  var dimY := RMQR_HEIGHTS[bestFI.Version - 1];

  // the finder sub pattern: the corners from both patterns
  var found: TPointD;
  if LocateQRAlignmentPattern(image, fp.size / 7,
    bestPT.Map(PointD(dimX - 3, dimY - 3)), found) then
  begin
    var spQuad: TQuadrilateralF;
    if FindConcentricPatternCorners(image, found, Trunc(fp.size / 2), 1,
      spQuad) then
    begin
      var a := fpQuad;
      var b := spQuad;
      var tlc := QuadCenter(a);
      var brc := QuadCenter(b);
      // rotate: the top left of a furthest away from b, the top left of b
      // closest to a
      var offsetA := 0;
      for var k := 1 to 3 do
        if (PointDistance(a[k], brc) > PointDistance(a[offsetA], brc)) then
          offsetA := k;
      var offsetB := 0;
      for var k := 1 to 3 do
        if (PointDistance(b[k], tlc) < PointDistance(b[offsetB], tlc)) then
          offsetB := k;
      a := RotatedCorners(a, offsetA);
      b := RotatedCorners(b, offsetB);

      var intersectLines: TFunc<TPointD, TPointD, TPointD, TPointD, TPointD> :=
        function(p1, p2, q1, q2: TPointD): TPointD
        begin
          var l1 := TRegressionLine.Create(p1, p2);
          var l2 := TRegressionLine.Create(q1, q2);
          try
            Result := Intersect(l1, l2);
          finally
            l1.Free;
            l2.Free;
          end;
        end;
      var dest: TQuadrilateralF;
      dest[0] := tlc;
      dest[1] := (intersectLines(a[0], a[1], b[1], b[2]) +
        intersectLines(a[3], a[2], b[0], b[3])) / 2;
      dest[2] := brc;
      dest[3] := (intersectLines(a[0], a[3], b[2], b[3]) +
        intersectLines(a[1], a[2], b[0], b[1])) / 2;

      var src: TQuadrilateralF;
      var dst: TQuadrilateralF;
      if (dimY <= 9) then
      begin
        src[0] := PointD(6.5, 0.5);
        src[1] := PointD(dimX - 1.5, dimY - 3.5);
        src[2] := PointD(dimX - 1.5, dimY - 1.5);
        src[3] := PointD(6.5, 6.5);
        // (the top right and bottom right of both, rotated like in
        // zxing-cpp)
        dst[0] := a[1];
        dst[1] := b[1];
        dst[2] := b[2];
        dst[3] := a[2];
      end
      else
      begin
        dest[0] := fp.p;
        dest[2] := found;
        src[0] := PointD(3.5, 3.5);
        src[1] := PointD(dimX - 2.5, 3.5);
        src[2] := PointD(dimX - 2.5, dimY - 2.5);
        src[3] := PointD(3.5, dimY - 2.5);
        dst := dest;
      end;
      var pt := TPerspectiveTransformF.Create(src, dst);
      if pt.IsValid then
        bestPT := pt;
    end;
  end;

  Result := SampleGrid(image, dimX, dimY, bestPT);
end;

{ TMicroQRCodeReader }

constructor TMicroQRCodeReader.Create(micro, rmqr: Boolean);
begin
  inherited Create;
  FMicro := micro;
  FRMQR := rmqr;
end;

function TMicroQRCodeReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TMicroQRCodeReader.decode(const image: TBinaryBitmap;
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

/// <summary>Whether p lies inside the position of one of results.</summary>
function InResult(results: TList<TReadResult>; const p: TPointD): Boolean;
begin
  Result := false;
  for var r in results do
  begin
    var pos := r.Position;
    if (Length(pos) <> 4) then
      continue;
    var minX, minY, maxX, maxY: Single;
    minX := MaxInt;
    minY := MaxInt;
    maxX := -MaxInt;
    maxY := -MaxInt;
    for var q in pos do
    begin
      minX := Min(minX, q.X);
      minY := Min(minY, q.Y);
      maxX := Max(maxX, q.X);
      maxY := Max(maxY, q.Y);
    end;
    if (p.X >= minX) and (p.X <= maxX) and (p.Y >= minY) and (p.Y <= maxY) then
      exit(true);
  end;
end;

procedure TMicroQRCodeReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);

  /// <summary>Decodes a sampled symbol; true when it was added.</summary>
  function tryAdd(var s: TSampled): Boolean;
  begin
    Result := false;
    if (s.Bits = nil) then
      exit;
    try
      var format := TBarcodeFormat.MICRO_QR_CODE;
      if (s.Bits.Width <> s.Bits.Height) then
        format := TBarcodeFormat.RMQR_CODE;
      var error: string;
      var decoded := DecodeMicroQR(s.Bits, error);
      if (decoded = nil) then
      begin
        AddFailedResult(hints, error, format, s.Position, Copy(s.Position));
        exit;
      end;
      try
        var r := TReadResult.Create(decoded.Text, decoded.RawBytes,
          s.Position, format);
        // (a copy: the points and the position are mapped separately)
        r.Position := Copy(s.Position);
        r.SymbologyIdentifier := decoded.SymbologyIdentifier;
        r.IsMirrored := decoded.IsMirrored;
        if (decoded.ECLevel <> '') then
          r.putMetadata(TResultMetadataType.ERROR_CORRECTION_LEVEL,
            TResultMetaData.CreateStringMetadata(decoded.ECLevel));
        if ContainsResult(results, r) then
          r.Free
        else
        begin
          results.Add(r);
          Result := true;
        end;
      finally
        decoded.Free;
      end;
    finally
      FreeAndNil(s.Bits);
    end;
  end;

begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var matrix := image.BlackMatrix;

  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.PURE_BARCODE) then
  begin
    var s: TSampled;
    s.Bits := nil;
    if FMicro then
      s := DetectPureMQR(matrix);
    if (s.Bits = nil) and FRMQR then
      s := DetectPureRMQR(matrix);
    tryAdd(s);
    exit;
  end;

  var tryHarder := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.TRY_HARDER);
  var allFPs := FindQRFinderPatterns(matrix, tryHarder);
  // the finder patterns of decoded symbols (or of found QR Codes) are not
  // used again
  var used: TArray<Boolean>;
  SetLength(used, Length(allFPs));
  for var i := 0 to High(allFPs) do
    used[i] := InResult(results, allFPs[i].p);

  if FMicro then
    for var i := 0 to High(allFPs) do
    begin
      if used[i] then
        continue;
      var s := SampleMQR(matrix, allFPs[i]);
      if tryAdd(s) then
      begin
        used[i] := true;
        if ResultsFull(results, maxCount) then
          exit;
      end;
    end;

  if FRMQR then
    for var i := 0 to High(allFPs) do
    begin
      if used[i] then
        continue;
      var s := SampleRMQR(matrix, allFPs[i]);
      if tryAdd(s) then
      begin
        used[i] := true;
        if ResultsFull(results, maxCount) then
          exit;
      end;
    end;
end;

procedure TMicroQRCodeReader.reset;
begin
  // do nothing
end;

end.
