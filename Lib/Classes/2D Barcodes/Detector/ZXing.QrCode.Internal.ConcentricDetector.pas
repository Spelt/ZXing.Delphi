unit ZXing.QrCode.Internal.ConcentricDetector;

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

  * Ported from zxing-cpp (qrcode/QRDetector.cpp, QR Code model 2): finds the
  * finder patterns as concentric patterns, combines them to plausible sets
  * of three, sorted by a score, and samples the symbol of every set with the
  * alignment patterns located in the image (tiled sampling), which handles
  * perspective and not flat symbols much better than the old detector.
}

interface

uses
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint,
  ZXing.Common.Geometry,
  ZXing.Common.ConcentricFinder;

type
  /// <summary>Gets every candidate grid: the sampled bits (freed by the
  /// detector after the call), its result points (bottom left, top left and
  /// top right finder pattern) and its position (the corners top left, top
  /// right, bottom right, bottom left). Return true when it was decoded.
  /// </summary>
  TQRCodeCandidate = reference to function(bits: TBitMatrix;
    const points, position: TArray<IResultPoint>): Boolean;

/// <summary>
/// Looks for QR Codes (model 2) by their finder patterns. Without tryHarder
/// every few rows are scanned, depending on the image height, with it every
/// third row. Stops after maxSymbols decoded symbols (0: no limit); the
/// finder patterns of a decoded symbol are not used again. Returns true
/// when at least one candidate was decoded.
/// </summary>
function DetectQRCodesByFinderPatterns(image: TBitMatrix; tryHarder: Boolean;
  const onCandidate: TQRCodeCandidate; maxSymbols: Integer = 1): Boolean;

/// <summary>Detects a code in a "pure" image: an unrotated, unskewed code
/// with only a white border around it (zxing-cpp's DetectPureQR). Returns
/// the sampled bits (to be freed by the caller), or nil when the image is
/// not pure.</summary>
function DetectPureQRCode(image: TBitMatrix;
  out points: TArray<IResultPoint>): TBitMatrix;

/// <summary>The finder patterns of the image (zxing-cpp's
/// FindFinderPatterns), also for Micro QR Codes and rMQR Codes.</summary>
function FindQRFinderPatterns(image: TBitMatrix; tryHarder: Boolean)
  : TArray<TConcentricPattern>;
/// <summary>The center of an alignment pattern near estimate.</summary>
function LocateQRAlignmentPattern(image: TBitMatrix; moduleSize: Double;
  const estimate: TPointD; out res: TPointD): Boolean;

implementation

uses
  System.Types,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  ZXing.Common.BitMatrixCursor,
  ZXing.Common.Pattern,
  ZXing.Common.LocalGrid,
  ZXing.QrCode.Internal.Version,
  ZXing.QrCode.Internal.BitMatrixParser;

/// <summary>Whether the format information of the sampled bits of a QR Code
/// is readable.</summary>
function HasFormatInformation(bits: TBitMatrix): Boolean;
begin
  var parser := TBitMatrixParser.createBitMatrixParser(bits);
  if (parser = nil) then
    exit(false);
  try
    Result := (parser.readFormatInformation <> nil);
  finally
    parser.Free;
  end;
end;

const
  // the 1:1:3:1:1 finder pattern
  PATTERN: array [0 .. 4] of Integer = (1, 1, 3, 1, 1);

type
  TFinderPatternSet = record
    bl, tl, tr: TConcentricPattern;
  end;

  TDimensionEstimate = record
    dim: Integer;
    ms: Double;
    err: Integer;
  end;

/// <summary>The points of a spiral around (0, 0) up to radius (zxing-cpp's
/// Spiral).</summary>
function Spiral(radius: Integer): TArray<TPoint>;
begin
  SetLength(Result, Sqr(2 * radius + 1));
  var n := 0;
  Result[n] := PointI(0, 0);
  Inc(n);
  for var r := 1 to radius do
  begin
    for var k := 0 to 2 * r - 1 do // right -> down
    begin
      Result[n] := PointI(r, -(r - 1) + k);
      Inc(n);
    end;
    for var k := 0 to 2 * r - 1 do // bottom -> left
    begin
      Result[n] := PointI((r - 1) - k, r);
      Inc(n);
    end;
    for var k := 0 to 2 * r - 1 do // left -> up
    begin
      Result[n] := PointI(-r, (r - 1) - k);
      Inc(n);
    end;
    for var k := 0 to 2 * r - 1 do // top -> right
    begin
      Result[n] := PointI(-(r - 1) + k, -r);
      Inc(n);
    end;
  end;
end;

/// <summary>std::lround: halfway cases away from zero.</summary>
function LRound(d: Double): Integer;
begin
  if (d >= 0) then
    Result := Trunc(d + 0.5)
  else
    Result := -Trunc(-d + 0.5);
end;

function ResultPointOf(const p: TPointD): IResultPoint;
begin
  Result := TResultPointHelpers.CreateResultPoint(p.X, p.Y);
end;

{ finder patterns }

/// <summary>A fast plausibility test for the 1:1:3:1:1 pattern, then the
/// pattern test itself.</summary>
function IsFinderPattern(const view: TPatternView;
  spaceInPixel: Integer): Boolean;
begin
  if (view[2] < 3) or (view[2] < 2 * Max(view[0], view[4])) or
    (view[2] < Max(view[1], view[3])) then
    exit(false);
  // the requires 4, here we accept almost 0
  Result := IsPattern(view, PATTERN, true, spaceInPixel, 0.1) <> 0;
end;

/// <summary>The next 1:1:3:1:1 pattern in view (FindLeftGuard).</summary>
function FindPattern(const view: TPatternView): TPatternView;
const
  LEN = 5;
begin
  Result := TPatternView.Empty;
  if (view.Size < LEN) then
    exit;
  var window := view.SubView(0, LEN);
  if window.IsAtFirstBar and IsFinderPattern(window, MaxInt) then
    exit(window);
  var fin := view.Data + view.Size - LEN;
  while (window.Data < fin) do
  begin
    if IsFinderPattern(window, window[-1]) then
      exit(window);
    window.SkipPair;
  end;
end;

function ContainsPattern(const list: TList<TConcentricPattern>;
  const p: TConcentricPattern): Boolean;
begin
  for var q in list do
    if q.SameAs(p) then
      exit(true);
  Result := false;
end;

function FindFinderPatterns(image: TBitMatrix; tryHarder: Boolean)
  : TArray<TConcentricPattern>;
const
  MIN_SKIP = 3; // 1 pixel/module times 3 modules/center
  MAX_MODULES_FAST = 20 * 4 + 17; // support up to version 20 for mobile clients
begin
  // Let's assume that the maximum version QR Code we support takes up 1/4 the
  // height of the image, and then account for the center being 3 modules in
  // size. This gives the smallest number of pixels the center could be, so
  // skip this often. When trying harder, look for all QR versions regardless
  // of how dense they are.
  var height := image.Height;
  var skip := (3 * height) div (4 * MAX_MODULES_FAST);
  if (skip < MIN_SKIP) or tryHarder then
    skip := MIN_SKIP;

  var res := TList<TConcentricPattern>.Create;
  try
    var row: TPatternRow;
    var y := skip - 1;
    while (y < height) do
    begin
      GetPatternRow(image, y, row);
      var next := TPatternView.Create(row);

      while true do
      begin
        next := FindPattern(next);
        if not next.IsValid then
          break;

        var p := PointD(next.PixelsInFront + next[0] + next[1] + next[2] / 2.0,
          y + 0.5);

        // make sure p is not 'inside' an already found pattern area
        var inside := false;
        for var old in res do
          if (PointDistance(p, old.p) < old.size / 2) then
          begin
            inside := true;
            break;
          end;

        if not inside then
        begin
          // the factor 2 allows for a maximum aspect ratio of 4:1 due to
          // perspective distortion
          var width := 2 * next.Sum;
          var located: TConcentricPattern;
          if LocateConcentricPattern(image, PATTERN, true, p, width, located) and
            not ContainsPattern(res, located) then
            res.Add(located);
        end;

        next.SkipPair;
        next.SkipPair;
        next.Extend;
      end;
      Inc(y, skip);
    end;
    Result := res.ToArray;
  finally
    res.Free;
  end;
end;

type
  TScoredSet = record
    score: Double;
    fpSet: TFinderPatternSet;
  end;

/// <summary>Plausible finder pattern sets, sorted by decreasing
/// plausibility.</summary>
function GenerateFinderPatternSets(var patterns: TArray<TConcentricPattern>)
  : TArray<TFinderPatternSet>;
const
  MAX_MODULE_COUNT = 177 * 1.5;
  MAX_CANDIDATES = 15;
  SET_SIZE_LIMIT = 256;
var
  sets: TList<TScoredSet>;
  bins: TArray<TList<Integer>>;
  binsW, binsH, binSize: Integer;
  mX, mY: Double;

  function bin(const p: TPointD): TPoint;
  begin
    Result := PointI(EnsureRange(Trunc((p.X - mX) / binSize), 0, binsW - 1),
      EnsureRange(Trunc((p.Y - mY) / binSize), 0, binsH - 1));
  end;

  // The scaling of the distance based on the b/a size ratio is a very coarse
  // compensation for the shortening effect of the camera projection on
  // slanted symbols.
  function squaredDistance(const a, b: TConcentricPattern): Double;
  begin
    Result := Dot(a.p - b.p, a.p - b.p) * b.size / a.size;
  end;

begin
  Result := nil;
  var count := System.Length(patterns);
  if (count < 3) then
    exit;

  TArray.Sort<TConcentricPattern>(patterns,
    TComparer<TConcentricPattern>.Construct(
    function(const a, b: TConcentricPattern): Integer
    begin
      Result := CompareValue(b.size, a.size);
    end));

  var cosUpper: Double := Cos(60 / 180 * Pi);
  var cosLower: Double := Cos(120 / 180 * Pi);

  // Bin finder patterns into spatial bins to reduce the number of candidates
  // to compare with the geometry heuristics below.
  mX := patterns[0].p.X;
  var maxX := mX;
  mY := patterns[0].p.Y;
  var maxY := mY;
  for var pt in patterns do
  begin
    mX := Min(mX, pt.p.X);
    maxX := Max(maxX, pt.p.X);
    mY := Min(mY, pt.p.Y);
    maxY := Max(maxY, pt.p.Y);
  end;
  var medianSize := Trunc(patterns[count div 2].size);
  // 3 for minimum symbol size of 21 modules
  binSize := Max(32, medianSize * 3);
  binsW := Ceil((maxX - mX + 1) / binSize);
  binsH := Ceil((maxY - mY + 1) / binSize);

  sets := TList<TScoredSet>.Create;
  SetLength(bins, binsW * binsH);
  try
    for var i := 0 to High(bins) do
      bins[i] := TList<Integer>.Create;
    for var idx := 0 to count - 1 do
    begin
      var b := bin(patterns[idx].p);
      bins[b.Y * binsW + b.X].Add(idx);
    end;

    var candidates := TList<Integer>.Create;
    try
      // for a small number of patterns, we apply no/less filters (like size
      // ratio, leg ratio, angle)
      var useFilters := count > 5;
      for var i := 0 to count - 3 do
      begin
        var c0 := patterns[i];
        var maxDistToC: Double := c0.size / 7.0 * MAX_MODULE_COUNT;
        var cBin := bin(c0.p);
        var binRadius := Ceil(maxDistToC / binSize);
        candidates.Clear;

        for var d in Spiral(binRadius) do
        begin
          var bx := cBin.X + d.X;
          var by := cBin.Y + d.Y;
          if (bx < 0) or (bx >= binsW) or (by < 0) or (by >= binsH) then
            continue;

          for var idx in bins[by * binsW + bx] do
          begin
            if (idx <= i) then
              continue;
            if useFilters and (c0.size > patterns[idx].size * 2 + 2) then
              continue;
            candidates.Add(idx);
          end;

          if (candidates.Count >= MAX_CANDIDATES) then
            break;
        end;

        for var u := 0 to candidates.Count - 2 do
          for var v := u + 1 to candidates.Count - 1 do
          begin
            var j := candidates[u];
            var k := candidates[v];

            // patterns is sorted descending by size, but the geometry/size
            // heuristics below assume a <= b <= c in size
            var a := patterns[Max(j, k)];
            var b := patterns[Min(j, k)];
            var c := c0;

            // Orders the three points in an order [A,B,C] such that AB is
            // less than AC and BC is less than AC, and the angle between BC
            // and BA is less than 180 degrees.
            var distAB2: Double := squaredDistance(a, b);
            var distBC2: Double := squaredDistance(b, c);
            var distAC2: Double := squaredDistance(a, c);

            if (distBC2 >= distAB2) and (distBC2 >= distAC2) then
            begin
              var t := a;
              a := b;
              b := t;
              var td := distBC2;
              distBC2 := distAC2;
              distAC2 := td;
            end
            else if (distAB2 >= distAC2) and (distAB2 >= distBC2) then
            begin
              var t := b;
              b := c;
              c := t;
              var td := distAB2;
              distAB2 := distAC2;
              distAC2 := td;
            end;

            // Make sure distAB and distBC don't differ more than reasonable
            var maxRatio := 8;
            if useFilters then
              maxRatio := 4;
            if (distAB2 > maxRatio * distBC2) or (distBC2 > maxRatio * distAB2)
            then
              continue;

            var distAB: Double := Sqrt(distAB2);
            var distBC: Double := Sqrt(distBC2);

            // Estimate the module count and ignore this set if it can not
            // result in a valid decoding
            var moduleCount: Double := (distAB + distBC) /
              (2 * (a.size + b.size + c.size) / (3 * 7.0)) + 7;
            if (moduleCount < 21 * 0.9) or (moduleCount > 177 * 1.5) then
              continue;

            // Make sure the angle between AB and BC does not deviate from 90
            // degrees too much
            var cosAB_BC: Double := (distAB2 + distBC2 - distAC2) /
              (2 * distAB * distBC);
            if useFilters and (IsNan(cosAB_BC) or (cosAB_BC > cosUpper) or
              (cosAB_BC < cosLower)) then
              continue;

            // a score to determine which sets are most likely to be actual
            // finder pattern sets, the smaller the better
            var score: Double := distAB + distBC + Abs(distAB - distBC);

            // arbitrarily limit the number of potential sets
            if (sets.Count < SET_SIZE_LIMIT) or (sets.Last.score > score) then
            begin
              // Use cross product to figure out whether A and C are correct
              // or flipped.
              if (Cross(c.p - b.p, a.p - b.p) < 0) then
              begin
                var t := a;
                a := c;
                c := t;
              end;

              var item: TScoredSet;
              item.score := score;
              item.fpSet.bl := a;
              item.fpSet.tl := b;
              item.fpSet.tr := c;
              // after all sets with the same score, like std::multimap
              var pos := sets.Count;
              while (pos > 0) and (sets[pos - 1].score > score) do
                Dec(pos);
              sets.Insert(pos, item);
              if (sets.Count > SET_SIZE_LIMIT) then
                sets.Delete(sets.Count - 1);
            end;
          end;
      end;
    finally
      candidates.Free;
    end;

    SetLength(Result, sets.Count);
    for var i := 0 to sets.Count - 1 do
      Result[i] := sets[i].fpSet;
  finally
    for var i := 0 to High(bins) do
      bins[i].Free;
    sets.Free;
  end;
end;

{ sampling }

function EstimateModuleSize(image: TBitMatrix;
  const a, b: TConcentricPattern): Double;
begin
  var cur := TBitMatrixCursorF.Create(image, a.p, b.p - a.p);
  var widths: TArray<Integer>;
  if not ReadSymmetricPattern5(cur, Trunc(a.size * 2), widths) or
    (IsPattern(widths, PATTERN, true) = 0) then
    exit(-1);

  var sum := 0;
  for var v in widths do
    Inc(sum, v);
  Result := (2 * sum - widths[0] - widths[4]) / 12.0 * PointLength(cur.d);
end;

function EstimateDimension(image: TBitMatrix; const a, b: TConcentricPattern)
  : TDimensionEstimate;
begin
  Result.dim := 0;
  Result.ms := 0;
  Result.err := 4;

  var msA := EstimateModuleSize(image, a, b);
  var msB := EstimateModuleSize(image, b, a);
  if (msA < 0) or (msB < 0) then
    exit;

  var moduleSize: Double := (msA + msB) / 2;
  var dimension := LRound(PointDistance(a.p, b.p) / moduleSize) + 7;
  var error := 1 - (dimension mod 4);

  Result.dim := dimension + error;
  Result.ms := moduleSize;
  Result.err := Abs(error);
end;

/// <summary>The regression line of the black line (edge 2: outer, edge 3:
/// inner) of the 1 module wide square around the finder pattern at p in
/// direction d. The caller frees it.</summary>
function TraceLine(image: TBitMatrix; const p, d: TPointD; edge: Integer)
  : TRegressionLine;
begin
  var cur := TBitMatrixCursorF.Create(image, p, d - p);
  var line := TRegressionLine.Create;
  line.SetDirectionInward(cur.Back);

  // collect points inside the black line -> backup on 3rd edge
  cur.StepToEdge(edge, 0, edge = 3);
  if (edge = 3) then
    cur.TurnBack;

  var md := MainDirection(cur.d);
  var curI := TBitMatrixCursorI.Create(image, ToPointI(cur.p),
    PointI(Trunc(md.X), Trunc(md.Y)));
  // make sure curI positioned such that the white->black edge is directly
  // behind
  while curI.IsIn and (curI.EdgeAtBack = CURSOR_INVALID) do
  begin
    if (curI.EdgeAtLeft <> CURSOR_INVALID) then
      curI.TurnRight
    else if (curI.EdgeAtRight <> CURSOR_INVALID) then
      curI.TurnLeft
    else
      curI.Step(-1);
  end;

  for var side := 0 to 1 do
  begin
    var dir := DIR_LEFT;
    if (side = 1) then
      dir := DIR_RIGHT;
    var c := TBitMatrixCursorI.Create(image, curI.p, curI.Direction(dir));
    var stepCount := Trunc(MaxAbsComponent(cur.p - p));
    repeat
      line.Add(CenteredI(c.p));
      Dec(stepCount);
    until not ((stepCount > 0) and c.StepAlongEdge(dir, true));
  end;

  line.Evaluate(1.0, true);
  Result := line;
end;

/// <summary>How tilted the symbol is (between 1 and 2).</summary>
function EstimateTilt(const fp: TFinderPatternSet): Double;
begin
  var mn := Trunc(Min(fp.bl.size, Min(fp.tl.size, fp.tr.size)));
  var mx := Trunc(Max(fp.bl.size, Max(fp.tl.size, fp.tr.size)));
  Result := mx / mn;
end;

function MakeMod2Pix(dimension: Integer; const brOffset: TPointD;
  const pix: TQuadrilateralF): TPerspectiveTransformF;
begin
  var quad := RectangleF(dimension, dimension, 3.5);
  quad[2] := quad[2] - brOffset;
  Result := TPerspectiveTransformF.Create(quad, pix);
end;

function LocateAlignmentPattern(image: TBitMatrix; moduleSize: Double;
  const estimate: TPointD; out res: TPointD): Boolean;
const
  DIRS: array [0 .. 8, 0 .. 1] of Integer = ((0, 0), (0, -1), (0, 1), (-1, 0),
    (1, 0), (-1, -1), (1, -1), (1, 1), (-1, 1));
begin
  Result := false;
  for var k := 0 to 8 do
  begin
    var p := estimate + (moduleSize * 2.8) * PointD(DIRS[k, 0], DIRS[k, 1]);
    if not IsInImage(image, p) then
      continue;

    var cor: TConcentricPattern;
    if not CenterOfRing(image, ToPointI(p), Trunc(moduleSize * 3), 1, false,
      cor) then
      continue;
    // if we did not land on a black pixel the concentric pattern finder will
    // fail
    if not IsInImage(image, cor.p) or not BlackAtPoint(image, cor.p) then
      continue;

    var cor1, cor2: TConcentricPattern;
    if CenterOfRing(image, ToPointI(cor.p), Trunc(moduleSize * 2), 1, true,
      cor1) and CenterOfRing(image, ToPointI(cor.p), Trunc(moduleSize * 3), 2,
      true, cor2) and (PointDistance(cor1.p, cor2.p) < moduleSize / 2) and
      (cor2.size > cor1.size) then
    begin
      res := (cor1.p + cor2.p) / 2;
      exit(true);
    end;
  end;
end;

function ReadVersion(image: TBitMatrix; dimension: Integer;
  const mod2Pix: TPerspectiveTransformF): TVersion;
var
  bits: array [Boolean] of Integer;
begin
  for var mirror := false to true do
  begin
    // Read top-right/bottom-left version info: 3 wide by 6 tall (depending
    // on mirrored)
    var versionBits := 0;
    for var y := 5 downto 0 do
      for var x := dimension - 9 downto dimension - 11 do
      begin
        var pix: TPointD;
        if mirror then
          pix := mod2Pix.Map(CenteredI(PointI(y, x)))
        else
          pix := mod2Pix.Map(CenteredI(PointI(x, y)));
        if not IsInImage(image, pix) then
          versionBits := -1
        else
          versionBits := (versionBits shl 1) or Ord(BlackAtPoint(image, pix));
      end;
    bits[mirror] := versionBits;
  end;
  Result := TVersion.decodeVersionInformation(bits[false], bits[true]);
end;

function Quad(const a, b, c, d: TPointD): TQuadrilateralF;
begin
  Result[0] := a;
  Result[1] := b;
  Result[2] := c;
  Result[3] := d;
end;

/// <summary>Locates the alignment patterns at module positions apM (x and
/// y) in the image, starting from their projection with mod2Pix: the inner
/// corners of the finder patterns, then the alignment patterns near their
/// guessed positions, then the remaining ones from their neighbours. apP
/// holds the pixel position of (apM[x], apM[y]) at y * Length(apM) + x,
/// apFound whether it was found.</summary>
procedure LocateAlignmentPatterns(image: TBitMatrix;
  const fp: TFinderPatternSet; const apM: TArray<Integer>;
  const mod2Pix: TPerspectiveTransformF; out apP: TArray<TPointD>;
  out apFound: TArray<Boolean>);
var
  nx, n, x, y, i, f, idx, xi, yi: Integer;
  fps: array [0 .. 2] of TConcentricPattern;
  fpX, fpY: array [0 .. 2] of Integer;
  pc, guessed, found: TPointD;
  fpQuad: TQuadrilateralF;
  hori, verti: TArray<TPointD>;
  lh, lv: TRegressionLine;

  // project the alignment pattern at module coordinates x/y to pixel
  // coordinate based on the current mod2Pix
  function projectM2P(x, y: Integer): TPointD;
  begin
    Result := mod2Pix.Map(CenteredI(PointI(apM[x], apM[y])));
  end;

  // estimate the module size at module coordinates x/y based on mod2Pix
  function estimateModuleSizeAt(x, y: Integer): Double;
  begin
    var p0 := mod2Pix.Map(PointD(apM[x], apM[y]));
    var p1 := mod2Pix.Map(PointD(apM[x] + 1, apM[y]));
    var p2 := mod2Pix.Map(PointD(apM[x], apM[y] + 1));
    Result := (PointDistance(p0, p1) + PointDistance(p0, p2)) / 2;
  end;

  function bestGuessAPP(x, y: Integer): TPointD;
  begin
    if apFound[y * nx + x] then
      Result := apP[y * nx + x]
    else
      Result := projectM2P(x, y);
  end;

begin
  nx := System.Length(apM);
  n := nx - 1;
  SetLength(apP, nx * nx);
  SetLength(apFound, nx * nx);

  // the inner corners of the 3 finder patterns
  fps[0] := fp.tl;
  fpX[0] := 0;
  fpY[0] := 0;
  fps[1] := fp.bl;
  fpX[1] := 0;
  fpY[1] := n;
  fps[2] := fp.tr;
  fpX[2] := n;
  fpY[2] := 0;
  for f := 0 to 2 do
  begin
    idx := fpY[f] * nx + fpX[f];
    pc := projectM2P(fpX[f], fpY[f]);
    apP[idx] := pc;
    apFound[idx] := true;
    if FindConcentricPatternCorners(image, fps[f].p, Trunc(fps[f].size), 2,
      fpQuad) then
      for i := 0 to 3 do
        if (PointDistance(fpQuad[i], pc) < fps[f].size / 2) then
          apP[idx] := fpQuad[i];
  end;

  for y := 0 to n do
    for x := 0 to n do
    begin
      if apFound[y * nx + x] then
        continue;
      if (x * y = 0) then
        guessed := bestGuessAPP(x, y)
      else
        guessed := bestGuessAPP(x - 1, y) + bestGuessAPP(x, y - 1) -
          bestGuessAPP(x - 1, y - 1);
      if LocateAlignmentPattern(image, estimateModuleSizeAt(x, y), guessed,
        found) then
      begin
        apP[y * nx + x] := found;
        apFound[y * nx + x] := true;
      end;
    end;

  // go over the whole set of alignment patterns again and try to fill any
  // remaining gap by using available neighbors as guides
  for y := 0 to n do
    for x := 0 to n do
    begin
      if apFound[y * nx + x] then
        continue;

      // find the two closest valid alignment pattern pixel positions both
      // horizontally and vertically
      hori := nil;
      verti := nil;
      i := 2;
      while (i < 2 * n + 2) and (System.Length(hori) < 2) do
      begin
        xi := x + (i div 2) * IfThen(Odd(i), 1, -1);
        if (0 <= xi) and (xi <= n) and apFound[y * nx + xi] then
          hori := hori + [apP[y * nx + xi]];
        Inc(i);
      end;
      i := 2;
      while (i < 2 * n + 2) and (System.Length(verti) < 2) do
      begin
        yi := y + (i div 2) * IfThen(Odd(i), 1, -1);
        if (0 <= yi) and (yi <= n) and apFound[yi * nx + x] then
          verti := verti + [apP[yi * nx + x]];
        Inc(i);
      end;

      // if we found 2 each, intersect the two lines that are formed by
      // connecting the point pairs
      if (System.Length(hori) = 2) and (System.Length(verti) = 2) then
      begin
        lh := TRegressionLine.Create(hori[0], hori[1]);
        lv := TRegressionLine.Create(verti[0], verti[1]);
        try
          guessed := Intersect(lh, lv);
          // search again near that intersection and if the search fails, use
          // the intersection
          if LocateAlignmentPattern(image, estimateModuleSizeAt(x, y),
            guessed, found) then
            apP[y * nx + x] := found
          else
            apP[y * nx + x] := guessed;
          apFound[y * nx + x] := true;
        finally
          lh.Free;
          lv.Free;
        end;
      end;
    end;
end;

/// <summary>The grid of dimension modules sampled with mod2Pix, the module
/// centers moved by dx, dy pixels.</summary>
function PlainGrid(image: TBitMatrix; dimension: Integer;
  const mod2Pix: TPerspectiveTransformF; dx, dy: Double): TBitMatrix;
begin
  var rois: TArray<TGridROI>;
  SetLength(rois, 1);
  rois[0].x0 := 0;
  rois[0].x1 := dimension;
  rois[0].y0 := 0;
  rois[0].y1 := dimension;
  rois[0].mod2Pix := mod2Pix;
  Result := SampleGridROIs(image, dimension, dimension, rois, dx, dy);
end;

/// <summary>Samples the grid of the symbol with the finder patterns fp in
/// one or more ways and passes the results to onCandidate. Returns true
/// when it stopped the detection.</summary>
function SampleQR(image: TBitMatrix; const fp: TFinderPatternSet;
  const onCandidate: TQRCodeCandidate): Boolean;
var
  points: TArray<IResultPoint>;

  // sample(dx, dy) samples the grid with the module centers moved by dx, dy
  // pixels
  function yieldGrid(const sample: TFunc<Double, Double, TBitMatrix>;
    dimension: Integer; const m2p: TPerspectiveTransformF): Boolean;
  const
    // half a pixel beside: the module centers of a symbol that is not read
    // can lie on the edge of their modules (small modules, round dots), see
    // qrcode-2/qr-circles-1.png
    OFFSETS: array [0 .. 3, 0 .. 1] of Double = ((-0.5, 0), (0.5, 0),
      (0, -0.5), (0, 0.5));
  begin
    Result := false;
    var bits := sample(0, 0);
    if (bits = nil) then
      exit;
    // the corners of the symbol (with the transformation of its tiles
    // only approximately)
    var position: TArray<IResultPoint> := [ResultPointOf(m2p.Map(PointD(0,
      0))), ResultPointOf(m2p.Map(PointD(dimension, 0))),
      ResultPointOf(m2p.Map(PointD(dimension, dimension))),
      ResultPointOf(m2p.Map(PointD(0, dimension)))];
    var isQRCode: Boolean;
    try
      Result := onCandidate(bits, points, position);
      // not read, but certainly a QR Code: its format information is
      // readable
      isQRCode := not Result and HasFormatInformation(bits);
    finally
      bits.Free;
    end;
    if not isQRCode then
      exit;

    // sample it once more half a pixel beside in each direction
    for var i := 0 to High(OFFSETS) do
    begin
      bits := sample(OFFSETS[i, 0], OFFSETS[i, 1]);
      if (bits = nil) then
        continue;
      try
        if onCandidate(bits, points, position) then
          exit(true);
      finally
        bits.Free;
      end;
    end;
  end;

var
  bl2, bl3, tr2, tr3: TRegressionLine;
begin
  Result := false;
  points := [ResultPointOf(fp.bl.p), ResultPointOf(fp.tl.p),
    ResultPointOf(fp.tr.p)];

  var top := EstimateDimension(image, fp.tl, fp.tr);
  var left := EstimateDimension(image, fp.tl, fp.bl);
  if (top.dim = 0) and (left.dim = 0) then
    exit;

  var best: TDimensionEstimate;
  if (top.err = left.err) then
  begin
    if (top.dim > left.dim) then
      best := top
    else
      best := left;
  end
  else if (top.err < left.err) then
    best := top
  else
    best := left;
  var dimension := best.dim;
  var moduleSize: Double;
  if (top.dim = left.dim) then
    moduleSize := (top.ms + left.ms) / 2
  else
    moduleSize := best.ms;

  var br := PointD(-1, -1);
  var brOffset := PointD(3, 3);
  var brFound := false;

  // Everything except version 1 (21 modules) has an alignment pattern.
  // Estimate the center of that by intersecting line extensions of the 1
  // module wide square around the finder patterns.
  bl2 := nil;
  bl3 := nil;
  tr2 := nil;
  tr3 := nil;
  try
    // the outer and inner edge of the 1 module wide black line between the
    // two outer and the inner (tl) finder pattern
    bl2 := TraceLine(image, fp.bl.p, fp.tl.p, 2);
    bl3 := TraceLine(image, fp.bl.p, fp.tl.p, 3);
    tr2 := TraceLine(image, fp.tr.p, fp.tl.p, 2);
    tr3 := TraceLine(image, fp.tr.p, fp.tl.p, 3);

    if bl2.IsValid and tr2.IsValid and bl3.IsValid and tr3.IsValid then
    begin
      // intersect both outer and inner line pairs and take the center point
      // between the two intersection points
      var brInter := (Intersect(bl2, tr2) + Intersect(bl3, tr3)) / 2;

      var brCP: TPointD;
      if (dimension > 21) and LocateAlignmentPattern(image, moduleSize,
        brInter, brCP) then
        br := brCP;

      brFound := IsInImage(image, br);
      if not brFound then
        br := brInter;
    end;

    // otherwise or if finder patterns are not square, the simple estimation
    // is used as a best guess fallback
    var square: TQuadrilateralF;
    if not IsInImage(image, br) or not FitSquareToPoints(image, fp.bl.p,
      Trunc(fp.bl.size), 2, false, square) then
    begin
      br := fp.tr.p - fp.tl.p + fp.bl.p;
      brOffset := PointD(0, 0);
    end;

    var mod2Pix := MakeMod2Pix(dimension, brOffset, Quad(fp.tl.p, fp.tr.p, br,
      fp.bl.p));

    // version 7 and up: read the version and sample tiled between the
    // alignment patterns
    if (dimension >= 17 + 4 * 7) then
    begin
      var version := ReadVersion(image, dimension, mod2Pix);

      // if the version bits are garbage -> discard the detection
      if (version = nil) or (Min(Abs(version.DimensionForVersion - top.dim),
        Abs(version.DimensionForVersion - left.dim)) > 8) then
        exit;
      if (version.DimensionForVersion <> dimension) then
      begin
        dimension := version.DimensionForVersion;
        mod2Pix := MakeMod2Pix(dimension, brOffset, Quad(fp.tl.p, fp.tr.p, br,
          fp.bl.p));
      end;

      var apM := version.alignmentPatternCenters;
      var apP: TArray<TPointD>;
      var apFound: TArray<Boolean>;
      LocateAlignmentPatterns(image, fp, apM, mod2Pix, apP, apFound);

      var n := System.Length(apM) - 1;
      if apFound[n * System.Length(apM) + n] then
        mod2Pix := MakeMod2Pix(dimension, PointD(3, 3), Quad(fp.tl.p, fp.tr.p,
          apP[n * System.Length(apM) + n], fp.bl.p));

      var m2p := mod2Pix;
      if yieldGrid(
        function(dx, dy: Double): TBitMatrix
        begin
          Result := SampleGridAligned(image, dimension, dimension, m2p, apP,
            apFound, apM, apM, dx, dy);
        end, dimension, mod2Pix) then
        exit(true);
    end
    else
    begin
      var m2p := mod2Pix;
      if yieldGrid(
        function(dx, dy: Double): TBitMatrix
        begin
          Result := PlainGrid(image, dimension, m2p, dx, dy);
        end, dimension, mod2Pix) then
        exit(true);
    end;

    // if we have not found the br alignment pattern, we check
    // a) if we have a version 1 symbol and tried and failed with the
    //    intersection of the trace lines, or
    // b) if the symbol is almost level and the resolution of the regression
    //    lines is not sufficient
    // we then try the fallback method of sampling the symbol with the br
    // corner extrapolated from the other three corners.
    if not brFound and (((dimension = 21) and (brOffset <> PointD(0, 0))) or
      ((EstimateTilt(fp) < 1.1) and not (bl2.IsHighRes and bl3.IsHighRes and
      tr2.IsHighRes and tr3.IsHighRes))) then
    begin
      mod2Pix := MakeMod2Pix(dimension, PointD(0, 0), Quad(fp.tl.p, fp.tr.p,
        fp.tr.p - fp.tl.p + fp.bl.p, fp.bl.p));
      var m2p := mod2Pix;
      if yieldGrid(
        function(dx, dy: Double): TBitMatrix
        begin
          Result := PlainGrid(image, dimension, m2p, dx, dy);
        end, dimension, mod2Pix) then
        exit(true);
    end;
  finally
    bl2.Free;
    bl3.Free;
    tr2.Free;
    tr3.Free;
  end;
end;

function DetectQRCodesByFinderPatterns(image: TBitMatrix; tryHarder: Boolean;
  const onCandidate: TQRCodeCandidate; maxSymbols: Integer): Boolean;
var
  floatMask: TFloatExceptionsMasked; // masked until this function returns
begin
  Result := false;
  if (image = nil) then
    exit;

  var allFPs := FindFinderPatterns(image, tryHarder);
  // the finder patterns of decoded symbols are not used for another one
  var usedFPs := TList<TConcentricPattern>.Create;
  try
    var count := 0;
    for var fpSet in GenerateFinderPatternSets(allFPs) do
    begin
      if ContainsPattern(usedFPs, fpSet.bl) or ContainsPattern(usedFPs,
        fpSet.tl) or ContainsPattern(usedFPs, fpSet.tr) then
        continue;
      if SampleQR(image, fpSet, onCandidate) then
      begin
        Result := true;
        usedFPs.Add(fpSet.bl);
        usedFPs.Add(fpSet.tl);
        usedFPs.Add(fpSet.tr);
        Inc(count);
        if (maxSymbols > 0) and (count >= maxSymbols) then
          exit;
      end;
    end;
  finally
    usedFPs.Free;
  end;
end;

function FindQRFinderPatterns(image: TBitMatrix; tryHarder: Boolean)
  : TArray<TConcentricPattern>;
var
  floatMask: TFloatExceptionsMasked; // masked until this function returns
begin
  Result := FindFinderPatterns(image, tryHarder);
end;

function LocateQRAlignmentPattern(image: TBitMatrix; moduleSize: Double;
  const estimate: TPointD; out res: TPointD): Boolean;
var
  floatMask: TFloatExceptionsMasked; // masked until this function returns
begin
  Result := LocateAlignmentPattern(image, moduleSize, estimate, res);
end;

function DetectPureQRCode(image: TBitMatrix;
  out points: TArray<IResultPoint>): TBitMatrix;
const
  MIN_MODULES = 21;
var
  floatMask: TFloatExceptionsMasked; // masked until this function returns
begin
  Result := nil;
  var left, top, width, height: Integer;
  if not image.findBoundingBox(left, top, width, height, MIN_MODULES) or
    (Abs(width - height) > 1) then
    exit;
  var right := left + width - 1;
  var bottom := top + height - 1;

  // allow corners be moved one pixel inside to accommodate for possible
  // aliasing artifacts
  var starts: array [0 .. 2] of TPoint;
  var dirs: array [0 .. 2] of TPoint;
  starts[0] := PointI(left, top);
  dirs[0] := PointI(1, 1);
  starts[1] := PointI(right, top);
  dirs[1] := PointI(-1, 1);
  starts[2] := PointI(left, bottom);
  dirs[2] := PointI(1, -1);
  var diagonal: TArray<Integer>;
  for var k := 0 to 2 do
  begin
    var cur := TBitMatrixCursorI.Create(image, starts[k], dirs[k]);
    diagonal := cur.ReadPatternFromBlack(5, 1, width div 3 + 1);
    if (diagonal = nil) or (IsPattern(diagonal, PATTERN, false) = 0) then
      exit;
  end;

  var fpWidth: Double := 0;
  for var v in diagonal do
    fpWidth := fpWidth + v;
  var dimension := EstimateDimension(image,
    TConcentricPattern.Create(PointD(left, top) + (fpWidth / 2) * PointD(1, 1),
    fpWidth), TConcentricPattern.Create(PointD(right, top) + (fpWidth / 2) *
    PointD(-1, 1), fpWidth)).dim;

  var moduleSize: Single := width / dimension;
  if not ((dimension >= 21) and (dimension <= 177) and (dimension mod 4 = 1))
    or not IsInImage(image, PointD(left + moduleSize / 2 + (dimension - 1) *
    moduleSize, top + moduleSize / 2 + (dimension - 1) * moduleSize)) then
    exit;

  // now just read off the bits (this is a crop + subsample)
  Result := TBitMatrix.Create(dimension, dimension);
  for var y := 0 to dimension - 1 do
  begin
    var py: Single := top + moduleSize / 2 + y * moduleSize;
    for var x := 0 to dimension - 1 do
    begin
      var px: Single := left + moduleSize / 2 + x * moduleSize;
      if IsInImage(image, PointD(px, py)) and image[FloorInt(px), FloorInt(py)] then
        Result[x, y] := true;
    end;
  end;

  points := [TResultPointHelpers.CreateResultPoint(left, bottom),
    TResultPointHelpers.CreateResultPoint(left, top),
    TResultPointHelpers.CreateResultPoint(right, top)];
end;

end.
