unit ZXing.Aztec.Internal.Detector;

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

  * Ported from zxing-cpp (AZDetector.cpp): finds the bull's eye center of
  * Aztec Codes (compact and full range, and Aztec Runes), reads the
  * orientation and the mode message around it and samples the grid, with
  * the reference grid (timing patterns every 16 modules) of large symbols.
}

interface

uses
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint;

type
  /// <summary>A sampled Aztec symbol and the parameters of its mode
  /// message.</summary>
  TAztecDetectorResult = class
  public
    /// <summary>The modules of the symbol (owned).</summary>
    Bits: TBitMatrix;
    /// <summary>The corners of the symbol in the image: top left, top right,
    /// bottom right, bottom left.</summary>
    Position: TArray<IResultPoint>;
    Compact: Boolean;
    NbDataBlocks: Integer;
    /// <summary>0 for an Aztec Rune.</summary>
    NbLayers: Integer;
    ReaderInit: Boolean;
    IsMirrored: Boolean;
    /// <summary>The value of an Aztec Rune (NbLayers 0), else -1.</summary>
    RuneValue: Integer;
    destructor Destroy; override;
  end;

  /// <summary>Gets every symbol found (freed by the detector after the
  /// call). Return true to stop the detection.</summary>
  TAztecCandidate = reference to function(detected: TAztecDetectorResult)
    : Boolean;

/// <summary>
/// Looks for Aztec Codes: with isPure for one symbol that fills the image,
/// otherwise for bull's eye centers on every row (tryHarder) or every few
/// rows in the middle half of the image.
/// </summary>
procedure DetectAztec(image: TBitMatrix; isPure, tryHarder: Boolean;
  const onCandidate: TAztecCandidate);

implementation

uses
  System.Types,
  System.Math,
  System.Generics.Collections,
  ZXing.Common.Geometry,
  ZXing.Common.BitMatrixCursor,
  ZXing.Common.Pattern,
  ZXing.Common.ConcentricFinder,
  ZXing.Common.LocalGrid,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder;

{ TAztecDetectorResult }

destructor TAztecDetectorResult.Destroy;
begin
  Bits.Free;
  inherited;
end;

{ geometry helpers }

/// <summary>The square of size modules around (0, 0): -size div 2 to
/// size div 2, like zxing-cpp's CenteredSquare.</summary>
function CenteredSquare(size: Integer): TQuadrilateralF;
begin
  var h := size div 2;
  Result[0] := PointD(-h, -h);
  Result[1] := PointD(h, -h);
  Result[2] := PointD(h, h);
  Result[3] := PointD(-h, h);
end;

function MoveQuad(const q: TQuadrilateralF; const offset: TPointD)
  : TQuadrilateralF;
begin
  for var i := 0 to 3 do
    Result[i] := q[i] + offset;
end;

/// <summary>The corners of q rotated by n positions (and mirrored).
/// </summary>
function RotatedCorners(const q: TQuadrilateralF; n: Integer;
  mirror: Boolean): TQuadrilateralF;
begin
  for var i := 0 to 3 do
    Result[i] := q[(i + n + 4) mod 4];
  if mirror then
  begin
    var t := Result[1];
    Result[1] := Result[3];
    Result[3] := t;
  end;
end;

function ResultPointOf(const p: TPointD): IResultPoint;
begin
  Result := TResultPointHelpers.CreateResultPoint(p.X, p.Y);
end;

{ finding the center }

/// <summary>Whether the 7 bars and spaces of view (starting white) are about
/// equally wide, with at least as much around them.</summary>
function IsAztecCenterPattern(const view: TPatternView): Boolean;
begin
  // the min and max of all pairs of a black and white run, close together
  var m := view[0] + view[1];
  var mx := m;
  for var i := 1 to view.Size - 2 do
  begin
    var v := view[i] + view[i + 1];
    m := Min(m, v);
    mx := Max(mx, v);
  end;
  Result := (mx <= m * 4 div 3 + 1) and
    (view[-1] >= view[view.Size div 2] - 2) and
    (view[view.Size] >= view[view.Size div 2] - 2);
end;

/// <summary>The first 1:1:1:1:1:1:1 center pattern of a compact Aztec Code
/// in view; an invalid view when there is none.</summary>
function FindAztecCenterPattern(const view: TPatternView): TPatternView;
const
  MIN_SIZE = 8; // Aztec Runes
begin
  var window := view.SubView(0, 7);
  var last := view.Data + view.Size - MIN_SIZE;
  while (window.Data < last) do
  begin
    if IsAztecCenterPattern(window) then
      exit(window);
    window.SkipPair;
  end;
  Result := TPatternView.Empty;
end;

/// <summary>The width of the symmetric center pattern (a white center and 3
/// rings on both sides) around the cursor in its direction, 0 when it does
/// not fit. With updatePosition the cursor moves to its center.</summary>
function CheckSymmetricAztecCenterPattern(var cur: TBitMatrixCursorI;
  range: Integer; updatePosition: Boolean): Integer;
var
  m, mx, spread, center: Integer;

  // 3 more edges in the direction of c, with runs about as wide as the
  // center
  function checkSide(var c: TFastEdgeToEdgeCounter): Boolean;
  begin
    Result := false;
    var lastS := center;
    for var i := 0 to 2 do
    begin
      var s := c.StepToNextEdge(range - spread);
      if (s = 0) then
        exit;
      var v := s + lastS;
      if (m = 0) then
      begin
        m := v;
        mx := v;
      end
      else
      begin
        m := Min(m, v);
        mx := Max(mx, v);
      end;
      if (mx > m * 4 div 3 + 1) then
        exit;
      Inc(spread, s);
      lastS := s;
    end;
    Result := true;
  end;

begin
  Result := 0;
  // tilted symbols may have a larger vertical than horizontal range
  range := range * 2;

  var curFwd := TFastEdgeToEdgeCounter.Create(cur);
  var curBwd := TFastEdgeToEdgeCounter.Create(cur.TurnedBack);

  var centerFwd := curFwd.StepToNextEdge(range div 7);
  if (centerFwd = 0) then
    exit;
  var centerBwd := curBwd.StepToNextEdge(range div 7);
  if (centerBwd = 0) then
    exit;
  // -1 because the starting pixel is counted twice
  center := centerFwd + centerBwd - 1;
  if (center > range div 7) or (center < range div (4 * 7)) then
    exit;

  spread := center;
  m := 0;
  mx := 0;
  if not checkSide(curFwd) or not checkSide(curBwd) then
    exit;

  if updatePosition then
    cur.Step(centerFwd - centerBwd);
  Result := spread;
end;

/// <summary>The center pattern around center in 4 directions.</summary>
function LocateAztecCenter(image: TBitMatrix; const center: TPointD;
  spreadH: Integer; out res: TConcentricPattern): Boolean;
const
  DIRS: array [0 .. 3, 0 .. 1] of Integer = ((0, 1), (1, 0), (1, 1), (1, -1));
begin
  Result := false;
  var cur := TBitMatrixCursorI.Create(image, ToPointI(center), Point(0, 0));
  var minSpread := spreadH;
  var maxSpread := 0;
  for var i := 0 to 3 do
  begin
    cur.d := Point(DIRS[i, 0], DIRS[i, 1]);
    var spread := CheckSymmetricAztecCenterPattern(cur, spreadH,
      DIRS[i, 0] = 0);
    if (spread = 0) then
      exit;
    minSpread := Min(minSpread, spread);
    maxSpread := Max(maxSpread, spread);
  end;
  res := TConcentricPattern.Create(CenteredI(cur.p),
    (maxSpread + minSpread) / 2);
  Result := true;
end;

/// <summary>The center of a pure symbol that fills the image.</summary>
function FindPureFinderPattern(image: TBitMatrix): TArray<TConcentricPattern>;
const
  PATTERN: array [0 .. 6] of Integer = (1, 1, 1, 1, 1, 1, 1);
var
  left, top, width, height: Integer;
  found: TArray<TConcentricPattern>;

  function tryLocate(l, t, size: Integer): Boolean;
  begin
    var p: TConcentricPattern;
    Result := LocateConcentricPattern(image, PATTERN, true,
      PointD(l + size div 2, t + size div 2), size div 2, p);
    if Result then
      found := [p];
  end;

begin
  Result := nil;
  found := nil;
  // the smallest symbol is an Aztec Rune of 11x11 modules; the runes 68 and
  // 223 have no bits set on their bottom row
  if not image.findBoundingBox(left, top, width, height, 10) then
    exit;

  // symbols can have a blank row or column at the edge
  if (width = height) and (width >= 11) then
    tryLocate(left, top, width)
  else if (width < height) and (width >= height * 10 div 11) and
    (image.Width >= height) then
  begin
    if not tryLocate(left, top, height) then
      tryLocate(left - (height - width), top, height);
  end
  else if (height < width) and (height >= width * 10 div 11) and
    (image.Height >= width) then
  begin
    if not tryLocate(left, top, width) then
      tryLocate(left, top - (width - height), width);
  end;
  Result := found;
end;

/// <summary>The center patterns found on the rows of the image.</summary>
function FindFinderPatterns(image: TBitMatrix; tryHarder: Boolean)
  : TArray<TConcentricPattern>;
begin
  var res := TList<TConcentricPattern>.Create;
  try
    var skip := 1;
    var margin := 5;
    if not tryHarder then
    begin
      skip := EnsureRange(image.Height div 2 div 100, 1, 5);
      margin := image.Height div 4;
    end;

    var row: TPatternRow;
    var y := margin;
    while (y < image.Height - margin) do
    begin
      GetPatternRow(image, y, row);
      var next := TPatternView.Create(row);
      // the center pattern starts with white and is 7 wide (compact code)
      next.Shift(1);
      while true do
      begin
        next := FindAztecCenterPattern(next);
        if not next.IsValid then
          break;
        var p := PointD(next.PixelsInFront + next[0] + next[1] + next[2] +
          next[3] / 2, y + 0.5);

        // not inside a pattern found already (from back to front, until out
        // of range in y)
        var found := false;
        for var i := res.Count - 1 downto 0 do
        begin
          var old := res[i];
          if (p.Y - old.p.Y > old.size / 2) then
            break;
          if (PointDistance(p, old.p) < old.size / 2) then
          begin
            found := true;
            break;
          end;
        end;

        if not found then
        begin
          var pattern: TConcentricPattern;
          if LocateAztecCenter(image, p, next.Sum, pattern) then
            res.Add(pattern);
        end;

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

{ orientation and mode message }

/// <summary>The rotation (0 to 3) of the 12 orientation bits, -1 when they
/// do not fit (at most 2 wrong bits).</summary>
function FindRotation(bits: Cardinal; mirror: Boolean): Integer;
begin
  var mask: Cardinal := $EE0; // 111 011 100 000
  if mirror then
    mask := $E0E; // 111 000 001 110
  for var i := 0 to 3 do
  begin
    var diff := mask xor bits;
    var count := 0;
    while (diff <> 0) do
    begin
      Inc(count, diff and 1);
      diff := diff shr 1;
    end;
    if (count <= 2) then
      exit(i);
    // rotate left by 3 bits (12 bits)
    bits := ((bits shl 3) and $FFF) or ((bits shr 9) and 7);
  end;
  Result := -1;
end;

const
  CORNER_DIRS: array [0 .. 3, 0 .. 1] of Integer = ((-1, -1), (1, -1), (1, 1),
    (-1, 1));

/// <summary>The 4 * 3 orientation bits at the corners of the finder pattern
/// at radius; 0 when outside the image.</summary>
function SampleOrientationBits(image: TBitMatrix;
  const mod2Pix: TPerspectiveTransformF; radius: Integer): Cardinal;
begin
  Result := 0;
  for var k := 0 to 3 do
  begin
    var dx := CORNER_DIRS[k, 0];
    var dy := CORNER_DIRS[k, 1];
    var corner := Point(radius * dx, radius * dy);
    var cornerL := Point(corner.X, corner.Y - dy);
    var cornerR := Point(corner.X - dx, corner.Y);
    if (dx <> dy) then
    begin
      var t := cornerL;
      cornerL := cornerR;
      cornerR := t;
    end;
    var corners: TArray<TPoint> := [cornerL, corner, cornerR];
    for var ps in corners do
    begin
      var p := mod2Pix.Map(PointD(ps.X, ps.Y));
      if not IsInImage(image, p) then
        exit(0);
      Result := (Result shl 1) or Cardinal(Ord(BlackAtPoint(image, p)));
    end;
  end;
end;

/// <summary>The mode message read around the finder pattern at radius (5:
/// compact, 7: full range), error corrected; -1 when it can not be read.
/// isRune when it is the one of an Aztec Rune.</summary>
function ModeMessage(image: TBitMatrix; const mod2Pix: TPerspectiveTransformF;
  radius: Integer; out isRune: Boolean): Integer;
begin
  Result := -1;
  var compact := (radius = 5);
  isRune := false;

  // the bits between the corner bits along the 4 edges
  var bits: UInt64 := 0;
  for var k := 0 to 3 do
  begin
    var dx := CORNER_DIRS[k, 0];
    var dy := CORNER_DIRS[k, 1];
    var next: TPoint;
    if (dx = dy) then
      next := Point(-dx, 0)
    else
      next := Point(0, -dy);
    for var i := 2 to 2 * radius - 2 do
    begin
      // the timing pattern
      if not compact and (i = 7) then
        continue;
      var p := mod2Pix.Map(PointD(radius * dx + i * next.X,
        radius * dy + i * next.Y));
      if not IsInImage(image, p) then
        exit;
      bits := (bits shl 1) or UInt64(Ord(BlackAtPoint(image, p)));
    end;
  end;

  // error correction of the 4 bit words
  var numCodewords := 10;
  var numDataCodewords := 4;
  if compact then
  begin
    numCodewords := 7;
    numDataCodewords := 2;
  end;
  var numECCodewords := numCodewords - numDataCodewords;

  var words: TArray<Integer>;
  SetLength(words, numCodewords);
  for var i := numCodewords - 1 downto 0 do
  begin
    words[i] := Integer(bits and $F);
    bits := bits shr 4;
  end;

  var rs := TReedSolomonDecoder.Create(TGenericGF.AZTEC_PARAM);
  try
    var tried := Copy(words);
    var ok := rs.decode(tried, numECCodewords);
    if not ok and compact then
    begin
      // an Aztec Rune has its mode message inverted in a pattern
      tried := Copy(words);
      for var i := 0 to High(tried) do
        tried[i] := tried[i] xor $A;
      ok := rs.decode(tried, numECCodewords);
      isRune := ok;
    end;
    if not ok then
      exit;
    Result := 0;
    for var i := 0 to numDataCodewords - 1 do
      Result := (Result shl 4) + tried[i];
  finally
    rs.Free;
  end;
end;

procedure ExtractParameters(modeMessage: Integer; compact: Boolean;
  out nbLayers, nbDataBlocks: Integer; out readerInit: Boolean);
begin
  readerInit := false;
  if compact then
  begin
    // 2 bits layers and 6 bits data blocks
    nbLayers := (modeMessage shr 6) + 1;
    // ISO/IEC 24778:2008 section 9: the highest bit set artificially
    if (nbLayers = 1) and (modeMessage and $20 <> 0) then
    begin
      readerInit := true;
      modeMessage := modeMessage and not $20;
    end;
    nbDataBlocks := (modeMessage and $3F) + 1;
  end
  else
  begin
    // 5 bits layers and 11 bits data blocks
    nbLayers := (modeMessage shr 11) + 1;
    if (nbLayers <= 22) and (modeMessage and $400 <> 0) then
    begin
      readerInit := true;
      modeMessage := modeMessage and not $400;
    end;
    nbDataBlocks := (modeMessage and $7FF) + 1;
  end;
end;

{ detection }

procedure DetectAztec(image: TBitMatrix; isPure, tryHarder: Boolean;
  const onCandidate: TAztecCandidate);
var
  radius, mirror, rotate, modeMsg: Integer;
  isRune: Boolean;

  // the orientation and mode message, compact (radius 5) or full range (7)
  function parseModeMessage(const srcQuad, fpQuad: TQuadrilateralF): Boolean;
  begin
    // ISO/IEC 24778:2008(E) 14.3.3 allows 3 wrong orientation bits, but
    // some patterns of different orientations differ in only 4 bits: at
    // most 2 are accepted, and the mode message decides
    radius := 5;
    while (radius <= 7) do
    begin
      var bits := SampleOrientationBits(image,
        TPerspectiveTransformF.Create(srcQuad, fpQuad), radius);
      if (bits <> 0) then
      begin
        mirror := 0;
        while (mirror <= 1) do
        begin
          rotate := FindRotation(bits, mirror = 1);
          if (rotate <> -1) then
          begin
            modeMsg := ModeMessage(image,
              TPerspectiveTransformF.Create(srcQuad, RotatedCorners(fpQuad,
              rotate, mirror = 1)), radius, isRune);
            if (modeMsg <> -1) then
              exit(true);
          end;
          Inc(mirror);
        end;
      end;
      Inc(radius, 2);
    end;
    Result := false;
  end;

begin
  var fps: TArray<TConcentricPattern>;
  if isPure then
    fps := FindPureFinderPattern(image)
  else
    fps := FindFinderPatterns(image, tryHarder);

  for var fp in fps do
  begin
    var fpQuad: TQuadrilateralF;
    if not FindConcentricPatternCorners(image, fp.p, Trunc(fp.size), 3, fpQuad)
    then
      continue;

    var srcQuad := CenteredSquare(7);
    var mod2Pix := TPerspectiveTransformF.Create(srcQuad, fpQuad);
    if not mod2Pix.IsValid then
      continue;

    modeMsg := -1;
    isRune := false;
    if not parseModeMessage(srcQuad, fpQuad) or (radius = 7) then
    begin
      // more precise: extrapolate from the outer square of white pixels (5
      // edges away from the center)
      var fpQuad5: TQuadrilateralF;
      if FindConcentricPatternCorners(image, fp.p, Trunc(fp.size * 5 / 3), 5,
        fpQuad5) then
        if parseModeMessage(CenteredSquare(11), fpQuad5) and (radius = 7) then
        begin
          srcQuad := CenteredSquare(11);
          fpQuad := fpQuad5;
        end;
      if (modeMsg = -1) then
        continue;
    end;

    fpQuad := RotatedCorners(fpQuad, rotate, mirror = 1);

    var nbLayers := 0;
    var nbDataBlocks := 0;
    var readerInit := false;
    if not isRune then
      ExtractParameters(modeMsg, radius = 5, nbLayers, nbDataBlocks,
        readerInit);

    var dim: Integer;
    if (radius = 5) then
      dim := 4 * nbLayers + 11
    else
      dim := 4 * nbLayers + 2 * ((2 * nbLayers + 6) div 15) + 15;

    var center := PointD(dim / 2, dim / 2);
    srcQuad := MoveQuad(srcQuad, center);
    mod2Pix := TPerspectiveTransformF.Create(srcQuad, fpQuad);

    var bits: TBitMatrix;
    if (dim >= 35) then
    begin
      // the reference grid (timing patterns every 16 modules): from the
      // center outwards
      var r0 := (dim div 2) div 16;
      var firstTimingPattern := dim div 2 - r0 * 16;
      var apM: TArray<Integer> := nil;
      var i := firstTimingPattern;
      while (i < dim) do
      begin
        apM := apM + [i];
        Inc(i, 16);
      end;
      var n := Length(apM);
      var apP: TArray<TPointD>;
      var apFound: TArray<Boolean>;
      SetLength(apP, n * n);
      SetLength(apFound, n * n);
      apP[(n div 2) * n + n div 2] := mod2Pix.Map(center);
      apFound[(n div 2) * n + n div 2] := true;

      for var r := r0 - 1 downto 0 do
      begin
        var src := MoveQuad(CenteredSquare(32 * (r0 - r)), center);
        var dst: TQuadrilateralF;
        var grid := TLocalGrid.Create(image, mod2Pix, Point(dim, dim));
        try
          for var k := 0 to 3 do
          begin
            var px := (r0 - r) * CORNER_DIRS[k, 0] + n div 2;
            var py := (r0 - r) * CORNER_DIRS[k, 1] + n div 2;
            var pos: TPointD;
            grid.At(ToPointI(src[k]), Point(0, 0));
            if grid.FindTimingPatternCross(true, 4, pos) then
            begin
              apP[py * n + px] := pos;
              apFound[py * n + px] := true;
              dst[k] := pos;
            end
            else
              dst[k] := mod2Pix.Map(src[k]);
          end;
        finally
          grid.Free;
        end;
        mod2Pix := TPerspectiveTransformF.Create(src, dst);
      end;

      // the remaining (not corner) alignment patterns
      var grid := TLocalGrid.Create(image, mod2Pix, Point(dim, dim));
      try
        for var y := 0 to n - 1 do
          for var x := 0 to n - 1 do
            if not apFound[y * n + x] then
            begin
              var pos: TPointD;
              grid.At(Point(apM[x], apM[y]), Point(0, 0));
              if grid.FindTimingPatternCross(true, 4, pos) then
              begin
                apP[y * n + x] := pos;
                apFound[y * n + x] := true;
              end;
            end;
      finally
        grid.Free;
      end;

      bits := SampleGridAligned(image, dim, dim, mod2Pix, apP, apFound,
        apM, apM);
    end
    else
    begin
      var rois: TArray<TGridROI>;
      SetLength(rois, 1);
      rois[0].x0 := 0;
      rois[0].x1 := dim;
      rois[0].y0 := 0;
      rois[0].y1 := dim;
      rois[0].mod2Pix := mod2Pix;
      bits := SampleGridROIs(image, dim, dim, rois);
    end;

    if (bits = nil) then
      continue;

    var detected := TAztecDetectorResult.Create;
    try
      detected.Bits := bits;
      detected.Position := [ResultPointOf(mod2Pix.Map(PointD(0, 0))),
        ResultPointOf(mod2Pix.Map(PointD(dim, 0))),
        ResultPointOf(mod2Pix.Map(PointD(dim, dim))),
        ResultPointOf(mod2Pix.Map(PointD(0, dim)))];
      detected.Compact := (radius = 5);
      detected.NbDataBlocks := nbDataBlocks;
      detected.NbLayers := nbLayers;
      detected.ReaderInit := readerInit;
      detected.IsMirrored := (mirror <> 0);
      detected.RuneValue := -1;
      if isRune then
        detected.RuneValue := modeMsg;
      if onCandidate(detected) then
        exit;
    finally
      detected.Free;
    end;
  end;
end;

end.
