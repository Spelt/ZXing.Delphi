{
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

  * Detector of the 4-state (and 2-state) postal barcodes for ZXing.Delphi:
  * a row of equally spaced bars that differ in height. zxing-cpp and ZXing
  * Java have none.
}

unit ZXing.Postal.FourStateDetector;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.Common.Pattern;

const
  // the states of the bars, as zint: full, ascender, descender, tracker
  BAR_FULL = 0;
  BAR_ASCENDER = 1;
  BAR_DESCENDER = 2;
  BAR_TRACKER = 3;

type
  /// <summary>The bars of a postal barcode: their states from left to right
  /// (or top to bottom in a transposed image) and the centers of the first
  /// and the last bar.</summary>
  TPostalBars = record
    States: TArray<Byte>;
    /// <summary>The least certain decisions (ascender or not, descender or
    /// not), the least certain first: 2 * bar + 0 for the top, + 1 for the
    /// bottom.</summary>
    Uncertain: TArray<Integer>;
    FirstX, FirstY, LastX, LastY: Double;
    /// <summary>The states reversed (read from the other end) and/or with
    /// ascenders and descenders swapped (seen from the other side): both
    /// for a barcode turned 180 degrees, one of them for a vertical one in
    /// the image transposed. The decisions Uncertain[i] with bit i of
    /// flips set are changed first.</summary>
    function Variant(reversed, swapped: Boolean;
      flips: Cardinal = 0): TArray<Byte>;
  end;

  /// <summary>An unsigned number of 128 bits (4 words, the lowest first),
  /// for the data of the postal barcodes.</summary>
  TPostalNumber = record
    W: array [0 .. 3] of Cardinal;
    /// <summary>Self * m + a.</summary>
    procedure MulAdd(m, a: Cardinal);
    /// <summary>Self div d; returns Self mod d.</summary>
    function DivMod(d: Cardinal): Cardinal;
    function IsZero: Boolean;
    /// <summary>The value when it fits in 64 bits; false when not.</summary>
    function ToUInt64(out value: UInt64): Boolean;
  end;

/// <summary>The postal barcodes in the image: rows of at least minBars
/// equally spaced bars, found on every rowStep-th row.</summary>
/// <summary>The image turned: rows become columns (the caller frees it).
/// </summary>
function Transposed(image: TBitMatrix): TBitMatrix;

function DetectPostalBars(image: TBitMatrix; rowStep, minBars: Integer)
  : TArray<TPostalBars>;

implementation

type
  TBar = record
    // the center on the row it was found on, and its ends
    X, Y: Double;
    TopX, Top, BottomX, Bottom: Double;
  end;

function Transposed(image: TBitMatrix): TBitMatrix;
begin
  Result := TBitMatrix.Create(image.Height, image.Width);
  for var y := 0 to image.Height - 1 do
    for var x := 0 to image.Width - 1 do
      if image[x, y] then
        Result[y, x] := true;
end;

{ TPostalNumber }

procedure TPostalNumber.MulAdd(m, a: Cardinal);
begin
  var carry: UInt64 := a;
  for var i := 0 to 3 do
  begin
    var v := UInt64(W[i]) * m + carry;
    W[i] := Cardinal(v);
    carry := v shr 32;
  end;
end;

function TPostalNumber.DivMod(d: Cardinal): Cardinal;
begin
  var remainder: UInt64 := 0;
  for var i := 3 downto 0 do
  begin
    var v := (remainder shl 32) or W[i];
    W[i] := Cardinal(v div d);
    remainder := v mod d;
  end;
  Result := Cardinal(remainder);
end;

function TPostalNumber.IsZero: Boolean;
begin
  Result := (W[0] = 0) and (W[1] = 0) and (W[2] = 0) and (W[3] = 0);
end;

function TPostalNumber.ToUInt64(out value: UInt64): Boolean;
begin
  Result := (W[2] = 0) and (W[3] = 0);
  value := UInt64(W[1]) shl 32 or W[0];
end;

{ TPostalBars }

function TPostalBars.Variant(reversed, swapped: Boolean;
  flips: Cardinal): TArray<Byte>;
const
  SWAP: array [0 .. 3] of Byte = (BAR_FULL, BAR_DESCENDER, BAR_ASCENDER,
    BAR_TRACKER);
  // the state with the top (ascender) or the bottom (descender) changed
  FLIP: array [0 .. 1, 0 .. 3] of Byte = ((BAR_DESCENDER, BAR_TRACKER,
    BAR_FULL, BAR_ASCENDER), (BAR_ASCENDER, BAR_FULL, BAR_TRACKER,
    BAR_DESCENDER));
begin
  var flipped := States;
  if (flips <> 0) then
  begin
    flipped := Copy(States);
    for var i := 0 to Min(High(Uncertain), 31) do
      if (flips shr i) and 1 = 1 then
      begin
        var bar := Uncertain[i] div 2;
        flipped[bar] := FLIP[Uncertain[i] mod 2, flipped[bar]];
      end;
  end;
  SetLength(Result, Length(States));
  for var i := 0 to High(States) do
  begin
    var state := flipped[i];
    if swapped then
      state := SWAP[state];
    if reversed then
      Result[High(States) - i] := state
    else
      Result[i] := state;
  end;
end;

/// <summary>The dark run of row y nearest to x (within maxDistance); false
/// when there is none.</summary>
function DarkRunNear(image: TBitMatrix; x: Double; y: Integer;
  maxDistance: Double; out left, right: Integer): Boolean;
begin
  Result := false;
  if (y < 0) or (y >= image.Height) then
    exit;
  var x0 := Round(x);
  // the nearest dark pixel
  var found := -1;
  var d := 0;
  while (d <= maxDistance) do
  begin
    if (x0 - d >= 0) and (x0 - d < image.Width) and image[x0 - d, y] then
    begin
      found := x0 - d;
      break;
    end;
    if (x0 + d >= 0) and (x0 + d < image.Width) and image[x0 + d, y] then
    begin
      found := x0 + d;
      break;
    end;
    Inc(d);
  end;
  if (found < 0) then
    exit;
  left := found;
  while (left > 0) and image[left - 1, y] do
    Dec(left);
  right := found;
  while (right < image.Width - 1) and image[right + 1, y] do
    Inc(right);
  Result := true;
end;

/// <summary>Follows the bar through (x, y) up and down (also a slanted
/// one): its ends; false when it is too wide there or longer than
/// maxLength.</summary>
function TraceBar(image: TBitMatrix; x: Double; y: Integer;
  barWidth, maxLength: Double; var bar: TBar): Boolean;
begin
  Result := false;
  var left, right: Integer;
  if not DarkRunNear(image, x, y, barWidth / 2 + 1, left, right) or
    (right - left + 1 > 2.5 * barWidth + 1) then
    exit;
  bar.X := (left + right + 1) / 2;
  bar.Y := y + 0.5;
  for var direction in [-1, 1] do
  begin
    var cx := bar.X;
    var yy := y;
    repeat
      if (Abs(yy - y) > maxLength) then
        exit;
      // the bar in the next row: near the center of this one, not much
      // wider (else it touches something)
      if not DarkRunNear(image, cx - 0.5, yy + direction, barWidth / 2,
        left, right) or (right - left + 1 > 2.5 * barWidth + 1) then
        break;
      cx := (left + right + 1) / 2;
      Inc(yy, direction);
    until false;
    if (direction < 0) then
    begin
      bar.Top := yy;
      bar.TopX := cx;
    end
    else
    begin
      bar.Bottom := yy + 1;
      bar.BottomX := cx;
    end;
  end;
  Result := true;
end;

/// <summary>The middle of the part that the bars from first to last have
/// in common (the tracker) at x0, along a line with the slope; false when
/// they have nothing in common. When that part is longer than half the
/// longest bar it is no tracker (the bars there are all ascenders and full
/// bars, or all descenders and full bars): then the bars around them too.
/// </summary>
function CommonMiddle(const bars: TArray<TBar>; first, last: Integer;
  slope, x0: Double; out middle: Double): Boolean;
begin
  var maxLength := 0.0;
  for var bar in bars do
    maxLength := Max(maxLength, bar.Bottom - bar.Top);
  repeat
    var top := -MaxDouble;
    var bottom := MaxDouble;
    for var i := Max(first, 0) to Min(last, High(bars)) do
    begin
      var shift := slope * (x0 - bars[i].X);
      top := Max(top, bars[i].Top + shift);
      bottom := Min(bottom, bars[i].Bottom + shift);
    end;
    Result := bottom > top;
    middle := (top + bottom) / 2;
    if not Result or (bottom - top <= 0.5 * maxLength) or
      ((first <= 0) and (last >= High(bars))) then
      exit;
    Dec(first, 4);
    Inc(last, 4);
  until false;
end;

/// <summary>Divides the values in two levels at the largest gap: the
/// threshold between them and the sum of the squared deviations from the
/// means of the levels; false when the gap is smaller than minGap (one
/// level, the deviation from its mean).</summary>
function SplitLevels(const levels: TArray<Double>; minGap: Double;
  out threshold, deviation: Double): Boolean;
begin
  // sorted (a copy)
  var values := Copy(levels);
  TArray.Sort<Double>(values);
  var n := Length(values);
  var best := -1;
  var bestGap := 0.0;
  for var i := 0 to n - 2 do
    if (values[i + 1] - values[i] > bestGap) then
    begin
      bestGap := values[i + 1] - values[i];
      best := i;
    end;
  Result := (best >= 0) and (bestGap >= minGap);
  if Result then
    threshold := (values[best] + values[best + 1]) / 2
  else
    best := n - 1;
  // the deviations within the levels
  deviation := 0;
  for var level := 0 to 1 do
  begin
    var first := 0;
    var last := best;
    if (level = 1) then
    begin
      first := best + 1;
      last := n - 1;
    end;
    if (last < first) then
      continue;
    var mean := 0.0;
    for var i := first to last do
      mean := mean + values[i];
    mean := mean / (last - first + 1);
    for var i := first to last do
      deviation := deviation + Sqr(values[i] - mean);
  end;
end;

/// <summary>The ends of the bars (top or bottom) minus slope times x.
/// </summary>
function Levels(const bars: TArray<TBar>; bottom: Boolean;
  slope: Double): TArray<Double>;
begin
  SetLength(Result, Length(bars));
  for var i := 0 to High(bars) do
    if bottom then
      Result[i] := bars[i].Bottom - slope * bars[i].X
    else
      Result[i] := bars[i].Top - slope * bars[i].X;
end;

/// <summary>The slope of the symbol: the one at which the tops and the
/// bottoms of the bars fall best in (at most) two levels each.</summary>
function EstimateSlope(const bars: TArray<TBar>; minGap: Double): Double;
const
  MAX_SLOPE = 0.35;
  STEPS = 70;
begin
  Result := 0;
  var bestDeviation := MaxDouble;
  for var s := -STEPS to STEPS do
  begin
    var slope := MAX_SLOPE * s / STEPS;
    var threshold, deviation, other: Double;
    SplitLevels(Levels(bars, false, slope), minGap, threshold, deviation);
    SplitLevels(Levels(bars, true, slope), minGap, threshold, other);
    if (deviation + other < bestDeviation) then
    begin
      bestDeviation := deviation + other;
      Result := slope;
    end;
  end;
end;

/// <summary>The distance between the means of the values below and above
/// the threshold.</summary>
function LevelDistance(const values: TArray<Double>;
  threshold: Double): Double;
begin
  var sums: array [Boolean] of Double;
  var counts: array [Boolean] of Integer;
  sums[false] := 0;
  sums[true] := 0;
  counts[false] := 0;
  counts[true] := 0;
  for var v in values do
  begin
    sums[v > threshold] := sums[v > threshold] + v;
    Inc(counts[v > threshold]);
  end;
  Result := 0;
  if (counts[false] > 0) and (counts[true] > 0) then
    Result := sums[true] / counts[true] - sums[false] / counts[false];
end;

/// <summary>Which ends of the bars stick out (ascenders at the top,
/// descenders at the bottom): the states, and the least certain decisions
/// (see TPostalBars.Uncertain).</summary>
procedure Classify(const bars: TArray<TBar>; out states: TArray<Byte>;
  out uncertain: TArray<Integer>);
const
  // a decision closer than this part of the distance between the levels to
  // their threshold is uncertain
  UNCERTAIN_MARGIN = 0.35;
begin
  var maxLength := 0.0;
  for var bar in bars do
    maxLength := Max(maxLength, bar.Bottom - bar.Top);
  var minGap := 0.15 * maxLength;
  var slope := EstimateSlope(bars, minGap);

  // the ends relative to the middle of the tracker there (the part the
  // bars around it have in common): independent of a slope or a curve
  var tops := Levels(bars, false, slope);
  var bottoms := Levels(bars, true, slope);
  // (where they have nothing in common: along the slope from the one before)
  var last := -1;
  var lastMiddle := 0.0;
  for var i := 0 to High(bars) do
  begin
    var middle: Double;
    if CommonMiddle(bars, i - 4, i + 4, slope, bars[i].X, middle) then
    begin
      last := i;
      lastMiddle := middle;
    end
    else if (last >= 0) then
      middle := lastMiddle + slope * (bars[i].X - bars[last].X)
    else
      middle := (bars[i].Top + bars[i].Bottom) / 2;
    tops[i] := bars[i].Top - middle;
    bottoms[i] := bars[i].Bottom - middle;
  end;

  // two levels of the tops: the higher ones are ascenders; one level: none
  var topThreshold, bottomThreshold, deviation: Double;
  var hasAscenders := SplitLevels(tops, minGap, topThreshold, deviation);
  var hasDescenders := SplitLevels(bottoms, minGap, bottomThreshold,
    deviation);

  SetLength(states, Length(bars));
  var margins: TArray<Double> := [];
  uncertain := [];
  for var i := 0 to High(bars) do
  begin
    var ascender := hasAscenders and (tops[i] < topThreshold);
    var descender := hasDescenders and (bottoms[i] > bottomThreshold);
    if ascender and descender then
      states[i] := BAR_FULL
    else if ascender then
      states[i] := BAR_ASCENDER
    else if descender then
      states[i] := BAR_DESCENDER
    else
      states[i] := BAR_TRACKER;
  end;

  // the uncertain decisions, sorted by their margin (insertion)
  for var side := 0 to 1 do
  begin
    var values := tops;
    var threshold := topThreshold;
    if (side = 1) then
    begin
      if not hasDescenders then
        continue;
      values := bottoms;
      threshold := bottomThreshold;
    end
    else if not hasAscenders then
      continue;
    var distance := LevelDistance(values, threshold);
    if (distance <= 0) then
      continue;
    for var i := 0 to High(bars) do
    begin
      var margin := Abs(values[i] - threshold) / distance;
      if (margin >= UNCERTAIN_MARGIN) then
        continue;
      var k := Length(margins);
      while (k > 0) and (margins[k - 1] > margin) do
        Dec(k);
      Insert(margin, margins, k);
      Insert(2 * i + side, uncertain, k);
    end;
  end;
end;

/// <summary>Whether the runs from index i (a bar) are at least count equally
/// spaced bars: their median width and pitch.</summary>
function IsBarSeed(const runs: TPatternRow; i, count: Integer;
  out barWidth, pitch: Double): Boolean;
begin
  Result := false;
  if (i + 2 * count - 1 > High(runs)) then
    exit;
  var widths, pitches: TArray<Double>;
  SetLength(widths, count);
  SetLength(pitches, count - 1);
  for var k := 0 to count - 1 do
  begin
    widths[k] := runs[i + 2 * k];
    if (k < count - 1) then
      pitches[k] := runs[i + 2 * k] + runs[i + 2 * k + 1];
  end;
  TArray.Sort<Double>(widths);
  TArray.Sort<Double>(pitches);
  barWidth := widths[count div 2];
  pitch := pitches[(count - 1) div 2];
  // the spaces about as wide as the bars or a few times wider
  if (pitch < 1.6 * barWidth) or (pitch > 5 * barWidth) then
    exit;
  for var k := 0 to count - 1 do
  begin
    if (runs[i + 2 * k] < 0.5 * barWidth) or (runs[i + 2 * k] > 1.8 * barWidth)
    then
      exit;
    if (k < count - 1) and (Abs(runs[i + 2 * k] + runs[i + 2 * k + 1] - pitch)
      > 0.3 * pitch + 1) then
      exit;
  end;
  Result := true;
end;

/// <summary>The next bar of the symbol in direction (1 right, -1 left) from
/// the bars: a pitch further, at the height of the part the bars at that
/// end have in common (the tracker), along the slope of the symbol; false
/// when there is none (the end).</summary>
function NextBar(image: TBitMatrix; const bars: TArray<TBar>;
  direction: Integer; barWidth, seedPitch, slope: Double;
  out bar: TBar): Boolean;
const
  WINDOW = 8;
begin
  Result := false;
  var n := Length(bars);
  var first := n - WINDOW;
  var last := n - 1;
  var lastBar := bars[n - 1];
  var before := bars[n - 2];
  if (direction < 0) then
  begin
    first := 0;
    last := WINDOW - 1;
    lastBar := bars[0];
    before := bars[1];
  end;
  var middle: Double;
  if not CommonMiddle(bars, first, last, slope, lastBar.X, middle) then
    exit;
  // the pitch here (perspective), about the one of the seed
  var pitch := Abs(lastBar.X - before.X);
  if (pitch < 0.75 * seedPitch) or (pitch > 1.33 * seedPitch) then
    pitch := seedPitch;
  var x := lastBar.X + direction * pitch;
  for var dy in [0, -1, 1, -2, 2] do
  begin
    var row := Floor(middle + slope * direction * pitch) + dy;
    var left, right: Integer;
    if DarkRunNear(image, x, row, 0.5 * pitch, left, right) and
      (Abs((left + right + 1) / 2 - x) < 0.4 * pitch) and
      TraceBar(image, (left + right + 1) / 2, row, barWidth, 12 * pitch, bar)
    then
      exit(true);
  end;
end;

/// <summary>The bars of the symbol from the seed at row y (count bars from
/// x): traced, then followed to both ends of the symbol.</summary>
function FollowBars(image: TBitMatrix; y: Integer; x: Double;
  count: Integer; barWidth, pitch: Double; const runs: TPatternRow;
  i: Integer): TArray<TBar>;
begin
  Result := [];
  var cx := x;
  for var k := 0 to count - 1 do
  begin
    var bar: TBar;
    if not TraceBar(image, cx + runs[i + 2 * k] / 2, y, barWidth,
      12 * pitch, bar) then
      exit(nil);
    Result := Result + [bar];
    cx := cx + runs[i + 2 * k] + runs[i + 2 * k + 1];
  end;

  // (to the right again at the end: with the slope of the symbol, not yet
  // known when the seed was at the right end)
  var slope := 0.0;
  for var pass := 0 to 2 do
  begin
    var direction := 1;
    if (pass = 1) then
      direction := -1
    else if (pass = 2) then
    begin
      var maxBar := 0.0;
      for var b in Result do
        maxBar := Max(maxBar, b.Bottom - b.Top);
      slope := EstimateSlope(Result, 0.15 * maxBar);
    end;
    repeat
      var bar: TBar;
      if not NextBar(image, Result, direction, barWidth, pitch, slope, bar)
      then
        break;
      if (direction > 0) then
        Result := Result + [bar]
      else
        Result := [bar] + Result;
      // the slope again from time to time
      if (Length(Result) mod 8 = 0) then
      begin
        var maxBar := 0.0;
        for var b in Result do
          maxBar := Max(maxBar, b.Bottom - b.Top);
        slope := EstimateSlope(Result, 0.15 * maxBar);
      end;
    until false;
  end;

  // on the part the bars have in common (the tracker) no bar between them
  // (else the seed row ran through ascenders or descenders only and every
  // other bar was taken)
  for var k := 0 to Length(Result) - 2 do
  begin
    var middle: Double;
    var x0 := (Result[k].X + Result[k + 1].X) / 2;
    var left, right: Integer;
    if CommonMiddle(Result, k - 4, k + 4, slope, x0, middle) and
      DarkRunNear(image, x0 - 0.5, Floor(middle), 0, left, right) and
      (left >= Result[k].X) and (right + 1 <= Result[k + 1].X) then
      exit(nil);
  end;
end;

function DetectPostalBars(image: TBitMatrix; rowStep, minBars: Integer)
  : TArray<TPostalBars>;
const
  SEED_BARS = 5;
var
  runs: TPatternRow;
begin
  Result := [];
  // the areas of the symbols found (not again)
  var areas: TArray<TArray<Double>> := [];
  var y := rowStep div 2;
  while (y < image.Height) do
  begin
    GetPatternRow(image, y, runs);
    var i := 1;
    var x := runs[0];
    while (i < Length(runs)) do
    begin
      var known := false;
      for var area in areas do
        if (x >= area[0]) and (x <= area[1]) and (y >= area[2]) and
          (y <= area[3]) then
          known := true;
      var barWidth, pitch: Double;
      if not known and IsBarSeed(runs, i, SEED_BARS, barWidth, pitch) then
      begin
        var bars := FollowBars(image, y, x, SEED_BARS, barWidth, pitch,
          runs, i);
        if (Length(bars) >= minBars) then
        begin
          var symbol: TPostalBars;
          Classify(bars, symbol.States, symbol.Uncertain);
          symbol.FirstX := bars[0].X;
          symbol.FirstY := bars[0].Y;
          symbol.LastX := bars[High(bars)].X;
          symbol.LastY := bars[High(bars)].Y;
          Result := Result + [symbol];
          var top := MaxDouble;
          var bottom := -MaxDouble;
          for var bar in bars do
          begin
            top := Min(top, bar.Top);
            bottom := Max(bottom, bar.Bottom);
          end;
          areas := areas + [[bars[0].X - pitch, bars[High(bars)].X + pitch,
            top, bottom]];
        end;
      end;
      Inc(x, runs[i]);
      if (i + 1 < Length(runs)) then
        Inc(x, runs[i + 1]);
      Inc(i, 2);
    end;
    Inc(y, rowStep);
  end;
end;

end.
