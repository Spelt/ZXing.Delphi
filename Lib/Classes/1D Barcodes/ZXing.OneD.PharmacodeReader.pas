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

  * Pharmacode (Laetus, one track) for ZXing.Delphi. The value as in the
  * PharmaCodeReader of ZXing.Net (zxing-cpp and ZXing Java have none), the
  * bars read like the other 1D readers.
}

unit ZXing.OneD.PharmacodeReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat,
  ZXing.BinaryBitmap;

type
  /// <summary>
  /// Decodes Pharmacode (Laetus, one track): 2 to 16 narrow and wide bars
  /// (1:3) with equal spaces (2), the value 3 to 131070. Pharmacode has no
  /// check, almost every row of bars is one, so it is only read when asked
  /// for (not in Auto). Read left to right: the value of a code turned 180
  /// degrees is a different one.
  /// </summary>
  TPharmacodeReader = class(TOneDReader)
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;


/// <summary>Whether the Pharmacode r (Position set) looks like one: bars
/// as tall as the symbol, the rows from the top to the bottom alike, the
/// quiet zones light over that height (else
/// noise or a part of a slanted symbol: Pharmacode has no check).</summary>
function PlausiblePharmacode(r: TReadResult; image: TBinaryBitmap): Boolean;

implementation

uses
  System.Math;

function PlausiblePharmacode(r: TReadResult; image: TBinaryBitmap): Boolean;
const
  // the rows compared (parts of the height), the places along them
  PARTS: array [0 .. 4] of Double = (0.2, 0.35, 0.5, 0.65, 0.8);
  COUNT = 200;
begin
  Result := false;
  var p := r.Position;
  var luminances := image.Luminances;
  var width := image.Width;
  var height := image.Height;
  if (Length(p) < 4) or (Length(luminances) < width * height) then
    exit;
  // the bars at least a tenth of the symbol high (they are some millimeters
  // high, a part of the width of the symbol; else a row of thin lines)
  var symbolWidth := Hypot(p[1].X - p[0].X, p[1].Y - p[0].Y);
  var symbolHeight := Hypot(p[3].X - p[0].X, p[3].Y - p[0].Y);
  if (symbolHeight < 0.1 * symbolWidth) then
    exit;
  // the luminances of the rows (from the left edge to the right one)
  var samples: array [0 .. 4, 0 .. COUNT - 1] of Integer;
  var darkest := 255;
  var lightest := 0;
  for var k := 0 to High(PARTS) do
  begin
    var f := PARTS[k];
    var ax := p[0].X + f * (p[3].X - p[0].X);
    var ay := p[0].Y + f * (p[3].Y - p[0].Y);
    var bx := p[1].X + f * (p[2].X - p[1].X);
    var by := p[1].Y + f * (p[2].Y - p[1].Y);
    for var j := 0 to COUNT - 1 do
    begin
      var x := Trunc(ax + (bx - ax) * (j + 0.5) / COUNT);
      var y := Trunc(ay + (by - ay) * (j + 0.5) / COUNT);
      var l := 255;
      if (x >= 0) and (y >= 0) and (x < width) and (y < height) then
        l := luminances[y * width + x];
      samples[k, j] := l;
      darkest := Min(darkest, l);
      lightest := Max(lightest, l);
    end;
  end;
  var threshold := (darkest + lightest + 1) div 2;
  // each row like the middle one
  var worst := COUNT;
  for var k := 0 to High(PARTS) do
  begin
    var same := 0;
    for var j := 0 to COUNT - 1 do
      if ((samples[k, j] < threshold) = (samples[2, j] < threshold)) then
        Inc(same);
    worst := Min(worst, same);
  end;
  Result := (worst >= 0.9 * COUNT);
  if not Result then
    exit;
  // the quiet zones light over the whole height (else the scan line ran
  // out of the bars of a slanted symbol: only a part of it read): from 0.3
  // to 2 pitches beyond each end, the pitch from the number of bars
  var bars := 0;
  var value := StrToIntDef(r.Text, 0) + 1;
  while (value > 1) do
  begin
    Inc(bars);
    value := value shr 1;
  end;
  if (bars < 1) then
    exit(false);
  var dx := (p[1].X - p[0].X) / bars;
  var dy := (p[1].Y - p[0].Y) / bars;
  var dark := 0;
  var total := 0;
  for var k := 0 to 8 do
  begin
    var f := 0.1 + 0.1 * k;
    for var side := 0 to 1 do
    begin
      // the end (left: p0 to p3, right: p1 to p2) and outward
      var ex := p[0].X + f * (p[3].X - p[0].X);
      var ey := p[0].Y + f * (p[3].Y - p[0].Y);
      var sign := -1;
      if (side = 1) then
      begin
        ex := p[1].X + f * (p[2].X - p[1].X);
        ey := p[1].Y + f * (p[2].Y - p[1].Y);
        sign := 1;
      end;
      var t := 0.3;
      while (t <= 2) do
      begin
        var x := Trunc(ex + sign * t * dx);
        var y := Trunc(ey + sign * t * dy);
        // (outside the image: light)
        if (x >= 0) and (y >= 0) and (x < width) and (y < height) and
          (luminances[y * width + x] < threshold) then
          Inc(dark);
        Inc(total);
        t := t + 0.1;
      end;
    end;
  end;
  Result := (dark <= 0.05 * total);
end;

{ TPharmacodeReader }

function TPharmacodeReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TPharmacodeReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TPharmacodeReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  MIN_BARS = 2;
  MAX_BARS = 16;
  // the quiet zones in spaces (the spec requires 3)
  QUIET_ZONE = 2.5;
begin
  Result := nil;

  // the first bar: with a quiet zone in front, much wider than the spaces
  // (also at the edge of the image: there a symbol can be cut off, so the
  // white pixels there count, not the edge), the next space about as wide
  // as a narrow bar is (1:2) or a wide one (3:2)
  next := FindLeftGuard(next, 2, 2 * MIN_BARS - 1,
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      var bar := window[0];
      var space := window[1];
      if window.IsAtFirstBar then
        spaceInPixel := window.PixelsInFront;
      Result := (bar >= 0.25 * space) and (bar <= 2.2 * space) and
        (spaceInPixel >= QUIET_ZONE * space);
    end);
  if not next.IsValid then
    exit;
  var xStart := next.PixelsInFront;

  // the bars up to the quiet zone behind them; the spaces about equal
  var space: Double := next[1];
  var bars: TArray<Integer> := [];
  var view := next;
  repeat
    bars := bars + [view[0]];
    if (Length(bars) > MAX_BARS) then
      exit;
    // the space behind the bar: the end (a quiet zone, much wider than the
    // spaces, also at the edge of the image: there a symbol can be cut
    // off), or the next space
    if not view.IsValid(2) then
      exit;
    if (view[1] >= QUIET_ZONE * space) then
      break;
    if not view.IsValid(3) then
      exit;
    if (view[1] < 0.6 * space) or (view[1] > 1.6 * space) then
      exit;
    space := (2 * space + view[1]) / 3;
    if not view.SkipPair or not view.IsValid(1) then
      exit;
  until false;
  if (Length(bars) < MIN_BARS) then
    exit;

  // narrow bars half a space wide, wide bars one and a half; the value:
  // binary 1 followed by a bit per bar (wide 1), minus 1
  var value := 1;
  for var bar in bars do
  begin
    if (bar < 0.25 * space) or (bar > 2.2 * space) then
      exit;
    value := 2 * value + Ord(bar > space);
  end;
  Dec(value);

  next := view.SubView(0, 1);
  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(IntToStr(value), nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.PHARMA_CODE);
end;

end.
