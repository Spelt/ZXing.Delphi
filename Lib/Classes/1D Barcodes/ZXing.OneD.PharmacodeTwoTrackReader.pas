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

  * Pharmacode two-track (Laetus) for ZXing.Delphi: zxing-cpp, ZXing Java and
  * ZXing.Net have no reader. The encoding as in zint (medical.c).
}

unit ZXing.OneD.PharmacodeTwoTrackReader;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap,
  ZXing.Common.BitMatrix;

type
  /// <summary>
  /// Reads Pharmacode two-track (Laetus): 3 to 16 equally spaced bars in
  /// the top track, the bottom track or both, the value 13 to 64570080.
  /// Bars of one kind only (like 4, 8 and 12) can not be read: they do not
  /// tell where the other track is.
  /// Horizontal or vertical. Like Pharmacode (one track) it has no check,
  /// so it is only read when asked for (not in Auto). Read with the top
  /// track at the top, left to right: the value of a code turned 180
  /// degrees is a different one.
  /// </summary>
  TPharmacodeTwoTrackReader = class(TInterfacedObject, IReader,
    IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>The value of the bars of a Pharmacode two-track from left to
/// right (1 bottom track, 2 top track, 3 both); -1 when it is none.
/// </summary>
function PharmacodeTwoTrackValue(const bars: string): Integer;

implementation

uses
  ZXing.ResultPoint,
  ZXing.Postal.FourStateDetector;

const
  MIN_BARS = 3;
  MAX_BARS = 16;
  MIN_VALUE = 4;
  MAX_VALUE = 64570080;

type
  TRun = record
    Left, Right: Integer;
  end;

  TExtent = record
    Top, Bottom: Integer;
  end;

function PharmacodeTwoTrackValue(const bars: string): Integer;
begin
  Result := -1;
  if (Length(bars) < MIN_BARS) or (Length(bars) > MAX_BARS) then
    exit;
  // bijective base 3 (digits 1 to 3), the highest first
  var value: Int64 := 0;
  for var c in bars do
  begin
    if (c < '1') or (c > '3') then
      exit;
    value := 3 * value + Ord(c) - Ord('0');
  end;
  if (value >= MIN_VALUE) and (value <= MAX_VALUE) then
    Result := value;
end;

/// <summary>The dark runs of row y where row y or row y2 is dark (y2 = y:
/// row y alone).</summary>
function DarkRuns(matrix: TBitMatrix; y, y2: Integer): TArray<TRun>;
begin
  Result := [];
  var x := 0;
  var w := matrix.Width;
  while (x < w) do
  begin
    if matrix[x, y] or matrix[x, y2] then
    begin
      var run: TRun;
      run.Left := x;
      while (x < w) and (matrix[x, y] or matrix[x, y2]) do
        Inc(x);
      run.Right := x - 1;
      Result := Result + [run];
    end
    else
      Inc(x);
  end;
end;

/// <summary>The rows the bar through (x, y) covers (it may be slanted a
/// little).</summary>
function TraceExtent(matrix: TBitMatrix; x, y: Integer): TExtent;
begin
  for var direction in [-1, 1] do
  begin
    var cx := x;
    var yy := y;
    while (yy + direction >= 0) and (yy + direction < matrix.Height) do
    begin
      // dark at cx or next to it
      var nx := -1;
      for var dx in [0, -1, 1] do
        if (cx + dx >= 0) and (cx + dx < matrix.Width) and
          matrix[cx + dx, yy + direction] then
        begin
          nx := cx + dx;
          break;
        end;
      if (nx < 0) then
        break;
      cx := nx;
      Inc(yy, direction);
    end;
    if (direction < 0) then
      Result.Top := yy
    else
      Result.Bottom := yy;
  end;
end;

/// <summary>Whether column x is white from row top to row bottom.</summary>
function IsWhiteColumn(matrix: TBitMatrix; x, top, bottom: Integer): Boolean;
begin
  Result := true;
  for var y := Max(top, 0) to Min(bottom, matrix.Height - 1) do
    if matrix[x, y] then
      exit(false);
end;

/// <summary>Whether the bar from column left to right is about as wide and
/// at the same place on every row where it is dark between top and bottom.
/// </summary>
function IsStraightBar(matrix: TBitMatrix; left, right, top,
  bottom: Integer): Boolean;
begin
  Result := true;
  var width := right - left + 1;
  for var y := Max(top, 0) to Min(bottom, matrix.Height - 1) do
    for var x := left to right do
      if matrix[x, y] then
      begin
        var l := x;
        while (l > 0) and matrix[l - 1, y] do
          Dec(l);
        var r := x;
        while (r < matrix.Width - 1) and matrix[r + 1, y] do
          Inc(r);
        if (r - l + 1 > 1.6 * width + 1) or (l < left - width) or
          (r > right + width) then
          exit(false);
        break;
      end;
end;

/// <summary>Whether the values differ at most tolerance from the first.</summary>
function Aligned(const values: TArray<Integer>; tolerance: Integer): Boolean;
begin
  Result := true;
  for var v in values do
    if (Abs(v - values[0]) > tolerance) then
      exit(false);
end;

/// <summary>The symbols in matrix (a row through the top track and one
/// through the bottom track merged): their bars and the centers of the
/// first and the last one.</summary>
procedure FindSymbols(matrix: TBitMatrix; rowStep: Integer;
  results: TList<TReadResult>; maxCount: Integer; vertical: Boolean);
const
  // the quiet zone in pitches (bar and space; the spec requires 6 mm, at
  // least 3 bar widths)
  QUIET_ZONE = 1.5;
begin
  var y := rowStep div 2;
  while (y < matrix.Height) and not ResultsFull(results, maxCount) do
  begin
    var runs := DarkRuns(matrix, y, y);
    // the rows of the other track: below or above the bars of this row
    // (half a bar further), or in a full bar a quarter from its end
    var partners: TArray<Integer> := [];
    for var i := 0 to High(runs) do
    begin
      var run := runs[i];
      if (run.Right - run.Left + 1 > matrix.Width div 20 + 2) then
        continue;
      // only a bar (taller than wide) with a neighbour about as wide in
      // this row, or in the other track (a row of bars)
      var w := run.Right - run.Left + 1;
      var e := TraceExtent(matrix, (run.Left + run.Right) div 2, y);
      var h := e.Bottom - e.Top + 1;
      if (h < 4) or (h < 2 * w) then
        continue;
      var neighbour := false;
      for var j in [i - 1, i + 1] do
        if (j >= 0) and (j <= High(runs)) then
        begin
          var wj := runs[j].Right - runs[j].Left + 1;
          var gap := Max(runs[j].Left - run.Right, run.Left - runs[j].Right);
          if (wj <= 2 * w + 1) and (w <= 2 * wj + 1) and (gap <= 8 * w) then
            neighbour := true;
        end;
      for var row in [e.Top - h div 2, e.Bottom + h div 2] do
        if not neighbour and (row >= 0) and (row < matrix.Height) then
          for var x := Max(0, run.Left - 3 * w) to Min(matrix.Width - 1,
            run.Right + 3 * w) do
            if matrix[x, row] then
            begin
              neighbour := true;
              break;
            end;
      if not neighbour then
        continue;
      for var p in [e.Bottom + h div 2, e.Top - h div 2, e.Top + h div 4,
        e.Bottom - h div 4] do
        if (p >= 0) and (p < matrix.Height) and (Abs(p - y) >= 2) then
        begin
          var known := false;
          for var q in partners do
            if (Abs(q - p) <= 1) then
              known := true;
          if not known then
            partners := partners + [p];
        end;
    end;

    for var y2 in partners do
    begin
      var topRow := Min(y, y2);
      var bottomRow := Max(y, y2);
      var merged := DarkRuns(matrix, y, y2);
      var i := 0;
      while (i < Length(merged)) and not ResultsFull(results, maxCount) do
      begin
        // a chain of equally spaced bars from merged[i]
        var last := i;
        while (last + 1 < Length(merged)) and (last - i + 1 < MAX_BARS + 1) do
        begin
          var w0 := merged[i].Right - merged[i].Left + 1;
          var w := merged[last + 1].Right - merged[last + 1].Left + 1;
          var pitch0: Double := merged[i + 1].Left - merged[i].Left;
          if (last > i) then
            pitch0 := (merged[last].Left - merged[i].Left) / (last - i);
          var pitch := merged[last + 1].Left - merged[last].Left;
          // bars about as wide as the spaces (1:1), the same pitch
          if (w > 1.6 * w0 + 1) or (w0 > 1.6 * w + 1) or
            (pitch < 1.5 * w0) or (pitch > 3 * w0 + 1) or
            ((last > i) and (Abs(pitch - pitch0) > 0.25 * pitch0 + 1)) then
            break;
          Inc(last);
        end;
        var count := last - i + 1;
        var first := i;
        i := last + 1;
        // (bars of at least 2 pixels: not a texture)
        if (count < MIN_BARS) or (count > MAX_BARS) or
          (merged[first].Right - merged[first].Left + 1 < 2) then
          continue;
        var pitch := 2.0 * (merged[first].Right - merged[first].Left + 1);
        if (count > 1) then
          pitch := (merged[last].Left - merged[first].Left) / (count - 1);
        // quiet zones (or the edge of the image)
        if ((first > 0) and (merged[first].Left - merged[first - 1].Right <
          QUIET_ZONE * pitch)) or ((last < High(merged)) and
          (merged[last + 1].Left - merged[last].Right < QUIET_ZONE * pitch))
        then
          continue;

        // the tracks of the bars, and their ends: the ones in the top track
        // start at the same row, the ones in the bottom track end at the
        // same row, the top and the bottom track touch
        var bars := '';
        var tops, bottoms, middlesTop, middlesBottom: TArray<Integer>;
        var ok := true;
        for var k := first to last do
        begin
          var x := (merged[k].Left + merged[k].Right) div 2;
          var inTop := matrix[x, topRow];
          var inBottom := matrix[x, bottomRow];
          var digit := Ord(inBottom) + 2 * Ord(inTop);
          if (digit = 0) then
          begin
            ok := false;
            break;
          end;
          bars := bars + Chr(Ord('0') + digit);
          var row := topRow;
          if not inTop then
            row := bottomRow;
          var e := TraceExtent(matrix, x, row);
          if inTop then
            tops := tops + [e.Top]
          else
            middlesBottom := middlesBottom + [e.Top];
          if inBottom then
            bottoms := bottoms + [e.Bottom]
          else
            middlesTop := middlesTop + [e.Bottom];
          // a bar in one track: not in the other one
          if not inBottom and (e.Bottom >= bottomRow) then
            ok := false;
          if not inTop and (e.Top <= topRow) then
            ok := false;
        end;
        if not ok then
          continue;
        var tolerance := Max(2, Round(0.4 * pitch));
        if not Aligned(tops, tolerance) or not Aligned(bottoms, tolerance) or
          not Aligned(middlesTop + middlesBottom, tolerance) then
          continue;

        // bars of one kind only do not tell where the other track is (all
        // in the bottom track or all in the top one, all full or all in one
        // track): at least 2 kinds
        var kinds := 0;
        for var c in ['1', '2', '3'] do
          if (Pos(c, bars) > 0) then
            Inc(kinds);
        if (kinds < 2) then
          continue;
        // the tracks about as high, at least 1.5 bar widths (not letters)
        var middles := middlesTop + middlesBottom;
        var barWidth := Max(pitch / 2, 2);
        var topHeight := middles[0] - tops[0];
        var bottomHeight := bottoms[0] - middles[0];
        if (topHeight < Max(1.5 * barWidth, 4)) or
          (bottomHeight < Max(1.5 * barWidth, 4)) or
          (topHeight > 2 * bottomHeight + 2) or
          (bottomHeight > 2 * topHeight + 2) then
          continue;
        // the spaces white over the whole height and the bars straight (not
        // the strokes of letters)
        // a bar in one track white in the other one (not the dot of an i)
        var top := tops[0];
        var bottom := bottoms[0];
        var middle := middles[0];
        for var k := first to last do
        begin
          var x := (merged[k].Left + merged[k].Right) div 2;
          if (k < last) and not IsWhiteColumn(matrix,
            (merged[k].Right + merged[k + 1].Left) div 2, top, bottom) then
            ok := false;
          if ok and not IsStraightBar(matrix, merged[k].Left, merged[k].Right,
            top, bottom) then
            ok := false;
          if ok and (bars[k - first + 1] = '1') and not IsWhiteColumn(matrix, x,
            top, middle - 2) then
            ok := false;
          if ok and (bars[k - first + 1] = '2') and not IsWhiteColumn(matrix, x,
            middle + 2, bottom) then
            ok := false;
        end;
        // white above and below the bars (a pitch)
        var margin := Round(pitch);
        for var x := Max(merged[first].Left - margin, 0) to
          Min(merged[last].Right + margin, matrix.Width - 1) do
          if ok and (((top - margin >= 0) and not IsWhiteColumn(matrix, x,
            top - margin, top - 2)) or ((bottom + margin < matrix.Height) and
            not IsWhiteColumn(matrix, x, bottom + 2, bottom + margin)) or
            (top - margin < 0) or (bottom + margin >= matrix.Height)) then
            ok := false;
        if not ok then
          continue;
        var value := PharmacodeTwoTrackValue(bars);
        if (value < 0) then
          continue;
        var cy := (topRow + bottomRow) / 2;
        var x1: Double := (merged[first].Left + merged[first].Right + 1) / 2;
        var x2: Double := (merged[last].Left + merged[last].Right + 1) / 2;
        var p1x := x1;
        var p1y := cy;
        var p2x := x2;
        var p2y := cy;
        if vertical then
        begin
          p1x := cy;
          p1y := x1;
          p2x := cy;
          p2y := x2;
        end;
        var r := TReadResult.Create(IntToStr(value), nil,
          [TResultPointHelpers.CreateResultPoint(p1x, p1y),
          TResultPointHelpers.CreateResultPoint(p2x, p2y)],
          TBarcodeFormat.PHARMA_CODE_TWO_TRACK);
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
      end;
    end;
    Inc(y, rowStep);
  end;
end;

{ TPharmacodeTwoTrackReader }

function TPharmacodeTwoTrackReader.decode(const image: TBinaryBitmap)
  : TReadResult;
begin
  Result := decode(image, nil);
end;

function TPharmacodeTwoTrackReader.decode(const image: TBinaryBitmap;
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

procedure TPharmacodeTwoTrackReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var rowStep := 4;
  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.TRY_HARDER) then
    rowStep := 2;
  // horizontal symbols, then vertical ones in the image turned (the top
  // track on the left, read top to bottom: mirrored)
  for var vertical in [false, true] do
  begin
    var matrix := image.BlackMatrix;
    if vertical then
      matrix := Transposed(matrix);
    try
      FindSymbols(matrix, rowStep, results, maxCount, vertical);
    finally
      if vertical then
        matrix.Free;
    end;
  end;
end;

procedure TPharmacodeTwoTrackReader.reset;
begin
  // do nothing
end;

end.
