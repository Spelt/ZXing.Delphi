unit ZXing.Common.Pattern;

{
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

  * Ported from zxing-cpp (Pattern.h, used by the QR Code detector and the 1D
  * readers): a row of the image as the widths of its bars and spaces, and a
  * view on a part of it to match patterns like the 1:1:3:1:1 of a QR finder
  * pattern or the start pattern of a 1D code.
}

interface

uses
  ZXing.Common.BitArray,
  ZXing.Common.BitMatrix;

type
  /// <summary>The widths of the white and black runs of a row: it always
  /// starts and ends with a white run (0 when the row starts or ends
  /// black).</summary>
  TPatternRow = TArray<Integer>;

  /// <summary>A view on Count elements of a TPatternRow from index Data.
  /// </summary>
  TPatternView = record
  private
    FRow: TPatternRow;
    FData: Integer; // -1: no view
    FSize: Integer;
    function GetItem(i: Integer): Integer; inline;
  public
    /// <summary>The view on a whole row: from its first black run.</summary>
    class function Create(const row: TPatternRow): TPatternView; static;
    class function Empty: TPatternView; static;

    function Sum(n: Integer = 0): Integer;
    function Size: Integer;
    function PixelsInFront: Integer;
    function IsAtFirstBar: Boolean;
    function IsValid: Boolean; overload;
    function IsValid(n: Integer): Boolean; overload;
    function SubView(offset, size: Integer): TPatternView;
    function Shift(n: Integer): Boolean;
    function SkipPair: Boolean;
    function SkipSymbol: Boolean;
    function SkipSingle(maxWidth: Integer): Boolean;
    procedure Extend;

    /// <summary>The number of bars and spaces from the first bar to the
    /// start of the view.</summary>
    function Index: Integer;
    /// <summary>The pixel position of the last pixel of the view.</summary>
    function PixelsTillEnd: Integer;
    function IsAtLastBar: Boolean;
    /// <summary>The white space in front (MaxInt at the first bar).</summary>
    function SpaceInFront: Integer;
    function HasQuietZoneBefore(scale: Double;
      acceptIfAtFirstBar: Boolean = false): Boolean;
    function HasQuietZoneAfter(scale: Double;
      acceptIfAtLastBar: Boolean = true): Boolean;

    /// <summary>The element i of the view; -1 is the one in front of it.
    /// </summary>
    property Items[i: Integer]: Integer read GetItem; default;
    property Data: Integer read FData;
  end;

  /// <summary>Two values, one for the bars and one for the spaces: Items[i]
  /// is the bar value for an even i and the space value for an odd one, so
  /// that it can be used with the index of a TPatternView.</summary>
  TBarAndSpace = record
  private
    FValues: array [0 .. 1] of Integer;
    function GetItem(i: Integer): Integer; inline;
    procedure SetItem(i: Integer; value: Integer); inline;
  public
    class function Create(bar, space: Integer): TBarAndSpace; static;
    function IsValid: Boolean;
    property Items[i: Integer]: Integer read GetItem write SetItem; default;
    property Bar: Integer read FValues[0] write FValues[0];
    property Space: Integer read FValues[1] write FValues[1];
  end;

/// <summary>The runs of row y of matrix.</summary>
procedure GetPatternRow(matrix: TBitMatrix; y: Integer;
  var row: TPatternRow); overload;
/// <summary>The runs of a row of width pixels.</summary>
procedure GetPatternRow(const bits: IBitArray; width: Integer;
  var row: TPatternRow); overload;

/// <summary>
/// Whether the first Length(pattern) elements of view match pattern
/// (zxing-cpp's IsPattern), edge to edge (e2e: bars and spaces each with
/// their own module size) or not. spaceInPixel: the white space in front,
/// at least minQuietZone modules when minQuietZone > 0. Returns the module
/// size, or 0 when the view does not match.
/// </summary>
function IsPattern(const view: TPatternView; const pattern: array of Integer;
  e2e: Boolean; spaceInPixel: Integer = 0; minQuietZone: Double = 0;
  moduleSizeRef: Double = 0): Double; overload;
/// <summary>The same for a plain array of widths, like a pattern read with
/// a cursor.</summary>
function IsPattern(const widths: array of Integer;
  const pattern: array of Integer; e2e: Boolean): Double; overload;

/// <summary>Whether view, the bars and spaces at the end of a symbol,
/// matches the (stop) pattern followed by a quiet zone of minQuietZone
/// modules (or the end of the row).</summary>
function IsRightGuard(const view: TPatternView;
  const pattern: array of Integer; minQuietZone: Double;
  e2e: Boolean = false; moduleSizeRef: Double = 0): Boolean;

type
  /// <summary>Whether window is the guard pattern, with spaceInPixel white
  /// pixels in front of it.</summary>
  TGuardPredicate = reference to function(const window: TPatternView;
    spaceInPixel: Integer): Boolean;

/// <summary>The first window of len bars and spaces in view (starting on a
/// bar) that isGuard accepts, with at least minSize elements from its start
/// to the end of view; an invalid view when there is none.</summary>
function FindLeftGuard(const view: TPatternView; len, minSize: Integer;
  const isGuard: TGuardPredicate): TPatternView; overload;
/// <summary>The same for a fixed pattern with a quiet zone of minQuietZone
/// modules in front of it.</summary>
function FindLeftGuard(const view: TPatternView; minSize: Integer;
  const pattern: array of Integer; minQuietZone: Double;
  e2e: Boolean = false): TPatternView; overload;

implementation

uses
  System.Math;

{ TPatternView }

class function TPatternView.Create(const row: TPatternRow): TPatternView;
begin
  Result.FRow := row;
  Result.FData := 1;
  Result.FSize := System.Length(row) - 1;
end;

class function TPatternView.Empty: TPatternView;
begin
  Result.FRow := nil;
  Result.FData := -1;
  Result.FSize := 0;
end;

function TPatternView.GetItem(i: Integer): Integer;
begin
  Result := FRow[FData + i];
end;

function TPatternView.Sum(n: Integer): Integer;
begin
  if (n = 0) then
    n := FSize;
  Result := 0;
  for var i := 0 to n - 1 do
    Inc(Result, FRow[FData + i]);
end;

function TPatternView.Size: Integer;
begin
  Result := FSize;
end;

function TPatternView.PixelsInFront: Integer;
begin
  Result := 0;
  for var i := 0 to FData - 1 do
    Inc(Result, FRow[i]);
end;

function TPatternView.IsAtFirstBar: Boolean;
begin
  Result := (FData = 1);
end;

function TPatternView.IsValid(n: Integer): Boolean;
begin
  Result := (FData >= 0) and (FData + n <= System.Length(FRow));
end;

function TPatternView.IsValid: Boolean;
begin
  Result := IsValid(FSize);
end;

function TPatternView.SubView(offset, size: Integer): TPatternView;
begin
  if (size = 0) then
    size := FSize - offset
  else if (size < 0) then
    size := FSize - offset + size;
  Result.FRow := FRow;
  Result.FData := FData + offset;
  Result.FSize := Max(size, 0);
end;

function TPatternView.Shift(n: Integer): Boolean;
begin
  if (FData < 0) then
    exit(false);
  Inc(FData, n);
  Result := (FData + FSize <= System.Length(FRow));
end;

function TPatternView.SkipPair: Boolean;
begin
  Result := Shift(2);
end;

function TPatternView.SkipSymbol: Boolean;
begin
  Result := Shift(FSize);
end;

function TPatternView.SkipSingle(maxWidth: Integer): Boolean;
begin
  Result := Shift(1) and (FRow[FData - 1] <= maxWidth);
end;

procedure TPatternView.Extend;
begin
  if (FData < 0) then
    FSize := 0
  else
    FSize := Max(0, System.Length(FRow) - FData);
end;

function TPatternView.Index: Integer;
begin
  Result := FData - 1;
end;

function TPatternView.PixelsTillEnd: Integer;
begin
  Result := -1;
  for var i := 0 to FData + FSize - 1 do
    Inc(Result, FRow[i]);
end;

function TPatternView.IsAtLastBar: Boolean;
begin
  Result := (FData + FSize = System.Length(FRow) - 1);
end;

function TPatternView.SpaceInFront: Integer;
begin
  if IsAtFirstBar then
    Result := MaxInt
  else
    Result := FRow[FData - 1];
end;

function TPatternView.HasQuietZoneBefore(scale: Double;
  acceptIfAtFirstBar: Boolean): Boolean;
begin
  Result := (acceptIfAtFirstBar and IsAtFirstBar) or
    (FRow[FData - 1] >= Sum * scale);
end;

function TPatternView.HasQuietZoneAfter(scale: Double;
  acceptIfAtLastBar: Boolean): Boolean;
begin
  Result := (acceptIfAtLastBar and IsAtLastBar) or
    (FRow[FData + FSize] >= Sum * scale);
end;

{ TBarAndSpace }

class function TBarAndSpace.Create(bar, space: Integer): TBarAndSpace;
begin
  Result.FValues[0] := bar;
  Result.FValues[1] := space;
end;

function TBarAndSpace.GetItem(i: Integer): Integer;
begin
  Result := FValues[i and 1];
end;

procedure TBarAndSpace.SetItem(i: Integer; value: Integer);
begin
  FValues[i and 1] := value;
end;

function TBarAndSpace.IsValid: Boolean;
begin
  Result := (FValues[0] <> 0) and (FValues[1] <> 0);
end;

procedure GetPatternRow(matrix: TBitMatrix; y: Integer; var row: TPatternRow);
begin
  GetPatternRow(matrix.getRow(y, nil), matrix.Width, row);
end;

const
  // de Bruijn sequence: the number of trailing zero bits of w is
  // DE_BRUIJN_BITS[((w and -w) * DE_BRUIJN) shr 27]
  DE_BRUIJN = $077CB531;
  DE_BRUIJN_BITS: array [0 .. 31] of Byte = (0, 1, 28, 2, 29, 14, 24, 3, 30,
    22, 20, 15, 25, 17, 4, 8, 31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11,
    5, 10, 9);

/// <summary>The number of trailing zero bits of w (not 0).</summary>
function TrailingZeros(w: Cardinal): Integer; inline;
begin
{$IFOPT Q+}{$DEFINE PATTERN_Q}{$Q-}{$ENDIF}
  Result := DE_BRUIJN_BITS[((w and (not w + 1)) * DE_BRUIJN) shr 27];
{$IFDEF PATTERN_Q}{$Q+}{$UNDEF PATTERN_Q}{$ENDIF}
end;

procedure GetPatternRow(const bits: IBitArray; width: Integer;
  var row: TPatternRow);
begin
  SetLength(row, width + 2);
  if (width = 0) then
  begin
    SetLength(row, 1);
    row[0] := 0;
    exit;
  end;

  // jump from edge to edge through the words of the bit array: the next
  // edge is the lowest bit (from x on) that differs from the current color
  var words := bits.Bits;
  var lastWord := Min(High(words), (width - 1) shr 5);
  var count := 0;
  var x := 0;
  // the first value is the number of white pixels, 0 when starting black
  var black := (words[0] and 1) <> 0;
  if black then
  begin
    row[0] := 0;
    count := 1;
  end;
  while (x < width) do
  begin
    var i := x shr 5;
    var w := Cardinal(words[i]);
    if black then
      w := not w;
    w := w and (Cardinal($FFFFFFFF) shl (x and 31));
    while (w = 0) and (i < lastWord) do
    begin
      Inc(i);
      w := Cardinal(words[i]);
      if black then
        w := not w;
    end;
    var next := width;
    if (w <> 0) then
      next := Min((i shl 5) + TrailingZeros(w), width);
    row[count] := next - x;
    Inc(count);
    x := next;
    black := not black;
  end;
  // the last value is the number of white pixels, 0 when ending black
  if not black then
  begin
    row[count] := 0;
    Inc(count);
  end;
  SetLength(row, count);
end;

function IsPatternWidths(const widths: array of Integer; first: Integer;
  const pattern: array of Integer; e2e: Boolean; spaceInPixel: Integer;
  minQuietZone, moduleSizeRef: Double): Double;
var
  n, sum: Integer;
begin
  n := System.Length(pattern);
  sum := 0;
  for var x := 0 to n - 1 do
    Inc(sum, pattern[x]);

  if e2e then
  begin
    // bars (even elements) and spaces (odd elements) separately
    var widthBar: Double := 0;
    var widthSpace: Double := 0;
    var sumBar := 0;
    var sumSpace := 0;
    for var x := 0 to n - 1 do
      if Odd(x) then
      begin
        widthSpace := widthSpace + widths[first + x];
        Inc(sumSpace, pattern[x]);
      end
      else
      begin
        widthBar := widthBar + widths[first + x];
        Inc(sumBar, pattern[x]);
      end;
    var modBar: Double := widthBar / sumBar;
    var modSpace: Double := widthSpace / sumSpace;
    // make sure module sizes of bars and spaces are not too far away from
    // each other
    if (Max(modBar, modSpace) > 4 * Min(modBar, modSpace)) then
      exit(0);
    if (minQuietZone <> 0) and (spaceInPixel < minQuietZone * modSpace) then
      exit(0);
    var thrBar: Double := modBar * 0.75 + 0.5;
    var thrSpace: Double := modSpace * 0.6 + 0.5;
    for var x := 0 to n - 1 do
      if Odd(x) then
      begin
        if (Abs(widths[first + x] - pattern[x] * modSpace) > thrSpace) then
          exit(0);
      end
      else if (Abs(widths[first + x] - pattern[x] * modBar) > thrBar) then
        exit(0);
    exit((modBar + modSpace) / 2);
  end;

  var width: Double := 0;
  for var x := 0 to n - 1 do
    width := width + widths[first + x];
  if (sum > n) and (width < sum) then
    exit(0);
  var moduleSize: Double := width / sum;
  if (minQuietZone <> 0) and (spaceInPixel < minQuietZone * moduleSize - 1)
  then
    exit(0);
  if (moduleSizeRef = 0) then
    moduleSizeRef := moduleSize;
  // the offset of 0.5 makes it less sensitive to quantization errors for
  // small (near 1) module sizes
  var threshold: Double := moduleSizeRef * 0.5 + 0.5;
  for var x := 0 to n - 1 do
    if (Abs(widths[first + x] - pattern[x] * moduleSizeRef) > threshold) then
      exit(0);
  Result := moduleSize;
end;

function IsPattern(const view: TPatternView; const pattern: array of Integer;
  e2e: Boolean; spaceInPixel: Integer; minQuietZone,
  moduleSizeRef: Double): Double;
begin
  Result := IsPatternWidths(view.FRow, view.FData, pattern, e2e, spaceInPixel,
    minQuietZone, moduleSizeRef);
end;

function IsPattern(const widths: array of Integer;
  const pattern: array of Integer; e2e: Boolean): Double;
begin
  Result := IsPatternWidths(widths, 0, pattern, e2e, 0, 0, 0);
end;

function IsRightGuard(const view: TPatternView;
  const pattern: array of Integer; minQuietZone: Double; e2e: Boolean;
  moduleSizeRef: Double): Boolean;
begin
  if not view.IsValid then
    exit(false);
  var spaceInPixel: Integer;
  if view.IsAtLastBar then
    spaceInPixel := MaxInt
  else
    spaceInPixel := view.FRow[view.FData + view.FSize];
  Result := IsPattern(view, pattern, e2e, spaceInPixel, minQuietZone,
    moduleSizeRef) <> 0;
end;

function FindLeftGuard(const view: TPatternView; len, minSize: Integer;
  const isGuard: TGuardPredicate): TPatternView;
begin
  if (view.Size < minSize) then
    exit(TPatternView.Empty);

  var window := view.SubView(0, len);
  if window.IsAtFirstBar and isGuard(window, MaxInt) then
    exit(window);
  var last := view.FData + view.FSize - minSize;
  while (window.FData < last) do
  begin
    if isGuard(window, window.FRow[window.FData - 1]) then
      exit(window);
    window.SkipPair;
  end;
  Result := TPatternView.Empty;
end;

function FindLeftGuard(const view: TPatternView; minSize: Integer;
  const pattern: array of Integer; minQuietZone: Double;
  e2e: Boolean): TPatternView;
begin
  var len := System.Length(pattern);
  minSize := Max(minSize, len);
  if (view.Size < minSize) then
    exit(TPatternView.Empty);

  // the same as above, without a closure (it can not capture an open array)
  var window := view.SubView(0, len);
  if window.IsAtFirstBar and (IsPatternWidths(window.FRow, window.FData,
    pattern, e2e, MaxInt, minQuietZone, 0) <> 0) then
    exit(window);
  var last := view.FData + view.FSize - minSize;
  while (window.FData < last) do
  begin
    if (IsPatternWidths(window.FRow, window.FData, pattern, e2e,
      window.FRow[window.FData - 1], minQuietZone, 0) <> 0) then
      exit(window);
    window.SkipPair;
  end;
  Result := TPatternView.Empty;
end;

end.
