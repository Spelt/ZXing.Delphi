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

  * Ported from zxing-cpp (ODDataBarCommon.h/.cpp): what the GS1 DataBar
  * readers (ISO/IEC 24724:2011) have in common.
}

unit ZXing.OneD.DataBarCommon;

interface

uses
  System.SysUtils,
  ZXing.Common.Pattern,
  ZXing.ResultPoint;

type
  /// <summary>A data character: its value and its checksum part; Value -1
  /// when it could not be read.</summary>
  TDataBarCharacter = record
    Value: Integer;
    Checksum: Integer;
    class function Create(value, checksum: Integer): TDataBarCharacter; static;
    class function Invalid: TDataBarCharacter; static;
    function IsValid: Boolean; inline;
    function SameAs(const other: TDataBarCharacter): Boolean; inline;
  end;

  /// <summary>A pair of data characters with the finder pattern between
  /// them, as found on a row: XStart and XStop in pixels, Y the row and
  /// Count how many rows it was found on. Finder 0 when it is not valid,
  /// negative when it is laid out right to left.</summary>
  TDataBarPair = record
    Left, Right: TDataBarCharacter;
    Finder, XStart, XStop, Y, Count: Integer;
    class function Create(const left, right: TDataBarCharacter;
      finder, xStart, xStop: Integer): TDataBarPair; static;
    class function Invalid: TDataBarPair; static;
    function IsValid: Boolean; inline;
    function SameAs(const other: TDataBarPair): Boolean;
    function Center: Integer; inline;
  end;

  TDataBarPairs = TArray<TDataBarPair>;

  /// <summary>The 3 edge to similar edge widths (in modules) of a finder
  /// pattern.</summary>
  TFinderE2E = array [0 .. 2] of Integer;

  TArray4I = array [0 .. 3] of Integer;

const
  FULL_PAIR_SIZE = 8 + 5 + 8;
  // a half pair has to be followed by a guard pattern
  HALF_PAIR_SIZE = 8 + 5 + 2;

/// <summary>Whether a to e (the 5 widths of a finder pattern, 15 modules)
/// fit a finder pattern.</summary>
function IsFinder(a, b, c, d, e: Integer): Boolean;
/// <summary>The finder pattern of a pair.</summary>
function FinderView(const view: TPatternView): TPatternView; inline;
function LeftCharView(const view: TPatternView): TPatternView; inline;
function RightCharView(const view: TPatternView): TPatternView; inline;
function ModSizeFinder(const view: TPatternView): Single;
function IsGuard(a, b: Integer): Boolean;
function IsCharacter(const view: TPatternView; modules: Integer;
  modSizeRef: Single): Boolean;

/// <summary>The edge to similar edge widths of the first len elements of
/// view in modules (len - 2 values), mods modules in total, from the back
/// with reverse.</summary>
function NormalizedE2EPattern(const view: TPatternView; len, mods: Integer;
  reverse: Boolean = false): TArray<Integer>;
/// <summary>The index + 1 of the finder pattern of patterns that view (the 5
/// elements of a finder) matches best, 0 when none does; negative when
/// reversed.</summary>
function ParseFinderPattern(const view: TPatternView; reversed: Boolean;
  const patterns: array of TFinderE2E): Integer;
/// <summary>The element widths of an (n, k) character of len elements and
/// mods modules: DataBar 8 elements of 15 or 16 modules, DataBar Expanded 8
/// of 17, DataBar Limited 14 of 26 or 18.</summary>
function NormalizedPatternFromE2E(const view: TPatternView; len, mods: Integer;
  reversed: Boolean = false): TArray<Integer>;
/// <summary>The odd and even element widths of a data character of 8
/// elements; false when they do not fit exactly.</summary>
function ReadDataCharacterRaw(const view: TPatternView; numModules: Integer;
  reversed: Boolean; out oddPattern, evnPattern: TArray4I): Boolean;
/// <summary>The value of the widths of a character (RSS utilities of
/// ISO/IEC 24724).</summary>
function GetValue(const widths: array of Integer; maxWidth: Integer;
  noNarrow: Boolean): Integer;

/// <summary>The widths as bits, bars as 1 (zxing-cpp's ToInt); -1 for a
/// negative width.</summary>
function PatternToInt(const widths: array of Integer): Integer;
/// <summary>The GS1 check digit of digits.</summary>
function GTINCheckDigit(const digits: string): Char;

/// <summary>Whether first and last are rows of a stacked symbol.</summary>
function IsStacked(const first, last: TDataBarPair): Boolean;
/// <summary>The result points: the line of a linear symbol, or top left,
/// bottom left, bottom right, top right of a stacked one.</summary>
function EstimatePosition(const first, last: TDataBarPair)
  : TArray<IResultPoint>;
/// <summary>The number of lines a symbol counts for (zxing-cpp's
/// EstimateLineCount + 1): 2 or more confirms it.</summary>
function EstimateLineCount(const first, last: TDataBarPair): Integer;

implementation

uses
  System.Math;

{ TDataBarCharacter }

class function TDataBarCharacter.Create(value, checksum: Integer)
  : TDataBarCharacter;
begin
  Result.Value := value;
  Result.Checksum := checksum;
end;

class function TDataBarCharacter.Invalid: TDataBarCharacter;
begin
  Result.Value := -1;
  Result.Checksum := 0;
end;

function TDataBarCharacter.IsValid: Boolean;
begin
  Result := (Value <> -1);
end;

function TDataBarCharacter.SameAs(const other: TDataBarCharacter): Boolean;
begin
  Result := (Value = other.Value) and (Checksum = other.Checksum);
end;

{ TDataBarPair }

class function TDataBarPair.Create(const left, right: TDataBarCharacter;
  finder, xStart, xStop: Integer): TDataBarPair;
begin
  Result.Left := left;
  Result.Right := right;
  Result.Finder := finder;
  Result.XStart := xStart;
  Result.XStop := xStop;
  Result.Y := -1;
  Result.Count := 1;
end;

class function TDataBarPair.Invalid: TDataBarPair;
begin
  Result := Create(TDataBarCharacter.Invalid, TDataBarCharacter.Invalid, 0,
    -1, 1);
end;

function TDataBarPair.IsValid: Boolean;
begin
  Result := (Finder <> 0);
end;

function TDataBarPair.SameAs(const other: TDataBarPair): Boolean;
begin
  Result := (Finder = other.Finder) and Left.SameAs(other.Left) and
    Right.SameAs(other.Right);
end;

function TDataBarPair.Center: Integer;
begin
  Result := XStart + (XStop - XStart) div 2;
end;

{ finder patterns and characters }

function IsFinder(a, b, c, d, e: Integer): Boolean;
begin
  //  a,b,c,d,e, g | sum(a..e) = 15
  //  1,1,2
  //  | | |,1,1, 1
  //  3,8,9
  // only pairs of bar + space, to limit the effect of a poor threshold:
  // b + c can be 10, 11 or 12 modules, d + e is always 2; the offsets (5
  // and 2) reduce quantization effects for small module sizes
  var w := 2 * (b + c);
  var n := d + e;
  Result := (w + 5 > 9 * n) and (w - 5 < 13 * n) and (a < 2 + 4 * e) and
    (4 * a > n);
end;

function FinderView(const view: TPatternView): TPatternView;
begin
  Result := view.SubView(8, 5);
end;

function LeftCharView(const view: TPatternView): TPatternView;
begin
  Result := view.SubView(0, 8);
end;

function RightCharView(const view: TPatternView): TPatternView;
begin
  Result := view.SubView(13, 8);
end;

function ModSizeFinder(const view: TPatternView): Single;
begin
  Result := FinderView(view).Sum / 15;
end;

function IsGuard(a, b: Integer): Boolean;
begin
  Result := (a > b * 3 div 4 - 2) and (a < b * 5 div 4 + 2);
end;

function IsCharacter(const view: TPatternView; modules: Integer;
  modSizeRef: Single): Boolean;
begin
  var err := Abs(view.Sum / modules / modSizeRef - 1);
  Result := (err < 0.1);
end;

function NormalizedE2EPattern(const view: TPatternView; len, mods: Integer;
  reverse: Boolean): TArray<Integer>;
begin
  SetLength(Result, len - 2);
  var moduleSize: Double := view.Sum(len) / mods;
  if not (moduleSize > 0) then
    exit;
  for var i := 0 to len - 3 do
  begin
    var iv := i;
    if reverse then
      iv := len - 2 - i;
    var v: Double := (view[iv] + view[iv + 1]) / moduleSize;
    Result[i] := Trunc(v + 0.5);
  end;
end;

function ParseFinderPattern(const view: TPatternView; reversed: Boolean;
  const patterns: array of TFinderE2E): Integer;
begin
  var e2e := NormalizedE2EPattern(view, 5, 15, reversed);
  var bestI := -1;
  var bestE := 3;
  for var i := 0 to High(patterns) do
  begin
    var e := 0;
    for var j := 0 to 2 do
      Inc(e, Abs(patterns[i][j] - e2e[j]));
    if (e < bestE) then
    begin
      bestE := e;
      bestI := i;
    end;
  end;
  Result := 0;
  if (bestE <= 1) then
    Result := 1 + bestI;
  if reversed then
    Result := -Result;
end;

function NormalizedPatternFromE2E(const view: TPatternView; len, mods: Integer;
  reversed: Boolean): TArray<Integer>;
begin
  // To disambiguate the edge to edge measurements, either the odd or the
  // even elements contain at least 1 element that is 1 module wide (the
  // even elements, 2nd, 4th, ..., have odd indexes): min-even-is-one for
  // DataBar Limited and the outside characters of DataBar, min-odd-is-one
  // for DataBar Expanded and the inside characters of DataBar (zxing-cpp
  // issue 935)
  var minOddIsOne := (mods = 15) or (mods = 17);
  var e2e := NormalizedE2EPattern(view, len, mods, reversed);
  SetLength(Result, len);

  // the element widths from the edge to similar edge widths, assuming the
  // first bar is 1 (or 8)
  if minOddIsOne then
    Result[0] := 8
  else
    Result[0] := 1;
  var barSum := Result[0];
  for var i := 0 to High(e2e) do
  begin
    Result[i + 1] := e2e[i] - Result[i];
    Inc(barSum, Result[i + 1]);
  end;
  // the last even element makes mods modules
  Result[len - 1] := mods - barSum;

  var minOdd := Result[0];
  var minEvn := Result[1];
  for var i := 2 to len - 1 do
    if Odd(i) then
      minEvn := Min(minEvn, Result[i])
    else
      minOdd := Min(minOdd, Result[i]);

  if minOddIsOne and (minOdd > 1) then
  begin
    // the minimum odd width is too big: readjust so that it is 1
    var i := 0;
    while (i < len) do
    begin
      Dec(Result[i], minOdd - 1);
      Inc(Result[i + 1], minOdd - 1);
      Inc(i, 2);
    end;
  end
  else if not minOddIsOne and (minEvn > 1) then
  begin
    // the minimum even width is too big: readjust so that it is 1
    var i := 0;
    while (i < len) do
    begin
      Inc(Result[i], minEvn - 1);
      Dec(Result[i + 1], minEvn - 1);
      Inc(i, 2);
    end;
  end;
end;

function ReadDataCharacterRaw(const view: TPatternView; numModules: Integer;
  reversed: Boolean; out oddPattern, evnPattern: TArray4I): Boolean;
begin
  var pattern := NormalizedPatternFromE2E(view, 8, numModules, reversed);
  var oddSum := 0;
  var evnSum := 0;
  for var i := 0 to 7 do
    if Odd(i) then
    begin
      evnPattern[i div 2] := pattern[i];
      Inc(evnSum, pattern[i]);
    end
    else
    begin
      oddPattern[i div 2] := pattern[i];
      Inc(oddSum, pattern[i]);
    end;

  // a DataBar Expanded data character is 17 modules wide, a DataBar outside
  // one 16 and a DataBar inside one 15; each has 4 bars and 4 spaces
  var minSum := 4;
  var maxSum := numModules - minSum;
  var sumErr := oddSum + evnSum - numModules;
  var oddSumErr := Min(0, oddSum - (minSum + Ord(numModules = 15))) +
    Max(0, oddSum - maxSum);
  var evnSumErr := Min(0, evnSum - minSum) +
    Max(0, evnSum - (maxSum - Ord(numModules = 15)));
  var oddParityErr := ((oddSum and 1) = 1) = (numModules > 15);
  var evnParityErr := ((evnSum and 1) = 1) = (numModules < 17);

  // like zxing-cpp: no attempt to fix off by one errors (that gives many
  // misreads with DataBar Expanded), only characters that fit exactly
  Result := (sumErr = 0) and (oddSumErr = 0) and (evnSumErr = 0) and
    not oddParityErr and not evnParityErr;
end;

function Combins(n, r: Integer): Integer;
begin
  var minDenom, maxDenom: Integer;
  if (n - r > r) then
  begin
    minDenom := r;
    maxDenom := n - r;
  end
  else
  begin
    minDenom := n - r;
    maxDenom := r;
  end;
  var val := 1;
  var j := 1;
  var i := n;
  while (i > maxDenom) do
  begin
    val := val * i;
    if (j <= minDenom) then
    begin
      val := val div j;
      Inc(j);
    end;
    Dec(i);
  end;
  while (j <= minDenom) do
  begin
    val := val div j;
    Inc(j);
  end;
  Result := val;
end;

function GetValue(const widths: array of Integer; maxWidth: Integer;
  noNarrow: Boolean): Integer;
begin
  var elements := Length(widths);
  var n := 0;
  for var w in widths do
    Inc(n, w);
  var val := 0;
  var narrowMask := 0;
  for var bar := 0 to elements - 2 do
  begin
    var elmWidth := 1;
    narrowMask := narrowMask or (1 shl bar);
    while (elmWidth < widths[bar]) do
    begin
      var subVal := Combins(n - elmWidth - 1, elements - bar - 2);
      if noNarrow and (narrowMask = 0) and
        (n - elmWidth - (elements - bar - 1) >= elements - bar - 1) then
        Dec(subVal, Combins(n - elmWidth - (elements - bar),
          elements - bar - 2));
      if (elements - bar - 1 > 1) then
      begin
        var lessVal := 0;
        var mxwElement := n - elmWidth - (elements - bar - 2);
        while (mxwElement > maxWidth) do
        begin
          Inc(lessVal, Combins(n - elmWidth - mxwElement - 1,
            elements - bar - 3));
          Dec(mxwElement);
        end;
        Dec(subVal, lessVal * (elements - 1 - bar));
      end
      else if (n - elmWidth > maxWidth) then
        Dec(subVal);
      Inc(val, subVal);
      Inc(elmWidth);
      narrowMask := narrowMask and not (1 shl bar);
    end;
    Dec(n, elmWidth);
  end;
  Result := val;
end;

function PatternToInt(const widths: array of Integer): Integer;
begin
  Result := 0;
  for var i := 0 to High(widths) do
  begin
    if (widths[i] < 0) or (widths[i] > 31) then
      exit(-1);
    Result := Result shl widths[i];
    if not Odd(i) then
      Result := Result or ((1 shl widths[i]) - 1);
  end;
end;

function GTINCheckDigit(const digits: string): Char;
begin
  var sum := 0;
  var n := Length(digits);
  var i := n;
  while (i >= 1) do
  begin
    Inc(sum, Ord(digits[i]) - Ord('0'));
    Dec(i, 2);
  end;
  sum := sum * 3;
  i := n - 1;
  while (i >= 1) do
  begin
    Inc(sum, Ord(digits[i]) - Ord('0'));
    Dec(i, 2);
  end;
  Result := Char(Ord('0') + (10 - sum mod 10) mod 10);
end;

{ position }

function IsStacked(const first, last: TDataBarPair): Boolean;
begin
  // two halves far away from each other in y or overlapping in x
  Result := (Abs(first.Y - last.Y) > first.XStop - first.XStart) or
    (last.XStart < (first.XStart + first.XStop) div 2);
end;

function EstimatePosition(const first, last: TDataBarPair)
  : TArray<IResultPoint>;

  function point(x, y: Integer): IResultPoint;
  begin
    Result := TResultPointHelpers.CreateResultPoint(x, y);
  end;

begin
  if not IsStacked(first, last) then
  begin
    var y := (first.Y + last.Y) div 2;
    Result := [point(first.XStart, y), point(last.XStop, y)];
  end
  else
    Result := [point(first.XStart, first.Y), point(last.XStart, last.Y),
      point(last.XStop, last.Y), point(first.XStop, first.Y)];
end;

function EstimateLineCount(const first, last: TDataBarPair): Integer;
begin
  Result := Max(0, Min(first.Count, last.Count) - 2) +
    Ord(IsStacked(first, last)) + 1;
end;

end.
