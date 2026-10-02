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

  * Ported from zxing-cpp (Pattern.h, the parts used by the QR Code detector):
  * a row of the image as the widths of its bars and spaces, and a view on a
  * part of it to match patterns like the 1:1:3:1:1 of a QR finder pattern.
}

interface

uses
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
    procedure Extend;

    /// <summary>The element i of the view; -1 is the one in front of it.
    /// </summary>
    property Items[i: Integer]: Integer read GetItem; default;
    property Data: Integer read FData;
  end;

/// <summary>The runs of row y of matrix.</summary>
procedure GetPatternRow(matrix: TBitMatrix; y: Integer; var row: TPatternRow);

/// <summary>
/// Whether the first Length(pattern) elements of view match pattern
/// (zxing-cpp's IsPattern), edge to edge (e2e: bars and spaces each with
/// their own module size) or not. spaceInPixel: the white space in front,
/// at least minQuietZone modules when minQuietZone > 0. Returns the module
/// size, or 0 when the view does not match.
/// </summary>
function IsPattern(const view: TPatternView; const pattern: array of Integer;
  e2e: Boolean; spaceInPixel: Integer = 0; minQuietZone: Double = 0): Double;
  overload;
/// <summary>The same for a plain array of widths, like a pattern read with
/// a cursor.</summary>
function IsPattern(const widths: array of Integer;
  const pattern: array of Integer; e2e: Boolean): Double; overload;

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

procedure TPatternView.Extend;
begin
  FSize := Max(0, System.Length(FRow) - FData);
end;

procedure GetPatternRow(matrix: TBitMatrix; y: Integer; var row: TPatternRow);
begin
  var width := matrix.Width;
  SetLength(row, width + 2);
  FillChar(row[0], System.Length(row) * SizeOf(Integer), 0);
  if (width = 0) then
  begin
    SetLength(row, 1);
    exit;
  end;

  // the first value is the number of white pixels, 0 when starting black
  var pos := 0;
  var last := matrix[0, y];
  if last then
    pos := 1;
  Inc(row[pos]);
  for var x := 1 to width - 1 do
  begin
    var v := matrix[x, y];
    if (v <> last) then
    begin
      Inc(pos);
      last := v;
    end;
    Inc(row[pos]);
  end;
  // the last value is the number of white pixels, 0 when ending black
  if last then
    Inc(pos);
  SetLength(row, pos + 1);
end;

function IsPatternWidths(const widths: array of Integer; first: Integer;
  const pattern: array of Integer; e2e: Boolean; spaceInPixel: Integer;
  minQuietZone: Double): Double;
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
  var threshold: Double := moduleSize * 0.5 + 0.5;
  for var x := 0 to n - 1 do
    if (Abs(widths[first + x] - pattern[x] * moduleSize) > threshold) then
      exit(0);
  Result := moduleSize;
end;

function IsPattern(const view: TPatternView; const pattern: array of Integer;
  e2e: Boolean; spaceInPixel: Integer; minQuietZone: Double): Double;
begin
  Result := IsPatternWidths(view.FRow, view.FData, pattern, e2e, spaceInPixel,
    minQuietZone);
end;

function IsPattern(const widths: array of Integer;
  const pattern: array of Integer; e2e: Boolean): Double;
begin
  Result := IsPatternWidths(widths, 0, pattern, e2e, 0, 0);
end;

end.
