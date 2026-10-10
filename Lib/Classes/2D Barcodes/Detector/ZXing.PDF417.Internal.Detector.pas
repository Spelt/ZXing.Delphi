{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
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

  * Ported from zxing-cpp (PDFDetector.cpp), originally by SITA Lab, Daniel
  * Switkin and Guenther Grau: finds PDF417 symbols by their start and stop
  * patterns (0 and 180 degrees, and 90 and 270 with tryRotate).
}

unit ZXing.PDF417.Internal.Detector;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint;

type
  /// <summary>The vertices of a symbol (nil when not found): 0 top left,
  /// 1 bottom left, 2 top right, 3 bottom right of the symbol, 4 to 7 the
  /// same of its codeword area.</summary>
  TPDF417Vertices = array [0 .. 7] of IResultPoint;

  TPDF417DetectorResult = class
  public
    /// <summary>The image, rotated by Rotation (owned when it is not the
    /// image that was given).</summary>
    Bits: TBitMatrix;
    OwnsBits: Boolean;
    Points: TList<TPDF417Vertices>;
    Rotation: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

/// <summary>The PDF417 symbols in image (also only one, when not multiple);
/// nil when there are none.</summary>
/// <param name="rotated90">Optional: gives the image rotated by 90
/// degrees (TBitMatrix.Rotated90), shared with other readers; called only
/// when needed. The result does not own it.</param>
function DetectPDF417(image: TBitMatrix; multiple, tryRotate: Boolean;
  const rotated90: TFunc<TBitMatrix> = nil): TPDF417DetectorResult;

/// <summary>A copy of image rotated by 90 degrees counter clockwise (like
/// zxing-cpp's BitMatrix.rotate90).</summary>
function RotatedBitMatrix90(image: TBitMatrix): TBitMatrix;
/// <summary>A copy of image rotated by 180 degrees.</summary>
function RotatedBitMatrix180(image: TBitMatrix): TBitMatrix;

implementation

uses
  System.Math,
  ZXing.Common.Pattern;

const
  INDEXES_START_PATTERN: array [0 .. 3] of Integer = (0, 4, 1, 5);
  INDEXES_STOP_PATTERN: array [0 .. 3] of Integer = (6, 2, 7, 3);
  MAX_AVG_VARIANCE = 0.42;
  MAX_INDIVIDUAL_VARIANCE = 0.8;
  MAX_PIXEL_DRIFT = 3;
  MAX_PATTERN_DRIFT = 5;
  // too low: the height of damaged start patterns is not found; too high:
  // the start pattern of a neighbouring symbol may be found
  SKIPPED_ROW_COUNT_MAX = 25;
  // at least 3 rows of >= 3 module widths: at least 9 pixels high
  ROW_STEP = 8;
  BARCODE_MIN_HEIGHT = 10;

  START_PATTERN: array [0 .. 7] of Integer = (8, 1, 1, 1, 1, 1, 1, 3);
  STOP_PATTERN: array [0 .. 8] of Integer = (7, 1, 1, 3, 1, 1, 1, 2, 1);

type
  TVertices4 = array [0 .. 3] of IResultPoint;

{ TPDF417DetectorResult }

constructor TPDF417DetectorResult.Create;
begin
  inherited;
  Points := TList<TPDF417Vertices>.Create;
  Rotation := -1;
end;

destructor TPDF417DetectorResult.Destroy;
begin
  Points.Free;
  if OwnsBits then
    Bits.Free;
  inherited;
end;

function RotatedBitMatrix90(image: TBitMatrix): TBitMatrix;
begin
  Result := image.Rotated90;
end;

function RotatedBitMatrix180(image: TBitMatrix): TBitMatrix;
begin
  Result := TBitMatrix.Create(image.Width, image.Height);
  for var y := 0 to image.Height - 1 do
    for var x := 0 to image.Width - 1 do
      if image[x, y] then
        Result[image.Width - x - 1, image.Height - y - 1] := true;
end;

function PatternMatchVariance(const counters, pattern: array of Integer;
  maxIndividualVariance: Single): Single;
begin
  var total := 0;
  var patternLength := 0;
  for var i := 0 to High(counters) do
  begin
    Inc(total, counters[i]);
    Inc(patternLength, pattern[i]);
  end;
  // less than one pixel per module: too small to match reliably
  if (total < patternLength) then
    exit(MaxSingle);
  var unitBarWidth: Single := total / patternLength;
  maxIndividualVariance := maxIndividualVariance * unitBarWidth;
  var totalVariance: Single := 0;
  for var x := 0 to High(counters) do
  begin
    var scaledPattern := pattern[x] * unitBarWidth;
    var variance := Abs(counters[x] - scaledPattern);
    if (variance > maxIndividualVariance) then
      exit(MaxSingle);
    totalVariance := totalVariance + variance;
  end;
  Result := totalVariance / total;
end;

/// <summary>The start and end of the guard pattern on the row, searching
/// from column.</summary>
function FindGuardPattern(matrix: TBitMatrix; column, row, width: Integer;
  whiteFirst: Boolean; const pattern: array of Integer;
  var counters: TArray<Integer>; out startPos, endPos: Integer): Boolean;
begin
  for var i := 0 to High(counters) do
    counters[i] := 0;
  var patternLength := Length(pattern);
  var isWhite := whiteFirst;
  var patternStart := column;
  var pixelDrift := 0;

  // black pixels left of the start: shift left, at most MAX_PIXEL_DRIFT
  while matrix[patternStart, row] and (patternStart > 0) and
    (pixelDrift < MAX_PIXEL_DRIFT) do
  begin
    Inc(pixelDrift);
    Dec(patternStart);
  end;
  var x := patternStart;
  var counterPosition := 0;
  while (x < width) do
  begin
    var pixel := matrix[x, row];
    if (pixel <> isWhite) then
      Inc(counters[counterPosition])
    else
    begin
      if (counterPosition = patternLength - 1) then
      begin
        if (PatternMatchVariance(counters, pattern, MAX_INDIVIDUAL_VARIANCE) <
          MAX_AVG_VARIANCE) then
        begin
          startPos := patternStart;
          endPos := x;
          exit(true);
        end;
        Inc(patternStart, counters[0] + counters[1]);
        for var i := 2 to patternLength - 1 do
          counters[i - 2] := counters[i];
        counters[patternLength - 2] := 0;
        counters[patternLength - 1] := 0;
        Dec(counterPosition);
      end
      else
        Inc(counterPosition);
      counters[counterPosition] := 1;
      isWhite := not isWhite;
    end;
    Inc(x);
  end;
  if (counterPosition = patternLength - 1) and
    (PatternMatchVariance(counters, pattern, MAX_INDIVIDUAL_VARIANCE) <
    MAX_AVG_VARIANCE) then
  begin
    startPos := patternStart;
    endPos := x - 1;
    exit(true);
  end;
  Result := false;
end;

function FindRowsWithPattern(matrix: TBitMatrix; height, width, startRow,
  startColumn: Integer; const pattern: array of Integer): TVertices4;
begin
  for var i := 0 to 3 do
    Result[i] := nil;
  var found := false;
  var startPos, endPos: Integer;
  var minStartRow := startRow;
  var counters: TArray<Integer>;
  SetLength(counters, Length(pattern));
  while (startRow < height) do
  begin
    if FindGuardPattern(matrix, startColumn, startRow, width, false, pattern,
      counters, startPos, endPos) then
    begin
      while (startRow > minStartRow + 1) do
      begin
        Dec(startRow);
        if not FindGuardPattern(matrix, startColumn, startRow, width, false,
          pattern, counters, startPos, endPos) then
        begin
          Inc(startRow);
          break;
        end;
      end;
      Result[0] := TResultPointHelpers.CreateResultPoint(startPos, startRow);
      Result[1] := TResultPointHelpers.CreateResultPoint(endPos, startRow);
      found := true;
      break;
    end;
    Inc(startRow, ROW_STEP);
  end;
  var stopRow := startRow + 1;
  // the last row of the symbol with the pattern
  if found then
  begin
    var skippedRowCount := 0;
    var previousRowStart := Trunc(Result[0].X);
    var previousRowEnd := Trunc(Result[1].X);
    while (stopRow < height) do
    begin
      var rowFound := FindGuardPattern(matrix, previousRowStart, stopRow,
        width, false, pattern, counters, startPos, endPos);
      // only the same symbol when the start and end differ not too much
      // (a slightly larger drift is allowed, skipped rows are not checked)
      if rowFound and (Abs(previousRowStart - startPos) < MAX_PATTERN_DRIFT)
        and (Abs(previousRowEnd - endPos) < MAX_PATTERN_DRIFT) then
      begin
        previousRowStart := startPos;
        previousRowEnd := endPos;
        skippedRowCount := 0;
      end
      else if (skippedRowCount > SKIPPED_ROW_COUNT_MAX) then
        break
      else
        Inc(skippedRowCount);
      Inc(stopRow);
    end;
    Dec(stopRow, skippedRowCount + 1);
    Result[2] := TResultPointHelpers.CreateResultPoint(previousRowStart,
      stopRow);
    Result[3] := TResultPointHelpers.CreateResultPoint(previousRowEnd,
      stopRow);
  end;
  if (stopRow - startRow < BARCODE_MIN_HEIGHT) then
    for var i := 0 to 3 do
      Result[i] := nil;
end;

/// <summary>The vertices of a symbol with its start and (when found) stop
/// pattern.</summary>
function FindVertices(matrix: TBitMatrix; startRow, startColumn: Integer)
  : TPDF417Vertices;
begin
  for var i := 0 to 7 do
    Result[i] := nil;
  var tmp := FindRowsWithPattern(matrix, matrix.Height, matrix.Width, startRow,
    startColumn, START_PATTERN);
  for var i := 0 to 3 do
    Result[INDEXES_START_PATTERN[i]] := tmp[i];
  // only symbols with a start pattern (twice as fast on images without a
  // symbol)
  if (Result[4] <> nil) then
  begin
    startColumn := Trunc(Result[4].X);
    startRow := Trunc(Result[4].Y);
    tmp := FindRowsWithPattern(matrix, matrix.Height, matrix.Width, startRow,
      startColumn, STOP_PATTERN);
    for var i := 0 to 3 do
      Result[INDEXES_STOP_PATTERN[i]] := tmp[i];
  end;
end;

procedure DetectBarcode(bitMatrix: TBitMatrix; multiple: Boolean;
  coordinates: TList<TPDF417Vertices>);
begin
  var row := 0;
  var column := 0;
  var foundBarcodeInRow := false;
  while (row < bitMatrix.Height) do
  begin
    var vertices := FindVertices(bitMatrix, row, column);
    if (vertices[0] = nil) and (vertices[3] = nil) then
    begin
      if not foundBarcodeInRow then
        break;
      // none at this column and row: try again from the first column below
      // the lowest symbol found so far
      foundBarcodeInRow := false;
      column := 0;
      for var c in coordinates do
      begin
        if (c[1] <> nil) then
          row := Max(row, Trunc(c[1].Y));
        if (c[3] <> nil) then
          row := Max(row, Trunc(c[3].Y));
      end;
      Inc(row, ROW_STEP);
      continue;
    end;
    foundBarcodeInRow := true;
    coordinates.Add(vertices);
    if not multiple then
      break;
    // without a right row indicator column, search on behind the start
    // pattern of this symbol
    if (vertices[2] <> nil) then
    begin
      column := Trunc(vertices[2].X);
      row := Trunc(vertices[2].Y);
    end
    else
    begin
      column := Trunc(vertices[4].X);
      row := Trunc(vertices[4].Y);
    end;
  end;
end;

procedure GetPatternColumn(m: TBitMatrix; x: Integer; var row: TPatternRow);
begin
  // like GetPatternRow, for a column from top to bottom
  var n := 0;
  SetLength(row, m.Height + 2);
  var current := false; // white
  row[0] := 0;
  for var y := 0 to m.Height - 1 do
  begin
    if (m[x, y] <> current) then
    begin
      Inc(n);
      row[n] := 0;
      current := not current;
    end;
    Inc(row[n]);
  end;
  // end with a white run
  if current then
  begin
    Inc(n);
    row[n] := 0;
  end;
  SetLength(row, n + 1);
end;

function HasStartPattern(m: TBitMatrix; rotate90: Boolean): Boolean;
const
  MIN_SYMBOL_WIDTH = 3 * 8 + 1; // compact symbol
begin
  var row: TPatternRow;
  var ending := m.Height;
  if rotate90 then
    ending := m.Width;
  var r := ROW_STEP;
  while (r < ending) do
  begin
    if rotate90 then
      GetPatternColumn(m, r, row)
    else
      GetPatternRow(m, r, row);
    if FindLeftGuard(TPatternView.Create(row), MIN_SYMBOL_WIDTH, START_PATTERN,
      2).IsValid then
      exit(true);
    // reversed
    var n := Length(row);
    for var i := 0 to n div 2 - 1 do
    begin
      var t := row[i];
      row[i] := row[n - 1 - i];
      row[n - 1 - i] := t;
    end;
    if FindLeftGuard(TPatternView.Create(row), MIN_SYMBOL_WIDTH, START_PATTERN,
      2).IsValid then
      exit(true);
    Inc(r, ROW_STEP);
  end;
  Result := false;
end;

function DetectPDF417(image: TBitMatrix; multiple, tryRotate: Boolean;
  const rotated90: TFunc<TBitMatrix>): TPDF417DetectorResult;
begin
  Result := nil;
  if (image = nil) then
    exit;
  for var rotate90 := 0 to Ord(tryRotate) do
  begin
    if not HasStartPattern(image, rotate90 = 1) then
      continue;

    var res := TPDF417DetectorResult.Create;
    try
      res.Rotation := 90 * rotate90;
      if (rotate90 = 1) and Assigned(rotated90) then
      begin
        // the shared rotated matrix (also used by MicroPDF417)
        res.Bits := rotated90();
        res.OwnsBits := false;
      end
      else if (rotate90 = 1) then
      begin
        res.Bits := RotatedBitMatrix90(image);
        res.OwnsBits := true;
      end
      else
      begin
        res.Bits := image;
        res.OwnsBits := false;
      end;

      DetectBarcode(res.Bits, multiple, res.Points);
      if (res.Points.Count = 0) then
      begin
        var newBits := RotatedBitMatrix180(res.Bits);
        if res.OwnsBits then
          res.Bits.Free;
        res.Bits := newBits;
        res.OwnsBits := true;
        Inc(res.Rotation, 180);
        DetectBarcode(res.Bits, multiple, res.Points);
      end;

      if (res.Points.Count > 0) then
      begin
        Result := res;
        res := nil;
        exit;
      end;
    finally
      res.Free;
    end;
  end;
end;

end.
