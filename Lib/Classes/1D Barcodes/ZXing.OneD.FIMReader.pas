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

  * USPS FIM (Facing Identification Mark) for ZXing.Delphi: zxing-cpp,
  * ZXing Java and ZXing.Net have no reader. The patterns as in zint
  * (postal.c).
}

unit ZXing.OneD.FIMReader;

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
  /// Reads the USPS FIM (Facing Identification Mark): one of the 5 fixed
  /// patterns of narrow bars A to E (the text). It has no check, so the
  /// bars must be tall (at least 10 times their width), straight and of the
  /// same height, the spaces white and the mark in a clear zone; horizontal
  /// or vertical. Only read when asked for (not in Auto).
  /// </summary>
  TFIMReader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

implementation

uses
  ZXing.ResultPoint,
  ZXing.Common.Pattern,
  ZXing.Postal.FourStateDetector;

const
  // the quiet zones in modules (the clear zone of the USPS is much larger)
  QUIET_ZONE = 6;
  // the bars at least this many times as high as wide (the USPS: 20)
  MIN_HEIGHT = 10;
  // the bars and spaces of FIM A to E (each the same turned 180 degrees)
  PATTERNS: array [0 .. 4] of string = ('111515111', '13111311131',
    '11131313111', '1111131311111', '1317131');

/// <summary>Whether the pixels of column x from row top to row bottom are
/// all dark (dark) or all white.</summary>
function IsColumn(matrix: TBitMatrix; x, top, bottom: Integer;
  dark: Boolean): Boolean;
begin
  Result := true;
  for var y := Max(top, 0) to Min(bottom, matrix.Height - 1) do
    if (matrix[x, y] <> dark) then
      exit(false);
end;

/// <summary>Whether the pixels of row y from column left to column right
/// are all white.</summary>
function IsWhiteRow(matrix: TBitMatrix; y, left, right: Integer): Boolean;
begin
  Result := true;
  for var x := Max(left, 0) to Min(right, matrix.Width - 1) do
    if matrix[x, y] then
      exit(false);
end;

/// <summary>The FIM in the runs of row y from run d (a bar): its letter, or
/// '' when there is none.</summary>
function CheckFIM(matrix: TBitMatrix; const runs: TPatternRow; d, y: Integer;
  out x0, x1, top, bottom: Integer): string;
begin
  Result := '';
  for var p := 0 to High(PATTERNS) do
  begin
    var pattern := PATTERNS[p];
    var n := Length(pattern);
    if (d + n >= Length(runs)) then
      continue;
    var widths: TArray<Integer>;
    SetLength(widths, n);
    for var k := 0 to n - 1 do
      widths[k] := runs[d + k];
    var expected: TArray<Integer>;
    SetLength(expected, n);
    for var k := 0 to n - 1 do
      expected[k] := Ord(pattern.Chars[k]) - Ord('0');
    var module := IsPattern(widths, expected, false);
    if (module = 0) then
      continue;
    // quiet zones (or the edge of the image)
    if ((d > 1) and (runs[d - 1] < QUIET_ZONE * module)) or
      ((d + n < Length(runs) - 1) and (runs[d + n] < QUIET_ZONE * module)) then
      continue;

    // the bars: tall, straight, the same height; the spaces white
    x0 := 0;
    for var k := 0 to d - 1 do
      Inc(x0, runs[k]);
    var x := x0;
    var ok := true;
    top := -1;
    bottom := -1;
    for var k := 0 to n - 1 do
    begin
      var cx := x + widths[k] div 2;
      if not Odd(k) then
      begin
        var t := y;
        while (t > 0) and matrix[cx, t - 1] do
          Dec(t);
        var b := y;
        while (b < matrix.Height - 1) and matrix[cx, b + 1] do
          Inc(b);
        if (top < 0) then
        begin
          top := t;
          bottom := b;
        end
        else if (Abs(t - top) > Max(2.0, module)) or
          (Abs(b - bottom) > Max(2.0, module)) then
          ok := false;
        // straight: dark from its left to its right edge over the height
        // (inside), and white next to it
        if ok and (not IsColumn(matrix, x, t + 1, b - 1, true) and
          not IsColumn(matrix, x + widths[k] - 1, t + 1, b - 1, true)) then
          ok := false;
      end
      else if not IsColumn(matrix, cx, top, bottom, false) then
        ok := false;
      if not ok then
        break;
      Inc(x, widths[k]);
    end;
    x1 := x - 1;
    if not ok or (bottom - top + 1 < MIN_HEIGHT * module) then
      continue;
    // the clear zone: white around the mark (in the image)
    var margin := Round(QUIET_ZONE * module);
    if (top - 2 < 0) or (bottom + 2 >= matrix.Height) or
      (x0 - margin < 0) or (x1 + margin >= matrix.Width) or
      not IsWhiteRow(matrix, top - 2, x0 - margin, x1 + margin) or
      not IsWhiteRow(matrix, bottom + 2, x0 - margin, x1 + margin) then
      continue;
    for var cx := x0 - margin to x0 - 1 do
      if ok and not IsColumn(matrix, cx, top, bottom, false) then
        ok := false;
    for var cx := x1 + 1 to x1 + margin do
      if ok and not IsColumn(matrix, cx, top, bottom, false) then
        ok := false;
    if not ok then
      continue;
    exit(Chr(Ord('A') + p));
  end;
end;

{ TFIMReader }

function TFIMReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TFIMReader.decode(const image: TBinaryBitmap;
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

procedure TFIMReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var rowStep := 4;
  if (hints <> nil) and hints.ContainsKey(TDecodeHintType.TRY_HARDER) then
    rowStep := 2;
  // horizontal marks, then vertical ones in the image turned
  for var vertical in [false, true] do
  begin
    var matrix := image.BlackMatrix;
    if vertical then
      matrix := Transposed(matrix);
    try
      var runs: TPatternRow;
      var y := rowStep div 2;
      while (y < matrix.Height) and not ResultsFull(results, maxCount) do
      begin
        GetPatternRow(matrix, y, runs);
        var d := 1;
        while (d < Length(runs)) do
        begin
          var x0, x1, top, bottom: Integer;
          var text := CheckFIM(matrix, runs, d, y, x0, x1, top, bottom);
          if (text <> '') then
          begin
            var cy := (top + bottom) / 2;
            var p1x: Double := x0;
            var p1y := cy;
            var p2x: Double := x1;
            var p2y := cy;
            if vertical then
            begin
              p1x := cy;
              p1y := x0;
              p2x := cy;
              p2y := x1;
            end;
            var r := TReadResult.Create(text, nil,
              [TResultPointHelpers.CreateResultPoint(p1x, p1y),
              TResultPointHelpers.CreateResultPoint(p2x, p2y)],
              TBarcodeFormat.FIM);
            if ContainsResult(results, r) then
              r.Free
            else
              results.Add(r);
          end;
          Inc(d, 2);
        end;
        Inc(y, rowStep);
      end;
    finally
      if vertical then
        matrix.Free;
    end;
  end;
end;

procedure TFIMReader.reset;
begin
  // do nothing
end;

end.
