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

  * Code 16K for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net have no
  * reader. The encoding as in zint (code16k.c, after BS EN 12323:2005).
}

unit ZXing.Stacked.Code16KReader;

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
  /// Reads Code 16K: 2 to 16 rows of 5 Code 128 characters, each row with
  /// a start and a stop pattern that tell its number, the first character
  /// the number of rows and the mode, the last two check characters of the
  /// whole symbol. Horizontal, also upside down; vertical with TRY_HARDER.
  /// </summary>
  TCode16KReader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>The text of the characters of a Code 16K (5 per row, the rows in
/// the order of their numbers, the check characters included) and whether
/// it starts with FNC1 (GS1); '' when they are none.</summary>
function DecodeCode16K(const values: TArray<Integer>;
  out fnc1First: Boolean): string;

implementation

uses
  ZXing.ResultPoint,
  ZXing.Common.Pattern,
  ZXing.OneD.Code128Reader,
  ZXing.Postal.FourStateDetector;

const
  CODE_SHIFT = 98;
  CODE_CODE_C = 99;
  CODE_FNC_1 = 102;
  CODE_PAD = 103;
  CODE_START_A = 103;
  CODE_START_B = 104;
  CODE_START_C = 105;
  CHAR_LEN = 6;
  // the start and stop patterns (4 elements, 7 modules)
  START_STOP: array [0 .. 7, 0 .. 3] of Integer = ((3, 2, 1, 1), (2, 2, 2, 1),
    (2, 1, 2, 2), (1, 4, 1, 1), (1, 1, 3, 2), (1, 2, 3, 1), (1, 1, 1, 4),
    (3, 1, 1, 2));
  // the start and stop patterns of the rows
  START_OF_ROW: array [0 .. 15] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 0, 1,
    2, 3, 4, 5, 6, 7);
  STOP_OF_ROW: array [0 .. 15] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6,
    7, 0, 1, 2, 3);
  QUIET_ZONE = 5;

type
  TRowRead = record
    Values: TArray<Integer>;
    Index: Integer;
    XStart, XStop: Double;
    Y: Integer;
  end;

function DecodeCode16K(const values: TArray<Integer>;
  out fnc1First: Boolean): string;
begin
  Result := '';
  fnc1First := false;
  var n := Length(values);
  if (n < 10) or (n mod 5 <> 0) then
    exit;
  // the check characters (modulo 107)
  var first := 0;
  var second := 0;
  for var i := 0 to n - 3 do
  begin
    Inc(first, (i + 2) * values[i]);
    Inc(second, (i + 1) * values[i]);
  end;
  first := first mod 107;
  Inc(second, first * (n - 1));
  if (values[n - 2] <> first) or (values[n - 1] <> second mod 107) then
    exit;
  // the number of rows and the mode (4.3.4.2)
  if (values[0] div 7 + 2 <> n div 5) then
    exit;
  var mode := values[0] mod 7;
  // as Code 128 codes: a start code (and FNC1), the data without the pads
  var data := values;
  SetLength(data, n - 2);
  var last := High(data);
  while (last >= 1) and (data[last] = CODE_PAD) do
    Dec(last);
  var codes: TArray<Integer>;
  case mode of
    0:
      codes := [CODE_START_A];
    1, 5, 6:
      codes := [CODE_START_B];
    2:
      codes := [CODE_START_C];
    3:
      codes := [CODE_START_B, CODE_FNC_1];
    4:
      codes := [CODE_START_C, CODE_FNC_1];
  end;
  for var i := 1 to last do
  begin
    // (a pad in the data: none)
    if (data[i] = CODE_PAD) then
      exit;
    codes := codes + [data[i]];
    // Shift B (5): one character of B, Double Shift B (6): two, then C
    if (mode = 5) and (i = 1) or (mode = 6) and (i = 2) then
      codes := codes + [CODE_CODE_C];
  end;
  Result := TCode128Reader.CodesToText(codes, false, fnc1First);
end;

/// <summary>The index of the start or stop pattern of the 4 widths from
/// view[offset] (module size module, about); -1 when it is none.</summary>
function StartStopIndex(const view: TPatternView; offset: Integer;
  module: Double): Integer;
begin
  for var k := 0 to 7 do
  begin
    var fits := true;
    for var i := 0 to 3 do
      if (Abs(view[offset + i] - START_STOP[k, i] * module) >
        0.5 * module + 0.5) then
      begin
        fits := false;
        break;
      end;
    if fits then
      exit(k);
  end;
  Result := -1;
end;

/// <summary>The rows of a Code 16K in the runs of row y: a start pattern, a
/// guard bar, 5 characters (spaces first) and a stop pattern, between quiet
/// zones.</summary>
procedure ReadRows(const runs: TPatternRow; y, width: Integer;
  reversed: Boolean; rows: TList<TRowRead>);
const
  // start 4, guard 1, characters 30, stop 4
  ROW_LEN = 39;
begin
  var view := TPatternView.Create(runs);
  view := view.SubView(0, ROW_LEN);
  while view.IsValid do
  begin
    // a quiet zone in front, 70 modules
    var module: Double := view.Sum(ROW_LEN) / 70;
    if (module > 0) and ((view.IsAtFirstBar or (view.SpaceInFront >=
      QUIET_ZONE * module)) and (view.IsAtLastBar or (view[ROW_LEN] >=
      QUIET_ZONE * module))) then
    begin
      var start := StartStopIndex(view, 0, module);
      var stop := StartStopIndex(view, ROW_LEN - 4, module);
      if (start >= 0) and (stop >= 0) and
        (Abs(view[4] - module) <= 0.5 * module + 0.5) then
      begin
        var row: TRowRead;
        row.Index := -1;
        for var r := 0 to 15 do
          if (START_OF_ROW[r] = start) and (STOP_OF_ROW[r] = stop) then
            row.Index := r;
        SetLength(row.Values, 5);
        for var c := 0 to 4 do
        begin
          var code := TCode128Reader.DecodeCodeOf(view.SubView(5 + CHAR_LEN *
            c, CHAR_LEN), false);
          row.Values[c] := code;
          if (code < 0) or (code > 106) then
            row.Index := -1;
        end;
        if (row.Index >= 0) then
        begin
          row.XStart := view.PixelsInFront;
          row.XStop := view.PixelsInFront + view.Sum(ROW_LEN);
          if reversed then
          begin
            row.XStart := width - row.XStart;
            row.XStop := width - row.XStop;
          end;
          row.Y := y;
          rows.Add(row);
        end;
      end;
    end;
    if not view.SkipPair then
      break;
  end;
end;

{ TCode16KReader }

function TCode16KReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TCode16KReader.decode(const image: TBinaryBitmap;
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

procedure TCode16KReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var tryHarder := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.TRY_HARDER);
  // (the rows are at least 8 modules high)
  var rowStep := 4;
  if tryHarder then
    rowStep := 2;
  // horizontal symbols (the rows of the image as the 1D readers have them),
  // with TRY_HARDER also vertical ones in the image turned
  for var vertical in [false, true] do
  begin
    if vertical and not tryHarder then
      break;
    var matrix := image.BlackMatrix;
    if vertical then
      matrix := Transposed(matrix);
    var rows := TList<TRowRead>.Create;
    try
      var runs: TPatternRow;
      var y := 0;
      while (y < matrix.Height) do
      begin
        // left to right, and right to left (upside down)
        for var reversed in [false, true] do
        begin
          if vertical then
          begin
            var forward: TPatternRow;
            GetPatternRow(matrix, y, forward);
            runs := forward;
            if reversed then
            begin
              runs := nil;
              SetLength(runs, Length(forward));
              for var i := 0 to High(forward) do
                runs[High(forward) - i] := forward[i];
            end;
          end
          else
            runs := image.getPatternRow(y, reversed);
          if (runs <> nil) then
            ReadRows(runs, y, matrix.Width, reversed, rows);
        end;
        Inc(y, rowStep);
      end;

      // the symbols: from each first row the other rows at about the same
      // place, the nearest one of each number
      for var first in rows do
      begin
        if (first.Index <> 0) or ResultsFull(results, maxCount) then
          continue;
        var n := first.Values[0] div 7 + 2;
        if (n > 16) then
          continue;
        var values := first.Values;
        var tolerance := Abs(first.XStop - first.XStart) / 20;
        var minY := first.Y;
        var maxY := first.Y;
        var complete := true;
        for var k := 1 to n - 1 do
        begin
          var best := -1;
          var bestDistance := MaxInt;
          for var i := 0 to rows.Count - 1 do
          begin
            var row := rows[i];
            if (row.Index = k) and
              (Abs(row.XStart - first.XStart) <= tolerance) and
              (Abs(row.XStop - first.XStop) <= tolerance) and
              (Abs(row.Y - first.Y) < bestDistance) then
            begin
              best := i;
              bestDistance := Abs(row.Y - first.Y);
            end;
          end;
          if (best < 0) then
          begin
            complete := false;
            break;
          end;
          values := values + rows[best].Values;
          minY := Min(minY, rows[best].Y);
          maxY := Max(maxY, rows[best].Y);
        end;
        if not complete then
          continue;
        var fnc1First: Boolean;
        var text := DecodeCode16K(values, fnc1First);
        if (text = '') then
          continue;
        var x1 := Min(first.XStart, first.XStop);
        var x2 := Max(first.XStart, first.XStop);
        var points: TArray<IResultPoint>;
        if vertical then
          points := [TResultPointHelpers.CreateResultPoint(minY, x1),
            TResultPointHelpers.CreateResultPoint(maxY, x1),
            TResultPointHelpers.CreateResultPoint(maxY, x2),
            TResultPointHelpers.CreateResultPoint(minY, x2)]
        else
          points := [TResultPointHelpers.CreateResultPoint(x1, minY),
            TResultPointHelpers.CreateResultPoint(x2, minY),
            TResultPointHelpers.CreateResultPoint(x2, maxY),
            TResultPointHelpers.CreateResultPoint(x1, maxY)];
        var r := TReadResult.Create(text, nil, points,
          TBarcodeFormat.CODE_16K);
        // ISO/IEC 15424: ]K0 Code 16K, ]K1 with FNC1 first (GS1)
        if fnc1First then
          r.SymbologyIdentifier := ']K1'
        else
          r.SymbologyIdentifier := ']K0';
        if ContainsResult(results, r) then
          r.Free
        else
          results.Add(r);
      end;
    finally
      rows.Free;
      if vertical then
        matrix.Free;
    end;
  end;
end;

procedure TCode16KReader.reset;
begin
  // do nothing
end;

end.
