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

  * Codablock F for ZXing.Delphi: zxing-cpp, ZXing Java and ZXing.Net have no
  * reader. The encoding as in zint (codablock.c, after AIM ISS-X-24).
}

unit ZXing.Stacked.CodablockFReader;

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
  /// Reads Codablock F: 2 to 44 rows of Code 128 characters, each row with
  /// its number and its own checksum, the first one with the number of
  /// rows, the last one with the check characters K1 and K2 of the whole
  /// message. Horizontal or vertical, also upside down.
  /// </summary>
  TCodablockFReader = class(TInterfacedObject, IReader, IMultipleReader)
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>The text of the codes of the rows of a Codablock F (each from
/// its start code to its checksum, in the order of their row numbers);
/// '' when they are none (row numbers, K1 and K2).</summary>
function DecodeCodablockF(const rows: TArray<TArray<Integer>>): string;

implementation

uses
  ZXing.ResultPoint,
  ZXing.Common.Pattern,
  ZXing.OneD.Code128Reader,
  ZXing.Postal.FourStateDetector;

const
  CODE_SHIFT = 98;
  CODE_CODE_C = 99;
  CODE_CODE_B = 100;
  CODE_CODE_A = 101;
  CODE_START_A = 103;
  CODE_START_B = 104;
  CODE_START_C = 105;
  CODE_STOP = 106;
  CHAR_LEN = 6;
  // the specification has 10, as Code 128
  QUIET_ZONE = 5;
  // start, code set, row indicator, at least 4 data characters, checksum
  MIN_CODES = 8;

type
  TCodeSet = (csA, csB, csC);

  TRowRead = record
    // the codes from the start code to the checksum
    Codes: TArray<Integer>;
    // the row number (0 the first), and for the first one the number of
    // rows
    Index, Count: Integer;
    // the middle of the start and the stop code, on the row y
    XStart, XStop: Double;
    Y: Integer;
  end;

/// <summary>The value of a row indicator or check character in code set
/// (Tables D.2, D.3 and F.1 of the specification, as zint); -1 when it is
/// none.</summary>
function IndicatorValue(code: Integer; codeSet: TCodeSet): Integer;
begin
  if (codeSet = csC) then
  begin
    if (code < 100) then
      exit(code);
    exit(-1);
  end;
  // A and B the same: 0-31 as 96-127, 32-47 as themselves, 48- as 58-
  if (code >= 64) and (code <= 95) then
    Result := code - 64
  else if (code <= 15) then
    Result := code + 32
  else if (code >= 26) and (code <= 63) then
    Result := code + 22
  else
    Result := -1;
end;

/// <summary>The code set of a row from its second code; false when it is
/// none.</summary>
function RowCodeSet(code: Integer; out codeSet: TCodeSet): Boolean;
begin
  Result := true;
  case code of
    CODE_SHIFT:
      codeSet := csA;
    CODE_CODE_B:
      codeSet := csB;
    CODE_CODE_C:
      codeSet := csC;
  else
    Result := false;
  end;
end;

/// <summary>The code set after code in codeSet (a change of code set, a
/// shift only for the next code: not).</summary>
function NextCodeSet(code: Integer; codeSet: TCodeSet): TCodeSet;
begin
  Result := codeSet;
  case codeSet of
    csA:
      if (code = CODE_CODE_B) then
        Result := csB
      else if (code = CODE_CODE_C) then
        Result := csC;
    csB:
      if (code = CODE_CODE_A) then
        Result := csA
      else if (code = CODE_CODE_C) then
        Result := csC;
    csC:
      if (code = CODE_CODE_A) then
        Result := csA
      else if (code = CODE_CODE_B) then
        Result := csB;
  end;
end;

function DecodeCodablockF(const rows: TArray<TArray<Integer>>): string;
const
  START_CODES: array [TCodeSet] of Integer = (CODE_START_A, CODE_START_B,
    CODE_START_C);
begin
  Result := '';
  var n := Length(rows);
  if (n < 2) then
    exit;
  var columns := Length(rows[0]);
  var text := '';
  var k1 := -1;
  var k2 := -1;
  for var r := 0 to n - 1 do
  begin
    var codes := rows[r];
    var codeSet: TCodeSet;
    if (Length(codes) <> columns) or (codes[0] <> CODE_START_A) or
      not RowCodeSet(codes[1], codeSet) then
      exit;
    // the row indicator: the number of rows (the first), the row number
    var indicator := IndicatorValue(codes[2], codeSet);
    if (r = 0) and (indicator <> n - 2) or (r > 0) and (indicator <> r + 42)
    then
      exit;
    // the data (in the last row followed by K1 and K2), then the checksum
    var last := High(codes) - 1;
    if (r = n - 1) then
      Dec(last, 2);
    var data: TArray<Integer> := [START_CODES[codeSet]];
    var current := codeSet;
    var shifted := false;
    for var i := 3 to last do
    begin
      data := data + [codes[i]];
      if shifted then
        shifted := false
      else if (current <> csC) and (codes[i] = CODE_SHIFT) then
        shifted := true
      else
        current := NextCodeSet(codes[i], current);
    end;
    var fnc1First: Boolean;
    text := text + TCode128Reader.CodesToText(data, false, fnc1First);
    if (r = n - 1) then
    begin
      k1 := IndicatorValue(codes[last + 1], current);
      k2 := IndicatorValue(codes[last + 2], current);
    end;
  end;

  // the data check characters K1 and K2 (Annex F): modulo 86
  var sum1 := 0;
  var sum2 := 0;
  for var i := 1 to Length(text) do
  begin
    var c := Ord(text[i]);
    if (c > 255) then
      exit;
    sum1 := (sum1 + i * c) mod 86;
    sum2 := (sum2 + (i - 1) * c) mod 86;
  end;
  if (k1 <> sum1) or (k2 <> sum2) or (text = '') then
    exit;
  Result := text;
end;

/// <summary>The Code 128 rows of a Codablock F in the runs of row y (start
/// A, a code set, a row indicator, data, a checksum and the stop), with
/// width the width of the row (for x of the reversed runs).</summary>
procedure ReadRows(const runs: TPatternRow; y, width: Integer;
  reversed: Boolean; rows: TList<TRowRead>);
begin
  var view := TPatternView.Create(runs);
  while view.IsValid and (view.Size > MIN_CODES * CHAR_LEN) do
  begin
    var next := FindLeftGuard(view, MIN_CODES * CHAR_LEN, [2, 1, 1],
      QUIET_ZONE);
    if not next.IsValid then
      exit;
    // the next try behind this start
    view := next;
    view.Shift(2);
    view.Extend;

    next := next.SubView(0, CHAR_LEN);
    if (TCode128Reader.DecodeCodeOf(next, true) <> CODE_START_A) then
      continue;
    var startView := next;
    var codes: TArray<Integer> := [CODE_START_A];
    var ok := true;
    repeat
      if not next.SkipSymbol then
      begin
        ok := false;
        break;
      end;
      var code := TCode128Reader.DecodeCodeOf(next, false);
      if (code = -1) or (code > CODE_STOP) or
        (code >= CODE_START_A) and (code < CODE_STOP) then
      begin
        ok := false;
        break;
      end;
      if (code = CODE_STOP) then
        break;
      codes := codes + [code];
    until false;
    if not ok or (Length(codes) < MIN_CODES) then
      continue;
    // the termination bar and the quiet zone
    var stopView := next;
    next := next.SubView(0, CHAR_LEN + 1);
    if not next.IsValid or (next[CHAR_LEN] > next.Sum(CHAR_LEN) div 4) or
      not next.HasQuietZoneAfter(QUIET_ZONE / 13) then
      continue;
    // the checksum
    var checksum := codes[0];
    for var i := 1 to High(codes) - 1 do
      Inc(checksum, i * codes[i]);
    if (checksum mod 103 <> codes[High(codes)]) then
      continue;
    var codeSet: TCodeSet;
    if not RowCodeSet(codes[1], codeSet) then
      continue;
    var indicator := IndicatorValue(codes[2], codeSet);
    var row: TRowRead;
    row.Codes := codes;
    row.Count := 0;
    if (indicator >= 0) and (indicator <= 42) then
    begin
      row.Index := 0;
      row.Count := indicator + 2;
    end
    else if (indicator >= 43) and (indicator <= 85) then
      row.Index := indicator - 42
    else
      continue;
    row.XStart := startView.PixelsInFront + startView.Sum / 2;
    row.XStop := stopView.PixelsInFront + stopView.Sum / 2;
    if reversed then
    begin
      row.XStart := width - row.XStart;
      row.XStop := width - row.XStop;
    end;
    row.Y := y;
    rows.Add(row);
    // behind this row
    view := stopView;
    view.Extend;
  end;
end;

{ TCodablockFReader }

function TCodablockFReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TCodablockFReader.decode(const image: TBinaryBitmap;
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

procedure TCodablockFReader.decodeMultiple(const image: TBinaryBitmap;
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

      // the symbols: from each first row the other rows with the same codes
      // count and about the same place, the nearest one of each number
      for var first in rows do
      begin
        if (first.Index <> 0) or ResultsFull(results, maxCount) then
          continue;
        var n := first.Count;
        var columns := Length(first.Codes);
        var symbolRows: TArray<TArray<Integer>>;
        SetLength(symbolRows, n);
        symbolRows[0] := first.Codes;
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
            if (row.Index = k) and (Length(row.Codes) = columns) and
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
          symbolRows[k] := rows[best].Codes;
          minY := Min(minY, rows[best].Y);
          maxY := Max(maxY, rows[best].Y);
        end;
        if not complete then
          continue;
        var text := DecodeCodablockF(symbolRows);
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
          TBarcodeFormat.CODABLOCK_F);
        // ISO/IEC 15424: ]O4 Codablock F
        r.SymbologyIdentifier := ']O4';
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

procedure TCodablockFReader.reset;
begin
  // do nothing
end;

end.
