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

  * The CC-A 2D component of GS1 Composite symbols for ZXing.Delphi:
  * zxing-cpp, ZXing Java and ZXing.Net have no reader. The structure and the
  * tables as in zint (composite.c, composite.h, after ISO/IEC 24723).
}

unit ZXing.Composite.CCA;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.Common.BitMatrix;

/// <summary>The GS1 text of a CC-A component in the area of the image
/// (left, top to right, bottom; rows of 2, 3 or 4 codewords between row
/// address patterns, also upside down); '' when there is none.</summary>
function ReadCCA(image: TBitMatrix; left, top, right, bottom: Integer): string;

/// <summary>The bits of the data codewords of a CC-A (base 928, 7 codewords
/// per 69 bits) of cols columns; nil when they are none.</summary>
function CCABits(const codewords: TArray<Integer>; cols: Integer)
  : TArray<Boolean>;

implementation

uses
  ZXing.Common.Pattern,
  ZXing.PDF417.Internal.CodewordDecoder,
  ZXing.PDF417.Internal.ErrorCorrection,
  ZXing.PDF417.MicroPDF417Reader,
  ZXing.OneD.DataBarExpandedBitDecoder;

const
  // Table 9: the rows, the error correction codewords
  VARIANT_ROWS: array [0 .. 16] of Integer = (5, 6, 7, 8, 9, 10, 12, 4, 5, 6,
    7, 8, 3, 4, 5, 6, 7);
  VARIANT_ECCS: array [0 .. 16] of Integer = (4, 4, 5, 5, 6, 6, 7, 4, 5, 6, 7,
    7, 4, 5, 6, 7, 8);
  VARIANT_COLS: array [0 .. 16] of Integer = (2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3,
    3, 4, 4, 4, 4, 4);
  // Tables 10 and 11: the first left, center and right row address
  // patterns (1 to 52) and the first cluster (/ 3)
  VARIANT_LEFT: array [0 .. 16] of Integer = (39, 1, 32, 8, 14, 43, 20, 11, 1,
    5, 15, 21, 40, 43, 46, 34, 29);
  VARIANT_CENTER: array [0 .. 16] of Integer = (0, 0, 0, 0, 0, 0, 0, 43, 33, 37,
    47, 1, 20, 23, 26, 14, 9);
  VARIANT_RIGHT: array [0 .. 16] of Integer = (19, 33, 12, 40, 46, 23, 52, 23,
    13, 17, 27, 33, 52, 3, 6, 46, 41);
  VARIANT_CLUSTER: array [0 .. 16] of Integer = (2, 0, 1, 1, 1, 0, 1, 1, 0, 1,
    2, 2, 0, 0, 0, 0, 1);
  // the number of bits of the data of the numbers of data codewords, by
  // columns
  BIT_LENGTHS: array [2 .. 4, 0 .. 6, 0 .. 1] of Integer = (((6, 59), (8, 78),
    (9, 88), (11, 108), (12, 118), (14, 138), (17, 167)), ((8, 78), (10, 98),
    (12, 118), (14, 138), (17, 167), (0, 0), (0, 0)), ((8, 78), (11, 108),
    (14, 138), (17, 167), (20, 197), (0, 0), (0, 0)));

type
  TCCARow = record
    Cols, Left, Center, Right, Cluster: Integer;
    Codewords: TArray<Integer>;
  end;

function CCABits(const codewords: TArray<Integer>; cols: Integer)
  : TArray<Boolean>;
begin
  Result := nil;
  if (cols < 2) or (cols > 4) then
    exit;
  var bitLength := 0;
  for var i := 0 to 6 do
    if (BIT_LENGTHS[cols, i, 0] = Length(codewords)) then
      bitLength := BIT_LENGTHS[cols, i, 1];
  if (bitLength = 0) then
    exit;
  SetLength(Result, bitLength);
  // groups of 69 bits in 7 codewords (the last one: bits div 10 + 1)
  var cw := 0;
  var b := 0;
  while (b < bitLength) do
  begin
    var bitCount := Min(69, bitLength - b);
    var count := bitCount div 10 + 1;
    var digits: TArray<Integer> := Copy(codewords, cw, count);
    // the bits from the lowest: the base 928 number divided by 2
    for var i := bitCount - 1 downto 0 do
    begin
      var remainder := 0;
      for var k := 0 to count - 1 do
      begin
        var v := remainder * 928 + digits[k];
        digits[k] := v div 2;
        remainder := v mod 2;
      end;
      Result[b + i] := (remainder = 1);
    end;
    // (more than the bits: not)
    for var d in digits do
      if (d <> 0) then
        exit(nil);
    Inc(cw, count);
    Inc(b, bitCount);
  end;
end;

/// <summary>The codeword (and its cluster) of 8 widths of 17 modules; -1
/// when it is none.</summary>
function ReadCodeword(const widths: array of Integer;
  out cluster: Integer): Integer;
begin
  var sum := 0;
  for var w in widths do
    Inc(sum, w);
  var module := sum / 17;
  var e2e: TCodewordE2E;
  for var i := 0 to 5 do
    e2e[i] := Round((widths[i] + widths[i + 1]) / module);
  Result := GetCodewordFromE2E(e2e);
  cluster := CodewordClusterE2E(e2e);
end;

/// <summary>The row of a CC-A of cols columns from view (a bar): its row
/// address patterns, cluster and codewords; false when it is none.</summary>
function ReadRow(const view: TPatternView; cols: Integer;
  out row: TCCARow): Boolean;
begin
  Result := false;
  // the blocks: L (6), codewords (8), C (6), R (6), the stop bar
  var blocks: string;
  case cols of
    2:
      blocks := 'LWWR';
    3:
      blocks := 'WCWWR';
  else
    blocks := 'LWWCWWR';
  end;
  var elements := 1;
  var modules := 1;
  for var c in blocks do
    if (c = 'W') then
    begin
      Inc(elements, 8);
      Inc(modules, 17);
    end
    else
    begin
      Inc(elements, 6);
      Inc(modules, 10);
    end;
  if not view.IsValid(elements + 1) then
    exit;
  var module: Double := view.Sum(elements) / modules;
  // the stop bar of 1 module, a space in front and behind it
  if (Abs(view[elements - 1] - module) > 0.6 * module + 0.5) or
    not view.IsAtFirstBar and (view.SpaceInFront < module) or
    (view[elements] < module) then
    exit;
  row.Cols := cols;
  row.Left := 0;
  row.Center := 0;
  row.Right := 0;
  row.Cluster := -1;
  row.Codewords := [];
  var e := 0;
  for var c in blocks do
  begin
    var size := 6;
    var blockModules := 10;
    if (c = 'W') then
    begin
      size := 8;
      blockModules := 17;
    end;
    var widths: TArray<Integer>;
    SetLength(widths, size);
    var sum := 0;
    for var i := 0 to size - 1 do
    begin
      widths[i] := view[e + i];
      Inc(sum, widths[i]);
    end;
    if (Abs(sum - blockModules * module) > 1.5 * module + 1) then
      exit;
    if (c = 'W') then
    begin
      var cluster: Integer;
      var codeword := ReadCodeword(widths, cluster);
      if (codeword < 0) or (row.Cluster >= 0) and (cluster <> row.Cluster) then
        exit;
      row.Cluster := cluster;
      row.Codewords := row.Codewords + [codeword];
    end
    else
    begin
      var index := MicroPDF417RAPIndex(widths, c = 'C');
      if (index = 0) then
        exit;
      case c of
        'L':
          row.Left := index;
        'C':
          row.Center := index;
        'R':
          row.Right := index;
      end;
    end;
    Inc(e, size);
  end;
  Result := true;
end;

function ReadCCA(image: TBitMatrix; left, top, right, bottom: Integer): string;
begin
  Result := '';
  left := Max(left, 0);
  top := Max(top, 0);
  right := Min(right, image.Width - 1);
  bottom := Min(bottom, image.Height - 1);
  if (right - left < 55) or (bottom < top) then
    exit;
  // the rows read, then for each variant the codewords voted per row and
  // column (codeword + 1000 * count)
  var rows := TList<TCCARow>.Create;
  try
    var runs: TPatternRow;
    var bits := TBitMatrix.Create(right - left + 1, 1);
    try
      for var y := top to bottom do
      begin
        for var x := left to right do
          bits[x - left, 0] := image[x, y];
        GetPatternRow(bits, 0, runs);
        // left to right, and right to left (upside down)
        for var reversed in [false, true] do
        begin
          var row := runs;
          if reversed then
          begin
            row := nil;
            SetLength(row, Length(runs));
            for var i := 0 to High(runs) do
              row[High(runs) - i] := runs[i];
          end;
          var view := TPatternView.Create(row);
          while view.IsValid and (view.Size > 29) do
          begin
            for var cols := 2 to 4 do
            begin
              var ccaRow: TCCARow;
              if ReadRow(view, cols, ccaRow) then
                rows.Add(ccaRow);
            end;
            if not view.SkipPair then
              break;
          end;
        end;
      end;
    finally
      bits.Free;
    end;

    for var v := 0 to 16 do
    begin
      var cols := VARIANT_COLS[v];
      var n := VARIANT_ROWS[v];
      // the votes per row and column
      var votes := TDictionary<Integer, Integer>.Create;
      try
        for var row in rows do
        begin
          if (row.Cols <> cols) then
            continue;
          var r := (row.Right - VARIANT_RIGHT[v] + 52) mod 52;
          if (r >= n) or (row.Cluster <> (VARIANT_CLUSTER[v] + r) mod 3 * 3) then
            continue;
          if (cols <> 3) and (row.Left <> (VARIANT_LEFT[v] - 1 + r) mod 52 + 1)
          then
            continue;
          if (cols >= 3) and (row.Center <> (VARIANT_CENTER[v] - 1 + r) mod 52
            + 1) then
            continue;
          for var c := 0 to cols - 1 do
          begin
            var key := ((r * cols + c) shl 10) or row.Codewords[c];
            var count: Integer;
            if not votes.TryGetValue(key, count) then
              count := 0;
            votes.AddOrSetValue(key, count + 1);
          end;
        end;
        // the codeword seen most at each place
        var codewords: TArray<Integer>;
        SetLength(codewords, n * cols);
        var counts: TArray<Integer>;
        SetLength(counts, n * cols);
        for var i := 0 to n * cols - 1 do
        begin
          codewords[i] := 0;
          counts[i] := 0;
        end;
        for var pair in votes do
        begin
          var place := pair.Key shr 10;
          if (pair.Value > counts[place]) then
          begin
            counts[place] := pair.Value;
            codewords[place] := pair.Key and $3FF;
          end;
        end;
        // missing codewords as erasures
        var erasures: TArray<Integer> := [];
        for var i := 0 to n * cols - 1 do
          if (counts[i] = 0) then
            erasures := erasures + [i];
        if (Length(erasures) = n * cols) then
          continue;
        var used: Integer;
        if not PDF417ReedSolomonDecode(codewords, VARIANT_ECCS[v], erasures,
          used) then
          continue;
        var data := Copy(codewords, 0, n * cols - VARIANT_ECCS[v]);
        var dataBits := CCABits(data, cols);
        if (dataBits = nil) then
          continue;
        Result := DecodeCompositeBits(dataBits);
        if (Result <> '') then
          exit;
      finally
        votes.Free;
      end;
    end;
  finally
    rows.Free;
  end;
end;

end.
