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

  * The compact dark blobs of a binary image (the dots of DotCode, the
  * finder pattern cores of Han Xin Code).
}

unit ZXing.Common.Blobs;

interface

uses
  ZXing.Common.BitMatrix;

type
  /// <summary>A compact dark blob: its centre, size (the larger side of its
  /// bounding box) and number of pixels.</summary>
  TBlob = record
    X, Y: Double;
    Size, Count: Integer;
  end;

/// <summary>The compact blobs (about round or square, about filled, sides
/// of at most maxSize) of the image.</summary>
function FindBlobs(image: TBitMatrix; maxSize: Integer): TArray<TBlob>;

implementation

uses
  System.Math,
  ZXing.Common.Pattern;

/// <summary>The blobs (connected dark areas) of the image: from the runs of
/// its rows, joined where they touch.</summary>
function FindBlobs(image: TBitMatrix; maxSize: Integer): TArray<TBlob>;
var
  parents: TArray<Integer>;

  function Find(i: Integer): Integer;
  begin
    Result := i;
    while (parents[Result] <> Result) do
    begin
      parents[Result] := parents[parents[Result]];
      Result := parents[Result];
    end;
  end;

begin
  Result := [];
  // the runs: left, right, row
  var lefts, rights, rows: TArray<Integer>;
  var count := 0;
  var previousFirst := 0;
  var previousLast := -1;
  var runs: TPatternRow;
  for var y := 0 to image.Height - 1 do
  begin
    GetPatternRow(image, y, runs);
    var first := count;
    var x := runs[0];
    var i := 1;
    var p := previousFirst;
    while (i < Length(runs)) do
    begin
      var w := runs[i];
      if (w > 0) and (w <= maxSize) then
      begin
        if (count >= Length(lefts)) then
        begin
          SetLength(lefts, 2 * count + 64);
          SetLength(rights, 2 * count + 64);
          SetLength(rows, 2 * count + 64);
          SetLength(parents, 2 * count + 64);
        end;
        lefts[count] := x;
        rights[count] := x + w - 1;
        rows[count] := y;
        parents[count] := count;
        // joined with the runs of the row before that touch it (8-connected;
        // the runs from left to right)
        while (p <= previousLast) and (rights[p] < x - 1) do
          Inc(p);
        var q := p;
        while (q <= previousLast) and (lefts[q] <= x + w) do
        begin
          var a := Find(q);
          var b := Find(count);
          if (a <> b) then
            parents[b] := a;
          Inc(q);
        end;
        Inc(count);
      end;
      Inc(x, w);
      if (i + 1 < Length(runs)) then
        Inc(x, runs[i + 1]);
      Inc(i, 2);
    end;
    previousFirst := first;
    previousLast := count - 1;
  end;

  // the blobs: the sums of their runs
  var index: TArray<Integer>;
  SetLength(index, count);
  var sumX, sumY: TArray<Double>;
  var areas, minX, maxX, minY, maxY: TArray<Integer>;
  var n := 0;
  for var k := 0 to count - 1 do
  begin
    var root := Find(k);
    if (root = k) then
    begin
      index[k] := n;
      Inc(n);
    end;
  end;
  SetLength(sumX, n);
  SetLength(sumY, n);
  SetLength(areas, n);
  SetLength(minX, n);
  SetLength(maxX, n);
  SetLength(minY, n);
  SetLength(maxY, n);
  for var b := 0 to n - 1 do
  begin
    sumX[b] := 0;
    sumY[b] := 0;
    areas[b] := 0;
    minX[b] := MaxInt;
    maxX[b] := -1;
    minY[b] := MaxInt;
    maxY[b] := -1;
  end;
  for var k := 0 to count - 1 do
  begin
    var b := index[Find(k)];
    var len := rights[k] - lefts[k] + 1;
    sumX[b] := sumX[b] + len * (lefts[k] + rights[k] + 1) / 2;
    sumY[b] := sumY[b] + len * (rows[k] + 0.5);
    Inc(areas[b], len);
    minX[b] := Min(minX[b], lefts[k]);
    maxX[b] := Max(maxX[b], rights[k]);
    minY[b] := Min(minY[b], rows[k]);
    maxY[b] := Max(maxY[b], rows[k]);
  end;
  SetLength(Result, n);
  var found := 0;
  // compact blobs only (a dot: about round, about filled)
  for var b := 0 to n - 1 do
  begin
    var w := maxX[b] - minX[b] + 1;
    var h := maxY[b] - minY[b] + 1;
    if (w > maxSize) or (h > maxSize) or (w > 2 * h + 1) or (h > 2 * w + 1)
      or (areas[b] < 0.4 * w * h) then
      continue;
    var blob: TBlob;
    blob.X := sumX[b] / areas[b];
    blob.Y := sumY[b] / areas[b];
    blob.Size := Max(w, h);
    blob.Count := areas[b];
    Result[found] := blob;
    Inc(found);
  end;
  SetLength(Result, found);
end;


end.
