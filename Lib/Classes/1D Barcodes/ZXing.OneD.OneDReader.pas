{
  * Copyright 2007 ZXing authors
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

  * Original Authors: dswitkin@google.com (Daniel Switkin) and Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.OneD.OneDReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Math,
  ZXing.Reader,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.Common.Detector.MathUtils;

type
  TOneDPattern  = array of Integer;
  TOneDPatterns = array of TOneDPattern;

  /// <summary>
  /// Encapsulates functionality and implementation that is common to all families
  /// of one-dimensional barcodes.
  /// </summary>
  TOneDReader = class(TInterfacedObject, IReader, IMultipleReader)
  private
    /// <summary>
    /// We're going to examine rows from the middle outward, searching alternately above and below the
    /// middle, and farther out each time. rowStep is the number of rows between each successive
    /// attempt above and below the middle. So we'd scan row middle, then middle - rowStep, then
    /// middle + rowStep, then middle - (2 * rowStep), etc.
    /// rowStep is bigger as the image is taller, but is always at least 1. We've somewhat arbitrarily
    /// decided that moving up and down by about 1/16 of the image is pretty good; we try more of the
    /// image if "trying harder".
    /// </summary>
    /// <param name="image">The image to decode</param>
    /// <param name="hints">Any hints that were requested</param>
    /// <returns>The contents of the decoded barcode</returns>
    /// <summary>Scans the rows from the middle out and returns the first
    /// barcode; with a collector all (new) barcodes that are read on at
    /// least 2 rows are added to it instead, up to maxCount results (0: no
    /// limit), and the result is nil. pending holds (and keeps, for the
    /// caller to free) the ones read on 1 row so far.</summary>
    function doDecode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      collector: TList<TReadResult> = nil; maxCount: Integer = 0;
      pending: TList<TReadResult> = nil): TReadResult;
    /// <summary>Adds r to collector when it is read on a second row, keeps
    /// it in pending otherwise, frees it when it is already known. Returns
    /// true when collector is full.</summary>
    function CollectResult(r: TReadResult;
      collector, pending: TList<TReadResult>; maxCount: Integer): Boolean;
  protected
    const INTEGER_MATH_SHIFT = 8;

    /// <summary>
    /// Determines how closely a set of observed counts of runs of black/white values matches a given
    /// target pattern. This is reported as the ratio of the total variance from the expected pattern
    /// proportions across all pattern elements, to the length of the pattern.
    /// </summary>
    /// <param name="counters">observed counters</param>
    /// <param name="pattern">expected pattern</param>
    /// <param name="maxIndividualVariance">The most any counter can differ before we give up</param>
    /// <returns>ratio of total variance between counters and pattern compared to total pattern size,
    /// where the ratio has been multiplied by 256. So, 0 means no variance (perfect match); 256 means
    /// the total variance between counters and patterns equals the pattern length, higher values mean
    /// even more variance</returns>
    class function patternMatchVariance(counters: TArray<Integer>; pattern: TOneDPattern;
      maxIndividualVariance: Integer): Integer;

    /// <summary>
    /// Records the pattern in reverse.
    /// </summary>
    /// <param name="row">The row.</param>
    /// <param name="start">The start.</param>
    /// <param name="counters">The counters.</param>
    /// <returns></returns>
    function RecordPatternInReverse(row: IBitArray; start: Integer;
      counters: TArray<Integer>): Boolean;
  public
    /// <summary>
    /// Resets any internal state the implementation has after a decode, to prepare it
    /// for reuse.
    /// </summary>
    procedure reset(); virtual;
    /// <summary>
    /// Locates and decodes a barcode in some format within an image.
    /// </summary>
    /// <param name="image">image of barcode to decode</param>
    /// <returns>
    /// String which the barcode encodes
    /// </returns>
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    /// <summary>
    /// Locates and decodes a barcode in some format within an image. This method also accepts
    /// hints, each possibly associated to some data, which may help the implementation decode.
    /// Note that we don't try rotation without the try harder flag, even if rotation was supported.
    /// </summary>
    /// <param name="image">image of barcode to decode</param>
    /// <param name="hints">passed as a <see cref="IDictionary{TKey, TValue}"/> from <see cref="DecodeHintType"/>
    /// to arbitrary data. The
    /// meaning of the data depends upon the hint type. The implementation may or may not do
    /// anything with these hints.</param>
    /// <returns>
    /// String which the barcode encodes
    /// </returns>
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      overload; virtual;
    /// <summary>All barcodes of this format on different rows of the image,
    /// see IMultipleReader.</summary>
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);

    /// <summary>
    /// Records the size of successive runs of white and black pixels in a row, starting at a given point.
    /// The values are recorded in the given array, and the number of runs recorded is equal to the size
    /// of the array. If the row starts on a white pixel at the given start point, then the first count
    /// recorded is the run of white pixels starting from that point; likewise it is the count of a run
    /// of black pixels if the row begin on a black pixels at that point.
    /// </summary>
    /// <param name="row">row to count from</param>
    /// <param name="start">offset into row to start at</param>
    /// <param name="counters">array into which to record counts</param>
    class function recordPattern(row: IBitArray; start: Integer;
      counters: TArray<Integer>): Boolean;

    /// <summary>
    /// Attempts to decode a one-dimensional barcode format given a single row of
    /// an image.
    /// </summary>
    /// <param name="rowNumber">row number from top of the row</param>
    /// <param name="row">the black/white pixel data of the row</param>
    /// <param name="hints">decode hints</param>
    /// <returns>
    /// <see cref="Result"/>containing encoded string and start/end of barcode
    /// </returns>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      virtual; abstract;
  protected
    /// <summary>
    /// The decoder of zxing-cpp (its RowReader.decodePattern): decodes the
    /// first symbol that starts at or behind the start of next, a view on
    /// the bars and spaces of the row. On return next is the view where
    /// decoding stopped. nil when no symbol is found. Only used when
    /// HasPatternDecoder, as a fallback for decodeRow.
    /// </summary>
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      virtual;
    function HasPatternDecoder: Boolean; virtual;

    /// <summary>
    /// The thresholds between narrow and wide bars and between narrow and
    /// wide spaces of view, for the codes with wide elements 2 to 3 times as
    /// wide as narrow ones (ITF, Code 39). Invalid when the widths do not
    /// fit.
    /// </summary>
    class function NarrowWideThreshold(const view: TPatternView)
      : TBarAndSpace; static;
    /// <summary>The bars and spaces of view as bits (wide = 1); -1 when
    /// they do not fit.</summary>
    class function NarrowWideBitPattern(const view: TPatternView)
      : Integer; static;
    /// <summary>
    /// How closely the first Length(pattern) bars and spaces of view match
    /// pattern: the total variance divided by the total width, a large
    /// value when one element differs more than maxIndividualVariance
    /// modules.
    /// </summary>
    class function PatternMatchVarianceF(const view: TPatternView;
      const pattern: TOneDPattern; maxIndividualVariance: Double)
      : Double; static;
    /// <summary>
    /// The index of the pattern that matches view best, with a variance
    /// below maxAvgVariance; -1 when none does (or, when
    /// requireUnambiguousMatch, when two match equally well).
    /// </summary>
    class function DecodeDigitF(const view: TPatternView;
      const patterns: TOneDPatterns; maxAvgVariance,
      maxIndividualVariance: Double; requireUnambiguousMatch: Boolean = true)
      : Integer; static;
  private
    FBars: TPatternRow;
    FTryHarder: Boolean;
    FUsePattern: Boolean;
    /// <summary>One pass of decodeMultiple, with decodePattern or with
    /// decodeRow (FUsePattern). Returns the first code read on 1 row only
    /// when no new code is read on 2 rows (nil otherwise).</summary>
    function decodeMultiplePass(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer): TReadResult;
    /// <summary>
    /// The barcodes decodePattern finds on row rowNumber, in both
    /// directions: from every bar with TRY_HARDER, else from the first
    /// guard and behind each barcode found. Without collector the first
    /// one is returned; else they are collected (see CollectResult), with
    /// the rows next to a new one in checkRows, and the result is nil.
    /// </summary>
    function decodePatternRow(const image: TBinaryBitmap; rowNumber: Integer;
      const hints: TDictionary<TDecodeHintType, TObject>;
      collector: TList<TReadResult>; maxCount: Integer;
      pending: TList<TReadResult>; isCheckRow: Boolean;
      var checkRows: TArray<Integer>): TReadResult;
  end;

implementation

procedure MapFromRotated(r: TReadResult; rotatedHeight: Integer); forward;

{ TOneDReader }

function TOneDReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TOneDReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  // Like zxing-cpp: a barcode counts when it is read on 2 rows, so that a
  // false positive does not stop the search. When there is none, the first
  // barcode read on 1 row (the result of the first row hit, as before).
  // With TRY_HARDER also the image rotated by 90 degrees.
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

/// <summary>For a result found in the image rotated by 90 degrees counter
/// clockwise (with height rotatedHeight): records the orientation and maps
/// the result points back to the image.</summary>
procedure MapFromRotated(r: TReadResult; rotatedHeight: Integer);
var
  metadata: TResultMetadata;
  orientation: Integer;
  points: TArray<IResultPoint>;
begin
  // Record that we found it rotated 90 degrees CCW / 270 degrees CW
  metadata := r.ResultMetadata;
  orientation := 270;
  if ((metadata <> nil) and metadata.ContainsKey
    (ZXing.ResultMetadataType.orientation)) then
  begin
    // But if we found it reversed in doDecode(), add in that result here:
    orientation :=
      (orientation + (metadata[ZXing.ResultMetadataType.orientation]
      as IIntegerMetadata).Value) mod 360;
  end;

  r.putMetadata(ZXing.ResultMetadataType.orientation,
    TResultMetadata.CreateIntegerMetadata(orientation));
  // Update result points
  points := r.ResultPoints;
  for var i := 0 to High(points) do
    if (points[i] <> nil) then
      points[i] := TResultPointHelpers.CreateResultPoint
        (rotatedHeight - points[i].Y - 1, points[i].X);
end;

/// <summary>The largest x of the result points of r.</summary>
function RightEdgeOf(r: TReadResult): Single;
begin
  Result := 0;
  for var p in r.ResultPoints do
    if (p <> nil) and (p.X > Result) then
      Result := p.X;
end;

/// <summary>A copy of row with all bits before start cleared (white).
/// </summary>
function RowFrom(const row: IBitArray; start: Integer): IBitArray;
begin
  Result := TBitArrayHelpers.CreateBitArray(row.Size);
  var bits := row.Bits;
  for var i := Max(start, 0) shr 5 to High(bits) do
  begin
    var word := bits[i];
    if (i = start shr 5) and ((start and $1F) <> 0) then
      // only the bits from start on
      word := word and not ((1 shl (start and $1F)) - 1);
    if (word <> 0) then
      Result.setBulk(i shl 5, word);
  end;
end;

/// <summary>Whether the lines of two results, read on different rows, can
/// be the same symbol, like zxing-cpp: the starts less than half the length
/// apart and the lengths less than 20% different (in either direction, as a
/// row can be read reversed).</summary>
function IsCloseLine(a, b: TReadResult): Boolean;

  function dist(const p, q: IResultPoint): Single;
  begin
    Result := Max(Abs(p.X - q.X), Abs(p.Y - q.Y));
  end;

begin
  var pa := a.ResultPoints;
  var pb := b.ResultPoints;
  if (Length(pa) < 2) or (Length(pb) < 2) or (pa[0] = nil) or
    (pa[High(pa)] = nil) or (pb[0] = nil) or (pb[High(pb)] = nil) then
    exit(true);
  var a0 := pa[0];
  var a1 := pa[High(pa)];
  var b0 := pb[0];
  var b1 := pb[High(pb)];
  var lenA := dist(a0, a1);
  var lenB := dist(b0, b1);
  var dStart := Min(dist(a0, b0), dist(a0, b1));
  Result := (dStart < lenA / 2) and (Abs(lenA - lenB) < lenA / 5);
end;

/// <summary>The length of the text of r, with its EAN/UPC add-on.</summary>
function CompleteLength(r: TReadResult): Integer;
begin
  Result := Length(r.Text);
  var meta: IMetaData;
  var ext: IStringMetadata;
  if (r.ResultMetaData <> nil) and r.ResultMetaData.TryGetValue
    (TResultMetadataType.UPC_EAN_EXTENSION, meta) and
    Supports(meta, IStringMetadata, ext) then
    Inc(Result, Length(ext.Value));
end;

function TOneDReader.CollectResult(r: TReadResult;
  collector, pending: TList<TReadResult>; maxCount: Integer): Boolean;
begin
  Result := false;
  // a new barcode counts when it is read on a second row too (like
  // zxing-cpp's minLineCount 2), which prevents most false positives
  if ContainsResult(collector, r) then
  begin
    r.Free;
    exit;
  end;

  // the same code at about the same place on another row
  var confirmed := -1;
  for var i := 0 to pending.Count - 1 do
    if (pending[i].BarcodeFormat = r.BarcodeFormat) and
      (pending[i].Text = r.Text) and IsCloseLine(pending[i], r) then
    begin
      confirmed := i;
      break;
    end;
  // else also when read with a text that contains the other one (e.g.
  // without its add-on)
  if (confirmed < 0) then
    for var i := 0 to pending.Count - 1 do
      if IsSameLinearSymbol(pending[i], r) and
        ((Pos(pending[i].Text, r.Text) > 0) or (Pos(r.Text, pending[i].Text) > 0))
      then
      begin
        confirmed := i;
        break;
      end;
  if (confirmed < 0) then
  begin
    pending.Add(r);
    exit;
  end;

  // keep the most complete reading
  var best := pending[confirmed];
  pending.Delete(confirmed);
  if (CompleteLength(r) > CompleteLength(best)) then
  begin
    best.Free;
    best := r;
  end
  else
    r.Free;
  collector.Add(best);
  Result := ResultsFull(collector, maxCount);
end;

procedure TOneDReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  if (image = nil) or ResultsFull(results, maxCount) then
    exit;
  var before := results.Count;
  var candidate: TReadResult := nil;
  var oldCandidate: TReadResult := nil;
  try
    // first the decoder of zxing-cpp (decodePattern), which is stricter
    if HasPatternDecoder then
    begin
      FUsePattern := true;
      candidate := decodeMultiplePass(image, hints, results, maxCount);
      if (results.Count > before) then
        exit;
      // a pure barcode or an image of one row: one row is enough (like
      // zxing-cpp)
      if (candidate <> nil) and ((image.Height < 2) or (hints <> nil) and
        hints.ContainsKey(ZXing.DecodeHintType.PURE_BARCODE)) then
      begin
        results.Add(candidate);
        candidate := nil;
        exit;
      end;
    end;

    // nothing read on 2 rows: the decoder of before (decodeRow)
    FUsePattern := false;
    oldCandidate := decodeMultiplePass(image, hints, results, maxCount);
    if (results.Count > before) then
      exit;

    // still nothing read on 2 rows: the first code read on one row only by
    // decodeRow, the result of before (zxing-cpp has none)
    if (oldCandidate <> nil) and not ContainsResult(results, oldCandidate) then
    begin
      results.Add(oldCandidate);
      oldCandidate := nil;
    end;
  finally
    candidate.Free;
    oldCandidate.Free;
  end;
end;

function TOneDReader.decodeMultiplePass(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer): TReadResult;
begin
  Result := nil;
  var before := results.Count;
  var rotated: TBinaryBitmap := nil;
  // the codes read on one row only (not counted, see doDecode)
  var pending := TList<TReadResult>.Create;
  var rotatedPending := TList<TReadResult>.Create;
  try
    // every row, also after a barcode was found
    doDecode(image, hints, results, maxCount, pending);

    // with TRY_HARDER also the image rotated by 90 degrees, for vertical
    // codes, also when horizontal ones were found
    var rotate := (hints <> nil) and
      hints.ContainsKey(ZXing.DecodeHintType.TRY_HARDER) and
      not hints.ContainsKey(ZXing.DecodeHintType.TRY_HARDER_WITHOUT_ROTATION);
    // (decode with decodeRow, maxCount 1: only when nothing was read, as
    // before)
    if (maxCount = 1) and not FUsePattern and (pending.Count > 0) then
      rotate := false;
    if rotate and image.RotateSupported and not ResultsFull(results, maxCount)
    then
    begin
      rotated := image.rotateCounterClockwise;
      var rotatedResults := TList<TReadResult>.Create;
      try
        // stop as soon as the rest is found (the results are not in
        // results yet)
        var remaining := 0;
        if (maxCount > 0) then
          remaining := maxCount - results.Count;
        doDecode(rotated, hints, rotatedResults, remaining, rotatedPending);
        for var r in rotatedResults do
        begin
          MapFromRotated(r, rotated.height);
          if ResultsFull(results, maxCount) or ContainsResult(results, r) then
            r.Free
          else
            results.Add(r);
        end;
      finally
        rotatedResults.Free;
      end;
    end;

    // nothing new: the first code read on one row only (the first row hit)
    if (results.Count = before) then
    begin
      if (pending.Count > 0) then
        Result := pending.Extract(pending[0])
      else if (rotatedPending.Count > 0) then
      begin
        Result := rotatedPending.Extract(rotatedPending[0]);
        MapFromRotated(Result, rotated.height);
      end;
    end;
  finally
    for var r in pending do
      r.Free;
    for var r in rotatedPending do
      r.Free;
    pending.Free;
    rotatedPending.Free;
    rotated.Free;
  end;
end;

function TOneDReader.doDecode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>;
  collector: TList<TReadResult>; maxCount: Integer;
  pending: TList<TReadResult>): TReadResult;

var
  attempt, X, rowNumber, rowStepsAboveOrBelow, width, height, middle, rowStep,
    maxLines: Integer;
  row: IBitArray;
  isAbove, hadResultPointCallBack: Boolean;
  ReadResult: TReadResult;
  points: TArray<IResultPoint>;
  obj: TObject;
  needsCallBack : boolean;
begin
  needsCallBack := (hints <> nil) and (hints.ContainsKey(ZXing.DecodeHintType.NEED_RESULT_POINT_CALLBACK));
  FTryHarder := (hints <> nil) and hints.ContainsKey(ZXing.DecodeHintType.TRY_HARDER);
  width := image.width;
  height := image.height;
  row := TBitArrayHelpers.CreateBitArray(width);
  // row is a interfaced object: we don't need to free it explicitly

  middle := TMathUtils.Asr(height, 1);

  // every 5th row; with TRY_HARDER every height/256th row like zxing-cpp
  // (every row of a small image, where only a few rows may be readable),
  // for the decoder of zxing-cpp only (decodeRow finds more false positives)
  rowStep := 5;
  if FTryHarder and FUsePattern then
    rowStep := Min(rowStep, Max(1, height div 256));
  maxLines := height; // Look at the whole image, not just the center

  // rows close to a row with a new barcode, to confirm it quickly (like
  // zxing-cpp); the last one is next
  var checkRows: TArray<Integer> := nil;
  X := -1;
  while true do
  begin
    var isCheckRow := (Length(checkRows) > 0);
    if isCheckRow then
    begin
      rowNumber := checkRows[High(checkRows)];
      SetLength(checkRows, High(checkRows));
      if (rowNumber < 0) or (rowNumber >= height) then
        continue;
    end
    else
    begin
      Inc(X);
      if (X >= maxLines) then
        break;
      // Scanning from the middle out. Determine which row we're looking at next:
      rowStepsAboveOrBelow := TMathUtils.Asr((X + 1), 1);
      isAbove := (X and $01) = 0; // i.e. is x even?

      if (not isAbove) then
      begin
        rowStepsAboveOrBelow := rowStepsAboveOrBelow * -1;
      end;
      rowNumber := middle + rowStep * rowStepsAboveOrBelow;

      if ((rowNumber < 0) or (rowNumber >= height)) then
      begin
        // Oops, if we run off the top or bottom, stop
        break;
      end;
    end;

    // the decoder of zxing-cpp, on the bars and spaces of the row
    if FUsePattern then
    begin
      ReadResult := decodePatternRow(image, rowNumber, hints, collector,
        maxCount, pending, isCheckRow, checkRows);
      if (ReadResult <> nil) or (collector <> nil) and
        ResultsFull(collector, maxCount) then
        Exit(ReadResult);
      continue;
    end;

    // Estimate black point for this row and load it:
    row := image.getBlackRow(rowNumber, row);
    if (row = nil) then
    begin
      continue;
    end;

    // While we have the image data in a BitArray, it's fairly cheap to reverse it in place to
    // handle decoding upside down barcodes.
    // for attempt := 0 to (attempt < 2) do
    for attempt := 0 to 1 do
    begin

      hadResultPointCallBack := false;
      if (attempt = 1) then
      begin
        // trying again?
        row.Reverse();


        // This means we will only ever draw result points *once* in the life of this method
        // since we want to avoid drawing the wrong points after flipping the row, and,
        // don't want to clutter with noise from every single row scan -- just the scans
        // that start on the center line.

        if needsCallBack then
        begin
          hints.TryGetValue
            (ZXing.DecodeHintType.NEED_RESULT_POINT_CALLBACK, obj);
          hints.Remove(ZXing.DecodeHintType.NEED_RESULT_POINT_CALLBACK);
          hadResultPointCallBack := true;
        end;

      end;

      // Look for a barcode
      ReadResult := decodeRow(rowNumber, row, hints);
      if hadResultPointCallBack then
      begin
        hints.Add(ZXing.DecodeHintType.NEED_RESULT_POINT_CALLBACK, obj);
      end;

      if (ReadResult = nil) then
      begin
        continue;
      end;

      // We found our barcode
      if (attempt = 1) then
      begin
        // But it was upside down, so note that
        // ReadResult.putMetadata(ResultMetadataType.orientation, TObject(180));
        // And remember to flip the result points horizontally.
        // (all of them, also those of an add-on)
        points := ReadResult.ResultPoints;
        for var i := 0 to High(points) do
          if (points[i] <> nil) then
            points[i] := TResultPointHelpers.CreateResultPoint
              (width - points[i].X - 1, points[i].Y);

      end;

      if (collector = nil) then
        Exit(ReadResult);

      // collecting all barcodes
      var pendingBefore := pending.Count;
      var rightEdge := RightEdgeOf(ReadResult);
      if CollectResult(ReadResult, collector, pending, maxCount) then
        Exit(nil);

      // more barcodes further on the same row (only the first one of a row
      // is found): look again behind the found one
      if (attempt = 0) then
        for var more := 1 to 8 do
        begin
          var rest := RowFrom(row, Trunc(rightEdge) + 1);
          var nextResult := decodeRow(rowNumber, rest, hints);
          if (nextResult = nil) then
            break;
          var nextEdge := RightEdgeOf(nextResult);
          if CollectResult(nextResult, collector, pending, maxCount) then
            Exit(nil);
          if (nextEdge <= rightEdge) then
            break;
          rightEdge := nextEdge;
        end;

      // a new barcode (read on this row only): check the rows next to it
      if (pending.Count > pendingBefore) and not isCheckRow then
        checkRows := [rowNumber + 2, rowNumber - 2, rowNumber + 1,
          rowNumber - 1];
      break;

    end; // loop

  end;

  Result := nil;
end;

function TOneDReader.decodePattern(rowNumber: Integer; var next: TPatternView;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
end;

function TOneDReader.HasPatternDecoder: Boolean;
begin
  Result := false;
end;

function TOneDReader.decodePatternRow(const image: TBinaryBitmap;
  rowNumber: Integer; const hints: TDictionary<TDecodeHintType, TObject>;
  collector: TList<TReadResult>; maxCount: Integer;
  pending: TList<TReadResult>; isCheckRow: Boolean;
  var checkRows: TArray<Integer>): TReadResult;
begin
  Result := nil;
  var bars := image.getPatternRow(rowNumber);
  if (bars = nil) then
    exit;
  var pendingBefore := 0;
  if (pending <> nil) then
    pendingBefore := pending.Count;
  var width := image.Width;

  // the row and, like zxing-cpp, the row reversed (cheap for the widths) to
  // decode upside down barcodes
  for var attempt := 0 to 1 do
  begin
    var view: TPatternView;
    if (attempt = 0) then
      view := TPatternView.Create(bars)
    else
    begin
      var n := Length(bars);
      SetLength(FBars, n);
      for var i := 0 to n - 1 do
        FBars[i] := bars[n - 1 - i];
      view := TPatternView.Create(FBars);
    end;

    var found := false;
    var r: TReadResult;
    repeat
      r := decodePattern(rowNumber, view, hints);
      if (r <> nil) then
      begin
        // upside down: flip the points horizontally
        if (attempt = 1) then
        begin
          var points := r.ResultPoints;
          for var i := 0 to High(points) do
            if (points[i] <> nil) then
              points[i] := TResultPointHelpers.CreateResultPoint
                (width - points[i].X - 1, points[i].Y);
        end;
        if (collector = nil) then
          exit(r);
        found := true;
        if CollectResult(r, collector, pending, maxCount) then
          exit;
      end;
      // make sure we make progress and start the next try on a bar
      view.Shift(2 - (view.Index mod 2));
      view.Extend;
      // without TRY_HARDER only behind a barcode found
    until (not FTryHarder and (r = nil)) or (view.Size = 0);

    // found in this direction: not the other one, like decodeRow
    if found then
      break;
  end;

  // a new barcode (read on this row only): check the rows next to it
  if (pending <> nil) and (pending.Count > pendingBefore) and not isCheckRow
  then
    checkRows := [rowNumber + 2, rowNumber - 2, rowNumber + 1, rowNumber - 1];
end;

class function TOneDReader.NarrowWideThreshold(const view: TPatternView)
  : TBarAndSpace;
begin
  var m := TBarAndSpace.Create(view[0], view[1]);
  var mx := m;
  for var i := 2 to view.Size - 1 do
  begin
    if (view[i] < m[i]) then
      m[i] := view[i];
    if (view[i] > mx[i]) then
      mx[i] := view[i];
  end;

  // the max spread between bars and spaces depends on whether both have
  // seen narrow and wide ones
  var maxSpread := 4;
  if (mx[0] >= 2 * m[0]) and (mx[1] >= 2 * m[1]) then
    maxSpread := 2;

  Result := TBarAndSpace.Create(0, 0);
  for var i := 0 to 1 do
  begin
    // wide <= 4 * narrow and bars and spaces not more than a factor of
    // spread apart from each other
    if (mx[i] > 4 * (m[i] + 1)) or (mx[i] > maxSpread * mx[i + 1]) or
      (m[i] > maxSpread * (m[i + 1] + 1)) then
      exit(TBarAndSpace.Create(0, 0));
    // the average of min and max, but at least 1.5 * min
    Result[i] := Max((m[i] + mx[i]) div 2, m[i] * 3 div 2);
  end;
end;

class function TOneDReader.NarrowWideBitPattern(const view: TPatternView)
  : Integer;
begin
  var threshold := NarrowWideThreshold(view);
  if not threshold.IsValid then
    exit(-1);

  Result := 0;
  for var i := 0 to view.Size - 1 do
  begin
    if (view[i] > threshold[i] * 2) then
      exit(-1);
    Result := (Result shl 1) or Ord(view[i] > threshold[i]);
  end;
end;

class function TOneDReader.PatternMatchVarianceF(const view: TPatternView;
  const pattern: TOneDPattern; maxIndividualVariance: Double): Double;
const
  NO_MATCH = 1E30;
begin
  var n := Length(pattern);
  var total := view.Sum(n);
  var patternLength := 0;
  for var p in pattern do
    Inc(patternLength, p);
  // less than one pixel per module: too small to match reliably
  if (total < patternLength) then
    exit(NO_MATCH);

  var unitBarWidth: Double := total / patternLength;
  maxIndividualVariance := maxIndividualVariance * unitBarWidth;

  var totalVariance: Double := 0;
  for var x := 0 to n - 1 do
  begin
    var variance: Double := Abs(view[x] - pattern[x] * unitBarWidth);
    if (variance > maxIndividualVariance) then
      exit(NO_MATCH);
    totalVariance := totalVariance + variance;
  end;
  Result := totalVariance / total;
end;

class function TOneDReader.DecodeDigitF(const view: TPatternView;
  const patterns: TOneDPatterns; maxAvgVariance, maxIndividualVariance: Double;
  requireUnambiguousMatch: Boolean): Integer;
begin
  var bestVariance := maxAvgVariance; // the worst variance accepted
  Result := -1;
  for var i := 0 to High(patterns) do
  begin
    var variance := PatternMatchVarianceF(view, patterns[i],
      maxIndividualVariance);
    if (variance < bestVariance) then
    begin
      bestVariance := variance;
      Result := i;
    end
    else if requireUnambiguousMatch and (variance = bestVariance) then
      // two equally good matches: not reliable
      Result := -1;
  end;
end;

class function TOneDReader.patternMatchVariance(counters: TArray<Integer>;
  pattern: TOneDPattern; maxIndividualVariance: Integer): Integer;
var
  scaledPattern, variance, counter, totalVariance, X, unitBarWidth, i,
    patternLength, numCounters, total: Integer;
begin
  Result := High(Integer);

  numCounters := Length(counters);
  total := 0;
  patternLength := 0;
  for i := 0 to numCounters - 1 do
  begin
    total := total + counters[i];
    patternLength := patternLength + pattern[i];
  end;

  if (total < patternLength) then
    // If we don't even have one pixel per unit of bar width, assume this is too small
    // to reliably match, so fail:
    Exit;

  // We're going to fake floating-point math in integers. We just need to use more bits.
  // Scale up patternLength so that intermediate values below like scaledCounter will have
  // more "significant digits"
  unitBarWidth := (total shl INTEGER_MATH_SHIFT) div patternLength;
  maxIndividualVariance :=
    TMathUtils.Asr((maxIndividualVariance * unitBarWidth), INTEGER_MATH_SHIFT);

  totalVariance := 0;
  for X := 0 to numCounters - 1 do
  begin
    counter := counters[X] shl INTEGER_MATH_SHIFT;
    scaledPattern := pattern[X] * unitBarWidth;

    if (counter > scaledPattern) then
      variance := counter - scaledPattern
    else
      variance := scaledPattern - counter;

    if (variance > maxIndividualVariance) then
      Exit;

    totalVariance := totalVariance + variance;
  end;

  Result := totalVariance div total;
end;

class function TOneDReader.recordPattern(row: IBitArray; start: Integer;
  counters: TArray<Integer>): Boolean;
var
  i, counterPosition, idx, ending, numCounters: Integer;
  isWhite: Boolean;
begin
  numCounters := Length(counters);
  for idx := 0 to numCounters - 1 do
  begin
    counters[idx] := 0;
  end;

  ending := row.Size;

  if (start >= ending) then
  begin
    Result := false;
    Exit;
  end;

  isWhite := not row[start];
  counterPosition := 0;
  i := start;
  while (i < ending) do
  begin
    if (row[i] xor isWhite) then
    begin // that is, exactly one is true
      inc(counters[counterPosition]);
    end
    else
    begin
      inc(counterPosition);
      if (counterPosition = numCounters) then
      begin
        break;
      end
      else
      begin
        counters[counterPosition] := 1;
        isWhite := not isWhite;
      end;
    end;
    inc(i);
  end;

  // If we read fully the last section of pixels and filled up our counters -- or filled
  // the last counter but ran off the side of the image, OK. Otherwise, a problem.
  Result := ((counterPosition = numCounters) or
    ((counterPosition = (numCounters - 1)) and (i = ending)));
end;

function TOneDReader.RecordPatternInReverse(row: IBitArray;
  start: Integer; counters: TArray<Integer>): Boolean;
var
  numTransitionsLeft: Integer;
  last: Boolean;
begin
  // This could be more efficient I guess
  numTransitionsLeft := Length(counters);
  last := row[start];
  while ((start > 0) and (numTransitionsLeft >= 0)) do
  begin
    dec(start);
    if (row[start] <> last) then
    begin
      dec(numTransitionsLeft);
      last := not last;
    end;
  end;

  if (numTransitionsLeft >= 0) then
  begin
    Result := false;
    Exit;
  end;
  Result := recordPattern(row, start + 1, counters);
end;

procedure TOneDReader.reset;
begin
  // do nothing
end;
end.
