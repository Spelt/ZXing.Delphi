unit OneDReader;
{
  * Copyright 2008 ZXing authors
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

  * Implemented by E. Spelt for Delphi
}
interface

uses SysUtils, Generics.Collections, Math,
  Reader, BinaryBitmap, ReadResult, DecodeHintType, ResultMetadataType,
  ResultPoint, BitArray, MathUtils;

type
  /// <summary>
  /// Encapsulates functionality and implementation that is common to all families
  /// of one-dimensional barcodes.
  /// <author>dswitkin@google.com (Daniel Switkin)</author>
  /// <author>Sean Owen</author>
  /// </summary>
  TOneDReader = class(TInterfacedObject, IReader)
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
    function DoDecode(image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
  protected

    class var INTEGER_MATH_SHIFT: Integer;
    class var PATTERN_MATCH_RESULT_SCALE_FACTOR: Integer;

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
    class function patternMatchVariance(counters, pattern: TArray<Integer>;
      maxIndividualVariance: Int64): Integer; static;

    /// <summary>
    /// Records the pattern in reverse.
    /// </summary>
    /// <param name="row">The row.</param>
    /// <param name="start">The start.</param>
    /// <param name="counters">The counters.</param>
    /// <returns></returns>
    class function RecordPatternInReverse(row: TBitArray; start: Integer;
      counters: TArray<Integer>): Boolean; Static;


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
    class function recordPattern(row: TBitArray; start: Integer;
      counters: TArray<Integer>; numCounters: Integer): Boolean;
      overload; static;

  public
    /// <summary>
    /// Resets any internal state the implementation has after a decode, to prepare it
    /// for reuse.
    /// </summary>
    procedure Reset(); virtual;
    /// <summary>
    /// Locates and decodes a barcode in some format within an image.
    /// </summary>
    /// <param name="image">image of barcode to decode</param>
    /// <returns>
    /// String which the barcode encodes
    /// </returns>
    function Decode(image: TBinaryBitmap): TReadResult; overload;
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
    function Decode(image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      overload; virtual;

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
    class function recordPattern(row: TBitArray; start: Integer;
      counters: TArray<Integer>): Boolean; overload; static;


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
    function DecodeRow(rowNumber: Integer; row: TBitArray;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      virtual; abstract;

    constructor Create();

  end;

implementation

{ TOneDReader }

function TOneDReader.Decode(image: TBinaryBitmap): TReadResult;
begin
  Result := Decode(image, nil);
end;

constructor TOneDReader.Create;
begin
  inherited Create;
  INTEGER_MATH_SHIFT := 8;
  PATTERN_MATCH_RESULT_SCALE_FACTOR := 1 shl INTEGER_MATH_SHIFT;
end;

function TOneDReader.Decode(image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;

var
  tryHarder, tryHarderWithoutRotation: Boolean;
  rotatedImage: TBinaryBitmap;
  metadata: TDictionary<TResultMetadataType, TObject>;
  orientation, height, i, l: Integer;
  points: TArray<TResultPoint>;

begin
  Result := DoDecode(image, hints);
  if (Result = nil) then
  begin
    Exit;
  end;

  tryHarder := (hints <> nil) and
    (hints.ContainsKey(DecodeHintType.TRY_HARDER));

  tryHarderWithoutRotation := (hints <> nil) and
    (hints.ContainsKey(DecodeHintType.TRY_HARDER_WITHOUT_ROTATION));

  if (tryHarder and ((not tryHarderWithoutRotation) and image.RotateSupported))
  then
  begin
    rotatedImage := image.rotateCounterClockwise();
    Result := DoDecode(rotatedImage, hints);
    if (Result = nil) then
    begin
      Exit;
    end;

    // Record that we found it rotated 90 degrees CCW / 270 degrees CW
    metadata := Result.ResultMetadata;
    orientation := 270;
    if ((metadata <> nil) and metadata.ContainsKey
      (ResultMetadataType.orientation)) then
    begin
      // But if we found it reversed in doDecode(), add in that result here:
      orientation :=
        (orientation + Integer(metadata[ResultMetadataType.orientation]
        )) mod 360;
    end;

    Result.putMetadata(ResultMetadataType.orientation, TObject(orientation));
    // Update result points
    points := Result.ResultPoints;
    if (points <> nil) then
    begin
      height := rotatedImage.height;
      l := Length(points) - 1;
      for i := 0 to l do
      begin
        points[i] := TResultPoint.Create(height - points[i].Y - 1, points[i].X);
      end;
    end;

  end;

end;


function TOneDReader.DoDecode(image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;

var
  attempt, X, rowNumber, rowStepsAboveOrBelow, sr, width, height, middle,
    rowStep, maxLines: Integer;
  row: TBitArray;
  tryHarder, isAbove: Boolean;
  newHints: TDictionary<TDecodeHintType, TObject>;
  ReadResult: TReadResult;
  points: TArray<TResultPoint>;
  Key: TDecodeHintType;

begin
  width := image.width;
  height := image.height;
  row := TBitArray.Create(width);

  middle := TMathUtils.Asr(height, 1);
  tryHarder := (hints <> nil) and hints.ContainsKey(DecodeHintType.TRY_HARDER);

  sr := 5;
  if (tryHarder) then
  begin
    sr := 8;
  end;

  rowStep := Max(1, TMathUtils.Asr(height, sr));

  if (tryHarder) then
  begin
    maxLines := height; // Look at the whole image, not just the center
  end
  else
  begin
    maxLines := 15;
    // 15 rows spaced 1/32 apart is roughly the middle half of the image
  end;

  for x := 0 to maxLines - 1 do
  begin
    // Scanning from the middle out. Determine which row we're looking at next:
    rowStepsAboveOrBelow := TMathUtils.Asr((X + 1), 1);
    isAbove := (x and $01) = 0; // i.e. is x even?

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

      if (attempt = 1) then
      begin
        // trying again?
        row.Reverse();
        // row.reverse(); // reverse the row and continue
        // This means we will only ever draw result points *once* in the life of this method
        // since we want to avoid drawing the wrong points after flipping the row, and,
        // don't want to clutter with noise from every single row scan -- just the scans
        // that start on the center line.
        if ((hints <> nil) and hints.ContainsKey
          (DecodeHintType.NEED_RESULT_POINT_CALLBACK)) then
        begin
          newHints := TDictionary<TDecodeHintType, TObject>.Create;
          for Key in hints.Keys do
          begin

            if (Key <> DecodeHintType.NEED_RESULT_POINT_CALLBACK) then
            begin
              newHints.Add(Key, hints[Key]);
            end;

          end;
          hints := newHints;

        end;
      end;

      // Look for a barcode
      ReadResult := decodeRow(rowNumber, row, hints);
      if (ReadResult = nil) then
      begin
        continue;
      end;

      // We found our barcode
      if (attempt = 1) then
      begin
        // But it was upside down, so note that
//  ReadResult.putMetadata(ResultMetadataType.orientation, TObject(180));
        // And remember to flip the result points horizontally.
        points := ReadResult.ResultPoints;
        if (points <> nil) then
        begin
          points[0] := TResultPoint.Create(width - points[0].X - 1,
            points[0].Y);
          points[1] := TResultPoint.Create(width - points[1].X - 1,
            points[1].Y);
        end;

      end;

      Result := ReadResult;
      Exit;

      // inc(attempt);

    end; // attempt loop

  end;

  Result := nil;
end;

class function TOneDReader.patternMatchVariance(counters,
  pattern: TArray<Integer>; maxIndividualVariance: Int64): Integer;

var
  scaledPattern, variance, counter, totalVariance, X, unitBarWidth, i,
    patternLength, numCounters, total: Integer;

begin
  numCounters := Length(counters);
  total := 0;
  patternLength := 0;
  for i := 0 to numCounters - 1 do
  begin
    total := total + counters[i];
    patternLength := patternLength + pattern[i];
  end;

  if ((total < patternLength) or (patternLength < 1)) then
  begin
    // If we don't even have one pixel per unit of bar width, assume this is too small
    // to reliably match, so fail:
    Result := Int32.MaxValue;
    Exit;
  end;
  // We're going to fake floating-point math in integers. We just need to use more bits.
  // Scale up patternLength so that intermediate values below like scaledCounter will have
  // more "significant digits"
  unitBarWidth := (total shl INTEGER_MATH_SHIFT) div patternLength;
  maxIndividualVariance := TMathUtils.Asr((maxIndividualVariance * unitBarWidth)
    ,INTEGER_MATH_SHIFT);

  totalVariance := 0;
  for x := 0 to numCounters - 1 do
  begin
    counter := counters[X] shl INTEGER_MATH_SHIFT;
    scaledPattern := pattern[X] * unitBarWidth;

    if (counter > scaledPattern) then
    begin
      variance := counter - scaledPattern;
    end
    else
    begin
      variance := scaledPattern - counter;
    end;

    if (variance > maxIndividualVariance) then
    begin
      Result := Int32.MaxValue;
      Exit;
    end;
    totalVariance := totalVariance + variance;
  end;

  Result := totalVariance div total;
end;

class function TOneDReader.recordPattern(row: TBitArray; start: Integer;
  counters: TArray<Integer>): Boolean;
begin
  Result := recordPattern(row, start, counters, Length(counters));
end;

class function TOneDReader.recordPattern(row: TBitArray; start: Integer;
  counters: TArray<Integer>; numCounters: Integer): Boolean;

var
  i, counterPosition, idx, ending: Integer;
  isWhite: Boolean;

begin
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

class function TOneDReader.RecordPatternInReverse(row: TBitArray;
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

procedure TOneDReader.Reset;
begin
  // do nothing
end;

end.
