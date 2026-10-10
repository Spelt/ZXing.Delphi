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

unit ZXing.BinaryBitmap;

interface

uses
  System.SysUtils,
  ZXing.Binarizer,
  ZXing.LuminanceSource,
  ZXing.Common.BitArray,
  ZXing.Common.BitMatrix,
  ZXing.Common.Pattern,
  System.Diagnostics,
  ZXing.ReaderTimings;

type
  TBinaryBitmap = class
  private
    Binarizer: TBinarizer;
    Matrix: TBitMatrix;
    /// <summary>True for a bitmap made by rotateCounterClockwise: it then
    /// frees its binarizer and luminance source. Otherwise the caller that
    /// created them frees them.</summary>
    FOwnsBinarizer: Boolean;
    // the black rows already calculated, shared by all (1D) readers: the
    // words of the row, or nil with FRowState 1 when the binarizer gave nil
    FRowCache: TArray<TArray<Integer>>;
    FRowState: TArray<Byte>; // 0 not calculated, 1 nil, 2 cached
    // the widths of the bars and spaces of the black rows, shared by the 1D
    // readers of zxing-cpp (decodePattern)
    FPatternRows: TArray<TPatternRow>;
    FPatternRowsReversed: TArray<TPatternRow>;
    FPatternState: TArray<Byte>; // 0 not calculated, 1 nil, 2 cached
    FPatternBits: IBitArray;
    FPatternScratch: TPatternRow;
    // the image rotated by 90 degrees, made once for all readers that scan
    // it (the 1D readers with TRY_HARDER), owned by this bitmap
    FRotated: TBinaryBitmap;
    // the black matrix rotated by 90 degrees and transposed, made once for
    // the readers that scan them (PDF417, MicroPDF417, the stacked and the
    // postal codes), owned by this bitmap
    FMatrixRotated90: TBitMatrix;
    FMatrixTransposed: TBitMatrix;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetBlackMatrix: TBitMatrix;
  public
    constructor Create(Binarizer: TBinarizer);
    destructor Destroy(); override;
    /// <summary>
    /// Converts one row of luminance data to 1 bit data. May actually do the conversion, or return
    /// cached data. Callers should assume this method is expensive and call it as seldom as possible.
    /// This method is intended for decoding 1D barcodes and may choose to apply sharpening.
    /// </summary>
    /// <param name="y">The row to fetch, which must be in [0, bitmap height).</param>
    /// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
    /// If used, the Binarizer will call BitArray.clear(). Always use the returned object.
    /// </param>
    /// <returns> The array of bits for this row (true means black).</returns>
    function getBlackRow(y: Integer; row: IBitArray): IBitArray;
    /// <summary>
    /// The widths of the bars and spaces of black row y (see TPatternRow),
    /// from right to left when reversed, calculated once; nil when the
    /// binarizer has no row. The caller must not change it.
    /// </summary>
    function getPatternRow(y: Integer; reversed: Boolean = false)
      : TPatternRow;
    function RotateSupported: Boolean;
    /// <summary>A new bitmap of the image rotated by 90 degrees counter
    /// clockwise; the caller frees it.</summary>
    function rotateCounterClockwise(): TBinaryBitmap;
    /// <summary>The image rotated by 90 degrees counter clockwise, made on
    /// the first call and shared afterwards (with its black rows and
    /// pattern rows): the readers that scan the rotated image do not each
    /// rotate and binarize it again. Owned by this bitmap: do not free.
    /// </summary>
    function RotatedCounterClockwise: TBinaryBitmap;
    /// <summary>The black matrix rotated by 90 degrees counter clockwise
    /// (TBitMatrix.Rotated90), made on the first call and shared afterwards;
    /// nil when there is no black matrix. Owned by this bitmap: do not free
    /// or change.</summary>
    function BlackMatrixRotated90: TBitMatrix;
    /// <summary>The black matrix transposed (TBitMatrix.Transposed), made on
    /// the first call and shared afterwards; nil when there is no black
    /// matrix. Owned by this bitmap: do not free or change.</summary>
    function BlackMatrixTransposed: TBitMatrix;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property BlackMatrix: TBitMatrix read GetBlackMatrix;
    /// <summary>The luminances of the image (row major, 0 black, the array
    /// of the luminance source: do not change it); nil without a source.
    /// </summary>
    function Luminances: TArray<Byte>;
  end;

implementation

{ TBinaryBitmap }

constructor TBinaryBitmap.Create(Binarizer: TBinarizer);
begin

  if (Binarizer = nil) then
  begin
    raise EArgumentException.Create('Binarizer must be non-null.');
  end;

  Self.Binarizer := Binarizer;

end;

destructor TBinaryBitmap.Destroy;
begin
  FRotated.Free;
  FMatrixRotated90.Free;
  FMatrixTransposed.Free;
  if Assigned(Matrix) then
    FreeAndNil(Matrix);

  if FOwnsBinarizer then
  begin
    Binarizer.LuminanceSource.Free;
    FreeAndNil(Binarizer);
  end;
  inherited;
end;

function TBinaryBitmap.GetBlackMatrix: TBitMatrix;
begin
  if (Matrix = nil) then
  begin
    Matrix := Binarizer.BlackMatrix();
  end;

  result := Matrix;

end;

function TBinaryBitmap.getBlackRow(y: Integer; row: IBitArray): IBitArray;
begin
  if (y < 0) or (y >= Height) then
    exit(Binarizer.getBlackRow(y, row));

  if (FRowState = nil) then
  begin
    SetLength(FRowState, Height);
    SetLength(FRowCache, Height);
  end;

  if (FRowState[y] = 0) then
  begin
    // calculate the row once and keep a copy of its words
    // (the time of the binarization of the rows is booked apart from the
    // reader that asks for them first, when the benchmark measures)
    var start: Int64 := 0;
    if ReaderTimingsEnabled then
      start := TStopwatch.GetTimeStamp;
    result := Binarizer.getBlackRow(y, row);
    if ReaderTimingsEnabled then
      AddNestedReaderTiming(READER_TIMING_BLACK_ROWS,
        TStopwatch.GetTimeStamp - start);
    if (result = nil) then
      FRowState[y] := 1
    else
    begin
      FRowCache[y] := Copy(result.Bits, 0, (Width + 31) shr 5);
      FRowState[y] := 2;
    end;
    exit;
  end;

  // from the cache, in the row the caller passed, like the binarizer does
  var w := Width;
  if (row = nil) or (row.Size < w) then
    row := TBitArrayHelpers.CreateBitArray(w)
  else
    row.clear();
  if (FRowState[y] = 1) then
    exit(nil);
  var words := FRowCache[y];
  for var i := 0 to High(words) do
    if (words[i] <> 0) then
      row.setBulk(i shl 5, words[i]);
  result := row;
end;

function TBinaryBitmap.getPatternRow(y: Integer; reversed: Boolean)
  : TPatternRow;
begin
  if (y < 0) or (y >= Height) then
    exit(nil);
  if (FPatternState = nil) then
  begin
    SetLength(FPatternState, Height);
    SetLength(FPatternRows, Height);
  end;
  if (FPatternState[y] = 0) then
  begin
    FPatternBits := getBlackRow(y, FPatternBits);
    if (FPatternBits = nil) then
      FPatternState[y] := 1
    else
    begin
      // (the runs in a buffer used for every row, then a copy of the exact
      // length: no zeroed array of the width and no reallocation per row)
      var start: Int64 := 0;
      if ReaderTimingsEnabled then
        start := TStopwatch.GetTimeStamp;
      var bits := FPatternBits.Bits;
      var count := GetPatternRowInto(PInteger(bits), Length(bits), Width,
        FPatternScratch);
      FPatternRows[y] := Copy(FPatternScratch, 0, count);
      FPatternState[y] := 2;
      if ReaderTimingsEnabled then
        AddNestedReaderTiming(READER_TIMING_PATTERN_ROWS,
          TStopwatch.GetTimeStamp - start);
    end;
  end;
  if not reversed then
    exit(FPatternRows[y]);

  // the row from right to left: the widths in reverse order
  if (FPatternRowsReversed = nil) then
    SetLength(FPatternRowsReversed, Height);
  Result := FPatternRowsReversed[y];
  if (Result = nil) and (FPatternRows[y] <> nil) then
  begin
    var row := FPatternRows[y];
    var n := Length(row);
    SetLength(Result, n);
    for var i := 0 to n - 1 do
      Result[i] := row[n - 1 - i];
    FPatternRowsReversed[y] := Result;
  end;
end;

function TBinaryBitmap.GetHeight: Integer;
begin
  result := Binarizer.Height;
end;

function TBinaryBitmap.GetWidth: Integer;
begin
  result := Binarizer.Width;
end;

function TBinaryBitmap.rotateCounterClockwise: TBinaryBitmap;
var
  newSource: TLuminanceSource;
begin
  newSource := Binarizer.LuminanceSource.rotateCounterClockwise();
  result := TBinaryBitmap.Create(Binarizer.createBinarizer(newSource));
  result.FOwnsBinarizer := true;
end;

/// <summary>The time stamp when the benchmark measures, else 0.</summary>
function TimingStart: Int64;
begin
  Result := 0;
  if ReaderTimingsEnabled then
    Result := TStopwatch.GetTimeStamp;
end;

/// <summary>Books the time since start (when the benchmark measures) as the
/// shared work of turning the image, apart from the reader that asked.
/// </summary>
procedure TimingTurned(start: Int64);
begin
  if ReaderTimingsEnabled then
    AddNestedReaderTiming(READER_TIMING_TURNED, TStopwatch.GetTimeStamp - start);
end;

function TBinaryBitmap.RotatedCounterClockwise: TBinaryBitmap;
begin
  if (FRotated = nil) then
  begin
    var start := TimingStart;
    FRotated := rotateCounterClockwise;
    TimingTurned(start);
  end;
  Result := FRotated;
end;

function TBinaryBitmap.BlackMatrixRotated90: TBitMatrix;
begin
  if (FMatrixRotated90 = nil) and (BlackMatrix <> nil) then
  begin
    var start := TimingStart;
    FMatrixRotated90 := BlackMatrix.Rotated90;
    TimingTurned(start);
  end;
  Result := FMatrixRotated90;
end;

function TBinaryBitmap.BlackMatrixTransposed: TBitMatrix;
begin
  if (FMatrixTransposed = nil) and (BlackMatrix <> nil) then
  begin
    var start := TimingStart;
    FMatrixTransposed := BlackMatrix.Transposed;
    TimingTurned(start);
  end;
  Result := FMatrixTransposed;
end;

function TBinaryBitmap.Luminances: TArray<Byte>;
begin
  Result := nil;
  if (Binarizer <> nil) and (Binarizer.LuminanceSource <> nil) then
    Result := Binarizer.LuminanceSource.Matrix;
end;

function TBinaryBitmap.RotateSupported: Boolean;
begin
  result := Binarizer.LuminanceSource.RotateSupported();
end;

end.
