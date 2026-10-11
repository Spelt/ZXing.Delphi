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
  System.SyncObjs,
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
    // the lock of the caches above: readers in tasks share the image
    FLock: TCriticalSection;
    function DoGetBlackRow(y: Integer; row: IBitArray): IBitArray;
    function RowFromCache(y: Integer; row: IBitArray): IBitArray;
    function DoGetPatternRow(y: Integer; reversed: Boolean): TPatternRow;
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
  FLock := TCriticalSection.Create;
  // (the caches of the rows, for all rows from the start: the readers test
  // their states without the lock)
  SetLength(FRowState, Height);
  SetLength(FRowCache, Height);
  SetLength(FPatternState, Height);
  SetLength(FPatternRows, Height);
  SetLength(FPatternRowsReversed, Height);

end;

destructor TBinaryBitmap.Destroy;
begin
  FLock.Free;
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
  // (made once, under the lock; then without it, readers in tasks ask for
  // it all the time)
  Result := Matrix;
  if (Result <> nil) then
  begin
    MemoryBarrier;
    exit;
  end;
  FLock.Enter;
  try
    if (Matrix = nil) then
    begin
      var m := Binarizer.BlackMatrix();
      MemoryBarrier;
      Matrix := m;
    end;
    result := Matrix;
  finally
    FLock.Leave;
  end;
end;

function TBinaryBitmap.getBlackRow(y: Integer; row: IBitArray): IBitArray;
begin
  // a row in the cache without the lock (readers in tasks ask for the same
  // rows): its state is published after its words, see DoGetBlackRow
  if (y >= 0) and (y < Height) and (FRowState[y] <> 0) then
  begin
    MemoryBarrier;
    exit(RowFromCache(y, row));
  end;
  FLock.Enter;
  try
    Result := DoGetBlackRow(y, row);
  finally
    FLock.Leave;
  end;
end;

function TBinaryBitmap.RowFromCache(y: Integer; row: IBitArray): IBitArray;
begin
  // from the cache, in the row the caller passed, like the binarizer does
  var w := Width;
  if row.IsEmpty or (row.Size < w) then
    row := TBitArrayHelpers.CreateBitArray(w)
  else
    row.clear();
  if (FRowState[y] = 1) then
    exit(TBitArray.Empty);
  var words := FRowCache[y];
  for var i := 0 to High(words) do
    if (words[i] <> 0) then
      row.setBulk(i shl 5, words[i]);
  result := row;
end;

function TBinaryBitmap.DoGetBlackRow(y: Integer; row: IBitArray): IBitArray;
begin
  if (y < 0) or (y >= Height) then
    exit(Binarizer.getBlackRow(y, row));

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
    if result.IsEmpty then
    begin
      MemoryBarrier;
      FRowState[y] := 1;
    end
    else
    begin
      FRowCache[y] := Copy(result.Bits, 0, (Width + 31) shr 5);
      // (the words before the state, for the readers without the lock)
      MemoryBarrier;
      FRowState[y] := 2;
    end;
    exit;
  end;
  Result := RowFromCache(y, row);
end;

function TBinaryBitmap.getPatternRow(y: Integer; reversed: Boolean)
  : TPatternRow;
begin
  if (y < 0) or (y >= Height) then
    exit(nil);
  // a row in the cache without the lock, see getBlackRow
  if (FPatternState[y] <> 0) then
  begin
    MemoryBarrier;
    if not reversed then
      exit(FPatternRows[y]);
    Result := FPatternRowsReversed[y];
    if (Result <> nil) or (FPatternState[y] = 1) then
      exit;
  end;
  FLock.Enter;
  try
    Result := DoGetPatternRow(y, reversed);
  finally
    FLock.Leave;
  end;
end;

function TBinaryBitmap.DoGetPatternRow(y: Integer; reversed: Boolean)
  : TPatternRow;
begin
  if (FPatternState[y] = 0) then
  begin
    FPatternBits := getBlackRow(y, FPatternBits);
    if FPatternBits.IsEmpty then
    begin
      MemoryBarrier;
      FPatternState[y] := 1;
    end
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
      MemoryBarrier;
      FPatternState[y] := 2;
      if ReaderTimingsEnabled then
        AddNestedReaderTiming(READER_TIMING_PATTERN_ROWS,
          TStopwatch.GetTimeStamp - start);
    end;
  end;
  if not reversed then
    exit(FPatternRows[y]);

  // the row from right to left: the widths in reverse order
  Result := FPatternRowsReversed[y];
  if (Result = nil) and (FPatternRows[y] <> nil) then
  begin
    var row := FPatternRows[y];
    var n := Length(row);
    var rev: TPatternRow;
    SetLength(rev, n);
    for var i := 0 to n - 1 do
      rev[i] := row[n - 1 - i];
    // (the values before the array, for the readers without the lock)
    MemoryBarrier;
    FPatternRowsReversed[y] := rev;
    Result := rev;
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
  Result := FRotated;
  if (Result <> nil) then
  begin
    MemoryBarrier;
    exit;
  end;
  FLock.Enter;
  try
    if (FRotated = nil) then
    begin
      var start := TimingStart;
      var turned := rotateCounterClockwise;
      TimingTurned(start);
      MemoryBarrier;
      FRotated := turned;
    end;
    Result := FRotated;
  finally
    FLock.Leave;
  end;
end;

function TBinaryBitmap.BlackMatrixRotated90: TBitMatrix;
begin
  Result := FMatrixRotated90;
  if (Result <> nil) then
  begin
    MemoryBarrier;
    exit;
  end;
  FLock.Enter;
  try
    if (FMatrixRotated90 = nil) and (BlackMatrix <> nil) then
    begin
      var start := TimingStart;
      var turned := BlackMatrix.Rotated90;
      TimingTurned(start);
      MemoryBarrier;
      FMatrixRotated90 := turned;
    end;
    Result := FMatrixRotated90;
  finally
    FLock.Leave;
  end;
end;

function TBinaryBitmap.BlackMatrixTransposed: TBitMatrix;
begin
  Result := FMatrixTransposed;
  if (Result <> nil) then
  begin
    MemoryBarrier;
    exit;
  end;
  FLock.Enter;
  try
    if (FMatrixTransposed = nil) and (BlackMatrix <> nil) then
    begin
      var start := TimingStart;
      var turned := BlackMatrix.Transposed;
      TimingTurned(start);
      MemoryBarrier;
      FMatrixTransposed := turned;
    end;
    Result := FMatrixTransposed;
  finally
    FLock.Leave;
  end;
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
