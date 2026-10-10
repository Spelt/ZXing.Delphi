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

  * The scan of the stacked barcodes (Codablock F, Code 16K, Code 49) for
  * ZXing.Delphi: the rows of the image read once for all of them.
}

unit ZXing.Stacked.StackedReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.ReadResult,
  ZXing.Reader,
  ZXing.DecodeHintType,
  ZXing.BinaryBitmap,
  ZXing.Common.Pattern;

type
  /// <summary>
  /// A reader of a stacked barcode: it reads its rows in the rows of the
  /// image (the scan, ScanStacked) and puts them together into symbols.
  /// </summary>
  TStackedRowReader = class(TInterfacedObject, IReader, IMultipleReader)
  protected
    /// <summary>Reads the rows of the symbol in the runs of row y of the
    /// image (width pixels wide, the runs reversed: from right to left).
    /// </summary>
    procedure ReadRows(const runs: TPatternRow; y, width: Integer;
      reversed: Boolean); virtual; abstract;
    /// <summary>The symbols of the rows read (vertical: in the image
    /// turned) to results.</summary>
    procedure AddSymbols(results: TList<TReadResult>; maxCount: Integer;
      vertical: Boolean); virtual; abstract;
    /// <summary>Forgets the rows read.</summary>
    procedure ClearRows; virtual; abstract;
  public
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

  /// <summary>
  /// Reads several stacked barcodes with one scan of the image.
  /// </summary>
  TStackedReader = class(TInterfacedObject, IReader, IMultipleReader)
  private
    FReaders: TArray<TStackedRowReader>;
    // (the references that keep them)
    FReferences: TArray<IReader>;
  public
    constructor Create(const readers: array of TStackedRowReader);
    function decode(const image: TBinaryBitmap): TReadResult; overload;
    function decode(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; overload;
    procedure decodeMultiple(const image: TBinaryBitmap;
      hints: TDictionary<TDecodeHintType, TObject>;
      results: TList<TReadResult>; maxCount: Integer);
    procedure reset;
  end;

/// <summary>Scans the image for the stacked barcodes of readers: the rows
/// as the 1D readers have them (every 4th, with TRY_HARDER every 2nd: the
/// rows of the symbols are at least 8 modules high), left to right and right
/// to left (upside down), with TRY_HARDER also in the image turned
/// (vertical).</summary>
procedure ScanStacked(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer; const readers: array of TStackedRowReader);

implementation

uses
  ZXing.Common.BitMatrix,
  ZXing.Postal.FourStateDetector;

procedure ScanStacked(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer; const readers: array of TStackedRowReader);
begin
  if (image = nil) or (image.BlackMatrix = nil) or
    ResultsFull(results, maxCount) then
    exit;
  var tryHarder := (hints <> nil) and
    hints.ContainsKey(TDecodeHintType.TRY_HARDER);
  var rowStep := 4;
  if tryHarder then
    rowStep := 2;
  for var vertical in [false, true] do
  begin
    if vertical and not tryHarder then
      break;
    var matrix := image.BlackMatrix;
    if vertical then
      // (made once, shared with the postal reader, owned by the image)
      matrix := image.BlackMatrixTransposed;
    try
      for var reader in readers do
        reader.ClearRows;
      var runs, forward, backward: TPatternRow;
      var y := 0;
      while (y < matrix.Height) do
      begin
        // the vertical rows (columns) are not cached by the image: once per
        // row, the reverse in a buffer used again
        if vertical then
        begin
          GetPatternRow(matrix, y, forward);
          var n := Length(forward);
          if (Length(backward) < n) then
            SetLength(backward, n);
          for var i := 0 to n - 1 do
            backward[n - 1 - i] := forward[i];
        end;
        for var reversed in [false, true] do
        begin
          if vertical then
          begin
            if reversed then
              runs := backward
            else
              runs := forward;
          end
          else
            runs := image.getPatternRow(y, reversed);
          if (runs <> nil) then
            for var reader in readers do
              reader.ReadRows(runs, y, matrix.Width, reversed);
        end;
        Inc(y, rowStep);
      end;
      for var reader in readers do
        reader.AddSymbols(results, maxCount, vertical);
    finally
      for var reader in readers do
        reader.ClearRows;
    end;
  end;
end;

/// <summary>The first of the results of decodeMultiple of reader.</summary>
function DecodeFirst(const reader: IMultipleReader; const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := nil;
  var results := TList<TReadResult>.Create;
  try
    reader.decodeMultiple(image, hints, results, 1);
    if (results.Count > 0) then
      Result := results.Extract(results[0]);
  finally
    for var r in results do
      r.Free;
    results.Free;
  end;
end;

{ TStackedRowReader }

function TStackedRowReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TStackedRowReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := DecodeFirst(Self, image, hints);
end;

procedure TStackedRowReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  ScanStacked(image, hints, results, maxCount, [Self]);
end;

procedure TStackedRowReader.reset;
begin
  // do nothing
end;

{ TStackedReader }

constructor TStackedReader.Create(const readers: array of TStackedRowReader);
begin
  inherited Create;
  for var reader in readers do
  begin
    FReaders := FReaders + [reader];
    FReferences := FReferences + [reader as IReader];
  end;
end;

function TStackedReader.decode(const image: TBinaryBitmap): TReadResult;
begin
  Result := decode(image, nil);
end;

function TStackedReader.decode(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
begin
  Result := DecodeFirst(Self, image, hints);
end;

procedure TStackedReader.decodeMultiple(const image: TBinaryBitmap;
  hints: TDictionary<TDecodeHintType, TObject>; results: TList<TReadResult>;
  maxCount: Integer);
begin
  ScanStacked(image, hints, results, maxCount, FReaders);
end;

procedure TStackedReader.reset;
begin
  // do nothing
end;

end.
