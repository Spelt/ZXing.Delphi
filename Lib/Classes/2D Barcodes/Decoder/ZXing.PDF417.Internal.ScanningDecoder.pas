{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
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

  * Ported from zxing-cpp (PDFScanningDecoder.cpp, PDFDetectionResult.cpp,
  * PDFDetectionResultColumn.cpp, PDFBoundingBox.cpp, PDFBarcodeValue.cpp,
  * PDFCodeword.h, PDFBarcodeMetadata.h), originally by Guenther Grau: reads
  * the codewords of a PDF417 symbol between its start and stop patterns,
  * row by row of the image, and assigns them to the rows of the symbol with
  * the row indicator columns.
}

unit ZXing.PDF417.Internal.ScanningDecoder;

interface

uses
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint,
  ZXing.DecoderResult,
  ZXing.PDF417.ResultMetadata;

/// <summary>
/// Decodes the symbol between the corners of its codeword area (nil when a
/// start or stop pattern was not found). nil when it can not be read, with
/// error 'Checksum', 'Format' or 'Unsupported' (or '' when the symbol
/// structure was not found); approxSymbolWidth is the estimated width of
/// the symbol in pixels.
/// </summary>
function DecodePDF417Scanning(image: TBitMatrix;
  const imageTopLeft, imageBottomLeft, imageTopRight,
  imageBottomRight: IResultPoint; minCodewordWidth, maxCodewordWidth: Integer;
  out error: string; out extra: IPDF417ResultMetadata;
  out approxSymbolWidth: Integer): TDecoderResult;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  ZXing.PDF417.Internal.CodewordDecoder,
  ZXing.PDF417.Internal.DecodedBitStreamParser;

const
  CODEWORD_SKEW_SIZE = 2;
  MAX_NEARBY_DISTANCE = 5;
  MIN_ROWS_IN_BARCODE = 3;
  MAX_ROWS_IN_BARCODE = 90;
  ADJUST_ROW_NUMBER_SKIP = 2;
  BARCODE_ROW_UNKNOWN = -1;

type
  TBarcodeMetadata = record
    ColumnCount, ErrorCorrectionLevel, RowCountUpperPart,
      RowCountLowerPart: Integer;
    function RowCount: Integer;
  end;

  /// <summary>How often each value was seen.</summary>
  TBarcodeValue = record
    Values: TArray<Integer>;
    Counts: TArray<Integer>;
    procedure SetValue(value: Integer);
    /// <summary>The values seen most often, in ascending order.</summary>
    function Value: TArray<Integer>;
  end;

  TPDFCodeword = record
    Valid: Boolean;
    StartX, EndX, Bucket, Value, RowNumber: Integer;
    class function Create(startX, endX, bucket, value: Integer)
      : TPDFCodeword; static;
    class function None: TPDFCodeword; static;
    function HasValidRowNumber: Boolean;
    function IsValidRowNumber(rowNumber: Integer): Boolean;
    procedure SetRowNumberAsRowIndicatorColumn;
    function Width: Integer;
  end;

  TBoundingBox = record
    Valid: Boolean;
    ImgWidth, ImgHeight: Integer;
    TopLeft, BottomLeft, TopRight, BottomRight: IResultPoint;
    MinX, MaxX, MinY, MaxY: Integer;
    procedure CalculateMinMaxValues;
    class function None: TBoundingBox; static;
    class function Create(imgWidth, imgHeight: Integer;
      const topLeft, bottomLeft, topRight, bottomRight: IResultPoint;
      out box: TBoundingBox): Boolean; static;
    class function Merge(const leftBox, rightBox: TBoundingBox;
      out box: TBoundingBox): Boolean; static;
    class function AddMissingRows(const box: TBoundingBox;
      missingStartRows, missingEndRows: Integer; isLeft: Boolean;
      out res: TBoundingBox): Boolean; static;
  end;

  TRowIndicator = (riNone, riLeft, riRight);

  TDetectionResultColumn = class
  private
    procedure SetRowNumbers;
    procedure AdjustIncompleteIndicatorColumnRowNumbers
      (const metadata: TBarcodeMetadata);
  public
    BoundingBox: TBoundingBox;
    Codewords: TArray<TPDFCodeword>;
    RowIndicator: TRowIndicator;
    constructor Create(const boundingBox: TBoundingBox;
      rowIndicator: TRowIndicator = riNone);
    function IsRowIndicator: Boolean;
    function IsLeftRowIndicator: Boolean;
    function ImageRowToCodewordIndex(imageRow: Integer): Integer;
    procedure SetCodeword(imageRow: Integer; const codeword: TPDFCodeword);
    function Codeword(imageRow: Integer): TPDFCodeword;
    function CodewordNearby(imageRow: Integer): TPDFCodeword;
    procedure AdjustCompleteIndicatorColumnRowNumbers
      (const metadata: TBarcodeMetadata);
    function GetRowHeights(out heights: TArray<Integer>): Boolean;
    function GetBarcodeMetadata(out metadata: TBarcodeMetadata): Boolean;
  end;

  /// <summary>The columns (row indicators at 0 and ColumnCount + 1) of a
  /// symbol; the columns are owned by the decoding (see TColumnStore).
  /// </summary>
  TDetectionResult = class
  public
    Metadata: TBarcodeMetadata;
    Columns: TArray<TDetectionResultColumn>;
    BoundingBox: TBoundingBox;
    procedure Init(const metadata: TBarcodeMetadata;
      const boundingBox: TBoundingBox);
    function BarcodeColumnCount: Integer;
    function BarcodeRowCount: Integer;
    function BarcodeECLevel: Integer;
    /// <summary>The columns with their row numbers adjusted.</summary>
    function AllColumns: TArray<TDetectionResultColumn>;
  end;

  TModuleBitCountType = TModuleBitCount;

{ TBarcodeMetadata }

function TBarcodeMetadata.RowCount: Integer;
begin
  Result := RowCountUpperPart + RowCountLowerPart;
end;

{ TBarcodeValue }

procedure TBarcodeValue.SetValue(value: Integer);
begin
  for var i := 0 to High(Values) do
    if (Values[i] = value) then
    begin
      Inc(Counts[i]);
      exit;
    end;
  Values := Values + [value];
  Counts := Counts + [1];
end;

function TBarcodeValue.Value: TArray<Integer>;
begin
  Result := nil;
  var maxConfidence := 0;
  for var c in Counts do
    maxConfidence := Max(maxConfidence, c);
  for var i := 0 to High(Values) do
    if (Counts[i] = maxConfidence) then
      Result := Result + [Values[i]];
  TArray.Sort<Integer>(Result);
end;

{ TPDFCodeword }

class function TPDFCodeword.Create(startX, endX, bucket, value: Integer)
  : TPDFCodeword;
begin
  Result.Valid := true;
  Result.StartX := startX;
  Result.EndX := endX;
  Result.Bucket := bucket;
  Result.Value := value;
  Result.RowNumber := BARCODE_ROW_UNKNOWN;
end;

class function TPDFCodeword.None: TPDFCodeword;
begin
  Result := Create(0, 0, 0, 0);
  Result.Valid := false;
end;

function TPDFCodeword.IsValidRowNumber(rowNumber: Integer): Boolean;
begin
  Result := (rowNumber <> BARCODE_ROW_UNKNOWN) and
    (Bucket = (rowNumber mod 3) * 3);
end;

function TPDFCodeword.HasValidRowNumber: Boolean;
begin
  Result := IsValidRowNumber(RowNumber);
end;

procedure TPDFCodeword.SetRowNumberAsRowIndicatorColumn;
begin
  RowNumber := (Value div 30) * 3 + Bucket div 3;
end;

function TPDFCodeword.Width: Integer;
begin
  Result := EndX - StartX;
end;

{ TBoundingBox }

class function TBoundingBox.None: TBoundingBox;
begin
  Result.Valid := false;
  Result.ImgWidth := 0;
  Result.ImgHeight := 0;
  Result.TopLeft := nil;
  Result.BottomLeft := nil;
  Result.TopRight := nil;
  Result.BottomRight := nil;
  Result.MinX := 0;
  Result.MaxX := 0;
  Result.MinY := 0;
  Result.MaxY := 0;
end;

procedure TBoundingBox.CalculateMinMaxValues;
begin
  if (TopLeft = nil) then
  begin
    TopLeft := TResultPointHelpers.CreateResultPoint(0, TopRight.Y);
    BottomLeft := TResultPointHelpers.CreateResultPoint(0, BottomRight.Y);
  end
  else if (TopRight = nil) then
  begin
    TopRight := TResultPointHelpers.CreateResultPoint(ImgWidth - 1, TopLeft.Y);
    BottomRight := TResultPointHelpers.CreateResultPoint(ImgWidth - 1,
      BottomLeft.Y);
  end;
  MinX := Trunc(Min(TopLeft.X, BottomLeft.X));
  MaxX := Trunc(Max(TopRight.X, BottomRight.X));
  MinY := Trunc(Min(TopLeft.Y, TopRight.Y));
  MaxY := Trunc(Max(BottomLeft.Y, BottomRight.Y));
end;

class function TBoundingBox.Create(imgWidth, imgHeight: Integer;
  const topLeft, bottomLeft, topRight, bottomRight: IResultPoint;
  out box: TBoundingBox): Boolean;
begin
  box := None;
  if ((topLeft = nil) and (topRight = nil)) or
    ((bottomLeft = nil) and (bottomRight = nil)) or
    ((topLeft <> nil) and (bottomLeft = nil)) or
    ((topRight <> nil) and (bottomRight = nil)) then
    exit(false);
  box.Valid := true;
  box.ImgWidth := imgWidth;
  box.ImgHeight := imgHeight;
  box.TopLeft := topLeft;
  box.BottomLeft := bottomLeft;
  box.TopRight := topRight;
  box.BottomRight := bottomRight;
  box.CalculateMinMaxValues;
  Result := true;
end;

class function TBoundingBox.Merge(const leftBox, rightBox: TBoundingBox;
  out box: TBoundingBox): Boolean;
begin
  if not leftBox.Valid then
  begin
    box := rightBox;
    exit(true);
  end;
  if not rightBox.Valid then
  begin
    box := leftBox;
    exit(true);
  end;
  Result := Create(leftBox.ImgWidth, leftBox.ImgHeight, leftBox.TopLeft,
    leftBox.BottomLeft, rightBox.TopRight, rightBox.BottomRight, box);
end;

class function TBoundingBox.AddMissingRows(const box: TBoundingBox;
  missingStartRows, missingEndRows: Integer; isLeft: Boolean;
  out res: TBoundingBox): Boolean;
begin
  var newTopLeft := box.TopLeft;
  var newBottomLeft := box.BottomLeft;
  var newTopRight := box.TopRight;
  var newBottomRight := box.BottomRight;

  if (missingStartRows > 0) then
  begin
    var top := box.TopRight;
    if isLeft then
      top := box.TopLeft;
    var newMinY := Max(0, Trunc(top.Y) - missingStartRows);
    var newTop := TResultPointHelpers.CreateResultPoint(top.X, newMinY);
    if isLeft then
      newTopLeft := newTop
    else
      newTopRight := newTop;
  end;

  if (missingEndRows > 0) then
  begin
    var bottom := box.BottomRight;
    if isLeft then
      bottom := box.BottomLeft;
    var newMaxY := Min(box.ImgHeight - 1, Trunc(bottom.Y) + missingEndRows);
    var newBottom := TResultPointHelpers.CreateResultPoint(bottom.X, newMaxY);
    if isLeft then
      newBottomLeft := newBottom
    else
      newBottomRight := newBottom;
  end;

  Result := Create(box.ImgWidth, box.ImgHeight, newTopLeft, newBottomLeft,
    newTopRight, newBottomRight, res);
end;

{ TDetectionResultColumn }

constructor TDetectionResultColumn.Create(const boundingBox: TBoundingBox;
  rowIndicator: TRowIndicator);
begin
  inherited Create;
  Self.BoundingBox := boundingBox;
  Self.RowIndicator := rowIndicator;
  if (boundingBox.MaxY < boundingBox.MinY) then
    raise EArgumentException.Create('Invalid bounding box');
  SetLength(Codewords, boundingBox.MaxY - boundingBox.MinY + 1);
  for var i := 0 to High(Codewords) do
    Codewords[i] := TPDFCodeword.None;
end;

function TDetectionResultColumn.IsRowIndicator: Boolean;
begin
  Result := (RowIndicator <> riNone);
end;

function TDetectionResultColumn.IsLeftRowIndicator: Boolean;
begin
  Result := (RowIndicator = riLeft);
end;

function TDetectionResultColumn.ImageRowToCodewordIndex(imageRow: Integer)
  : Integer;
begin
  Result := imageRow - BoundingBox.MinY;
end;

procedure TDetectionResultColumn.SetCodeword(imageRow: Integer;
  const codeword: TPDFCodeword);
begin
  Codewords[ImageRowToCodewordIndex(imageRow)] := codeword;
end;

function TDetectionResultColumn.Codeword(imageRow: Integer): TPDFCodeword;
begin
  Result := Codewords[ImageRowToCodewordIndex(imageRow)];
end;

function TDetectionResultColumn.CodewordNearby(imageRow: Integer)
  : TPDFCodeword;
begin
  var index := ImageRowToCodewordIndex(imageRow);
  if Codewords[index].Valid then
    exit(Codewords[index]);
  for var i := 1 to MAX_NEARBY_DISTANCE - 1 do
  begin
    var nearImageRow := index - i;
    if (nearImageRow >= 0) and Codewords[nearImageRow].Valid then
      exit(Codewords[nearImageRow]);
    nearImageRow := index + i;
    if (nearImageRow < Length(Codewords)) and Codewords[nearImageRow].Valid
    then
      exit(Codewords[nearImageRow]);
  end;
  Result := TPDFCodeword.None;
end;

procedure TDetectionResultColumn.SetRowNumbers;
begin
  for var i := 0 to High(Codewords) do
    if Codewords[i].Valid then
      Codewords[i].SetRowNumberAsRowIndicatorColumn;
end;

/// <summary>Removes the codewords that do not match the metadata.</summary>
procedure RemoveIncorrectCodewords(isLeft: Boolean;
  var codewords: TArray<TPDFCodeword>; const metadata: TBarcodeMetadata);
begin
  for var i := 0 to High(codewords) do
  begin
    if not codewords[i].Valid then
      continue;
    var rowIndicatorValue := codewords[i].Value mod 30;
    var codewordRowNumber := codewords[i].RowNumber;
    if (codewordRowNumber > metadata.RowCount) then
    begin
      codewords[i].Valid := false;
      continue;
    end;
    if not isLeft then
      Inc(codewordRowNumber, 2);
    case codewordRowNumber mod 3 of
      0:
        if (rowIndicatorValue * 3 + 1 <> metadata.RowCountUpperPart) then
          codewords[i].Valid := false;
      1:
        if (rowIndicatorValue div 3 <> metadata.ErrorCorrectionLevel) or
          (rowIndicatorValue mod 3 <> metadata.RowCountLowerPart) then
          codewords[i].Valid := false;
      2:
        if (rowIndicatorValue + 1 <> metadata.ColumnCount) then
          codewords[i].Valid := false;
    end;
  end;
end;

procedure TDetectionResultColumn.AdjustCompleteIndicatorColumnRowNumbers
  (const metadata: TBarcodeMetadata);
begin
  if not IsRowIndicator then
    exit;
  SetRowNumbers;
  RemoveIncorrectCodewords(IsLeftRowIndicator, Codewords, metadata);
  var top := BoundingBox.TopRight;
  var bottom := BoundingBox.BottomRight;
  if IsLeftRowIndicator then
  begin
    top := BoundingBox.TopLeft;
    bottom := BoundingBox.BottomLeft;
  end;
  var firstRow := ImageRowToCodewordIndex(Trunc(top.Y));
  var lastRow := ImageRowToCodewordIndex(Trunc(bottom.Y));
  // careful with the average row height: a skewed symbol has smaller and
  // taller rows
  var barcodeRow := -1;
  var maxRowHeight := 1;
  var currentRowHeight := 0;
  var increment := 1;
  for var codewordsRow := firstRow to lastRow - 1 do
  begin
    if not Codewords[codewordsRow].Valid then
      continue;
    var cw := Codewords[codewordsRow];
    if (barcodeRow = -1) and (cw.RowNumber = metadata.RowCount - 1) then
    begin
      increment := -1;
      barcodeRow := metadata.RowCount;
    end;

    var rowDifference := cw.RowNumber - barcodeRow;
    if (rowDifference = 0) then
      Inc(currentRowHeight)
    else if (rowDifference = increment) then
    begin
      maxRowHeight := Max(maxRowHeight, currentRowHeight);
      currentRowHeight := 1;
      barcodeRow := cw.RowNumber;
    end
    else if (rowDifference < 0) or (cw.RowNumber >= metadata.RowCount) or
      (rowDifference > codewordsRow) then
      Codewords[codewordsRow].Valid := false
    else
    begin
      var checkedRows: Integer;
      if (maxRowHeight > 2) then
        checkedRows := (maxRowHeight - 2) * rowDifference
      else
        checkedRows := rowDifference;
      var closePreviousCodewordFound := (checkedRows >= codewordsRow);
      var i := 1;
      while (i <= checkedRows) and not closePreviousCodewordFound do
      begin
        // there must be (height * rowDifference) codewords missing; for
        // now height = 1
        closePreviousCodewordFound := Codewords[codewordsRow - i].Valid;
        Inc(i);
      end;
      if closePreviousCodewordFound then
        Codewords[codewordsRow].Valid := false
      else
      begin
        barcodeRow := cw.RowNumber;
        currentRowHeight := 1;
      end;
    end;
  end;
end;

procedure TDetectionResultColumn.AdjustIncompleteIndicatorColumnRowNumbers
  (const metadata: TBarcodeMetadata);
begin
  if not IsRowIndicator then
    exit;
  var top := BoundingBox.TopRight;
  var bottom := BoundingBox.BottomRight;
  if IsLeftRowIndicator then
  begin
    top := BoundingBox.TopLeft;
    bottom := BoundingBox.BottomLeft;
  end;
  var firstRow := ImageRowToCodewordIndex(Trunc(top.Y));
  var lastRow := ImageRowToCodewordIndex(Trunc(bottom.Y));
  var barcodeRow := -1;
  var maxRowHeight := 1;
  var currentRowHeight := 0;
  for var codewordsRow := firstRow to lastRow - 1 do
  begin
    if not Codewords[codewordsRow].Valid then
      continue;
    Codewords[codewordsRow].SetRowNumberAsRowIndicatorColumn;
    var rowNumber := Codewords[codewordsRow].RowNumber;
    var rowDifference := rowNumber - barcodeRow;
    if (rowDifference = 0) then
      Inc(currentRowHeight)
    else if (rowDifference = 1) then
    begin
      maxRowHeight := Max(maxRowHeight, currentRowHeight);
      currentRowHeight := 1;
      barcodeRow := rowNumber;
    end
    else if (rowNumber >= metadata.RowCount) then
      Codewords[codewordsRow].Valid := false
    else
    begin
      barcodeRow := rowNumber;
      currentRowHeight := 1;
    end;
  end;
end;

function TDetectionResultColumn.GetRowHeights(out heights
  : TArray<Integer>): Boolean;
begin
  heights := nil;
  var metadata: TBarcodeMetadata;
  if not GetBarcodeMetadata(metadata) then
    exit(false);
  AdjustIncompleteIndicatorColumnRowNumbers(metadata);
  SetLength(heights, metadata.RowCount);
  for var i := 0 to High(heights) do
    heights[i] := 0;
  for var cw in Codewords do
    if cw.Valid then
    begin
      // more rows than the metadata allows for are ignored
      if (cw.RowNumber < 0) or (cw.RowNumber >= Length(heights)) then
        continue;
      Inc(heights[cw.RowNumber]);
    end;
  Result := true;
end;

function TDetectionResultColumn.GetBarcodeMetadata(out metadata
  : TBarcodeMetadata): Boolean;
begin
  Result := false;
  if not IsRowIndicator then
    exit;
  var columnCount, rowCountUpperPart, rowCountLowerPart, ecLevel: TBarcodeValue;
  for var i := 0 to High(Codewords) do
  begin
    if not Codewords[i].Valid then
      continue;
    Codewords[i].SetRowNumberAsRowIndicatorColumn;
    var rowIndicatorValue := Codewords[i].Value mod 30;
    var codewordRowNumber := Codewords[i].RowNumber;
    if not IsLeftRowIndicator then
      Inc(codewordRowNumber, 2);
    case codewordRowNumber mod 3 of
      0:
        rowCountUpperPart.SetValue(rowIndicatorValue * 3 + 1);
      1:
        begin
          ecLevel.SetValue(rowIndicatorValue div 3);
          rowCountLowerPart.SetValue(rowIndicatorValue mod 3);
        end;
      2:
        columnCount.SetValue(rowIndicatorValue + 1);
    end;
  end;
  var cc := columnCount.Value;
  var rcu := rowCountUpperPart.Value;
  var rcl := rowCountLowerPart.Value;
  var ec := ecLevel.Value;
  if (cc = nil) or (rcu = nil) or (rcl = nil) or (ec = nil) or (cc[0] < 1) or
    (rcu[0] + rcl[0] < MIN_ROWS_IN_BARCODE) or
    (rcu[0] + rcl[0] > MAX_ROWS_IN_BARCODE) then
    exit;
  metadata.ColumnCount := cc[0];
  metadata.RowCountUpperPart := rcu[0];
  metadata.RowCountLowerPart := rcl[0];
  metadata.ErrorCorrectionLevel := ec[0];
  RemoveIncorrectCodewords(IsLeftRowIndicator, Codewords, metadata);
  Result := true;
end;

{ TDetectionResult }

procedure TDetectionResult.Init(const metadata: TBarcodeMetadata;
  const boundingBox: TBoundingBox);
begin
  Self.Metadata := metadata;
  Self.BoundingBox := boundingBox;
  SetLength(Columns, metadata.ColumnCount + 2);
  for var i := 0 to High(Columns) do
    Columns[i] := nil;
end;

function TDetectionResult.BarcodeColumnCount: Integer;
begin
  Result := Metadata.ColumnCount;
end;

function TDetectionResult.BarcodeRowCount: Integer;
begin
  Result := Metadata.RowCount;
end;

function TDetectionResult.BarcodeECLevel: Integer;
begin
  Result := Metadata.ErrorCorrectionLevel;
end;

procedure AdjustRowNumbersFromBothRI(const columns
  : TArray<TDetectionResultColumn>);
begin
  var lri := columns[0];
  var rri := columns[High(columns)];
  if (lri = nil) or (rri = nil) then
    exit;
  for var codewordsRow := 0 to High(lri.Codewords) do
    if lri.Codewords[codewordsRow].Valid and rri.Codewords[codewordsRow].Valid
      and (lri.Codewords[codewordsRow].RowNumber = rri.Codewords[codewordsRow]
      .RowNumber) then
      for var c := 1 to High(columns) - 1 do
      begin
        var column := columns[c];
        if (column = nil) then
          continue;
        if column.Codewords[codewordsRow].Valid then
        begin
          column.Codewords[codewordsRow].RowNumber :=
            lri.Codewords[codewordsRow].RowNumber;
          if not column.Codewords[codewordsRow].HasValidRowNumber then
            column.Codewords[codewordsRow].Valid := false;
        end;
      end;
end;

function AdjustRowNumberIfValid(rowIndicatorRowNumber,
  invalidRowCounts: Integer; var codeword: TPDFCodeword): Integer;
begin
  if not codeword.HasValidRowNumber then
  begin
    if codeword.IsValidRowNumber(rowIndicatorRowNumber) then
    begin
      codeword.RowNumber := rowIndicatorRowNumber;
      invalidRowCounts := 0;
    end
    else
      Inc(invalidRowCounts);
  end;
  Result := invalidRowCounts;
end;

/// <summary>The row numbers from the left (or right) row indicator column;
/// the number of codewords left without a valid row number.</summary>
function AdjustRowNumbersFromRI(const columns: TArray<TDetectionResultColumn>;
  ri: TDetectionResultColumn): Integer;
begin
  Result := 0;
  if (ri = nil) then
    exit;
  for var codewordsRow := 0 to High(ri.Codewords) do
  begin
    if not ri.Codewords[codewordsRow].Valid then
      continue;
    var rowIndicatorRowNumber := ri.Codewords[codewordsRow].RowNumber;
    var invalidRowCounts := 0;
    var c := 1;
    while (c < High(columns)) and (invalidRowCounts < ADJUST_ROW_NUMBER_SKIP) do
    begin
      var column := columns[c];
      Inc(c);
      if (column = nil) then
        continue;
      if column.Codewords[codewordsRow].Valid then
      begin
        invalidRowCounts := AdjustRowNumberIfValid(rowIndicatorRowNumber,
          invalidRowCounts, column.Codewords[codewordsRow]);
        if not column.Codewords[codewordsRow].HasValidRowNumber then
          Inc(Result);
      end;
    end;
  end;
end;

function AdjustRowNumbersByRow(const columns
  : TArray<TDetectionResultColumn>): Integer;
begin
  AdjustRowNumbersFromBothRI(columns);
  Result := AdjustRowNumbersFromRI(columns, columns[0]) +
    AdjustRowNumbersFromRI(columns, columns[High(columns)]);
end;

/// <summary>True when the row number of codeword was set from
/// otherCodeword.</summary>
function AdjustRowNumber(var codeword: TPDFCodeword;
  const otherCodeword: TPDFCodeword): Boolean;
begin
  Result := codeword.Valid and otherCodeword.Valid and
    otherCodeword.HasValidRowNumber and (otherCodeword.Bucket = codeword.Bucket);
  if Result then
    codeword.RowNumber := otherCodeword.RowNumber;
end;

procedure AdjustRowNumbersOfCodeword(const columns
  : TArray<TDetectionResultColumn>; barcodeColumn, codewordsRow: Integer);
begin
  var codewords := columns[barcodeColumn].Codewords;
  var previous := columns[barcodeColumn - 1];
  var next := columns[barcodeColumn + 1];
  if (next = nil) then
    next := previous;
  if (previous = nil) then
    previous := next;

  var others: array [0 .. 13] of TPDFCodeword;
  for var i := 0 to High(others) do
    others[i] := TPDFCodeword.None;

  if (previous <> nil) then
  begin
    others[2] := previous.Codewords[codewordsRow];
    others[3] := next.Codewords[codewordsRow];
  end;
  if (codewordsRow > 0) then
  begin
    others[0] := codewords[codewordsRow - 1];
    if (previous <> nil) then
    begin
      others[4] := previous.Codewords[codewordsRow - 1];
      others[5] := next.Codewords[codewordsRow - 1];
    end;
  end;
  if (codewordsRow > 1) then
  begin
    others[8] := codewords[codewordsRow - 2];
    if (previous <> nil) then
    begin
      others[10] := previous.Codewords[codewordsRow - 2];
      others[11] := next.Codewords[codewordsRow - 2];
    end;
  end;
  if (codewordsRow < High(codewords)) then
  begin
    others[1] := codewords[codewordsRow + 1];
    if (previous <> nil) then
    begin
      others[6] := previous.Codewords[codewordsRow + 1];
      others[7] := next.Codewords[codewordsRow + 1];
    end;
  end;
  if (codewordsRow < High(codewords) - 1) then
  begin
    others[9] := codewords[codewordsRow + 2];
    if (previous <> nil) then
    begin
      others[12] := previous.Codewords[codewordsRow + 2];
      others[13] := next.Codewords[codewordsRow + 2];
    end;
  end;
  for var other in others do
    if AdjustRowNumber(columns[barcodeColumn].Codewords[codewordsRow], other)
    then
      exit;
end;

/// <summary>The number of codewords without a valid row number (counted
/// several times: only an indicator of when to stop).</summary>
function AdjustRowNumbers(const columns: TArray<TDetectionResultColumn>)
  : Integer;
begin
  Result := AdjustRowNumbersByRow(columns);
  if (Result = 0) then
    exit;
  for var barcodeColumn := 1 to High(columns) - 1 do
  begin
    if (columns[barcodeColumn] = nil) then
      continue;
    for var codewordsRow := 0 to High(columns[barcodeColumn].Codewords) do
      if columns[barcodeColumn].Codewords[codewordsRow].Valid and
        not columns[barcodeColumn].Codewords[codewordsRow].HasValidRowNumber
      then
        AdjustRowNumbersOfCodeword(columns, barcodeColumn, codewordsRow);
  end;
end;

function TDetectionResult.AllColumns: TArray<TDetectionResultColumn>;
begin
  if (Columns[0] <> nil) then
    Columns[0].AdjustCompleteIndicatorColumnRowNumbers(Metadata);
  if (Columns[High(Columns)] <> nil) then
    Columns[High(Columns)].AdjustCompleteIndicatorColumnRowNumbers(Metadata);
  var unadjustedCodewordCount := MAX_CODEWORDS_IN_BARCODE;
  var previousUnadjustedCount: Integer;
  repeat
    previousUnadjustedCount := unadjustedCodewordCount;
    unadjustedCodewordCount := AdjustRowNumbers(Columns);
  until not ((unadjustedCodewordCount > 0) and
    (unadjustedCodewordCount < previousUnadjustedCount));
  Result := Columns;
end;

{ scanning }

function AdjustCodewordStartColumn(image: TBitMatrix;
  minColumn, maxColumn: Integer; leftToRight: Boolean;
  codewordStartColumn, imageRow: Integer): Integer;
begin
  var correctedStartColumn := codewordStartColumn;
  var increment := 1;
  if leftToRight then
    increment := -1;
  // no black pixels before the start column, else start earlier
  for var i := 0 to 1 do
  begin
    while (correctedStartColumn >= minColumn) and
      (correctedStartColumn < maxColumn) and
      (leftToRight = image[correctedStartColumn, imageRow]) do
    begin
      if (Abs(codewordStartColumn - correctedStartColumn) > CODEWORD_SKEW_SIZE)
      then
        exit(codewordStartColumn);
      Inc(correctedStartColumn, increment);
    end;
    increment := -increment;
    leftToRight := not leftToRight;
  end;
  Result := correctedStartColumn;
end;

function GetModuleBitCount(image: TBitMatrix; minColumn, maxColumn: Integer;
  leftToRight: Boolean; startColumn, imageRow: Integer;
  out moduleBitCount: TModuleBitCountType): Boolean;
begin
  var imageColumn := startColumn;
  var moduleNumber := 0;
  var increment := 1;
  if not leftToRight then
    increment := -1;
  var previousPixelValue := leftToRight;
  for var i := 0 to BARS_IN_MODULE - 1 do
    moduleBitCount[i] := 0;
  while (imageColumn >= minColumn) and (imageColumn < maxColumn) and
    (moduleNumber < BARS_IN_MODULE) do
  begin
    if (image[imageColumn, imageRow] = previousPixelValue) then
    begin
      Inc(moduleBitCount[moduleNumber]);
      Inc(imageColumn, increment);
    end
    else
    begin
      Inc(moduleNumber);
      previousPixelValue := not previousPixelValue;
    end;
  end;
  var edge := minColumn;
  if leftToRight then
    edge := maxColumn;
  Result := (moduleNumber = BARS_IN_MODULE) or ((imageColumn = edge) and
    (moduleNumber = BARS_IN_MODULE - 1));
end;

function CheckCodewordSkew(codewordSize, minCodewordWidth,
  maxCodewordWidth: Integer): Boolean;
begin
  Result := (minCodewordWidth - CODEWORD_SKEW_SIZE <= codewordSize) and
    (codewordSize <= maxCodewordWidth + CODEWORD_SKEW_SIZE);
end;

function GetBitCountForCodeword(codeword: Integer): TModuleBitCountType;
begin
  for var k := 0 to BARS_IN_MODULE - 1 do
    Result[k] := 0;
  var previousValue := 0;
  var i := BARS_IN_MODULE - 1;
  while true do
  begin
    if ((codeword and 1) <> previousValue) then
    begin
      previousValue := codeword and 1;
      Dec(i);
      if (i < 0) then
        break;
    end;
    Inc(Result[i]);
    codeword := codeword shr 1;
  end;
end;

function GetCodewordBucketNumber(codeword: Integer): Integer;
begin
  var m := GetBitCountForCodeword(codeword);
  Result := (m[0] - m[2] + m[4] - m[6] + 9) mod 9;
end;

function DetectCodeword(image: TBitMatrix; minColumn, maxColumn: Integer;
  leftToRight: Boolean; startColumn, imageRow, minCodewordWidth,
  maxCodewordWidth: Integer): TPDFCodeword;
begin
  Result := TPDFCodeword.None;
  startColumn := AdjustCodewordStartColumn(image, minColumn, maxColumn,
    leftToRight, startColumn, imageRow);
  var moduleBitCount: TModuleBitCountType;
  if not GetModuleBitCount(image, minColumn, maxColumn, leftToRight,
    startColumn, imageRow, moduleBitCount) then
    exit;
  var codewordBitCount := 0;
  for var c in moduleBitCount do
    Inc(codewordBitCount, c);
  var endColumn: Integer;
  if leftToRight then
    endColumn := startColumn + codewordBitCount
  else
  begin
    for var k := 0 to BARS_IN_MODULE div 2 - 1 do
    begin
      var t := moduleBitCount[k];
      moduleBitCount[k] := moduleBitCount[BARS_IN_MODULE - 1 - k];
      moduleBitCount[BARS_IN_MODULE - 1 - k] := t;
    end;
    endColumn := startColumn;
    startColumn := endColumn - codewordBitCount;
  end;
  if not CheckCodewordSkew(codewordBitCount, minCodewordWidth,
    maxCodewordWidth) then
    exit;
  var decodedValue := GetDecodedValue(moduleBitCount);
  if (decodedValue <> -1) then
  begin
    var codeword := GetCodeword(decodedValue);
    if (codeword <> -1) then
      Result := TPDFCodeword.Create(startColumn, endColumn,
        GetCodewordBucketNumber(decodedValue), codeword);
  end;
end;

type
  /// <summary>Owns the columns created while decoding one symbol.</summary>
  TColumnStore = class(TObjectList<TDetectionResultColumn>)
  public
    function New(const boundingBox: TBoundingBox;
      rowIndicator: TRowIndicator): TDetectionResultColumn;
  end;

function TColumnStore.New(const boundingBox: TBoundingBox;
  rowIndicator: TRowIndicator): TDetectionResultColumn;
begin
  Result := TDetectionResultColumn.Create(boundingBox, rowIndicator);
  Add(Result);
end;

function GetRowIndicatorColumn(store: TColumnStore; image: TBitMatrix;
  const boundingBox: TBoundingBox; const startPoint: IResultPoint;
  leftToRight: Boolean; minCodewordWidth, maxCodewordWidth: Integer)
  : TDetectionResultColumn;
begin
  var ri := riRight;
  if leftToRight then
    ri := riLeft;
  Result := store.New(boundingBox, ri);
  for var i := 0 to 1 do
  begin
    var increment := 1;
    if (i = 1) then
      increment := -1;
    var startColumn := Trunc(startPoint.X);
    var imageRow := Trunc(startPoint.Y);
    while (imageRow <= boundingBox.MaxY) and (imageRow >= boundingBox.MinY) do
    begin
      var codeword := DetectCodeword(image, 0, image.Width, leftToRight,
        startColumn, imageRow, minCodewordWidth, maxCodewordWidth);
      if codeword.Valid then
      begin
        Result.SetCodeword(imageRow, codeword);
        if leftToRight then
          startColumn := codeword.StartX
        else
          startColumn := codeword.EndX;
      end;
      Inc(imageRow, increment);
    end;
  end;
end;

function GetBarcodeMetadata(leftRI, rightRI: TDetectionResultColumn;
  out metadata: TBarcodeMetadata): Boolean;
begin
  var leftMetadata: TBarcodeMetadata;
  if (leftRI = nil) or not leftRI.GetBarcodeMetadata(leftMetadata) then
    exit((rightRI <> nil) and rightRI.GetBarcodeMetadata(metadata));

  var rightMetadata: TBarcodeMetadata;
  if (rightRI = nil) or not rightRI.GetBarcodeMetadata(rightMetadata) then
  begin
    metadata := leftMetadata;
    exit(true);
  end;

  if (leftMetadata.ColumnCount <> rightMetadata.ColumnCount) and
    (leftMetadata.ErrorCorrectionLevel <> rightMetadata.ErrorCorrectionLevel)
    and (leftMetadata.RowCount <> rightMetadata.RowCount) then
    exit(false);
  metadata := leftMetadata;
  Result := true;
end;

function AdjustBoundingBox(ri: TDetectionResultColumn;
  out box: TBoundingBox): Boolean;
begin
  box := TBoundingBox.None;
  if (ri = nil) then
    exit(true);
  var rowHeights: TArray<Integer>;
  if not ri.GetRowHeights(rowHeights) then
    exit(true);
  var maxRowHeight := -1;
  for var h in rowHeights do
    maxRowHeight := Max(maxRowHeight, h);
  var missingStartRows := 0;
  for var h in rowHeights do
  begin
    Inc(missingStartRows, maxRowHeight - h);
    if (h > 0) then
      break;
  end;
  var row := 0;
  while (missingStartRows > 0) and (row < Length(ri.Codewords)) and
    not ri.Codewords[row].Valid do
  begin
    Dec(missingStartRows);
    Inc(row);
  end;
  var missingEndRows := 0;
  for var r := High(rowHeights) downto 0 do
  begin
    Inc(missingEndRows, maxRowHeight - rowHeights[r]);
    if (rowHeights[r] > 0) then
      break;
  end;
  row := High(ri.Codewords);
  while (missingEndRows > 0) and (row >= 0) and
    not ri.Codewords[row].Valid do
  begin
    Dec(missingEndRows);
    Dec(row);
  end;
  Result := TBoundingBox.AddMissingRows(ri.BoundingBox, missingStartRows,
    missingEndRows, ri.IsLeftRowIndicator, box);
end;

function Merge(leftRI, rightRI: TDetectionResultColumn;
  detectionResult: TDetectionResult): Boolean;
begin
  Result := false;
  if (leftRI = nil) and (rightRI = nil) then
    exit;
  var metadata: TBarcodeMetadata;
  if not GetBarcodeMetadata(leftRI, rightRI, metadata) then
    exit;
  var leftBox, rightBox, mergedBox: TBoundingBox;
  if AdjustBoundingBox(leftRI, leftBox) and
    AdjustBoundingBox(rightRI, rightBox) and
    TBoundingBox.Merge(leftBox, rightBox, mergedBox) then
  begin
    detectionResult.Init(metadata, mergedBox);
    Result := true;
  end;
end;

function IsValidBarcodeColumn(detectionResult: TDetectionResult;
  barcodeColumn: Integer): Boolean;
begin
  Result := (barcodeColumn >= 0) and
    (barcodeColumn <= detectionResult.BarcodeColumnCount + 1);
end;

function GetStartColumn(detectionResult: TDetectionResult;
  barcodeColumn, imageRow: Integer; leftToRight: Boolean): Integer;
begin
  var offset := -1;
  if leftToRight then
    offset := 1;
  var codeword := TPDFCodeword.None;
  if IsValidBarcodeColumn(detectionResult, barcodeColumn - offset) and
    (detectionResult.Columns[barcodeColumn - offset] <> nil) then
    codeword := detectionResult.Columns[barcodeColumn - offset]
      .Codeword(imageRow);
  if codeword.Valid then
    if leftToRight then
      exit(codeword.EndX)
    else
      exit(codeword.StartX);
  codeword := detectionResult.Columns[barcodeColumn].CodewordNearby(imageRow);
  if codeword.Valid then
    if leftToRight then
      exit(codeword.StartX)
    else
      exit(codeword.EndX);
  if IsValidBarcodeColumn(detectionResult, barcodeColumn - offset) and
    (detectionResult.Columns[barcodeColumn - offset] <> nil) then
    codeword := detectionResult.Columns[barcodeColumn - offset]
      .CodewordNearby(imageRow);
  if codeword.Valid then
    if leftToRight then
      exit(codeword.EndX)
    else
      exit(codeword.StartX);

  var skippedColumns := 0;
  while IsValidBarcodeColumn(detectionResult, barcodeColumn - offset) do
  begin
    Dec(barcodeColumn, offset);
    if (detectionResult.Columns[barcodeColumn] <> nil) then
      for var previousRowCodeword in detectionResult.Columns[barcodeColumn]
        .Codewords do
        if previousRowCodeword.Valid then
        begin
          if leftToRight then
            Result := previousRowCodeword.EndX
          else
            Result := previousRowCodeword.StartX;
          exit(Result + offset * skippedColumns *
            (previousRowCodeword.EndX - previousRowCodeword.StartX));
        end;
    Inc(skippedColumns);
  end;
  if leftToRight then
    Result := detectionResult.BoundingBox.MinX
  else
    Result := detectionResult.BoundingBox.MaxX;
end;

type
  TBarcodeMatrix = TArray<TArray<TBarcodeValue>>;

function CreateBarcodeMatrix(detectionResult: TDetectionResult)
  : TBarcodeMatrix;
begin
  SetLength(Result, detectionResult.BarcodeRowCount);
  for var r := 0 to High(Result) do
    SetLength(Result[r], detectionResult.BarcodeColumnCount + 2);
  var column := 0;
  for var resultColumn in detectionResult.AllColumns do
  begin
    if (resultColumn <> nil) then
      for var cw in resultColumn.Codewords do
        if cw.Valid and (cw.RowNumber >= 0) then
        begin
          // more rows than the metadata allows for are ignored
          if (cw.RowNumber >= Length(Result)) then
            continue;
          Result[cw.RowNumber][column].SetValue(cw.Value);
        end;
    Inc(column);
  end;
end;

function AdjustCodewordCount(detectionResult: TDetectionResult;
  var barcodeMatrix: TBarcodeMatrix): Boolean;
begin
  if (Length(barcodeMatrix) = 0) then
    exit(false);
  var numberOfCodewords := barcodeMatrix[0][1].Value;
  var calculatedNumberOfCodewords := detectionResult.BarcodeColumnCount *
    detectionResult.BarcodeRowCount -
    NumECCodewords(detectionResult.BarcodeECLevel);
  if (calculatedNumberOfCodewords < 1) or
    (calculatedNumberOfCodewords > MAX_CODEWORDS_IN_BARCODE) then
    calculatedNumberOfCodewords := 0;
  if (numberOfCodewords = nil) then
  begin
    if (calculatedNumberOfCodewords = 0) then
      exit(false);
    barcodeMatrix[0][1].SetValue(calculatedNumberOfCodewords);
  end
  else if (calculatedNumberOfCodewords <> 0) and
    (numberOfCodewords[0] <> calculatedNumberOfCodewords) then
    // the calculated one is more reliable (from the row indicators)
    barcodeMatrix[0][1].SetValue(calculatedNumberOfCodewords);
  Result := true;
end;

/// <summary>Decodes with the first of the ambiguous values, and with others
/// when that gives a checksum error (at most 100 tries).</summary>
function CreateDecoderResultFromAmbiguousValues(ecLevel: Integer;
  var codewords: TArray<Integer>; const erasureArray: TArray<Integer>;
  const ambiguousIndexes: TArray<Integer>;
  const ambiguousIndexValues: TArray<TArray<Integer>>; out error: string;
  out extra: IPDF417ResultMetadata): TDecoderResult;
begin
  Result := nil;
  error := 'Checksum';
  extra := nil;
  var ambiguousIndexCount: TArray<Integer>;
  SetLength(ambiguousIndexCount, Length(ambiguousIndexes));
  for var i := 0 to High(ambiguousIndexCount) do
    ambiguousIndexCount[i] := 0;

  var tries := 100;
  while (tries > 0) do
  begin
    Dec(tries);
    for var i := 0 to High(ambiguousIndexCount) do
      codewords[ambiguousIndexes[i]] := ambiguousIndexValues[i]
        [ambiguousIndexCount[i]];
    // (the error correction changes them)
    var tried := Copy(codewords);
    var res := DecodePDF417Codewords(tried, NumECCodewords(ecLevel),
      erasureArray, false, error, extra);
    if (res <> nil) or (error <> 'Checksum') then
      exit(res);

    if (Length(ambiguousIndexCount) = 0) then
      exit;
    for var i := 0 to High(ambiguousIndexCount) do
    begin
      if (ambiguousIndexCount[i] < High(ambiguousIndexValues[i])) then
      begin
        Inc(ambiguousIndexCount[i]);
        break;
      end
      else
      begin
        ambiguousIndexCount[i] := 0;
        if (i = High(ambiguousIndexCount)) then
          exit;
      end;
    end;
  end;
end;

function CreateDecoderResult(detectionResult: TDetectionResult;
  out error: string; out extra: IPDF417ResultMetadata): TDecoderResult;
begin
  Result := nil;
  error := '';
  extra := nil;
  var barcodeMatrix := CreateBarcodeMatrix(detectionResult);
  if not AdjustCodewordCount(detectionResult, barcodeMatrix) then
    exit;
  var erasures: TArray<Integer> := nil;
  var codewords: TArray<Integer>;
  SetLength(codewords, detectionResult.BarcodeRowCount *
    detectionResult.BarcodeColumnCount);
  for var i := 0 to High(codewords) do
    codewords[i] := 0;
  var ambiguousIndexValues: TArray<TArray<Integer>> := nil;
  var ambiguousIndexes: TArray<Integer> := nil;
  for var row := 0 to detectionResult.BarcodeRowCount - 1 do
    for var column := 0 to detectionResult.BarcodeColumnCount - 1 do
    begin
      var values := barcodeMatrix[row][column + 1].Value;
      var codewordIndex := row * detectionResult.BarcodeColumnCount + column;
      if (values = nil) then
        erasures := erasures + [codewordIndex]
      else if (Length(values) = 1) then
        codewords[codewordIndex] := values[0]
      else
      begin
        ambiguousIndexes := ambiguousIndexes + [codewordIndex];
        ambiguousIndexValues := ambiguousIndexValues + [values];
      end;
    end;
  Result := CreateDecoderResultFromAmbiguousValues
    (detectionResult.BarcodeECLevel, codewords, erasures, ambiguousIndexes,
    ambiguousIndexValues, error, extra);
end;

function DecodePDF417Scanning(image: TBitMatrix;
  const imageTopLeft, imageBottomLeft, imageTopRight,
  imageBottomRight: IResultPoint; minCodewordWidth, maxCodewordWidth: Integer;
  out error: string; out extra: IPDF417ResultMetadata;
  out approxSymbolWidth: Integer): TDecoderResult;
begin
  Result := nil;
  error := '';
  extra := nil;
  approxSymbolWidth := -1;
  var boundingBox: TBoundingBox;
  if not TBoundingBox.Create(image.Width, image.Height, imageTopLeft,
    imageBottomLeft, imageTopRight, imageBottomRight, boundingBox) then
    exit;

  var store := TColumnStore.Create(true);
  var detectionResult := TDetectionResult.Create;
  try
    var leftRI: TDetectionResultColumn := nil;
    var rightRI: TDetectionResultColumn := nil;
    for var i := 0 to 1 do
    begin
      if (imageTopLeft <> nil) then
        leftRI := GetRowIndicatorColumn(store, image, boundingBox,
          imageTopLeft, true, minCodewordWidth, maxCodewordWidth);
      if (imageTopRight <> nil) then
        rightRI := GetRowIndicatorColumn(store, image, boundingBox,
          imageTopRight, false, minCodewordWidth, maxCodewordWidth);
      if not Merge(leftRI, rightRI, detectionResult) then
        exit;
      if (i = 0) and detectionResult.BoundingBox.Valid and
        ((detectionResult.BoundingBox.MinY < boundingBox.MinY) or
        (detectionResult.BoundingBox.MaxY > boundingBox.MaxY)) then
        boundingBox := detectionResult.BoundingBox
      else
      begin
        detectionResult.BoundingBox := boundingBox;
        break;
      end;
    end;

    var maxBarcodeColumn := detectionResult.BarcodeColumnCount + 1;
    detectionResult.Columns[0] := leftRI;
    detectionResult.Columns[maxBarcodeColumn] := rightRI;

    var leftToRight := (leftRI <> nil);
    for var barcodeColumnCount := 1 to maxBarcodeColumn do
    begin
      var barcodeColumn := barcodeColumnCount;
      if not leftToRight then
        barcodeColumn := maxBarcodeColumn - barcodeColumnCount;
      // (the opposite row indicator column is not decoded again)
      if (detectionResult.Columns[barcodeColumn] <> nil) then
        continue;
      var ri := riNone;
      if (barcodeColumn = 0) then
        ri := riLeft
      else if (barcodeColumn = maxBarcodeColumn) then
        ri := riRight;
      detectionResult.Columns[barcodeColumn] := store.New(boundingBox, ri);
      var startColumn := -1;
      var previousStartColumn := startColumn;
      for var imageRow := boundingBox.MinY to boundingBox.MaxY do
      begin
        startColumn := GetStartColumn(detectionResult, barcodeColumn, imageRow,
          leftToRight);
        if (startColumn < 0) or (startColumn > boundingBox.MaxX) then
        begin
          if (previousStartColumn = -1) then
            continue;
          startColumn := previousStartColumn;
        end;
        var codeword := DetectCodeword(image, boundingBox.MinX,
          boundingBox.MaxX, leftToRight, startColumn, imageRow,
          minCodewordWidth, maxCodewordWidth);
        if codeword.Valid then
        begin
          detectionResult.Columns[barcodeColumn].SetCodeword(imageRow,
            codeword);
          previousStartColumn := startColumn;
          minCodewordWidth := Min(minCodewordWidth, codeword.Width);
          maxCodewordWidth := Max(maxCodewordWidth, codeword.Width);
        end;
      end;
    end;
    Result := CreateDecoderResult(detectionResult, error, extra);
    if (extra <> nil) then
      approxSymbolWidth := (detectionResult.BarcodeColumnCount + 2) *
        (minCodewordWidth + maxCodewordWidth) div 2;
  finally
    detectionResult.Free;
    store.Free;
  end;
end;

end.
