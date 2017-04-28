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

  * Original Author: bbrown@google.com (Brian Brown)
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.Internal.BitMatrixParser;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.DecodeHintType,
  ZXing.Common.BitMatrix,
  ZXing.QrCode.Internal.FormatInformation,
  ZXing.Common.Detector.MathUtils,
  ZXing.Datamatrix.Internal.Version;

type
  TBitMatrixParser = class sealed
  private
    mappingBitMatrix: TBitMatrix;
    readMappingMatrix: TBitMatrix;
    Fversion: TVersion;

    /// <summary>
    /// <p>Reads a bit of the mapping matrix accounting for boundary wrapping.</p>
    ///
    /// <param name="row">Row to read in the mapping matrix</param>
    /// <param name="column">Column to read in the mapping matrix</param>
    /// <param name="numRows">Number of rows in the mapping matrix</param>
    /// <param name="numColumns">Number of columns in the mapping matrix</param>
    /// <returns>value of the given bit in the mapping matrix</returns>
    /// </summary>
    function readModule(row, column, numRows, numColumns: Integer): Boolean;

    /// <summary>
    /// <p>Reads the 8 bits of the standard Utah-shaped pattern.</p>
    ///
    /// <p>See ISO 16022:2006, 5.8.1 Figure 6</p>
    ///
    /// <param name="row">Current row in the mapping matrix, anchored at the 8th bit (LSB) of the pattern</param>
    /// <param name="column">Current column in the mapping matrix, anchored at the 8th bit (LSB) of the pattern</param>
    /// <param name="numRows">Number of rows in the mapping matrix</param>
    /// <param name="numColumns">Number of columns in the mapping matrix</param>
    /// <returns>byte from the utah shape</returns>
    /// </summary>
    function readUtah(row, column, numRows, numColumns: Integer): Integer;

    /// <summary>
    /// <p>Reads the 8 bits of the special corner condition 1.</p>
    ///
    /// <p>See ISO 16022:2006, Figure F.3</p>
    ///
    /// <param name="numRows">Number of rows in the mapping matrix</param>
    /// <param name="numColumns">Number of columns in the mapping matrix</param>
    /// <returns>byte from the Corner condition 1</returns>
    /// </summary>
    function readCorner1(numRows, numColumns: Integer): Integer;

    /// <summary>
    /// <p>Reads the 8 bits of the special corner condition 2.</p>
    ///
    /// <p>See ISO 16022:2006, Figure F.4</p>
    ///
    /// <param name="numRows">Number of rows in the mapping matrix</param>
    /// <param name="numColumns">Number of columns in the mapping matrix</param>
    /// <returns>byte from the Corner condition 2</returns>
    /// </summary>
    function readCorner2(numRows, numColumns: Integer): Integer;

    /// <summary>
    /// <p>Reads the 8 bits of the special corner condition 3.</p>
    ///
    /// <p>See ISO 16022:2006, Figure F.5</p>
    ///
    /// <param name="numRows">Number of rows in the mapping matrix</param>
    /// <param name="numColumns">Number of columns in the mapping matrix</param>
    /// <returns>byte from the Corner condition 3</returns>
    /// </summary>
    function readCorner3(numRows, numColumns: Integer): Integer;

    /// <summary>
    /// <p>Reads the 8 bits of the special corner condition 4.</p>
    ///
    /// <p>See ISO 16022:2006, Figure F.6</p>
    ///
    /// <param name="numRows">Number of rows in the mapping matrix</param>
    /// <param name="numColumns">Number of columns in the mapping matrix</param>
    /// <returns>byte from the Corner condition 4</returns>
    /// </summary>
    function readCorner4(numRows, numColumns: Integer): Integer;

    /// <summary>
    /// <p>Creates the version object based on the dimension of the original bit matrix from
    /// the datamatrix code.</p>
    ///
    /// <p>See ISO 16022:2006 Table 7 - ECC 200 symbol attributes</p>
    ///
    /// <param name="bitMatrix">Original <see cref="BitMatrix" />including alignment patterns</param>
    /// <returns><see cref="Version" />encapsulating the Data Matrix Code's "version"</returns>
    /// <exception cref="FormatException">if the dimensions of the mapping matrix are not valid</exception>
    /// Data Matrix dimensions.
    /// </summary>
    function readVersion(BitMatrix: TBitMatrix): TVersion;
  public
    /// <param name="bitMatrix">{@link TBitMatrix} to parse</param>
    /// <throws>ReaderException if dimension is not >= 21 and 1 mod 4</throws>
    constructor Create(const BitMatrix: TBitMatrix);


    /// <summary>
    /// <p>Extracts the data region from a <see cref="BitMatrix" />that contains
    /// alignment patterns.</p>
    ///
    /// <param name="bitMatrix">Original <see cref="BitMatrix" />with alignment patterns</param>
    /// <returns>BitMatrix that has the alignment patterns removed</returns>
    /// </summary>
    function extractDataRegion(BitMatrix: TBitMatrix): TBitMatrix;

    /// <summary>
    /// <p>Reads the bits in the <see cref="BitMatrix" />representing the mapping matrix (No alignment patterns)
    /// in the correct order in order to reconstitute the codewords bytes contained within the
    /// Data Matrix Code.</p>
    ///
    /// <returns>bytes encoded within the Data Matrix Code</returns>
    /// <exception cref="FormatException">if the exact number of bytes expected is not read</exception>
    /// </summary>
    function readCodewords: TArray<Byte>;
    destructor Destroy; override;

    property Version: TVersion read Fversion;
  end;

implementation

{ TBitMatrixParser }

constructor TBitMatrixParser.Create(const BitMatrix: TBitMatrix);
var
  dimension: Integer;
begin
  dimension := BitMatrix.Height;
  if (((dimension >= 8) and (dimension <= 144)) and ((dimension and $01) = 0))
  then
  begin
    self.Fversion := readVersion(BitMatrix);
    if (self.Version <> nil) then
    begin
      self.mappingBitMatrix := extractDataRegion(BitMatrix);
      self.readMappingMatrix := TBitMatrix.Create(self.mappingBitMatrix.Width,
        self.mappingBitMatrix.Height)
    end;
  end;
end;

destructor TBitMatrixParser.Destroy;
begin
  self.mappingBitMatrix.Free;
  self.readMappingMatrix.Free;
  inherited;
end;



function TBitMatrixParser.readVersion(BitMatrix: TBitMatrix): TVersion;
begin
  Result := TVersion.getVersionForDimensions(BitMatrix.Height, BitMatrix.Width);
end;

function TBitMatrixParser.readCodewords: TArray<Byte>;
var
  resultOffset, row, column, numRows, numColumns: Integer;
  corner1Read, corner2Read, corner3Read, corner4Read: Boolean;
begin
  Result := TArray<Byte>.Create();
  SetLength(Result, Version.TotalCodewords);
  resultOffset := 0;

  row := 4;
  column := 0;

  numRows := self.mappingBitMatrix.Height;
  numColumns := self.mappingBitMatrix.Width;

  corner1Read := false;
  corner2Read := false;
  corner3Read := false;
  corner4Read := false;

  // Read all of the codewords
  while ((row < numRows) or (column < numColumns)) do
  begin
    // Check the four corner cases
    if ((row = numRows) and (column = 0) and (not corner1Read)) then
    begin
      Result[resultOffset] := Byte(readCorner1(numRows, numColumns));
      Inc(resultOffset);
      Dec(row, 2);
      Inc(column, 2);
      corner1Read := true;
    end
    else
    begin
      if ((row = (numRows - 2)) and (column = 0) and ((numColumns and $03) <> 0)
        and (not corner2Read)) then
      begin
        Result[resultOffset] := Byte(readCorner2(numRows, numColumns));
        Inc(resultOffset);
        Dec(row, 2);
        Inc(column, 2);
        corner2Read := true;
      end
      else
      begin
        if ((row = (numRows + 4)) and (column = 2) and
          ((numColumns and $07) = 0) and (not corner3Read)) then
        begin
          Result[resultOffset] := Byte(readCorner3(numRows, numColumns));
          Inc(resultOffset);
          Dec(row, 2);
          Inc(column, 2);
          corner3Read := true;
        end
        else
        begin
          if ((row = (numRows - 2)) and (column = 0) and
            ((numColumns and $07) = 4) and (not corner4Read)) then
          begin
            Result[resultOffset] := Byte(readCorner4(numRows, numColumns));
            Inc(resultOffset);
            Dec(row, 2);
            Inc(column, 2);
            corner4Read := true;
          end
          else
          begin
            // Sweep upward diagonally to the right
            while ((row >= 0) and (column < numColumns)) do
            begin
              if ((row < numRows) and (column >= 0) and
                (not readMappingMatrix[column, row])) then
              begin
                Result[resultOffset] :=
                  Byte(readUtah(row, column, numRows, numColumns));
                Inc(resultOffset);
              end;
              Dec(row, 2);
              Inc(column, 2)
            end;
            Inc(row);
            Inc(column, 3);

            // Sweep downward diagonally to the left
            while ((row < numRows) and (column >= 0)) do
            begin
              if ((row >= 0) and (column < numColumns) and
                (not readMappingMatrix[column, row])) then
              begin
                Result[resultOffset] :=
                  Byte(readUtah(row, column, numRows, numColumns));
                Inc(resultOffset);
              end;
              Inc(row, 2);
              Dec(column, 2)
            end;
            Inc(row, 3);
            Inc(column)
          end;
        end;
      end;
    end;
  end;

  if (resultOffset <> Version.TotalCodewords) then
    Result := nil;
end;

function TBitMatrixParser.readModule(row, column, numRows,
  numColumns: Integer): Boolean;
begin
  // Adjust the row and column indices based on boundary wrapping
  if (row < 0) then
  begin
    Inc(row, numRows);
    Inc(column, (4 - ((numRows + 4) and $07)))
  end;
  if (column < 0) then
  begin
    Inc(column, numColumns);
    Inc(row, (4 - ((numColumns + 4) and $07)))
  end;
  readMappingMatrix[column, row] := true;

  Result := mappingBitMatrix[column, row];
end;

function TBitMatrixParser.readUtah(row, column, numRows,
  numColumns: Integer): Integer;
var
  currentByte: Byte;
begin
  currentByte := 0;
  if (readModule((row - 2), (column - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((row - 2), (column - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((row - 1), (column - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((row - 1), (column - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((row - 1), column, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(row, (column - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(row, (column - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(row, column, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  Result := currentByte;
end;

function TBitMatrixParser.readCorner1(numRows, numColumns: Integer): Integer;
var
  currentByte: Byte;
begin
  currentByte := 0;
  if (readModule((numRows - 1), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 1), 1, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 1), 2, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(1, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(2, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(3, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  Result := currentByte;
end;

function TBitMatrixParser.readCorner2(numRows, numColumns: Integer): Integer;
var
  currentByte: Byte;
begin
  currentByte := 0;
  if (readModule((numRows - 3), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 2), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 1), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 4), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 3), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(1, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  Result := currentByte;
end;

function TBitMatrixParser.readCorner3(numRows, numColumns: Integer): Integer;
var
  currentByte: Byte;
begin
  currentByte := 0;
  if (readModule((numRows - 1), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 1), (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 3), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(1, (numColumns - 3), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(1, (numColumns - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(1, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  Result := currentByte;
end;

function TBitMatrixParser.readCorner4(numRows, numColumns: Integer): Integer;
var
  currentByte: Byte;
begin
  currentByte := 0;
  if (readModule((numRows - 3), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 2), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule((numRows - 1), 0, numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (self.readModule(0, (numColumns - 2), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(0, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(1, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(2, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  currentByte := (currentByte shl 1);
  if (readModule(3, (numColumns - 1), numRows, numColumns)) then
    currentByte := (currentByte or 1);

  Result := currentByte;
end;

function TBitMatrixParser.extractDataRegion(BitMatrix: TBitMatrix): TBitMatrix;
var
  i, j, symbolSizeRows, dataRegionSizeRows,
    dataRegionSizeColumns, numDataRegionsRow, numDataRegionsColumn,
    sizeDataRegionRow, sizeDataRegionColumn, dataRegionRow, dataRegionRowOffset,
    dataRegionColumn, dataRegionColumnOffset, readRowOffset, writeRowOffset,
    readColumnOffset, writeColumnOffset: Integer;
  bitMatrixWithoutAlignment: TBitMatrix;
begin
  symbolSizeRows := Version.symbolSizeRows;

  if (BitMatrix.Height <> symbolSizeRows) then
    raise EArgumentException.Create
      ('Dimension of bitMarix must match the version size');

  dataRegionSizeRows := Version.dataRegionSizeRows;
  dataRegionSizeColumns := Version.dataRegionSizeColumns;

  numDataRegionsRow := (symbolSizeRows div dataRegionSizeRows);
  numDataRegionsColumn := (Version.symbolSizeColumns div dataRegionSizeColumns);

  sizeDataRegionRow := (numDataRegionsRow * dataRegionSizeRows);
  sizeDataRegionColumn := (numDataRegionsColumn * dataRegionSizeColumns);

  bitMatrixWithoutAlignment := TBitMatrix.Create(sizeDataRegionColumn,
    sizeDataRegionRow);

  for dataRegionRow := 0 to Pred(numDataRegionsRow) do
  begin
    dataRegionRowOffset := (dataRegionRow * dataRegionSizeRows);
    for dataRegionColumn := 0 to Pred(numDataRegionsColumn) do
    begin
      dataRegionColumnOffset := (dataRegionColumn * dataRegionSizeColumns);
      for i := 0 to Pred(dataRegionSizeRows) do
      begin
        readRowOffset := (dataRegionRow * (dataRegionSizeRows + 2)) + 1 + i;
        writeRowOffset := (dataRegionRowOffset + i);
        for j := 0 to Pred(dataRegionSizeColumns) do
        begin
          readColumnOffset :=
            (dataRegionColumn * (dataRegionSizeColumns + 2)) + 1 + j;
          if (BitMatrix[readColumnOffset, readRowOffset]) then
          begin
            writeColumnOffset := (dataRegionColumnOffset + j);
            bitMatrixWithoutAlignment[writeColumnOffset,
              writeRowOffset] := true;
          end;
        end;
      end;
    end;
  end;

  Result := bitMatrixWithoutAlignment;

end;

end.
