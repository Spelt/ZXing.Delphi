unit BitMatrixParser;

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

uses SysUtils, Generics.Collections, DecodeHintType, BitMatrix, Version,
  FormatInformation, MathUtils, DataMask;

type

  TBitMatrixParser = class
  private

  var
    BitMatrix: TBitMatrix;
    mirrored: boolean;
    parsedVersion: TVersion;

    constructor Create(BitMatrix: TBitMatrix);
    function copyBit(i: Integer; j: Integer; versionBits: Integer): Integer;

  public
  var
    parsedFormatInfo: TFormatInformation;

    class function createBitMatrixParser(BitMatrix: TBitMatrix)
      : TBitMatrixParser; static;
    procedure mirror;
    procedure remask;
    procedure setMirror(mirror: boolean);
    function readFormatInformation: TFormatInformation;
    function readVersion: TVersion;
    function readCodewords: TArray<Byte>;

  end;

implementation

{ TBitMatrixParser }

function TBitMatrixParser.copyBit(i, j, versionBits: Integer): Integer;
var
  bit: boolean;
begin

  if mirrored then
  begin
    bit := self.BitMatrix[j, i];
  end
  else
  begin
    bit := self.BitMatrix[i, j]
  end;

  if (bit) then
  begin
    result := ((versionBits shl 1) or $1)
  end
  else
  begin
    result := (versionBits shl 1);
  end;

end;

constructor TBitMatrixParser.Create(BitMatrix: TBitMatrix);
begin
  self.BitMatrix := BitMatrix
end;

class function TBitMatrixParser.createBitMatrixParser(BitMatrix: TBitMatrix)
  : TBitMatrixParser;
var
  dimension: Integer;
begin
  dimension := BitMatrix.Height;
  if ((dimension >= $15) and ((dimension and 3) = 1)) then
  begin
    result := TBitMatrixParser.Create(BitMatrix);
    exit
  end;

  result := nil;
end;

procedure TBitMatrixParser.mirror;
var
  x, y: Integer;
begin
  x := 0;
  while ((x < self.BitMatrix.Width)) do
  begin
    y := (x + 1);
    while ((y < self.BitMatrix.Height)) do
    begin
      if (self.BitMatrix[x, y] <> self.BitMatrix[y, x]) then
      begin
        self.BitMatrix.flip(y, x);
        self.BitMatrix.flip(x, y)
      end;
      inc(y)
    end;
    inc(x)
  end
end;

function TBitMatrixParser.readCodewords: TArray<Byte>;
var
  formatInfo: TFormatInformation;
  dimension, resultOffset, currentByte, bitsRead, j, i, count, col: Integer;
  functionPattern: TBitMatrix;
  readingUp: boolean;
  Version: TVersion;
  DataMask: TDataMask;

begin

  formatInfo := self.readFormatInformation;
  if (formatInfo = nil) then
  begin
    result := nil;
    exit
  end;

  Version := self.readVersion;
  if (Version = nil) then
  begin
    result := nil;
    exit
  end;

  DataMask := TDataMask.forReference(formatInfo.DataMask);
  dimension := BitMatrix.Height;
  DataMask.unmaskBitMatrix(self.BitMatrix, dimension);

  functionPattern := Version.buildFunctionPattern;
  readingUp := true;
  result := TArray<Byte>.Create();
  SetLength(result, Version.TotalCodewords);
  resultOffset := 0;
  currentByte := 0;
  bitsRead := 0;
  j := (dimension - 1);

  while ((j > 0)) do
  begin

    if (j = 6) then
      dec(j);
    count := 0;
    while ((count < dimension)) do
    begin

      if readingUp then
        i := ((dimension - 1) - count)
      else
        i := count;

      col := 0;
      while ((col < 2)) do
      begin
        if (not functionPattern[(j - col), i]) then
        begin
          inc(bitsRead);
          currentByte := (currentByte shl 1);
          if (self.BitMatrix[(j - col), i]) then
            currentByte := (currentByte or 1);
          if (bitsRead = 8) then
          begin
            result[resultOffset] := Byte(currentByte);
            inc(resultOffset);
            bitsRead := 0;
            currentByte := 0
          end
        end;
        inc(col)
      end;
      inc(count)
    end;

    readingUp := not readingUp;
    dec(j, 2);

  end;

  if (resultOffset <> Version.TotalCodewords) then
  begin
    result := nil;
  end;

end;

function TBitMatrixParser.readFormatInformation: TFormatInformation;
var
  i, formatInfoBits1, formatInfoBits2, dimension, j, jMin: Integer;
begin
  if (self.parsedFormatInfo <> nil) then
  begin
    result := self.parsedFormatInfo;
    exit
  end;

  formatInfoBits1 := 0;
  i := 0;
  while ((i < 6)) do
  begin
    formatInfoBits1 := self.copyBit(i, 8, formatInfoBits1);
    inc(i)
  end;

  formatInfoBits1 := self.copyBit(7, 8, formatInfoBits1);
  formatInfoBits1 := self.copyBit(8, 8, formatInfoBits1);
  formatInfoBits1 := self.copyBit(8, 7, formatInfoBits1);
  j := 5;
  while ((j >= 0)) do
  begin
    formatInfoBits1 := self.copyBit(8, j, formatInfoBits1);
    dec(j)
  end;

  dimension := self.BitMatrix.Height;
  formatInfoBits2 := 0;
  jMin := (dimension - 7);
  j := (dimension - 1);
  while ((j >= jMin)) do
  begin
    formatInfoBits2 := self.copyBit(8, j, formatInfoBits2);
    dec(j)
  end;

  i := (dimension - 8);
  while ((i < dimension)) do
  begin
    formatInfoBits2 := self.copyBit(i, 8, formatInfoBits2);
    inc(i)
  end;

  self.parsedFormatInfo := TFormatInformation.DecodeFormatInformation
    (formatInfoBits1, formatInfoBits2);

  if (self.parsedFormatInfo <> nil) then
  begin
    result := self.parsedFormatInfo;
    exit
  end;

  result := nil;
end;

function TBitMatrixParser.readVersion: TVersion;
var
  dimension, provisionalVersion, versionBits, i, j, ijMin: Integer;
begin

  if (self.parsedVersion <> nil) then
  begin
    result := self.parsedVersion;
    exit
  end;

  dimension := self.BitMatrix.Height;
  provisionalVersion := TMathUtils.Asr(dimension - $11, 2);
  if (provisionalVersion <= 6) then
  begin
    result := TVersion.getVersionForNumber(provisionalVersion);
    exit
  end;

  versionBits := 0;
  ijMin := (dimension - 11);
  j := 5;
  while ((j >= 0)) do
  begin
    i := (dimension - 9);
    while ((i >= ijMin)) do
    begin
      versionBits := self.copyBit(i, j, versionBits);
      dec(i)
    end;
    dec(j)
  end;

  self.parsedVersion := TVersion.decodeVersionInformation(versionBits);
  if ((self.parsedVersion <> nil) and
    (self.parsedVersion.DimensionForVersion = dimension)) then
  begin
    result := self.parsedVersion;
    exit
  end;

  versionBits := 0;
  i := 5;
  while ((i >= 0)) do
  begin
    j := (dimension - 9);
    while ((j >= ijMin)) do
    begin
      versionBits := self.copyBit(i, j, versionBits);
      dec(j)
    end;
    dec(i)
  end;

  self.parsedVersion := TVersion.decodeVersionInformation(versionBits);
  if ((self.parsedVersion <> nil) and
    (self.parsedVersion.DimensionForVersion = dimension)) then
  begin
    result := self.parsedVersion;
    exit
  end;

  result := nil;
end;

procedure TBitMatrixParser.remask;
var
  dimension: Integer;
begin
  if (self.parsedFormatInfo <> nil) then
  begin
    dimension := self.BitMatrix.Height;
    TDataMask.forReference(self.parsedFormatInfo.DataMask)
      .unmaskBitMatrix(self.BitMatrix, dimension)
  end

end;

procedure TBitMatrixParser.setMirror(mirror: boolean);
begin
  self.parsedVersion := nil;
  self.parsedFormatInfo := nil;
  self.mirrored := mirror
end;

end.
