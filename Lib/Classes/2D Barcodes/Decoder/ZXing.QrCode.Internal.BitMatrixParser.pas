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

  * Original Author: Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.BitMatrixParser;

interface

uses 
  SysUtils, 
  Generics.Collections, 
  ZXing.DecodeHintType,
  ZXing.Common.BitMatrix, 
  ZXing.QrCode.Internal.Version,
  ZXing.QrCode.Internal.FormatInformation, 
  ZXing.Common.Detector.MathUtils, 
  ZXing.QrCode.Internal.DataMask;

type
  TBitMatrixParser = class
  private
    BitMatrix: TBitMatrix;
    mirrored: boolean;
    parsedVersion: TVersion;

    function copyBit(const i, j, versionBits: Integer): Integer;
  public
    parsedFormatInfo: TFormatInformation;

    constructor Create(const BitMatrix: TBitMatrix);

    /// <param name="bitMatrix">{@link TBitMatrix} to parse</param>
    /// <throws>ReaderException if dimension is not >= 21 and 1 mod 4</throws>
    class function createBitMatrixParser(
      const bitMatrix: TBitMatrix): TBitMatrixParser; static;

    (** Mirror the bit matrix in order to attempt a second reading. *)
    procedure mirror;

    (**
      * Revert the mask removal done while reading the code words. The bit matrix should revert to its original state.
      *)
    procedure remask;
    (**
      * Prepare the parser for a mirrored operation.
      * This flag has effect only on the {@link #readFormatInformation()} and the
      * {@link #readVersion()}. Before proceeding with {@link #readCodewords()} the
      * {@link #mirror()} method should be called.
      *
      * @param mirror Whether to read version and format information mirrored.
      *)
    procedure setMirror(const mirror: Boolean);

    /// <summary> <p>Reads format information from one of its two locations within the QR Code.</p>
    ///
    /// </summary>
    /// <returns> {@link FormatInformation} encapsulating the QR Code's format info
    /// </returns>
    /// <throws>  ReaderException if both format information locations cannot be parsed as </throws>
    /// <summary> the valid encoding of format information
    /// </summary>
    function readFormatInformation: TFormatInformation;

    /// <summary> <p>Reads version information from one of its two locations within the QR Code.</p>
    ///
    /// </summary>
    /// <returns> {@link Version} encapsulating the QR Code's version
    /// </returns>
    /// <throws>  ReaderException if both version information locations cannot be parsed as </throws>
    /// <summary> the valid encoding of version information
    /// </summary>
    function readVersion: TVersion;

    // <summary> <p>Reads the bits in the {@link BitMatrix} representing the finder pattern in the
    // correct order in order to reconstruct the codewords bytes contained within the
    /// QR Code.</p>
    ///
    /// </summary>
    /// <returns> bytes encoded within the QR Code
    /// </returns>
    /// <throws>  ReaderException if the exact number of bytes expected is not read </throws>
    function readCodewords: TArray<Byte>;
  end;

implementation

{ TBitMatrixParser }

constructor TBitMatrixParser.Create(const bitMatrix: TBitMatrix);
begin
  // Should only be called from createBitMatrixParser with the important checks before
  Self.bitMatrix := BitMatrix;
end;

class function TBitMatrixParser.createBitMatrixParser(
  const bitMatrix: TBitMatrix): TBitMatrixParser;
var
  dimension: Integer;
begin
  dimension := BitMatrix.Height;
  if ((dimension < 21) and ((dimension and $03) <> 1))
  then
     Result := nil
  else
     Result := TBitMatrixParser.Create(bitMatrix);
end;

function TBitMatrixParser.copyBit(const i, j, versionBits: Integer): Integer;
var
  bit: Boolean;
begin
  if (mirrored)
  then
     bit := BitMatrix[j, i]
  else
     bit := BitMatrix[i, j];

  if (bit)
  then
     Result := ((versionBits shl 1) or $1)
  else
     Result := (versionBits shl 1);
end;

procedure TBitMatrixParser.mirror;
var
  x, y: Integer;
begin
  for x := 0 to Pred(bitMatrix.width) do
    for y := (x + 1) to Pred(bitMatrix.height) do
      if (bitMatrix[x, y] <> bitMatrix[y, x]) then
      begin
        bitMatrix.flip(y, x);
        bitMatrix.flip(x, y);
      end;
end;

function TBitMatrixParser.readCodewords: TArray<Byte>;
var
  formatInfo: TFormatInformation;
  dimension, resultOffset,
  currentByte, bitsRead,
  j, i, count, col: Integer;
  functionPattern: TBitMatrix;
  readingUp: boolean;
  Version: TVersion;
  DataMask: TDataMask;
begin
  Result := nil;

  formatInfo := readFormatInformation;
  if (formatInfo = nil)
  then
     exit;

  Version := readVersion;
  if (Version = nil)
  then
     exit;

  // Get the data mask for the format used in this QR Code. This will exclude
  // some bits from reading as we wind through the bit matrix.
  DataMask := TDataMask.forReference(formatInfo.DataMask);
  dimension := bitMatrix.Height;
  DataMask.unmaskBitMatrix(BitMatrix, dimension);

  functionPattern := Version.buildFunctionPattern;

  try
    readingUp := true;
    Result := TArray<Byte>.Create();
    SetLength(Result, version.TotalCodewords);
    resultOffset := 0;
    currentByte := 0;
    bitsRead := 0;
    j := (dimension - 1);
    // Read columns in pairs, from right to left
    while ((j > 0)) do
    begin
      if (j = 6)
      then
         // Skip whole column with vertical alignment pattern;
         // saves time and makes the other code proceed more cleanly
         Dec(j);
      // Read alternatingly from bottom to top then top to bottom
      for count := 0 to Pred(dimension) do
      begin
        if (readingUp)
        then
           i := ((dimension - 1) - count)
        else
           i := count;
        for col := 0 to Pred(2) do
        begin
          // Ignore bits covered by the function pattern
          if (not functionPattern[(j - col), i]) then
          begin
            // Read a bit
            Inc(bitsRead);
            currentByte := (currentByte shl 1);
            if (bitMatrix[(j - col), i])
            then
               currentByte := (currentByte or 1);
            // If we've made a whole byte, save it off
            if (bitsRead = 8) then
            begin
//              if resultoffset > high(Result) then
//                 raise EProgrammerNotFound.CreateFmt('resultoffset %d> high(Result) %d',[resultOffset,high(Result)])
//              else if resultoffset < 0 then
//                 raise EProgrammerNotFound.CreateFmt('resultoffset %d<0!',[resultOffset]);

              if (resultoffset>=0) and  (resultoffset <= high(Result)) then // minchiata aggiunta da carlo
                  Result[resultOffset] := Byte(currentByte);

              Inc(resultOffset);
              bitsRead := 0;
              currentByte := 0;
            end
          end;
        end;
      end;
      readingUp := not readingUp; // readingUp = !readingUp; // switch directions
      Dec(j, 2);
    end;

    if (resultOffset <> version.TotalCodewords)
    then
       Result := nil;
  finally
    FreeAndNil(functionPattern);
  end;
end;

function TBitMatrixParser.readFormatInformation: TFormatInformation;
var
  formatInfoBits1,
  formatInfoBits2,
  dimension,
  i, j, jMin: Integer;
begin
  if (Self.parsedFormatInfo <> nil) then
  begin
    Result := Self.parsedFormatInfo;
    exit;
  end;

  // Read top-left format info bits
  formatInfoBits1 := 0;
  for i := 0 to Pred(6) do
    formatInfoBits1 := self.copyBit(i, 8, formatInfoBits1);
  // .. and skip a bit in the timing pattern ...
  formatInfoBits1 := self.copyBit(7, 8, formatInfoBits1);
  formatInfoBits1 := self.copyBit(8, 8, formatInfoBits1);
  formatInfoBits1 := self.copyBit(8, 7, formatInfoBits1);
  // .. and skip a bit in the timing pattern ...
  for j := 5 downto 0 do
    formatInfoBits1 := self.copyBit(8, j, formatInfoBits1);
  // Read the top-right/bottom-left pattern too
  dimension := Self.bitMatrix.Height;
  formatInfoBits2 := 0;
  jMin := (dimension - 7);
  for j := (dimension - 1) downto jMin do
    formatInfoBits2 := self.copyBit(8, j, formatInfoBits2);
  for i := (dimension - 8) to Pred(dimension) do
    formatInfoBits2 := self.copyBit(i, 8, formatInfoBits2);

  parsedFormatInfo := TFormatInformation.DecodeFormatInformation(formatInfoBits1, formatInfoBits2);

  if (parsedFormatInfo <> nil)
  then
     Result := Self.parsedFormatInfo
  else
     Result := nil;
end;

function TBitMatrixParser.readVersion: TVersion;
var
  dimension,
  provisionalVersion,
  versionBits,
  i, j, ijMin: Integer;
begin
  if (Self.parsedVersion <> nil) then
  begin
    Result := Self.parsedVersion;
    exit;
  end;

  dimension := Self.bitMatrix.Height;
  provisionalVersion := TMathUtils.Asr((dimension - 17), 2);
  if (provisionalVersion <= 6) then
  begin
    Result := TVersion.getVersionForNumber(provisionalVersion);
    exit;
  end;

  // Read top-right version info: 3 wide by 6 tall
  versionBits := 0;
  ijMin := (dimension - 11);
  for j := 5 downto 0 do
    for i := (dimension - 9) downto ijMin do
      versionBits := Self.copyBit(i, j, versionBits);

  parsedVersion := TVersion.decodeVersionInformation(versionBits);
  if ((parsedVersion <> nil) and
      (parsedVersion.DimensionForVersion = dimension)) then
  begin
    Result := parsedVersion;
    exit;
  end;

  // Hmm, failed. Try bottom left: 6 wide by 3 tall
  versionBits := 0;
  for i := 5 downto 0 do
    for j := (dimension - 9) downto ijMin do
      versionBits := self.copyBit(i, j, versionBits);

  parsedVersion := TVersion.decodeVersionInformation(versionBits);
  if ((parsedVersion <> nil) and
      (parsedVersion.DimensionForVersion = dimension))
  then
     Result := self.parsedVersion
  else
     Result := nil;
end;

procedure TBitMatrixParser.remask;
var
  dimension: Integer;
begin
  if (parsedFormatInfo <> nil) then
  begin
    dimension := bitMatrix.Height;
    TDataMask.forReference(parsedFormatInfo.DataMask)
      .unmaskBitMatrix(bitMatrix, dimension);
  end;
end;

procedure TBitMatrixParser.setMirror(const mirror: Boolean);
begin
  Self.parsedVersion := nil;
  Self.parsedFormatInfo := nil;
  Self.mirrored := mirror;
end;

end.
