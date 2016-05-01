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
  * Ported from ZXING Java Source: www.Redivivus.in (suraj.supekar@redivivus.in)
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.DataBlock;

interface

uses 
  System.SysUtils, 
  ZXing.QrCode.Internal.Version,
  ZXing.QrCode.Internal.ErrorCorrectionLevel;

type
  /// <summary> <p>Encapsulates a block of data within a QR Code. QR Codes may split their data into
  /// multiple blocks, each of which is a unit of data and error-correction codewords. Each
  /// is represented by an instance of this class.</p>
  /// </summary>
  TDataBlock = class sealed
  private
    Fcodewords: TArray<Byte>;
    FnumDataCodewords: Integer;
  public
    constructor Create(const codewords: TArray<Byte>;
      const NumDataCodewords: Integer);
    destructor Destroy; override;

    /// <summary> <p>When QR Codes use multiple data blocks, they are actually interleaved.
    /// That is, the first byte of data block 1 to n is written, then the second bytes, and so on. This
    /// method will separate the data into original blocks.</p>
    ///
    /// </summary>
    /// <param name="rawCodewords">bytes as read directly from the QR Code
    /// </param>
    /// <param name="version">version of the QR Code
    /// </param>
    /// <param name="ecLevel">error-correction level of the QR Code
    /// </param>
    /// <returns> {@link TDataBlock}s containing original bytes, "de-interleaved" from representation in the
    /// QR Code
    /// </returns>
    class function getDataBlocks(const rawCodewords: TArray<Byte>;
      const version: TVersion; const ecLevel: TErrorCorrectionLevel): TArray<TDataBlock>; static;

    property codewords : TArray<Byte> read Fcodewords;
    property numDataCodewords : Integer read FnumDataCodewords;
  end;

implementation

{ TDataBlock }

constructor TDataBlock.Create(const codewords: TArray<Byte>;
  const numDataCodewords: Integer);
begin
  Fcodewords := codewords;
  FnumDataCodewords := numDataCodewords;
end;

destructor TDataBlock.Destroy;
begin
  Fcodewords := nil;
  inherited;
end;

class function TDataBlock.getDataBlocks(const rawCodewords: TArray<Byte>;
  const version: TVersion; const ecLevel: TErrorCorrectionLevel): TArray<TDataBlock>;
var
  numResultBlocks,
  shorterBlocksTotalCodewords,
  longerBlocksStartAt,
  shorterBlocksNumDataCodewords,
  max, i, j,
  totalBlocks,
  numBlockCodewords,
  iOffset,
  rawCodewordsOffset,
  NumDataCodewords: Integer;
  ecBlock: Tversion.TECB;
  ecBlocks: Tversion.TECBlocks;
  ecBlockArray: TArray<Tversion.TECB>;
  numBlockCodewordsBytes: TArray<Byte>;

begin
  if (Length(rawCodewords) <> Version.TotalCodewords)
  then
     raise EArgumentException.Create('Invalid arguments');

  // Figure out the number and size of data blocks used by this version and
  // error correction level
  ecBlocks := version.getECBlocksForLevel(ecLevel);

  // First count the total number of data blocks
  totalBlocks := 0;
  ecBlockArray := ecBlocks.getECBlocks;

  for ecBlock in ecBlockArray do
    Inc(totalBlocks, ecBlock.Count);

  // Now establish DataBlocks of the appropriate size and number of data codewords
  Result := TArray<TDataBlock>.Create();
  SetLength(result, totalBlocks);
  numResultBlocks := 0;
  for ecBlock in ecBlockArray do
  begin
    for i := 0 to Pred(ecBlock.count) do
    begin
      numDataCodewords := ecBlock.DataCodewords;
      numBlockCodewords := ecBlocks.ECCodewordsPerBlock + numDataCodewords;

      numBlockCodewordsBytes := TArray<Byte>.Create();
      SetLength(numBlockCodewordsBytes, numBlockCodewords);

      Result[numResultBlocks] := TDataBlock.Create(numBlockCodewordsBytes, numDataCodewords);
      Inc(numResultBlocks);
    end
  end;

  numBlockCodewordsBytes := nil;
  // All blocks have the same amount of data, except that the last n
  // (where n may be 0) have 1 more byte. Figure out where these start.
  shorterBlocksTotalCodewords := Length(Result[0].codewords);
  longerBlocksStartAt := Length(result) - 1;
  while ((longerBlocksStartAt >= 0)) do
  begin
    if (Length(Result[longerBlocksStartAt].codewords) = shorterBlocksTotalCodewords)
    then
       break;
    Dec(longerBlocksStartAt);
  end;
  Inc(longerBlocksStartAt);

  shorterBlocksNumDataCodewords := (shorterBlocksTotalCodewords - ecBlocks.ECCodewordsPerBlock);
  // The last elements of result may be 1 element longer;
  // first fill out as many elements as all of them have
  rawCodewordsOffset := 0;
  for i := 0 to Pred(shorterBlocksNumDataCodewords) do
  begin
    for j := 0 to Pred(numResultBlocks) do
    begin
      Result[j].codewords[i] := rawCodewords[rawCodewordsOffset];
      Inc(rawCodewordsOffset);
    end;
  end;
  // Fill out the last data block in the longer ones
  for j := longerBlocksStartAt to Pred(numResultBlocks) do
  begin
    result[j].codewords[shorterBlocksNumDataCodewords] :=
      rawCodewords[rawCodewordsOffset];
    Inc(rawCodewordsOffset);
  end;
  // Now add in error correction blocks
  max := Length(result[0].codewords);

  for i := shorterBlocksNumDataCodewords to Pred(max) do
  begin
    for j := 0 to Pred(numResultBlocks) do
    begin
      if (j < longerBlocksStartAt)
      then
         iOffset := i
      else
         iOffset := i + 1;

      Result[j].codewords[iOffset] := rawCodewords[rawCodewordsOffset];
      Inc(rawCodewordsOffset);
    end;
  end;
end;

end.
