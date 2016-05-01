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

unit ZXing.Datamatrix.Internal.DataBlock;

interface

uses 
  System.SysUtils, 
  ZXing.Datamatrix.Internal.Version;
  
type
  /// <summary>
  /// <p>Encapsulates a block of data within a Data Matrix Code. Data Matrix Codes may split their data into
  /// multiple blocks, each of which is a unit of data and error-correction codewords. Each
  /// is represented by an instance of this class.</p>
  /// </summary>
  TDataBlock = class sealed
  private
    Fcodewords: TArray<Byte>;
    FNumDataCodewords: Integer;
  public
    constructor Create(const Acodewords: TArray<Byte>;
      const ANumDataCodewords: Integer);
    destructor Destroy;override;

    class function getDataBlocks(rawCodewords: TArray<Byte>;
      version: TVersion): TArray<TDataBlock>;

    property codewords: TArray<Byte> read Fcodewords;
    property NumDataCodewords: Integer read FNumDataCodewords;
  end;

implementation

{ TDataBlock }  
  
constructor TDataBlock.Create(const Acodewords: TArray<Byte>;
  const ANumDataCodewords: Integer);
begin
  FnumDataCodewords := AnumDataCodewords;
  Fcodewords := Acodewords;
end;

destructor TDataBlock.Destroy;
begin
  Fcodewords := nil;
  inherited;
end;

class function TDataBlock.getDataBlocks(rawCodewords: TArray<Byte>;
  version: TVersion): TArray<TDataBlock>;
var
  i, j: Integer;
  ecBlock  : TVersion.TECB;
  ecBlocks : TVersion.TECBlocks;
  totalBlocks : Integer;
  ecBlockArray : TArray<TVersion.TECB>;
  byteArray    : TArray<Byte>;
  numResultBlocks,
  numDataCodewords,
  numBlockCodewords,
  longerBlocksTotalCodewords,
  //shorterBlocksTotalCodewords
  longerBlocksNumDataCodewords,
  shorterBlocksNumDataCodewords,
  rawCodewordsOffset,
  numLongerBlocks,
  max,
  iOffset,
  jOffset : Integer;
  specialVersion: Boolean;
begin
  // Figure out the number and size of data blocks used by this version
  ecBlocks := version.ecBlocks;
  
  // First count the total number of data blocks
  totalBlocks := 0;
  ecBlockArray := ecBlocks.ECBBlocksValue;
  for ecBlock in ecBlockArray do
  begin
    inc(totalBlocks, ecBlock.Count)
  end;
    
  // Now establish DataBlocks of the appropriate size and number of data codewords
  result := TArray<TDataBlock>.Create();
  SetLength(result, totalBlocks);
  numResultBlocks := 0;

  for ecBlock in ecBlockArray do
  begin
    for i := 0 to Pred(ecBlock.Count) do
	  begin
      numDataCodewords := ecBlock.DataCodewords;
      numBlockCodewords := (ecBlocks.ECCodewords + numDataCodewords);
      byteArray := TArray<Byte>.Create();
      SetLength(byteArray, numBlockCodewords);
      result[numResultBlocks] := TDataBlock.Create(byteArray, numDataCodewords);
      Inc(numResultBlocks);
    end
  end;
  
  // All blocks have the same amount of data, except that the last n
  // (where n may be 0) have 1 less byte. Figure out where these start.
  // TODO: There is only one case where there is a difference for Data Matrix for size 144
  longerBlocksTotalCodewords := Length(result[0].codewords);
  //shorterBlocksTotalCodewords := (longerBlocksTotalCodewords - 1);
		 
  longerBlocksNumDataCodewords := (longerBlocksTotalCodewords - ecBlocks.ECCodewords);
  shorterBlocksNumDataCodewords := (longerBlocksNumDataCodewords - 1);
  // The last elements of result may be 1 element shorter for 144 matrix
  // first fill out as many elements as all of them have minus 1
  rawCodewordsOffset := 0;
  for i := 0 to Pred(shorterBlocksNumDataCodewords) do
  begin
    for j := 0 to Pred(numResultBlocks) do
    begin
      result[j].codewords[i] := rawCodewords[rawCodewordsOffset];
      Inc(rawCodewordsOffset);
    end;
  end;
  
  // Fill out the last data block in the longer ones
  specialVersion := (version.versionNumber = 24);
  if specialVersion 
  then 
     numLongerBlocks := 8 
  else 
     numLongerBlocks := numResultBlocks;
  for j := 0 to Pred(numLongerBlocks) do
  begin
    result[j].codewords[(longerBlocksNumDataCodewords - 1)] := rawCodewords[rawCodewordsOffset];
    Inc(rawCodewordsOffset);
  end;
  
  // Now add in error correction blocks
  max := Length(result[0].codewords);
  for i := longerBlocksNumDataCodewords to Pred(max) do
  begin
    for j := 0 to Pred(numResultBlocks) do
    begin
      if (specialVersion)
      then
         jOffset := (j + 8) mod numResultBlocks
      else
         jOffset := j;
      
	  if (specialVersion and (jOffset > 7))
      then
         iOffset := (i - 1)
      else
         iOffset := i;
      result[jOffset].codewords[iOffset] := rawCodewords[rawCodewordsOffset];
      Inc(rawCodewordsOffset);
    end;
  end;
  
  if (not (rawCodewordsOffset = Length(rawCodewords)))
  then
     raise EArgumentException.Create('Arguments error');
end;

end.