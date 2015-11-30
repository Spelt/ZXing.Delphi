unit Datablock;

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

uses SysUtils, Version, ErrorCorrectionLevel;

type
  TDataBlock = class sealed
  private
    constructor Create(codewords: TArray<Byte>; NumDataCodewords: Integer);
  public
  var
    codewords: TArray<Byte>;
    NumDataCodewords: Integer;
    class function getDataBlocks(rawCodewords: TArray<Byte>; Version: Tversion;
      ecLevel: TErrorCorrectionLevel): TArray<TDataBlock>; static;
  end;

implementation

{ TDataBlock }

constructor TDataBlock.Create(codewords: TArray<Byte>;
  NumDataCodewords: Integer);
begin
  self.codewords := codewords;
  self.NumDataCodewords := NumDataCodewords;
end;

class function TDataBlock.getDataBlocks(rawCodewords: TArray<Byte>;
  Version: Tversion; ecLevel: TErrorCorrectionLevel): TArray<TDataBlock>;
var
  i, numResultBlocks, shorterBlocksTotalCodewords, longerBlocksStartAt,
    shorterBlocksNumDataCodewords, j, totalBlocks, numBlockCodewords, iOffset,
    rawCodewordsOffset, max, NumDataCodewords: Integer;
  ecBlock: Tversion.TECB;
  ecBlocks: Tversion.TECBlocks;
  ecBlockArray: TArray<Tversion.TECB>;
  numBlockCodewordsBytes: TArray<Byte>;

begin
  if (Length(rawCodewords) <> Version.TotalCodewords) then
    raise EArgumentException.Create('Arguments in error');

  ecBlocks := Version.getECBlocksForLevel(ecLevel);
  totalBlocks := 0;
  ecBlockArray := ecBlocks.getECBlocks;

  for ecBlock in ecBlockArray do
  begin
    inc(totalBlocks, ecBlock.Count)
  end;

  result := TArray<TDataBlock>.Create();
  SetLength(result, totalBlocks);

  numResultBlocks := 0;
  for ecBlock in ecBlockArray do
  begin

    i := 0;
    while ((i < ecBlock.Count)) do
    begin
      NumDataCodewords := ecBlock.DataCodewords;
      numBlockCodewords := ecBlocks.ECCodewordsPerBlock + NumDataCodewords;

      numBlockCodewordsBytes := TArray<Byte>.Create();
      SetLength(numBlockCodewordsBytes, numBlockCodewords);

      result[numResultBlocks] := TDataBlock.Create(numBlockCodewordsBytes,
        NumDataCodewords);
      inc(numResultBlocks);
      inc(i)
    end

  end;

  shorterBlocksTotalCodewords := Length(result[0].codewords);
  longerBlocksStartAt := Length(result) - 1;
  while ((longerBlocksStartAt >= 0)) do
  begin
    if (Length(result[longerBlocksStartAt].codewords)
      = shorterBlocksTotalCodewords) then
      break;
    dec(longerBlocksStartAt)
  end;

  inc(longerBlocksStartAt);
  shorterBlocksNumDataCodewords :=
    (shorterBlocksTotalCodewords - ecBlocks.ECCodewordsPerBlock);
  rawCodewordsOffset := 0;
  i := 0;
  while ((i < shorterBlocksNumDataCodewords)) do
  begin
    j := 0;
    while ((j < numResultBlocks)) do
    begin
      result[j].codewords[i] := rawCodewords[rawCodewordsOffset];
      inc(rawCodewordsOffset);
      inc(j)
    end;
    inc(i)
  end;

  j := longerBlocksStartAt;
  while ((j < numResultBlocks)) do
  begin
    result[j].codewords[shorterBlocksNumDataCodewords] :=
      rawCodewords[rawCodewordsOffset];
    inc(rawCodewordsOffset);
    inc(j)
  end;

  max := Length(result[0].codewords);
  i := shorterBlocksNumDataCodewords;
  while ((i < max)) do
  begin
    j := 0;
    while ((j < numResultBlocks)) do
    begin

      if (j < longerBlocksStartAt) then
      begin
        iOffset := i;
      end
      else
      begin
        iOffset := i + 1;
      end;

      result[j].codewords[iOffset] := rawCodewords[rawCodewordsOffset];
      inc(rawCodewordsOffset);
      inc(j)
    end;

    inc(i)

  end;

end;

end.
