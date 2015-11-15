unit Datablock;

interface

uses SysUtils, Version, ErrorCorrectionLevel;

type
  TDataBlock = class sealed
  private
    constructor Create(codewords: TArray<Byte>);
  public
  var
    codewords: TArray<Byte>;
    class function getDataBlocks(rawCodewords: TArray<Byte>; Version: Tversion;
      ecLevel: TErrorCorrectionLevel): TArray<TDataBlock>; static;
  end;

implementation

{ TDataBlock }

constructor TDataBlock.Create(codewords: TArray<Byte>);
begin
  self.codewords := codewords
end;

class function TDataBlock.getDataBlocks(rawCodewords: TArray<Byte>;
  Version: Tversion; ecLevel: TErrorCorrectionLevel): TArray<TDataBlock>;
var
  i, numResultBlocks, shorterBlocksTotalCodewords, longerBlocksStartAt,
    shorterBlocksNumDataCodewords, j, totalBlocks, numBlockCodewords, iOffset,
    rawCodewordsOffset, max, numDataCodewords: Integer;
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
      numDataCodewords := ecBlock.DataCodewords;
      numBlockCodewords := (ecBlocks.ECCodewordsPerBlock + numDataCodewords);
      inc(numResultBlocks);

      numBlockCodewordsBytes := TArray<Byte>.Create();
      SetLength(numBlockCodewordsBytes, numBlockCodewords);

      result[numResultBlocks] := TDataBlock.Create(numBlockCodewordsBytes);

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
      inc(rawCodewordsOffset);
      result[j].codewords[i] := rawCodewords[rawCodewordsOffset];
      inc(j)
    end;
    inc(i)
  end;

  j := longerBlocksStartAt;
  while ((j < numResultBlocks)) do
  begin
    inc(rawCodewordsOffset);
    result[j].codewords[shorterBlocksNumDataCodewords] :=
      rawCodewords[rawCodewordsOffset];
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

      inc(rawCodewordsOffset);
      result[j].codewords[iOffset] := rawCodewords[rawCodewordsOffset];
      inc(j)
    end;

    inc(i)

  end;

  result := result;

end;

end.
