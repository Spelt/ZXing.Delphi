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

  * Original Author: bbrown@google.com (Brian Brown)  
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.Internal.Version;

interface

uses
  System.SysUtils, 
  ZXing.Common.BitMatrix, 
  ZXing.QrCode.Internal.FormatInformation, 
  ZXing.Common.Detector.MathUtils;

type
  /// <summary>
  /// The Version object encapsulates attributes about a particular
  /// size Data Matrix Code.
  /// </summary>  
  TVersion = class sealed

  type
    /// <summary>
    /// <p>Encapsualtes the parameters for one error-correction block in one symbol version.
    /// This includes the number of data codewords, and the number of times a block with these
    /// parameters is used consecutively in the Data Matrix code version's format.</p>
    /// </summary>
	TECB = class
    private      
	  Fcount: Integer;
      FdataCodewords: Integer;
    public
      constructor Create(count: Integer; dataCodewords: Integer);

  	  property count: Integer read Fcount;
	  property dataCodewords: Integer read FdataCodewords;
    end;

    /// <summary>
    /// <p>Encapsulates a set of error-correction blocks in one symbol version. Most versions will
    /// use blocks of differing sizes within one version, so, this encapsulates the parameters for
    /// each set of blocks. It also holds the number of error-correction codewords per block since it
    /// will be the same across all blocks within one version.</p>
    /// </summary>    
	TECBlocks = class
    private
      _ecBlocksValue: TArray<TECB>;
      _ecCodewords: Integer;
    public
  	  property ECBBlocksValue: TArray<TECB> read _ecBlocksValue;
      property ECCodewords: Integer read _ecCodewords;

      constructor Create(ecCodewords: Integer;
         ecBlocks: TECB); overload;
      constructor Create(ecCodewords: Integer;
         ecBlocks1: TECB; ecBlocks2: TECB); overload;
    end;

  private
    FdataRegionSizeColumns: Integer;
    FdataRegionSizeRows: Integer;
    FecBlocks: TECBlocks;
    FsymbolSizeColumns: Integer;
    FsymbolSizeRows: Integer;
    FtotalCodewords: Integer;
    FversionNumber: Integer;

  	constructor Create(versionNumber, symbolSizeRows, symbolSizeColumns,
      dataRegionSizeRows, dataRegionSizeColumns: Integer; ecBlocks: TECBlocks);

    class function buildVersions: TArray<TVersion>; static;
  public
    //destructor Destroy(); override;
    function ToString: string; override;
    class function getVersionForDimensions(numRows: Integer; numColumns: Integer): TVersion;

    property dataRegionSizeColumns: Integer read FdataRegionSizeColumns;
	  property dataRegionSizeRows: Integer read FdataRegionSizeRows;
	  property ecBlocks: TECBlocks read FecBlocks;
	  property symbolSizeColumns: Integer read FsymbolSizeColumns;
	  property symbolSizeRows: Integer read FsymbolSizeRows;
   	property totalCodewords: Integer read FtotalCodewords;
	  property versionNumber: Integer read FversionNumber;
  end;
  
implementation  
  
{ TVersion.TECB }

constructor TVersion.TECB.Create(count, dataCodewords: Integer);
begin
  self.Fcount := count;
  self.FdataCodewords := dataCodewords;
end;

{ TVersion.TECBlocks }

constructor TVersion.TECBlocks.Create(ecCodewords: Integer;
  ecBlocks: TECB);
begin
  self._ecCodewords := ecCodewords;
  self._ecBlocksValue := TArray<TECB>.Create(ecBlocks);
end;

constructor TVersion.TECBlocks.Create(ecCodewords: Integer;
  ecBlocks1: TECB; ecBlocks2: TECB);
begin
  self._ecCodewords := ecCodewords;
  self._ecBlocksValue := TArray<TECB>.Create(ecBlocks1, ecBlocks2);
end;

{ TVersion }
  
constructor TVersion.Create(versionNumber, symbolSizeRows, symbolSizeColumns,
  dataRegionSizeRows, dataRegionSizeColumns: Integer; ecBlocks: TECBlocks);
var
  ecBlock : TECB;
  total : Integer;
  ecCodewords : Integer;
begin
  self.FversionNumber := versionNumber;
  self.FsymbolSizeRows := symbolSizeRows;
  self.FsymbolSizeColumns := symbolSizeColumns;
  self.FdataRegionSizeRows := dataRegionSizeRows;
  self.FdataRegionSizeColumns := dataRegionSizeColumns;
  self.FecBlocks := ecBlocks;
  total := 0;
  ecCodewords := ecBlocks.ECCodewords;

  for ecBlock in ecBlocks.ECBBlocksValue do
  begin
    Inc(total, (ecBlock.Count * (ecBlock.DataCodewords + ecCodewords)))
  end;

  self.FtotalCodewords := total;
end;

/// <summary>
/// See ISO 16022:2006 5.5.1 Table 7
/// </summary>
class function TVersion.buildVersions: TArray<TVersion>;
begin
  Result := TArray<TVersion>.Create(  
    TVersion.Create(1, 10, 10, 8, 8, TECBlocks.Create(5, TECB.Create(1, 3))),
    TVersion.Create(2, 12, 12, 10, 10, TECBlocks.Create(7, TECB.Create(1, 5))),
    TVersion.Create(3, 14, 14, 12, 12, TECBlocks.Create(10, TECB.Create(1, 8))),
    TVersion.Create(4, $10, $10, 14, 14, TECBlocks.Create(12, TECB.Create(1, 12))),
    TVersion.Create(5, $12, $12, $10, $10, TECBlocks.Create(14, TECB.Create(1, $12))),
    TVersion.Create(6, 20, 20, $12, $12, TECBlocks.Create($12, TECB.Create(1, $16))),
    TVersion.Create(7, $16, $16, 20, 20, TECBlocks.Create(20, TECB.Create(1, 30))),
    TVersion.Create(8, $18, $18, $16, $16, TECBlocks.Create($18, TECB.Create(1, $24))),
    TVersion.Create(9, $1a, $1a, $18, $18, TECBlocks.Create($1c, TECB.Create(1, $2c))),
    TVersion.Create(10, $20, $20, 14, 14, TECBlocks.Create($24, TECB.Create(1, $3e))),
    TVersion.Create(11, $24, $24, $10, $10, TECBlocks.Create($2a, TECB.Create(1, $56))),
    TVersion.Create(12, 40, 40, $12, $12, TECBlocks.Create($30, TECB.Create(1, $72))),
    TVersion.Create(13, $2c, $2c, 20, 20, TECBlocks.Create($38, TECB.Create(1, $90))),
    TVersion.Create(14, $30, $30, $16, $16, TECBlocks.Create($44, TECB.Create(1, $ae))),
    TVersion.Create(15, $34, $34, $18, $18, TECBlocks.Create($2a, TECB.Create(2, $66))),
    TVersion.Create($10, $40, $40, 14, 14, TECBlocks.Create($38, TECB.Create(2, 140))),
    TVersion.Create($11, $48, $48, $10, $10, TECBlocks.Create($24, TECB.Create(4, $5c))),
    TVersion.Create($12, 80, 80, $12, $12, TECBlocks.Create($30, TECB.Create(4, $72))),
    TVersion.Create($13, $58, $58, 20, 20, TECBlocks.Create($38, TECB.Create(4, $90))),
    TVersion.Create(20, $60, $60, $16, $16, TECBlocks.Create($44, TECB.Create(4, $ae))),
    TVersion.Create($15, $68, $68, $18, $18, TECBlocks.Create($38, TECB.Create(6, $88))),
    TVersion.Create($16, 120, 120, $12, $12, TECBlocks.Create($44, TECB.Create(6, $af))),
    TVersion.Create($17, $84, $84, 20, 20, TECBlocks.Create($3e, TECB.Create(8, $a3))),
    TVersion.Create($18, $90, $90, $16, $16, TECBlocks.Create($3e, TECB.Create(8, $9c), TECB.Create(2, $9b))),
    TVersion.Create($19, 8, $12, 6, $10, TECBlocks.Create(7, TECB.Create(1, 5))),
    TVersion.Create($1a, 8, $20, 6, 14, TECBlocks.Create(11, TECB.Create(1, 10))),
    TVersion.Create($1b, 12, $1a, 10, $18, TECBlocks.Create(14, TECB.Create(1, $10))),
    TVersion.Create($1c, 12, $24, 10, $10, TECBlocks.Create($12, TECB.Create(1, $16))),
    TVersion.Create($1d, $10, $24, 14, $10, TECBlocks.Create($18, TECB.Create(1, $20))),
    TVersion.Create(30, $10, $30, 14, $16, TECBlocks.Create($1c, TECB.Create(1, $31)))
    );
end;

/// <summary>
/// <p>Deduces version information from Data Matrix dimensions.</p>
///
/// <param name="numRows">Number of rows in modules</param>
/// <param name="numColumns">Number of columns in modules</param>
/// <returns>Version for a Data Matrix Code of those dimensions</returns>
/// <exception cref="FormatException">if dimensions do correspond to a valid Data Matrix size</exception>
/// </summary>
class function TVersion.getVersionForDimensions(numRows: Integer; numColumns: Integer): TVersion;
var
  version: TVersion;
begin
  Result := nil;

  if (((numRows and $01) = 0) and ((numColumns and $01) = 0)) then
  begin
    for version in TVersion.buildVersions do
    begin
      if ((version.symbolSizeRows = numRows) and 
          (version.symbolSizeColumns = numColumns)) then
      begin
        Result := version;
        break;
      end;
    end;
  end;  
end;

function TVersion.ToString: string;
begin
  result := versionNumber.ToString();
end; 

end.
