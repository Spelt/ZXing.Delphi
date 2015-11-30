unit Version;

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

uses SysUtils, Bitmatrix, ErrorCorrectionLevel, FormatInformation, MathUtils;

type

  TVersion = class

  type
    TECB = class
    public
      count: Integer;
      dataCodewords: Integer;
      constructor Create(count: Integer; dataCodewords: Integer);
    end;

    TECBlocks = class sealed
    private
      ecBlocks: TArray<TECB>;

      function getTotalECCodeWords: Integer;
      function get_NumBlocks: Integer;
    public
      ecCodewordsPerBlock: Integer;
      constructor Create(ecCodewordsPerBlock: Integer; ecBlocks: TArray<TECB>);

      function getECBlocks: TArray<TECB>;
      { property ecCodewordsPerBlock: Integer read get_ECCodewordsPerBlock;
        property NumBlocks: Integer read get_NumBlocks;
        property TotalECCodewords: Integer read get_TotalECCodewords;
      }
      property NumBlocks: Integer read get_NumBlocks;
      property TotalECCodewords: Integer read getTotalECCodeWords;

    end;

  private
    FalignmentPatternCenters: TArray<Integer>;
    FecBlocks: TArray<TECBlocks>;
    FTotalCodewords: Integer;
    FversionNumber: Integer;

    class var VERSION_DECODE_INFO: TArray<Integer>;

    constructor Create(versionNumber: Integer;
      alignmentPatternCenters: TArray<Integer>; ecBlocks: TArray<TECBlocks>);

    function CalcDimensionForVersion: Integer;
    class function buildVersions: TArray<TVersion>; static;
    class procedure ClassInit; static;

  public
    class function decodeVersionInformation(versionBits: Integer)
      : TVersion; static;
    function buildFunctionPattern: TBitMatrix;
    function getECBlocksForLevel(ecLevel: TErrorCorrectionLevel): TECBlocks;
    class function getProvisionalVersionForDimension(dimension: Integer)
      : TVersion; static;
    class function getVersionForNumber(versionNumber: Integer)
      : TVersion; static;
    function ToString: string; override;

    property DimensionForVersion: Integer read CalcDimensionForVersion;
    property alignmentPatternCenters: TArray<Integer>
      read FalignmentPatternCenters;

    property TotalCodewords: Integer read FTotalCodewords;
    property versionNumber: Integer read FversionNumber;
  end;

implementation

{ TVersion.TECB }

constructor TVersion.TECB.Create(count, dataCodewords: Integer);
begin
  self.count := count;
  self.dataCodewords := dataCodewords;
end;

{ TVersion.TECBlocks }

constructor TVersion.TECBlocks.Create(ecCodewordsPerBlock: Integer;
  ecBlocks: TArray<TECB>);
begin
  self.ecBlocks := ecBlocks;
  self.ecCodewordsPerBlock := ecCodewordsPerBlock;
end;

function TVersion.TECBlocks.getECBlocks: TArray<TECB>;
begin
  result := self.ecBlocks;
end;

function TVersion.TECBlocks.getTotalECCodeWords: Integer;
begin
  result := ecCodewordsPerBlock * NumBlocks;
end;

function TVersion.TECBlocks.get_NumBlocks: Integer;
var
  total: Integer;
  ecBlock: TECB;
begin
  total := 0;
  for ecBlock in ecBlocks do
  begin
    inc(total, ecBlock.count);
  end;

  result := total;

end;

{ TVersion }

function TVersion.buildFunctionPattern: TBitMatrix;
var
  dimension, max, x, i, y: Integer;
  Bitmatrix: TBitMatrix;
begin
  dimension := DimensionForVersion;
  Bitmatrix := TBitMatrix.Create(dimension);
  Bitmatrix.setRegion(0, 0, 9, 9);
  Bitmatrix.setRegion((dimension - 8), 0, 8, 9);
  Bitmatrix.setRegion(0, (dimension - 8), 9, 8);
  max := Length(FalignmentPatternCenters);
  x := 0;

  while ((x < max)) do
  begin
    i := (FalignmentPatternCenters[x] - 2);
    y := 0;
    while ((y < max)) do
    begin
      if (((x <> 0) or ((y <> 0) and (y <> (max - 1)))) and
        ((x <> (max - 1)) or (y <> 0))) then
        Bitmatrix.setRegion((self.alignmentPatternCenters[y] - 2), i, 5, 5);
      inc(y)
    end;
    inc(x)
  end;

  Bitmatrix.setRegion(6, 9, 1, (dimension - $11));
  Bitmatrix.setRegion(9, 6, (dimension - $11), 1);
  if (self.versionNumber > 6) then
  begin
    Bitmatrix.setRegion((dimension - 11), 0, 3, 6);
    Bitmatrix.setRegion(0, (dimension - 11), 6, 3)
  end;

  result := Bitmatrix;
end;

class function TVersion.buildVersions: TArray<TVersion>;
begin
  result := TArray<TVersion>.Create(

    TVersion.Create(1, TArray<Integer>.Create(),

    { TArray<TECBlocks>.Create(
      TECBlocks.Create(7,TECB.Create(1, 19)),
      TECBlocks.Create(10,TECB.Create(1, 16)),
      TECBlocks.Create(13,TECB.Create(1, 13)),
      TECBlocks.Create(17,TECB.Create(1, 9))),
    }
    TArray<TECBlocks>.Create(TECBlocks.Create(7,
    TArray<TECB>.Create(TECB.Create(1, 19))), TECBlocks.Create(10,
    TArray<TECB>.Create(TECB.Create(1, 16))), TECBlocks.Create(13,
    TArray<TECB>.Create(TECB.Create(1, 13))), TECBlocks.Create(17,
    TArray<TECB>.Create(TECB.Create(1, 9))))),

    TVersion.Create(2, TArray<Integer>.Create(6, 18),
    TArray<TECBlocks>.Create(TECBlocks.Create(10,
    TArray<TECB>.Create(TECB.Create(1, 34))), TECBlocks.Create(16,
    TArray<TECB>.Create(TECB.Create(1, 28))), TECBlocks.Create(22,
    TArray<TECB>.Create(TECB.Create(1, 22))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(1, 16))))),

    TVersion.Create(3, TArray<Integer>.Create(6, 22),
    TArray<TECBlocks>.Create(TECBlocks.Create(15,
    TArray<TECB>.Create(TECB.Create(1, 55))), TECBlocks.Create(26,
    TArray<TECB>.Create(TECB.Create(1, 44))), TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(2, 17))), TECBlocks.Create(22,
    TArray<TECB>.Create(TECB.Create(2, 13))))),

    TVersion.Create(4, TArray<Integer>.Create(6, 26),
    TArray<TECBlocks>.Create(TECBlocks.Create(20,
    TArray<TECB>.Create(TECB.Create(1, 80))), TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(2, 32))), TECBlocks.Create(26,
    TArray<TECB>.Create(TECB.Create(2, 24))), TECBlocks.Create(16,
    TArray<TECB>.Create(TECB.Create(4, 9))))),

    TVersion.Create(5, TArray<Integer>.Create(6, 30),
    TArray<TECBlocks>.Create(TECBlocks.Create(26,
    TArray<TECB>.Create(TECB.Create(1, 108))), TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(2, 43))), TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(2, 15), TECB.Create(2, 16))),
    TECBlocks.Create(22, TArray<TECB>.Create(TECB.Create(2, 11),
    TECB.Create(2, 12))))),

    TVersion.Create(6, TArray<Integer>.Create(6, 34),
    TArray<TECBlocks>.Create(TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(2, 68))), TECBlocks.Create(16,
    TArray<TECB>.Create(TECB.Create(4, 27))), TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(4, 19))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(4, 15))))),

    TVersion.Create(7, TArray<Integer>.Create(6, 22, 38),
    TArray<TECBlocks>.Create(TECBlocks.Create(20,
    TArray<TECB>.Create(TECB.Create(2, 78))), TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(4, 31))), TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(2, 14), TECB.Create(4, 15))),
    TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(4, 13),
    TECB.Create(1, 14))))),

    TVersion.Create(8, TArray<Integer>.Create(6, 24, 42),
    TArray<TECBlocks>.Create(TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(2, 97))), TECBlocks.Create(22,
    TArray<TECB>.Create(TECB.Create(2, 38), TECB.Create(2, 39))),
    TECBlocks.Create(22, TArray<TECB>.Create(TECB.Create(4, 18), TECB.Create(2,
    19))), TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(4, 14),
    TECB.Create(2, 15))))),

    TVersion.Create(9, TArray<Integer>.Create(6, 26, 46),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(2, 116))), TECBlocks.Create(22,
    TArray<TECB>.Create(TECB.Create(3, 36), TECB.Create(2, 37))),
    TECBlocks.Create(20, TArray<TECB>.Create(TECB.Create(4, 16), TECB.Create(4,
    17))), TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(4, 12),
    TECB.Create(4, 13))))),

    TVersion.Create(10, TArray<Integer>.Create(6, 28, 50),
    TArray<TECBlocks>.Create(TECBlocks.Create(18,
    TArray<TECB>.Create(TECB.Create(2, 68), TECB.Create(2, 69))),
    TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(4, 43), TECB.Create(1,
    44))), TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(6, 19),
    TECB.Create(2, 20))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(6, 15), TECB.Create(2, 16))))),

    TVersion.Create(11, TArray<Integer>.Create(6, 30, 54),
    TArray<TECBlocks>.Create(TECBlocks.Create(20,
    TArray<TECB>.Create(TECB.Create(4, 81))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(1, 50), TECB.Create(4, 51))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(4, 22), TECB.Create(4,
    23))), TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(3, 12),
    TECB.Create(8, 13))))),

    TVersion.Create(12, TArray<Integer>.Create(6, 32, 58),
    TArray<TECBlocks>.Create(TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(2, 92), TECB.Create(2, 93))),
    TECBlocks.Create(22, TArray<TECB>.Create(TECB.Create(6, 36), TECB.Create(2,
    37))), TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(4, 20),
    TECB.Create(6, 21))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(7, 14), TECB.Create(4, 15))))),

    TVersion.Create(13, TArray<Integer>.Create(6, 34, 62),
    TArray<TECBlocks>.Create(TECBlocks.Create(26,
    TArray<TECB>.Create(TECB.Create(4, 107))), TECBlocks.Create(22,
    TArray<TECB>.Create(TECB.Create(8, 37), TECB.Create(1, 38))),
    TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(8, 20), TECB.Create(4,
    21))), TECBlocks.Create(22, TArray<TECB>.Create(TECB.Create(12, 11),
    TECB.Create(4, 12))))),

    TVersion.Create(14, TArray<Integer>.Create(6, 26, 46, 66),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(3, 115), TECB.Create(1, 116))),
    TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(4, 40), TECB.Create(5,
    41))), TECBlocks.Create(20, TArray<TECB>.Create(TECB.Create(11, 16),
    TECB.Create(5, 17))), TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(11, 12), TECB.Create(5, 13))))),

    TVersion.Create(15, TArray<Integer>.Create(6, 26, 48, 70),
    TArray<TECBlocks>.Create(TECBlocks.Create(22,
    TArray<TECB>.Create(TECB.Create(5, 87), TECB.Create(1, 88))),
    TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(5, 41), TECB.Create(5,
    42))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(5, 24),
    TECB.Create(7, 25))), TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(11, 12), TECB.Create(7, 13))))),

    TVersion.Create(16, TArray<Integer>.Create(6, 26, 50, 74),
    TArray<TECBlocks>.Create(TECBlocks.Create(24,
    TArray<TECB>.Create(TECB.Create(5, 98), TECB.Create(1, 99))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(7, 45), TECB.Create(3,
    46))), TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(15, 19),
    TECB.Create(2, 20))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(3, 15), TECB.Create(13, 16))))),

    TVersion.Create(17, TArray<Integer>.Create(6, 30, 54, 78),
    TArray<TECBlocks>.Create(TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(1, 107), TECB.Create(5, 108))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(10, 46), TECB.Create(1,
    47))), TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(1, 22),
    TECB.Create(15, 23))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(2, 14), TECB.Create(17, 15))))),

    TVersion.Create(18, TArray<Integer>.Create(6, 30, 56, 82),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(5, 120), TECB.Create(1, 121))),
    TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(9, 43), TECB.Create(4,
    44))), TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(17, 22),
    TECB.Create(1, 23))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(2, 14), TECB.Create(19, 15))))),

    TVersion.Create(19, TArray<Integer>.Create(6, 30, 58, 86),
    TArray<TECBlocks>.Create(TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(3, 113), TECB.Create(4, 114))),
    TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(3, 44), TECB.Create(11,
    45))), TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(17, 21),
    TECB.Create(4, 22))), TECBlocks.Create(26,
    TArray<TECB>.Create(TECB.Create(9, 13), TECB.Create(16, 14))))),

    TVersion.Create(20, TArray<Integer>.Create(6, 34, 62, 90),
    TArray<TECBlocks>.Create(TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(3, 107), TECB.Create(5, 108))),
    TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(3, 41), TECB.Create(13,
    42))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(15, 24),
    TECB.Create(5, 25))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(15, 15), TECB.Create(10, 16))))),

    TVersion.Create(21, TArray<Integer>.Create(6, 28, 50, 72, 94),
    TArray<TECBlocks>.Create(TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(4, 116), TECB.Create(4, 117))),
    TECBlocks.Create(26, TArray<TECB>.Create(TECB.Create(17, 42))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(17, 22), TECB.Create(6,
    23))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(19, 16),
    TECB.Create(6, 17))))),

    TVersion.Create(22, TArray<Integer>.Create(6, 26, 50, 74, 98),
    TArray<TECBlocks>.Create(TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(2, 111), TECB.Create(7, 112))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(17, 46))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(7, 24), TECB.Create(16,
    25))), TECBlocks.Create(24, TArray<TECB>.Create(TECB.Create(34, 13))))),

    TVersion.Create(23, TArray<Integer>.Create(6, 30, 54, 78, 102),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(4, 121), TECB.Create(5, 122))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(4, 47), TECB.Create(14,
    48))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(11, 24),
    TECB.Create(14, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(16, 15), TECB.Create(14, 16))))),

    TVersion.Create(24, TArray<Integer>.Create(6, 28, 54, 80, 106),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(6, 117), TECB.Create(4, 118))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(6, 45), TECB.Create(14,
    46))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(11, 24),
    TECB.Create(16, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(30, 16), TECB.Create(2, 17))))),

    TVersion.Create(25, TArray<Integer>.Create(6, 32, 58, 84, 110),
    TArray<TECBlocks>.Create(TECBlocks.Create(26,
    TArray<TECB>.Create(TECB.Create(8, 106), TECB.Create(4, 107))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(8, 47), TECB.Create(13,
    48))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(7, 24),
    TECB.Create(22, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(22, 15), TECB.Create(13, 16))))),

    TVersion.Create(26, TArray<Integer>.Create(6, 30, 58, 86, 114),
    TArray<TECBlocks>.Create(TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(10, 114), TECB.Create(2, 115))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(19, 46), TECB.Create(4,
    47))), TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(28, 22),
    TECB.Create(6, 23))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(33, 16), TECB.Create(4, 17))))),

    TVersion.Create(27, TArray<Integer>.Create(6, 34, 62, 90, 118),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(8, 122), TECB.Create(4, 123))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(22, 45), TECB.Create(3,
    46))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(8, 23),
    TECB.Create(26, 24))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(12, 15), TECB.Create(28, 16))))),

    TVersion.Create(28, TArray<Integer>.Create(6, 26, 50, 74, 98, 122),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(3, 117), TECB.Create(10, 118))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(3, 45), TECB.Create(23,
    46))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(4, 24),
    TECB.Create(31, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(11, 15), TECB.Create(31, 16))))),

    TVersion.Create(29, TArray<Integer>.Create(6, 30, 54, 78, 102, 126),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(7, 116), TECB.Create(7, 117))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(21, 45), TECB.Create(7,
    46))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(1, 23),
    TECB.Create(37, 24))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(19, 15), TECB.Create(26, 16))))),

    TVersion.Create(30, TArray<Integer>.Create(6, 26, 52, 78, 104, 130),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(5, 115), TECB.Create(10, 116))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(19, 47),
    TECB.Create(10, 48))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(15, 24), TECB.Create(25, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(23, 15),
    TECB.Create(25, 16))))),

    TVersion.Create(31, TArray<Integer>.Create(6, 30, 56, 82, 108, 134),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(13, 115), TECB.Create(3, 116))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(2, 46), TECB.Create(29,
    47))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(42, 24),
    TECB.Create(1, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(23, 15), TECB.Create(28, 16))))),

    TVersion.Create(32, TArray<Integer>.Create(6, 34, 60, 86, 112, 138),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(17, 115))), TECBlocks.Create(28,
    TArray<TECB>.Create(TECB.Create(10, 46), TECB.Create(23, 47))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(10, 24),
    TECB.Create(35, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(19, 15), TECB.Create(35, 16))))),

    TVersion.Create(33, TArray<Integer>.Create(6, 30, 58, 86, 114, 142),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(17, 115), TECB.Create(1, 116))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(14, 46),
    TECB.Create(21, 47))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(29, 24), TECB.Create(19, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(11, 15),
    TECB.Create(46, 16))))),

    TVersion.Create(34, TArray<Integer>.Create(6, 34, 62, 90, 118, 146),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(13, 115), TECB.Create(6, 116))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(14, 46),
    TECB.Create(23, 47))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(44, 24), TECB.Create(7, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(59, 16),
    TECB.Create(1, 17))))),

    TVersion.Create(35, TArray<Integer>.Create(6, 30, 54, 78, 102, 126, 150),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(12, 121), TECB.Create(7, 122))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(12, 47),
    TECB.Create(26, 48))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(39, 24), TECB.Create(14, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(22, 15),
    TECB.Create(41, 16))))),

    TVersion.Create(36, TArray<Integer>.Create(6, 24, 50, 76, 102, 128, 154),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(6, 121), TECB.Create(14, 122))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(6, 47), TECB.Create(34,
    48))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(46, 24),
    TECB.Create(10, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(2, 15), TECB.Create(64, 16))))),

    TVersion.Create(37, TArray<Integer>.Create(6, 28, 54, 80, 106, 132, 158),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(17, 122), TECB.Create(4, 123))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(29, 46),
    TECB.Create(14, 47))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(49, 24), TECB.Create(10, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(24, 15),
    TECB.Create(46, 16))))),

    TVersion.Create(38, TArray<Integer>.Create(6, 32, 58, 84, 110, 136, 162),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(4, 122), TECB.Create(18, 123))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(13, 46),
    TECB.Create(32, 47))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(48, 24), TECB.Create(14, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(42, 15),
    TECB.Create(32, 16))))),

    TVersion.Create(39, TArray<Integer>.Create(6, 26, 54, 82, 110, 138, 166),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(20, 117), TECB.Create(4, 118))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(40, 47), TECB.Create(7,
    48))), TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(43, 24),
    TECB.Create(22, 25))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(10, 15), TECB.Create(67, 16))))),

    TVersion.Create(40, TArray<Integer>.Create(6, 30, 58, 86, 114, 142, 170),
    TArray<TECBlocks>.Create(TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(19, 118), TECB.Create(6, 119))),
    TECBlocks.Create(28, TArray<TECB>.Create(TECB.Create(18, 47),
    TECB.Create(31, 48))), TECBlocks.Create(30,
    TArray<TECB>.Create(TECB.Create(34, 24), TECB.Create(34, 25))),
    TECBlocks.Create(30, TArray<TECB>.Create(TECB.Create(20, 15),
    TECB.Create(61, 16)))))

    );

end;

function TVersion.CalcDimensionForVersion: Integer;
begin
  result := 17 + 4 * versionNumber;
end;

constructor TVersion.Create(versionNumber: Integer;
  alignmentPatternCenters: TArray<Integer>; ecBlocks: TArray<TECBlocks>);
var
  ecBlock: TECB;
  total, ecCodewords: Integer;
  ecbArray: TArray<TECB>;

begin
  FversionNumber := versionNumber;
  FalignmentPatternCenters := alignmentPatternCenters;
  FecBlocks := ecBlocks;
  total := 0;
  ecCodewords := ecBlocks[0].ecCodewordsPerBlock;
  ecbArray := ecBlocks[0].getECBlocks;

  for ecBlock in ecbArray do
  begin
    inc(total, (ecBlock.count * (ecBlock.dataCodewords + ecCodewords)))
  end;

  FTotalCodewords := total
end;

class function TVersion.decodeVersionInformation(versionBits: Integer)
  : TVersion;
var
  bestDifference, bestversion, i, targetVersion, bitsDifference: Integer;
begin
  bestDifference := $7FFFFFFF;
  bestversion := 0;
  i := 0;

  while ((i < Length(TVersion.VERSION_DECODE_INFO))) do
  begin
    targetVersion := TVersion.VERSION_DECODE_INFO[i];

    if (targetVersion = versionBits) then
    begin
      result := TVersion.getVersionForNumber((i + 7));
      exit
    end;

    bitsDifference := TFormatInformation.numBitsDiffering(versionBits,
      targetVersion);

    if (bitsDifference < bestDifference) then
    begin
      bestversion := (i + 7);
      bestDifference := bitsDifference
    end;
    inc(i)

  end;

  if (bestDifference <= 3) then
  begin
    result := TVersion.getVersionForNumber(bestversion);
    exit
  end;

  result := nil;
end;

function TVersion.getECBlocksForLevel(ecLevel: TErrorCorrectionLevel)
  : TECBlocks;
begin
  result := FecBlocks[ecLevel.ordinal]
end;

class function TVersion.getProvisionalVersionForDimension(dimension: Integer)
  : TVersion;
begin
  if ((dimension mod 4) <> 1) then
  begin
    result := nil;
    exit
  end;
  try
    begin
      result := TVersion.getVersionForNumber
        (TMathUtils.Asr(dimension - $11, 2));
      exit
    end
  except
    on exception1: EArgumentException do
    begin
      result := nil;
    end
  end

end;

class function TVersion.getVersionForNumber(versionNumber: Integer): TVersion;
begin
  if ((versionNumber < 1) or (versionNumber > 40)) then
    raise EArgumentException.Create
      ('version number needs to be between 1 and 40');

  result := TVersion.buildVersions[(versionNumber - 1)];

end;

function TVersion.ToString: string;
begin
  result := self.versionNumber.ToString();
end;

class procedure TVersion.ClassInit();
begin
  TVersion.VERSION_DECODE_INFO := TArray<Integer>.Create($7C94, $85BC, $9A99,
    $A4D3, $BBF6, $C762, $D847, $E60D, $F928, $10B78, $1145D, $12A17, $13532,
    $149A6, $15683, $168C9, $177EC, $18EC4, $191E1, $1AFAB, $1B08E, $1CC1A,
    $1D33F, $1ED75, $1F250, $209D5, $216F0, $228BA, $2379F, $24B0B, $2542E,
    $26A64, $27541, $28C69);
end;

Initialization

TVersion.ClassInit();

end.
