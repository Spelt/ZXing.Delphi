unit BitMatrixParser;

interface

uses SysUtils, Generics.Collections, DecodeHintType, BitMatrixx, Version,
  FormatInformation;

type

  TBitMatrixParser = class
  private

  var
    BitMatrix: TBitMatrix;
    mirrored: boolean;
    parsedVersion: TVersion;
    parsedFormatInfo: TFormatInformation;

    constructor Create(BitMatrix: TBitMatrix);
    function copyBit(i: Integer; j: Integer; versionBits: Integer): Integer;

    class function createBitMatrixParser(BitMatrix: TBitMatrix)
      : TBitMatrixParser; static;

    procedure mirror;
    function readCodewords: TArray<Byte>;
    function readFormatInformation: TFormatInformation;
    function readVersion: TVersion;
    procedure remask;
    procedure setMirror(mirror: boolean);
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
begin

end;

procedure TBitMatrixParser.mirror;
begin

end;

function TBitMatrixParser.readCodewords: TArray<Byte>;
begin

end;

function TBitMatrixParser.readFormatInformation: TFormatInformation;
begin

end;

function TBitMatrixParser.readVersion: TVersion;
begin

end;

procedure TBitMatrixParser.remask;
begin

end;

procedure TBitMatrixParser.setMirror(mirror: boolean);
begin

end;

end.
