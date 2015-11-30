unit Mode;

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

uses SysUtils, Version;

type

  TMode = class
  private
    characterCountBitsForVersions: TArray<Integer>;
    constructor Create(characterCountBitsForVersions: TArray<Integer>;
      bits: Integer; name: string); overload;
    class procedure ClassInit;
  public
    name: string;
    bits: Integer;

    class var ALPHANUMERIC: TMode;
    class var BYTE: TMode;
    class var ECI: TMode;
    class var FNC1_FIRST_POSITION: TMode;
    class var FNC1_SECOND_POSITION: TMode;
    class var HANZI: TMode;
    class var KANJI: TMode;
    class var NUMERIC: TMode;
    class var STRUCTURED_APPEND: TMode;
    class var TERMINATOR: TMode;

    class function forBits(bits: Integer): TMode; static;
    function getCharacterCountBits(Version: TVersion): Integer;
    function ToString: string; override;

  end;

implementation

{ TMode }

class procedure TMode.ClassInit;
var
  CS: TArray<Integer>;
begin
  CS := TArray<Integer>.Create(0, 0, 0);
  TMode.TERMINATOR := TMode.Create(CS, 0, 'TERMINATOR');
  TMode.NUMERIC := TMode.Create(TArray<Integer>.Create(10, 12, 14), 1,
    'NUMERIC');
  TMode.ALPHANUMERIC := TMode.Create(TArray<Integer>.Create(9, 11, 13), 2,
    'ALPHANUMERIC');
  CS := TArray<Integer>.Create(0, 0, 0);
  TMode.STRUCTURED_APPEND := TMode.Create(CS, 3, 'STRUCTURED_APPEND');
  TMode.BYTE := TMode.Create(TArray<Integer>.Create(8, $10, $10), 4, 'BYTE');
  TMode.ECI := TMode.Create(nil, 7, 'ECI');
  TMode.KANJI := TMode.Create(TArray<Integer>.Create(8, 10, 12), 8, 'KANJI');
  TMode.FNC1_FIRST_POSITION := TMode.Create(nil, 5, 'FNC1_FIRST_POSITION');
  TMode.FNC1_SECOND_POSITION := TMode.Create(nil, 9, 'FNC1_SECOND_POSITION');
  TMode.HANZI := TMode.Create(TArray<Integer>.Create(8, 10, 12), 13, 'HANZI')
end;

constructor TMode.Create(characterCountBitsForVersions: TArray<Integer>;
  bits: Integer; name: string);
begin
  self.characterCountBitsForVersions := characterCountBitsForVersions;
  self.bits := bits;
  self.name := name
end;

class function TMode.forBits(bits: Integer): TMode;
begin
  case bits of
    0:
      begin
        begin
          Result := TMode.TERMINATOR;
          exit
        end
      end;
    1:
      begin
        begin
          Result := TMode.NUMERIC;
          exit
        end
      end;
    2:
      begin
        begin
          Result := TMode.ALPHANUMERIC;
          exit
        end
      end;
    3:
      begin
        begin
          Result := TMode.STRUCTURED_APPEND;
          exit
        end
      end;
    4:
      begin
        begin
          Result := TMode.BYTE;
          exit
        end
      end;
    5:
      begin
        begin
          Result := TMode.FNC1_FIRST_POSITION;
          exit
        end
      end;
    7:
      begin
        begin
          Result := TMode.ECI;
          exit
        end
      end;
    8:
      begin
        begin
          Result := TMode.KANJI;
          exit
        end
      end;
    9:
      begin
        begin
          Result := TMode.FNC1_SECOND_POSITION;
          exit
        end
      end;
    13:
      begin
        begin
          Result := TMode.HANZI;
          exit
        end
      end;
  end;

  raise EArgumentException.Create('Argument exception');
end;

function TMode.getCharacterCountBits(Version: TVersion): Integer;
var
  offset, number: Integer;
begin

  if (self.characterCountBitsForVersions = nil) then
    raise EArgumentException.Create
      ('Character count doesn''t apply to this mode');

  number := Version.VersionNumber;
  if (number <= 9) then
    offset := 0
  else if (number <= $1A) then
    offset := 1
  else
    offset := 2;

  Result := self.characterCountBitsForVersions[offset];
end;

function TMode.ToString: string;
begin
  Result := self.name
end;

Initialization

TMode.ClassInit;

end.
