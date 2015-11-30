unit Datamask;

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

uses Bitmatrix, SysUtils, MathUtils;

type

  TDataMask = class abstract
  private
    class var DATA_MASKS: TArray<TDataMask>;
  public
    class function forReference(reference: Integer): TDataMask; static;
    procedure unmaskBitMatrix(bits: TBitMatrix; dimension: Integer);
    function isMasked(i: Integer; j: Integer): boolean; virtual; abstract;
    constructor Create;
  end;

  TDataMask000 = class(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask001 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask010 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask011 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask100 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask101 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask110 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

  TDataMask111 = class sealed(TDataMask)
  public
    function isMasked(i: Integer; j: Integer): boolean; override;
  end;

implementation

{ TDataMask }

constructor TDataMask.Create;
begin

end;

class function TDataMask.forReference(reference: Integer): TDataMask;
begin
  if ((reference < 0) or (reference > 7)) then
    raise EArgumentException.Create('');

  Result := TDataMask.DATA_MASKS[reference];
end;

procedure TDataMask.unmaskBitMatrix(bits: TBitMatrix; dimension: Integer);
var
  i, j: Integer;
begin
  i := 0;
  while ((i < dimension)) do
  begin
    j := 0;
    while ((j < dimension)) do
    begin
      if (self.isMasked(i, j)) then
        bits.flip(j, i);
      inc(j)
    end;
    inc(i)
  end;
end;
{ TDataMask000 }

function TDataMask000.isMasked(i, j: Integer): boolean;
begin
  Result := (((i + j) and 1) = 0)
end;

{ TDataMask001 }

function TDataMask001.isMasked(i, j: Integer): boolean;
begin
  Result := ((i and 1) = 0)
end;

{ TDataMask010 }

function TDataMask010.isMasked(i, j: Integer): boolean;
begin
  Result := ((j mod 3) = 0)
end;

{ TDataMask011 }

function TDataMask011.isMasked(i, j: Integer): boolean;
begin
  Result := (((i + j) mod 3) = 0)
end;

{ TDataMask100 }

function TDataMask100.isMasked(i, j: Integer): boolean;
begin
  Result := (((TMathUtils.Asr(i, 1) + (j div 3)) and 1) = 0)
end;

{ TDataMask101 }

function TDataMask101.isMasked(i, j: Integer): boolean;
var
  temp: Integer;
begin
  temp := (i * j);
  Result := (((temp and 1) + (temp mod 3)) = 0)
end;

{ TDataMask110 }

function TDataMask110.isMasked(i, j: Integer): boolean;
var
  temp: Integer;
begin
  temp := (i * j);
  Result := ((((temp and 1) + (temp mod 3)) and 1) = 0)
end;

{ TDataMask111 }

function TDataMask111.isMasked(i, j: Integer): boolean;
begin
  Result := (((((i + j) and 1) + ((i * j) mod 3)) and 1) = 0)
end;

Initialization

TDataMask.DATA_MASKS := TArray<TDataMask>.Create(TDataMask000.Create,
  TDataMask001.Create, TDataMask010.Create, TDataMask011.Create,
  TDataMask100.Create, TDataMask101.Create, TDataMask110.Create,
  TDataMask111.Create);

end.
