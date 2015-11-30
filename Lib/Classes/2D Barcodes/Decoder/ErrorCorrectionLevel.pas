unit ErrorCorrectionLevel;

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

uses SysUtils;

type

  TErrorCorrectionLevel = class
  private
    Fbits: Integer;
    Fname: string;
    Fordinal_Renamed_Field: Integer;
    class var FOR_BITS: TArray<TErrorCorrectionLevel>;
    class procedure InitClass; Static;
  public
    constructor Create(ordinal: Integer; bits: Integer; name: string);
    class function forBits(bits: Integer): TErrorCorrectionLevel; static;
    class var H: TErrorCorrectionLevel;
    class var L: TErrorCorrectionLevel;
    class var M: TErrorCorrectionLevel;
    class var Q: TErrorCorrectionLevel;

    function ordinal: Integer;
    function ToString: string; override;

    property bits: Integer read Fbits;
    property Name: string read Fname;
  end;

implementation

class procedure TErrorCorrectionLevel.InitClass;
begin
  H := TErrorCorrectionLevel.Create(3, 2, 'H');
  L := TErrorCorrectionLevel.Create(0, 1, 'L');
  M := TErrorCorrectionLevel.Create(1, 0, 'M');
  Q := TErrorCorrectionLevel.Create(2, 3, 'Q');

  FOR_BITS := TArray<TErrorCorrectionLevel>.Create(M, L, H, Q);
end;

constructor TErrorCorrectionLevel.Create(ordinal, bits: Integer; name: string);
begin
  Fordinal_Renamed_Field := ordinal;
  Fbits := bits;
  Fname := name
end;

class function TErrorCorrectionLevel.forBits(bits: Integer)
  : TErrorCorrectionLevel;
begin
  if ((bits < 0) or (bits >= Length(TErrorCorrectionLevel.FOR_BITS))) then
    raise EArgumentException.Create('');
  begin
    Result := TErrorCorrectionLevel.FOR_BITS[bits];
    exit
  end
end;

function TErrorCorrectionLevel.ordinal: Integer;
begin
  Result := Fordinal_Renamed_Field
end;

function TErrorCorrectionLevel.ToString: string;
begin
  Result := Fname
end;

Initialization

TErrorCorrectionLevel.InitClass;

end.
