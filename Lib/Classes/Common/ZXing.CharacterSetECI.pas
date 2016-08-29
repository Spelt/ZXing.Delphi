unit ZXing.CharacterSetECI;

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

uses SysUtils, Generics.Collections;

type

  TECI = class abstract
  private
    value_Renamed: Integer;
    constructor Create(value_Renamed: Integer);
  public
    class function getECIByValue(value_Renamed: Integer): TECI; static;
    property value: Integer read value_Renamed;
  end;

  TCharacterSetECI = class sealed(TECI)
  strict private
    FEncodingName: string;
    class var NAME_TO_ECI: TDictionary<string, TCharacterSetECI>;
    class var VALUE_TO_ECI: TDictionary<Integer, TCharacterSetECI>;
    ///<summary>
    /// this objectlist is just for doing proper deallocation of TCharacterSetECI instances under Win32/Win64,
    /// without having to mess with TInterfacedObject. Since instances are meant to exist for the whole program life, there's no need of doing
    /// anything much more complicated than this.
    /// </summary>
    class var AllocatedInstances:TObjectList<TCharacterSetECI>;

    class constructor Create;
    constructor Create(value: Integer; encodingName: string); overload;
    class destructor Destroy;

    class procedure addCharacterSet(value: Integer;  encodingNames: TArray<string>); static;

  public

    property encodingName: string read FEncodingName;

    class function getCharacterSetECIByName(name: string)
      : TCharacterSetECI; static;
    class function getCharacterSetECIByValue(value: Integer)
      : TCharacterSetECI; static;

  end;

implementation

{ TECI }

constructor TECI.Create(value_Renamed: Integer);
begin
  self.value_Renamed := value_Renamed
end;

class function TECI.getECIByValue(value_Renamed: Integer): TECI;
begin
  if ((value_Renamed < 0) or (value_Renamed > $F423F)) then
    raise EArgumentException.Create('Bad ECI value: ' +
      value_Renamed.ToString());

  if (value_Renamed < 900) then
  begin
    Result := TCharacterSetECI.getCharacterSetECIByValue(value_Renamed);
    exit
  end;

  Result := nil;
end;

{ TCharacterSetECI }

constructor TCharacterSetECI.Create(value: Integer; encodingName: string);
begin
  inherited Create(value);
  FEncodingName := encodingName;
  AllocatedInstances.Add(self); // add the instance to the global list of all created instances
end;

class destructor TCharacterSetECI.Destroy;
begin
   NAME_TO_ECI.Free;
   VALUE_TO_ECI.Free;

   if  AllocatedInstances<>nil then begin
      AllocatedInstances.Clear;
      AllocatedInstances.Free;
end;

end;

class constructor TCharacterSetECI.Create;
begin
  NAME_TO_ECI:=TDictionary<string, TCharacterSetECI>.Create;
  VALUE_TO_ECI:=TDictionary<Integer, TCharacterSetECI>.Create;
  // under Win32/64 TObjectList by default "OWNS" the objects it contains:
  // if we clear the list, the objects contained get automatically destroyed
  AllocatedInstances:= TObjectList<TCharacterSetECI>.Create;

  TCharacterSetECI.addCharacterSet(  0, ['CP437']);
  TCharacterSetECI.addCharacterSet(  1, ['ISO-8859-1','ISO8859_1']);
  TCharacterSetECI.addCharacterSet(  2, ['CP437']);
  TCharacterSetECI.addCharacterSet(  3, ['ISO-8859-1', 'ISO8859_1']);
  TCharacterSetECI.addCharacterSet(  4, ['ISO-8859-2', 'ISO8859_2']);
  TCharacterSetECI.addCharacterSet(  5, ['ISO-8859-3', 'ISO8859_3']);
  TCharacterSetECI.addCharacterSet(  6, ['ISO-8859-4', 'ISO8859_4']);
  TCharacterSetECI.addCharacterSet(  7, ['ISO-8859-5', 'ISO8859_5']);
  TCharacterSetECI.addCharacterSet(  8, ['ISO-8859-6', 'ISO8859_6']);
  TCharacterSetECI.addCharacterSet(  9, ['ISO-8859-7', 'ISO8859_7']);
  TCharacterSetECI.addCharacterSet( 10, ['ISO-8859-8', 'ISO8859_8']);
  TCharacterSetECI.addCharacterSet( 11, ['ISO-8859-9', 'ISO8859_9']);
  TCharacterSetECI.addCharacterSet( 12, ['ISO-8859-4', 'ISO-8859-10', 'ISO8859_10']);
  TCharacterSetECI.addCharacterSet( 13, ['ISO-8859-11', 'ISO8859_11']);
  TCharacterSetECI.addCharacterSet( 15, ['ISO-8859-13', 'ISO8859_13']);
  TCharacterSetECI.addCharacterSet($10, ['ISO-8859-1', 'ISO-8859-14', 'ISO8859_14']);
  TCharacterSetECI.addCharacterSet($11, ['ISO-8859-15', 'ISO8859_15']);
  TCharacterSetECI.addCharacterSet($12, ['ISO-8859-3', 'ISO-8859-16', 'ISO8859_16']);
  TCharacterSetECI.addCharacterSet( 20, ['SJIS', 'Shift_JIS']);
  TCharacterSetECI.addCharacterSet($15, ['WINDOWS-1250','CP1250']);
  TCharacterSetECI.addCharacterSet($16, ['WINDOWS-1251','CP1251']);
  TCharacterSetECI.addCharacterSet($17, ['WINDOWS-1252','CP1252']);
  TCharacterSetECI.addCharacterSet($18, ['WINDOWS-1256', 'CP1256']);
  TCharacterSetECI.addCharacterSet($19, ['UTF-16BE','UNICODEBIG']);
  TCharacterSetECI.addCharacterSet($1A, ['UTF-8', 'UTF8']);
  TCharacterSetECI.addCharacterSet($1B, ['US-ASCII']);
  TCharacterSetECI.addCharacterSet(170, ['US-ASCII']);
  TCharacterSetECI.addCharacterSet($1C, ['BIG5']);
  TCharacterSetECI.addCharacterSet($1D, ['GB18030', 'GB2312', 'EUC_CN', 'GBK'] );
  TCharacterSetECI.addCharacterSet( 30, ['EUC-KR', 'EUC_KR']);

end;

class procedure TCharacterSetECI.addCharacterSet(value: Integer; encodingNames: TArray<string>);
var
  t: string;
  Eci: TCharacterSetECI;
begin
  Eci := TCharacterSetECI.Create(value, encodingNames[0]);
  TCharacterSetECI.VALUE_TO_ECI.AddOrSetValue(value,Eci);   // note : VALUE_TO_ECI[value] := Eci raises "key not found exception": it cant' be used for adding not existing values

  for t in encodingNames do
     TCharacterSetECI.NAME_TO_ECI.AddOrSetValue(t,Eci);
end;

class function TCharacterSetECI.getCharacterSetECIByName(name: string)
  : TCharacterSetECI;
begin
  Result := TCharacterSetECI.NAME_TO_ECI[name.ToUpper]
end;

class function TCharacterSetECI.getCharacterSetECIByValue(value: Integer)
  : TCharacterSetECI;
begin
  if ((value < 0) or (value >= 900)) then
  begin
    Result := nil;
    exit
  end;

  Result := TCharacterSetECI.VALUE_TO_ECI[value];
end;

end.
