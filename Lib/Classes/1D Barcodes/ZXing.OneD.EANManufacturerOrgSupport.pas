{
  * Copyright 2010 ZXing authors
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

  * Original Author: Sean Owen
  * Delphi Implementation by K. Gossens
}

unit ZXing.OneD.EANManufacturerOrgSupport;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  ZXing.OneD.UPCEANExtension2Support,
  ZXing.OneD.UPCEANExtension5Support,
  ZXing.Reader,
  ZXing.BinaryBitmap,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultMetadataType,
  ZXing.ResultPoint,
  ZXing.Common.BitArray,
  ZXing.Common.Detector.MathUtils;

type
  // <summary>
  /// Records EAN prefix to GS1 Member Organization, where the member organization
  /// correlates strongly with a country. This is an imperfect means of identifying
  /// a country of origin by EAN-13 barcode value. See
  /// <a href="http://en.wikipedia.org/wiki/List_of_GS1_country_codes">
  /// http://en.wikipedia.org/wiki/List_of_GS1_country_codes</a>.
  /// </summary>
  TEANManufacturerOrgSupport = class sealed
  private
  var
    ranges: TList<TArray<Integer>>;
    countryIdentifiers: TStringList;

    procedure add(const range: TArray<Integer>; const id: string);
    procedure initIfNeeded;
  public
    constructor Create;
    destructor Destroy; override;

    function lookupCountryIdentifier(const productCode: String): String;
  end;

implementation

{ TEANManufacturerOrgSupport }

constructor TEANManufacturerOrgSupport.Create;
begin
  ranges := TList < TArray < Integer >>.Create;
  countryIdentifiers := TStringList.Create;
end;

destructor TEANManufacturerOrgSupport.Destroy;
begin
  countryIdentifiers.Free;
  ranges.Free;
  inherited;
end;

function TEANManufacturerOrgSupport.lookupCountryIdentifier(const productCode
  : String): String;
var
  i, prefix, max: Integer;
  range: TArray<Integer>;
  start, ending: Integer;
begin
  Result := '';

  initIfNeeded;
  prefix := Integer.Parse(productCode.Substring(0, 3));
  max := ranges.Count;
  for i := 0 to Pred(max) do
  begin
    range := ranges[i];
    start := range[0];
    if (prefix < start) then
      exit;
    if (Length(range) = 1) then
      ending := start
    else
      ending := range[1];
    if (prefix <= ending) then
    begin
      Result := countryIdentifiers[i];
      break;
    end;
  end;
end;

procedure TEANManufacturerOrgSupport.add(const range: TArray<Integer>;
  const id: string);
begin
  ranges.add(range);
  countryIdentifiers.add(id);
end;

procedure TEANManufacturerOrgSupport.initIfNeeded;
begin
  if (ranges.Count = 0) then
  begin
    add(TArray<Integer>.Create(0, 19), 'US/CA');
    add(TArray<Integer>.Create(30, 39), 'US');
    add(TArray<Integer>.Create(60, 139), 'US/CA');
    add(TArray<Integer>.Create(300, 379), 'FR');
    add(TArray<Integer>.Create(380), 'BG');
    add(TArray<Integer>.Create(383), 'SI');
    add(TArray<Integer>.Create(385), 'HR');
    add(TArray<Integer>.Create(387), 'BA');
    add(TArray<Integer>.Create(400, 440), 'DE');
    add(TArray<Integer>.Create(450, 459), 'JP');
    add(TArray<Integer>.Create(460, 469), 'RU');
    add(TArray<Integer>.Create(471), 'TW');
    add(TArray<Integer>.Create(474), 'EE');
    add(TArray<Integer>.Create(475), 'LV');
    add(TArray<Integer>.Create(476), 'AZ');
    add(TArray<Integer>.Create(477), 'LT');
    add(TArray<Integer>.Create(478), 'UZ');
    add(TArray<Integer>.Create(479), 'LK');
    add(TArray<Integer>.Create(480), 'PH');
    add(TArray<Integer>.Create(481), 'BY');
    add(TArray<Integer>.Create(482), 'UA');
    add(TArray<Integer>.Create(484), 'MD');
    add(TArray<Integer>.Create(485), 'AM');
    add(TArray<Integer>.Create(486), 'GE');
    add(TArray<Integer>.Create(487), 'KZ');
    add(TArray<Integer>.Create(489), 'HK');
    add(TArray<Integer>.Create(490, 499), 'JP');
    add(TArray<Integer>.Create(500, 509), 'GB');
    add(TArray<Integer>.Create(520), 'GR');
    add(TArray<Integer>.Create(528), 'LB');
    add(TArray<Integer>.Create(529), 'CY');
    add(TArray<Integer>.Create(531), 'MK');
    add(TArray<Integer>.Create(535), 'MT');
    add(TArray<Integer>.Create(539), 'IE');
    add(TArray<Integer>.Create(540, 549), 'BE/LU');
    add(TArray<Integer>.Create(560), 'PT');
    add(TArray<Integer>.Create(569), 'IS');
    add(TArray<Integer>.Create(570, 579), 'DK');
    add(TArray<Integer>.Create(590), 'PL');
    add(TArray<Integer>.Create(594), 'RO');
    add(TArray<Integer>.Create(599), 'HU');
    add(TArray<Integer>.Create(600, 601), 'ZA');
    add(TArray<Integer>.Create(603), 'GH');
    add(TArray<Integer>.Create(608), 'BH');
    add(TArray<Integer>.Create(609), 'MU');
    add(TArray<Integer>.Create(611), 'MA');
    add(TArray<Integer>.Create(613), 'DZ');
    add(TArray<Integer>.Create(616), 'KE');
    add(TArray<Integer>.Create(618), 'CI');
    add(TArray<Integer>.Create(619), 'TN');
    add(TArray<Integer>.Create(621), 'SY');
    add(TArray<Integer>.Create(622), 'EG');
    add(TArray<Integer>.Create(624), 'LY');
    add(TArray<Integer>.Create(625), 'JO');
    add(TArray<Integer>.Create(626), 'IR');
    add(TArray<Integer>.Create(627), 'KW');
    add(TArray<Integer>.Create(628), 'SA');
    add(TArray<Integer>.Create(629), 'AE');
    add(TArray<Integer>.Create(640, 649), 'FI');
    add(TArray<Integer>.Create(690, 695), 'CN');
    add(TArray<Integer>.Create(700, 709), 'NO');
    add(TArray<Integer>.Create(729), 'IL');
    add(TArray<Integer>.Create(730, 739), 'SE');
    add(TArray<Integer>.Create(740), 'GT');
    add(TArray<Integer>.Create(741), 'SV');
    add(TArray<Integer>.Create(742), 'HN');
    add(TArray<Integer>.Create(743), 'NI');
    add(TArray<Integer>.Create(744), 'CR');
    add(TArray<Integer>.Create(745), 'PA');
    add(TArray<Integer>.Create(746), 'DO');
    add(TArray<Integer>.Create(750), 'MX');
    add(TArray<Integer>.Create(754, 755), 'CA');
    add(TArray<Integer>.Create(759), 'VE');
    add(TArray<Integer>.Create(760, 769), 'CH');
    add(TArray<Integer>.Create(770), 'CO');
    add(TArray<Integer>.Create(773), 'UY');
    add(TArray<Integer>.Create(775), 'PE');
    add(TArray<Integer>.Create(777), 'BO');
    add(TArray<Integer>.Create(779), 'AR');
    add(TArray<Integer>.Create(780), 'CL');
    add(TArray<Integer>.Create(784), 'PY');
    add(TArray<Integer>.Create(785), 'PE');
    add(TArray<Integer>.Create(786), 'EC');
    add(TArray<Integer>.Create(789, 790), 'BR');
    add(TArray<Integer>.Create(800, 839), 'IT');
    add(TArray<Integer>.Create(840, 849), 'ES');
    add(TArray<Integer>.Create(850), 'CU');
    add(TArray<Integer>.Create(858), 'SK');
    add(TArray<Integer>.Create(859), 'CZ');
    add(TArray<Integer>.Create(860), 'YU');
    add(TArray<Integer>.Create(865), 'MN');
    add(TArray<Integer>.Create(867), 'KP');
    add(TArray<Integer>.Create(868, 869), 'TR');
    add(TArray<Integer>.Create(870, 879), 'NL');
    add(TArray<Integer>.Create(880), 'KR');
    add(TArray<Integer>.Create(885), 'TH');
    add(TArray<Integer>.Create(888), 'SG');
    add(TArray<Integer>.Create(890), 'IN');
    add(TArray<Integer>.Create(893), 'VN');
    add(TArray<Integer>.Create(896), 'PK');
    add(TArray<Integer>.Create(899), 'ID');
    add(TArray<Integer>.Create(900, 919), 'AT');
    add(TArray<Integer>.Create(930, 939), 'AU');
    add(TArray<Integer>.Create(940, 949), 'AZ');
    add(TArray<Integer>.Create(955), 'MY');
    add(TArray<Integer>.Create(958), 'MO');
  end;
end;

end.
