unit ZXing.GS1;

{
  * Copyright 2022 Axel Waggershauser
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

  * Ported from zxing-cpp (HRI.cpp): the human readable interpretation (HRI)
  * of GS1 element strings, like '(01)05909990329717(21)1039'. The table of
  * the application identifiers (AI) comes from the GS1 Syntax Dictionary
  * (https://github.com/gs1/gs1-syntax-dictionary, 2025-12-08).
}

interface

/// <summary>
/// The human readable interpretation of the GS1 element strings in gs1: every
/// application identifier (AI) between parentheses followed by its data, like
/// '(01)05909990329717(17)270331(10)AB123'. Data of variable length ends at a
/// GS character (ASCII 29) or at its maximum length. Returns '' when gs1 is
/// not valid GS1 data (an unknown AI or too short data).
/// </summary>
function HRIFromGS1(const gs1: string): string;

implementation

uses
  System.SysUtils;

type
  TAiInfo = record
    /// <summary>The AI, or its first digits.</summary>
    Prefix: string;
    /// <summary>The length of the data; negative for a variable length
    /// with this maximum.</summary>
    Size: Integer;
  end;

const
  AI_INFOS: array [0 .. 213] of TAiInfo = (
    (Prefix: '00'; Size: 18),
    (Prefix: '01'; Size: 14),
    (Prefix: '02'; Size: 14),
    (Prefix: '03'; Size: 14),
    (Prefix: '10'; Size: -20),
    (Prefix: '11'; Size: 6),
    (Prefix: '12'; Size: 6),
    (Prefix: '13'; Size: 6),
    (Prefix: '15'; Size: 6),
    (Prefix: '16'; Size: 6),
    (Prefix: '17'; Size: 6),
    (Prefix: '20'; Size: 2),
    (Prefix: '21'; Size: -20),
    (Prefix: '22'; Size: -20),
    (Prefix: '30'; Size: -8),
    (Prefix: '37'; Size: -8),
    (Prefix: '90'; Size: -30),
    (Prefix: '91'; Size: -90),
    (Prefix: '92'; Size: -90),
    (Prefix: '93'; Size: -90),
    (Prefix: '94'; Size: -90),
    (Prefix: '95'; Size: -90),
    (Prefix: '96'; Size: -90),
    (Prefix: '97'; Size: -90),
    (Prefix: '98'; Size: -90),
    (Prefix: '99'; Size: -90),
    (Prefix: '235'; Size: -28),
    (Prefix: '240'; Size: -30),
    (Prefix: '241'; Size: -30),
    (Prefix: '242'; Size: -6),
    (Prefix: '243'; Size: -20),
    (Prefix: '250'; Size: -30),
    (Prefix: '251'; Size: -30),
    (Prefix: '253'; Size: -30),
    (Prefix: '254'; Size: -20),
    (Prefix: '255'; Size: -25),
    (Prefix: '400'; Size: -30),
    (Prefix: '401'; Size: -30),
    (Prefix: '402'; Size: 17),
    (Prefix: '403'; Size: -30),
    (Prefix: '410'; Size: 13),
    (Prefix: '411'; Size: 13),
    (Prefix: '412'; Size: 13),
    (Prefix: '413'; Size: 13),
    (Prefix: '414'; Size: 13),
    (Prefix: '415'; Size: 13),
    (Prefix: '416'; Size: 13),
    (Prefix: '417'; Size: 13),
    (Prefix: '420'; Size: -20),
    (Prefix: '421'; Size: -12),
    (Prefix: '422'; Size: 3),
    (Prefix: '423'; Size: -15),
    (Prefix: '424'; Size: 3),
    (Prefix: '425'; Size: -15),
    (Prefix: '426'; Size: 3),
    (Prefix: '427'; Size: -3),
    (Prefix: '710'; Size: -20),
    (Prefix: '711'; Size: -20),
    (Prefix: '712'; Size: -20),
    (Prefix: '713'; Size: -20),
    (Prefix: '714'; Size: -20),
    (Prefix: '715'; Size: -20),
    (Prefix: '716'; Size: -20),
    (Prefix: '717'; Size: -20),
    (Prefix: '310'; Size: 6),
    (Prefix: '311'; Size: 6),
    (Prefix: '312'; Size: 6),
    (Prefix: '313'; Size: 6),
    (Prefix: '314'; Size: 6),
    (Prefix: '315'; Size: 6),
    (Prefix: '316'; Size: 6),
    (Prefix: '320'; Size: 6),
    (Prefix: '321'; Size: 6),
    (Prefix: '322'; Size: 6),
    (Prefix: '323'; Size: 6),
    (Prefix: '324'; Size: 6),
    (Prefix: '325'; Size: 6),
    (Prefix: '326'; Size: 6),
    (Prefix: '327'; Size: 6),
    (Prefix: '328'; Size: 6),
    (Prefix: '329'; Size: 6),
    (Prefix: '330'; Size: 6),
    (Prefix: '331'; Size: 6),
    (Prefix: '332'; Size: 6),
    (Prefix: '333'; Size: 6),
    (Prefix: '334'; Size: 6),
    (Prefix: '335'; Size: 6),
    (Prefix: '336'; Size: 6),
    (Prefix: '337'; Size: 6),
    (Prefix: '340'; Size: 6),
    (Prefix: '341'; Size: 6),
    (Prefix: '342'; Size: 6),
    (Prefix: '343'; Size: 6),
    (Prefix: '344'; Size: 6),
    (Prefix: '345'; Size: 6),
    (Prefix: '346'; Size: 6),
    (Prefix: '347'; Size: 6),
    (Prefix: '348'; Size: 6),
    (Prefix: '349'; Size: 6),
    (Prefix: '350'; Size: 6),
    (Prefix: '351'; Size: 6),
    (Prefix: '352'; Size: 6),
    (Prefix: '353'; Size: 6),
    (Prefix: '354'; Size: 6),
    (Prefix: '355'; Size: 6),
    (Prefix: '356'; Size: 6),
    (Prefix: '357'; Size: 6),
    (Prefix: '360'; Size: 6),
    (Prefix: '361'; Size: 6),
    (Prefix: '362'; Size: 6),
    (Prefix: '363'; Size: 6),
    (Prefix: '364'; Size: 6),
    (Prefix: '365'; Size: 6),
    (Prefix: '366'; Size: 6),
    (Prefix: '367'; Size: 6),
    (Prefix: '368'; Size: 6),
    (Prefix: '369'; Size: 6),
    (Prefix: '390'; Size: -15),
    (Prefix: '391'; Size: -18),
    (Prefix: '392'; Size: -15),
    (Prefix: '393'; Size: -18),
    (Prefix: '394'; Size: 4),
    (Prefix: '395'; Size: 6),
    (Prefix: '703'; Size: -30),
    (Prefix: '723'; Size: -30),
    (Prefix: '4300'; Size: -35),
    (Prefix: '4301'; Size: -35),
    (Prefix: '4302'; Size: -70),
    (Prefix: '4303'; Size: -70),
    (Prefix: '4304'; Size: -70),
    (Prefix: '4305'; Size: -70),
    (Prefix: '4306'; Size: -70),
    (Prefix: '4307'; Size: 2),
    (Prefix: '4308'; Size: -30),
    (Prefix: '4309'; Size: 20),
    (Prefix: '4310'; Size: -35),
    (Prefix: '4311'; Size: -35),
    (Prefix: '4312'; Size: -70),
    (Prefix: '4313'; Size: -70),
    (Prefix: '4314'; Size: -70),
    (Prefix: '4315'; Size: -70),
    (Prefix: '4316'; Size: -70),
    (Prefix: '4317'; Size: 2),
    (Prefix: '4318'; Size: -20),
    (Prefix: '4319'; Size: -30),
    (Prefix: '4320'; Size: -35),
    (Prefix: '4321'; Size: 1),
    (Prefix: '4322'; Size: 1),
    (Prefix: '4323'; Size: 1),
    (Prefix: '4324'; Size: 10),
    (Prefix: '4325'; Size: 10),
    (Prefix: '4326'; Size: 6),
    (Prefix: '4330'; Size: -7),
    (Prefix: '4331'; Size: -7),
    (Prefix: '4332'; Size: -7),
    (Prefix: '4333'; Size: -7),
    (Prefix: '7001'; Size: 13),
    (Prefix: '7002'; Size: -30),
    (Prefix: '7003'; Size: 10),
    (Prefix: '7004'; Size: -4),
    (Prefix: '7005'; Size: -12),
    (Prefix: '7006'; Size: 6),
    (Prefix: '7007'; Size: -12),
    (Prefix: '7008'; Size: -3),
    (Prefix: '7009'; Size: -10),
    (Prefix: '7010'; Size: -2),
    (Prefix: '7011'; Size: -10),
    (Prefix: '7020'; Size: -20),
    (Prefix: '7021'; Size: -20),
    (Prefix: '7022'; Size: -20),
    (Prefix: '7023'; Size: -30),
    (Prefix: '7040'; Size: 4),
    (Prefix: '7041'; Size: -4),
    (Prefix: '7240'; Size: -20),
    (Prefix: '7241'; Size: 2),
    (Prefix: '7242'; Size: -25),
    (Prefix: '7250'; Size: 8),
    (Prefix: '7251'; Size: 12),
    (Prefix: '7252'; Size: 1),
    (Prefix: '7253'; Size: -40),
    (Prefix: '7254'; Size: -40),
    (Prefix: '7255'; Size: -10),
    (Prefix: '7256'; Size: -90),
    (Prefix: '7257'; Size: -70),
    (Prefix: '7258'; Size: 3),
    (Prefix: '7259'; Size: -40),
    (Prefix: '8001'; Size: 14),
    (Prefix: '8002'; Size: -20),
    (Prefix: '8003'; Size: -30),
    (Prefix: '8004'; Size: -30),
    (Prefix: '8005'; Size: 6),
    (Prefix: '8006'; Size: 18),
    (Prefix: '8007'; Size: -34),
    (Prefix: '8008'; Size: -12),
    (Prefix: '8009'; Size: -50),
    (Prefix: '8010'; Size: -30),
    (Prefix: '8011'; Size: -12),
    (Prefix: '8012'; Size: -20),
    (Prefix: '8013'; Size: -25),
    (Prefix: '8014'; Size: -25),
    (Prefix: '8017'; Size: 18),
    (Prefix: '8018'; Size: 18),
    (Prefix: '8019'; Size: -10),
    (Prefix: '8020'; Size: -25),
    (Prefix: '8026'; Size: 18),
    (Prefix: '8030'; Size: -90),
    (Prefix: '8040'; Size: 15),
    (Prefix: '8041'; Size: 15),
    (Prefix: '8042'; Size: 32),
    (Prefix: '8043'; Size: -20),
    (Prefix: '8110'; Size: -70),
    (Prefix: '8111'; Size: 4),
    (Prefix: '8112'; Size: -70),
    (Prefix: '8200'; Size: -70)
  );

/// <summary>The number of digits of the AI: 4 for the AIs 31nn to 36nn
/// (with a decimal point position) and 703n and 723n, otherwise the length
/// of the prefix.</summary>
function AiSize(const info: TAiInfo): Integer;
begin
  if ((info.Prefix[1] = '3') and (Pos(info.Prefix[2], '1234569') > 0)) or
    (info.Prefix = '703') or (info.Prefix = '723') then
    Result := 4
  else
    Result := Length(info.Prefix);
end;

function HRIFromGS1(const gs1: string): string;
const
  GS = #29; // the GS character
begin
  Result := '';
  var res := TStringBuilder.Create;
  try
    var pos := 1; // the first character of the remaining text
    var len := Length(gs1);
    while (pos <= len) do
    begin
      var info := -1;
      for var i := 0 to High(AI_INFOS) do
        if (Copy(gs1, pos, Length(AI_INFOS[i].Prefix)) = AI_INFOS[i].Prefix) then
        begin
          info := i;
          break;
        end;
      if (info < 0) then
        exit;

      var aiLength := AiSize(AI_INFOS[info]);
      if (len - pos + 1 < aiLength) then
        exit;

      res.Append('(').Append(Copy(gs1, pos, aiLength)).Append(')');
      Inc(pos, aiLength);

      var fieldSize := Abs(AI_INFOS[info].Size);
      if (AI_INFOS[info].Size < 0) then
      begin
        // variable length: up to the GS character, at most the maximum
        var gsPos := System.Pos(GS, gs1, pos);
        var available := len - pos + 1;
        if (gsPos > 0) then
          available := gsPos - pos;
        if (available < fieldSize) then
          fieldSize := available;
      end;
      if (fieldSize = 0) or (len - pos + 1 < fieldSize) then
        exit;

      res.Append(Copy(gs1, pos, fieldSize));
      Inc(pos, fieldSize);

      // a single separator character immediately following any element
      // string is tolerated (GS1 General Specifications 7.8.6.3)
      if (pos <= len) and (gs1[pos] = GS) then
        Inc(pos);
    end;
    Result := res.ToString;
  finally
    res.Free;
  end;
end;

end.
