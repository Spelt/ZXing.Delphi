{
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

  * Australia Post 4-State Customer Barcode for ZXing.Delphi: the encoding of
  * zint (auspost.c) reversed. Reed-Solomon in GF(64), like MaxiCode.
}

unit ZXing.Postal.AustraliaPost;

interface

/// <summary>The text of the bars of an Australia Post barcode (37, 52 or 67
/// bars, states as TPostalBars): the format control code (2 digits), the
/// Delivery Point Identifier (8 digits) and the customer information (Customer
/// Barcode 2 and 3); errors corrected (Reed-Solomon); '' when they are none.
/// </summary>
function DecodeAustraliaPost(const states: TArray<Byte>): string;

implementation

uses
  System.SysUtils,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder,
  ZXing.Postal.FourStateDetector;

const
  // the bars of the digits (N table) and of the characters of GDSET (C table)
  // as zint: 0 full, 1 ascender, 2 descender, 3 tracker
  N_TABLE: array [0 .. 9, 0 .. 1] of Byte = ((0, 0), (0, 1), (0, 2), (1, 0),
    (1, 1), (1, 2), (2, 0), (2, 1), (2, 2), (3, 0));
  GDSET = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz #';
  C_TABLE: array [0 .. 63, 0 .. 2] of Byte = ((2, 2, 2), (3, 0, 0), (3, 0, 1),
    (3, 0, 2), (3, 1, 0), (3, 1, 1), (3, 1, 2), (3, 2, 0), (3, 2, 1),
    (3, 2, 2), (0, 0, 0), (0, 0, 1), (0, 0, 2), (0, 1, 0), (0, 1, 1),
    (0, 1, 2), (0, 2, 0), (0, 2, 1), (0, 2, 2), (1, 0, 0), (1, 0, 1),
    (1, 0, 2), (1, 1, 0), (1, 1, 1), (1, 1, 2), (1, 2, 0), (1, 2, 1),
    (1, 2, 2), (2, 0, 0), (2, 0, 1), (2, 0, 2), (2, 1, 0), (2, 1, 1),
    (2, 1, 2), (2, 2, 0), (2, 2, 1), (0, 2, 3), (0, 3, 0), (0, 3, 1),
    (0, 3, 2), (0, 3, 3), (1, 0, 3), (1, 1, 3), (1, 2, 3), (1, 3, 0),
    (1, 3, 1), (1, 3, 2), (1, 3, 3), (2, 0, 3), (2, 1, 3), (2, 2, 3),
    (2, 3, 0), (2, 3, 1), (2, 3, 2), (2, 3, 3), (3, 0, 3), (3, 1, 3),
    (3, 2, 3), (3, 3, 0), (3, 3, 1), (3, 3, 2), (3, 3, 3), (0, 0, 3),
    (0, 1, 3));
  ECC_SYMBOLS = 4;

/// <summary>The digits of the bars from first to last (2 per digit, N
/// table), trackers at the end being filler; '' when they are none.
/// </summary>
function NDigits(const bars: TArray<Byte>; first, last: Integer): string;
begin
  Result := '';
  // without the filler (no digit ends with a tracker)
  while (last >= first) and (bars[last] = BAR_TRACKER) do
    Dec(last);
  if Odd(last - first + 1) then
    exit;
  var i := first;
  while (i < last) do
  begin
    var digit := -1;
    for var d := 0 to 9 do
      if (bars[i] = N_TABLE[d, 0]) and (bars[i + 1] = N_TABLE[d, 1]) then
        digit := d;
    if (digit < 0) then
      exit('');
    Result := Result + Chr(Ord('0') + digit);
    Inc(i, 2);
  end;
end;

/// <summary>The characters of the bars from first to last (3 per character,
/// C table), trackers at the end being filler (so no z at the end); ''
/// when they are none.</summary>
function CChars(const bars: TArray<Byte>; first, last: Integer): string;
begin
  Result := '';
  // the bars that do not fill a character are filler
  while ((last - first + 1) mod 3 <> 0) do
  begin
    if (bars[last] <> BAR_TRACKER) then
      exit;
    Dec(last);
  end;
  var i := first;
  while (i < last) do
  begin
    var c := -1;
    for var k := 0 to 63 do
      if (bars[i] = C_TABLE[k, 0]) and (bars[i + 1] = C_TABLE[k, 1]) and
        (bars[i + 2] = C_TABLE[k, 2]) then
        c := k;
    if (c < 0) then
      exit('');
    Result := Result + GDSET.Chars[c];
    Inc(i, 3);
  end;
  // 3 trackers at the end are filler as well (the same bars as z)
  while Result.EndsWith('z') do
    SetLength(Result, Length(Result) - 1);
end;

function DecodeAustraliaPost(const states: TArray<Byte>): string;
begin
  Result := '';
  // start and stop: ascender, tracker
  var n := Length(states);
  if ((n <> 37) and (n <> 52) and (n <> 67)) or (states[0] <> BAR_ASCENDER) or
    (states[1] <> BAR_TRACKER) or (states[n - 2] <> BAR_ASCENDER) or
    (states[n - 1] <> BAR_TRACKER) then
    exit;

  // the symbols of 3 bars from the third bar on: the data, then 4 error
  // correction symbols (zint: the lowest one first)
  var count := (n - 4) div 3;
  var dataCount := count - ECC_SYMBOLS;
  var symbols: TArray<Integer>;
  SetLength(symbols, count);
  for var i := 0 to count - 1 do
  begin
    var s := 16 * states[2 + 3 * i] + 4 * states[3 + 3 * i] +
      states[4 + 3 * i];
    if (i < dataCount) then
      symbols[i] := s
    else
      symbols[count - 1 - (i - dataCount)] := s;
  end;
  var rs := TReedSolomonDecoder.Create(TGenericGF.MAXICODE_FIELD_64);
  try
    if not rs.decode(symbols, ECC_SYMBOLS) then
      exit;
  finally
    rs.Free;
  end;

  // the bars of the (corrected) data
  var bars: TArray<Byte>;
  SetLength(bars, 3 * dataCount);
  for var i := 0 to dataCount - 1 do
  begin
    bars[3 * i] := symbols[i] shr 4;
    bars[3 * i + 1] := (symbols[i] shr 2) and 3;
    bars[3 * i + 2] := symbols[i] and 3;
  end;

  // the format control code and the Delivery Point Identifier
  var fcc := NDigits(bars, 0, 3);
  var dpid := NDigits(bars, 4, 19);
  if (Length(fcc) <> 2) or (Length(dpid) <> 8) then
    exit;
  // the customer information of Customer Barcode 2 and 3: digits (N) or
  // characters (C); the rest filler
  var info := '';
  if (n > 37) then
  begin
    if ((fcc <> '59') or (n <> 52)) and ((fcc <> '62') or (n <> 67)) then
      exit;
    info := NDigits(bars, 20, High(bars));
    if (info = '') then
      info := CChars(bars, 20, High(bars));
    if (info = '') then
    begin
      // only filler
      for var i := 20 to High(bars) do
        if (bars[i] <> BAR_TRACKER) then
          exit;
    end;
  end
  else
  begin
    // the rest is filler
    for var i := 20 to High(bars) do
      if (bars[i] <> BAR_TRACKER) then
        exit;
    if (fcc = '59') or (fcc = '62') then
      exit;
  end;
  Result := fcc + dpid + info;
end;

end.
