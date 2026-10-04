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

  * Royal Mail Mailmark 4-state barcode (barcode C and L) for ZXing.Delphi:
  * the encoding of zint (mailmark.c, after "Royal Mail Mailmark barcode C/L
  * encoding and decoding instructions", 2015) reversed.
}

unit ZXing.Postal.Mailmark;

interface

/// <summary>The text of the bars of a Mailmark 4-state barcode (66 bars
/// barcode C, 78 bars barcode L; states as TPostalBars), as zint takes it:
/// format, version ID, class, supply chain ID (2 or 6 digits), item ID (8
/// digits) and destination postcode plus DPS (9 characters); errors
/// corrected (Reed-Solomon); '' when the bars are none.</summary>
function DecodeMailmark(const states: TArray<Byte>): string;

implementation

uses
  System.SysUtils,
  ZXing.Common.ReedSolomon.GenericGF,
  ZXing.Common.ReedSolomon.ReedSolomonDecoder,
  ZXing.Postal.FourStateDetector;

const
  SET_A = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  SET_L = 'ABDEFGHJLNPQRSTUWXYZ';
  // the postcode formats (types 1 to 6): A letter, L limited letter, N digit,
  // S space
  POSTCODE_FORMATS: array [1 .. 6] of string = ('ANANLLNLS', 'AANNLLNLS',
    'AANNNLLNL', 'AANANLLNL', 'ANNLLNLSS', 'ANNNLLNLS');
  // the first value of each type (after 0: the international XY11)
  POSTCODE_STARTS: array [1 .. 7] of UInt64 = (1, 5408000001, 10816000001,
    64896000001, 205504000001, 205712000001, 207792000001);
  // the data symbols of the data numbers (Table 5)
  SYMBOLS_ODD: array [0 .. 31] of Byte = ($01, $02, $04, $07, $08, $0B, $0D,
    $0E, $10, $13, $15, $16, $19, $1A, $1C, $1F, $20, $23, $25, $26, $29, $2A,
    $2C, $2F, $31, $32, $34, $37, $38, $3B, $3D, $3E);
  SYMBOLS_EVEN: array [0 .. 29] of Byte = ($03, $05, $06, $09, $0A, $0C, $0F,
    $11, $12, $14, $17, $18, $1B, $1D, $1E, $21, $22, $24, $27, $28, $2B, $2D,
    $2E, $30, $33, $35, $36, $39, $3A, $3C);
  // the extender group of each symbol
  EXTENDER_C: array [0 .. 21] of Byte = (3, 5, 7, 11, 13, 14, 16, 17, 19, 0, 1,
    2, 4, 6, 8, 9, 10, 12, 15, 18, 20, 21);
  EXTENDER_L: array [0 .. 25] of Byte = (2, 5, 7, 8, 13, 14, 15, 16, 21, 22, 23,
    0, 1, 3, 4, 6, 9, 10, 11, 12, 17, 18, 19, 20, 24, 25);

var
  // GF(32), x^5 + x^2 + 1
  Field32: TGenericGF;

/// <summary>The number of the symbol in the table; -1 when it is none.
/// </summary>
function SymbolNumber(symbol: Integer; const table: array of Byte): Integer;
begin
  for var i := 0 to High(table) do
    if (table[i] = symbol) then
      exit(i);
  Result := -1;
end;

/// <summary>The destination postcode plus DPS of its value; '' when it is
/// none.</summary>
function Postcode(value: UInt64): string;
begin
  if (value = 0) then
    exit('XY11     ');
  Result := '';
  for var t := 1 to 6 do
    if (value >= POSTCODE_STARTS[t]) and (value < POSTCODE_STARTS[t + 1]) then
    begin
      var rest := value - POSTCODE_STARTS[t];
      var format := POSTCODE_FORMATS[t];
      var chars: array [0 .. 8] of Char;
      for var i := 8 downto 0 do
        case format.Chars[i] of
          'A':
            begin
              chars[i] := SET_A.Chars[rest mod 26];
              rest := rest div 26;
            end;
          'L':
            begin
              chars[i] := SET_L.Chars[rest mod 20];
              rest := rest div 20;
            end;
          'N':
            begin
              chars[i] := Chr(Ord('0') + rest mod 10);
              rest := rest div 10;
            end;
        else
          chars[i] := ' ';
        end;
      exit(string(chars));
    end;
end;

function DecodeMailmark(const states: TArray<Byte>): string;
begin
  Result := '';
  var groups := Length(states) div 3;
  if (Length(states) <> 66) and (Length(states) <> 78) then
    exit;
  var barcodeC := (groups = 22);

  // the extender groups: each bar 2 bits of the 6, which side is which
  // alternating
  var extender: array [0 .. 25] of Integer;
  for var i := 0 to groups - 1 do
  begin
    var value := 0;
    for var j := 0 to 2 do
    begin
      var topBit := 0;
      var bottomBit := 0;
      case states[3 * i + j] of
        BAR_FULL:
          begin
            topBit := 1;
            bottomBit := 1;
          end;
        BAR_ASCENDER:
          if Odd(i) then
            bottomBit := 1
          else
            topBit := 1;
        BAR_DESCENDER:
          if Odd(i) then
            topBit := 1
          else
            bottomBit := 1;
      end;
      value := value or (topBit shl (5 - j)) or (bottomBit shl (2 - j));
    end;
    extender[i] := value;
  end;

  // the data and check numbers
  var dataStep := 10;
  var dataCount := 19;
  if barcodeC then
  begin
    dataStep := 8;
    dataCount := 16;
  end;
  var checkCount := groups - dataCount;
  var numbers: TArray<Integer>;
  SetLength(numbers, groups);
  var invalid: TArray<Boolean>;
  SetLength(invalid, groups);
  for var i := 0 to groups - 1 do
  begin
    var symbol: Integer;
    if barcodeC then
      symbol := extender[EXTENDER_C[i]]
    else
      symbol := extender[EXTENDER_L[i]];
    var number: Integer;
    if (i <= dataStep) then
      number := SymbolNumber(symbol, SYMBOLS_EVEN)
    else
      number := SymbolNumber(symbol, SYMBOLS_ODD);
    // (a symbol that is none: 0, for the error correction)
    invalid[i] := (number < 0);
    if invalid[i] then
      number := 0;
    numbers[i] := number;
  end;
  var received := Copy(numbers);
  var rs := TReedSolomonDecoder.Create(Field32);
  try
    if not rs.decode(numbers, checkCount) then
      exit;
  finally
    rs.Free;
  end;
  // the symbols that were none are errors too, also when the correction
  // left them 0 (else bars that are no symbols at all read as zeros): at most
  // as many errors as the error correction can correct
  var errors := 0;
  for var i := 0 to groups - 1 do
    if invalid[i] or (numbers[i] <> received[i]) then
      Inc(errors);
  if (errors > checkCount div 2) then
    exit;
  // the corrected data numbers of the even symbols must fit their table
  for var i := 0 to dataStep do
    if (numbers[i] >= Length(SYMBOLS_EVEN)) then
      exit;

  // the consolidated data value: base 30, then base 32
  var cdv: TPostalNumber;
  FillChar(cdv, SizeOf(cdv), 0);
  for var i := 0 to dataCount - 1 do
    if (i <= dataStep) then
      cdv.MulAdd(30, numbers[i])
    else
      cdv.MulAdd(32, numbers[i]);

  var version := cdv.DivMod(4) + 1;
  var format := cdv.DivMod(5);
  var mailClass := cdv.DivMod(15);
  var supplyChain: Cardinal;
  var supplyDigits: Integer;
  if barcodeC then
  begin
    supplyChain := cdv.DivMod(100);
    supplyDigits := 2;
  end
  else
  begin
    supplyChain := cdv.DivMod(1000000);
    supplyDigits := 6;
  end;
  var item := cdv.DivMod(100000000);
  var destination: UInt64;
  if not cdv.ToUInt64(destination) then
    exit;
  var post := Postcode(destination);
  if (post = '') then
    exit;
  Result := IntToStr(format) + IntToStr(version) +
    '0123456789ABCDE'.Chars[mailClass] + System.SysUtils.Format('%.*d',
    [supplyDigits, supplyChain]) + System.SysUtils.Format('%.8d', [item]) +
    post;
end;

initialization

Field32 := TGenericGF.Create($25, 32, 1);

finalization

Field32.Free;

end.
