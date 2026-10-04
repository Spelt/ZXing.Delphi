{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
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

  * Ported from zxing-cpp (ODDataBarExpandedBitDecoder.cpp).
}

unit ZXing.OneD.DataBarExpandedBitDecoder;

interface

/// <summary>The GS1 text (AIs without parentheses, GS (#29) between
/// variable length fields) of the bits of a DataBar Expanded symbol; ''
/// when they can not be decoded.</summary>
function DecodeExpandedBits(const bits: TArray<Boolean>): string;

implementation

uses
  System.SysUtils,
  ZXing.OneD.DataBarCommon;

type
  EDataBarFormat = class(Exception);

  /// <summary>Reads bits (highest first) from a bit array.</summary>
  TBitReader = record
    Bits: TArray<Boolean>;
    Pos: Integer;
    function Size: Integer; inline;
    function PeekBits(n: Integer): Integer;
    function ReadBits(n: Integer): Integer;
    procedure SkipBits(n: Integer);
  end;

  TGPState = (gpNumeric, gpAlpha, gpIsoIec646);

const
  GS = #29; // FNC1

function TBitReader.Size: Integer;
begin
  Result := Length(Bits) - Pos;
end;

function TBitReader.PeekBits(n: Integer): Integer;
begin
  if (n > Size) then
    raise EDataBarFormat.Create('Truncated bit stream');
  Result := 0;
  for var i := 0 to n - 1 do
    Result := (Result shl 1) or Ord(Bits[Pos + i]);
end;

function TBitReader.ReadBits(n: Integer): Integer;
begin
  Result := PeekBits(n);
  Inc(Pos, n);
end;

procedure TBitReader.SkipBits(n: Integer);
begin
  if (n > Size) then
    raise EDataBarFormat.Create('Truncated bit stream');
  Inc(Pos, n);
end;

function Digits(value, width: Integer): string;
begin
  Result := Format('%.*d', [width, value]);
end;

function DecodeGeneralPurposeBits(var bits: TBitReader): string;
const
  LUT_58_TO_62 = '*,-./';
  LUT_232_TO_252 = '!"%&''()*+,-./:;<=>?_ ';
var
  state: TGPState;
  res: string;

  procedure decode5Bits;
  begin
    var v := bits.ReadBits(5);
    if (v = 4) then
    begin
      if (state = gpAlpha) then
        state := gpIsoIec646
      else
        state := gpAlpha;
    end
    else if (v = 15) then
    begin
      // FNC1 + latch to numeric
      res := res + GS;
      state := gpNumeric;
      // some generators wrongly place a numeric latch '000' after an FNC1
      if (bits.Size >= 7) and (bits.PeekBits(7) < 8) then
        bits.SkipBits(3);
    end
    else
      res := res + Char(v + 43);
  end;

  function isPadding: Boolean;
  begin
    if (state = gpNumeric) then
      Result := (bits.Size < 4)
    else
      Result := (bits.Size < 5) and
        ((4 shr (5 - bits.Size)) = bits.PeekBits(bits.Size));
    if Result then
      bits.SkipBits(bits.Size);
  end;

begin
  state := gpNumeric;
  res := '';
  while (bits.Size >= 3) do
    case state of
      gpNumeric:
        begin
          if isPadding then
            continue;
          if (bits.Size < 7) then
          begin
            var v := bits.ReadBits(4);
            if (v > 0) then
              res := res + Char(Ord('0') + v - 1);
          end
          else if (bits.PeekBits(4) = 0) then
          begin
            bits.SkipBits(4);
            state := gpAlpha;
          end
          else
          begin
            var v := bits.ReadBits(7);
            var pairDigits: array [0 .. 1] of Integer;
            pairDigits[0] := (v - 8) div 11;
            pairDigits[1] := (v - 8) mod 11;
            for var digit in pairDigits do
              if (digit = 10) then
                res := res + GS
              else
                res := res + Char(Ord('0') + digit);
          end;
        end;
      gpAlpha:
        begin
          if isPadding then
            continue;
          if (bits.PeekBits(1) = 1) then
          begin
            var v := bits.ReadBits(6);
            if (v < 58) then
              res := res + Char(v + 33)
            else if (v < 63) then
              res := res + LUT_58_TO_62[v - 58 + 1]
            else
              raise EDataBarFormat.Create('Format');
          end
          else if (bits.PeekBits(3) = 0) then
          begin
            bits.SkipBits(3);
            state := gpNumeric;
          end
          else
            decode5Bits;
        end;
      gpIsoIec646:
        begin
          if isPadding then
            continue;
          if (bits.PeekBits(3) = 0) then
          begin
            bits.SkipBits(3);
            state := gpNumeric;
          end
          else
          begin
            var v := bits.PeekBits(5);
            if (v < 16) then
              decode5Bits
            else if (v < 29) then
            begin
              v := bits.ReadBits(7);
              if (v < 90) then
                res := res + Char(v + 1)
              else
                res := res + Char(v + 7);
            end
            else
            begin
              v := bits.ReadBits(8);
              if (v < 232) or (252 < v) then
                raise EDataBarFormat.Create('Format');
              res := res + LUT_232_TO_252[v - 232 + 1];
            end;
          end;
        end;
    end;

  // in numeric encodation there might be a trailing FNC1 to ignore
  if res.EndsWith(GS) then
    SetLength(res, Length(res) - 1);
  Result := res;
end;

function DecodeCompressedGTIN(const prefix: string;
  var bits: TBitReader): string;
begin
  Result := prefix;
  for var i := 0 to 3 do
    Result := Result + Digits(bits.ReadBits(10), 3);
  Result := Result + GTINCheckDigit(Copy(Result, 3, MaxInt));
end;

function DecodeAI01GTIN(var bits: TBitReader): string;
begin
  Result := DecodeCompressedGTIN('019', bits);
end;

function DecodeAI01AndOtherAIs(var bits: TBitReader): string;
begin
  bits.SkipBits(2); // the variable length symbol bit field
  var header := DecodeCompressedGTIN('01' + IntToStr(bits.ReadBits(4)), bits);
  var trailer := DecodeGeneralPurposeBits(bits);
  Result := header + trailer;
end;

function DecodeAnyAI(var bits: TBitReader): string;
begin
  bits.SkipBits(2); // the variable length symbol bit field
  Result := DecodeGeneralPurposeBits(bits);
end;

function DecodeAI013103(var bits: TBitReader): string;
begin
  Result := DecodeAI01GTIN(bits) + '3103' + Digits(bits.ReadBits(15), 6);
end;

function DecodeAI01320x(var bits: TBitReader): string;
begin
  Result := DecodeAI01GTIN(bits);
  var weight := bits.ReadBits(15);
  if (weight < 10000) then
    Result := Result + '3202' + Digits(weight, 6)
  else
    Result := Result + '3203' + Digits(weight - 10000, 6);
end;

function DecodeAI0139yx(var bits: TBitReader; y: Char): string;
begin
  bits.SkipBits(2); // the variable length symbol bit field
  var buffer := DecodeAI01GTIN(bits) + '39' + y + IntToStr(bits.ReadBits(2));
  if (y = '3') then
    buffer := buffer + Digits(bits.ReadBits(10), 3);
  var trailer := DecodeGeneralPurposeBits(bits);
  if (trailer = '') then
    exit('');
  Result := buffer + trailer;
end;

function DecodeAI013x0x1x(var bits: TBitReader;
  const aiPrefix, dateCode: string): string;
begin
  Result := DecodeAI01GTIN(bits) + aiPrefix;
  var weight := bits.ReadBits(20);
  Result := Result + IntToStr(weight div 100000) + Digits(weight mod 100000, 6);
  var date := bits.ReadBits(16);
  if (date <> 38400) then
  begin
    var day := date mod 32;
    date := date div 32;
    var month := date mod 12 + 1;
    date := date div 12;
    var year := date;
    Result := Result + dateCode + Digits(year, 2) + Digits(month, 2) +
      Digits(day, 2);
  end;
end;

function DecodeExpandedBits(const bits: TArray<Boolean>): string;
begin
  Result := '';
  var r: TBitReader;
  r.Bits := bits;
  r.Pos := 0;
  try
    r.ReadBits(1); // the linkage bit

    if (r.PeekBits(1) = 1) then
    begin
      r.SkipBits(1);
      exit(DecodeAI01AndOtherAIs(r));
    end;

    if (r.PeekBits(2) = 0) then
    begin
      r.SkipBits(2);
      exit(DecodeAnyAI(r));
    end;

    case r.PeekBits(4) of
      4:
        begin
          r.SkipBits(4);
          exit(DecodeAI013103(r));
        end;
      5:
        begin
          r.SkipBits(4);
          exit(DecodeAI01320x(r));
        end;
    end;

    case r.PeekBits(5) of
      12:
        begin
          r.SkipBits(5);
          exit(DecodeAI0139yx(r, '2'));
        end;
      13:
        begin
          r.SkipBits(5);
          exit(DecodeAI0139yx(r, '3'));
        end;
    end;

    case r.ReadBits(7) of
      56:
        Result := DecodeAI013x0x1x(r, '310', '11');
      57:
        Result := DecodeAI013x0x1x(r, '320', '11');
      58:
        Result := DecodeAI013x0x1x(r, '310', '13');
      59:
        Result := DecodeAI013x0x1x(r, '320', '13');
      60:
        Result := DecodeAI013x0x1x(r, '310', '15');
      61:
        Result := DecodeAI013x0x1x(r, '320', '15');
      62:
        Result := DecodeAI013x0x1x(r, '310', '17');
      63:
        Result := DecodeAI013x0x1x(r, '320', '17');
    end;
  except
    on EDataBarFormat do
      Result := '';
  end;
end;

end.
