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

  * Plessey (the original UK Plessey code) for ZXing.Delphi: zxing-cpp,
  * ZXing Java and ZXing.Net have no reader. The patterns and the CRC as in
  * the encoder of zint (plessey.c).
}

unit ZXing.OneD.PlesseyReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes Plessey barcodes: hexadecimal digits (0-9, A-F) of 4 bits,
  /// each a bar and a space of which one is wide (1:3), the lowest bit
  /// first, followed by an 8 bit CRC that is checked (not in the text).
  /// Only read when asked for (not in Auto).
  /// </summary>
  TPlesseyReader = class(TOneDReader)
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

/// <summary>Whether the last 8 of the bits are the Plessey CRC of the bits
/// in front of them (polynomial x^8 + x^7 + x^6 + x^5 + x^3 + 1, as zint).
/// </summary>
function IsPlesseyCRCValid(const bits: TArray<Byte>): Boolean;

implementation

const
  // the start: the bits 1, 1, 0, 1 (wide bar 1, wide space 0)
  START: array [0 .. 7] of Integer = (3, 1, 3, 1, 1, 3, 3, 1);
  // the stop (termination)
  STOP: array [0 .. 8] of Integer = (3, 3, 1, 3, 1, 1, 3, 1, 3);

function IsPlesseyCRCValid(const bits: TArray<Byte>): Boolean;
const
  POLYNOMIAL: array [0 .. 8] of Byte = (1, 1, 1, 1, 0, 1, 0, 0, 1);
begin
  var n := Length(bits) - 8;
  if (n <= 0) then
    exit(false);
  // the remainder of the data bits (followed by 8 zero bits)
  var check: TArray<Byte>;
  SetLength(check, n + 8);
  for var i := 0 to n - 1 do
    check[i] := bits[i];
  for var i := 0 to n - 1 do
    if (check[i] = 1) then
      for var j := 0 to 8 do
        check[i + j] := check[i + j] xor POLYNOMIAL[j];
  for var i := 0 to 7 do
    if (check[n + i] <> bits[n + i]) then
      exit(false);
  Result := true;
end;

{ TPlesseyReader }

function TPlesseyReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TPlesseyReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TPlesseyReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  HEX = '0123456789ABCDEF';
  // the quiet zones in modules
  QUIET_ZONE = 8;
  MIN_DIGITS = 1;
begin
  Result := nil;

  next := FindLeftGuard(next, Length(START) + 8 * MIN_DIGITS + 16 +
    Length(STOP), START, QUIET_ZONE);
  if not next.IsValid then
    exit;
  var xStart := next.PixelsInFront;
  // a bar and a space are always 4 modules wide
  var pairWidth: Double := next.Sum / 4;

  // the bits up to the stop
  var bits: TArray<Byte> := [];
  next := next.SubView(Length(START), 2);
  repeat
    if not next.IsValid(Length(STOP)) then
      exit;
    if IsRightGuard(next.SubView(0, Length(STOP)), STOP, QUIET_ZONE) then
      break;
    var bar := next[0];
    var space := next[1];
    var width := bar + space;
    if (width < 0.6 * pairWidth) or (width > 1.5 * pairWidth) or
      (bar = space) then
      exit;
    bits := bits + [Ord(bar > space)];
    pairWidth := (3 * pairWidth + width) / 4;
    if not next.SkipPair then
      exit;
  until false;

  // digits of 4 bits, then the CRC of 8
  var count := Length(bits) - 8;
  if (count < 4 * MIN_DIGITS) or (count mod 4 <> 0) or
    not IsPlesseyCRCValid(bits) then
    exit;
  var text := '';
  for var d := 0 to count div 4 - 1 do
  begin
    // the lowest bit first
    var value := bits[4 * d] + 2 * bits[4 * d + 1] + 4 * bits[4 * d + 2] +
      8 * bits[4 * d + 3];
    text := text + HEX.Chars[value];
  end;

  next := next.SubView(0, Length(STOP));
  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(text, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)],
    TBarcodeFormat.PLESSEY);
  // ISO/IEC 15424: ]P0, Plessey
  Result.SymbologyIdentifier := ']P0';
end;

end.
