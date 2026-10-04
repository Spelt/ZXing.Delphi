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

  * The (non-interleaved) Code 2 of 5 variants for ZXing.Delphi: Industrial,
  * IATA, Matrix and Datalogic. zxing-cpp, ZXing Java and ZXing.Net have no
  * reader. The patterns as in the encoder of zint (2of5.c).
}

unit ZXing.OneD.Code2of5Reader;

interface

uses
  System.SysUtils,
  System.Math,
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
  /// Decodes a Code 2 of 5 variant: digits of 5 elements of which 2 are
  /// wide. Industrial and IATA: in the bars, the spaces narrow; Matrix and
  /// Datalogic: in the bars and the spaces, a narrow space between the
  /// digits. The start and stop of Industrial and Matrix are their own,
  /// IATA and Datalogic share theirs. A check digit is optional (it stays
  /// in the text). Only read when asked for (not in Auto).
  /// </summary>
  TCode2of5Reader = class(TOneDReader)
  private
    FFormat: TBarcodeFormat;
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    /// <summary>format: INDUSTRIAL_2_OF_5, IATA_2_OF_5, MATRIX_2_OF_5 or
    /// DATALOGIC_2_OF_5.</summary>
    constructor Create(format: TBarcodeFormat);
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

const
  // the wide elements of the digits
  DIGITS: array [0 .. 9] of string = ('00110', '10001', '01001', '11000',
    '00101', '10100', '01100', '00011', '10010', '01010');
  // the quiet zones in narrow modules (the spec requires 10)
  QUIET_ZONE = 5;
  MIN_DIGITS = 3;
  // with the start of IATA and Datalogic (4 narrow elements, weak)
  MIN_DIGITS_IATA_START = 4;

/// <summary>The digit of the 5 widths and their narrow and wide width; -1
/// when they are none (not 2 clearly wider than the other 3).</summary>
function ClassifyDigit(const widths: array of Integer;
  out narrow, wide: Double): Integer;
begin
  Result := -1;
  var sorted: TArray<Integer>;
  SetLength(sorted, 5);
  for var i := 0 to 4 do
    sorted[i] := widths[i];
  TArray.Sort<Integer>(sorted);
  // wide about 2 to 3 times narrow
  if (sorted[3] < 1.4 * sorted[2]) or (sorted[4] > 5 * sorted[0] + 1) then
    exit;
  narrow := (sorted[0] + sorted[1] + sorted[2]) / 3;
  wide := (sorted[3] + sorted[4]) / 2;
  var pattern := '';
  for var i := 0 to 4 do
    if (widths[i] >= sorted[3]) then
      pattern := pattern + '1'
    else
      pattern := pattern + '0';
  for var d := 0 to 9 do
    if (DIGITS[d] = pattern) then
      exit(d);
end;

{ TCode2of5Reader }

constructor TCode2of5Reader.Create(format: TBarcodeFormat);
begin
  inherited Create;
  FFormat := format;
end;

function TCode2of5Reader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TCode2of5Reader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TCode2of5Reader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
  // the digits in the bars (Industrial, IATA) or in the bars and spaces
  var inBars := (FFormat = TBarcodeFormat.INDUSTRIAL_2_OF_5) or
    (FFormat = TBarcodeFormat.IATA_2_OF_5);
  var ownGuards := (FFormat = TBarcodeFormat.INDUSTRIAL_2_OF_5) or
    (FFormat = TBarcodeFormat.MATRIX_2_OF_5);
  // the start and the stop: wide 1 (the Matrix start bar 4 modules: wide),
  // IATA and Datalogic narrow only (1 1 1 1) and wide narrow narrow
  var start, stop: string;
  if not ownGuards then
  begin
    start := '0000';
    stop := '100';
  end
  else if inBars then
  begin
    start := '101000';
    stop := '10001';
  end
  else
  begin
    start := '100000';
    stop := '10000';
  end;
  var digitLength := 6;
  if inBars then
    digitLength := 10;

  // the start, with a quiet zone in front
  var startLength := Length(start);
  next := FindLeftGuard(next, startLength, startLength + MIN_DIGITS *
    digitLength + Length(stop),
    function(const window: TPatternView; spaceInPixel: Integer): Boolean
    begin
      var narrow := window[0];
      var wide := window[0];
      for var i := 1 to startLength - 1 do
      begin
        narrow := Min(narrow, window[i]);
        wide := Max(wide, window[i]);
      end;
      if (start = '0000') then
        Result := (wide <= 1.5 * narrow + 1)
      else
      begin
        // the narrow ones about the same, the wide ones 2 times wider
        var narrowMax := 0;
        var wideMin := MaxInt;
        for var i := 0 to startLength - 1 do
          if (start.Chars[i] = '1') then
            wideMin := Min(wideMin, window[i])
          else
            narrowMax := Max(narrowMax, window[i]);
        Result := (wideMin >= 1.4 * narrowMax) and
          (narrowMax <= 1.5 * narrow + 1);
      end;
      Result := Result and (spaceInPixel >= QUIET_ZONE * narrow);
    end);
  if not next.IsValid then
    exit;
  var xStart := next.PixelsInFront;
  var startNarrow := MaxInt;
  for var i := 0 to startLength - 1 do
    if (start.Chars[i] = '0') then
      startNarrow := Min(startNarrow, next[i]);
  var narrow: Double := startNarrow;
  var threshold := 2 * narrow;
  if not next.Shift(startLength) then
    exit;

  var text := '';
  repeat
    // the stop, with a quiet zone behind it (or the end of the row)
    var stopView := next.SubView(0, Length(stop));
    if (text <> '') and stopView.IsValid then
    begin
      var isStop := true;
      for var i := 0 to Length(stop) - 1 do
        if ((stopView[i] > threshold) <> (stop.Chars[i] = '1')) then
          isStop := false;
      if isStop and (stopView.IsAtLastBar or
        (next[Length(stop)] >= QUIET_ZONE * narrow)) then
      begin
        next := stopView;
        break;
      end;
    end;

    // a digit
    if not next.IsValid(digitLength) then
      exit;
    var widths: array [0 .. 4] of Integer;
    for var i := 0 to 4 do
      if inBars then
        widths[i] := next[2 * i]
      else
        widths[i] := next[i];
    var digitNarrow, digitWide: Double;
    var d := ClassifyDigit(widths, digitNarrow, digitWide);
    if (d < 0) or (digitNarrow < 0.6 * narrow) or
      (digitNarrow > 1.6 * narrow + 1) then
      exit;
    var digitThreshold := (digitNarrow + digitWide) / 2;
    // the spaces narrow
    if inBars then
    begin
      for var i := 0 to 4 do
        if (next[2 * i + 1] >= digitThreshold) then
          exit;
    end
    else if (next[5] >= digitThreshold) then
      exit;
    narrow := (narrow + digitNarrow) / 2;
    threshold := (threshold + digitThreshold) / 2;
    text := text + Chr(Ord('0') + d);
    next.Shift(digitLength);
  until false;

  if (Length(text) < MIN_DIGITS) or not ownGuards and
    (Length(text) < MIN_DIGITS_IATA_START) then
    exit;

  var xStop := next.PixelsTillEnd;
  Result := TReadResult.Create(text, nil,
    [TResultPointHelpers.CreateResultPoint(xStart, rowNumber),
    TResultPointHelpers.CreateResultPoint(xStop, rowNumber)], FFormat);
  // ISO/IEC 15424: ]S0 2 of 5 with a start of 3 bars (Industrial), ]R0
  // with one of 2 bars (IATA), ]X0 other (Matrix, Datalogic)
  if (FFormat = TBarcodeFormat.INDUSTRIAL_2_OF_5) then
    Result.SymbologyIdentifier := ']S0'
  else if (FFormat = TBarcodeFormat.IATA_2_OF_5) then
    Result.SymbologyIdentifier := ']R0'
  else
    Result.SymbologyIdentifier := ']X0';
end;

end.
