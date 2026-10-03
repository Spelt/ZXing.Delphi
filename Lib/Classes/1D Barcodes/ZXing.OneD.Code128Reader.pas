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

  * Original Authors: Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.OneD.Code128Reader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  ZXing.Helpers,
  ZXing.OneD.OneDReader,
  ZXing.Common.Pattern,
  ZXing.Common.BitArray,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// <p>Decodes Code 128 barcodes.</p>
  /// </summary>
  TCode128Reader = class(TOneDReader)
  private const
    CODE_SHIFT = 98;
    CODE_CODE_C = 99;
    CODE_CODE_B = 100;
    CODE_CODE_A = 101;
    CODE_FNC_1 = 102;
    CODE_FNC_2 = 97;
    CODE_FNC_3 = 96;
    CODE_FNC_4_A = 101;
    CODE_FNC_4_B = 100;
    CODE_START_A = 103;
    CODE_START_B = 104;
    CODE_START_C = 105;
    CODE_STOP = 106;
    CODE_PATTERNS: TOneDPatterns = [[2, 1, 2, 2, 2, 2], // 0
      [2, 2, 2, 1, 2, 2], [2, 2, 2, 2, 2, 1], [1, 2, 1, 2, 2, 3],
        [1, 2, 1, 3, 2, 2], [1, 3, 1, 2, 2, 2], // 5
      [1, 2, 2, 2, 1, 3], [1, 2, 2, 3, 1, 2], [1, 3, 2, 2, 1, 2],
        [2, 2, 1, 2, 1, 3], [2, 2, 1, 3, 1, 2], // 10
      [2, 3, 1, 2, 1, 2], [1, 1, 2, 2, 3, 2], [1, 2, 2, 1, 3, 2],
        [1, 2, 2, 2, 3, 1], [1, 1, 3, 2, 2, 2], // 15
      [1, 2, 3, 1, 2, 2], [1, 2, 3, 2, 2, 1], [2, 2, 3, 2, 1, 1],
        [2, 2, 1, 1, 3, 2], [2, 2, 1, 2, 3, 1], // 20
      [2, 1, 3, 2, 1, 2], [2, 2, 3, 1, 1, 2], [3, 1, 2, 1, 3, 1],
        [3, 1, 1, 2, 2, 2], [3, 2, 1, 1, 2, 2], // 25
      [3, 2, 1, 2, 2, 1], [3, 1, 2, 2, 1, 2], [3, 2, 2, 1, 1, 2],
        [3, 2, 2, 2, 1, 1], [2, 1, 2, 1, 2, 3], // 30
      [2, 1, 2, 3, 2, 1], [2, 3, 2, 1, 2, 1], [1, 1, 1, 3, 2, 3],
        [1, 3, 1, 1, 2, 3], [1, 3, 1, 3, 2, 1], // 35
      [1, 1, 2, 3, 1, 3], [1, 3, 2, 1, 1, 3], [1, 3, 2, 3, 1, 1],
        [2, 1, 1, 3, 1, 3], [2, 3, 1, 1, 1, 3], // 40
      [2, 3, 1, 3, 1, 1], [1, 1, 2, 1, 3, 3], [1, 1, 2, 3, 3, 1],
        [1, 3, 2, 1, 3, 1], [1, 1, 3, 1, 2, 3], // 45
      [1, 1, 3, 3, 2, 1], [1, 3, 3, 1, 2, 1], [3, 1, 3, 1, 2, 1],
        [2, 1, 1, 3, 3, 1], [2, 3, 1, 1, 3, 1], // 50
      [2, 1, 3, 1, 1, 3], [2, 1, 3, 3, 1, 1], [2, 1, 3, 1, 3, 1],
        [3, 1, 1, 1, 2, 3], [3, 1, 1, 3, 2, 1], // 55
      [3, 3, 1, 1, 2, 1], [3, 1, 2, 1, 1, 3], [3, 1, 2, 3, 1, 1],
        [3, 3, 2, 1, 1, 1], [3, 1, 4, 1, 1, 1], // 60
      [2, 2, 1, 4, 1, 1], [4, 3, 1, 1, 1, 1], [1, 1, 1, 2, 2, 4],
        [1, 1, 1, 4, 2, 2], [1, 2, 1, 1, 2, 4], // 65
      [1, 2, 1, 4, 2, 1], [1, 4, 1, 1, 2, 2], [1, 4, 1, 2, 2, 1],
        [1, 1, 2, 2, 1, 4], [1, 1, 2, 4, 1, 2], // 70
      [1, 2, 2, 1, 1, 4], [1, 2, 2, 4, 1, 1], [1, 4, 2, 1, 1, 2],
        [1, 4, 2, 2, 1, 1], [2, 4, 1, 2, 1, 1], // 75
      [2, 2, 1, 1, 1, 4], [4, 1, 3, 1, 1, 1], [2, 4, 1, 1, 1, 2],
        [1, 3, 4, 1, 1, 1], [1, 1, 1, 2, 4, 2], // 80
      [1, 2, 1, 1, 4, 2], [1, 2, 1, 2, 4, 1], [1, 1, 4, 2, 1, 2],
        [1, 2, 4, 1, 1, 2], [1, 2, 4, 2, 1, 1], // 85
      [4, 1, 1, 2, 1, 2], [4, 2, 1, 1, 1, 2], [4, 2, 1, 2, 1, 1],
        [2, 1, 2, 1, 4, 1], [2, 1, 4, 1, 2, 1], // 90
      [4, 1, 2, 1, 2, 1], [1, 1, 1, 1, 4, 3], [1, 1, 1, 3, 4, 1],
        [1, 3, 1, 1, 4, 1], [1, 1, 4, 1, 1, 3], // 95
      [1, 1, 4, 3, 1, 1], [4, 1, 1, 1, 1, 3], [4, 1, 1, 3, 1, 1],
        [1, 1, 3, 1, 4, 1], [1, 1, 4, 1, 3, 1], // 100
      [3, 1, 1, 1, 4, 1], [4, 1, 1, 1, 3, 1], [2, 1, 1, 4, 1, 2],
        [2, 1, 1, 2, 1, 4], [2, 1, 1, 2, 3, 2], // 105
      [2, 3, 3, 1, 1, 1, 2]];

    MAX_AVG_VARIANCE: Integer = 64; // = Floor((1 shl INTEGER_MATH_SHIFT) * 0.25);
    MAX_INDIVIDUAL_VARIANCE: Integer = 179; // = Floor((1 shl INTEGER_MATH_SHIFT) * 0.7);

    function FindStartPattern(row: IBitArray): TArray<Integer>;
    function DecodeCode(row: IBitArray; counters: TArray<Integer>;
      rowOffset: Integer; var code: Integer): Boolean;

    /// <summary>The code of the 6 bars and spaces of view: the reference
    /// algorithm of the specification (edge to edge widths), else (not for
    /// the start code) the variance of before; -1 when none.</summary>
    class function DecodeCodeOf(const view: TPatternView;
      start: Boolean): Integer; static;
    /// <summary>The text of the codes between the start code and the
    /// checksum, as decodeRow makes it.</summary>
    class function CodesToText(const codes: TArray<Integer>;
      convertFNC1: Boolean; out fnc1First: Boolean): string; static;
  protected
    /// <summary>The decoder of zxing-cpp: the start pattern from its 2-1-1
    /// prefix with a quiet zone of 5 modules, the codes with the reference
    /// algorithm, the stop pattern with its termination bar.</summary>
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
  public
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

const
  CHAR_LEN = 6;

var
  // the codes with 6 elements (the stop code without its termination bar)
  // and their edge to edge widths
  CodePatterns6: TOneDPatterns;
  CodeE2E: TArray<TArray<Integer>>;

procedure InitPatterns(const patterns: TOneDPatterns);
begin
  SetLength(CodePatterns6, Length(patterns));
  SetLength(CodeE2E, Length(patterns));
  for var k := 0 to High(patterns) do
  begin
    CodePatterns6[k] := Copy(patterns[k], 0, CHAR_LEN);
    SetLength(CodeE2E[k], CHAR_LEN - 2);
    for var i := 0 to CHAR_LEN - 3 do
      CodeE2E[k][i] := patterns[k][i] + patterns[k][i + 1];
  end;
end;

class function TCode128Reader.DecodeCodeOf(const view: TPatternView;
  start: Boolean): Integer;
const
  CHAR_MODS = 11;
  MAX_AVG_VARIANCE_F = 0.25;
  MAX_INDIVIDUAL_VARIANCE_F = 0.7;
begin
  if (CodeE2E = nil) then
    InitPatterns(CODE_PATTERNS);

  // the edge to edge widths in modules
  var moduleSize: Double := view.Sum(CHAR_LEN) / CHAR_MODS;
  Result := -1;
  if (moduleSize > 0) then
  begin
    var e2e: array [0 .. CHAR_LEN - 3] of Integer;
    for var i := 0 to CHAR_LEN - 3 do
      e2e[i] := Trunc((view[i] + view[i + 1]) / moduleSize + 0.5);
    for var k := 0 to High(CodeE2E) do
      if (CodeE2E[k][0] = e2e[0]) and (CodeE2E[k][1] = e2e[1]) and
        (CodeE2E[k][2] = e2e[2]) and (CodeE2E[k][3] = e2e[3]) then
        exit(k);
  end;
  // the reference algorithm fails: the one of before (needed for a few
  // samples)
  if not start then
    Result := DecodeDigitF(view, CodePatterns6, MAX_AVG_VARIANCE_F,
      MAX_INDIVIDUAL_VARIANCE_F);
end;

class function TCode128Reader.CodesToText(const codes: TArray<Integer>;
  convertFNC1: Boolean; out fnc1First: Boolean): string;
var
  codeSet: Integer;
  upperMode, shiftUpperMode, isNextShifted, unshift: Boolean;

  procedure fnc1;
  begin
    // FNC1 as first character: GS1-128 (symbology ]C1)
    if (Length(Result) = 0) then
      fnc1First := true;
    if convertFNC1 then
      if (Length(Result) = 0) then
        // GS1 specification 5.4.3.7. and 5.4.6.4. If the first char after
        // the start code is FNC1 then this is GS1-128. We add the symbology
        // identifier.
        Result := Result + ']C1'
      else
        // GS1 specification 5.4.7.5. Every subsequent FNC1 is returned as
        // ASCII 29 (GS)
        Result := Result + Char(29);
  end;

  procedure fnc4;
  begin
    if not upperMode and shiftUpperMode then
    begin
      upperMode := true;
      shiftUpperMode := false;
    end
    else if upperMode and shiftUpperMode then
    begin
      upperMode := false;
      shiftUpperMode := false;
    end
    else
      shiftUpperMode := true;
  end;

begin
  // the same as decodeRow
  Result := '';
  fnc1First := false;
  case codes[0] of
    CODE_START_A:
      codeSet := CODE_CODE_A;
    CODE_START_B:
      codeSet := CODE_CODE_B;
  else
    codeSet := CODE_CODE_C;
  end;
  upperMode := false;
  shiftUpperMode := false;
  isNextShifted := false;

  for var n := 1 to High(codes) do
  begin
    var code := codes[n];
    unshift := isNextShifted;
    isNextShifted := false;
    case codeSet of
      CODE_CODE_A:
        if (code < 64) then
        begin
          if (shiftUpperMode = upperMode) then
            Result := Result + Char(32 + code)
          else
            Result := Result + Char(32 + code + 128);
          shiftUpperMode := false;
        end
        else if (code < 96) then
        begin
          if (shiftUpperMode = upperMode) then
            Result := Result + Char(code - 64)
          else
            Result := Result + Char(code + 64);
          shiftUpperMode := false;
        end
        else
          case code of
            CODE_FNC_1:
              fnc1;
            CODE_FNC_4_A:
              fnc4;
            CODE_SHIFT:
              begin
                isNextShifted := true;
                codeSet := CODE_CODE_B;
              end;
            CODE_CODE_B:
              codeSet := CODE_CODE_B;
            CODE_CODE_C:
              codeSet := CODE_CODE_C;
          end;
      CODE_CODE_B:
        if (code < 96) then
        begin
          if (shiftUpperMode = upperMode) then
            Result := Result + Char(32 + code)
          else
            Result := Result + Char(32 + code + 128);
          shiftUpperMode := false;
        end
        else
          case code of
            CODE_FNC_1:
              fnc1;
            CODE_FNC_4_B:
              fnc4;
            CODE_SHIFT:
              begin
                isNextShifted := true;
                codeSet := CODE_CODE_A;
              end;
            CODE_CODE_A:
              codeSet := CODE_CODE_A;
            CODE_CODE_C:
              codeSet := CODE_CODE_C;
          end;
      CODE_CODE_C:
        if (code < 100) then
        begin
          if (code < 10) then
            Result := Result + '0';
          Result := Result + IntToStr(code);
        end
        else
          case code of
            CODE_FNC_1:
              fnc1;
            CODE_CODE_A:
              codeSet := CODE_CODE_A;
            CODE_CODE_B:
              codeSet := CODE_CODE_B;
          end;
    end;

    // Unshift back to another code set if we were shifted
    if unshift then
      if (codeSet = CODE_CODE_A) then
        codeSet := CODE_CODE_B
      else
        codeSet := CODE_CODE_A;
  end;
end;

function TCode128Reader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TCode128Reader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  MIN_CHAR_COUNT = 4; // start + payload + checksum + stop
  // the specification has 10, real world examples ignore that
  QUIET_ZONE = 5;
begin
  Result := nil;
  next := FindLeftGuard(next, MIN_CHAR_COUNT * CHAR_LEN, [2, 1, 1],
    QUIET_ZONE);
  if not next.IsValid then
    exit;

  next := next.SubView(0, CHAR_LEN);
  var startCode := DecodeCodeOf(next, true);
  if (startCode < CODE_START_A) or (startCode > CODE_START_C) then
    exit;
  var startView := next;

  var codes: TArray<Integer> := [startCode];
  while true do
  begin
    if not next.SkipSymbol then
      exit;
    var code := DecodeCodeOf(next, false);
    if (code = -1) or (code > CODE_STOP) then
      exit;
    if (code = CODE_STOP) then
      break;
    if (code >= CODE_START_A) then
      exit;
    codes := codes + [code];
  end;
  // (the stop code is not in codes)
  if (Length(codes) < MIN_CHAR_COUNT - 1) then
    exit;

  // the termination bar (not wider than about 2 modules) and the quiet zone
  // (the view is 13 modules wide)
  var stopView := next;
  next := next.SubView(0, CHAR_LEN + 1);
  if not next.IsValid or (next[CHAR_LEN] > next.Sum(CHAR_LEN) div 4) or
    not next.HasQuietZoneAfter(QUIET_ZONE / 13) then
    exit;

  // the last code is the checksum
  var checksum := codes[0];
  for var i := 1 to High(codes) - 1 do
    Inc(checksum, i * codes[i]);
  if (checksum mod 103 <> codes[High(codes)]) then
    exit;

  var fnc1First: Boolean;
  var text := CodesToText(Copy(codes, 0, High(codes)),
    (hints <> nil) and hints.ContainsKey(ZXing.DecodeHintType.ASSUME_GS1),
    fnc1First);
  // false positive
  if (text = '') then
    exit;

  var rawBytes: TArray<Byte>;
  SetLength(rawBytes, Length(codes) + 1);
  for var i := 0 to High(codes) do
    rawBytes[i] := Byte(codes[i]);
  rawBytes[High(rawBytes)] := CODE_STOP;

  // the middle of the start and stop codes, like decodeRow
  Result := TReadResult.Create(text, rawBytes,
    [TResultPointHelpers.CreateResultPoint(startView.PixelsInFront +
    startView.Sum / 2, rowNumber), TResultPointHelpers.CreateResultPoint
    (stopView.PixelsInFront + stopView.Sum / 2, rowNumber)],
    TBarcodeFormat.CODE_128);
  if fnc1First then
    Result.SymbologyIdentifier := ']C1'
  else
    Result.SymbologyIdentifier := ']C0';
end;

function TCode128Reader.FindStartPattern(row: IBitArray): TArray<Integer>;
var
  i, bestMatch, startCode, bestVariance, width, rowOffset, patternStart,
    patternLength, variance, counterPosition: Integer;
  counters: TArray<Integer>;
  isWhite: Boolean;

begin
  width := row.Size;
  rowOffset := row.getNextSet(0);
  counterPosition := 0;
  counters := TArray<Integer>.Create();
  SetLength(counters, 6);
  patternStart := rowOffset;
  isWhite := false;
  patternLength := Length(counters);

  for i := rowOffset to width - 1 do
  begin

    if row[i] xor isWhite then
    begin
      inc(counters[counterPosition])
    end
    else
    begin
      if counterPosition = patternLength - 1 then
      begin
        bestVariance := MAX_AVG_VARIANCE;
        bestMatch := -1;

        startCode := CODE_START_A;
        while startCode <= CODE_START_C do
        begin

          variance := patternMatchVariance(counters, CODE_PATTERNS[startCode],
            MAX_INDIVIDUAL_VARIANCE);

          if variance < bestVariance then
          begin
            bestVariance := variance;
            bestMatch := startCode
          end;

          inc(startCode);
        end;

        if bestMatch >= 0 then
        begin
          // Look for whitespace before start pattern, >= 50% of width of start pattern
          if row.isRange(Max(0, patternStart - (i - patternStart) div 2),
            patternStart, false) then
          begin
            result := [patternStart, i, bestMatch];
            exit;
          end
        end;

        patternStart := patternStart + counters[0] + counters[1];

        // Array.Copy(counters, 2, counters, 0, patternLength - 2);
        // source, source index, dest, dest index, lengte

        counters := TArray.CopyInSameArray(counters, 2, patternLength - 2);
        counters[patternLength - 2] := 0;
        counters[patternLength - 1] := 0;

        dec(counterPosition);
      end
      else
      begin
        inc(counterPosition);
      end;

      counters[counterPosition] := 1;
      isWhite := not isWhite
    end;

  end;

  result := nil;

end;

function TCode128Reader.DecodeCode(row: IBitArray;
  counters: TArray<Integer>; rowOffset: Integer; var code: Integer): Boolean;
var
  bestVariance, l, d, variance: Integer;
  pattern: TOneDPattern;
begin

  code := -1;
  if not recordPattern(row, rowOffset, counters) then
  begin
    result := false;
    exit;
  end;

  bestVariance := MAX_AVG_VARIANCE;

  // worst variance we'll accept
  l := Length(CODE_PATTERNS) - 1;
  for d := 0 to l do
  begin
    pattern := CODE_PATTERNS[d];
    variance := patternMatchVariance(counters, pattern,
      MAX_INDIVIDUAL_VARIANCE);
    if variance < bestVariance then
    begin
      bestVariance := variance;
      code := d
    end;
  end;

  // TODO We're overlooking the fact that the STOP pattern has 7 values, not 6.
  result := code >= 0;
end;

function TCode128Reader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
var
  shiftUpperMode, upperMode, lastCharacterWasPrintable, convertFNC1, done,
    unshift, isNextShifted, fnc1First: Boolean;
  counters, startPatternInfo: TArray<Integer>;
  lastPatternSize, multiplier, checksumTotal, code, lastCode, nextStart,
    lastStart, startCode, codeSet, l, rawCodesSize: Integer;
  rawCodes: TList<Byte>;
  rawBytes: TArray<Byte>;
  aResult: String;
  counter, resultLength, i: Integer;
  left, right: Single;
  resultPointCallback: TResultPointCallback;
  obj: TObject;
  resultPoints: TArray<IResultPoint>;
  resultPointLeft, resultPointRight: IResultPoint;
begin
  fnc1First := false;
  convertFNC1 := (hints <> nil) and
    (hints.ContainsKey(ZXing.DecodeHintType.ASSUME_GS1));

  startPatternInfo := FindStartPattern(row);
  if (startPatternInfo = nil) then
  begin
    result := nil;
    exit;
  end;

  startCode := startPatternInfo[2];

  rawCodes := TList<Byte>.Create();

  try

    rawCodes.Add(Byte(startCode));

    case startCode of
      CODE_START_A:
        begin
          codeSet := CODE_CODE_A;
        end;
      CODE_START_B:
        begin
          codeSet := CODE_CODE_B;
        end;
      CODE_START_C:
        begin
          codeSet := CODE_CODE_C;
        end
    else
      result := nil;
      exit;
    end;

    done := false;
    isNextShifted := false;
    lastStart := startPatternInfo[0];
    nextStart := startPatternInfo[1];
    SetLength(counters, 6);
    lastCode := 0;
    code := 0;
    checksumTotal := startCode;
    multiplier := 0;
    lastCharacterWasPrintable := true;
    upperMode := false;
    shiftUpperMode := false;

    while not done do
    begin

      unshift := isNextShifted;
      isNextShifted := false;

      // Save off last code
      lastCode := code;

      // Decode another code from image
      if not DecodeCode(row, counters, nextStart, code) then
      begin
        result := nil;
        exit;
      end;

      rawCodes.Add(Byte(code));

      // Remember whether the last code was printable or not (excluding CODE_STOP)
      if code <> CODE_STOP then
      begin
        lastCharacterWasPrintable := true
      end;

      // Add to checksum computation (if not CODE_STOP of course)
      if code <> CODE_STOP then
      begin
        inc(multiplier);
        checksumTotal := checksumTotal + (multiplier * code);
      end;

      // Advance to where the next code will to start
      lastStart := nextStart;
      l := Length(counters) - 1;
      for counter := 0 to l do
      begin
        nextStart := nextStart + counters[counter];
      end;

      // Take care of illegal start codes
      case code of
        CODE_START_A, CODE_START_B, CODE_START_C:
          begin
            result := nil;
            exit;
          end;
      end;

      case codeSet of

        CODE_CODE_A:
          begin
            if code < 64 then
            begin
              if shiftUpperMode = upperMode then
              begin
                aResult := aResult + Char(32 + code);
              end
              else
              begin
                aResult := aResult + Char(32 + code + 128)
              end;
              shiftUpperMode := false
            end
            else if code < 96 then
            begin
              if shiftUpperMode = upperMode then
              begin
                aResult := aResult + Char(code - 64)
              end
              else
              begin
                aResult := aResult + Char(code + 64)
              end;
              shiftUpperMode := false
            end
            else
            begin
              // Don't let CODE_STOP, which always appears, affect whether whether we think the last
              // code was printable or not.
              if code <> CODE_STOP then
              begin
                lastCharacterWasPrintable := false
              end;
              case code of
                CODE_FNC_1:
                  begin
                    // FNC1 as first character: GS1-128 (symbology ]C1)
                    if (Length(aResult) = 0) then
                      fnc1First := true;
                    if convertFNC1 then
                    begin
                      if Length(aResult) = 0 then
                      begin
                        // GS1 specification 5.4.3.7. and 5.4.6.4. If the first char after the start code
                        // is FNC1 then this is GS1-128. We add the symbology identifier.
                        aResult := aResult + ']C1'
                      end
                      else
                      begin
                        // GS1 specification 5.4.7.5. Every subsequent FNC1 is returned as ASCII 29 (GS)
                        aResult := aResult + Char(29)
                      end
                    end;
                  end;
                CODE_FNC_2, CODE_FNC_3:
                  begin

                    // do nothing?
                  end;
                CODE_FNC_4_A:
                  begin
                    if (not upperMode) and (shiftUpperMode) then
                    begin
                      upperMode := true;
                      shiftUpperMode := false
                    end
                    else if (upperMode) and (shiftUpperMode) then
                    begin
                      upperMode := false;
                      shiftUpperMode := false
                    end
                    else
                    begin
                      shiftUpperMode := true
                    end;
                  end;
                CODE_SHIFT:
                  begin
                    isNextShifted := true;
                    codeSet := CODE_CODE_B;
                  end;
                CODE_CODE_B:
                  begin
                    codeSet := CODE_CODE_B;
                  end;
                CODE_CODE_C:
                  begin
                    codeSet := CODE_CODE_C;
                  end;
                CODE_STOP:
                  begin
                    done := true;
                  end;
              end
            end;
          end;
        CODE_CODE_B:
          begin
            if code < 96 then
            begin
              if shiftUpperMode = upperMode then
              begin
                aResult := aResult + Char(32 + code)
              end
              else
              begin
                aResult := aResult + Char(32 + code + 128)
              end;
              shiftUpperMode := false
            end
            else
            begin
              if code <> CODE_STOP then
              begin
                lastCharacterWasPrintable := false
              end;
              case code of
                CODE_FNC_1:
                  begin
                    // FNC1 as first character: GS1-128 (symbology ]C1)
                    if (Length(aResult) = 0) then
                      fnc1First := true;
                    if convertFNC1 then
                    begin
                      if Length(aResult) = 0 then
                      begin
                        // GS1 specification 5.4.3.7. and 5.4.6.4. If the first char after the start code
                        // is FNC1 then this is GS1-128. We add the symbology identifier.
                        aResult := aResult + (']C1')
                      end
                      else
                      begin
                        // GS1 specification 5.4.7.5. Every subsequent FNC1 is returned as ASCII 29 (GS)
                        aResult := aResult + Char(29)
                      end
                    end;
                  end;
                CODE_FNC_2, CODE_FNC_3:
                  begin
                    // do nothing?
                  end;
                CODE_FNC_4_B:
                  begin
                    if (not upperMode) and (shiftUpperMode) then
                    begin
                      upperMode := true;
                      shiftUpperMode := false
                    end
                    else if (upperMode) and (shiftUpperMode) then
                    begin
                      upperMode := false;
                      shiftUpperMode := false
                    end
                    else
                    begin
                      shiftUpperMode := true
                    end;
                  end;
                CODE_SHIFT:
                  begin
                    isNextShifted := true;
                    codeSet := CODE_CODE_A;
                  end;
                CODE_CODE_A:
                  begin
                    codeSet := CODE_CODE_A;
                  end;
                CODE_CODE_C:
                  begin
                    codeSet := CODE_CODE_C;
                  end;
                CODE_STOP:
                  begin
                    done := true;
                  end;
              end
            end;
          end;
        CODE_CODE_C:
          begin
            if code < 100 then
            begin
              if code < 10 then
              begin
                aResult := aResult + ('0')
              end;
              aResult := aResult + IntToStr(code);
            end
            else
            begin
              if code <> CODE_STOP then
              begin
                lastCharacterWasPrintable := false
              end;
              case code of
                CODE_FNC_1:
                  begin
                    // FNC1 as first character: GS1-128 (symbology ]C1)
                    if (Length(aResult) = 0) then
                      fnc1First := true;
                    if convertFNC1 then
                    begin
                      if Length(aResult) = 0 then
                      begin
                        // GS1 specification 5.4.3.7. and 5.4.6.4. If the first char after the start code
                        // is FNC1 then this is GS1-128. We add the symbology identifier.
                        aResult := aResult + (']C1')
                      end
                      else
                      begin
                        // GS1 specification 5.4.7.5. Every subsequent FNC1 is returned as ASCII 29 (GS)
                        aResult := aResult + Char(29)
                      end
                    end;
                  end;
                CODE_CODE_A:
                  begin
                    codeSet := CODE_CODE_A;
                  end;
                CODE_CODE_B:
                  begin
                    codeSet := CODE_CODE_B;
                  end;
                CODE_STOP:
                  begin
                    done := true;
                  end;
              end
            end;
          end;
      end;

      // Unshift back to another code set if we were shifted
      if (unshift) then
      begin
        if (codeSet = CODE_CODE_A) then
          codeSet := CODE_CODE_B
        else
          codeSet := CODE_CODE_A;
      end;
    end;

    lastPatternSize := nextStart - lastStart;

    // Check for ample whitespace following pattern, but, to do this we first need to remember that
    // we fudged decoding CODE_STOP since it actually has 7 bars, not 6. There is a black bar left
    // to read off. Would be slightly better to properly read. Here we just skip it:
    nextStart := row.getNextUnset(nextStart);
    if not row.isRange(nextStart, System.Math.Min(row.Size,
      nextStart + (nextStart - lastStart) div 2), false) then
    begin
      result := nil;
      exit;
    end;

    // Pull out from sum the value of the penultimate check code
    checksumTotal := checksumTotal - (multiplier * lastCode);
    // lastCode is the checksum then:
    if checksumTotal mod 103 <> lastCode then
    begin
      result := nil;
      exit;
    end;

    // Need to pull out the check digits from string
    resultLength := Length(aResult);
    if resultLength = 0 then
    begin
      // false positive
      result := nil;
      exit;
    end;

    // Only bother if the result had at least one character, and if the checksum digit happened to
    // be a printable character. If it was just interpreted as a control code, nothing to remove.
    if (resultLength > 0) and (lastCharacterWasPrintable) then
    begin
      if codeSet = CODE_CODE_C then
      begin
        aResult := aResult.Remove(resultLength - 2, 2);
      end
      else
      begin
        aResult := aResult.Remove(resultLength - 1, 1);
      end
    end;

    left := (startPatternInfo[1] + startPatternInfo[0]) / 2;
    right := lastStart + lastPatternSize / 2;

    rawCodesSize := rawCodes.Count;
    SetLength(rawBytes, rawCodesSize);

    for i := 0 to rawCodesSize - 1 do
      rawBytes[i] := rawCodes[i];

    resultPointLeft := TResultPointHelpers.CreateResultPoint(left, rowNumber);
    resultPointRight := TResultPointHelpers.CreateResultPoint(right, rowNumber);
    resultPoints := [resultPointLeft, resultPointRight];

    resultPointCallback := nil;
    // had to add this initialization because the following if/else construct does not assign a value
    // for all possible cases. Being resultPointCallback a local variable it is not initialized by default to null,
    // so you could get unpredictable results

    if ((hints = nil) or
      (not hints.ContainsKey(TDecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
      resultPointCallback := nil
    else
    begin
      obj := hints[TDecodeHintType.NEED_RESULT_POINT_CALLBACK];
      if (obj is TResultPointEventObject) then
        resultPointCallback := TResultPointEventObject(obj).Event;
    end;

    if Assigned(resultPointCallback) then
    begin
      resultPointCallback(resultPointLeft);
      resultPointCallback(resultPointRight);
    end;
  finally
    FreeAndNil(rawCodes);
  end;

  result := TReadResult.Create(aResult, rawBytes, resultPoints,
    TBarcodeFormat.CODE_128);
  if fnc1First then
    result.SymbologyIdentifier := ']C1'
  else
    result.SymbologyIdentifier := ']C0';
end;

end.
