unit Code128Reader;

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

uses
  SysUtils, Generics.Collections, Math, OneDReader, BitArray, ReadResult,
  DecodeHintType, ResultPoint, BarcodeFormat, Helpers;

/// <summary>
/// <p>Decodes Code 128 barcodes.</p>
///
/// <author>Sean Owen</author>
/// </summary>

type
  TArrInt = TArray<Integer>;
  TMultiIntegerArray = TArray<TArrInt>;

var
  CODE_PATTERNS: TMultiIntegerArray;

type

  TCode128Reader = class(TOneDReader)
  private

    const

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

    class var MAX_AVG_VARIANCE: Integer;
    class var MAX_INDIVIDUAL_VARIANCE: Integer;
    class function FindStartPattern(row: TBitArray): TArray<Integer>;
    class function DecodeCode(row: TBitArray; counters: TArray<Integer>;
      rowOffset: Integer; var code: Integer): Boolean;

  public
    constructor Create();
    destructor Destroy(); override;
    function DecodeRow(rowNumber: Integer; row: TBitArray;
      hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;

  end;

implementation

constructor TCode128Reader.Create();
begin
  inherited;
  CODE_PATTERNS := [[2, 1, 2, 2, 2, 2], // 0
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

  MAX_AVG_VARIANCE := Floor(PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.25);
  MAX_INDIVIDUAL_VARIANCE := Floor(PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.7);
end;

destructor TCode128Reader.Destroy;
begin
  CODE_PATTERNS := nil;
  inherited;
end;

class function TCode128Reader.FindStartPattern(row: TBitArray): TArray<Integer>;
var
  i, bestMatch, startCode, bestVariance, width, rowOffset, patternStart,
    patternLength, variance, counterPosition: Integer;
  counters:TArray<Integer>;
  // CopyCounters:TArray<Integer>; // never used
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

class function TCode128Reader.DecodeCode(row: TBitArray;
  counters: TArray<Integer>; rowOffset: Integer; var code: Integer): Boolean;
var
  bestVariance, l, d, variance: Integer;
  pattern: TArray<Integer>;
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

function TCode128Reader.DecodeRow(rowNumber: Integer; row: TBitArray;
  hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  shiftUpperMode, upperMode, lastCharacterWasPrintable, convertFNC1, done,
    unshift, isNextShifted: Boolean;
  counters, startPatternInfo: TArray<Integer>;
  lastPatternSize, multiplier, checksumTotal, code, lastCode, nextStart,
    lastStart, startCode, codeSet, l, rawCodesSize: Integer;
  rawCodes: TList<Byte>;
  rawBytes: TArray<Byte>;
  aResult: String;
  counter, resultLength, i: Integer;
  left, right: Single;
//   resultPointCallback: TResultPointCallback; never used
//  ReadResult: TReadResult; never used
  resultPoints: TArray<IResultPoint>;
  resultPointLeft, resultPointRight: IResultPoint;

begin
  convertFNC1 := (hints <> nil) and
    (hints.ContainsKey(DecodeHintType.ASSUME_GS1));

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
                aResult := aResult + Char(32 + 128)
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
      if unshift then
      begin

        if (codeSet = CODE_CODE_A) then
        begin
          codeSet := CODE_CODE_B
        end
        else
        begin
          codeSet := CODE_CODE_A;
        end;

      end;

    end;

    lastPatternSize := nextStart - lastStart;

    // Check for ample whitespace following pattern, but, to do this we first need to remember that
    // we fudged decoding CODE_STOP since it actually has 7 bars, not 6. There is a black bar left
    // to read off. Would be slightly better to properly read. Here we just skip it:
    nextStart := row.getNextUnset(nextStart);
    if not row.isRange(nextStart, Math.Min(row.Size,
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

    // ES: Nice to have

    // if ((hints = nil) or
    // (not hints.ContainsKey(DecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
    // begin
    // resultPointCallback := nil;
    // end
    // else
    // begin
    // resultPointCallback := hints[DecodeHintType.NEED_RESULT_POINT_CALLBACK] as TResultPointCallback;
    // end;

    // if resultPointCallback <> nil then
    // begin
    // resultPointCallback(new TResultPoint(left, rowNumber));
    // resultPointCallback(new TResultPoint(right, rowNumber))
    // end;

    rawCodesSize := rawCodes.Count;
    SetLength(rawBytes, rawCodesSize);

    for i := 0 to rawCodesSize - 1 do
    begin
      rawBytes[i] := rawCodes[i]
    end;

    resultPointLeft := TResultPointHelpers.CreateResultPoint(left, rowNumber);
    resultPointRight := TResultPointHelpers.CreateResultPoint(right, rowNumber);
    resultPoints := [resultPointLeft, resultPointRight];

  finally
    FreeAndNil(rawCodes);
  end;

  result := TReadResult.Create(aResult, rawBytes, resultPoints,
    BarcodeFormat.CODE_128);

end;

end.
