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

  * Original Authors: dswitkin@google.com (Daniel Switkin), Sean Owen and
  *                   alasdair@google.com (Alasdair Mackintosh)
  * Delphi Implementation by K. Gossens
}

unit ZXing.OneD.UPCEANReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  ZXing.OneD.OneDReader,
  ZXing.Common.BitArray,
  ZXing.OneD.UPCEANExtensionSupport,
  ZXing.OneD.EANManufacturerOrgSupport,
  ZXing.ResultMetadataType,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.ResultPoint,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// <p>Encapsulates functionality and implementation that is common to UPC and EAN families
  /// of one-dimensional barcodes.</p>
  /// </summary>
  TUPCEANReader = class abstract(TOneDReader)
  private
    decodeRowStringBuffer: TStringBuilder;
    extensionReader: TUPCEANExtensionSupport;
    eanManSupport: TEANManufacturerOrgSupport;

    /// <summary>
    /// </summary>
    /// <param name="row">row of black/white values to search</param>
    /// <param name="rowOffset">position to start search</param>
    /// <param name="whiteFirst">if true, indicates that the pattern specifies white/black/white/...</param>
    /// pixel counts, otherwise, it is interpreted as black/white/black/...
    /// <param name="pattern">pattern of counts of number of black and white pixels that are being</param>
    /// searched for as a pattern
    /// <param name="counters">array of counters, as long as pattern, to re-use</param>
    /// <returns>start/end horizontal offset of guard pattern, as an array of two ints</returns>
    class function findGuardPattern(const row: IBitArray; rowOffset: Integer;
      const whiteFirst: Boolean; const pattern: TArray<Integer>;
      counters: TArray<Integer>): TArray<Integer>; overload;

    /// <summary>
    /// Computes the UPC/EAN checksum on a string of digits, and reports
    /// whether the checksum is correct or not.
    /// </summary>
    /// <param name="s">string of digits to check</param>
    /// <returns>true iff string of digits passes the UPC/EAN checksum algorithm</returns>
    class function checkStandardUPCEANChecksum(const s: String)
      : Boolean; static;

    class procedure InitializeClass; static;
    class procedure FinalizeClass; static;
  protected
  class var
    // These two values are critical for determining how permissive the decoding will be.
    // We've arrived at these values through a lot of trial and error. Setting them any higher
    // lets false positives creep in quickly.
    MAX_AVG_VARIANCE: Integer;
    MAX_INDIVIDUAL_VARIANCE: Integer;

    /// <summary>
    /// Start/end guard pattern.
    /// </summary>
    START_END_PATTERN: TArray<Integer>;

    /// <summary>
    /// Pattern marking the middle of a UPC/EAN pattern, separating the two halves.
    /// </summary>
    MIDDLE_PATTERN: TArray<Integer>;

    /// <summary>
    /// "Odd", or "L" patterns used to encode UPC/EAN digits.
    /// </summary>
    L_PATTERNS: TArray<TArray<Integer>>;

    /// <summary>
    /// Decodes the end.
    /// </summary>
    /// <param name="row">The row.</param>
    /// <param name="endStart">The end start.</param>
    /// <returns></returns>
    class function decodeEnd(const row: IBitArray; const endStart: Integer)
      : TArray<Integer>; virtual;

    /// <summary>
    /// </summary>
    /// <param name="s">string of digits to check</param>
    /// <returns>see <see cref="checkStandardUPCEANChecksum(String)"/></returns>
    class function checkChecksum(const s: String): Boolean; virtual;
  public
    class var
    /// <summary>
    /// As above but also including the "even", or "G" patterns used to encode UPC/EAN digits.
    /// </summary>
      L_AND_G_PATTERNS: TArray<TArray<Integer>>;

    /// <summary>
    /// Initializes a new instance of the <see cref="TUPCEANReader"/> class.
    /// </summary>
    constructor Create; virtual;
    destructor Destroy; override;

    /// <summary>
    /// Subclasses override this to decode the portion of a barcode between the start
    /// and end guard patterns.
    /// </summary>
    /// <param name="row">row of black/white values to search</param>
    /// <param name="startRange">start/end offset of start guard pattern</param>
    /// <param name="resultString"><see cref="StringBuilder" />to append decoded chars to</param>
    /// <returns>horizontal offset of first pixel after the "middle" that was decoded or -1 if decoding could not complete successfully</returns>
    class function DecodeMiddle(const row: IBitArray;
      const startRange: TArray<Integer>; const resultString: TStringBuilder)
      : Integer; virtual; abstract;

    /// <summary>
    /// Get the format of this decoder.
    /// </summary>
    /// <returns>The 1D format.</returns>
    function BarcodeFormat: TBarcodeFormat; virtual; abstract;

    class function findStartGuardPattern(const row: IBitArray)
      : TArray<Integer>; static;

    class function findGuardPattern(const row: IBitArray;
      const rowOffset: Integer; const whiteFirst: Boolean;
      const pattern: TArray<Integer>): TArray<Integer>; overload;

    /// <summary>
    /// Attempts to decode a single UPC/EAN-encoded digit.
    /// </summary>
    /// <param name="row">row of black/white values to decode</param>
    /// <param name="counters">the counts of runs of observed black/white/black/... values</param>
    /// <param name="rowOffset">horizontal offset to start decoding from</param>
    /// <param name="patterns">the set of patterns to use to decode -- sometimes different encodings</param>
    /// for the digits 0-9 are used, and this indicates the encodings for 0 to 9 that should
    /// be used
    /// <returns>horizontal offset of first pixel beyond the decoded digit</returns>
    class function decodeDigit(const row: IBitArray;
      const counters: TArray<Integer>; const rowOffset: Integer;
      const patterns: TArray<TArray<Integer>>; var digit: Integer): Boolean;

    /// <summary>
    /// <p>Attempts to decode a one-dimensional barcode format given a single row of
    /// an image.</p>
    /// </summary>
    /// <param name="rowNumber">row number from top of the row</param>
    /// <param name="row">the black/white pixel data of the row</param>
    /// <param name="hints">decode hints</param>
    /// <returns>
    /// <see cref="TReadResult"/>containing encoded string and start/end of barcode or null, if an error occurs or barcode cannot be found
    /// </returns>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      overload; override;

    /// <summary>
    /// <p>Like decodeRow(int, BitArray, java.util.Map), but
    /// allows caller to inform method about where the UPC/EAN start pattern is
    /// found. This allows this to be computed once and reused across many implementations.</p>
    /// </summary>
    /// <param name="rowNumber">row index into the image</param>
    /// <param name="row">encoding of the row of the barcode image</param>
    /// <param name="startGuardRange">start/end column where the opening start pattern was found</param>
    /// <param name="hints">optional hints that influence decoding</param>
    /// <returns><see cref="TReadResult"/> encapsulating the result of decoding a barcode in the row</returns>
    function DoDecodeRow(const rowNumber: Integer; const row: IBitArray;
      const startGuardRange: TArray<Integer>;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;

  end;

implementation

{ TUPCEANReader }

constructor TUPCEANReader.Create;
begin
  inherited;

  decodeRowStringBuffer := TStringBuilder.Create(20);
  extensionReader := TUPCEANExtensionSupport.Create;
  eanManSupport := TEANManufacturerOrgSupport.Create;
end;

destructor TUPCEANReader.Destroy;
begin
  eanManSupport.Free;
  extensionReader.Free;
  decodeRowStringBuffer.Free;
  inherited;
end;

class procedure TUPCEANReader.InitializeClass;
var
  i, j: Integer;
  widths, reversedWidths: TArray<Integer>;
begin
  MAX_AVG_VARIANCE :=
    Trunc(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.48);
  MAX_INDIVIDUAL_VARIANCE :=
    Trunc(TOneDReader.PATTERN_MATCH_RESULT_SCALE_FACTOR * 0.7);

  START_END_PATTERN := TArray<Integer>.Create(1, 1, 1);
  MIDDLE_PATTERN := TArray<Integer>.Create(1, 1, 1, 1, 1);

  L_PATTERNS := TArray < TArray < Integer >>
    .Create(TArray<Integer>.Create(3, 2, 1, 1), // 0
    TArray<Integer>.Create(2, 2, 2, 1), // 1
    TArray<Integer>.Create(2, 1, 2, 2), // 2
    TArray<Integer>.Create(1, 4, 1, 1), // 3
    TArray<Integer>.Create(1, 1, 3, 2), // 4
    TArray<Integer>.Create(1, 2, 3, 1), // 5
    TArray<Integer>.Create(1, 1, 1, 4), // 6
    TArray<Integer>.Create(1, 3, 1, 2), // 7
    TArray<Integer>.Create(1, 2, 1, 3), // 8
    TArray<Integer>.Create(3, 1, 1, 2) // 9
    );

  L_AND_G_PATTERNS := L_PATTERNS;
  SetLength(L_AND_G_PATTERNS, 20);

  // Move(L_PATTERNS[0], L_AND_G_PATTERNS[0], 10);
  for i := 10 to Pred(20) do
  begin
    widths := L_PATTERNS[(i - 10)];
    reversedWidths := TArray<Integer>.Create();
    SetLength(reversedWidths, Length(widths));
    for j := 0 to Pred(Length(widths)) do
      reversedWidths[j] := widths[((Length(widths) - j) - 1)];
    L_AND_G_PATTERNS[i] := reversedWidths;
  end;
end;

class procedure TUPCEANReader.FinalizeClass;
begin
  START_END_PATTERN := nil;
  MIDDLE_PATTERN := nil;
  L_PATTERNS := nil;
  L_AND_G_PATTERNS := nil;

end;

class function TUPCEANReader.checkChecksum(const s: String): Boolean;
begin
  Result := checkStandardUPCEANChecksum(s);
end;

class function TUPCEANReader.checkStandardUPCEANChecksum
  (const s: String): Boolean;
var
  len, sum, i, digit, ZeroInt: Integer;
begin
{$ZEROBASEDSTRINGS ON}
  Result := false;

  len := Length(s);
  if (len = 0) then
    exit;

  sum := 0;
  ZeroInt := Ord('0');
  i := (len - 2);
  while ((i >= 0)) do
  begin
    digit := Ord(s[i]) - ZeroInt;
    if ((digit < 0) or (digit > 9)) then
      exit;
    Inc(sum, digit);
    Dec(i, 2);
  end;
  sum := (sum * 3);
  i := (len - 1);
  while ((i >= 0)) do
  begin
    digit := Ord(s[i]) - ZeroInt;
    if ((digit < 0) or (digit > 9)) then
      exit;
    Inc(sum, digit);
    Dec(i, 2);
  end;

{$ZEROBASEDSTRINGS OFF}
  Result := ((sum mod 10) = 0);
end;

class function TUPCEANReader.decodeEnd(const row: IBitArray;
  const endStart: Integer): TArray<Integer>;
begin
  Result := findGuardPattern(row, endStart, false, START_END_PATTERN);
end;

class function TUPCEANReader.findStartGuardPattern(const row: IBitArray)
  : TArray<Integer>;
var
  foundStart: Boolean;
  startRange, counters: TArray<Integer>;
  nextStart, start, quietStart, idx, l: Integer;
begin
  Result := nil;

  foundStart := false;
  startRange := nil;
  nextStart := 0;
  counters := TArray<Integer>.Create();
  l := Length(START_END_PATTERN);
  SetLength(counters, l);
  while (not foundStart) do
  begin
    for idx := 0 to l - 1 do
      counters[idx] := 0;
    startRange := findGuardPattern(row, nextStart, false, START_END_PATTERN,
      counters);
    if (startRange = nil) then
      exit;
    start := startRange[0];
    nextStart := startRange[1];
    // Make sure there is a quiet zone at least as big as the start pattern before the barcode.
    // If this check would run off the left edge of the image, do not accept this barcode,
    // as it is very likely to be a false positive.
    quietStart := start - (nextStart - start);
    if (quietStart >= 0) then
      foundStart := row.isRange(quietStart, start, false);
  end;

  counters:=nil;
  Result := startRange;
end;

class function TUPCEANReader.findGuardPattern(const row: IBitArray;
  const rowOffset: Integer; const whiteFirst: Boolean;
  const pattern: TArray<Integer>): TArray<Integer>;
var
  counters: TArray<Integer>;
begin
  counters := TArray<Integer>.Create();
  SetLength(counters, Length(pattern));
  Result := findGuardPattern(row, rowOffset, whiteFirst, pattern, counters);
  counters:=nil;
end;

class function TUPCEANReader.findGuardPattern(const row: IBitArray;
  rowOffset: Integer; const whiteFirst: Boolean; const pattern: TArray<Integer>;
  counters: TArray<Integer>): TArray<Integer>;
var
  patternLength, width, x: Integer;
  isWhite: Boolean;
  counterPosition, patternStart: Integer;
  curCounter: TArray<Integer>;
begin
  Result := nil;

  patternLength := Length(pattern);
  width := row.Size;
  isWhite := whiteFirst;
  if whiteFirst then
    rowOffset := row.getNextUnset(rowOffset)
  else
    rowOffset := row.getNextSet(rowOffset);

  counterPosition := 0;
  patternStart := rowOffset;
  for x := rowOffset to Pred(width) do
  begin
    if (row[x] xor isWhite) then
      Inc(counters[counterPosition])
    else
    begin
      if (counterPosition = (patternLength - 1)) then
      begin
        if (patternMatchVariance(counters, pattern, MAX_INDIVIDUAL_VARIANCE) <
          MAX_AVG_VARIANCE) then
        begin
          Result := TArray<Integer>.Create(patternStart, x);
          break;
        end;
        Inc(patternStart, (counters[0] + counters[1]));
        curCounter := TArray<Integer>.Create();
        SetLength(curCounter, Length(counters));
        TArray.Copy<Integer>(counters, curCounter, 2, 0, (patternLength - 2));
        { curCounter[patternLength - 2] := 0;
          curCounter[patternLength - 1] := 0; }
        counters := curCounter;
        Dec(counterPosition);
      end
      else
        Inc(counterPosition);
      counters[counterPosition] := 1;
      isWhite := not isWhite;
    end;
  end;
end;

class function TUPCEANReader.decodeDigit(const row: IBitArray;
  const counters: TArray<Integer>; const rowOffset: Integer;
  const patterns: TArray<TArray<Integer>>; var digit: Integer): Boolean;
var
  bestVariance, variance, i, max: Integer;
  pattern: TArray<Integer>;
begin
  Result := false;

  digit := -1;
  if (not TOneDReader.recordPattern(row, rowOffset, counters)) then
    exit;

  bestVariance := TUPCEANReader.MAX_AVG_VARIANCE; // worst variance we'll accept
  max := Length(patterns);
  for i := 0 to Pred(max) do
  begin
    pattern := patterns[i];
    variance := TOneDReader.patternMatchVariance(counters, pattern,
      TUPCEANReader.MAX_INDIVIDUAL_VARIANCE);
    if (variance < bestVariance) then
    begin
      bestVariance := variance;
      digit := i;
    end;
  end;

  Result := (digit >= 0);
end;

function TUPCEANReader.decodeRow(const rowNumber: Integer; const row: IBitArray;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  startRange: TArray<Integer>;
begin
  startRange := findStartGuardPattern(row);
  if startRange = nil then
    exit(nil);

  Result := DoDecodeRow(rowNumber, row, startRange, hints);
end;

function TUPCEANReader.DoDecodeRow(const rowNumber: Integer;
  const row: IBitArray;

  const startGuardRange: TArray<Integer>;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
  len: Integer;
  res: TStringBuilder;
  resultString: String;
  endStart, ending, quietEnd: Integer;
  endRange: TArray<Integer>;
  resultPoints: TArray<IResultPoint>;
  left, right: Single;
  resultPointCallback: TResultPointCallback;
  obj: TObject;
  resPoint: IResultPoint;
  decodeResult, extensionResult: TReadResult;
  format: TBarcodeFormat;
  allowedExtensions: TArray<Integer>;
  valid: Boolean;
  extensionLength: Integer;
  countryID: String;
begin
  Result := nil;
  resultPointCallback := nil;

  if ((hints <> nil) and
    (hints.ContainsKey(TDecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
  begin
    obj := hints[TDecodeHintType.NEED_RESULT_POINT_CALLBACK];
    if (obj is TResultPointEventObject) then
      resultPointCallback := TResultPointEventObject(obj).Event;
  end;

  res := decodeRowStringBuffer;
  res.Length := 0;
  endStart := DecodeMiddle(row, startGuardRange, res);
  if (endStart < 0) then
    exit;

  if Assigned(resultPointCallback) then
  begin
    resPoint := TResultPointHelpers.CreateResultPoint(endStart, rowNumber);
    resultPointCallback(resPoint);
  end;

  endRange := decodeEnd(row, endStart);
  if (endRange = nil) then
    exit;

  if Assigned(resultPointCallback) then
  begin
    resPoint := TResultPointHelpers.CreateResultPoint
      ((endRange[0] + endRange[1]) div 2, rowNumber);
    resultPointCallback(resPoint);
  end;

  ending := endRange[1];
  quietEnd := (ending + (ending - endRange[0]));
  if ((quietEnd >= row.Size) or not row.isRange(ending, quietEnd, false)) then
    exit;

  resultString := res.ToString;
  if (Length(resultString) < 8) then
    exit;
  if (not checkChecksum(resultString)) then
    exit;
  left := ((startGuardRange[1] + startGuardRange[0]) div 2);
  right := ((endRange[1] + endRange[0]) div 2);
  format := BarcodeFormat;
  resultPoints := TArray<IResultPoint>.Create
    (TResultPointHelpers.CreateResultPoint(left, rowNumber),
    TResultPointHelpers.CreateResultPoint(right, rowNumber));

  decodeResult := TReadResult.Create(resultString, nil, resultPoints, format);
  extensionResult := extensionReader.decodeRow(rowNumber, row, endRange[1]);
  if (extensionResult <> nil) then
  begin
    decodeResult.putMetadata(TResultMetadataType.UPC_EAN_EXTENSION,
      TResultMetaData.CreateStringMetadata(extensionResult.Text));
    decodeResult.putAllMetadata(extensionResult.ResultMetadata);
    decodeResult.addResultPoints(extensionResult.resultPoints);
    extensionLength := Length(extensionResult.Text);
    extensionResult.Free;

    if (hints <> nil) and
      (hints.ContainsKey(TDecodeHintType.ALLOWED_EAN_EXTENSIONS)) then
      allowedExtensions := TArray<Integer>
        (hints[TDecodeHintType.ALLOWED_EAN_EXTENSIONS])
    else
      allowedExtensions := nil;

    if (allowedExtensions <> nil) then
    begin
      valid := false;

      for len in allowedExtensions do
      begin
        if (extensionLength = len) then
        begin
          valid := true;
          break;
        end;
      end;
      if (not valid) then
        exit;
    end
  end;

  case format of
    TBarcodeFormat.EAN_13, TBarcodeFormat.UPC_A:
      begin
        countryID := eanManSupport.lookupCountryIdentifier(resultString);
        if (countryID <> '') then
          decodeResult.putMetadata(TResultMetadataType.POSSIBLE_COUNTRY,
            TResultMetaData.CreateStringMetadata(countryID));
      end;
  end;

  Result := decodeResult;
end;

initialization

TUPCEANReader.InitializeClass;

finalization

TUPCEANReader.FinalizeClass;

end.
