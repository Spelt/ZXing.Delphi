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
  ZXing.Common.Pattern,
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
  protected const
    // These two values are critical for determining how permissive the decoding will be.
    // We've arrived at these values through a lot of trial and error. Setting them any higher
    // lets false positives creep in quickly.
    MAX_AVG_VARIANCE: Integer = 122; // i.e. Trunc((1 shl INTEGER_MATH_SHIFT) * 0.48);
    MAX_INDIVIDUAL_VARIANCE: Integer = 179; // i.e. Trunc((1 shl INTEGER_MATH_SHIFT) * 0.7);

    /// <summary>
    /// Start/end guard pattern.
    /// </summary>
    START_END_PATTERN: TOneDPattern = [1, 1, 1];
    /// <summary>
    /// Pattern marking the middle of a UPC/EAN pattern, separating the two halves.
    /// </summary>
    MIDDLE_PATTERN: TOneDPattern = [1, 1, 1, 1, 1];
    /// <summary>
    /// "Odd", or "L" patterns used to encode UPC/EAN digits.
    /// </summary>

  public const
    L_PATTERNS: TOneDPatterns = [
      [3, 2, 1, 1], // 0
      [2, 2, 2, 1], // 1
      [2, 1, 2, 2], // 2
      [1, 4, 1, 1], // 3
      [1, 1, 3, 2], // 4
      [1, 2, 3, 1], // 5
      [1, 1, 1, 4], // 6
      [1, 3, 1, 2], // 7
      [1, 2, 1, 3], // 8
      [3, 1, 1, 2]  // 9
    ];
    /// <summary>
    /// "Even", or "G" patterns used to encode UPC/EAN digits.
    /// </summary>
    G_PATTERNS: TOneDPatterns = [
      [1, 1, 2, 3], // 0
      [1, 2, 2, 2], // 1
      [2, 2, 1, 2], // 2
      [1, 1, 4, 1], // 3
      [2, 3, 1, 1], // 4
      [1, 3, 2, 1], // 5
      [4, 1, 1, 1], // 6
      [2, 1, 3, 1], // 7
      [3, 1, 2, 1], // 8
      [2, 1, 1, 3]  // 9
    ];

  private
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
      const whiteFirst: Boolean; const pattern: TOneDPattern;
      counters: TArray<Integer>): TArray<Integer>; overload;

    /// <summary>
    /// Computes the UPC/EAN checksum on a string of digits, and reports
    /// whether the checksum is correct or not.
    /// </summary>
    /// <param name="s">string of digits to check</param>
    /// <returns>true iff string of digits passes the UPC/EAN checksum algorithm</returns>
    function checkStandardUPCEANChecksum(const s: String): Boolean;

    // the decoders of zxing-cpp for the symbol starting with the guard
    // begin; the text as decodeRow returns it, ending the end guard
    function DecodeDigitOf(const view: TPatternView; var txt: string;
      lgPattern: PInteger): Boolean;
    function DecodeDigits(digitCount: Integer; var next: TPatternView;
      var txt: string; lgPattern: PInteger): Boolean;
    function DecodeEAN13(const guard: TPatternView; out txt: string;
      out ending: TPatternView): Boolean;
    function DecodeEAN8(const guard: TPatternView; out txt: string;
      out ending: TPatternView): Boolean;
    function DecodeUPCE(const guard: TPatternView; out txt: string;
      out ending: TPatternView): Boolean;
    function DecodeAddOn(const guard: TPatternView; digitCount: Integer;
      out txt: string; out ending: TPatternView): Boolean;
    /// <summary>Adds the add-on, the symbology identifier and the country
    /// to r; false when the add-on is not one of the allowed ones (hint
    /// ALLOWED_EAN_EXTENSIONS).</summary>
    function CompleteResult(r: TReadResult; extensionResult: TReadResult;
      const hints: TDictionary<TDecodeHintType, TObject>): Boolean;

  public
    /// <summary>For the EAN-13 reader of the UPC-A reader: decodePattern
    /// skips EAN-13 symbols that are not a UPC-A (not starting with 0),
    /// like zxing-cpp, so that the search goes on.</summary>
    UPCAOnly: Boolean;
    /// <summary>The decoder of zxing-cpp: from every start guard with a
    /// quiet zone of 6 modules, stricter checks of the module size of
    /// EAN-8 and UPC-E. Public for the UPC-A reader, that uses the one of
    /// its EAN-13 reader.</summary>
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  protected
    function HasPatternDecoder: Boolean; override;

    /// <summary>
    /// Decodes the end.
    /// </summary>
    /// <param name="row">The row.</param>
    /// <param name="endStart">The end start.</param>
    /// <returns></returns>
    function decodeEnd(const row: IBitArray; const endStart: Integer): TArray<Integer>; virtual;

    /// <summary>
    /// </summary>
    /// <param name="s">string of digits to check</param>
    /// <returns>see <see cref="checkStandardUPCEANChecksum(String)"/></returns>
    function checkChecksum(const s: String): Boolean; virtual;
  public
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
    function DecodeMiddle(const row: IBitArray; const startRange: TArray<Integer>;
      const resultString: TStringBuilder): Integer; virtual; abstract;

    /// <summary>
    /// Get the format of this decoder.
    /// </summary>
    /// <returns>The 1D format.</returns>
    function BarcodeFormat: TBarcodeFormat; virtual; abstract;

    function findStartGuardPattern(const row: IBitArray): TArray<Integer>;

    class function findGuardPattern(const row: IBitArray;
      const rowOffset: Integer; const whiteFirst: Boolean;
      const pattern: TOneDPattern): TArray<Integer>; overload;

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
      const patterns: TOneDPatterns; var digit: Integer): Boolean;

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
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult; override;

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

  extensionReader := TUPCEANExtensionSupport.Create;
  eanManSupport := TEANManufacturerOrgSupport.Create;
end;

destructor TUPCEANReader.Destroy;
begin
  eanManSupport.Free;
  extensionReader.Free;

  inherited;
end;

function TUPCEANReader.checkChecksum(const s: String): Boolean;
begin
  Result := checkStandardUPCEANChecksum(s);
end;

function TUPCEANReader.checkStandardUPCEANChecksum
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

function TUPCEANReader.decodeEnd(const row: IBitArray;
  const endStart: Integer): TArray<Integer>;
begin
  Result := findGuardPattern(row, endStart, false, START_END_PATTERN);
end;

function TUPCEANReader.findStartGuardPattern(const row: IBitArray)
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
  const pattern: TOneDPattern): TArray<Integer>;
var
  counters: TArray<Integer>;
begin
  counters := TArray<Integer>.Create();
  SetLength(counters, Length(pattern));
  Result := findGuardPattern(row, rowOffset, whiteFirst, pattern, counters);
  counters:=nil;
end;

class function TUPCEANReader.findGuardPattern(const row: IBitArray;
  rowOffset: Integer; const whiteFirst: Boolean; const pattern: TOneDPattern;
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
  const patterns: TOneDPatterns; var digit: Integer): Boolean;
var
  bestVariance, variance, i, max: Integer;
  pattern: TOneDPattern;
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
    variance := patternMatchVariance(counters, pattern,
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

  try
    Result := DoDecodeRow(rowNumber, row, startRange, hints);
  except
    result := nil;
  end;

end;

function TUPCEANReader.DoDecodeRow(const rowNumber: Integer;
  const row: IBitArray;
  const startGuardRange: TArray<Integer>;
  const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
var
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

  res := TStringBuilder.Create(20);
  try
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
    if not CompleteResult(decodeResult, extensionResult, hints) then
    begin
      decodeResult.Free;
      exit;
    end;

    Result := decodeResult;

  finally
    res.Free;
  end;
end;

function TUPCEANReader.CompleteResult(r: TReadResult;
  extensionResult: TReadResult;
  const hints: TDictionary<TDecodeHintType, TObject>): Boolean;
var
  allowedExtensions: TArray<Integer>;
begin
  // ISO/IEC 15424: ]E4 for EAN-8, ]E0 for the others, ]E3 with add-on
  if (r.BarcodeFormat = TBarcodeFormat.EAN_8) then
    r.SymbologyIdentifier := ']E4'
  else
    r.SymbologyIdentifier := ']E0';
  var extensionLength := 0;
  if (extensionResult <> nil) then
  begin
    r.SymbologyIdentifier := ']E3';
    r.putMetadata(TResultMetadataType.UPC_EAN_EXTENSION,
      TResultMetaData.CreateStringMetadata(extensionResult.Text));
    r.putAllMetadata(extensionResult.ResultMetadata);
    r.addResultPoints(extensionResult.resultPoints);
    extensionLength := Length(extensionResult.Text);
    extensionResult.Free;
  end;

  // With ALLOWED_EAN_EXTENSIONS an extension of one of those lengths is
  // required: without it (length 0) there is no result either.
  if (hints <> nil) and
    (hints.ContainsKey(TDecodeHintType.ALLOWED_EAN_EXTENSIONS)) then
    allowedExtensions := TArray<Integer>
      (hints[TDecodeHintType.ALLOWED_EAN_EXTENSIONS])
  else
    allowedExtensions := nil;

  if (allowedExtensions <> nil) then
  begin
    var valid := false;
    for var len in allowedExtensions do
      if (extensionLength = len) then
      begin
        valid := true;
        break;
      end;
    if not valid then
      exit(false);
  end;

  case r.BarcodeFormat of
    TBarcodeFormat.EAN_13, TBarcodeFormat.UPC_A:
      begin
        var countryID := eanManSupport.lookupCountryIdentifier(r.Text);
        if (countryID <> '') then
          r.putMetadata(TResultMetadataType.POSSIBLE_COUNTRY,
            TResultMetaData.CreateStringMetadata(countryID));
      end;
  end;
  Result := true;
end;

const
  CHAR_LEN = 4;
  // the quiet zones of zxing-cpp (in modules); the GS1 specification has:
  // left 11 (EAN-13), 7 (EAN-8), 9 (UPC-A/E), 7-12 (add-on); right 7
  // (EAN-13, EAN-8, UPC-E), 9 (UPC-A), 5 (add-on)
  QUIET_ZONE_LEFT = 6;
  QUIET_ZONE_RIGHT_EAN = 3;
  QUIET_ZONE_RIGHT_UPC = 6;
  QUIET_ZONE_ADDON = 3;

  FIRST_DIGIT_ENCODINGS: array [0 .. 9] of Integer = ($00, $0B, $0D, $0E,
    $13, $19, $1C, $15, $16, $1A);
  // index div 10 is the number system, index mod 10 the check digit
  NUMSYS_AND_CHECK_DIGIT_PATTERNS: array [0 .. 19] of Integer = ($38, $34,
    $32, $31, $2C, $26, $23, $2A, $29, $25, $07, $0B, $0D, $0E, $13, $19, $1C,
    $15, $16, $1A);
  EXT_CHECK_DIGIT_ENCODINGS: array [0 .. 9] of Integer = ($18, $14, $12, $11,
    $0C, $06, $03, $0A, $09, $05);

  L_AND_G_PATTERNS: TOneDPatterns = [
    [3, 2, 1, 1], [2, 2, 2, 1], [2, 1, 2, 2], [1, 4, 1, 1], [1, 1, 3, 2],
    [1, 2, 3, 1], [1, 1, 1, 4], [1, 3, 1, 2], [1, 2, 1, 3], [3, 1, 1, 2],
    [1, 1, 2, 3], [1, 2, 2, 2], [2, 2, 1, 2], [1, 1, 4, 1], [2, 3, 1, 1],
    [1, 3, 2, 1], [4, 1, 1, 1], [2, 1, 3, 1], [3, 1, 2, 1], [2, 1, 1, 3]];

function IndexOfValue(const values: array of Integer; value: Integer): Integer;
begin
  for var i := 0 to High(values) do
    if (values[i] = value) then
      exit(i);
  Result := -1;
end;

function TUPCEANReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

function TUPCEANReader.DecodeDigitOf(const view: TPatternView; var txt: string;
  lgPattern: PInteger): Boolean;
const
  // critical for how permissive the decoding is: higher values let false
  // positives creep in quickly
  MAX_AVG_VARIANCE = 0.48;
  MAX_INDIVIDUAL_VARIANCE = 0.7;
begin
  var bestMatch: Integer;
  if (lgPattern <> nil) then
    bestMatch := DecodeDigitF(view, L_AND_G_PATTERNS, MAX_AVG_VARIANCE,
      MAX_INDIVIDUAL_VARIANCE, false)
  else
    bestMatch := DecodeDigitF(view, L_PATTERNS, MAX_AVG_VARIANCE,
      MAX_INDIVIDUAL_VARIANCE, false);
  if (bestMatch = -1) then
    exit(false);

  txt := txt + Chr(Ord('0') + bestMatch mod 10);
  if (lgPattern <> nil) then
    lgPattern^ := (lgPattern^ shl 1) or Ord(bestMatch >= 10);
  Result := true;
end;

function TUPCEANReader.DecodeDigits(digitCount: Integer;
  var next: TPatternView; var txt: string; lgPattern: PInteger): Boolean;
begin
  for var j := 0 to digitCount - 1 do
  begin
    if not DecodeDigitOf(next, txt, lgPattern) then
      exit(false);
    next.SkipSymbol;
  end;
  Result := true;
end;

/// <summary>Whether the module size of digit i of the digits from start of
/// guard is about moduleSizeRef.</summary>
function PlausibleDigitModuleSize(const guard: TPatternView;
  start, i: Integer; moduleSizeRef: Double): Boolean;
begin
  var moduleSizeData: Double := guard.SubView(start + i * 4, 4).Sum / 7;
  Result := Abs(moduleSizeData / moduleSizeRef - 1) < 0.2;
end;

function TUPCEANReader.DecodeEAN13(const guard: TPatternView; out txt: string;
  out ending: TPatternView): Boolean;
begin
  Result := false;
  txt := '';
  var mid := guard.SubView(27, 5);
  ending := guard.SubView(56, 3);
  if not IsRightGuard(ending, [1, 1, 1], QUIET_ZONE_RIGHT_EAN) or
    (IsPattern(mid, [1, 1, 1, 1, 1], false) = 0) then
    exit;

  var next := guard.SubView(3, CHAR_LEN);
  var lgPattern := 0;
  if not DecodeDigits(6, next, txt, @lgPattern) then
    exit;
  next := next.SubView(5, CHAR_LEN);
  if not DecodeDigits(6, next, txt, nil) then
    exit;

  // the first digit is encoded in the L and G patterns of the first six
  var first := IndexOfValue(FIRST_DIGIT_ENCODINGS, lgPattern);
  if (first = -1) then
    exit;
  txt := Chr(Ord('0') + first) + txt;
  Result := true;
end;

function TUPCEANReader.DecodeEAN8(const guard: TPatternView; out txt: string;
  out ending: TPatternView): Boolean;
begin
  Result := false;
  txt := '';
  var mid := guard.SubView(19, 5);
  ending := guard.SubView(40, 3);
  if not IsRightGuard(ending, [1, 1, 1], QUIET_ZONE_RIGHT_EAN) or
    (IsPattern(mid, [1, 1, 1, 1, 1], false) = 0) then
    exit;

  // the module size of the guards and of the digits is about the same
  var moduleSizeGuard: Double := (guard.Sum + mid.Sum + ending.Sum) / 11;
  for var start in TArray<Integer>.Create(3, 24) do
    for var i := 0 to 3 do
      if not PlausibleDigitModuleSize(guard, start, i, moduleSizeGuard) then
        exit;

  var next := guard.SubView(3, CHAR_LEN);
  if not DecodeDigits(4, next, txt, nil) then
    exit;
  next := next.SubView(5, CHAR_LEN);
  if not DecodeDigits(4, next, txt, nil) then
    exit;
  Result := true;
end;

function TUPCEANReader.DecodeUPCE(const guard: TPatternView; out txt: string;
  out ending: TPatternView): Boolean;
begin
  Result := false;
  txt := '';
  ending := guard.SubView(27, 6);
  if not IsRightGuard(ending, [1, 1, 1, 1, 1, 1], QUIET_ZONE_RIGHT_UPC) then
    exit;

  // the module size of the guards and of the digits is about the same,
  // which brings the false positives down
  var moduleSizeGuard: Double := (guard.Sum + ending.Sum) / 9;
  for var i := 0 to 5 do
    if not PlausibleDigitModuleSize(guard, 3, i, moduleSizeGuard) then
      exit;

  var next := guard.SubView(3, CHAR_LEN);
  var lgPattern := 0;
  if not DecodeDigits(6, next, txt, @lgPattern) then
    exit;

  var i := IndexOfValue(NUMSYS_AND_CHECK_DIGIT_PATTERNS, lgPattern);
  if (i = -1) then
    exit;
  txt := Chr(Ord('0') + i div 10) + txt + Chr(Ord('0') + i mod 10);
  Result := true;
end;

/// <summary>The checksum of a 5 digit add-on.</summary>
function Ean5Checksum(const s: string): Integer;
begin
  var n := Length(s);
  var sum := 0;
  var i := n - 1;
  while (i >= 1) do
  begin
    Inc(sum, Ord(s[i]) - Ord('0'));
    Dec(i, 2);
  end;
  sum := sum * 3;
  i := n;
  while (i >= 1) do
  begin
    Inc(sum, Ord(s[i]) - Ord('0'));
    Dec(i, 2);
  end;
  sum := sum * 3;
  Result := sum mod 10;
end;

function TUPCEANReader.DecodeAddOn(const guard: TPatternView;
  digitCount: Integer; out txt: string; out ending: TPatternView): Boolean;
begin
  Result := false;
  txt := '';
  var ext := guard.SubView(0, 3 + digitCount * 4 + (digitCount - 1) * 2);
  ending := ext;
  if not ext.IsValid then
    exit;
  var moduleSize := IsPattern(ext, [1, 1, 2], false);
  if (moduleSize = 0) then
    exit;
  if not ext.IsAtLastBar and (ext[ext.Size] <= QUIET_ZONE_ADDON * moduleSize -
    1) then
    exit;

  ext := ext.SubView(3, CHAR_LEN);
  var lgPattern := 0;
  for var i := 0 to digitCount - 1 do
  begin
    if not DecodeDigitOf(ext, txt, @lgPattern) then
      exit;
    ext.SkipSymbol;
    if (i < digitCount - 1) then
    begin
      // the separator
      if (IsPattern(ext, [1, 1], false, 0, 0, moduleSize) = 0) then
        exit;
      ext.SkipPair;
    end;
  end;

  if (digitCount = 2) then
    Result := (StrToInt(txt) mod 4 = lgPattern)
  else
    Result := (Ean5Checksum(txt) = IndexOfValue(EXT_CHECK_DIGIT_ENCODINGS,
      lgPattern));
end;

function TUPCEANReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
const
  MIN_SIZE = 3 + 6 * 4 + 6; // UPC-E
begin
  Result := nil;
  next := FindLeftGuard(next, MIN_SIZE, [1, 1, 1], QUIET_ZONE_LEFT);
  if not next.IsValid then
    exit;

  var guard := next;
  var txt: string;
  var ending: TPatternView;
  var ok := false;
  case BarcodeFormat of
    TBarcodeFormat.EAN_13:
      ok := DecodeEAN13(guard, txt, ending);
    TBarcodeFormat.EAN_8:
      ok := DecodeEAN8(guard, txt, ending);
    TBarcodeFormat.UPC_E:
      ok := DecodeUPCE(guard, txt, ending);
  end;
  if not ok or not checkChecksum(txt) or (UPCAOnly and (txt[1] <> '0')) then
    exit;

  next := ending;

  // an add-on close behind the end guard
  var extensionResult: TReadResult := nil;
  var ext := ending;
  if ext.SkipSymbol and ext.SkipSingle(Trunc(guard.Sum * 3.5)) then
  begin
    var extTxt: string;
    var extEnding: TPatternView;
    if DecodeAddOn(ext, 5, extTxt, extEnding) or
      DecodeAddOn(ext, 2, extTxt, extEnding) then
    begin
      extensionResult := TReadResult.Create(extTxt, nil,
        [TResultPointHelpers.CreateResultPoint(ext.PixelsInFront +
        ext.Sum(3) div 2, rowNumber), TResultPointHelpers.CreateResultPoint
        (extEnding.PixelsTillEnd, rowNumber)], TBarcodeFormat.UPC_EAN_EXTENSION);
      var extData := extensionReader.parseExtensionString(extTxt);
      if (extData <> nil) then
      begin
        extensionResult.putAllMetadata(extData);
        extData.Free;
      end;
      next := extEnding;
    end;
  end;

  // the middle of the start and end guards, like decodeRow
  Result := TReadResult.Create(txt, nil,
    [TResultPointHelpers.CreateResultPoint(guard.PixelsInFront +
    guard.Sum div 2, rowNumber), TResultPointHelpers.CreateResultPoint
    (ending.PixelsInFront + ending.Sum div 2, rowNumber)], BarcodeFormat);
  if not CompleteResult(Result, extensionResult, hints) then
    FreeAndNil(Result);
end;

end.
