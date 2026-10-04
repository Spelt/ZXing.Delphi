{
  * Copyright 2016 Nu-book Inc.
  * Copyright 2016 ZXing authors
  * Copyright 2020 Axel Waggershauser
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

  * Ported from zxing-cpp (ODDataBarExpandedReader.cpp).
}

unit ZXing.OneD.DataBarExpandedReader;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  ZXing.OneD.OneDReader,
  ZXing.OneD.DataBarCommon,
  ZXing.Common.BitArray,
  ZXing.Common.Pattern,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.BarcodeFormat;

type
  /// <summary>
  /// Decodes GS1 DataBar Expanded and Expanded Stacked (ISO/IEC 24724). The
  /// pairs of characters are collected over the rows, so that the rows of a
  /// stacked symbol are found too. The text is the GS1 data without
  /// parentheses, with GS (#29) after variable length fields, like
  /// '0190012345678908310301223315991231'; SymbologyIdentifier ]e0.
  /// </summary>
  TDataBarExpandedReader = class(TOneDReader)
  private
    // the pairs found so far by finder (the most common first)
    FAllPairs: TObjectDictionary<Integer, TList<TDataBarPair>>;
    function PairsOf(finder: Integer): TList<TDataBarPair>;
    function Insert(const row: TDataBarPairs): Boolean;
    function FindValidSequence: TDataBarPairs;
    function FindSequenceRest(const sequence: array of Integer; index: Integer;
      stack: TList<TDataBarPair>): Boolean;
    procedure RemovePairs(const pairs: TDataBarPairs);
  protected
    function decodePattern(rowNumber: Integer; var next: TPatternView;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
    function HasPatternDecoder: Boolean; override;
    procedure ResetDecodingState; override;
    function UsesDecodingState: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>There is no decoder of before: nil.</summary>
    function decodeRow(const rowNumber: Integer; const row: IBitArray;
      const hints: TDictionary<TDecodeHintType, TObject>): TReadResult;
      override;
  end;

implementation

uses
  System.Math,
  ZXing.OneD.DataBarExpandedBitDecoder;

const
  FINDER_A = 1;
  FINDER_B = 2;
  FINDER_C = 3;
  FINDER_D = 4;
  FINDER_E = 5;
  FINDER_F = 6;

  // a negative finder is laid out right to left; each finder occurs only
  // once in a symbol. The sequences of 2 to 11 pairs, 0 terminated.
  FINDER_PATTERN_SEQUENCES: array [0 .. 9, 0 .. 11] of Integer = (
    (FINDER_A, -FINDER_A, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (FINDER_A, -FINDER_B, FINDER_B, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (FINDER_A, -FINDER_C, FINDER_B, -FINDER_D, 0, 0, 0, 0, 0, 0, 0, 0),
    (FINDER_A, -FINDER_E, FINDER_B, -FINDER_D, FINDER_C, 0, 0, 0, 0, 0, 0, 0),
    (FINDER_A, -FINDER_E, FINDER_B, -FINDER_D, FINDER_D, -FINDER_F, 0, 0, 0,
    0, 0, 0),
    (FINDER_A, -FINDER_E, FINDER_B, -FINDER_D, FINDER_E, -FINDER_F, FINDER_F,
    0, 0, 0, 0, 0),
    (FINDER_A, -FINDER_A, FINDER_B, -FINDER_B, FINDER_C, -FINDER_C, FINDER_D,
    -FINDER_D, 0, 0, 0, 0),
    (FINDER_A, -FINDER_A, FINDER_B, -FINDER_B, FINDER_C, -FINDER_C, FINDER_D,
    -FINDER_E, FINDER_E, 0, 0, 0),
    (FINDER_A, -FINDER_A, FINDER_B, -FINDER_B, FINDER_C, -FINDER_C, FINDER_D,
    -FINDER_E, FINDER_F, -FINDER_F, 0, 0),
    (FINDER_A, -FINDER_A, FINDER_B, -FINDER_B, FINDER_C, -FINDER_D, FINDER_D,
    -FINDER_E, FINDER_E, -FINDER_F, FINDER_F, 0));

  VALID_HALF_PAIRS: array [0 .. 6] of Integer = (-FINDER_A, FINDER_B,
    -FINDER_D, FINDER_C, -FINDER_F, FINDER_F, FINDER_E);

  FINDER_PATTERNS: array [0 .. 5] of TFinderE2E = (
    (9, 12, 5), // {1, 8, 4, 1, 1} A
    (9, 10, 5), // {3, 6, 4, 1, 1} B
    (7, 10, 7), // {3, 4, 6, 1, 1} C
    (5, 10, 9), // {3, 2, 8, 1, 1} D
    (8, 11, 6), // {2, 6, 5, 1, 1} E
    (4, 11, 10)); // {2, 2, 9, 1, 1} F

  WEIGHTS: array [0 .. 23, 0 .. 7] of Integer = (
    (0, 0, 0, 0, 0, 0, 0, 0), // the check character itself
    (1, 3, 9, 27, 81, 32, 96, 77),
    (20, 60, 180, 118, 143, 7, 21, 63),
    (189, 145, 13, 39, 117, 140, 209, 205),
    (193, 157, 49, 147, 19, 57, 171, 91),
    (62, 186, 136, 197, 169, 85, 44, 132),
    (185, 133, 188, 142, 4, 12, 36, 108),
    (113, 128, 173, 97, 80, 29, 87, 50),
    (150, 28, 84, 41, 123, 158, 52, 156),
    (46, 138, 203, 187, 139, 206, 196, 166),
    (76, 17, 51, 153, 37, 111, 122, 155),
    (43, 129, 176, 106, 107, 110, 119, 146),
    (16, 48, 144, 10, 30, 90, 59, 177),
    (109, 116, 137, 200, 178, 112, 125, 164),
    (70, 210, 208, 202, 184, 130, 179, 115),
    (134, 191, 151, 31, 93, 68, 204, 190),
    (148, 22, 66, 198, 172, 94, 71, 2),
    (6, 18, 54, 162, 64, 192, 154, 40),
    (120, 149, 25, 75, 14, 42, 126, 167),
    (79, 26, 78, 23, 69, 207, 199, 175),
    (103, 98, 83, 38, 114, 131, 182, 124),
    (161, 61, 183, 127, 170, 88, 53, 159),
    (55, 165, 73, 8, 24, 72, 5, 15),
    (45, 135, 194, 160, 58, 174, 100, 89));

type
  TDirection = (dirRight, dirLeft);

function IsFinderPattern(a, b, c, d, e: Integer): Boolean;
begin
  Result := IsFinder(a, b, c, d, e) and (c > 3 * e);
end;

function IsCharacterPair(const v: TPatternView): Boolean;
begin
  var modSizeRef := ModSizeFinder(v);
  Result := IsCharacter(LeftCharView(v), 17, modSizeRef) and
    ((v.Size = HALF_PAIR_SIZE) or IsCharacter(RightCharView(v), 17,
    modSizeRef));
end;

function IsL2RPair(const v: TPatternView): Boolean;
begin
  Result := IsFinderPattern(v[8], v[9], v[10], v[11], v[12]) and
    IsCharacterPair(v);
end;

function IsR2LPair(const v: TPatternView): Boolean;
begin
  Result := IsFinderPattern(v[12], v[11], v[10], v[9], v[8]) and
    IsCharacterPair(v);
end;

function ReadDataCharacter(const view: TPatternView; finder: Integer;
  reversed: Boolean): TDataBarCharacter;
const
  SYMBOL_WIDEST: array [0 .. 4] of Integer = (7, 5, 4, 3, 1);
  EVEN_TOTAL_SUBSET: array [0 .. 4] of Integer = (4, 20, 52, 104, 204);
  GSUM: array [0 .. 4] of Integer = (0, 348, 1388, 2948, 3988);
begin
  var oddCounts, evnCounts: TArray4I;
  if not ReadDataCharacterRaw(view, 17, reversed, oddCounts, evnCounts) then
    exit(TDataBarCharacter.Invalid);

  var weightRow := 4 * (Abs(finder) - 1) + Ord(finder < 0) * 2 +
    Ord(reversed);
  var checksum := 0;
  for var i := 0 to 3 do
    Inc(checksum, oddCounts[i] * WEIGHTS[weightRow, 2 * i] + evnCounts[i] *
      WEIGHTS[weightRow, 1 + 2 * i]);

  // (the sum is checked in ReadDataCharacterRaw)
  var oddSum := oddCounts[0] + oddCounts[1] + oddCounts[2] + oddCounts[3];
  var group := (13 - oddSum) div 2;
  var oddWidest := SYMBOL_WIDEST[group];
  var evnWidest := 9 - oddWidest;
  var vOdd := GetValue(oddCounts, oddWidest, true);
  var vEvn := GetValue(evnCounts, evnWidest, false);
  Result := TDataBarCharacter.Create(vOdd * EVEN_TOTAL_SUBSET[group] + vEvn +
    GSUM[group], checksum);
end;

function ParseFinder(const view: TPatternView; dir: TDirection): Integer;
begin
  Result := ParseFinderPattern(view, dir = dirLeft, FINDER_PATTERNS);
end;

function ChecksumIsValid(const pairs: TDataBarPairs): Boolean; overload;
begin
  var sum := 0;
  for var p in pairs do
    Inc(sum, p.Left.Checksum + p.Right.Checksum);
  var checksum := sum mod 211 + 211 * (2 * Length(pairs) - 4 -
    Ord(not pairs[High(pairs)].Right.IsValid));
  Result := (pairs[0].Left.Value = checksum);
end;

/// <summary>The index (the length of the sequence - 2) of the only valid
/// sequence for the given first character of FINDER_A, from the checksum
/// value in it.</summary>
function SequenceIndex(const first: TDataBarCharacter): Integer;
begin
  Result := (first.Value div 211 + 4 + 1) div 2 - 2;
end;

function ChecksumIsValid(const first: TDataBarCharacter): Boolean; overload;
begin
  var i := SequenceIndex(first);
  Result := (i >= 0) and (i <= High(FINDER_PATTERN_SEQUENCES));
end;

function IsValidHalfPair(finder: Integer): Boolean;
begin
  Result := false;
  for var f in VALID_HALF_PAIRS do
    if (f = finder) then
      exit(true);
end;

function ReadPair(const view: TPatternView; dir: TDirection): TDataBarPair;
begin
  Result := TDataBarPair.Invalid;
  var finder := ParseFinder(FinderView(view), dir);
  if (finder = 0) then
    exit;
  var charL := ReadDataCharacter(LeftCharView(view), finder, false);
  if not charL.IsValid then
    exit;
  if (finder = FINDER_A) and not ChecksumIsValid(charL) then
    exit;
  var charR := TDataBarCharacter.Invalid;
  if RightCharView(view).IsValid and IsCharacter(RightCharView(view), 17,
    ModSizeFinder(view)) then
    charR := ReadDataCharacter(RightCharView(view), finder, true);
  if charR.IsValid or IsValidHalfPair(finder) then
  begin
    var xStop := FinderView(view).PixelsTillEnd;
    if charR.IsValid then
      xStop := RightCharView(view).PixelsTillEnd;
    Result := TDataBarPair.Create(charL, charR, finder, view.PixelsInFront,
      xStop);
  end;
end;

/// <summary>The pairs of a row (of a stacked symbol): the first pair is
/// left to right starting on a space, or right to left starting on a bar,
/// maybe a half pair.</summary>
function ReadRowOfPairs(var next: TPatternView; rowNumber: Integer)
  : TDataBarPairs;

  function flippedDir(const p: TDataBarPair): TDirection;
  begin
    if (p.Finder < 0) then
      Result := dirRight
    else
      Result := dirLeft;
  end;

  function isValidPair(const p: TDataBarPair; const v: TPatternView): Boolean;
  begin
    if p.Right.IsValid then
      exit(true);
    if (p.Finder < 0) then
      Result := IsGuard(v[9], v[13])
    else
      Result := IsGuard(v[11], v[13]);
  end;

begin
  Result := nil;
  var pair := TDataBarPair.Invalid;
  next := next.SubView(0, HALF_PAIR_SIZE);
  while next.Shift(1) do
  begin
    if IsL2RPair(next) then
    begin
      pair := ReadPair(next, dirRight);
      if pair.IsValid and ((pair.Finder <> FINDER_A) or
        IsGuard(next[-1], next[11])) then
        break;
    end;
    if next.Shift(1) and IsR2LPair(next) then
    begin
      pair := ReadPair(next, dirLeft);
      if pair.IsValid then
        break;
    end;
  end;

  if not pair.IsValid then
  begin
    // not a single pair: consume the rest of the row
    next := TPatternView.Empty;
    exit;
  end;

  while true do
  begin
    pair.Y := rowNumber;
    Result := Result + [pair];
    if not pair.Right.IsValid or not next.Shift(FULL_PAIR_SIZE) then
      break;
    pair := ReadPair(next, flippedDir(pair));
    if not pair.IsValid or not isValidPair(pair, next) then
      break;
  end;
end;

function BuildBitArray(const pairs: TDataBarPairs): TArray<Boolean>;
var
  bits: TArray<Boolean>;
  count: Integer;

  procedure append(value: Integer);
  begin
    for var b := 11 downto 0 do
    begin
      bits[count] := ((value shr b) and 1) = 1;
      Inc(count);
    end;
  end;

begin
  SetLength(bits, 12 * 2 * Length(pairs));
  count := 0;
  append(pairs[0].Right.Value);
  for var i := 1 to High(pairs) do
  begin
    append(pairs[i].Left.Value);
    if pairs[i].Right.IsValid then
      append(pairs[i].Right.Value);
  end;
  SetLength(bits, count);
  Result := bits;
end;

{ TDataBarExpandedReader }

constructor TDataBarExpandedReader.Create;
begin
  inherited;
  FAllPairs := TObjectDictionary<Integer, TList<TDataBarPair>>.Create
    ([doOwnsValues]);
end;

destructor TDataBarExpandedReader.Destroy;
begin
  FAllPairs.Free;
  inherited;
end;

function TDataBarExpandedReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

procedure TDataBarExpandedReader.ResetDecodingState;
begin
  FAllPairs.Clear;
end;

function TDataBarExpandedReader.UsesDecodingState: Boolean;
begin
  Result := true;
end;

function TDataBarExpandedReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TDataBarExpandedReader.PairsOf(finder: Integer)
  : TList<TDataBarPair>;
begin
  // like std::map's operator[]: the key is added when it is not there
  if not FAllPairs.TryGetValue(finder, Result) then
  begin
    Result := TList<TDataBarPair>.Create;
    FAllPairs.Add(finder, Result);
  end;
end;

function TDataBarExpandedReader.Insert(const row: TDataBarPairs): Boolean;
begin
  // the pairs of the row are added, or counted once more
  Result := false;
  for var pair in row do
  begin
    var pairs := PairsOf(pair.Finder);
    var i := 0;
    while (i < pairs.Count) and not pairs[i].SameAs(pair) do
      Inc(i);
    if (i < pairs.Count) then
    begin
      var p := pairs[i];
      Inc(p.Count);
      pairs[i] := p;
      // the pairs seen most often first, they are tried first
      while (i > 0) and (pairs[i].Count > pairs[i - 1].Count) do
      begin
        pairs.Exchange(i, i - 1);
        Dec(i);
      end;
    end
    else
      pairs.Add(pair);
    Result := true;
  end;
end;

function TDataBarExpandedReader.FindSequenceRest(const sequence
  : array of Integer; index: Integer; stack: TList<TDataBarPair>): Boolean;
const
  // only the N most common pairs are tried: at most N^11 evaluations of
  // ChecksumIsValid (11 is the maximum length of a sequence)
  N = 2;
begin
  if (index > High(sequence)) or (sequence[index] = 0) then
    exit(ChecksumIsValid(stack.ToArray));

  Result := false;
  var pairs: TList<TDataBarPair>;
  if not FAllPairs.TryGetValue(sequence[index], pairs) then
    exit;
  var isLast := (index = High(sequence)) or (sequence[index + 1] = 0);
  for var k := 0 to Min(N, pairs.Count) - 1 do
  begin
    var p := pairs[k];
    // a half pair only at the end of the sequence
    if not p.Right.IsValid and not isLast then
      continue;
    stack.Add(p);
    if FindSequenceRest(sequence, index + 1, stack) then
      exit(true);
    stack.Delete(stack.Count - 1);
  end;
end;

function TDataBarExpandedReader.FindValidSequence: TDataBarPairs;
begin
  Result := nil;
  var stack := TList<TDataBarPair>.Create;
  try
    var firsts := PairsOf(FINDER_A).ToArray;
    for var first in firsts do
    begin
      var sequenceIndex := SequenceIndex(first.Left);
      // not enough pairs seen to complete the sequence: wait for more
      if (FAllPairs.Count < sequenceIndex + 2) then
        continue;
      var sequence: TArray<Integer>;
      SetLength(sequence, 12);
      for var i := 0 to 11 do
        sequence[i] := FINDER_PATTERN_SEQUENCES[sequenceIndex, i];
      stack.Add(first);
      // fill the stack with pairs according to the finder sequence
      if FindSequenceRest(sequence, 1, stack) then
        exit(stack.ToArray);
      stack.Delete(stack.Count - 1);
    end;
  finally
    stack.Free;
  end;
end;

procedure TDataBarExpandedReader.RemovePairs(const pairs: TDataBarPairs);
begin
  for var p in pairs do
  begin
    var list := PairsOf(p.Finder);
    for var i := 0 to list.Count - 1 do
      if list[i].SameAs(p) then
      begin
        var q := list[i];
        Dec(q.Count);
        if (q.Count = 0) then
          list.Delete(i)
        else
          list[i] := q;
        break;
      end;
  end;
end;

function TDataBarExpandedReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  // Stacked symbols can be laid out in a number of ways:
  // * the first row starts with FINDER_A left to right (l2r)
  // * l2r pairs start with a space, right to left (r2l) ones with a bar
  // * l2r and r2l finders always alternate
  // * rows may contain any number of pairs
  // * even rows may be reversed
  // * a l2r pair that starts with a bar is a r2l pair on a reversed row
  // * the last pair of the symbol may miss its right character
  Result := nil;
  if not Insert(ReadRowOfPairs(next, rowNumber)) then
    exit;

  var pairs := FindValidSequence;
  if (pairs = nil) then
    exit;

  var txt := DecodeExpandedBits(BuildBitArray(pairs));
  if (txt = '') then
    exit;

  RemovePairs(pairs);

  // the symbology identifier: ISO/IEC 24724:2011 section 9 and GS1
  // General Specifications 5.1.3 figure 5.1.3-2
  var first := pairs[0];
  var last := pairs[High(pairs)];
  Result := TReadResult.Create(txt, nil, EstimatePosition(first, last),
    TBarcodeFormat.RSS_EXPANDED);
  Result.SymbologyIdentifier := ']e0';
  FLineCount := EstimateLineCount(first, last);
end;

end.
