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

  * Ported from zxing-cpp (ODDataBarReader.cpp).
}

unit ZXing.OneD.DataBarReader;

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
  /// Decodes GS1 DataBar (formerly RSS-14): Omnidirectional, Truncated,
  /// Stacked and Stacked Omnidirectional (ISO/IEC 24724). The two pairs of
  /// characters are collected over the rows, so that they are also found
  /// on different rows (stacked). The text is the GTIN with AI 01, like
  /// '0120358468019312'; SymbologyIdentifier ]e0.
  /// </summary>
  TDataBarReader = class(TOneDReader)
  private
    FLeftPairs, FRightPairs: TList<TDataBarPair>;
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
  System.Math;

const
  FINDER_PATTERNS: array [0 .. 8] of TFinderE2E = (
    (11, 10, 3), // {3, 8, 2, 1, 1}
    (8, 10, 6), // {3, 5, 5, 1, 1}
    (6, 10, 8), // {3, 3, 7, 1, 1}
    (4, 10, 10), // {3, 1, 9, 1, 1}
    (9, 11, 5), // {2, 7, 4, 1, 1}
    (7, 11, 7), // {2, 5, 6, 1, 1}
    (5, 11, 9), // {2, 3, 8, 1, 1}
    (6, 11, 8), // {1, 5, 7, 1, 1}
    (4, 12, 10)); // {1, 3, 9, 1, 1}

function IsCharacterPair(const v: TPatternView;
  modsLeft, modsRight: Integer): Boolean;
begin
  var modSizeRef := ModSizeFinder(v);
  Result := IsCharacter(LeftCharView(v), modsLeft, modSizeRef) and
    IsCharacter(RightCharView(v), modsRight, modSizeRef);
end;

function IsLeftPair(const v: TPatternView): Boolean;
begin
  Result := IsFinder(v[8], v[9], v[10], v[11], v[12]) and
    IsGuard(v[-1], v[11]) and IsCharacterPair(v, 16, 15);
end;

function IsRightPair(const v: TPatternView): Boolean;
begin
  Result := IsFinder(v[12], v[11], v[10], v[9], v[8]) and
    IsGuard(v[9], v[21]) and IsCharacterPair(v, 15, 16);
end;

function ChecksumPortion(const counts: TArray4I): Integer;
begin
  Result := 0;
  for var i := 3 downto 0 do
    Result := 9 * Result + counts[i];
end;

function ReadDataCharacter(const view: TPatternView;
  outsideChar, rightPair: Boolean): TDataBarCharacter;
const
  OUTSIDE_EVEN_TOTAL_SUBSET: array [0 .. 4] of Integer = (1, 10, 34, 70, 126);
  INSIDE_ODD_TOTAL_SUBSET: array [0 .. 3] of Integer = (4, 20, 48, 81);
  OUTSIDE_GSUM: array [0 .. 4] of Integer = (0, 161, 961, 2015, 2715);
  INSIDE_GSUM: array [0 .. 3] of Integer = (0, 336, 1036, 1516);
  OUTSIDE_ODD_WIDEST: array [0 .. 4] of Integer = (8, 6, 4, 3, 1);
  INSIDE_ODD_WIDEST: array [0 .. 3] of Integer = (2, 4, 6, 8);
begin
  var oddPattern, evnPattern: TArray4I;
  var modules := 15;
  if outsideChar then
    modules := 16;
  if not ReadDataCharacterRaw(view, modules, outsideChar = rightPair,
    oddPattern, evnPattern) then
    exit(TDataBarCharacter.Invalid);

  var checksum := ChecksumPortion(oddPattern) + 3 *
    ChecksumPortion(evnPattern);

  if outsideChar then
  begin
    // (the sums are checked in ReadDataCharacterRaw)
    var oddSum := oddPattern[0] + oddPattern[1] + oddPattern[2] +
      oddPattern[3];
    var group := (12 - oddSum) div 2;
    var oddWidest := OUTSIDE_ODD_WIDEST[group];
    var evnWidest := 9 - oddWidest;
    var vOdd := GetValue(oddPattern, oddWidest, false);
    var vEvn := GetValue(evnPattern, evnWidest, true);
    Result := TDataBarCharacter.Create(vOdd * OUTSIDE_EVEN_TOTAL_SUBSET[group]
      + vEvn + OUTSIDE_GSUM[group], checksum);
  end
  else
  begin
    var evnSum := evnPattern[0] + evnPattern[1] + evnPattern[2] +
      evnPattern[3];
    var group := (10 - evnSum) div 2;
    var oddWidest := INSIDE_ODD_WIDEST[group];
    var evnWidest := 9 - oddWidest;
    var vOdd := GetValue(oddPattern, oddWidest, true);
    var vEvn := GetValue(evnPattern, evnWidest, false);
    Result := TDataBarCharacter.Create(vEvn * INSIDE_ODD_TOTAL_SUBSET[group]
      + vOdd + INSIDE_GSUM[group], checksum);
  end;
end;

function ReadPair(const view: TPatternView; rightPair: Boolean): TDataBarPair;
begin
  Result := TDataBarPair.Invalid;
  var pattern := ParseFinderPattern(FinderView(view), rightPair,
    FINDER_PATTERNS);
  if (pattern = 0) then
    exit;
  var outsideView := LeftCharView(view);
  var insideView := RightCharView(view);
  if rightPair then
  begin
    outsideView := RightCharView(view);
    insideView := LeftCharView(view);
  end;
  var outside := ReadDataCharacter(outsideView, true, rightPair);
  if not outside.IsValid then
    exit;
  var inside := ReadDataCharacter(insideView, false, rightPair);
  if not inside.IsValid then
    exit;
  // including the left and right guards
  var xStart := view.PixelsInFront - view[-1];
  var xStop := view.PixelsTillEnd + 2 * view[FULL_PAIR_SIZE];
  Result := TDataBarPair.Create(outside, inside, pattern, xStart, xStop);
end;

function PairValue(const p: TDataBarPair): Int64;
begin
  Result := 1597 * Int64(p.Left.Value) + p.Right.Value;
end;

function SymbolValue(const leftPair, rightPair: TDataBarPair): Int64;
begin
  Result := 4537077 * PairValue(leftPair) + PairValue(rightPair);
  // strip the 2D linkage flag (GS1 Composite) if any (ISO/IEC 24724:2011
  // section 5.2.3)
  if (Result >= 10000000000000) then
    Dec(Result, 10000000000000);
end;

function ChecksumIsValid(const leftPair, rightPair: TDataBarPair): Boolean;

  function checksum(const p: TDataBarPair): Integer;
  begin
    Result := p.Left.Checksum + 4 * p.Right.Checksum;
  end;

begin
  var a := (checksum(leftPair) + 16 * checksum(rightPair)) mod 79;
  var b := 9 * (Abs(leftPair.Finder) - 1) + (Abs(rightPair.Finder) - 1);
  if (b > 72) then
    Dec(b);
  if (b > 8) then
    Dec(b);
  // 13 digits
  Result := (a = b) and (SymbolValue(leftPair, rightPair) <= 9999999999999);
end;

function PositionIsPlausible(const l, r: TDataBarPair): Boolean;
begin
  var wl := l.XStop - l.XStart;
  var wr := r.XStop - r.XStart;
  var h := Abs(l.Y - r.Y);
  var dxl := Abs(l.XStart - r.XStart);
  var dxr := Abs(l.XStop - r.XStart);
  // the pairs wider than the stack is high, roughly of the same width and
  // aligned (the stack not too slanted)
  Result := (h < wl) and (h < wr) and (wl > wr div 2) and (wr > wl div 2) and
    ((dxl < wl div 2) or (dxr < wr div 2));
end;

function ConstructText(const leftPair, rightPair: TDataBarPair): string;
begin
  // ISO/IEC 24724:2011 section 9
  var txt := Format('%.13d', [SymbolValue(leftPair, rightPair)]);
  Result := '01' + txt + GTINCheckDigit(txt);
end;

/// <summary>Adds pair to pairs, or counts it once more.</summary>
procedure InsertPair(pairs: TList<TDataBarPair>; const pair: TDataBarPair);
begin
  for var i := 0 to pairs.Count - 1 do
    if pairs[i].SameAs(pair) then
    begin
      var p := pairs[i];
      Inc(p.Count);
      pairs[i] := p;
      exit;
    end;
  pairs.Add(pair);
end;

{ TDataBarReader }

constructor TDataBarReader.Create;
begin
  inherited;
  FLeftPairs := TList<TDataBarPair>.Create;
  FRightPairs := TList<TDataBarPair>.Create;
end;

destructor TDataBarReader.Destroy;
begin
  FLeftPairs.Free;
  FRightPairs.Free;
  inherited;
end;

function TDataBarReader.HasPatternDecoder: Boolean;
begin
  Result := true;
end;

procedure TDataBarReader.ResetDecodingState;
begin
  FLeftPairs.Clear;
  FRightPairs.Clear;
end;

function TDataBarReader.UsesDecodingState: Boolean;
begin
  Result := true;
end;

function TDataBarReader.decodeRow(const rowNumber: Integer;
  const row: IBitArray; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
end;

function TDataBarReader.decodePattern(rowNumber: Integer;
  var next: TPatternView; const hints: TDictionary<TDecodeHintType, TObject>)
  : TReadResult;
begin
  Result := nil;
  // +1 for the guard pattern on the right (see IsRightPair); the first
  // view tested is at index 1 (a bar at 0 would be the guard pattern)
  next := next.SubView(0, FULL_PAIR_SIZE + 1);
  while next.Shift(1) do
  begin
    if IsLeftPair(next) then
    begin
      var leftPair := ReadPair(next, false);
      if leftPair.IsValid then
      begin
        leftPair.Y := rowNumber;
        InsertPair(FLeftPairs, leftPair);
        next.Shift(FULL_PAIR_SIZE - 1);
      end;
    end;

    if next.Shift(1) and IsRightPair(next) then
    begin
      var rightPair := ReadPair(next, true);
      if rightPair.IsValid then
      begin
        rightPair.Y := rowNumber;
        InsertPair(FRightPairs, rightPair);
        next.Shift(FULL_PAIR_SIZE + 2);
      end;
    end;
  end;

  for var li := 0 to FLeftPairs.Count - 1 do
    for var ri := 0 to FRightPairs.Count - 1 do
    begin
      var leftPair := FLeftPairs[li];
      var rightPair := FRightPairs[ri];
      if ChecksumIsValid(leftPair, rightPair) and
        PositionIsPlausible(leftPair, rightPair) and (leftPair.Count > 1) and
        (rightPair.Count > 1) then
      begin
        // the symbology identifier: ISO/IEC 24724:2011 section 9 and GS1
        // General Specifications 5.1.3 figure 5.1.3-2
        Result := TReadResult.Create(ConstructText(leftPair, rightPair), nil,
          EstimatePosition(leftPair, rightPair), TBarcodeFormat.RSS_14);
        Result.SymbologyIdentifier := ']e0';
        FLineCount := EstimateLineCount(leftPair, rightPair);
        FLeftPairs.Delete(li);
        FRightPairs.Delete(ri);
        exit;
      end;
    end;

  // guarantee progress (see the loop in decodePatternRow)
  next := TPatternView.Empty;
end;

end.
