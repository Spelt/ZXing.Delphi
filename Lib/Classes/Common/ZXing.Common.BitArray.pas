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

  * Original Author: Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Common.BitArray;

interface

type
  /// <summary>
  /// A simple, fast array of bits, represented compactly by an array of
  /// 32 bit words: a row of a binary image for the 1D readers. A record
  /// (nothing to free), with the words in a dynamic array: a copy of the
  /// record refers to the same bits, like a copy of the interface reference
  /// did before, and Reverse and clear work on the bits in place. An empty
  /// record (no words) is "no row", IsEmpty, where nil was used before.
  /// Bits outside the array read as false and are not written.
  /// </summary>
  TBitArray = record
  private
    FBits: TArray<Integer>;
    FSize: Integer;
    function GetBit(i: Integer): Boolean;
    procedure SetBit(i: Integer; Value: Boolean);
  public
    /// <summary>An array of Size bits, all false.</summary>
    class function Create(const Size: Integer): TBitArray; static;
    /// <summary>No bits (nil before).</summary>
    class function Empty: TBitArray; static;
    /// <summary>True when there are no bits (nil before).</summary>
    function IsEmpty: Boolean; inline;

    function Size: Integer; inline;
    function SizeInBytes: Integer;

    property Bit[i: Integer]: Boolean read GetBit write SetBit; default;
    /// <summary>The words, bit 0 of word 0 first: the same array as the
    /// record uses, so writes to it change the bits.</summary>
    property Bits: TArray<Integer> read FBits;

    /// <summary>
    /// The index of the first bit that is set, starting from the given
    /// index, or Size if none are set at or beyond this index.
    /// </summary>
    function getNextSet(from: Integer): Integer;
    /// <summary>See getNextSet: the index of the next unset bit, or Size if
    /// none are unset until the end.</summary>
    function getNextUnset(from: Integer): Integer;

    /// <summary>Sets a block of 32 bits, starting at bit i (a multiple of
    /// 32): the least significant bit of newBits is bit i, the next bit
    /// i + 1, and so on.</summary>
    procedure setBulk(i, newBits: Integer);
    /// <summary>Sets the bits from start (inclusive) to ending (exclusive).
    /// </summary>
    procedure setRange(start, ending: Integer);
    /// <summary>Reverses the order of the bits, in place.</summary>
    procedure Reverse;
    /// <summary>Clears all bits (sets to false), in place.</summary>
    procedure clear;

    /// <summary>
    /// Efficient method to check if a range of bits is set, or not set:
    /// true iff all bits from start (inclusive) to ending (exclusive) are
    /// set (value true) or not set (value false). An empty range matches;
    /// a range outside the bits does not.
    /// </summary>
    function isRange(start, ending: Integer; const value: Boolean): Boolean;
  end;

  /// <summary>The name of the interface of before, for existing code: a
  /// bit array is a record now.</summary>
  IBitArray = TBitArray;

  /// <summary>The constructors of before.</summary>
  TBitArrayHelpers = class
    /// <summary>No bits.</summary>
    class function CreateBitArray: TBitArray; overload;
    class function CreateBitArray(const Size: Integer): TBitArray; overload;
  end;

implementation

uses
  System.SysUtils,
  ZXing.Common.Detector.MathUtils;

/// <summary>The bits firstBit to lastBit (0 to 31, inclusive) of a word
/// set.</summary>
function BitMask(firstBit, lastBit: Integer): Integer; inline;
begin
  // all bits from firstBit on, and all bits up to lastBit
  Result := Integer((Cardinal($FFFFFFFF) shl firstBit) and
    (Cardinal($FFFFFFFF) shr (31 - lastBit)));
end;

{ TBitArray }

class function TBitArray.Create(const Size: Integer): TBitArray;
begin
  if (Size < 1) then
    raise EArgumentException.Create('size must be at least 1.');
  Result.FSize := Size;
  SetLength(Result.FBits, (Size + 31) shr 5);
end;

class function TBitArray.Empty: TBitArray;
begin
  Result.FSize := 0;
  Result.FBits := nil;
end;

function TBitArray.IsEmpty: Boolean;
begin
  Result := (FBits = nil);
end;

function TBitArray.Size: Integer;
begin
  Result := FSize;
end;

function TBitArray.SizeInBytes: Integer;
begin
  Result := (FSize + 7) shr 3;
end;

function TBitArray.GetBit(i: Integer): Boolean;
begin
  // (as an unsigned compare: one test for both ends)
  if (Cardinal(i) < Cardinal(FSize)) then
    Result := (FBits[i shr 5] and (1 shl (i and $1F))) <> 0
  else
    Result := false;
end;

procedure TBitArray.SetBit(i: Integer; Value: Boolean);
var
  index: Integer;
begin
  if (Cardinal(i) >= Cardinal(FSize)) then
    exit;
  index := i shr 5;
  if (Value) then
    FBits[index] := FBits[index] or (1 shl (i and $1F))
  else
    FBits[index] := FBits[index] and not (1 shl (i and $1F));
end;

function TBitArray.getNextSet(from: Integer): Integer;
var
  bitsOffset: Integer;
  currentBits: Cardinal;
begin
  if (from < 0) then
    from := 0;
  if (from >= FSize) then
    exit(FSize);

  bitsOffset := from shr 5;
  // mask off lesser bits first
  currentBits := Cardinal(FBits[bitsOffset]) and
    (Cardinal($FFFFFFFF) shl (from and $1F));

  while (currentBits = 0) do
  begin
    Inc(bitsOffset);
    if (bitsOffset = Length(FBits)) then
      exit(FSize);
    currentBits := Cardinal(FBits[bitsOffset]);
  end;

  Result := (bitsOffset shl 5) + TMathUtils.TrailingZeros(currentBits);
  if (Result > FSize) then
    Result := FSize;
end;

function TBitArray.getNextUnset(from: Integer): Integer;
var
  bitsOffset: Integer;
  currentBits: Cardinal;
begin
  if (from < 0) then
    from := 0;
  if (from >= FSize) then
    exit(FSize);

  bitsOffset := from shr 5;
  // mask off lesser bits first
  currentBits := (not Cardinal(FBits[bitsOffset])) and
    (Cardinal($FFFFFFFF) shl (from and $1F));

  while (currentBits = 0) do
  begin
    Inc(bitsOffset);
    if (bitsOffset = Length(FBits)) then
      exit(FSize);
    currentBits := not Cardinal(FBits[bitsOffset]);
  end;

  Result := (bitsOffset shl 5) + TMathUtils.TrailingZeros(currentBits);
  if (Result > FSize) then
    Result := FSize;
end;

procedure TBitArray.setBulk(i, newBits: Integer);
begin
  if (Cardinal(i) < Cardinal(FSize)) then
    FBits[i shr 5] := newBits;
end;

procedure TBitArray.setRange(start, ending: Integer);
var
  firstInt, lastInt, i, firstBit, lastBit: Integer;
begin
  if (ending < start) then
    raise EArgumentException.Create('Start is greater than end');
  // (only the bits of the array)
  if (start < 0) then
    start := 0;
  if (ending > FSize) then
    ending := FSize;
  if (ending <= start) then
    exit;
  Dec(ending); // will be easier to treat this as the last actually set bit -- inclusive
  firstInt := start shr 5;
  lastInt := ending shr 5;
  for i := firstInt to lastInt do
  begin
    if (i > firstInt) then
      firstBit := 0
    else
      firstBit := (start and $1F);
    if (i < lastInt) then
      lastBit := 31
    else
      lastBit := (ending and $1F);
    FBits[i] := FBits[i] or BitMask(firstBit, lastBit);
  end;
end;

procedure TBitArray.Reverse;
var
  i, len, oldBitsLen, leftOffset: Integer;
  x, y, mask, nextInt, currentInt: Cardinal;

  function Reversed(x: Cardinal): Cardinal; inline;
  begin
    // (logical shifts: every step masks the 32 bits it keeps, as the Java
    // original does on a long)
    x := ((x shr 1) and $55555555) or ((x and $55555555) shl 1);
    x := ((x shr 2) and $33333333) or ((x and $33333333) shl 2);
    x := ((x shr 4) and $0F0F0F0F) or ((x and $0F0F0F0F) shl 4);
    x := ((x shr 8) and $00FF00FF) or ((x and $00FF00FF) shl 8);
    Result := (x shr 16) or (x shl 16);
  end;

begin
  if (FSize = 0) then
    exit;
  // reverse the words first, each with its bits reversed, in place
  len := (FSize - 1) shr 5;
  oldBitsLen := len + 1;
  for i := 0 to (oldBitsLen div 2) - 1 do
  begin
    x := Reversed(Cardinal(FBits[i]));
    y := Reversed(Cardinal(FBits[len - i]));
    FBits[i] := Integer(y);
    FBits[len - i] := Integer(x);
  end;
  if Odd(oldBitsLen) then
    FBits[len shr 1] := Integer(Reversed(Cardinal(FBits[len shr 1])));
  // now correct the words if the bit size isn't a multiple of 32: shift
  // the bits down by the unused bits of the last word (each word is read
  // before the word below it is written)
  if (FSize <> oldBitsLen * 32) then
  begin
    leftOffset := oldBitsLen * 32 - FSize;
    mask := Cardinal($FFFFFFFF) shr leftOffset;
    currentInt := Cardinal(FBits[0]) shr leftOffset;
    for i := 1 to oldBitsLen - 1 do
    begin
      nextInt := Cardinal(FBits[i]);
      currentInt := currentInt or (nextInt shl (32 - leftOffset));
      FBits[i - 1] := Integer(currentInt);
      currentInt := (nextInt shr leftOffset) and mask;
    end;
    FBits[oldBitsLen - 1] := Integer(currentInt);
  end;
end;

procedure TBitArray.clear;
begin
  if (FBits <> nil) then
    FillChar(FBits[0], Length(FBits) * SizeOf(FBits[0]), 0);
end;

function TBitArray.isRange(start, ending: Integer;
  const value: Boolean): Boolean;
var
  firstInt, lastInt, firstBit, lastBit, mask, i, temp: Integer;
begin
  if (ending = start) then
    exit(true); // empty range matches
  if (start < 0) or (ending > FSize) or (ending < start) then
    exit(false);
  Dec(ending); // will be easier to treat this as the last actually set bit -- inclusive

  firstInt := start shr 5;
  lastInt := ending shr 5;
  for i := firstInt to lastInt do
  begin
    if (i > firstInt) then
      firstBit := 0
    else
      firstBit := (start and $1F);
    if (i < lastInt) then
      lastBit := 31
    else
      lastBit := (ending and $1F);

    mask := BitMask(firstBit, lastBit);
    // Return false if we're looking for 1s and the masked bits[i] isn't all 1s (that is,
    // equals the mask, or we're looking for 0s and the masked portion is not all 0s
    if (value) then
      temp := mask
    else
      temp := 0;
    if ((FBits[i] and mask) <> temp) then
      exit(false);
  end;

  Result := true;
end;

{ TBitArrayHelpers }

class function TBitArrayHelpers.CreateBitArray: TBitArray;
begin
  Result := TBitArray.Empty;
end;

class function TBitArrayHelpers.CreateBitArray(const Size: Integer): TBitArray;
begin
  Result := TBitArray.Create(Size);
end;

end.
