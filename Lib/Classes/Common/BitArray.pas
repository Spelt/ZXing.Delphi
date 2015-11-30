unit BitArray;

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

uses SysUtils, MathUtils;

type
  TBitArray = class
  private
    _lookup: TArray<Integer>;

    FBits: TArray<Integer>;
    Fsize: Integer;
    procedure InitLookup();
    function GetBit(i: Integer): Boolean;
    procedure SetBit(i: Integer; Value: Boolean);
    function MakeArray(Size: Integer): TArray<Integer>;

    function numberOfTrailingZeros(num: Integer): Integer;

  public
    property Size: Integer read Fsize;
    function SizeInBytes: Integer;

    property Self[i: Integer]: Boolean read GetBit write SetBit; default;
    property Bits: TArray<Integer> read FBits;

    constructor Create(); overload;
    constructor Create(Size: Integer); overload;
    function getNextSet(from: Integer): Integer;
    function getNextUnset(from: Integer): Integer;

    procedure setBulk(i, newBits: Integer);
    procedure Reverse();
    procedure Clear();
    /// <summary> Efficient method to check if a range of bits is set, or not set.
    ///
    /// </summary>
    /// <param name="start">start of range, inclusive.
    /// </param>
    /// <param name="end">end of range, exclusive
    /// </param>
    /// <param name="value">if true, checks that bits in range are set, otherwise checks that they are not set
    /// </param>
    /// <returns> true iff all bits are set or not set in range, according to value argument
    /// </returns>
    /// <throws>  IllegalArgumentException if end is less than or equal to start </throws>
    function isRange(start: Integer; ending: Integer; Value: Boolean): Boolean;

  end;

implementation

{ TBitArray }

constructor TBitArray.Create;
begin
  Fsize := 0;
  SetLength(FBits, 1);
  InitLookup();
end;

procedure TBitArray.Clear;
var
  n, i: Integer;
begin
  n := Length(FBits) - 1;
  for i := 0 to n do
  begin
    FBits[i] := 0;
  end;
end;

constructor TBitArray.Create(Size: Integer);
begin
  if (Size < 1) then
  begin
    // raise EArgumentException.Create('size must be at least 1.');
    exit;
  end;

  Fsize := Size;
  FBits := MakeArray(Size);
  InitLookup();

end;

function TBitArray.GetBit(i: Integer): Boolean;
begin
  Result := ((FBits[TMathUtils.Asr(i, 5)]) and (1 shl (i and $1F))) <> 0;
end;

function TBitArray.getNextSet(from: Integer): Integer;
var
  bitsOffset, currentBits: Integer;

begin
  if (from >= Fsize) then
  begin
    Result := Fsize;
    exit;
  end;
  bitsOffset := TMathUtils.Asr(from, 5);
  currentBits := FBits[bitsOffset];
  // mask off lesser bits first
  currentBits := currentBits and (not((1 shl (from and $1F)) - 1));
  while (currentBits = 0) do
  begin
    inc(bitsOffset);
    if (bitsOffset = Length(FBits)) then
    begin
      Result := Fsize;
      exit;
    end;
    currentBits := FBits[bitsOffset];
  end;

  Result := (bitsOffset shl 5) + numberOfTrailingZeros(currentBits);

  if (Result > Fsize) then
  begin
    Result := Fsize;
  end;

end;

function TBitArray.getNextUnset(from: Integer): Integer;
var
  bitsOffset, currentBits: Integer;
begin

  if (from >= Fsize) then
  begin
    Result := Fsize;
    exit;
  end;
  bitsOffset := TMathUtils.Asr(from, 5);
  currentBits := not FBits[bitsOffset];

  // mask off lesser bits first
  currentBits := currentBits and (not((1 shl (from and $1F)) - 1));
  while (currentBits = 0) do
  begin
    inc(bitsOffset);
    if (bitsOffset = Length(FBits)) then
    begin
      Result := Size;
      exit;
    end;
    currentBits := not FBits[bitsOffset];
  end;
  Result := (bitsOffset shl 5) + numberOfTrailingZeros(currentBits);

  if (Result > Size) then
  begin
    Result := Size;
  end;

end;

procedure TBitArray.InitLookup;
begin
  _lookup := TArray<Integer>.Create(32, 0, 1, 26, 2, 23, 27, 0, 3, 16, 24, 30,
    28, 11, 0, 13, 4, 7, 17, 0, 25, 22, 31, 15, 29, 10, 12, 6, 0, 21, 14, 9, 5,
    20, 8, 19, 18);
end;

function TBitArray.isRange(start, ending: Integer; Value: Boolean): Boolean;
var
  firstInt, lastInt, firstBit, lastBit, mask, i, j, temp: Integer;
begin

  if (ending < start) then
  begin
    Result := False; // there is a bug here some how. We just exits with fals
    exit;
    raise EArgumentException.Create('End is greater then start');
  end;

  if (ending = start) then
  begin
    Result := true; // empty range matches
    exit;
  end;
  dec(ending);

  // will be easier to treat this as the last actually set bit -- inclusive
  firstInt := TMathUtils.Asr(start, 5);
  lastInt := TMathUtils.Asr(ending, 5);
  for i := firstInt to lastInt do
  begin

    if (i > firstInt) then
    begin
      firstBit := 0;
    end
    else
    begin
      firstBit := start and $1F;
    end;

    if (i < lastInt) then
    begin
      lastBit := 31;
    end
    else
    begin
      lastBit := ending and $1F;
    end;

    if ((firstBit = 0) and (lastBit = 31)) then
    begin
      mask := -1;
    end
    else
    begin
      mask := 0;
      for j := firstBit to lastBit do
      begin
        mask := mask or (1 shl j);
      end;
    end;

    // Return false if we're looking for 1s and the masked bits[i] isn't all 1s (that is,
    // equals the mask, or we're looking for 0s and the masked portion is not all 0s

    temp := 0;
    if (Value) then
    begin
      temp := mask;
    end;

    if ((FBits[i] and mask) <> (temp)) then
    begin
      Result := False;
      exit;
    end;

  end;

  Result := true;
end;

function TBitArray.MakeArray(Size: Integer): TArray<Integer>;
var
  ar: TArray<Integer>;
begin
  SetLength(ar, TMathUtils.Asr((Size + 31), 5));
  Result := ar;
end;

function TBitArray.numberOfTrailingZeros(num: Integer): Integer;
var
  index: Integer;
begin
  index := (-num and num) mod 37;
  if (index < 0) then
  begin
    index := index * -1;
  end;
  Result := _lookup[index];
end;

procedure TBitArray.Reverse;
var
  newBits: TArray<Integer>;
  i, len, oldBitsLen, leftOffset, mask, nextInt, currentInt: Integer;
  x: Int64;
begin

  SetLength(newBits, Length(FBits));
  // reverse all int's first
  len := TMathUtils.Asr((Size - 1), 5);
  oldBitsLen := len + 1;
  for i := 0 to oldBitsLen - 1 do
  begin
    x := FBits[i];
    x := (TMathUtils.Asr(x, 1) and $55555555) or ((x and $55555555) shl 1);
    x := (TMathUtils.Asr(x, 2) and $33333333) or ((x and $33333333) shl 2);
    x := (TMathUtils.Asr(x, 4) and $0F0F0F0F) or ((x and $0F0F0F0F) shl 4);
    x := (TMathUtils.Asr(x, 8) and $00FF00FF) or ((x and $00FF00FF) shl 8);
    x := (TMathUtils.Asr(x, 16) and $0000FFFF) or ((x and $0000FFFF) shl 16);
    newBits[len - i] := x;
  end;
  // now correct the int's if the bit size isn't a multiple of 32
  if (Size <> oldBitsLen * 32) then
  begin
    leftOffset := oldBitsLen * 32 - Size;
    mask := 1;
    for i := 0 to 31 - leftOffset - 1 do
    begin
      mask := (mask shl 1) or 1;
    end;

    currentInt := TMathUtils.Asr(newBits[0], leftOffset) and mask;
    for i := 1 to oldBitsLen - 1 do
    begin
      nextInt := newBits[i];
      currentInt := currentInt or (nextInt shl (32 - leftOffset));
      newBits[i - 1] := currentInt;
      currentInt := TMathUtils.Asr(nextInt, leftOffset) and mask;
    end;
    newBits[oldBitsLen - 1] := currentInt;
  end;

  FBits := newBits;

end;

procedure TBitArray.SetBit(i: Integer; Value: Boolean);
var
  index: Integer;
begin
  if (Value) then
  begin
    index := TMathUtils.Asr(i, 5);
    FBits[index] := FBits[index] or 1 shl (i and $1F);
  end;
end;

function TBitArray.SizeInBytes: Integer;
begin
  Result := TMathUtils.Asr(Size + 7, 3);
end;

procedure TBitArray.setBulk(i: Integer; newBits: Integer);
var
  r: Integer;
begin
  r := TMathUtils.Asr(i, 5);
  FBits[r] := newBits
end;

end.
