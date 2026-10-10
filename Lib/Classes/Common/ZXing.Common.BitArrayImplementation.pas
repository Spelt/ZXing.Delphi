unit ZXing.Common.BitArrayImplementation;

interface
uses ZXing.Common.BitArray;

function NewBitArray:IBitArray; overload;
function NewBitArray(const Size: Integer):IBitArray; overload;

implementation
uses
  System.SysUtils,
  ZXing.Common.Detector.MathUtils,
  Math;


type
  /// <summary>
  /// A simple, fast array of bits, represented compactly by an array of ints internally.
  /// </summary>
  TBitArrayImplementation = class(TInterfacedObject, IBitArray)
  strict private
    Fbits: TArray<Integer>;
    Fsize: Integer;
    function GetBit(i: Integer): Boolean;
    procedure SetBit(i: Integer; Value: Boolean);
    function makeArray(Size: Integer): TArray<Integer>;
    function GetBits: TArray<Integer>;

  private
    constructor Create(); overload;
    constructor Create(const Size: Integer); overload;

  public
    function Size: Integer;
    function SizeInBytes: Integer;

    property Self[i: Integer]: Boolean read GetBit write SetBit; default;
    property Bits: TArray<Integer> read Fbits;

    destructor Destroy; override;
    function getNextSet(from: Integer): Integer;
    function getNextUnset(from: Integer): Integer;

    procedure setBulk(i, newBits: Integer);
    procedure setRange(start, ending: Integer);
    procedure Reverse();
    procedure clear();

    function isRange(start, ending: Integer;
      const value: Boolean): Boolean;
  end;

/// <summary>The bits firstBit to lastBit (0 to 31, inclusive) of a word
/// set.</summary>
function BitMask(firstBit, lastBit: Integer): Integer; inline;
begin
  // all bits from firstBit on, and all bits up to lastBit
  Result := Integer((Cardinal($FFFFFFFF) shl firstBit) and
    (Cardinal($FFFFFFFF) shr (31 - lastBit)));
end;

constructor TBitArrayImplementation.Create;
begin
  Fsize := 0;
  SetLength(Fbits, 1);
end;

constructor TBitArrayImplementation.Create(const Size: Integer);
begin
  if (Size < 1)
  then
     raise EArgumentException.Create('size must be at least 1.');

  Fsize := Size;
  Fbits := makeArray(Size);
end;

destructor TBitArrayImplementation.Destroy;
begin
  Fbits := nil;
  inherited;
end;

function TBitArrayImplementation.GetBit(i: Integer): Boolean;
begin
  // i shr 5 is the arithmetic shift of before for i >= 0
  if (i >= 0) then
    Result := (Fbits[i shr 5] and (1 shl (i and $1F))) <> 0
  else
    Result := ((Fbits[TMathUtils.Asr(i, 5)]) and (1 shl (i and $1F))) <> 0;
end;

function TBitArrayImplementation.GetBits: TArray<Integer>;
begin
   result := FBits;
end;

/// <summary>
/// Gets the next set.
/// </summary>
/// <param name="from">first bit to check</param>
/// <returns>index of first bit that is set, starting from the given index, or size if none are set
/// at or beyond this given index</returns>
function TBitArrayImplementation.getNextSet(from: Integer): Integer;
var
  bitsOffset: Integer;
  currentBits: Cardinal;
begin
  if (from >= Fsize) then
    Exit(FSize);

  bitsOffset := from shr 5;
  // mask off lesser bits first
  currentBits := Cardinal(Fbits[bitsOffset]) and
    (Cardinal($FFFFFFFF) shl (from and $1F));

  while (currentBits = 0) do
  begin
    Inc(bitsOffset);
    if (bitsOffset = Length(Fbits)) then
      Exit(FSize);

    currentBits := Cardinal(Fbits[bitsOffset]);
  end;

  Result := (bitsOffset shl 5) + TMathUtils.TrailingZeros(currentBits);
  if (Result > FSize) then
    Result := FSize;
end;

/// <summary>
/// see getNextSet(int)
/// </summary>
/// <param name="from">index to start looking for unset bit</param>
/// <returns>index of next unset bit, or <see cref="Size"/> if none are unset until the end</returns>
function TBitArrayImplementation.getNextUnset(from: Integer): Integer;
var
  bitsOffset: Integer;
  currentBits: Cardinal;
begin
  if (from >= Fsize) then
    Exit(Fsize);

  bitsOffset := from shr 5;
  // mask off lesser bits first
  currentBits := (not Cardinal(Fbits[bitsOffset])) and
    (Cardinal($FFFFFFFF) shl (from and $1F));

  while (currentBits = 0) do
  begin
    Inc(bitsOffset);
    if (bitsOffset = Length(Fbits)) then
      Exit(Fsize);

    currentBits := not Cardinal(Fbits[bitsOffset]);
  end;

  Result := (bitsOffset shl 5) + TMathUtils.TrailingZeros(currentBits);
  if (Result > Fsize) then
    Result := Fsize;
end;

function TBitArrayImplementation.makeArray(Size: Integer): TArray<Integer>;
begin
  SetLength(Result, (Size + 31) shr 5);
end;

procedure TBitArrayImplementation.Reverse;
var
  newBits: TArray<Integer>;
  i, len, oldBitsLen, leftOffset: Integer;
  x, mask, nextInt, currentInt: Cardinal;
begin

  SetLength(newBits, Length(Fbits));
  // reverse all int's first (logical shifts: every step masks the 32 bits
  // it keeps, as the Java original does on a long)
  len := (Fsize - 1) shr 5;
  oldBitsLen := len + 1;
  for i := 0 to oldBitsLen - 1 do
  begin
    x := Cardinal(Fbits[i]);
    x := ((x shr 1) and $55555555) or ((x and $55555555) shl 1);
    x := ((x shr 2) and $33333333) or ((x and $33333333) shl 2);
    x := ((x shr 4) and $0F0F0F0F) or ((x and $0F0F0F0F) shl 4);
    x := ((x shr 8) and $00FF00FF) or ((x and $00FF00FF) shl 8);
    x := (x shr 16) or (x shl 16);
    newBits[len - i] := Integer(x);
  end;
  // now correct the int's if the bit size isn't a multiple of 32
  if (Fsize <> oldBitsLen * 32) then
  begin
    leftOffset := oldBitsLen * 32 - Fsize;
    mask := Cardinal($FFFFFFFF) shr leftOffset;

    currentInt := Cardinal(newBits[0]) shr leftOffset;
    for i := 1 to oldBitsLen - 1 do
    begin
      nextInt := Cardinal(newBits[i]);
      currentInt := currentInt or (nextInt shl (32 - leftOffset));
      newBits[i - 1] := Integer(currentInt);
      currentInt := (nextInt shr leftOffset) and mask;
    end;
    newBits[oldBitsLen - 1] := Integer(currentInt);
  end;

  Fbits := newBits;
end;

procedure TBitArrayImplementation.SetBit(i: Integer; Value: Boolean);
var
  index: Integer;
begin
  if (i >= 0) then
    index := i shr 5
  else
    index := TMathUtils.Asr(i, 5);
  if (Value) then
    Fbits[index] := Fbits[index] or (1 shl (i and $1F))
  else
    Fbits[index] := Fbits[index] and not (1 shl (i and $1F));
end;

function TBitArrayImplementation.Size: Integer;
begin
  result := FSize;
end;

function TBitArrayImplementation.SizeInBytes: Integer;
begin
  Result := (Fsize + 7) shr 3;
end;

/// <summary> Sets a block of 32 bits, starting at bit i.
///
/// </summary>
/// <param name="i">first bit to set
/// </param>
/// <param name="newBits">the new value of the next 32 bits. Note again that the least-significant bit
/// corresponds to bit i, the next-least-significant to i+1, and so on.
/// </param>
procedure TBitArrayImplementation.setBulk(i, newBits: Integer);
begin
  Fbits[i shr 5] := newBits;
end;

/// <summary>
/// Sets a range of bits.
/// </summary>
/// <param name="start">start of range, inclusive.</param>
/// <param name="ending">end of range, exclusive</param>
procedure TBitArrayImplementation.setRange(start, ending: Integer);
var
  firstInt,
  lastInt,
  i : Integer;
  firstBit,
  lastBit : Integer;
begin
  if (ending < start)
  then
     raise EArgumentException.Create('Start is greater than end');

  if (ending = start)
  then
     exit;
  Dec(ending); // will be easier to treat this as the last actually set bit -- inclusive
  firstInt := start shr 5;
  lastInt := ending shr 5;
  for i := firstInt to lastInt do
  begin
    if (i > firstInt)
    then
       firstBit := 0
    else
       firstBit := (start and $1F);
    if (i < lastInt)
    then
       lastBit := 31
    else
       lastBit := (ending and $1F);

    Fbits[i] := Fbits[i] or BitMask(firstBit, lastBit);
  end;
end;

/// <summary> Clears all bits (sets to false).</summary>
procedure TBitArrayImplementation.Clear;
begin
  if (Length(Fbits) > 0) then
    FillChar(Fbits[0], Length(Fbits) * SizeOf(Fbits[0]), 0);
end;

/// <summary> Efficient method to check if a range of bits is set, or not set.
///
/// </summary>
/// <param name="start">start of range, inclusive.
/// </param>
/// <param name="ending">end of range, exclusive
/// </param>
/// <param name="value">if true, checks that bits in range are set, otherwise checks that they are not set
/// </param>
/// <returns> true iff all bits are set or not set in range, according to value argument
/// </returns>
/// <throws>  EIllegalArgumentException if end is less than or equal to start </throws>
function TBitArrayImplementation.isRange(start, ending: Integer;
  const value: Boolean): Boolean;
var
  firstInt,
  lastInt,
  firstBit,
  lastBit,
  mask,
  i,
  temp: Integer;
begin
  if (ending = start) then
  begin
    Result := true; // empty range matches
    exit;
  end;
  Dec(ending); // will be easier to treat this as the last actually set bit -- inclusive

  firstInt := start shr 5;
  lastInt := ending shr 5;
  for i := firstInt to lastInt do
  begin
    if (i > firstInt)
    then
       firstBit := 0
    else
       firstBit := (start and $1F);

    if (i < lastInt)
    then
       lastBit := 31
    else
       lastBit := (ending and $1F);

    mask := BitMask(firstBit, lastBit);

    // Return false if we're looking for 1s and the masked bits[i] isn't all 1s (that is,
    // equals the mask, or we're looking for 0s and the masked portion is not all 0s
    if (Value)
    then
       temp := mask
    else
       temp := 0;

    if ((Fbits[i] and mask) <> (temp)) then
    begin
      Result := False;
      exit;
    end;
  end;

  Result := true;
end;


function NewBitArray:IBitArray;
begin
   result :=  TBitArrayImplementation.Create;
end;


function NewBitArray(const Size: Integer):IBitArray;
begin
   result :=  TBitArrayImplementation.Create(size);
end;


end.
