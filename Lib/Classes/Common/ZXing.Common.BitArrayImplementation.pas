unit ZXing.Common.BitArrayImplementation;

interface
uses ZXing.Common.BitArray;

function NewBitArray:IBitArray; overload;
function NewBitArray(const Size: Integer):IBitArray; overload;

implementation
uses
  System.SysUtils,
  ZXing.Common.Detector.MathUtils;


type
  /// <summary>
  /// A simple, fast array of bits, represented compactly by an array of ints internally.
  /// </summary>
  TBitArrayImplementation = class(TInterfacedObject, IBitArray)
  strict private
    _lookup: TArray<Integer>;
    Fbits: TArray<Integer>;
    Fsize: Integer;
    procedure InitLookup();
    function GetBit(i: Integer): Boolean;
    procedure SetBit(i: Integer; Value: Boolean);
    function makeArray(Size: Integer): TArray<Integer>;
    procedure ensureCapacity(size: Integer);


    function numberOfTrailingZeros(num: Integer): Integer;
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
    procedure appendBit(bit: Boolean);
    procedure Reverse();
    procedure clear();

    function isRange(start, ending: Integer;
      const value: Boolean): Boolean;
  end;


constructor TBitArrayImplementation.Create;
begin
  Fsize := 0;
  SetLength(Fbits, 1);
  InitLookup();
end;

constructor TBitArrayImplementation.Create(const Size: Integer);
begin
  if (Size < 1)
  then
     raise EArgumentException.Create('size must be at least 1.');

  Fsize := Size;
  Fbits := makeArray(Size);
  InitLookup();
end;

procedure TBitArrayImplementation.ensureCapacity(size: Integer);
var
  newBits : TArray<Integer>;
begin
  if (size > TMathUtils.Asr(Length(Fbits), 5)) then
  begin
    newBits := makeArray(size);
    Move(Fbits[0], newBits[0], Length(Fbits));
    Fbits := newBits;
  end;
end;

destructor TBitArrayImplementation.Destroy;
begin
  Fbits := nil;
  _lookup := nil;
  inherited;
end;

function TBitArrayImplementation.GetBit(i: Integer): Boolean;
begin
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
  bitsOffset,
  currentBits : Integer;
begin
  if (from >= Fsize) then
  begin
    Result := Fsize;
    exit;
  end;
  bitsOffset := TMathUtils.Asr(from, 5);
  currentBits := Fbits[bitsOffset];
  // mask off lesser bits first
  currentBits := currentBits and (not((1 shl (from and $1F)) - 1));
  while (currentBits = 0) do
  begin
    Inc(bitsOffset);
    if (bitsOffset = Length(Fbits)) then
    begin
      Result := Fsize;
      exit;
    end;
    currentBits := Fbits[bitsOffset];
  end;

  Result := (bitsOffset shl 5) + numberOfTrailingZeros(currentBits);

  if (Result > Fsize) then
  begin
    Result := Fsize;
  end;

end;

/// <summary>
/// see getNextSet(int)
/// </summary>
/// <param name="from">index to start looking for unset bit</param>
/// <returns>index of next unset bit, or <see cref="Size"/> if none are unset until the end</returns>
function TBitArrayImplementation.getNextUnset(from: Integer): Integer;
var
  bitsOffset,
  currentBits: Integer;
begin
  if (from >= Fsize) then
  begin
    Result := Fsize;
    exit;
  end;
  bitsOffset := TMathUtils.Asr(from, 5);
  currentBits := not Fbits[bitsOffset];

  // mask off lesser bits first
  currentBits := currentBits and (not( (1 shl (from and $1F)) - 1));
  while (currentBits = 0) do
  begin
    Inc(bitsOffset);
    if (bitsOffset = Length(Fbits)) then
    begin
      Result := Size;
      exit;
    end;
    currentBits := not Fbits[bitsOffset];
  end;
  Result := (bitsOffset shl 5) + numberOfTrailingZeros(currentBits);

  if (Result > Size)
  then
     Result := Size;
end;

procedure TBitArrayImplementation.InitLookup;
begin
  _lookup := TArray<Integer>.Create(32, 0, 1, 26, 2, 23, 27, 0, 3, 16, 24, 30,
    28, 11, 0, 13, 4, 7, 17, 0, 25, 22, 31, 15, 29, 10, 12, 6, 0, 21, 14, 9, 5,
    20, 8, 19, 18);
end;

function TBitArrayImplementation.makeArray(Size: Integer): TArray<Integer>;
var
  ar: TArray<Integer>;
begin
  SetLength(ar, TMathUtils.Asr((Size + 31), 5));
  Result := ar;
end;

function TBitArrayImplementation.numberOfTrailingZeros(num: Integer): Integer;
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

procedure TBitArrayImplementation.Reverse;
var
  newBits: TArray<Integer>;
  i, len, oldBitsLen, leftOffset, mask, nextInt, currentInt: Integer;
  x: Int64;
begin

  SetLength(newBits, Length(Fbits));
  // reverse all int's first
  len := TMathUtils.Asr((Size - 1), 5);
  oldBitsLen := len + 1;
  for i := 0 to oldBitsLen - 1 do
  begin
    x := Fbits[i];
    x := (TMathUtils.Asr(x, 1) and $55555555) or ((x and $55555555) shl 1);
    x := (TMathUtils.Asr(x, 2) and $33333333) or ((x and $33333333) shl 2);
    x := (TMathUtils.Asr(x, 4) and $0F0F0F0F) or ((x and $0F0F0F0F) shl 4);
    x := (TMathUtils.Asr(x, 8) and $00FF00FF) or ((x and $00FF00FF) shl 8);
    x := (TMathUtils.Asr(x, 16) and $0000FFFF) or ((x and $0000FFFF) shl 16);
    newBits[len - i] := integer(x) ;
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

  Fbits := newBits;
end;

procedure TBitArrayImplementation.SetBit(i: Integer; Value: Boolean);
var
  index: Integer;
begin
  if (Value) then
  begin
    index := TMathUtils.Asr(i, 5);
    Fbits[index] := Fbits[index] or 1 shl (i and $1F);
  end;
end;

function TBitArrayImplementation.Size: Integer;
begin
  result := FSize;
end;

function TBitArrayImplementation.SizeInBytes: Integer;
begin
  Result := TMathUtils.Asr(Size + 7, 3);
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
var
  r: Integer;
begin
  r := TMathUtils.Asr(i, 5);
  Fbits[r] := newBits;
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
  mask,
  i, j : Integer;
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
  firstInt := TMathUtils.Asr(start, 5);
  lastInt := TMathUtils.Asr(ending, 5);
  for i := firstInt to lastInt do
  begin
    if (i > firstInt)
    then
       firstBit := start
    else
       firstBit := $1F;
    if (i < lastInt)
    then
       lastBit := 31
    else
       lastBit := (ending and $1F);

    if ((firstBit = 0) and (lastBit = 31))
    then
       mask := -1
    else
    begin
      mask := 0;
      for j := firstBit to lastBit do
      begin
        mask := (mask or (1 shl j));
      end;
    end;
    bits[i] := (bits[i] or mask);
  end;
end;

/// <summary> Clears all bits (sets to false).</summary>
procedure TBitArrayImplementation.Clear;
var
  max,
  i: Integer;
begin
  max := Length(Fbits);
  for i := 0 to Pred(max) do
  begin
    Fbits[i] := 0;
  end;
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
  i, j,
  temp: Integer;
begin
  if (ending < start) then
  begin
    Result := False; // there is a bug here some how. We just exits with false
    //exit;
    //raise EArgumentException.Create('End is greater then start');
  end;

  if (ending = start) then
  begin
    Result := true; // empty range matches
    exit;
  end;
  Dec(ending); // will be easier to treat this as the last actually set bit -- inclusive

  firstInt := TMathUtils.Asr(start, 5);
  lastInt := TMathUtils.Asr(ending, 5);
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

    if ((firstBit = 0) and (lastBit = 31))
  	then
       mask := -1
    else
    begin
      mask := 0;
      for j := firstBit to lastBit do
        mask := mask or (1 shl j);
    end;

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

/// <summary>
/// Appends the bit.
/// </summary>
/// <param name="bit">The bit.</param>
procedure TBitArrayImplementation.appendBit(bit: Boolean);
var
  i: Integer;
begin
  ensureCapacity(Fsize + 1);
  if (bit) then
  begin
    i := TMathUtils.Asr(Fsize, 5);
    bits[i] := (bits[i] or (Fsize and $1F));
  end;
  Dec(Fsize);
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
