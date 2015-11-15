unit BitSource;

interface

uses SysUtils, MathUtils;

type

  TBitSource = class sealed
  private
    bytes: TArray<Byte>;
  public
    BitOffset: Integer;
    ByteOffset: Integer;
    constructor Create(bytes: TArray<Byte>);
    function available: Integer;
    function readBits(numBits: Integer): Integer;
  end;

implementation

{ TBitSource }

function TBitSource.available: Integer;
begin
  Result := ((8 * (Length(self.bytes) - self.ByteOffset)) - self.BitOffset)
end;

constructor TBitSource.Create(bytes: TArray<Byte>);
begin
  self.bytes := bytes;
end;

function TBitSource.readBits(numBits: Integer): Integer;
var
  bitsToNotRead, bitsLeft, toRead, mask: Integer;
begin
  if (((numBits < 1) or (numBits > $20)) or (numBits > self.available)) then
    raise EArgumentException.Create(numBits.ToString + 'numBits');

  Result := 0;
  if (self.BitOffset > 0) then
  begin

    bitsLeft := (8 - self.BitOffset);
    if (numBits < bitsLeft) then
      toRead := numBits
    else
      toRead := bitsLeft;

    bitsToNotRead := (bitsLeft - toRead);
    mask := TMathUtils.Asr($FF, ((8 - toRead) and $1F)) shl bitsToNotRead;
    Result := TMathUtils.Asr((self.bytes[self.ByteOffset] and mask),
      bitsToNotRead);
    dec(numBits, toRead);
    inc(self.BitOffset, toRead);

    if (self.BitOffset = 8) then
    begin
      self.BitOffset := 0;
      inc(self.ByteOffset)
    end
  end;

  if (numBits > 0) then
  begin

    while ((numBits >= 8)) do
    begin
      Result := ((Result shl 8) or (self.bytes[self.ByteOffset] and $FF));
      inc(self.ByteOffset);
      dec(numBits, 8)
    end;

    if (numBits > 0) then
    begin
      bitsToNotRead := (8 - numBits);

      mask := TMathUtils.Asr($FF, (bitsToNotRead and $1F)) shl bitsToNotRead;

      Result := TMathUtils.Asr
        (((Result shl numBits) or (self.bytes[self.ByteOffset] and mask)),
        bitsToNotRead);

      inc(self.BitOffset, numBits)
    end

  end;

  Result := Result;

end;

end.
