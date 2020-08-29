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

  * Original Authors: Sean Owen and dswitkin@google.com (Daniel Switkin)
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.Common.BitMatrix;

interface

uses
  SysUtils,
{$IFDEF USE_VCL_BITMAP}
  VCL.Graphics,
{$ELSE}
  FMX.Graphics,
{$ENDIF}
  Generics.Collections,
  ZXing.Common.BitArray,
  ZXing.BarcodeFormat,
  ZXing.Helpers,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary>
  /// <p>Represents a 2D matrix of bits. In function arguments below, and throughout the common
  /// module, x is the column position, and y is the row position. The ordering is always x, y.
  /// The origin is at the top-left.</p>
  /// <p>Internally the bits are represented in a 1-D array of 32-bit ints. However, each row begins
  /// with a new int. This is done intentionally so that we can copy out a row into a BitArray very
  /// efficiently.</p>
  /// <p>The ordering of bits is row-major. Within each int, the least significant bits are used first,
  /// meaning they represent lower x values. This is compatible with BitArray's implementation.</p>
  /// </summary>
  TBitMatrix = class sealed
  private
    Fbits: TArray<Integer>;
    Fheight: Integer;
    FrowSize: Integer;
    Fwidth: Integer;

    function getBit(x, y: Integer): Boolean;
    procedure setBit(x, y: Integer; const value: Boolean);

    constructor Create(const width, height, rowSize: Integer;
      const bits: TArray<Integer>); overload;
  public
    constructor Create(const width, height: Integer); overload;
    constructor Create(const dimension: Integer); overload;
    destructor Destroy; override;

    procedure clear;
    function Clone: TObject;
    function Equals(obj: TObject): Boolean; override;
    procedure flip(x: Integer; y: Integer);
    function getBottomRightOnBit: TArray<Integer>;
    function getEnclosingRectangle: TArray<Integer>;
    function GetHashCode: Integer; override;
    function getRow(const y: Integer; row: IBitArray): IBitArray;
    function getTopLeftOnBit: TArray<Integer>;
    procedure Rotate180;
    procedure setRegion(left: Integer; top: Integer; width: Integer;
      height: Integer);
    procedure setRow(y: Integer; row: IBitArray);
    function ToBitmap: TBitmap; overload;
    function ToBitmap(format: TBarcodeFormat; content: string)
      : TBitmap; overload;

    property width: Integer read Fwidth;
    property height: Integer read Fheight;
    property Matrix[x, y: Integer]: Boolean read getBit write setBit; default;
    // added for debugging
    function ToString: string; override;
  end;

implementation

{ TBitMatrix }

function TBitMatrix.getBit(x, y: Integer): Boolean;
var
  offset, v, bits, shift: Int64;
  uBits: Cardinal;
begin
  offset := y * FrowSize + TMathUtils.Asr(x, 5);
  try
    bits := Fbits[offset];
    uBits := Cardinal(bits);
    shift := (x and $1F);
    v := TMathUtils.Asr(uBits, shift);
    Result := (v and 1) <> 0;
  except
    Result := false;
  end;

end;

procedure TBitMatrix.setBit(x, y: Integer; const value: Boolean);
var
  offset: NativeInt;
begin
  if (value) then
  begin
    offset := y * FrowSize + TMathUtils.Asr(x, 5);
    Fbits[offset] := Fbits[offset] or (1 shl (x and $1F));
  end
  else
  begin
    offset := Trunc(y * FrowSize + (x / 32));
    Fbits[offset] := Fbits[offset] and (not(1 shl (x and $1F)));
  end;
end;

procedure TBitMatrix.clear;
var
  max, i: Integer;
begin
  max := Length(Fbits);
  i := 0;
  while ((i < max)) do
  begin
    Fbits[i] := 0;
    Inc(i);
  end;
end;

function TBitMatrix.Clone: TObject;
var
  b: TArray<Integer>;
begin
  b := TArray.Clone(Fbits);
  Result := TBitMatrix.Create(Self.Fwidth, Self.Fheight, Self.FrowSize, b);
end;

constructor TBitMatrix.Create(const width, height, rowSize: Integer;
  const bits: TArray<Integer>);
begin
  if ((width < 1) or (height < 1)) then
    raise EArgumentException.Create('Both dimensions must be greater than 0');

  Self.Fwidth := width;
  Self.Fheight := height;
  Self.FrowSize := rowSize;
  Self.Fbits := bits;
end;

constructor TBitMatrix.Create(const width, height: Integer);
begin
  if ((width < 1) or (height < 1)) then
    raise EArgumentException.Create('Both dimensions must be greater than 0');

  Self.Fwidth := width;
  Self.Fheight := height;
  Self.FrowSize := TMathUtils.Asr((width + $1F), 5);
  SetLength(Self.Fbits, Self.FrowSize * height);
  Self.clear;
end;

constructor TBitMatrix.Create(const dimension: Integer);
begin
  Self.Create(dimension, dimension);
end;

destructor TBitMatrix.Destroy;
begin
  Self.Fbits := nil;
  inherited;
end;

function TBitMatrix.Equals(obj: TObject): Boolean;
var
  other: TBitMatrix;
  i, l: Integer;
begin
  if (not(obj is TBitMatrix)) then
  begin
    Result := false;
    exit;
  end;

  other := (obj as TBitMatrix);
  if ((((Fwidth <> other.Fwidth) or (Fheight <> other.Fheight)) or
    (FrowSize <> other.FrowSize)) or (Length(Fbits) <> Length(other.Fbits)))
  then
  begin
    Result := false;
    exit
  end;

  i := 0;
  l := Length(Fbits);

  while ((i < l)) do
  begin
    if (Fbits[i] <> other.Fbits[i]) then
    begin
      Result := false;
      exit
    end;
    Inc(i)
  end;

  Result := true;
end;

procedure TBitMatrix.flip(x, y: Integer);
var
  offset, s: Integer;
begin
  s := TMathUtils.Asr(x, 5);
  offset := ((y * FrowSize) + s);
  Fbits[offset] := (Fbits[offset] xor (1 shl x))
end;

function TBitMatrix.getBottomRightOnBit: TArray<Integer>;
var
  bitsOffset, x, y, theBits, bit: Integer;
begin
  bitsOffset := Length(Fbits) - 1;
  while ((bitsOffset >= 0) and (Self.Fbits[bitsOffset] = 0)) do
  begin
    dec(bitsOffset)
  end;

  if (bitsOffset < 0) then
  begin
    Result := nil;
    exit
  end;

  y := (bitsOffset div FrowSize);
  x := ((bitsOffset mod FrowSize) shl 5);
  theBits := Fbits[bitsOffset];
  bit := $1F;

  while ((TMathUtils.Asr(theBits, bit) = 0)) do
  begin
    dec(bit);
  end;

  Inc(x, bit);
  Result := TArray<Integer>.Create(x, y);
end;

function TBitMatrix.getEnclosingRectangle: TArray<Integer>;
var
  bit, left, top, right, bottom, y, x32, theBits, widthTmp, heightTmp: Integer;
begin
  left := Self.Fwidth;
  top := Self.Fheight;
  right := -1;
  bottom := -1;
  y := 0;

  while ((y < Self.Fheight)) do
  begin
    x32 := 0;

    while ((x32 < Self.FrowSize)) do
    begin
      theBits := Self.Fbits[((y * Self.FrowSize) + x32)];

      if (theBits <> 0) then
      begin

        if (y < top) then
          top := y;

        if (y > bottom) then
          bottom := y;

        if ((x32 * $20) < left) then
        begin
          bit := 0;
          while (((theBits shl ($1F - bit)) = 0)) do
          begin
            Inc(bit)
          end;
          if (((x32 * $20) + bit) < left) then
            left := ((x32 * $20) + bit)
        end;

        if (((x32 * $20) + $1F) > right) then
        begin
          bit := $1F;
          while ((TMathUtils.Asr(theBits, bit) = 0)) do
          begin
            dec(bit)
          end;
          if (((x32 * $20) + bit) > right) then
            right := ((x32 * $20) + bit)
        end
      end;
      Inc(x32)
    end;
    Inc(y)
  end;

  widthTmp := (right - left);
  heightTmp := (bottom - top);

  if ((widthTmp < 0) or (heightTmp < 0)) then
  begin
    Result := nil;
    exit;
  end;

  Result := TArray<Integer>.Create(left, top, widthTmp, heightTmp);
end;

function TBitMatrix.GetHashCode: Integer;
var
  bit, hash: Integer;
begin
  hash := Self.Fwidth;
  hash := (($1F * hash) + Self.Fwidth);
  hash := (($1F * hash) + Self.Fheight);
  hash := (($1F * hash) + Self.FrowSize);

  for bit in Self.Fbits do
  begin
    hash := (($1F * hash) + bit); // .GetHashCode
  end;

  Result := hash;
end;

function TBitMatrix.getRow(const y: Integer; row: IBitArray): IBitArray;
var
  x, offset: Integer;
begin
  if ((row = nil) or (row.Size < Self.Fwidth)) then
    row := TBitArrayHelpers.CreateBitArray(Self.Fwidth)
  else
    row.clear;
  offset := (y * Self.FrowSize);
  x := 0;

  while ((x < Self.FrowSize)) do
  begin
    row.setBulk((x shl 5), Self.Fbits[(offset + x)]);
    Inc(x)
  end;

  Result := row;
end;

function TBitMatrix.getTopLeftOnBit: TArray<Integer>;
var
  bitsOffset, x, y, theBits, bit: Integer;
begin
  bitsOffset := 0;

  while (((bitsOffset < Length(Fbits)) and (Fbits[bitsOffset] = 0))) do
  begin
    Inc(bitsOffset)
  end;

  if (bitsOffset = Length(Fbits)) then
  begin
    Result := nil;
    exit
  end;

  y := (bitsOffset div Self.FrowSize);
  x := ((bitsOffset mod Self.FrowSize) shl 5);
  theBits := Self.Fbits[bitsOffset];
  bit := 0;

  while (((theBits shl ($1F - bit)) = 0)) do
  begin
    Inc(bit)
  end;

  Inc(x, bit);
  Result := TArray<Integer>.Create(x, y);
end;

procedure TBitMatrix.Rotate180;
var
  i, width, height: Integer;
  topRow, bottomRow: IBitArray;
begin
  width := Self.Fwidth;
  height := Self.Fheight;
  topRow := TBitArrayHelpers.CreateBitArray(width);
  bottomRow := TBitArrayHelpers.CreateBitArray(width);
  i := 0;

  while ((i < ((height + 1) div 2))) do
  begin
    topRow := Self.getRow(i, topRow);
    bottomRow := Self.getRow(((height - 1) - i), bottomRow);
    topRow.reverse;
    bottomRow.reverse;
    Self.setRow(i, bottomRow);
    Self.setRow(((height - 1) - i), topRow);
    Inc(i)
  end;
end;

procedure TBitMatrix.setRegion(left, top, width, height: Integer);
var
  x, y, offset, right, bottom: Integer;
begin
  if ((top < 0) or (left < 0)) then
    raise EArgumentException.Create('Left and top must be non-negative');

  if ((height < 1) or (width < 1)) then
    raise EArgumentException.Create('Height and width must be at least 1');

  right := (left + width);
  bottom := (top + height);

  if ((bottom > Self.Fheight) or (right > Self.Fwidth)) then
    raise EArgumentException.Create('The region must fit inside the matrix');

  y := top;

  while ((y < bottom)) do
  begin
    offset := (y * Self.FrowSize);
    x := left;
    while ((x < right)) do
    begin
      Fbits[(offset + TMathUtils.Asr(x, 5))] :=
        (Fbits[(offset + TMathUtils.Asr(x, 5))] or (1 shl x));
      Inc(x)
    end;
    Inc(y)
  end;
end;

procedure TBitMatrix.setRow(y: Integer; row: IBitArray);
begin
  Fbits := System.Copy(row.bits, (y * FrowSize), FrowSize);
end;

function TBitMatrix.ToBitmap(format: TBarcodeFormat; content: string): TBitmap;
begin
  raise ENotImplemented.Create('Converting to bitmap is not implemented yet!');
end;

function TBitMatrix.ToString: string;
var
  r, c: Integer;
begin
  for r := 0 to Fheight - 1 do
  begin
    for c := 0 to Fwidth - 1 do
    begin
      if Matrix[r, c] then
        Result := Result + '*'
      else
        Result := Result + '.';
    end;
    Result := Result + #13#10;
  end;
end;

function TBitMatrix.ToBitmap: TBitmap;
begin
  Result := ToBitmap(TBarcodeFormat.CODE_128, '')
end;

end.
