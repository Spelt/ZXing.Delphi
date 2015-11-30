unit Bitmatrix;

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

uses SysUtils, Generics.Collections, BitArray, BarcodeFormat,
  Fmx.Graphics, Helpers, MathUtils;

type

  TBitMatrix = class
  private
    Fbits: TArray<Integer>;
    Fheight: Integer;
    FrowSize: Integer;
    Fwidth: Integer;

    function GetBit(x, y: Integer): Boolean;
    procedure SetBit(x, y: Integer; Value: Boolean);

    constructor Create(width: Integer; height: Integer; rowSize: Integer;
      bits: TArray<Integer>); overload;

  public
    constructor Create(dimension: Integer); overload;
    constructor Create(width: Integer; height: Integer); overload;
    procedure Clear;
    function Clone: TObject;
    function Equals(obj: TObject): Boolean; override;
    procedure flip(x: Integer; y: Integer);
    function getBottomRightOnBit: TArray<Integer>;
    function getEnclosingRectangle: TArray<Integer>;
    function GetHashCode: Integer; override;
    function getRow(y: Integer; row: TBitArray): TBitArray;
    function getTopLeftOnBit: TArray<Integer>;
    procedure Rotate180;
    procedure setRegion(left: Integer; top: Integer; width: Integer;
      height: Integer);
    procedure setRow(y: Integer; row: TBitArray);
    function ToBitmap: TBitmap; overload;
    function ToBitmap(format: TBarcodeFormat; content: string)
      : TBitmap; overload;

    property width: Integer read Fwidth;
    property height: Integer read Fheight;
    property Self[x, y: Integer]: Boolean read GetBit write SetBit; default;

  end;

implementation

{ TBitMatrix }

function TBitMatrix.GetBit(x, y: Integer): Boolean;
var
  offset, s, shiftOp: Integer;
begin
  s := TMathUtils.Asr(x, 5);
  offset := y * FrowSize + s;

  shiftOp := TMathUtils.Asr(uint32(Fbits[offset]), (x and $1F));

  result := (shiftOp and 1) <> 0;
end;

procedure TBitMatrix.SetBit(x, y: Integer; Value: Boolean);
var
  offset, s: Integer;
begin
  if (Value) then
  begin
    s := TMathUtils.Asr(x, 5);
    offset := y * FrowSize + s;
    Fbits[offset] := Fbits[offset] or (1 shl (x and $1F));
  end;
end;

procedure TBitMatrix.Clear;
var
  max, i: Integer;
begin
  max := Length(Fbits);
  i := 0;
  while ((i < max)) do
  begin
    Fbits[i] := 0;
    inc(i)
  end
end;

function TBitMatrix.Clone: TObject;
var
  b: TArray<Integer>;
begin
  b := TArray.Clone(Fbits);
  result := TBitMatrix.Create(Self.Fwidth, Self.Fheight, Self.FrowSize, b);
end;

constructor TBitMatrix.Create(width, height, rowSize: Integer;
  bits: TArray<Integer>);
begin
  if ((width < 1) or (height < 1)) then
    raise EArgumentException.Create('Both dimensions must be greater than 0');

  Self.Fwidth := width;
  Self.Fheight := height;
  Self.FrowSize := rowSize;
  Self.Fbits := bits
end;

constructor TBitMatrix.Create(width, height: Integer);
begin
  if ((width < 1) or (height < 1)) then
    raise EArgumentException.Create('Both dimensions must be greater than 0');

  Self.Fwidth := width;
  Self.Fheight := height;
  Self.FrowSize := TMathUtils.Asr((width + $1F), 5);
  SetLength(Self.Fbits, Self.FrowSize * height);
end;

constructor TBitMatrix.Create(dimension: Integer);
begin
  Self.Create(dimension, dimension);
end;

function TBitMatrix.Equals(obj: TObject): Boolean;
var
  other: TBitMatrix;
  i, l: Integer;
begin
  if (not(obj is TBitMatrix)) then
  begin
    result := false;
    exit
  end;

  other := (obj as TBitMatrix);
  if ((((Fwidth <> other.Fwidth) or (Fheight <> other.Fheight)) or
    (FrowSize <> other.FrowSize)) or (Length(Fbits) <> Length(other.Fbits)))
  then
  begin
    result := false;
    exit
  end;

  i := 0;
  l := Length(Fbits);

  while ((i < l)) do
  begin
    if (Fbits[i] <> other.Fbits[i]) then
    begin
      result := false;
      exit
    end;
    inc(i)
  end;

  result := true;
  exit

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
    result := nil;
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

  inc(x, bit);
  result := TArray<Integer>.Create(x, y);
  exit;

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
            inc(bit)
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
      inc(x32)
    end;
    inc(y)
  end;

  widthTmp := (right - left);
  heightTmp := (bottom - top);

  if ((widthTmp < 0) or (heightTmp < 0)) then
  begin
    result := nil;
    exit
  end;

  result := TArray<Integer>.Create(left, top, widthTmp, heightTmp);
  exit;

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

  result := hash;
  exit

end;

function TBitMatrix.getRow(y: Integer; row: TBitArray): TBitArray;
var
  offset, x: Integer;
begin

  if ((row = nil) or (row.Size < Self.Fwidth)) then
    row := TBitArray.Create(Self.Fwidth)
  else
    row.Clear;
  offset := (y * Self.FrowSize);
  x := 0;

  while ((x < Self.FrowSize)) do
  begin
    row.setBulk((x shl 5), Self.Fbits[(offset + x)]);
    inc(x)
  end;

  result := row;
  exit

end;

function TBitMatrix.getTopLeftOnBit: TArray<Integer>;
var
  bitsOffset, x, y, theBits, bit: Integer;
begin
  bitsOffset := 0;

  while (((bitsOffset < Length(Fbits)) and (Fbits[bitsOffset] = 0))) do
  begin
    inc(bitsOffset)
  end;

  if (bitsOffset = Length(Fbits)) then
  begin
    result := nil;
    exit
  end;

  y := (bitsOffset div Self.FrowSize);
  x := ((bitsOffset mod Self.FrowSize) shl 5);
  theBits := Self.Fbits[bitsOffset];
  bit := 0;

  while (((theBits shl ($1F - bit)) = 0)) do
  begin
    inc(bit)
  end;

  inc(x, bit);
  result := TArray<Integer>.Create(x, y);
  exit

end;

procedure TBitMatrix.Rotate180;
var
  i, width, height: Integer;
  topRow, bottomRow: TBitArray;

begin
  width := Self.Fwidth;
  height := Self.Fheight;
  topRow := TBitArray.Create(width);
  bottomRow := TBitArray.Create(width);
  i := 0;

  while ((i < ((height + 1) div 2))) do
  begin
    topRow := Self.getRow(i, topRow);
    bottomRow := Self.getRow(((height - 1) - i), bottomRow);
    topRow.reverse;
    bottomRow.reverse;
    Self.setRow(i, bottomRow);
    Self.setRow(((height - 1) - i), topRow);
    inc(i)
  end;

end;

procedure TBitMatrix.setRegion(left, top, width, height: Integer);
var
  x, y, offset, right, bottom: Integer;
begin
  if ((top < 0) or (left < 0)) then
    raise EArgumentException.Create('Left and top must be nonnegative');

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
      inc(x)
    end;
    inc(y)
  end

end;

procedure TBitMatrix.setRow(y: Integer; row: TBitArray);
begin
  Fbits := System.Copy(row.bits, (y * FrowSize), FrowSize);
end;

function TBitMatrix.ToBitmap(format: TBarcodeFormat; content: string): TBitmap;
begin
  raise ENotImplemented.Create('converting to bitmap');
end;

function TBitMatrix.ToBitmap: TBitmap;
begin
  result := ToBitmap(BarcodeFormat.CODE_128, '')
end;

end.
