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
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  VCL.Graphics,
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
    /// <summary>The words of row y (RowSize of them, bit 0 of the first
    /// one is x = 0), to read a row without copying it. Read only.
    /// </summary>
    function RowWords(y: Integer): PInteger; inline;
    function getTopLeftOnBit: TArray<Integer>;
    procedure setRegion(left: Integer; top: Integer; width: Integer;
      height: Integer);
    /// <summary>Sets the 8 bits x to x + 7 of row y to the lowest 8 bits of
    /// bits (bit 0 for x), much faster than 8 times Matrix[x, y]. Ignored
    /// when they do not lie completely inside the matrix.</summary>
    procedure setBits8(x, y: Integer; bits: Cardinal);
    /// <summary>A new matrix with the morphological closing of this one with
    /// a square of (2 * radius + 1) pixels, clipped at the borders: first
    /// grow the black areas (a pixel becomes black when any pixel of its
    /// square is black), then shrink them (a pixel stays black only when all
    /// pixels of its square inside the matrix are black). Merges dots that
    /// lie close together, like those of dot-peen codes. radius 1 to 31.
    /// </summary>
    function closed(radius: Integer): TBitMatrix;
    /// <summary>The smallest rectangle containing all black pixels (as in
    /// zxing-cpp); false when there are none or it is smaller than minSize
    /// in width or height.</summary>
    function findBoundingBox(out left, top, width, height: Integer;
      minSize: Integer = 1): Boolean;
    function ToBitmap: TBitmap; overload;
    function ToBitmap(format: TBarcodeFormat; content: string)
      : TBitmap; overload;

    property width: Integer read Fwidth;
    property height: Integer read Fheight;
    /// <summary>The number of words per row.</summary>
    property RowSize: Integer read FrowSize;
    property Matrix[x, y: Integer]: Boolean read getBit write setBit; default;
    // added for debugging
    function ToString: string; override;
  end;

implementation

uses
  System.Math;

{ TBitMatrix }

function TBitMatrix.getBit(x, y: Integer): Boolean;
begin
  // false outside the matrix (the unsigned compare also catches negative
  // x and y); this is the pixel accessor of the detectors, so it is inline
  if (Cardinal(x) < Cardinal(Fwidth)) and (Cardinal(y) < Cardinal(Fheight))
  then
    Result := ((Cardinal(Fbits[y * FrowSize + (x shr 5)]) shr (x and $1F))
      and 1) <> 0
  else
    Result := False;
end;

function TBitMatrix.RowWords(y: Integer): PInteger;
begin
  Result := @Fbits[y * FrowSize];
end;

procedure TBitMatrix.setBit(x, y: Integer; const value: Boolean);
var
  offset: NativeInt;
begin
  // like getBit: ignore positions outside the matrix instead of writing
  // into other memory
  if (x < 0) or (x >= Fwidth) or (y < 0) or (y >= Fheight) then
    exit;

  offset := y * FrowSize + (x shr 5);
  if (value) then
    Fbits[offset] := Fbits[offset] or (1 shl (x and $1F))
  else
    Fbits[offset] := Fbits[offset] and (not(1 shl (x and $1F)));
end;

procedure TBitMatrix.setBits8(x, y: Integer; bits: Cardinal);
begin
  if (x < 0) or (x + 8 > Fwidth) or (y < 0) or (y >= Fheight) then
    exit;

  bits := bits and $FF;
  var offset := y * FrowSize + (x shr 5);
  var shift := x and $1F;
  // the bits in the first word
  var mask: Cardinal := Cardinal($FF) shl shift;
  Fbits[offset] := Integer((Cardinal(Fbits[offset]) and not mask) or
    (bits shl shift));
  // the rest in the next word
  if (shift > 24) then
  begin
    var written := 32 - shift;
    mask := Cardinal($FF) shr written;
    Fbits[offset + 1] := Integer((Cardinal(Fbits[offset + 1]) and not mask) or
      (bits shr written));
  end;
end;

function TBitMatrix.closed(radius: Integer): TBitMatrix;
var
  n: Integer;
  padMask: Cardinal; // the bits of the last word of a row beyond the width
  src, line, shifted: TArray<Cardinal>;

  // shifted[i] := line shifted by k bits towards higher x (up) or lower x,
  // with fill for the bits coming from outside the row
  procedure shiftLine(k: Integer; up: Boolean; fill: Cardinal);
  begin
    for var i := 0 to n - 1 do
      if up then
      begin
        var lower := fill;
        if (i > 0) then
          lower := line[i - 1];
        shifted[i] := (line[i] shl k) or (lower shr (32 - k));
      end
      else
      begin
        var higher := fill;
        if (i < n - 1) then
          higher := line[i + 1];
        shifted[i] := (line[i] shr k) or (higher shl (32 - k));
      end;
  end;

  // one row (words at offset) grown (dilate) or shrunk horizontally
  procedure horizontal(offset: Integer; dilate: Boolean);
  begin
    for var i := 0 to n - 1 do
      line[i] := src[offset + i];
    var fill: Cardinal := 0;
    if dilate then
      // for growing, pixels outside the row count as white
      line[n - 1] := line[n - 1] and not padMask
    else
    begin
      // for shrinking, pixels outside the row count as black
      fill := $FFFFFFFF;
      line[n - 1] := line[n - 1] or padMask;
    end;
    var res := Copy(line);
    for var k := 1 to radius do
      for var up := false to true do
      begin
        shiftLine(k, up, fill);
        for var i := 0 to n - 1 do
          if dilate then
            res[i] := res[i] or shifted[i]
          else
            res[i] := res[i] and shifted[i];
      end;
    res[n - 1] := res[n - 1] and not padMask;
    for var i := 0 to n - 1 do
      src[offset + i] := res[i];
  end;

  // all rows grown (dilate) or shrunk vertically, clipped at the borders
  procedure vertical(dilate: Boolean);
  begin
    var res: TArray<Cardinal>;
    SetLength(res, System.Length(src));
    for var y := 0 to Fheight - 1 do
      for var i := 0 to n - 1 do
      begin
        var w := src[y * n + i];
        for var yy := Max(y - radius, 0) to Min(y + radius, Fheight - 1) do
          if dilate then
            w := w or src[yy * n + i]
          else
            w := w and src[yy * n + i];
        res[y * n + i] := w;
      end;
    src := res;
  end;

begin
  n := FrowSize;
  Result := TBitMatrix.Create(Fwidth, Fheight);
  if (n = 0) or (Fheight = 0) or (radius < 1) or (radius > 31) then
  begin
    for var i := 0 to High(Fbits) do
      Result.Fbits[i] := Fbits[i];
    exit;
  end;

  if ((Fwidth and $1F) = 0) then
    padMask := 0
  else
    padMask := not ((Cardinal(1) shl (Fwidth and $1F)) - 1);

  SetLength(src, System.Length(Fbits));
  for var i := 0 to High(Fbits) do
    src[i] := Cardinal(Fbits[i]);
  SetLength(line, n);
  SetLength(shifted, n);

  // grow: horizontally then vertically (a square is separable)
  for var y := 0 to Fheight - 1 do
    horizontal(y * n, true);
  vertical(true);
  // shrink
  for var y := 0 to Fheight - 1 do
    horizontal(y * n, false);
  vertical(false);

  for var i := 0 to High(src) do
    Result.Fbits[i] := Integer(src[i]);
end;

function TBitMatrix.findBoundingBox(out left, top, width, height: Integer;
  minSize: Integer): Boolean;
begin
  Result := false;
  var topLeft := getTopLeftOnBit;
  var bottomRight := getBottomRightOnBit;
  if (topLeft = nil) or (bottomRight = nil) then
    exit;
  left := topLeft[0];
  top := topLeft[1];
  var right := bottomRight[0];
  var bottom := bottomRight[1];
  if (bottom - top + 1 < minSize) then
    exit;

  for var y := top to bottom do
  begin
    for var x := 0 to left - 1 do
      if getBit(x, y) then
      begin
        left := x;
        break;
      end;
    for var x := Fwidth - 1 downto right + 1 do
      if getBit(x, y) then
      begin
        right := x;
        break;
      end;
  end;

  width := right - left + 1;
  height := bottom - top + 1;
  Result := (width >= minSize) and (height >= minSize);
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
  Self.FrowSize := (width + $1F) shr 5;
  // (SetLength clears the new array)
  SetLength(Self.Fbits, Self.FrowSize * height);
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
  offset: Integer;
begin
  // (the shift count masked: a shift by 32 or more is not 'mod 32' on
  // every CPU)
  offset := (y * FrowSize) + (x shr 5);
  Fbits[offset] := (Fbits[offset] xor (1 shl (x and $1F)))
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

procedure TBitMatrix.setRegion(left, top, width, height: Integer);
var
  y, offset, right, bottom, firstWord, lastWord, w: Integer;
  mask: Cardinal;
begin
  if ((top < 0) or (left < 0)) then
    raise EArgumentException.Create('Left and top must be non-negative');

  if ((height < 1) or (width < 1)) then
    raise EArgumentException.Create('Height and width must be at least 1');

  right := (left + width);
  bottom := (top + height);

  if ((bottom > Self.Fheight) or (right > Self.Fwidth)) then
    raise EArgumentException.Create('The region must fit inside the matrix');

  // per word: the bits of the region in it at once
  firstWord := left shr 5;
  lastWord := (right - 1) shr 5;
  for y := top to bottom - 1 do
  begin
    offset := (y * Self.FrowSize);
    for w := firstWord to lastWord do
    begin
      mask := $FFFFFFFF;
      if (w = firstWord) then
        mask := mask shl (left and $1F);
      if (w = lastWord) then
        mask := mask and (Cardinal($FFFFFFFF) shr (31 - ((right - 1) and $1F)));
      Fbits[offset + w] := Integer(Cardinal(Fbits[offset + w]) or mask);
    end;
  end;
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
