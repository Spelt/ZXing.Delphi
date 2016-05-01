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
  * Restructured by K. Gossens
}

unit ZXing.Common.Detector.WhiteRectangleDetector;

interface

uses
  System.SysUtils,
  ZXing.Common.BitMatrix,
  ZXing.Common.Detector.MathUtils,
  ZXing.ResultPoint;

type
  TWhiteRectangleDetector = class sealed
  private 
    const
      CORR: Integer = 1;
      INIT_SIZE: Integer = 10;
    var
      downInit: Integer;
      height: Integer;
	    image: TBitMatrix;
      leftInit: Integer;
      rightInit: Integer;
      upInit: Integer;
      width: Integer;
    constructor Create(image: TBitMatrix); overload;
    constructor Create(image: TBitMatrix; initSize: Integer;
      x: Integer; y: Integer); overload;
    function centerEdges(y: TResultPoint; z: TResultPoint;
      x: TResultPoint; t: TResultPoint): TArray<TResultPoint>;
    function containsBlackPoint(a: Integer; b: Integer; fixed: Integer;
      horizontal: boolean): boolean;
    function getBlackPointOnSegment(aX: Single; aY: Single;
      bX: Single; bY: Single): TResultPoint;
  public
    class function New(image: TBitMatrix): TWhiteRectangleDetector; overload;
    class function New(image: TBitMatrix; initSize: Integer;
      x: Integer; y: Integer): TWhiteRectangleDetector; overload; static;
    function detect(): TArray<TResultPoint>;
  end;


implementation

{ TWhiteRectangleDetector }

constructor TWhiteRectangleDetector.Create(image: TBitMatrix);
begin
  Self.Create(image, 10, image.width div 2, image.height div 2);
end;

constructor TWhiteRectangleDetector.Create(image: TBitMatrix;
  initSize: Integer; x: Integer; y: Integer);
var
  halfsize: Integer;
begin
  Self.image := image;
  Self.height := image.Height;
  Self.width := image.Width;
  halfsize := (initSize div 2);
  Self.leftInit := (x - halfsize);
  Self.rightInit := (x + halfsize);
  Self.upInit := (y - halfsize);
  Self.downInit := (y + halfsize);
end;

function TWhiteRectangleDetector.centerEdges(y: TResultPoint; z: TResultPoint;
  x: TResultPoint; t: TResultPoint): TArray<TResultPoint>;
var
  yi, yj,
  zi, zj,
  xi, xj,
  ti, tj  : Single;
begin
  yi := y.X;
  yj := y.Y;
  zi := z.X;
  zj := z.Y;
  xi := x.X;
  xj := x.Y;
  ti := t.X;
  tj := t.Y;
  if (yi < (Self.Width div 2)) then
  begin
    Result := TArray<TResultPoint>.Create(
                 TResultPoint.Create((ti - 1), (tj + 1)),
                 TResultPoint.Create((zi + 1), (zj + 1)),
                 TResultPoint.Create((xi - 1), (xj - 1)),
                 TResultPoint.Create((yi + 1), (yj - 1))
                );
    exit;
  end;
  Result := TArray<TResultPoint>.Create(
              TResultPoint.Create((ti + 1), (tj + 1)),
              TResultPoint.Create((zi + 1), (zj - 1)),
              TResultPoint.Create((xi - 1), (xj + 1)),
              TResultPoint.Create((yi - 1), (yj - 1))
              );
  exit;
end;

function TWhiteRectangleDetector.containsBlackPoint(a: Integer; b: Integer;
  fixed: Integer; horizontal: boolean): boolean;
var
  x, y : Integer;
begin
  x := 0;
  y := 0;
  if (horizontal) then
  begin
    x := a;

    while ((x <= b)) do
    begin
      if (self.image[x, fixed]) then
      begin
        Result := true;
        exit;
      end;
      Inc(x)
    end;
  end else
  begin
    y := a;

    while ((y <= b)) do
    begin
      if (self.image[fixed, y]) then
      begin
        Result := true;
        exit;
      end;
      Inc(y);
    end;
  end;

  Result := false;
end;

class function TWhiteRectangleDetector.New(
  image: TBitMatrix): TWhiteRectangleDetector;
var
  instance : TWhiteRectangleDetector;
begin
  Result := nil;
  if (image <> nil) then
  begin
    instance := TWhiteRectangleDetector.Create(image);
    if (((instance.upInit >= 0) and (instance.leftInit >= 0)) and
        ((instance.downInit < instance.height) and
         (instance.rightInit < instance.width)))
    then
       Result := instance
    else
       instance.Free;
  end;
end;

class function TWhiteRectangleDetector.New(image: TBitMatrix;
  initSize: Integer; x: Integer; y: Integer): TWhiteRectangleDetector;
var
  instance : TWhiteRectangleDetector;
begin
  instance := TWhiteRectangleDetector.Create(image, initSize, x, y);
  if (((instance.upInit >= 0) and (instance.leftInit >= 0)) and
      ((instance.downInit < instance.height) and (instance.rightInit < instance.width)))
  then
     Result := instance
  else
  begin
    instance.Free;
    Result := nil;
  end;
end;

function TWhiteRectangleDetector.detect: TArray<TResultPoint>;
var
  left,
  right,
  up,
  down : Integer;
  sizeExceeded,
  aBlackPointFoundOnBorder,
  atLeastOneBlackPointFoundOnBorder,
  atLeastOneBlackPointFoundOnRight,
  atLeastOneBlackPointFoundOnBottom,
  atLeastOneBlackPointFoundOnLeft,
  atLeastOneBlackPointFoundOnTop,
  rightBorderNotWhite,
  bottomBorderNotWhite,
  leftBorderNotWhite,
  topBorderNotWhite : Boolean;
  i : Integer;
  z, t,
  x, y  : TResultPoint;
  maxSize : Integer;
begin
  left := self.leftInit;
  right := self.rightInit;
  up := self.upInit;
  down := self.downInit;
  sizeExceeded := false;
  aBlackPointFoundOnBorder := true;
  atLeastOneBlackPointFoundOnBorder := false;
  atLeastOneBlackPointFoundOnRight := false;
  atLeastOneBlackPointFoundOnBottom := false;
  atLeastOneBlackPointFoundOnLeft := false;
  atLeastOneBlackPointFoundOnTop := false;
  while (aBlackPointFoundOnBorder) do
  begin
    aBlackPointFoundOnBorder := false;
    rightBorderNotWhite := true;
    while ((rightBorderNotWhite or (not atLeastOneBlackPointFoundOnRight)) and
           (right < self.width)) do
    begin
      rightBorderNotWhite := self.containsBlackPoint(up, down, right, false);
      if (rightBorderNotWhite) then
      begin
        Inc(right);
        aBlackPointFoundOnBorder := true;
        atLeastOneBlackPointFoundOnRight := true;
      end else
             if (not atLeastOneBlackPointFoundOnRight)
             then
                Inc(right);
    end;

    if (right >= self.width) then
    begin
      sizeExceeded := true;
      break;
    end;

    bottomBorderNotWhite := true;
    while ((bottomBorderNotWhite or (not atLeastOneBlackPointFoundOnBottom)) and (down < self.height)) do
    begin
      bottomBorderNotWhite := self.containsBlackPoint(left, right, down, true);
      if (bottomBorderNotWhite) then
      begin
        Inc(down);
        aBlackPointFoundOnBorder := true;
        atLeastOneBlackPointFoundOnBottom := true;
      end else
             if (not atLeastOneBlackPointFoundOnBottom)
             then
                Inc(down);
    end;

    if (down >= self.height) then
    begin
      sizeExceeded := true;
      break;
    end;

    leftBorderNotWhite := true;
    while ((leftBorderNotWhite or (not atLeastOneBlackPointFoundOnLeft)) and
           (left >= 0)) do
    begin
      leftBorderNotWhite := self.containsBlackPoint(up, down, left, false);
      if (leftBorderNotWhite) then
      begin
        Dec(left);
        aBlackPointFoundOnBorder := true;
        atLeastOneBlackPointFoundOnLeft := true;
      end else
             if (not atLeastOneBlackPointFoundOnLeft)
             then
                Dec(left);
    end;

    if (left < 0) then
    begin
      sizeExceeded := true;
      break;
    end;

    topBorderNotWhite := true;
    while ((topBorderNotWhite or (not atLeastOneBlackPointFoundOnTop)) and
           (up >= 0)) do
    begin
      topBorderNotWhite := self.containsBlackPoint(left, right, up, true);
      if (topBorderNotWhite) then
      begin
        Dec(up);
        aBlackPointFoundOnBorder := true;
        atLeastOneBlackPointFoundOnTop := true;
      end else
             if (not atLeastOneBlackPointFoundOnTop)
             then
                Dec(up);
    end;

    if (up < 0) then
    begin
      sizeExceeded := true;
      break;
    end;

    if (aBlackPointFoundOnBorder)
    then
       atLeastOneBlackPointFoundOnBorder := true;
  end;

  if (not (not sizeExceeded and atLeastOneBlackPointFoundOnBorder)) then
  begin
    Result := nil;
    exit;
  end;

  maxSize := (right - left);
  z := nil;
  i := 1;
  while ((i < maxSize)) do
  begin
    z := self.getBlackPointOnSegment(left, (down - i), (left + i), down);
    if (z <> nil)
    then
       break;
    Inc(i);
  end;

  if (z = nil) then
  begin
    Result := nil;
    exit;
  end;

  t := nil;
  i := 1;
  while ((i < maxSize)) do
  begin
    t := self.getBlackPointOnSegment(left, (up + i), (left + i), up);
    if (t <> nil)
    then
       break;
    Inc(i);
  end;

  if (t = nil) then
  begin
    Result := nil;
    z.Free;
    exit;
  end;

  x := nil;
  i := 1;
  while ((i < maxSize)) do
  begin
    x := self.getBlackPointOnSegment(right, (up + i), (right - i), up);
    if (x <> nil)
    then
       break;
    Inc(i);
   end;

  if (x = nil) then
  begin
    Result := nil;
    z.Free;
    t.Free;
    exit;
  end;

  y := nil;
  i := 1;
  while ((i < maxSize)) do
  begin
    y := self.getBlackPointOnSegment(right, (down - i), (right - i), down);
    if (y <> nil)
    then
       break;
    Inc(i);
  end;

  if (y = nil) then
  begin
    Result := nil;
    z.Free;
    t.Free;
    x.Free;
    exit;
  end;

  Result := self.centerEdges(y, z, x, t);
end;

function TWhiteRectangleDetector.getBlackPointOnSegment(aX: Single; aY: Single;
  bX: Single; bY: Single): TResultPoint;
var
  i,
  x, y,
  dist   : Integer;
  xStep,
  yStep  : Single;
begin
  dist  := TMathUtils.round(TMathUtils.distance(aX, aY, bX, bY));
  xStep := ((bX - aX) / dist);
  yStep := ((bY - aY) / dist);
  i := 0;
  while ((i < dist)) do
  begin
    x := TMathUtils.round((aX + (i * xStep)));
    y := TMathUtils.round((aY + (i * yStep)));
    if (self.image[x, y]) then
    begin
      Result := TResultPoint.Create(x, y);
      exit;
    end;
    Inc(i);
  end;
  Result := nil;
end;

end.