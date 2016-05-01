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

unit ZXing.Common.Detector.MonochromeRectangleDetector;

interface

uses 
  System.SysUtils,
  BitMatrix,
  ZXing.ResultPoint;

type
  TMonochromeRectangleDetector = class sealed
  public
    constructor Create(image: TBitMatrix);
    function detect: TArray<TResultPoint>;
  private 
    const MAX_MODULES: Integer = $20;
    image: TBitMatrix;
	
    function blackWhiteRange(fixedDimension: Integer; maxWhiteRun: Integer; minDim: Integer; maxDim: Integer; horizontal: boolean): TArray<Integer>; 
    class function findCornerFromCenter(centerX: Integer; deltaX: Integer; left: Integer; right: Integer; centerY: Integer; deltaY: Integer; top: Integer; bottom: Integer; maxWhiteRun: Integer): TResultPoint; 
  end;


implementation

{ TMonochromeRectangleDetector }

constructor TMonochromeRectangleDetector.Create(image: TBitMatrix);
begin
  inherited;
  
  Self.image := image;
end;

function TMonochromeRectangleDetector.blackWhiteRange(fixedDimension: Integer; 
  maxWhiteRun: Integer; minDim: Integer; maxDim: Integer; 
  horizontal: boolean): TArray<Integer>;
begin
  center := ((minDim + maxDim) shr 1);
  start := center;
  while ((start >= minDim)) do
  begin
    if ( {pseudo} (if horizontal then self.image.Item[start, fixedDimension] else self.image.Item[fixedDimension, start])) 
	then
       dec(start)
    else
    begin
      whiteRunStart := start;
      repeat
        dec(start);
      until ((start < minDim) or  {pseudo} (if horizontal then self.image.Item[start, fixedDimension] else self.image.Item[fixedDimension, start]));
      whiteRunSize := (whiteRunStart - start);
      if ((start < minDim) or 
	      (whiteRunSize > maxWhiteRun)) then
      begin
        start := whiteRunStart;
        break;
	  end;
    end;
  end;
  inc(start);
  end := center;
  while ((end < maxDim)) do
  begin
    if ( {pseudo} (if horizontal then self.image.Item[end, fixedDimension] else self.image.Item[fixedDimension, end])) 
	then
       inc(end)
    else
    begin
      whiteRunStart := end;
      repeat
        inc(end);
      until ((end >= maxDim) or  {pseudo} (if horizontal then self.image.Item[end, fixedDimension] else self.image.Item[fixedDimension, end]));
      whiteRunSize := (end - whiteRunStart);
      if ((end >= maxDim) or 
	      (whiteRunSize > maxWhiteRun)) then
      begin
        end := whiteRunStart;
        break;
      end;
    end
  end;
  dec(end);
  if (end <= start) then
  begin
    Result := nil;
    exit;
  end;
  
  Result := TArray<Integer>.Create(start, end);
end;

function TMonochromeRectangleDetector.detect: TArray<TResultPoint>;
begin
  height := self.image.Height;
  width := self.image.Width;
  halfHeight := (height shr 1);
  halfWidth := (width shr 1);
  deltaY := Math.Max(1, (height div $100));
  deltaX := Math.Max(1, (width div $100));
  top := 0;
  bottom := height;
  left := 0;
  right := width;
  pointA := self.findCornerFromCenter(halfWidth, 0, left, right, halfHeight, -deltaY, top, bottom, (halfWidth shr 1));
  if (pointA = nil) then
  begin
    Result := nil;
    exit;
  end;
  top := ((pointA.Y as Integer) - 1);
  pointB := self.findCornerFromCenter(halfWidth, -deltaX, left, right, halfHeight, 0, top, bottom, (halfHeight shr 1));
  if (pointB = nil) then
  begin
    Result := nil;
    exit;
  end;
  left := ((pointB.X as Integer) - 1);
  pointC := self.findCornerFromCenter(halfWidth, deltaX, left, right, halfHeight, 0, top, bottom, (halfHeight shr 1));
  if (pointC = nil) then
  begin
    Result := nil;
    exit;
  end;
  right := ((pointC.X as Integer) + 1);
  pointD := self.findCornerFromCenter(halfWidth, 0, left, right, halfHeight, deltaY, top, bottom, (halfWidth shr 1));
  if (pointD = nil) then
  begin
    Result := nil;
    exit;
  end;
  bottom := ((pointD.Y as Integer) + 1);
  pointA := self.findCornerFromCenter(halfWidth, 0, left, right, halfHeight, -deltaY, top, bottom, (halfWidth shr 2));
  if (pointA = nil) then
  begin
    Result := nil;
    exit;
  end;
  
  Result := TArray<TResultPoint>.Create(pointA, pointB, pointC, pointD);
end;

function TMonochromeRectangleDetector.findCornerFromCenter(centerX: Integer; deltaX: Integer; left: Integer; right: Integer; centerY: Integer; deltaY: Integer; top: Integer; bottom: Integer; maxWhiteRun: Integer): TResultPoint;
var
  range: TArray<Integer>;
begin
  lastRange := nil;
  y := centerY;
  x := centerX;
  while ((((y < bottom) and (y >= top)) and 
          ((x < right) and (x >= left)))) do
  begin
    if (deltaX = 0) 
	then
       range := self.blackWhiteRange(y, maxWhiteRun, left, right, true)
    else
       range := self.blackWhiteRange(x, maxWhiteRun, top, bottom, false);
    if (range = nil) then
    begin
      if (lastRange = nil) then
      begin
        Result := nil;
        exit;
      end;
      if (deltaX = 0) then
      begin
        lastY := (y - deltaY);
        if (lastRange[0] >= centerX) then
        begin
          Result := TResultPoint.Create((lastRange[1] as Single), (lastY as Single));
          exit;
        end;
        if (lastRange[1] > centerX) then
        begin
          Result := TResultPoint.Create( {pseudo} (if (deltaY > 0) then (lastRange[0] as Single) else (lastRange[1] as Single)), (lastY as Single));
          exit;
        end;
        
		Result := TResultPoint.Create((lastRange[0] as Single), (lastY as Single));
        exit;
      end;
      lastX := (x - deltaX);
      if (lastRange[0] >= centerY) then
      begin
        Result := TResultPoint.Create((lastX as Single), (lastRange[1] as Single));
        exit;
      end;
      if (lastRange[1] > centerY) then
      begin
        Result := TResultPoint.Create((lastX as Single),  {pseudo} (if (deltaX < 0) then (lastRange[0] as Single) else (lastRange[1] as Single)));
        exit;
      end;
      Result := TResultPoint.Create((lastX as Single), (lastRange[0] as Single));
    end;
    lastRange := range;
    inc(y, deltaY);
    inc(x, deltaX)
  end;
  Result := nil;
end;

end.