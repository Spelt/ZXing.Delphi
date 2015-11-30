unit RGBLuminanceSource;
{
  * Copyright 2009 ZXing authors
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

  * 2015-3 Adapted for Delphi/Object Pascal FireMonkey XE7 mobile by E.Spelt
}

interface
uses
  System.SysUtils, System.UITypes, FMX.Graphics,  LuminanceSource;

type

  TRGBLuminanceSource = class(TLuminanceSource)
  private
    luminances: TArray<Byte>;
    isRotated: boolean;

  protected

  public
    constructor RGBLuminanceSource(sourceBitmap: TBitmap; w: Integer;
      h: Integer);
    function GetRow(y: Integer; row: TArray<Byte>): TArray<Byte>; override;
    function Matrix: TArray<Byte>; override;
    function Crop(left: Integer; top: Integer; width: Integer; height: Integer)
      : TLuminanceSource; override;
    function RotateCounterClockwise(): TLuminanceSource; override;
    function RotateSupported: boolean; override;

  end;

implementation

constructor TRGBLuminanceSource.RGBLuminanceSource(sourceBitmap: TBitmap;
  w: Integer; h: Integer);

var
  x, y, offset: Integer;
  color: TAlphaColor;
  red, green, blue: Byte;
  currentData: TBitmapData;

begin
  inherited LuminanceSource(w, h);

  FWidth := w;
  FHeight := h;

  // In order to measure pure decoding speed, we convert the entire image to a greyscale array
  // up front, which is the same as the Y channel of the YUVLuminanceSource in the real app.
  SetLength(luminances, FWidth * FHeight);

  if (sourceBitmap.Map(TMapAccess.Read, currentData)) then
  begin
    try

      for y := 0 to FHeight - 1 do
      begin
        offset := y * FWidth;
        for x := 0 to FWidth - 1 do
        begin
          color := currentData.GetPixel(x, y);
          red := TAlphaColorRec(color).R;
          green := TAlphaColorRec(color).G;
          blue := TAlphaColorRec(color).B;
          luminances[offset + x] := red shl 16 or green shl 8 or blue;
        end;

      end;
    finally
      sourceBitmap.Unmap(currentData);
      //sourceBitmap.DisposeOf();
    end;
  end;

end;

function TRGBLuminanceSource.GetRow(y: Integer; row: TArray<Byte>)
  : TArray<Byte>;
var
  i, rowSize: Integer;
begin

  rowSize := Length(row);

  if (not isRotated) then
  begin

    if ((row = nil) or (rowSize < FWidth)) then
    begin
      SetLength(row, FWidth);
    end;

    for i := 0 to FWidth - 1 do
    begin
      row[i] := luminances[y * FWidth + i];
    end;

  end
  else
  begin

    if ((row = nil) or (rowSize < FHeight)) then
    begin
      SetLength(row, FHeight);
    end;

    for i := 0 to FHeight - 1 do
    begin
      row[i] := luminances[i * FWidth + y];
    end;

  end;

  result := row;
end;


function TRGBLuminanceSource.Matrix: TArray<Byte>;
begin
  result := luminances;
end;

function TRGBLuminanceSource.Crop(left: Integer; top: Integer; width: Integer;
  height: Integer): TLuminanceSource;
begin
  result := Inherited Crop(left, top, width, height);
end;

function TRGBLuminanceSource.RotateCounterClockwise(): TLuminanceSource;
begin
  isRotated := true;
  result := Self;
end;

function TRGBLuminanceSource.RotateSupported: boolean;
begin
  result := true;
end;

end.
