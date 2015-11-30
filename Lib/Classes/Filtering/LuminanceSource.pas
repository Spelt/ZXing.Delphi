unit LuminanceSource;
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
  System.SysUtils;

/// <summary>
/// The purpose of this class hierarchy is to abstract different bitmap implementations across
/// platforms into a standard interface for requesting greyscale luminance values. The interface
/// only provides immutable methods; therefore crop and rotation create copies. This is to ensure
/// that one Reader does not modify the original luminance source and leave it in an unknown state
/// for other Readers in the chain.
/// </summary>
/// <author>dswitkin@google.com (Daniel Switkin)</author>

Type

  TLuminanceSource = class
  protected
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor LuminanceSource(width: Integer; height: Integer); virtual;
    function Matrix: TArray<byte>; virtual; abstract;
    function GetRow(y: Integer; row: TArray<byte>): TArray<byte>;
      virtual; abstract;

    function CropSupported(): Boolean; virtual;
    function Crop(left: Integer; top: Integer; width: Integer; height: Integer)
      : TLuminanceSource; virtual;
    function RotateCounterClockwise(): TLuminanceSource; virtual;
    function RotateSupported: Boolean; virtual;
    function InversionSupported: Boolean; virtual;
    function rotateCounterClockwise45(): TLuminanceSource; virtual;
    function Invert(): TLuminanceSource; virtual;
    function ToString(): String; override;
    function Height(): Integer;
    function Width(): Integer;
  end;

implementation

constructor TLuminanceSource.LuminanceSource(width: Integer; height: Integer);
begin
  FWidth := width;
  FHeight := height;
end;

function TLuminanceSource.CropSupported: Boolean;
begin
  Result := False;
end;

function TLuminanceSource.Height: Integer;
begin
  Result:=FHeight;
end;

function TLuminanceSource.Width: Integer;
begin
  Result:=FWidth;
end;

// <returns> Whether this subclass supports counter-clockwise rotation.</returns>
function TLuminanceSource.RotateSupported: Boolean;
begin
  Result := False;
end;

/// <returns>Whether this subclass supports invertion.</returns>
function TLuminanceSource.InversionSupported: Boolean;
begin
  Result := False;
end;

/// <summary>
/// Returns a new object with cropped image data. Implementations may keep a reference to the
/// original data rather than a copy. Only callable if CropSupported is true.
/// </summary>
/// <param name="left">The left coordinate, which must be in [0, Width)</param>
/// <param name="top">The top coordinate, which must be in [0, Height)</param>
/// <param name="width">The width of the rectangle to crop.</param>
/// <param name="height">The height of the rectangle to crop.</param>
/// <returns> A cropped version of this object.</returns>
function TLuminanceSource.Crop(left: Integer; top: Integer; width: Integer;
  height: Integer): TLuminanceSource;
begin
  raise ENotImplemented.Create
    ('This luminance source does not support cropping.');
end;

/// <summary>
/// Returns a new object with rotated image data by 90 degrees counterclockwise.
/// Only callable if <see cref="RotateSupported"/> is true.
/// </summary>
/// <returns>A rotated version of this object.</returns>

function TLuminanceSource.RotateCounterClockwise(): TLuminanceSource;
begin
  raise ENotImplemented.Create
    ('This luminance source does not support rotation.');
end;

/// <summary>
/// Returns a new object with rotated image data by 45 degrees counterclockwise.
/// Only callable if <see cref="RotateSupported"/> is true.
/// </summary>
/// <returns>A rotated version of this object.</returns>
function TLuminanceSource.rotateCounterClockwise45(): TLuminanceSource;
begin
  raise ENotImplemented.Create
    ('This luminance source does not support rotation by 45 degrees.');
end;

function TLuminanceSource.Invert(): TLuminanceSource;
begin
  raise ENotImplemented.Create
    ('This luminance source does not support inversion.');
end;

function TLuminanceSource.ToString(): String;
var
  row: TArray<byte>;
  x, y, luminance: Integer;
  c: char;
  sResult: String;
begin

  SetLength(row, FWidth);

  for y := 0 to FHeight - 1 do
  begin
    row := GetRow(y, row);
    for x := 0 to FWidth - 1 do
    begin
      luminance := row[x] and $FF;
      if (luminance < $40) then
      begin
        c := '#';
      end
      else if (luminance < $80) then
      begin
        c := '+';
      end
      else if (luminance < $C0) then
      begin
        c := '.';
      end
      else
      begin
        c := ' ';
      end;
      sResult := sResult + c;
    end;
    sResult := sResult + #10;
  end;

  Result := sResult;
  row := nil;
  // ES: can become a be a big memory hog, so release it as quickly as possible.
end;

end.
