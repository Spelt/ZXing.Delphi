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

  * Original Author: dswitkin@google.com (Daniel Switkin)
  * Delphi Implementation by E.Spelt and K. Gossens
}

unit ZXing.LuminanceSource;

interface

uses
  System.SysUtils;

type
  /// <summary>
  /// The purpose of this class hierarchy is to abstract different bitmap implementations across
  /// platforms into a standard interface for requesting greyscale luminance values. The interface
  /// only provides immutable methods; therefore crop and rotation create copies. This is to ensure
  /// that one Reader does not modify the original luminance source and leave it in an unknown state
  /// for other Readers in the chain.
  /// </summary>
  TLuminanceSource = class
  protected
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(const width, height: Integer); virtual;
    /// <summary>
    /// Fetches luminance data for the underlying bitmap. Values should be fetched using:
    /// <code>int luminance = array[y * width + x] and 0xff</code>
    /// </summary>
    /// <returns>
    /// A row-major 2D array of luminance values. Do not use result.length as it may be
    /// larger than width * height bytes on some platforms. Do not modify the contents
    /// of the result.
    /// </returns>
	function Matrix: TArray<byte>; virtual; abstract;
    /// <summary>
    /// Fetches one row of luminance data from the underlying platform's bitmap. Values range from
    /// 0 (black) to 255 (white). Because Java does not have an unsigned byte type, callers will have
    /// to bitwise and with 0xff for each value. It is preferable for implementations of this method
    /// to only fetch this row rather than the whole image, since no 2D Readers may be installed and
    /// getMatrix() may never be called.
    /// </summary>
    /// <param name="y">The row to fetch, which must be in [0, bitmap height)</param>
    /// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
    /// Always use the returned object, and ignore the .length of the array.
    /// </param>
    /// <returns> An array containing the luminance data.</returns>    
	function getRow(const y: Integer; row: TArray<byte>): TArray<byte>;
      virtual; abstract;

    /// <summary> 
    /// Returns a new object with cropped image data. Implementations may keep a reference to the
    /// original data rather than a copy. Only callable if CropSupported is true.
    /// </summary>
    /// <param name="left">The left coordinate, which must be in [0, Width)</param>
    /// <param name="top">The top coordinate, which must be in [0, Height)</param>
    /// <param name="width">The width of the rectangle to crop.</param>
    /// <param name="height">The height of the rectangle to crop.</param>
    /// <returns> A cropped version of this object.</returns>
	function crop(const left, top, width,
      height: Integer): TLuminanceSource; virtual;
    function invert(): TLuminanceSource; virtual;
    function ToString(): String; override;

    /// <returns> Whether this subclass supports counter-clockwise rotation.</returns>
	function RotateSupported: Boolean; virtual;
    /// <returns> Whether this subclass supports cropping.</returns>
	function CropSupported(): Boolean; virtual;
    /// <returns>Whether this subclass supports invertion.</returns>
	function InversionSupported: Boolean; virtual;

    /// <summary>
    /// Returns a new object with rotated image data by 90 degrees counterclockwise.
    /// Only callable if <see cref="RotateSupported"/> is true.
    /// </summary>
    /// <returns>A rotated version of this object.</returns>
	function rotateCounterClockwise(): TLuminanceSource; virtual;
    /// <summary>
    /// Returns a new object with rotated image data by 45 degrees counterclockwise.
    /// Only callable if <see cref="RotateSupported"/> is true.
    /// </summary>
    /// <returns>A rotated version of this object.</returns>    
	function rotateCounterClockwise45(): TLuminanceSource; virtual;

    /// <returns> The height of the bitmap.</returns>
	property Height: Integer read FHeight;
    /// <returns> The width of the bitmap.</returns>
	property Width: Integer read FWidth;
  end;

implementation

constructor TLuminanceSource.Create(const width, height: Integer);
begin
  FWidth := width;
  FHeight := height;
end;

function TLuminanceSource.CropSupported: Boolean;
begin
  Result := False;
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
function TLuminanceSource.Crop(const left, top,
  width, height: Integer): TLuminanceSource;
begin
  raise ENotImplemented.Create
    ('This luminance source does not support cropping.');
end;

/// <summary>
/// Returns a new object with rotated image data by 90 degrees counterclockwise.
/// Only callable if <see cref="RotateSupported"/> is true.
/// </summary>
/// <returns>A rotated version of this object.</returns>
function TLuminanceSource.rotateCounterClockwise(): TLuminanceSource;
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

function TLuminanceSource.invert(): TLuminanceSource;
begin
  raise ENotImplemented.Create
    ('This luminance source does not support inversion.');
end;

function TLuminanceSource.ToString(): String;
var
  row: TArray<byte>;
  res: TStringBuilder;
  x, y,
  luminance: Integer;
  c: char;
begin
  row := TArray<Byte>.Create();
  SetLength(row, FWidth);

  res := TStringBuilder.Create(height * (width + 1));
  try
    for y := 0 to FHeight - 1 do
    begin
      row := GetRow(y, row);
      for x := 0 to Pred(FWidth) do
      begin
        luminance := row[x] and $FF;
        if (luminance < $40)
        then
           c := '#'
        else
           if (luminance < $80)
           then
              c := '+'
           else
              if (luminance < $C0)
              then
                 c := '.'
              else
                 c := ' ';

        res.Append(c);
      end;

      res.Append(#10);
    end;
  finally
    row := nil; // ES: can become a be a big memory hog, so release it as quickly as possible.
    Result := res.ToString;
    res.Free;
  end;

end;

end.
