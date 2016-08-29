{
  * Copyright 2007 ZXing authors
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
  * Delphi Implementation by K. Gossens
}

unit ZXing.BaseLuminanceSource;

interface
uses
  System.SysUtils,
  System.UITypes,
{$IFDEF USE_VCL_BITMAP}
  VCL.Graphics,
{$ELSE}
  FMX.Graphics,
{$ENDIF}
  ZXing.LuminanceSource,
  ZXing.InvertedLuminanceSource;

type
  /// <summary>
  /// The base class for luminance sources which supports 
  /// cropping and rotating based upon the luminance values.
  /// </summary>
  TBaseLuminanceSource = class abstract (TLuminanceSource)
  protected
    luminances: TArray<Byte>;

    const
      RChannelWeight : Integer = 19562;
      GChannelWeight : Integer = 38550;
      BChannelWeight : Integer = 7424;
      ChannelWeight  : Integer = 16;
	  
    function CreateLuminanceSource(const newLuminances: TArray<Byte>;
      const width, height: Integer): TLuminanceSource; virtual; abstract;
  public
    
    // added the "reintroduce" keyword to shut off the "method hides another method with the same name in the base class"
    constructor Create(const width, height: Integer);  reintroduce; overload; 
    constructor Create(const luminanceArray: TArray<Byte>; const width, height: Integer);  reintroduce; overload;

    function getRow(const y: Integer; row: TArray<Byte>): TArray<Byte>; override;
    function rotateCounterClockwise(): TLuminanceSource; override;
    function rotateCounterClockwise45(): TLuminanceSource; override;
    function crop(const left, top,
      width, height: Integer): TLuminanceSource; override;

    function Matrix: TArray<Byte>; override;
    function RotateSupported: Boolean; override;
    function InversionSupported: Boolean; override;
    function CropSupported: Boolean; override;
    function invert(): TLuminanceSource; override;
  end;

implementation

{ TBaseLuminanceSource }

constructor TBaseLuminanceSource.Create(const width, height: Integer);
begin
  inherited Create(width, height);
  
  luminances := TArray<Byte>.Create();
  SetLength(luminances, (width * height));
end;

/// <summary>
/// Initializes a new instance of the <see cref="BaseLuminanceSource"/> class.
/// </summary>
/// <param name="luminanceArray">The luminance array.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
constructor TBaseLuminanceSource.Create(const luminanceArray: TArray<Byte>; 
  const width, height: Integer);
begin
  Self.Create(width, height);
  Copy(luminanceArray, 0, Length(luminances));
end;

/// <summary>
/// Fetches one row of luminance data from the underlying platform's bitmap. Values range from
/// 0 (black) to 255 (white). It is preferable for implementations of this method
/// to only fetch this row rather than the whole image, since no 2D Readers may be installed and
/// getMatrix() may never be called.
/// </summary>
/// <param name="y">The row to fetch, 0 &lt;= y &lt; Height.</param>
/// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
/// Always use the returned object, and ignore the .length of the array.</param>
/// <returns>
/// An array containing the luminance data.
/// </returns>
function TBaseLuminanceSource.getRow(const y: Integer; 
  row: TArray<Byte>): TArray<Byte>;
var
  i, width: Integer;
begin
  width := Self.Width;
  if ((row = nil) or (Length(row) < width)) then
  begin
    row := nil;
    row := TArray<Byte>.Create();
    SetLength(row, width);
  end;

  for i := 0 to Pred(width) do
    row[i] := luminances[y * width + i];

  Result := row;  
end;

function TBaseLuminanceSource.Matrix: TArray<Byte>;
begin
  Result := luminances;
end;

/// <summary>
/// Returns a new object with rotated image data by 90 degrees counterclockwise.
/// Only callable if {@link #isRotateSupported()} is true.
/// </summary>
/// <returns>
/// A rotated version of this object.
/// </returns>
function TBaseLuminanceSource.rotateCounterClockwise(): TLuminanceSource;
var
  rotatedLuminances : TArray<Byte>;
  newWidth,
  newHeight : Integer;
  localLuminances : TArray<Byte>;
  yold, ynew,
  xold, xnew : Integer;
begin
  rotatedLuminances := TArray<Byte>.Create();
  SetLength(rotatedLuminances, (Width * Height));

  newWidth := Height;
  newHeight := Width;
  localLuminances := Matrix;
  for yold := 0 to Pred(Height) do
  begin
     for xold := 0 to Pred(Width) do
	 begin
     ynew := newHeight - xold - 1;
	   xnew := yold;
     rotatedLuminances[ynew * newWidth + xnew] := localLuminances[yold * Width + xold];
   end;
  end;
  Result := CreateLuminanceSource(rotatedLuminances, newWidth, newHeight);
end;

/// <summary>
/// TODO: not implemented yet
/// </summary>
/// <returns>
/// A rotated version of this object.
/// </returns>
function TBaseLuminanceSource.rotateCounterClockwise45(): TLuminanceSource;
begin
  // TODO: implement a good 45 degrees rotation without lost of information
  Result := inherited rotateCounterClockwise45();
end;

/// <summary>
/// </summary>
/// <returns> Whether this subclass supports counter-clockwise rotation.</returns>
function TBaseLuminanceSource.RotateSupported: Boolean;
begin
  Result := true;
end;

/// <summary>
/// Returns a new object with cropped image data. Implementations may keep a reference to the
/// original data rather than a copy. Only callable if CropSupported is true.
/// </summary>
/// <param name="left">The left coordinate, 0 &lt;= left &lt; Width.</param>
/// <param name="top">The top coordinate, 0 &lt;= top &lt;= Height.</param>
/// <param name="width">The width of the rectangle to crop.</param>
/// <param name="height">The height of the rectangle to crop.</param>
/// <returns>
/// A cropped version of this object.
/// </returns>
function TBaseLuminanceSource.Crop(const left, top,
  width, height: Integer): TLuminanceSource;
var
  croppedLuminances,
  oldLuminances : TArray<Byte>;
  oldWidth,
  oldRightBound,
  oldBottomBound : Integer;
  yold, ynew,
  xold, xnew : Integer;
begin
  if ((left + width > Self.Width) or (top + height > Self.Height))
  then
     raise EArgumentException.Create('Crop rectangle does not fit within image data.');

  croppedLuminances := TArray<Byte>.Create();
  SetLength(croppedLuminances, (width * height));
  oldLuminances := Self.Matrix;
  oldWidth := Self.Width;
  oldRightBound := left + width;
  oldBottomBound := top + height;
  ynew := 0;
  for yold := top to Pred(oldBottomBound) do
  begin
    xnew := 0;
    for xold := left to Pred(oldRightBound) do
      croppedLuminances[ynew * width + xnew] := oldLuminances[yold * oldWidth + xold];
    Inc(ynew);
  end;

  Result := CreateLuminanceSource(croppedLuminances, width, height);
end;

/// <summary>
/// </summary>
/// <returns> Whether this subclass supports cropping.</returns>
function TBaseLuminanceSource.CropSupported: Boolean;
begin
  Result := true;
end;

/// <summary>
/// </summary>
/// <returns>Whether this subclass supports invertion.</returns>
function TBaseLuminanceSource.InversionSupported: Boolean;
begin
  Result := true;
end;

 /// <summary>
/// Inverts the luminance values (newValue = 255 - oldValue)
/// </summary>
function TBaseLuminanceSource.invert(): TLuminanceSource;
begin
  Result := TInvertedLuminanceSource.Create(Self);
end;

end.
