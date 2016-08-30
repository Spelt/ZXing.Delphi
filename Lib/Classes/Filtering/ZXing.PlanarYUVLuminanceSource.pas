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
  * Delphi Implementation K. Gossens
}

unit ZXing.PlanarYUVLuminanceSource;

interface
uses
  System.SysUtils,
  System.UITypes,
  System.TypInfo,
  ZXing.LuminanceSource,
  ZXing.BaseLuminanceSource;

type 
  /// <summary>
  /// Luminance source class which support different formats of images.
  /// </summary>
  TPlanarYUVLuminanceSource = class(TBaseLuminanceSource)
  private
    yuvData : TArray<Byte>;
    dataWidth,
    dataHeight,
    left, top : Integer;

    const
       THUMBNAIL_SCALE_FACTOR : Integer = 2;

    procedure reverseHorizontal(const width, height: Integer);
  protected
    function CreateLuminanceSource(const newLuminances: TArray<Byte>;
      const width, height: Integer): TLuminanceSource; override;
  public
    constructor Create(const yuvData: TArray<Byte>;
      const dataWidth, dataHeight, left, top, width, height: Integer;
      const reverseHoriz: Boolean); overload;
    constructor Create(const luminances: TArray<Byte>;
      const width, height: Integer); overload;

    function getRow(const y: Integer; row: TArray<Byte>): TArray<Byte>; override;
    function crop(const left, top,
      width, height: Integer): TLuminanceSource; override;

    function Matrix: TArray<Byte>; override;
    function CropSupported: Boolean; override;

    function renderThumbnail(): TArray<Integer>;
    function ThumbnailWidth(): Integer;
    function ThumbnailHeight(): Integer;

  end;

implementation

/// <summary>
/// Initializes a new instance of the <see cref="PlanarYUVLuminanceSource"/> class.
/// </summary>
/// <param name="yuvData">The yuv data.</param>
/// <param name="dataWidth">Width of the data.</param>
/// <param name="dataHeight">Height of the data.</param>
/// <param name="left">The left.</param>
/// <param name="top">The top.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
/// <param name="reverseHoriz">if set to <c>true</c> [reverse horiz].</param>
constructor TPlanarYUVLuminanceSource.Create(const yuvData: TArray<Byte>; 
  const dataWidth, dataHeight, left, top, width, height: Integer; 
  const reverseHoriz: Boolean);
begin
  inherited Create(width, height);
  
  if ((left + width > dataWidth) or (top + height > dataHeight)) 
  then
     raise EArgumentException.Create('Crop rectangle does not fit within image data.');

  Self.yuvData := yuvData;
  Self.dataWidth := dataWidth;
  Self.dataHeight := dataHeight;
  Self.left := left;
  Self.top := top;
  if (reverseHoriz) 
  then
     reverseHorizontal(width, height);
end;

/// <summary>
/// Initializes a new instance of the <see cref="PlanarYUVLuminanceSource"/> class.
/// </summary>
/// <param name="luminances">The luminances.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
constructor TPlanarYUVLuminanceSource.Create(const luminances: TArray<Byte>; 
  const width, height: Integer);
begin
  inherited Create(width, height);

  Self.yuvData := luminances;
  Self.luminances := luminances;
  Self.dataWidth := width;
  Self.dataHeight := height;
  Self.left := 0;
  Self.top := 0;
end;

/// <summary>
/// Fetches one row of luminance data from the underlying platform's bitmap. Values range from
/// 0 (black) to 255 (white). Because Java does not have an unsigned byte type, callers will have
/// to bitwise and with 0xff for each value. It is preferable for implementations of this method
/// to only fetch this row rather than the whole image, since no 2D Readers may be installed and
/// getMatrix() may never be called.
/// </summary>
/// <param name="y">The row to fetch, 0 &lt;= y &lt; Height.</param>
/// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
/// Always use the returned object, and ignore the .length of the array.</param>
/// <returns>
/// An array containing the luminance data.
/// </returns>
 function TPlanarYUVLuminanceSource.getRow(const y: Integer;
   row: TArray<Byte>): TArray<Byte>;
 var
   width,
   offset: Integer;
 begin
  if ((y < 0) or (y >= Height)) 
  then
     raise EArgumentException.Create('Requested row is outside the image: ' + IntToStr(y));
  
  width := Self.Width;
  if ((row = nil) or (Length(row) < width)) then
  begin
    row := nil;
    row := TArray<Byte>.Create();
    SetLength(row, width);
  end;

  offset := (y + top) * dataWidth + left;
  row := Copy(yuvData, offset, width);
  
  Result := row;
end;

/// <summary>
///
/// </summary>
function TPlanarYUVLuminanceSource.Matrix: TArray<Byte>;
var
  width,
  height,
  area, y : Integer;
  inputOffset,
  outputOffset : Integer;
  matrix,
  yuv: TArray<Byte>;
begin
  width := Self.Width;
  height := Self.Height;

  // If the caller asks for the entire underlying image, save the copy and give them the
  // original data. The docs specifically warn that result.length must be ignored.
  if ((width = dataWidth) and (height = dataHeight)) then
  begin
    Result := Self.yuvData;
    exit;
  end;

  area := width * height;
  matrix := TArray<Byte>.Create();
  SetLength(matrix, area);
  inputOffset := (top * dataWidth) + left;

  // If the width matches the full width of the underlying data, perform a single copy.
  if (width = dataWidth) then
  begin
    matrix := Copy(yuvData, inputOffset, area);
    Result := matrix;
    exit;
  end;

  // Otherwise copy one cropped row at a time.
  yuv := yuvData;
  for y := 0 to Pred(height) do
  begin
    outputOffset := y * width;
    Move(yuv[inputOffSet], matrix[outputOffset], width);
    Inc(inputOffset, dataWidth);
  end;

  Result := matrix;
end;

/// <summary>
/// </summary>
/// <returns> Whether this subclass supports cropping.</returns>
function TPlanarYUVLuminanceSource.CropSupported: Boolean;
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
function TPlanarYUVLuminanceSource.crop(const left, top,
  width, height: Integer): TLuminanceSource;
begin
  Result := TPlanarYUVLuminanceSource.Create(yuvData,
                                             dataWidth,
                                             dataHeight,
                                             Self.left + left,
                                             Self.top + top,
                                             width,
                                             height,
                                             false);
end;

/// <summary>
/// Renders the cropped greyscale bitmap.
/// </summary>
/// <returns></returns>
function TPlanarYUVLuminanceSource.renderThumbnail(): TArray<Integer>;
var
  width,
  height : Integer;
  pixels : TArray<Integer>;
  yuv : TArray<Byte>;
  x, y,
  inputOffset,
  outputOffset : Integer;
  grey : Byte;
begin
  width := Self.Width div THUMBNAIL_SCALE_FACTOR;
  height := Self.Height div THUMBNAIL_SCALE_FACTOR;
  pixels := TArray<Integer>.Create();
  SetLength(pixels, (width * height));

  yuv := yuvData;
  inputOffset := (top * dataWidth) + left;

  for y := 0 to Pred(height) do
  begin
    outputOffset := y * width;
    for x := 0 to Pred(width) do
    begin
      grey := yuv[inputOffset + x * THUMBNAIL_SCALE_FACTOR] and $ff;
      pixels[outputOffset + x] := (($00FF0000 shl 8) or (grey * $00010101));
    end;
    Inc(inputOffset, (dataWidth * THUMBNAIL_SCALE_FACTOR));
  end;
  Result := pixels;
end;

/// <summary>
/// width of image from {@link #renderThumbnail()}
/// </summary>
function TPlanarYUVLuminanceSource.ThumbnailWidth(): Integer;
begin
  Result := (Self.Width div THUMBNAIL_SCALE_FACTOR);
end;

/// <summary>
/// height of image from {@link #renderThumbnail()}
/// </summary>
function TPlanarYUVLuminanceSource.ThumbnailHeight(): Integer;
begin
  Result := (Self.Height div THUMBNAIL_SCALE_FACTOR);
end;

procedure TPlanarYUVLuminanceSource.reverseHorizontal(const width, height: Integer);
var
  yuvData : TArray<Byte>;
  y,
  x1, x2,
  middle,
  rowStart : Integer;
  temp : Byte;
begin
  yuvData := Self.yuvData;
  y := 0;
  rowStart := ((self.top * self.dataWidth) + self.left);
  while ((y < height)) do
  begin
    middle := (rowStart + (width div 2));
    x1 := rowStart;
    x2 := ((rowStart + width) - 1);
    while ((x1 < middle)) do
    begin
      temp := yuvData[x1];
      yuvData[x1] := yuvData[x2];
      yuvData[x2] := temp;
      Inc(x1);
      Dec(x2)
    end;
    Inc(y);
    Inc(rowStart, self.dataWidth)
  end;
end;

function TPlanarYUVLuminanceSource.CreateLuminanceSource(
  const newLuminances: TArray<Byte>;
  const width, height: Integer): TLuminanceSource;
begin
  Result := TPlanarYUVLuminanceSource.Create(newLuminances, width, height);
end;

end.
