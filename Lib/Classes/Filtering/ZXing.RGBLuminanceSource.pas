{
  * Copyright 2012 ZXing authors
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

unit ZXing.RGBLuminanceSource;

interface
uses
  System.SysUtils,
  System.UITypes,
  System.TypInfo,
{$IFDEF USE_VCL_BITMAP}
  Winapi.Windows,
  VCL.Graphics,
{$ELSE}
  FMX.Graphics,
{$ENDIF}
  ZXing.LuminanceSource,
  ZXing.BaseLuminanceSource,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary>
  /// enumeration of supported bitmap format which the RGBLuminanceSource can process
  /// </summary>
  TBitmapFormat = (
    /// <summary>
    /// format of the byte[] isn't known. RGBLuminanceSource tries to determine the best possible value
    /// </summary>
    Unknown,
    /// <summary>
    /// grayscale array, the byte array is a luminance array with 1 byte per pixel
    /// </summary>
    Gray8,
    /// <summary>
    /// 3 bytes per pixel with the channels red, green and blue
    /// </summary>
    RGB24,
    /// <summary>
    /// 4 bytes per pixel with the channels red, green and blue
    /// </summary>
    RGB32,
    /// <summary>
    /// 4 bytes per pixel with the channels alpha, red, green and blue
    /// </summary>
    ARGB32,
    /// <summary>
    /// 3 bytes per pixel with the channels blue, green and red
    /// </summary>
    BGR24,
    /// <summary>
    /// 4 bytes per pixel with the channels blue, green and red
    /// </summary>
    BGR32,
    /// <summary>
    /// 4 bytes per pixel with the channels blue, green, red and alpha
    /// </summary>
    BGRA32,
    /// <summary>
    /// 2 bytes per pixel, 5 bit red, 6 bits green and 5 bits blue
    /// </summary>
    RGB565,
    /// <summary>
    /// 4 bytes per pixel with the channels red, green, blue and alpha
    /// </summary>
    RGBA32); 

  /// <summary>
  /// Luminance source class which support different formats of images.
  /// </summary>
  TRGBLuminanceSource = class(TBaseLuminanceSource)
  private
    function DetermineBitmapFormat(const rgbRawBytes: TArray<Byte>;
      const width, height: Integer): TBitmapFormat;

    procedure CalculateLuminanceRGB24(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceRGB32(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceRGBA32(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceRGB565(const rgb565RawData: TArray<Byte>);

    procedure CalculateLuminanceBGR24(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceBGR32(const rgbRawBytes: TArray<Byte>);
    procedure CalculateLuminanceBGRA32(const rgbRawBytes: TArray<Byte>);

    procedure CalculateLuminanceARGB32(const rgbRawBytes: TArray<Byte>);

    function GetBitmapFormat: TBitmapFormat;
  protected
    function CreateLuminanceSource(const newLuminances: TArray<Byte>;
      const width, height: Integer): TLuminanceSource; override;
    procedure CalculateLuminance(const rgbRawBytes: TArray<Byte>;
      bitmapFormat: TBitmapFormat);
  public
    constructor Create(const width, height: Integer); overload;
    constructor Create(const rgbRawBytes: TArray<Byte>;
      const width, height: Integer); overload;
    constructor Create(const rgbRawBytes: TArray<Byte>;
      const width, height: Integer; const bitmapFormat: TBitmapFormat); overload;
    constructor CreateFromBitmap(const sourceBitmap: TBitmap;
      const width, height: Integer);

    property BitmapFormat : TBitmapFormat read GetBitmapFormat;
  end;

implementation
uses ZXing.Helpers;

/// <summary>
/// Initializes a new instance of the <see cref="RGBLuminanceSource"/> class.
/// </summary>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
constructor TRGBLuminanceSource.Create(const width, height: Integer);
begin
  inherited Create(width, height);
end;

/// <summary>
/// Initializes a new instance of the <see cref="RGBLuminanceSource"/> class.
/// It supports a byte array with 3 bytes per pixel (RGB24).
/// </summary>
/// <param name="rgbRawBytes">The RGB raw bytes.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
constructor TRGBLuminanceSource.Create(const rgbRawBytes: TArray<Byte>;
  const width, height: Integer);
begin
  Create(rgbRawBytes, width, height, TBitmapFormat.RGB24);
end;

/// <summary>
/// Initializes a new instance of the <see cref="RGBLuminanceSource"/> class.
/// It supports a byte array with 3 bytes per pixel (RGB24).
/// </summary>
/// <param name="rgbRawBytes">The RGB raw bytes.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
/// <param name="bitmapFormat">The bitmap format.</param>
constructor TRGBLuminanceSource.Create(const rgbRawBytes: TArray<Byte>;
  const width, height: Integer; const bitmapFormat: TBitmapFormat);
begin
  inherited Create(width, height);
  CalculateLuminance(rgbRawBytes, bitmapFormat);
end;


{$IFDEF USE_VCL_BITMAP}
// VCL TBitmap implementation
constructor TRGBLuminanceSource.CreateFromBitmap(const sourceBitmap: TBitmap; const width, height: Integer);
type
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray; // Use a PByteArray for pf8bit color.

var
  x, y,
  offset : Integer;
  r, g, b : Byte;
  P: pRGBTripleArray;

begin
  Self.Create(width, height);
  sourceBitmap.PixelFormat := pf24bit;
  for y := 0 to sourceBitmap.Height - 1 do
  begin
    offset := y * FWidth;
    P := sourceBitmap.ScanLine[y];
    for x := 0 to sourceBitmap.Width - 1 do
    begin
       r := P[x].rgbtRed;
       g := P[x].rgbtGreen;
       b := P[x].rgbtBlue;
       luminances[offset + x] := TMathUtils.Asr(3482*r + 11721*g + 1181*b, 14);
    end;
  end;
end;
{$ELSE}
// FMX TBitmap implementation
constructor TRGBLuminanceSource.CreateFromBitmap(const sourceBitmap: TBitmap;
   const width, height: Integer);
var
  x, y,
  offset : Integer;
  color : TAlphaColor;
  r, g, b : Byte;
  currentData: TBitmapData;
begin
  Self.Create(width, height);

  // In order to measure pure decoding speed, we convert the entire image to a greyscale array
  // up front, which is the same as the Y channel of the YUVLuminanceSource in the real app.

  if (sourceBitmap.Map(TMapAccess.Read, currentData)) then
  begin
    try
      for y := 0 to FHeight - 1 do
      begin
        offset := y * FWidth;
        for x := 0 to FWidth - 1 do
        begin
          color := currentData.GetPixel(x, y);
          r := TAlphaColorRec(color).R;
          g := TAlphaColorRec(color).G;
          b := TAlphaColorRec(color).B;
          luminances[offset + x] := TMathUtils.Asr(3482*r + 11721*g + 1181*b, 14);
        end;
      end;
    finally
      sourceBitmap.Unmap(currentData);
      //sourceBitmap.DisposeOf();
    end;
  end;
end;
{$ENDIF}

/// <summary>
/// Should create a new luminance source with the right class type.
/// The method is used in methods crop and rotate.
/// </summary>
/// <param name="newLuminances">The new luminances.</param>
/// <param name="width">The width.</param>
/// <param name="height">The height.</param>
/// <returns></returns>
function TRGBLuminanceSource.CreateLuminanceSource(
  const newLuminances: TArray<Byte>;
  const width, height: Integer): TLuminanceSource;
begin
  Result := TRGBLuminanceSource.Create(width, height);
  luminances := newLuminances;
end;

function TRGBLuminanceSource.DetermineBitmapFormat(
  const rgbRawBytes: TArray<Byte>;
  const width, height: Integer): TBitmapFormat;
var
  square,
  byteperpixel : Integer;
begin
  square := (width * height);
  byteperpixel := (Length(rgbRawBytes) div square);

  case byteperpixel of
    1: Result := TBitmapFormat.Gray8;
    2: Result := TBitmapFormat.RGB565;
    3: Result := TBitmapFormat.RGB24;
    4: Result := TBitmapFormat.RGB32;
    else
       raise EArgumentException.Create('The bitmap format could not be determined. Please specify the correct value.');
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminance(const rgbRawBytes: TArray<Byte>;
  bitmapFormat: TBitmapFormat);
var
  len: Integer;
begin
  if (bitmapFormat = TBitmapFormat.Unknown)
  then
     bitmapFormat := DetermineBitmapFormat(rgbRawBytes, self.Width, self.Height);

  case bitmapFormat of
    TBitmapFormat.Gray8 :
      begin
        if (Length(rgbRawBytes) < Length(luminances))
        then
           len := Length(rgbRawBytes)
        else
           len := Length(luminances);

        Copy(rgbRawBytes, 0, len);
      end;
    TBitmapFormat.RGB24 :
      begin
        CalculateLuminanceRGB24(rgbRawBytes);
      end;
    TBitmapFormat.RGB32 :
      begin
        CalculateLuminanceRGB32(rgbRawBytes);
      end;
    TBitmapFormat.ARGB32 :
      begin
        CalculateLuminanceARGB32(rgbRawBytes);
      end;
    TBitmapFormat.BGR24 :
      begin
        CalculateLuminanceBGR24(rgbRawBytes);
      end;
    TBitmapFormat.BGR32 :
      begin
        CalculateLuminanceBGR32(rgbRawBytes);
      end;
    TBitmapFormat.BGRA32 :
      begin
        CalculateLuminanceBGRA32(rgbRawBytes);
      end;
    TBitmapFormat.RGB565 :
      begin
        CalculateLuminanceRGB565(rgbRawBytes);
      end;
    TBitmapFormat.RGBA32 :
      begin
        CalculateLuminanceRGBA32(rgbRawBytes);
      end;
     else
        raise EArgumentException.Create('The bitmap format isn''t supported.' +
          GetEnumName(TypeInfo(TBitmapFormat), Ord(bitmapFormat)))
    end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGB565(
  const rgb565RawData: TArray<Byte>);
var
  luminanceIndex,
  index  : Integer;
  byte1,
  byte2,
  b5, g5, r5,
  r8, g8, b8 : Byte;
begin
  luminanceIndex := 0;
  index := 0;
  while (((index < Length(rgb565RawData)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    byte1 := rgb565RawData[index];
    byte2 := rgb565RawData[index + 1];

    b5 := (byte1 and $1F);
    g5 := (TMathUtils.Asr(byte1 and $E0 , 5) or ((byte2 and $03) shl 3)) and $1F;
    r5 := TMathUtils.Asr(byte2, 2) and $1F;
    r8 := TMathUtils.Asr(r5 * 527 + 23, 6);
    g8 := TMathUtils.Asr(g5 * 527 + 23, 6);
    b8 := TMathUtils.Asr(b5 * 527 + 23, 6);

    // cheap, not fully accurate conversion
    //pixel := (byte2 shl 8) or byte1;
    //b8 := (((pixel) and $001F) shl 3);
    //g8 := (((pixel) and $07E0) shr 2) and $FF;
    //r8 := (((pixel) and $F800) shr 8);

    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r8 + GChannelWeight * g8 +
                                    BChannelWeight * b8, ChannelWeight);
    Inc(index, 2);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGB24(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b : Integer;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b,ChannelWeight);
    inc(luminanceIndex)
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceBGR24(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  b, g, r : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b,ChannelWeight);
    Inc(luminanceIndex)
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGB32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex, 2);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b, ChannelWeight);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceBGR32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  b, g, r : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex, 2);
    luminances[luminanceIndex] := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                                    BChannelWeight * b, ChannelWeight);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceBGRA32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  b, g, r,
  alpha : Byte;
  luminance : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    alpha := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);

    luminance := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                   BChannelWeight * b, ChannelWeight);
    luminances[luminanceIndex] := (TMathUtils.Asr(luminance * alpha, 8) + TMathUtils.Asr(255 * (255 - alpha), 8));
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceRGBA32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b,
  alpha : Byte;
  luminance : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    alpha := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);

    luminance := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                   BChannelWeight * b, ChannelWeight);
    luminances[luminanceIndex] := TMathUtils.Asr(luminance * alpha, 8) + TMathUtils.Asr(255 * (255 - alpha), 8);
    Inc(luminanceIndex);
  end;
end;

procedure TRGBLuminanceSource.CalculateLuminanceARGB32(
  const rgbRawBytes: TArray<Byte>);
var
  rgbIndex,
  luminanceIndex,
  r, g, b,
  alpha : Byte;
  luminance : Byte;
begin
  rgbIndex := 0;
  luminanceIndex := 0;
  while (((rgbIndex < Length(rgbRawBytes)) and
          (luminanceIndex < Length(luminances)))) do
  begin
    // Calculate luminance cheaply, favoring green.
    alpha := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    r := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    g := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);
    b := rgbRawBytes[rgbIndex];
    Inc(rgbIndex);

    luminance := TMathUtils.Asr(RChannelWeight * r + GChannelWeight * g +
                   BChannelWeight * b, ChannelWeight);
    luminances[luminanceIndex] := TMathUtils.Asr(luminance * alpha,8) + TMathUtils.Asr(255 * (255 - alpha),8);
    Inc(luminanceIndex);
  end;
end;

function TRGBLuminanceSource.GetBitmapFormat: TBitmapFormat;
begin
  try
    Result := DetermineBitmapFormat(luminances, Width, Height);
  except
    Result := TBitmapFormat.Unknown;
  end;
end;

end.
