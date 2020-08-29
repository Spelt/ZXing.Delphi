unit ZXing.HybridBinarizer;
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
  SysUtils, 
  ZXing.GlobalHistogramBinarizer,
  ZXing.LuminanceSource,
  ZXing.Common.BitMatrix,
  ZXing.binarizer,
  ZXing.Common.Detector.MathUtils;

/// <summary> This class implements a local thresholding algorithm, which while slower than the
/// GlobalHistogramBinarizer, is fairly efficient for what it does. It is designed for
/// high frequency images of barcodes with black data on white backgrounds. For this application,
/// it does a much better job than a global blackpoint with severe shadows and gradients.
/// However it tends to produce artifacts on lower frequency images and is therefore not
/// a good general purpose binarizer for uses outside ZXing.
///
/// This class extends GlobalHistogramBinarizer, using the older histogram approach for 1D readers,
/// and the newer local approach for 2D readers. 1D decoding using a per-row histogram is already
/// inherently local, and only fails for horizontal gradients. We can revisit that problem later,
/// but for now it was not a win to use local blocks for 1D.
///
/// This Binarizer is the default for the unit tests and the recommended class for library users.
///
/// </summary>
/// <author>  dswitkin@google.com (Daniel Switkin)
/// </author>
/// <author>www.Redivivus.in (suraj.supekar@redivivus.in) - Ported from ZXING Java Source
/// </author>
/// <author>Adapted for Delphi/Object Pascal FireMonkey XE7 mobile by E.Spelt
/// </author>

type
  TArrayIntOfInt = TArray<TArray<Integer>>;

  THybridBinarizer = class(TGlobalHistogramBinarizer)
  private
    matrix: TBitMatrix;
    procedure BinarizeEntireImage;
    procedure calculateThresholdForBlock(luminances: TArray<Byte>;
      subWidth: Integer; subHeight: Integer; width: Integer; height: Integer;
      blackPoints: TArrayIntOfInt; matrix: TBitMatrix);
    function calculateBlackPoints(luminances: TArray<Byte>; subWidth: Integer;
      subHeight: Integer; width: Integer; height: Integer): TArrayIntOfInt;
    procedure thresholdBlock(luminances: TArray<Byte>; xoffset: Integer;
      yoffset: Integer; threshold: Integer; stride: Integer;
      matrix: TBitMatrix);
    function cap(value: Integer; min: Integer; max: Integer): Integer;
  protected
    //
  public
    function createBinarizer(source: TLuminanceSource): TBinarizer; override;

    constructor Create(source: TLuminanceSource);
    function BlackMatrix: TBitMatrix; override;
  end;

implementation

{ THybridBinarizer }

function THybridBinarizer.BlackMatrix: TBitMatrix;
begin
  BinarizeEntireImage();
  result := matrix;
end;

function THybridBinarizer.createBinarizer(source: TLuminanceSource): TBinarizer;
begin
  result := THybridBinarizer.Create(source)
end;

procedure THybridBinarizer.BinarizeEntireImage;
var
  source: TLuminanceSource;
  luminances: TArray<Byte>;
  subWidth, subHeight, width, height: Integer;
  blackPoints: TArrayIntOfInt;
  newMatrix: TBitMatrix;
begin
  if (self.matrix <> nil) then
  begin
    inherited BlackMatrix;
    Exit;
  end;

  newMatrix := nil;
  try

    source := self.LuminanceSource;
    width := source.width;
    height := source.height;

    if ((width >= 40) and (height >= 40)) then
    begin
      luminances := source.matrix;
      subWidth := TMathUtils.Asr(width, 3);

      if ((width and 7) <> 0) then
        inc(subWidth);

      subHeight := TMathUtils.Asr(height, 3);

      if ((height and 7) <> 0) then
        inc(subHeight);

      blackPoints := calculateBlackPoints(luminances, subWidth, subHeight,
        width, height);

      newMatrix := TBitMatrix.Create(width, height);
      calculateThresholdForBlock(luminances, subWidth, subHeight, width, height,
        blackPoints, newMatrix);

      self.matrix := newMatrix;
    end;

  finally
    newMatrix := nil;
    SetLength(blackPoints,0);
    blackPoints := nil;
    luminances := nil;
  end;
end;

function THybridBinarizer.calculateBlackPoints(luminances: TArray<Byte>;
  subWidth: Integer; subHeight: Integer; width: Integer; height: Integer)
  : TArrayIntOfInt;
var
  blackPoints: TArrayIntOfInt;
  i, x, y, yoffset, maxYOffset, xoffset, maxXOffset, sum, min, max, yy, offset,
    xx, pixel, average, averageNeighborBlackPoint: Integer;

begin
  blackPoints := TArrayIntOfInt.Create();
  SetLength(blackPoints, subHeight);
  i := 0;

  while ((i < subHeight)) do
  begin
    blackPoints[i] := TArray<Integer>.Create();
    SetLength(blackPoints[i], subWidth);
    inc(i)
  end;

  y := 0;

  while ((y < subHeight)) do
  begin

    yoffset := (y shl 3);
    maxYOffset := (height - 8);

    if (yoffset > maxYOffset) then
      yoffset := maxYOffset;
    x := 0;

    while ((x < subWidth)) do
    begin
      xoffset := (x shl 3);
      maxXOffset := (width - 8);
      if (xoffset > maxXOffset) then
        xoffset := maxXOffset;
      sum := 0;
      min := $FF;
      max := 0;
      yy := 0;
      offset := ((yoffset * width) + xoffset);

      while ((yy < 8)) do
      begin
        xx := 0;
        while ((xx < 8)) do
        begin
          pixel := (luminances[(offset + xx)] and $FF);
          inc(sum, pixel);
          if (pixel < min) then
            min := pixel;
          if (pixel > max) then
            max := pixel;
          inc(xx)
        end;

        if ((max - min) > $18) then
        begin
          inc(yy);
          inc(offset, width);
          while ((yy < 8)) do
          begin
            xx := 0;
            while ((xx < 8)) do
            begin
              inc(sum, (luminances[(offset + xx)] and $FF));
              inc(xx)
            end;
            inc(yy);
            inc(offset, width)
          end
        end;
        inc(yy);
        inc(offset, width)
      end;

      average := TMathUtils.Asr(sum, 6);
      if ((max - min) <= $18) then
      begin
        average := TMathUtils.Asr(min, 1);
        if ((y > 0) and (x > 0)) then
        begin
          averageNeighborBlackPoint :=
            TMathUtils.Asr
            (((blackPoints[(y - 1)][x] + (2 * blackPoints[y][(x - 1)])) +
            blackPoints[(y - 1)][(x - 1)]), 2);
          if (min < averageNeighborBlackPoint) then
            average := averageNeighborBlackPoint
        end
      end;
      blackPoints[y][x] := average;
      inc(x)
    end;
    inc(y)
  end;

  result := blackPoints;
end;

procedure THybridBinarizer.calculateThresholdForBlock(luminances: TArray<Byte>;
  subWidth: Integer; subHeight: Integer; width: Integer; height: Integer;
  blackPoints: TArrayIntOfInt; matrix: TBitMatrix);
var
  y, yoffset, maxYOffset, x, xoffset, maxXOffset, left, top, sum, z,
    average: Integer;
  blackRow: TArray<Integer>;
begin
  y := 0;
  while ((y < subHeight)) do
  begin
    yoffset := (y shl 3);
    maxYOffset := (height - 8);
    if (yoffset > maxYOffset) then
      yoffset := maxYOffset;
    x := 0;
    while ((x < subWidth)) do
    begin
      xoffset := (x shl 3);
      maxXOffset := (width - 8);
      if (xoffset > maxXOffset) then
        xoffset := maxXOffset;
      left := cap(x, 2, (subWidth - 3));
      top := cap(y, 2, (subHeight - 3));
      sum := 0;
      z := -2;
      while ((z <= 2)) do
      begin
        blackRow := blackPoints[(top + z)];
        inc(sum, blackRow[(left - 2)]);
        inc(sum, blackRow[(left - 1)]);
        inc(sum, blackRow[left]);
        inc(sum, blackRow[(left + 1)]);
        inc(sum, blackRow[(left + 2)]);
        inc(z)
      end;
      average := (sum div $19);
      thresholdBlock(luminances, xoffset, yoffset, average, width, matrix);
      inc(x)
    end;
    inc(y)
  end
end;

constructor THybridBinarizer.Create(source: TLuminanceSource);
begin
  inherited Create(source);
end;

procedure THybridBinarizer.thresholdBlock(luminances: TArray<Byte>;
  xoffset: Integer; yoffset: Integer; threshold: Integer; stride: Integer;
  matrix: TBitMatrix);
var
  offset, x, y, pixel: Integer;
begin
  offset := ((yoffset * stride) + xoffset);
  y := 0;
  while ((y < 8)) do
  begin
    x := 0;
    while ((x < 8)) do
    begin
      pixel := (luminances[(offset + x)] and $FF);
      matrix[(xoffset + x), (yoffset + y)] := (pixel <= threshold);
      inc(x)
    end;
    inc(y);
    inc(offset, stride)
  end
end;

function THybridBinarizer.cap(value: Integer; min: Integer;
  max: Integer): Integer;
begin

  if (value < min) then
  begin
    result := min
  end
  else if (value > max) then
  begin
    result := max
  end
  else
    result := value
end;

end.
