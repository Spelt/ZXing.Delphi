unit ZXing.GlobalHistogramBinarizer;
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
  ZXing.Binarizer,
  ZXing.LuminanceSource,
  ZXing.Common.BitArray,
  ZXing.Common.BitMatrix,
  ZXing.Common.Detector.MathUtils;

type
  TGlobalHistogramBinarizer = class(TBinarizer)
  private const
    LUMINANCE_BITS = 5;
    LUMINANCE_SHIFT = 8 - LUMINANCE_BITS;
    LUMINANCE_BUCKETS = 1 shl LUMINANCE_BITS;
  private type
    TBuckets = array[0..LUMINANCE_BUCKETS-1] of Integer;
  private
    buckets: TBuckets;
    luminances: TArray<Byte>;

    procedure InitArrays(luminanceSize: Integer);
    function estimateBlackPoint(buckets: TBuckets;
      var blackPoint: Integer): Boolean;
  public
    constructor Create(source: TLuminanceSource);

    // constructor GlobalHistogramBinarizer(source: TLuminanceSource);
    function GetBlackRow(y: Integer; row: IBitArray): IBitArray; override;
    /// <summary>
    /// Does not sharpen the data, as this call is intended to only be used by
    /// 2D Readers. Returns nil when the image has too little contrast; the
    /// caller frees the result.
    /// </summary>
    function BlackMatrix: TBitMatrix; override;
    function createBinarizer(source: TLuminanceSource): TBinarizer; override;
  end;

implementation

{ TGlobalHistogramBinarizer }

constructor TGlobalHistogramBinarizer.Create(source: TLuminanceSource);
begin
  inherited Create(source);
  luminances := [];
end;

function TGlobalHistogramBinarizer.GetBlackRow(y: Integer; row: IBitArray)
  : IBitArray;
var
  localLuminances: TArray<Byte>;
  localBuckets: TBuckets;
  w, blackPoint, x, left, right, center, luminance: Integer;
begin
  w := width;
  if ((row = nil) or (row.Size < w)) then
    row := TBitArrayHelpers.CreateBitArray(w)
  else
    row.Clear();

  InitArrays(w);
  localLuminances := LuminanceSource.getRow(y, luminances);
  localBuckets := buckets;

  // (pointers through the row: this runs for every row of the image)
  var pl: PByte := @localLuminances[0];
  for x := 0 to w - 1 do
  begin
    Inc(localBuckets[pl^ shr LUMINANCE_SHIFT]);
    Inc(pl);
  end;

  if (not estimateBlackPoint(localBuckets, blackPoint)) then
  begin
    result := nil;
    exit;
  end;

  if (w < 3) then
  begin
    // Special case for very small images
    for x := 0 to w - 1 do
      if ((localLuminances[x] and $FF) < blackPoint) then
        row[x] := true;
  end
  else
  begin
    left := localLuminances[0];
    center := localLuminances[1];
    pl := @localLuminances[2];

    // collect the bits per 32 and write them at once into the words of the
    // row (the row is cleared)
    var words := row.Bits;
    var word: Integer := 0;
    for x := 1 to w - 2 do
    begin

      right := pl^;
      Inc(pl);
      // A simple -1 4 -1 box filter with a weight of 2. A negative value is
      // always below the black point (which is not negative), otherwise
      // shr 1 is the same as the arithmetic shift of before.
      luminance := (center shl 2) - left - right;
      if (luminance < 0) or ((luminance shr 1) < blackPoint) then
        word := word or (1 shl (x and $1F));
      if ((x and $1F) = $1F) then
      begin
        words[x shr 5] := word;
        word := 0;
      end;
      left := center;
      center := right;
    end;
    if (word <> 0) then
      words[(w - 2) shr 5] := word;
  end;

  result := row;

end;

function TGlobalHistogramBinarizer.BlackMatrix: TBitMatrix;
var
  localLuminances: TArray<Byte>;
  localBuckets: TBuckets;
  w, h, x, y, rowNumber, right, pixel, offset, blackPoint: Integer;
begin
  w := width;
  h := height;

  // Quickly calculates the histogram by sampling four rows from the image.
  // This proved to be more robust on the blackbox tests than sampling a
  // diagonal as we used to do.
  InitArrays(w);
  localBuckets := buckets;
  for y := 1 to 4 do
  begin
    rowNumber := (h * y) div 5;
    localLuminances := LuminanceSource.getRow(rowNumber, luminances);
    right := (w * 4) div 5;
    for x := (w div 5) to right - 1 do
    begin
      pixel := localLuminances[x] and $FF;
      Inc(localBuckets[TMathUtils.Asr(pixel, LUMINANCE_SHIFT)]);
    end;
  end;

  if (not estimateBlackPoint(localBuckets, blackPoint)) then
  begin
    result := nil;
    exit;
  end;

  // We delay reading the entire image luminance until the black point
  // estimation succeeds. Although we end up reading four rows twice, it is
  // consistent with our motto of "fail quickly" which is necessary for
  // continuous scanning.
  result := TBitMatrix.Create(w, h);
  localLuminances := LuminanceSource.Matrix;
  for y := 0 to h - 1 do
  begin
    offset := y * w;
    for x := 0 to w - 1 do
    begin
      pixel := localLuminances[offset + x] and $FF;
      if (pixel < blackPoint) then
        result[x, y] := true;
    end;
  end;
end;

function TGlobalHistogramBinarizer.createBinarizer(source: TLuminanceSource)
  : TBinarizer;
begin
  result := TGlobalHistogramBinarizer.Create(source);
end;

procedure TGlobalHistogramBinarizer.InitArrays(luminanceSize: Integer);
var
  x: Integer;
begin

  if (Length(luminances) < luminanceSize) then
  begin
    SetLength(luminances, luminanceSize);
  end;

  for x := 0 to LUMINANCE_BUCKETS - 1 do
  begin
    buckets[x] := 0;
  end;

end;

function TGlobalHistogramBinarizer.estimateBlackPoint(buckets: TBuckets;
  var blackPoint: Integer): Boolean;
var
  x, maxBucketCount, firstPeak, firstPeakSize, secondPeak,
  secondPeakScore, distanceToBiggest, score, temp, bestValley,
  bestValleyScore, fromFirst: Integer;
begin
  blackPoint := 0;

  // Find the tallest peak in the histogram.
  maxBucketCount := 0;
  firstPeak := 0;
  firstPeakSize := 0;

  for x := Low(buckets) to High(buckets) do
  begin

    if (buckets[x] > firstPeakSize) then
    begin
      firstPeak := x;
      firstPeakSize := buckets[x];
    end;

    if (buckets[x] > maxBucketCount) then
    begin
      maxBucketCount := buckets[x];
    end;

  end;

  // Find the second-tallest peak which is somewhat far from the tallest peak.
  secondPeak := 0;
  secondPeakScore := 0;
  for x := Low(buckets) to High(buckets) do
  begin
    distanceToBiggest := x - firstPeak;
    // Encourage more distant second peaks by multiplying by square of distance.
    score := buckets[x] * distanceToBiggest * distanceToBiggest;
    if (score > secondPeakScore) then
    begin
      secondPeak := x;
      secondPeakScore := score;
    end;
  end;

  // Make sure firstPeak corresponds to the black peak.
  if (firstPeak > secondPeak) then
  begin
    temp := firstPeak;
    firstPeak := secondPeak;
    secondPeak := temp;
  end;

  // If there is too little contrast in the image to pick a meaningful black point, throw rather
  // than waste time trying to decode the image, and risk false positives.
  // TODO: It might be worth comparing the brightest and darkest pixels seen, rather than the
  // two peaks, to determine the contrast.
  if ((secondPeak - firstPeak) <= (TMathUtils.Asr(LUMINANCE_BUCKETS, 4))) then
  begin
    result := false;
    exit;
  end;

  // Find a valley between them that is low and closer to the white peak.
  bestValley := secondPeak - 1;
  bestValleyScore := -1;

  x := secondPeak - 1;
  while (x > firstPeak) do
  begin
    fromFirst := x - firstPeak;
    score := fromFirst * fromFirst * (secondPeak - x) *
      (maxBucketCount - buckets[x]);

    if (score > bestValleyScore) then
    begin
      bestValley := x;
      bestValleyScore := score;
    end;

    Dec(x);
  end;

  blackPoint := bestValley shl LUMINANCE_SHIFT;
  result := true;
end;

end.
