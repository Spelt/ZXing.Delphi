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
  ZXing.Common.Detector.MathUtils;

type
  TGlobalHistogramBinarizer = class(TBinarizer)
  private

    class var LUMINANCE_BITS, LUMINANCE_SHIFT, LUMINANCE_BUCKETS: Integer;
    EMPTY: TArray<Byte>;

    luminances: TArray<Byte>;
    buckets: TArray<Integer>;
    procedure InitArrays(luminanceSize: Integer);
    function estimateBlackPoint(buckets: TArray<Integer>;
      var blackPoint: Integer): Boolean;
    class procedure ClassInit; static;

  public
    constructor Create(source: TLuminanceSource);

    // constructor GlobalHistogramBinarizer(source: TLuminanceSource);
    function GetBlackRow(y: Integer; row: IBitArray): IBitArray; override;
  end;

implementation

{ TGlobalHistogramBinarizer }

class procedure TGlobalHistogramBinarizer.ClassInit();
begin
  LUMINANCE_BITS := 5;
  LUMINANCE_SHIFT := 8 - LUMINANCE_BITS;
  LUMINANCE_BUCKETS := 1 shl LUMINANCE_BITS;
  EMPTY := TArray<Byte>.Create();
  SetLength(EMPTY, 0);
end;

constructor TGlobalHistogramBinarizer.Create(source: TLuminanceSource);
begin
  inherited Create(source);
  luminances := EMPTY;
  buckets := TArray<Integer>.Create();
  SetLength(self.buckets, LUMINANCE_BUCKETS);
end;

{
  constructor TGlobalHistogramBinarizer.GlobalHistogramBinarizer
  (source: TLuminanceSource);
  begin
  Inherited Create(source);



  SetLength(luminances, 0);
  SetLength(buckets, LUMINANCE_BUCKETS);
  end;
}

function TGlobalHistogramBinarizer.GetBlackRow(y: Integer; row: IBitArray)
  : IBitArray;
var
  localLuminances: TArray<Byte>;
  localBuckets: TArray<Integer>;
  i, w, blackPoint, x, pixel, left, right, center, luminance: Integer;
begin
  w := width;
  if ((row = nil) or (row.Size < w)) then
  begin
    row := TBitArrayHelpers.CreateBitArray(w);
  end
  else
  begin
    row.Clear();
  end;

  InitArrays(w);
  localLuminances := LuminanceSource.getRow(y, luminances);
  localBuckets := buckets;

  for x := 0 to w - 1 do
  begin
    pixel := localLuminances[x] and $FF;
    i := TMathUtils.Asr(pixel, LUMINANCE_SHIFT);
    localBuckets[i] := localBuckets[i] + 1;
  end;

  if (not estimateBlackPoint(localBuckets, blackPoint)) then
  begin
    result := nil;
    exit;
  end;

  left := localLuminances[0] and $FF;
  center := localLuminances[1] and $FF;

  for x := 1 to w - 2 do
  begin

    right := localLuminances[x + 1] and $FF;
    // A simple -1 4 -1 box filter with a weight of 2.

    luminance := (center shl 2) - left - right;
    luminance := TMathUtils.Asr(luminance, 1);
    row[x] := (luminance < blackPoint);
    left := center;
    center := right;
  end;

  result := row;

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

function TGlobalHistogramBinarizer.estimateBlackPoint(buckets: TArray<Integer>;
  var blackPoint: Integer): Boolean;

var
  x, numBuckets, maxBucketCount, firstPeak, firstPeakSize, secondPeak,
    secondPeakScore, distanceToBiggest, score, temp, bestValley,
    bestValleyScore, fromFirst: Integer;

begin
  blackPoint := 0;

  // Find the tallest peak in the histogram.
  numBuckets := Length(buckets);
  maxBucketCount := 0;
  firstPeak := 0;
  firstPeakSize := 0;

  for x := 0 to numBuckets - 1 do
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
  for x := 0 to numBuckets - 1 do
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
  if ((secondPeak - firstPeak) <= (TMathUtils.Asr(numBuckets, 4))) then
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

Initialization

TGlobalHistogramBinarizer.ClassInit();

end.
