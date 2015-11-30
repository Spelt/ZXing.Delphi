unit FinderPatternFinder;

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
}
interface

uses System.Generics.Defaults, SysUtils, Generics.collections,
  BitMatrix, resultPoint, // NullableType,
  DecodeHinttype, FinderPatternInfo, finderPattern, math;

type

  TFinderPatternFinder = class
  private

    const
    CENTER_QUORUM: Integer = 2;
    INTEGER_MATH_SHIFT: Integer = 8;
    MIN_SKIP: Integer = 3;
    MAX_MODULES: Integer = $39;

  var
    FCrossCheckStateCount: TArray<Integer>;
    hasSkipped: boolean;
    FImage: TBitMatrix;
    PossibleCenters: TList<TFinderPattern>;
    resultPointCallback: TresultPointCallback;

    class function centerFromEnd(stateCount: TArray<Integer>; pEnd: Integer)
      : Single; static;
    function crossCheckDiagonal(startI: Integer; centerJ: Integer;
      maxCount: Integer; originalStateCountTotal: Integer): boolean;
    function crossCheckHorizontal(startJ: Integer; centerI: Integer;
      maxCount: Integer; originalStateCountTotal: Integer): Single;
    function crossCheckVertical(startI: Integer; centerJ: Integer;
      maxCount: Integer; originalStateCountTotal: Integer): Single;
    function findRowSkip: Integer;
    function haveMultiplyConfirmedCenters: boolean;
    function selectBestPatterns: TArray<TFinderPattern>;

    function CrossCheckStateCount: TArray<Integer>;
  public
    // constructor Create(image: TBitMatrix); overload;
    constructor Create(image: TBitMatrix;
      resultPointCallback: TresultPointCallback);
    function find(hints: TDictionary<TDecodeHintType, TObject>)
      : TFinderPatternInfo; virtual;

  protected
    class function foundPatternCross(stateCount: TArray<Integer>)
      : boolean; static;
    function handlePossibleCenter(stateCount: TArray<Integer>; i: Integer;
      j: Integer; pureBarcode: boolean): boolean;
    property image: TBitMatrix read FImage;

  end;

  TFurthestFromAverageComparator = class sealed(TInterfacedObject,
    IComparer<TFinderPattern>)
  private
    average: Single;
  public
    constructor Create(f: Single);
    function Compare(const Left, Right: TFinderPattern): Integer;
  end;

  TCenterComparator = class sealed(TInterfacedObject, IComparer<TFinderPattern>)
  private
    average: Single;
  public
    constructor Create(f: Single);
    function Compare(const Left, Right: TFinderPattern): Integer;
  end;

implementation

{ TFinderPatternFinder }

class function TFinderPatternFinder.centerFromEnd(stateCount: TArray<Integer>;
  pEnd: Integer): Single;
var
  aResult: Single;
begin

  aResult := (pEnd - stateCount[4] - stateCount[3]) - (stateCount[2] / 2);

  if (Single.IsNaN(aResult)) then
  begin
    result := -1;
    exit
  end;

  result := aResult;

end;

{
  constructor TFinderPatternFinder.Create(image: TBitMatrix);
  begin
  Create(image, nil);
  end;
}

constructor TFinderPatternFinder.Create(image: TBitMatrix;
  resultPointCallback: TresultPointCallback);
begin
  FImage := image;
  self.PossibleCenters := TList<TFinderPattern>.Create;
  FCrossCheckStateCount := TArray<Integer>.Create();
  SetLength(FCrossCheckStateCount, 5);
  self.resultPointCallback := resultPointCallback;
end;

function TFinderPatternFinder.crossCheckDiagonal(startI, centerJ, maxCount,
  originalStateCountTotal: Integer): boolean;
var
  stateCount: TArray<Integer>;
  i, maxI, maxJ, stateCountTotal: Integer;
begin
  stateCount := CrossCheckStateCount;
  i := 0;
  while ((((startI >= i) and (centerJ >= i)) and self.image[(centerJ - i),
    (startI - i)])) do
  begin
    inc(stateCount[2]);
    inc(i)
  end;
  if ((startI < i) or (centerJ < i)) then
  begin
    result := false;
    exit
  end;
  while (((((startI >= i) and (centerJ >= i)) and not self.image[(centerJ - i),
    (startI - i)]) and (stateCount[1] <= maxCount))) do
  begin
    inc(stateCount[1]);
    inc(i)
  end;
  if (((startI < i) or (centerJ < i)) or (stateCount[1] > maxCount)) then
  begin
    result := false;
    exit
  end;
  while (((((startI >= i) and (centerJ >= i)) and self.image[(centerJ - i),
    (startI - i)]) and (stateCount[0] <= maxCount))) do
  begin
    inc(stateCount[0]);
    inc(i)
  end;
  if (stateCount[0] > maxCount) then
  begin
    result := false;
    exit
  end;
  maxI := self.image.Height;
  maxJ := self.image.Width;
  i := 1;
  while (((((startI + i) < maxI) and ((centerJ + i) < maxJ)) and
    self.image[(centerJ + i), (startI + i)])) do
  begin
    inc(stateCount[2]);
    inc(i)
  end;
  if (((startI + i) >= maxI) or ((centerJ + i) >= maxJ)) then
  begin
    result := false;
    exit
  end;
  while ((((((startI + i) < maxI) and ((centerJ + i) < maxJ)) and
    not self.image[(centerJ + i), (startI + i)]) and
    (stateCount[3] < maxCount))) do
  begin
    inc(stateCount[3]);
    inc(i)
  end;
  if ((((startI + i) >= maxI) or ((centerJ + i) >= maxJ)) or
    (stateCount[3] >= maxCount)) then
  begin
    result := false;
    exit
  end;
  while ((((((startI + i) < maxI) and ((centerJ + i) < maxJ)) and
    self.image[(centerJ + i), (startI + i)]) and (stateCount[4] < maxCount))) do
  begin
    inc(stateCount[4]);
    inc(i)
  end;
  if (stateCount[4] >= maxCount) then
  begin
    result := false;
    exit
  end;

  stateCountTotal := ((((stateCount[0] + stateCount[1]) + stateCount[2]) +
    stateCount[3]) + stateCount[4]);

  result := (Abs(stateCountTotal - originalStateCountTotal) <
    (2 * originalStateCountTotal)) and TFinderPatternFinder.foundPatternCross
    (stateCount);

end;

function TFinderPatternFinder.crossCheckHorizontal(startJ: Integer;
  centerI: Integer; maxCount: Integer;
  originalStateCountTotal: Integer): Single;
var
  stateCount: TArray<Integer>;
  maxJ, j, stateCountTotal: Integer;

begin
  maxJ := FImage.Width;
  stateCount := CrossCheckStateCount;

  j := startJ;
  while (((j >= 0) and FImage[j, centerI])) do
  begin
    inc(stateCount[2]);
    dec(j)
  end;

  if (j < 0) then
  begin
    result := -1;
    exit
  end;

  while ((((j >= 0) and not FImage[j, centerI]) and
    (stateCount[1] <= maxCount))) do
  begin
    inc(stateCount[1]);
    dec(j)
  end;

  if ((j < 0) or (stateCount[1] > maxCount)) then
  begin
    result := -1;
    exit
  end;

  while ((((j >= 0) and FImage[j, centerI]) and (stateCount[0] <= maxCount))) do
  begin
    inc(stateCount[0]);
    dec(j)
  end;

  if (stateCount[0] > maxCount) then
  begin
    result := -1;
    exit
  end;
  j := (startJ + 1);

  while (((j < maxJ) and FImage[j, centerI])) do
  begin
    inc(stateCount[2]);
    inc(j)
  end;

  if (j = maxJ) then
  begin
    result := -1;
    exit
  end;

  while ((((j < maxJ) and not FImage[j, centerI]) and
    (stateCount[3] < maxCount))) do
  begin
    inc(stateCount[3]);
    inc(j)
  end;

  if ((j = maxJ) or (stateCount[3] >= maxCount)) then
  begin
    result := -1;
    exit
  end;

  while (((j < maxJ) and FImage[j, centerI]) and (stateCount[4] < maxCount)) do
  begin
    inc(stateCount[4]);
    inc(j)
  end;

  if (stateCount[4] >= maxCount) then
  begin
    result := -1;
    exit
  end;

  stateCountTotal := ((((stateCount[0] + stateCount[1]) + stateCount[2]) +
    stateCount[3]) + stateCount[4]);

  if (5 * Abs(stateCountTotal - originalStateCountTotal) >=
    originalStateCountTotal) then
  begin
    result := -1;
    exit
  end;

  if TFinderPatternFinder.foundPatternCross(stateCount) then
  begin
    result := TFinderPatternFinder.centerFromEnd(stateCount, j);
    exit;
  end;

  result := -1;

end;

function TFinderPatternFinder.CrossCheckStateCount: TArray<Integer>;
begin
  FCrossCheckStateCount[0] := 0;
  FCrossCheckStateCount[1] := 0;
  FCrossCheckStateCount[2] := 0;
  FCrossCheckStateCount[3] := 0;
  FCrossCheckStateCount[4] := 0;
  result := FCrossCheckStateCount;
end;

function TFinderPatternFinder.crossCheckVertical(startI: Integer;
  centerJ: Integer; maxCount: Integer;
  originalStateCountTotal: Integer): Single;
var
  stateCount: TArray<Integer>;
  maxI, i, stateCountTotal: Integer;
begin

  maxI := FImage.Height;
  stateCount := CrossCheckStateCount;
  i := startI;

  while (((i >= 0) and FImage[centerJ, i])) do
  begin
    inc(stateCount[2]);
    dec(i)
  end;

  if (i < 0) then
  begin
    result := -1;
    exit
  end;

  while ((((i >= 0) and not FImage[centerJ, i]) and
    (stateCount[1] <= maxCount))) do
  begin
    inc(stateCount[1]);
    dec(i)
  end;

  if ((i < 0) or (stateCount[1] > maxCount)) then
  begin
    result := -1;
    exit
  end;

  while ((((i >= 0) and FImage[centerJ, i]) and (stateCount[0] <= maxCount))) do
  begin
    inc(stateCount[0]);
    dec(i)
  end;

  if (stateCount[0] > maxCount) then
  begin
    result := -1;
    exit
  end;
  i := (startI + 1);

  while (((i < maxI) and FImage[centerJ, i])) do
  begin
    inc(stateCount[2]);
    inc(i)
  end;

  if (i = maxI) then
  begin
    result := -1;
    exit
  end;

  while ((((i < maxI) and not FImage[centerJ, i]) and
    (stateCount[3] < maxCount))) do
  begin
    inc(stateCount[3]);
    inc(i)
  end;

  if ((i = maxI) or (stateCount[3] >= maxCount)) then
  begin
    result := -1;
    exit
  end;

  while ((((i < maxI) and FImage[centerJ, i]) and
    (stateCount[4] < maxCount))) do
  begin
    inc(stateCount[4]);
    inc(i)
  end;

  if (stateCount[4] >= maxCount) then
  begin
    result := -1;
    exit
  end;

  stateCountTotal := ((((stateCount[0] + stateCount[1]) + stateCount[2]) +
    stateCount[3]) + stateCount[4]);

  if ((5 * Abs(stateCountTotal - originalStateCountTotal)) >=
    (2 * originalStateCountTotal)) then
  begin
    result := -1;
    exit
  end;

  if TFinderPatternFinder.foundPatternCross(stateCount) then
  begin
    result := TFinderPatternFinder.centerFromEnd(stateCount, i);
    exit;
  end;

  result := -1;

end;

function TFinderPatternFinder.find(hints: TDictionary<TDecodeHintType, TObject>)
  : TFinderPatternInfo;
var
  tryHarder, pureBarcode, done, confirmed: boolean;
  maxI, maxJ, iSkip, i, currentState, j, rowSkip: Integer;
  stateCount: TArray<Integer>;
  patternInfo: TArray<TFinderPattern>;
  resultInfo: TArray<TResultPoint>;
begin
  tryHarder := ((hints <> nil) and
    hints.ContainsKey(DecodeHinttype.TRY_HARDER));
  pureBarcode := ((hints <> nil) and
    hints.ContainsKey(DecodeHinttype.PURE_BARCODE));
  maxI := self.image.Height;
  maxJ := self.image.Width;
  iSkip := ((3 * maxI) div $E4);
  if ((iSkip < 3) or tryHarder) then
    iSkip := 3;
  done := false;
  stateCount := TArray<Integer>.Create();
  SetLength(stateCount, 5);
  i := (iSkip - 1);
  while (((i < maxI) and not done)) do
  begin
    stateCount[0] := 0;
    stateCount[1] := 0;
    stateCount[2] := 0;
    stateCount[3] := 0;
    stateCount[4] := 0;
    currentState := 0;
    j := 0;

    while ((j < maxJ)) do
    begin
      if (FImage[j, i]) then
      begin
        if ((currentState and 1) = 1) then
          inc(currentState);
        inc(stateCount[currentState])
      end
      else if ((currentState and 1) = 0) then
        if (currentState = 4) then
          if (TFinderPatternFinder.foundPatternCross(stateCount)) then
          begin
            if (self.handlePossibleCenter(stateCount, i, j, pureBarcode)) then
            begin
              iSkip := 2;
              if (self.hasSkipped) then
                done := self.haveMultiplyConfirmedCenters
              else
              begin
                rowSkip := self.findRowSkip;
                if (rowSkip > stateCount[2]) then
                begin
                  inc(i, ((rowSkip - stateCount[2]) - iSkip));
                  j := (maxJ - 1)
                end
              end
            end
            else
            begin
              stateCount[0] := stateCount[2];
              stateCount[1] := stateCount[3];
              stateCount[2] := stateCount[4];
              stateCount[3] := 1;
              stateCount[4] := 0;
              currentState := 3;
              Continue;
            end;

            currentState := 0;
            stateCount[0] := 0;
            stateCount[1] := 0;
            stateCount[2] := 0;
            stateCount[3] := 0;
            stateCount[4] := 0
          end
          else
          begin
            stateCount[0] := stateCount[2];
            stateCount[1] := stateCount[3];
            stateCount[2] := stateCount[4];
            stateCount[3] := 1;
            stateCount[4] := 0;
            currentState := 3
          end
        else
        begin
          inc(currentState);
          inc(stateCount[currentState])
        end
      else
        inc(stateCount[currentState]);
      inc(j)
    end;

    if (TFinderPatternFinder.foundPatternCross(stateCount)) then
    begin
      confirmed := self.handlePossibleCenter(stateCount, i, maxJ, pureBarcode);
      if confirmed then
      begin
        iSkip := stateCount[0];
        if (hasSkipped) then
          done := self.haveMultiplyConfirmedCenters
      end;
    end;

    inc(i, iSkip)

  end;

  patternInfo := selectBestPatterns();
  if (patternInfo = nil) then
  begin
    result := nil;
    exit
  end;

  resultInfo := TArray<TResultPoint>.Create();
  SetLength(resultInfo, 3);

  resultInfo[0] := patternInfo[0] as TResultPoint;
  resultInfo[1] := patternInfo[1] as TResultPoint;
  resultInfo[2] := patternInfo[2] as TResultPoint;
  TResultPoint.orderBestPatterns(resultInfo);

  patternInfo[0] := resultInfo[0] as TFinderPattern;
  patternInfo[1] := resultInfo[1] as TFinderPattern;
  patternInfo[2] := resultInfo[2] as TFinderPattern;

  result := TFinderPatternInfo.Create(patternInfo);

end;

function TFinderPatternFinder.findRowSkip: Integer;
var
  center: TFinderPattern;
  firstConfirmedCenter: TResultPoint;
begin

  if (self.PossibleCenters.Count > 1) then
  begin
    firstConfirmedCenter := nil;

    for center in self.PossibleCenters do
    begin
      if (center.Count >= 2) then
      begin
        if (firstConfirmedCenter <> nil) then
        begin
          self.hasSkipped := true;
          result := Floor(Abs(firstConfirmedCenter.X - center.X) -
            Abs(firstConfirmedCenter.Y - center.Y)) div 2;
          exit
        end;
        firstConfirmedCenter := center
      end
    end
  end;

  result := 0;
  exit

end;

class function TFinderPatternFinder.foundPatternCross
  (stateCount: TArray<Integer>): boolean;
var
  i, totalModuleSize, Count, moduleSize, maxVariance: Integer;
begin
  totalModuleSize := 0;
  i := 0;

  while ((i < 5)) do
  begin
    Count := stateCount[i];
    if (Count = 0) then
    begin
      result := false;
      exit
    end;
    inc(totalModuleSize, Count);
    inc(i)
  end;

  if (totalModuleSize < 7) then
  begin
    result := false;
    exit
  end;

  moduleSize := ((totalModuleSize shl 8) div 7);
  maxVariance := (moduleSize div 2);

  result := ((((Abs(((moduleSize - (stateCount[0] shl 8)))) < maxVariance) and
    (Abs(((moduleSize - (stateCount[1] shl 8)))) < maxVariance)) and
    ((Abs((((3 * moduleSize) - (stateCount[2] shl 8)))) < (3 * maxVariance)) and
    (Abs(((moduleSize - (stateCount[3] shl 8)))) < maxVariance))) and
    (Abs(((moduleSize - (stateCount[4] shl 8)))) < maxVariance));
  exit

end;

function TFinderPatternFinder.handlePossibleCenter(stateCount: TArray<Integer>;
  i, j: Integer; pureBarcode: boolean): boolean;
var
  stateCountTotal, index: Integer;
  centerJ, centerI: Single;
  center, point: TFinderPattern;
  found: boolean;
  estimatedModuleSize: Single;
begin

  stateCountTotal := ((((stateCount[0] + stateCount[1]) + stateCount[2]) +
    stateCount[3]) + stateCount[4]);

  centerJ := TFinderPatternFinder.centerFromEnd(stateCount, j);

  if (centerJ = -1) then
  begin
    result := false;
    exit
  end;

  centerI := self.crossCheckVertical(i, Floor(centerJ), stateCount[2],
    stateCountTotal);

  if (centerI = -1) then
  begin
    result := false;
    exit
  end;

  centerJ := crossCheckHorizontal(Floor(centerJ), Floor(centerI), stateCount[2],
    stateCountTotal);

  if ((centerJ <> -1) and (not pureBarcode or
    self.crossCheckDiagonal(Floor(centerI), Floor(centerJ), stateCount[2],
    stateCountTotal))) then
  begin
    estimatedModuleSize := stateCountTotal / 7;
    found := false;
    index := 0;
    while ((index < self.PossibleCenters.Count)) do
    begin
      center := self.PossibleCenters[index];
      if (center.aboutEquals(estimatedModuleSize, centerI, centerJ)) then
      begin
        self.PossibleCenters.Delete(index);
        self.PossibleCenters.Insert(index, center.combineEstimate(centerI,
          centerJ, estimatedModuleSize));
        found := true;
        break;

      end;
      inc(index)
    end;

    if (not found) then
    begin

      point := TFinderPattern.Create(centerJ, centerI, estimatedModuleSize);
      self.PossibleCenters.Add(point);

      // todo: 2015-10-16
      // if (self.resultPointCallback <> nil) then
      // self.resultPointCallback.Invoke(point)

    end;

    result := true;

  end;

  Result:=false;

end;

function TFinderPatternFinder.haveMultiplyConfirmedCenters: boolean;
var
  confirmedCount, max, i: Integer;
  totalModuleSize, average, totalDeviation: Single;
  pattern: TFinderPattern;
begin
  confirmedCount := 0;
  totalModuleSize := 0;

  max := self.PossibleCenters.Count;

  for pattern in PossibleCenters do
  begin
    if (pattern.Count >= CENTER_QUORUM) then
    begin
      inc(confirmedCount);
      totalModuleSize := totalModuleSize + pattern.estimatedModuleSize;
    end;
  end;

  if (confirmedCount < 3) then
  begin
    result := false;
    exit
  end;

  average := totalModuleSize / max;
  totalDeviation := 0;
  i := 0;

  while ((i < max)) do
  begin
    pattern := self.PossibleCenters[i];
    totalDeviation := totalDeviation +
      Abs(pattern.estimatedModuleSize - average);
    inc(i)
  end;

  result := (totalDeviation <= (0.05 * totalModuleSize));

end;

function TFinderPatternFinder.selectBestPatterns: TArray<TFinderPattern>;
var
  totalModuleSize, average, size, stdDev, square, limit: Single;
  center, possibleCenter, pattern: TFinderPattern;
  startSize, i: Integer;
begin
  startSize := self.PossibleCenters.Count;
  if (startSize < 3) then
  begin
    result := nil;
    exit
  end;
  if (startSize > 3) then
  begin
    totalModuleSize := 0;
    square := 0;

    for center in self.PossibleCenters do
    begin
      size := center.estimatedModuleSize;
      totalModuleSize := totalModuleSize + size;
      square := square + (size * size);
    end;

    average := totalModuleSize / startSize;
    stdDev := Sqrt((square / startSize) - (average * average));

    self.PossibleCenters.Sort(TFurthestFromAverageComparator.Create(average));
    limit := math.max((0.2 * average), stdDev);
    i := 0;
    while (((i < self.PossibleCenters.Count) and
      (self.PossibleCenters.Count > 3))) do
    begin
      pattern := self.PossibleCenters[i];
      if (Abs(pattern.estimatedModuleSize - average) > limit) then
      begin
        self.PossibleCenters.Delete(i);
        dec(i)
      end;
      inc(i)
    end
  end;

  if (self.PossibleCenters.Count > 3) then
  begin
    totalModuleSize := 0;

    for possibleCenter in self.PossibleCenters do
    begin
      totalModuleSize := totalModuleSize + possibleCenter.estimatedModuleSize;
    end;

    average := totalModuleSize / self.PossibleCenters.Count;
    self.PossibleCenters.Sort(TCenterComparator.Create(average));

    // if (PossibleCenters.Count > 4) then
    // begin
    // self.PossibleCenters.DeleteRange(4, self.PossibleCenters.Count - 1);
    // end;

  end;

  result := TArray<TFinderPattern>.Create(self.PossibleCenters[0],
    self.PossibleCenters[1], self.PossibleCenters[2]);

end;

{ TFurthestFromAverageComparator }

function TFurthestFromAverageComparator.Compare(const Left,
  Right: TFinderPattern): Integer;
var
  dA, dB: Single;
begin
  dA := Abs(Right.estimatedModuleSize - self.average);
  dB := Abs(Left.estimatedModuleSize - self.average);

  if (dA < dB) then
  begin
    result := -1;
  end
  else if (dA = dB) then
  begin
    result := 0;
  end
  else
    result := 1;

end;

constructor TFurthestFromAverageComparator.Create(f: Single);
begin
  average := f;
end;

{ TCenterComparator }

function TCenterComparator.Compare(const Left, Right: TFinderPattern): Integer;
var
  dA, dB: Single;
begin
  if (Right.Count = Left.Count) then
  begin
    dA := Abs(Right.estimatedModuleSize - self.average);
    dB := Abs(Left.estimatedModuleSize - self.average);

    if (dA < dB) then
    begin
      result := 1;
    end
    else if (dA = dB) then
    begin
      result := 0;
    end
    else
      result := -1;

    exit;
  end;

  result := (Right.Count - Left.Count);

end;

constructor TCenterComparator.Create(f: Single);
begin
  average := f;
end;

end.
