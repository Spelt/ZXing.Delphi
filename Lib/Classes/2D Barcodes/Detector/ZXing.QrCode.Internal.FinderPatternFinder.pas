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

  * Original Author: Sean Owen
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.FinderPatternFinder;

interface

uses 
  System.SysUtils,
  System.Math,
  System.Generics.Defaults,
  System.Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint,
  ZXing.DecodeHinttype,
  ZXing.QrCode.Internal.FinderPatternInfo,
  ZXing.QrCode.Internal.FinderPattern;

type
  /// <summary>
  /// <p>This class attempts to find finder patterns in a QR Code. Finder patterns are the square
  /// markers at three corners of a QR Code.</p>
  ///
  /// <p>This class is thread-safe but not reentrant. Each thread must allocate its own object.
  /// </summary>
  TFinderPatternFinder = class
  private
    const
      CENTER_QUORUM: Integer = 2;
      INTEGER_MATH_SHIFT: Integer = 8;
    var
      FImage: TBitMatrix;
      FPossibleCenters: TList<IFinderPattern>;
      FhasSkipped: Boolean;
      FCrossCheckStateCount: TArray<Integer>;
      FresultPointCallback: TResultPointCallback;

    class function centerFromEnd(stateCount: TArray<Integer>; pEnd: Integer): Single; static;

    function crossCheckDiagonal(startI: Integer; centerJ: Integer; maxCount: Integer; originalStateCountTotal: Integer): boolean;
    function crossCheckHorizontal(startJ: Integer; centerI: Integer; maxCount: Integer; originalStateCountTotal: Integer): Single;
    function crossCheckVertical(startI: Integer; centerJ: Integer; maxCount: Integer; originalStateCountTotal: Integer): Single;
    function findRowSkip: Integer;
    function haveMultiplyConfirmedCenters: boolean;
    function selectBestPatterns: TArray<IFinderPattern>;

    function CrossCheckStateCount: TArray<Integer>;
  protected
    const
      /// <summary>
      /// 1 pixel/module times 3 modules/center
      /// </summary>
      MIN_SKIP: Integer = 3;
      /// <summary>
      /// support up to version 10 for mobile clients
      /// </summary>
      MAX_MODULES: Integer = 97;
    class function foundPatternCross(const stateCount: TArray<Integer>): Boolean; static;
    function handlePossibleCenter(stateCount: TArray<Integer>; i: Integer;
      j: Integer): boolean;
    property image: TBitMatrix read FImage;
  public
    /// <summary>
    /// <p>Creates a finder that will search the image for three finder patterns.</p>
    /// </summary>
    /// <param name="image">image to search</param>
    constructor Create(const image: TBitMatrix); overload;

    /// <summary>
    /// Initializes a new instance of the <see cref="FinderPatternFinder"/> class.
    /// </summary>
    /// <param name="image">The image.</param>
    /// <param name="resultPointCallback">The result point callback.</param>
    constructor Create(const image: TBitMatrix;
      resultPointCallback: TResultPointCallback); overload;

    destructor Destroy; override;

    function find(const hints: TDictionary<TDecodeHintType, TObject>): TFinderPatternInfo; virtual;
  end;

  TFurthestFromAverageComparator = class sealed(TInterfacedObject,
    IComparer<IFinderPattern>)
  private
    average: Single;
  public
    constructor Create(f: Single);
    function Compare(const Left, Right: IFinderPattern): Integer;
  end;

  TCenterComparator = class sealed(TInterfacedObject, IComparer<IFinderPattern>)
  private
    average: Single;
  public
    constructor Create(f: Single);
    function Compare(const Left, Right: IFinderPattern): Integer;
  end;

implementation

{ TFinderPatternFinder }

constructor TFinderPatternFinder.Create(const image: TBitMatrix);
begin
  Self.Create(image, TResultPointCallback(nil));
end;

constructor TFinderPatternFinder.Create(const image: TBitMatrix;
  resultPointCallback: TResultPointCallback);
begin
  FImage := image;
  FPossibleCenters := TList<IFinderPattern>.Create;
  FCrossCheckStateCount := TArray<Integer>.Create(0, 0, 0, 0, 0);
  FresultPointCallback := resultPointCallback;
end;

destructor TFinderPatternFinder.Destroy;
begin
  FImage := nil;

  {for i := Pred(PossibleCenters.Count) downto 0 do
  begin
    PossibleCenters[i].Free;
    PossibleCenters.Delete(i);
  end;}

  FPossibleCenters.Free;
  FCrossCheckStateCount := nil;

  inherited;
end;

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

function TFinderPatternFinder.crossCheckDiagonal(startI, centerJ, maxCount,
  originalStateCountTotal: Integer): boolean;
var
  stateCount: TArray<Integer>;
  i, maxI, maxJ, stateCountTotal: Integer;
begin
  stateCount := CrossCheckStateCount;
  try
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
    while (((((startI >= i) and (centerJ >= i)) and
      not self.image[(centerJ - i), (startI - i)]) and
      (stateCount[1] <= maxCount))) do
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
      self.image[(centerJ + i), (startI + i)]) and
      (stateCount[4] < maxCount))) do
    begin
      inc(stateCount[4]);
      inc(i)
    end;
    if (stateCount[4] >= maxCount) then
    begin
      result := false;
      exit
    end;

    stateCountTotal :=
      ((((stateCount[0] + stateCount[1]) + stateCount[2]) + stateCount[3]) +
      stateCount[4]);

    Result := (Abs(stateCountTotal - originalStateCountTotal) <
      (2 * originalStateCountTotal)) and TFinderPatternFinder.foundPatternCross
      (stateCount);

  finally
    stateCount := nil;
  end;
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

  try

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

    while ((((j >= 0) and FImage[j, centerI]) and
      (stateCount[0] <= maxCount))) do
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

    while (((j < maxJ) and FImage[j, centerI]) and
      (stateCount[4] < maxCount)) do
    begin
      inc(stateCount[4]);
      inc(j)
    end;

    if (stateCount[4] >= maxCount) then
    begin
      result := -1;
      exit
    end;

    stateCountTotal :=
      ((((stateCount[0] + stateCount[1]) + stateCount[2]) + stateCount[3]) +
      stateCount[4]);

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

  finally
    stateCount := nil;
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
  try
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

    while ((((i >= 0) and FImage[centerJ, i]) and
      (stateCount[0] <= maxCount))) do
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

    stateCountTotal :=
      ((((stateCount[0] + stateCount[1]) + stateCount[2]) + stateCount[3]) +
      stateCount[4]);

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

  finally
    stateCount := nil;
  end;

  result := -1;

end;

function TFinderPatternFinder.find(
  const hints: TDictionary<TDecodeHintType, TObject>): TFinderPatternInfo;
var
  tryHarder, pureBarcode,
  done, confirmed: Boolean;
  maxI, maxJ, iSkip,
  i, currentState, j, rowSkip: Integer;
  stateCount: TArray<Integer>;
  patternInfo: TArray<IFinderPattern>;
  resultInfo: TArray<IResultPoint>;
begin
  Result := nil;

  tryHarder := (hints <> nil) and (hints.ContainsKey(ZXing.DecodeHinttype.TRY_HARDER));
  pureBarcode := (hints <> nil) and (hints.ContainsKey(ZXing.DecodeHinttype.PURE_BARCODE));
  maxI := Self.image.Height;
  maxJ := Self.image.Width;
  // We are looking for black/white/black/white/black modules in
  // 1:1:3:1:1 ratio; this tracks the number of such modules seen so far
  // Let's assume that the maximum version QR Code we support takes up 1/4 the height of the
  // image, and then account for the center being 3 modules in size. This gives the smallest
  // number of pixels the center could be, so skip this often. When trying harder, look for all
  // QR versions regardless of how dense they are.
  iSkip := (3 * maxI) div (4 * MAX_MODULES);
  if (iSkip < MIN_SKIP) or (tryHarder)
  then
     iSkip := MIN_SKIP;

  done := false;
  stateCount := TArray<Integer>.Create(0, 0, 0, 0, 0);
  try
    i := (iSkip - 1);
    while ((i < maxI) and (not done)) do
    begin
      // Get a row of black/white values
      stateCount[0] := 0;
      stateCount[1] := 0;
      stateCount[2] := 0;
      stateCount[3] := 0;
      stateCount[4] := 0;
      currentState := 0;

      j := 0;
      while (j < maxJ) do
      begin
        if (FImage[j, i]) then
        begin
          // Black pixel
          if ((currentState and 1) = 1)
          then
             Inc(currentState);
          Inc(stateCount[currentState]);
        end else
        begin
          // White pixel
          if ((currentState and 1) = 0) then
          begin
            // Counting black pixels
            if (currentState = 4) then
            begin
              // A winner?
              if TFinderPatternFinder.foundPatternCross(stateCount) then
              begin
                // Yes
                confirmed := handlePossibleCenter(stateCount, i, j);
                if (confirmed) then
                begin
                  // Start examining every other line. Checking each line turned out to be too
                  // expensive and didn't improve performance.
                  iSkip := 2;
                  if (FhasSkipped)
                  then
                     done := haveMultiplyConfirmedCenters()
                  else
                  begin
                    rowSkip := findRowSkip();
                    if (rowSkip > stateCount[2]) then
                    begin
                      // Skip rows between row of lower confirmed center
                      // and top of presumed third confirmed center
                      // but back up a bit to get a full chance of detecting
                      // it, entire width of center of finder pattern

                      // Skip by rowSkip, but back off by stateCount[2] (size of last center
                      // of pattern we saw) to be conservative, and also back off by iSkip which
                      // is about to be re-added
                      Inc(i, ((rowSkip - stateCount[2]) - iSkip));
                      j := (maxJ - 1);
                    end;
                  end;
                end else
                begin
                  stateCount[0] := stateCount[2];
                  stateCount[1] := stateCount[3];
                  stateCount[2] := stateCount[4];
                  stateCount[3] := 1;
                  stateCount[4] := 0;
                  currentState := 3;
                  continue;
                end;
                // Clear state to start looking again
                currentState := 0;
                stateCount[0] := 0;
                stateCount[1] := 0;
                stateCount[2] := 0;
                stateCount[3] := 0;
                stateCount[4] := 0
              end else
              begin
                // No, shift counts back by two
                stateCount[0] := stateCount[2];
                stateCount[1] := stateCount[3];
                stateCount[2] := stateCount[4];
                stateCount[3] := 1;
                stateCount[4] := 0;
                currentState := 3;
              end;
            end else
            begin
              Inc(currentState);
              Inc(stateCount[currentState]);
            end;
          end else
                 // Counting white pixels
                 Inc(stateCount[currentState]);
        end;
        Inc(j);
      end;

      if foundPatternCross(stateCount) then
      begin
        confirmed := Self.handlePossibleCenter(stateCount, i, maxJ);
        if (confirmed) then
        begin
          iSkip := stateCount[0];
          if (FhasSkipped)
          then
             // Found a third one
             done := self.haveMultiplyConfirmedCenters
        end;
      end;
      Inc(i, iSkip);
    end;

    patternInfo := selectBestPatterns();
    if (patternInfo = nil)
    then
       exit;

    TFinderPatternHelpers.orderBestPatterns(patternInfo);
    Result := TFinderPatternInfo.Create(patternInfo);
  finally
    resultInfo := nil;
    stateCount := nil;
    patternInfo := nil;
  end;
end;

function TFinderPatternFinder.findRowSkip: Integer;
var
  center: IFinderPattern;
  firstConfirmedCenter: IResultPoint;
begin
  Result := 0;

  if (FPossibleCenters.Count > 1) then
  begin
    firstConfirmedCenter := nil;

    for center in FPossibleCenters do
    begin
      if (center.Count >= 2) then
      begin
        if (firstConfirmedCenter <> nil) then
        begin
          FhasSkipped := true;
          Result := Floor(Abs(firstConfirmedCenter.X - center.X) -
            Abs(firstConfirmedCenter.Y - center.Y)) div 2;
          exit;
        end;
        firstConfirmedCenter := center;
      end
    end
  end;
end;

/// <param name="stateCount">count of black/white/black/white/black pixels just read
/// </param>
/// <returns> true iff the proportions of the counts is close enough to the 1/1/3/1/1 ratios
/// used by finder patterns to be considered a match
/// </returns>
class function TFinderPatternFinder.foundPatternCross(
  const stateCount: TArray<Integer>): Boolean;
var
  i, totalModuleSize,
  count, moduleSize,
  maxVariance: Integer;
begin
  Result := false;

  totalModuleSize := 0;
  for i := 0 to Pred(5) do
  begin
    count := stateCount[i];
    if (count = 0)
    then
       exit;
    Inc(totalModuleSize, count);
  end;

  if (totalModuleSize < 7)
  then
     exit;

  moduleSize := ((totalModuleSize shl 8) div 7);
  maxVariance := (moduleSize div 2);
  // Allow less than 50% variance from 1-1-3-1-1 proportions
  Result := (Abs(moduleSize - (stateCount[0] shl INTEGER_MATH_SHIFT)) < maxVariance) and
            (Abs(moduleSize - (stateCount[1] shl INTEGER_MATH_SHIFT)) < maxVariance) and
            (Abs(3 * moduleSize - (stateCount[2] shl INTEGER_MATH_SHIFT)) < (3 * maxVariance)) and
            (Abs(moduleSize - (stateCount[3] shl INTEGER_MATH_SHIFT)) < maxVariance) and
            (Abs(moduleSize - (stateCount[4] shl INTEGER_MATH_SHIFT)) < maxVariance);
end;

function TFinderPatternFinder.handlePossibleCenter(stateCount: TArray<Integer>;
  i, j: Integer): boolean;
var
  stateCountTotal, index: Integer;
  centerJ, centerI: Single;
  center, point: IFinderPattern;
  found: boolean;
  estimatedModuleSize: Single;
begin
  Result := false;

  center := nil;
  stateCountTotal := ((((stateCount[0] + stateCount[1]) + stateCount[2]) +
    stateCount[3]) + stateCount[4]);

  centerJ := TFinderPatternFinder.centerFromEnd(stateCount, j);

  if (centerJ = -1)
  then
     exit;

  centerI := self.crossCheckVertical(i, Floor(centerJ), stateCount[2],
    stateCountTotal);

  if (centerI = -1)
  then
     exit;

  centerJ := crossCheckHorizontal(Floor(centerJ), Floor(centerI), stateCount[2],
    stateCountTotal);

  if ((centerJ <> -1) and (self.crossCheckDiagonal(Floor(centerI), Floor(centerJ), stateCount[2],
    stateCountTotal))) then
  begin
    estimatedModuleSize := stateCountTotal / 7;
    found := false;
    index := 0;
    while ((index < FPossibleCenters.Count)) do
    begin
      center := FPossibleCenters[index];
      if (center.aboutEquals(estimatedModuleSize, centerI, centerJ)) then
      begin
        FPossibleCenters[index] := nil;
        FPossibleCenters.Delete(index);
        FPossibleCenters.Insert(index, center.combineEstimate(centerI, centerJ,
          estimatedModuleSize));
        found := true;
        break;
      end;

      inc(index)
    end;

    if (not found) then
    begin
      point := TFinderPatternHelpers.CreateFinderPattern(centerJ, centerI, estimatedModuleSize);
      FPossibleCenters.Add(point);

      if Assigned(FresultPointCallback)
      then
         FresultPointCallBack(point);
    end;
    Result := true;
  end;
end;

function TFinderPatternFinder.haveMultiplyConfirmedCenters: boolean;
var
  confirmedCount, max, i: Integer;
  totalModuleSize, average, totalDeviation: Single;
  pattern: IFinderPattern;
begin
  confirmedCount := 0;
  totalModuleSize := 0;

  max := FPossibleCenters.Count;

  for pattern in FPossibleCenters do
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
    pattern := FPossibleCenters[i];
    totalDeviation := totalDeviation +
      Abs(pattern.estimatedModuleSize - average);
    inc(i)
  end;

  result := (totalDeviation <= (0.05 * totalModuleSize));

end;

function TFinderPatternFinder.selectBestPatterns: TArray<IFinderPattern>;
var
	average, square, totalModuleSize: Double;
  size, stdDev, limit, avrsng: Single;
  center, possibleCenter, pattern: IFinderPattern;
  startSize, i: Integer;
  comparator: IComparer<IFinderPattern>;
begin
  startSize := FPossibleCenters.Count;
  if (startSize < 3) then
  begin
    result := nil;
    exit
  end;
  if (startSize > 3) then
  begin
    totalModuleSize := 0;
    square := 0;

    for center in FPossibleCenters do
    begin
      size := center.estimatedModuleSize;
      totalModuleSize := totalModuleSize + size;
      square := square + (size * size);
    end;

    average := totalModuleSize / startSize;
    stdDev := Sqrt((square / startSize) - (average * average));
    // this intermediate assignment is a workaround for old-gen compilers
    // that are not deallocating TFurthestFromAverageComparator instance
    // (bug verified on XE8: just declare a destructor Destroy override
    // in TFurthestFromAverageComparator put a breakpoint in it and see
    // that the destructor never gets called if  you don't use this intermediate
    // assignment)
    avrsng := average;
    comparator :=  TFurthestFromAverageComparator.Create(avrsng);
    FPossibleCenters.Sort(comparator);
    limit := System.Math.Max((0.2 * avrsng), stdDev);
    i := 0;
    while (((i < FPossibleCenters.Count) and
      (FPossibleCenters.Count > 3))) do
    begin
      pattern := FPossibleCenters[i];
      if (Abs(pattern.estimatedModuleSize - average) > limit) then
      begin
        FPossibleCenters.Delete(i);
        dec(i)
      end;
      inc(i)
    end
  end;

  if (FPossibleCenters.Count > 3) then
  begin
    totalModuleSize := 0;

    for possibleCenter in FPossibleCenters do
    begin
      totalModuleSize := totalModuleSize + possibleCenter.estimatedModuleSize;
    end;

    average := totalModuleSize / FPossibleCenters.Count;
    // this intermediate assignment is a workaround for old-gen compilers
    // that are not deallocating TCenterComparator instance
    // (bug verified on XE8: just declare a destructor Destroy override in
    // TCenterComparator,put a breakpoint in it and see that the destructor
    // never gets called if  you don't use this intermediate assignment)
    comparator :=  TCenterComparator.Create(average);

    FPossibleCenters.Sort( comparator );

    // if (PossibleCenters.Count > 4) then
    // begin
    // self.PossibleCenters.DeleteRange(4, self.PossibleCenters.Count - 1);
    // end;

  end;

  result := TArray<IFinderPattern>.Create(FPossibleCenters[0],
    FPossibleCenters[1], FPossibleCenters[2]);

end;

{ TFurthestFromAverageComparator }

function TFurthestFromAverageComparator.Compare(const Left,
  Right: IFinderPattern): Integer;
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

function TCenterComparator.Compare(const Left, Right: IFinderPattern): Integer;
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
