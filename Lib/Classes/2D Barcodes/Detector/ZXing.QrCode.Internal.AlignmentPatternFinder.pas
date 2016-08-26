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
  * Ported from ZXING Java Source: www.Redivivus.in (suraj.supekar@redivivus.in)
  * Delphi Implementation by E.Spelt and K. Gossens
}

unit ZXing.QrCode.Internal.AlignmentPatternFinder;

interface

uses 
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.QrCode.Internal.AlignmentPattern, // nullableType,
  ZXing.ResultPoint,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary> <p>This class attempts to find alignment patterns in a QR Code. Alignment patterns look like finder
  /// patterns but are smaller and appear at regular intervals throughout the image.</p>
  ///
  /// <p>At the moment this only looks for the bottom-right alignment pattern.</p>
  ///
  /// <p>This is mostly a simplified copy of {@link FinderPatternFinder}. It is copied,
  /// pasted and stripped down here for maximum performance but does unfortunately duplicate
  /// some code.</p>
  ///
  /// <p>This class is thread-safe but not reentrant. Each thread must allocate its own object.</p>
  ///
  /// </summary>
  TAlignmentPatternFinder = class sealed
  private
    image: TBitMatrix;
    possibleCenters: TList<IAlignmentPattern>;
    startX, startY: Integer;
    width, height: Integer;
    moduleSize: Single;
    crossCheckStateCount: TArray<Integer>;
    resultPointCallback: TResultPointCallback;

    /// <summary> Given a count of black/white/black pixels just seen and an end position,
    /// figures the location of the center of this black/white/black run.
    /// </summary>
    class function centerFromEnd(const stateCount: TArray<Integer>;
      const pEnd: Integer): Single; static;

    /// <param name="stateCount">count of black/white/black pixels just read
    /// </param>
    /// <returns> true iff the proportions of the counts is close enough to the 1/1/1 ratios
    /// used by alignment patterns to be considered a match
    /// </returns>
    function foundPatternCross(const stateCount: TArray<Integer>): Boolean;

    /// <summary>
    ///   <p>After a horizontal scan finds a potential alignment pattern, this method
    /// "cross-checks" by scanning down vertically through the center of the possible
    /// alignment pattern to see if the same proportion is detected.</p>
    /// </summary>
    /// <param name="startI">row where an alignment pattern was detected</param>
    /// <param name="centerJ">center of the section that appears to cross an alignment pattern</param>
    /// <param name="maxCount">maximum reasonable number of modules that should be
    /// observed in any reading state, based on the results of the horizontal scan</param>
    /// <param name="originalStateCountTotal">The original state count total.</param>
    /// <returns>
    /// vertical center of alignment pattern, or null if not found
    /// </returns>
    function crossCheckVertical(const startI, centerJ, maxCount,
      originalStateCountTotal: Integer): Single;

    /// <summary> <p>This is called when a horizontal scan finds a possible alignment pattern. It will
    /// cross check with a vertical scan, and if successful, will see if this pattern had been
    /// found on a previous horizontal scan. If so, we consider it confirmed and conclude we have
    /// found the alignment pattern.</p>
    ///
    /// </summary>
    /// <param name="stateCount">reading state module counts from horizontal scan
    /// </param>
    /// <param name="i">row where alignment pattern may be found
    /// </param>
    /// <param name="j">end of possible alignment pattern in row
    /// </param>
    /// <returns> {@link TAlignmentPattern} if we have found the same pattern twice, or null if not
    /// </returns>
    function handlePossibleCenter(const stateCount: TArray<Integer>;
      const i, j: Integer): IAlignmentPattern;
  public
    /// <summary> <p>Creates a finder that will look in a portion of the whole image.</p>
    ///
    /// </summary>
    /// <param name="image">image to search
    /// </param>
    /// <param name="startX">left column from which to start searching
    /// </param>
    /// <param name="startY">top row from which to start searching
    /// </param>
    /// <param name="width">width of region to search
    /// </param>
    /// <param name="height">height of region to search
    /// </param>
    /// <param name="moduleSize">estimated module size so far
    /// </param>
    constructor Create(const image: TBitMatrix; const startX, startY,
      width, height: Integer; const moduleSize: Single;
      resultPointCallback: TResultPointCallback);
    destructor Destroy; override;

    /// <summary> <p>This method attempts to find the bottom-right alignment pattern in the image. It is a bit messy since
    /// it's pretty performance-critical and so is written to be fast foremost.</p>
    ///
    /// </summary>
    /// <returns> {@link TAlignmentPattern} if found
    /// </returns>
    function find: IAlignmentPattern;
  end;

implementation

{ TAlignmentPatternFinder }

constructor TAlignmentPatternFinder.Create(const image: TBitMatrix;
  const startX, startY, width, height: Integer; const moduleSize: Single;
  resultPointCallback: TResultPointCallback);
begin
  Self.image := image;
  Self.possibleCenters := TList<IAlignmentPattern>.Create;
  Self.possibleCenters.Capacity := 5;
  Self.startX := startX;
  Self.startY := startY;
  Self.width := width;
  Self.height := height;
  Self.moduleSize := moduleSize;
  Self.crossCheckStateCount := TArray<Integer>.Create(0, 0, 0);
  Self.resultPointCallback := resultPointCallback;
end;

destructor TAlignmentPatternFinder.Destroy;
begin
  if possibleCenters<>nil then
     possibleCenters.Clear;
  FreeAndNil(self.possibleCenters);
  self.crossCheckStateCount := nil;
  self.resultPointCallback :=nil;
  inherited;
end;

function TAlignmentPatternFinder.find: IAlignmentPattern;
var
  confirmed: IAlignmentPattern;
  maxJ, middleI, iGen, i, j, currentState: Integer;
  stateCount: TArray<Integer>;
begin
  startX := Self.startX;
  height := Self.height;
  maxJ := startX + self.width;
  middleI := Self.startY + (TMathUtils.Asr(height, 1));
  // We are looking for black/white/black modules in 1:1:1 ratio;
  // this tracks the number of black/white/black modules seen so far
  stateCount := TArray<Integer>.Create(0, 0, 0);
  for iGen := 0 to Pred(height) do
  begin
    // Search from middle outwards
    if ((iGen and 1) = 0)
    then
       i := middleI + (TMathUtils.Asr(iGen + 1, 1))
    else
       i := middleI + (-1 * TMathUtils.Asr(iGen + 1, 1));

    stateCount[0] := 0;
    stateCount[1] := 0;
    stateCount[2] := 0;
    j := startX;
    // Burn off leading white pixels before anything else; if we start in the middle of
    // a white run, it doesn't make sense to count its length, since we don't know if the
    // white run continued to the left of the start point
    while ((j < maxJ) and (not Self.image[j, i])) do
      Inc(j);

    currentState := 0;
    while ((j < maxJ)) do
    begin
      if (Self.image[j, i]) then
      begin
        // Black pixel
        if (currentState = 1) then
        begin
          // Counting black pixels
          Inc(stateCount[currentState]);
        end else
        begin
          // Counting white pixels
          if (currentState = 2) then
          begin
            // A winner?
            if (Self.foundPatternCross(stateCount)) then
            begin
              // Yes
              confirmed := self.handlePossibleCenter(stateCount, i, j);
              if (confirmed <> nil) then
              begin
                result := confirmed;
                exit
              end;
            end;
            stateCount[0] := stateCount[2];
            stateCount[1] := 1;
            stateCount[2] := 0;
            currentState := 1;
          end else
          begin
            Inc(currentState);
            Inc(stateCount[currentState]);
          end;
        end;
      end else
      begin
        // White pixel
        if (currentState = 1)
        then
           // Counting black pixels
           Inc(currentState);
        Inc(stateCount[currentState])
      end;
      Inc(j)
    end;

    if (Self.foundPatternCross(stateCount)) then
    begin
      confirmed := self.handlePossibleCenter(stateCount, i, maxJ);
      if (confirmed <> nil) then
      begin
        result := confirmed;
        exit;
      end;
    end;
  end;

  // Hmm, nothing we saw was observed and confirmed twice. If we had
  // any guess at all, return it.
  if (Self.possibleCenters.Count <> 0)
  then
     Result := Self.possibleCenters[0]
  else
     Result := nil;
end;

class function TAlignmentPatternFinder.centerFromEnd(
  const stateCount: TArray<Integer>; const pEnd: Integer): Single;
begin
  Result := (pEnd - stateCount[2]) - (stateCount[1] / 2.0);
  if (Single.IsNaN(Result))
  then
     Result := -1;
end;

function TAlignmentPatternFinder.foundPatternCross(
  const stateCount: TArray<Integer>): Boolean;
var
  maxVariance: Single;
  i: Integer;
begin
  maxVariance := moduleSize / 2;
  for i := 0 to Pred(3) do
  begin
    if (Abs(self.moduleSize - stateCount[i]) >= maxVariance) then
    begin
      Result := false;
      exit;
    end;
  end;
  Result := true;
end;

function TAlignmentPatternFinder.crossCheckVertical(const startI, centerJ,
  maxCount, originalStateCountTotal: Integer): Single;
var
  maxI,
  i, stateCountTotal: Integer;
  stateCount: TArray<Integer>;
begin
  maxI := Self.image.height;
  stateCount := Self.crossCheckStateCount;
  stateCount[0] := 0;
  stateCount[1] := 0;
  stateCount[2] := 0;

  // Start counting up from center
  i := startI;
  while (((i >= 0) and self.image[centerJ, i]) and
          (stateCount[1] <= maxCount)) do
  begin
    Inc(stateCount[1]);
    Dec(i);
  end;
  // If already too many modules in this state or ran off the edge:
  if ((i < 0) or (stateCount[1] > maxCount)) then
  begin
    Result := -1;
    exit;
  end;
  while (((i >= 0) and (not Self.image[centerJ, i])) and
          (stateCount[0] <= maxCount)) do
  begin
    Inc(stateCount[0]);
    Dec(i);
  end;
  if (stateCount[0] > maxCount) then
  begin
    Result := -1;
    exit;
  end;

  // Now also count down from center
  i := (startI + 1);
  while (((i < maxI) and Self.image[centerJ, i]) and
           (stateCount[1] <= maxCount)) do
  begin
    Inc(stateCount[1]);
    Inc(i);
  end;
  if ((i = maxI) or (stateCount[1] > maxCount)) then
  begin
    Result := -1;
    exit;
  end;
  while (((i < maxI) and not self.image[centerJ, i]) and
          (stateCount[2] <= maxCount)) do
  begin
    Inc(stateCount[2]);
    Inc(i);
  end;
  if (stateCount[2] > maxCount) then
  begin
    Result := -1;
    exit;
  end;

  stateCountTotal := (stateCount[0] + stateCount[1] + stateCount[2]);
  if ((5 * Abs(stateCountTotal - originalStateCountTotal)) >=
      (2 * originalStateCountTotal)) then
  begin
    Result := -1;
    exit;
  end;

  if Self.foundPatternCross(stateCount)
  then
     Result := TAlignmentPatternFinder.centerFromEnd(stateCount, i)
  else
     Result := -1;
end;

function TAlignmentPatternFinder.handlePossibleCenter(
  const stateCount: TArray<Integer>; const i, j: Integer): IAlignmentPattern;
var
  center, point: IAlignmentPattern;
  stateCountTotal: Integer;
  estimatedModuleSize: Single;
  centerJ, centerI: Single;
begin
  stateCountTotal := (stateCount[0] + stateCount[1] + stateCount[2]);
  centerJ := TAlignmentPatternFinder.centerFromEnd(stateCount, j);
  if (centerJ = -1) then
  begin
    Result := nil;
    exit;
  end;
  centerI := crossCheckVertical(i, Floor(centerJ), (2 * stateCount[1]),
    stateCountTotal);
  if (centerI <> -1) then
  begin
    estimatedModuleSize := (stateCount[0] + stateCount[1] + stateCount[2]) / 3.0;
    for center in Self.possibleCenters do
    begin
      // Look for about the same center and module size:
      if (center.aboutEquals(estimatedModuleSize, centerI, centerJ)) then
      begin
        Result := center.combineEstimate(centerI, centerJ, estimatedModuleSize);
        exit;
      end;
    end;
    // Hadn't found this before; save it
    point := TAlignmentPatternHelpers.CreateAlignmentPattern(centerJ, centerI, estimatedModuleSize);
    Self.possibleCenters.Add(point);
    if Assigned(resultPointCallback)
    then
       Self.resultPointCallback(point);
  end;
  Result := nil;
end;

end.
