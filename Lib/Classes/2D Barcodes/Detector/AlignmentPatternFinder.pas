unit AlignmentPatternFinder;

interface

uses sysutils, BitMatrixx, nullableType, AlignmentPattern,
  ResultPoint, generics.collections, MathUtils;

type

  TAlignmentPatternFinder = class sealed
  private
    crossCheckStateCount: TArray<Integer>;
    height: Integer;
    image: TBitMatrix;
    moduleSize: Single;
    width: Integer;
    startX, startY: Integer;
    possibleCenters: TList<TAlignmentPattern>;
    resultPointCallback: TResultPointCallback;

    class function centerFromEnd(stateCount: TArray<Integer>; pEnd: Integer)
      : Single; static;
    function crossCheckVertical(startI: Integer; centerJ: Integer;
      maxCount: Integer; originalStateCountTotal: Integer): Nullable<Single>;
    function foundPatternCross(stateCount: TArray<Integer>): boolean;
    function handlePossibleCenter(stateCount: TArray<Integer>; i: Integer;
      j: Integer): TAlignmentPattern;

  public
    constructor Create(image: TBitMatrix; startX: Integer; startY: Integer;
      width: Integer; height: Integer; moduleSize: Single;
      resultPointCallback: TResultPointCallback);
    function find: TAlignmentPattern;

  end;

implementation

{ TAlignmentPatternFinder }

class function TAlignmentPatternFinder.centerFromEnd
  (stateCount: TArray<Integer>; pEnd: Integer): Single;
begin
  result := (pEnd - stateCount[2]) - (stateCount[1] / 2);
end;

constructor TAlignmentPatternFinder.Create(image: TBitMatrix;
  startX, startY, width, height: Integer; moduleSize: Single;
  resultPointCallback: TResultPointCallback);
begin
  self.image := image;
  self.possibleCenters := TList<TAlignmentPattern>.Create();
  self.possibleCenters.Capacity := 5;
  self.startX := startX;
  self.startY := startY;
  self.width := width;
  self.height := height;
  self.moduleSize := moduleSize;
  self.crossCheckStateCount := TArray<Integer>.Create();
  SetLength(self.crossCheckStateCount, 3);
  self.resultPointCallback := resultPointCallback
end;

function TAlignmentPatternFinder.crossCheckVertical(startI, centerJ, maxCount,
  originalStateCountTotal: Integer): Nullable<Single>;
var
  maxI, i, stateCountTotal: Integer;
  stateCount: TArray<Integer>;

begin
  maxI := self.image.height;
  stateCount := self.crossCheckStateCount;
  stateCount[0] := 0;
  stateCount[1] := 0;
  stateCount[2] := 0;
  i := startI;
  while ((((i >= 0) and self.image[centerJ, i]) and
    (stateCount[1] <= maxCount))) do
  begin
    inc(stateCount[1]);
    dec(i)
  end;

  if ((i < 0) or (stateCount[1] > maxCount)) then
  begin
    result := nil;
    exit
  end;

  while ((((i >= 0) and not self.image[centerJ, i]) and
    (stateCount[0] <= maxCount))) do
  begin
    inc(stateCount[0]);
    dec(i)
  end;

  if (stateCount[0] > maxCount) then
  begin
    result := nil;
    exit
  end;

  i := (startI + 1);
  while ((((i < maxI) and self.image[centerJ, i]) and
    (stateCount[1] <= maxCount))) do
  begin
    inc(stateCount[1]);
    inc(i)
  end;

  if ((i = maxI) or (stateCount[1] > maxCount)) then
  begin
    result := nil;
    exit
  end;

  while ((((i < maxI) and not self.image[centerJ, i]) and
    (stateCount[2] <= maxCount))) do
  begin
    inc(stateCount[2]);
    inc(i)
  end;

  if (stateCount[2] > maxCount) then
  begin
    result := nil;
    exit
  end;

  stateCountTotal := ((stateCount[0] + stateCount[1]) + stateCount[2]);
  if ((5 * Abs(stateCountTotal - originalStateCountTotal)) >=
    (2 * originalStateCountTotal)) then
  begin
    result := nil;
    exit
  end;

  if self.foundPatternCross(stateCount) then
  begin
    result := TAlignmentPatternFinder.centerFromEnd(stateCount, i);
    exit;
  end;

  result := nil;
end;

function TAlignmentPatternFinder.find: TAlignmentPattern;
var
  confirmed: TAlignmentPattern;
  maxJ, middleI, iGen, i, j, currentState: Integer;
  stateCount: TArray<Integer>;
begin
  startX := self.startX;
  height := self.height;
  maxJ := startX + self.width;
  middleI := self.startY + (TMathUtils.Asr(height, 1));
  SetLength(stateCount, 3);
  iGen := 0;

  while ((iGen < height)) do
  begin

    if ((iGen and 1) = 0) then
    begin
      i := middleI + (TMathUtils.Asr(iGen + 1, 1));
    end
    else
    begin
      i := middleI + (-1 * TMathUtils.Asr(iGen + 1, 1));
    end;

    stateCount[0] := 0;
    stateCount[1] := 0;
    stateCount[2] := 0;
    j := startX;

    while (((j < maxJ) and not self.image[j, i])) do
    begin
      inc(j)
    end;

    currentState := 0;
    while ((j < maxJ)) do
    begin

      if (self.image[j, i]) then
      begin

        if (currentState = 1) then
        begin
          inc(stateCount[currentState]);
        end
        else if (currentState = 2) then
        begin
          if (self.foundPatternCross(stateCount)) then
          begin
            confirmed := self.handlePossibleCenter(stateCount, i, j);
            if (confirmed <> nil) then
            begin
              result := confirmed;
              exit
            end
          end;
          stateCount[0] := stateCount[2];
          stateCount[1] := 1;
          stateCount[2] := 0;
          currentState := 1;
        end
        else
        begin
          inc(currentState);
          inc(stateCount[currentState]);
        end;

      end
      else
      begin
        if (currentState = 1) then
          inc(currentState);

        inc(stateCount[currentState])
      end;

      inc(j)
    end;

    if (self.foundPatternCross(stateCount)) then
    begin
      confirmed := self.handlePossibleCenter(stateCount, i, maxJ);
      if (confirmed <> nil) then
      begin
        result := confirmed;
        exit
      end
    end;

    inc(iGen)

  end;

  if (self.possibleCenters.Count <> 0) then
  begin
    result := self.possibleCenters[0];
    exit
  end;

  result := nil;

end;

function TAlignmentPatternFinder.foundPatternCross
  (stateCount: TArray<Integer>): boolean;
var
  maxVariance: Single;
  i: Integer;
begin
  maxVariance := moduleSize / 2;
  i := 0;
  while ((i < 3)) do
  begin
    if (Abs(self.moduleSize - stateCount[i]) >= maxVariance) then
    begin
      result := false;
      exit
    end;
    inc(i)
  end;

  result := true;
end;

function TAlignmentPatternFinder.handlePossibleCenter
  (stateCount: TArray<Integer>; i, j: Integer): TAlignmentPattern;
var
  center, point: TAlignmentPattern;
  stateCountTotal: Integer;
  estimatedModuleSize: Single;
  centerJ, centerI: Nullable<Single>;

begin
  stateCountTotal := ((stateCount[0] + stateCount[1]) + stateCount[2]);
  centerJ := TAlignmentPatternFinder.centerFromEnd(stateCount, j);

  if (centerJ.HasValue) then
  begin

    centerI := self.crossCheckVertical(i, trunc(centerJ.Value),
      (2 * stateCount[1]), stateCountTotal);

    if (centerI.HasValue) then
    begin

      estimatedModuleSize :=
        (stateCount[0] + stateCount[1] + stateCount[2]) / 3;

      for center in self.possibleCenters do
      begin
        if (center.aboutEquals(estimatedModuleSize, centerI.Value,
          centerJ.Value)) then
        begin
          result := center.combineEstimate(centerI.Value, centerJ.Value,
            estimatedModuleSize);
          exit
        end

      end;

      point := TAlignmentPattern.Create(centerJ.Value, centerI.Value,
        estimatedModuleSize);

      self.possibleCenters.Add(point);

      if (assigned(resultPointCallback)) then
      begin
        self.resultPointCallback(point);
      end;
    end
  end;

  result := nil;

end;

end.
