unit Detector;

interface

uses SysUtils, Generics.Collections, Bitmatrixx, Resultpoint,
  DetectorResult, DecodeHintType, AlignmentPattern, perspectiveTransform,
  finderpatternInfo, math, FinderPatternFinder, FinderPattern,
  AlignmentPatternFinder, Version, GridSampler, MathUtils;

type

  TDetector = class
  private

    FImage: TBitMatrix;
    FResultPointCallback: TResultPointCallback;
    function Get_Image(): TBitMatrix;
    function get_ResultPointCallback: TResultPointCallback;

    function calculateModuleSizeOneWay(pattern: TResultpoint;
      otherPattern: TResultpoint): Single;

    class function computeDimension(topLeft: TResultpoint;
      topRight: TResultpoint; bottomLeft: TResultpoint; moduleSize: Single;
      var dimension: Integer): boolean; static;

    class function createTransform(topLeft: TResultpoint;
      topRight: TResultpoint; bottomLeft: TResultpoint;
      AlignmentPattern: TResultpoint; dimension: Integer)
      : TPerspectiveTransform; static;

    class function sampleGrid(image: TBitMatrix;
      transform: TPerspectiveTransform; dimension: Integer): TBitMatrix; static;
    function sizeOfBlackWhiteBlackRun(fromX: Integer; fromY: Integer;
      toX: Integer; toY: Integer): Single;
    function sizeOfBlackWhiteBlackRunBothWays(fromX: Integer; fromY: Integer;
      toX: Integer; toY: Integer): Single;

  protected

  var
    ResultPointCallback: TResultPointCallback;

    function calculateModuleSize(topLeft: TResultpoint; topRight: TResultpoint;
      bottomLeft: TResultpoint): Single; virtual;
    function findAlignmentInRegion(overallEstModuleSize: Single;
      estAlignmentX: Integer; estAlignmentY: Integer; allowanceFactor: Single)
      : TAlignmentPattern;
    function processFinderPatternInfo(info: TFinderPatternInfo)
      : TDetectorResult; virtual;

  public
    constructor Create(image: TBitMatrix);

    function detect(): TDetectorResult; overload;
    function detect(hints: TDictionary<TDecodeHintType, TObject>)
      : TDetectorResult; overload;

  end;

implementation

// Methods
constructor TDetector.Create(image: TBitMatrix);
begin
  FImage := image
end;

function TDetector.Get_Image(): TBitMatrix;
begin
  Result := FImage;
end;

function TDetector.get_ResultPointCallback: TResultPointCallback;
begin
  Result := FResultPointCallback;
end;

function TDetector.calculateModuleSize(topLeft: TResultpoint;
  topRight: TResultpoint; bottomLeft: TResultpoint): Single;
begin
  Result := ((self.calculateModuleSizeOneWay(topLeft, topRight) +
    self.calculateModuleSizeOneWay(topLeft, bottomLeft)) / 2)
end;

function TDetector.calculateModuleSizeOneWay(pattern: TResultpoint;
  otherPattern: TResultpoint): Single;
var
  moduleSizeEst1, moduleSizeEst2: Single;

begin
  moduleSizeEst1 := self.sizeOfBlackWhiteBlackRunBothWays(round(pattern.X),
    round(pattern.Y), round(otherPattern.X), round(otherPattern.Y));
  moduleSizeEst2 := self.sizeOfBlackWhiteBlackRunBothWays(round(otherPattern.X),
    round(otherPattern.Y), round(pattern.X), round(pattern.Y));
  if (Single.IsNaN(moduleSizeEst1)) then
  begin
    Result := (moduleSizeEst2 / 7);
    exit
  end;
  if (Single.IsNaN(moduleSizeEst2)) then
  begin
    Result := (moduleSizeEst1 / 7);
    exit
  end;

  Result := ((moduleSizeEst1 + moduleSizeEst2) / 14);
end;

class function TDetector.computeDimension(topLeft: TResultpoint;
  topRight: TResultpoint; bottomLeft: TResultpoint; moduleSize: Single;
  var dimension: Integer): boolean;

var
  tltrCentersDimension, tlblCentersDimension: Integer;
begin
  tltrCentersDimension :=
    round((TResultpoint.distance(topLeft, topRight) / moduleSize));
  tlblCentersDimension :=
    round((TResultpoint.distance(topLeft, bottomLeft) / moduleSize));
  dimension := (((tltrCentersDimension + tlblCentersDimension) shr 1) + 7);

  case (dimension and 3) of
    0:
      begin
        inc(dimension);
      end;
    2:
      begin
        dec(dimension);
      end;
    3:
      begin
        begin
          Result := true;
          exit
        end
      end;
  end;

  Result := true;
end;

class function TDetector.createTransform(topLeft: TResultpoint;
  topRight: TResultpoint; bottomLeft: TResultpoint;
  AlignmentPattern: TResultpoint; dimension: Integer): TPerspectiveTransform;
var
  bottomRightX, bottomRightY, sourceBottomRightX, sourceBottomRightY,
    dimMinusThree: Single;
begin
  dimMinusThree := (dimension - 3.5);
  if (AlignmentPattern <> nil) then
  begin
    bottomRightX := AlignmentPattern.X;
    bottomRightY := AlignmentPattern.Y;
    sourceBottomRightX := (dimMinusThree - 3);
    sourceBottomRightY := sourceBottomRightX;
  end
  else
  begin
    bottomRightX := ((topRight.X - topLeft.X) + bottomLeft.X);
    bottomRightY := ((topRight.Y - topLeft.Y) + bottomLeft.Y);
    sourceBottomRightX := dimMinusThree;
    sourceBottomRightY := sourceBottomRightX;
  end;

  Result := TPerspectiveTransform.quadrilateralToQuadrilateral(3.5, 3.5,
    dimMinusThree, 3.5, sourceBottomRightX, sourceBottomRightY, 3.5,
    dimMinusThree, topLeft.X, topLeft.Y, topRight.X, topRight.Y, bottomRightX,
    bottomRightY, bottomLeft.X, bottomLeft.Y);
end;

function TDetector.detect: TDetectorResult;
begin
  Result := self.detect(nil)
end;

function TDetector.detect(hints: TDictionary<TDecodeHintType, TObject>)
  : TDetectorResult;
var
  info: TFinderPatternInfo;
  finder: TFinderPatternFinder;
begin

  if ((hints = nil) or not hints.ContainsKey
    (DecodeHintType.NEED_RESULT_POINT_CALLBACK)) then
  begin
    self.ResultPointCallback := nil;
  end
  else
  begin
    // todo: Edward 2015-10-16, does not work
    // self.resultPointCallback :=  hints[DecodeHintType.NEED_RESULT_POINT_CALLBACK] as TResultPointCallback;
  end;

  info := TFinderPatternFinder.Create(Fimage, ResultPointCallback).find(hints);

  if (info = nil) then
  begin
    Result := nil;
    exit
  end;

  Result := self.processFinderPatternInfo(info);
end;

function TDetector.findAlignmentInRegion(overallEstModuleSize: Single;
  estAlignmentX: Integer; estAlignmentY: Integer; allowanceFactor: Single)
  : TAlignmentPattern;
var
  allowance, alignmentAreaRightX, alignmentAreaLeftX, alignmentAreaTopY,
    alignmentAreaBottomY: Integer;
  alignmentFinder: TAlignmentPatternFinder;
begin

  allowance := round(allowanceFactor * overallEstModuleSize);
  alignmentAreaLeftX := math.Max(0, (estAlignmentX - allowance));
  alignmentAreaRightX := math.Min((Fimage.Width - 1),
    (estAlignmentX + allowance));
  if ((alignmentAreaRightX - alignmentAreaLeftX) < (overallEstModuleSize * 3))
  then
  begin
    Result := nil;
    exit
  end;

  alignmentAreaTopY := math.Max(0, (estAlignmentY - allowance));

  alignmentAreaBottomY := math.Min((Fimage.Height - 1),
    (estAlignmentY + allowance));

  alignmentFinder := TAlignmentPatternFinder.Create(Fimage,
    alignmentAreaLeftX, alignmentAreaTopY,
    (alignmentAreaRightX - alignmentAreaLeftX),
    (alignmentAreaBottomY - alignmentAreaTopY), overallEstModuleSize,
    self.ResultPointCallback);

  Result := alignmentFinder.find;

end;

function TDetector.processFinderPatternInfo(info: TFinderPatternInfo)
  : TDetectorResult;
var
  moduleSize, bottomRightX, bottomRightY, correctionToTopLeft: Single;
  dimension, modulesBetweenFPCenters, i, estAlignmentX, estAlignmentY: Integer;
  points: TArray<TResultpoint>;
  topLeft, topRight, bottomLeft: TFinderPattern;
  provisionalVersion: TVersion;
  transform: TPerspectiveTransform;
  bits: TBitMatrix;
  AlignmentPattern: TAlignmentPattern;

begin
  topLeft := info.topLeft;
  topRight := info.topRight;
  bottomLeft := info.bottomLeft;

  moduleSize := self.calculateModuleSize(topLeft, topRight, bottomLeft);
  if (moduleSize < 1) then
  begin
    Result := nil;
    exit
  end;

  if (not TDetector.computeDimension(topLeft, topRight, bottomLeft, moduleSize,
    dimension)) then
  begin
    Result := nil;
    exit
  end;

  provisionalVersion := TVersion.GetProvisionalVersionForDimension(dimension);
  if (provisionalVersion = nil) then
  begin
    Result := nil;
    exit
  end;

  modulesBetweenFPCenters := (provisionalVersion.DimensionForVersion - 7);
  AlignmentPattern := nil;
  if (Length(provisionalVersion.AlignmentPatternCenters) > 0) then
  begin
    bottomRightX := ((topRight.X - topLeft.X) + bottomLeft.X);
    bottomRightY := ((topRight.Y - topLeft.Y) + bottomLeft.Y);
    correctionToTopLeft := (1 - (3 div modulesBetweenFPCenters));
    estAlignmentX :=
      round(topLeft.X + (correctionToTopLeft * (bottomRightX - topLeft.X)));
    estAlignmentY :=
      round(topLeft.Y + (correctionToTopLeft * (bottomRightY - topLeft.Y)));
    i := 4;
    while ((i <= $10)) do
    begin
      AlignmentPattern := self.findAlignmentInRegion(moduleSize, estAlignmentX,
        estAlignmentY, i);
      if (AlignmentPattern <> nil) then
        break;;
      i := (i shl 1)
    end
  end;

  transform := TDetector.createTransform(topLeft, topRight, bottomLeft,
    AlignmentPattern, dimension);

  bits := TDetector.sampleGrid(Fimage, transform, dimension);

  if (bits = nil) then
  begin
    Result := nil;
    exit
  end;

  if (AlignmentPattern = nil) then
    points := TArray<TResultpoint>.Create(bottomLeft, topLeft, topRight)
  else
    points := TArray<TResultpoint>.Create(bottomLeft, topLeft, topRight,
      AlignmentPattern);

  Result := TDetectorResult.Create(bits, points);

end;

class function TDetector.sampleGrid(image: TBitMatrix;
  transform: TPerspectiveTransform; dimension: Integer): TBitMatrix;
begin
  Result := TGridSampler.sampleGrid(image, dimension, dimension, transform)
end;

function TDetector.sizeOfBlackWhiteBlackRun(fromX: Integer; fromY: Integer;
  toX: Integer; toY: Integer): Single;
var
  steep: boolean;
  temp, dx, dy, xstep, ystep, X, Y, state, xlimit, realx, realy, error: Integer;

begin
  steep := Abs(toY - fromY) > Abs(toX - fromX);
  if (steep) then
  begin
    temp := fromX;
    fromX := fromY;
    fromY := temp;
    temp := toX;
    toX := toY;
    toY := temp
  end;

  dx := Abs(toX - fromX);
  dy := Abs(toY - fromY);
  error := (-dx shr 1);
  if (fromX < toX) then
    xstep := 1
  else
    xstep := -1;

  if (fromY < toY) then
    ystep := 1
  else
    ystep := 1;

  state := 0;
  xlimit := (toX + xstep);
  X := fromX;
  Y := fromY;
  while ((X <> xlimit)) do
  begin
    if steep then
      realx := Y
    else
      realx := X;
    if steep then
      realy := X
    else
      realy := Y;

    if ((state = 1) = Fimage[realx, realy]) then
    begin
      if (state = 2) then
      begin
        Result := TMathUtils.distance(X, Y, fromX, fromY);
        exit
      end;
      inc(state)
    end;

    inc(error, dy);
    if (error > 0) then
    begin
      if (Y = toY) then
        break;;
      inc(Y, ystep);
      dec(error, dx)
    end;
    inc(X, xstep)
  end;

  if (state = 2) then
  begin
    Result := TMathUtils.distance((toX + xstep), toY, fromX, fromY);
    exit
  end;

  Result := Single.NaN;
end;

function TDetector.sizeOfBlackWhiteBlackRunBothWays(fromX: Integer;
  fromY: Integer; toX: Integer; toY: Integer): Single;
var scale, otherToX, otherToY : integer;
begin
  Result := self.sizeOfBlackWhiteBlackRun(fromX, fromY, toX, toY);
  scale := 1;
  otherToX := (fromX - (toX - fromX));
  if (otherToX < 0) then
  begin
    scale := (fromX div (fromX - otherToX));
    otherToX := 0
  end
  else if (otherToX >= Fimage.Width) then
  begin
    scale := (((Fimage.Width - 1) - fromX)
      div (otherToX - fromX));
    otherToX := (Fimage.Width - 1)
  end;
  otherToY := (fromY - ((toY - fromY) * scale));
  scale := 1;
  if (otherToY < 0) then
  begin
    scale := fromY div (fromY - otherToY);
    otherToY := 0
  end
  else if (otherToY >= Fimage.Height) then
  begin
    scale := (((Fimage.Height - 1) - fromY)
      div (otherToY - fromY));
    otherToY := (Fimage.Height - 1)
  end;
  otherToX := (fromX + (otherToX - fromX) * scale);
  result := Result + self.sizeOfBlackWhiteBlackRun(fromX, fromY, otherToX, otherToY);

  Result := (Result - 1);
end;

end.
