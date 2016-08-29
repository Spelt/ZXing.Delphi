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

unit ZXing.QrCode.Internal.Detector;

interface

uses
  System.SysUtils,
  System.Math,
  Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.ResultPoint,
  ZXing.Common.DetectorResult,
  ZXing.DecodeHintType,
  ZXing.Common.PerspectiveTransform,
  ZXing.QrCode.Internal.FinderPatternInfo,
  ZXing.QrCode.Internal.FinderPatternFinder,
  ZXing.QrCode.Internal.FinderPattern,
  ZXing.QrCode.Internal.AlignmentPattern,
  ZXing.QrCode.Internal.AlignmentPatternFinder,
  ZXing.QrCode.Internal.Version,
  ZXing.DefaultGridSampler,
  ZXing.Common.Detector.MathUtils;

type
  /// <summary>
  /// <p>Encapsulates logic that can detect a QR Code in an image, even if the QR Code
  /// is rotated or skewed, or partially obscured.</p>
  /// </summary>
  TDetector = class
  private
    FImage: TBitMatrix;
    FResultPointCallback: TResultPointCallback;

    function calculateModuleSizeOneWay(pattern: IResultpoint;
      otherPattern: IResultpoint): Single;

    class function computeDimension(topLeft: IResultpoint;
      topRight: IResultpoint; bottomLeft: IResultpoint; moduleSize: Single;
      var dimension: Integer): boolean; static;

    class function createTransform(const topLeft, topRight, bottomLeft,
      AlignmentPattern: IResultpoint; const dimension: Integer)
      : TPerspectiveTransform; static;

    class function sampleGrid(image: TBitMatrix;
      transform: TPerspectiveTransform; dimension: Integer): TBitMatrix; static;
    function sizeOfBlackWhiteBlackRun(fromX: Integer; fromY: Integer;
      toX: Integer; toY: Integer): Single;
    function sizeOfBlackWhiteBlackRunBothWays(fromX: Integer; fromY: Integer;
      toX: Integer; toY: Integer): Single;
  protected
    function calculateModuleSize(const topLeft, topRight,
      bottomLeft: IResultPoint): Single; virtual;

    function findAlignmentInRegion(const overallEstModuleSize: Single;
      const estAlignmentX, estAlignmentY: Integer;
      const allowanceFactor: Single): IAlignmentPattern;

    /// <summary>
    /// Processes the finder pattern info.
    /// </summary>
    /// <param name="info">The info.</param>
    /// <returns></returns>
    function processFinderPatternInfo(const info: TFinderPatternInfo)
      : TDetectorResult; virtual;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="Detector"/> class.
    /// </summary>
    /// <param name="image">The image.</param>
    constructor Create(const image: TBitMatrix);
    destructor Destroy; override;

    /// <summary>
    /// <p>Detects a QR Code in an image.</p>
    /// </summary>
    /// <returns>
    /// <see cref="TDetectorResult"/> encapsulating results of detecting a QR Code
    /// </returns>
    function detect(): TDetectorResult; overload;

    /// <summary>
    /// <p>Detects a QR Code in an image.</p>
    /// </summary>
    /// <param name="hints">optional hints to detector</param>
    /// <returns>
    /// <see cref="TDetectorResult"/> encapsulating results of detecting a QR Code
    /// </returns>
    function detect(const hints: TDictionary<TDecodeHintType, TObject>)
      : TDetectorResult; overload;

    property image: TBitMatrix read FImage;
    property ResultPointCallback: TResultPointCallback
      read FResultPointCallback;
  end;

implementation

{ TDetector }

constructor TDetector.Create(const image: TBitMatrix);
begin
  FImage := image;
end;

destructor TDetector.Destroy;
begin
  FResultPointCallback := nil;
  inherited;
end;

function TDetector.calculateModuleSize(const topLeft, topRight,
  bottomLeft: IResultPoint): Single;
begin
  Result := ((self.calculateModuleSizeOneWay(topLeft, topRight) +
    self.calculateModuleSizeOneWay(topLeft, bottomLeft)) / 2)
end;

function TDetector.calculateModuleSizeOneWay(pattern: IResultPoint;
  otherPattern: IResultPoint): Single;
var
  moduleSizeEst1, moduleSizeEst2: Single;

begin
  moduleSizeEst1 := self.sizeOfBlackWhiteBlackRunBothWays(Floor(pattern.X),
    Floor(pattern.Y), Floor(otherPattern.X), Floor(otherPattern.Y));
  moduleSizeEst2 := self.sizeOfBlackWhiteBlackRunBothWays(Floor(otherPattern.X),
    Floor(otherPattern.Y), Floor(pattern.X), Floor(pattern.Y));

  if (moduleSizeEst1 = -1) then
  begin
    Result := (moduleSizeEst2 / 7);
    exit
  end;

  if (moduleSizeEst2 = -1) then
  begin
    Result := (moduleSizeEst1 / 7);
    exit
  end;

  Result := ((moduleSizeEst1 + moduleSizeEst2) / 14);
end;

class function TDetector.computeDimension(topLeft: IResultPoint;
  topRight: IResultPoint; bottomLeft: IResultPoint; moduleSize: Single;
  var dimension: Integer): boolean;

var
  tltrCentersDimension, tlblCentersDimension: Integer;
begin
  tltrCentersDimension :=
    round((TResultPointHelpers.distance(topLeft, topRight) / moduleSize));
  tlblCentersDimension :=
    round((TResultPointHelpers.distance(topLeft, bottomLeft) / moduleSize));
  dimension := TMathUtils.Asr
    ((tltrCentersDimension + tlblCentersDimension), 1) + 7;

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

class function TDetector.createTransform(const topLeft, topRight, bottomLeft,
  AlignmentPattern: IResultPoint; const dimension: Integer)
  : TPerspectiveTransform;
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

function TDetector.detect(const hints: TDictionary<TDecodeHintType, TObject>)
  : TDetectorResult;
var
  obj: TObject;
  info: TFinderPatternInfo;
  finder: TFinderPatternFinder;
begin
  Result := nil;

  if ((hints = nil) or
    (not hints.ContainsKey(TDecodeHintType.NEED_RESULT_POINT_CALLBACK))) then
    FResultPointCallback := nil
  else
  begin
    obj := hints[TDecodeHintType.NEED_RESULT_POINT_CALLBACK];
    if (obj is TResultPointEventObject) then
      FResultPointCallback := TResultPointEventObject(obj).Event;
  end;

  finder := TFinderPatternFinder.Create(FImage, ResultPointCallback);
  try
    info := finder.find(hints);
    if Assigned(info) then
    begin
      Result := processFinderPatternInfo(info);
      info.Free;
    end;
  finally
    finder.Free;
  end;
end;

function TDetector.processFinderPatternInfo(const info: TFinderPatternInfo)
  : TDetectorResult;
var
  topLeft, topRight, bottomLeft: IFinderPattern;
  moduleSize, bottomRightX, bottomRightY, correctionToTopLeft: Single;
  dimension, i, modulesBetweenFPCenters, estAlignmentX, estAlignmentY: Integer;
  points: TArray<IResultPoint>;
  provisionalVersion: TVersion;
  transform: TPerspectiveTransform;
  bits: TBitMatrix;
  AlignmentPattern: IAlignmentPattern;
begin
  Result := nil;

  topLeft := info.topLeft;
  topRight := info.topRight;
  bottomLeft := info.bottomLeft;

  moduleSize := calculateModuleSize(topLeft, topRight, bottomLeft);
  if (moduleSize < 1.0) then
    exit;

  if (not TDetector.computeDimension(topLeft, topRight, bottomLeft, moduleSize,
    dimension)) then
    exit;

  provisionalVersion := TVersion.getProvisionalVersionForDimension(dimension);
  if (provisionalVersion = nil) then
    exit;

  modulesBetweenFPCenters := (provisionalVersion.DimensionForVersion - 7);

  AlignmentPattern := nil;
  // Anything above version 1 has an alignment pattern
  if (Length(provisionalVersion.AlignmentPatternCenters) > 0) then
  begin
    // Guess where a "bottom right" finder pattern would have been
    bottomRightX := (topRight.X - topLeft.X) + bottomLeft.X;
    bottomRightY := (topRight.Y - topLeft.Y) + bottomLeft.Y;

    // Estimate that alignment pattern is closer by 3 modules
    // from "bottom right" to known top left location
    correctionToTopLeft := (1.0 - (3.0 / modulesBetweenFPCenters));
    estAlignmentX :=
      Floor(topLeft.X + (correctionToTopLeft * (bottomRightX - topLeft.X)));
    estAlignmentY :=
      Floor(topLeft.Y + (correctionToTopLeft * (bottomRightY - topLeft.Y)));
    i := 4;
    while ((i <= 16)) do
    begin
      AlignmentPattern := findAlignmentInRegion(moduleSize, estAlignmentX,
        estAlignmentY, i);
      if (AlignmentPattern <> nil) then
        break;
      i := (i shl 1);
    end;
    // If we didn't find alignment pattern... well try anyway without it
  end;

  transform := TDetector.createTransform(topLeft, topRight, bottomLeft,
    AlignmentPattern, dimension);
  try
    bits := TDetector.sampleGrid(FImage, transform, dimension);
  finally
    transform.Free;
  end;

  if (bits = nil) then
    exit;

  if (AlignmentPattern = nil) then
    points := TArray<IResultPoint>.Create(bottomLeft, topLeft, topRight)
  else
    points := TArray<IResultPoint>.Create(bottomLeft, topLeft, topRight,
      AlignmentPattern);

  Result := TDetectorResult.Create(bits, points);
end;

function TDetector.findAlignmentInRegion(const overallEstModuleSize: Single;
  const estAlignmentX, estAlignmentY: Integer; const allowanceFactor: Single)
  : IAlignmentPattern;
var
  allowance, alignmentAreaRightX, alignmentAreaLeftX, alignmentAreaTopY,
    alignmentAreaBottomY: Integer;
  alignmentFinder: TAlignmentPatternFinder;
  candidateResult: IAlignmentPattern;
begin
  Result := nil;

  allowance := Floor(allowanceFactor * overallEstModuleSize);
  alignmentAreaLeftX := System.Math.Max(0, (estAlignmentX - allowance));
  alignmentAreaRightX := System.Math.Min((FImage.Width - 1),
    (estAlignmentX + allowance));

  if ((alignmentAreaRightX - alignmentAreaLeftX) < (overallEstModuleSize * 3))
  then
    exit;

  alignmentAreaTopY := System.Math.Max(0, (estAlignmentY - allowance));

  alignmentAreaBottomY := System.Math.Min((FImage.Height - 1),
    (estAlignmentY + allowance));

  alignmentFinder := TAlignmentPatternFinder.Create(FImage, alignmentAreaLeftX,
    alignmentAreaTopY, (alignmentAreaRightX - alignmentAreaLeftX),
    (alignmentAreaBottomY - alignmentAreaTopY), overallEstModuleSize,
    self.ResultPointCallback);

  candidateResult := alignmentFinder.find;

  if candidateResult = nil then
    Result := nil
  else
    Result := candidateResult;

  FreeAndNil(alignmentFinder);
end;

class function TDetector.sampleGrid(image: TBitMatrix;
  transform: TPerspectiveTransform; dimension: Integer): TBitMatrix;
begin
  Result := TDefaultGridSampler.sampleGrid(image, dimension, dimension,
    transform)
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
  error := TMathUtils.Asr(-dx, 1);
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

    if ((state = 1) = FImage[realx, realy]) then
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
var
  scale, otherToX, otherToY: Integer;
begin
  Result := self.sizeOfBlackWhiteBlackRun(fromX, fromY, toX, toY);
  scale := 1;
  otherToX := (fromX - (toX - fromX));
  if (otherToX < 0) then
  begin
    scale := (fromX div (fromX - otherToX));
    otherToX := 0
  end
  else if (otherToX >= FImage.Width) then
  begin
    scale := (((FImage.Width - 1) - fromX) div (otherToX - fromX));
    otherToX := (FImage.Width - 1)
  end;
  otherToY := (fromY - ((toY - fromY) * scale));
  scale := 1;
  if (otherToY < 0) then
  begin
    scale := fromY div (fromY - otherToY);
    otherToY := 0
  end
  else if (otherToY >= FImage.Height) then
  begin
    scale := (((FImage.Height - 1) - fromY) div (otherToY - fromY));
    otherToY := (FImage.Height - 1)
  end;
  otherToX := (fromX + (otherToX - fromX) * scale);
  Result := Result + self.sizeOfBlackWhiteBlackRun(fromX, fromY, otherToX,
    otherToY);

  Result := (Result - 1);
end;

end.
