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

  * Original Author: Sean Owen
  * Delphi Implementation by K. Gossens
}

unit ZXing.Datamatrix.Internal.Detector;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Defaults,
  System.Generics.Collections,
  ZXing.Common.BitMatrix,
  ZXing.DefaultGridSampler,
  ZXing.Common.DetectorResult,
  ZXing.ResultPoint,
  ZXing.Common.GridSampler,
  ZXing.Common.Detector.MathUtils,
  ZXing.Common.Detector.WhiteRectangleDetector;

type
  /// <summary>
  /// <p>Encapsulates logic that can detect a Data Matrix Code in an image, even if the Data Matrix Code
  /// is rotated or skewed, or partially obscured.</p>
  /// </summary>
  TDataMatrixDetector = class(TObject)
  private
  var
    Fimage: TBitMatrix;
    FrectangleDetector: TWhiteRectangleDetector;

  type
    /// <summary>
    /// Simply encapsulates two points and a number of transitions between them.
    /// </summary>
    TResultPointsAndTransitions = class sealed
    public
      From: IResultPoint;
      To_: IResultPoint;
      Transitions: Integer;

      constructor Create(From: IResultPoint; To_: IResultPoint;
        Transitions: Integer);
      destructor Destroy; override;
      function ToString: String; override;
    end;

    /// <summary>
    /// Orders ResultPointsAndTransitions by number of transitions, ascending.
    /// </summary>
    TResultPointsAndTransitionsComparator = class sealed
      (TComparer<TResultPointsAndTransitions>)
    public
      function Compare(const o1, o2: TResultPointsAndTransitions)
        : Integer; override;
    end;

  var
    FtransCompare: TResultPointsAndTransitionsComparator;
  public
    constructor Create(const image: TBitMatrix); overload;
    /// <summary>
    /// Starts the search for the white rectangle around the code at (x, y)
    /// instead of at the center of the image.
    /// </summary>
    constructor Create(const image: TBitMatrix; x, y: Integer); overload;
    destructor Destroy; override;
    function detect: TDetectorResult; overload;
    /// <param name="countThroughCenters">count the modules through the
    /// centers of the outer modules instead of along the edge. This works
    /// better for rotated codes, but worse for codes with very small modules
    /// (around 4 pixels or less), so callers try both.</param>
    function detect(countThroughCenters: Boolean): TDetectorResult; overload;
    function transitionsBetween(Afrom, Ato: IResultPoint)
      : TResultPointsAndTransitions;
    function distance(a: IResultPoint; b: IResultPoint): Integer;
    procedure increment(table: TDictionary<IResultPoint, Integer>;
      key: IResultPoint);
    function sampleGrid(image: TBitMatrix; topLeft, bottomLeft, bottomRight,
      topRight: IResultPoint; dimensionX, dimensionY: Integer): TBitMatrix; overload;
    /// <param name="edge">distance in modules between the corner points and
    /// the outer edge of the code</param>
    function sampleGrid(image: TBitMatrix; topLeft, bottomLeft, bottomRight,
      topRight: IResultPoint; dimensionX, dimensionY: Integer; edge: Single)
      : TBitMatrix; overload;
    function isValid(p: IResultPoint): Boolean;
    function correctTopRight(bottomLeft, bottomRight, topLeft,
      topRight: IResultPoint; dimension: Integer): IResultPoint;
    function correctTopRightRectangular(bottomLeft, bottomRight, topLeft,
      topRight: IResultPoint; dimensionTop, dimensionRight: Integer)
      : IResultPoint;
    /// <summary>
    /// Refines the dimensions by counting transitions through the centers of
    /// the modules of the top row and right column, using the given
    /// dimensions as estimate of the module size. The results are not
    /// rounded to even numbers.
    /// </summary>
    procedure countModulesThroughCenters(topLeft, bottomLeft, bottomRight,
      topRight: IResultPoint; var dimensionTop, dimensionRight: Integer);
  end;

implementation

{ TDataMatrixDetector }

/// <summary>
/// Initializes a new instance of the <see cref="Detector"/> class.
/// </summary>
/// <param name="image">The image.</param>
constructor TDataMatrixDetector.Create(const image: TBitMatrix);
begin
  Self.Fimage := image;
  Self.FrectangleDetector := TWhiteRectangleDetector.New(image);
  Self.FtransCompare := TResultPointsAndTransitionsComparator.Create;
end;

constructor TDataMatrixDetector.Create(const image: TBitMatrix; x, y: Integer);
begin
  Self.Fimage := image;
  Self.FrectangleDetector := TWhiteRectangleDetector.New(image, 10, x, y);
  Self.FtransCompare := TResultPointsAndTransitionsComparator.Create;
end;

destructor TDataMatrixDetector.Destroy;
begin
  if (FrectangleDetector <> nil) then
    FrectangleDetector.Free;

  FtransCompare.Free;
  inherited;
end;

/// <summary>
/// <p>Detects a Data Matrix Code in an image.</p>
/// </summary>
/// <returns><see cref="DetectorResult" />encapsulating results of detecting a Data Matrix Code or null</returns>
function TDataMatrixDetector.detect(): TDetectorResult;
begin
  Result := detect(false);
end;

function TDataMatrixDetector.detect(countThroughCenters: Boolean)
  : TDetectorResult;
var
  topRight, correctedTopRight, pointA, pointB, pointC, pointD, maybeTopLeft,
    bottomLeft, bottomRight, topLeft, maybeBottomRight, point: IResultPoint;

  cornerPoints: TArray<IResultPoint>;
  bits: TBitMatrix;
  entry: TPair<IResultPoint, Integer>; // TKeyValuePair
  Transitions: TObjectList<TResultPointsAndTransitions>;
  lSideOne, lSideTwo, transBetween, transA, transB: TResultPointsAndTransitions;
  pointCount: TDictionary<IResultPoint, Integer>;

  corners: TArray<IResultPoint>;

  dimensionTop, dimensionRight, dimension, dimensionCorrected: Integer;
begin
  Result := nil;

  if (FrectangleDetector = nil) then
    // can be null, if the image is to small
    exit;

  cornerPoints := FrectangleDetector.detect();
  if (cornerPoints = nil) then
    exit;

  pointA := cornerPoints[0];
  pointB := cornerPoints[1];
  pointC := cornerPoints[2];
  pointD := cornerPoints[3];

  // Point A and D are across the diagonal from one another,
  // as are B and C. Figure out which are the solid black lines
  // by counting transitions
  Transitions := TObjectList<TResultPointsAndTransitions>.Create;
  pointCount := TDictionary<IResultPoint, Integer>.Create();
  try
    Transitions.Add(transitionsBetween(pointA, pointB));
    Transitions.Add(transitionsBetween(pointA, pointC));
    Transitions.Add(transitionsBetween(pointB, pointD));
    Transitions.Add(transitionsBetween(pointC, pointD));
    Transitions.Sort(FtransCompare);

    // Sort by number of transitions. First two will be the two solid sides; last two
    // will be the two alternating black/white sides
    lSideOne := Transitions[0];
    lSideTwo := Transitions[1];

    // Figure out which point is their intersection by tallying up the number of times we see the
    // endpoints in the four endpoints. One will show up twice.

    increment(pointCount, lSideOne.From);
    increment(pointCount, lSideOne.To_);
    increment(pointCount, lSideTwo.From);
    increment(pointCount, lSideTwo.To_);

    maybeTopLeft := nil;
    bottomLeft := nil;
    maybeBottomRight := nil;

    for entry in pointCount do
    begin
      point := entry.key;
      if (entry.Value = 2) then
        bottomLeft := point
        // this is definitely the bottom left, then -- end of two L sides
      else
      begin
        // Otherwise it's either top left or bottom right -- just assign the two arbitrarily now
        if (maybeTopLeft = nil) then
          maybeTopLeft := point
        else
          maybeBottomRight := point;
      end;
    end;

    if ((maybeTopLeft = nil) or (bottomLeft = nil) or (maybeBottomRight = nil))
    then
      exit;

    // Bottom left is correct but top left and bottom right might be switched
    corners := TArray<IResultPoint>.Create(maybeTopLeft, bottomLeft,
      maybeBottomRight);
    // Use the dot product trick to sort them out
    TResultPointHelpers.orderBestPatterns(corners);

    // Now we know which is which:
    bottomRight := corners[0];
    bottomLeft := corners[1];
    topLeft := corners[2];

    // Which point didn't we find in relation to the "L" sides? that's the top right corner
    if (not pointCount.ContainsKey(pointA)) then
      topRight := pointA
    else if (not pointCount.ContainsKey(pointB)) then
      topRight := pointB
    else if (not pointCount.ContainsKey(pointC)) then
      topRight := pointC
    else
      topRight := pointD;

    // Next determine the dimension by tracing along the top or right side and counting black/white
    // transitions. Since we start inside a black module, we should see a number of transitions
    // equal to 1 less than the code dimension. Well, actually 2 less, because we are going to
    // end on a black module:

    // The top right point is actually the corner of a module, which is one of the two black modules
    // adjacent to the white module at the top right. Tracing to that corner from either the top left
    // or bottom right should work here.
    transBetween := transitionsBetween(topLeft, topRight);
    dimensionTop := transBetween.Transitions;
    transBetween.Free;

    transBetween := transitionsBetween(bottomRight, topRight);
    dimensionRight := transBetween.Transitions;
    transBetween.Free;

    if ((dimensionTop and $01) = 1) then
      // it can't be odd, so, round... up?
      Inc(dimensionTop);
    Inc(dimensionTop, 2);

    if ((dimensionRight and $01) = 1) then
      // it can't be odd, so, round... up?
      Inc(dimensionRight);
    Inc(dimensionRight, 2);

    // Rectangular symbols are 6x16, 6x28, 10x24, 10x32, 14x32, or 14x44. If one dimension is more
    // than twice the other, it's certainly rectangular, but to cut a bit more slack we accept it as
    // rectangular if the bigger side is at least 7/4 times the other:
    if (((4 * dimensionTop) >= (7 * dimensionRight)) or
      ((4 * dimensionRight) >= (7 * dimensionTop))) then
    begin
      // The matrix is rectangular
      correctedTopRight := correctTopRightRectangular(bottomLeft, bottomRight,
        topLeft, topRight, dimensionTop, dimensionRight);
      if (correctedTopRight = nil) then
        correctedTopRight := topRight;

      transBetween := transitionsBetween(topLeft, correctedTopRight);
      dimensionTop := transBetween.Transitions;
      transBetween.Free;

      transBetween := transitionsBetween(bottomRight, correctedTopRight);
      dimensionRight := transBetween.Transitions;
      transBetween.Free;

      if ((dimensionTop and $01) = 1) then
        // it can't be odd, so, round... up?
        Inc(dimensionTop);

      if ((dimensionRight and $01) = 1) then
        // it can't be odd, so, round... up?
        Inc(dimensionRight);

      if countThroughCenters then
      begin
        countModulesThroughCenters(topLeft, bottomLeft, bottomRight,
          correctedTopRight, dimensionTop, dimensionRight);
        if Odd(dimensionTop) then
          Inc(dimensionTop);
        if Odd(dimensionRight) then
          Inc(dimensionRight);
      end;

      bits := sampleGrid(Fimage, topLeft, bottomLeft, bottomRight,
        correctedTopRight, dimensionTop, dimensionRight)
    end
    else
    begin
      // The matrix is square
      dimension := System.Math.Min(dimensionRight, dimensionTop);
      // correct top right point to match the white module
      correctedTopRight := correctTopRight(bottomLeft, bottomRight, topLeft,
        topRight, dimension);
      if (correctedTopRight = nil) then
        correctedTopRight := topRight;

      // Redetermine the dimension using the corrected top right point
      transA := transitionsBetween(topLeft, correctedTopRight);
      transB := transitionsBetween(bottomRight, correctedTopRight);
      dimensionCorrected :=
        (System.Math.Max(transA.Transitions, transB.Transitions) + 1);

      transA.Free;
      transB.Free;

      if ((dimensionCorrected and $01) = 1) then
        Inc(dimensionCorrected);

      if countThroughCenters then
      begin
        dimensionTop := dimensionCorrected;
        dimensionRight := dimensionCorrected;
        countModulesThroughCenters(topLeft, bottomLeft, bottomRight,
          correctedTopRight, dimensionTop, dimensionRight);
        // A side can be one off (e.g. 35 and 37 for 36), so use the even
        // number closest to the average of both sides (ties round up).
        dimensionCorrected := 2 * ((dimensionTop + dimensionRight + 2) div 4);
      end;

      bits := sampleGrid(Fimage, topLeft, bottomLeft, bottomRight,
        correctedTopRight, dimensionCorrected, dimensionCorrected);
    end;

    if (bits = nil) then
      exit;

    Result := TDetectorResult.Create(bits, TArray<IResultPoint>.Create(topLeft,
      bottomLeft, bottomRight, correctedTopRight));

  finally
    pointCount.Free;
    Transitions.Free;
    cornerPoints := nil;
  end;
end;

/// <summary>
/// Calculates the position of the white top right module using the output of the rectangle detector
/// for a rectangular matrix
/// </summary>
function TDataMatrixDetector.correctTopRightRectangular(bottomLeft, bottomRight,
  topLeft, topRight: IResultPoint; dimensionTop, dimensionRight: Integer)
  : IResultPoint;
var
  corr, norm, cos, sin: Single;
  c1, c2: IResultPoint;
  l1, l2: Integer;
  rpT1, rpT2: TResultPointsAndTransitions;
begin
  corr := (distance(bottomLeft, bottomRight) / dimensionTop);
  norm := distance(topLeft, topRight);
  if (norm = 0) then
  begin
    Result := nil;
    exit
  end;
  cos := ((topRight.X - topLeft.X) / norm);
  sin := ((topRight.Y - topLeft.Y) / norm);

  c1 := TResultPointHelpers.CreateResultPoint((topRight.X + (corr * cos)),
    (topRight.Y + (corr * sin)));

  corr := (distance(bottomLeft, topLeft) / dimensionRight);
  norm := distance(bottomRight, topRight);
  if (norm = 0) then
  begin
    Result := nil;
    exit;
  end;
  cos := ((topRight.X - bottomRight.X) / norm);
  sin := ((topRight.Y - bottomRight.Y) / norm);

  c2 := TResultPointHelpers.CreateResultPoint((topRight.X + (corr * cos)),
    (topRight.Y + (corr * sin)));
  if (not isValid(c1)) then
  begin
    if (isValid(c2)) then
    begin
      Result := c2;
      exit;
    end;

    Result := nil;
    exit;
  end;
  if (not isValid(c2)) then
  begin
    Result := c1;
    exit;
  end;

  rpT1 := transitionsBetween(topLeft, c1);
  rpT2 := transitionsBetween(bottomRight, c1);

  l1 := (Abs((dimensionTop - rpT1.Transitions)) +
    Abs((dimensionRight - rpT2.Transitions)));

  FreeAndNil(rpT1);
  FreeAndNil(rpT2);

  rpT1 := transitionsBetween(topLeft, c2);
  rpT2 := transitionsBetween(bottomRight, c2);

  l2 := (Abs((dimensionTop - rpT1.Transitions)) +
    Abs((dimensionRight - rpT2.Transitions)));

  FreeAndNil(rpT1);
  FreeAndNil(rpT2);

  if (l1 <= l2) then
  begin
    Result := c1;
  end
  else
  begin
    Result := c2;
  end;
end;

/// <summary>
/// Calculates the position of the white top right module using the output of the rectangle detector
/// for a square matrix
/// </summary>
function TDataMatrixDetector.correctTopRight(bottomLeft, bottomRight, topLeft,
  topRight: IResultPoint; dimension: Integer): IResultPoint;
var
  corr, cos, sin: Single;
  norm: Integer;
  c1, c2: IResultPoint;
  l1, l2: Integer;
  transA, transB: TResultPointsAndTransitions;
begin
  corr := (distance(bottomLeft, bottomRight) / dimension);
  norm := distance(topLeft, topRight);
  if (norm = 0) then
  begin
    Result := nil;
    exit;
  end;
  cos := ((topRight.X - topLeft.X) / norm);
  sin := ((topRight.Y - topLeft.Y) / norm);

  c1 := TResultPointHelpers.CreateResultPoint((topRight.X + (corr * cos)),
    (topRight.Y + (corr * sin)));

  corr := (distance(bottomLeft, topLeft) / dimension);
  norm := distance(bottomRight, topRight);
  if (norm = 0) then
  begin
    Result := nil;
    exit;
  end;
  cos := ((topRight.X - bottomRight.X) / norm);
  sin := ((topRight.Y - bottomRight.Y) / norm);

  c2 := TResultPointHelpers.CreateResultPoint((topRight.X + (corr * cos)),
    (topRight.Y + (corr * sin)));

  if (not isValid(c1)) then
  begin
    if (isValid(c2)) then
    begin
      Result := c2;
      exit;
    end;

    Result := nil;
    exit;
  end;

  if (not isValid(c2)) then
  begin
    Result := c1;
    exit;
  end;

  transA := transitionsBetween(topLeft, c1);
  transB := transitionsBetween(bottomRight, c1);
  l1 := (Abs(transA.Transitions - transB.Transitions));
  transA.Free;
  transB.Free;

  transA := transitionsBetween(topLeft, c2);
  transB := transitionsBetween(bottomRight, c2);
  l2 := (Abs(transA.Transitions - transB.Transitions));
  transA.Free;
  transB.Free;

  if (l1 <= l2) then
  begin
    Result := c1;
  end
  else
  begin
    Result := c2;
  end;

end;

procedure TDataMatrixDetector.countModulesThroughCenters(topLeft, bottomLeft,
  bottomRight, topRight: IResultPoint; var dimensionTop, dimensionRight: Integer);

  function shifted(p, towardA, towardB: IResultPoint;
    fractionA, fractionB: Single): IResultPoint;
  begin
    Result := TResultPointHelpers.CreateResultPoint(
      p.X + (towardA.X - p.X) * fractionA + (towardB.X - p.X) * fractionB,
      p.Y + (towardA.Y - p.Y) * fractionA + (towardB.Y - p.Y) * fractionB);
  end;

var
  halfModuleTop, halfModuleRight: Single;
  centerTopLeft, centerTopRight, centerBottomRight: IResultPoint;
  trans: TResultPointsAndTransitions;
begin
  // Counting along the edge is unstable for rotated codes: the line runs over
  // the border of the modules and picks up extra transitions. Count through
  // the centers of the outer modules instead.
  if (dimensionTop <= 0) or (dimensionRight <= 0) then
    exit;

  halfModuleTop := 0.5 / dimensionTop;
  halfModuleRight := 0.5 / dimensionRight;
  centerTopLeft := shifted(topLeft, topRight, bottomLeft, halfModuleTop,
    halfModuleRight);
  centerTopRight := shifted(topRight, topLeft, bottomRight, halfModuleTop,
    halfModuleRight);
  centerBottomRight := shifted(bottomRight, bottomLeft, topRight, halfModuleTop,
    halfModuleRight);

  // The top row and right column alternate black/white over their full
  // length, so there is one transition less than there are modules.
  trans := transitionsBetween(centerTopLeft, centerTopRight);
  dimensionTop := trans.Transitions + 1;
  trans.Free;

  trans := transitionsBetween(centerBottomRight, centerTopRight);
  dimensionRight := trans.Transitions + 1;
  trans.Free;
end;

function TDataMatrixDetector.isValid(p: IResultPoint): Boolean;
begin
  Result := ((((p.X >= 0) and (p.X < Fimage.Width)) and (p.Y > 0)) and
    (p.Y < Fimage.Height))
end;

// L2 distance
function TDataMatrixDetector.distance(a: IResultPoint; b: IResultPoint)
  : Integer;
begin
  Result := TMathUtils.round(TResultPointHelpers.distance(a, b))
end;

/// <summary>
/// Increments the Integer associated with a key by one.
/// </summary>
procedure TDataMatrixDetector.increment
  (table: TDictionary<IResultPoint, Integer>; key: IResultPoint);
var
  Value: Integer;
begin
  if (table.ContainsKey(key)) then
  begin
    Value := table[key];
    table[key] := (Value + 1);
  end
  else
    table.Add(key, 1); // table[key] := 1;
end;

function TDataMatrixDetector.sampleGrid(image: TBitMatrix;
  topLeft, bottomLeft, bottomRight, topRight: IResultPoint;
  dimensionX, dimensionY: Integer): TBitMatrix;
begin
  // The corner points lie near the outer edge of the code, not in the center
  // of the corner modules. Mapping them to 0.5 put the sample points up to half
  // a module off near the edges, which misses the dots of dot-peen codes.
  // All test images decode with values between 0.15 and 0.30.
  Result := sampleGrid(image, topLeft, bottomLeft, bottomRight, topRight,
    dimensionX, dimensionY, 0.25);
end;

function TDataMatrixDetector.sampleGrid(image: TBitMatrix;
  topLeft, bottomLeft, bottomRight, topRight: IResultPoint;
  dimensionX, dimensionY: Integer; edge: Single): TBitMatrix;
begin
  // TGridSampler.instance
  Result := TDefaultGridSampler.sampleGrid(image, dimensionX, dimensionY, edge,
    edge, (dimensionX - edge), edge, (dimensionX - edge), (dimensionY - edge),
    edge, (dimensionY - edge), topLeft.X, topLeft.Y, topRight.X, topRight.Y,
    bottomRight.X, bottomRight.Y, bottomLeft.X, bottomLeft.Y);
end;

/// <summary>
/// Counts the number of black/white transitions between two points, using something like Bresenham's algorithm.
/// </summary>
function TDataMatrixDetector.transitionsBetween(Afrom, Ato: IResultPoint)
  : TResultPointsAndTransitions;
var
  temp, fromX, fromY, toX, toY: Integer;
  steep: Boolean;
  dx, dy: Int64;
  xstep, ystep, Transitions: Integer;
  error: Int64;
  inBlack, isBlack: Boolean;
  X, Y: Integer;
begin
  // See QR Code Detector, sizeOfBlackWhiteBlackRun()
  fromX := Trunc(Afrom.X);
  fromY := Trunc(Afrom.Y);
  toX := Trunc(Ato.X);
  toY := Trunc(Ato.Y);
  steep := (Abs((toY - fromY)) > Abs((toX - fromX)));
  if (steep) then
  begin
    temp := fromX;
    fromX := fromY;
    fromY := temp;
    temp := toX;
    toX := toY;
    toY := temp;
  end;

  dx := Abs(toX - fromX);
  dy := Abs(toY - fromY);
  error := TMathUtils.Asr(-dx, 1);
  if (fromY < toY) then
    ystep := 1
  else
    ystep := -1;
  if (fromX < toX) then
    xstep := 1
  else
    xstep := -1;
  Transitions := 0;
  if steep then
    inBlack := Fimage[fromY, fromX]
  else
    inBlack := Fimage[fromX, fromY];

  X := fromX;
  Y := fromY;
  while ((X <> toX)) do
  begin
    if steep then
      isBlack := Fimage[Y, X]
    else
      isBlack := Fimage[X, Y];

    if (isBlack <> inBlack) then
    begin
      Inc(Transitions);
      inBlack := isBlack;
    end;
    Inc(error, dy);
    if (error > 0) then
    begin
      if (Y = toY) then
        break;
      Inc(Y, ystep);
      Dec(error, dx);
    end;
    Inc(X, xstep)
  end;

  Result := TResultPointsAndTransitions.Create(Afrom, Ato, Transitions);
end;

{ TResultPointsAndTransitions }

constructor TDataMatrixDetector.TResultPointsAndTransitions.Create
  (From: IResultPoint; To_: IResultPoint; Transitions: Integer);
begin
  Self.From := From;
  Self.To_ := To_;
  Self.Transitions := Transitions;
end;

destructor TDataMatrixDetector.TResultPointsAndTransitions.Destroy;
begin

// if (Assigned(Self.From)) then
//    Self.From := nil;
//
// if (Assigned(Self.To_)) then
//    Self.To_:=nil;

  inherited;
end;

function TDataMatrixDetector.TResultPointsAndTransitions.ToString: string;
begin
  Result := From.ToString + '/' + To_.ToString + '/' + IntToStr(Transitions);
end;

function TDataMatrixDetector.TResultPointsAndTransitionsComparator.Compare
  (const o1, o2: TResultPointsAndTransitions): Integer;
begin
  Result := (o1.Transitions - o2.Transitions);
end;

end.
