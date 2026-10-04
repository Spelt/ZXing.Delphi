unit ZXing.Common.BitMatrixCursor;

{
  * Copyright 2020 Axel Waggershauser
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

  * Ported from zxing-cpp (BitMatrixCursor.h): a position inside an image and
  * a direction it can advance towards, discrete (TBitMatrixCursorI: steps
  * horizontal, vertical or diagonal) or in Bresenham style
  * (TBitMatrixCursorF).
}

interface

uses
  System.Types,
  ZXing.Common.BitMatrix,
  ZXing.Common.Geometry;

const
  // the value of a pixel: outside the image, white or black
  CURSOR_INVALID = -1;
  CURSOR_WHITE = 0;
  CURSOR_BLACK = 1;

  // a direction relative to the current one
  DIR_LEFT = -1;
  DIR_RIGHT = 1;

type
  TBitMatrixCursorI = record
  public
    img: TBitMatrix;
    p: TPoint; // current position
    d: TPoint; // current direction

    class function Create(image: TBitMatrix; const pos, dir: TPoint)
      : TBitMatrixCursorI; static;

    /// <summary>CURSOR_INVALID outside the image, otherwise CURSOR_WHITE or
    /// CURSOR_BLACK.</summary>
    function TestAt(const q: TPoint): Integer;
    function BlackAt(const q: TPoint): Boolean;
    function WhiteAt(const q: TPoint): Boolean;
    function IsIn: Boolean;
    function IsBlack: Boolean;
    function IsWhite: Boolean;

    function Front: TPoint;
    function Back: TPoint;
    function Left: TPoint;
    function Right: TPoint;
    /// <summary>dir: DIR_LEFT or DIR_RIGHT.</summary>
    function Direction(dir: Integer): TPoint;
    procedure TurnBack;
    procedure TurnLeft;
    procedure TurnRight;
    procedure Turn(dir: Integer);
    function TurnedBack: TBitMatrixCursorI;

    /// <summary>The value at the current position when the pixel in
    /// direction dd has another value, CURSOR_INVALID otherwise.</summary>
    function EdgeAt(const dd: TPoint): Integer;
    function EdgeAtFront: Integer;
    function EdgeAtBack: Integer;
    function EdgeAtLeft: Integer;
    function EdgeAtRight: Integer;
    function EdgeAtDir(dir: Integer): Integer;

    function Step(s: Integer = 1): Boolean;
    /// <summary>Advances to one step behind the next (or nth) edge, at most
    /// range steps (0: unlimited); with backup one step less. Returns the
    /// number of steps taken, or 0 when moved outside of range/image.
    /// </summary>
    function StepToEdge(nth: Integer = 1; range: Integer = 0;
      backup: Boolean = false): Integer;
    function StepAlongEdge(dir: Integer; skipCorner: Boolean = false): Boolean;
    /// <summary>Reads count bars and spaces (readPattern), max: maximum total
    /// width (0: unlimited). An element is 0 when the pattern ended before.
    /// </summary>
    function ReadPattern(count: Integer; max: Integer = 0): TArray<Integer>;
    /// <summary>Like ReadPattern, but first skips at most maxWhitePrefix
    /// white pixels; nil when there are more.</summary>
    function ReadPatternFromBlack(count, maxWhitePrefix: Integer;
      max: Integer = 0): TArray<Integer>;
  end;

  TBitMatrixCursorF = record
  public
    img: TBitMatrix;
    p: TPointD; // current position
    d: TPointD; // current direction (Bresenham)

    class function Create(image: TBitMatrix; const pos, dir: TPointD)
      : TBitMatrixCursorF; static;
    function TestAt(const q: TPointD): Integer;
    function IsIn: Boolean;
    function IsBlack: Boolean;
    function IsWhite: Boolean;
    function Back: TPointD;
    procedure SetDirection(const dir: TPointD);
    procedure TurnBack;
    function TurnedBack: TBitMatrixCursorF;
    function Step(s: Double = 1): Boolean;
    function StepToEdge(nth: Integer = 1; range: Integer = 0;
      backup: Boolean = false): Integer;
    function Left: TPointD;
    function Right: TPointD;
    procedure TurnRight;
    function MovedBy(const o: TPointD): TBitMatrixCursorF;
    /// <summary>The value of the pixel when the next one in front of it has
    /// another value, else CURSOR_INVALID.</summary>
    function EdgeAtFront: Integer;
    /// <summary>The widths of the next count runs, at most max pixels in
    /// total (0: no limit); with min the result is empty when they are
    /// less than min pixels and the cursor skips on (in steps of 2 edges)
    /// to min pixels (zxing-cpp's readPattern).</summary>
    function ReadPattern(count: Integer; max: Integer = 0;
      min: Integer = 0): TArray<Integer>;
    /// <summary>ReadPattern from the next black pixel, at most
    /// maxWhitePrefix pixels away.</summary>
    function ReadPatternFromBlack(count, maxWhitePrefix: Integer;
      max: Integer = 0; min: Integer = 0): TArray<Integer>;
  end;

  /// <summary>Counts the steps of a TBitMatrixCursorI to the next pixel with
  /// another value, fast (zxing-cpp's FastEdgeToEdgeCounter).</summary>
  TFastEdgeToEdgeCounter = record
  private
    img: TBitMatrix;
    x, y, dx, dy, stepsToBorder: Integer;
  public
    class function Create(const cur: TBitMatrixCursorI)
      : TFastEdgeToEdgeCounter; static;
    /// <summary>The steps to the next pixel with another value (or to just
    /// outside the image), at most range; 0 when there is none within range.
    /// </summary>
    function StepToNextEdge(range: Integer): Integer;
  end;

function PointI(x, y: Integer): TPoint; inline;
/// <summary>The center of pixel p.</summary>
function CenteredI(const p: TPoint): TPointD; inline;
/// <summary>Truncated like zxing-cpp's PointI(PointF).</summary>
function ToPointI(const p: TPointD): TPoint; inline;

implementation

uses
  System.Math;

function PointI(x, y: Integer): TPoint;
begin
  Result.X := x;
  Result.Y := y;
end;

function CenteredI(const p: TPoint): TPointD;
begin
  Result := PointD(p.X + 0.5, p.Y + 0.5);
end;

function ToPointI(const p: TPointD): TPoint;
begin
  Result.X := Trunc(p.X);
  Result.Y := Trunc(p.Y);
end;

{ TBitMatrixCursorI }

class function TBitMatrixCursorI.Create(image: TBitMatrix;
  const pos, dir: TPoint): TBitMatrixCursorI;
begin
  Result.img := image;
  Result.p := pos;
  Result.d := dir;
end;

function TBitMatrixCursorI.TestAt(const q: TPoint): Integer;
begin
  if (q.X < 0) or (q.X >= img.Width) or (q.Y < 0) or (q.Y >= img.Height) then
    Result := CURSOR_INVALID
  else
    Result := Ord(img[q.X, q.Y]);
end;

function TBitMatrixCursorI.BlackAt(const q: TPoint): Boolean;
begin
  Result := TestAt(q) = CURSOR_BLACK;
end;

function TBitMatrixCursorI.WhiteAt(const q: TPoint): Boolean;
begin
  Result := TestAt(q) = CURSOR_WHITE;
end;

function TBitMatrixCursorI.IsIn: Boolean;
begin
  Result := TestAt(p) <> CURSOR_INVALID;
end;

function TBitMatrixCursorI.IsBlack: Boolean;
begin
  Result := BlackAt(p);
end;

function TBitMatrixCursorI.IsWhite: Boolean;
begin
  Result := WhiteAt(p);
end;

function TBitMatrixCursorI.Front: TPoint;
begin
  Result := d;
end;

function TBitMatrixCursorI.Back: TPoint;
begin
  Result := PointI(-d.X, -d.Y);
end;

function TBitMatrixCursorI.Left: TPoint;
begin
  Result := PointI(d.Y, -d.X);
end;

function TBitMatrixCursorI.Right: TPoint;
begin
  Result := PointI(-d.Y, d.X);
end;

function TBitMatrixCursorI.Direction(dir: Integer): TPoint;
begin
  var r := Right;
  Result := PointI(dir * r.X, dir * r.Y);
end;

procedure TBitMatrixCursorI.TurnBack;
begin
  d := Back;
end;

procedure TBitMatrixCursorI.TurnLeft;
begin
  d := Left;
end;

procedure TBitMatrixCursorI.TurnRight;
begin
  d := Right;
end;

procedure TBitMatrixCursorI.Turn(dir: Integer);
begin
  d := Direction(dir);
end;

function TBitMatrixCursorI.TurnedBack: TBitMatrixCursorI;
begin
  Result := TBitMatrixCursorI.Create(img, p, Back);
end;

function TBitMatrixCursorI.EdgeAt(const dd: TPoint): Integer;
begin
  var v := TestAt(p);
  if (TestAt(PointI(p.X + dd.X, p.Y + dd.Y)) <> v) then
    Result := v
  else
    Result := CURSOR_INVALID;
end;

function TBitMatrixCursorI.EdgeAtFront: Integer;
begin
  Result := EdgeAt(Front);
end;

function TBitMatrixCursorI.EdgeAtBack: Integer;
begin
  Result := EdgeAt(Back);
end;

function TBitMatrixCursorI.EdgeAtLeft: Integer;
begin
  Result := EdgeAt(Left);
end;

function TBitMatrixCursorI.EdgeAtRight: Integer;
begin
  Result := EdgeAt(Right);
end;

function TBitMatrixCursorI.EdgeAtDir(dir: Integer): Integer;
begin
  Result := EdgeAt(Direction(dir));
end;

function TBitMatrixCursorI.Step(s: Integer): Boolean;
begin
  p := PointI(p.X + s * d.X, p.Y + s * d.Y);
  Result := IsIn;
end;

function TBitMatrixCursorI.StepToEdge(nth, range: Integer;
  backup: Boolean): Integer;
begin
  var steps := 0;
  var lv := TestAt(p);
  while (nth > 0) and ((range = 0) or (steps < range)) and
    (lv <> CURSOR_INVALID) do
  begin
    Inc(steps);
    var v := TestAt(PointI(p.X + steps * d.X, p.Y + steps * d.Y));
    if (lv <> v) then
    begin
      lv := v;
      Dec(nth);
    end;
  end;
  if backup then
    Dec(steps);
  p := PointI(p.X + steps * d.X, p.Y + steps * d.Y);
  if (nth = 0) then
    Result := steps
  else
    Result := 0;
end;

function TBitMatrixCursorI.StepAlongEdge(dir: Integer;
  skipCorner: Boolean): Boolean;
begin
  if (EdgeAtDir(dir) = CURSOR_INVALID) then
    Turn(dir)
  else if (EdgeAtFront <> CURSOR_INVALID) then
  begin
    Turn(-dir);
    if (EdgeAtFront <> CURSOR_INVALID) then
    begin
      Turn(-dir);
      if (EdgeAtFront <> CURSOR_INVALID) then
        exit(false);
    end;
  end;

  Result := Step;
  if Result and skipCorner and (EdgeAtDir(dir) = CURSOR_INVALID) then
  begin
    Turn(dir);
    Result := Step;
  end;
end;

function TBitMatrixCursorI.ReadPattern(count, max: Integer): TArray<Integer>;
begin
  SetLength(Result, count);
  for var i := 0 to count - 1 do
  begin
    Result[i] := StepToEdge(1, max);
    if (Result[i] = 0) then
      exit;
    if (max <> 0) then
      Dec(max, Result[i]);
  end;
end;

function TBitMatrixCursorI.ReadPatternFromBlack(count, maxWhitePrefix,
  max: Integer): TArray<Integer>;
begin
  if (maxWhitePrefix <> 0) and IsWhite and (StepToEdge(1, maxWhitePrefix) = 0)
  then
    exit(nil);
  Result := ReadPattern(count, max);
end;

{ TBitMatrixCursorF }

class function TBitMatrixCursorF.Create(image: TBitMatrix;
  const pos, dir: TPointD): TBitMatrixCursorF;
begin
  Result.img := image;
  Result.p := pos;
  Result.SetDirection(dir);
end;

function TBitMatrixCursorF.TestAt(const q: TPointD): Integer;
begin
  if IsInImage(img, q) then
    Result := Ord(BlackAtPoint(img, q))
  else
    Result := CURSOR_INVALID;
end;

function TBitMatrixCursorF.IsIn: Boolean;
begin
  Result := IsInImage(img, p);
end;

function TBitMatrixCursorF.IsBlack: Boolean;
begin
  Result := TestAt(p) = CURSOR_BLACK;
end;

function TBitMatrixCursorF.IsWhite: Boolean;
begin
  Result := TestAt(p) = CURSOR_WHITE;
end;

function TBitMatrixCursorF.Back: TPointD;
begin
  Result := -d;
end;

procedure TBitMatrixCursorF.SetDirection(const dir: TPointD);
begin
  d := BresenhamDirection(dir);
end;

procedure TBitMatrixCursorF.TurnBack;
begin
  d := Back;
end;

function TBitMatrixCursorF.TurnedBack: TBitMatrixCursorF;
begin
  Result.img := img;
  Result.p := p;
  Result.d := Back;
end;

function TBitMatrixCursorF.Step(s: Double): Boolean;
begin
  p := p + s * d;
  Result := IsIn;
end;

function TBitMatrixCursorF.StepToEdge(nth, range: Integer;
  backup: Boolean): Integer;
begin
  var steps := 0;
  var lv := TestAt(p);
  while (nth > 0) and ((range = 0) or (steps < range)) and
    (lv <> CURSOR_INVALID) do
  begin
    Inc(steps);
    var v := TestAt(p + steps * d);
    if (lv <> v) then
    begin
      lv := v;
      Dec(nth);
    end;
  end;
  if backup then
    Dec(steps);
  p := p + steps * d;
  if (nth = 0) then
    Result := steps
  else
    Result := 0;
end;

function TBitMatrixCursorF.Left: TPointD;
begin
  Result := LeftOf(d);
end;

function TBitMatrixCursorF.Right: TPointD;
begin
  Result := RightOf(d);
end;

procedure TBitMatrixCursorF.TurnRight;
begin
  d := RightOf(d);
end;

function TBitMatrixCursorF.MovedBy(const o: TPointD): TBitMatrixCursorF;
begin
  Result := Self;
  Result.p := p + o;
end;

function TBitMatrixCursorF.EdgeAtFront: Integer;
begin
  Result := TestAt(p);
  if (TestAt(p + d) = Result) then
    Result := CURSOR_INVALID;
end;

function TBitMatrixCursorF.ReadPattern(count, max, min: Integer)
  : TArray<Integer>;
begin
  SetLength(Result, count);
  for var i := 0 to count - 1 do
    Result[i] := 0;
  for var i := 0 to count - 1 do
  begin
    Result[i] := StepToEdge(1, max);
    if (Result[i] = 0) then
      exit;
    if (max <> 0) then
      Dec(max, Result[i]);
  end;
  if (min <> 0) and (max <> 0) then
  begin
    for var w in Result do
      Dec(min, w);
    if (min > 0) then
      for var i := 0 to count - 1 do
        Result[i] := 0;
    var steps := -1;
    while (min > 0) and (max <> 0) and (steps <> 0) do
    begin
      steps := StepToEdge(2, max);
      Dec(max, steps);
      Dec(min, steps);
    end;
  end;
end;

function TBitMatrixCursorF.ReadPatternFromBlack(count, maxWhitePrefix, max,
  min: Integer): TArray<Integer>;
begin
  if (maxWhitePrefix <> 0) and IsWhite and (StepToEdge(1, maxWhitePrefix) = 0)
  then
  begin
    SetLength(Result, count);
    for var i := 0 to count - 1 do
      Result[i] := 0;
    exit;
  end;
  Result := ReadPattern(count, max, min);
end;

{ TFastEdgeToEdgeCounter }

class function TFastEdgeToEdgeCounter.Create(const cur: TBitMatrixCursorI)
  : TFastEdgeToEdgeCounter;
begin
  Result.img := cur.img;
  Result.x := cur.p.X;
  Result.y := cur.p.Y;
  Result.dx := cur.d.X;
  Result.dy := cur.d.Y;
  var maxStepsX, maxStepsY: Integer;
  if (cur.d.X = 0) then
    maxStepsX := MaxInt
  else if (cur.d.X > 0) then
    maxStepsX := cur.img.Width - 1 - cur.p.X
  else
    maxStepsX := cur.p.X;
  if (cur.d.Y = 0) then
    maxStepsY := MaxInt
  else if (cur.d.Y > 0) then
    maxStepsY := cur.img.Height - 1 - cur.p.Y
  else
    maxStepsY := cur.p.Y;
  Result.stepsToBorder := Min(maxStepsX, maxStepsY);
end;

function TFastEdgeToEdgeCounter.StepToNextEdge(range: Integer): Integer;
begin
  var maxSteps := Min(stepsToBorder, range);
  if (maxSteps < 0) then // already outside the image
    exit(0);

  // move forward until we a) find a different pixel value or b) step outside
  // the image or c) reach the range limit
  var v := img[x, y];
  var steps := 0;
  repeat
    Inc(steps);
    if (steps > maxSteps) then
    begin
      if (maxSteps = stepsToBorder) then
        break
      else
        exit(0);
    end;
  until img[x + steps * dx, y + steps * dy] <> v;

  Inc(x, steps * dx);
  Inc(y, steps * dy);
  Dec(stepsToBorder, steps);
  Result := steps;
end;

end.
