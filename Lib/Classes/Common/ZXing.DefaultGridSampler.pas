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
  * Delphi Implementation by E. Spelt and K. Gossens
}

unit ZXing.DefaultGridSampler;

interface

uses
  System.SysUtils,
  System.Math,
  ZXing.Common.BitMatrix,
  ZXing.Common.PerspectiveTransform,
  ZXing.Common.Detector.MathUtils;

type
  // TODO(KG) Add GridSampler basis class
  TDefaultGridSampler = class //(TGridSampler)
  private
    class var GridSampler: TDefaultGridSampler;
    class function get_Instance(): TDefaultGridSampler; static;
  protected
    /// <summary> <p>Checks a set of points that have been transformed to sample points on an image against
    /// the image's dimensions to see if the point are even within the image.</p>
    ///
    /// <p>This method will actually "nudge" the endpoints back onto the image if they are found to be
    /// barely (less than 1 pixel) off the image. This accounts for imperfect detection of finder
    /// patterns in an image where the QR Code runs all the way to the image border.</p>
    ///
    /// <p>For efficiency, the method will check points from either end of the line until one is found
    /// to be within the image. Because the set of points are assumed to be linear, this is valid.</p>
    ///
    /// </summary>
    /// <param name="image">image into which the points should map
    /// </param>
    /// <param name="points">actual points in x1,y1,...,xn,yn form
    /// </param>
    class function checkAndNudgePoints(const image: TBitMatrix;
      const points: TArray<Single>): boolean; static;
  public
    class function sampleGrid(const image: TBitMatrix;
      const dimensionX, dimensionY: Integer;
      const p1ToX, p1ToY, p2ToX, p2ToY, p3ToX, p3ToY, p4ToX, p4ToY,
      p1FromX, p1FromY, p2FromX, p2FromY, p3FromX, p3FromY, p4FromX,
      p4FromY: Single): TBitMatrix; overload; static;

    class function sampleGrid(const image: TBitMatrix;
      const dimensionX, dimensionY: Integer;
      const transform: TPerspectiveTransform): TBitMatrix; overload; static;

    class procedure setGridSampler(newGridSampler: TDefaultGridSampler); static;

    // Properties
    class property Instance: TDefaultGridSampler read get_Instance;

  end;

implementation

{ TDefaultGridSampler }

class function TDefaultGridSampler.sampleGrid(const image: TBitMatrix;
  const dimensionX, dimensionY: Integer; const p1ToX, p1ToY, p2ToX, p2ToY,
  p3ToX, p3ToY, p4ToX, p4ToY, p1FromX, p1FromY, p2FromX, p2FromY, p3FromX,
  p3FromY, p4FromX, p4FromY: Single): TBitMatrix;
var
  transform: TPerspectiveTransform;
begin
  transform := TPerspectiveTransform.quadrilateralToQuadrilateral(p1ToX, p1ToY,
    p2ToX, p2ToY, p3ToX, p3ToY, p4ToX, p4ToY, p1FromX, p1FromY, p2FromX,
    p2FromY, p3FromX, p3FromY, p4FromX, p4FromY);

  try
    Result := sampleGrid(image, dimensionX, dimensionY, transform);
  finally
    transform.free;
  end;

end;

class function TDefaultGridSampler.sampleGrid(const image: TBitMatrix;
  const dimensionX, dimensionY: Integer;
  const transform: TPerspectiveTransform) : TBitMatrix;
var
  bits: TBitMatrix;
  points: TArray<Single>;
  y, x, max: Integer;
  iValue: Single;
begin
  Result := nil;

  if ((dimensionX <= 0) or
      (dimensionY <= 0))
  then
     exit;

  bits := TBitMatrix.Create(dimensionX, dimensionY);
  points := TArray<Single>.Create();
  SetLength(points, dimensionX shl 1);

  try
    for y := 0 to Pred(dimensionY) do
    begin
      max := Length(points);
      iValue := (y + 0.5);
      x := 0;
      while ((x < max)) do
      begin
        points[x] := TMathUtils.Asr(x, 1) + 0.5;
        points[(x + 1)] := iValue;
        Inc(x, 2);
      end;
      transform.transformPoints(points);

      // Quick check to see if points transformed to something inside the image;
      // sufficient to check the endpoints
      if (not checkAndNudgePoints(image, points)) then
      begin
        bits.Free;
        exit;
      end;

      try
        x := 0;
        while (x < max) do
        begin
          bits[(TMathUtils.Asr(x, 1)), y] := image[Floor(points[x]), Floor(points[x + 1])];
          Inc(x, 2);
        end
      except
        // This feels wrong, but, sometimes if the finder patterns are misidentified, the resulting
        // transform gets "twisted" such that it maps a straight line of points to a set of points
        // whose endpoints are in bounds, but others are not. There is probably some mathematical
        // way to detect this about the transformation that I don't know yet.
        // This results in an ugly runtime exception despite our clever checks above -- can't have
        // that. We could check each point's coordinates but that feels duplicative. We settle for
        // catching and wrapping ArrayIndexOutOfBoundsException.
        exit;
      end;
    end;
  finally
    points := nil;
  end;
  Result := bits;
end;

class function TDefaultGridSampler.checkAndNudgePoints(const image: TBitMatrix;
  const points: TArray<Single>): Boolean;
var
  offset,
  x, y,
  width, height : Integer;
  nudged : Boolean;
begin
  width  := image.width;
  height := image.height;
  // Check and nudge points from start until we see some that are OK:
  nudged := true;
  offset := 0;

  while ((offset < Length(points)) and nudged) do
  begin
    x := Floor(points[offset]);
    y := Floor(points[(offset + 1)]);
    if ((((x < -1) or (x > width)) or (y < -1)) or (y > height)) then
    begin
      Result := false;
      exit
    end;
    nudged := false;
    if (x = -1) then
    begin
      points[offset] := 0;
      nudged := true
    end
    else if (x = width) then
    begin
      points[offset] := (width - 1);
      nudged := true
    end;
    if (y = -1) then
    begin
      points[(offset + 1)] := 0;
      nudged := true
    end
    else if (y = height) then
    begin
      points[(offset + 1)] := (height - 1);
      nudged := true
    end;
    inc(offset, 2)
  end;

  // Check and nudge points from end:
  nudged := true;
  offset := (Length(points) - 2);
  while ((offset >= 0) and nudged) do
  begin
    x := Floor(points[offset]);
    y := Floor(points[(offset + 1)]);
    if ((((x < -1) or (x > width)) or (y < -1)) or (y > height)) then
    begin
      Result := false;
      exit
    end;
    nudged := false;
    if (x = -1) then
    begin
      points[offset] := 0;
      nudged := true
    end
    else if (x = width) then
    begin
      points[offset] := (width - 1);
      nudged := true
    end;
    if (y = -1) then
    begin
      points[(offset + 1)] := 0;
      nudged := true
    end
    else if (y = height) then
    begin
      points[(offset + 1)] := (height - 1);
      nudged := true
    end;
    Dec(offset, 2);
  end;

  Result := true;
end;

class function TDefaultGridSampler.get_Instance: TDefaultGridSampler;
begin
  GridSampler := TDefaultGridSampler.Create();
  Result := GridSampler;
end;

class procedure TDefaultGridSampler.setGridSampler(newGridSampler
  : TDefaultGridSampler);
begin
  if (newGridSampler = nil) then
  begin
    raise EArgumentException.Create('Arguments in error');
  end;

  GridSampler := newGridSampler;
end;

end.
