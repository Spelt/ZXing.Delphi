unit DefaultGridSampler;

interface

uses SysUtils, BitMatrixx, PerspectiveTransform, mathUtils;

type
  TDefaultGridSampler = class

  private
    class var GridSampler: TDefaultGridSampler;
    class function get_Instance(): TDefaultGridSampler; static;

  protected
    class function checkAndNudgePoints(image: TBitMatrix;
      points: TArray<Single>): boolean; static;

  public
    class function sampleGrid(image: TBitMatrix; dimensionX: Integer;
      dimensionY: Integer; transform: TPerspectiveTransform): TBitMatrix;
      overload; static;

    class function sampleGrid(image: TBitMatrix; dimensionX: Integer;
      dimensionY: Integer; p1ToX: Single; p1ToY: Single; p2ToX: Single;
      p2ToY: Single; p3ToX: Single; p3ToY: Single; p4ToX: Single; p4ToY: Single;
      p1FromX: Single; p1FromY: Single; p2FromX: Single; p2FromY: Single;
      p3FromX: Single; p3FromY: Single; p4FromX: Single; p4FromY: Single)
      : TBitMatrix; overload; static;

    class procedure setGridSampler(newGridSampler: TDefaultGridSampler); static;

    // Properties
    class property Instance: TDefaultGridSampler read get_Instance;

  end;

implementation

{ TDefaultGridSampler }

class function TDefaultGridSampler.sampleGrid(image: TBitMatrix;
  dimensionX, dimensionY: Integer; transform: TPerspectiveTransform)
  : TBitMatrix;

var
  bits: TBitMatrix;
  points: TArray<Single>;
  y, x, max: Integer;
  iValue: Single;
begin
  if ((dimensionX <= 0) or (dimensionY <= 0)) then
  begin
    Result := nil;
    exit
  end;
  bits := TBitMatrix.Create(dimensionX, dimensionY);
  points := TArray<Single>.Create();
  SetLength(points, dimensionX shl 1);
  y := 0;

  while ((y < dimensionY)) do
  begin
    max := Length(points);
    iValue := (y + 0.5);
    x := 0;
    while ((x < max)) do
    begin
      points[x] := ((x shr 1) + 0.5);
      points[(x + 1)] := iValue;
      inc(x, 2)
    end;

    transform.transformPoints(points);

    if (not checkAndNudgePoints(image, points)) then
    begin
      Result := nil;
      exit
    end;
    try
      x := 0;
      while ((x < max)) do
      begin

        bits[(TMathUtils.Asr(x, 1)), y] :=
          image[trunc(points[x]), trunc(points[x + 1])];

        inc(x, 2)
      end
    except
      Result := nil;
      exit
    end;

    inc(y)
  end;

  Result := bits;

end;

class function TDefaultGridSampler.checkAndNudgePoints(image: TBitMatrix;
  points: TArray<Single>): boolean;
var
  offset, x, y, width, height: Integer;
  nudged: boolean;
begin
  width := image.width;
  height := image.height;
  nudged := true;
  offset := 0;
  while ((offset < Length(points)) and nudged) do
  begin
    x := round(points[offset]);
    y := round(points[(offset + 1)]);
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

  nudged := true;
  offset := (Length(points) - 2);

  while ((offset >= 0) and nudged) do
  begin
    x := round(points[offset]);
    y := round(points[(offset + 1)]);
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
    dec(offset, 2)
  end;

  Result := true;

end;

class function TDefaultGridSampler.get_Instance: TDefaultGridSampler;
begin
  GridSampler := TDefaultGridSampler.Create();
end;

class function TDefaultGridSampler.sampleGrid(image: TBitMatrix;
  dimensionX, dimensionY: Integer; p1ToX, p1ToY, p2ToX, p2ToY, p3ToX, p3ToY,
  p4ToX, p4ToY, p1FromX, p1FromY, p2FromX, p2FromY, p3FromX, p3FromY, p4FromX,
  p4FromY: Single): TBitMatrix;

var
  transform: TPerspectiveTransform;

begin
  transform := TPerspectiveTransform.quadrilateralToQuadrilateral(p1ToX, p1ToY,
    p2ToX, p2ToY, p3ToX, p3ToY, p4ToX, p4ToY, p1FromX, p1FromY, p2FromX,
    p2FromY, p3FromX, p3FromY, p4FromX, p4FromY);

  Result := sampleGrid(image, dimensionX, dimensionY, transform)
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
