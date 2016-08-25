unit FinderPatternImpl;

interface
uses FinderPattern;


function NewFinderPattern(const posX,posY,estimatedModuleSize: Single):IFinderPattern; overload;
function NewFinderPattern(const posX,posY,estimatedModuleSize: Single; const count :integer):IFinderPattern; overload;


implementation
uses ResultPoint,ResultPointImpl;

type
  TFinderPatternImpl = class(TResultPointImpl,IResultPoint,IFinderPattern)
  strict private
    FCount: Integer;
    FEstimatedModuleSize: Single;
  public
    function aboutEquals(const moduleSize,i,j: Single): boolean;
    function combineEstimate(const i,j,newModuleSize: Single) : IFinderPattern;
    function Count:integer;
    function EstimatedModuleSize: Single;
  protected
    constructor Create(const posX,posY,estimatedModuleSize: Single); overload;
    constructor Create(const posX,posY,estimatedModuleSize: Single; const count :integer); overload;
  end;



constructor TFinderPatternImpl.Create(const posX,posY,estimatedModuleSize: Single);
begin
  inherited Create(posX, posY);
  self.FEstimatedModuleSize := estimatedModuleSize;
  self.FCount := 1
end;

function TFinderPatternImpl.Count: integer;
begin
  result := FCount;
end;

constructor TFinderPatternImpl.Create(const posX,posY,estimatedModuleSize: Single; const count :integer);
begin
  inherited Create(posX, posY);
  self.FEstimatedModuleSize := estimatedModuleSize;
  self.FCount := count
end;

function TFinderPatternImpl.EstimatedModuleSize: Single;
begin
  result := FEstimatedModuleSize;
end;

function TFinderPatternImpl.aboutEquals(const moduleSize,i,j: Single): boolean;
var
  moduleSizeDiff, x, y: Single;

begin
  x := self.x;
  y := self.y;
  if ((Abs(i - self.y) <= moduleSize) and (Abs(j - self.x) <= moduleSize)) then
  begin

    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);

    Result := ((moduleSizeDiff <= 1) or
      (moduleSizeDiff <= self.estimatedModuleSize));
    exit
  end;

  Result := false;

end;

function TFinderPatternImpl.combineEstimate(const i,j,newModuleSize: Single): IFinderPattern;
var
  combinedCount: Integer;
  combinedX, combinedY: Single;

begin
  combinedCount := (self.count + 1);
  combinedX := ((self.count * self.x) + j) / combinedCount;
  combinedY := ((self.count * self.y) + i) / combinedCount;
  Result := TFinderPatternImpl.Create(combinedX, combinedY,
    (((self.count * self.estimatedModuleSize) + newModuleSize) / combinedCount),
    combinedCount)
end;




function NewFinderPattern(const posX,posY,estimatedModuleSize: Single):IFinderPattern; overload;
begin
   result := TFinderPatternImpl.Create(posX,posY,estimatedModuleSize);
end;

function NewFinderPattern(const posX,posY,estimatedModuleSize: Single; const count :integer):IFinderPattern; overload;
begin
   result := TFinderPatternImpl.Create(posX,posY,estimatedModuleSize,count);
end;


end.
