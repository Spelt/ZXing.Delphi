unit FinderPattern;

interface

uses Resultpoint;

type
  TFinderPattern = class(TResultpoint)
  public
    constructor Create(posX: Single; posY: Single;
      estimatedModuleSize: Single); overload;
    constructor Create(posX: Single; posY: Single; estimatedModuleSize: Single;
      count: Integer); overload;
    function aboutEquals(moduleSize: Single; i: Single; j: Single): boolean;
    function combineEstimate(i: Single; j: Single; newModuleSize: Single)
      : TFinderPattern;

  var
    count: Integer;
    estimatedModuleSize: Single;
  end;

implementation

constructor TFinderPattern.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single);
begin
  inherited Create(posX, posY);
  self.estimatedModuleSize := estimatedModuleSize;
  self.count := 1
end;

constructor TFinderPattern.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single; count: Integer);
begin
  inherited Create(posX, posY);
  self.estimatedModuleSize := estimatedModuleSize;
  self.count := count
end;

function TFinderPattern.aboutEquals(moduleSize: Single; i: Single;
  j: Single): boolean;
var
  moduleSizeDiff,x,y: Single;

begin
  x:=self.x;
  y:=self.y;
  if ((Abs(i - self.Y) <= moduleSize) and (Abs(j - self.X) <= moduleSize)) then
  begin

    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);

    Result := ((moduleSizeDiff <= 1) or
      (moduleSizeDiff <= self.estimatedModuleSize));
    exit
  end;

  Result := false;

end;

function TFinderPattern.combineEstimate(i: Single; j: Single;
  newModuleSize: Single): TFinderPattern;
var
  combinedCount: Integer;
  combinedX, combinedY: Single;

begin
  combinedCount := (self.count + 1);
  combinedX := ((self.count * self.X) + j) / combinedCount;
  combinedY := ((self.count * self.Y) + i) / combinedCount;
  Result := TFinderPattern.Create(combinedX, combinedY,
    (((self.count * self.estimatedModuleSize) + newModuleSize) / combinedCount),
    combinedCount)
end;

end.
