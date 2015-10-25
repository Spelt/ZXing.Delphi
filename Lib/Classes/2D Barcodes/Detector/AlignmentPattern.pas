unit AlignmentPattern;

interface

uses Resultpoint;

type

  TAlignmentPattern = class sealed(TResultpoint)
  private
    estimatedModuleSize: Single;
  public
    constructor Create(posX: Single; posY: Single; estimatedModuleSize: Single);
    function aboutEquals(moduleSize: Single; i: Single; j: Single): boolean;
    function combineEstimate(i: Single; j: Single; newModuleSize: Single)
      : TAlignmentPattern;
  end;

implementation

constructor TAlignmentPattern.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single);
begin
  self.estimatedModuleSize := estimatedModuleSize;
  x := posX;
  y := posY;
end;

function TAlignmentPattern.aboutEquals(moduleSize: Single; i: Single;
  j: Single): boolean;

var
  moduleSizeDiff: Single;

begin
  if ((Abs(i - self.y) <= moduleSize) and (Abs(j - self.x) <= moduleSize)) then
  begin
    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);
    begin
      Result := ((moduleSizeDiff <= 1) or
        (moduleSizeDiff <= self.estimatedModuleSize));
      exit
    end
  end;

  Result := false;
end;

function TAlignmentPattern.combineEstimate(i: Single; j: Single;
  newModuleSize: Single): TAlignmentPattern;
var
  combinedX, combinedY: Single;
begin
  combinedX := ((self.x + j) / 2);
  combinedY := ((self.y + i) / 2);
  Result := TAlignmentPattern.Create(combinedX, combinedY,
    ((self.estimatedModuleSize + newModuleSize) / 2))
end;

end.
