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
  self.estimatedModuleSize := estimatedModuleSize
end;

function TAlignmentPattern.aboutEquals(moduleSize: Single; i: Single;
  j: Single): boolean;

var
  moduleSizeDiff: Single;

begin
  if ((Abs(i - self.Y) <= moduleSize) and (Abs(j - self.X) <= moduleSize)) then
  begin
    moduleSizeDiff := Abs(moduleSize - self.estimatedModuleSize);
    begin
      Result := ((moduleSizeDiff <= 1) or
        (moduleSizeDiff <= self.estimatedModuleSize));
      exit
    end
  end;
  begin
    Result := false;
    exit
  end
end;

function TAlignmentPattern.combineEstimate(i: Single; j: Single;
  newModuleSize: Single): TAlignmentPattern;
var
  combinedX, combinedY: Single;
begin
  combinedX := ((self.X + j) / 2);
  combinedY := ((self.Y + i) / 2);
  Result := TAlignmentPattern.Create(combinedX, combinedY,
    ((self.estimatedModuleSize + newModuleSize) / 2))
end;

end.
