unit AlignmentPatternImpl;

interface
uses AlignmentPattern;

   function NewAlignmentPattern(const posX: Single; posY: Single; const estimatedModuleSize: Single):IAlignmentPattern;

implementation
uses ResultPoint,ResultPointImpl;

type
  TAlignmentPatternImpl = class (TResultPointImpl, IResultpoint, IAlignmentPattern)
  protected
     constructor Create(posX: Single; posY: Single; estimatedModuleSize: Single);
  strict private
     estimatedModuleSize: Single;
     function aboutEquals(const moduleSize,i,j: Single): boolean;
     function combineEstimate(const i, j, newModuleSize: Single):IAlignmentPattern;
  end;


function NewAlignmentPattern(const posX: Single; posY: Single; const estimatedModuleSize: Single):IAlignmentPattern;
begin
   result :=  TAlignmentPatternImpl.Create(posx,posy,estimatedModuleSize);
end;




constructor TAlignmentPatternImpl.Create(posX: Single; posY: Single;
  estimatedModuleSize: Single);
begin
  inherited Create(posX, posY);
  self.estimatedModuleSize := estimatedModuleSize;
end;

function TAlignmentPatternImpl.aboutEquals(const moduleSize,i,j: Single): boolean;
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

function TAlignmentPatternImpl.combineEstimate(const i,j,newModuleSize: Single): IAlignmentPattern;
var
  combinedX, combinedY: Single;
begin
  combinedX := ((self.x + j) / 2);
  combinedY := ((self.y + i) / 2);
  Result := TAlignmentPatternImpl.Create(combinedX, combinedY,
    ((self.estimatedModuleSize + newModuleSize) / 2))
end;

end.
