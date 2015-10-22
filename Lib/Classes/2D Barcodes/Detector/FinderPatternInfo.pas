unit FinderPatternInfo;

interface

uses FinderPattern;

type

  TFinderPatternInfo = class sealed
  public
  var
    bottomLeft: TFinderPattern;
    topLeft: TFinderPattern;
    topRight: TFinderPattern;
    constructor Create(patternCenters: TArray<TFinderPattern>);
  end;

implementation

constructor TFinderPatternInfo.Create(patternCenters: TArray<TFinderPattern>);
begin
  self.bottomLeft := patternCenters[0];
  self.topLeft := patternCenters[1];
  self.topRight := patternCenters[2]
end;

end.
