unit DetectorResult;

interface

uses BitMatrixx, Resultpoint;

type
  TDetectorResult = class

    FBits: TBitmatrix;
    FPoints: TArray<TResultPoint>;
  private
    function get_Bits: TBitmatrix;
    function get_Points: TArray<TResultPoint>;

  public
    constructor Create(bits: TBitmatrix; points: TArray<TResultPoint>);

    property bits: TBitmatrix read get_Bits;
    property points: TArray<TResultPoint> read get_Points;
  end;

implementation

constructor TDetectorResult.Create(bits: TBitmatrix;
  points: TArray<TResultPoint>);
begin
  FBits := bits;
  FPoints := points
end;

function TDetectorResult.get_Bits: TBitmatrix;
begin
  result := FBits;
end;

function TDetectorResult.get_Points: TArray<TResultPoint>;
begin
  result := FPoints;
end;

end.
