unit QRCodeDecoderMetadata;

interface

uses ResultPoint;

type

  TQRCodeDecoderMetaData = class sealed

  private
    FMirrored: boolean;
    function get_IsMirrored: boolean;
  public
    property IsMirrored: boolean read get_IsMirrored;
    constructor Create(mirrored: boolean);
    procedure applyMirroredCorrection(points: TArray<TResultPoint>);
  end;

implementation

constructor TQRCodeDecoderMetaData.Create(mirrored: boolean);
begin
  FMirrored := mirrored
end;

function TQRCodeDecoderMetaData.get_IsMirrored: boolean;
begin
  Result:=FMirrored;
end;

procedure TQRCodeDecoderMetaData.applyMirroredCorrection
  (points: TArray<TResultPoint>);
var
  bottomLeft: TResultPoint;
begin
  if ((FMirrored and (points <> nil)) and (Length(points) >= 3)) then
  begin
    bottomLeft := points[0];
    points[0] := points[2];
    points[2] := bottomLeft
  end
end;

end.
