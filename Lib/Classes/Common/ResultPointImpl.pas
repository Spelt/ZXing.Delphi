unit ResultPointImpl;

interface
uses sysutils,ResultPoint;


  function NewResultPoint  (const pX, pY: single):IResultPoint;


type
  // this class must NOT be instantiated directly: it is declared in the interface section
  // and not in the implementation only to allow the declaration of derived classes..
  // that must too be accessed only via interfaces
  TResultPointImpl = class(TInterfacedObject,IResultPoint)
  strict private
    Fx, Fy: single;
    bytesX, bytesY: byteArray;
    FToString: string;
    function GetX: single;
    function GetY: single;
    procedure SetX(const Value: single);
    procedure SetY(const Value: single);

  protected
    constructor Create(const pX, pY: single);
  strict protected
    destructor Destroy(); override;
    function Equals(other: IResultPoint): Boolean; reintroduce;
    function GetHashCode(): Integer; override;
    function ToString(): String; override;

    property x: single read GetX write SetX;
    property y: single read GetY write SetY;
  end;


implementation
uses MathUtils;


constructor TResultPointImpl.Create(const pX, pY: single);
begin
  Fx := pX;
  Fy := pY;
  SetLength(bytesX,4);
  SetLength(bytesY,4);
  //bytesX := byteArray(pX);
  //bytesY := byteArray(pY);
end;

destructor TResultPointImpl.Destroy;
begin
  bytesX := nil;
  bytesY := nil;
  inherited;
end;

function TResultPointImpl.Equals(other: IResultPoint): Boolean;
begin
  if (other = nil) then
  begin
    result := false;
    Exit;
  end;

  result := ((other.x = Fx) and (other.y = Fy));
end;

function TResultPointImpl.GetHashCode: Integer;
begin
  result := 31 * ((bytesX[0] shl 24) + (bytesX[1] shl 16) + (bytesX[2] shl 8) +
    bytesX[3]) + (bytesY[0] shl 24) + (bytesY[1] shl 16) + (bytesY[2] shl 8) +
    bytesY[3];
end;

function TResultPointImpl.GetX: single;
begin
  result := Fx;
end;

function TResultPointImpl.GetY: single;
begin
  result := Fy;
end;

procedure TResultPointImpl.SetX(const Value: single);
begin
   Fx := value;
end;

procedure TResultPointImpl.SetY(const Value: single);
begin
  Fy := value;
end;

function TResultPointImpl.ToString: String;
begin
  if (FToString = '') then
  begin
    FToString := Format('(%g),(%g)', [Fx, Fy]);
  end;

  result := FToString;
end;


function NewResultPoint  (const pX, pY: single):IResultPoint;
begin
    result := TResultPointImpl.Create(px,py);
end;


end.
