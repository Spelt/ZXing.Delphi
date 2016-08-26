unit ZXing.ResultPointImplementation;

interface
uses system.SysUtils, 
     ZXing.ResultPoint;

type
  /// <summary>
  /// Encapsulates a point of interest in an image containing a barcode. Typically, this
  /// would be the location of a finder pattern or the corner of the barcode, for example.
  /// this class is meant to be used only indirectly, via the IResultPoint reference-counted interface
  // it implements. It is exposed in the interface section just to allow the declaration of its derived classes
  /// </summary>
  TResultPoint = class(TInterfacedObject,IResultPoint)
  private type
    TSingleArray = array [0 .. Pred(SizeOf(Single))] of Byte;
  var
    Fx, Fy: Single;
    bytesX, bytesY: TSingleArray;
    FToString: String;
    procedure SetX(const AValue: Single);
    procedure SetY(const AValue: Single);
    function GetX: Single;
    function GetY: Single;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TResultPoint"/> class.
    /// </summary>
    constructor Create; overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="TResultPoint"/> class.
    /// </summary>
    /// <param name="x">The x.</param>
    /// <param name="y">The y.</param>
    constructor Create(const pX, pY: Single); overload;
    destructor Destroy; override;

    /// <summary>
    /// Determines whether the specified <see cref="System.TObject"/> is equal to this instance.
    /// </summary>
    /// <param name="other">The <see cref="System.TObject"/> to compare with this instance.</param>
    /// <returns>
    /// <c>true</c> if the specified <see cref="System.TObject"/> is equal to this instance; otherwise, <c>false</c>.
    /// </returns>
    function Equals(other: TObject): Boolean; override;
    /// <summary>
    /// Returns a hash code for this instance.
    /// </summary>
    /// <returns>
    /// A hash code for this instance, suitable for use in hashing algorithms and data structures like a hash table.
    /// </returns>
    function GetHashCode(): Integer; override;
    /// <summary>
    /// Returns a <see cref="System.String"/> that represents this instance.
    /// </summary>
    /// <returns>
    /// A <see cref="System.String"/> that represents this instance.
    /// </returns>
    function ToString(): String; override;

    property x: Single read GetX write SetX;
    property y: Single read GetY write SetY;
  end;

// these are the helping functions that actually create a new instance.
// The actual constructor cannot be called outside of this unit
function NewResultPoint:IResultPoint; overload;
function NewResultPoint(const pX, pY: Single):IResultPoint; overload;


implementation

function NewResultPoint:IResultPoint;
begin
  result := TResultPoint.Create;
end;

function NewResultPoint(const pX, pY: Single):IResultPoint;
begin
   result := TResultPoint.Create(px,py);
end;


{ TResultPoint }


constructor TResultPoint.Create;
begin
  inherited;
end;

constructor TResultPoint.Create(const pX, pY: Single);
begin
  inherited Create;

  Fx := pX;
  Fy := pY;
  bytesX := TSingleArray(pX);
  bytesY := TSingleArray(pY);
end;

destructor TResultPoint.Destroy;
var
  n: Single;
begin
  n := 0;
  bytesX := TSingleArray(n);
  bytesY := TSingleArray(n);
  inherited;
end;

procedure TResultPoint.SetX(const AValue: Single);
begin
  if (AValue <> Fx) then
  begin
    Fx := AValue;
    bytesX := TSingleArray(Fx);
  end;
end;

procedure TResultPoint.SetY(const AValue: Single);
begin
  if (AValue <> Fy) then
  begin
    Fy := AValue;
    bytesY := TSingleArray(Fy);
  end;
end;

function TResultPoint.GetX: Single;
begin
   result := Fx;
end;

function TResultPoint.GetY: Single;
begin
   result := Fy;
end;

function TResultPoint.Equals(other: TObject): Boolean;
var
  otherPoint: TResultPoint;
begin
  otherPoint := other as TResultPoint;
  if (otherPoint = nil) then
    Result := false
  else
    Result := ((otherPoint.x = Fx) and (otherPoint.y = Fy));
end;

function TResultPoint.GetHashCode: Integer;
begin
  Result := 31 * ((bytesX[0] shl 24) + (bytesX[1] shl 16) + (bytesX[2] shl 8) +
    bytesX[3]) + (bytesY[0] shl 24) + (bytesY[1] shl 16) + (bytesY[2] shl 8) +
    bytesY[3];
end;


function TResultPoint.ToString: String;
begin
  if (FToString = '') then
    FToString := Format('(%g),(%g)', [Fx, Fy]);
  Result := FToString;
end;



end.
