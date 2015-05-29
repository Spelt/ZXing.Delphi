unit MathUtils;

interface
uses SysUtils;

type
  TMathUtils = class
  public
    class function Distance(aX, aY, bX, bY: double): double; static;
   // class function Asr(x, y: integer): integer; overload; static;
    class function Asr(Value: Int64; ShiftBits: integer): Int64; static;

  end;

implementation

{ TMathUtils }

class function TMathUtils.Distance(aX, aY, bX, bY: double): double;
var
  xDiff, yDiff: double;
begin
  xDiff := aX - bX;
  yDiff := aY - bY;
  result := Sqrt(xDiff * xDiff + yDiff * yDiff);
end;

{
class function TMathUtils.Asr(x, y: integer): integer;
begin
  result := x div (1 shl y);
end;
 }
class function TMathUtils.Asr(value: Int64; ShiftBits: integer): Int64;
begin
  Result := Value shr ShiftBits;
  if (Value and $8000000000000000) > 0 then
    Result := Result or ($FFFFFFFFFFFFFFFF shl (64 - ShiftBits));
end;


end.
