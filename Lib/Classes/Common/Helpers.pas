unit Helpers;

interface


type
  TArray = class
    class function Clone(original: TArray<Integer>)
      : TArray<Integer>; static;
  end;

implementation

class function TArray.Clone(original: TArray<Integer>)
  : TArray<Integer>;
var
  i: Integer;
  l: SmallInt;
begin
  l := Length(original);
  SetLength(Result, l);

  for i := 0 to l - 1 do
  begin
    Result[i] := original[i];
  end;
end;

end.
