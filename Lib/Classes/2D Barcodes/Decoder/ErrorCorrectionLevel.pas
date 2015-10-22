unit ErrorCorrectionLevel;

interface

uses SysUtils;

type

  TErrorCorrectionLevel = class
  private
    Fbits: Integer;
    Fname: string;
    Fordinal_Renamed_Field: Integer;
    class var FOR_BITS: TArray<TErrorCorrectionLevel>;
  public
    constructor Create(ordinal: Integer; bits: Integer; name: string);
    class function forBits(bits: Integer): TErrorCorrectionLevel; static;
    class var H: TErrorCorrectionLevel;
    class var L: TErrorCorrectionLevel;
    class var M: TErrorCorrectionLevel;
    class var Q: TErrorCorrectionLevel;

    function ordinal: Integer;
    function ToString: string; override;

    property bits: Integer read Fbits;
    property Name: string read Fname;
  end;

implementation

constructor TErrorCorrectionLevel.Create(ordinal, bits: Integer; name: string);
begin
  H := TErrorCorrectionLevel.Create(3, 2, 'H');
  L := TErrorCorrectionLevel.Create(0, 1, 'L');
  M := TErrorCorrectionLevel.Create(1, 0, 'M');
  Q := TErrorCorrectionLevel.Create(2, 3, 'Q');

  Fordinal_Renamed_Field := ordinal;
  Fbits := bits;
  Fname := name
end;

class function TErrorCorrectionLevel.forBits(bits: Integer)
  : TErrorCorrectionLevel;
begin
  if ((bits < 0) or (bits >= Length(TErrorCorrectionLevel.FOR_BITS))) then
    raise EArgumentException.Create('');
  begin
    Result := TErrorCorrectionLevel.FOR_BITS[bits];
    exit
  end
end;

function TErrorCorrectionLevel.ordinal: Integer;
begin
  Result := Fordinal_Renamed_Field
end;

function TErrorCorrectionLevel.ToString: string;
begin
  Result := Fname
end;

end.
