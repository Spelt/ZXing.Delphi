unit ZXIng.ByteSegments;
interface

type
    /// <summary>
    ///  implements the ByteSegments (which was declared as a TList<TArray<Byte>>
    ///  throughout the code as reference-counted interface object)
    /// </summary>
    IByteSegments = Interface
        ['{0994FC90-E8F5-40D8-8A48-9B05DFFF2635}']
        function Count:integer;
        procedure Clear;
        function GetCapacity: integer;
        procedure SetCapacity(const Value: integer);
        property Capacity:integer read GetCapacity write SetCapacity;
        function Add(const item:TArray<byte>):integer;
     end;


function ByteSegmentsCreate:IByteSegments;

implementation
uses system.SysUtils,
     Generics.Collections;

type
  TByteSegments = class(TInterfacedObject,IByteSegments)
  private
     FList: TList<TArray<Byte>>;
     function Count:integer;
     procedure Clear;
     constructor Create;
     function GetCapacity: integer;
     procedure SetCapacity(const Value: integer);
     function Add(const item:TArray<byte>):integer;
  public
     destructor Destroy; override;
  end;




function ByteSegmentsCreate:IByteSegments;
begin
   result := TByteSegments.Create;

end;


{ TByteSegments }

function TByteSegments.Add(const item: TArray<byte>): integer;
begin
   result := FList.Add(item);
end;

procedure TByteSegments.Clear;
begin
   FList.Clear;
end;

function TByteSegments.Count: integer;
begin
   result := FList.Count;
end;

constructor TByteSegments.Create;
begin
   FList := TList<TArray<Byte>>.Create;
   inherited Create;
end;

destructor TByteSegments.Destroy;
begin
  FList.Free;
  inherited;
end;

function TByteSegments.GetCapacity: integer;
begin
   result := FList.Capacity;
end;

procedure TByteSegments.SetCapacity(const Value: integer);
begin
  FList.Capacity := value;
end;

end.
