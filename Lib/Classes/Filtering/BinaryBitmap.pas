unit BinaryBitmap;

interface

uses SysUtils, Binarizer, LuminanceSource, BitArray;

type
  TBinaryBitmap = class

  private
    Binarizer: TBinarizer;
    function GetWidth: Integer;
    function GetHeight: Integer;

  public
    constructor BinaryBitmap(Binarizer: TBinarizer);
     /// <summary>
    /// Converts one row of luminance data to 1 bit data. May actually do the conversion, or return
    /// cached data. Callers should assume this method is expensive and call it as seldom as possible.
    /// This method is intended for decoding 1D barcodes and may choose to apply sharpening.
    /// </summary>
    /// <param name="y">The row to fetch, which must be in [0, bitmap height).</param>
    /// <param name="row">An optional preallocated array. If null or too small, it will be ignored.
    /// If used, the Binarizer will call BitArray.clear(). Always use the returned object.
    /// </param>
    /// <returns> The array of bits for this row (true means black).</returns>
    function getBlackRow(y: Integer; row: TBitArray): TBitArray;
    function RotateSupported: Boolean;
    function rotateCounterClockwise(): TBinaryBitmap;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

{ TBinaryBitmap }

constructor TBinaryBitmap.BinaryBitmap(Binarizer: TBinarizer);
begin

  if (Binarizer = nil) then
  begin
    raise EArgumentException.Create('Binarizer must be non-null.');
  end;

  Self.Binarizer := Binarizer;

end;

function TBinaryBitmap.getBlackRow(y: Integer; row: TBitArray)
  : TBitArray;
begin
  Result := Binarizer.getBlackRow(y, row);
end;

function TBinaryBitmap.GetHeight: Integer;
begin
  Result := Binarizer.Height;
end;

function TBinaryBitmap.GetWidth: Integer;
begin
  result := Binarizer.Width;
end;

function TBinaryBitmap.rotateCounterClockwise: TBinaryBitmap;
var
  newSource: TLuminanceSource;
begin
  newSource := Binarizer.LuminanceSource.rotateCounterClockwise();
  Result := TBinaryBitmap.BinaryBitmap(Binarizer.createBinarizer(newSource));
end;

function TBinaryBitmap.RotateSupported: Boolean;
begin
  Result := Binarizer.LuminanceSource.RotateSupported();
end;

end.
