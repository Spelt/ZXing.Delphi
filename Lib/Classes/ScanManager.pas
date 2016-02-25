unit ScanManager;

interface

uses SysUtils, FMX.Graphics, generics.collections, RGBLuminanceSource,
  HybridBinarizer, BinaryBitmap,
  MultiFormatReader, ReadResult, BarcodeFormat, DecodeHintType;

type
  TScanManager = class
  private
    FMultiFormatReader: TMultiFormatReader;
    hints: TDictionary<TDecodeHintType, TObject>;
    listFormats: TList<TBarcodeFormat>;

    function GetMultiFormatReader(format: TBarcodeFormat;
      additionalHints: TDictionary<TDecodeHintType, TObject>)
      : TMultiFormatReader;
  public
    function Scan(pBitmapForScan: TBitmap): TReadResult;
    constructor Create(format: TBarcodeFormat;
      additionalHints: TDictionary<TDecodeHintType, TObject>);
    destructor Destroy; override;
  end;

implementation

constructor TScanManager.Create(format: TBarcodeFormat;
  additionalHints: TDictionary<TDecodeHintType, TObject>);
begin
  inherited Create;
  FMultiFormatReader := GetMultiFormatReader(format, additionalHints);
end;

destructor TScanManager.Destroy;
begin

  if (Assigned(listFormats)) then
  begin
    listFormats.Clear;
    FreeAndNil(listFormats);
  end;

  if (Assigned(hints)) then
  begin
    hints.Clear();
    FreeAndNil(hints);
  end;
  
  FreeAndNil(FMultiFormatReader);
  inherited;
end;

function TScanManager.GetMultiFormatReader(format: TBarcodeFormat;
  additionalHints: TDictionary<TDecodeHintType, TObject>): TMultiFormatReader;

var

  ahKey: TDecodeHintType;
  ahValue: TObject;

begin
  Result := TMultiFormatReader.Create;
  hints := TDictionary<TDecodeHintType, TObject>.Create;
  listFormats:= nil;
  
  if ((format <> TBarcodeFormat.Auto)) then
  begin
    listFormats := TList<TBarcodeFormat>.Create();
    listFormats.Add(format);
    hints.Add(DecodeHintType.POSSIBLE_FORMATS, listFormats);
  end;

  if (additionalHints <> nil) then
  begin
    for ahKey in additionalHints.Keys do
    begin
      ahValue := additionalHints[ahKey];
      hints.Add(ahKey, ahValue);
    end;
  end;

  Result.hints := hints;
end;

function TScanManager.Scan(pBitmapForScan: TBitmap): TReadResult;
var
  RGBLuminanceSource: TRGBLuminanceSource;
  HybridBinarizer: THybridBinarizer;
  BinaryBitmap: TBinaryBitmap;
begin
  try
    RGBLuminanceSource := TRGBLuminanceSource.RGBLuminanceSource(pBitmapForScan,
      pBitmapForScan.Width, pBitmapForScan.Height);

    HybridBinarizer := THybridBinarizer.Create(RGBLuminanceSource);

    BinaryBitmap := TBinaryBitmap.BinaryBitmap(HybridBinarizer);

    Result := FMultiFormatReader.Decode(BinaryBitmap, true);
  finally

    if (BinaryBitmap <> nil) then
    begin
      BinaryBitmap.Free;
    end;

    if (HybridBinarizer <> nil) then
    begin
      HybridBinarizer.Free;
    end;

    if (RGBLuminanceSource <> nil) then
    begin
      RGBLuminanceSource.Free;
    end;
  end;
end;

end.
