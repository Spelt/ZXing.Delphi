unit ZXing.ScanManager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  FMX.Graphics,
  ZXing.LuminanceSource,
  ZXing.RGBLuminanceSource,
  ZXing.InvertedLuminanceSource,
  ZXing.HybridBinarizer,
  ZXing.BinaryBitmap,
  ZXing.MultiFormatReader,
  ZXing.BarcodeFormat,
  ZXing.ResultPoint,
  ZXing.ReadResult,
  ZXing.DecodeHintType;

type
  TScanManager = class
  private
    FResultPointEvent: TResultPointCallback;
    FMultiFormatReader: TMultiFormatReader;
    hints: TDictionary<TDecodeHintType, TObject>;
    listFormats: TList<TBarcodeFormat>;

    function GetMultiFormatReader(const format: TBarcodeFormat;
      const additionalHints: TDictionary<TDecodeHintType, TObject>)
      : TMultiFormatReader;

    procedure SetResultPointEvent(const AValue: TResultPointCallback);
  public
    destructor Destroy; override;

    function Scan(const pBitmapForScan: TBitmap): TReadResult;
    constructor Create(const format: TBarcodeFormat;
      const additionalHints: TDictionary<TDecodeHintType, TObject>);

    property OnResultPoint: TResultPointCallback read FResultPointEvent
      write SetResultPointEvent;
  end;

implementation

constructor TScanManager.Create(const format: TBarcodeFormat;
  const additionalHints: TDictionary<TDecodeHintType, TObject>);
begin
  inherited Create;
  FMultiFormatReader := GetMultiFormatReader(format, additionalHints);
end;

destructor TScanManager.Destroy;
begin
  if Assigned(listFormats) then
  begin
    listFormats.Clear;
    listFormats.Free;
  end;

  if Assigned(hints) then
  begin

    hints.Clear();
    hints.Free;
  end;

  FMultiFormatReader.Free;
  inherited;
end;

procedure TScanManager.SetResultPointEvent(const AValue: TResultPointCallback);
var
  ahKey: TDecodeHintType;
  a: TResultPointCallback;
  ahValue: TObject;
begin
  FResultPointEvent := AValue;

  ahKey := TDecodeHintType.NEED_RESULT_POINT_CALLBACK;
  if Assigned(FResultPointEvent) then
  begin
    ahValue := TResultPointEventObject.Create(FResultPointEvent);
    hints.AddOrSetValue(ahKey, ahValue);
  end
  else
    hints.Remove(ahKey);
end;

function TScanManager.GetMultiFormatReader(const format: TBarcodeFormat;
  const additionalHints: TDictionary<TDecodeHintType, TObject>)
  : TMultiFormatReader;
var
  ahKey: TDecodeHintType;
  ahValue: TObject;
  o: TObject;

begin
  Result := TMultiFormatReader.Create;
  hints := TDictionary<TDecodeHintType, TObject>.Create();
  listFormats := nil;

  if (additionalHints <> nil) then
  begin
    for ahKey in additionalHints.Keys do
    begin
      ahValue := additionalHints[ahKey];
      hints.Add(ahKey, ahValue);
    end;
  end;

  if ((format <> TBarcodeFormat.Auto)) then
  begin

    if (hints.TryGetValue(ZXing.DecodeHintType.POSSIBLE_FORMATS, o)) then
       listFormats := o as TList<TBarcodeFormat>
    else
      begin
        listFormats := TList<TBarcodeFormat>.Create();
        hints.Add(ZXing.DecodeHintType.POSSIBLE_FORMATS, listFormats);
      end;

    if (listFormats.Count = 0) then
      listFormats.Add(format)
    else
      listFormats.Insert(0, format);  // favor the format parameter

  end;

  Result.hints := hints;
end;

function TScanManager.Scan(const pBitmapForScan: TBitmap): TReadResult;
var
  LuminanceSource, InvLuminanceSource: TLuminanceSource;
  HybridBinarizer: THybridBinarizer;
  BinaryBitmap: TBinaryBitmap;
begin
  InvLuminanceSource := nil;
  LuminanceSource := nil;
  HybridBinarizer := nil;
  BinaryBitmap := nil;
  try

    LuminanceSource := TRGBLuminanceSource.CreateFromBitmap(pBitmapForScan,
      pBitmapForScan.Width, pBitmapForScan.Height);
    HybridBinarizer := THybridBinarizer.Create(LuminanceSource);
    BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);
    Result := FMultiFormatReader.Decode(BinaryBitmap, true);

    if (Result = nil) then
    begin
      if (LuminanceSource.InversionSupported) then
      begin
        if (BinaryBitmap <> nil) then
          FreeAndNil(BinaryBitmap);

        if (HybridBinarizer <> nil) then
          FreeAndNil(HybridBinarizer);

        InvLuminanceSource := LuminanceSource.invert();
        HybridBinarizer := THybridBinarizer.Create(InvLuminanceSource);
        BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);
        Result := FMultiFormatReader.Decode(BinaryBitmap, true);

      end;
    end;
  finally
    BinaryBitmap.Free;
    HybridBinarizer.Free;
    InvLuminanceSource.Free;
    LuminanceSource.Free;
  end;
end;

end.
