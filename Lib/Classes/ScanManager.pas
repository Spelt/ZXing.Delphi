unit ScanManager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  FMX.Graphics,
  ZXing.LuminanceSource,
  ZXing.RGBLuminanceSource,
  ZXing.InvertedLuminanceSource,
  HybridBinarizer,
  BinaryBitmap,
  MultiFormatReader,
  ZXing.BarcodeFormat,
  ZXing.ResultPoint,
  ZXing.ReadResult,
  DecodeHintType;

type
  TScanManager = class
  private
    FResultPointEvent : TResultPointCallback;
    FMultiFormatReader: TMultiFormatReader;
    hints: TDictionary<TDecodeHintType, TObject>;
    listFormats: TList<TBarcodeFormat>;

    function GetMultiFormatReader(const format: TBarcodeFormat;
      const additionalHints: TDictionary<TDecodeHintType, TObject>): TMultiFormatReader;

    procedure SetResultPointEvent(const AValue: TResultPointCallback);
  public
    destructor Destroy; override;

    function Scan(const pBitmapForScan: TBitmap): TReadResult;
    constructor Create(const format: TBarcodeFormat;
      const additionalHints: TDictionary<TDecodeHintType, TObject>);

    property OnResultPoint : TResultPointCallback read FResultPointEvent write SetResultPointEvent;
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
  a : TResultPointCallback;
  ahValue: TObject;
begin
  FResultPointEvent := AValue;

  ahKey := TDecodeHintType.NEED_RESULT_POINT_CALLBACK;
  if Assigned(FResultPointEvent) then
  begin
    ahValue := TResultPointEventObject.Create(FResultPointEvent);
    hints.AddOrSetValue(ahKey, ahValue);
  end else hints.Remove(ahKey);
end;

function TScanManager.GetMultiFormatReader(const format: TBarcodeFormat;
  const additionalHints: TDictionary<TDecodeHintType, TObject>): TMultiFormatReader;
var
  ahKey: TDecodeHintType;
  ahValue: TObject;
begin
  Result := TMultiFormatReader.Create;
  hints := TDictionary<TDecodeHintType, TObject>.Create();
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

function TScanManager.Scan(const pBitmapForScan: TBitmap): TReadResult;
var
  LuminanceSource,
  InvLuminanceSource : TLuminanceSource;
  HybridBinarizer: THybridBinarizer;
  BinaryBitmap: TBinaryBitmap;
begin
  try
    InvLuminanceSource := nil;
    LuminanceSource := TRGBLuminanceSource.CreateFromBitmap(pBitmapForScan,
      pBitmapForScan.Width, pBitmapForScan.Height);

    HybridBinarizer := THybridBinarizer.Create(LuminanceSource);

    BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);

    Result := FMultiFormatReader.Decode(BinaryBitmap, true);

    if (result = nil) then
    begin
      if (LuminanceSource.InversionSupported) then
      begin
        if (BinaryBitmap <> nil)
        then
           BinaryBitmap.Free;

        if (HybridBinarizer <> nil)
        then
           HybridBinarizer.Free;

        InvLuminanceSource := LuminanceSource.invert();
        HybridBinarizer := THybridBinarizer.Create(InvLuminanceSource);
        BinaryBitmap    := TBinaryBitmap.Create(HybridBinarizer);

        Result := FMultiFormatReader.Decode(BinaryBitmap, true);

        {if (usePreviousState and FMultiFormatReader <> nil) then
        begin
          Result := FMultiFormatReader.decodeWithState(BinaryBitmap);
        endelse
        begin
          Result = FMultiFormatReader.decode(binaryBitmap, Options.Hints);
          usePreviousState := true;
        end;}
      end;
    end;
  finally
    if (BinaryBitmap <> nil)
    then
       BinaryBitmap.Free;

    if (HybridBinarizer <> nil)
    then
       HybridBinarizer.Free;

    if (InvLuminanceSource <> nil)
    then
       InvLuminanceSource.Free;

    if (LuminanceSource <> nil)
    then
       LuminanceSource.Free;
  end;
end;

end.
