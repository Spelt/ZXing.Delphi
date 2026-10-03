unit ZXing.ScanManager;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  VCL.Graphics,
{$ENDIF}
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
    FEnableInversion: Boolean;
    FTryDownscale: Boolean;
    FHints: TDictionary<TDecodeHintType, TObject>;
    FResultPointEvent: TResultPointCallback;
    FMultiFormatReader: TMultiFormatReader;
    listFormats: TList<TBarcodeFormat>;

    function GetMultiFormatReader(const format: TBarcodeFormat)
      : TMultiFormatReader;

    procedure SetResultPointEvent(const AValue: TResultPointCallback);
    /// <summary>Decodes one image (layer), also inverted with
    /// ENABLE_INVERSION. Does not free source.</summary>
    function DecodeLayer(source: TLuminanceSource): TReadResult;
  public
    destructor Destroy; override;

    function Scan(const pBitmapForScan: TBitmap): TReadResult;
    /// <summary>
    /// All barcodes in the bitmap, at most maxCount (0: no limit), each one
    /// once. Scans all layers (normal, inverted with ENABLE_INVERSION and
    /// the downscaled ones), so it takes more time than Scan. The caller
    /// frees the list, which frees the results in it.
    /// </summary>
    function ScanAll(const pBitmapForScan: TBitmap; maxCount: Integer = 0)
      : TObjectList<TReadResult>;
    constructor Create(const format: TBarcodeFormat;
      Hints: TDictionary<TDecodeHintType, TObject>);

    property OnResultPoint: TResultPointCallback read FResultPointEvent
      write SetResultPointEvent;

    /// <summary>
    /// When nothing is found in the full image, also scan downscaled copies
    /// of it (a third of the size per step, as long as the image is larger
    /// than 500 pixels), like zxing-cpp. Helps for large, blurry and dot-peen
    /// codes. The result points are in the coordinates of the full image
    /// (the points reported to OnResultPoint during the detection in a
    /// downscaled layer are not). On by default; costs about 10% more time
    /// on images without a code.
    /// </summary>
    property TryDownscale: Boolean read FTryDownscale write FTryDownscale;
  end;

implementation

uses
  System.Math,
  ZXing.Reader;

procedure ScaleResult(r: TReadResult; scale: Single); forward;

constructor TScanManager.Create(const format: TBarcodeFormat;
  Hints: TDictionary<TDecodeHintType, TObject>);
begin
  inherited Create;
  FEnableInversion := False;
  FTryDownscale := true;
  FHints := Hints;
  FMultiFormatReader := GetMultiFormatReader(format);
end;

destructor TScanManager.Destroy;
var
  hint: TPair<TDecodeHintType, TObject>;
  o: TObject;
begin
  if Assigned(FHints) then
  begin

    for hint in FHints do
    begin
      o := hint.Value;
      // the lengths can also be an array cast to TObject (as in older
      // versions): that is not an object, and the caller frees it
      if (hint.Key in [ZXing.DecodeHintType.ALLOWED_LENGTHS,
        ZXing.DecodeHintType.ALLOWED_EAN_EXTENSIONS]) and
        not IsIntegerArrayHint(o) then
        continue;
      if Assigned(o) then
        o.Free;
    end;

    FHints.Clear();
    FreeAndNil(FHints);
  end;

  FreeAndNil(FMultiFormatReader);
  inherited;
end;

procedure TScanManager.SetResultPointEvent(const AValue: TResultPointCallback);
var
  ahKey: TDecodeHintType;
  ahValue: TObject;
begin
  FResultPointEvent := AValue;

  ahKey := TDecodeHintType.NEED_RESULT_POINT_CALLBACK;
  if Assigned(FResultPointEvent) then
  begin
    ahValue := TResultPointEventObject.Create(FResultPointEvent);
    FHints.AddOrSetValue(ahKey, ahValue);
  end
  else
  begin
    if Fhints.TryGetValue(ahKey, ahValue) then
    begin
      ahValue.Free;
      ahValue := nil;
      FHints.Remove(ahKey);
    end;
  end;
end;

function TScanManager.GetMultiFormatReader(const format: TBarcodeFormat)
  : TMultiFormatReader;
var
  o: TObject;

begin
  Result := TMultiFormatReader.Create;
  listFormats := nil;

  if FHints = nil then
    FHints := TDictionary<TDecodeHintType, TObject>.Create();

  if FHints.ContainsKey(ZXing.DecodeHintType.ENABLE_INVERSION) then
    FEnableInversion := true;

  if format <> TBarcodeFormat.Auto then
  begin
    if (FHints.TryGetValue(ZXing.DecodeHintType.POSSIBLE_FORMATS, o)) then
      listFormats := o as TList<TBarcodeFormat>
    else
    begin
      listFormats := TList<TBarcodeFormat>.Create();
      FHints.Add(ZXing.DecodeHintType.POSSIBLE_FORMATS, listFormats);
    end;

    if (listFormats.Count = 0) then
      listFormats.Add(format)
    else
      listFormats.Insert(0, format); // favor the format parameter

  end;

  Result.Hints := FHints;
end;

const
  // the image pyramid of zxing-cpp: downscale by this factor while the
  // largest side is larger than the threshold
  DOWNSCALE_FACTOR = 3;
  DOWNSCALE_THRESHOLD = 500;

/// <summary>The luminances downscaled by factor (the rounded average of
/// every factor x factor pixels).</summary>
function Downscaled(const luminances: TArray<Byte>; width, height,
  factor: Integer): TArray<Byte>;
begin
  var newWidth := width div factor;
  var newHeight := height div factor;
  var area := factor * factor;
  SetLength(Result, newWidth * newHeight);
  for var dy := 0 to newHeight - 1 do
    for var dx := 0 to newWidth - 1 do
    begin
      var sum := area div 2;
      for var ty := 0 to factor - 1 do
      begin
        var offset := (dy * factor + ty) * width + dx * factor;
        for var tx := 0 to factor - 1 do
          Inc(sum, luminances[offset + tx]);
      end;
      Result[dy * newWidth + dx] := sum div area;
    end;
end;

function TScanManager.Scan(const pBitmapForScan: TBitmap): TReadResult;
begin
  var LuminanceSource := TRGBLuminanceSource.CreateFromBitmap(pBitmapForScan,
    pBitmapForScan.Width, pBitmapForScan.Height);
  try
    Result := DecodeLayer(LuminanceSource);
    if (Result <> nil) or not FTryDownscale then
      exit;

    // nothing found: try the downscaled layers of the image pyramid
    var fullWidth := LuminanceSource.Width;
    var luminances := LuminanceSource.Matrix;
    var width := LuminanceSource.Width;
    var height := LuminanceSource.Height;
    while (Max(width, height) > DOWNSCALE_THRESHOLD) and
      (Min(width, height) >= DOWNSCALE_FACTOR) do
    begin
      luminances := Downscaled(luminances, width, height, DOWNSCALE_FACTOR);
      width := width div DOWNSCALE_FACTOR;
      height := height div DOWNSCALE_FACTOR;
      var layer := TRGBLuminanceSource.Create(luminances, width, height,
        TBitmapFormat.Gray8);
      try
        Result := DecodeLayer(layer);
      finally
        layer.Free;
      end;

      if (Result <> nil) then
      begin
        // back to the coordinates of the full image
        ScaleResult(Result, fullWidth / width);
        exit;
      end;
    end;
  finally
    LuminanceSource.Free;
  end;
end;

/// <summary>Scales the position and the result points of r, from a
/// downscaled layer back to the full image.</summary>
procedure ScaleResult(r: TReadResult; scale: Single);
begin
  var position := r.Position;
  for var i := 0 to High(position) do
    if (position[i] <> nil) then
      position[i] := TResultPointHelpers.CreateResultPoint(position[i].x *
        scale, position[i].y * scale);
  r.Position := position;
  var points := r.resultPoints;
  for var i := 0 to High(points) do
    if (points[i] <> nil) then
      points[i] := TResultPointHelpers.CreateResultPoint(points[i].x * scale,
        points[i].y * scale);
  r.resultPoints := points;
end;

function TScanManager.ScanAll(const pBitmapForScan: TBitmap; maxCount: Integer)
  : TObjectList<TReadResult>;
var
  all: TObjectList<TReadResult>;

  // all barcodes of one layer, normal and inverted, scaled to the full image
  procedure scanLayer(source: TLuminanceSource; scale: Single);
  begin
    var remaining := 0;
    if (maxCount > 0) then
      remaining := maxCount - all.Count;
    var layerResults := TList<TReadResult>.Create;
    try
      var binarizer := THybridBinarizer.Create(source);
      var bitmap := TBinaryBitmap.Create(binarizer);
      try
        FMultiFormatReader.decodeMultiple(bitmap, layerResults, remaining);

        if FEnableInversion and not ResultsFull(layerResults, remaining) then
        begin
          var first := layerResults.Count;
          var inverted := source.invert;
          var invertedBinarizer := THybridBinarizer.CreateInverted(inverted,
            binarizer);
          var invertedBitmap := TBinaryBitmap.Create(invertedBinarizer);
          try
            FMultiFormatReader.decodeMultiple(invertedBitmap, layerResults,
              remaining);
          finally
            invertedBitmap.Free;
            invertedBinarizer.Free;
            inverted.Free;
          end;
          for var i := first to layerResults.Count - 1 do
            layerResults[i].IsInverted := true;
        end;
      finally
        bitmap.Free;
        binarizer.Free;
      end;

      for var r in layerResults do
      begin
        if (scale <> 1) then
          ScaleResult(r, scale);
        if ResultsFull(all, maxCount) or ContainsResult(all, r) then
          r.Free
        else
          all.Add(r);
      end;
    finally
      layerResults.Free;
    end;
  end;

begin
  all := TObjectList<TReadResult>.Create(true);
  try
    var LuminanceSource := TRGBLuminanceSource.CreateFromBitmap
      (pBitmapForScan, pBitmapForScan.Width, pBitmapForScan.Height);
    try
      scanLayer(LuminanceSource, 1);

      // the downscaled layers of the image pyramid
      var fullWidth := LuminanceSource.Width;
      var luminances := LuminanceSource.Matrix;
      var width := LuminanceSource.Width;
      var height := LuminanceSource.Height;
      while FTryDownscale and not ResultsFull(all, maxCount) and
        (Max(width, height) > DOWNSCALE_THRESHOLD) and
        (Min(width, height) >= DOWNSCALE_FACTOR) do
      begin
        luminances := Downscaled(luminances, width, height, DOWNSCALE_FACTOR);
        width := width div DOWNSCALE_FACTOR;
        height := height div DOWNSCALE_FACTOR;
        var layer := TRGBLuminanceSource.Create(luminances, width, height,
          TBitmapFormat.Gray8);
        try
          scanLayer(layer, fullWidth / width);
        finally
          layer.Free;
        end;
      end;
    finally
      LuminanceSource.Free;
    end;
  except
    all.Free;
    raise;
  end;
  Result := all;
end;

function TScanManager.DecodeLayer(source: TLuminanceSource): TReadResult;
var
  LuminanceSource, InvLuminanceSource: TLuminanceSource;
  HybridBinarizer: THybridBinarizer;
  BinaryBitmap: TBinaryBitmap;
begin
  InvLuminanceSource := nil;
  LuminanceSource := source;
  HybridBinarizer := nil;
  BinaryBitmap := nil;
  try

    HybridBinarizer := THybridBinarizer.Create(LuminanceSource);
    BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);
    Result := FMultiFormatReader.Decode(BinaryBitmap, true);

    if (Result = nil) then
    begin
      if (FEnableInversion) then
      begin
        if (BinaryBitmap <> nil) then
          FreeAndNil(BinaryBitmap);

        // the inverted binarizer reuses the block statistics of the first
        // one, which is faster with the same result
        InvLuminanceSource := LuminanceSource.invert();
        var invertedBinarizer := THybridBinarizer.CreateInverted
          (InvLuminanceSource, HybridBinarizer);
        FreeAndNil(HybridBinarizer);
        HybridBinarizer := invertedBinarizer;
        BinaryBitmap := TBinaryBitmap.Create(HybridBinarizer);
        Result := FMultiFormatReader.Decode(BinaryBitmap, true);
        if (Result <> nil) then
          Result.IsInverted := true;

      end;
    end;
  finally
    BinaryBitmap.Free;
    HybridBinarizer.Free;
    InvLuminanceSource.Free;
  end;
end;

end.
