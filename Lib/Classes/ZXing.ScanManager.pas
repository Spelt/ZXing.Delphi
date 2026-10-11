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
  /// <summary>
  /// The scanner as an interface: hold a TScanManager in a variable of this
  /// type and it is freed when the last reference goes out of scope, no
  /// try finally needed:
  ///   var scanner: IScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
  /// A TScanManager variable with Free works as before. Do not mix the two
  /// for one instance: an interface reference frees the object when it is
  /// the last one, also when an object variable still points to it.
  /// </summary>
  IScanManager = interface
    ['{5B9C1F3A-2D4E-4F6A-9B8C-7D1E2F3A4B5C}']
    function GetOnResultPoint: TResultPointCallback;
    procedure SetResultPointEvent(const AValue: TResultPointCallback);
    function GetTryDownscale: Boolean;
    procedure SetTryDownscale(const AValue: Boolean);
    function GetReturnErrors: Boolean;
    procedure SetReturnErrors(const AValue: Boolean);
    function GetScanning: Boolean;
    /// <summary>The first barcode in the bitmap, nil when there is none.
    /// The caller frees the result.</summary>
    function Scan(const pBitmapForScan: TBitmap): TReadResult; overload;
    /// <summary>The same for the pixels of a luminance source (e.g. the
    /// frame of a camera), which the caller owns.</summary>
    function Scan(const source: TLuminanceSource): TReadResult; overload;
    /// <summary>Scan in a task, see TScanManager.ScanAsync.</summary>
    procedure ScanAsync(const pBitmapForScan: TBitmap;
      const OnCompleted: TProc<TReadResult>;
      const OnException: TProc<Exception> = nil);
    /// <summary>True while a scan started with ScanAsync runs.</summary>
    property Scanning: Boolean read GetScanning;
    /// <summary>All barcodes in the bitmap, see TScanManager.ScanAll.
    /// </summary>
    function ScanAll(const pBitmapForScan: TBitmap; maxCount: Integer = 0)
      : TObjectList<TReadResult>;
    property OnResultPoint: TResultPointCallback read GetOnResultPoint
      write SetResultPointEvent;
    property TryDownscale: Boolean read GetTryDownscale write SetTryDownscale;
    property ReturnErrors: Boolean read GetReturnErrors write SetReturnErrors;
  end;

  TScanManager = class(TInterfacedObject, IScanManager)
  private
    FEnableInversion: Boolean;
    FTryDownscale: Boolean;
    FHints: TDictionary<TDecodeHintType, TObject>;
    FResultPointEvent: TResultPointCallback;
    FMultiFormatReader: TMultiFormatReader;
    listFormats: TList<TBarcodeFormat>;
    FReturnErrors: Boolean;
    // 1 while a scan started with ScanAsync runs (TInterlocked)
    FScanning: Integer;
    // during a scan with ReturnErrors: the symbols found but not read
    // (hint RETURN_ERRORS), in the coordinates of the full image after each
    // layer
    FFailed: TObjectList<TReadResult>;

    /// <summary>With ReturnErrors: a new list of failed results, given to
    /// the readers as hint RETURN_ERRORS.</summary>
    procedure BeginFailed;
    /// <summary>Removes the hint and frees the failed results.</summary>
    procedure EndFailed;
    function FailedCount: Integer;
    /// <summary>The failed results from index first on were found in a
    /// downscaled or inverted layer.</summary>
    procedure AdjustFailed(first: Integer; scale: Single; inverted: Boolean);

    function GetMultiFormatReader(const format: TBarcodeFormat)
      : TMultiFormatReader;

    procedure SetResultPointEvent(const AValue: TResultPointCallback);
    function GetOnResultPoint: TResultPointCallback;
    function GetTryDownscale: Boolean;
    procedure SetTryDownscale(const AValue: Boolean);
    function GetReturnErrors: Boolean;
    procedure SetReturnErrors(const AValue: Boolean);
    function GetScanning: Boolean;
    /// <summary>Decodes one image (layer), also inverted with
    /// ENABLE_INVERSION. Does not free source.</summary>
    function DecodeLayer(source: TLuminanceSource): TReadResult;
    /// <summary>Scan without the failed results: the full image, then the
    /// downscaled layers.</summary>
    function ScanLayers(const LuminanceSource: TLuminanceSource): TReadResult;
  public
    destructor Destroy; override;

    /// <summary>The first barcode in the bitmap, nil when there is none.
    /// The caller frees the result.</summary>
    function Scan(const pBitmapForScan: TBitmap): TReadResult; overload;
    /// <summary>The same for the pixels of a luminance source (e.g. the
    /// frame of a camera as TPlanarYUVLuminanceSource), which the caller
    /// owns.</summary>
    function Scan(const source: TLuminanceSource): TReadResult; overload;
    /// <summary>
    /// Scan in a task of the thread pool: the pixels of the bitmap are taken
    /// in the calling thread (the bitmap is free again when this returns),
    /// the decoding runs in the task, and OnCompleted is called in the main
    /// thread (TThread.Synchronize) with the first barcode, nil when there
    /// is none; the receiver frees the result. An exception in the decoding
    /// goes to OnException, also in the main thread, with an exception the
    /// scan manager frees afterwards; without OnException it ends the scan
    /// like nothing found: OnCompleted(nil). One scan at a time: while a
    /// scan runs (Scanning is true), the call does nothing, e.g. a camera
    /// frame is skipped; check Scanning first to know. A scan
    /// manager held as IScanManager stays alive until the task is done;
    /// one held as TScanManager must not be freed before OnCompleted (its
    /// destructor waits for the task).
    /// </summary>
    procedure ScanAsync(const pBitmapForScan: TBitmap;
      const OnCompleted: TProc<TReadResult>;
      const OnException: TProc<Exception> = nil);
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

    /// <summary>
    /// Also return QR Codes and Data Matrix codes that were found but could
    /// not be read, with Error set ('Checksum' or 'Format') and their
    /// position, e.g. to tell the user to hold the camera still or closer.
    /// Scan returns one only when no barcode could be read; ScanAll adds
    /// them behind the barcodes that were read (not at the same place).
    /// QR Codes count only when their format information could be read,
    /// Data Matrix codes only when the edge tracing detector found them with
    /// a valid size. Off by default.
    /// </summary>
    property ReturnErrors: Boolean read FReturnErrors write FReturnErrors;
    /// <summary>True while a scan started with ScanAsync runs.</summary>
    property Scanning: Boolean read GetScanning;
  end;

implementation

uses
  System.Math,
  System.Classes,
  System.SyncObjs,
  System.Threading,
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
  // a scan of ScanAsync still running: wait for it (and let it call back
  // when this is the main thread)
  while (FScanning <> 0) do
    if (TThread.CurrentThread.ThreadID = MainThreadID) then
      CheckSynchronize(10)
    else
      Sleep(10);
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

function TScanManager.GetOnResultPoint: TResultPointCallback;
begin
  Result := FResultPointEvent;
end;

function TScanManager.GetTryDownscale: Boolean;
begin
  Result := FTryDownscale;
end;

procedure TScanManager.SetTryDownscale(const AValue: Boolean);
begin
  FTryDownscale := AValue;
end;

function TScanManager.GetReturnErrors: Boolean;
begin
  Result := FReturnErrors;
end;

procedure TScanManager.SetReturnErrors(const AValue: Boolean);
begin
  FReturnErrors := AValue;
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

procedure TScanManager.BeginFailed;
begin
  if not FReturnErrors then
    exit;
  if (FHints = nil) then
    FHints := TDictionary<TDecodeHintType, TObject>.Create;
  FFailed := TObjectList<TReadResult>.Create(true);
  FHints.AddOrSetValue(ZXing.DecodeHintType.RETURN_ERRORS, FFailed);
end;

procedure TScanManager.EndFailed;
begin
  if (FFailed = nil) then
    exit;
  FHints.Remove(ZXing.DecodeHintType.RETURN_ERRORS);
  FreeAndNil(FFailed);
end;

function TScanManager.FailedCount: Integer;
begin
  if (FFailed = nil) then
    Result := 0
  else
    Result := FFailed.Count;
end;

procedure TScanManager.AdjustFailed(first: Integer; scale: Single;
  inverted: Boolean);
begin
  for var i := first to FailedCount - 1 do
  begin
    if (scale <> 1) then
      ScaleResult(FFailed[i], scale);
    if inverted then
      FFailed[i].IsInverted := true;
  end;
end;

function TScanManager.Scan(const pBitmapForScan: TBitmap): TReadResult;
begin
  var source := TRGBLuminanceSource.CreateFromBitmap(pBitmapForScan,
    pBitmapForScan.Width, pBitmapForScan.Height);
  try
    Result := Scan(source);
  finally
    source.Free;
  end;
end;

function TScanManager.Scan(const source: TLuminanceSource): TReadResult;
begin
  BeginFailed;
  try
    Result := ScanLayers(source);
    // nothing read: the first symbol found but not read
    if (Result = nil) and (FailedCount > 0) then
      Result := FFailed.Extract(FFailed[0]);
  finally
    EndFailed;
  end;
end;

function TScanManager.ScanLayers(const LuminanceSource: TLuminanceSource)
  : TReadResult;
begin
  begin
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
      var layer := TRGBLuminanceSource.CreateAdopting(luminances, width, height);
      var failedBefore := FailedCount;
      try
        Result := DecodeLayer(layer);
      finally
        layer.Free;
      end;
      AdjustFailed(failedBefore, fullWidth / width, false);

      if (Result <> nil) then
      begin
        // back to the coordinates of the full image
        ScaleResult(Result, fullWidth / width);
        exit;
      end;
    end;
  end;
end;

function TScanManager.GetScanning: Boolean;
begin
  Result := (FScanning <> 0);
end;

procedure TScanManager.ScanAsync(const pBitmapForScan: TBitmap;
  const OnCompleted: TProc<TReadResult>;
  const OnException: TProc<Exception>);
var
  keep: IScanManager;
  source: TLuminanceSource; // (not inline: captured by the task)
begin
  if not Assigned(OnCompleted) then
    raise EArgumentNilException.Create('ScanAsync: OnCompleted');
  // one scan at a time
  if (TInterlocked.CompareExchange(FScanning, 1, 0) <> 0) then
    exit;
  // the pixels, taken in the calling thread: the caller may change or free
  // the bitmap as soon as this returns
  source := TRGBLuminanceSource.CreateFromBitmap(pBitmapForScan,
    pBitmapForScan.Width, pBitmapForScan.Height);
  // held by interface references (the count is 0 for a TScanManager that
  // is freed by its owner): one more, so that it lives until the task is
  // done
  keep := nil;
  if (RefCount > 0) then
    keep := Self;
  TTask.Run(
    procedure
    var
      r: TReadResult;
      e: Exception;
    begin
      r := nil;
      e := nil;
      try
        try
          r := Scan(source);
        except
          e := Exception(AcquireExceptionObject);
        end;
      finally
        source.Free;
      end;
      TThread.Synchronize(nil,
        procedure
        begin
          // done before the callback: it may start the next scan
          FScanning := 0;
          try
            if (e <> nil) and Assigned(OnException) then
              OnException(e)
            else
              OnCompleted(r); // (nil after an exception)
          finally
            e.Free;
          end;
        end);
      keep := nil;
    end);
end;

/// <summary>Scales the position and the result points of r, from a
/// downscaled layer back to the full image.</summary>
procedure ScaleResult(r: TReadResult; scale: Single);
begin
  var position := r.Position;
  for var i := 0 to High(position) do
    if position[i].HasValue then
      position[i] := TResultPointHelpers.CreateResultPoint(position[i].x *
        scale, position[i].y * scale);
  r.Position := position;
  var points := r.resultPoints;
  for var i := 0 to High(points) do
    if points[i].HasValue then
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
      var failedBefore := FailedCount;
      try
        FMultiFormatReader.decodeMultiple(bitmap, layerResults, remaining);
        AdjustFailed(failedBefore, scale, false);

        if FEnableInversion and not ResultsFull(layerResults, remaining) then
        begin
          failedBefore := FailedCount;
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
          AdjustFailed(failedBefore, scale, true);
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
  BeginFailed;
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
        var layer := TRGBLuminanceSource.CreateAdopting(luminances, width, height);
        try
          scanLayer(layer, fullWidth / width);
        finally
          layer.Free;
        end;
      end;
    finally
      LuminanceSource.Free;
    end;

    // the symbols found but not read, where nothing was read
    var i := 0;
    while (i < FailedCount) do
      if not ResultsFull(all, maxCount) and not OverlapsResult(all, FFailed[i])
      then
        all.Add(FFailed.Extract(FFailed[i]))
      else
        Inc(i);
  except
    all.Free;
    EndFailed;
    raise;
  end;
  EndFailed;
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
        var failedBefore := FailedCount;
        Result := FMultiFormatReader.Decode(BinaryBitmap, true);
        if (Result <> nil) then
          Result.IsInverted := true;
        AdjustFailed(failedBefore, 1, true);

      end;
    end;
  finally
    BinaryBitmap.Free;
    HybridBinarizer.Free;
    InvLuminanceSource.Free;
  end;
end;

end.
