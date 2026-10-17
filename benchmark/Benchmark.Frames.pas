unit Benchmark.Frames;

{
  * Camera-like frames: times TScanManager.Scan on a 1920x1080 and a
  * 1280x720 frame, without a barcode (a lit, grainy surface) and with one
  * (a QR Code of the samples pasted in the middle), a number of frames each,
  * with and without the buffer pool (ZXing.Common.BufferPool), in two passes
  * each so that a run-to-run swing shows. For the gain of the pool over
  * camera frames, which the sample images (small, one scan each) do not
  * show.
}

interface

/// <summary>Runs the frames and writes a table: ms per frame for every
/// frame, with and without the pool, two passes.</summary>
procedure RunFrames(const samplesDir: string; frames: Integer);

implementation

uses
  System.SysUtils,
  System.Diagnostics,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  Vcl.Graphics,
{$ENDIF}
  ZXing.ScanManager,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.DecodeHintType,
  ZXing.Common.BufferPool,
  Benchmark.Images;

type
  TPixel = packed record
    B, G, R, A: Byte;
  end;

  PPixelArray = ^TPixelArray;
  TPixelArray = array [0 .. MaxInt div SizeOf(TPixel) - 1] of TPixel;

  /// <summary>The grey values of a frame, to be put into a bitmap.</summary>
  TGreyFrame = record
    Width, Height: Integer;
    Grey: TArray<Byte>;
  end;

/// <summary>A lit, grainy surface: a gradient with noise, nothing to read.
/// </summary>
function BackgroundFrame(width, height: Integer): TGreyFrame;
var
  seed: Cardinal;
begin
  Result.Width := width;
  Result.Height := height;
  SetLength(Result.Grey, width * height);
  seed := 12345;
  for var y := 0 to height - 1 do
    for var x := 0 to width - 1 do
    begin
      // (a linear congruential generator: the same frame every run)
      seed := seed * 1664525 + 1013904223;
      var noise := Integer(seed shr 27) - 16; // -16 .. 15
      var grey := 90 + (100 * x) div width + (30 * y) div height + noise;
      Result.Grey[y * width + x] := EnsureRange(grey, 0, 255);
    end;
end;

/// <summary>The grey values of an image of the samples (its luminance).
/// </summary>
function LoadGrey(const fileName: string): TGreyFrame;
var
  bitmap: TBitmap;
begin
  bitmap := LoadImage(fileName);
  try
    Result.Width := bitmap.Width;
    Result.Height := bitmap.Height;
    SetLength(Result.Grey, Result.Width * Result.Height);
{$IFDEF FRAMEWORK_FMX}
    var data: TBitmapData;
    if bitmap.Map(TMapAccess.Read, data) then
      try
        for var y := 0 to Result.Height - 1 do
        begin
          var row: PPixelArray := data.GetScanline(y);
          for var x := 0 to Result.Width - 1 do
            Result.Grey[y * Result.Width + x] :=
              (3482 * row[x].R + 11721 * row[x].G + 1181 * row[x].B) shr 14;
        end;
      finally
        bitmap.Unmap(data);
      end;
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
    bitmap.PixelFormat := pf32bit;
    for var y := 0 to Result.Height - 1 do
    begin
      var row: PPixelArray := bitmap.ScanLine[y];
      for var x := 0 to Result.Width - 1 do
        Result.Grey[y * Result.Width + x] :=
          (3482 * row[x].R + 11721 * row[x].G + 1181 * row[x].B) shr 14;
    end;
{$ENDIF}
  finally
    bitmap.Free;
  end;
end;

/// <summary>Pastes symbol into the middle of frame, enlarged by an integer
/// factor so that it is about a third of the frame height, with a white
/// border around it.</summary>
procedure PasteSymbol(var frame: TGreyFrame; const symbol: TGreyFrame);
begin
  var factor := Max(1, (frame.Height div 3) div symbol.Height);
  var w := symbol.Width * factor;
  var h := symbol.Height * factor;
  var border := 2 * factor;
  var left := (frame.Width - w) div 2;
  var top := (frame.Height - h) div 2;
  for var y := top - border to top + h + border - 1 do
    for var x := left - border to left + w + border - 1 do
    begin
      var grey := 255;
      var sx := (x - left) div factor;
      var sy := (y - top) div factor;
      if (x >= left) and (x < left + w) and (y >= top) and (y < top + h) then
        grey := symbol.Grey[sy * symbol.Width + sx];
      frame.Grey[y * frame.Width + x] := grey;
    end;
end;

/// <summary>A bitmap with the grey values of frame.</summary>
function FrameBitmap(const frame: TGreyFrame): TBitmap;

  procedure FillRow(row: PPixelArray; y: Integer);
  begin
    for var x := 0 to frame.Width - 1 do
    begin
      var grey := frame.Grey[y * frame.Width + x];
      row[x].B := grey;
      row[x].G := grey;
      row[x].R := grey;
      row[x].A := 255;
    end;
  end;

begin
{$IFDEF FRAMEWORK_FMX}
  Result := TBitmap.Create(frame.Width, frame.Height);
  var data: TBitmapData;
  if Result.Map(TMapAccess.Write, data) then
    try
      for var y := 0 to frame.Height - 1 do
        FillRow(data.GetScanline(y), y);
    finally
      Result.Unmap(data);
    end;
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(frame.Width, frame.Height);
  for var y := 0 to frame.Height - 1 do
    FillRow(Result.ScanLine[y], y);
{$ENDIF}
end;

/// <summary>Scans the bitmap frames times (after ten frames of warming
/// up) and returns the median of the ms per frame (the mean is swayed by
/// the hiccups of the machine, a virtual one above all); text is the result
/// of the last scan.</summary>
function TimeFrames(bitmap: TBitmap; frames: Integer; inversion: Boolean;
  out text: string): Double;
var
  hints: TDictionary<TDecodeHintType, TObject>;
  scanner: TScanManager;
  r: TReadResult;
  sw: TStopwatch;
  times: TArray<Double>;
begin
  // like a camera app: every format, the hints of the mode
  hints := TDictionary<TDecodeHintType, TObject>.Create;
  if inversion then
    hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);
  scanner := TScanManager.Create(TBarcodeFormat.Auto, hints);
  try
    text := '';
    for var i := 1 to 10 do
    begin
      r := scanner.Scan(bitmap);
      r.Free;
    end;
    SetLength(times, frames);
    for var i := 0 to frames - 1 do
    begin
      sw := TStopwatch.StartNew;
      r := scanner.Scan(bitmap);
      times[i] := sw.Elapsed.TotalMilliseconds;
      if (r <> nil) then
        text := r.text;
      r.Free;
    end;
    TArray.Sort<Double>(times);
    Result := times[frames div 2];
  finally
    scanner.Free;
  end;
end;

procedure RunFrames(const samplesDir: string; frames: Integer);
const
  SIZES: array [0 .. 1, 0 .. 1] of Integer = ((1920, 1080), (1280, 720));
var
  symbol: TGreyFrame;
  ms: array [Boolean, 1 .. 2] of Double;
  text: string;
begin
  symbol := LoadGrey(IncludeTrailingPathDelimiter(samplesDir) + 'qrcode-1' +
    PathDelim + '1.png');
  Writeln(Format('Camera frames: TScanManager.Scan (Auto), %d frames per ' +
    'cell after 10 of warming up, median ms per frame.', [frames]));
  Writeln('pool off = SetLength per buffer (as before), pool on = ' +
    'ZXing.Common.BufferPool; two passes each, alternately.');
  Writeln;
  Writeln(Format('%-10s %-9s %-10s | %-21s | %-21s | %s',
    ['frame', 'code', 'hints', 'pool off  pass 1    2',
    'pool on   pass 1    2', 'gain']));
  var wasPooling := BufferPooling;
  try
    for var s := 0 to High(SIZES) do
      for var withCode := false to true do
      begin
        var frame := BackgroundFrame(SIZES[s, 0], SIZES[s, 1]);
        if withCode then
          PasteSymbol(frame, symbol);
        var bitmap := FrameBitmap(frame);
        try
          for var inversion := false to true do
          begin
            text := '';
            for var pass := 1 to 2 do
              for var pooled := false to true do
              begin
                BufferPooling := pooled;
                ClearBufferPool;
                ms[pooled, pass] := TimeFrames(bitmap, frames, inversion, text);
              end;
            var codeText: string := 'none';
            if withCode then
              codeText := 'QR';
            var hintsText: string := '-';
            if inversion then
              hintsText := 'inversion';
            // the gain from the better pass of each (the first pass of a
            // frame on a virtual machine can be far slower: new pages)
            var bestOff := Min(ms[false, 1], ms[false, 2]);
            var bestOn := Min(ms[true, 1], ms[true, 2]);
            var gain := bestOn - bestOff;
            var sign: string := '-';
            if (gain >= 0) then
              sign := '+';
            var resultText: string := '';
            if withCode and (text = '') then
              resultText := '  NOT READ'
            else if withCode then
              resultText := '  read: ' + Copy(text, 1, 20);
            Writeln(Format('%4dx%-5d %-9s %-10s | %8.2f %8.2f    | %8.2f ' +
              '%8.2f    | %s%.2f ms (%s%.0f%%)%s', [SIZES[s, 0], SIZES[s, 1],
              codeText, hintsText, ms[false, 1], ms[false, 2], ms[true, 1],
              ms[true, 2], sign, Abs(gain), sign, Abs(100 * gain / bestOff),
              resultText], TFormatSettings.Invariant));
          end;
        finally
          bitmap.Free;
        end;
      end;
  finally
    BufferPooling := wasPooling;
  end;
end;

end.
