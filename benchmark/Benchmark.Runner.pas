unit Benchmark.Runner;

{
  * Runs ZXing.Delphi over one folder of zxing-cpp samples and counts the
  * results. Used by the benchmark (VCL) and by the unit tests (FMX).
  *
  * Modes, compared with the modes of zxing-cpp's BlackboxTestRunner:
  *   slow  TRY_HARDER + ENABLE_INVERSION   (zxing-cpp: tryHarder, tryRotate, tryInvert)
  *   fast  no hints                        (zxing-cpp: all of those off)
  *   pure  PURE_BARCODE                    (zxing-cpp: isPure, fixed threshold)
  *
  * Texts are compared in the form zxing-cpp uses in its samples, see
  * ResultText and Comparable. Only the decoding is timed, not loading and
  * rotating the images.
}

interface

uses
  System.SysUtils,
  Benchmark.Samples;

type
  TModeStats = record
    /// <summary>Expected symbols at the tested positions.</summary>
    Checks: Integer;
    ReadByCpp: Integer;
    ReadByDelphi: Integer;
    /// <summary>Read by Delphi where zxing-cpp does not read it.</summary>
    DelphiOnly: Integer;
    /// <summary>Delphi returned a result that matches no expected symbol
    /// (wrong text or a false positive).</summary>
    Wrong: Integer;
    /// <summary>Exceptions raised by the decoder.</summary>
    Errors: Integer;
    TimeMs: Double;
    procedure Add(const other: TModeStats);
  end;

  TStats = array [TTestMode] of TModeStats;
  TTestModes = set of TTestMode;

procedure AddStats(var total: TStats; const stats: TStats);

var
  /// <summary>Use TScanManager.ScanAll (all symbols of an image) instead of
  /// Scan (one symbol).</summary>
  UseScanAll: Boolean = false;
  /// <summary>TScanManager.ReturnErrors: the symbols found but not read are
  /// logged (FAILED) and not counted.</summary>
  UseReturnErrors: Boolean = false;

/// <summary>Runs all images of the folder in the given modes. When log is
/// assigned, it gets a line for every symbol that zxing-cpp reads and Delphi
/// not, every wrong read and every exception.</summary>
function RunFolder(folder: TSampleFolder; modes: TTestModes;
  const log: TProc<string>): TStats;

implementation

uses
  System.Diagnostics,
  System.StrUtils,
  System.Generics.Collections,
{$IFDEF FRAMEWORK_FMX}
  FMX.Graphics,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  Vcl.Graphics,
{$ENDIF}
  ZXing.ScanManager,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ResultMetadataType,
  ZXing.DecodeHintType,
  Benchmark.Images;

procedure TModeStats.Add(const other: TModeStats);
begin
  Inc(Checks, other.Checks);
  Inc(ReadByCpp, other.ReadByCpp);
  Inc(ReadByDelphi, other.ReadByDelphi);
  Inc(DelphiOnly, other.DelphiOnly);
  Inc(Wrong, other.Wrong);
  Inc(Errors, other.Errors);
  TimeMs := TimeMs + other.TimeMs;
end;

procedure AddStats(var total: TStats; const stats: TStats);
var
  mode: TTestMode;
begin
  for mode := Low(TTestMode) to High(TTestMode) do
    total[mode].Add(stats[mode]);
end;

function CreateScanManager(folder: TSampleFolder; mode: TTestMode): TScanManager;
var
  hints: TDictionary<TDecodeHintType, TObject>;
begin
  hints := TDictionary<TDecodeHintType, TObject>.Create;
  case mode of
    tmSlow:
      begin
        hints.Add(TDecodeHintType.TRY_HARDER, nil);
        hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);
      end;
    tmPure:
      hints.Add(TDecodeHintType.PURE_BARCODE, nil);
  end;
  if folder.Name.StartsWith('code39ext') then
    hints.Add(TDecodeHintType.USE_CODE_39_EXTENDED_MODE, nil);
  // zxing-cpp returns the start and stop characters of Codabar
  if folder.Name.StartsWith('codabar') then
    hints.Add(TDecodeHintType.RETURN_CODABAR_START_END, nil);
  // zxing-cpp's eanAddOnSymbol "require": only EAN/UPC codes with an add-on
  if (folder.Images.Count > 0) and
    SameText(folder.Images[0].EanAddOnSymbol, 'require') then
    hints.Add(TDecodeHintType.ALLOWED_EAN_EXTENSIONS,
      TIntegerArrayHint.Create([2, 5]));

  // the scan manager owns and frees the hints
  Result := TScanManager.Create(folder.ScanFormat, hints);
  Result.ReturnErrors := UseReturnErrors;
end;

/// <summary>Expands an 8 digit UPC-E code to the 12 digit UPC-A code.</summary>
function UPCEtoUPCA(const upce: string): string;
var
  middle: string;
begin
  middle := Copy(upce, 2, 6);
  case middle[6] of
    '0', '1', '2':
      Result := Copy(middle, 1, 2) + middle[6] + '0000' + Copy(middle, 3, 3);
    '3':
      Result := Copy(middle, 1, 3) + '00000' + Copy(middle, 4, 2);
    '4':
      Result := Copy(middle, 1, 4) + '00000' + middle[5];
  else
    Result := Copy(middle, 1, 5) + '0000' + middle[6];
  end;
  Result := upce[1] + Result + upce[8];
end;

/// <summary>Text of a result in the form zxing-cpp uses in its samples.</summary>
function ResultText(r: TReadResult; image: TSampleImage): string;
var
  meta: IMetaData;
  ext: IStringMetadata;
begin
  Result := r.Text;

  // zxing-cpp does not put the FNC1 that marks a GS1 code in the text
  if ((r.BarcodeFormat = TBarcodeFormat.DATA_MATRIX) or
    (r.BarcodeFormat = TBarcodeFormat.QR_CODE)) and Result.StartsWith(#29) then
    Delete(Result, 1, 1);

  // zxing-cpp returns UPC-A and UPC-E as 13 digit EAN-13 (GTIN-13) text
  if (r.BarcodeFormat = TBarcodeFormat.UPC_A) and (Length(Result) = 12) then
    Result := '0' + Result
  else if (r.BarcodeFormat = TBarcodeFormat.UPC_E) and (Length(Result) = 8) then
    Result := '0' + UPCEtoUPCA(Result);

  // zxing-cpp ignores EAN/UPC add-ons unless asked for, and then appends the
  // add-on directly after the code
  if (image.EanAddOnSymbol <> '') and not SameText(image.EanAddOnSymbol, 'ignore')
    and (r.ResultMetaData <> nil) and
    r.ResultMetaData.TryGetValue(TResultMetadataType.UPC_EAN_EXTENSION, meta) and
    Supports(meta, IStringMetadata, ext) then
    Result := Result + ext.Value;
end;

/// <summary>Escaped text with every kind of line ending as &lt;LF&gt;:
/// ZXing.Delphi replaces every line ending in QR codes by CR (on purpose, to
/// not break existing apps), which is not a detection problem.</summary>
function Comparable(const escapedText: string): string;
begin
  Result := escapedText.Replace('<CR><LF>', '<LF>').Replace('<CR>', '<LF>');
end;

function Matches(const symbol: TExpectedSymbol; r: TReadResult;
  const text: string): Boolean;
begin
  // UPC-A is EAN-13 with a leading 0; the texts are compared as EAN-13
  if (r.BarcodeFormat <> symbol.Format) and
    not ((r.BarcodeFormat = TBarcodeFormat.UPC_A) and
    (symbol.Format = TBarcodeFormat.EAN_13)) and
    not ((r.BarcodeFormat = TBarcodeFormat.EAN_13) and
    (symbol.Format = TBarcodeFormat.UPC_A)) then
    exit(false);
  if (symbol.TextEscaped <> '') then
    Result := (Comparable(EscapeNonGraphical(text)) =
      Comparable(symbol.TextEscaped))
  else if symbol.HasText then
    Result := (Comparable(EscapeNonGraphical(text)) =
      Comparable(EscapeNonGraphical(symbol.TextPlain)))
  else
    Result := true; // only TextHex given: compare the format only
end;

function ExpectedText(const symbol: TExpectedSymbol): string;
begin
  if (symbol.TextEscaped <> '') then
    Result := symbol.TextEscaped
  else if symbol.HasText then
    Result := EscapeNonGraphical(symbol.TextPlain)
  else
    Result := '(' + symbol.FormatName + ', no text)';
end;

function Abbrev(const s: string): string;
begin
  if (Length(s) > 40) then
    Result := Copy(s, 1, 37) + '...'
  else
    Result := s;
end;

procedure TestImage(folder: TSampleFolder; image: TSampleImage;
  modes: TTestModes; const scanManagers: array of TScanManager;
  const log: TProc<string>; var stats: TStats);
var
  original: TBitmap;
  rotated: array [0 .. 3] of TBitmap;
  mode: TTestMode;
  rotation, i, j, matched: Integer;
  r: TReadResult;
  results: TObjectList<TReadResult>;
  texts: TArray<string>;
  used: TArray<Boolean>;
  where: string;
  readByCpp, hasUnsupported: Boolean;
  sw: TStopwatch;
begin
  try
    original := LoadImage(image.FileName);
  except
    on E: Exception do
    begin
      if Assigned(log) then
        log(Format('%s/%s  cannot load: %s', [folder.Name,
          ExtractFileName(image.FileName), E.Message]));
      exit;
    end;
  end;

  FillChar(rotated, SizeOf(rotated), 0);
  try
    rotated[0] := original;
    for mode := Low(TTestMode) to High(TTestMode) do
    begin
      if not (mode in modes) then
        continue;

      for rotation := 0 to 3 do
      begin
        if not image.Search.Contains(mode, rotation) then
          continue;
        if (rotated[rotation] = nil) then
          rotated[rotation] := RotateImage(original, rotation);
        where := Format('%-40s %s/%3d', [folder.Name + '/' +
          ExtractFileName(image.FileName), TestModeName(mode), rotation * 90]);

        // one result with Scan, all of them with ScanAll
        results := TObjectList<TReadResult>.Create(true);
        sw := TStopwatch.StartNew;
        try
          if UseScanAll then
          begin
            results.Free;
            results := scanManagers[Ord(mode)].ScanAll(rotated[rotation]);
          end
          else
          begin
            r := scanManagers[Ord(mode)].Scan(rotated[rotation]);
            if (r <> nil) then
              results.Add(r);
          end;
        except
          on E: Exception do
          begin
            Inc(stats[mode].Errors);
            if Assigned(log) then
              log(Format('%s  ERROR %s: %s', [where, E.ClassName, E.Message]));
          end;
        end;
        stats[mode].TimeMs := stats[mode].TimeMs + sw.Elapsed.TotalMilliseconds;

        // the symbols found but not read (ReturnErrors): only logged
        for i := results.Count - 1 downto 0 do
          if (results[i].Error <> '') then
          begin
            if Assigned(log) then
              log(Format('%s  FAILED: %s %s', [where,
                IfThen(results[i].BarcodeFormat = TBarcodeFormat.QR_CODE, 'QR',
                IfThen(results[i].BarcodeFormat = TBarcodeFormat.DATA_MATRIX, 'DM',
                IfThen(results[i].BarcodeFormat = TBarcodeFormat.AZTEC, 'Aztec',
                IntToStr(Ord(results[i].BarcodeFormat))))), results[i].Error]));
            results.Delete(i);
          end;

        try
          SetLength(texts, results.Count);
          SetLength(used, results.Count);
          for i := 0 to results.Count - 1 do
          begin
            texts[i] := ResultText(results[i], image);
            used[i] := false;
          end;

          hasUnsupported := false;
          for i := 0 to High(image.Symbols) do
          begin
            if not image.Symbols[i].Supported then
            begin
              hasUnsupported := true;
              continue;
            end;
            readByCpp := image.Symbols[i].ReadByCpp(mode, rotation);
            Inc(stats[mode].Checks);
            if readByCpp then
              Inc(stats[mode].ReadByCpp);

            // the first result that matches this symbol and no other one
            matched := -1;
            for j := 0 to results.Count - 1 do
              if not used[j] and Matches(image.Symbols[i], results[j], texts[j])
              then
              begin
                matched := j;
                break;
              end;

            if (matched >= 0) then
            begin
              used[matched] := true;
              Inc(stats[mode].ReadByDelphi);
              if not readByCpp then
                Inc(stats[mode].DelphiOnly);
            end
            else if readByCpp and Assigned(log) then
              log(Format('%s  missing: %s', [where,
                Abbrev(ExpectedText(image.Symbols[i]))]));
          end;

          // A result for an image with a format variant Delphi does not know
          // (e.g. Code 32 or PZN, read as plain Code 39) is not counted as wrong.
          for j := 0 to results.Count - 1 do
            if not used[j] and not hasUnsupported then
            begin
              Inc(stats[mode].Wrong);
              if Assigned(log) then
                log(Format('%s  WRONG:   %s', [where,
                  Abbrev(EscapeNonGraphical(texts[j]))]));
            end;
        finally
          results.Free;
        end;
      end;
    end;
  finally
    for rotation := 1 to 3 do
      rotated[rotation].Free;
    original.Free;
  end;
end;

function RunFolder(folder: TSampleFolder; modes: TTestModes;
  const log: TProc<string>): TStats;
var
  scanManagers: array [TTestMode] of TScanManager;
  mode: TTestMode;
  image: TSampleImage;
begin
  FillChar(Result, SizeOf(Result), 0);
  for mode := Low(TTestMode) to High(TTestMode) do
    scanManagers[mode] := CreateScanManager(folder, mode);
  try
    for image in folder.Images do
      TestImage(folder, image, modes, scanManagers, log, Result);
  finally
    for mode := Low(TTestMode) to High(TTestMode) do
      scanManagers[mode].Free;
  end;
end;

end.
