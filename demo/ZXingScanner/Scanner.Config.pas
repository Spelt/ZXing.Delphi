unit Scanner.Config;

{
  The settings of the scanner: what the settings screen edits, what the scan
  screen scans with. It knows how to make a scan manager for itself
  (CreateScanManager: the format and the hints of ZXing.Delphi) and keeps
  itself in a small JSON file, so the app starts as it was left.

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections,
  ZXing.BarcodeFormat,
  ZXing.ScanManager;

type
  TScannerConfig = class
  private
    FFormats: TList<TBarcodeFormat>;
    function GetFileName: string;
  public
    // decoding
    /// <summary>Also light codes on a dark background (ENABLE_INVERSION).
    /// </summary>
    Inverted: Boolean;
    /// <summary>More accurate on damaged, tiny or rotated codes, slower
    /// (TRY_HARDER).</summary>
    TryHarder: Boolean;
    /// <summary>Every code in the frame with ScanAll, instead of the first
    /// one with Scan.</summary>
    MultiResult: Boolean;
    /// <summary>Also scan downscaled copies of the frame when nothing is
    /// found in it (TScanManager.TryDownscale).</summary>
    Downscale: Boolean;
    /// <summary>Decode with the tasks of the thread pool
    /// (ZXing.Parallel.ParallelDecoding).</summary>
    Parallel: Boolean;
    /// <summary>Also show codes found but not read, to hold the camera
    /// still (TScanManager.ReturnErrors).</summary>
    ShowErrors: Boolean;
    // on a hit
    TrackColor: TAlphaColor;
    Haptic: Boolean;
    Beep: Boolean;

    constructor Create;
    destructor Destroy; override;

    /// <summary>The formats that are read. Empty: the formats of
    /// TBarcodeFormat.Auto.</summary>
    property Formats: TList<TBarcodeFormat> read FFormats;
    /// <summary>The formats of Auto.</summary>
    procedure ResetFormats;
    /// <summary>Whether the format is read.</summary>
    function Reads(format: TBarcodeFormat): Boolean;
    /// <summary>Switches a format on or off.</summary>
    procedure SetFormat(format: TBarcodeFormat; on: Boolean);

    /// <summary>A scan manager for these settings: the format (Auto, All
    /// or the chosen formats as POSSIBLE_FORMATS) and the hints. The hints
    /// belong to the scan manager, which frees them.</summary>
    function CreateScanManager: IScanManager;

    /// <summary>Reads the settings from the file; the defaults when there
    /// is none.</summary>
    procedure Load;
    /// <summary>Writes the settings to the file.</summary>
    procedure Save;
    /// <summary>The settings file, in the documents folder of the app.
    /// </summary>
    property FileName: string read GetFileName;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.JSON,
  ZXing.DecodeHintType,
  Scanner.Formats,
  Scanner.Theme;

{ TScannerConfig }

constructor TScannerConfig.Create;
begin
  inherited Create;
  FFormats := TList<TBarcodeFormat>.Create;
  ResetFormats;
  Inverted := False;
  TryHarder := False;
  MultiResult := False;
  Downscale := True;
  Parallel := TThread.ProcessorCount > 1;
  ShowErrors := True;
  TrackColor := TrackColors[3]; // yellow
  Haptic := True;
  Beep := False;
end;

destructor TScannerConfig.Destroy;
begin
  FFormats.Free;
  inherited;
end;

procedure TScannerConfig.ResetFormats;
begin
  FFormats.Clear;
  for var info in Scanner.Formats.Formats do
    if info.InAuto then
      FFormats.Add(info.Format);
end;

function TScannerConfig.Reads(format: TBarcodeFormat): Boolean;
begin
  Result := FFormats.Contains(format);
end;

procedure TScannerConfig.SetFormat(format: TBarcodeFormat; on: Boolean);
begin
  if on then
  begin
    if not FFormats.Contains(format) then
      FFormats.Add(format);
  end
  else
    FFormats.Remove(format);
end;

function TScannerConfig.CreateScanManager: IScanManager;
var
  hints: TDictionary<TDecodeHintType, TObject>;
  format: TBarcodeFormat;
  possible: TList<TBarcodeFormat>;
  autoCount, autoTotal, extraCount, extraTotal: Integer;
begin
  // The hints: a dictionary of TDecodeHintType; the flags have no value.
  hints := TDictionary<TDecodeHintType, TObject>.Create;
  if Inverted then
    hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);
  if TryHarder then
    hints.Add(TDecodeHintType.TRY_HARDER, nil);

  // The formats: all of Auto -> Auto (the default readers, nothing to
  // list); all of Auto and every other one -> All; else the chosen ones
  // as the hint POSSIBLE_FORMATS (the scan manager frees the list).
  autoCount := 0;
  autoTotal := 0;
  extraCount := 0;
  extraTotal := 0;
  for var info in Scanner.Formats.Formats do
    if info.InAuto then
    begin
      Inc(autoTotal);
      if Reads(info.Format) then
        Inc(autoCount);
    end
    else
    begin
      Inc(extraTotal);
      if Reads(info.Format) then
        Inc(extraCount);
    end;
  if (autoCount = autoTotal) and (extraCount = extraTotal) then
    format := TBarcodeFormat.All
  else if (autoCount = autoTotal) and (extraCount = 0) then
    format := TBarcodeFormat.Auto
  else
  begin
    format := TBarcodeFormat.Auto;
    possible := TList<TBarcodeFormat>.Create;
    possible.AddRange(FFormats);
    hints.Add(TDecodeHintType.POSSIBLE_FORMATS, possible);
  end;

  Result := TScanManager.Create(format, hints);
  Result.TryDownscale := Downscale;
  Result.ReturnErrors := ShowErrors;
end;

function TScannerConfig.GetFileName: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'ZXingScanner.json');
end;

procedure TScannerConfig.Load;
var
  json: TJSONObject;
  formats: TJSONArray;
  format: TBarcodeFormat;
begin
  if not TFile.Exists(FileName) then
    exit;
  json := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName, TEncoding.UTF8))
    as TJSONObject;
  if json = nil then
    exit;
  try
    Inverted := json.GetValue<Boolean>('inverted', Inverted);
    TryHarder := json.GetValue<Boolean>('tryHarder', TryHarder);
    MultiResult := json.GetValue<Boolean>('multiResult', MultiResult);
    Downscale := json.GetValue<Boolean>('downscale', Downscale);
    Parallel := json.GetValue<Boolean>('parallel', Parallel);
    ShowErrors := json.GetValue<Boolean>('showErrors', ShowErrors);
    TrackColor := TAlphaColor(json.GetValue<Cardinal>('trackColor', TrackColor));
    Haptic := json.GetValue<Boolean>('haptic', Haptic);
    Beep := json.GetValue<Boolean>('beep', Beep);
    if json.TryGetValue<TJSONArray>('formats', formats) then
    begin
      FFormats.Clear;
      for var item in formats do
        if FormatByName(item.Value, format) then
          FFormats.Add(format);
    end;
  finally
    json.Free;
  end;
end;

procedure TScannerConfig.Save;
var
  json: TJSONObject;
  formats: TJSONArray;
begin
  json := TJSONObject.Create;
  try
    json.AddPair('inverted', TJSONBool.Create(Inverted));
    json.AddPair('tryHarder', TJSONBool.Create(TryHarder));
    json.AddPair('multiResult', TJSONBool.Create(MultiResult));
    json.AddPair('downscale', TJSONBool.Create(Downscale));
    json.AddPair('parallel', TJSONBool.Create(Parallel));
    json.AddPair('showErrors', TJSONBool.Create(ShowErrors));
    json.AddPair('trackColor', TJSONNumber.Create(Int64(TrackColor)));
    json.AddPair('haptic', TJSONBool.Create(Haptic));
    json.AddPair('beep', TJSONBool.Create(Beep));
    formats := TJSONArray.Create;
    for var format in FFormats do
      formats.Add(FormatName(format));
    json.AddPair('formats', formats);
    TFile.WriteAllText(FileName, json.Format(2), TEncoding.UTF8);
  finally
    json.Free;
  end;
end;

end.
