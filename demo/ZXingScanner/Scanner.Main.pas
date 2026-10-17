unit Scanner.Main;

{
  The scan screen: the camera fills the screen, the codes it finds get an
  outline, their text comes in the sheet at the bottom. One TScanManager
  (held as IScanManager, freed with the form) scans every frame with
  ScanAsync or ScanAllAsync: the decoding runs in a task, the result comes
  back in the main thread, and frames that arrive while a scan runs are
  skipped.

  On the desktop (no camera, or to test with a file) an image can be
  opened with the button at the top left, dropped on the window, or given
  on the command line.

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  System.Diagnostics,
  System.Permissions,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Media,
  FMX.Platform,
  FMX.Ani,
  FMX.Controls.Presentation,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager,
  Scanner.Config,
  Scanner.Settings;

type
  /// <summary>A barcode found in a frame, as the screen keeps it: the
  /// results of the library are freed after each scan.</summary>
  THit = record
    Text: string;
    Format: string;
    /// <summary>The human readable form of GS1 data, '' for other codes.
    /// </summary>
    GS1: string;
    /// <summary>'' for a code that was read; else why it could not be
    /// (TScanManager.ReturnErrors).</summary>
    Error: string;
    /// <summary>The corners in the frame.</summary>
    Corners: TArray<TPointF>;
    function IsUrl: Boolean;
  end;

  TMainForm = class(TForm)
    pbCamera: TPaintBox;
    layTop: TLayout;
    btnLeft: TCircle;
    pthLeft: TPath;
    btnSettings: TCircle;
    pthSettings: TPath;
    pillBrand: TRectangle;
    dotBrand: TCircle;
    aniDot: TFloatAnimation;
    lblBrand: TLabel;
    pillResult: TRectangle;
    pthKind: TPath;
    lblHit: TLabel;
    pthGo: TPath;
    sheet: TRectangle;
    layGrip: TLayout;
    grip: TRectangle;
    lblHint: TLabel;
    layHeader: TLayout;
    chipFormat: TRectangle;
    lblFormat: TLabel;
    lblTime: TLabel;
    lblCount: TLabel;
    lblSpeed: TLabel;
    lblCopyAll: TLabel;
    lblText: TLabel;
    layResults: TLayout;
    CameraComponent: TCameraComponent;
    tmrTick: TTimer;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure FormDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure pbCameraPaint(Sender: TObject; Canvas: TCanvas);
    procedure btnLeftClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure pillResultClick(Sender: TObject);
    procedure lblTextClick(Sender: TObject);
    procedure lblCopyAllClick(Sender: TObject);
    procedure CameraComponentSampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure tmrTickTimer(Sender: TObject);
  private
    fConfig: TScannerConfig;
    fSettings: TSettingsForm;
    // as an interface: freed with the form, no FreeAndNil
    fScanManager: IScanManager;
    // the frame of the camera, or the image that was opened
    fFrame: TBitmap;
    fStill: Boolean;
    fCameraWanted: Boolean;
    fPermissionCamera: string;
    // the codes of the last frame with codes (the outlines) and the codes
    // that were read last (the sheet)
    fHits: TArray<THit>;
    fHitWatch: TStopwatch;
    fShown: TArray<THit>;
    fShownWatch: TStopwatch;
    fLastKey: string;
    fCopiedWatch: TStopwatch;
    // the time of the scans: the last one and the average
    fScanWatch: TStopwatch;
    fScanCount: Integer;
    fScanMs: Double;
    // the frame on the screen: frame * scale + offset
    fScale: Single;
    fOffset: TPointF;

    function IsDesktop: Boolean;
    function HasCamera: Boolean;
    procedure ApplyConfig;
    procedure StartCamera;
    procedure StopCamera;
    procedure ResumeCamera;
    procedure LoadImage(const fileName: string);
    procedure ScanFrame;
    procedure ShowResults(const results: TArray<TReadResult>);
    procedure ShowError(e: Exception);
    procedure Feedback;
    procedure ComputeMapping;
    function ToView(const p: TPointF): TPointF;
    procedure DrawOutline(Canvas: TCanvas; const hit: THit; number: Integer);
    procedure UpdateLeftButton;
    procedure UpdateSheet;
    procedure LayoutSheet;
    procedure BuildRows;
    procedure UpdatePill;
    procedure SetHint(const text: string);
    procedure Copied(const text: string);
    procedure RowCopyClick(Sender: TObject);
    procedure SettingsDone(Sender: TObject);
{$IF CompilerVersion >= 35.0}
    // from Delphi 11 Alexandria
    procedure CameraPermissionRequestResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    procedure ExplainReason(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const APostRationaleProc: TProc);
{$ELSE}
    // Delphi 10.4 Sydney
    procedure CameraPermissionRequestResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
    procedure ExplainReason(Sender: TObject; const APermissions: TArray<string>;
      const APostRationaleProc: TProc);
{$ENDIF}
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService,
  ZXing.ResultPoint,
  ZXing.Parallel,
  Scanner.Formats,
  Scanner.Theme,
  Scanner.Feedback;

{$R *.fmx}

const
  HintCamera = 'Point the camera at a barcode';
  HintNoCamera = 'No camera found. Open an image to scan it.';
  HintHoldStill = 'A code is there: hold the camera still';
  HintScanning = 'Scanning' + #8230;
  HintNothing = 'No barcode in this image';
  HintNoFormats = 'No format chosen: pick one in the settings';
  // after this many ms without the code in the frame its outline goes
  OutlineLifetime = 800;

{ THit }

function THit.IsUrl: Boolean;
begin
  Result := Text.StartsWith('http://', True) or Text.StartsWith('https://', True);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  appEvents: IFMXApplicationEventService;
begin
  fConfig := TScannerConfig.Create;
  fConfig.Load;
  fFrame := TBitmap.Create;

  fSettings := TSettingsForm.Create(Self);
  fSettings.Config := fConfig;
  fSettings.OnDone := SettingsDone;

  // the camera is released when the app goes to the background
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, appEvents) then
    appEvents.SetApplicationEventHandler(AppEvent);

  // the look: the icons and the fonts (the layout is in the form)
  SetIcon(pthSettings, icoSettings, clText);
  SetIcon(pthGo, icoChevron, clTextDark);
  StyleLabel(lblBrand, 14, clText, True);
  StyleLabel(lblHit, 15, clTextDark, True);
  StyleLabel(lblHint, 15, clTextMuted, False, False, True);
  StyleLabel(lblFormat, 12, clAccent, False, True);
  StyleLabel(lblTime, 13, clTextMuted);
  StyleLabel(lblCount, 17, clText, True);
  StyleLabel(lblSpeed, 13, clTextMuted);
  MakeLink(lblCopyAll, 13, clText);
  StyleLabel(lblText, 17, clText, False, True, True);
  lblHit.Trimming := TTextTrimming.Character;
  pillResult.Visible := False;

{$IFDEF ANDROID}
  fPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
{$ENDIF}
  ApplyConfig;
  UpdateSheet;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  StopCamera;
  // a running scan calls back through Synchronize: wait for it, so that it
  // does not reach a form that is gone
  while (fScanManager <> nil) and fScanManager.Scanning do
    CheckSynchronize(10);
  fScanManager := nil;
  fFrame.Free;
  fConfig.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if fCameraWanted or fStill then
    exit;
  // an image on the command line (desktop): scan that instead
  if IsDesktop and (ParamCount > 0) and FileExists(ParamStr(1)) then
    LoadImage(ParamStr(1))
  else
    StartCamera;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  UpdateSheet;
  UpdatePill;
end;

function TMainForm.IsDesktop: Boolean;
begin
  Result := TOSVersion.Platform in [TOSVersion.TPlatform.pfWindows,
    TOSVersion.TPlatform.pfMacOS, TOSVersion.TPlatform.pfLinux];
end;

function TMainForm.HasCamera: Boolean;
begin
  Result := (TCaptureDeviceManager.Current <> nil) and
    (TCaptureDeviceManager.Current.DefaultVideoCaptureDevice <> nil);
end;

procedure TMainForm.ApplyConfig;
begin
  // The scan manager for the settings: the format and the hints. Held as
  // IScanManager: the previous one is freed here, after the scan that may
  // still run in it.
  ParallelDecoding := fConfig.Parallel;
  fScanManager := fConfig.CreateScanManager;
  pillResult.Fill.Color := fConfig.TrackColor;
  fScanCount := 0;
  fScanMs := 0;
  fLastKey := '';
end;

{ the camera }

procedure TMainForm.StartCamera;
begin
  if not HasCamera then
  begin
    SetHint(HintNoCamera);
    UpdateLeftButton;
    exit;
  end;
  // the permission (Android asks the user; the other platforms grant it)
  PermissionsService.RequestPermissions([fPermissionCamera],
    CameraPermissionRequestResult, ExplainReason);
end;

{$IF CompilerVersion >= 35.0}
procedure TMainForm.CameraPermissionRequestResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
{$ELSE}
procedure TMainForm.CameraPermissionRequestResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
begin
  if (Length(AGrantResults) = 1) and
    (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    CameraComponent.Active := False;
    CameraComponent.Quality := TVideoCaptureQuality.MediumQuality;
    CameraComponent.Kind := TCameraKind.BackCamera;
    CameraComponent.FocusMode := TFocusMode.ContinuousAutoFocus;
    try
      CameraComponent.Active := True;
    except
      on e: Exception do
      begin
        SetHint(e.Message);
        exit;
      end;
    end;
    fCameraWanted := True;
    aniDot.Enabled := True;
    if Length(fShown) = 0 then
      SetHint(HintCamera);
  end
  else
    SetHint('The camera is not allowed: nothing to scan');
  UpdateLeftButton;
end;

{$IF CompilerVersion >= 35.0}
procedure TMainForm.ExplainReason(Sender: TObject;
  const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
{$ELSE}
procedure TMainForm.ExplainReason(Sender: TObject;
  const APermissions: TArray<string>; const APostRationaleProc: TProc);
{$ENDIF}
begin
  TDialogService.ShowMessage('The app needs the camera to scan barcodes',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end);
end;

procedure TMainForm.StopCamera;
begin
  if CameraComponent.Active then
  begin
    // (a device without a torch raises when the torch is touched)
    if CameraComponent.HasTorch then
      CameraComponent.TorchMode := TTorchMode.ModeOff;
    CameraComponent.Active := False;
  end;
  aniDot.Enabled := False;
  dotBrand.Opacity := 1;
end;

procedure TMainForm.ResumeCamera;
begin
  fStill := False;
  fFrame.SetSize(0, 0);
  fHits := nil;
  fShown := nil;
  pillResult.Visible := False;
  SetHint(HintCamera);
  UpdateSheet;
  UpdateLeftButton;
  pbCamera.Repaint;
  StartCamera;
end;

procedure TMainForm.CameraComponentSampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  // the camera calls from its own thread: the frame and the scan manager
  // are used in the main thread
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      if fStill or not CameraComponent.Active then
        exit;
      CameraComponent.SampleBufferToBitmap(fFrame, True);
      pbCamera.Repaint;
      ScanFrame;
    end);
end;

{ an image instead of the camera (desktop) }

procedure TMainForm.LoadImage(const fileName: string);
begin
  try
    fFrame.LoadFromFile(fileName);
  except
    on e: Exception do
    begin
      SetHint('Not an image: ' + ExtractFileName(fileName));
      exit;
    end;
  end;
  StopCamera;
  fStill := True;
  fHits := nil;
  fShown := nil;
  fLastKey := '';
  pillResult.Visible := False;
  SetHint(HintScanning);
  UpdateSheet;
  UpdateLeftButton;
  pbCamera.Repaint;
  ScanFrame;
end;

procedure TMainForm.FormDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  if Length(Data.Files) > 0 then
    Operation := TDragOperation.Copy;
end;

procedure TMainForm.FormDragDrop(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  if Length(Data.Files) > 0 then
    LoadImage(Data.Files[0]);
end;

{ the scan }

procedure TMainForm.ScanFrame;
begin
  // A scan still running: this frame is skipped (ScanAsync does nothing
  // then either). The pixels are taken here, in the main thread, so the
  // frame is free again when ScanAsync returns; the decoding runs in a
  // task and the callback comes in the main thread.
  if (fScanManager = nil) or fScanManager.Scanning or fFrame.IsEmpty then
    exit;
  // (without a format the library would read every format of Auto)
  if fConfig.Formats.Count = 0 then
  begin
    SetHint(HintNoFormats);
    exit;
  end;
  fScanWatch := TStopwatch.StartNew;
  if fConfig.MultiResult then
    // every code in the frame
    fScanManager.ScanAllAsync(fFrame,
      procedure(results: TObjectList<TReadResult>)
      begin
        try
          ShowResults(results.ToArray);
        finally
          results.Free;
        end;
      end, ShowError)
  else
    // the first code
    fScanManager.ScanAsync(fFrame,
      procedure(r: TReadResult)
      begin
        try
          if r = nil then
            ShowResults([])
          else
            ShowResults([r]);
        finally
          r.Free;
        end;
      end, ShowError);
end;

procedure TMainForm.ShowResults(const results: TArray<TReadResult>);
var
  hits, shown: TArray<THit>;
  key: string;
begin
  // the time of this scan and the average
  var ms := fScanWatch.Elapsed.TotalMilliseconds;
  Inc(fScanCount);
  fScanMs := fScanMs + (ms - fScanMs) / fScanCount;
  var cores := 'sequential';
  if fConfig.Parallel then
    cores := Format('%d cores', [TThread.ProcessorCount]);
  lblSpeed.Text := Format('%.0f ms ' + #183 + ' %s', [ms, cores]);

  // the results as the screen keeps them
  SetLength(hits, Length(results));
  for var i := 0 to High(results) do
  begin
    var r := results[i];
    hits[i].Text := r.Text.Replace(#29, '<GS>');
    hits[i].Format := FormatName(r.BarcodeFormat);
    if r.IsGS1 then
      hits[i].GS1 := r.GS1HRI;
    hits[i].Error := r.Error;
    var corners := r.Position;
    SetLength(hits[i].Corners, Length(corners));
    for var k := 0 to High(corners) do
      hits[i].Corners[k] := TPointF.Create(corners[k].X, corners[k].Y);
  end;

  if Length(hits) = 0 then
  begin
    // nothing in this frame: the outline of the last one goes after a
    // while (the timer), the sheet keeps the last code
    if fStill then
      SetHint(HintNothing);
    exit;
  end;

  fHits := hits;
  fHitWatch := TStopwatch.StartNew;

  // the codes that were read, for the sheet; the others were found but
  // not read (TScanManager.ReturnErrors): only their outline
  shown := nil;
  key := '';
  for var hit in hits do
    if hit.Error = '' then
    begin
      shown := shown + [hit];
      key := key + hit.Format + ':' + hit.Text + #10;
    end;
  if Length(shown) > 0 then
  begin
    fShown := shown;
    fShownWatch := TStopwatch.StartNew;
    fCopiedWatch.Reset;
    // a beep and a vibration once per code, not on every frame
    if key <> fLastKey then
    begin
      fLastKey := key;
      Feedback;
    end;
  end
  else if Length(fShown) = 0 then
    SetHint(HintHoldStill);

  UpdateSheet;
  UpdatePill;
  pbCamera.Repaint;
end;

procedure TMainForm.ShowError(e: Exception);
begin
  SetHint(e.Message);
end;

procedure TMainForm.Feedback;
begin
  if fConfig.Haptic then
    Vibrate;
  if fConfig.Beep then
    Scanner.Feedback.Beep;
end;

procedure TMainForm.tmrTickTimer(Sender: TObject);
begin
  // the age of the code in the sheet
  if (Length(fShown) > 0) and not fStill then
  begin
    if fCopiedWatch.IsRunning and (fCopiedWatch.ElapsedMilliseconds < 1500) then
      lblTime.Text := 'Copied'
    else if fShownWatch.ElapsedMilliseconds < 3000 then
      lblTime.Text := 'Just now'
    else
      lblTime.Text := Format('%d s ago', [fShownWatch.ElapsedMilliseconds div 1000]);
  end;
  // the outline of a code that left the frame
  if (Length(fHits) > 0) and not fStill and
    (fHitWatch.ElapsedMilliseconds > OutlineLifetime) then
  begin
    fHits := nil;
    pillResult.Visible := False;
    pbCamera.Repaint;
  end;
end;

{ the frame and the outlines }

procedure TMainForm.ComputeMapping;
var
  sx, sy: Single;
begin
  if fFrame.IsEmpty then
    exit;
  sx := pbCamera.Width / fFrame.Width;
  sy := pbCamera.Height / fFrame.Height;
  // the camera fills the screen (cropped), an image is shown whole
  if fStill then
    fScale := Min(sx, sy)
  else
    fScale := Max(sx, sy);
  fOffset := TPointF.Create((pbCamera.Width - fFrame.Width * fScale) / 2,
    (pbCamera.Height - fFrame.Height * fScale) / 2);
end;

function TMainForm.ToView(const p: TPointF): TPointF;
begin
  Result := TPointF.Create(p.X * fScale + fOffset.X, p.Y * fScale + fOffset.Y);
end;

procedure TMainForm.pbCameraPaint(Sender: TObject; Canvas: TCanvas);
begin
  if fFrame.IsEmpty then
    exit;
  ComputeMapping;
  Canvas.DrawBitmap(fFrame, TRectF.Create(0, 0, fFrame.Width, fFrame.Height),
    TRectF.Create(fOffset.X, fOffset.Y, fOffset.X + fFrame.Width * fScale,
    fOffset.Y + fFrame.Height * fScale), 1, True);
  for var i := 0 to High(fHits) do
    DrawOutline(Canvas, fHits[i], i + 1);
end;

procedure TMainForm.DrawOutline(Canvas: TCanvas; const hit: THit;
  number: Integer);
var
  pts: TArray<TPointF>;
  path: TPathData;
  color: TAlphaColor;

  // the corner at a, with its legs towards p and q
  procedure Bracket(const a, p, q: TPointF);
    function Leg(const towards: TPointF): TPointF;
    var
      d: Single;
    begin
      d := a.Distance(towards);
      if d < 1 then
        exit(a);
      Result := a + (towards - a) * (Min(d * 0.3, 34) / d);
    end;

  begin
    path.Clear;
    path.MoveTo(Leg(p));
    path.LineTo(a);
    path.LineTo(Leg(q));
    Canvas.DrawPath(path, 1);
  end;

begin
  SetLength(pts, Length(hit.Corners));
  for var i := 0 to High(pts) do
    pts[i] := ToView(hit.Corners[i]);
  if Length(pts) < 2 then
    exit;

  if hit.Error = '' then
    color := fConfig.TrackColor
  else
    color := clError;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := color;
  Canvas.Stroke.Cap := TStrokeCap.Round;
  Canvas.Stroke.Join := TStrokeJoin.Round;
  if hit.Error = '' then
    Canvas.Stroke.Dash := TStrokeDash.Solid
  else
    Canvas.Stroke.Dash := TStrokeDash.Dash;
  if fConfig.MultiResult then
    Canvas.Stroke.Thickness := 3
  else
    Canvas.Stroke.Thickness := 4;

  path := TPathData.Create;
  try
    if Length(pts) < 4 then
    begin
      path.MoveTo(pts[0]);
      for var i := 1 to High(pts) do
        path.LineTo(pts[i]);
      Canvas.DrawPath(path, 1);
    end
    else
      // the 4 corners: top left, top right, bottom right, bottom left
      for var i := 0 to 3 do
        Bracket(pts[i], pts[(i + 3) mod 4], pts[(i + 1) mod 4]);
  finally
    path.Free;
  end;

  // the number of the code, at its top left corner (multi result)
  if fConfig.MultiResult and (hit.Error = '') then
  begin
    var r := TRectF.Create(pts[0].X - 11, pts[0].Y - 11, pts[0].X + 11,
      pts[0].Y + 11);
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := color;
    Canvas.FillEllipse(r, 1);
    Canvas.Fill.Color := clTextDark;
    Canvas.Font.Size := 12;
    Canvas.Font.Style := [TFontStyle.fsBold];
    Canvas.FillText(r, IntToStr(number), False, 1, [], TTextAlign.Center,
      TTextAlign.Center);
  end;
end;

{ the top bar }

procedure TMainForm.UpdateLeftButton;
begin
  if fStill then
  begin
    // back to the camera
    SetIcon(pthLeft, icoCamera, clText);
    btnLeft.Visible := HasCamera;
  end
  else if IsDesktop then
  begin
    // an image to scan
    SetIcon(pthLeft, icoFolder, clText);
    btnLeft.Visible := True;
  end
  else
  begin
    // the torch, where there is one
    btnLeft.Visible := CameraComponent.Active and CameraComponent.HasTorch;
    if btnLeft.Visible and (CameraComponent.TorchMode = TTorchMode.ModeOn) then
      SetIcon(pthLeft, icoTorch, clAccent)
    else
      SetIcon(pthLeft, icoTorch, clText);
  end;
end;

procedure TMainForm.btnLeftClick(Sender: TObject);
begin
  if fStill then
    ResumeCamera
  else if IsDesktop then
  begin
    if dlgOpen.Execute then
      LoadImage(dlgOpen.FileName);
  end
  else if CameraComponent.HasTorch then
  begin
    if CameraComponent.TorchMode = TTorchMode.ModeOn then
      CameraComponent.TorchMode := TTorchMode.ModeOff
    else
      CameraComponent.TorchMode := TTorchMode.ModeOn;
    UpdateLeftButton;
  end;
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
begin
  // the camera rests while the settings are open; SettingsDone starts it
  // again with the new settings
  StopCamera;
  fSettings.Show;
end;

procedure TMainForm.SettingsDone(Sender: TObject);
begin
  fConfig.Save;
  ApplyConfig;
  fHits := nil;
  fShown := nil;
  pillResult.Visible := False;
  pbCamera.Repaint;
  if fStill then
  begin
    SetHint(HintScanning);
    UpdateSheet;
    ScanFrame;
  end
  else
  begin
    SetHint(HintCamera);
    UpdateSheet;
    if fCameraWanted then
      StartCamera;
  end;
end;

{ the sheet }

procedure TMainForm.SetHint(const text: string);
begin
  lblHint.Text := text;
  if Length(fShown) = 0 then
    UpdateSheet;
end;

procedure TMainForm.UpdateSheet;
var
  shown: Boolean;
  text: string;
begin
  shown := Length(fShown) > 0;
  lblHint.Visible := not shown;
  layHeader.Visible := shown;
  lblText.Visible := shown and not fConfig.MultiResult;
  layResults.Visible := shown and fConfig.MultiResult;
  chipFormat.Visible := not fConfig.MultiResult;
  lblTime.Visible := not fConfig.MultiResult;
  lblCount.Visible := fConfig.MultiResult;
  lblCopyAll.Visible := fConfig.MultiResult;

  if not shown then
    lblHint.Height := Max(22, MeasureText(lblHint, lblHint.Text,
      sheet.Width - sheet.Padding.Left - sheet.Padding.Right, True).Height)
  else if fConfig.MultiResult then
  begin
    lblCount.Text := Format('%d codes found', [Length(fShown)]);
    BuildRows;
  end
  else
  begin
    // the first code: its format, its text and for GS1 data also the
    // readable form, like (01)04412345678909(17)...
    lblFormat.Text := fShown[0].Format;
    chipFormat.Width := lblFormat.Width + 18;
    lblTime.Text := 'Just now';
    text := fShown[0].Text;
    if fShown[0].GS1 <> '' then
      text := text + sLineBreak + fShown[0].GS1;
    lblText.Text := text;
    lblText.Height := Min(140, MeasureText(lblText, text,
      sheet.Width - sheet.Padding.Left - sheet.Padding.Right, True).Height + 2);
  end;
  LayoutSheet;
end;

procedure TMainForm.LayoutSheet;
var
  h: Single;
begin
  // as high as what it shows
  h := sheet.Padding.Top + sheet.Padding.Bottom;
  for var i := 0 to sheet.ControlsCount - 1 do
  begin
    var c := sheet.Controls[i];
    if c.Visible then
      h := h + c.Height + c.Margins.Top + c.Margins.Bottom;
  end;
  sheet.Height := h;
end;

procedure TMainForm.BuildRows;
const
  RowHeight = 54;
begin
  // a row per code: its number (as at its outline), format and text, and
  // a button that copies the text
  layResults.BeginUpdate;
  try
    while layResults.ControlsCount > 0 do
      layResults.Controls[0].Free;
    for var i := 0 to High(fShown) do
    begin
      var row := TLayout.Create(layResults);
      row.Parent := layResults;
      row.Align := TAlignLayout.Top;
      row.Position.Y := i * RowHeight;
      row.Height := RowHeight;

      var badge := TCircle.Create(row);
      badge.Parent := row;
      badge.Align := TAlignLayout.Left;
      badge.Width := 26;
      badge.Margins.Rect := TRectF.Create(0, 14, 0, 14);
      badge.Fill.Color := fConfig.TrackColor;
      badge.Stroke.Kind := TBrushKind.None;
      var number := TLabel.Create(badge);
      number.Parent := badge;
      number.Align := TAlignLayout.Contents;
      number.TextSettings.HorzAlign := TTextAlign.Center;
      number.Text := IntToStr(i + 1);
      StyleLabel(number, 13, clTextDark, True);

      var copy := TRectangle.Create(row);
      copy.Parent := row;
      copy.Align := TAlignLayout.Right;
      copy.Width := 44;
      copy.Margins.Rect := TRectF.Create(0, 5, 0, 5);
      copy.XRadius := 12;
      copy.YRadius := 12;
      // (filled: a shape without a fill does not get the taps on its inside)
      copy.Fill.Color := clSheet;
      copy.Stroke.Kind := TBrushKind.None;
      copy.Tag := i;
      copy.OnClick := RowCopyClick;
      var icon := TPath.Create(copy);
      icon.Parent := copy;
      icon.Align := TAlignLayout.Contents;
      icon.Margins.Rect := TRectF.Create(13, 13, 13, 13);
      SetIcon(icon, icoCopy, clTextMuted);

      var texts := TLayout.Create(row);
      texts.Parent := row;
      texts.Align := TAlignLayout.Client;
      texts.Margins.Rect := TRectF.Create(12, 8, 4, 8);
      var lblF := TLabel.Create(texts);
      lblF.Parent := texts;
      lblF.Align := TAlignLayout.Top;
      lblF.Height := 16;
      lblF.Text := fShown[i].Format;
      StyleLabel(lblF, 11, clTextMuted, False, True);
      var lblT := TLabel.Create(texts);
      lblT.Parent := texts;
      lblT.Align := TAlignLayout.Client;
      lblT.Trimming := TTextTrimming.Character;
      lblT.Text := fShown[i].Text;
      StyleLabel(lblT, 14, clText, False, True);
    end;
    layResults.Height := Length(fShown) * RowHeight;
  finally
    layResults.EndUpdate;
  end;
end;

procedure TMainForm.UpdatePill;
var
  hit: THit;
  w, top, left, minY, maxY, cx: Single;
begin
  // the text of the code, under its outline (single result)
  if fConfig.MultiResult or (Length(fHits) = 0) or (fHits[0].Error <> '') or
    fFrame.IsEmpty then
  begin
    pillResult.Visible := False;
    exit;
  end;
  hit := fHits[0];
  ComputeMapping;
  minY := MaxSingle;
  maxY := -MaxSingle;
  cx := 0;
  for var c in hit.Corners do
  begin
    var p := ToView(c);
    minY := Min(minY, p.Y);
    maxY := Max(maxY, p.Y);
    cx := cx + p.X / Length(hit.Corners);
  end;

  if hit.IsUrl then
    SetIcon(pthKind, icoGlobe, clTextDark)
  else
    SetIcon(pthKind, icoText, clTextDark);
  // the first line of the text (the sheet has all of it)
  var line := hit.Text;
  var p := line.IndexOfAny([#10, #13]);
  if p >= 0 then
    line := line.Substring(0, p) + #8230;
  lblHit.Text := line;
  // (the icon, the gaps, the chevron and a little room for the text)
  w := MeasureText(lblHit, line, 10000, False).Width + 12 + 18 + 8 + 6 +
    16 + 14 + 10;
  w := Min(w, pbCamera.Width - 40);
  pillResult.Width := w;

  top := maxY + 24;
  if top + pillResult.Height > sheet.Position.Y - 10 then
    top := minY - 24 - pillResult.Height;
  top := Max(top, layTop.Position.Y + layTop.Height + 10);
  left := EnsureRange(cx - w / 2, 20, Max(20, pbCamera.Width - 20 - w));
  pillResult.Position.Point := TPointF.Create(left, top);
  pillResult.Visible := True;
  pillResult.BringToFront;
end;

{ copying }

procedure TMainForm.Copied(const text: string);
begin
  CopyToClipboard(text);
  fCopiedWatch := TStopwatch.StartNew;
  lblTime.Text := 'Copied';
end;

procedure TMainForm.pillResultClick(Sender: TObject);
begin
  if Length(fHits) > 0 then
    Copied(fHits[0].Text);
end;

procedure TMainForm.lblTextClick(Sender: TObject);
begin
  if Length(fShown) > 0 then
    Copied(fShown[0].Text);
end;

procedure TMainForm.RowCopyClick(Sender: TObject);
begin
  var i := TComponent(Sender).Tag;
  if (i >= 0) and (i <= High(fShown)) then
    Copied(fShown[i].Text);
end;

procedure TMainForm.lblCopyAllClick(Sender: TObject);
var
  all: string;
begin
  for var hit in fShown do
    all := all + hit.Text + sLineBreak;
  Copied(all.TrimRight);
  lblCopyAll.Text := 'Copied';
  TThread.ForceQueue(nil,
    procedure
    begin
      lblCopyAll.Text := 'Copy all';
    end, 1500);
end;

{ the app in the background: the camera is released }

function TMainForm.AppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.WillBecomeInactive, TApplicationEvent.EnteredBackground,
      TApplicationEvent.WillTerminate:
      StopCamera;
    TApplicationEvent.BecameActive:
      if fCameraWanted and not fStill and not CameraComponent.Active and
        not fSettings.Visible then
      begin
        CameraComponent.Active := True;
        aniDot.Enabled := True;
      end;
  end;
  Result := True;
end;

end.
