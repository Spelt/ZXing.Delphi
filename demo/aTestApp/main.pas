unit main;

{
  * Copyright 2015 E Spelt for test project stuff
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *      http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.

  * Implemented by E. Spelt for Delphi
}
interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Actions,
  System.Diagnostics,
  System.Permissions,
  Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Media,
  FMX.Platform,
  FMX.MultiView,
  FMX.ListView.Types,
  FMX.ListView,
  FMX.Layouts,
  FMX.ActnList,
  FMX.TabControl,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls3D,
  ZXing.BarcodeFormat,
  ZXing.ReadResult,
  ZXing.ScanManager, FMX.Memo.Types;

type
  TMainForm = class(TForm)
    btnStartCamera: TButton;
    btnStopCamera: TButton;
    lblScanStatus: TLabel;
    imgCamera: TImage;
    ToolBar1: TToolBar;
    btnMenu: TButton;
    Layout2: TLayout;
    ToolBar3: TToolBar;
    CameraComponent1: TCameraComponent;
    Memo1: TMemo;
    openDlg: TOpenDialog;
    Camera1: TCamera;
    chkScanAll: TCheckBox;
    chkParallel: TCheckBox;
    procedure btnStartCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure FormDestroy(Sender: TObject);
    procedure chkParallelChange(Sender: TObject);
  private
    { Private declarations }
    fPermissionCamera: String;
    // as an interface: freed with the form, no FreeAndNil
    fScanManager: IScanManager;
    // the time of the scans: the last one and the average over the frames
    fScanStarted: TStopwatch;
    fScanCount: Integer;
    fScanMs: Double;
    procedure ScanFrame;
    procedure ShowResults(const results: TArray<TReadResult>);
    procedure ShowError(e: Exception);
{$IF CompilerVersion >= 35.0}
    // after Delphi 11 Alexandria
    procedure CameraPermissionRequestResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    procedure ExplainReason(Sender: TObject; const APermissions: TClassicStringDynArray;
      const APostRationaleProc: TProc);
{$ELSE}
    // before Delphi 11 Alexandria
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
  ZXing.DecodeHintType,
  ZXing.Parallel;

{$R *.fmx}


procedure TMainForm.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, IInterface(AppEventSvc)) then
  begin
    AppEventSvc.SetApplicationEventHandler(AppEvent);
  end;

  lblScanStatus.Text := '';
  fScanCount := 0;
  fScanMs := 0;

  // One scan manager for all frames, like a real app would do. It scans in
  // a task (ScanAsync) and calls back in the main thread; while a scan runs
  // the next frames are skipped. Pass hints (e.g. ENABLE_INVERSION,
  // TRY_HARDER) as second parameter; the scan manager frees them.
  var hints := TDictionary<TDecodeHintType, TObject>.Create();
  //hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);
  //hints.Add(TDecodeHintType.TRY_HARDER, nil);
  fScanManager := TScanManager.Create(TBarcodeFormat.All, hints);

  // decoding with tasks (ZXing.Parallel): on by default on this device?
  chkParallel.IsChecked := ParallelDecoding;
  chkParallel.Text := Format('Parallel (%d cores)', [TThread.ProcessorCount]);

{$IFDEF ANDROID}
  fPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  CameraComponent1.Active := false;

  // a running scan calls back through Synchronize: wait for it, so that it
  // does not reach a form that is gone
  while (fScanManager <> nil) and fScanManager.Scanning do
    CheckSynchronize(10);
end;

procedure TMainForm.chkParallelChange(Sender: TObject);
begin
  ParallelDecoding := chkParallel.IsChecked;
  fScanCount := 0;
  fScanMs := 0;
end;

{$IF CompilerVersion >= 35.0}
    // after Delphi 11 Alexandria
procedure TMainForm.CameraPermissionRequestResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
{$ELSE}
    // before Delphi 11 Alexandria
procedure TMainForm.CameraPermissionRequestResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
begin
  if (Length(AGrantResults) = 1) and
    (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    CameraComponent1.Active := false;
    CameraComponent1.Quality := FMX.Media.TVideoCaptureQuality.MediumQuality;
    CameraComponent1.Kind := FMX.Media.TCameraKind.BackCamera;
    CameraComponent1.FocusMode := FMX.Media.TFocusMode.ContinuousAutoFocus;
    CameraComponent1.Active := True;
    lblScanStatus.Text := '';
    Memo1.Lines.Clear;
  end
  else
    TDialogService.ShowMessage
      ('Cannot scan for barcodes because the required permissions is not granted')
end;

{$IF CompilerVersion >= 35.0}
    // after Delphi 11 Alexandria
procedure TMainForm.ExplainReason(Sender: TObject;
  const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
{$ELSE}
    // before Delphi 11 Alexandria
procedure TMainForm.ExplainReason(Sender: TObject;
  const APermissions: TArray<string>; const APostRationaleProc: TProc);
{$ENDIF}
begin

  TDialogService.ShowMessage
    ('The app needs to access the camera to scan barcodes ...',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)

end;

procedure TMainForm.btnStartCameraClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([fPermissionCamera],
    CameraPermissionRequestResult, ExplainReason);
end;

procedure TMainForm.btnStopCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := false;
end;

procedure TMainForm.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  // (the camera calls from its own thread: the bitmap and the scan manager
  // are used in the main thread)
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      CameraComponent1.SampleBufferToBitmap(imgCamera.Bitmap, True);
      ScanFrame;
    end);
end;

procedure TMainForm.ScanFrame;
begin
  // A scan still running: this frame is skipped (ScanAsync does nothing
  // then too). The pixels are taken here, in the main thread, so the camera
  // bitmap is free again when ScanAsync returns; the decoding runs in a
  // task and the callback comes in the main thread.
  // (To save battery, an app could also skip every n-th frame here.)
  if (fScanManager = nil) or fScanManager.Scanning then
    exit;
  fScanStarted := TStopwatch.StartNew;
  if chkScanAll.IsChecked then
    // all codes in the frame
    fScanManager.ScanAllAsync(imgCamera.Bitmap,
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
    fScanManager.ScanAsync(imgCamera.Bitmap,
      procedure(r: TReadResult)
      begin
        try
          if (r = nil) then
            ShowResults([])
          else
            ShowResults([r]);
        finally
          r.Free;
        end;
      end, ShowError);
end;

procedure TMainForm.ShowResults(const results: TArray<TReadResult>);
begin
  // the time of this scan and the average, for a look at the device
  var ms := fScanStarted.Elapsed.TotalMilliseconds;
  Inc(fScanCount);
  fScanMs := fScanMs + (ms - fScanMs) / fScanCount;
  var status := Format('%.0f ms (avg %.0f)', [ms, fScanMs]);
  if (Length(results) > 0) then
    status := status + ' *';
  lblScanStatus.Text := status;

  if (Length(results) = 0) then
    exit;
  // the text of every code, with its symbology identifier and for GS1
  // codes also the human readable form, like (01)...(17)...; newest on top,
  // the codes of one frame in their order
  Memo1.Lines.Clear();
  var dt := FormatDateTime('h:nn:ss:z', Now);
  for var i := High(results) downto 0 do
  begin
    var line := results[i].SymbologyIdentifier + ' ' +
      results[i].Text.Replace(#29, '<GS>');
    if results[i].IsGS1 then
      line := line + sLineBreak + '    ' + results[i].GS1HRI;
    Memo1.Lines.Add(dt + ': ' + line);
  end;
end;

procedure TMainForm.ShowError(e: Exception);
begin
  lblScanStatus.Text := e.Message;
end;

{ Make sure the camera is released if you're going away. }
function TMainForm.AppEvent(AAppEvent: TApplicationEvent;
AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.WillBecomeInactive, TApplicationEvent.EnteredBackground,
      TApplicationEvent.WillTerminate:
      CameraComponent1.Active := false;
  end;

  Result := true;
end;

end.
