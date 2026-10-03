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
  System.Threading,
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
    procedure btnStartCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fPermissionCamera: String;
    fScanInProgress: Boolean;
    fFrameTake: Integer;
    fScanManager: TScanManager;
    procedure ParseImage(const scanBitmap: TBitmap);
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
  ZXing.DecodeHintType;

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
  fFrameTake := 0;
  fScanInProgress := false;

  // One scan manager for all frames, like a real app would do. Only one scan
  // may use it at a time. Pass hints (e.g. ENABLE_INVERSION, TRY_HARDER) as
  // second parameter; the scan manager frees them.

  var hints := TDictionary<TDecodeHintType, TObject>.Create();
  //hints.Add(TDecodeHintType.ENABLE_INVERSION, nil);
  hints.Add(TDecodeHintType.TRY_HARDER, nil);
  fScanManager := TScanManager.Create(TBarcodeFormat.Auto, hints);

{$IFDEF ANDROID}
  fPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  CameraComponent1.Active := false;

  // Wait for a running scan; it calls Synchronize, so keep processing those.
  while fScanInProgress do
    CheckSynchronize(10);

  FreeAndNil(fScanManager);
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

procedure TMainForm.CameraComponent1SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin

  TThread.Synchronize(TThread.CurrentThread,
  procedure
  var
    scanBitmap: TBitmap;
  begin
    CameraComponent1.SampleBufferToBitmap(imgCamera.Bitmap, True);

    if (fScanInProgress) or (fScanManager = nil) then
    begin
      exit;
    end;

    { This code will take every 4 frame. }
    inc(fFrameTake);
    if (fFrameTake mod 4 <> 0) then
    begin
      exit;
    end;

    // Set the flag here in the main thread, before the scan thread starts,
    // so the next frame can never start a second scan.
    fScanInProgress := True;

    // The scan thread gets its own copy of the frame and frees it.
    scanBitmap := TBitmap.Create();
    scanBitmap.Assign(imgCamera.Bitmap);

    ParseImage(scanBitmap);
  end);

end;

procedure TMainForm.ParseImage(const scanBitmap: TBitmap);
var
  scanAll: Boolean;
begin
  // read the check box here, in the main thread
  scanAll := chkScanAll.IsChecked;

  TThread.CreateAnonymousThread(
    procedure
    var
      ReadResult: TReadResult;
      results: TObjectList<TReadResult>;
      lines: TArray<string>;
    begin
      results := nil;

      try

        try
          // all codes in the frame, or the first one
          if scanAll then
            results := fScanManager.ScanAll(scanBitmap)
          else
          begin
            results := TObjectList<TReadResult>.Create(true);
            ReadResult := fScanManager.Scan(scanBitmap);
            if (ReadResult <> nil) then
              results.Add(ReadResult);
          end;

          // the text of every code, with its symbology identifier and for
          // GS1 codes also the human readable form, like (01)...(17)...
          for ReadResult in results do
          begin
            var line := ReadResult.SymbologyIdentifier + ' ' +
              ReadResult.Text.Replace(#29, '<GS>');
            if ReadResult.IsGS1 then
              line := line + sLineBreak + '    ' + ReadResult.GS1HRI;
            lines := lines + [line];
          end;
        except
          on E: Exception do
          begin
            TThread.Synchronize(TThread.CurrentThread,
              procedure
              begin
                lblScanStatus.Text := E.Message;
              end);
            exit;
          end;
        end;

        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin

            if (Length(lblScanStatus.Text) > 10) then
            begin
              lblScanStatus.Text := '*';
            end;

            if Length(lines) > 0 then
            begin
              lblScanStatus.Text := lblScanStatus.Text + '*';
              // newest on top, the codes of one frame in their order
              Memo1.Lines.Clear();
              var dt := FormatDateTime('h:nn:ss:z',Now);
              for var i := High(lines) downto 0 do
                Memo1.Lines.Add(dt + ': ' + lines[i]);
            end;

          end);

      finally
        // frees the results too
        results.Free;

        // An FMX bitmap must be freed in the main thread (on Android it holds
        // a graphics handle), so free the frame copy there.
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            scanBitmap.Free;
            fScanInProgress := false;
          end);
      end;

    end).Start();

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
