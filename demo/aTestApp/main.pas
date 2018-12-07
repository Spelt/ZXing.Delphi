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
  ZXing.ScanManager;

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
    procedure btnStartCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure imgCameraClick(Sender: TObject);
  private
    { Private declarations }
    FPermissionCamera : String;

    FScanManager: TScanManager;
    FScanInProgress: Boolean;
    FFrameTake: Integer;
    procedure GetImage();
    procedure CameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure ExplainReason(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
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
  FMX.DialogService;

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
  FFrameTake := 0;
  FScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);

  {$IFDEF ANDROID}
  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  {$EndIf}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FScanManager.Free;
end;

procedure TMainForm.CameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  if (Length(AGrantResults) = 1) and
     (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    CameraComponent1.Quality := FMX.Media.TVideoCaptureQuality.HighQuality;
    CameraComponent1.Active := false;
    CameraComponent1.Kind := FMX.Media.TCameraKind.BackCamera;
    CameraComponent1.FocusMode := FMX.Media.TFocusMode.ContinuousAutoFocus;
    CameraComponent1.Active := True;
    lblScanStatus.Text := '';
    Memo1.Lines.Clear;
  end
  else
    TDialogService.ShowMessage('Cannot scan for barcodes because the required permissions is not granted')
end;

procedure TMainForm.ExplainReason(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin

  TDialogService.ShowMessage('The app needs to access the camera to scan barcodes ...',
                            procedure(const AResult: TModalResult)
                            begin
                              APostRationaleProc;
                            end)
end;

procedure TMainForm.btnStartCameraClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([FPermissionCamera], CameraPermissionRequestResult, ExplainReason);
end;

procedure TMainForm.btnStopCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := false;
end;

procedure TMainForm.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  TThread.Synchronize(TThread.CurrentThread, GetImage);
end;

procedure TMainForm.GetImage;
var
  scanBitmap: TBitmap;
  ReadResult: TReadResult;

begin
  CameraComponent1.SampleBufferToBitmap(imgCamera.Bitmap, True);

  if (FScanInProgress) then
  begin
    exit;
  end;

  { This code will take every 4 frame. }
  inc(FFrameTake);
  if (FFrameTake mod 4 <> 0) then
  begin
    exit;
  end;

  scanBitmap := TBitmap.Create();
  scanBitmap.Assign(imgCamera.Bitmap);
  ReadResult := nil;

// There is bug in Delphi Berlin 10.1 update 2 which causes the TTask and
// the TThread.Synchronize to cause exceptions.
// See: https://quality.embarcadero.com/browse/RSP-16377?jql=project%20%3D%20RSP%20AND%20issuetype%20%3D%20Bug%20AND%20affectedVersion%20%3D%20%2210.1%20Berlin%20Update%202%22%20AND%20status%20%3D%20Open%20ORDER%20BY%20priority%20DESC

  TTask.Run(
    procedure
    begin
      try
        FScanInProgress := True;
        try
          ReadResult := FScanManager.Scan(scanBitmap);
        except
          on E: Exception do
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                lblScanStatus.Text := E.Message;
              end);

            exit;
          end;
        end;

        TThread.Synchronize(nil,
          procedure
          begin

            if (length(lblScanStatus.Text) > 10) then
            begin
              lblScanStatus.Text := '*';
            end;

            lblScanStatus.Text := lblScanStatus.Text + '*';
            if (ReadResult <> nil) then
            begin
              Memo1.Lines.Insert(0, ReadResult.Text);
            end;

          end);

      finally
        ReadResult.Free;
        scanBitmap.Free;
        FScanInProgress := false;
      end;

    end);

end;

procedure TMainForm.imgCameraClick(Sender: TObject);
begin

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
end;

end.
