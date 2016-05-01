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
  ScanManager;

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
    Camera1: TCamera;
    Button1: TButton;
    Button2: TButton;
    procedure btnStartCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure Button1Click(Sender: TObject);
    procedure imgCameraClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    FScanManager: TScanManager;
    FScanInProgress: Boolean;
    frameTake: Integer;
    procedure GetImage();
    procedure AddResult(const AText: String);
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  curImgIdx : Integer = 0;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
  CaptureSetting : TVideoCaptureSetting;
  LSettings: TArray<TVideoCaptureSetting>;
begin

  lblScanStatus.Text := '';
  frameTake := 0;

  { by default, we start with Front Camera and Flash Off }
  { cbFrontCamera.IsChecked := True;
    CameraComponent1.Kind := FMX.Media.TCameraKind.ckFrontCamera;

    cbFlashOff.IsChecked := True;
    if CameraComponent1.HasFlash then
    CameraComponent1.FlashMode := FMX.Media.TFlashMode.fmFlashOff;
  }

  { Add platform service to see camera state. }
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, IInterface(AppEventSvc)) then
    AppEventSvc.SetApplicationEventHandler(AppEvent);

  CameraComponent1.CaptureSettingPriority := TVideoCaptureSettingPriority.FrameRate;
  LSettings := CameraComponent1.AvailableCaptureSettings;
  CameraComponent1.CaptureSetting := LSettings[0];
  {CaptureSetting := CameraComponent1.GetCaptureSetting;
  CaptureSetting.SetFrameRate(25, 30);
  CaptureSetting.Width  := 640;
  CaptureSetting.Height := 480;}
  CameraComponent1.Quality := FMX.Media.TVideoCaptureQuality.CaptureSettings;

  CameraComponent1.SetCaptureSetting(CaptureSetting);
  lblScanStatus.Text := '';
  FScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FScanManager.Free;
end;

procedure TMainForm.btnStartCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := False;
  CameraComponent1.Kind := FMX.Media.TCameraKind.BackCamera;
  CameraComponent1.FocusMode := FMX.Media.TFocusMode.ContinuousAutoFocus;
  CameraComponent1.Active := True;

  lblScanStatus.Text := '';
  memo1.Lines.Clear;
end;

procedure TMainForm.AddResult(const AText: String);
{var
  FormatSettings: TFormatSettings;}
begin
  with memo1 do
  begin
    if Length(AText) > 0 then
    begin
      //FormatSettings := TFormatSettings.Create($409);
      if (Lines.Count > 1)
      then
         Lines.Insert(0, '-------------------------------------------------------');
      Lines.Insert(0, AText);
      Lines.Insert(0, FormatDateTime('hh' + FormatSettings.TimeSeparator +
                                     'nn' + FormatSettings.TimeSeparator +
                                     'ss.zzz',
                                     Now,
                                     FormatSettings));
    end;
  end;
end;

procedure TMainForm.btnStopCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := False;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Inc(curImgIdx);
  if (curImgIdx > Pred(imgCamera.MultiResBitmap.Count))
  then
     curImgIdx := 1;

  imgCamera.Bitmap := imgCamera.MultiResBitmap.Items[curImgIdx].Bitmap;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  ReadResult: TReadResult;
begin
  //imgCamera.Bitmap := imgCamera.Bitmap.CreateThumbnail(640, 640);
  ReadResult := FScanManager.Scan(imgCamera.Bitmap);
  if (ReadResult <> nil) then
  begin
    AddResult(ReadResult.Text);
  end;
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
    Exit;
  end;

  {
    inc(frameTake);
    if (frameTake mod 4 <> 0) then
    begin
    Exit;
    end;
  }

  scanBitmap := TBitmap.Create();
  scanBitmap.Assign(imgCamera.Bitmap);

  TTask.Run(
    procedure
    begin

      try
        FScanInProgress := True;

        scanBitmap.Assign(imgCamera.Bitmap);

        ReadResult := FScanManager.Scan(scanBitmap);
        FScanInProgress := False;
      except
        on E: Exception do
        begin
          FScanInProgress := False;
          TThread.Synchronize(nil,
            procedure
            begin
              // lblScanStatus.Text := E.Message;
              // lblScanResults.Text := '';
            end);

          if (scanBitmap <> nil) then
          begin
            scanBitmap.Free;
          end;

          Exit;

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

          if (ReadResult <> nil)
          then
             AddResult(ReadResult.Text);

          if (scanBitmap <> nil) then
          begin
            scanBitmap.Free;
          end;

          FreeAndNil(ReadResult);
        end);
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
    TApplicationEvent.WillBecomeInactive,
    TApplicationEvent.EnteredBackground,
    TApplicationEvent.WillTerminate :
      CameraComponent1.Active := False;
  end;
end;

end.
