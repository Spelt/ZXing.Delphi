unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Media, FMX.Platform, FMX.MultiView, FMX.ListView.Types,
  FMX.ListView, FMX.Layouts, System.Actions, FMX.ActnList, FMX.TabControl,
  FMX.ListBox, Threading, ScanManager, BarcodeFormat, ReadResult,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

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
    procedure btnStartCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
  private
    { Private declarations }

    FScanManager: TScanManager;
    FScanInProgress: Boolean;
    frameTake: Integer;
    procedure GetImage();
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
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

  CameraComponent1.Quality := FMX.Media.TVideoCaptureQuality.MediumQuality;
  lblScanStatus.Text := '';
  FScanManager := TScanManager.Create(TBarcodeFormat.CODE_128, nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FScanManager.Free;
end;

procedure TMainForm.btnStartCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := False;
  CameraComponent1.Kind := FMX.Media.TCameraKind.BackCamera;
  CameraComponent1.Active := True;

  lblScanStatus.Text := '';
  memo1.Lines.Clear;

end;

procedure TMainForm.btnStopCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := False;
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

          if (ReadResult <> nil) then
          begin
            memo1.Lines.Insert(0,ReadResult.Text);
          end;

          if (scanBitmap <> nil) then
          begin
            scanBitmap.Free;
          end;

        end);
    end);

end;

{ Make sure the camera is released if you're going away. }
function TMainForm.AppEvent(AAppEvent: TApplicationEvent;
AContext: TObject): Boolean;
begin

  case AAppEvent of
    TApplicationEvent.WillBecomeInactive:
      CameraComponent1.Active := False;
    TApplicationEvent.EnteredBackground:
      CameraComponent1.Active := False;
    TApplicationEvent.WillTerminate:
      CameraComponent1.Active := False;
  end;

end;

end.
