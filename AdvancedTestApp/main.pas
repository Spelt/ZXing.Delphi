unit uFormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Media, FMX.Controls3D, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  ZXing.BarcodeFormat,
  FMX.Platform,
  System.Diagnostics,
  System.Threading,
  ZXing.ReadResult,
  ZXing.ScanManager, FMX.ListBox;

type

  TScanBufferEvent = procedure(Sender: TObject; ABitmap: TBitmap) of object;

  TFormMain = class(TForm)
    Layout2: TLayout;
    Memo1: TMemo;
    ToolBar3: TToolBar;
    Camera1: TCamera;
    SwitchScanning: TSwitch;
    PaintBox1: TPaintBox;
    LabelFPS: TLabel;
    btnBackCamera: TSpeedButton;
    btnFrontCamera: TSpeedButton;
    cbResolutions: TComboBox;
    Layout3: TLayout;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure SwitchScanningSwitch(Sender: TObject);
    procedure btnBackCameraClick(Sender: TObject);
    procedure btnFrontCameraClick(Sender: TObject);
    procedure cbResolutionsChange(Sender: TObject);
  private

    FScanManager: TScanManager;
    FScanInProgress: Boolean;
    FFrameTake: Integer;
    FStopwatch: TStopwatch;
    FFrameCount: Integer;
    FVideoSettings: TArray<TVideoCaptureSetting>;

    FActive: Boolean;
    FBuffer: TBitmap;
    FViewportBuffer: TBitmap;
    FVideoCamera: TVideoCaptureDevice;
    FOnScanBuffer: TScanBufferEvent;
    procedure DoScanBuffer(Sender: TObject; const ATime: TMediaTime);
    procedure SynchroniseBuffer;

    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure LoadResolutions;
    procedure StartCapture;
    procedure StopCapture;
    function GetDevice(Kind : TCameraKind): TVideoCaptureDevice;
    // procedure GetImage();
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
begin

  if TPlatformServices.Current.SupportsPlatformService
    (IFMXApplicationEventService, IInterface(AppEventSvc)) then
  begin
    AppEventSvc.SetApplicationEventHandler(AppEvent);
  end;

  FActive := False;
  FBuffer := TBitmap.Create();
  FVideoCamera := GetDevice(TCameraKind.FrontCamera);

  FVideoCamera.OnSampleBufferReady := DoScanBuffer;

  FFrameTake := 0;
  btnBackCamera.IsPressed := true;
  FVideoSettings := nil;
  LoadResolutions();

  FScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
end;

function TFormMain.GetDevice (Kind : TCameraKind): TVideoCaptureDevice;
var
  I: Integer;
  D: TCaptureDevice;
begin

    for I := 0 to TCaptureDeviceManager.Current.Count - 1 do
    begin

      D := TCaptureDeviceManager.Current.Devices[I];

      if (D.MediaType = TMediaType.Video) and (D is TVideoCaptureDevice) then
      begin

        if (Kind = TCameraKind.FrontCamera) and (TVideoCaptureDevice(D).Position = TDevicePosition.Front) then
        begin
          Result := TVideoCaptureDevice(D);
          Break;
        end;

        if (Kind = TCameraKind.BackCamera) and (TVideoCaptureDevice(D).Position = TDevicePosition.Back) then
        begin
          Result := TVideoCaptureDevice(D);
          Break;
        end;
      end;

    end;

    if Result = nil then
      Result := TCaptureDeviceManager.Current.DefaultVideoCaptureDevice;

end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FBuffer.Free;
  StopCapture();

  FScanManager.Free;
end;

{ Make sure the camera is released if you're going away. }
function TFormMain.AppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.WillBecomeInactive, TApplicationEvent.EnteredBackground,
      TApplicationEvent.WillTerminate:
      StopCapture();
  end;
end;

procedure TFormMain.StartCapture;
begin

  FVideoCamera.Quality := TVideoCaptureQuality.MediumQuality;
  FVideoCamera.FocusMode := TFocusMode.ContinuousAutoFocus;

  FBuffer.Clear(TAlphaColors.White);
  FActive := true;
  LabelFPS.Text := 'Starting capture...';
  FFrameCount := 0;
  FStopwatch := TStopwatch.StartNew;
  FVideoCamera.StartCapture;

end;

procedure TFormMain.StopCapture;
begin
  FActive := False;
  if FVideoCamera <> nil then
  begin
    FVideoCamera.StopCapture;
   // FreeAndNil(FVideoCamera);
    LabelFPS.Text := '';
  end;
end;

procedure TFormMain.DoScanBuffer(Sender: TObject; const ATime: TMediaTime);
var
  sec: double;
begin
  TThread.Synchronize(TThread.CurrentThread, SynchroniseBuffer);
  sec := FStopwatch.Elapsed.TotalSeconds;
  { Ignore the first 3 seconds to get up to speed }
  if (sec > 3) then
  begin
    sec := sec - 3;
    Inc(FFrameCount);
    LabelFPS.Text := Format('%.2f fps (%d x %d)',
      [FFrameCount / sec, FBuffer.Width, FBuffer.Height]);
  end;

end;

procedure TFormMain.SynchroniseBuffer;
begin

  if FActive = False then
  begin
    Exit;
  end;

  FVideoCamera.SampleBufferToBitmap(FBuffer, true);
  if Assigned(FOnScanBuffer) then
    FOnScanBuffer(Self, FBuffer);

  PaintBox1.Repaint;
end;

procedure TFormMain.btnBackCameraClick(Sender: TObject);
begin
  // CameraComponent1.Kind := TCameraKind.BackCamera;
  LoadResolutions();
end;

procedure TFormMain.btnFrontCameraClick(Sender: TObject);
begin
  // CameraComponent1.Kind := TCameraKind.FrontCamera;
  LoadResolutions();
end;

procedure TFormMain.LoadResolutions();
var
  i: Integer;
begin

  cbResolutions.Clear;

  // FVideoSettings := CameraComponent1.AvailableCaptureSettings;
  for i := Low(FVideoSettings) to High(FVideoSettings) do
  begin
    cbResolutions.Items.Add(FVideoSettings[i].Width.toString + ' x ' +
      FVideoSettings[i].Height.toString + ' x ' + FVideoSettings[i]
      .FrameRate.toString + ' fps');
  end;

  if (cbResolutions.Items.Count > 0) then
  begin
    cbResolutions.ItemIndex := 0;
    // CameraComponent1.SetCaptureSetting(FVideoSettings[0]);
  end;

end;

procedure TFormMain.cbResolutionsChange(Sender: TObject);
begin
//  StopCamera();
  // if FVideoSettings <> nil  then
  // CameraComponent1.SetCaptureSetting(FVideoSettings[cbResolutions.ItemIndex]);
end;

procedure TFormMain.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  SR, DR, PR: TRectF;
begin
  if (SwitchScanning.IsChecked) then
  begin
    PR := RectF(0, 0, PaintBox1.Width, PaintBox1.Height);
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := TAlphaColors.Black;
    Canvas.FillRect(PR, 0, 0, [], 1);

    if (FBuffer.Width > 0) then
    begin
      SR := RectF(0, 0, FBuffer.Width, FBuffer.Height);
      DR := SR;
      if (DR.Width < PaintBox1.Width) and (DR.Height < PaintBox1.Height) then
        RectCenter(DR, PR)
      else
        DR := DR.FitInto(PR);

      Canvas.DrawBitmap(FBuffer, SR, DR, 1);
    end;
  end;
end;

procedure TFormMain.SwitchScanningSwitch(Sender: TObject);
begin
  if (SwitchScanning.IsChecked) then
  begin
    // Memo1.Lines.Clear;
    // CameraComponent1.Active := True;
    // Application.ProcessMessages();
    // ResetCapture();

    StartCapture();

  end
  else
    StopCapture();
  // StopCamera();
end;

end.
