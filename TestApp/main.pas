unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Media, FMX.Platform, FMX.MultiView, FMX.ListView.Types,
  FMX.ListView, FMX.Layouts, System.Actions, FMX.ActnList, FMX.TabControl,
  FMX.ListBox, Threading, ScanManager, BarcodeFormat, ReadResult,
  FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    btnStartCamera: TButton;
    btnStopCamera: TButton;
    lblScanResults: TLabel;
    lblScanStatus: TLabel;
    imgCamera: TImage;
    CameraComponent1: TCameraComponent;
    MultiView1: TMultiView;
    ToolBar1: TToolBar;
    btnMenu: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Layout2: TLayout;
    ListBox2: TListBox;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxItem17: TListBoxItem;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    ListBoxItem22: TListBoxItem;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    ListBoxItem26: TListBoxItem;
    ListBoxItem27: TListBoxItem;
    ListBoxItem28: TListBoxItem;
    ActionList1: TActionList;
    ChangeTabAction1: TChangeTabAction;
    ChangeTabAction2: TChangeTabAction;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem29: TListBoxItem;
    ListBoxItem30: TListBoxItem;
    ListBoxItem31: TListBoxItem;
    ChangeTabActionNext: TChangeTabAction;
    PreviousTabActionPrev: TPreviousTabAction;
    ToolBar3: TToolBar;
    Button1: TButton;
    ToolBar2: TToolBar;
    Button2: TButton;
    procedure btnStartCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);
   // procedure CameraComponent1SampleBufferReady(Sender: TObject;
   //   const ATime: Int64);

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
  lblScanResults.Text := '';
  frameTake :=0;

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
  lblScanResults.Text := '';

end;

procedure TMainForm.btnStopCameraClick(Sender: TObject);
begin
  CameraComponent1.Active := False;
end;

procedure TMainForm.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin

end;

{procedure TMainForm.CameraComponent1SampleBufferReady(Sender: TObject;
  const ATime: Int64);
begin
  TThread.Synchronize(TThread.CurrentThread, GetImage);
end;
 }


procedure TMainForm.GetImage;
var
  scanBitmap: TBitmap;
  ReadResult: TReadResult;

begin

  CameraComponent1.SampleBufferToBitmap(imgCamera.Bitmap, True);


  // if (StopCameraOrdered) then
  // begin
  // FObr1.Active := False;
  // Exit;
  // end;

 {
  inc(frameTake);
  if (frameTake mod 4 <> 0) then
  begin
    Exit;
  end;
  }

  if (not FScanInProgress) then
  begin

    scanBitmap := TBitmap.Create();
    scanBitmap.Assign(imgCamera.Bitmap);

    TTask.Run(
      procedure
      begin

        try
          FScanInProgress := True;
          ReadResult := FScanManager.Scan(scanBitmap);
          FScanInProgress := False;
        except
          on E: Exception do
          begin
            FScanInProgress := False;
            TThread.Synchronize(nil,
              procedure
              begin
                lblScanStatus.Text := E.Message;
                lblScanResults.Text := '';
              end);

            if (scanBitmap <> nil) then
            begin
              scanBitmap.Free;
            end;

          end;

        end;

        TThread.Synchronize(nil,
          procedure
          begin

            if (scanBitmap <> nil) then
            begin
              scanBitmap.Free;
            end;

            if (length(lblScanStatus.Text) > 10) then
            begin
              lblScanStatus.Text := '';
            end;

            lblScanStatus.Text := lblScanStatus.Text + '*';

            if (ReadResult <> nil) then
            begin

              if (length(lblScanResults.Text) > 70) then
              begin
                lblScanResults.Text := '';
              end;

              lblScanResults.Text := lblScanResults.Text + ' | ' +
                ReadResult.Text;
            end;

          end);
      end);

  end;

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
