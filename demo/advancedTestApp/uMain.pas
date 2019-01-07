unit uMain;
{
	* Copyright 2017 ZXing.NET authors
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
	*
	* Implemented by E. Spelt for Delphi
	*
	* Uses examples from https://quality.embarcadero.com/browse/RSP-10592 from Erik van Bilsen


	READ ME

	This project is an example of a professional barcode project.

	No support what so ever ids offered cause this is just a sample project and uses soms exotic libraries.

	It offers:
	- Huge camera performance tweak from Erik van Bilsen. See: https://quality.embarcadero.com/browse/RSP-10592
	Look in the path settings of the project for the tweak.
	- Plays sound VIA NATIVE API for fast sounds via the audio manager from fmxexpress.com
	- Barcode scanning via our perfomant native ZXing library.
	- A good barcode strategie.
	- Background task based and therefore a responsive GUI.
	- Fancy GUI.
	- Check + warning for slow camera fps (frames per second). 20fps is recommended minimum.
	- Marks your barcode hit in the HUD

	- Tested in Delphi Tokyo edition.



	Performance TIPS:
	- Tweak with SCAN_EVERY_N_FRAME_FREQ maybe you can set it higher, depending on your situation.
	- Use only the Barcode Types you need to scan. Set it on auto with care.

}

interface

uses
	System.SysUtils, System.Types, System.UITypes, System.Classes,
	System.Variants,
	FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
	System.Math.Vectors, FMX.Media,
	FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
	ZXing.BarcodeFormat,
	ZXing.ResultPoint,
	FMX.Platform,
	System.Generics.Defaults,
	System.Generics.Collections,
	System.Diagnostics,
	System.Threading,
	System.Math,
	System.IOUtils,
	ZXing.ReadResult,
	ZXing.ScanManager, FMX.ListBox, FMX.ExtCtrls, FMX.ScrollBox, FMX.Memo,
	FMX.Ani,
	FMX.Effects,
	System.Permissions,
	AudioManager;

const

	// Seconds to wait before update and check the frame rate
	FPS_POLLING_FREQ: Integer = 3;

	// Skip n frames to do a barcode scan
	FRAME_PER_SECOND_SPEED_ALERT_THRESHOLD: Integer = 20;

	// Skip n frames to do a barcode scan
	SCAN_EVERY_N_FRAME_FREQ: Integer = 4;

type

	TScanBufferEvent = procedure(Sender: TObject; ABitmap: TBitmap) of object;

	TFormMain = class(TForm)
		Layout2: TLayout;
		Memo1: TMemo;
		ToolBar3: TRectangle;
		SwitchScanning: TSwitch;
		LabelFPS: TLabel;
		btnBackCamera: TSpeedButton;
		btnFrontCamera: TSpeedButton;
		ListBox1: TListBox;
		ListBoxItem1: TListBoxItem;
		ListBoxItem2: TListBoxItem;
		CameraComponent1: TCameraComponent;
		PopupBoxSetting: TPopupBox;
		RectImageSurface: TRectangle;
		PlotGridVizer: TPlotGrid;
		RectVizer: TRectangle;
		lblScanning: TLabel;
		FaLblScanning: TFloatAnimation;
		Layout1: TLayout;
		lblSlowWarning: TLabel;
		rectSlowWarning: TRectangle;
		TimerShowHit: TTimer;
		Label1: TLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure SwitchScanningSwitch(Sender: TObject);
		procedure btnBackCameraClick(Sender: TObject);
		procedure btnFrontCameraClick(Sender: TObject);
		procedure CameraComponent1SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
		procedure PopupBoxSettingChange(Sender: TObject);
		procedure TimerShowHitTimer(Sender: TObject);

	private
		FPermissionCamera: string;
		FScanManager: TScanManager;
		FScanInProgress: Boolean;
		FFrameTake: Integer;
		FStopwatch: TStopwatch;
		FFrameCount: Integer;
		FCaptureSettings: TArray<TVideoCaptureSetting>;
		targetRect: TRect;
		FActive: Boolean;
		FBuffer: TBitmap;
		FScanBitmap: TBitmap;
		FAudioMgr: TAudioManager;
		procedure ParseBitmap;
		function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
		procedure UpdateCaptureSettings(CameraKind: TCameraKind);
		procedure StartCapture;
		procedure StopCapture;
		procedure StartStopWatch;
		procedure DisplaySlowWarning(Show: Boolean);
		procedure MarkBarcode(resultPoints: TArray<IResultPoint>);
		procedure AccessCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>;
			const AGrantResults: TArray<TPermissionStatus>);
		procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
		procedure ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>;
			const AGrantResults: TArray<TPermissionStatus>);
    procedure SyncBitmap;

	end;

var
	FormMain: TFormMain;

implementation

uses
{$IFDEF ANDROID}
	Androidapi.Helpers,
	Androidapi.JNI.JavaTypes,
	Androidapi.JNI.Os,
{$ENDIF}
	FMX.DialogService;

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
var
	AppEventSvc: IFMXApplicationEventService;
	AudioFilePath: string;
begin

{$IFDEF ANDROID}
	FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
{$ENDIF}
	PermissionsService.RequestPermissions([FPermissionCamera], AccessCameraPermissionRequestResult, DisplayRationale);

	if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventSvc)) then
	begin
		AppEventSvc.SetApplicationEventHandler(AppEvent);
	end;

	FAudioMgr := TAudioManager.Create;
	AudioFilePath := TPath.Combine(TPath.GetDocumentsPath, 'Ok.wav');
	if FileExists(AudioFilePath) then
		FAudioMgr.AddSound(AudioFilePath)
	else
		Showmessage('Error loading OK.wav');

	FActive := False;
	FBuffer := TBitmap.Create();
	FScanBitmap := TBitmap.Create();
	FFrameTake := 0;
	FScanInProgress := False;

	UpdateCaptureSettings(TCameraKind.BackCamera);
	btnBackCamera.IsPressed := True;

	// Use only the Barcode Types you need to scan. Set it on auto with care!!
	FScanManager := TScanManager.Create(TBarcodeFormat.Auto, nil);
	DisplaySlowWarning(False);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
	FScanBitmap.Free;
	FBuffer.Free;
	StopCapture();
	CameraComponent1.Active := False;
	FScanManager.Free;
	FAudioMgr.Free
end;

procedure TFormMain.AccessCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>;
	const AGrantResults: TArray<TPermissionStatus>);
begin
	// 1 permission involved: CAMERA
	if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
	{ Fill the resolutions. }
	begin
		CameraComponent1.Quality := TVideoCaptureQuality.MediumQuality;
		CameraComponent1.FocusMode := TFocusMode.AutoFocus;
	end
	else
		Showmessage('Cannot access the camera because the required permission has not been granted')
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TFormMain.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
	// Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
	// After the user sees the explanation, invoke the post-rationale routine to request the permissions
	TDialogService.Showmessage('The app needs to access the camera in order to work',
		procedure(const AResult: TModalResult)
		begin
			APostRationaleProc
		end)
end;

{ Make sure the camera is released if you're going away. }
function TFormMain.AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
	case AAppEvent of
		TApplicationEvent.EnteredBackground, TApplicationEvent.WillTerminate:
			StopCapture();
	end;
end;

procedure TFormMain.StartCapture;
begin

	FBuffer.Clear(TAlphaColors.White);
	FActive := True;
	LabelFPS.Text := 'Starting capture...';
	PermissionsService.RequestPermissions([FPermissionCamera], ActivateCameraPermissionRequestResult, DisplayRationale);

	StartStopWatch();
	lblScanning.Text := 'Scanning on';
	FaLblScanning.Enabled := True;
end;

procedure TFormMain.ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>;
const AGrantResults: TArray<TPermissionStatus>);
begin
	// 1 permission involved: CAMERA
	if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
	begin
		{ Turn on the Camera }
		CameraComponent1.Active := True;
	end
	else
		Showmessage('Cannot start the camera because the required permission has not been granted')
end;

procedure TFormMain.StopCapture;
begin
	SwitchScanning.IsChecked := False;
	lblScanning.Text := 'No scanning';
	FaLblScanning.Enabled := False;
	FActive := False;
	DisplaySlowWarning(False);
	CameraComponent1.Active := False; // WARNING: CAUSES A CRASH WITH THE CAMERA SPEED TWEAK
	LabelFPS.Text := '';
end;

procedure TFormMain.StartStopWatch();
begin
	FStopwatch := TStopwatch.StartNew;
	FFrameCount := 0;
end;

procedure TFormMain.CameraComponent1SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
	TThread.Synchronize(TThread.CurrentThread, SyncBitmap);
	ParseBitmap();
end;

procedure TFormMain.SyncBitmap();
begin
 CameraComponent1.SampleBufferToBitmap(FBuffer, True);
end;

procedure TFormMain.ParseBitmap;
var
	sec, fps: double;
	ReadResult: TReadResult;

begin

	sec := FStopwatch.Elapsed.TotalSeconds;
	Inc(FFrameCount);

	if (Ceil(sec) mod FPS_POLLING_FREQ = 0) then
	begin
		fps := FFrameCount / sec;
		LabelFPS.Text := Format('%.1f fps (%d x %d)', [fps, CameraComponent1.CaptureSetting.Width, CameraComponent1.CaptureSetting.Height]);

		DisplaySlowWarning(fps < FRAME_PER_SECOND_SPEED_ALERT_THRESHOLD);

		StartStopWatch();

	end;

	if not FActive then
	begin
		Exit;
	end;

	RectImageSurface.Fill.Bitmap.Bitmap := FBuffer;

	if FScanInProgress then
		Exit;

	if ((FFrameCount mod SCAN_EVERY_N_FRAME_FREQ) <> 0) then
		Exit;

	FScanBitmap.Assign(FBuffer);

	ReadResult := nil;
	TTask.Run(
		procedure
		begin
			try
				try
					FScanInProgress := True;
					ReadResult := FScanManager.Scan(FScanBitmap);
				except
					on E: Exception do
					begin
						TThread.Synchronize(nil,
							procedure
							begin
								Memo1.Lines.Insert(0, formatdatetime('c ', Now) + E.Message);
							end);

						Exit;
					end;
				end;

				if (ReadResult <> nil) then
				begin

					TThread.Synchronize(nil,
						procedure
						begin
							MarkBarcode(ReadResult.resultPoints);
							FAudioMgr.PlaySound(0); // 0 is the 0 index of the sound collecion
							Memo1.Lines.Insert(0, formatdatetime('c ', Now) + ReadResult.Text);

						end);

				end;

			finally
				ReadResult.Free;
				FScanInProgress := False;
			end;

		end);

end;

procedure TFormMain.MarkBarcode(resultPoints: TArray<IResultPoint>);
const
	iSize = 15;
begin
	FActive := False;
	TimerShowHit.Enabled := True;

	RectImageSurface.Fill.Bitmap.Bitmap.Assign(FScanBitmap); // make sure it is the same bitmap

	RectImageSurface.Fill.Bitmap.Bitmap.Canvas.BeginScene;
	try
		RectImageSurface.Fill.Bitmap.Bitmap.Canvas.Fill.Color := TAlphaColors.Orange;

		if (Length(resultPoints) = 2) then
		begin
			// When 2 points then draw a line on the bitmap.
			RectImageSurface.Fill.Bitmap.Bitmap.Canvas.FillRect(TRectF.Create(resultPoints[0].x - iSize, resultPoints[0].y - iSize,
				resultPoints[1].x + iSize, resultPoints[1].y + iSize), 0, 0, AllCorners, 1);
		end
		else if (Length(resultPoints) = 3) then
		begin
			// When 3 points then draw a square on the bitmap.
			RectImageSurface.Fill.Bitmap.Bitmap.Canvas.FillRect(TRectF.Create(resultPoints[0].x, resultPoints[0].y, resultPoints[2].x,
				resultPoints[2].y), 0, 0, AllCorners, 1);
		end;

	finally
		RectImageSurface.Fill.Bitmap.Bitmap.Canvas.EndScene;
	end;
end;

procedure TFormMain.TimerShowHitTimer(Sender: TObject);
begin
	FActive := True;
	TimerShowHit.Enabled := False;
end;

procedure TFormMain.DisplaySlowWarning(Show: Boolean);
begin
	rectSlowWarning.Visible := Show;
end;

procedure TFormMain.PopupBoxSettingChange(Sender: TObject);
begin

	if (PopupBoxSetting.ItemIndex < 0) then
		Exit;

	CameraComponent1.CaptureSetting := FCaptureSettings[PopupBoxSetting.ItemIndex];
	StartCapture();
end;

procedure TFormMain.SwitchScanningSwitch(Sender: TObject);
begin

	if (SwitchScanning.IsChecked) then
		StartCapture()
	else
		StopCapture();
end;

procedure TFormMain.btnBackCameraClick(Sender: TObject);
begin
	UpdateCaptureSettings(TCameraKind.BackCamera);
end;

procedure TFormMain.btnFrontCameraClick(Sender: TObject);
begin
	UpdateCaptureSettings(TCameraKind.FrontCamera);
end;

procedure TFormMain.UpdateCaptureSettings(CameraKind: TCameraKind);
var
	Setting, MaxSetting: TVideoCaptureSetting;
	UsefulSettings: TDictionary<TSize, TVideoCaptureSetting>;
	Size: TSize;
	I: Integer;
begin

	StopCapture();
	CameraComponent1.Kind := CameraKind;

	{ GetAvailableCaptureSettings can return A LOT of settings. For each supported
		resolution, it can return a large number of settings with different frame
		rates. We only care about the highest framerate supported by each
		resolution. We use a dictionary to keep track of this. }
	UsefulSettings := TDictionary<TSize, TVideoCaptureSetting>.Create;
	try
		for Setting in CameraComponent1.GetAvailableCaptureSettings(nil) do
		begin
			Size := TSize.Create(Setting.Width, Setting.Height);
			if (UsefulSettings.TryGetValue(Size, MaxSetting)) then
			begin
				{ Dictionary contains requested resolution. Update its framerate to the
					maximum supported one. }
				if (Setting.FrameRate > MaxSetting.FrameRate) then
					UsefulSettings.AddOrSetValue(Size, Setting);
			end
			else
				UsefulSettings.Add(Size, Setting);
		end;

		{ Now we can get a list of settings, with only one per resolution. We sort it
			maually by resolution. }
		FCaptureSettings := UsefulSettings.Values.ToArray;

		{ Now we can get a list of settings, with only one per resolution. We sort it
			maually by resolution. }
		FCaptureSettings := UsefulSettings.Values.ToArray;
		TArray.Sort<TVideoCaptureSetting>(FCaptureSettings, TComparer<TVideoCaptureSetting>.Construct(
			function(const Left, right: TVideoCaptureSetting): Integer
			var
				Difference: Integer;
			begin
				Difference := (Left.Width * Left.Height) - (right.Width * right.Height);
				if (Difference < 0) then
					Result := 1
				else if (Difference > 0) then
					Result := -1
				else
					Result := 0;
			end));

	finally
		UsefulSettings.Free;
	end;

	{ Populate popup box with settings }
	PopupBoxSetting.BeginUpdate;
	try
		PopupBoxSetting.Items.Clear;
		for I := 0 to Length(FCaptureSettings) - 1 do
		begin
			Setting := FCaptureSettings[I];
			PopupBoxSetting.Items.Add(Format('%d x %d x %dfps ', [Setting.Width, Setting.Height, Round(Setting.FrameRate)]));
		end;
	finally
		PopupBoxSetting.EndUpdate;
	end;

	PopupBoxSetting.ItemIndex := -1;
end;

end.
