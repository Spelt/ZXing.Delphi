{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2019 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{ Changes made by Erik van Bilsen to TAndroidVideoCaptureDevice (look for "EvB"):
  * Instead of creating a new callback buffer with every frame, we allocate
  3 callback buffers up front, and reuse the buffer passed to OnPreviewFrame.
  * Removed SharedBuffer, SharedBufferBytes and QueueSection since these are
  not needed anymore.
  * Added an array of 3 buffers instead.
  * Perform optimized YUV-to-RGB conversion instead of compressing YUV to a
  JPEG stream and then decompressing it again to a RGB bitmap.
  * Perform optimized image rotation instead of using FireMonkey bitmap
  rotation.
  * There are both Delphi versions and NEON versions of the optimized routines
  mentioned above.

  13/08/2019: By Iwan Cahyadi Sugeng (iwan.c.sugeng@gmail.com)
  * Updated to Rio 10.3.2 Source Code
  * #01 note for release the camera component, found out that when it is
    released on the destroy event instead on the StopCapture event, the camera
    is not actually released
    To reproduce the error:
    - Open our camera
    - switch to device camera
    - switch back to our apps and try to open the camera ,will generate
       “getParameters failed (empty parameters)” error
  * The code is used from Rio 10.3.2 source code then compared to original EvB
    modified version, maybe only need to move the release routine to
    StopCapture method

    21/08/2019 By ES
    Note camera is created dynamically because of a crash when switched back on.

}
unit FMX.Media.Android;

interface

{$SCOPEDENUMS ON}

uses
  Androidapi.JNI.Media, Androidapi.JNI.VideoView, Androidapi.JNI.App, Androidapi.JNI.Widget, System.Types,
  System.Messaging, FMX.Media, Androidapi.JNI.Embarcadero, FMX.Platform.Android, Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, FMX.ZOrder.Android;

type
  TAndroidCaptureDeviceManager = class(TCaptureDeviceManager)
  public
    constructor Create; override;
  end;

  TAndroidMedia = class(TMedia)
  private
    FPlayer: JMediaPlayer;
    FVolume: Single;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure SeekToBegin;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
  end;

  TAndroidMediaCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

  TAndroidVideo = class(TMedia)
  private
    type
      TCommonVolume = class
      strict private
        FAudioService: JObject;
        FAudioManager: JAudioManager;
        FMaxVolume: Integer;
        procedure SetVolume(const Value: Single);
        function GetVolume: Single;
      public
        constructor Create;
        property Value: Single read GetVolume write SetVolume;
      end;
  private
    FVolume: TCommonVolume;
    FScale: Single;
    FJustAudio: TAndroidMedia;
    FVideoPlayer: JVideoView;
    FVideoSize: TSize;
    FVideoEnabled: Boolean;
    function AllAssigned: Boolean;
    procedure RealignView;
    procedure RetreiveVideoSize;
    procedure CheckVideo;
    procedure InitInstance;
    function InstanceCreated: Boolean;
    function IsVideoEnabled: Boolean;
    function GetZOrderManager: TAndroidZOrderManager;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure SeekToBegin;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
    property ZOrderManager: TAndroidZOrderManager read GetZOrderManager;
  end;

  TAndroidVideoCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

implementation

uses
  System.Classes,  System.RTLConsts,System.Threading, System.Math, System.Permissions, System.SysUtils, System.SyncObjs,
  System.Generics.Collections, Androidapi.Bitmap, Androidapi.JNI.Hardware, Androidapi.Gles, Androidapi.JNI.Os,
  Androidapi.Helpers, FMX.Consts, FMX.Types, FMX.Surfaces, FMX.Graphics, FMX.Helpers.Android, FMX.Forms, FMX.Platform, FastUtils;

const
  THEME_DARK_NOTITLE_FULLSCREEN = $0103000A;

type
  TVideoInstance = record
    VideoPlayer: JVideoView;
  end;

  TVideoPool = class
  private
    FUsed: TList<TVideoInstance>;
    FReadyToUse: TList<TVideoInstance>;
    procedure CreateOneMoreInstance;
    procedure UIFreezeInstance(const AInstance: TVideoInstance);
  public
    function UIGetInstance: TVideoInstance;
    procedure UIReturnInstance(const AInstance: TVideoInstance);
    destructor Destroy; override;
    constructor Create;
  end;

var
  VideoPool: TVideoPool;

{$REGION 'Local Class Declarations'}

type
  TAndroidAudioCaptureDevice = class(TAudioCaptureDevice)
  private
    FRecorder: JMediaRecorder;
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    function GetDeviceState: TCaptureDeviceState; override;
  public
    destructor Destroy; override;
  end;

  TAndroidVideoCaptureDevice = class;

  TAndroidVideoCaptureCallback = class(TJavaLocal, JCamera_PreviewCallback)
  private
    [Weak] FCaptureDevice: TAndroidVideoCaptureDevice;
  public
    procedure onPreviewFrame(AData: TJavaArray<Byte>; ACamera: JCamera); cdecl;
  end;

  TAndroidVideoCaptureDevice = class(TVideoCaptureDevice)
  private const
    HighestJpegQuality = 100;
    MediumJpegQuality = 75;
    LowestJpegQuality = 50;
    DefaultCaptureTimeInterval = 20; // was 33 = 30fps, 20 = 50fps
    JPEGQualityKey = 'jpeg-quality';
    BUFFER_COUNT = 10; // EvB: Added ES: Changed from 3 to 10

//    DefaultCaptureTimeInterval = 33;
//    JPEGQualityKey = 'jpeg-quality';

  private
    SurfaceSection: TCriticalSection;
    UpdatedSection: TCriticalSection;
//    QueueSection: TCriticalSection;   // EvB: Removed

    FCameraId: Integer;
    FCapturing: Boolean;
    FCaptureTimerIntervalBusy: Boolean; // EvB: Added
    FCaptureTimerInterval: Integer;
    FVideoConversionJPEGQuality: Integer;
    PreviewBufferSize: TPoint;
//    SharedBuffer: TJavaArray<Byte>;   // EvB: Removed
    SharedBufferSize: TPoint;
//    SharedBufferBytes: Integer;       // EvB: Removed
//    QueuedBufferCount: Integer;       // EvB: Removed
//    SharedBufferFormat: Integer;      // EvB: Removed
    SharedSurface: TBitmapSurface;
    SharedSurfaceUpdated: Boolean;
    SurfaceTexture: JSurfaceTexture;
    SurfaceTextureId: GLuint;
    CapturePollingTimer: TTimer;
    ManualBitmapRotation: Integer;
    FOrientationChangedId: Integer;
    FPreviewConversionTask: ITask;
    FBuffers: array [0 .. BUFFER_COUNT - 1] of TJavaArray<Byte>; // EvB: Added
    class var FCallback: TAndroidVideoCaptureCallback;
    class var FCurrentCamera: JCamera;
    class var FCurrentCameraID: Integer;

    function GetManualBitmapRotation: Integer;
    procedure CopyBufferToSurface(AnArray: TJavaArray<Byte>; ACamera: JCamera);
//    procedure ConvertYuvToBitmapSurfaceHandler(Sender: TObject);  // EvB: Removed
    function GetCamera: JCamera;
//    procedure AddQueueBuffer;         // EvB: Removed
//    procedure RemoveQueueBuffer;      // EvB: Removed
    procedure AddCallbackBuffers;       // EvB: Added
    procedure DeleteCallbackBuffers;    // EvB: Added
    procedure OnCaptureTimer(Sender: TObject);
    procedure OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    procedure DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean); override;
    function GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string; override;
    function GetDeviceState: TCaptureDeviceState; override;
    function GetPosition: TDevicePosition; override;
    procedure DoSetQuality(const Value: TVideoCaptureQuality); override;
    function GetHasFlash: Boolean; override;
    function GetFlashMode: TFlashMode; override;
    procedure SetFlashMode(const Value: TFlashMode); override;
    function GetHasTorch: Boolean; override;
    function GetTorchMode: TTorchMode; override;
    procedure SetTorchMode(const Value: TTorchMode); override;
    function GetFocusMode: TFocusMode; override;
    procedure SetFocusMode(const Value: TFocusMode); override;
    class function GetCallbackInstance: TAndroidVideoCaptureCallback;
    function GetCaptureSetting: TVideoCaptureSetting; override;
    function DoSetCaptureSetting(const ASetting: TVideoCaptureSetting): Boolean; override;
    function DoGetAvailableCaptureSettings: TArray<TVideoCaptureSetting>; override;
  public
    property CameraId: Integer read FCameraId;
    property Camera: JCamera read GetCamera;

    constructor Create(const AManager: TCaptureDeviceManager; const ADefault: Boolean); override;
    destructor Destroy; override;
  end;

{$ENDREGION}
{$REGION 'TAndroidCaptureDeviceManager'}

constructor TAndroidCaptureDeviceManager.Create;
var
  I: Integer;
  CameraDevice: TAndroidVideoCaptureDevice;
begin
  inherited;

  TAndroidAudioCaptureDevice.Create(Self, True);

  for I := 0 to TJCamera.JavaClass.getNumberOfCameras - 1 do
  begin
    CameraDevice := TAndroidVideoCaptureDevice.Create(Self, I = 0);
    CameraDevice.FCameraId := I;
  end;
end;

{$ENDREGION}
{$REGION 'TAndroidAudioCaptureDevice'}

destructor TAndroidAudioCaptureDevice.Destroy;
begin
  FRecorder := nil;

  inherited;
end;

procedure TAndroidAudioCaptureDevice.DoStartCapture;
begin
  if PermissionsService.IsPermissionGranted(JStringToString(TJManifest_permission.JavaClass.RECORD_AUDIO)) then
  begin
    FRecorder := TJMediaRecorder.JavaClass.init;
    FRecorder.setAudioSource(TJMediaRecorder_AudioSource.JavaClass.MIC);
    FRecorder.setOutputFormat(TJMediaRecorder_OutputFormat.JavaClass.THREE_GPP);
    FRecorder.setAudioEncoder(TJMediaRecorder_AudioEncoder.JavaClass.AMR_NB);
    FRecorder.setOutputFile(StringToJString(FileName));
    FRecorder.prepare;
    FRecorder.start;
  end
  else
    raise EPermissionException.CreateFmt(SRequiredPermissionsAreAbsent, ['RECORD_AUDIO']);
end;

procedure TAndroidAudioCaptureDevice.DoStopCapture;
begin
  if FRecorder <> nil then
  begin
    FRecorder.stop;
    FRecorder := nil;
  end;
end;

function TAndroidAudioCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FRecorder <> nil then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

{$ENDREGION}
{$REGION 'TAndroidVideoCaptureDevice'}

procedure TAndroidVideoCaptureCallback.onPreviewFrame(AData: TJavaArray<Byte>; ACamera: JCamera);
begin
  if FCaptureDevice <> nil then
  begin
    if FCaptureDevice.FCapturing then
      FCaptureDevice.CopyBufferToSurface(AData, ACamera);
//    FCaptureDevice.RemoveQueueBuffer;   // EvB: Removed
    ACamera.addCallbackBuffer(AData);     // EvB ICS: Added
  end;
end;

constructor TAndroidVideoCaptureDevice.Create(const AManager: TCaptureDeviceManager; const ADefault: Boolean);
begin
  inherited;

  // Set default values
  FCaptureTimerInterval := DefaultCaptureTimeInterval;
  FVideoConversionJPEGQuality := MediumJpegQuality;

  SurfaceSection:= TCriticalSection.Create;
  UpdatedSection:= TCriticalSection.Create;
//  QueueSection:= TCriticalSection.Create;   // EvB: Remove

  CapturePollingTimer := TTimer.Create(nil);              // EvB ICS: Added
  CapturePollingTimer.Interval := FCaptureTimerInterval;  // EvB ICS: Added
  CapturePollingTimer.OnTimer := OnCaptureTimer;          // EvB ICS: Added

  FCaptureTimerIntervalBusy := False;                     // EvB ICS: Added

  FCapturing := False;
  FOrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage,
    OrientationChangedHandler);
end;

// EvB: Added
procedure TAndroidVideoCaptureDevice.DeleteCallbackBuffers;
var
  I: Integer;
begin
  for I := 0 to BUFFER_COUNT - 1 do
    FreeAndNil(FBuffers[I]);
end;

destructor TAndroidVideoCaptureDevice.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, FOrientationChangedId);
  DoStopCapture;

  FreeAndNil(CapturePollingTimer);  // EvB ICS: Added

//  QueueSection.Free;      // EvB: Remove
  UpdatedSection.Free;
  SurfaceSection.Free;

//  FCurrentCamera.release;           // EvB ICS: Do not release here #01
//  FCurrentCamera := nil;            // EvB ICS: Do not release here #01

  inherited;
end;

function TAndroidVideoCaptureDevice.GetCamera: JCamera;
begin
  if not PermissionsService.IsPermissionGranted(
           JStringToString(TJManifest_permission.JavaClass.CAMERA)) then
    raise EPermissionException.CreateFmt(SRequiredPermissionsAreAbsent, ['CAMERA']);

  if FCurrentCamera = nil then
    FCurrentCamera := TJCamera.JavaClass.open(FCameraId)
  else if FCurrentCameraID <> FCameraId then
  begin
    FCurrentCamera.release;
    FCurrentCamera := TJCamera.JavaClass.open(FCameraId);
    FCurrentCameraID := FCameraId;
  end;

  Result := FCurrentCamera;
end;

function TAndroidVideoCaptureDevice.GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string;
begin
  case Prop of
    TCaptureDevice.TProperty.UniqueID:
      Result := FCameraId.ToString;

    else
      Result := '';
  end;
end;

function TAndroidVideoCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FCapturing then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

procedure TAndroidVideoCaptureDevice.DoSetQuality(const Value: TVideoCaptureQuality);
var
  Params: JCamera_Parameters;
  SettingsList: TArray<TVideoCaptureSetting>;
  Priority: TVideoCaptureSettingPriority;
begin
  if Value <> TVideoCaptureQuality.CaptureSettings then
  begin
    Priority := CaptureSettingPriority;
    try
      CaptureSettingPriority := TVideoCaptureSettingPriority.Resolution;
      SettingsList := AvailableCaptureSettings;
    finally
      CaptureSettingPriority := Priority;
    end;

    if Length(SettingsList) > 0 then
    begin
      case Value of
        TVideoCaptureQuality.PhotoQuality: CaptureSetting := SettingsList[0];
        TVideoCaptureQuality.HighQuality: CaptureSetting := SettingsList[0];
        TVideoCaptureQuality.MediumQuality: CaptureSetting := SettingsList[Length(SettingsList) div 2];
        TVideoCaptureQuality.LowQuality: CaptureSetting := SettingsList[High(SettingsList)];
      end;
    end;

    Params := Camera.getParameters;
    if Params <> nil then
    begin

      case Value of
        TVideoCaptureQuality.PhotoQuality: ;
        TVideoCaptureQuality.HighQuality:
        begin
          Params.setPictureFormat(TJImageFormat.JavaClass.JPEG);
          Params.&set(StringToJString(JPEGQualityKey), HighestJpegQuality);
          Params.setJpegQuality(HighestJpegQuality);
          FVideoConversionJPEGQuality := HighestJpegQuality;
        end;
        TVideoCaptureQuality.MediumQuality:
        begin
          Params.setPictureFormat(TJImageFormat.JavaClass.JPEG);
          Params.&set(StringToJString(JPEGQualityKey), MediumJpegQuality);
          Params.setJpegQuality(MediumJpegQuality);
          FVideoConversionJPEGQuality := MediumJpegQuality;
        end;
        TVideoCaptureQuality.LowQuality:
        begin
          Params.setPictureFormat(TJImageFormat.JavaClass.JPEG);
          Params.&set(StringToJString(JPEGQualityKey), LowestJpegQuality);
          Params.setJpegQuality(LowestJpegQuality);
          FVideoConversionJPEGQuality := LowestJpegQuality;
        end;
      end;
      Camera.setParameters(Params);
    end;
    inherited;
  end;
end;

function TAndroidVideoCaptureDevice.GetPosition: TDevicePosition;
var
  CameraInfo: JCamera_CameraInfo;
begin
  CameraInfo := TJCamera_CameraInfo.JavaClass.init;
  TJCamera.JavaClass.getCameraInfo(CameraId, CameraInfo);

  if CameraInfo.facing = TJCamera_CameraInfo.JavaClass.CAMERA_FACING_BACK then
    Result := TDevicePosition.Back
  else if CameraInfo.facing = TJCamera_CameraInfo.JavaClass.CAMERA_FACING_FRONT then
    Result := TDevicePosition.Front
  else
    Result := TDevicePosition.Unspecified;
end;

function TAndroidVideoCaptureDevice.GetCaptureSetting: TVideoCaptureSetting;
var
  Params: JCamera_Parameters;
  Size: JCamera_Size;
begin
  Result := TVideoCaptureSetting.Create;
  Params := Camera.getParameters;
  if Params <> nil then
  begin
    Size := Params.getPreviewSize;
    Result := TVideoCaptureSetting.Create(Size.width, Size.height, Params.getPreviewFrameRate);
  end;
end;

function TAndroidVideoCaptureDevice.GetHasFlash: Boolean;
var
  Params: JCamera_Parameters;
  ModeList: JList;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(False);

  ModeList := Params.getSupportedFlashModes;

  if ModeList = nil then
    Exit(False);

  Result := ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_ON) or
    ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO);
end;

function TAndroidVideoCaptureDevice.GetHasTorch: Boolean;
var
  Params: JCamera_Parameters;
  ModeList: JList;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(False);

  ModeList := Params.getSupportedFlashModes;

  if ModeList = nil then
    Exit(False);

  Result := ModeList.contains(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH);
end;

function TAndroidVideoCaptureDevice.GetFlashMode: TFlashMode;
var
  Params: JCamera_Parameters;
  FlashMode: JString;
  FlashModeText: string;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(inherited);

  FlashMode := Params.getFlashMode;
  if FlashMode = nil then
    Exit(inherited);

  FlashModeText := JStringToString(FlashMode);

  if SameText(FlashModeText, JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_ON)) then
    Result := TFlashMode.FlashOn
  else if SameText(FlashModeText, JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO)) then
    Result := TFlashMode.AutoFlash
  else
    Result := TFlashMode.FlashOff;
end;

procedure TAndroidVideoCaptureDevice.SetFlashMode(const Value: TFlashMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  case Value of
    TFlashMode.AutoFlash:
      Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_AUTO);

    TFlashMode.FlashOff:
      Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_OFF);

    TFlashMode.FlashOn:
      Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_ON);
  end;

  Camera.setParameters(Params);
end;

function TAndroidVideoCaptureDevice.GetFocusMode: TFocusMode;
var
  Params: JCamera_Parameters;
  FocusMode: JString;
  FocusModeText: string;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(inherited);

  FocusMode := Params.getFocusMode;
  if FocusMode = nil then
    Exit(inherited);

  FocusModeText := JStringToString(FocusMode);

  if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_AUTO)) then
    Result := TFocusMode.AutoFocus
  else if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_VIDEO)) then
    Result := TFocusMode.ContinuousAutoFocus
  else if SameText(FocusModeText, JStringToString(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE)) then
    Result := TFocusMode.ContinuousAutoFocus
  else
    Result := TFocusMode.Locked;
end;

procedure TAndroidVideoCaptureDevice.SetFocusMode(const Value: TFocusMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  case Value of
    TFocusMode.AutoFocus:
      Params.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_AUTO);

    TFocusMode.ContinuousAutoFocus:
      Params.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_CONTINUOUS_PICTURE);

    TFocusMode.Locked:
      Params.setFocusMode(TJCamera_Parameters.JavaClass.FOCUS_MODE_FIXED);
  end;

  Camera.setParameters(Params);
end;

function TAndroidVideoCaptureDevice.GetTorchMode: TTorchMode;
var
  Params: JCamera_Parameters;
  FlashMode: JString;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit(inherited);

  FlashMode := Params.getFlashMode;
  if FlashMode = nil then
    Exit(inherited);

  if SameText(JStringToString(FlashMode), JStringToString(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH)) then
    Result := TTorchMode.ModeOn
  else
    Result := TTorchMode.ModeOff
end;

procedure TAndroidVideoCaptureDevice.SetTorchMode(const Value: TTorchMode);
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  case Value of
    TTorchMode.ModeOff:
      if GetTorchMode = TTorchMode.ModeOn then
      begin
        Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_OFF);
        Camera.setParameters(Params);
      end;

    TTorchMode.ModeOn:
      if GetTorchMode = TTorchMode.ModeOff then
      begin
        Params.setFlashMode(TJCamera_Parameters.JavaClass.FLASH_MODE_TORCH);
        Camera.setParameters(Params);
      end;
  end;
end;

// EvB: Removed
{
procedure TAndroidVideoCaptureDevice.AddQueueBuffer;
begin
  QueueSection.Acquire;
  try
    if QueuedBufferCount < 1 then
    begin
      FreeAndNil(SharedBuffer);
      SharedBuffer := TJavaArray<Byte>.Create(SharedBufferBytes);
      Camera.addCallbackBuffer(SharedBuffer);
      Inc(QueuedBufferCount);
    end;
  finally
    QueueSection.Release;
  end;
end;

procedure TAndroidVideoCaptureDevice.RemoveQueueBuffer;
begin
  QueueSection.Acquire;
  try
    QueuedBufferCount := Max(QueuedBufferCount - 1, 0);
  finally
    QueueSection.Release;
  end;
end;
}

function TAndroidVideoCaptureDevice.GetManualBitmapRotation: Integer;
var
  CameraInfo: JCamera_CameraInfo;
  Display: JDisplay;
  DisplayOrientation: Integer;
begin
  CameraInfo := TJCamera_CameraInfo.JavaClass.init;
  TJCamera.JavaClass.getCameraInfo(FCameraId, CameraInfo);

  Display := TAndroidHelper.Display;
  if Display = nil then
    Exit(0);

  case Display.getRotation of
    0: // TJSurface.JavaClass.ROTATION_0
      DisplayOrientation := 0;
    1: // TJSurface.JavaClass.ROTATION_90
      DisplayOrientation := 90;
    2: // TJSurface.JavaClass.ROTATION_180
      DisplayOrientation := 180;
    3: // TJSurface.JavaClass.ROTATION_270
      DisplayOrientation := 270;
  else
    Exit(0);
  end;

  if CameraInfo.facing = TJCamera_CameraInfo.JavaClass.CAMERA_FACING_FRONT then
    Result := (DisplayOrientation + CameraInfo.orientation) mod 360
  else
    Result := (360 + CameraInfo.orientation - DisplayOrientation) mod 360;
end;

procedure TAndroidVideoCaptureDevice.DoStartCapture;
var
  Params: JCamera_Parameters;
  PreviewSize: JCamera_Size;
begin
  if FCapturing then
    Exit;

  Params := Camera.getParameters;
  if Params = nil then
    Exit;

  // Workaround for Google Glass
  if TPlatformServices.Current.GlobalFlags.ContainsKey(EnableGlassFPSWorkaround) and
    TPlatformServices.Current.GlobalFlags[EnableGlassFPSWorkaround] then
  begin
    Params.setPreviewFpsRange(30000, 30000);
//    Camera.setParameters(Params);     // EvB: Remove
  end;

  // EvB: Added BEGIN
  Params.setPreviewFormat(TJImageFormat.JavaClass.YV12);
  Camera.setParameters(Params);
  // EvB: Added END

  ManualBitmapRotation := GetManualBitmapRotation;

  PreviewSize := Params.getPreviewSize;
  PreviewBufferSize := TPoint.Create(PreviewSize.width, PreviewSize.height);

  if (ManualBitmapRotation div 90) mod 2 > 0 then
    SharedBufferSize := TPoint.Create(PreviewSize.height, PreviewSize.width)
  else
    SharedBufferSize := TPoint.Create(PreviewSize.width, PreviewSize.height);

  { EvB: Remove
  SharedBufferFormat := Params.getPreviewFormat;
  SharedBufferBytes := SharedBufferSize.X * SharedBufferSize.Y *
    (TJImageFormat.JavaClass.getBitsPerPixel(SharedBufferFormat));
  }

  GetCallbackInstance.FCaptureDevice := Self;

  SharedSurface := TBitmapSurface.Create;
  { EvB: Replace
  SharedSurface.SetSize(SharedBufferSize.X, SharedBufferSize.Y, TPixelFormat.RGBA);
  }
  SharedSurface.SetSize(PreviewSize.width, PreviewSize.height,
    TPixelFormat.RGBA);
  SharedSurfaceUpdated := False;

  // In Android 3.0 changed the behaviour of onPreviewFrame
  // forcing to assign a SurfaceTexture or SurfaveView in order to get called

  if TOSVersion.Check(3, 0) then
  begin
	  if SurfaceTexture <> nil then
	    SurfaceTexture.release;

    glGenTextures(1, @SurfaceTextureId);
    glBindTexture(GL_TEXTURE_2D, SurfaceTextureId);

    SurfaceTexture := TJSurfaceTexture.JavaClass.init(SurfaceTextureId);
    Camera.setPreviewTexture(SurfaceTexture);
  end
  else
    Camera.setPreviewDisplay(nil);

  // reset buffer
//  QueuedBufferCount := 0;   // EvB: Remove
  AddCallbackBuffers; // EvB: Add
  Camera.setPreviewCallbackWithBuffer(GetCallbackInstance);
  Camera.startPreview;
//  AddQueueBuffer;           // EvB: Remove

  FCapturing := True;
  CapturePollingTimer.Enabled := True;
end;

procedure TAndroidVideoCaptureDevice.DoStopCapture;
begin
  if FCapturing then
  begin
    FCapturing := False;
    CapturePollingTimer.Enabled := False;   // EvB ICS: Added
//    FreeAndNil(CapturePollingTimer);      // EvB ICS: Removed

    SurfaceSection.Acquire;
    try
      FreeAndNil(SharedSurface);
    finally
      SurfaceSection.Release;
    end;
  end;

  if FPreviewConversionTask <> nil then
    FPreviewConversionTask.Cancel;

  if FCurrentCamera <> nil then
  begin
    FCurrentCamera.setPreviewCallbackWithBuffer(nil);
    FCurrentCamera.stopPreview;

    FCurrentCamera.release;                 // EvB ICS: Do not removed #01
    FCurrentCamera := nil;                  // EvB ICS: Do not removed #01

    // EvB: Added BEGIN
    if (SurfaceTexture <> nil) then
    begin
      SurfaceTexture.release;
      SurfaceTexture := nil;
    end;

    if (SurfaceTextureId <> 0) then
    begin
      glDeleteTextures(1, @SurfaceTextureId);
      SurfaceTextureId := 0;
    end;

  end;

  DeleteCallbackBuffers;
  // EvB: Added END
//  end;
end;

{ EvB: Original version:
procedure TAndroidVideoCaptureDevice.ConvertYuvToBitmapSurfaceHandler(Sender: TObject);

  function CreateJBitmapFromYuvBuffer: JBitmap;
  var
    Image: JYuvImage;
    Rect: JRect;
    Stream: JByteArrayOutputStream;
    LoadOptions: JBitmapFactory_Options;
  begin
    QueueSection.Acquire;
    try
      Image := TJYuvImage.JavaClass.init(SharedBuffer, SharedBufferFormat, PreviewBufferSize.X, PreviewBufferSize.Y,
        nil);
    finally
      QueueSection.Release;
    end;

    Rect := TJRect.JavaClass.init(0, 0, Image.getWidth, Image.getHeight);
    Stream := TJByteArrayOutputStream.JavaClass.init(0);
    Image.compressToJpeg(Rect, FVideoConversionJPEGQuality, Stream);

    // Some resources are freed as early as possible to reduce impact on working memory.
    Rect := nil;
    Image := nil;

    LoadOptions := TJBitmapFactory_Options.JavaClass.init;
    Result := TJBitmapFactory.JavaClass.decodeByteArray(Stream.toByteArray, 0, Stream.Size, LoadOptions);
  end;

  procedure RotateBitmap(var ABitmap: JBitmap; const AAngle: Integer);
  var
    ScaledSize: TPoint;
    TempBitmap: JBitmap;
    Matrix: JMatrix;
    RotatedBitmap: JBitmap;
  begin
    ScaledSize := SharedBufferSize;
    if (ManualBitmapRotation div 90) mod 2 > 0 then
      ScaledSize := TPoint.Create(SharedBufferSize.Y, SharedBufferSize.X);

    TempBitmap := TJBitmap.JavaClass.createScaledBitmap(ABitmap, ScaledSize.X, ScaledSize.Y, True);
    try
      Matrix := TJMatrix.JavaClass.init;
      Matrix.postRotate(ManualBitmapRotation);

      RotatedBitmap := TJBitmap.JavaClass.createBitmap(TempBitmap, 0, 0, TempBitmap.getWidth, TempBitmap.getHeight, Matrix, True);
    finally
      ABitmap.recycle;
      TempBitmap.recycle;
    end;

    ABitmap := RotatedBitmap;
  end;

var
  Bitmap: JBitmap;
  IsSurfaceAlive: Boolean;
begin
  if (SharedBuffer <> nil) and (SharedSurface <> nil) then
  begin
    Bitmap := CreateJBitmapFromYuvBuffer;
    try
      if ManualBitmapRotation <> 0 then
        RotateBitmap(Bitmap, ManualBitmapRotation);

      SurfaceSection.Acquire;
      try
        IsSurfaceAlive := SharedSurface <> nil;
        if IsSurfaceAlive then
          JBitmapToSurface(Bitmap, SharedSurface);
      finally
        SurfaceSection.Release;
      end;
    finally
      Bitmap.recycle;
      Bitmap := nil;
    end;

    if IsSurfaceAlive then
    begin
      UpdatedSection.Acquire;
      try
        SharedSurfaceUpdated := True;
      finally
        UpdatedSection.Release;
      end;
    end;
  end;
end;
}

// EvB: Added
procedure TAndroidVideoCaptureDevice.AddCallbackBuffers;
var
  I, BufferSize: Integer;
begin
  BufferSize := PreviewBufferSize.X * PreviewBufferSize.Y *
    TJImageFormat.JavaClass.getBitsPerPixel(TJImageFormat.JavaClass.YV12);

  for I := 0 to BUFFER_COUNT - 1 do
  begin
    FBuffers[I] := TJavaArray<Byte>.Create(BufferSize);
    Camera.addCallbackBuffer(FBuffers[I]);
  end;
end;

{ EvB: "Fast" version: }
procedure TAndroidVideoCaptureDevice.CopyBufferToSurface
  (AnArray: TJavaArray<Byte>; ACamera: JCamera);
var
  YPtr, UPtr, VPtr: PByte;
  YStride, UVStride, YSize, UVSize: Integer;
begin
  if (SharedSurface <> nil) then
  begin
    SurfaceSection.Acquire;
    try
      YStride := ((PreviewBufferSize.X + 15) shr 4) shl 4;
      UVStride := (((YStride shr 1) + 15) shr 4) shl 4;

      YSize := YStride * PreviewBufferSize.Y;
      UVSize := UVStride * (PreviewBufferSize.Y shr 1);

      YPtr := PByte(AnArray.Data);
      VPtr := YPtr + YSize;
      UPtr := VPtr + UVSize;

      YV12ToRGBA(YPtr, UPtr, VPtr, SharedSurface.Bits, YStride, UVStride,
        PreviewBufferSize.X shl 2, PreviewBufferSize.X, PreviewBufferSize.Y);
    finally
      SurfaceSection.release;
    end;

    UpdatedSection.Acquire;
    try
      SharedSurfaceUpdated := True;
    finally
      UpdatedSection.release;
    end;
  end;
end;

{ EvB: Original version:
procedure TAndroidVideoCaptureDevice.CopyBufferToSurface(AnArray: TJavaArray<Byte>; ACamera: JCamera);
begin
  if (FPreviewConversionTask <> nil) and (FPreviewConversionTask.Status in [TTaskStatus.Running, TTaskStatus.WaitingToRun]) then
    Exit;

  FPreviewConversionTask := TTask.Create(Self, ConvertYuvToBitmapSurfaceHandler).Start;
end;
}

procedure TAndroidVideoCaptureDevice.OnCaptureTimer(Sender: TObject);
var
  UpdatePending: Boolean;
begin
  if (FCaptureTimerIntervalBusy) then     // EvB ICS: Added
    Exit;                                 // EvB ICS: Added
  FCaptureTimerIntervalBusy := True;      // EvB ICS: Added

  UpdatedSection.Acquire;
  try
    UpdatePending := SharedSurfaceUpdated and (SharedSurface <> nil);
    SharedSurfaceUpdated := False;
  finally
    UpdatedSection.Release;
  end;

  if UpdatePending then
  begin
    if Assigned(OnSampleBufferReady) then
      OnSampleBufferReady(Self, 0);
  end;

  FCaptureTimerIntervalBusy := False;   // EvB ICS: Added
//  AddQueueBuffer;   // EvB: Removed
end;

{ EvB: Original version:
procedure TAndroidVideoCaptureDevice.DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);

  procedure DrawBitmapScaled(Source, Dest: TBitmap);
  var
    PrevScale: Single;
  begin
    PrevScale := Dest.BitmapScale;
    Dest.BitmapScale := 1;
    try
      if Dest.Canvas.BeginScene then
      try
        Dest.Canvas.DrawBitmap(Source, TRectF.Create(0, 0, Source.Width, Source.Height), TRectF.Create(0, 0,
          Dest.Width, Dest.Height), 1);
      finally
        Dest.Canvas.EndScene;
      end;
    finally
      Dest.BitmapScale := PrevScale;
    end;
  end;

var
  BiData: TBitmapData;
  TempBitmap: TBitmap;
  I: Integer;
begin
  if not FCapturing then
    Exit;

  if ASetSize then
    ABitmap.SetSize(SharedBufferSize.X, SharedBufferSize.Y);

  SurfaceSection.Acquire;
  try
    if SharedSurface <> nil then
    begin
      if (SharedSurface.Width = ABitmap.Width) and (SharedSurface.Height = ABitmap.Height) then
      begin // Bitmap has exact size, so can copy directly.
        if ABitmap.Map(TMapAccess.Write, BiData) then
        try
          for I := 0 to SharedBufferSize.Y - 1 do
            Move(SharedSurface.Scanline[I]^, BiData.GetScanline(I)^, SharedBufferSize.X * SharedSurface.BytesPerPixel);
        finally
          ABitmap.Unmap(BiData);
        end;
      end
      else
      begin // Bitmap has different size, rescaling is needed.
        TempBitmap := TBitmap.Create;
        try
          TempBitmap.SetSize(SharedBufferSize.X, SharedBufferSize.Y);
          if TempBitmap.Map(TMapAccess.Write, BiData) then
          begin
            try
              for I := 0 to SharedBufferSize.Y - 1 do
                Move(SharedSurface.Scanline[I]^, BiData.GetScanline(I)^, SharedBufferSize.X *
                  SharedSurface.BytesPerPixel);
            finally
              TempBitmap.Unmap(BiData);
            end;

            DrawBitmapScaled(TempBitmap, ABitmap);
          end;
        finally
          TempBitmap := nil;
        end;
      end;
    end;
  finally
    SurfaceSection.Release;
  end;
end;
}

{ EvB: Fast version: }
procedure TAndroidVideoCaptureDevice.DoSampleBufferToBitmap
  (const ABitmap: TBitmap; const ASetSize: Boolean);
var
  Map: TBitmapData;
begin
  if (not FCapturing) then
    Exit;

  if ASetSize then
    ABitmap.SetSize(SharedBufferSize.X, SharedBufferSize.Y);

  SurfaceSection.Acquire;
  try
    if (SharedSurface <> nil) then
    begin
      Assert(SharedSurface.width = PreviewBufferSize.X);
      Assert(SharedSurface.height = PreviewBufferSize.Y);
      if (ABitmap.Map(TMapAccess.Write, Map)) then
        try
{$IFOPT C+}
          if ((ManualBitmapRotation div 90) mod 2 > 0) then
          begin
            Assert(Map.width = SharedSurface.height);
            Assert(Map.height = SharedSurface.width);
          end
          else
          begin
            Assert(Map.width = SharedSurface.width);
            Assert(Map.height = SharedSurface.height);
          end;
{$ENDIF}
          RotateBitmap(SharedSurface.Bits, Map.Data, SharedSurface.width,
            SharedSurface.height, ManualBitmapRotation);
        finally
          ABitmap.Unmap(Map);
        end;
    end;
  finally
    SurfaceSection.release;
  end;
end;

function TAndroidVideoCaptureDevice.DoSetCaptureSetting(const ASetting: TVideoCaptureSetting): Boolean;
var
  Params: JCamera_Parameters;
begin
  Params := Camera.getParameters;
  if Params <> nil then
  begin
    Params.setPreviewSize(ASetting.Width, ASetting.Height);
    Params.setPreviewFrameRate(Round(ASetting.FrameRate));
    Camera.setParameters(Params);
  end;
  Result := Params <> nil;
end;

procedure TAndroidVideoCaptureDevice.OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  if FCapturing then
  begin
    ManualBitmapRotation := GetManualBitmapRotation;

    if (ManualBitmapRotation div 90) mod 2 > 0 then
      SharedBufferSize := TPoint.Create(PreviewBufferSize.Y, PreviewBufferSize.X)
    else
      SharedBufferSize := PreviewBufferSize;

//    if SharedSurface <> nil then    // EvB: Removed
//      SharedSurface.SetSize(SharedBufferSize.X, SharedBufferSize.Y, TPixelFormat.RGBA);

    SharedSurfaceUpdated := False;
  end;
end;

function TAndroidVideoCaptureDevice.DoGetAvailableCaptureSettings: TArray<TVideoCaptureSetting>;
var
  Params: JCamera_Parameters;
  Size: JCamera_Size;
  SizeList, FramerateList: JList;
  I, J: Integer;
  List: TList<TVideoCaptureSetting>;
  Setting: TVideoCaptureSetting;
  Framerate: JInteger;
begin
  SetLength(Result, 0);
  Params := Camera.getParameters;
  if Params <> nil then
  begin
    List := TList<TVideoCaptureSetting>.Create;
    try
      SizeList := Params.getSupportedPreviewSizes;
      FramerateList := Params.getSupportedPreviewFrameRates;
      for I := 0 to SizeList.size - 1 do
      begin
        Size := TJCamera_Size.Wrap(SizeList.get(I));
        for J := 0 to FramerateList.size - 1 do
        begin
          Framerate := TJInteger.Wrap(FramerateList.get(J));
          try
            Setting := TVideoCaptureSetting.Create(Size.width, Size.height, Framerate.intValue);
            List.Add(Setting);
          finally
            Framerate := nil;
          end;
        end;
        Size := nil;
      end;
      Result := List.ToArray;
    finally
      List.Free;
    end;
  end;
end;

class function TAndroidVideoCaptureDevice.GetCallbackInstance: TAndroidVideoCaptureCallback;
begin
  if FCallback = nil then
    FCallback := TAndroidVideoCaptureCallback.Create;

  Result := FCallback;
end;

{$ENDREGION}
{$REGION 'TAndroidMedia'}

constructor TAndroidMedia.Create(const AFileName: string);
var
  AudioService: JObject;
  AudioManager: JAudioManager;
  MaxVolume: Integer;
begin
  inherited Create(AFileName);
  FPlayer := TJMediaPlayer.JavaClass.init;
  FPlayer.setDataSource(StringToJString(FileName));
  FPlayer.prepare;
  AudioService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  if AudioService <> nil then
    AudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(AudioService));
  if AudioManager <> nil then
  begin
    MaxVolume := AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
    FVolume := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
    if MaxVolume > 0 then
      FVolume := FVolume / MaxVolume ;
    FVolume := Min(FVolume, 1);
  end;
end;

destructor TAndroidMedia.Destroy;
begin
  FPlayer.release;
  FPlayer := nil;
  inherited Destroy;
end;

function TAndroidMedia.GetCurrent: TMediaTime;
begin
  Result := Round(FPlayer.getCurrentPosition * MediaTimeScale / MSecsPerSec);
end;

function TAndroidMedia.GetDuration: TMediaTime;
begin
  Result := Round(FPlayer.getDuration * MediaTimeScale / MSecsPerSec);
end;

function TAndroidMedia.GetMediaState: TMediaState;
begin
  if FPlayer.isPlaying then
    Result := TMediaState.Playing
  else
    Result := TMediaState.Stopped;
end;

function TAndroidMedia.GetVideoSize: TPointF;
begin
  Result := TPointF.Create(FPlayer.getVideoWidth, FPlayer.getVideoHeight)
end;

function TAndroidMedia.GetVolume: Single;
begin
  Result := FVolume
end;

function TAndroidMedia.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := FPlayer.QueryInterface(IID, Obj);
end;

procedure TAndroidMedia.SeekToBegin;
begin
  FPlayer.seekTo(0);
end;

procedure TAndroidMedia.SetCurrent(const Value: TMediaTime);
begin
  FPlayer.seekTo(Max(0, Round(Value * MSecsPerSec / MediaTimeScale)));
end;

procedure TAndroidMedia.SetVolume(const Value: Single);
begin
  FVolume := EnsureRange(Value, 0, 1);
  FPlayer.setVolume(FVolume, FVolume);
end;

procedure TAndroidMedia.UpdateMediaFromControl;
begin
end;

procedure TAndroidMedia.DoStop;
begin
  FPlayer.pause;
end;

procedure TAndroidMedia.DoPlay;
begin
  FPlayer.start;
end;

{$ENDREGION}
{$REGION 'TAndroidMediaCodec'}

function TAndroidMediaCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TAndroidMedia.Create(AFileName);
end;

{$ENDREGION}

{$REGION 'TAndroidVideoCodec'}

function TAndroidVideoCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TAndroidVideo.Create(AFileName);
end;
{$ENDREGION}

{$REGION 'TAndroidVideo'}

constructor TAndroidVideo.Create(const AFileName: string);
const
  DefaultScale = 1;
var
  ScreenSrv: IFMXScreenService;
begin
  FVolume := TCommonVolume.Create;
  FVideoEnabled := False;
  inherited Create(AFileName);

  CheckVideo;

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenSrv) then
    FScale := ScreenSrv.GetScreenScale
  else
    FScale := DefaultScale;
end;

function TAndroidVideo.AllAssigned: Boolean;
begin
  Result := FVideoPlayer <> nil;
end;

procedure TAndroidVideo.CheckVideo;
const
  CYes = 'yes';
var
  MMR : JMediaMetadataRetriever;
  HasVideoValue: string;
begin
  if TOSVersion.Check(4, 0) and FileExists(FileName) then
  begin
    MMR := TJMediaMetadataRetriever.JavaClass.init;
    MMR.setDataSource(StringToJString(FileName));
    HasVideoValue := JStringTOString(MMR.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_HAS_VIDEO));
    FVideoEnabled := CYes = HasVideoValue;
    MMR := nil;
  end
  else
    FVideoEnabled := Control <> nil;
end;

procedure TAndroidVideo.RealignView;

  procedure UpdateViewBounds;
  var
    OriginalVideoRect: TRectF;
    FitRect: TRectF;
    HMargins: Single;
    VMargins: Single;
    LP: JRelativeLayout_LayoutParams;
  begin
    OriginalVideoRect := TRectF.Create(0, 0, FVideoSize.Width, FVideoSize.Height);
    FitRect := OriginalVideoRect.FitInto(Control.LocalRect);
    if not FitRect.IsEmpty then
    begin
      HMargins := (Control.Width - FitRect.Width) / 2;
      VMargins := (Control.Height - FitRect.Height) / 2;

      LP := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
                                                         TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_TOP, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_BOTTOM, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_LEFT, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_RIGHT, TJRelativeLayout.JavaClass.TRUE);

      if not FitRect.IsEmpty then
      begin
        LP.leftMargin := Round(HMargins * FScale);
        LP.topMargin := Round(VMargins * FScale);
        LP.rightMargin := Round(HMargins * FScale);
        LP.bottomMargin := Round(VMargins * FScale);
      end;

      FVideoPlayer.setLayoutParams(LP);
    end;
  end;

var
  SavedCurrentPosition: Integer;
  SavedIsStarted: Boolean;
begin
  SavedCurrentPosition := FVideoPlayer.getCurrentPosition;
  SavedIsStarted := FVideoPlayer.isPlaying;

  if ZOrderManager <> nil then
  begin
    ZOrderManager.UpdateOrderAndBounds(Control);
    // VideoView is placed inside Container. ZOrderManager controls visibility of Container based on TMediaPlayerControl.
    // But the Android doesn't hide VideoView, if parent is hidden. It's a bug in Android VideoView, so we need to update
    // visibility of VideoView also.
    if Control.ParentedVisible then
      FVideoPlayer.setVisibility(TJView.JavaClass.VISIBLE)
    else
      FVideoPlayer.setVisibility(TJView.JavaClass.GONE);
  end;
  FVideoPlayer.seekTo(SavedCurrentPosition);
  if SavedIsStarted then
    FVideoPlayer.start;

  UpdateViewBounds;
end;

procedure TAndroidVideo.RetreiveVideoSize;
var
  MediaPlayer: JMediaPlayer;
begin
  MediaPlayer := TJMediaPlayer.JavaClass.init;
  MediaPlayer.setDataSource(StringToJString(FileName));
  MediaPlayer.prepare;
  FVideoSize := TSize.Create(MediaPlayer.getVideoWidth, MediaPlayer.getVideoHeight);
  MediaPlayer := nil;
end;

destructor TAndroidVideo.Destroy;

  procedure RemoveContainer;
  var
    Container: JViewGroup;
  begin
    if FVideoPlayer.getParent <> nil then
    begin
      Container := TJViewGroup.Wrap(FVideoPlayer.getParent);
      Container.removeView(FVideoPlayer);

      if Container.getParent <> nil then
        TJViewGroup.Wrap(Container.getParent).removeView(FVideoPlayer);
    end;
  end;

var
  LInstance: TVideoInstance;
begin
  if ZOrderManager <> nil then
    ZOrderManager.RemoveLink(Control);
  if FVideoPlayer <> nil then
  begin
    RemoveContainer;

    LInstance.VideoPlayer := FVideoPlayer;
    VideoPool.UIReturnInstance(LInstance);
  end;
  inherited Destroy;
end;

procedure TAndroidVideo.DoPlay;
begin
  inherited;
  if IsVideoEnabled then
    FVideoPlayer.start
  else
    FJustAudio.DoPlay;
end;

procedure TAndroidVideo.DoStop;
begin
  inherited;
  if IsVideoEnabled then
    FVideoPlayer.pause
  else
    FJustAudio.DoStop;
end;

function TAndroidVideo.GetCurrent: TMediaTime;
begin
  Result := 0;
  if IsVideoEnabled then
  begin
    if AllAssigned then
    begin
      Result := FVideoPlayer.getCurrentPosition;
      Result := Round(Result * MediaTimeScale / MSecsPerSec);
    end;
  end
  else
    Result := FJustAudio.GetCurrent;
end;

function TAndroidVideo.GetDuration: TMediaTime;
begin
  Result := 0;
  if IsVideoEnabled then
  begin
    if AllAssigned then
    begin
      Result := FVideoPlayer.getDuration;
      Result := Round(Result * MediaTimeScale / MSecsPerSec);
    end;
  end
  else
    Result := FJustAudio.GetDuration;
end;

function TAndroidVideo.GetMediaState: TMediaState;
begin
  if IsVideoEnabled then
  begin
    if FVideoPlayer <> nil then
    begin
      if FVideoPlayer.isPlaying then
        Result := TMediaState.Playing
      else
        Result := TMediaState.Stopped;
    end
    else
      Result := TMediaState.Unavailable;
  end
  else
    Result := FJustAudio.GetMediaState;
end;

function TAndroidVideo.GetVideoSize: TPointF;
begin
  if IsVideoEnabled then
    Result := TPointF.Create(FVideoSize.Width, FVideoSize.Height)
  else
    Result := TPointF.Zero;
end;

function TAndroidVideo.GetVolume: Single;
begin
  if IsVideoEnabled then
    Result := FVolume.Value
  else
    Result := FJustAudio.GetVolume;
end;

function TAndroidVideo.GetZOrderManager: TAndroidZOrderManager;
var
  Form: TCommonCustomForm;
begin
  if (Control <> nil) and (Control.Root <> nil) and (Control.Root.GetObject is TCommonCustomForm) then
  begin
    Form := TCommonCustomForm(Control.Root);
    Result := WindowHandleToPlatform(Form.Handle).ZOrderManager;
  end
  else
    Result := nil;
end;

procedure TAndroidVideo.InitInstance;
var
  LFileName: string;
  LInstance: TVideoInstance;
  Container: JRelativeLayout;
  LP: JRelativeLayout_LayoutParams;
begin
  LFileName := FileName;
  if FVideoEnabled then
  begin
    RetreiveVideoSize;
    if FVideoPlayer = nil then
    begin
      LInstance := VideoPool.UIGetInstance;
      FVideoPlayer := LInstance.VideoPlayer;
      FVideoPlayer.setVisibility(TJView.JavaClass.VISIBLE);
      FVideoPlayer.setVideoPath(StringToJString(LFileName));

      Container := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
      LP := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
                                                         TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_TOP, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_BOTTOM, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_LEFT, TJRelativeLayout.JavaClass.TRUE);
      LP.addRule(TJRelativeLayout.JavaClass.ALIGN_PARENT_RIGHT, TJRelativeLayout.JavaClass.TRUE);
      Container.addView(FVideoPlayer, LP);

      ZOrderManager.AddOrSetLink(Control, {FVideoPlayer}Container, nil);
      ZOrderManager.UpdateOrderAndBounds(Control);
    end;
    RealignView;
  end
  else
    FJustAudio := TAndroidMedia.Create(FileName);
end;

function TAndroidVideo.InstanceCreated: Boolean;
begin
  Result := (FJustAudio <> nil) or (FVideoPlayer <> nil);
end;

procedure TAndroidVideo.SeekToBegin;
begin
  if IsVideoEnabled then
  begin
    if AllAssigned then
    begin
      FVideoPlayer.stopPlayback;
      FVideoPlayer.seekTo(0);
    end;
  end
  else
    FJustAudio.SeekToBegin;
end;

procedure TAndroidVideo.SetCurrent(const Value: TMediaTime);
begin
  inherited;
  if IsVideoEnabled then
  begin
    if AllAssigned then
      FVideoPlayer.seekTo(Round(Value * MSecsPerSec / MediaTimeScale));
  end
  else
    FJustAudio.SetCurrent(Value);
end;

procedure TAndroidVideo.SetVolume(const Value: Single);
begin
  inherited;
  if IsVideoEnabled then
    FVolume.Value := Value
  else
    FJustAudio.SetVolume(Value);
end;

procedure TAndroidVideo.UpdateMediaFromControl;
begin
  inherited;
  if IsVideoEnabled then
    RealignView
  else
    FJustAudio.UpdateMediaFromControl;
end;

function TAndroidVideo.IsVideoEnabled: Boolean;
begin
  if not InstanceCreated then
    InitInstance;
  Result := FVideoEnabled and (Control <> nil);
end;

function TAndroidVideo.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Supports(FVideoPlayer, IID, Obj) then
    Result := S_OK
  else
    Result := E_NOTIMPL;
end;

{$ENDREGION}

{ TAndroidVideo.TVolume }

constructor TAndroidVideo.TCommonVolume.Create;
begin
  FAudioService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  if FAudioService <> nil then
    FAudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(FAudioService));
  if FAudioManager <> nil then
    FMaxVolume := FAudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
end;

function TAndroidVideo.TCommonVolume.GetVolume: Single;
begin
  if FMaxVolume = 0 then
    Result := 0
  else
    Result := Min(1, FAudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC) / FMaxVolume);
end;

procedure TAndroidVideo.TCommonVolume.SetVolume(const Value: Single);
begin
  if FAudioManager <> nil then
    FAudioManager.setStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, Round(Value * FMaxVolume), 0);
end;

{ TVideoPool }

constructor TVideoPool.Create;
begin
  FUsed := TList<TVideoInstance>.Create;
  FReadyToUse := TList<TVideoInstance>.Create;
end;

procedure TVideoPool.CreateOneMoreInstance;
var
  LItem: TVideoInstance;
begin
  LItem.VideoPlayer := TJVideoView.JavaClass.init(TAndroidHelper.Activity);
  LItem.VideoPlayer.requestFocus(0);
  LItem.VideoPlayer.setZOrderOnTop(True);
  LItem.VideoPlayer.setZOrderMediaOverlay(True);
  FReadyToUse.Add(LItem);
end;

destructor TVideoPool.Destroy;
var
  LItem: TVideoInstance;
begin
  while FUsed.Count > 0 do
  begin
    LItem := FUsed.First;
    VideoPool.UIReturnInstance(LItem);
  end;
  FUsed.Free;
  FReadyToUse.Free;
  inherited;
end;

procedure TVideoPool.UIFreezeInstance(const AInstance: TVideoInstance);
begin
  AInstance.VideoPlayer.stopPlayback;
  AInstance.VideoPlayer.setVisibility(TJView.JavaClass.INVISIBLE);
end;

function TVideoPool.UIGetInstance: TVideoInstance;
begin
  if FReadyToUse.Count = 0 then
    CreateOneMoreInstance;
  Result := FReadyToUse.First;
  FReadyToUse.Remove(Result);
  FUsed.Add(Result);
end;

procedure TVideoPool.UIReturnInstance(const AInstance: TVideoInstance);
begin
  UIFreezeInstance(AInstance);
  FUsed.Remove(AInstance);
  FReadyToUse.Add(AInstance);
end;

initialization
  VideoPool := TVideoPool.Create;
  TMediaCodecManager.RegisterMediaCodecClass('.mov', SVMOVFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.m4v', SVM4VFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp4', SVMP4Files, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.3gp', SV3GPFiles, TMediaType.Video, TAndroidVideoCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp3', SVMP3Files, TMediaType.Audio, TAndroidMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.caf', SVCAFFiles, TMediaType.Audio, TAndroidMediaCodec);

  TMediaCodecManager.RegisterMediaCodecClass(SAllFilesExt, SDefault, TMediaType.Video, TAndroidMediaCodec);

finalization
  VideoPool.Free;

end.
