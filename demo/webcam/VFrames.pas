unit VFrames;

(******************************************************************************

  VFrames.pas
  Class TVideoImage

About
  The TVideoImage class provides a simplified access to the class TVideoSample
  from source unit VSample.pas.
  It is used to access WebCams and similar Video-capture devices via DirectShow.
  Its focus is on acquiring single images (frames) from the running video stream
  sent by the cameras. There exist methods to control properties (e.g. size,
  brightness etc.)
  Acquisition usually is fast enough to simulate running video.
  No audio support.

History
  Version 1.6
    2012-07-09
    Support for 8-bit Grayscale images. Reduces time for image expansion for some types
    of compressions. (But not for all, e.g. RGB!)
    Some memory leaks fixed.

  Version 1.5
    GDI+ support for MJPG, if GDI+ available
    YUY2 relaxed check of data size to support 1280*720 video size for Microsoft LifeCam Cinema

  Version 1.4
    Added support for YUY2 (YUYV, YUNV), MJPG, I420 (YV12, IYUV)

  Version 1.3
  07.09.2008
    Added Video-Size and Video-property control
    Added check for extreme CPU load

  Version 1.2
  30.08.2008
    Added Pause and Resume
    
  Version 1.1
  26.07.2008

Contact:
  michael@grizzlymotion.com

Copyright
  For copyrights of the DirectX Header ports see the original source files.
  Other code (unless stated otherwise, see comments): Copyright (C) M. Braun

Licence:
  The lion share of this project lies within the ports of the DirectX header
  files (which are under the Mozilla Public License Version 1.1), and the
  original SDK sample files from Microsoft (END-USER LICENSE AGREEMENT FOR
  MICROSOFT SOFTWARE DirectX 9.0 Software Development Kit Update (Summer 2003))

  My own contribution compared to that work is very small (although it cost me
  lots of time), but still is "significant enough" to fulfill Microsofts licence
  agreement ;)
  So I think, the ZLib licence (http://www.zlib.net/zlib_license.html)
  should be sufficient for my code contributions.

Please note:
  There exist much more complete alternatives (incl. sound, AVI etc.):
  - DSPack (http://www.progdigy.com/)
  - TVideoCapture by Egor Averchenkov (can be found at http://www.torry.net)


******************************************************************************)



interface


USES Windows, Messages, Controls, Forms, SysUtils, Graphics, Classes,
     AppEvnts, MMSystem, DirectShow9, JPEG, Math,
     VSample;

CONST
  CBufferCnt = 3;  // Triple-Buffer

TYPE
  TNewVideoFrameEvent = procedure(Sender : TObject; Width, Height: integer; DataPtr: pointer) of object;
  TVideoProperty = (VP_Brightness,
                    VP_Contrast,
                    VP_Hue,
                    VP_Saturation,
                    VP_Sharpness,
                    VP_Gamma,
                    VP_ColorEnable,
                    VP_WhiteBalance,
                    VP_BacklightCompensation,
                    VP_Gain);
  TVideoImage = class
                  private
                    VideoSample   : TVideoSample;
                    OnNewFrameBusy: boolean;
                    fVideoRunning : boolean;
                    fBusy         : boolean;
                    fGray8Bit    : boolean;
                    fSkipCnt      : integer;
                    fFrameCnt     : integer;
                    f30FrameTick  : cardinal;
                    fFPS          : double;  // "Real" fps, even if not all frames will be displayed.
                    fWidth,
                    fHeight       : integer;
                    fFourCC       : cardinal;
                    fBitmap       : TBitmap;
                    fBitmapGray   : TBitmap;
                    fDisplayCanvas: TCanvas;
                    fImagePtr     : ARRAY[0..CBufferCnt] OF pointer; // Local copy of image data
                    fImagePtrSize : ARRAY[0..CBufferCnt] OF integer;
                    fImagePtrIndex: integer;
                    fMessageHWND  : HWND;
                    fMsgNewFrame  : uint;
                    fOnNewFrame   : TNewVideoFrameEvent;
                    AppEvent      : TApplicationEvents;
                    IdleEventTick : cardinal;
                    ValueY_298,
                    ValueU_100,
                    ValueU_516,
                    ValueV_409,
                    ValueV_208    : ARRAY[byte] OF integer;
                    ValueL_255    : ARRAY[byte] OF byte;
                    ValueClip     : ARRAY[-1023..1023] OF byte;
                    GrayConvR,
                    GrayConvG,
                    GrayConvB     : ARRAY[0..255] OF integer;
                    fYUY2TablesPrepared : boolean;
                    JPG           : TJPEGImage;
                    MemStream     : TMemoryStream;
                    fImageUnpacked: boolean;
                    procedure     PaintFrame;
                    procedure     UnpackFrame(Size: integer; pData: pointer);
                    procedure     WndProc(var Msg: TMessage);
                    function      VideoSampleIsPaused: boolean;
                    procedure     AppEventsIdle(Sender: TObject; var Done: Boolean);
                    procedure     CallBack(pb : pbytearray; var Size: integer);
                    function      TranslateProperty(const VP: TVideoProperty; VAR VPAP: TVideoProcAmpProperty): HResult;
                    PROCEDURE     PrepareGrayBMP(VAR BM : TBitmap; W, H: integer);
                    PROCEDURE     PrepareTables;
                    procedure     YUY2_to_RGB(pData: pointer);
                    procedure     YUY2_to_Gray8Bit(pData: pointer);
                    procedure     I420_to_RGB(pData: pointer);
                    procedure     I420_to_Gray8Bit(pData: pointer);
                    procedure     RGB_to_Gray8Bit(pData: pointer);
                  public
                    constructor   Create;
                    destructor    Destroy; override;
                    property      IsPaused: boolean read VideoSampleIsPaused;
                    property      VideoRunning : boolean read fVideoRunning;
                    property      VideoWidth: integer read fWidth;
                    property      VideoHeight: integer read fHeight;
                    property      Gray8Bit: boolean read fGray8Bit write fGray8Bit;
                    property      OnNewVideoFrame : TNewVideoFrameEvent read fOnNewFrame write fOnNewFrame;
                    property      FramesPerSecond: double read fFPS;
                    property      FramesSkipped: integer read fSkipCnt;
                    procedure     GetListOfDevices(DeviceList: TStringList);
                    procedure     VideoStop;
                    procedure     VideoPause;
                    procedure     VideoResume;
                    function      VideoStart(DeviceName: string): integer;
                    procedure     GetBitmap(BMP: TBitmap);
                    procedure     SetDisplayCanvas(Canvas: TCanvas);
                    procedure     ShowProperty;
                    procedure     ShowProperty_Stream;
                    FUNCTION      ShowVfWCaptureDlg: HResult;
                    procedure     GetBrightnessSettings(VAR Actual: integer);
                    procedure     SetBrightnessSettings(const Actual: integer);
                    PROCEDURE     GetListOfSupportedVideoSizes(VidSize: TStringList);
                    PROCEDURE     SetResolutionByIndex(Index: integer);
                    FUNCTION      GetVideoPropertySettings(    VP                : TVideoProperty;
                                                           VAR MinVal, MaxVal,
                                                               StepSize, Default,
                                                               Actual            : integer;
                                                           VAR AutoMode: boolean): HResult;
                    FUNCTION      SetVideoPropertySettings(VP: TVideoProperty; Actual: integer; AutoMode: boolean): HResult;
                    PROCEDURE     Convert24ToGray(BM24: TBitmap; BMGray: TBitmap);
                end;



FUNCTION GetVideoPropertyName(VP: TVideoProperty): string;


// http://www.fourcc.org/yuv.php#UYVY

CONST
  FourCC_YUY2 = $32595559;
  FourCC_YUYV = $56595559;
  FourCC_YUNV = $564E5559;

  FourCC_MJPG = $47504A4D;

  FourCC_I420 = $30323449;
  FourCC_YV12 = $32315659;
  FourCC_IYUV = $56555949;




implementation



FUNCTION GetVideoPropertyName(VP: TVideoProperty): string;
BEGIN
  CASE VP OF
    VP_Brightness           : Result := 'Brightness';
    VP_Contrast             : Result := 'Contrast';
    VP_Hue                  : Result := 'Hue';
    VP_Saturation           : Result := 'Saturation';
    VP_Sharpness            : Result := 'Sharpness';
    VP_Gamma                : Result := 'Gamma';
    VP_ColorEnable          : Result := 'ColorEnable';
    VP_WhiteBalance         : Result := 'WhiteBalance';
    VP_BacklightCompensation: Result := 'Backlight';
    VP_Gain                 : Result := 'Gain';
  END; {case}
END;



(* Finally, callback seems to work. Previously it only ran for a few seconds.
   The reason for that seemed to be a deadlock (see http://msdn.microsoft.com/en-us/library/ms786692(VS.85).aspx)
   Now the image data is copied immediatly, and a message is sent to invoke the
   display of the data. *)
procedure TVideoImage.CallBack(pb : pbytearray; var Size: integer);
var
  i  : integer;
  T1 : cardinal;
begin
  Inc(fFrameCnt);

  // Calculate "Frames per second"...
  T1 := TimeGetTime;
  IF fFrameCnt mod 30 = 0 then
    begin
      if f30FrameTick > 0 then
        fFPS := 30000 / (T1-f30FrameTick);
      f30FrameTick := T1;
    end;



  // führt auf Windows 7 zu unendlich kleinen Frameraten! -cm
{
  // Does the application run in unhealthy CPU usage?
  // Check, if no idle event has occured for at least 1 sec.
  // If so, skip current frame and give application time to "breathe".
  IF Abs(T1-IdleEventTick) > 1000 then
    begin
      Inc(fSkipCnt);
      exit;
    end;
}
  // Adjust pointer to image data if necessary
  i := (fImagePtrIndex+1) mod CBufferCnt;
  IF fImagePtrSize[i] <> Size then
    begin
      IF fImagePtrSize[i] > 0 then
        FreeMem(fImagePtr[i], fImagePtrSize[i]);
      fImagePtrSize[i] := Size;
      GetMem(fImagePtr[i], fImagePtrSize[i]);
    end;
  // Save image data to local memory
  move(pb^, fImagePtr[i]^, Size);
  fImagePtrIndex := i;
  fImageUnpacked := false;

  // This routine is called by the video software and therefore runs within their thread.
  // Posting a message to our own HWND will transport the information to the main thread.
  PostMessage(fMessageHWND, fMsgNewFrame, Size, integer(fImagePtr[i]));
  sleep(0);
end;



// Own windows message handler only to get the "New Video Frame has arrived" message.
// Used to get the information out of the Camera-Thread into the application's thread.
// Otherwise we would run into a deadlock.
procedure TVideoImage.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = fMsgNewFrame then
      try
        IF not fBusy then
          begin
            fBusy := true;
            fImageUnpacked := false;
            PaintFrame; // If a Display-Canvas has been set, paint video image on it.
            IF assigned(fOnNewFrame) then
              fOnNewFrame(self, fWidth, fHeight, fImagePtr[fImagePtrIndex]);
            fBusy := false;
          end
          else Inc(fSkipCnt);
      except
        Application.HandleException(Self);
        fBusy := false;
      end
    else Result := DefWindowProc(fMessageHWND, Msg, wParam, lParam);
end;



constructor TVideoImage.Create;
VAR
  i : integer;
begin
  inherited Create;
  fVideoRunning   := false;
  OnNewFrameBusy  := false;
  fBitmap         := TBitmap.Create;
  fBitmapGray     := TBitmap.Create;
  fDisplayCanvas  := nil;
  fWidth          := 0;
  fHeight         := 0;
  fFourCC         := 0;
  FOR i := 0 TO CBufferCnt-1 DO
    BEGIN
      fImagePtr[i]     := nil; 
      fImagePtrSize[i] := 0;
    END;
  fMsgNewFrame    := wm_user+662;
  fOnNewFrame     := nil;
  fBusy           := false;
  // Create a HWND that can capture some messages for us...
  fMessageHWND    := AllocateHWND(WndProc);
  AppEvent        := TApplicationEvents.Create(Application.MainForm);
  AppEvent.OnIdle := AppEventsIdle;
  JPG             := TJPEGImage.Create;
//  JPG.Performance := jpBestSpeed;
  MemStream       := TMemoryStream.Create;

  fGray8Bit := false;
  FOR i := 0 TO 255 DO
    BEGIN
      GrayConvR[i] := 100 * i;
      GrayConvG[i] := 128 * i;
      GrayConvB[i] :=  28 * i  +127;
    END;

  PrepareTables;
end;


// Check, when the last OnIdle message arrived. Save a time stamp.
// Used to check the CPU load. If necessary, we will skip video frames...
procedure TVideoImage.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  IdleEventTick := TimeGetTime;
  Done := true;
end;


destructor  TVideoImage.Destroy;
VAR
  i : integer;
begin
  FOR i := CBufferCnt-1 DOWNTO 0 DO
    IF fImagePtrSize[i] <> 0 then
      begin
        FreeMem(fImagePtr[i], fImagePtrSize[i]);
        fImagePtr[i] := nil;
        fImagePtrSize[i] := 0;
      end;
  DeallocateHWnd(fMessageHWND);

  fDisplayCanvas := nil;
  fBitmapGray.Free;
  fBitmap.Free;
  JPG.Free;
  AppEvent.OnIdle := nil;
  AppEvent.Free;
  AppEvent := nil;
  MemStream.Free;

  inherited Destroy;
end;

// For Properties see also http://msdn.microsoft.com/en-us/library/ms786938(VS.85).aspx
function TVideoImage.TranslateProperty(const VP: TVideoProperty; VAR VPAP: TVideoProcAmpProperty): HResult;
begin
  Result := S_OK;
  CASE VP OF
    VP_Brightness             : VPAP := VideoProcAmp_Brightness;
    VP_Contrast               : VPAP := VideoProcAmp_Contrast;
    VP_Hue                    : VPAP := VideoProcAmp_Hue;
    VP_Saturation             : VPAP := VideoProcAmp_Saturation;
    VP_Sharpness              : VPAP := VideoProcAmp_Sharpness;
    VP_Gamma                  : VPAP := VideoProcAmp_Gamma;
    VP_ColorEnable            : VPAP := VideoProcAmp_ColorEnable;
    VP_WhiteBalance           : VPAP := VideoProcAmp_WhiteBalance;
    VP_BacklightCompensation  : VPAP := VideoProcAmp_BacklightCompensation;
    VP_Gain                   : VPAP := VideoProcAmp_Gain;
    else Result := S_False;
  END; {case}
end;



FUNCTION TVideoImage.GetVideoPropertySettings(VP: TVideoProperty; VAR MinVal, MaxVal, StepSize, Default, Actual: integer; VAR AutoMode: boolean): HResult;
VAR
  VPAP       : TVideoProcAmpProperty;
  pCapsFlags : TVideoProcAmpFlags;
BEGIN
  Result   := S_FALSE;
  MinVal   := -1;
  MaxVal   := -1;
  StepSize := 0;
  Default  := 0;
  Actual   := 0;
  AutoMode := true;
  IF not(assigned(VideoSample)) or Failed(TranslateProperty(VP, VPAP)) then
    exit;
  Result := TranslateProperty(VP, VPAP);
  IF Failed(Result) then
    exit;

  Result := VideoSample.GetVideoPropAmpEx(VPAP, MinVal, MaxVal, StepSize, Default, pCapsFlags, Actual);
  IF Failed(Result) then
    begin
      MinVal   := -1;
      MaxVal   := -1;
      StepSize := 0;
      Default  := 0;
      Actual   := 0;
      AutoMode := true;
    end
    else begin
      AutoMode := pCapsFlags <> VideoProcAmp_Flags_Manual;
    end;
END;



FUNCTION TVideoImage.SetVideoPropertySettings(VP: TVideoProperty; Actual: integer; AutoMode: boolean): HResult;
VAR
  VPAP       : TVideoProcAmpProperty;
  pCapsFlags : TVideoProcAmpFlags;
BEGIN
  Result := TranslateProperty(VP, VPAP);
  IF not(assigned(VideoSample)) or Failed(Result) then
    exit;
  IF AutoMode
    then pCapsFlags := VideoProcAmp_Flags_Auto
    else pCapsFlags := VideoProcAmp_Flags_Manual;
  Result := VideoSample.SetVideoPropAmpEx(VPAP, pCapsFlags, Actual);
END;



procedure TVideoImage.GetListOfDevices(DeviceList: TStringList);
begin
  GetCaptureDeviceList(DeviceList);
end;


procedure TVideoImage.VideoPause;
begin
  if not assigned(VideoSample) then
    exit;
  VideoSample.PauseVideo;
end;



procedure TVideoImage.VideoResume;
begin
  if not assigned(VideoSample) then
    exit;
  VideoSample.ResumeVideo;
end;



procedure TVideoImage.VideoStop;
begin
  fFPS := 0;
  if not assigned(VideoSample) then
    exit;

  try
    VideoSample.Free;
    VideoSample := nil;
  except
  end;
  fVideoRunning := false;
end;



function TVideoImage.VideoStart(DeviceName: string): integer;
VAR
  hr     : HResult;
  st     : string;
  W, H   : integer;
  FourCC : cardinal;
begin
  fSkipCnt       := 0;
  fFrameCnt      := 0;
  f30FrameTick   := 0;
  fFPS           := 0;
  fImageUnpacked := false;

  Result := 0;
  if assigned(VideoSample) then
    VideoStop;

  VideoSample := TVideoSample.Create(Application.MainForm.Handle, false, 0, HR); // No longer force RGB24
  try
    hr := VideoSample.StartVideo(DeviceName, false, st) // Not visible. Displays itself...
  except
    hr := -1;
  end;

  if Failed(hr)
    then begin
      VideoStop;
     // ShowMessage(DXGetErrorDescription9A(hr));
     Result := 1;
    end
    else begin
      hr := VideoSample.GetStreamInfo(W, H, FourCC);
      IF Failed(HR)
        then begin
          VideoStop;
          Result := 1;
        end
        else BEGIN
          fWidth := W;
          fHeight := H;
          fFourCC := FourCC;
          FBitmap.PixelFormat := pf24bit;
          FBitmap.Width := W;
          FBitmap.Height := H;
          PrepareGrayBMP(FBitmapGray, W, H);
          VideoSample.SetCallBack(CallBack);  // Do not call GDI routines in Callback!
        END;
    end;
end;



function TVideoImage.VideoSampleIsPaused: boolean;
begin
  if assigned(VideoSample)
    then Result := VideoSample.PlayState = PS_PAUSED
    else Result := false;
end;



// Create an 8bit grayscale palette image with width W and Height H.
PROCEDURE TVideoImage.PrepareGrayBMP(VAR BM : TBitmap; W, H: integer);
TYPE
  TLogPal =  packed record
               palVersion: Word;
               palNumEntries: Word;
               palPalEntry: array[0..255] of TPaletteEntry;  // In contrast to original declaration uses 255 instead of 0
             end;
VAR
  Pal  : TLogPal;
  _Pal : tagLogPalette absolute Pal;  // Trick! ;)
  dw   : LongWord;
BEGIN
   WITH Pal DO
     BEGIN
       palVersion:=$300;
       palNumEntries:=256;
       FOR dw := 0 TO 255 DO
         palPalEntry[dw] := TPaletteEntry(dw * $010101);
     END;
  BM.width := W;
  BM.Height := H;
  BM.Transparent := false;
  BM.pixelformat := pf8bit;
  BM.Palette := CreatePalette(_Pal);
END; {PrepareGrayBMP}



PROCEDURE TVideoImage.Convert24ToGray(BM24: TBitmap; BMGray: TBitmap);
{ - Convert a 24bit RGB bitmap into a 8bit grayscale image }
//type
//  tbytearray = ARRAY[0..16387] OF byte;
//  pbytearray = ^tbytearray;
//VAR
//  p24, p8  : pbytearray;
//  X, Y, X3 : integer;
BEGIN
  IF BM24.PixelFormat = pf8bit then
    begin
      BMGray.assign(BM24);
      exit;
    end;

  if (BM24.Width <> BMGray.Width) or (BM24.Height <> BMGray.Height) or (BMGray.PixelFormat <> pf8bit) then
    PrepareGrayBMP(BMGray, BM24.Width, bm24.Height);
  {  This is the do-it-yourself way of converting RGB to GrayScale:
  FOR Y := BM24.height-1 DOWNTO 0 do
    begin
      p24 := BM24.ScanLine[Y];
      p8  := BMGray.ScanLine[Y];
      X3 := 0;
      FOR X := 0 TO BMGray.Width-1 DO
        begin
          p8^[X] := (GrayConvB[p24^[X3]] + GrayConvG[p24^[X3+1]] + GrayConvR[p24^[X3+2]]) div 256;
          Inc(X3, 3);
        end;
    end;
   }
  BMGray.Canvas.Draw(0, 0, BM24);
END;



PROCEDURE TVideoImage.PrepareTables;
VAR
  i : integer;
BEGIN
  IF fYUY2TablesPrepared then
    exit;
  FOR i := 0 TO 255 DO
    BEGIN
      { http://msdn.microsoft.com/en-us/library/ms893078.aspx
      ValueY_298[i] := (i- 16) * 298  +  128;      //  -4640 .. 71350
      ValueU_100[i] := (i-128) * 100;              // -12800 .. 12700
      ValueU_516[i] := (i-128) * 516;              // -66048 .. 65532
      ValueV_409[i] := (i-128) * 409;              // -52352 .. 51943
      ValueV_208[i] := (i-128) * 208;              // -26624 .. 26416
      }
      // http://en.wikipedia.org/wiki/YCbCr  (ITU-R BT.601)
      ValueY_298[i] := round(i *  298.082);
      ValueU_100[i] := round(i * -100.291);
      ValueU_516[i] := round(i *  516.412  - 276.836*256);
      ValueV_409[i] := round(i *  408.583  - 222.921*256);
      ValueV_208[i] := round(i * -208.120  + 135.576*256);
      ValueL_255[i] := Min(255, round(i *  298.082 / 255));
    END;
  FillChar(ValueClip, SizeOf(ValueClip), #0);
  FOR i := 0 TO 255 DO
    ValueClip[i] := i;
  FOR i := 256 TO 1023 DO
    ValueClip[i] := 255;
  fYUY2TablesPrepared := true;
END;




procedure TVideoImage.I420_to_RGB(pData: pointer);
// http://en.wikipedia.org/wiki/YCbCr
VAR
  L, X, Y    : integer;
  ps         : pbyte;
  pY, pU, pV : pbyte;
begin
  pY := pData;
  PrepareTables;
  FOR Y := 0 TO fBitmap.Height-1 DO
    BEGIN
      ps := fBitmap.ScanLine[Y];

      pU := pData;
      Inc(pU, fBitmap.Width*(fBitmap.height+ Y div 4));
      pV := PU;
      Inc(pV, fBitmap.Width*fBitmap.height div 4);

      FOR X := 0 TO (fBitmap.Width div 2)-1 DO
        begin
          L := ValueY_298[pY^];
          ps^ := ValueClip[(L + ValueU_516[pU^]                  ) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          Inc(ps);
          Inc(pY);

          L := ValueY_298[pY^];
          ps^ := ValueClip[(L + ValueU_516[pU^]                     ) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          Inc(ps);
          Inc(pY);

          Inc(pU);
          Inc(pV);
        end;
    END;
end;


procedure TVideoImage.I420_to_Gray8Bit(pData: pointer);
// http://en.wikipedia.org/wiki/YCbCr
var
  Y  : integer;
  pY : pbyte;
begin
  pY := pData;
  FOR Y := 0 TO fBitmapGray.Height-1 DO
    begin
      move(pY^, fBitmapGray.ScanLine[Y]^, fBitmapGray.Width);
      Inc(pY, fBitmapGray.Width);
    end;
end;




procedure TVideoImage.YUY2_to_RGB(pData: pointer);
// http://msdn.microsoft.com/en-us/library/ms893078.aspx
// http://en.wikipedia.org/wiki/YCbCr
type
  TFour  = ARRAY[0..3] OF byte;
VAR
  L, X, Y : integer;
  ps      : pbyte;
  pf      : ^TFour;
begin
  pf := pData;
  PrepareTables;
  FOR Y := 0 TO fBitmap.Height-1 DO
    BEGIN
      ps := fBitmap.ScanLine[Y];
      FOR X := 0 TO (fBitmap.Width div 2)-1 DO
        begin
          L := ValueY_298[pf^[0]];
          ps^ := ValueClip[(L + ValueU_516[pf^[1]]                     ) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_100[pf^[1]] + ValueV_208[pf^[3]]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                      + ValueV_409[pf^[3]]) div 256];
          Inc(ps);

          L := ValueY_298[pf^[2]];
          ps^ := ValueClip[(L + ValueU_516[pf^[1]]                     ) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_100[pf^[1]] + ValueV_208[pf^[3]]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                      + ValueV_409[pf^[3]]) div 256];
          Inc(ps);

          Inc(pf);
        end;
    END;
end;



procedure TVideoImage.YUY2_to_Gray8Bit(pData: pointer);
// http://msdn.microsoft.com/en-us/library/ms893078.aspx
// http://en.wikipedia.org/wiki/YCbCr
type
  TFour  = ARRAY[0..3] OF byte;
VAR
  X, Y : integer;
  ps   : pbyte;
  pf   : ^byte;
begin
  pf := pData;
  FOR Y := 0 TO fBitmapGray.Height-1 DO
    BEGIN
      ps := fBitmapGray.ScanLine[Y];
      FOR X := 0 TO (fBitmapGray.Width div 2)-1 DO
        begin
          ps^ := pf^;
          Inc(ps);
          Inc(pf, 2);
          ps^ := pf^;
          Inc(ps);
          Inc(pf, 2);
        end;
    END;
end;



procedure TVideoImage.RGB_to_Gray8Bit(pData: pointer);
type
  TRGB       = ARRAY[0..5] OF byte;
  TPTRGB     = ^TRGB;
  TWordArr   = ARRAY[0..5759] OF word;
  TPTWordArr = ^TWordArr;
VAR
  X, Y : integer;
  p8   : TPTWordArr;
  pf   : TPTRGB;
begin
  pf := pData;

  FOR Y := fBitmapGray.height-1 DOWNTO 0 do
    begin
      p8  := fBitmapGray.ScanLine[Y];
      FOR X := 0 TO fBitmapGray.Width div 2-1 DO
        begin
          p8^[X] := ((GrayConvB[pf^[3]] + GrayConvG[pf^[4]] + GrayConvR[pf^[5]]) and $FF00) +
                     (GrayConvB[pf^[0]] + GrayConvG[pf^[1]] + GrayConvR[pf^[2]]) shr 8;
          Inc(pf);
        end;
    end;

end;



procedure TVideoImage.PaintFrame;
BEGIN
  // Paint FBitmap to fDisplayCanvas, if available
  if assigned(fDisplayCanvas) then
    begin
      IF not fImageUnpacked then
        UnpackFrame(fImagePtrSize[fImagePtrIndex], fImagePtr[fImagePtrIndex]);
      IF fDisplayCanvas.LockCount < 1 then
        begin
          fDisplayCanvas.lock;
          try
            IF fGray8Bit
              then fDisplayCanvas.Draw(0, 0, fBitmapGray)
              else fDisplayCanvas.Draw(0, 0, fBitmap);
          finally
            fDisplayCanvas.unlock;
          end;
        end;
    end;
END;



procedure TVideoImage.UnpackFrame(Size: integer; pData: pointer);
var
  {f       : file;}
  Unknown : boolean;
  FourCCSt: string[4];
begin
  IF pData = nil
    then exit;
  Unknown := false;
  try
    Case fFourCC OF
      0           :  BEGIN
                       IF (Size = fWidth*fHeight*3)
                         then begin
                           if fGray8Bit
                             then RGB_to_Gray8Bit(pData) // Okay, this is when Grayscale is much slower than color  :(
                             else move(pData^, FBitmap.scanline[fHeight-1]^, Size);
                         end
                         else Unknown := true;
                     END;
      FourCC_YUY2,
      FourCC_YUYV,
      FourCC_YUNV :  BEGIN
                       Unknown := (Size <> fWidth*fHeight*2);
                       IF Unknown then
                         begin
                           // Special treatment in case too much data is sent.
                           // e.g. Microsoft LifeCam Cinema delivers 1280*1080*2 Bytes
                           //      when 1280*720 was selected. The extra Bytes do not
                           //      contain video data. One third of the data (921600 Bytes)
                           //      is wasted by the driver!
                           if (Size > fWidth * fHeight * 2) then
                             Unknown := (Size div (2 * fWidth)) mod 4 <> 0;  // Width a multiple of 4? Maybe OK.
                         end;
                       IF not(Unknown) then
                         begin
                           IF fGray8Bit
                             then YUY2_to_Gray8Bit(pData)
                             else YUY2_to_RGB(pData);
                         end;
                     END;
      FourCC_MJPG :  BEGIN
                       try
                         MemStream.Clear;
                         MemStream.SetSize(Size);
                         MemStream.Position := 0;
                         MemStream.WriteBuffer(pData^, Size);
                         MemStream.Position := 0;
                         JPG.Grayscale := fGray8Bit;
                         JPG.LoadFromStream(MemStream);
                         if fGray8Bit
                           then FBitmapGray.Canvas.Draw(0, 0, JPG)
                           else FBitmap.Canvas.Draw(0, 0, JPG);
                       except
                         Unknown := true;
                       end;
                     END;
      FourCC_I420,
      FourCC_YV12,
      FourCC_IYUV : BEGIN
                      Unknown := (Size <> (fWidth*fHeight*3) div 2);
                      IF not Unknown then
                        IF fGray8Bit
                          then I420_to_Gray8Bit(pData)
                          else I420_to_RGB(pData);
                    END;
      else          BEGIN
                      {
                      assignfile(f, 'Unknown_Frame.dat');
                      rewrite(f, 1);
                      Blockwrite(f, pData^, Size);
                      closefile(f);
                      }
                      Unknown := true;
                    END;
    end; {case}

    IF Unknown then
      begin
        IF fFourCC = 0
          then FourCCSt := 'RGB'
          else begin
            FourCCSt := '    ';
            move(fFourCC, FourCCSt[1], 4);
          end;
        FBitmap.Canvas.TextOut(0,  0, 'Unknown compression');
        FBitmap.Canvas.TextOut(0, FBitmap.Canvas.TextHeight('X'), 'DataSize: '+INtToStr(Size)+'  FourCC: '+FourCCSt);
      end;

    fImageUnpacked := true;
  except
  end;
end;



procedure TVideoImage.GetBitmap(BMP: TBitmap);
begin
  IF not fImageUnpacked then
    UnpackFrame(fImagePtrSize[fImagePtrIndex], fImagePtr[fImagePtrIndex]);
  if fGray8Bit
    then BMP.Assign(fBitmapGray)
    else BMP.Assign(fBitmap);
  (*
  BMP.PixelFormat := pf24bit;
  BMP.Width := fBitmap.Width;
  BMP.Height := fBitmap.Height;
  move(fBitmap.ScanLine[fBitmap.Height-1]^, BMP.ScanLine[BMP.height-1]^, BMP.Height*BMP.Width*3);
  //BMP.Canvas.Draw(0, 0, fBitmap);
  *)
end;



procedure TVideoImage.SetDisplayCanvas(Canvas: TCanvas);
begin
  fDisplayCanvas := Canvas;
end;



procedure TVideoImage.ShowProperty;
begin
  VideoSample.ShowPropertyDialog;
end;



procedure TVideoImage.ShowProperty_Stream;
var
  hr     : HResult;
  W, H   : integer;
  FourCC : cardinal;
begin
  VideoSample.ShowPropertyDialog_CaptureStream;
  hr := VideoSample.GetStreamInfo(W, H, FourCC);
  IF Failed(HR)
    then begin
      VideoStop;
    end
    else BEGIN
      fWidth := W;
      fHeight := H;
      fFourCC := FourCC;
      FBitmap.PixelFormat := pf24bit;
      FBitmap.Width := W;
      FBitmap.Height := H;
      PrepareGrayBMP(FBitmapGray, W, H);
      VideoSample.SetCallBack(CallBack);
    END;
end;



FUNCTION  TVideoImage.ShowVfWCaptureDlg: HResult;
begin
  Result := VideoSample.ShowVfWCaptureDlg;
end;



procedure TVideoImage.GetBrightnessSettings(VAR Actual: integer);
begin
//  VideoSample.GetVideoPropAmp(VideoProcAmp_Brightness, Actual)
end;



procedure TVideoImage.SetBrightnessSettings(const Actual: integer);
begin
//  VideoSample.SetVideoPropAmp(VideoProcAmp_Brightness, Actual);
end;


PROCEDURE TVideoImage.GetListOfSupportedVideoSizes(VidSize: TStringList);
BEGIN
  VideoSample.GetListOfVideoSizes(VidSize);
END;


PROCEDURE TVideoImage.SetResolutionByIndex(Index: integer);
VAR
  hr     : HResult;
  W, H   : integer;
  FourCC : cardinal;
BEGIN
  VideoSample.SetVideoSizeByListIndex(Index);
  hr := VideoSample.GetStreamInfo(W, H, FourCC);
  IF Succeeded(HR)
    then begin
      fWidth := W;
      fHeight := H;
      fFourCC := FourCC;
      FBitmap.PixelFormat := pf24bit;
      FBitmap.Width := W;
      FBitmap.Height := H;
      PrepareGrayBMP(FBitmapGray, W, H);
    END;
END;


end.
