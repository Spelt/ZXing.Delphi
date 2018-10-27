unit VSample;

(******************************************************************************

  VSample.pas
  Class TVideoSample

About
  The TVideoSample class provides access to WebCams and similar Video-capture
  devices via DirectShow.
  It is based mainly on C++ examples from the Microsoft DirectX 9.0 SDK Update
  (Summer 2003): PlayCap and PlayCapMoniker. Comments found in those samples
  are copied into this Delphi code.

  Depends on the DirectX Header conversion files which could be found here:
  - http://www.progdigy.com
  - http://www.clootie.ru/delphi

History
  Version 1.22
  2012-07-08 (Fixed some memory leaks. List of supported video sizes/compressions corrected)
  Version 1.21
  06.05.2012  (ansichar instead of char)
  Version 1.2
  23.08.2009
  Version 1.1
  07.09.2008
  Version 1.03
  30.08.2008
  Version 1.02
  26.07.2008
  Version 1.01
  03.05.2008
  Version 1.0
  16.01.2006

Contact:
  michael@grizzlymotion.com

Copyright
  Portions created by Microsoft are Copyright (C) Microsoft Corporation.
  Original file names: PlayCap.cpp, PlayCapMoniker.cpp.
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




USES Windows, Messages, SysUtils, Classes, ActiveX, Forms,
     {$ifdef DXErr} DXErr9, {$endif}
     DirectShow9;


{ $ define REGISTER_FILTERGRAPH}


CONST
  WM_GRAPHNOTIFY = WM_APP+1;
  WM_NewFrame    = WM_User+2;   // Used to inform application that a new video
                                // frame has arrived. Necessary only, if
                                // application hasn't defined a callback
                                // routine via TVideoSample.SetCallBack(...).


CONST  { Copied from OLE2.pas }
  {$EXTERNALSYM IID_IUnknown}
  IID_IUnknown: TGUID = (
    D1:$00000000;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));



TYPE
  TPLAYSTATE      = (PS_Stopped,
                     {PS_Init,}
                     PS_Paused,
                     PS_Running);




// ---= Pseudo-Interface for Frame Grabber Callback Routines =-------------
// c.f. Delphi Help text "Delegating to a class-type property"
//
// ISampleGrabber.SetCallback verlangt als ersten Parameter ein "ISampleGrabberCB"
// Um für ein solches Interface Routinen zu deklarieren ist scheinbar das
// folgende, sonderbare Konstrukt nötig.
//
// ISampleGrabber.SetCallback needs an "ISampleGrabberCB" as first parameter.
// This is my attempt to build such a thing with Delphi.

TYPE
  TVideoSampleCallBack= procedure(pb : pbytearray; var Size: integer) of object;
  TSampleGrabberCBInt = interface(ISampleGrabberCB)
                          function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
                          function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
                        end;
  TSampleGrabberCBImpl= class
                          CallBack    : TVideoSampleCallBack;
                          function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
                          function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
                        end;
  TSampleGrabberCB =    class(TInterfacedObject, TSampleGrabberCBInt)
                          FSampleGrabberCB: TSampleGrabberCBImpl;
                          CallBack    : TVideoSampleCallBack;
                          property SampleGrabberCB: TSampleGrabberCBImpl read FSampleGrabberCB implements TSampleGrabberCBInt;
                        end;


  TFormatInfo   = RECORD
                    Width,
                    Height : integer;
                    SSize  : cardinal;
                    OIndex : integer;
                    mt     : TAMMediaType;
                    FourCC : ARRAY[0..3] OF ansichar;  // ansichar, because in Delphi 2009 char is something different ;)
                  END;

  TVideoSample  = class(TObject)
                    private
                      ghApp             : HWND;
                      pIVideoWindow     : IVideoWindow;
                      pIMediaControl    : IMediaControl;
                      pIMediaEventEx    : IMediaEventEx;
                      pIGraphBuilder    : IGraphBuilder;
                      pICapGraphBuild2  : ICaptureGraphBuilder2;
                      g_psCurrent       : TPLAYSTATE;

                      pIAMStreamConfig  : IAMStreamConfig;
                      piBFSampleGrabber : IBaseFilter;
                      pIAMVideoProcAmp  : IAMVideoProcAmp;
                      pIBFNullRenderer  : IBaseFilter;

                      pIKsPropertySet   : IKsPropertySet;
                      pISampleGrabber   : ISampleGrabber;
                      pIBFVideoSource   : IBaseFilter;

                      {$ifdef REGISTER_FILTERGRAPH}
                        g_dwGraphRegister :DWORD;
                      {$endif}

                      SGrabberCB  : TSampleGrabberCB;
                      _SGrabberCB : TSampleGrabberCBInt;
                      fVisible    : boolean;
                      CallBack    : TVideoSampleCallBack;
                      FormatArr   : ARRAY OF TFormatInfo;
                      FUNCTION    GetInterfaces(ForceRGB: boolean; WhichMethodToCallback: integer): HRESULT;
                      FUNCTION    SetupVideoWindow(): HRESULT;
                      FUNCTION    ConnectToCaptureDevice(DeviceName: string; VAR DeviceSelected: string; VAR ppIBFVideoSource: IBaseFilter): HRESULT;
                      FUNCTION    RestartVideoEx(Visible: boolean):HRESULT;
                      FUNCTION    ShowPropertyDialogEx(const IBF: IUnknown; FilterName:  PWideChar): HResult;
                      FUNCTION    LoadListOfResolution: HResult;
                      procedure   DeleteBelow(const IBF: IBaseFilter);
                      procedure   CloseInterfaces;
                    public
                      {$ifdef DXErr}
                        DXErrString: string;  // for debugging
                      {$endif}
                      constructor Create(VideoCanvasHandle: THandle; ForceRGB: boolean; WhichMethodToCallback: integer; VAR HR: HResult);
                      destructor  Destroy; override;
                      property    PlayState: TPLAYSTATE read g_psCurrent;
                      procedure   ResizeVideoWindow();
                      FUNCTION    RestartVideo:HRESULT;
                      FUNCTION    StartVideo(CaptureDeviceName: string; Visible: boolean; VAR DeviceSelected: string):HRESULT;
                      FUNCTION    PauseVideo: HResult;  // Pause running video
                      FUNCTION    ResumeVideo: HResult; // Re-start paused video
                      FUNCTION    StopVideo: HResult;
                      function    GetImageBuffer(VAR pb : pbytearray; var Size: integer): HResult;
                      FUNCTION    SetPreviewState(nShow: boolean): HRESULT;
                      FUNCTION    ShowPropertyDialog: HResult;
                      FUNCTION    ShowPropertyDialog_CaptureStream: HResult;
                      FUNCTION    GetVideoPropAmpEx(    Prop           : TVideoProcAmpProperty;
                                                    VAR pMin, pMax,
                                                        pSteppingDelta,
                                                        pDefault       : longint;
                                                    VAR pCapsFlags     : TVideoProcAmpFlags;
                                                    VAR pActual        : longint): HResult;
                      FUNCTION    SetVideoPropAmpEx(    Prop           : TVideoProcAmpProperty;
                                                        pCapsFlags     : TVideoProcAmpFlags;
                                                        pActual        : longint): HResult;
                      PROCEDURE   GetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; VAR AcPerCent: integer);
                      PROCEDURE   SetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; AcPerCent: integer);
                      PROCEDURE   GetVideoSize(VAR Width, height: integer);
                      FUNCTION    ShowVfWCaptureDlg: HResult;
                      FUNCTION    GetStreamInfo(VAR Width, Height: integer; VAR FourCC: dword): HResult;
                      FUNCTION    GetExProp(    guidPropSet   : TGuiD;
                                                dwPropID      : TAMPropertyPin;
                                                pInstanceData : pointer;
                                                cbInstanceData: DWORD;
                                            out pPropData;
                                                cbPropData    : DWORD;
                                            out pcbReturned   : DWORD): HResult;
                      FUNCTION    SetExProp(   guidPropSet : TGuiD;
                                                  dwPropID : TAMPropertyPin;
                                            pInstanceData  : pointer;
                                            cbInstanceData : DWORD;
                                                 pPropData : pointer;
                                                cbPropData : DWORD): HResult;
                      FUNCTION    GetCaptureIAMStreamConfig(VAR pSC: IAMStreamConfig): HResult;
                      PROCEDURE   DeleteCaptureGraph;
                      PROCEDURE   SetCallBack(CB: TVideoSampleCallBack);
                      FUNCTION    GetPlayState: TPlayState;  // Deprecated
                      PROCEDURE   GetListOfVideoSizes(VidSize: TStringList);
                      FUNCTION    SetVideoSizeByListIndex(ListIndex: integer): HResult;
                      {$ifdef REGISTER_FILTERGRAPH}
                        FUNCTION AddGraphToRot(pUnkGraph: IUnknown; VAR pdwRegister: DWORD):HRESULT;
                        procedure RemoveGraphFromRot(pdwRegister: dword);
                      {$endif}
                  END;



FUNCTION TGUIDEqual(const TG1, TG2 : TGUID): boolean;

FUNCTION GetCaptureDeviceList(VAR SL: TStringList): HResult;



implementation



FUNCTION TGUIDEqual(const TG1, TG2 : TGUID): boolean;
BEGIN
  Result := CompareMem(@TG1, @TG2, SizeOf(TGUID));
END; {TGUIDEqual}


{ Get a list of all capture devices installed }
FUNCTION GetCaptureDeviceList(VAR SL: TStringList): HResult;
VAR
  pDevEnum     : ICreateDevEnum;
  pClassEnum   : IEnumMoniker;
  st           : string;

          // Okay, in the original C code from the microsoft samples this
          // is not a subroutine.
          // I decided to use it as a subroutine, because Delphi won't let
          // me free pMoniker or pPropertyBag myself. ( ":= nil" )
          // Hopefully ending the subroutine will clean up all instances of
          // these interfaces automatically...
          FUNCTION GetNextDeviceName(VAR Name: string): boolean;
          VAR
            pMoniker     : IMoniker;
            pPropertyBag : IPropertyBag;
            v            : OLEvariant;
            cFetched     : ulong;
          BEGIN
            Result := false;
            Name   := '';
            pMoniker := nil;
            IF (S_OK = (pClassEnum.Next (1, pMoniker, @cFetched))) THEN
              BEGIN
                pPropertyBag := nil;
                if S_OK = pMoniker.BindToStorage(nil, nil, IPropertyBag, pPropertyBag) then
                  begin
                    if S_OK = pPropertyBag.Read('FriendlyName', v, nil) then
                      begin
                        Name := v;
                        Result := true;
                      end;
                  end;
              END;
          END; {GetNextDeviceName}

begin
  Result := S_FALSE;
  if not(assigned(SL)) then
    SL := TStringlist.Create;
  try
    SL.Clear;
  except
    exit;
  end;

  // Create the system device enumerator
  Result := CoCreateInstance (CLSID_SystemDeviceEnum,
                              nil,
                              CLSCTX_INPROC_SERVER,
                              IID_ICreateDevEnum,
                              pDevEnum);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      // Couldn't create system enumerator!
      exit;
    end;

  // Create an enumerator for the video capture devices
  pClassEnum := nil;

  Result := pDevEnum.CreateClassEnumerator (CLSID_VideoInputDeviceCategory, pClassEnum, 0);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      // Couldn't create class enumerator!
      exit;
    end;

  // If there are no enumerators for the requested type, then
  // CreateClassEnumerator will succeed, but pClassEnum will be nil.
  if (pClassEnum = nil) then
    begin
       // No video capture device was detected.
       exit;
    end;

  WHILE GetNextDeviceName(st) DO
    SL.Add(st);
end; {GetCaptureDeviceList}





// ---= Sample Grabber callback routines =------------------------------------


// In routine TVideoSample.GetInterfaces(..) the callback routine is defined
// with pISampleGrabber.SetCallback(..,..). If the second parameter in that
// call is 1, then the routine below is called during a callback.
// Otherwise, if the parameter is 0, callback routine BufferCB would be called.
function TSampleGrabberCBImpl.SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
var
  BufferLen: integer;
  ppBuffer : pbyte;
begin
  BufferLen := pSample.GetSize;
  if BufferLen > 0 then
    begin
      pSample.GetPointer(ppBuffer); {*}
      if @CallBack = nil
        then SendMessage(Application.Mainform.handle, WM_NewFrame, BufferLen, integer(ppBuffer))
        else Callback(pbytearray(ppBuffer), BufferLen);
    end;
  Result := 0;
end;

{*}
// Nebenbei bemerkt: Beim Debuggen fiel mir auf, daß die von mir verwendete
// WebCam scheinbar einen Triple-Buffer für die Bilddaten verwendet. Die oben
// von pSample.GetPointer(ppBuffer) zurückgelieferte Adresse wiederholt sich
// in einem 3-er Zyklus. Wenn das ein Feature von DirectShow ist und nicht
// von der Kamera-Steuersoftware, dann könnte man selbst auf Double- oder
// Triplebuffering verzichten. 


// In routine TVideoSample.GetInterfaces(..) the callback routine is defined
// with pISampleGrabber.SetCallback(..,..). If the second parameter in that
// call is 0, then the routine below is called during a callback.
// Otherwise, if the parameter is 1, callback routine SampleCB would be called.
function TSampleGrabberCBImpl.BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
begin
  if BufferLen > 0 then
    begin
      if @CallBack = nil
        then SendMessage(Application.Mainform.handle, WM_NewFrame, BufferLen, integer(pBuffer))
        else Callback(pbytearray(pBuffer), BufferLen);
    end;
  Result := 0;
end;


// ---= End of Sample Grabber callback routines =---------------------------






constructor TVideoSample.Create(VideoCanvasHandle: THandle; ForceRGB: boolean; WhichMethodToCallback: integer; VAR HR: HResult);
begin
  ghApp             := 0;

  pIVideoWindow     := nil;
  pIMediaControl    := nil;
  pIMediaEventEx    := nil;
  pIGraphBuilder    := nil;
  pICapGraphBuild2  := nil;
  g_pSCurrent       := PS_Stopped;

  pIAMStreamConfig  := nil;
  piBFSampleGrabber := nil;
  pIAMVideoProcAmp  := nil;

  pIKsPropertySet   := nil;

  {$ifdef REGISTER_FILTERGRAPH}
  g_dwGraphRegister:=0;
  {$endif}

  pISampleGrabber   := nil;
  pIBFVideoSource   := nil;
  SGrabberCB        := nil;
  _SGrabberCB       := nil;
  pIBFNullRenderer  := nil;

  CallBack          := nil;

  inherited create;

  ghApp             := VideoCanvasHandle;

  HR                := GetInterfaces(ForceRGB, WhichMethodToCallback);
end;




FUNCTION TVideoSample.GetInterfaces(ForceRGB: boolean; WhichMethodToCallback: integer): HRESULT;
VAR
  MT: _AMMediaType;
BEGIN
  //--- Create the filter graph
  Result := CoCreateInstance(CLSID_FilterGraph,
                             nil,
                             CLSCTX_INPROC,
                             IID_IGraphBuilder,
                             pIGraphBuilder);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
   exit;

  //--- Create Sample grabber
  Result := CoCreateInstance(CLSID_SampleGrabber,
                             nil,
                             CLSCTX_INPROC_SERVER,
                             IBaseFilter,
                             piBFSampleGrabber);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC_SERVER,
                             IID_IBaseFilter, pIBFNullRenderer);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := piBFSampleGrabber.QueryInterface(IID_ISampleGrabber, pISampleGrabber);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  pISampleGrabber.SetBufferSamples(false);  // No buffering required in this demo

  //--- Force 24bit color depth. (RGB24 erzwingen)
  IF ForceRGB then
    begin
      FillChar(MT, sizeOf(MT), #0);
      MT.majortype := MediaType_Video;
      MT.subtype := MediaSubType_RGB24;
      Result := pISampleGrabber.SetMediaType(MT);
      {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
      if (FAILED(Result)) then
        exit;
    end;

  //--- Prepare Sample-Grabber Callback Object----
  if not assigned(SGrabberCB) then
    begin
      SGrabberCB := TSampleGrabberCB.Create;
      TSampleGrabberCB(SGrabberCB).FSampleGrabberCB := TSampleGrabberCBImpl.Create;
      _SGrabberCB := TSampleGrabberCB(SGrabberCB);
         // Should this be _SGrabberCB := SGrabberCB as TSampleGrabberCB ?????!!!!!
         // Compare discussion on
         // http://delphi.newswhat.com/geoxml/forumgetthread?groupname=borland.public.delphi.oodesign&messageid=44f84705@newsgroups.borland.com&displaymode=all
         // However, link has been lost in the web  :(
    end;

  pISampleGrabber.SetCallback(ISampleGrabberCB(_SGrabberCB), WhichMethodToCallback);
         // WhichMethodToCallback=0: SampleGrabber calls SampleCB with the original media sample
         // WhichMethodToCallback=1: SampleGrabber calls BufferCB with a copy of the media sample

  //--- Create the capture graph builder
  Result := CoCreateInstance(CLSID_CaptureGraphBuilder2,
                             nil,
                             CLSCTX_INPROC,
                             IID_ICaptureGraphBuilder2,
                             pICapGraphBuild2);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  // Obtain interfaces for media control and Video Window
  Result := pIGraphBuilder.QueryInterface(IID_IMediaControl, pIMediaControl);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := pIGraphBuilder.QueryInterface(IID_IVideoWindow, pIVideoWindow);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  Result := pIGraphBuilder.QueryInterface(IID_IMediaEvent, pIMediaEventEx);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    exit;

  //--- Set the window handle used to process graph events
  Result := pIMediaEventEx.SetNotifyWindow(OAHWND(ghApp), WM_GRAPHNOTIFY, 0);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
end;




FUNCTION TVideoSample.ConnectToCaptureDevice(DeviceName: string; VAR DeviceSelected: string; VAR ppIBFVideoSource: IBaseFilter): HRESULT;
VAR
  pDevEnum   : ICreateDevEnum;
  pClassEnum : IEnumMoniker;
  Index      : integer;
  Found      : boolean;


          // see also: http://msdn.microsoft.com/en-us/library/ms787619.aspx
          FUNCTION CheckNextDeviceName(Name: string; VAR Found: boolean): HResult;
          VAR
            pMoniker     : IMoniker;
            pPropertyBag : IPropertyBag;
            v            : OLEvariant;
            cFetched     : ulong;
            MonName      : string;
          BEGIN
            Found  := false;
            pMoniker := nil;
            // Note that if the Next() call succeeds but there are no monikers,
            // it will return S_FALSE (which is not a failure).  Therefore, we
            // check that the return code is S_OK instead of using SUCCEEDED() macro.
            Result := pClassEnum.Next(1, pMoniker, @cFetched);
            IF (S_OK = Result) THEN
              BEGIN
                Inc(Index);
                pPropertyBag := nil;
                Result := pMoniker.BindToStorage(nil, nil, IPropertyBag, pPropertyBag);
                if S_OK = Result then
                  begin
                    Result := pPropertyBag.Read('FriendlyName', v, nil);   // BTW: Other useful parameter: 'DevicePath'
                    if S_OK = Result then
                      begin
                        MonName := v;
                        if (Uppercase(Trim(MonName)) = UpperCase(Trim(Name))) or
                          ((Length(Name)=2) and (Name[1]='#') and (ord(Name[2])-48=Index)) then
                          begin
                            DeviceSelected := Trim(MonName);
                            Result := pMoniker.BindToObject(nil, nil, IID_IBaseFilter, ppIBFVideoSource);
                            Found := Result = S_OK;
                          end;
                      end;
                  end;
              END;
          END; {CheckNextDeviceName}



BEGIN
  DeviceSelected := '';
  Index := 0;
  DeviceName := Trim(DeviceName);
  IF DeviceName = '' then
    DeviceName := '#1'; // Default: First device (Erstes Gerät)

  if @ppIBFVideoSource = nil then
    begin
      result := E_POINTER;
      exit;
    end;

  // Create the system device enumerator
  Result := CoCreateInstance(CLSID_SystemDeviceEnum,
                             nil,
                             CLSCTX_INPROC,
                             IID_ICreateDevEnum,
                             pDevEnum);
  if (FAILED(Result)) then
    begin
      // Couldn't create system enumerator!
      exit;
    end;

  // Create an enumerator for the video capture devices
  pClassEnum := nil;

  Result := pDevEnum.CreateClassEnumerator (CLSID_VideoInputDeviceCategory, pClassEnum, 0);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      // Couldn't create class enumerator!
      exit;
    end;

  // If there are no enumerators for the requested type, then
  // CreateClassEnumerator will succeed, but pClassEnum will be nil.
  if (pClassEnum = nil) then
    begin
      // No video capture device was detected.
      result := E_FAIL;
      exit;
    end;

  Found := false;
  REPEAT
    try
      Result := CheckNextDeviceName(DeviceName, Found)
    except
      IF Result = 0 then
        result := E_FAIL;
    end;
  UNTIL Found or (Result <> S_OK);
end; {ConnectToCaptureDevice}





procedure TVideoSample.ResizeVideoWindow();
var
  rc : TRect;
begin
  // Resize the video preview window to match owner window size
  if (pIVideoWindow) <> nil then
    begin
        // Make the preview video fill our window
      GetClientRect(ghApp, rc);
      pIVideoWindow.SetWindowPosition(0, 0, rc.right, rc.bottom);
    end;
end; {ResizeVideoWindow}




FUNCTION TVideoSample.SetupVideoWindow(): HRESULT;
BEGIN
  // Set the video window to be a child of the main window
  Result := pIVideoWindow.put_Owner(OAHWND(ghApp));
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      exit;
    end;

  // Set video window style
  Result := pIVideoWindow.put_WindowStyle(WS_CHILD or WS_CLIPCHILDREN);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      exit;
    end;

  // Use helper function to position video window in client rect
  // of main application window
  ResizeVideoWindow();

  // Make the video window visible, now that it is properly positioned
  Result := pIVideoWindow.put_Visible(TRUE);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if (FAILED(Result)) then
    begin
      exit;
    end;

end; {SetupVideoWindow}




FUNCTION TVideoSample.RestartVideoEx(Visible: boolean):HRESULT;
VAR
  pCut, pTyp : pGuiD;
  {
  pAMVidControl: IAMVideoControl;
  pPin         : IPin;
  }
BEGIN
  if (pIAMVideoProcAmp = nil) then
    if not(S_OK = pIBFVideoSource.QueryInterface(IID_IAMVideoProcAmp, pIAMVideoProcAmp)) then
      pIAMVideoProcAmp := nil;

   if (pIKsPropertySet = nil) then
    if not(S_OK = pIBFVideoSource.QueryInterface(IID_IKsPropertySet, pIKsPropertySet)) then
      pIKsPropertySet := nil;


    // Add Capture filter to our graph.
    Result := pIGraphBuilder.AddFilter(pIBFVideoSource, Widestring('Video Capture'));
    if (FAILED(Result)) then
      begin
        // Couldn''t add the capture filter to the graph!
        exit;
      end;

    Result := pIGraphBuilder.AddFilter(piBFSampleGrabber, Widestring('Sample Grabber'));
    if (FAILED(Result)) then
      EXIT;

    if not(Visible) then
      begin
        Result := pIGraphBuilder.AddFilter(pIBFNullRenderer, WideString('Null Renderer'));
        if (FAILED(Result)) then
          EXIT;
      end;

    // Render the preview pin on the video capture filter
    // Use this instead of pIGraphBuilder->RenderFile
    New(pCut);
    New(pTyp);
    //pCut^ := PIN_CATEGORY_PREVIEW;
    pCut^ := PIN_CATEGORY_CAPTURE;
    pTyp^ := MEDIATYPE_Video;
    try
      if Visible
        then Result := pICapGraphBuild2.RenderStream (pCut, pTyp,
                                    //Addr(PIN_CATEGORY_PREVIEW), Addr(MEDIATYPE_Video),
                                    pIBFVideoSource, piBFSampleGrabber, nil)

        else Result := pICapGraphBuild2.RenderStream (pCut, pTyp,
                                    //Addr(PIN_CATEGORY_PREVIEW), Addr(MEDIATYPE_Video),
                                    pIBFVideoSource, piBFSampleGrabber, pIBFNullRenderer);
    except
      Result := -1;
    end;
    if (FAILED(Result)) then
      begin
        // Couldn''t render the video capture stream.
        // The capture device may already be in use by another application.
        Dispose(pTyp);
        Dispose(pCut);
        exit;
      end;


    // Set video window style and position
    if Visible then
      begin
        Result := SetupVideoWindow();
        if (FAILED(Result)) then
          begin
            // Couldn't initialize video window!
            Dispose(pTyp);
            Dispose(pCut);
            exit;
          end;
      end;

{$ifdef REGISTER_FILTERGRAPH}
    // Add our graph to the running object table, which will allow
    // the GraphEdit application to "spy" on our graph
    try
      hr := AddGraphToRot(IUnknown(pIGraphBuilder), g_dwGraphRegister);
    except
      // Failed to register filter graph with ROT!
    end;
    if (FAILED(Result)) then
      begin
        // Failed to register filter graph with ROT!
        g_dwGraphRegister := 0;
      end;
{$endif}

  //  if Visible then
      begin
        // Start previewing video data
        Result := pIMediaControl.Run();
        if (FAILED(Result)) then
          begin
            // Couldn't run the graph!
          end;
      end;

    // Remember current state
    g_psCurrent := PS_Running;

    (*
    // !!!!!!!!!
    // Prepare getting images in higher resolution than video stream
    // See DirectX9 Help "Capturing an Image From a Still Image Pin"
    // Not working yet.....
    pAMVidControl := nil;
    Result := pIBFVideoSource.QueryInterface(IID_IAMVideoControl, pAMVidControl);
    IF succeeded(Result) then
      begin
        pTyp := 0;
        pPin := nil;
        Result := pICapGraphBuild2.FindPin(pIBFVideoSource, PINDIR_OUTPUT, PIN_CATEGORY_STILL, pTyp^, false, 0, pPin);
        if (SUCCEEDED(Result)) then
          Result := pAMVidControl.SetMode(pPin, VideoControlFlag_Trigger);
      end;
    *)
  Dispose(pTyp);
  Dispose(pCut);
end; {RestartVideoEx}


FUNCTION TVideoSample.RestartVideo: HRESULT;
BEGIN
  Result := RestartVideoEx(FVisible);
END; {RestartVideo}


FUNCTION TVideoSample.StartVideo(CaptureDeviceName: string; Visible: boolean; VAR DeviceSelected: string):HRESULT;
BEGIN
  pIBFVideoSource := nil;
  FVisible   := Visible;

   // Attach the filter graph to the capture graph
  Result := pICapGraphBuild2.SetFiltergraph(pIGraphBuilder);
  if (FAILED(Result)) then
    begin
      // Failed to set capture filter graph!
      exit;
    end;

  // Use the system device enumerator and class enumerator to find
  // a video capture/preview device, such as a desktop USB video camera.
  Result := ConnectToCaptureDevice(CaptureDeviceName, DeviceSelected, pIBFVideoSource);
  if (FAILED(Result)) then
    begin
      exit;
    end;

  LoadListOfResolution;
  Result := RestartVideo;
end;



FUNCTION TVideoSample.PauseVideo: HResult;
BEGIN
  IF g_psCurrent = PS_Paused
    then begin
      Result := S_OK;
      EXIT;
    end;
  IF g_psCurrent = PS_Running then
    begin
      Result := pIMediaControl.Pause;
      if Succeeded(Result) then
        g_psCurrent := PS_Paused;
    end
    else Result := S_FALSE;
END;


FUNCTION TVideoSample.ResumeVideo: HResult;
BEGIN
  IF g_psCurrent = PS_Running then
    begin
      Result := S_OK;
      EXIT;
    end;
  IF g_psCurrent = PS_Paused then
    begin
      Result := pIMediaControl.Run;
      if Succeeded(Result) then
        g_psCurrent := PS_Running;
    end
    else Result := S_FALSE;
END;



FUNCTION TVideoSample.StopVideo: HResult;
BEGIN
  // Stop previewing video data
  Result := pIMediaControl.StopWhenReady();
  g_psCurrent := PS_Stopped;
  SetLength(FormatArr, 0);
END;



// Delete filter and pins bottom-up...
PROCEDURE TVideoSample.DeleteBelow(const IBF: IBaseFilter);
VAR
  hr         : HResult;
  pins       : IEnumPins;
  pIPinFrom,
  pIPinTo    : IPin;
  fetched    : ulong;
  pInfo      : _PinInfo;
BEGIN
  pIPinFrom := nil;
  pIPinTo   := nil;
  hr := IBF.EnumPins(pins);
  WHILE (hr = NoError) DO
    BEGIN
      hr := pins.Next(1, pIPinFrom, @fetched);
      if (hr = S_OK) and (pIPinFrom <> nil) then
        BEGIN
          hr := pIPinFrom.ConnectedTo(pIPinTo);
          if (hr = S_OK) and (pIPinTo <> nil) then
            BEGIN
              hr := pIPinTo.QueryPinInfo(pInfo);
              if (hr = NoError) then
                BEGIN
                  if pinfo.dir = PINDIR_INPUT then
                    BEGIN
                      DeleteBelow(pInfo.pFilter);
                      pIGraphBuilder.Disconnect(pIPinTo);
                      pIGraphBuilder.Disconnect(pIPinFrom);
                      pIGraphBuilder.RemoveFilter(pInfo.pFilter);
                    ENd;
                END;
            END;
        END;
    END;
END; {DeleteBelow}



PROCEDURE TVideoSample.DeleteCaptureGraph;
BEGIN
  pIBFVideoSource.Stop;
  DeleteBelow(pIBFVideoSource);
END;



procedure TVideoSample.CloseInterfaces;
begin
  if (pISampleGrabber <> nil) then
    pISampleGrabber.SetCallback(nil, 1);

  // Stop previewing data
  if (pIMediaControl <> nil) then
    pIMediaControl.StopWhenReady();

  g_psCurrent := PS_Stopped;

  // Stop receiving events
  if (pIMediaEventEx <> nil) then
    pIMediaEventEx.SetNotifyWindow(OAHWND(nil), WM_GRAPHNOTIFY, 0);

  // Relinquish ownership (IMPORTANT!) of the video window.
  // Failing to call put_Owner can lead to assert failures within
  // the video renderer, as it still assumes that it has a valid
  // parent window.
  if (pIVideoWindow<>nil) then
    begin
      pIVideoWindow.put_Visible(FALSE);
      pIVideoWindow.put_Owner(OAHWND(nil));
    end;

  {$ifdef REGISTER_FILTERGRAPH}
    // Remove filter graph from the running object table
    if (g_dwGraphRegister<>nil) then
      RemoveGraphFromRot(g_dwGraphRegister);
  {$endif}
end;



function TVideoSample.GetImageBuffer(VAR pb : pbytearray; var Size: integer): HResult;
VAR
  NewSize : integer;
begin
  Result := pISampleGrabber.GetCurrentBuffer(NewSize, nil);
  if (Result <> S_OK) then
    EXIT;
  if (pb <> nil) then
    begin
      if Size <> NewSize then
        begin
          try
            FreeMem(pb, Size);
          except
          end;
          pb := nil;
          Size := 0;
        end;
    end;
  Size := NewSize;
  IF Result = S_OK THEN
    BEGIN
      if pb = nil then
        GetMem(pb, NewSize);
      Result := pISampleGrabber.GetCurrentBuffer(NewSize, pb);
    END;
end;



FUNCTION TVideoSample.SetPreviewState(nShow: boolean): HRESULT;
BEGIN
  Result := S_OK;

  // If the media control interface isn't ready, don't call it
  if (pIMediaControl = nil) then
    exit;

  if (nShow) then
    begin
      if (g_psCurrent <> PS_Running) then
        begin
          // Start previewing video data
          Result := pIMediaControl.Run();
          g_psCurrent := PS_Running;
        end;
    end
    else begin
        // Stop previewing video data
        // Result := pIMediaControl.StopWhenReady(); // Program may get stucked here!
        Result := pIMediaControl.Stop;
        g_psCurrent := PS_Stopped;
    end;
end;




FUNCTION TVideoSample.ShowPropertyDialogEx(const IBF: IUnknown; FilterName: PWideChar): HResult;
VAR
  pProp      : ISpecifyPropertyPages;
  c          : tagCAUUID;
begin
 pProp  := nil;
 Result := IBF.QueryInterface(ISpecifyPropertyPages, pProp);
 if Result = S_OK then
   begin
     Result := pProp.GetPages(c);
     if (Result = S_OK) and (c.cElems > 0) then
       begin
         Result := OleCreatePropertyFrame(ghApp, 0, 0, FilterName, 1, @IBF, c.cElems, c.pElems, 0, 0, nil);
         CoTaskMemFree(c.pElems);
       end;
   end;
end;





FUNCTION TVideoSample.ShowPropertyDialog: HResult;
VAR
  FilterInfo : FILTER_INFO;
begin
  Result := pIBFVideoSource.QueryFilterInfo(FilterInfo);
  if not(Failed(Result)) then
    Result := ShowPropertyDialogEx(pIBFVideoSource, FilterInfo.achName);
end;



FUNCTION TVideoSample.GetCaptureIAMStreamConfig(VAR pSC: IAMStreamConfig): HResult;
BEGIN
  pSC := nil;
  Result := pICapGraphBuild2.FindInterface(@PIN_CATEGORY_capture,
                                           @MEDIATYPE_Video,
                                           pIBFVideoSource,
                                           IID_IAMStreamConfig, pSC);

END;



FUNCTION TVideoSample.ShowPropertyDialog_CaptureStream: HResult;
VAR
  pSC       : IAMStreamConfig;
BEGIN
  pIMediaControl.Stop;
  Result := GetCaptureIAMStreamConfig(pSC);
  if Result = S_OK then
    Result := ShowPropertyDialogEx(pSC, '');
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  pIMediaControl.Run;
END;


(*
PROCEDURE DumpMediaType(const mt: TAMMediaType; VAR Dump: TStringList);
begin
  Dump.Add('================');
  Dump.Add('MajorType=' + GuidToString(mt.majortype));
  Dump.Add('SubType=' +   GuidToString(mt.subtype));
  Dump.Add('FixedSizeSamples=' + BoolToStr(mt.bFixedSizeSamples));
  Dump.Add('TemporalCompression=' + BoolToStr(mt.bTemporalCompression));
  Dump.Add('lSampleSize=' + IntToStr(mt.lSampleSize));
  Dump.Add('FormatType='  + GuidToString(mt.formattype));
  //Dump.Add('pUnk='  +   GuidToString(mt.pUnk));
  Dump.Add('cbFormat=' + IntToHex(mt.cbFormat, 8));
  Dump.Add('pbFormat=' + IntToHex(integer(mt.pbFormat), 4));
end;
*)

// Fills "FormatArr" with list of all supported video formats (resolution, compression etc...)
FUNCTION TVideoSample.LoadListOfResolution: HResult;
VAR
  pSC                   : IAMStreamConfig;
  VideoStreamConfigCaps : TVideoStreamConfigCaps;
  p                     : ^TVideoStreamConfigCaps;
  ppmt                  : PAMMediaType;
  i, j,
  piCount,
  piSize                : integer;
  Swap                  : boolean;
  FM                    : TFormatInfo;
BEGIN
  SetLength(FormatArr, 0);
  Result := GetCaptureIAMStreamConfig(pSC);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  IF Result = S_OK then
    Result := pSC.GetNumberOfCapabilities(piCount, piSize);
  j := 0;
  if Result = S_OK then
    begin
      FOR i := 0 TO piCount-1 DO
        begin
          p := @VideoStreamConfigCaps;
          Result := pSC.GetStreamCaps(i, ppmt, p^);
          IF Succeeded(Result) then
            IF not(IsEqualGUID(ppmt^.formattype, KSDATAFORMAT_SPECIFIER_VIDEOINFO2)) then // Only first part of info is relevant
              begin
                SetLength(FormatArr, j+1);
                FormatArr[j].OIndex := i;
                FormatArr[j].Width  := p^.InputSize.cx;
                FormatArr[j].Height := p^.InputSize.cy;
                FormatArr[j].mt     := ppmt^;
                FormatArr[j].SSize  := ppmt^.lSampleSize;
                IF TGuIDEqual(MEDIASUBTYPE_RGB24, ppmt^.Subtype)
                  then FormatArr[j].FourCC := 'RGB '
                  else move(ppmt^.Subtype.D1, FormatArr[j].FourCC, 4);
                Inc(j);
              end;
        end;
    end;

  // Simple sort by width and height
  IF j > 1 then
    begin
      REPEAT
        Swap := false;
        FOR i := 0 TO j-2 DO
          IF (FormatArr[i].Width > FormatArr[i+1].Width) or
             (((FormatArr[i].Width = FormatArr[i+1].Width)) and ((FormatArr[i].Height > FormatArr[i+1].Height)))
          then
            begin
              Swap := true;
              FM := FormatArr[i];
              FormatArr[i] := FormatArr[i+1];
              FormatArr[i+1] := FM;
            end;
      UNTIL not(Swap);
    end;
END;



FUNCTION TVideoSample.SetVideoSizeByListIndex(ListIndex: integer): HResult;
// Sets one of the supported video stream sizes listed in "FormatArr".
// ListIndex is the index to one of the sizes from the stringlist received
// from "GetListOfVideoSizes".
VAR
  pSC                   : IAMStreamConfig;
BEGIN
  IF (ListIndex < 0) or (ListIndex >= Length(FormatArr)) then
    begin
      Result := S_FALSE;
      exit;
    end;

  pIMediaControl.Stop;

  Result := GetCaptureIAMStreamConfig(pSC);

  IF Succeeded(Result) then
    //Result := pSC.SetFormat(FormatArr[ListIndex].mt);
    // Sometimes delivers VFW_E_INVALIDMEDIATYPE, even for formats returned by GetStreamCaps

  pIMediaControl.Run;
END;



FUNCTION TVideoSample.GetStreamInfo(VAR Width, Height: integer; VAR FourCC: dword): HResult;
VAR
  pSC   : IAMStreamConfig;
  ppmt  : PAMMediaType;
  pmt   : _AMMediaType;

  VI    : VideoInfo;
  VIH   : VideoInfoHeader;
BEGIN
  Width := 0;
  Height := 0;
  //pIMediaControl.Stop; // Crash with FakeWebCam. Thanks to "Zacherl" from Delphi-Praxis http://www.delphipraxis.net/1165063-post16.html
  pIBFVideoSource.Stop;  // nicht zwingend nötig

  Result := GetCaptureIAMStreamConfig(pSC);
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if Result = S_OK then
    begin
      Result := pSC.GetFormat(ppmt);
      pmt := ppmt^;
      if  TGUIDEqual(ppmt.formattype, FORMAT_VideoInfo) then
        begin
          FillChar(VI, SizeOf(VI), #0);
          VIH := VideoInfoHeader(ppmt^.pbFormat^);
          move(VIH, VI, SizeOf(VIH));
          Width := VI.bmiHeader.biWidth;
          Height := Abs(VI.bmiHeader.biHeight);
          FourCC := VI.bmiHeader.biCompression;
        end;
    end;
  pIBFVideoSource.Run(0);// nicht zwingend nötig
  //pIMediaControl.Run;  // If we don't stop it, we don't need to start it...
END;






// See also: http://msdn.microsoft.com/en-us/library/ms784400(VS.85).aspx
FUNCTION TVideoSample.GetVideoPropAmpEx(    Prop                     : TVideoProcAmpProperty;
                                        VAR pMin, pMax,
                                            pSteppingDelta, pDefault : longint;
                                        VAR pCapsFlags               : TVideoProcAmpFlags;
                                        VAR pActual                  : longint): HResult;
BEGIN
  Result := S_False;
  if pIAMVideoProcAmp = nil then
    exit;
  Result := pIAMVideoProcAmp.GetRange(Prop, pMin, pMax, pSteppingDelta, pDefault, pCapsFlags);
  pActual := pDefault;
  IF Result = S_OK then
    Result := pIAMVideoProcAmp.Get(Prop, pActual, pCapsFlags)
END;



FUNCTION TVideoSample.SetVideoPropAmpEx(    Prop           : TVideoProcAmpProperty;
                                            pCapsFlags     : TVideoProcAmpFlags;
                                            pActual        : longint): HResult;
BEGIN
  Result := S_False;
  if pIAMVideoProcAmp = nil then
    exit;
  Result := pIAMVideoProcAmp.Set_(Prop, pActual, pCapsFlags)
END;



PROCEDURE TVideoSample.GetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; VAR AcPerCent: integer);
VAR
  pMin, pMax,
  pSteppingDelta,
  pDefault       : longint;
  pCapsFlags     : TVideoProcAmpFlags;
  pActual        : longint;
BEGIN
  IF GetVideoPropAmpEx(Prop, pMin, pMax, pSteppingDelta, pDefault, pCapsFlags, pActual) = S_OK
    THEN BEGIN
      AcPerCent := round(100 * (pActual-pMin)/(pMax-pMin));
    END
    ELSE AcPerCent := -1;
END;



PROCEDURE TVideoSample.SetVideoPropAmpPercent(Prop: TVideoProcAmpProperty; AcPerCent: integer);
VAR
  pMin, pMax,
  pSteppingDelta,
  pDefault        : longint;
  pCapsFlags      : TVideoProcAmpFlags;
  pActual         : longint;
  d               : double;
BEGIN
  IF GetVideoPropAmpEx(Prop, pMin, pMax, pSteppingDelta, pDefault, pCapsFlags, pActual) = S_OK
    THEN BEGIN
      IF (AcPercent < 0) or (AcPercent > 100) then
        begin
          pActual := pDefault;
        end
        else begin
          d := (pMax-pMin)/100*AcPercent;
          pActual := round(d);
          pActual := (pActual div pSteppingDelta) * pSteppingDelta;
          pActual := pActual + pMin;
        end;
      pIAMVideoProcAmp.Set_(Prop, pActual, pCapsFlags);
    END
END;



PROCEDURE TVideoSample.GetVideoSize(VAR Width, height: integer);
VAR
  pBV : IBasicVideo;
BEGIN
  Width := 0;
  Height := 0;
  pBV := nil;
  if pIGraphBuilder.QueryInterface(IID_IBasicVideo, pBV)=S_OK then
//  if pICapGraphBuild2.FindInterface(@PIN_CATEGORY_capture, @MEDIATYPE_Video, pIBFVideoSource, IID_IBasicVideo, pBV) = S_OK then
    pBV.GetVideoSize(Width, height);
END; {GetVideoSize}



FUNCTION TVideoSample.ShowVfWCaptureDlg: HResult;
VAR
  pVfw : IAMVfwCaptureDialogs;
BEGIN
  pVfw := nil;
  pIMediaControl.Stop;
  Result := pICapGraphBuild2.FindInterface(@PIN_CATEGORY_CAPTURE,
                                     @MEDIATYPE_Video,
                                     pIBFVideoSource,
                                     IID_IAMVfwCaptureDialogs, pVfW);

  if not(Succeeded(Result)) then // Retry
    Result := pICapGraphBuild2.queryinterface(IID_IAMVfwCaptureDialogs, pVfw);
  if not(Succeeded(Result)) then // Retry
    Result := pIGraphBuilder.queryinterface(IID_IAMVfwCaptureDialogs, pVfw);

  if (SUCCEEDED(Result)) THEN
    BEGIN
      // Check if the device supports this dialog box.
      if (S_OK = pVfw.HasDialog(VfwCaptureDialog_Source)) then
        // Show the dialog box.
        Result := pVfw.ShowDialog(VfwCaptureDialog_Source, ghApp);
    END;
  pIMediaControl.Run;
END;



FUNCTION TVideoSample.GetExProp(   guidPropSet : TGuiD;
                                      dwPropID : TAMPropertyPin;
                                pInstanceData  : pointer;
                                cbInstanceData : DWORD;
                                 out pPropData;
                                    cbPropData : DWORD;
                                out pcbReturned: DWORD): HResult;
BEGIN
  Result := pIKsPropertySet.Get(guidPropSet, dwPropID, pInstanceData, cbInstanceData, pPropData, cbPropData, pcbReturned);
END;



FUNCTION TVideoSample.SetExProp(   guidPropSet : TGuiD;
                                      dwPropID : TAMPropertyPin;
                                pInstanceData  : pointer;
                                cbInstanceData : DWORD;
                                     pPropData : pointer;
                                    cbPropData : DWORD): HResult;
BEGIN
  Result := pIKsPropertySet.Set_(guidPropSet, dwPropID, pInstanceData, cbInstanceData, pPropData, cbPropData);
END;


// Does work, if no GDI functions are called within callback!
// See remark on http://msdn.microsoft.com/en-us/library/ms786692(VS.85).aspx
PROCEDURE TVideoSample.SetCallBack(CB: TVideoSampleCallBack);
BEGIN
  CallBack := CB;
  SGrabberCB.FSampleGrabberCB.CallBack := CB;
END;


FUNCTION TVideoSample.GetPlayState: TPlayState;
BEGIN
  Result := g_psCurrent;
END;



PROCEDURE TVideoSample.GetListOfVideoSizes(VidSize: TStringList);
VAR
  i : integer;
BEGIN
  try
    IF not(assigned(VidSize)) then
      VidSize := TStringList.Create;
    VidSize.Clear;
  except
    exit;
  end;
  IF g_psCurrent < PS_Paused then
    exit;
  FOR i := 0 TO Length(FormatArr)-1 DO
    VidSize.Add(IntToStr(FormatArr[i].Width)+'*'+IntToStr(FormatArr[i].Height) + '  (' + FormatArr[i].FourCC+')');
END;




{$ifdef REGISTER_FILTERGRAPH}

FUNCTION TVideoSample.AddGraphToRot(pUnkGraph: IUnknown; VAR pdwRegister: DWORD):HRESULT;
VAR
  pMoniker   : IMoniker;
  pRot       : IRunningObjectTable;
  sz         : string;
  wsz        : ARRAY[0..128] OF wchar;
  hr         : HResult;
  dwRegister : integer absolute pdwregister;
  i : integer;
BEGIN
    {
    if (!pUnkGraph || !pdwRegister)
        return E_POINTER;
    }
    if (FAILED(GetRunningObjectTable(0, pROT))) then
      begin
        result := E_FAIL;
        exit;
      end;
    {
    wsprintfW(wsz, 'FilterGraph %08x pid %08x\0', DWORD_PTR(pUnkGraph),
              GetCurrentProcessId());
    }
    sz := 'FilterGraph ' + lowercase(IntToHex(integer((pUnkGraph)), 8))+' pid '+
                           lowercase(IntToHex(GetCurrentProcessID,8))+#0;
    fillchar(wsz, sizeof(wsz), #0);
    for i := 1 to length(sz) DO
      wsz[i-1] := widechar(sz[i]);
    hr := CreateItemMoniker('!', wsz, pMoniker);
    if (SUCCEEDED(hr)) then
      begin
        // Use the ROTFLAGS_REGISTRATIONKEEPSALIVE to ensure a strong reference
        // to the object.  Using this flag will cause the object to remain
        // registered until it is explicitly revoked with the Revoke() method.
        //
        // Not using this flag means that if GraphEdit remotely connects
        // to this graph and then GraphEdit exits, this object registration
        // will be deleted, causing future attempts by GraphEdit to fail until
        // this application is restarted or until the graph is registered again.
        hr := pROT.Register(ROTFLAGS_REGISTRATIONKEEPSALIVE, pUnkGraph,
                            pMoniker, dwRegister);
//        i := pMoniker._Release;  // <- Delphi wont let me do this myself!
      end;

//    pROT._Release(); // <- Delphi wont let me do this myself!
    result := hr;
end;



// Removes a filter graph from the Running Object Table
procedure TVideoSample.RemoveGraphFromRot(pdwRegister: dword);
VAR
  pROT :  IRunningObjectTable;
begin
  if (SUCCEEDED(GetRunningObjectTable(0, pROT))) then
    begin
      pROT.Revoke(pdwRegister);
//      pROT._Release();
    end;
end;

{$endif}




(*
FUNCTION TVideoSample.GetStreamInfoTest(VAR Width, Height: integer; VAR FourCC: dword): HResult;
VAR
  pSC   : IAMStreamConfig;
  ppmt  : PAMMediaType;
  pmt   : _AMMediaType;

  VI    : VideoInfo;
  VIH   : VideoInfoHeader;
BEGIN
  Width := 0;
  Height := 0;
  pIMediaControl.Stop;
  pIBFVideoSource.Stop;  // nicht zwingend nötig

  pSC := nil;
  Result := pICapGraphBuild2.FindInterface(@PIN_CATEGORY_capture,
                                           @MEDIATYPE_Video,
                                           pIBFVideoSource,
                                           IID_IAMStreamConfig, pSC);
  pSC.GetNumberOfCapabilities(piCount, piSize)
  {$ifdef DXErr} DXErrString := DXGetErrorDescription9A(Result); {$endif}
  if Result = S_OK then
    begin
      pSC.GetFormat(ppmt);
      pmt := ppmt^;
      if  TGUIDEqual(ppmt.formattype, FORMAT_VideoInfo) then
        begin
          FillChar(VI, SizeOf(VI), #0);
          VIH := VideoInfoHeader(ppmt^.pbFormat^);
          move(VIH, VI, SizeOf(VIH));
          Width := VI.bmiHeader.biWidth;
          Height := Abs(VI.bmiHeader.biHeight);
          FourCC := VI.bmiHeader.biCompression;
        end;
    end;
  pIBFVideoSource.Run(0);// nicht zwingend nötig
  pIMediaControl.Run;
END;
*)








destructor TVideoSample.Destroy;
begin
  try
    SetPreviewState(false);
    pIMediaControl.Stop;
    pIBFVideoSource.Stop;
    DeleteCaptureGraph;
    closeInterfaces;
    if assigned(SGrabberCB) and assigned(TSampleGrabberCB(SGrabberCB).FSampleGrabberCB) then
      begin
        TSampleGrabberCB(SGrabberCB).FSampleGrabberCB.Free;
        TSampleGrabberCB(SGrabberCB).FSampleGrabberCB := nil;
      end;



  finally
    try
      inherited destroy;
    except
    end;
  end;
end;











end.
