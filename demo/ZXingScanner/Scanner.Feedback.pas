unit Scanner.Feedback;

{
  A beep and a short vibration when a barcode is found, with what each
  platform has on board: no library beyond the RTL.

    Windows  MessageBeep; no vibration
    Android  ToneGenerator and Vibrator (the permission VIBRATE)
    iOS      the system sounds of AudioToolbox (the vibration is one of them)
    macOS    the beep of the terminal; no vibration

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

/// <summary>A short beep.</summary>
procedure Beep;

/// <summary>A short vibration, where the device has a motor.</summary>
procedure Vibrate;

implementation

uses
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows;
{$ELSEIF DEFINED(ANDROID)}
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Media,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText;
{$ELSEIF DEFINED(IOS)}
  iOSapi.AudioToolbox;
{$ELSE}
  System.SysUtils;
{$ENDIF}

{$IF DEFINED(ANDROID)}
var
  ToneGenerator: JToneGenerator;
{$ENDIF}

procedure Beep;
begin
{$IF DEFINED(MSWINDOWS)}
  MessageBeep(MB_OK);
{$ELSEIF DEFINED(ANDROID)}
  if ToneGenerator = nil then
    ToneGenerator := TJToneGenerator.JavaClass.init
      (TJAudioManager.JavaClass.STREAM_MUSIC, 80);
  ToneGenerator.startTone(TJToneGenerator.JavaClass.TONE_PROP_BEEP, 120);
{$ELSEIF DEFINED(IOS)}
  // 1057: the short "tink" of the system
  AudioServicesPlaySystemSound(1057);
{$ELSE}
  System.SysUtils.Beep;
{$ENDIF}
end;

procedure Vibrate;
{$IF DEFINED(ANDROID)}
var
  service: JObject;
  vibrator: JVibrator;
{$ENDIF}
begin
{$IF DEFINED(ANDROID)}
  service := TAndroidHelper.Context.getSystemService
    (TJContext.JavaClass.VIBRATOR_SERVICE);
  if service <> nil then
  begin
    vibrator := TJVibrator.Wrap((service as ILocalObject).GetObjectID);
    vibrator.vibrate(60);
  end;
{$ELSEIF DEFINED(IOS)}
  AudioServicesPlaySystemSound(kSystemSoundID_Vibrate);
{$ENDIF}
end;

end.
