unit Scanner.Tracking;

{
  The codes followed on the screen. The camera gives a hit now and then, with
  the corners of a code where the frame had it; the outline on the screen
  should not jump to every hit, go when a frame happens to miss the code, or
  vanish in one go. So the screen keeps a TTrackedCode per code, and a
  TCodeTracker that is fed the hits of every frame:

  - a known code gets the corners of the hit as its target, unless they moved
    less than the dead zone: then the outline stays where it is;
  - a new code comes in where it was seen and fades in;
  - every frame of the display (a TAnimation of FMX, which runs on the
    display link of the platform) the drawn corners ease towards the target
    and a code that has not been seen for a while fades out and is dropped.

  The easing is an exponential approach: per frame a fixed part of the
  distance that is left, so the outline moves fast when it is far off and
  settles softly, and a target that changes every hit only changes its
  direction. The part is taken for the time the frame took, so it looks the
  same at 60 and 120 Hz.

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Diagnostics,
  System.Generics.Collections,
  FMX.Types,
  FMX.Ani;

const
  /// <summary>The outline is drawn this far outside the corners of the
  /// code (in pixels of the frame).</summary>
  OutlinePadding = 20;
  /// <summary>A hit whose corners all moved less than this (in pixels of
  /// the frame) leaves the outline where it is: the noise of the detection,
  /// not a shift of the camera (a larger zone makes the outline follow a
  /// slow shift in steps).</summary>
  OutlineDeadZone = 3;
  /// <summary>The part of the distance to the target the outline covers
  /// per frame of 16 ms: the slow one when the target hardly moves (the
  /// noise of the detection is averaged out), the fast one when it moves
  /// fast (the outline follows at once), in between for the speeds between
  /// SlowSpeed and FastSpeed (pixels of the frame per ms).</summary>
  OutlineEaseSlow = 0.07;
  OutlineEaseFast = 0.5;
  SlowSpeed = 0.25;
  FastSpeed = 0.6;
  /// <summary>A read code is kept this long (ms) after it was last seen,
  /// a code that was found but not read shorter.</summary>
  CodeLifetime = 2000;
  UnreadLifetime = 800;
  /// <summary>The fade in and the fade out, in ms.</summary>
  FadeIn = 150;
  FadeOut = 300;
  /// <summary>The change of color when a code that was found gets read,
  /// in ms.</summary>
  ReadBlendMs = 200;

type
  /// <summary>A barcode found in a frame, as the screen keeps it: the
  /// results of the library are freed after each scan.</summary>
  THit = record
    Text: string;
    Format: string;
    /// <summary>The human readable form of GS1 data, '' for other codes.
    /// </summary>
    GS1: string;
    /// <summary>'' for a code that was read; else why it could not be
    /// (TScanManager.ReturnErrors).</summary>
    Error: string;
    /// <summary>The corners in the frame.</summary>
    Corners: TArray<TPointF>;
    /// <summary>The number of the code on the screen (multi result).
    /// </summary>
    Number: Integer;
    function IsUrl: Boolean;
    function Center: TPointF;
  end;

  /// <summary>A code on the screen: what was read, where its outline is
  /// (Corners) and where it goes (Target), how visible it is.</summary>
  TTrackedCode = class
  public
    Text: string;
    Format: string;
    GS1: string;
    Error: string;
    /// <summary>The number of the code on the screen, given when it was
    /// first seen; 0 for a code that was not read.</summary>
    Number: Integer;
    /// <summary>The corners of the last hit, in the frame.</summary>
    Target: TArray<TPointF>;
    /// <summary>The corners that are drawn: on their way to Target.
    /// </summary>
    Corners: TArray<TPointF>;
    /// <summary>0 to 1: fading in, there, fading out.</summary>
    Opacity: Single;
    /// <summary>Since the last hit.</summary>
    LastSeen: TStopwatch;
    /// <summary>How fast the target moved at the last hit, in pixels of
    /// the frame per ms: the easing follows it.</summary>
    Speed: Single;
    /// <summary>0 for a code that was found but not read, 1 for a read
    /// code; on its way from 0 to 1 when an unread code gets read (the
    /// color of the outline follows it).</summary>
    ReadBlend: Single;
    function Ease: Single;
    function Readable: Boolean;
    function IsUrl: Boolean;
    function Center: TPointF;
    /// <summary>The code as the sheet shows it.</summary>
    function AsHit: THit;
  end;

  /// <summary>The animation that ticks the tracker: a TAnimation that
  /// loops, with the work in OnProcess.</summary>
  TTrackingAnimation = class(TAnimation)
  protected
    procedure ProcessAnimation; override;
  end;

  TCodeTracker = class
  private
    FCodes: TObjectList<TTrackedCode>;
    FNext: Integer;
    FFrameWidth: Single;
    FFrozen: Boolean;
    FAnimation: TTrackingAnimation;
    FWatch: TStopwatch;
    FOnMoved: TNotifyEvent;
    FOnListChanged: TNotifyEvent;
    FOnAppeared: TNotifyEvent;
    function Find(const hit: THit): TTrackedCode;
    function InDeadZone(const code: TTrackedCode;
      const corners: TArray<TPointF>): Boolean;
    procedure Tick(Sender: TObject);
    procedure StartTicking;
  public
    /// <summary>The animation is a child of the parent (it needs a form
    /// above it to run on the display link).</summary>
    constructor Create(parent: TFmxObject);
    destructor Destroy; override;
    /// <summary>The codes of a frame: known codes get a new target, new
    /// ones come in (OnAppeared for a read code), and the ticking starts.
    /// </summary>
    procedure Update(const hits: TArray<THit>; frameWidth: Single);
    /// <summary>Drops every code.</summary>
    procedure Clear;
    /// <summary>The first code that was read, nil when there is none.
    /// </summary>
    function FirstReadable: TTrackedCode;
    /// <summary>The codes that were read, as the sheet shows them.</summary>
    function ReadableHits: TArray<THit>;
    property Codes: TObjectList<TTrackedCode> read FCodes;
    /// <summary>True for a still image: nothing ages or fades.</summary>
    property Frozen: Boolean read FFrozen write FFrozen;
    /// <summary>After a frame in which an outline moved or faded.</summary>
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    /// <summary>After codes came in or were dropped.</summary>
    property OnListChanged: TNotifyEvent read FOnListChanged
      write FOnListChanged;
    /// <summary>A read code that was not on the screen came in.</summary>
    property OnAppeared: TNotifyEvent read FOnAppeared write FOnAppeared;
  end;

implementation

uses
  System.Math;

{ THit }

function THit.IsUrl: Boolean;
begin
  Result := Text.StartsWith('http://', True) or Text.StartsWith('https://', True);
end;

function THit.Center: TPointF;
begin
  Result := TPointF.Zero;
  if Length(Corners) = 0 then
    exit;
  for var c in Corners do
    Result := Result + c;
  Result := Result / Length(Corners);
end;

{ TTrackedCode }

function TTrackedCode.Readable: Boolean;
begin
  Result := Error = '';
end;

function TTrackedCode.IsUrl: Boolean;
begin
  Result := Text.StartsWith('http://', True) or Text.StartsWith('https://', True);
end;

function TTrackedCode.Center: TPointF;
begin
  Result := TPointF.Zero;
  if Length(Target) = 0 then
    exit;
  for var c in Target do
    Result := Result + c;
  Result := Result / Length(Target);
end;

function TTrackedCode.Ease: Single;
begin
  // slow for the noise, fast for a real move, in between for the rest
  var t := EnsureRange((Speed - SlowSpeed) / (FastSpeed - SlowSpeed), 0, 1);
  Result := OutlineEaseSlow + (OutlineEaseFast - OutlineEaseSlow) * t;
end;

function TTrackedCode.AsHit: THit;
begin
  Result.Text := Text;
  Result.Format := Format;
  Result.GS1 := GS1;
  Result.Error := Error;
  Result.Corners := Copy(Corners);
  Result.Number := Number;
end;

{ TTrackingAnimation }

procedure TTrackingAnimation.ProcessAnimation;
begin
  // nothing to interpolate here: the tracker does its work in OnProcess
end;

{ TCodeTracker }

constructor TCodeTracker.Create(parent: TFmxObject);
begin
  inherited Create;
  FCodes := TObjectList<TTrackedCode>.Create(True);
  FAnimation := TTrackingAnimation.Create(parent);
  FAnimation.Parent := parent;
  FAnimation.Loop := True;
  FAnimation.Duration := 1;
  FAnimation.OnProcess := Tick;
end;

destructor TCodeTracker.Destroy;
begin
  FAnimation.Enabled := False;
  FAnimation.Free;
  FCodes.Free;
  inherited;
end;

procedure TCodeTracker.Clear;
begin
  FAnimation.Enabled := False;
  FCodes.Clear;
  FNext := 0;
end;

function TCodeTracker.Find(const hit: THit): TTrackedCode;
var
  nearest: Single;
begin
  Result := nil;
  if hit.Error = '' then
    // a read code: the one with the same text
    for var code in FCodes do
      if (code.Text = hit.Text) and (code.Format = hit.Format) then
        exit(code);
  // a code that was found but not read has no text: the nearest code, when
  // it is close enough; for a read code only one that is not read yet (it
  // becomes this one: the camera held still)
  nearest := FFrameWidth / 8;
  for var code in FCodes do
  begin
    if (hit.Error = '') and code.Readable then
      continue;
    var d := code.Center.Distance(hit.Center);
    if d < nearest then
    begin
      nearest := d;
      Result := code;
    end;
  end;
end;

function TCodeTracker.InDeadZone(const code: TTrackedCode;
  const corners: TArray<TPointF>): Boolean;
begin
  // the corners all moved less than the dead zone
  if Length(corners) <> Length(code.Target) then
    exit(False);
  for var i := 0 to High(corners) do
    if corners[i].Distance(code.Target[i]) >= OutlineDeadZone then
      exit(False);
  Result := True;
end;

procedure TCodeTracker.Update(const hits: TArray<THit>; frameWidth: Single);
var
  added: Boolean;
begin
  FFrameWidth := frameWidth;
  added := False;
  for var hit in hits do
  begin
    var code := Find(hit);
    if code = nil then
    begin
      // new on the screen: where it was seen, fading in
      code := TTrackedCode.Create;
      code.Text := hit.Text;
      code.Format := hit.Format;
      code.GS1 := hit.GS1;
      code.Error := hit.Error;
      code.Target := Copy(hit.Corners);
      code.Corners := Copy(hit.Corners);
      code.Opacity := 0;
      if code.Readable then
      begin
        Inc(FNext);
        code.Number := FNext;
        code.ReadBlend := 1;
      end;
      code.LastSeen := TStopwatch.StartNew;
      FCodes.Add(code);
      added := True;
      if code.Readable and Assigned(FOnAppeared) then
        FOnAppeared(Self);
    end
    else
    begin
      if (hit.Error = '') and not code.Readable then
      begin
        // a code that was found but not read is read now: the same code,
        // with its text and a number; its outline changes color
        code.Text := hit.Text;
        code.Format := hit.Format;
        code.GS1 := hit.GS1;
        code.Error := '';
        Inc(FNext);
        code.Number := FNext;
        added := True;
        if Assigned(FOnAppeared) then
          FOnAppeared(Self);
      end;
      // seen again: a new target, unless it hardly moved; its speed from
      // how far it went since the last hit
      if not InDeadZone(code, hit.Corners) then
      begin
        var dist: Single := 0;
        for var c := 0 to Min(High(hit.Corners), High(code.Target)) do
          dist := Max(dist, hit.Corners[c].Distance(code.Target[c]));
        // (averaged over the hits: one noisy hit must not make it fast)
        var speed := dist / Max(1, code.LastSeen.ElapsedMilliseconds);
        code.Speed := code.Speed + (speed - code.Speed) * 0.5;
        code.Target := Copy(hit.Corners);
      end
      else
        code.Speed := code.Speed * 0.5;
      code.LastSeen := TStopwatch.StartNew;
    end;
  end;
  if added and Assigned(FOnListChanged) then
    FOnListChanged(Self);
  if FCodes.Count > 0 then
    StartTicking;
end;

procedure TCodeTracker.StartTicking;
begin
  if not FAnimation.Enabled then
  begin
    FWatch := TStopwatch.StartNew;
    FAnimation.Enabled := True;
  end;
end;

procedure TCodeTracker.Tick(Sender: TObject);
var
  dt, k: Single;
  moved, dropped: Boolean;
begin
  // the time since the last frame, in ms (the first frame, or after a
  // pause: as one frame)
  dt := FWatch.Elapsed.TotalMilliseconds;
  FWatch := TStopwatch.StartNew;
  dt := EnsureRange(dt, 1, 100);

  moved := False;
  dropped := False;
  for var i := FCodes.Count - 1 downto 0 do
  begin
    var code := FCodes[i];
    // the outline on its way to the target: the part of the way for this
    // frame, from the easing of the code (its speed) per 16 ms
    k := 1 - Power(1 - code.Ease, dt / 16);
    for var c := 0 to High(code.Corners) do
      if code.Corners[c] <> code.Target[c] then
      begin
        code.Corners[c] := code.Corners[c] +
          (code.Target[c] - code.Corners[c]) * k;
        if code.Corners[c].Distance(code.Target[c]) < 0.2 then
          code.Corners[c] := code.Target[c];
        moved := True;
      end;
    // fading in, or out when not seen for a while (not on a still image)
    var lifetime := CodeLifetime;
    if not code.Readable then
      lifetime := UnreadLifetime;
    if (not FFrozen) and (code.LastSeen.ElapsedMilliseconds > lifetime) then
    begin
      code.Opacity := code.Opacity - dt / FadeOut;
      moved := True;
      if code.Opacity <= 0 then
      begin
        FCodes.Delete(i);
        dropped := True;
      end;
    end
    else if code.Opacity < 1 then
    begin
      code.Opacity := Min(1, code.Opacity + dt / FadeIn);
      moved := True;
    end;
    // the color of a code that got read, from white to the outline color
    if code.Readable and (code.ReadBlend < 1) then
    begin
      code.ReadBlend := Min(1, code.ReadBlend + dt / ReadBlendMs);
      moved := True;
    end;
  end;

  if FCodes.Count = 0 then
  begin
    // nothing left: the numbers start over, the ticking stops
    FNext := 0;
    FAnimation.Enabled := False;
  end;
  if dropped and Assigned(FOnListChanged) then
    FOnListChanged(Self);
  if moved and Assigned(FOnMoved) then
    FOnMoved(Self);
end;

function TCodeTracker.FirstReadable: TTrackedCode;
begin
  for var code in FCodes do
    if code.Readable then
      exit(code);
  Result := nil;
end;

function TCodeTracker.ReadableHits: TArray<THit>;
begin
  Result := nil;
  for var code in FCodes do
    if code.Readable then
      Result := Result + [code.AsHit];
end;

end.
