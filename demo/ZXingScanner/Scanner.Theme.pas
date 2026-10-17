unit Scanner.Theme;

{
  The look of the demo: its colors, fonts and icons, and a few helpers for
  the controls. Everything here is done with the standard FMX controls, no
  custom style is needed: the labels get their colors from here, the icons
  are TPath controls with SVG path data, the cards and pills are rectangles
  with rounded corners.

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

uses
  System.Types,
  System.UITypes,
  FMX.Graphics,
  FMX.Objects,
  FMX.StdCtrls;

const
  // the colors of the design
  clBackground = TAlphaColor($FF0B0D10);
  clSheet = TAlphaColor($FF15171B);
  clCard = TAlphaColor($FF15171B);
  clCardRaised = TAlphaColor($FF23262C);
  clChip = TAlphaColor($FF262A31);
  clSeparator = TAlphaColor($FF23262C);
  clBorder = TAlphaColor($FF2E3238);
  clGrip = TAlphaColor($FF3A3E46);
  clSwitchOff = TAlphaColor($FF3A3E46);
  clOverlay = TAlphaColor($9E121418); // the translucent buttons on the camera
  clText = TAlphaColor($FFFFFFFF);
  clTextMuted = TAlphaColor($FFA3A9B3);
  clTextFaint = TAlphaColor($FF8C929C);
  clTextDark = TAlphaColor($FF111214);
  clAccent = TAlphaColor($FF3EBDE4);
  clUnread = TAlphaColor($FFFFFFFF); // the outline of a code found but not read

  // the colors to choose from for the outline of a found code
  TrackColors: array [0 .. 3] of TAlphaColor = ($FFFF7F50, $FF3EBDE4,
    $FF30D158, $FFFFD60A);

  // The icons: the SVG path data of the Material Design icons of Google
  // (https://github.com/google/material-design-icons, Apache License 2.0),
  // 24 x 24, filled by a TPath (SetIcon).
  icoSettings = 'M19.14,12.94c0.04-0.3,0.06-0.61,0.06-0.94c0-0.32-0.02-0.64-0.07-0.94l2.03-1.58' +
    'c0.18-0.14,0.23-0.41,0.12-0.61l-1.92-3.32c-0.12-0.22-0.37-0.29-0.59-0.22l-2.39,0.96' +
    'c-0.5-0.38-1.03-0.7-1.62-0.94L14.4,2.81c-0.04-0.24-0.24-0.41-0.48-0.41h-3.84' +
    'c-0.24,0-0.43,0.17-0.47,0.41L9.25,5.35C8.66,5.59,8.12,5.92,7.63,6.29L5.24,5.33' +
    'c-0.22-0.08-0.47,0-0.59,0.22L2.74,8.87C2.62,9.08,2.66,9.34,2.86,9.48l2.03,1.58' +
    'C4.84,11.36,4.8,11.69,4.8,12s0.02,0.64,0.07,0.94l-2.03,1.58c-0.18,0.14-0.23,0.41-0.12,0.61' +
    'l1.92,3.32c0.12,0.22,0.37,0.29,0.59,0.22l2.39-0.96c0.5,0.38,1.03,0.7,1.62,0.94l0.36,2.54' +
    'c0.05,0.24,0.24,0.41,0.48,0.41h3.84c0.24,0,0.44-0.17,0.47-0.41l0.36-2.54' +
    'c0.59-0.24,1.13-0.56,1.62-0.94l2.39,0.96c0.22,0.08,0.47,0,0.59-0.22l1.92-3.32' +
    'c0.12-0.22,0.07-0.47-0.12-0.61L19.14,12.94z M12,15.6c-1.98,0-3.6-1.62-3.6-3.6' +
    's1.62-3.6,3.6-3.6s3.6,1.62,3.6,3.6S13.98,15.6,12,15.6z';
  icoTorch = 'M6,2h12v3H6z M6,7v1l2,3v11h8V11l2-3V7H6z M12,15.5c-0.83,0-1.5-0.67-1.5-1.5' +
    's0.67-1.5,1.5-1.5s1.5,0.67,1.5,1.5S12.83,15.5,12,15.5z';
  icoBack = 'M20 11H7.83l5.59-5.59L12 4l-8 8 8 8 1.41-1.41L7.83 13H20v-2z';
  icoChevron = 'M10 6L8.59 7.41 13.17 12l-4.58 4.59L10 18l6-6z';
  icoGlobe = 'M11.99 2C6.47 2 2 6.48 2 12s4.47 10 9.99 10C17.52 22 22 17.52 22 12S17.52 2 11.99 2z' +
    'm6.93 6h-2.95c-.32-1.25-.78-2.45-1.38-3.56 1.84.63 3.37 1.91 4.33 3.56zM12 4.04' +
    'c.83 1.2 1.48 2.53 1.91 3.96h-3.82c.43-1.43 1.08-2.76 1.91-3.96zM4.26 14C4.1 13.36 4 12.69 4 12' +
    's.1-1.36.26-2h3.38c-.08.66-.14 1.32-.14 2 0 .68.06 1.34.14 2H4.26zm.82 2h2.95' +
    'c.32 1.25.78 2.45 1.38 3.56-1.84-.63-3.37-1.9-4.33-3.56zm2.95-8H5.08' +
    'c.96-1.66 2.49-2.93 4.33-3.56C8.81 5.55 8.35 6.75 8.03 8zM12 19.96' +
    'c-.83-1.2-1.48-2.53-1.91-3.96h3.82c-.43 1.43-1.08 2.76-1.91 3.96zM14.34 14H9.66' +
    'c-.09-.66-.16-1.32-.16-2 0-.68.07-1.35.16-2h4.68c.09.65.16 1.32.16 2 0 .68-.07 1.34-.16 2z' +
    'm.25 5.56c.6-1.11 1.06-2.31 1.38-3.56h2.95c-.96 1.65-2.49 2.93-4.33 3.56zM16.36 14' +
    'c.08-.66.14-1.32.14-2 0-.68-.06-1.34-.14-2h3.38c.16.64.26 1.31.26 2s-.1 1.36-.26 2h-3.38z';
  icoText = 'M3 18h12v-2H3v2zM3 6v2h18V6H3zm0 7h18v-2H3v2z';
  icoCopy = 'M16 1H4c-1.1 0-2 .9-2 2v14h2V3h12V1zm3 4H8c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h11' +
    'c1.1 0 2-.9 2-2V7c0-1.1-.9-2-2-2zm0 16H8V7h11v14z';
  icoFolder = 'M20 6h-8l-2-2H4c-1.1 0-1.99.9-1.99 2L2 18c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V8' +
    'c0-1.1-.9-2-2-2zm0 12H4V8h16v10z';
  icoCamera = 'M8.8,12a3.2,3.2 0 1 0 6.4,0a3.2,3.2 0 1 0 -6.4,0z M9 2L7.17 4H4c-1.1 0-2 .9-2 2v12' +
    'c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V6c0-1.1-.9-2-2-2h-3.17L15 2H9zm3 15c-2.76 0-5-2.24-5-5' +
    's2.24-5 5-5 5 2.24 5 5-2.24 5-5 5z';
  icoInverted = 'M12,4.81L12,19c-3.31,0-6-2.63-6-5.87c0-1.56,0.62-3.03,1.75-4.14L12,4.81' +
    ' M6.35,7.56L6.35,7.56C4.9,8.99,4,10.96,4,13.13C4,17.48,7.58,21,12,21c4.42,0,8-3.52,8-7.87' +
    'c0-2.17-0.9-4.14-2.35-5.57l0,0L12,2L6.35,7.56z';
  icoTryHarder = 'M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3' +
    'S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5z' +
    'm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z' +
    ' M12 10h-2v2H9v-2H7V9h2V7h1v2h2v1z';
  icoMulti = 'M14.67,5v6.5H9.33V5H14.67z M15.67,11.5H21V5h-5.33V11.5z M14.67,19v-6.5H9.33V19H14.67z' +
    ' M15.67,12.5V19H21v-6.5H15.67z M8.33,12.5H3V19h5.33V12.5z M8.33,11.5V5H3v6.5H8.33z';
  icoDownscale = 'M11.99 18.54l-7.37-5.73L3 14.07l9 7 9-7-1.63-1.27-7.38 5.74zM12 16l7.36-5.73L21 9' +
    'l-9-7-9 7 1.63 1.27L12 16z';
  icoParallel = 'M15 9H9v6h6V9zm-2 4h-2v-2h2v2zm8-2V9h-2V7c0-1.1-.9-2-2-2h-2V3h-2v2h-2V3H9v2H7' +
    'c-1.1 0-2 .9-2 2v2H3v2h2v2H3v2h2v2c0 1.1.9 2 2 2h2v2h2v-2h2v2h2v-2h2c1.1 0 2-.9 2-2v-2h2' +
    'v-2h-2v-2h2zm-4 6H7V7h10v10z';
  icoBattery = 'M16,4h-2V2h-4v2H8C7.45,4,7,4.45,7,5v16c0,0.55,0.45,1,1,1h8c0.55,0,1-0.45,1-1V5' +
    'C17,4.45,16.55,4,16,4z M15,14h-2v2h-2v-2H9v-2h2v-2h2v2h2V14z';
  icoErrors = 'M12,5.99L19.53,19H4.47L12,5.99 M12,2L1,21h22L12,2L12,2z M13,16L11,16L11,18L13,18z' +
    ' M13,10L11,10L11,15L13,15z';
  icoHaptic = 'M0 15h2V9H0v6zm3 2h2V7H3v10zm19-8v6h2V9h-2zm-3 8h2V7h-2v10zM16.5 3h-9' +
    'C6.67 3 6 3.67 6 4.5v15c0 .83.67 1.5 1.5 1.5h9c.83 0 1.5-.67 1.5-1.5v-15c0-.83-.67-1.5-1.5-1.5z' +
    'M16 19H8V5h8v14z';
  icoBeep = 'M3 9v6h4l5 5V4L7 9H3zm13.5 3c0-1.77-1.02-3.29-2.5-4.03v8.05c1.48-.73 2.5-2.25 2.5-4.02z' +
    'M14 3.23v2.06c2.89.86 5 3.54 5 6.71s-2.11 5.85-5 6.71v2.06c4.01-.91 7-4.49 7-8.77' +
    's-2.99-7.86-7-8.77z';

/// <summary>The monospace font of the platform, for the text of a barcode
/// and the format names.</summary>
function MonoFontFamily: string;

/// <summary>Gives a TPath an icon: filled in the color, fitted to the
/// control.</summary>
procedure SetIcon(path: TPath; const data: string; color: TAlphaColor);

/// <summary>Sets a label's font, color and wrapping (and switches off the
/// styled settings, so that the style of the platform does not override
/// them).</summary>
procedure StyleLabel(lbl: TLabel; size: Single; color: TAlphaColor;
  bold: Boolean = False; mono: Boolean = False; wrap: Boolean = False);

/// <summary>The size of a text in the font of the label, wrapped at the
/// width when wordWrap is set (else the width is its limit).</summary>
function MeasureText(lbl: TLabel; const text: string; maxWidth: Single;
  wordWrap: Boolean): TSizeF;

/// <summary>Makes a label a tappable link: it answers to taps up to 12
/// points beside and 14 points above and below its text, so that a line of
/// text is as easy to hit as a button, without taking that room in the
/// layout (TouchTargetExpansion).</summary>
procedure MakeLink(lbl: TLabel; size: Single; color: TAlphaColor);

/// <summary>The color between a and b: a for t = 0, b for t = 1.</summary>
function BlendColor(a, b: TAlphaColor; t: Single): TAlphaColor;

/// <summary>Puts the text on the clipboard of the platform.</summary>
procedure CopyToClipboard(const text: string);

implementation

uses
  System.Math,
  System.Rtti,
  FMX.Types,
  FMX.Platform,
  FMX.TextLayout;

function MonoFontFamily: string;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := 'Consolas';
{$ELSEIF DEFINED(MACOS)}
  // macOS and iOS
  Result := 'Menlo';
{$ELSEIF DEFINED(ANDROID)}
  Result := 'monospace';
{$ELSE}
  Result := '';
{$ENDIF}
end;

procedure SetIcon(path: TPath; const data: string; color: TAlphaColor);
begin
  // The 24 x 24 frame of the icon as a speck in two of its corners (too
  // small to see), so that Fit keeps the space around the icon and every
  // icon has the size its designer gave it.
  path.Data.Data := 'M0,0h0.01v0.01H0z M23.99,23.99h0.01v0.01h-0.01z ' + data;
  path.WrapMode := TPathWrapMode.Fit;
  path.Fill.Kind := TBrushKind.Solid;
  path.Fill.Color := color;
  path.Stroke.Kind := TBrushKind.None;
  path.HitTest := False;
end;

procedure StyleLabel(lbl: TLabel; size: Single; color: TAlphaColor;
  bold, mono, wrap: Boolean);
begin
  lbl.StyledSettings := [];
  // (a TLabel wraps by default)
  lbl.WordWrap := wrap;
  lbl.TextSettings.Font.Size := size;
  lbl.TextSettings.FontColor := color;
  if bold then
    lbl.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    lbl.TextSettings.Font.Style := [];
  if mono and (MonoFontFamily <> '') then
    lbl.TextSettings.Font.Family := MonoFontFamily;
end;

function MeasureText(lbl: TLabel; const text: string; maxWidth: Single;
  wordWrap: Boolean): TSizeF;
var
  layout: TTextLayout;
begin
  layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    layout.BeginUpdate;
    layout.Font.Assign(lbl.TextSettings.Font);
    layout.WordWrap := wordWrap;
    layout.MaxSize := TPointF.Create(maxWidth, 10000);
    layout.Text := text;
    layout.EndUpdate;
    Result := TSizeF.Create(layout.TextWidth, layout.TextHeight);
  finally
    layout.Free;
  end;
end;

procedure MakeLink(lbl: TLabel; size: Single; color: TAlphaColor);
begin
  StyleLabel(lbl, size, color, True);
  lbl.HitTest := True;
  lbl.TouchTargetExpansion.Rect := TRectF.Create(12, 14, 12, 14);
end;

function BlendColor(a, b: TAlphaColor; t: Single): TAlphaColor;
  function Mix(x, y: Byte): Byte;
  begin
    Result := Round(x + (y - x) * t);
  end;

begin
  t := Min(1, Max(0, t));
  TAlphaColorRec(Result).A := Mix(TAlphaColorRec(a).A, TAlphaColorRec(b).A);
  TAlphaColorRec(Result).R := Mix(TAlphaColorRec(a).R, TAlphaColorRec(b).R);
  TAlphaColorRec(Result).G := Mix(TAlphaColorRec(a).G, TAlphaColorRec(b).G);
  TAlphaColorRec(Result).B := Mix(TAlphaColorRec(a).B, TAlphaColorRec(b).B);
end;

procedure CopyToClipboard(const text: string);
var
  clipboard: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
    clipboard) then
    clipboard.SetClipboard(TValue.From<string>(text));
end;

end.
