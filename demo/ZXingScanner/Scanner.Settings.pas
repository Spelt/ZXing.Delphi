unit Scanner.Settings;

{
  The settings screen: what the scanner decodes (the hints and the
  TryDownscale, ReturnErrors and ParallelDecoding switches of the library),
  which formats it reads, and what happens on a hit. It edits the
  TScannerConfig it is given; Done hides the screen and tells the scan
  screen (OnDone), which saves the settings and scans with them.

  The cards and section labels are in the form; the rows and chips, which
  are all alike, are made by the code below.

  Copyright 2026 E Spelt
  Licensed under the Apache License, Version 2.0.
}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls.Presentation,
  ZXing.BarcodeFormat,
  Scanner.Config;

type
  TSettingKind = (skInverted, skTryHarder, skMulti, skDownscale, skParallel,
    skErrors, skHaptic, skBeep);

  TSettingsForm = class(TForm)
    layHeader: TLayout;
    lblKicker: TLabel;
    layTitles: TLayout;
    lblTitle: TLabel;
    btnDone: TRectangle;
    lblDone: TLabel;
    scroll: TVertScrollBox;
    lblDecoding: TLabel;
    cardDecoding: TRectangle;
    layFormatsHeader: TLayout;
    lblFormats: TLabel;
    lblReset: TLabel;
    lblClear: TLabel;
    lblFormatCount: TLabel;
    cardFormats: TRectangle;
    lblAuto: TLabel;
    flowAuto: TFlowLayout;
    layExtraHeader: TLayout;
    lblExtra: TLabel;
    lblExtraClear: TLabel;
    lblExtraAll: TLabel;
    flowExtra: TFlowLayout;
    lblOnHit: TLabel;
    cardOnHit: TRectangle;
    layColors: TLayout;
    lblColor: TLabel;
    layDots: TLayout;
    lblFooter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnDoneClick(Sender: TObject);
    procedure lblResetClick(Sender: TObject);
    procedure lblClearClick(Sender: TObject);
    procedure lblExtraClearClick(Sender: TObject);
    procedure lblExtraAllClick(Sender: TObject);
  private
    FConfig: TScannerConfig;
    FOnDone: TNotifyEvent;
    fLoading: Boolean;
    fSwitches: array [TSettingKind] of TSwitch;
    // the rows with a switch and the label of their explanation
    fRows: TArray<TLayout>;
    fRowTexts: TArray<TLabel>;
    procedure AddSwitchRow(card: TRectangle; kind: TSettingKind;
      const icon, title, subtitle: string);
    procedure AddChips(flow: TFlowLayout; inAuto: Boolean);
    procedure AddDots;
    procedure SwitchChanged(Sender: TObject);
    procedure ChipClick(Sender: TObject);
    procedure DotClick(Sender: TObject);
    procedure StyleChip(chip: TRectangle);
    procedure StyleDots;
    procedure LoadValues;
    procedure UpdateFormatCount;
    procedure SetExtraFormats(on: Boolean);
    procedure FitRows(width: Single);
    procedure FitFlow(flow: TFlowLayout; width: Single);
    procedure FitCard(card: TRectangle);
    procedure LayoutCards;
  public
    /// <summary>The settings that are edited; set before Show.</summary>
    property Config: TScannerConfig read FConfig write FConfig;
    /// <summary>Called when the screen is done (Done, the back button or
    /// the window closed).</summary>
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
  end;

implementation

uses
  System.Math,
  Scanner.Formats,
  Scanner.Theme;

{$R *.fmx}

const
  ChipHeight = 36;
  ChipGap = 8;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  // the look of what the form holds
  StyleLabel(lblKicker, 13, clTextMuted, True);
  StyleLabel(lblTitle, 28, clText, True);
  StyleLabel(lblDone, 15, clTextDark, True);
  StyleLabel(lblDecoding, 12, clTextMuted, True);
  StyleLabel(lblFormats, 12, clTextMuted, True);
  StyleLabel(lblFormatCount, 13, clTextMuted);
  MakeLink(lblReset, 13, clAccent);
  MakeLink(lblClear, 13, clAccent);
  MakeLink(lblExtraClear, 13, clAccent);
  MakeLink(lblExtraAll, 13, clAccent);
  StyleLabel(lblAuto, 12, clTextMuted, True);
  StyleLabel(lblExtra, 12, clTextMuted, True);
  StyleLabel(lblOnHit, 12, clTextMuted, True);
  StyleLabel(lblColor, 16, clText, True);
  StyleLabel(lblFooter, 12, clTextFaint, False, False, True);

  // the rows: what the scanner does, in the words of the library
  AddSwitchRow(cardDecoding, skInverted, icoInverted, 'Also inverted images',
    'Light codes on a dark background too (ENABLE_INVERSION)');
  AddSwitchRow(cardDecoding, skTryHarder, icoTryHarder, 'Try harder',
    'More accurate on damaged, tiny or rotated codes, a bit slower (TRY_HARDER)');
  AddSwitchRow(cardDecoding, skMulti, icoMulti, 'Multi result reader',
    'Find every code in the frame (ScanAll) instead of the first one');
  AddSwitchRow(cardDecoding, skDownscale, icoDownscale, 'Downscaled copies',
    'Also scan smaller copies of the frame: large, blurry and dot-peen codes (TryDownscale)');
  AddSwitchRow(cardDecoding, skParallel, icoParallel,
    Format('Parallel decoding (%d cores)', [TThread.ProcessorCount]),
    'The readers side by side in the tasks of a thread pool (ParallelDecoding)');
  AddSwitchRow(cardDecoding, skErrors, icoErrors, 'Show unreadable codes',
    'Outline a code that was found but not read, to hold still (ReturnErrors)');
  AddChips(flowAuto, True);
  AddChips(flowExtra, False);
  AddDots;
  AddSwitchRow(cardOnHit, skHaptic, icoHaptic, 'Haptic feedback',
    'A short vibration on a new code');
  AddSwitchRow(cardOnHit, skBeep, icoBeep, 'Beep', 'A beep on a new code');
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  LoadValues;
  LayoutCards;
  scroll.ViewportPosition := TPointF.Zero;
end;

procedure TSettingsForm.FormResize(Sender: TObject);
begin
  LayoutCards;
end;

procedure TSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // the screen stays, hidden, for the next time
  Action := TCloseAction.caHide;
  if Assigned(FOnDone) then
    FOnDone(Self);
end;

procedure TSettingsForm.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  // the back button of Android
  if Key = vkHardwareBack then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TSettingsForm.btnDoneClick(Sender: TObject);
begin
  Close;
end;

{ the rows }

procedure TSettingsForm.AddSwitchRow(card: TRectangle; kind: TSettingKind;
  const icon, title, subtitle: string);
const
  RowHeight = 66;
begin
  // a row: an icon, the title with a line of explanation, the switch; a
  // separator above it when it is not the first of the card
  if card.ControlsCount > 0 then
  begin
    var line := TRectangle.Create(card);
    line.Parent := card;
    line.Align := TAlignLayout.Top;
    line.Position.Y := card.ControlsCount * RowHeight;
    line.Height := 1;
    line.Margins.Left := 64;
    line.Fill.Color := clSeparator;
    line.Stroke.Kind := TBrushKind.None;
  end;

  var row := TLayout.Create(card);
  row.Parent := card;
  row.Align := TAlignLayout.Top;
  row.Position.Y := card.ControlsCount * RowHeight;
  row.Height := RowHeight;

  // (the icon and the switch keep their size in a layout of their own,
  // whatever the height of the row)
  var boxHolder := TLayout.Create(row);
  boxHolder.Parent := row;
  boxHolder.Align := TAlignLayout.Left;
  boxHolder.Width := 50;
  var box := TRectangle.Create(boxHolder);
  box.Parent := boxHolder;
  box.Align := TAlignLayout.Center;
  box.Width := 34;
  box.Height := 34;
  box.Margins.Left := 16;
  box.XRadius := 10;
  box.YRadius := 10;
  box.Fill.Color := clCardRaised;
  box.Stroke.Kind := TBrushKind.None;
  var path := TPath.Create(box);
  path.Parent := box;
  path.Align := TAlignLayout.Contents;
  path.Margins.Rect := TRectF.Create(6, 6, 6, 6);
  SetIcon(path, icon, clAccent);

  var swHolder := TLayout.Create(row);
  swHolder.Parent := row;
  swHolder.Align := TAlignLayout.Right;
  swHolder.Width := 68;
  var sw := TSwitch.Create(swHolder);
  sw.Parent := swHolder;
  sw.Align := TAlignLayout.Center;
  sw.Width := 52;
  sw.Height := 30;
  sw.Margins.Right := 16;
  sw.Tag := Ord(kind);
  sw.OnSwitch := SwitchChanged;
  fSwitches[kind] := sw;

  var texts := TLayout.Create(row);
  texts.Parent := row;
  texts.Align := TAlignLayout.Client;
  texts.Margins.Rect := TRectF.Create(14, 10, 12, 8);
  var lblTitleRow := TLabel.Create(texts);
  lblTitleRow.Parent := texts;
  lblTitleRow.Align := TAlignLayout.Top;
  lblTitleRow.Height := 22;
  lblTitleRow.Text := title;
  StyleLabel(lblTitleRow, 16, clText, True);
  var lblSub := TLabel.Create(texts);
  lblSub.Parent := texts;
  lblSub.Align := TAlignLayout.Client;
  lblSub.TextSettings.VertAlign := TTextAlign.Leading;
  lblSub.Text := subtitle;
  StyleLabel(lblSub, 12, clTextMuted, False, False, True);
  fRows := fRows + [row];
  fRowTexts := fRowTexts + [lblSub];
end;

procedure TSettingsForm.SwitchChanged(Sender: TObject);
var
  on: Boolean;
begin
  if fLoading then
    exit;
  on := TSwitch(Sender).IsChecked;
  case TSettingKind(TSwitch(Sender).Tag) of
    skInverted:
      FConfig.Inverted := on;
    skTryHarder:
      FConfig.TryHarder := on;
    skMulti:
      FConfig.MultiResult := on;
    skDownscale:
      FConfig.Downscale := on;
    skParallel:
      FConfig.Parallel := on;
    skErrors:
      FConfig.ShowErrors := on;
    skHaptic:
      FConfig.Haptic := on;
    skBeep:
      FConfig.Beep := on;
  end;
end;

{ the formats }

procedure TSettingsForm.AddChips(flow: TFlowLayout; inAuto: Boolean);
begin
  // a chip per format, its name as in code (TBarcodeFormat.QR_CODE)
  flow.HorizontalGap := ChipGap;
  flow.VerticalGap := ChipGap;
  for var info in Formats do
  begin
    if info.InAuto <> inAuto then
      continue;
    var chip := TRectangle.Create(flow);
    chip.Parent := flow;
    chip.Height := ChipHeight;
    chip.XRadius := 10;
    chip.YRadius := 10;
    chip.Stroke.Thickness := 1;
    chip.Tag := Ord(info.Format);
    chip.OnClick := ChipClick;
    var lbl := TLabel.Create(chip);
    lbl.Parent := chip;
    lbl.Align := TAlignLayout.Left;
    lbl.Margins.Left := 12;
    lbl.AutoSize := True;
    lbl.Text := info.Name;
    StyleLabel(lbl, 13, clTextMuted, False, True);
    chip.Width := lbl.Width + 24;
    chip.TagObject := lbl;
  end;
end;

procedure TSettingsForm.StyleChip(chip: TRectangle);
var
  on: Boolean;
  lbl: TLabel;
begin
  on := (FConfig <> nil) and FConfig.Reads(TBarcodeFormat(chip.Tag));
  lbl := chip.TagObject as TLabel;
  if on then
  begin
    chip.Fill.Kind := TBrushKind.Solid;
    chip.Fill.Color := clCardRaised;
    chip.Stroke.Color := clAccent;
    lbl.TextSettings.FontColor := clAccent;
  end
  else
  begin
    // (filled in the color of the card, not without a fill: a shape
    // without a fill does not get the taps on its inside)
    chip.Fill.Kind := TBrushKind.Solid;
    chip.Fill.Color := clCard;
    chip.Stroke.Color := clBorder;
    lbl.TextSettings.FontColor := clTextMuted;
  end;
end;

procedure TSettingsForm.ChipClick(Sender: TObject);
var
  chip: TRectangle;
  format: TBarcodeFormat;
begin
  chip := Sender as TRectangle;
  format := TBarcodeFormat(chip.Tag);
  FConfig.SetFormat(format, not FConfig.Reads(format));
  StyleChip(chip);
  UpdateFormatCount;
end;

procedure TSettingsForm.lblResetClick(Sender: TObject);
begin
  // the formats of Auto
  FConfig.ResetFormats;
  LoadValues;
end;

procedure TSettingsForm.lblClearClick(Sender: TObject);
begin
  // no format at all (the scanner reads nothing then)
  FConfig.Formats.Clear;
  LoadValues;
end;

procedure TSettingsForm.SetExtraFormats(on: Boolean);
begin
  // the formats only read when asked for, all of them on or off
  for var info in Formats do
    if not info.InAuto then
      FConfig.SetFormat(info.Format, on);
  LoadValues;
end;

procedure TSettingsForm.lblExtraClearClick(Sender: TObject);
begin
  SetExtraFormats(False);
end;

procedure TSettingsForm.lblExtraAllClick(Sender: TObject);
begin
  SetExtraFormats(True);
end;

procedure TSettingsForm.UpdateFormatCount;
begin
  lblFormatCount.Text := Format('%d active', [FConfig.Formats.Count]);
end;

{ the color of the outline }

procedure TSettingsForm.AddDots;
begin
  for var i := 0 to High(TrackColors) do
  begin
    var dot := TCircle.Create(layDots);
    dot.Parent := layDots;
    dot.Align := TAlignLayout.Left;
    dot.Width := 24;
    dot.Margins.Rect := TRectF.Create(IfThen(i = 0, 0, 10), 14, 0, 14);
    dot.Position.X := i * 34;
    dot.Fill.Color := TrackColors[i];
    dot.Stroke.Kind := TBrushKind.Solid;
    dot.Stroke.Thickness := 2;
    dot.Tag := i;
    dot.OnClick := DotClick;
  end;
  layDots.Width := High(TrackColors) * 34 + 24;
end;

procedure TSettingsForm.StyleDots;
begin
  // the chosen one has a white ring
  for var i := 0 to layDots.ControlsCount - 1 do
  begin
    var dot := layDots.Controls[i] as TCircle;
    if (FConfig <> nil) and (FConfig.TrackColor = TrackColors[dot.Tag]) then
      dot.Stroke.Color := clText
    else
      dot.Stroke.Color := clCard;
  end;
end;

procedure TSettingsForm.DotClick(Sender: TObject);
begin
  FConfig.TrackColor := TrackColors[TCircle(Sender).Tag];
  StyleDots;
end;

{ the values and the layout }

procedure TSettingsForm.LoadValues;
begin
  if FConfig = nil then
    exit;
  fLoading := True;
  try
    fSwitches[skInverted].IsChecked := FConfig.Inverted;
    fSwitches[skTryHarder].IsChecked := FConfig.TryHarder;
    fSwitches[skMulti].IsChecked := FConfig.MultiResult;
    fSwitches[skDownscale].IsChecked := FConfig.Downscale;
    fSwitches[skParallel].IsChecked := FConfig.Parallel;
    fSwitches[skErrors].IsChecked := FConfig.ShowErrors;
    fSwitches[skHaptic].IsChecked := FConfig.Haptic;
    fSwitches[skBeep].IsChecked := FConfig.Beep;
    for var i := 0 to flowAuto.ControlsCount - 1 do
      StyleChip(flowAuto.Controls[i] as TRectangle);
    for var i := 0 to flowExtra.ControlsCount - 1 do
      StyleChip(flowExtra.Controls[i] as TRectangle);
    StyleDots;
    UpdateFormatCount;
  finally
    fLoading := False;
  end;
end;

procedure TSettingsForm.FitRows(width: Single);
begin
  // a row as high as its title and its explanation, wrapped at the width
  // left beside the icon and the switch
  for var i := 0 to High(fRows) do
  begin
    var h := MeasureText(fRowTexts[i], fRowTexts[i].Text,
      width - 16 - 34 - 14 - 12 - 52 - 16, True).Height;
    fRows[i].Height := Max(66, 10 + 22 + 2 + h + 10);
  end;
end;

procedure TSettingsForm.FitFlow(flow: TFlowLayout; width: Single);
var
  x: Single;
  lines: Integer;
begin
  // as high as its lines of chips: a chip goes to the next line when it
  // does not fit, as the flow layout places them
  x := 0;
  lines := 1;
  for var i := 0 to flow.ControlsCount - 1 do
  begin
    var w := flow.Controls[i].Width;
    if (x > 0) and (x + w > width) then
    begin
      x := 0;
      Inc(lines);
    end;
    x := x + w + ChipGap;
  end;
  flow.Height := lines * ChipHeight + (lines - 1) * ChipGap;
end;

procedure TSettingsForm.FitCard(card: TRectangle);
var
  h: Single;
begin
  // as high as its rows
  h := card.Padding.Top + card.Padding.Bottom;
  for var i := 0 to card.ControlsCount - 1 do
  begin
    var c := card.Controls[i];
    if c.Visible then
      h := h + c.Height + c.Margins.Top + c.Margins.Bottom;
  end;
  card.Height := h;
end;

procedure TSettingsForm.LayoutCards;
var
  width: Single;
begin
  // the width of the chips' lines: the form without the margins of the
  // scroll box and the padding of the card
  width := ClientWidth - scroll.Margins.Left - scroll.Margins.Right;
  FitRows(width);
  width := width - cardFormats.Padding.Left - cardFormats.Padding.Right;
  FitFlow(flowAuto, width);
  FitFlow(flowExtra, width);
  FitCard(cardDecoding);
  FitCard(cardFormats);
  FitCard(cardOnHit);
end;

end.
