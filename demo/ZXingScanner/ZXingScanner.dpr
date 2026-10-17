program ZXingScanner;

{
  ZXing Scanner: the demo app of ZXing.Delphi. Two screens, the scan
  screen and its settings, with the standard FMX components only.
  See README.md.
}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Scanner.Main in 'Scanner.Main.pas' {MainForm},
  Scanner.Settings in 'Scanner.Settings.pas' {SettingsForm},
  Scanner.Config in 'Scanner.Config.pas',
  Scanner.Formats in 'Scanner.Formats.pas',
  Scanner.Theme in 'Scanner.Theme.pas',
  Scanner.Feedback in 'Scanner.Feedback.pas',
  Scanner.Tracking in 'Scanner.Tracking.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseMetal := True;
  GlobalUseVulkan := True;
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
