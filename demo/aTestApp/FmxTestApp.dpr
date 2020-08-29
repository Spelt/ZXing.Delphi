program FmxTestApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
