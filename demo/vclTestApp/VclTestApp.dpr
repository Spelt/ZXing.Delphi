program VclTestApp;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {mainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmainFrm, mainFrm);
  Application.Run;
end.
