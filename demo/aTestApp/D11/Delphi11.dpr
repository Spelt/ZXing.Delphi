program Delphi11;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in '..\main.pas' {MainForm};

{$R *.res}

begin
{$IF CompilerVersion <> 35.0} // Delphi 11 or more
{$ERROR} // please, revert all changes and run it on Delphi 11 ¬¬
// remind that every project is created in some Delphi version and must run just on this version
// 95% of the cases it won't broke, but who does want to be the 5%?
{$IFEND}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
