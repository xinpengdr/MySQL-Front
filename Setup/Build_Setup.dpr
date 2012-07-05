program Build_Setup;

uses
  Forms,
  fWMain in 'fWMain.pas' {WMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWMain, WMain);
  Application.Run;
end.

