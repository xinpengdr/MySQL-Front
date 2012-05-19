program Build_Setup;

uses
  Windows,
  Forms,
  Inifiles,
  SysUtils,
  Classes,
  StrUtils,
  fWMain in 'fWMain.pas' {WMain};

{$R *.res}

var
  IniFilename, SetupExeName, PADFileName, S: String;
  F: TSearchRec;
  StringList: TStringList;
begin
  if (UpperCase(ParamStr(1)) = '/PAD_FILE') then
  begin
    SetupExeName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('BuildTempPath')) + GetEnvironmentVariable('BuildName') + '_Setup.exe';
    PADFileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('BuildTempPath')) + 'pad_file.xml';

    if (not FileExists(PADFileName)) then
    begin
      MessageBox(0, PChar('File not found: ' + PADFileName), 'Error', MB_OK + MB_ICONERROR);
      ExitCode := 1;
    end
    else if (FindFirst(SetupExeName, faAnyFile, F) <> 0) then
    begin
      MessageBox(0, PChar('Cannot open file: ' + SetupExeName), 'Error', MB_OK + MB_ICONERROR);
      ExitCode := 1;
    end
    else
    begin
      StringList := TStringList.Create();
      StringList.LoadFromFile(PADFileName);
      S := StringList.Text;
      S := StringReplace(S, '{BuildPADFileSizeB}', IntToStr(F.Size), [rfReplaceAll]);
      S := StringReplace(S, '{BuildPADFileSizeKB}', IntToStr(F.Size div 1024), [rfReplaceAll]);
      S := StringReplace(S, '{BuildPADFileSizeMB}', IntToStr(F.Size div 1024 div 1024), [rfReplaceAll]);
      S := StringReplace(S, '{BuildPADFileSizeGB}', IntToStr(F.Size div 1024 div 1024 div 1024), [rfReplaceAll]);
      StringList.Text := S;
      StringList.SaveToFile(PADFileName);
      FreeAndNil(StringList);
    end;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TWMain, WMain);
  if (ExitCode = 0) then
      Application.Run;
  end;

  Halt(ExitCode);
end.

