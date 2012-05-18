unit fWMain;

interface {********************************************************************}

uses
  Classes, Controls, StdCtrls, Forms, ComCtrls, IniFiles;

type
  TWMain = class(TForm)
    FMajor: TEdit;
    FUDMajor: TUpDown;
    FMinor: TEdit;
    FPatch: TEdit;
    FBuild: TEdit;
    FUDMinor: TUpDown;
    FUDPatch: TUpDown;
    FUDBuild: TUpDown;
    GVersionInfo: TGroupBox;
    FLVersion: TLabel;
    FBCancel: TButton;
    FBOk: TButton;
    FBeta: TCheckBox;
    procedure FBCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FBOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FMajorChange(Sender: TObject);
    procedure FMinorChange(Sender: TObject);
    procedure FPatchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  WMain: TWMain;
  Ini: TIniFile;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils, SysUtils, Windows, ShellAPI, XMLDoc, XMLIntf;

function RFCDate(): String;
const
  EnglishLongMonthNames : Array[1..12] of PChar
    = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  EnglishShortDayNames : Array[1..7] of PChar
    = ('Sun', 'Mon', 'Thu', 'Wed', 'Thu', 'Fri', 'Sat');
var
  TempLongMonthNames: Array[1..12] of String;
  TempShortDayNames: Array[1..12] of String;
  Month, Day: Byte;
  SystemTime: _SYSTEMTIME;
  DateTime: TDateTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    DateTime := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
  for Month:=1 to 12 do TempLongMonthNames[Month]:=LongMonthNames[Month];
  for Month:=1 to 12 do LongMonthNames[Month]:=EnglishLongMonthNames[Month];
  for Day:=1 to 7 do TempShortDayNames[Day]:=ShortDayNames[Day];
  for Day:=1 to 7 do ShortDayNames[Day]:=EnglishShortDayNames[Day];
  Result := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', DateTime) + ' GMT';
  for Day:=1 to 7 do ShortDayNames[Day]:=TempShortDayNames[Day];
  for Month:=1 to 12 do LongMonthNames[Month]:=TempLongMonthNames[Month];
end;

{ TWMain **********************************************************************}

procedure TWMain.FormCreate(Sender: TObject);
var
  Filename: string;
begin
  Filename := IncludeTrailingPathDelimiter(GetEnvironmentVariable('BuildRootPath')) + 'Build_Setup.ini';
  if (not FileExists(Filename)) then
  begin
    Ini := nil;
    MessageBox(0, PChar('File not found: ' + Filename), 'Error', MB_OK + MB_ICONERROR);
    ExitCode := 1;
  end
  else
    Ini := TIniFile.Create(Filename);
end;

procedure TWMain.FormDestroy(Sender: TObject);
begin
  if (Assigned(Ini)) then
    Ini.Free();
end;

procedure TWMain.FormShow(Sender: TObject);
begin
  Caption := 'Build ' + GetEnvironmentVariable('BuildName');

  FUDMajor.Position := Ini.ReadInteger('Version', 'Major', 0);
  FUDMinor.Position := Ini.ReadInteger('Version', 'Minor', 0);
  FUDPatch.Position := Ini.ReadInteger('Version', 'Patch', 0);
  FUDBuild.Position := Ini.ReadInteger('Version', 'Build', 0);
  FBeta.Checked := Ini.ReadBool('Version', 'Beta', False);
end;

procedure TWMain.FBOkClick(Sender: TObject);
begin
  Close();
end;

procedure TWMain.FBCancelClick(Sender: TObject);
begin
  Close();
end;

procedure TWMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  MakeVerMajor, MakeVerMinor, MakeVerPatch, MakeVerBuild: Integer;
  MakeVerStr, MakeVerStrFull, MakeVerFileFlag: String;
  MakeVerYear, MakeVerMonth, MakeVerDay: Word;
  AwkFile: TStringList;
begin
  if (ModalResult <> mrOk) then
    ExitCode := 1
  else
  begin
    if ((FUDMajor.Position <= Ini.ReadInteger('Version', 'Major', 0))
      and (FUDMinor.Position <= Ini.ReadInteger('Version', 'Minor', 0))
      and (FUDPatch.Position <= Ini.ReadInteger('Version', 'Patch', 0))
      and (FUDBuild.Position <= Ini.ReadInteger('Version', 'Build', 0)) ) then
      CanClose := MessageBox(Handle, 'This version has been built before. Do you want to build it again?', 'Confirmation', MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES;
    if (CanClose) then
    begin
      MakeVerMajor := FUDMajor.Position;
      MakeVerMinor := FUDMinor.Position;
      MakeVerPatch := FUDPatch.Position;
      MakeVerBuild := FUDBuild.Position;
      MakeVerStr := IntToStr(MakeVerMajor) + '.' + IntToStr(MakeVerMinor);
      MakeVerStrFull := MakeVerStr + '.' + IntToStr(MakeVerPatch) + '.' + IntToStr(MakeVerBuild);
      DecodeDate(Date(), MakeVerYear, MakeVerMonth, MakeVerDay);


      Ini.WriteInteger('Version', 'Major', FUDMajor.Position);
      Ini.WriteInteger('Version', 'Minor', FUDMinor.Position);
      Ini.WriteInteger('Version', 'Patch', FUDPatch.Position);
      Ini.WriteInteger('Version', 'Build', FUDBuild.Position);
      Ini.WriteBool('Version', 'Beta', FBeta.Checked);

      if (not FBeta.Checked) then
        MakeVerFileFlag := ''
      else
        MakeVerFileFlag := 'FILEFLAGS 0x02L'; // VS_FF_PRERELEASE

      AwkFile := TStringList.Create();

      AwkFile.Add('{');
      AwkFile.Add('  gsub("{BuildName}", "' + GetEnvironmentVariable('BuildName') + '", $0)');
      AwkFile.Add('  gsub("{BuildProgramFiles}", "' + StringReplace(Ini.ReadString('Global', 'ProgramFiles', ''), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildAppData}", "' + StringReplace(Ini.ReadString('Global', 'AppData', ''), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildRegistry}", "' + StringReplace(Ini.ReadString('Global', 'Registry', ''), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildVerMajor}", "' + IntToStr(MakeVerMajor) + '", $0)');
      AwkFile.Add('  gsub("{BuildVerMinor}", "' + IntToStr(MakeVerMinor) + '", $0)');
      AwkFile.Add('  gsub("{BuildVerPatch}", "' + IntToStr(MakeVerPatch) + '", $0)');
      AwkFile.Add('  gsub("{BuildVerBuild}", "' + IntToStr(MakeVerBuild) + '", $0)');
      AwkFile.Add('  gsub("{BuildVerStr}", "' + MakeVerStr + '", $0)');
      AwkFile.Add('  gsub("{BuildVerStrFull}", "' + MakeVerStrFull + '", $0)');
      AwkFile.Add('  gsub("{BuildVerFileFlag}", "' + MakeVerFileFlag + '", $0)');
      AwkFile.Add('  gsub("{BuildVerYear}", "' + ReplaceStr(Format('%4d', [MakeVerYear]), ' ', '0') + '", $0)');
      AwkFile.Add('  gsub("{BuildVerMonth}", "' + ReplaceStr(Format('%2d', [MakeVerMonth]), ' ', '0') + '", $0)');
      AwkFile.Add('  gsub("{BuildVerDay}", "' + ReplaceStr(Format('%2d', [MakeVerDay]), ' ', '0') + '", $0)');
      AwkFile.Add('  gsub("{BuildInternetHomepage}", "' + Ini.ReadString('Internet', 'Homepage', '') + '", $0)');
      AwkFile.Add('  gsub("{BuildInternetDownload}", "' + Ini.ReadString('Internet', 'Download', '') + '", $0)');
      AwkFile.Add('  gsub("{BuildInternetDownloadPage}", "' + Ini.ReadString('Internet', 'DownloadPage', '') + '", $0)');
      AwkFile.Add('  gsub("{BuildInternetPadFile}", "' + Ini.ReadString('Internet', 'PadFile', '') + '", $0)');
      AwkFile.Add('  gsub("{BuildInternetPadFileIcon}", "' + Ini.ReadString('Internet', 'PadFileIcon', '') + '", $0)');
      AwkFile.Add('  gsub("{BuildInternetPadFileScreenshot}", "' + Ini.ReadString('Internet', 'PadFileScreenshot', '') + '", $0)');
      AwkFile.Add('  gsub("{BuildImagesPath}", "' + StringReplace(GetEnvironmentVariable('BuildImagesPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildLanguagesPath}", "' + StringReplace(GetEnvironmentVariable('BuildLanguagesPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildManualPath}", "' + StringReplace(GetEnvironmentVariable('BuildManualPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildPublishPath}", "' + StringReplace(GetEnvironmentVariable('BuildPublishPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildRootPath}", "' + StringReplace(GetEnvironmentVariable('BuildRootPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildSetupPath}", "' + StringReplace(GetEnvironmentVariable('BuildSetupPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildSkinsPath}", "' + StringReplace(GetEnvironmentVariable('BuildSkinsPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildSourcePath}", "' + StringReplace(GetEnvironmentVariable('BuildSourcePath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  gsub("{BuildTempPath}", "' + StringReplace(GetEnvironmentVariable('BuildTempPath'), '\', '\\', [rfReplaceAll]) + '", $0)');
      AwkFile.Add('  printf("%s\n", $0)');
      AwkFile.Add('}');

      AwkFile.SaveToFile(ExcludeTrailingPathDelimiter(GetEnvironmentVariable('BuildTempPath')) + '\Build_Setup.awk');
      FreeAndNil(AwkFile);
    end;
  end;
end;

procedure TWMain.FMajorChange(Sender: TObject);
begin
  FUDMinor.Position := 0;
end;

procedure TWMain.FMinorChange(Sender: TObject);
begin
  FUDPatch.Position := 0;
end;

procedure TWMain.FPatchChange(Sender: TObject);
begin
  FUDBuild.Position := 0;
end;

end.

