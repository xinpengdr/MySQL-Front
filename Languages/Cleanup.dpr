program Cleanup;

{$APPTYPE CONSOLE}

uses
  SysUtils, IniFiles, StrUtils, Classes;

var
  English: TMemIniFile;
  EnglishStrings: TStringList;
  F: TSearchRec;
  I: Integer;
  Language: TMemIniFile;
  Path: string;
  S: string;
  Strings: TStringList;
begin
  Path := '..\Languages\';

  English := TMemIniFile.Create(Path + 'English.ini');

  if (UpperCase(English.ReadString('Global', 'Type', '')) = 'LANGUAGE') then
  begin
    EnglishStrings := TStringList.Create();
    Strings := TStringList.Create();

    English.ReadSection('Strings', EnglishStrings);

    if ((EnglishStrings.Count > 0) and (FindFirst(Path + '*.ini', faAnyFile, F) = 0)) then
      repeat
        if (UpperCase(F.Name) <> UpperCase('English.ini')) then
        begin
          Write(F.Name + '...');
          Language := TMemIniFile.Create(Path + F.Name);
          if ((UpperCase(Language.ReadString('Global', 'Type', '')) = 'LANGUAGE') and Language.SectionExists('Strings')) then
          begin
            Language.ReadSection('Strings', Strings);

            for I := Strings.Count - 1 downto 0 do
              if (EnglishStrings.IndexOf(Strings[I]) < 0) then
                Language.DeleteKey('Strings', Strings[I]);
          end;
          Language.UpdateFile();
          Language.Free();

          WriteLn('  done.');
        end;
      until (FindNext(F) <> 0);
    FindClose(F);

    Strings.Free();
    EnglishStrings.Free();
  end;

  English.Free();
end.
