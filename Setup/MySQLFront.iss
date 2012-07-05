[Setup]
MinVersion=0,5.1
AppName={BuildName}
AppVerName={BuildName} {BuildVerStr}
AppPublisherURL={BuildInternetHomepage}
AppVersion={BuildVerStr}
DefaultDirName={pf}\{BuildProgramFiles}
DefaultGroupName={BuildName}
ChangesAssociations=yes
WizardImageFile={BuildImagesPath}\Setup.bmp
WizardSmallImageFile={BuildImagesPath}\Setup_Header.bmp
AllowNoIcons=yes
ShowLanguageDialog=auto
VersionInfoVersion={BuildVerStrFull}
SolidCompression=yes
UninstallDisplayName={BuildName}
UninstallDisplayIcon={app}\{BuildName}.exe

[Languages]
Name: "English"; MessagesFile: "compiler:Default.isl"
Name: "Arabic"; MessagesFile: "compiler:Languages\Arabic.isl"
Name: "Bulgarian"; MessagesFile: "compiler:Languages\Bulgarian.isl"
Name: "Catalan"; MessagesFile: "compiler:Languages\Catalan.isl"
Name: "ChineseSimp"; MessagesFile: "compiler:Languages\ChineseSimp.isl"
Name: "ChineseTrad"; MessagesFile: "compiler:Languages\ChineseTrad.isl"
Name: "Croatian"; MessagesFile: "compiler:Languages\Croatian.isl"
Name: "Czech"; MessagesFile: "compiler:Languages\Czech.isl"
Name: "Danish"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "Dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "French"; MessagesFile: "compiler:Languages\French.isl"
Name: "German"; MessagesFile: "compiler:Languages\German.isl"
Name: "Greek"; MessagesFile: "compiler:Languages\Greek.isl"
Name: "Hungarian"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "Indonesian"; MessagesFile: "compiler:Languages\Indonesian.isl"
Name: "Italian"; MessagesFile: "compiler:Languages\Italian.isl"
Name: "Japanese"; MessagesFile: "compiler:Languages\Japanese.isl"
Name: "Korean"; MessagesFile: "compiler:Languages\Korean.isl"
Name: "Lithuanian"; MessagesFile: "compiler:Languages\Lithuanian.isl"
Name: "Norwegian"; MessagesFile: "compiler:Languages\Norwegian.isl"
Name: "Portugues_Brazil"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
Name: "Polish"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "Romanian"; MessagesFile: "compiler:Languages\Romanian.isl"
Name: "Serbian"; MessagesFile: "compiler:Languages\Serbian.isl"
Name: "Slovak"; MessagesFile: "compiler:Languages\Slovak.isl"
Name: "Spanish"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "Swedish"; MessagesFile: "compiler:Languages\Swedish.isl"
Name: "Thai"; MessagesFile: "compiler:Languages\Thai.isl"
Name: "Turkish"; MessagesFile: "compiler:Languages\Turkish.isl"
Name: "Russian"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "Ukrainian"; MessagesFile: "compiler:Languages\Ukrainian.isl"

[Tasks]
Name: DesktopIcon; Description: "&Desktop Icon"; GroupDescription: "Icons:";
Name: AssociateSQL; Description: "Associate .sql (SQL File) with {BuildName}"; GroupDescription: "Explorer integration:";

[Registry]
Root: HKCU; Subkey: "Software\{BuildName}"; ValueType: string; ValueName: "LanguageFile"; ValueData: "{language}.ini"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCR; Subkey: ".sql"; ValueType: string; ValueName: ""; ValueData: "SQLFile"; Tasks: AssociateSQL; Flags: uninsdeletevalue
Root: HKCR; Subkey: "SQLFile"; ValueType: string; ValueName: ""; ValueData: "SQL Script"; Tasks: AssociateSQL; Flags: uninsdeletekey
Root: HKCR; Subkey: "SQLFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{BuildName}.exe,0"; Tasks: AssociateSQL; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCR; Subkey: "SQLFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{BuildName}.exe"" ""%1"""; Tasks: AssociateSQL; Flags: uninsdeletevalue
Root: HKCR; Subkey: "mysql"; ValueType: string; ValueName: ""; ValueData: "URL:mysql (MySQL Protocol)"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "mysql"; ValueType: string; ValueName: "URL Protocol"; ValueData: ""; Flags: uninsdeletekey
Root: HKCR; Subkey: "mysql\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{BuildName}.exe,0"; Flags: createvalueifdoesntexist uninsdeletevalue
Root: HKCR; Subkey: "mysql\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{BuildName}.exe"" ""%1"""; Flags: uninsdeletevalue

[Dirs]
Name: "{userappdata}\{BuildName}"
Name: "{userappdata}\{BuildName}\Accounts"

[Files]
Source: "{BuildTempPath}\{BuildName}.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "{BuildTempPath}\{BuildName}.chm"; DestDir: "{app}"
Source: "{BuildTempPath}\libMySQL.php"; DestDir: "{app}"
Source: "{BuildLanguagesPath}\*.ini"; DestDir: "{app}\Languages"; Flags: comparetimestamp

[Icons]
Name: "{group}\{BuildName}"; Filename: "{app}\{BuildName}.exe";
Name: "{group}\Help"; Filename: "{app}\{BuildName}.chm";
Name: "{userdesktop}\{BuildName}"; Filename: "{app}\{BuildName}.exe"; Tasks: DesktopIcon;

[Run]
Filename: "{app}\{BuildName}.exe"; Description: "&Launch {BuildName}"; Flags: postinstall nowait


