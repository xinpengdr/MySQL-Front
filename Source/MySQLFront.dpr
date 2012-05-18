program MySQLFront;

uses
  FastMM4,
  {$IFDEF EurekaLog}
  ExceptionLog,
  {$ENDIF}
  Windows,
  ShellAPI,
  SysUtils,
  Forms,
  MySQLClient in 'MySQL-DataSet\Source\MySQLClient.pas',
  MySQLConsts in 'MySQL-DataSet\Source\MySQLConsts.pas',
  MySQLDB in 'MySQL-DataSet\Source\MySQLDB.pas',
  HTTPTunnel in 'MySQL-DataSet\Source\HTTPTunnel.pas',
  SQLUtils in 'MySQL-DataSet\Source\SQLUtils.pas',
  CSVUtils in 'MySQL-DataSet\Source\CSVUtils.pas',
  MySQLDBGrid in 'MySQL-DataSet\Source\MySQLDBGrid.pas',
  ExtCtrls_Ext in 'VCL\Source\ExtCtrls_Ext.pas',
  StdCtrls_Ext in 'VCL\Source\StdCtrls_Ext.pas',
  StdActns_Ext in 'VCL\Source\StdActns_Ext.pas',
  Dialogs_Ext in 'VCL\Source\Dialogs_Ext.pas',
  ComCtrls_Ext in 'VCL\Source\ComCtrls_Ext.pas',
  CommCtrl_Ext in 'VCL\Source\CommCtrl_Ext.pas',
  Forms_Ext in 'VCL\Source\Forms_Ext.pas',
  fAccount in 'fAccount.pas',
  fClient in 'fClient.pas',
  fPreferences in 'fPreferences.pas',
  fTools in 'fTools.pas',
  fURI in 'fURI.pas',
  fBase in 'fBase.pas',
  fDAccount in 'fDAccount.pas' {DAccount},
  fDAccounts in 'fDAccounts.pas' {DAccounts},
  fDBookmark in 'fDBookmark.pas' {DBookmark},
  fDColumns in 'fDColumns.pas' {DColumns},
  fDConnecting in 'fDConnecting.pas' {DConnecting},
  fDDatabase in 'fDDatabase.pas' {DDatabase},
  fDDatabases in 'fDDatabases.pas' {DDatabases},
  fDEvent in 'fDEvent.pas' {DEvent},
  fDExport in 'fDExport.pas' {DExport},
  fDField in 'fDField.pas' {DField},
  fDForeignKey in 'fDForeignKey.pas' {DForeignKey},
  fDGoto in 'fDGoto.pas' {DGoto},
  fDHost in 'fDHost.pas' {DHost},
  fDHostDatabase in 'fDHostDatabase.pas' {DHostDatabase},
  fDImport in 'fDImport.pas' {DImport},
  fDIndex in 'fDIndex.pas' {DIndex},
  fDInfo in 'fDInfo.pas' {DInfo},
  fDInstallUpdate in 'fDInstallUpdate.pas' {DInstallUpdate},
  fDLogin in 'fDLogin.pas' {DDBLogin},
  fDOptions in 'fDOptions.pas' {DOptions},
  fDPartition in 'fDPartition.pas' {DPartition},
  fDPaste in 'fDPaste.pas' {DPaste},
  fDQuickFilter in 'fDQuickFilter.pas' {DQuickFilter},
  fDRoutine in 'fDRoutine.pas' {DRoutine},
  fDSearch in 'fDSearch.pas' {DSearch},
  fDSegment in 'fDSegment.pas' {DSegment},
  fDSelection in 'fDSelection.pas' {DSelection},
  fDServer in 'fDServer.pas' {DServer},
  fDSQLHelp in 'fDSQLHelp.pas' {DSQLHelp},
  fDStatement in 'fDStatement.pas' {DStatement},
  fDTable in 'fDTable.pas' {DTable},
  fDTableService in 'fDTableService.pas' {DTableService},
  fDTransfer in 'fDTransfer.pas' {DTransfer},
  fDTrigger in 'fDTrigger.pas' {DTrigger},
  fDUser in 'fDUser.pas' {DUser},
  fDUserRight in 'fDUserRight.pas' {DUserRight},
  fDVariable in 'fDVariable.pas' {DVariable},
  fDView in 'fDView.pas' {DView},
  fCWorkbench in 'fCWorkbench.pas',
  fFClient in 'fFClient.pas' {FClient},
  fWWindow in 'fWWindow.pas' {WWindow},
  fWForeignKeySelect in 'fWForeignKeySelect.pas' {WForeignKeySelect};

{$R *.res}

begin
  Preferences := TPPreferences.Create();

  if ((Preferences.SetupProgram <> '') and not Preferences.SetupProgramInstalled and (FindWindow(cWindowClassName + '.UnicodeClass', nil) = 0)) then
    Preferences.SetupProgramInstalled := ShellExecute(0, 'open', PChar(Preferences.SetupProgram), '/SILENT /NOICONS /TASKS=""', nil, SW_SHOW) >= 32
  else
  begin
    if (Preferences.SetupProgramInstalled) then
    begin
      if (FileExists(PChar(Preferences.SetupProgram))) then
        DeleteFile(PChar(Preferences.SetupProgram));
      Preferences.SetupProgram := '';
      Preferences.SetupProgramInstalled := False;
    end;

    Application.Initialize();
    Application.Title := LoadStr(1000);
    {$IFDEF Debug}
      if (Application.Title = '') then
        Application.Title := Copy(ExtractFileName(Application.ExeName), 1, Length(ExtractFileName(Application.ExeName)) - Length(ExtractFileExt(Application.ExeName)));
    {$ENDIF}
    Application.Icon.Handle := LoadImage(hInstance, 'MAINICON', IMAGE_ICON, Application.Icon.Height, Application.Icon.Height, LR_DEFAULTCOLOR);
    {$IFDEF Debug}
      if (Application.Icon.Handle = 0) then
        Application.Icon.Handle := LoadImage(hInstance, PChar('..\Images\MySQLFront.ico'), IMAGE_ICON, Application.Icon.Height, Application.Icon.Height, LR_DEFAULTCOLOR + LR_LOADFROMFILE);
    {$ENDIF}
    Application.CreateForm(TWWindow, WWindow);
    Application.MainForm.Perform(CM_CHANGEPREFERENCES, 0, 0);
    Application.Run();
    if (Application.Handle <> 0) then
      ShowOwnedPopups(Application.Handle, False);
    Application.ShowHint := False;
    Application.Destroying();
    Application.DestroyComponents();
  end;

  Preferences.Free();
end.

