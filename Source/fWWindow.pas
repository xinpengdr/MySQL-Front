unit fWWindow;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ComCtrls, DBActns, ExtCtrls, ImgList, Menus, StdActns,
  ActnCtrls, StdCtrls, ToolWin, HtmlHelpViewer,
  {$IFDEF EurekaLog}
  ExceptionLog,
  {$ENDIF}
  SynEditHighlighter, SynHighlighterSQL,
  ExtCtrls_Ext, Forms_Ext, StdCtrls_Ext, ComCtrls_Ext, Dialogs_Ext, StdActns_Ext,
  fClient, fPreferences, fFClient, fAccount, fBase, MySQLDB;

const
  cWindowClassName = 'MySQL-Front.Application';

  CM_ACTIVATETAB = WM_USER + 600;
  CM_UPDATEAVAILABLE = WM_USER + 601;
  CM_MYSQLCONNECTION_SYNCHRONIZE = WM_USER + 602;

type
  TCMAddTab = packed record
    Msg: Cardinal;
    WParam: WPARAM;
    Param: PChar;
    Result: Longint;
  end;

  TCMActivateTab = packed record
    Msg: Cardinal;
    WParam: WPARAM;
    Tab: TFClient;
    Result: Longint;
  end;

  TCMCloseTab = TCMActivateTab;

  TCMUpdateToolBar = TCMActivateTab;

  TWWindow = class (TForm_Ext)
    aBAdd: TAction;
    aBDelete: TAction;
    aBEdit: TAction;
    aBookmark: TAction;
    ActionList: TActionList;
    aDAutoCommit: TAction;
    aDCancel: TAction;
    aDCancelRecord: TDataSetCancel;
    aDCommit: TAction;
    aDCreateDatabase: TAction;
    aDCreateEvent: TAction;
    aDCreateField: TAction;
    aDCreateForeignKey: TAction;
    aDCreateFunction: TAction;
    aDCreateFunction1: TMenuItem;
    aDCreateHost: TAction;
    aDCreateIndex: TAction;
    aDCreateProcedure: TAction;
    aDCreateTable: TAction;
    aDCreateTrigger: TAction;
    aDCreateUser: TAction;
    aDCreateView: TAction;
    aDDeleteDatabase: TAction;
    aDDeleteEvent: TAction;
    aDDeleteField: TAction;
    aDDeleteForeignKey: TAction;
    aDDeleteHost: TAction;
    aDDeleteIndex: TAction;
    aDDeleteProcess: TAction;
    aDDeleteRecord: TAction;
    aDDeleteRoutine: TAction;
    aDDeleteTable: TAction;
    aDDeleteTrigger: TAction;
    aDDeleteUser: TAction;
    aDDeleteView: TAction;
    aDEditDatabase: TAction;
    aDEditEvent: TAction;
    aDEditField: TAction;
    aDEditForeignKey: TAction;
    aDEditHost: TAction;
    aDEditIndex: TAction;
    aDEditProcess: TAction;
    aDEditRecord: TAction;
    aDEditRoutine: TAction;
    aDEditServer: TAction;
    aDEditTable: TAction;
    aDEditTrigger: TAction;
    aDEditUser: TAction;
    aDEditVariable: TAction;
    aDEditView: TAction;
    aDEmpty: TAction;
    aDInsertRecord: TAction;
    aDPostObject: TAction;
    aDPostObject1: TMenuItem;
    aDPostRecord: TDataSetPost;
    aDRollback: TAction;
    aDRun: TAction;
    aDRunSelection: TAction;
    aECopy: TEditCopy;
    aECopyToFile: TAction;
    aECut: TEditCut;
    aEDelete: TEditDelete;
    aEFind: TAction;
    aEPaste: TEditPaste;
    aEPasteFrom1: TMenuItem;
    aEPasteFromFile: TAction;
    aERedo: TAction;
    aERename: TAction;
    aEReplace: TAction;
    aESelectAll: TEditSelectAll;
    aETransfer: TAction;
    aEUndo: TEditUndo;
    aFClose: TAction;
    aFCloseAll: TAction;
    aFCloseAll1: TMenuItem;
    aFExit: TAction;
    aFExportAccess: TAction;
    aFExportBitmap: TAction;
    aFExportExcel: TAction;
    aFExportHTML: TAction;
    aFExportODBC: TAction;
    aFExportODBC1: TMenuItem;
    aFExportSQL: TAction;
    aFExportSQLite: TAction;
    aFExportText: TAction;
    aFExportXML: TAction;
    aFImportAccess: TAction;
    aFImportExcel: TAction;
    aFImportODBC: TAction;
    aFImportSQL: TAction;
    aFImportSQLite: TAction;
    aFImportText: TAction;
    aFImportXML: TAction;
    aFOpen: TAction;
    aFOpenAccount: TAction;
    aFPrint: TAction;
    aFSave: TAction;
    aFSaveAs: TAction;
    aHIndex: TAction;
    aHInfo: TAction;
    aHManual: TAction;
    aHUpdate: TAction;
    aOGlobals: TAction;
    aOAccounts: TAction;
    aSAddress: TAction;
    aSGoto: TAction;
    aSSearchFind: TSearchFind_Ext;
    aSSearchNext: TSearchFindNext;
    aSSearchReplace: TSearchReplace_Ext;
    aVAddress: TAction;
    aVAddressBar: TAction;
    aVBookmarks: TAction;
    aVDataBrowser: TAction;
    aVDetails: TAction;
    aVDetails1: TMenuItem;
    aVDiagram: TAction;
    aVExplorer: TAction;
    aVNavigator: TAction;
    aVNext: TAction;
    aVObjectBrowser: TAction;
    aVObjectIDE: TAction;
    aVPrev: TAction;
    aVQueryBuilder: TAction;
    aVRefresh: TAction;
    aVRefreshAll: TAction;
    aVSQLEditor: TAction;
    aVSQLHistory: TAction;
    aVSQLHistory1: TMenuItem;
    aVSQLLog: TAction;
    FAddress: TComboBox_Ext;
    FAddressApply: TToolButton;
    Highlighter: TSynSQLSyn;
    MainMenu: TMainMenu;
    miBAdd: TMenuItem;
    miBDelete: TMenuItem;
    miBEdit: TMenuItem;
    miBookmarks: TMenuItem;
    miBSeparator: TMenuItem;
    miDatabase: TMenuItem;
    miDAutoCommit: TMenuItem;
    miDCancelRecord: TMenuItem;
    miDCommit: TMenuItem;
    miDCreate: TMenuItem;
    miDCreateDatabase: TMenuItem;
    miDCreateEvent: TMenuItem;
    miDCreateField: TMenuItem;
    miDCreateForeignKey: TMenuItem;
    miDCreateHost: TMenuItem;
    miDCreateIndex: TMenuItem;
    miDCreateRoutine: TMenuItem;
    miDCreateTable: TMenuItem;
    miDCreateTrigger: TMenuItem;
    miDCreateUser: TMenuItem;
    miDCreateView: TMenuItem;
    miDDelete: TMenuItem;
    miDDeleteDatabase: TMenuItem;
    miDDeleteEvent: TMenuItem;
    miDDeleteField: TMenuItem;
    miDDeleteForeignKey: TMenuItem;
    miDDeleteHost: TMenuItem;
    miDDeleteIndex: TMenuItem;
    miDDeleteProcess: TMenuItem;
    miDDeleteRecord: TMenuItem;
    miDDeleteRoutine: TMenuItem;
    miDDeleteTable: TMenuItem;
    miDDeleteTrigger: TMenuItem;
    miDDeleteUser: TMenuItem;
    miDDeleteView: TMenuItem;
    miDEditDatabase: TMenuItem;
    miDEditEvent: TMenuItem;
    miDEditField: TMenuItem;
    miDEditForeignKey: TMenuItem;
    miDEditHost: TMenuItem;
    miDEditIndex: TMenuItem;
    miDEditProcess: TMenuItem;
    miDEditRecord: TMenuItem;
    miDEditRoutine: TMenuItem;
    miDEditServer: TMenuItem;
    miDEditTable: TMenuItem;
    miDEditTrigger: TMenuItem;
    miDEditUser: TMenuItem;
    miDEditVariable: TMenuItem;
    miDEditView: TMenuItem;
    miDEmpty: TMenuItem;
    miDInsertRecord: TMenuItem;
    miDPostRecord: TMenuItem;
    miDProperties: TMenuItem;
    miDRollback: TMenuItem;
    miDRun: TMenuItem;
    miDRunSelection: TMenuItem;
    miECopy: TMenuItem;
    miECopyToFile: TMenuItem;
    miECut: TMenuItem;
    miEDelete: TMenuItem;
    miEdit: TMenuItem;
    miEFind: TMenuItem;
    miEPaste: TMenuItem;
    miERedo: TMenuItem;
    miERename: TMenuItem;
    miEReplace: TMenuItem;
    miESelectAll: TMenuItem;
    miETransfer: TMenuItem;
    miEUndo: TMenuItem;
    miExtras: TMenuItem;
    miFClose: TMenuItem;
    miFConnect: TMenuItem;
    miFExit: TMenuItem;
    miFExport: TMenuItem;
    miFExportAccess: TMenuItem;
    miFExportBitmap: TMenuItem;
    miFExportExcel: TMenuItem;
    miFExportHTML: TMenuItem;
    miFExportSQL: TMenuItem;
    miFExportText: TMenuItem;
    miFExportXML: TMenuItem;
    miFile: TMenuItem;
    miFImport: TMenuItem;
    miFImportAccess: TMenuItem;
    miFImportExcel: TMenuItem;
    miFImportODBC: TMenuItem;
    miFImportSQL: TMenuItem;
    miFImportSQLite1: TMenuItem;
    miFImportText: TMenuItem;
    miFImportXML: TMenuItem;
    miFOpen: TMenuItem;
    miFPrint: TMenuItem;
    miFSave: TMenuItem;
    miFSaveAs: TMenuItem;
    miHelp: TMenuItem;
    miHIndex: TMenuItem;
    miHInfo: TMenuItem;
    miHManual: TMenuItem;
    miHUpdate: TMenuItem;
    miOGlobals: TMenuItem;
    miOptions: TMenuItem;
    miOAccounts: TMenuItem;
    miSAddress: TMenuItem;
    miSearch: TMenuItem;
    miSGoto: TMenuItem;
    miSSearchFind: TMenuItem;
    miSSearchNext: TMenuItem;
    miSSearchReplace: TMenuItem;
    miVAddressBar: TMenuItem;
    miVBookmarks: TMenuItem;
    miVBrowser: TMenuItem;
    miVDiagram: TMenuItem;
    miVExplorer: TMenuItem;
    miView: TMenuItem;
    miVNavigator: TMenuItem;
    miVObjectBrowser: TMenuItem;
    miVObjectIDE: TMenuItem;
    miVQueryBuilder: TMenuItem;
    miVRefresh: TMenuItem;
    miVRefreshAll: TMenuItem;
    miVSidebar: TMenuItem;
    miVSQLEditor: TMenuItem;
    miVSQLLog: TMenuItem;
    MNext: TPopupMenu;
    MPrev: TPopupMenu;
    MTabControl: TPopupMenu;
    mtDCommit: TMenuItem;
    mtDRollback: TMenuItem;
    mtFClose: TMenuItem;
    mtFCloseAll: TMenuItem;
    mtFOpenAccount: TMenuItem;
    mtTabs: TMenuItem;
    mtVRefresh: TMenuItem;
    mtVRefreshAll: TMenuItem;
    N10: TMenuItem;
    N12: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N2: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N26: TMenuItem;
    N27: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    OpenDialog: TOpenDialog_Ext;
    PAddressBar: TPanel_Ext;
    PToolBar: TPanel_Ext;
    PWorkSpace: TPanel_Ext;
    SaveDialog: TSaveDialog_Ext;
    StatusBar: TStatusBar;
    TabControl: TTabControl;
    TBAddressBar: TToolBar;
    tbCancelRecord: TToolButton;
    tbCreateDatabase: TToolButton;
    tbCreateField: TToolButton;
    tbCreateForeignKey: TToolButton;
    tbCreateIndex: TToolButton;
    tbCreateTable: TToolButton;
    tbDBFirst: TToolButton;
    tbDBLast: TToolButton;
    tbDBNext: TToolButton;
    tbDBPrev: TToolButton;
    tbDCancel: TToolButton;
    tbDDeleteRecord: TToolButton;
    tbDeleteDatabase: TToolButton;
    tbDeleteField: TToolButton;
    tbDeleteForeignKey: TToolButton;
    tbDeleteIndex: TToolButton;
    tbDeleteTable: TToolButton;
    tbDEmpty: TToolButton;
    tbDInsertRecord: TToolButton;
    tbECopy: TToolButton;
    tbECut: TToolButton;
    tbEDelete: TToolButton;
    tbEPaste: TToolButton;
    tbNext: TToolButton;
    tbOpen: TToolButton;
    tbPostObject: TToolButton;
    tbPostRecord: TToolButton;
    tbPrev: TToolButton;
    tbProperties: TToolButton;
    tbRedo: TToolButton;
    tbRun: TToolButton;
    tbRunSelection: TToolButton;
    tbSave: TToolButton;
    tbSearchFind: TToolButton;
    tbSearchReplace: TToolButton;
    TBTabControl: TToolBar;
    tbUndo: TToolButton;
    tbVRefresh: TToolButton;
    tcOpenAccount: TToolButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton2: TToolButton;
    ToolButton23: TToolButton;
    ToolButton26: TToolButton;
    ToolButton30: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton7: TToolButton;
    procedure aDCreateParentExecute(Sender: TObject);
    procedure aEFindExecute(Sender: TObject);
    procedure aEReplaceExecute(Sender: TObject);
    procedure aETransferExecute(Sender: TObject);
    procedure aFCloseAllExecute(Sender: TObject);
    procedure aFCloseExecute(Sender: TObject);
    procedure aFExitExecute(Sender: TObject);
    procedure aFOpenAccountExecute(Sender: TObject);
    procedure aHIndexExecute(Sender: TObject);
    procedure aHInfoExecute(Sender: TObject);
    procedure aHManualExecute(Sender: TObject);
    procedure aHUpdateExecute(Sender: TObject);
    procedure aOGlobalsExecute(Sender: TObject);
    procedure aOAccountsExecute(Sender: TObject);
    procedure aSAddressExecute(Sender: TObject);
    procedure aSSearchFindNotFound(Sender: TObject);
    procedure aVAddressBarExecute(Sender: TObject);
    procedure aVAddressExecute(Sender: TObject);
    procedure aVNextExecute(Sender: TObject);
    procedure aVPrevExecute(Sender: TObject);
    procedure FAddressDropDown(Sender: TObject);
    procedure FAddressKeyPress(Sender: TObject; var Key: Char);
    procedure FAddressSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HandleParam(const AParam: string);
    procedure MNextPopup(Sender: TObject);
    procedure MPrevPopup(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure TabControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TabControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TabControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TabControlDrawTab(Control: TCustomTabControl;
      TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure TabControlEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TabControlGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TabControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TabControlResize(Sender: TObject);
    procedure TabControlStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure TBAddressBarResize(Sender: TObject);
    procedure tbPropertiesClick(Sender: TObject);
  private
    CaptureTabIndex: Integer;
    CloseButton: TPicture;
    CloseButtonRects: array of TRect;
    {$IFDEF EurekaLog}
    EurekaLog: TEurekaLog;
    {$ENDIF}
    FAddressDroppedDown: Boolean;
    FirstOpen: Boolean;
    MouseDownPoint: TPoint;
    Param: string; // erforderlich für PostMessage
    PreviousForm: TForm;
    QuitAfterShow: Boolean;
    TabControlDragMarkedTabIndex: Integer;
    TabControlDragStartTabIndex: Integer;
    Tabs: TList;
    UniqueTabNameCounter: Integer;
    UpdateAvailable: Boolean;
    procedure ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
    procedure ApplicationModalBegin(Sender: TObject);
    procedure ApplicationModalEnd(Sender: TObject);
    {$IFDEF EurekaLog}
    procedure EurekaLogCustomDataRequest(
      EurekaExceptionRecord: TEurekaExceptionRecord; DataFields: TStrings);
    procedure EurekaLogExceptionNotify(
      EurekaExceptionRecord: TEurekaExceptionRecord; var Handled: Boolean);
    {$ENDIF}
    function GetActiveTab(): TFClient;
    function GetNewTabIndex(Sender: TObject; X, Y: Integer): Integer;
    procedure InformUpdateAvailable();
    procedure MNextItemClick(Sender: TObject);
    procedure MPrevItemClick(Sender: TObject);
    procedure mtTabsClick(Sender: TObject);
    procedure MySQLConnectionSynchronize(const Data: Pointer); inline;
    procedure SetActiveTab(const FClient: TFClient);
    procedure SQLError(const Connection: TMySQLConnection; const ErrorCode: Integer; const ErrorMessage: string);
    function TabCaption(const ACaption: string): string;
    procedure CMActivateTab(var Message: TCMActivateTab); message CM_ACTIVATETAB;
    procedure CMAddTab(var Message: TCMAddTab); message CM_ADDTAB;
    procedure CMBookmarkChanged(var Message: TMessage); message CM_BOOKMARKCHANGED;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMClientSynchronize(var Message: TMessage); message CM_MYSQLCONNECTION_SYNCHRONIZE;
    procedure CMCloseTab(var Message: TCMCloseTab); message CM_CLOSE_TAB;
    procedure CMDeactivateTab(var Message: TMessage); message CM_DEACTIVATETAB;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMUpdateAvailable(var Message: TMessage); message CM_UPDATEAVAILABLE;
    procedure CMUpdateToolbar(var Message: TCMUpdateToolBar); message CM_UPDATETOOLBAR;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
    property ActiveTab: TFClient read GetActiveTab write SetActiveTab;
  protected
    procedure ApplicationException(Sender: TObject; E: Exception);
    procedure CreateParams(var Params: TCreateParams); override;
  public
    destructor Destroy(); override;
    function DBLogin(const Account: Pointer): Boolean;
  end;

var
  WWindow: TWWindow;

implementation {***************************************************************}

{$R *.dfm}

uses
  ShellApi, ShlObj, Themes, DBConsts, CommCtrl, StrUtils, ShLwApi, IniFiles,
  MySQLConsts,
  HTTPTunnel,
  fDAccounts, fDAccount, fDOptions, fDLogin, fDStatement,
  fDTransfer, fDSearch, fDConnecting,
  fDInfo, fDInstallUpdate, fURI;

{ TWWindow ********************************************************************}

procedure TWWindow.aDCreateParentExecute(Sender: TObject);
begin
  ; // Dummy, damit MenuItem enabled
end;

procedure TWWindow.aEFindExecute(Sender: TObject);
begin
  if (Assigned(ActiveTab) and (ActiveTab is TFClient)) then
    TFClient(ActiveTab).aEFindExecute(Sender)
  else
  begin
    DSearch.Client := nil;
    DSearch.SearchOnly := True;
    DSearch.Frame := nil;
    DSearch.Execute();
  end;
end;

procedure TWWindow.aEReplaceExecute(Sender: TObject);
begin
  if (Assigned(ActiveTab) and (ActiveTab is TFClient)) then
    TFClient(ActiveTab).aEReplaceExecute(Sender)
  else
  begin
    DSearch.Client := nil;
    DSearch.SearchOnly := False;
    DSearch.Frame := nil;
    DSearch.Execute();
  end;
end;

procedure TWWindow.aETransferExecute(Sender: TObject);
begin
  if (Assigned(ActiveTab) and (ActiveTab is TFClient)) then
    TFClient(ActiveTab).aETransferExecute(Sender)
  else
  begin
    DTransfer.MasterClient := nil;
    DTransfer.SlaveClient := nil;
    DTransfer.Execute();
  end;
end;

procedure TWWindow.aFCloseAllExecute(Sender: TObject);
var
  CanClose: Boolean;
  I: Integer;
begin
  CanClose := True;
  for I := Tabs.Count - 1 downto 0 do
  begin
    CanClose := CanClose and (SendMessage(TFClient(Tabs[I]).Handle, CM_CLOSE_TAB_QUERY, 0, 0) = 1);
    if (CanClose) then
      Perform(CM_CLOSE_TAB, 0, LPARAM(TFClient(Tabs[I])));
  end;
end;

procedure TWWindow.aFCloseExecute(Sender: TObject);
begin
  if (Assigned(ActiveTab) and Boolean(SendMessage(ActiveTab.Handle, CM_CLOSE_TAB_QUERY, 0, 0))) then
    Perform(CM_CLOSE_TAB, 0, LPARAM(ActiveTab));
end;

procedure TWWindow.aFExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TWWindow.aFOpenAccountExecute(Sender: TObject);
begin
  Perform(CM_ADDTAB, 0, 0);
end;

procedure TWWindow.aHIndexExecute(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TWWindow.aHInfoExecute(Sender: TObject);
begin
  DInfo.ShowModal();
end;

procedure TWWindow.aHManualExecute(Sender: TObject);
begin
  if (Assigned(ActiveTab)) then
    ShellExecute(Application.Handle, 'open', PChar(ActiveTab.Client.Account.ManualURL), '', '', SW_SHOW);
end;

procedure TWWindow.aHUpdateExecute(Sender: TObject);
begin
  DInstallUpdate.Execute();
end;

procedure TWWindow.aOGlobalsExecute(Sender: TObject);
var
  I: Integer;
  TempCursor: TCursor;
begin
  if (DOptions.Execute()) then
  begin
    TempCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;

    TabControl.Visible := Preferences.TabsVisible or not Preferences.TabsVisible and (Tabs.Count >= 2);
    TBTabControl.Visible := Preferences.TabsVisible;
    for I := 0 to Screen.FormCount - 1 do
      PostMessage(Screen.Forms[I].Handle, CM_CHANGEPREFERENCES, 0, 0);

    Screen.Cursor := TempCursor;
  end;
end;

procedure TWWindow.aOAccountsExecute(Sender: TObject);
begin
  DAccounts.Account := nil;
  DAccounts.Open := False;
  DAccounts.Execute();
end;

procedure TWWindow.ApplicationException(Sender: TObject; E: Exception);
var
  Msg: string;
begin
  if (E.Message <> SRecordChanged) then
  begin
    DiableApplicationActivate := True;

    Msg := 'Internal Program Bug:' + #13#10 + E.Message;

    {$IFNDEF EurekaLog}
    if (IsConnectedToInternet()) then
    begin
      CheckUpdateThread := TCheckUpdateThread.Create(True);
      CheckUpdateThread.Stream := TStringStream.Create('');
      CheckUpdateThread.Execute();
      CheckUpdateThread.Stream.Free();

      if (CheckUpdateThread.UpdateAvailable) then
        Msg := Msg + #13#10#13#10 + 'An update is available. You can install the update with menu: Help -> Install Update.';

      CheckUpdateThread.Free();
    end;
    {$ENDIF}

    MsgBox(Msg, Preferences.LoadStr(45), MB_OK + MB_ICONERROR);

    DiableApplicationActivate := False;
  end;
end;

procedure TWWindow.ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
var
  NewTabIndex: Integer;
begin
  if ((Screen.ActiveForm = Self) and (WM_KEYFIRST <= Msg.Message) and (Msg.Message <= WM_KEYLAST)
    and ((TWMKey(Pointer(@Msg.message)^).CharCode = VK_TAB) or (TWMKey(Pointer(@Msg.message)^).CharCode = VK_PRIOR) or (TWMKey(Pointer(@Msg.message)^).CharCode = VK_NEXT) or (TWMKey(Pointer(@Msg.message)^).CharCode in [Ord('1') .. Ord('9')])) and (GetKeyState(VK_CONTROL) < 0)) then
  begin
    if (Msg.Message = WM_KEYDOWN) then
    begin
      if ((TWMKey(Pointer(@Msg.message)^).CharCode = VK_TAB) and (GetKeyState(VK_SHIFT) < 0) or (TWMKey(Pointer(@Msg.message)^).CharCode =  VK_PRIOR)) then
      begin
        NewTabIndex := TabControl.TabIndex - 1;
        if (NewTabIndex < 0) then
          NewTabIndex := Tabs.Count - 1;
        Handled := True;
      end
      else if ((TWMKey(Pointer(@Msg.message)^).CharCode = VK_TAB) and (GetKeyState(VK_SHIFT) >= 0) or (TWMKey(Pointer(@Msg.message)^).CharCode =  VK_NEXT)) then
      begin
        NewTabIndex := TabControl.TabIndex + 1;
        if (NewTabIndex >= Tabs.Count) then
          NewTabIndex := 0;
        Handled := True;
      end
      else if ((Ord('1') <= TWMKey(Pointer(@Msg.message)^).CharCode) and (TWMKey(Pointer(@Msg.message)^).CharCode < Ord('1') + Tabs.Count)) then
      begin
        NewTabIndex := TWMKey(Pointer(@Msg.message)^).CharCode - Ord('1');
        Handled := True;
      end
      else
        NewTabIndex := TabControl.TabIndex;

      if (NewTabIndex <> TabControl.TabIndex) then
      begin
        Perform(CM_DEACTIVATETAB, 0, 0);
        Perform(CM_ACTIVATETAB, 0, LPARAM(Tabs[NewTabIndex]));
      end;
    end;
  end;
end;

procedure TWWindow.ApplicationModalBegin(Sender: TObject);
begin
  if (Assigned(Screen.ActiveForm) and Screen.ActiveForm.Active and (Screen.ActiveForm is TForm_Ext)) then
  begin
    TForm_Ext(Screen.ActiveForm).Deactivate();
    PreviousForm := Screen.ActiveForm;

    if ((Screen.ActiveForm = Self) and Assigned(ActiveTab)) then
      SendMessage(ActiveTab.Handle, CM_DEACTIVATEFRAME, 0, 0);
  end
  else
    PreviousForm := nil;
end;

procedure TWWindow.ApplicationModalEnd(Sender: TObject);
begin
  if (Assigned(PreviousForm) and (PreviousForm is TForm_Ext)) then
  begin
    TForm_Ext(PreviousForm).Activate();
    PreviousForm := nil;

    if (Screen.ActiveForm = Self) then
    begin
      if (UpdateAvailable) then
      begin
        UpdateAvailable := False;
        InformUpdateAvailable();
      end;

      if (Assigned(ActiveTab)) then
        PostMessage(ActiveTab.Handle, CM_ACTIVATEFRAME, 0, 0);
    end;
  end;
end;

procedure TWWindow.aSAddressExecute(Sender: TObject);
begin
  if (not aVAddressBar.Checked) then
    aVAddressBar.Execute();

  ActiveControl := FAddress;
end;

procedure TWWindow.aSSearchFindNotFound(Sender: TObject);
var
  FindText: string;
begin
  if (Sender is TSearchFind) then
    FindText := TSearchFind(Sender).Dialog.FindText
  else if (Sender is TSearchReplace) then
    FindText := TSearchReplace(Sender).Dialog.FindText
  else
    FindText := '';
  MsgBox(Preferences.LoadStr(533, FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
end;

procedure TWWindow.aVAddressBarExecute(Sender: TObject);
begin
  PAddressBar.Visible := aVAddressBar.Checked;
  TabControlResize(nil);

  if (PAddressBar.Visible) then
  begin
    // PAddressBar sicher unter PToolbar stellen
    PAddressBar.Align := alNone;
    PAddressBar.Top := 1;
    PAddressBar.Align := alTop;
  end;
end;

procedure TWWindow.aVAddressExecute(Sender: TObject);
var
  OldAddress: string;
begin
  if (FAddress.Text = 'debug-raise') then
    raise Exception.Create('Debug Exception')
  else if (not PathIsURL(PChar(Trim(FAddress.Text)))) then
  begin
    ActiveControl := FAddress;
    MessageBeep(MB_ICONERROR);
  end
  else if (Pos('mysql://', Trim(FAddress.Text)) <> 1) then
    ShellExecute(Application.Handle, 'open', PChar(Trim(FAddress.Text)), '', '', SW_SHOW)
  else if (Assigned(ActiveTab)) then
  begin
    OldAddress := ActiveTab.Address;
    ActiveTab.Address := FAddress.Text;

    FAddress.Items.Clear();
  end
  else
    Perform(CM_ADDTAB, 0, WPARAM(PChar(FAddress.Text)));

  ActiveControl := FAddress;
end;

procedure TWWindow.aVNextExecute(Sender: TObject);
begin
  MNextPopup(Sender);
  if (MNext.Items.Count > 0) then MNext.Items[0].Click();
end;

procedure TWWindow.aVPrevExecute(Sender: TObject);
begin
  MPrevPopup(Sender);
  if (MPrev.Items.Count > 0) then MPrev.Items[0].Click();
end;

procedure TWWindow.MySQLConnectionSynchronize(const Data: Pointer);
begin
  PostMessage(Handle, CM_MYSQLCONNECTION_SYNCHRONIZE, 0, LPARAM(Data));
end;

procedure TWWindow.CMActivateTab(var Message: TCMActivateTab);
begin
  ActiveTab := Message.Tab;

  Color := clBtnFace;

  if (Message.Tab.Visible) then
    SendMessage(Message.Tab.Handle, CM_ACTIVATEFRAME, 0, 0);

  Perform(CM_BOOKMARKCHANGED, 0, 0);

  aHManual.Enabled := Message.Tab.Client.Account.ManualURL <> '';

  tbDBPrev.Action := Message.Tab.aDPrev;
  tbDBFirst.Action := Message.Tab.DataSetFirst;
  tbDBLast.Action := Message.Tab.DataSetLast;
  tbDBNext.Action := Message.Tab.aDNext;
  tbPostRecord.Action := Message.Tab.DataSetPost;
  tbCancelRecord.Action := Message.Tab.DataSetCancel;

  aFClose.Enabled := True;

  MPrev.Items.Clear();
  MNext.Items.Clear();

  Perform(CM_UPDATETOOLBAR, 0, LPARAM(Message.Tab));

  Message.Result := 1;
end;

procedure TWWindow.CMAddTab(var Message: TCMAddTab);
var
  FClient: TFClient;
begin
  if (not FirstOpen or Assigned(Message.Param) and (Copy(Message.Param, 1, 8) = 'mysql://')) then
    DAccounts.Account := nil
  else
    DAccounts.Account := Accounts.AccountByURI(Message.Param);
  DAccounts.Open := True;
  if (not DAccounts.Execute()) then
    FClient := nil
  else
  begin
    Perform(CM_DEACTIVATETAB, 0, 0);

    if (Tabs.Count = 0) then
    begin
      // TabControl sicher unter PToolbar / PAddressBar stellen
      TabControl.Align := alNone;
      if (not PAddressBar.Visible) then
        TabControl.Top := PToolBar.Top + PToolBar.Height
      else
        TabControl.Top := PAddressBar.Top + PAddressBar.Height;
      TabControl.Align := alTop;

      TabControl.Tabs.Add(TabCaption(DAccounts.Client.Account.Name));
      TabControl.Visible := Preferences.TabsVisible;
      if (TabControl.Visible) then
        TabControlResize(nil);
    end
    else
    begin
      TabControl.Tabs.Add(TabCaption(DAccounts.Client.Account.Name));
      TabControl.TabIndex := TabControl.Tabs.Count - 1;
      if (not TabControl.Visible) then
      begin
        TabControl.Visible := True;
        TabControlResize(nil);
      end;
    end;

    FClient := TFClient.Create(Self, PWorkSpace, DAccounts.Client, Message.Param);

    Inc(UniqueTabNameCounter);
    FClient.Name := FClient.ClassName + '_' + IntToStr(UniqueTabNameCounter);

    FClient.StatusBar := StatusBar;

    aFCloseAll.Enabled := True;

    Tabs.Add(FClient);
    Perform(CM_ACTIVATETAB, 0, LPARAM(FClient));

    TBTabControl.Visible := TabControl.Visible;
  end;

  Message.Result := Integer(Assigned(FClient));

  FirstOpen := False;
end;

procedure TWWindow.CMBookmarkChanged(var Message: TMessage);
var
  I: Integer;
  Index: Integer;
  NewMenuItem: TMenuItem;
begin
  Index := miBookmarks.IndexOf(miBSeparator) + 1;
  while (miBookmarks.Count > Index) do
    miBookmarks.Items[Index].Free();
  if (Assigned(ActiveTab)) then
    for I := 0 to ActiveTab.Client.Account.Desktop.Bookmarks.Count - 1 do
    begin
      NewMenuItem := TMenuItem.Create(Self);
      NewMenuItem.Action := aBookmark;
      NewMenuItem.Caption := ActiveTab.Client.Account.Desktop.Bookmarks[I].Caption;
      miBookmarks.Add(NewMenuItem);
    end;
end;

procedure TWWindow.CMChangePreferences(var Message: TMessage);
var
  I: Integer;
begin
  ToolBar.Images := Preferences.LargeImages;
  TBAddressBar.Images := Preferences.SmallImages;
  TabControl.Images := Preferences.SmallImages;
  TBTabControl.Images := Preferences.SmallImages;

  Perform(CM_SYSFONTCHANGED, 0, 0);

  TabControl.Canvas.Font := Font;

  Caption := Application.Title;

  miFile.Caption := Preferences.LoadStr(3);
  aFOpenAccount.Caption := Preferences.LoadStr(1) + '...';
  aFOpen.Caption := Preferences.LoadStr(581) + '...';
  aFSave.Caption := Preferences.LoadStr(582);
  aFSaveAs.Caption := Preferences.LoadStr(583) + '...';
  miFImport.Caption := Preferences.LoadStr(371);
  aFImportSQL.Caption := Preferences.LoadStr(409) + '...';
  aFImportText.Caption := Preferences.LoadStr(410) + '...';
  aFImportExcel.Caption := Preferences.LoadStr(801) + '...';
  aFImportAccess.Caption := Preferences.LoadStr(695) + '...';
  aFImportSQLite.Caption := Preferences.LoadStr(870) + '...';
  aFImportODBC.Caption := Preferences.LoadStr(607) + '...';
  aFImportXML.Caption := Preferences.LoadStr(454) + '...';
  miFExport.Caption := Preferences.LoadStr(200);
  aFExportSQL.Caption := Preferences.LoadStr(409) + '...';
  aFExportText.Caption := Preferences.LoadStr(410) + '...';
  aFExportExcel.Caption := Preferences.LoadStr(801) + '...';
  aFExportAccess.Caption := Preferences.LoadStr(695) + '...';
  aFExportSQLite.Caption := Preferences.LoadStr(870) + '...';
  aFExportODBC.Caption := Preferences.LoadStr(607) + '...';
  aFExportXML.Caption := Preferences.LoadStr(454) + '...';
  aFExportHTML.Caption := Preferences.LoadStr(453) + '...';
  aFExportBitmap.Caption := Preferences.LoadStr(868) + '...';
  aFPrint.Caption := Preferences.LoadStr(577) + '...';
  aFClose.Caption := Preferences.LoadStr(7);
  aFCloseAll.Caption := Preferences.LoadStr(590);
  aFExit.Caption := Preferences.LoadStr(8);

  miEdit.Caption := Preferences.LoadStr(62);
  aEUndo.Caption := Preferences.LoadStr(425);
  aERedo.Caption := Preferences.LoadStr(705);
  aECut.Caption := Preferences.LoadStr(63) + #9 + ShortCutToText(scCtrl + Ord('X'));
  aECopy.Caption := Preferences.LoadStr(64) + #9 + ShortCutToText(scCtrl + Ord('C'));
  aEPaste.Caption := Preferences.LoadStr(65) + #9 + ShortCutToText(scCtrl + Ord('V'));
  aEDelete.Caption := Preferences.LoadStr(28) + #9 + ShortCutToText(VK_DELETE);
  aESelectAll.Caption := Preferences.LoadStr(572) + #9 + ShortCutToText(scCtrl + Ord('A'));
  aECopyToFile.Caption := Preferences.LoadStr(182) + '...';
  aEPasteFromFile.Caption := Preferences.LoadStr(183) + '...';
  aERename.Caption := Preferences.LoadStr(98);

  miSearch.Caption := Preferences.LoadStr(424);
  aSSearchFind.Caption := Preferences.LoadStr(187) + '...';
  aSSearchReplace.Caption := Preferences.LoadStr(416) + '...';
  aSSearchNext.Caption := Preferences.LoadStr(188);
  aSGoto.Caption := Preferences.LoadStr(676) + '...';
  aSAddress.Caption := Preferences.LoadStr(730) + '...';

  miView.Caption := Preferences.LoadStr(9);
  aVObjectBrowser.Caption := Preferences.LoadStr(4);
  aVDataBrowser.Caption := Preferences.LoadStr(5);
  aVObjectIDE.Caption := Preferences.LoadStr(865);
  aVQueryBuilder.Caption := Preferences.LoadStr(852);
  aVSQLEditor.Caption := Preferences.LoadStr(6);
  aVDiagram.Caption := Preferences.LoadStr(800);
  aVAddressBar.Caption := Preferences.LoadStr(731);
  miVSidebar.Caption := Preferences.LoadStr(736);
  aVNavigator.Caption := Preferences.LoadStr(10);
  aVBookmarks.Caption := Preferences.LoadStr(727);
  aVExplorer.Caption := Preferences.LoadStr(435);
  aVSQLHistory.Caption := Preferences.LoadStr(807);
  aVSQLLog.Caption := Preferences.LoadStr(11);
  aVDetails.Caption := Preferences.LoadStr(684) + '...';
  aVRefresh.Caption := Preferences.LoadStr(41);
  aVRefreshAll.Caption := Preferences.LoadStr(623);

  miBookmarks.Caption := Preferences.LoadStr(727);
  aBAdd.Caption := Preferences.LoadStr(728) + '...';
  aBDelete.Caption := Preferences.LoadStr(559);
  aBEdit.Caption := Preferences.LoadStr(97) + '...';

  miDatabase.Caption := Preferences.LoadStr(38);
  miDCreate.Caption := Preferences.LoadStr(26);
  aDCreateDatabase.Caption := Preferences.LoadStr(38) + '...';
  aDCreateTable.Caption := Preferences.LoadStr(302) + '...';
  aDCreateView.Caption := Preferences.LoadStr(738) + '...';
  aDCreateProcedure.Caption := Preferences.LoadStr(768) + '...';
  aDCreateFunction.Caption := Preferences.LoadStr(769) + '...';
  aDCreateTrigger.Caption := Preferences.LoadStr(788) + '...';
  aDCreateEvent.Caption := Preferences.LoadStr(812) + '...';
  aDCreateIndex.Caption := Preferences.LoadStr(163) + '...';
  aDCreateField.Caption := Preferences.LoadStr(164) + '...';
  aDCreateForeignKey.Caption := Preferences.LoadStr(248) + '...';
  aDCreateHost.Caption := Preferences.LoadStr(560) + '...';
  aDCreateUser.Caption := Preferences.LoadStr(561) + '...';
  miDDelete.Caption := Preferences.LoadStr(559);
  aDDeleteDatabase.Caption := Preferences.LoadStr(38);
  aDDeleteTable.Caption := Preferences.LoadStr(302);
  aDDeleteView.Caption := Preferences.LoadStr(738);
  aDDeleteRoutine.Caption := Preferences.LoadStr(774);
  aDDeleteIndex.Caption := Preferences.LoadStr(163);
  aDDeleteField.Caption := Preferences.LoadStr(164);
  aDDeleteForeignKey.Caption := Preferences.LoadStr(248);
  aDDeleteTrigger.Caption := Preferences.LoadStr(788);
  aDDeleteEvent.Caption := Preferences.LoadStr(812);
  aDDeleteHost.Caption := Preferences.LoadStr(560);
  aDDeleteUser.Caption := Preferences.LoadStr(561);
  aDDeleteProcess.Caption := Preferences.LoadStr(562);
  miDProperties.Caption := Preferences.LoadStr(97);
  aDEditServer.Caption := Preferences.LoadStr(37) + '...';
  aDEditDatabase.Caption := Preferences.LoadStr(38) + '...';
  aDEditTable.Caption := Preferences.LoadStr(302) + '...';
  aDEditView.Caption := Preferences.LoadStr(738) + '...';
  aDEditRoutine.Caption := Preferences.LoadStr(774) + '...';
  aDEditIndex.Caption := Preferences.LoadStr(163) + '...';
  aDEditField.Caption := Preferences.LoadStr(164) + '...';
  aDEditForeignKey.Caption := Preferences.LoadStr(248) + '...';
  aDEditTrigger.Caption := Preferences.LoadStr(788) + '...';
  aDEditEvent.Caption := Preferences.LoadStr(812) + '...';
  aDEditHost.Caption := Preferences.LoadStr(560) + '...';
  aDEditProcess.Caption := Preferences.LoadStr(562) + '...';
  aDEditUser.Caption := Preferences.LoadStr(561) + '...';
  aDEditVariable.Caption := Preferences.LoadStr(267) + '...';
  aDInsertRecord.Caption := Preferences.LoadStr(179) + #9 + ShortCutToText(VK_INSERT);
  aDDeleteRecord.Caption := Preferences.LoadStr(178) + #9 + ShortCutToText(scCtrl + VK_DELETE);
  aDEditRecord.Caption := Preferences.LoadStr(500);
  aDPostRecord.Caption := Preferences.LoadStr(516);
  aDCancelRecord.Caption := Preferences.LoadStr(517);
  aDCancel.Caption := Preferences.LoadStr(517);
  aDRun.Caption := Preferences.LoadStr(174);
  aDRunSelection.Caption := Preferences.LoadStr(175);
  aDPostObject.Caption := Preferences.LoadStr(582);
  aDEmpty.Caption := Preferences.LoadStr(181);
  aDAutoCommit.Caption := Preferences.LoadStr(802);
  aDCommit.Caption := Preferences.LoadStr(803);
  aDRollback.Caption := Preferences.LoadStr(804);

  miOptions.Caption := Preferences.LoadStr(13);
  aOGlobals.Caption := Preferences.LoadStr(52) + '...';
  aOAccounts.Caption := Preferences.LoadStr(25) + '...';

  miExtras.Caption := Preferences.LoadStr(707);
  aEFind.Caption := Preferences.LoadStr(187) + '...';
  aEReplace.Caption := Preferences.LoadStr(416) + '...';
  aETransfer.Caption := Preferences.LoadStr(753) + '...';

  miHelp.Caption := Preferences.LoadStr(167);
  aHIndex.Caption := Preferences.LoadStr(653) + '...';
  aHManual.Caption := Preferences.LoadStr(573);
  aHUpdate.Caption := Preferences.LoadStr(666) + '...';
  aHInfo.Caption := Preferences.LoadStr(168) + '...';

  for I := 0 to ActionList.ActionCount - 1 do
    if (ActionList.Actions[I] is TCustomAction) and (TCustomAction(ActionList.Actions[I]).Hint = '') then
      TCustomAction(ActionList.Actions[I]).Hint := ReplaceStr(TCustomAction(ActionList.Actions[I]).Caption, '&', '');


  mtTabs.Caption := Preferences.LoadStr(851);

  SetToolBarHints(ToolBar);
  tbCreateDatabase.Hint := Preferences.LoadStr(147) + '...';
  tbDeleteDatabase.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbCreateTable.Hint := Preferences.LoadStr(383) + '...';
  tbDeleteTable.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbCreateIndex.Hint := Preferences.LoadStr(160) + '...';
  tbDeleteIndex.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbCreateField.Hint := Preferences.LoadStr(87) + '...';
  tbDeleteField.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbCreateForeignKey.Hint := Preferences.LoadStr(249) + '...';
  tbDeleteForeignKey.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbProperties.Hint := ReplaceStr(Preferences.LoadStr(97), '&', '') + '...';
  tbPostRecord.Hint := Preferences.LoadStr(516);
  tbCancelRecord.Hint := Preferences.LoadStr(517);


  tbPrev.Hint := Preferences.LoadStr(512);
  tbNext.Hint := Preferences.LoadStr(513);
  FAddress.Hint := ReplaceStr(Preferences.LoadStr(730), '&', '');
  FAddressApply.Hint := ReplaceStr(Preferences.LoadStr(676), '&', '');


  SetToolBarHints(TBTabControl);


  Highlighter.CommentAttri.Foreground := Preferences.Editor.CommentForeground;
  Highlighter.CommentAttri.Background := Preferences.Editor.CommentBackground;
  Highlighter.CommentAttri.Style := Preferences.Editor.CommentStyle;
  Highlighter.ConditionalCommentAttri.Foreground := Preferences.Editor.ConditionalCommentForeground;
  Highlighter.ConditionalCommentAttri.Background := Preferences.Editor.ConditionalCommentBackground;
  Highlighter.ConditionalCommentAttri.Style := Preferences.Editor.ConditionalCommentStyle;
  Highlighter.DataTypeAttri.Foreground := Preferences.Editor.DataTypeForeground;
  Highlighter.DataTypeAttri.Background := Preferences.Editor.DataTypeBackground;
  Highlighter.DataTypeAttri.Style := Preferences.Editor.DataTypeStyle;
  Highlighter.DelimitedIdentifierAttri.Foreground := Preferences.Editor.IdentifierForeground;
  Highlighter.DelimitedIdentifierAttri.Background := Preferences.Editor.IdentifierBackground;
  Highlighter.DelimitedIdentifierAttri.Style := Preferences.Editor.IdentifierStyle;
  Highlighter.FunctionAttri.Foreground := Preferences.Editor.FunctionForeground;
  Highlighter.FunctionAttri.Background := Preferences.Editor.FunctionBackground;
  Highlighter.FunctionAttri.Style := Preferences.Editor.FunctionStyle;
  Highlighter.IdentifierAttri.Foreground := Preferences.Editor.IdentifierForeground;
  Highlighter.IdentifierAttri.Background := Preferences.Editor.IdentifierBackground;
  Highlighter.IdentifierAttri.Style := Preferences.Editor.IdentifierStyle;
  Highlighter.KeyAttri.Foreground := Preferences.Editor.KeywordForeground;
  Highlighter.KeyAttri.Background := Preferences.Editor.KeywordBackground;
  Highlighter.KeyAttri.Style := Preferences.Editor.KeywordStyle;
  Highlighter.NumberAttri.Foreground := Preferences.Editor.NumberForeground;
  Highlighter.NumberAttri.Background := Preferences.Editor.NumberBackground;
  Highlighter.NumberAttri.Style := Preferences.Editor.NumberStyle;
  Highlighter.PLSQLAttri.Foreground := Preferences.Editor.KeywordForeground;
  Highlighter.PLSQLAttri.Background := Preferences.Editor.KeywordBackground;
  Highlighter.PLSQLAttri.Style := Preferences.Editor.KeywordStyle;
  Highlighter.StringAttri.Foreground := Preferences.Editor.StringForeground;
  Highlighter.StringAttri.Background := Preferences.Editor.StringBackground;
  Highlighter.StringAttri.Style := Preferences.Editor.StringStyle;
  Highlighter.VariableAttri.Foreground := Preferences.Editor.VariableForeground;
  Highlighter.VariableAttri.Background := Preferences.Editor.VariableBackground;
  Highlighter.VariableAttri.Style := Preferences.Editor.VariableStyle;

  if (Assigned(Tabs)) then
    for I := 0 to Tabs.Count - 1 do
      SendMessage(TFClient(Tabs[0]).Handle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TWWindow.CMClientSynchronize(var Message: TMessage);
begin
  MySQLDB.MySQLConnectionSynchronize(Pointer(Message.LParam));
end;

procedure TWWindow.CMCloseTab(var Message: TCMCloseTab);
var
  NewTabIndex: Integer;
begin
  Perform(CM_DEACTIVATETAB, 0, 0);

  NewTabIndex := Tabs.IndexOf(Message.Tab);

  if (0 <= NewTabIndex) and (NewTabIndex < TabControl.Tabs.Count) then
  begin
    TabControl.Tabs.Delete(NewTabIndex);
    Tabs.Delete(Tabs.IndexOf(Message.Tab));
    if (TabControl.TabIndex < 0) then
      TabControl.TabIndex := Tabs.Count - 1;

    Dec(NewTabIndex, 1);
    if ((NewTabIndex < 0) and (Tabs.Count > 0)) then
      NewTabIndex := 0;
    if (NewTabIndex >= 0) then
      Perform(CM_ACTIVATETAB, 0, LPARAM(Tabs[NewTabIndex]));

    Message.Tab.Free();

    TBTabControl.Visible := Preferences.TabsVisible or not Preferences.TabsVisible and (Tabs.Count >= 2);
    TabControl.Visible := TBTabControl.Visible;
    TabControlResize(nil);

    aFCloseAll.Enabled := Tabs.Count > 0;
  end;
end;

procedure TWWindow.CMDeactivateTab(var Message: TMessage);
var
  I: Integer;
  Index: Integer;
begin
  if (Assigned(ActiveTab)) then
  begin
    SendMessage(ActiveTab.Handle, CM_DEACTIVATEFRAME, 0, 0);

    TabControl.TabIndex := -1;

    Caption := Application.Title;

    aFClose.Enabled := False;

    Index := miBookmarks.IndexOf(miBSeparator) + 1;
    while (miBookmarks.Count > Index) do
      miBookmarks.Items[Index].Free();

    miDAutoCommit.Action := nil;
    miDCommit.Action := nil;
    miDRollback.Action := nil;
    miDAutoCommit.Checked := False;

    aVObjectBrowser.Checked := False;
    aVDataBrowser.Checked := False;
    aVObjectIDE.Checked := False;
    aVQueryBuilder.Checked := False;
    aVSQLEditor.Checked := False;
    aVNavigator.Checked := False;
    aVBookmarks.Checked := False;
    aVExplorer.Checked := False;
    aVSQLHistory.Checked := False;
    aVSQLLog.Checked := False;
    tbVRefresh.Enabled := False;

    aFOpen.Enabled := False;
    aFSave.Enabled := False;
    aFSaveAs.Enabled := False;
    aECopy.Enabled := False;
    aEPaste.Enabled := False;
    aSAddress.Enabled := False;
    aVObjectBrowser.Enabled := False;
    aVDataBrowser.Enabled := False;
    aVObjectIDE.Enabled := False;
    aVQueryBuilder.Enabled := False;
    aVSQLEditor.Enabled := False;
    aVDiagram.Enabled := False;
    aVNavigator.Enabled := False;
    aVBookmarks.Enabled := False;
    aVExplorer.Enabled := False;
    aVSQLHistory.Enabled := False;
    aVSQLLog.Enabled := False;
    aBAdd.Enabled := False;
    aDCancel.Enabled := False;
    aDInsertRecord.Enabled := False;
    aDDeleteRecord.Enabled := False;
    aDRun.Enabled := False;
    aDRunSelection.Enabled := False;
    aHManual.Enabled := False;

    miVRefresh.Enabled := False;
    miVRefreshAll.Enabled := False;
    miDAutoCommit.Enabled := False;
    miDCommit.Enabled := False;
    miDRollback.Enabled := False;

    tbPrev.Enabled := False;
    tbNext.Enabled := False;
    FAddress.Clear();
  end;

  Perform(CM_UPDATETOOLBAR, 0, 0);
  for I := 0 to StatusBar.Panels.Count - 1 do
    StatusBar.Panels.Items[I].Text := '';
end;

procedure TWWindow.CMPostShow(var Message: TMessage);
var
  ExecutePostShow: Boolean;
  I: Integer;
begin
  ExecutePostShow := False;

  for I := 1 to ParamCount() do
    if (UpperCase(ParamStr(I)) = '/EXECUTE') then
    begin
      ExecutePostShow := True;
      QuitAfterShow := True;
    end;

  if (ParamCount() > 0) then
    for I := 1 to ParamCount() do
      HandleParam(ParamStr(I))
  else
    Perform(CM_ADDTAB, 0, 0);

  if (ExecutePostShow and (Tabs.Count = 1)) then
    PostMessage(TFClient(Tabs[0]).Handle, CM_EXECUTE, 0, 0);
end;

procedure TWWindow.CMSysFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;

  if (not CheckWin32Version(6)) then
  begin
    ToolBar.EdgeBorders := [ebTop,ebBottom];
    TBAddressBar.EdgeBorders := [ebBottom];

    ToolBar.BorderWidth := 0;
    TBAddressBar.BorderWidth := 0;
  end
  else
  begin
    ToolBar.EdgeBorders := [];
    TBAddressBar.EdgeBorders := [];

    ToolBar.BorderWidth := 2;
    TBAddressBar.BorderWidth := 2;
  end;

  if (Assigned(ToolBar.Images) and Assigned(TBAddressBar.Images)) then
  begin
    // Recalculate height of Toolbars:
    PToolBar.AutoSize := False;
    ToolBar.AutoSize := False;
    ToolBar.ButtonHeight := 0;
    ToolBar.ButtonHeight := ToolBar.Images.Height + 6;
    ToolBar.ButtonWidth := ToolBar.Images.Width + 7;
    ToolBar.AutoSize := True;
    PToolBar.AutoSize := True;

    PAddressBar.AutoSize := False;
    TBAddressBar.AutoSize := False; TBAddressBar.ButtonHeight := 0; TBAddressBar.ButtonHeight := TBAddressBar.Images.Height + 6; TBAddressBar.ButtonWidth := TBAddressBar.Images.Width + 7; TBAddressBar.AutoSize := True;
    PAddressBar.AutoSize := True;
  end;

  for I := 0 to TabControl.Tabs.Count - 1 do
    TabControl.Tabs[I] := TabCaption(Trim(TabControl.Tabs[I]));
end;

procedure TWWindow.CMUpdateAvailable(var Message: TMessage);
begin
  if (Screen.ActiveForm <> Self) then
    UpdateAvailable := True
  else
    InformUpdateAvailable();
end;

procedure TWWindow.CMUpdateToolbar(var Message: TCMUpdateToolBar);
var
  Found: Boolean;
  I: Integer;
  Tab: TFClient;
begin
  Tab := Message.Tab;

  for I := ToolBar.ButtonCount - 1 downto ToolButton11.Index do
    ToolBar.Buttons[I].Visible := False;

  if (Assigned(Tab) and (Tab = ActiveTab)) then
  begin
    Caption := Application.Title + ' - ' + Tab.ToolBarData.Caption;

    tbProperties.Action := Tab.ToolBarData.tbPropertiesAction;
    tbProperties.Caption := Preferences.LoadStr(97) + '...';
    tbProperties.ImageIndex := 11;

    FAddress.Items.Clear();
    FAddress.Text := Tab.ToolBarData.Address;

    aVPrev.Enabled := Tab.ToolBarData.CurrentAddress > 0;
    aVNext.Enabled := Assigned(Tab.ToolBarData.Addresses) and (Tab.ToolBarData.CurrentAddress < Tab.ToolBarData.Addresses.Count - 1);

    MPrev.Items.Clear();
    MNext.Items.Clear();
  end;

  tbCreateDatabase.Visible   := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);
  tbDeleteDatabase.Visible   := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);
  tbCreateTable.Visible      := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects, vDiagram]);
  tbDeleteTable.Visible      := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects, vDiagram]);
  tbCreateIndex.Visible      := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);
  tbDeleteIndex.Visible      := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);
  tbCreateField.Visible      := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);
  tbDeleteField.Visible      := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);
  tbCreateForeignKey.Visible := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects, vDiagram]);
  tbDeleteForeignKey.Visible := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects, vDiagram]);
  tbProperties.Visible       := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects, vDiagram]);

  tbOpen.Visible             := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbSave.Visible             := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbUndo.Visible             := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbRedo.Visible             := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbSearchFind.Visible       := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbSearchReplace.Visible    := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbRun.Visible              := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vBuilder, vEditor]);
  tbRunSelection.Visible     := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE, vEditor]);
  tbPostObject.Visible       := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vIDE]);

  tbDBFirst.Visible          := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser, vBuilder, vEditor]);
  tbDBPrev.Visible           := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser]);
  tbDBNext.Visible           := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser]);
  tbDBLast.Visible           := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser, vBuilder, vEditor]);
  tbDInsertRecord.Visible    := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser, vBuilder, vEditor]);
  tbDDeleteRecord.Visible    := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser, vBuilder, vEditor]);
  tbPostRecord.Visible       := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser]);
  tbCancelRecord.Visible     := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vBrowser]);

  tbDEmpty.Visible           := Assigned(Tab) and Tab.Visible and (Tab.ToolBarData.View in [vObjects]);

  // Auto hide unneeded separator buttons
  Found := False;
  for I := ToolBar.ButtonCount - 1 downto ToolButton11.Index do
    if (ToolBar.Buttons[I].ImageIndex >= 0) then
      Found := Found or ToolBar.Buttons[I].Visible
    else
    begin
      ToolBar.Buttons[I].Visible := Found;
      Found := False;
    end;

  Found := False;
  for I := ToolBar.ButtonCount - 1 downto ToolButton11.Index do
    Found := Found or ToolBar.Buttons[I].Visible and (ToolBar.Buttons[I].ImageIndex >= 0) and (ToolBar.Buttons[I].Width <> ToolBar.ButtonWidth);
  if (Found) then
    Toolbar.ButtonWidth := 0; // Without this, the Buttons are too small. Why??? A Delphi bug?
end;

procedure TWWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;

  StrCopy(Params.WinClassName, cWindowClassName);
end;

function TWWindow.DBLogin(const Account: Pointer): Boolean;
begin
  DLogin.Account := Account;
  DLogin.Filename := '';
  DLogin.Window := Self;
  Result := DLogin.Execute();
end;

destructor TWWindow.Destroy();
begin
  FreeAndNil(CloseButton);
  FreeAndNil(Tabs);
  FreeAndNil(Accounts);

  {$IFDEF EurekaLog}
    EurekaLog.Free();
  {$ENDIF}

  inherited;
end;

{$IFDEF EurekaLog}
procedure TWWindow.EurekaLogCustomDataRequest(
  EurekaExceptionRecord: TEurekaExceptionRecord; DataFields: TStrings);
var
  I: Integer;
begin
  if (IsWine()) then
    DataFields.Add('Wine=Yes');

  for I := 0 to Clients.Count - 1 do
    if (Clients[I].Connected) then
      DataFields.Add('MySQL Version=' + Clients[I].ServerVersionStr);
end;

procedure TWWindow.EurekaLogExceptionNotify(
  EurekaExceptionRecord: TEurekaExceptionRecord; var Handled: Boolean);
var
  CheckUpdateThread: TCheckUpdateThread;
  I: Integer;
begin
  for I := 0 to Tabs.Count - 1 do
    try TFClient(Tabs[I]).CrashRescue(); except end;

  try Accounts.SaveToXML(); except end;

  if (not IsConnectedToInternet()) then
    Handled := False
  else
  begin
    CheckUpdateThread := TCheckUpdateThread.Create(True);
    CheckUpdateThread.Stream := TStringStream.Create('');
    CheckUpdateThread.Execute();
    CheckUpdateThread.Stream.Free();

    Handled := not CheckUpdateThread.UpdateAvailable;

    CheckUpdateThread.Free();
  end;
end;
{$ENDIF}

procedure TWWindow.FAddressDropDown(Sender: TObject);
var
  I: Integer;
begin
  FAddressDroppedDown := True;

  if ((FAddress.Items.Count = 0) and Assigned(ActiveTab)) then
    for I := 0 to ActiveTab.ToolBarData.AddressMRU.Count - 1 do
      FAddress.Items.Add(ActiveTab.ToolBarData.AddressMRU.Values[I]);
end;

procedure TWWindow.FAddressKeyPress(Sender: TObject; var Key: Char);
begin
  if (Ord(Key) = VK_RETURN) then
  begin
    ActiveControl := nil;
    FAddressApply.Click();
    Key := #0;
  end
  else if ((Ord(Key) = VK_ESCAPE) and not Assigned(ActiveTab)) then
  begin
    FAddress.Text := '';
    Key := #0;
  end
  else if ((Ord(Key) = VK_ESCAPE) and Assigned(ActiveTab)) then
  begin
    FAddress.Text := ActiveTab.Address;
    ActiveControl := nil;
    ActiveControl := FAddress;
    Key := #0;
  end;
end;

procedure TWWindow.FAddressSelect(Sender: TObject);
begin
  if (FAddressDroppedDown) then
    FAddressApply.Click();

  FAddressDroppedDown := False;
end;

procedure TWWindow.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  aFCloseAllExecute(Sender);

  CanClose := Tabs.Count = 0;
end;

procedure TWWindow.FormCreate(Sender: TObject);
var
  ClassName: array [0..MAX_PATH] of Char;
  I: Integer;
  WindowPlacement: TWindowPlacement;
  Wnd: HWND;
begin
  DiableApplicationActivate := False;
  FirstOpen := True;
  QuitAfterShow := False;
  UniqueTabNameCounter := 0;
  MouseDownPoint := Point(-1, -1);
  UpdateAvailable := False;

  MySQLDB.MySQLConnectionOnSynchronize := MySQLConnectionSynchronize;

  Application.HelpFile := ExtractFilePath(Application.ExeName) + Copy(ExtractFileName(Application.ExeName), 1, Length(ExtractFileName(Application.ExeName)) - 4) + '.chm';
  Application.OnException := ApplicationException;
  Application.OnMessage := ApplicationMessage;
  Application.OnModalBegin := ApplicationModalBegin;
  Application.OnModalEnd := ApplicationModalEnd;

  {$IFDEF EurekaLog}
    EurekaLog := TEurekaLog.Create(Self);
    EurekaLog.OnExceptionNotify := EurekaLogExceptionNotify;
    EurekaLog.OnCustomDataRequest := EurekaLogCustomDataRequest;
  {$ENDIF}

  Accounts := TAAccounts.Create(DBLogin, SQLError);

  MainActionList := ActionList;
  MainHighlighter := Highlighter;

  GetClassName(Handle, @ClassName, Length(ClassName) - 1);
  Wnd := FindWindow(@ClassName, PChar(Application.Title));
  WindowPlacement.length := SizeOf(WindowPlacement);
  if ((Wnd > 0) and GetWindowPlacement(Wnd, @WindowPlacement)) then
  begin
    Inc(WindowPlacement.rcNormalPosition.Left, 2 * GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION));
    Inc(WindowPlacement.rcNormalPosition.Top, 2 * GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION));
    Inc(WindowPlacement.rcNormalPosition.Right, 2 * GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION));
    Inc(WindowPlacement.rcNormalPosition.Bottom, 2 * GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION));

    if (WindowPlacement.rcNormalPosition.Right > Screen.WorkAreaRect.Right) then
    begin
      Dec(WindowPlacement.rcNormalPosition.Right, WindowPlacement.rcNormalPosition.Left);
      WindowPlacement.rcNormalPosition.Left := 0;
      Dec(WindowPlacement.rcNormalPosition.Bottom, WindowPlacement.rcNormalPosition.Top);
      WindowPlacement.rcNormalPosition.Top := 0;
    end
    else if (WindowPlacement.rcNormalPosition.Bottom > Screen.WorkAreaRect.Bottom) then
    begin
      Dec(WindowPlacement.rcNormalPosition.Bottom, WindowPlacement.rcNormalPosition.Top);
      WindowPlacement.rcNormalPosition.Top := 0;
    end;

    WindowPlacement.showCmd := SW_HIDE;
    SetWindowPlacement(Handle, @WindowPlacement);

    if (GetWindowPlacement(Wnd, @WindowPlacement) and (WindowPlacement.showCmd = SW_MAXIMIZE)) then
      WindowState := wsMaximized;
  end
  else if (((Preferences.Height > 0) and (Preferences.Width > 0))) then
  begin
    Left := Preferences.Left;
    Top := Preferences.Top;
    Height := Preferences.Height;
    Width := Preferences.Width;
    WindowState := Preferences.WindowState;

    if (WindowState = wsNormal) then
    begin
      if (Width > Screen.WorkAreaRect.Right - Screen.WorkAreaRect.Left) then
        Left := 0
      else if (Left + Width > Screen.WorkAreaRect.Right) then
        Left := Screen.WorkAreaRect.Right - Width;

      if (Height > Screen.WorkAreaRect.Bottom - Screen.WorkAreaRect.Top) then
        Top := 0
      else if (Top + Height > Screen.WorkAreaRect.Bottom) then
        Top := Screen.WorkAreaRect.Bottom - Height;
    end;
  end;

  TBAddressBar.Images := Preferences.SmallImages;
  TabControl.Images := Preferences.SmallImages;
  TBTabControl.Images := Preferences.SmallImages;

  Tabs := TList.Create();
  TBTabControl.Visible := Preferences.TabsVisible;

  aHUpdate.Enabled := IsConnectedToInternet() and (Preferences.SetupProgram = '');

  aVAddressBar.Checked := Preferences.AddressBarVisible;
  PAddressBar.Visible := aVAddressBar.Checked;

  Perform(CM_UPDATETOOLBAR, 0, 0);


  CloseButton := TPicture.Create();
  if (StyleServices.Enabled) then
  begin
    CloseButton.Bitmap.Height := (GetSystemMetrics(SM_CYCAPTION) - 1) - (GetSystemMetrics(SM_CYEDGE) - 1) - GetSystemMetrics(SM_CYEDGE) - 4 + 1;
    if (CloseButton.Bitmap.Height > TabControl.Images.Height - 2) then
      CloseButton.Bitmap.Height := TabControl.Images.Height - 2;

    CloseButton.Bitmap.Width := CloseButton.Bitmap.Height
  end
  else
  begin
    CloseButton.Bitmap.Height := (GetSystemMetrics(SM_CYCAPTION) - 1) - (GetSystemMetrics(SM_CYEDGE) - 1) - GetSystemMetrics(SM_CYEDGE) - 4;
    CloseButton.Bitmap.Width := CloseButton.Bitmap.Height;
    if (CloseButton.Bitmap.Height <= 11) then CloseButton.Bitmap.Width := CloseButton.Bitmap.Width + 1;
  end;
  DrawCloseBitmap(CloseButton.Bitmap);

  for I := 0 to StatusBar.Panels.Count - 1 do
    StatusBar.Panels.Items[I].Text := '';
end;

procedure TWWindow.FormHide(Sender: TObject);
begin
  if (Assigned(CheckUpdateThread)) then
  begin
    try
      CheckUpdateThread.Terminate();
      CheckUpdateThread.WaitFor();
      FreeAndNil(CheckUpdateThread.Stream);
    except
      // It's not needed to see bugs on it.
    end;
  end;

  Preferences.WindowState := WindowState;
  if (WindowState = wsNormal) then
    begin Preferences.Top := Top; Preferences.Left := Left; Preferences.Height := Height; Preferences.Width := Width; end;

  Preferences.AddressBarVisible := aVAddressBar.Checked;
end;

procedure TWWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((ShortCut(Key, Shift) = aVPrev.ShortCut) and not aVPrev.Enabled) then Key := 0;
  if ((ShortCut(Key, Shift) = aVNext.ShortCut) and not aVNext.Enabled) then Key := 0;
end;

procedure TWWindow.FormResize(Sender: TObject);
var
  FirstPanelWidth: Integer;
  I: Integer;
begin
  if (Assigned(Preferences)) then
  begin
    StatusBar.Panels.Items[sbItem].Width := StatusBar.Canvas.TextWidth('9999 (999999999)') + StatusBar.BorderWidth + 15;
    StatusBar.Panels.Items[sbSummarize].Width := StatusBar.Canvas.TextWidth(Preferences.LoadStr(691, '9999', '999999999')) + StatusBar.BorderWidth + 15;
    StatusBar.Panels.Items[sbConnected].Width := StatusBar.Canvas.TextWidth(Preferences.LoadStr(519) + ': ' + FormatDateTime(FormatSettings.ShortTimeFormat, EncodeTime(23, 59, 59, 999))) + StatusBar.BorderWidth + 15;

    FirstPanelWidth := StatusBar.Width;
    for I := 1 to StatusBar.Panels.Count - 2 do
      FirstPanelWidth := FirstPanelWidth - StatusBar.Panels.Items[I].Width;

    if (WindowState = wsNormal) then
      FirstPanelWidth := FirstPanelWidth - StatusBar.Height;

    StatusBar.Panels.Items[0].Width := FirstPanelWidth;
  end;

  if (not TabControl.Visible and Assigned(TabControl.OnResize)) then
    TabControl.OnResize(Sender);
end;

procedure TWWindow.FormShow(Sender: TObject);
begin
  if (IsConnectedToInternet() and ((Preferences.UpdateCheck = utStartup) or ((Preferences.UpdateCheck = utDaily) and (Trunc(Preferences.UpdateChecked) < Date())))) then
  begin
    CheckUpdateThread := TCheckUpdateThread.Create(True);
    CheckUpdateThread.FreeOnTerminate := True;
    CheckUpdateThread.Stream := TStringStream.Create('');
    CheckUpdateThread.Wnd := Self.Handle;
    CheckUpdateThread.SuccessMessage := CM_UPDATEAVAILABLE;
    CheckUpdateThread.Start();
  end;

  PostMessage(Handle, CM_POSTSHOW, 0, 0);
end;

function TWWindow.GetActiveTab(): TFClient;
begin
  if (not Assigned(Tabs) or (TabControl.TabIndex < 0) or (Tabs.Count <= TabControl.TabIndex)) then
    Result := nil
  else
    Result := TFClient(Tabs[TabControl.TabIndex]);
end;

function TWWindow.GetNewTabIndex(Sender: TObject; X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := TabControlDragStartTabIndex;
  for I := TabControlDragStartTabIndex - 1 downto 0 do
    if (X < TabControl.TabRect(I).Left + 3 * ((TabControl.TabRect(I).Right - TabControl.TabRect(I).Left) div 4)) then
      Result := I;
  for I := TabControlDragStartTabIndex + 1 to TabControl.Tabs.Count - 1 do
    if (X > TabControl.TabRect(I).Left + ((TabControl.TabRect(I).Right - TabControl.TabRect(I).Left) div 4)) then
      Result := I;
end;

procedure TWWindow.HandleParam(const AParam: string);
begin
  if (Copy(Param, 1, 1) <> '/') then
  begin
    Param := AParam;
    Perform(CM_ADDTAB, 0, WPARAM(PChar(Param)));
  end;
end;

procedure TWWindow.InformUpdateAvailable();
begin
  if (MsgBox(Preferences.LoadStr(506) + #10#10 + Preferences.LoadStr(845), Preferences.LoadStr(43), MB_ICONQUESTION + MB_YESNOCANCEL) = ID_YES) then
    aHUpdate.Execute();
end;

procedure TWWindow.MNextItemClick(Sender: TObject);
var
  Diff: Integer;
  I: Integer;
begin
  Diff := 0;
  for I := 0 to MNext.Items.Count - 1 do
    if (MNext.Items[I] = Sender) then
      Diff := I + 1;

  ActiveTab.MoveToAddress(Diff);
end;

procedure TWWindow.MNextPopup(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if (Assigned(ActiveTab) and (MNext.Items.Count = 0)) then
    for I := ActiveTab.ToolBarData.CurrentAddress + 1 to ActiveTab.ToolBarData.Addresses.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := ActiveTab.AddressToCaption(ActiveTab.ToolBarData.Addresses[I]);
      MenuItem.OnClick := MNextItemClick;
      MNext.Items.Add(MenuItem);
    end;
end;

procedure TWWindow.MPrevItemClick(Sender: TObject);
var
  Diff: Integer;
  I: Integer;
begin
  Diff := 0;
  for I := 0 to MPrev.Items.Count - 1 do
    if (MPrev.Items[I] = Sender) then
      Diff := I + 1;

  ActiveTab.MoveToAddress(- Diff);
end;

procedure TWWindow.MPrevPopup(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if (Assigned(ActiveTab) and (MPrev.Items.Count = 0)) then
    for I := 0 to ActiveTab.ToolBarData.CurrentAddress - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := ActiveTab.AddressToCaption(ActiveTab.ToolBarData.Addresses[I]);
      MenuItem.OnClick := MPrevItemClick;
      MPrev.Items.Insert(0, MenuItem);
    end;
end;

procedure TWWindow.mtTabsClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    if (Assigned(ActiveTab)) then
      Perform(CM_DEACTIVATETAB, 0, 0);
    Perform(CM_ACTIVATETAB, 0, LPARAM(TFClient(Tabs[TMenuItem(Sender).Parent.IndexOf(TMenuItem(Sender))])));
  end;
end;

procedure TWWindow.SetActiveTab(const FClient: TFClient);
begin
  TabControl.TabIndex := Tabs.IndexOf(FClient);

  TFClient(Tabs[Tabs.IndexOf(FClient)]).BringToFront();
end;

procedure TWWindow.SQLError(const Connection: TMySQLConnection; const ErrorCode: Integer; const ErrorMessage: string);
var
  Msg: string;
  Tab: TFClient;
begin
  case (ErrorCode) of
    EE_READ: Msg := ErrorMessage;
    ER_DBACCESS_DENIED_ERROR:
      if (not Connection.Connected) then
        Msg := Preferences.LoadStr(165, IntToStr(ErrorCode), ErrorMessage + #10#10 + Preferences.LoadStr(497))
      else
        Msg := Preferences.LoadStr(165, IntToStr(ErrorCode), ErrorMessage);
    ER_ACCESS_DENIED_ERROR:
      if (not Connection.Connected) then
        Msg := Preferences.LoadStr(165, IntToStr(ErrorCode), ErrorMessage + #10#10 + Preferences.LoadStr(496))
      else
        Msg := Preferences.LoadStr(165, IntToStr(ErrorCode), ErrorMessage);
    ER_CANT_OPEN_LIBRARY: Msg := Preferences.LoadStr(570, Connection.LibraryName, ExtractFilePath(Application.ExeName));
    CR_COMMANDS_OUT_OF_SYNC: Msg := 'Internal bug: ' + ErrorMessage;
    CR_CONN_HOST_ERROR: if (Connection.Port = MYSQL_PORT) then Msg := Preferences.LoadStr(495, Connection.Host) else Msg := Preferences.LoadStr(495, Connection.Host + ':' + IntToStr(Connection.Port));
    CR_SERVER_GONE_ERROR: if (Connection.Port = MYSQL_PORT) then Msg := Preferences.LoadStr(881, Connection.Host) else Msg := Preferences.LoadStr(881, Connection.Host + ':' + IntToStr(Connection.Port));
    CR_UNKNOWN_HOST: if (Connection.Host <> '') then Msg := Preferences.LoadStr(706, Connection.Host) else Msg := Preferences.LoadStr(706);
    CR_OUT_OF_MEMORY: Msg := Preferences.LoadStr(733);
    CR_SERVER_LOST: Msg := Preferences.LoadStr(806, Connection.Host);
    CR_HTTPTUNNEL_UNKNOWN_ERROR: Msg := ErrorMessage + #10#10 + ReplaceStr(Preferences.LoadStr(652), '&', '') + ': ' + Connection.LibraryName;
    CR_HTTPTUNNEL_OLD: Msg := Preferences.LoadStr(659, Connection.LibraryName);
    CR_HTTPTUNNEL_CONN_ERROR: Msg := ErrorMessage;
    CR_HTTPTUNNEL_ACCESS_DENIED_ERROR: Msg := Preferences.LoadStr(860, Preferences.LoadStr(859), Copy(ErrorMessage, 1, Pos(' ', ErrorMessage) - 1)) + ':' + #10#10 + ErrorMessage  + #10#10 + ' (' + Connection.LibraryName + ')';
    CR_HTTPTUNNEL_NOT_FOUND: Msg := Preferences.LoadStr(523, Connection.LibraryName);
    CR_HTTPTUNNEL_SERVER_ERROR: Msg := Preferences.LoadStr(860, Preferences.LoadStr(859), Copy(ErrorMessage, 1, Pos(' ', ErrorMessage) - 1)) + ':' + #10#10 + ErrorMessage  + #10#10 + ' (' + Connection.LibraryName + ')';
    CR_HTTPTUNNEL_INVALID_SERVER_RESPONSE,
    CR_HTTPTUNNEL_INVALID_CONTENT_TYPE_ERROR,
    CR_IPSOCK_ERROR: Msg := ErrorMessage;
    CR_SET_NAMES: Msg := Preferences.LoadStr(878, ErrorMessage);
    CR_SERVER_OLD: Msg := Preferences.LoadStr(696, '3.23.20');
    else Msg := Preferences.LoadStr(165, IntToStr(ErrorCode), ErrorMessage);
  end;

  if (Msg <> '') then
  begin
    DiableApplicationActivate := True;
    MsgBox(Msg, Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
    DiableApplicationActivate := False;
  end;

  if ((ErrorCode = CR_SERVER_GONE_ERROR) and (Connection is TCClient)) then
  begin
    Tab := TFClient(TCClient(Connection).Account.Frame);
    if (Boolean(SendMessage(Tab.Handle, CM_CLOSE_TAB_QUERY, 0, 0))) then
      Perform(CM_CLOSE_TAB, 0, LPARAM(Tab));
  end;
end;

function TWWindow.TabCaption(const ACaption: string): string;
begin
  Result := ACaption + StringOfChar(' ', (CloseButton.Bitmap.Width + 7) div TabControl.Canvas.TextWidth(' '));
end;

procedure TWWindow.TabControlChange(Sender: TObject);
var
  Tab: TFClient;
begin
  if (TabControl.TabIndex < Tabs.Count) then
  begin
    Tab := TFClient(Tabs[TabControl.TabIndex]);

    Perform(CM_ACTIVATETAB, 0, LPARAM(Tab));
  end;
end;

procedure TWWindow.TabControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if (Assigned(ActiveTab)) then
    Perform(CM_DEACTIVATETAB, 0, 0);
end;

procedure TWWindow.TabControlContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  mtTabs.Clear();
  if (TabControl.Tabs.Count > 1) then
    for I := 0 to Tabs.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := TFClient(Tabs[I]).ToolBarData.Caption;
      MenuItem.RadioItem := True;
      MenuItem.Checked := I = TabControl.TabIndex;
      MenuItem.OnClick := mtTabsClick;
      mtTabs.Add(MenuItem);
    end;

  ShowEnabledItems(MTabControl.Items);
end;

procedure TWWindow.TabControlDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  I: Integer;
  NewTabIndex: Integer;
begin
  NewTabIndex := GetNewTabIndex(Sender, X, Y);

  Tabs.Move(TabControlDragStartTabIndex, NewTabIndex);

  TabControl.Tabs.Clear();
  for I := 0 to Tabs.Count - 1 do
    TabControl.Tabs.Add(TabCaption(TFClient(Tabs[I]).Client.Account.Name));

  TabControl.TabIndex := NewTabIndex;
end;

procedure TWWindow.TabControlDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  NewTabIndex: Integer;
begin
  NewTabIndex := GetNewTabIndex(Sender, X, Y);
  Accept := (Source = TabControl) and (NewTabIndex <> TabControlDragStartTabIndex);

  if ((TabControlDragStartTabIndex >= 0) and (NewTabIndex <> TabControlDragMarkedTabIndex)) then
    TabControl.Repaint();
  if (Accept) then
  begin
    TabControl.Canvas.Brush.Color := clBtnText;
    if (NewTabIndex < TabControlDragStartTabIndex) then
      TabControl.Canvas.FillRect(Rect(TabControl.TabRect(NewTabIndex).Left - 2, 0, TabControl.TabRect(NewTabIndex).Left + 2, TabControl.Height))
    else
      TabControl.Canvas.FillRect(Rect(TabControl.TabRect(NewTabIndex).Right - 2, 0, TabControl.TabRect(NewTabIndex).Right + 2, TabControl.Height));
    TabControlDragMarkedTabIndex := NewTabIndex;
  end;
end;

procedure TWWindow.TabControlDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  BackgroundRect: TRect;
  ElementDetails: TThemedElementDetails;
  IconRect: TRect;
  ImageIndex: Integer;
  S: string;
  TextRect: TRect;
begin
  if (Active) then
  begin
    IconRect.Left := Rect.Left + 4;
    IconRect.Top := Rect.Top + 4;
    IconRect.Right := IconRect.Left + TabControl.Images.Width;
    IconRect.Bottom := IconRect.Top + TabControl.Images.Height;

    TextRect.Left := IconRect.Right + 4;
    TextRect.Top := Rect.Top + 5;
    TextRect.Bottom := Rect.Bottom - 2;

    Control.Canvas.Font.Style := Control.Canvas.Font.Style + [fsBold]
  end
  else
  begin
    IconRect.Left := Rect.Left + 4;
    IconRect.Top := Rect.Top + 4;
    IconRect.Right := IconRect.Left + TabControl.Images.Width;
    IconRect.Bottom := IconRect.Top + TabControl.Images.Height;

    TextRect.Left := IconRect.Right + 4;
    TextRect.Top := Rect.Top + 4;
    TextRect.Bottom := Rect.Bottom - 2;

    Control.Canvas.Font.Style := Control.Canvas.Font.Style - [fsBold];
  end;

  if (Length(CloseButtonRects) < TabIndex + 1) then
    SetLength(CloseButtonRects, TabIndex + 1);
  CloseButtonRects[TabIndex].Left := Rect.Right - CloseButton.Bitmap.Width - (IconRect.Left - Rect.Left) - 2;
  CloseButtonRects[TabIndex].Top := TextRect.Top + 1;
  CloseButtonRects[TabIndex].Right := CloseButtonRects[TabIndex].Left + CloseButton.Bitmap.Width;
  CloseButtonRects[TabIndex].Bottom := CloseButtonRects[TabIndex].Top + CloseButton.Bitmap.Height;

  TextRect.Right := CloseButtonRects[TabIndex].Left - 2;

  if (StyleServices.Enabled) then
  begin
    BackgroundRect.Left := Rect.Left;
    BackgroundRect.Top := Rect.Top;
    BackgroundRect.Right := Rect.Right;
    BackgroundRect.Bottom := Rect.Bottom + (Control.Height - (Rect.Bottom - Rect.Top)) div 2;

    if (Active) then
    begin
      if ((TabIndex = 0) and (TabControl.Tabs.Count > 1)) then
        ElementDetails := StyleServices.GetElementDetails(ttTopTabItemLeftEdgeSelected)
      else if ((TabIndex = 0) and (TabControl.Tabs.Count = 1)) then
        ElementDetails := StyleServices.GetElementDetails(ttTopTabItemBothEdgeSelected)
      else if ((TabIndex = TabControl.Tabs.Count - 1) and (TabControl.Tabs.Count > 1)) then
        ElementDetails := StyleServices.GetElementDetails(ttTopTabItemRightEdgeSelected)
      else
        ElementDetails := StyleServices.GetElementDetails(ttTopTabItemSelected);

      StyleServices.DrawElement(Control.Canvas.Handle, ElementDetails, BackgroundRect);

      if (Assigned(TabControl.OnGetImageIndex)) then
      begin
        TabControl.OnGetImageIndex(TabControl, TabIndex, ImageIndex);
        StyleServices.DrawIcon(Control.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemSelected), IconRect, TabControl.Images.Handle, ImageIndex);
      end;
      StyleServices.DrawText(Control.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemSelected), TTabControl(Control).Tabs[TabIndex], TextRect, TTextFormat(Control.Canvas.TextFlags), 0);
      StyleServices.DrawElement(Control.Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonNormal), CloseButtonRects[TabIndex]);
    end
    else
    begin
      if ((TabIndex = 0) and (TabControl.Tabs.Count > 1)) then
        ElementDetails := StyleServices.GetElementDetails(ttTabItemLeftEdgeNormal)
      else if ((TabIndex = 0) and (TabControl.Tabs.Count = 1)) then
        ElementDetails := StyleServices.GetElementDetails(ttTabItemBothEdgeNormal)
      else if ((TabIndex = TabControl.Tabs.Count - 1) and (TabControl.Tabs.Count > 1)) then
        ElementDetails := StyleServices.GetElementDetails(ttTabItemRightEdgeNormal)
      else
        ElementDetails := StyleServices.GetElementDetails(ttTabItemNormal);

      StyleServices.DrawElement(Control.Canvas.Handle, ElementDetails, BackgroundRect);

      if (Assigned(TabControl.OnGetImageIndex)) then
      begin
        TabControl.OnGetImageIndex(TabControl, TabIndex, ImageIndex);
        StyleServices.DrawIcon(Control.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemNormal), IconRect, TabControl.Images.Handle, ImageIndex);
      end;
      StyleServices.DrawText(Control.Canvas.Handle, StyleServices.GetElementDetails(ttTabItemNormal), TTabControl(Control).Tabs[TabIndex], TextRect, TTextFormat(Control.Canvas.TextFlags), 0);
      StyleServices.DrawElement(Control.Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonNormal), CloseButtonRects[TabIndex]);
    end;
  end
  else
  begin
    Control.Canvas.FillRect(Rect);

    if (Assigned(TabControl.OnGetImageIndex)) then
    begin
      TabControl.OnGetImageIndex(TabControl, TabIndex, ImageIndex);
      Preferences.SmallImages.Draw(Control.Canvas, IconRect.Left, IconRect.Top, ImageIndex);
    end;

    S := Trim(TabControl.Tabs[TabIndex]);
    if (Control.Canvas.TextWidth(S) > TextRect.Right - TextRect.Left) then
    begin
      while ((Length(S) > 0) and (Control.Canvas.TextWidth(S + '...') > TextRect.Right - TextRect.Left)) do
        Delete(S, Length(S), 1);
      S := S + '...';
    end;
    Control.Canvas.TextOut(TextRect.Left, TextRect.Top, S);

    Control.Canvas.Draw(CloseButtonRects[TabIndex].Left, CloseButtonRects[TabIndex].Top, CloseButton.Bitmap);
  end;
end;

procedure TWWindow.TabControlEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  TabControl.Repaint();
end;

procedure TWWindow.TabControlGetImageIndex(Sender: TObject;
  TabIndex: Integer; var ImageIndex: Integer);
var
  Tab: TFClient;
begin
  if ((TabIndex < 0) or (Tabs.Count <= TabIndex)) then
    Tab := nil
  else
    Tab := TFClient(Tabs[TabIndex]);

  if (not Assigned(Tab)) then
    ImageIndex := iiServer
  else
    ImageIndex := Tab.Client.Account.ImageIndex;

  if (ImageIndex < 0) then
    ImageIndex := iiServer;
end;

procedure TWWindow.TabControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  if (Button = mbLeft) then
  begin
    TabIndex := TabControl.IndexOfTabAt(X, Y);

    if ((TabIndex >= 0) and PtInRect(CloseButtonRects[TabIndex], Point(X, Y))) then
    begin
      MouseDownPoint := Point(X, Y);
      TabControlMouseMove(Sender, Shift, X, Y);
    end
    else
    begin
      TabControl.TabIndex := TabIndex;
      TabControl.BeginDrag(False, 10);
    end;
  end;
end;

procedure TWWindow.TabControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Hint: string;
  TabControl: TTabControl;
  TabIndex: Integer;
begin
  if (Sender is TTabControl) then
  begin
    TabControl := TTabControl(Sender);
    TabIndex := TabControl.IndexOfTabAt(X, Y);

    if ((TabIndex < 0) or (Tabs.Count <= TabIndex)) then
      TabControl.Hint := ''
    else
    begin
      if (StyleServices.Enabled) then
      begin
        if (PtInRect(CloseButtonRects[TabIndex], Point(X, Y))) then
        begin
          SetCapture(TabControl.Handle);
          CaptureTabIndex := TabIndex;

          if (PtInRect(CloseButtonRects[TabIndex], MouseDownPoint)) then
            StyleServices.DrawElement(TabControl.Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonPushed), CloseButtonRects[CaptureTabIndex])
          else
            StyleServices.DrawElement(TabControl.Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonHot), CloseButtonRects[CaptureTabIndex]);
        end
        else
        begin
          ReleaseCapture();

          if (CaptureTabIndex <> TabControl.TabIndex) then
            StyleServices.DrawElement(TabControl.Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonDisabled), CloseButtonRects[CaptureTabIndex])
          else
            StyleServices.DrawElement(TabControl.Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonNormal), CloseButtonRects[CaptureTabIndex]);
        end
      end
      else
      begin
        if (PtInRect(CloseButtonRects[TabIndex], Point(X, Y))) then
        begin
          SetCapture(TabControl.Handle);
          CaptureTabIndex := TabIndex;

          Dec(CloseButtonRects[CaptureTabIndex].Left);
          Dec(CloseButtonRects[CaptureTabIndex].Top);
          Inc(CloseButtonRects[CaptureTabIndex].Right);
          Inc(CloseButtonRects[CaptureTabIndex].Bottom);
          if (PtInRect(CloseButtonRects[TabIndex], MouseDownPoint)) then
            Frame3D(TabControl.Canvas, CloseButtonRects[CaptureTabIndex], clDkGray, clWhite, 1)
          else
            Frame3D(TabControl.Canvas, CloseButtonRects[CaptureTabIndex], clWhite, clDkGray, 1);
        end
        else if (0 <= CaptureTabIndex) and (CaptureTabIndex < Length(CloseButtonRects)) then
        begin
          Dec(CloseButtonRects[CaptureTabIndex].Left);
          Dec(CloseButtonRects[CaptureTabIndex].Top);
          Inc(CloseButtonRects[CaptureTabIndex].Right);
          Inc(CloseButtonRects[CaptureTabIndex].Bottom);
          Frame3D(TabControl.Canvas, CloseButtonRects[CaptureTabIndex], TabControl.Canvas.Brush.Color, TabControl.Canvas.Brush.Color, 1);

          ReleaseCapture();
          CaptureTabIndex := -1;
        end;
      end;

      if (PtInRect(CloseButtonRects[TabIndex], Point(X, Y))) then
        Hint := ReplaceStr(aFClose.Caption, '&', '')
      else
      begin
        Hint := TFClient(Tabs[TabIndex]).Client.Account.Name;

        if (TabIndex < 9) then
          Hint := Hint + ' (' + ShortCutToText(ShortCut(Ord('1') + TabIndex, [ssCtrl])) + ')';
      end;

      if (Hint <> TabControl.Hint) then
        Application.CancelHint();
      TabControl.Hint := Hint;
    end;
  end;
end;

procedure TWWindow.TabControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  if (Button = mbLeft) then
  begin
    TabIndex := TabControl.IndexOfTabAt(X, Y);

    if ((TabIndex >= 0) and PtInRect(CloseButtonRects[TabIndex], Point(X, Y)) and PtInRect(CloseButtonRects[TabIndex], MouseDownPoint)) then
      aFClose.Execute();

    MouseDownPoint := Point(-1, -1);
    TabControlMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TWWindow.TabControlResize(Sender: TObject);
begin
  if (TabControl.Tabs.Count > 0) then
  begin
    TBTabControl.Left := TabControl.TabRect(TabControl.Tabs.Count - 1).Right + 5;
    TBTabControl.Top := TabControl.Top + 1;
    TBTabControl.Width := TabControl.Width - TBTabControl.Left;
    TBTabControl.Height := TabControl.Height - 3;
  end
  else if (PAddressBar.Visible) then
  begin
    TBTabControl.Left := PAddressBar.Left;
    TBTabControl.Top := PAddressBar.Top + PAddressBar.Height;
    TBTabControl.Width := PAddressBar.Width;
    TBTabControl.Height := TabControl.Height - 3;
  end
  else
  begin
    TBTabControl.Left := PToolBar.Left;
    TBTabControl.Top := PToolBar.Top + PToolBar.Height;
    TBTabControl.Width := PToolBar.Width;
    TBTabControl.Height := TabControl.Height - 3;
  end
end;

procedure TWWindow.TabControlStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  TabControlDragStartTabIndex := TabControl.TabIndex;
  TabControlDragMarkedTabIndex := -1;
end;

procedure TWWindow.TBAddressBarResize(Sender: TObject);
var
  I: Integer;
  Widths: Integer;
begin
  Widths := 2 * TBAddressBar.BorderWidth + 4;
  for I := 0 to TBAddressBar.ControlCount - 1 do
    if (TBAddressBar.Controls[I].Visible and (TBAddressBar.Controls[I] <> FAddress)) then
      Inc(Widths, TBAddressBar.Controls[I].Width);
  FAddress.Width := TBAddressBar.Width - Widths;
end;

procedure TWWindow.tbPropertiesClick(Sender: TObject);
begin
  if (ActiveTab.Address = ActiveTab.ToolBarData.Address) then
    ActiveTab.ToolBarData.tbPropertiesAction.Execute();
end;

procedure TWWindow.WMCopyData(var Msg: TWMCopyData);
begin
  SetForegroundWindow(Application.Handle);
  if IsIconic(Application.Handle) then
  begin
    WindowState := wsNormal;
    Application.Restore();
  end;

  if (Assigned(Screen.ActiveForm) and (fsModal in Screen.ActiveForm.FormState)) then
  begin
    Application.BringToFront();
    MessageBeep(MB_ICONERROR);
  end
  else
    HandleParam(PChar(Msg.CopyDataStruct^.lpData));
end;

procedure TWWindow.WMDrawItem(var Message: TWMDrawItem);
var
  Control: TControl;
begin
  Control := FindControl(Message.DrawItemStruct^.hwndItem);

  if ((Control is TTabControl) and TTabControl(Control).OwnerDraw) then
    Control.Perform(Message.Msg + CN_BASE, TMessage(Message).WParam, TMessage(Message).LParam)
  else
    inherited;
end;

procedure TWWindow.WMHelp(var Message: TWMHelp);
begin
  if (Message.HelpInfo.iContextType = HELPINFO_MENUITEM) then
    inherited;
end;

end.
