unit fDServer;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,
  SynEdit, SynMemo,
  Forms_Ext, StdCtrls_Ext,
  fBase, fClient;

type
  TDServer = class (TForm_Ext)
    FBCancel: TButton;
    FBFlushHosts: TButton;
    FBHelp: TButton;
    FBShutdown: TButton;
    FCharacterSet: TLabel;
    FComment: TLabel;
    FHost: TLabel;
    FHosts: TListView;
    FLCharacterSet: TLabel;
    FLComment: TLabel;
    FLHost: TLabel;
    FLibVersion: TLabel;
    FLLibVersion: TLabel;
    FLUptime: TLabel;
    FLUser: TLabel;
    FLVersion: TLabel;
    FPlugins: TListView;
    FProcesses: TListView;
    FSlowSQLLog: TSynMemo;
    FSQLLog: TSynMemo;
    FStartup: TSynMemo;
    FStati: TListView;
    FUptime: TLabel;
    FUser: TLabel;
    FUsers: TListView;
    FVariables: TListView;
    FVersion: TLabel;
    GConnection: TGroupBox_Ext;
    GServer: TGroupBox_Ext;
    GServiceHosts: TGroupBox_Ext;
    GServiceServer: TGroupBox_Ext;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miProperties: TMenuItem;
    MListView: TPopupMenu;
    msCopy: TMenuItem;
    msCut: TMenuItem;
    msDelete: TMenuItem;
    MSource: TPopupMenu;
    msPaste: TMenuItem;
    msSelectAll: TMenuItem;
    msUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PageControl: TPageControl;
    TSBasics: TTabSheet;
    TSExtras: TTabSheet;
    TSHosts: TTabSheet;
    TSPlugins: TTabSheet;
    TSProcesses: TTabSheet;
    TSSlowSQLLog: TTabSheet;
    TSSQLLog: TTabSheet;
    TSStartup: TTabSheet;
    TSStati: TTabSheet;
    TSUsers: TTabSheet;
    TSVariables: TTabSheet;
    procedure FBFlushHostsClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FBShutdownClick(Sender: TObject);
    procedure FHostsResize(Sender: TObject);
    procedure FHostsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FPluginsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FProcessesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FStatiSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FUsersResize(Sender: TObject);
    procedure FUsersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FVariablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewResize(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miPropertiesClick(Sender: TObject);
    procedure TSExtrasShow(Sender: TObject);
    procedure TSHostsShow(Sender: TObject);
    procedure TSPluginsShow(Sender: TObject);
    procedure TSProcessesShow(Sender: TObject);
    procedure TSSlowSQLLogShow(Sender: TObject);
    procedure TSSQLLogShow(Sender: TObject);
    procedure TSStartupShow(Sender: TObject);
    procedure TSStatiShow(Sender: TObject);
    procedure TSUsersShow(Sender: TObject);
    procedure TSVariablesShow(Sender: TObject);
  private
    procedure ListViewShowSortDirection(const ListView: TListView);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Client: TCClient;
    Tab: TCustomFrame;
    function Execute(): Boolean;
  end;

function DServer(): TDServer;

implementation {***************************************************************}

{$R *.dfm}

uses
  Math, CommCtrl, StrUtils,
  MySQLConsts,
  CommCtrl_Ext,
  MySQLDB, SQLUtils,
  fPreferences, fDVariable, fDUser, fDHost, fDStatement;

var
  FServer: TDServer;

function DServer(): TDServer;
begin
  if (not Assigned(FServer)) then
  begin
    Application.CreateForm(TDServer, FServer);
    FServer.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FServer;
end;

procedure TDServer.CMChangePreferences(var Message: TMessage);
begin
  TSBasics.Caption := Preferences.LoadStr(108);
  GServer.Caption := ReplaceStr(Preferences.LoadStr(37), '&', '');
  FLVersion.Caption := Preferences.LoadStr(169) + ':';
  FLComment.Caption := ReplaceStr(Preferences.LoadStr(111), '&', '') + ':';
  GConnection.Caption := Preferences.LoadStr(486);
  FLHost.Caption := ReplaceStr(Preferences.LoadStr(305), '&', '') + ':';
  FLLibVersion.Caption := ReplaceStr(Preferences.LoadStr(568), '&', '') + ':';
  FLUser.Caption := ReplaceStr(Preferences.LoadStr(561), '&', '') + ':';
  FLCharacterSet.Caption := ReplaceStr(Preferences.LoadStr(682), '&', '') + ':';

  TSProcesses.Caption := Preferences.LoadStr(24);
  FProcesses.Columns[0].Caption := Preferences.LoadStr(269);
  FProcesses.Columns[1].Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
  FProcesses.Columns[2].Caption := Preferences.LoadStr(271);
  FProcesses.Columns[3].Caption := ReplaceStr(Preferences.LoadStr(38), '&', '');
  FProcesses.Columns[4].Caption := Preferences.LoadStr(273);
  FProcesses.Columns[5].Caption := Preferences.LoadStr(274);
  FProcesses.Columns[6].Caption := ReplaceStr(Preferences.LoadStr(661), '&', '');
  FProcesses.Columns[7].Caption := Preferences.LoadStr(276);

  TSSQLLog.Caption := ReplaceStr(Preferences.LoadStr(11), '&', '');
  FSQLLog.Font.Name := Preferences.SQLFontName;
  FSQLLog.Font.Style := Preferences.SQLFontStyle;
  FSQLLog.Font.Color := Preferences.SQLFontColor;
  FSQLLog.Font.Size := Preferences.SQLFontSize;
  FSQLLog.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FSQLLog.Gutter.Font.Color := clWindowText
  else
    FSQLLog.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FSQLLog.Gutter.Color := clBtnFace
  else
    FSQLLog.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FSQLLog.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;

  TSSlowSQLLog.Caption := ReplaceStr(Preferences.LoadStr(847), '&', '');
  FSlowSQLLog.Font.Name := Preferences.SQLFontName;
  FSlowSQLLog.Font.Style := Preferences.SQLFontStyle;
  FSlowSQLLog.Font.Color := Preferences.SQLFontColor;
  FSlowSQLLog.Font.Size := Preferences.SQLFontSize;
  FSlowSQLLog.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FSlowSQLLog.Gutter.Font.Color := clWindowText
  else
    FSlowSQLLog.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FSlowSQLLog.Gutter.Color := clBtnFace
  else
    FSlowSQLLog.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FSlowSQLLog.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;

  TSStati.Caption := Preferences.LoadStr(23);
  FStati.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FStati.Columns[1].Caption := Preferences.LoadStr(268);

  TSVariables.Caption := Preferences.LoadStr(22);
  FVariables.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FVariables.Columns[1].Caption := Preferences.LoadStr(268);

  TSUsers.Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
  FUsers.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');

  TSHosts.Caption := ReplaceStr(Preferences.LoadStr(335), '&', '');
  FHosts.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(289), '&', '');

  TSStartup.Caption := Preferences.LoadStr(805);
  FStartup.Font.Name := Preferences.SQLFontName;
  FStartup.Font.Style := Preferences.SQLFontStyle;
  FStartup.Font.Color := Preferences.SQLFontColor;
  FStartup.Font.Size := Preferences.SQLFontSize;
  FStartup.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FStartup.Gutter.Font.Color := clWindowText
  else
    FStartup.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FStartup.Gutter.Color := clBtnFace
  else
    FStartup.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FStartup.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;

  TSPlugins.Caption := ReplaceStr(Preferences.LoadStr(811), '&', '');
  FPlugins.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FPlugins.Columns[1].Caption := ReplaceStr(Preferences.LoadStr(111), '&', '');

  TSExtras.Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
  GServiceServer.Caption := ReplaceStr(Preferences.LoadStr(37), '&', '');
  FLUptime.Caption := Preferences.LoadStr(520) + ':';
  FBShutdown.Caption := Preferences.LoadStr(323);

  GServiceHosts.Caption := ReplaceStr(Preferences.LoadStr(335), '&', '');
  FBFlushHosts.Caption := Preferences.LoadStr(329);

  miAdd.Caption := Preferences.LoadStr(26) + '...';
  miDelete.Caption := Preferences.LoadStr(28);
  miProperties.Caption := Preferences.LoadStr(97) + '...';

  msUndo.Action := MainAction('aEUndo'); msCut.ShortCut := 0;
  msCut.Action := MainAction('aECut'); msCut.ShortCut := 0;
  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msPaste.Action := MainAction('aEPaste'); msPaste.ShortCut := 0;
  msDelete.Action := MainAction('aEDelete'); msDelete.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBHelp.Caption := Preferences.LoadStr(167);
end;

function TDServer.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDServer.FBFlushHostsClick(Sender: TObject);
begin
  Client.FlushHosts();
end;

procedure TDServer.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDServer.FBShutdownClick(Sender: TObject);
var
  Host: string;
begin
  Host := Client.Host;
  if (Client.Port <> MYSQL_PORT) then
    Host := Host + ':' + IntToStr(Client.Port);
  if (MsgBox(Preferences.LoadStr(679, Host), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
    if (Boolean(SendMessage(Tab.Handle, CM_CLOSE_TAB_QUERY, 0, 0))) then
      if (Client.Shutdown()) then
      begin
        PostMessage(TForm(Tab.Owner).Handle, CM_CLOSE_TAB, 0, LPARAM(Tab));
        FBCancel.Click();
      end;

  ActiveControl := FBCancel;
end;

procedure TDServer.FHostsResize(Sender: TObject);
begin
  FHosts.Columns[0].Width := FHosts.ClientWidth;

  ListViewResize(FHosts);
end;

procedure TDServer.FHostsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  miAdd.Visible := not Assigned(FHosts.Selected);
  miDelete.Visible := Assigned(FHosts.Selected);
  miProperties.Visible := Assigned(FHosts.Selected);
end;

{ TDServer ********************************************************************}

procedure TDServer.FormCreate(Sender: TObject);
begin
  Preferences.SmallImages.GetIcon(iiServer, Icon);

  FProcesses.SmallImages := Preferences.SmallImages;
  FStati.SmallImages := Preferences.SmallImages;
  FVariables.SmallImages := Preferences.SmallImages;
  FUsers.SmallImages := Preferences.SmallImages;
  FHosts.SmallImages := Preferences.SmallImages;
  FStartup.Highlighter := MainHighlighter;
  FPlugins.SmallImages := Preferences.SmallImages;

  FSQLLog.Highlighter := MainHighlighter;
  FSlowSQLLog.Highlighter := MainHighlighter;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Server.Width >= Width) and (Preferences.Server.Height >= Height)) then
  begin
    Width := Preferences.Server.Width;
    Height := Preferences.Server.Height;
  end;

  SetWindowLong(ListView_GetHeader(FUsers.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FUsers.Handle), GWL_STYLE) or HDS_NOSIZING);
  SetWindowLong(ListView_GetHeader(FHosts.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FHosts.Handle), GWL_STYLE) or HDS_NOSIZING);

  PageControl.ActivePage := TSBasics;
end;

procedure TDServer.FormHide(Sender: TObject);
begin
  Preferences.Server.Width := Width;
  Preferences.Server.Height := Height;

  FProcesses.DisableAlign(); FProcesses.Items.BeginUpdate();
  FProcesses.Items.Clear();
  FProcesses.EnableAlign(); FProcesses.Items.EndUpdate();

  FSQLLog.Lines.Clear();

  FStati.DisableAlign(); FStati.Items.BeginUpdate();
  FStati.Items.Clear();
  FStati.EnableAlign(); FStati.Items.EndUpdate();

  FVariables.DisableAlign(); FVariables.Items.BeginUpdate();
  FVariables.Items.Clear();
  FVariables.EnableAlign(); FVariables.Items.EndUpdate();

  FUsers.DisableAlign(); FUsers.Items.BeginUpdate();
  FUsers.Items.Clear();
  FUsers.EnableAlign(); FUsers.Items.EndUpdate();

  FPlugins.DisableAlign(); FPlugins.Items.BeginUpdate();
  FPlugins.Items.Clear();
  FPlugins.EnableAlign(); FPlugins.Items.EndUpdate();
end;

procedure TDServer.FormShow(Sender: TObject);
begin
  Caption := Preferences.LoadStr(842, Client.Caption);

  FHost.Caption := Client.HostInfo;
  FVersion.Caption := Client.ServerVersionStr;
  FComment.Visible := Assigned(Client.VariableByName('version_comment'));
  FLComment.Visible := FComment.Visible;
  if (FComment.Visible) then
    FComment.Caption := Client.VariableByName('version_comment').Value;
  if (Client.LibraryType = ltDLL) then
    FLibVersion.Caption := Client.Lib.VersionStr
  else
    FLibVersion.Caption := Preferences.LoadStr(649);
  if (Client.CurrentUser = '') then
    FUser.Caption := '???'
  else
    FUser.Caption := Client.CurrentUser;
  FCharacterSet.Caption := Client.Charset;
  FUptime.Caption := '???';

  FStartup.Lines.Clear();

  TSProcesses.TabVisible := Assigned(Client.Processes);
  TSSQLLog.TabVisible := Client.LogActive;
  TSSlowSQLLog.TabVisible := Client.SlowLogActive;
  TSStartup.TabVisible := Assigned(Client.VariableByName('init_connect')) and (Client.VariableByName('init_connect').Value <> '');
  TSPlugins.TabVisible := Assigned(Client.Plugins);
  TSHosts.TabVisible := not Assigned(Client.UserRights) or Client.UserRights.RGrant;

  FBCancel.Caption := Preferences.LoadStr(231);

  PageControl.ActivePage := TSBasics;

  ActiveControl := FBCancel;
end;

procedure TDServer.FPluginsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  miAdd.Visible := not Assigned(FPlugins.Selected);
  miDelete.Visible := Assigned(FPlugins.Selected);
  miProperties.Visible := False;
end;

procedure TDServer.FProcessesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  miAdd.Visible := False;
  miDelete.Visible := Assigned(FProcesses.Selected) and (Trunc(StrToInt(FProcesses.Selected.Caption)) <> Client.ThreadId);
  miProperties.Visible := True;
end;

procedure TDServer.FStatiSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  miAdd.Visible := False;
  miDelete.Visible := False;
  miProperties.Visible := False;
end;

procedure TDServer.FUsersResize(Sender: TObject);
var
  I: Integer;
begin
  if (FUsers.Columns.Count = 1) then
    FUsers.Columns[0].Width := FUsers.ClientWidth
  else
    for I := 0 to FUsers.Columns.Count - 1 do
      FUsers.Columns[I].AutoSize := True;

  ListViewResize(FUsers);
end;

procedure TDServer.FUsersSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  miAdd.Visible := not Assigned(FUsers.Selected);
  miDelete.Visible := Assigned(FUsers.Selected) and (not Assigned(Client.User) or (FUsers.Selected.Caption <> Client.User.Name));
  miProperties.Visible := Assigned(FUsers.Selected);
end;

procedure TDServer.FVariablesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  miAdd.Visible := False;
  miDelete.Visible := False;
  miProperties.Visible := Assigned(FVariables.Selected);
end;

procedure TDServer.ListViewColumnClick(Sender: TObject; Column: TListColumn);
var
  I: Integer;
  ListView: TListView;
begin
  if (Sender is TListView) then
  begin
    ListView := TListView(Sender);

    for I := 0 to ListView.Columns.Count - 1 do
      if (ListView.Columns[I] <> Column) then
        ListView.Columns[I].Tag := 0
      else if (ListView.Columns[I].Tag < 0) then
        ListView.Columns[I].Tag := 1
      else if (ListView.Columns[I].Tag > 0) then
        ListView.Columns[I].Tag := -1
      else
        ListView.Columns[I].Tag := 1;

    ListView.Tag := Column.Index;
    ListView.AlphaSort();

    ListViewShowSortDirection(ListView);
  end;
end;

procedure TDServer.ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  Column: TListColumn;
  ListView: TListView;
begin
  ListView := TListView(Sender);
  Column := ListView.Columns[ListView.Tag];

  if (Column.Index = 0) then
    Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)))
  else
    Compare := Sign(lstrcmpi(PChar(Item1.SubItems[Column.Index - 1]), PChar(Item2.SubItems[Column.Index - 1])));

  if (Column.Tag < 0) then
    Compare := - Compare;
end;

procedure TDServer.ListViewDblClick(Sender: TObject);
var
  I: Integer;
  ListView: TListView;
  MenuItem: TMenuItem;
begin
  MenuItem := nil;

  ListView := TListView(Sender);
  if (Assigned(ListView.PopupMenu)) then
    for I := 0 to ListView.PopupMenu.Items.Count - 1 do
      if (ListView.PopupMenu.Items.Items[I].Default) and (ListView.PopupMenu.Items.Items[I].Enabled) then
        MenuItem := ListView.PopupMenu.Items.Items[I];
  if (Assigned(MenuItem) and Assigned(ListView.Selected)) then MenuItem.Click();
end;

procedure TDServer.ListViewKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    ListViewDblClick(Sender);
end;

procedure TDServer.ListViewResize(Sender: TObject);
begin
  if (Sender is TListView) then
    ListViewShowSortDirection(TListView(Sender));
end;

procedure TDServer.ListViewShowSortDirection(const ListView: TListView);
var
  Column: TListColumn;
  HDItem: THDItem;
  I: Integer;
begin
  Column := ListView.Columns[ListView.Tag];

  HDItem.Mask := HDI_FORMAT;
  for I := 0 to ListView.Columns.Count - 1 do
    if (BOOL(SendMessage(ListView_GetHeader(ListView.Handle), HDM_GETITEM, I, LParam(@HDItem)))) then
    begin
      case (ListView.Columns[I].Tag) of
        -1: HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        1: HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP;
        else HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN;
      end;
      SendMessage(ListView_GetHeader(ListView.Handle), HDM_SETITEM, I, LParam(@HDItem));
    end;

  if ((ComCtl32MajorVersion >= 6) and not CheckWin32Version(6, 1)) then
    SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, Column.Index, 0);
end;

procedure TDServer.miAddClick(Sender: TObject);
begin
  if (PageControl.ActivePage = TSHosts) then
  begin
    DHost.Client := Client;
    DHost.Host := nil;
    if (DHost.Execute()) then
    begin
      Client.Update();
      FHosts.Items.Clear();
      TSHostsShow(Sender);
    end;
  end
  else if (PageControl.ActivePage = TSUsers) then
  begin
    DUser.Client := Client;
    DUser.User := nil;
    if (DUser.Execute()) then
    begin
      FUsers.Items.Clear();
      TSUsersShow(Sender);
    end;
  end;
end;

procedure TDServer.miDeleteClick(Sender: TObject);
var
  I: Integer;
  List: TList;
  Msg: string;
  Success: Boolean;
begin
  List := TList.Create();

  if (PageControl.ActivePage = TSHosts) then
  begin
    if (FHosts.SelCount > 1) then
      Msg := Preferences.LoadStr(413)
    else
      Msg := Preferences.LoadStr(429, FHosts.Selected.Caption);
    Success := MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES;

    for I := 0 to FHosts.Items.Count - 1 do
      if (Success and FHosts.Items[I].Selected) then
        List.Add(Client.HostByCaption(FHosts.Items[I].Caption));
    if (Client.DeleteHosts(List)) then
    begin
      FHosts.Items.Clear();
      TSHostsShow(Sender);
    end;
  end
  else if (PageControl.ActivePage = TSProcesses) then
  begin
    if (FProcesses.SelCount > 1) then
      Msg := Preferences.LoadStr(413)
    else
      Msg := Preferences.LoadStr(534, FProcesses.Selected.Caption);
    Success := MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES;

    for I := 0 to FProcesses.Items.Count - 1 do
      if (Success and FProcesses.Items[I].Selected) then
        List.Add(Client.ProcessById(StrToInt(FProcesses.Items[I].Caption)));
    if (Client.DeleteProcesses(List)) then
    begin
      FProcesses.Items.Clear();
      TSProcessesShow(Sender);
    end;
  end
  else if (PageControl.ActivePage = TSUsers) then
  begin
    if (FUsers.SelCount > 1) then
      Msg := Preferences.LoadStr(413)
    else
      Msg := Preferences.LoadStr(428, FUsers.Selected.Caption);
    Success := MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES;

    for I := 0 to FUsers.Items.Count - 1 do
      if (Success and FUsers.Items[I].Selected) then
        List.Add(Client.UserByCaption(FUsers.Items[I].Caption));
    if (Client.DeleteUsers(List)) then
    begin
      FUsers.Items.Clear();
      TSUsersShow(Sender);
    end;
  end;

  List.Free();
end;

procedure TDServer.miPropertiesClick(Sender: TObject);
var
  Process: TCProcess;
  User: TCUser;
begin
  if (PageControl.ActivePage = TSProcesses) then
  begin
    Process := Client.ProcessById(StrToInt(FProcesses.Selected.Caption));

    DStatement.DatabaseName := Process.DatabaseName;
    DStatement.DateTime := Client.DateTime - Process.Time;
    DStatement.Host := Process.Host;
    DStatement.Id := StrToInt(Process.Name);
    DStatement.StatementTime := Process.Time;
    if (UpperCase(Process.Command) <> 'QUERY') then
      DStatement.SQL := ''
    else
      DStatement.SQL := Process.SQL + ';';
    DStatement.UserName := Process.UserName;
    DStatement.ViewType := vtProcess;

    DStatement.Execute();
  end
  else if (PageControl.ActivePage = TSHosts) then
  begin
    DHost.Client := Client;
    DHost.Host := Client.HostByCaption(FHosts.Selected.Caption);
    if (DHost.Execute()) then
    begin
      Client.Update();
      FHosts.Items.BeginUpdate();
      FHosts.Items.Clear();
      FHosts.Items.EndUpdate();
      TSHostsShow(Sender);
    end;
  end
  else if (PageControl.ActivePage = TSVariables) then
  begin
    DVariable.Client := Client;
    DVariable.Variable := Client.VariableByName(FVariables.Selected.Caption);
    if (DVariable.Execute()) then
    begin
      FVariables.Items.BeginUpdate();
      FVariables.Items.Clear();
      FVariables.Items.EndUpdate();
      TSVariablesShow(Sender);
    end;
  end
  else if (PageControl.ActivePage = TSUsers) then
  begin
    User := Client.UserByCaption(FUsers.Selected.Caption);

    DUser.Client := Client;
    DUser.User := User;
    if (DUser.Execute()) then
    begin
      FUsers.Items.BeginUpdate();
      FUsers.Items.Clear();
      FUsers.Items.EndUpdate();
      TSUsersShow(Sender);
    end;
  end;
end;

procedure TDServer.TSExtrasShow(Sender: TObject);
begin
  FUptime.Caption := SysUtils.DateTimeToStr(Client.StartTime, LocaleFormatSettings);

  FBShutdown.Enabled := Client.CanShutdown and (not Assigned(Client.UserRights) or Client.UserRights.RShutdown);
  FBFlushHosts.Enabled := (not Assigned(Client.UserRights) or Client.UserRights.RReload);
end;

procedure TDServer.TSHostsShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  if (FHosts.Items.Count = 0) then
  begin
    FHosts.DisableAlign(); FHosts.Items.BeginUpdate();

    for I := 0 to Client.Hosts.Count - 1 do
    begin
      Item := FHosts.Items.Add();
      Item.ImageIndex := iiHost;
      Item.Caption := Client.Hosts.Host[I].Caption;
    end;
    if (FHosts.Items.Count = 0) then
      FHosts.Selected := nil
    else
      FHosts.Selected := FHosts.Items[0];
    FHosts.ItemFocused := FHosts.Selected;

    FHosts.Columns[0].Tag := 1;
    ListViewShowSortDirection(FHosts);

    FHosts.EnableAlign(); FHosts.Items.EndUpdate();

    FHosts.Columns[0].Width := FHosts.ClientWidth;
  end;

  FHostsSelectItem(Sender, FHosts.Selected, Assigned(FHosts.Selected));
end;

procedure TDServer.TSPluginsShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  if (FPlugins.Items.Count = 0) then
  begin
    FPlugins.DisableAlign(); FPlugins.Items.BeginUpdate();

    for I := 0 to Client.Plugins.Count - 1 do
    begin
      Item := FPlugins.Items.Add();
      Item.Caption := Client.Plugins[I].Name;
      Item.ImageIndex := iiPlugin;
      Item.SubItems.Add(Client.Plugins[I].Comment);
    end;
    if (FPlugins.Items.Count = 0) then
      FPlugins.Selected := nil
    else
      FPlugins.Selected := FPlugins.Items[0];
    FPlugins.ItemFocused := FPlugins.Selected;

    FPlugins.Columns[0].Tag := 1;
    FPlugins.Columns[1].Tag := 0;
    ListViewShowSortDirection(FPlugins);

    FPlugins.EnableAlign(); FPlugins.Items.EndUpdate();

    FPlugins.Columns[0].Width := FPlugins.ClientWidth div 2;
    FPlugins.Columns[1].Width := FPlugins.ClientWidth - FPlugins.Columns[0].Width;
  end;

  FPluginsSelectItem(FPlugins, FPlugins.Selected, Assigned(FPlugins.Selected));
end;

procedure TDServer.TSProcessesShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  if (FProcesses.Items.Count = 0) then
  begin
    FProcesses.DisableAlign(); FProcesses.Items.BeginUpdate();

    Client.BeginSynchron();
    Client.Processes.Update();
    Client.EndSynchron();
    for I := 0 to Client.Processes.Count - 1 do
    begin
      Item := FProcesses.Items.Add();
      Item.ImageIndex := iiProcess;
      Item.Caption := Client.Processes[I].Name;
      Item.SubItems.Add(Client.Processes[I].UserName);
      Item.SubItems.Add(Client.Processes[I].Host);
      Item.SubItems.Add(Client.Processes[I].DatabaseName);
      Item.SubItems.Add(Client.Processes[I].Command);
      Item.SubItems.Add(SQLStmtToCaption(Client.Processes[I].SQL, 30));
      if (Client.Processes[I].Time = 0) then
        Item.SubItems.Add('???')
      else
        Item.SubItems.Add(ExecutionTimeToStr(Client.Processes[I].Time, 0));
      Item.SubItems.Add(Client.Processes[I].State);
    end;
    FProcesses.Selected := FProcesses.Items[0];
    FProcesses.ItemFocused := FProcesses.Selected;

    FProcesses.Columns[0].Tag := 1;
    ListViewShowSortDirection(FProcesses);

    FProcesses.EnableAlign(); FProcesses.Items.EndUpdate();

    FProcesses.Columns[0].Width := FProcesses.ClientWidth - (FProcesses.ClientWidth div FProcesses.Columns.Count) * (FProcesses.Columns.Count - 1);
    for I := 1 to FProcesses.Columns.Count - 1 do
    begin
      FProcesses.Columns[I].Tag := 0;
      FProcesses.Columns[I].Width := FProcesses.ClientWidth div FProcesses.Columns.Count;
    end;
  end;

  FProcessesSelectItem(Sender, FProcesses.Selected, Assigned(FProcesses.Selected));
end;

procedure TDServer.TSSlowSQLLogShow(Sender: TObject);
begin
  if (FSlowSQLLog.Lines.Text = '') then
  begin
    FSlowSQLLog.Text := Client.SlowLog;
    SendMessage(FSlowSQLLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end
end;

procedure TDServer.TSSQLLogShow(Sender: TObject);
begin
  if (FSQLLog.Lines.Text = '') then
  begin
    FSQLLog.Text := Client.Log;
    SendMessage(FSQLLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end
end;

procedure TDServer.TSStartupShow(Sender: TObject);
begin
  if (FStartup.Lines.Count = 0) then
    FStartup.Text := Trim(Client.VariableByName('init_connect').Value) + #13#10;
end;

procedure TDServer.TSStatiShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  if (FStati.Items.Count = 0) then
  begin
    FStati.DisableAlign(); FStati.Items.BeginUpdate();

    for I := 0 to Client.Stati.Count - 1 do
    begin
      Item := FStati.Items.Add();
      Item.ImageIndex := iiStatus;
      Item.Caption := Client.Stati[I].Name;
      Item.SubItems.Add(Client.Stati[I].Value);
    end;
    FStati.Selected := FStati.Items[0];
    FStati.ItemFocused := FStati.Selected;

    FStati.Columns[0].Tag := 1;
    FStati.Columns[1].Tag := 0;
    ListViewShowSortDirection(FStati);

    FStati.EnableAlign(); FStati.Items.EndUpdate();

    FStati.Columns[0].Width := FStati.ClientWidth div 2;
    FStati.Columns[1].Width := FStati.ClientWidth - FStati.Columns[0].Width;
  end;

  FStatiSelectItem(FStati, FStati.Selected, Assigned(FStati.Selected));
end;

procedure TDServer.TSUsersShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  if (FUsers.Items.Count = 0) then
  begin
    FUsers.DisableAlign(); FUsers.Items.BeginUpdate();

    for I := 0 to Client.Users.Count - 1 do
    begin
      Item := FUsers.Items.Add();
      Item.ImageIndex := iiUser;
      Item.Caption := Client.Users[I].Caption;
    end;
    FUsers.Selected := FUsers.Items[0];
    FUsers.ItemFocused := FUsers.Selected;

    FUsersResize(Sender);

    FUsers.Columns[0].Tag := 1;
    ListViewShowSortDirection(FUsers);

    FUsers.EnableAlign(); FUsers.Items.EndUpdate();

    FUsers.Columns[0].Width := FUsers.ClientWidth;
  end;

  FUsersSelectItem(FUsers, FUsers.Selected, Assigned(FUsers.Selected));
end;

procedure TDServer.TSVariablesShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  if (FVariables.Items.Count = 0) then
  begin
    FVariables.DisableAlign(); FVariables.Items.BeginUpdate();

    for I := 0 to Client.Variables.Count - 1 do
    begin
      Item := FVariables.Items.Add();
      Item.ImageIndex := iiVariable;
      Item.Caption := Client.Variables.Variable[I].Name;
      Item.SubItems.Add(Client.Variables.Variable[I].Value);
    end;
    FVariables.Selected := FVariables.Items[0];
    FVariables.ItemFocused := FVariables.Selected;

    FVariables.Columns[0].Tag := 1;
    FVariables.Columns[1].Tag := 0;
    ListViewShowSortDirection(FVariables);

    FVariables.EnableAlign(); FVariables.Items.EndUpdate();

    FVariables.Columns[0].Width := FVariables.ClientWidth div 2;
    FVariables.Columns[1].Width := FVariables.ClientWidth - FVariables.Columns[0].Width;
  end;

  FVariablesSelectItem(FVariables, FVariables.Selected, Assigned(FVariables.Selected));
end;

initialization
  FServer := nil;
end.
