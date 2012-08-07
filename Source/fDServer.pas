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
    FLCharacterSet: TLabel;
    FLComment: TLabel;
    FLHost: TLabel;
    FLibVersion: TLabel;
    FLLibVersion: TLabel;
    FLUptime: TLabel;
    FLUser: TLabel;
    FLVersion: TLabel;
    FPlugins: TListView;
    FSlowSQLLog: TSynMemo;
    FSQLLog: TSynMemo;
    FStartup: TSynMemo;
    FUptime: TLabel;
    FUser: TLabel;
    FVersion: TLabel;
    GConnection: TGroupBox_Ext;
    GServer: TGroupBox_Ext;
    GServiceHosts: TGroupBox_Ext;
    GServiceServer: TGroupBox_Ext;
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
    TSPlugins: TTabSheet;
    TSSlowSQLLog: TTabSheet;
    TSSQLLog: TTabSheet;
    TSStartup: TTabSheet;
    procedure FBFlushHostsClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FBShutdownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewKeyPress(Sender: TObject; var Key: Char);
    procedure ListViewResize(Sender: TObject);
    procedure TSExtrasShow(Sender: TObject);
    procedure TSPluginsShow(Sender: TObject);
    procedure TSSlowSQLLogShow(Sender: TObject);
    procedure TSSQLLogShow(Sender: TObject);
    procedure TSStartupShow(Sender: TObject);
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

{ TDServer ********************************************************************}

procedure TDServer.FormCreate(Sender: TObject);
begin
  Preferences.SmallImages.GetIcon(iiServer, Icon);

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

  PageControl.ActivePage := TSBasics;
end;

procedure TDServer.FormHide(Sender: TObject);
begin
  Preferences.Server.Width := Width;
  Preferences.Server.Height := Height;

  FSQLLog.Lines.Clear();

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

  TSSQLLog.TabVisible := Client.LogActive;
  TSSlowSQLLog.TabVisible := Client.SlowLogActive;
  TSStartup.TabVisible := Assigned(Client.VariableByName('init_connect')) and (Client.VariableByName('init_connect').Value <> '');
  TSPlugins.TabVisible := Assigned(Client.Plugins);

  FBCancel.Caption := Preferences.LoadStr(231);

  PageControl.ActivePage := TSBasics;

  ActiveControl := FBCancel;
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

procedure TDServer.TSExtrasShow(Sender: TObject);
begin
  FUptime.Caption := SysUtils.DateTimeToStr(Client.StartTime, LocaleFormatSettings);

  FBShutdown.Enabled := Client.CanShutdown and (not Assigned(Client.UserRights) or Client.UserRights.RShutdown);
  FBFlushHosts.Enabled := (not Assigned(Client.UserRights) or Client.UserRights.RReload);
end;

procedure TDServer.TSPluginsShow(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  Client.BeginSynchron();
  Client.Plugins.Update();
  Client.EndSynchron();
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

initialization
  FServer := nil;
end.
