unit fDUser;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, 
  SynEdit, SynMemo,
  Forms_Ext,  
  fClient, fBase, StdCtrls_Ext, ComCtrls_Ext, Vcl.ExtCtrls;

type
  TDUser = class (TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FBRightsDelete: TButton;
    FBRightsEdit: TButton;
    FBRightsNew: TButton;
    FConnectionsPerHour: TEdit;
    FHost: TEdit;
    FLConnectionsPerHour: TLabel;
    FLHost: TLabel;
    FLPassword: TLabel;
    FLQueriesPerHour: TLabel;
    FLUpdatesPerHour: TLabel;
    FLUser: TLabel;
    FLUserConnections: TLabel;
    FName: TEdit;
    FPassword: TEdit;
    FQueriesPerHour: TEdit;
    FRights: TListView;
    FSlowSQLLog: TSynMemo;
    FSource: TSynMemo;
    FSQLLog: TSynMemo;
    FUDConnectionsPerHour: TUpDown;
    FUDQueriesPerHour: TUpDown;
    FUDUpdatesPerHour: TUpDown;
    FUDUserConnections: TUpDown;
    FUpdatesPerHour: TEdit;
    FUserConnections: TEdit;
    GBasics: TGroupBox_Ext;
    GLimits: TGroupBox_Ext;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    PageControl: TPageControl;
    PSQLWait: TPanel;
    TSBasics: TTabSheet;
    TSLimits: TTabSheet;
    TSRights: TTabSheet;
    TSSlowSQLLog: TTabSheet;
    TSSource: TTabSheet;
    TSSQLLog: TTabSheet;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FBRightsDeleteClick(Sender: TObject);
    procedure FBRightsEditClick(Sender: TObject);
    procedure FBRightsNewClick(Sender: TObject);
    procedure FHostExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FRightsDblClick(Sender: TObject);
    procedure FRightsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewResize(Sender: TObject);
    procedure TSRightsShow(Sender: TObject);
    procedure TSSlowSQLLogShow(Sender: TObject);
    procedure TSSourceShow(Sender: TObject);
    procedure TSSQLLogShow(Sender: TObject);
  private
    NewUser: TCUser;
    RightsModified: Boolean;
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure FRightsRefresh(Sender: TObject);
    procedure ListViewShowSortDirection(const ListView: TListView);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
  public
    Client: TCClient;
    User: TCUser;
    function Execute(): Boolean;
  end;

function DUser(): TDUser;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommCtrl, Math, StrUtils,
  CommCtrl_Ext,
  SQLUtils, MySQLDB,
  fDUserRight, fPreferences;

var
  FUser: TDUser;

function DUser(): TDUser;
begin
  if (not Assigned(FUser)) then
  begin
    Application.CreateForm(TDUser, FUser);
    FUser.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FUser;
end;

{ TDUser **********************************************************************}

procedure TDUser.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiUser, Icon);

  TSBasics.Caption := ReplaceStr(Preferences.LoadStr(108), '&', '');
  GBasics.Caption := Preferences.LoadStr(85);
  FLUser.Caption := Preferences.LoadStr(561) + ':';
  FLHost.Caption := Preferences.LoadStr(289) + ':';
  FLPassword.Caption := Preferences.LoadStr(283) + ':';

  TSRights.Caption := Preferences.LoadStr(284);
  FBRightsNew.Caption := Preferences.LoadStr(26) + '...';
  FBRightsEdit.Caption := Preferences.LoadStr(97) + '...';
  FBRightsDelete.Caption := Preferences.LoadStr(28);

  TSLimits.Caption := Preferences.LoadStr(294);
  GLimits.Caption := Preferences.LoadStr(294);
  FLConnectionsPerHour.Caption := Preferences.LoadStr(292) + ':';
  FLQueriesPerHour.Caption := Preferences.LoadStr(290) + ':';
  FLUpdatesPerHour.Caption := Preferences.LoadStr(291) + ':';
  FLUserConnections.Caption := Preferences.LoadStr(871) + ':';

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

  TSSource.Caption := Preferences.LoadStr(198);
  FSource.Font.Name := Preferences.SQLFontName;
  FSource.Font.Style := Preferences.SQLFontStyle;
  FSource.Font.Color := Preferences.SQLFontColor;
  FSource.Font.Size := Preferences.SQLFontSize;
  FSource.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FSource.Gutter.Font.Color := clWindowText
  else
    FSource.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FSource.Gutter.Color := clBtnFace
  else
    FSource.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FSource.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;

  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

procedure TDUser.CMPostShow(var Message: TMessage);
begin
  FBRightsNew.Enabled := False;
  FBRightsEdit.Enabled := False;
  FBRightsDelete.Enabled := False;

  FBOk.Enabled := False;
end;

function TDUser.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDUser.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDUser.FBOkCheckEnabled(Sender: TObject);
var
  Username: string;
begin
  Username := Trim(FName.Text);
  if (Trim(FHost.Text) <> '') then
    Username := Username + '@' + Trim(FHost.Text);

  FBOk.Enabled := (not Assigned(Client.UserByName(Username)) or (lstrcmpi(PChar(Username), PChar(NewUser.Name)) = 0))
    and (FRights.Items.Count > 0);

  TSSource.TabVisible := False;
end;

procedure TDUser.FBRightsDeleteClick(Sender: TObject);
begin
  NewUser.DeleteRight(NewUser.Right[FRights.ItemIndex]);
  FRightsRefresh(Sender);

  FBOkCheckEnabled(Sender);
end;

procedure TDUser.FBRightsEditClick(Sender: TObject);
begin
  DUserRight.Client := Client;
  DUserRight.User := NewUser;
  DUserRight.UserRight := NewUser.RightByCaption(FRights.Selected.Caption);
  if (DUserRight.Execute()) then
  begin
    FRightsRefresh(Sender);

    FBOkCheckEnabled(Sender);
  end;
  ActiveControl := FRights;
end;

procedure TDUser.FBRightsNewClick(Sender: TObject);
begin
  DUserRight.Client := Client;
  DUserRight.User := NewUser;
  DUserRight.UserRight := nil;
  if (DUserRight.Execute()) then
  begin
    FRightsRefresh(Sender);

    FBOkCheckEnabled(Sender);
  end;

  ActiveControl := FRights;
end;

procedure TDUser.FHostExit(Sender: TObject);
begin
  if (Trim(FHost.Text) = '') then
    FHost.Text := '%';
end;

procedure TDUser.FormClientEvent(const Event: TCClient.TEvent);
begin
  if ((Event.EventType in [ceItemCreated, ceItemAltered]) and (Event.CItem is TCUser)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Client.ErrorCode <> 0)) then
  begin
    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end;
end;

procedure TDUser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
  begin
    if (Trim(FHost.Text) = '') then
      NewUser.Name := Trim(FName.Text)
    else
      NewUser.Name := Trim(FName.Text) + '@' + Trim(FHost.Text);
    if (FPassword.Text <> StringOfChar('*', FPassword.MaxLength)) then
      NewUser.NewPassword := Trim(FPassword.Text)
    else
      NewUser.NewPassword := NewUser.RawPassword;
    NewUser.ConnectionsPerHour := FUDConnectionsPerHour.Position;
    NewUser.QueriesPerHour := FUDQueriesPerHour.Position;
    NewUser.UpdatesPerHour := FUDUpdatesPerHour.Position;
    NewUser.UserConnections := FUDUserConnections.Position;
    for I := 0 to NewUser.RightCount - 1 do
      NewUser.Right[I].NewPassword := NewUser.NewPassword;

    if (not Assigned(User)) then
      CanClose := Client.AddUser(NewUser)
    else
      CanClose := Client.UpdateUser(User, NewUser);

    PageControl.Visible := CanClose or not Client.Asynchron;
    PSQLWait.Visible := not PageControl.Visible;
    if (PSQLWait.Visible) then
      ModalResult := mrNone;

    FBOk.Enabled := False;
  end;
end;

procedure TDUser.FormCreate(Sender: TObject);
begin
  NewUser := nil;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.User.Width >= Width) and (Preferences.User.Height >= Height)) then
  begin
    Width := Preferences.User.Width;
    Height := Preferences.User.Height;
  end;

  FRights.SmallImages := Preferences.SmallImages;

  FSQLLog.Highlighter := MainHighlighter;
  FSlowSQLLog.Highlighter := MainHighlighter;
  FSource.Highlighter := MainHighlighter;

  PageControl.ActivePage := TSBasics;
end;

procedure TDUser.FormHide(Sender: TObject);
begin
  Client.UnRegisterEventProc(FormClientEvent);

  Preferences.User.Width := Width;
  Preferences.User.Height := Height;

  FRights.Items.BeginUpdate();
  FRights.Items.Clear();
  FRights.Items.EndUpdate();
  FSlowSQLLog.Lines.Clear();
  FSQLLog.Lines.Clear();
  FSource.Lines.Clear();

  if (Assigned(NewUser)) then
    FreeAndNil(NewUser);

  PageControl.ActivePage := TSBasics;
end;

procedure TDUser.FormShow(Sender: TObject);
var
  NewUserRight: TCUserRight;
  UserName: string;
begin
  Client.RegisterEventProc(FormClientEvent);

  NewUser := TCUser.Create(Client.Users);
  if (Assigned(User)) then
    NewUser.Assign(User);

  RightsModified := False;
  if (not Assigned(User)) then
  begin
    Caption := Preferences.LoadStr(286);
    HelpContext := 1077;
  end
  else
  begin
    Caption := Preferences.LoadStr(842, NewUser.Caption);
    HelpContext := 1059;
  end;

  if (not Assigned(User)) then
  begin
    NewUserRight := TCUserRight.Create();
    NewUser.AddRight(NewUserRight);
    FreeAndNil(NewUserRight);

    FName.Text := Preferences.LoadStr(280);
    while (Assigned(Client.UserByCaption(FName.Text))) do
    begin
      UserName := FName.Text;
      Delete(UserName, 1, Length(Preferences.LoadStr(280)));
      if (UserName = '') then UserName := '1';
      UserName := Preferences.LoadStr(280) + IntToStr(StrToInt(UserName) + 1);
      FName.Text := UserName;
    end;

    FHost.Text := '%';
    FPassword.Text := '';

    FUDConnectionsPerHour.Position := 0;
    FUDQueriesPerHour.Position := 0;
    FUDUpdatesPerHour.Position := 0;
    FUDUserConnections.Position := 0;
  end
  else
  begin
    FName.Text := NewUser.Login;
    FHost.Text := NewUser.Host;
    if (NewUser.RawPassword = '') then
      FPassword.Text := ''
    else
      FPassword.Text := StringOfChar('*', FPassword.MaxLength);

    FUDConnectionsPerHour.Position := NewUser.ConnectionsPerHour;
    FUDQueriesPerHour.Position := NewUser.QueriesPerHour;
    FUDUpdatesPerHour.Position := NewUser.UpdatesPerHour;
    FUDUserConnections.Position := NewUser.UserConnections;
  end;

  FUserConnections.Visible := Client.ServerVersion >= 50003;
  FLUserConnections.Visible := FUserConnections.Visible;
  FUDUserConnections.Visible := FUserConnections.Visible;

  TSSlowSQLLog.TabVisible := Assigned(User) and Client.SlowLogActive;
  TSSQLLog.TabVisible := Assigned(User) and Client.LogActive;
  TSSource.TabVisible := Assigned(User);

  PageControl.ActivePage := TSBasics;
  FRightsRefresh(Sender);

  ActiveControl := FBCancel;
  ActiveControl := FName;

  PostMessage(Self.Handle, CM_POSTSHOW, 0, 0);
end;

procedure TDUser.FRightsDblClick(Sender: TObject);
begin
  if (Assigned(FRights.Selected)) then
    FBRightsEdit.Click();
end;

procedure TDUser.FRightsRefresh(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  FRights.Clear();

  for I := 0 to NewUser.RightCount - 1 do
  begin
    Item := FRights.Items.Add();
    Item.Caption := NewUser.Right[I].Caption;
    if (NewUser.Right[I].FieldName <> '') then
      Item.ImageIndex := iiField
    else if (NewUser.Right[I].TableName <> '') then
      Item.ImageIndex := iiBaseTable
    else if (NewUser.Right[I].ProcedureName <> '') then
      Item.ImageIndex := iiProcedure
    else if (NewUser.Right[I].FunctionName <> '') then
      Item.ImageIndex := iiFunction
    else if (NewUser.Right[I].DatabaseName <> '') then
      Item.ImageIndex := iiDatabase
    else
      Item.ImageIndex := iiServer;
  end;

  if (FRights.Items.Count > 0) then
    FRights.Selected := FRights.Items[0];
end;

procedure TDUser.FRightsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  I: Integer;
  RGrant: Boolean;
begin
  RGrant := not Assigned(Client.User);
  if (Assigned(Client.User)) then
    for I := 0 to Client.User.RightCount - 1 do
      RGrant := RGrant or Client.User.Right[I].RGrant;

  FBRightsNew.Enabled := RGrant;
  FBRightsEdit.Enabled := Selected;
  FBRightsDelete.Enabled := Selected and (FRights.Items.Count > 1) and RGrant;
end;

procedure TDUser.ListViewColumnClick(Sender: TObject;
  Column: TListColumn);
var
  I: Integer;
  ListView: TListView;
begin
  if (Sender is TListView) then
  begin
    ListView := TListView(Sender);

    for I := 0 to ListView.Columns.Count - 1 do
      if (ListView.Column[I] <> Column) then
        ListView.Column[I].Tag := 0
      else if (ListView.Column[I].Tag < 0) then
        ListView.Column[I].Tag := 1
      else if (ListView.Column[I].Tag > 0) then
        ListView.Column[I].Tag := -1
      else
        ListView.Column[I].Tag := 1;

    ListView.Tag := Column.Index;
    ListView.AlphaSort();

    ListViewShowSortDirection(ListView);
  end;
end;

procedure TDUser.ListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Column: TListColumn;
  ListView: TListView;
begin
  ListView := TListView(Sender);
  Column := ListView.Column[ListView.Tag];

  if (Column.Index = 0) then
    Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)))
  else
    Compare := Sign(lstrcmpi(PChar(Item1.SubItems[Column.Index - 1]), PChar(Item2.SubItems[Column.Index - 1])));

  if (Column.Tag < 0) then
    Compare := - Compare;
end;

procedure TDUser.ListViewResize(Sender: TObject);
begin
  if (Sender is TListView) then
    ListViewShowSortDirection(TListView(Sender));
end;

procedure TDUser.ListViewShowSortDirection(const ListView: TListView);
var
  Column: TListColumn;
  HDItem: THDItem;
  I: Integer;
begin
  Column := ListView.Column[ListView.Tag];

  HDItem.Mask := HDI_FORMAT;
  for I := 0 to ListView.Columns.Count - 1 do
    if (BOOL(SendMessage(ListView_GetHeader(ListView.Handle), HDM_GETITEM, I, LPARAM(@HDItem)))) then
    begin
      case (ListView.Column[I].Tag) of
        -1: HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        1: HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP;
        else HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN;
      end;
      SendMessage(ListView_GetHeader(ListView.Handle), HDM_SETITEM, I, LPARAM(@HDItem));
    end;

  if ((ComCtl32MajorVersion >= 6) and not CheckWin32Version(6, 1)) then
    SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, Column.Index, 0);
end;

procedure TDUser.TSRightsShow(Sender: TObject);
begin
  FRightsRefresh(Sender);

  ActiveControl := FRights;
end;

procedure TDUser.TSSlowSQLLogShow(Sender: TObject);
begin
  if (FSlowSQLLog.Lines.Text = '') then
  begin
    FSlowSQLLog.Text := User.SlowSQLLog;
    SendMessage(FSlowSQLLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end
end;

procedure TDUser.TSSourceShow(Sender: TObject);
begin
  if (FSource.Lines.Text = '') then
    FSource.Text := User.Source;
end;

procedure TDUser.TSSQLLogShow(Sender: TObject);
begin
  if (FSQLLog.Lines.Text = '') then
    FSQLLog.Text := User.SQLLog;
end;

initialization
  FUser := nil;
end.
