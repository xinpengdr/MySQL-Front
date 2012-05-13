unit fDSessions;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList, 
  StdCtrls_Ext, ComCtrls_Ext, ExtCtrls_Ext, Forms_Ext,
  fClient, fSession, fBase, ImgList, ToolWin;

type
  TDSessions = class (TForm_Ext)
    ActionList: TActionList;
    aDelete: TAction;
    aEdit: TAction;
    aNew: TAction;
    aOpen: TAction;
    FBCancel: TButton;
    FBDelete: TButton;
    FBEdit: TButton;
    FBNew: TButton;
    FBOk: TButton;
    FSessions: TListView_Ext;
    GSessions: TGroupBox_Ext;
    miDelete: TMenuItem;
    miEdit: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    N2: TMenuItem;
    PopupMenu: TPopupMenu;
    PSessions: TPanel_Ext;
    procedure aDeleteExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure FBOkEnabledCheck(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSessionsColumnClick(Sender: TObject; Column: TListColumn);
    procedure FSessionsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FSessionsDblClick(Sender: TObject);
    procedure FSessionsResize(Sender: TObject);
    procedure FSessionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PopupMenuPopup(Sender: TObject);
  private
    FSelected: TSSession;
    procedure ListViewShowSortDirection(const ListView: TListView);
    procedure SetFSessions(const ASelected: TSSession);
    procedure CMApplyAutosize(var Message: TMessage); message CM_APPLYAUTOSIZE;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Open: Boolean;
    function Execute(): Boolean;
    property Selected: TSSession read FSelected;
  end;

function DSessions(): TDSessions;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommCtrl, Math, StrUtils,
  CommCtrl_Ext,
  fDSession, fPreferences;

var
  FSessions: TDSessions;

function DSessions(): TDSessions;
begin
  if (not Assigned(FSessions)) then
  begin
    Application.CreateForm(TDSessions, FSessions);
    FSessions.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FSessions;
end;

{ TDSessions ******************************************************************}

procedure TDSessions.aDeleteExecute(Sender: TObject);
begin
  if (MsgBox(Preferences.LoadStr(46, FSessions.Selected.Caption), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
    if (Sessions.DeleteSession(Sessions.SessionByName(FSessions.Selected.Caption))) then
    begin
      SetFSessions(nil);
      FBCancel.Caption := Preferences.LoadStr(231);
    end;

  ActiveControl := FSessions;
end;

procedure TDSessions.aEditExecute(Sender: TObject);
begin
  DSession.Session := Sessions.SessionByName(FSessions.Selected.Caption);
  DSession.Username := DSession.Session.Connection.User;
  DSession.Password := DSession.Session.Connection.Password;
  DSession.ShowType := stDefault;
  if (DSession.Execute()) then
  begin
    SetFSessions(Sessions.SessionByName(DSession.SessionName));
    FBCancel.Caption := Preferences.LoadStr(231);
  end;
  ActiveControl := FSessions;
end;

procedure TDSessions.aNewExecute(Sender: TObject);
begin
  DSession.Session := nil;
  DSession.Username := 'root';
  DSession.Password := '';
  DSession.ShowType := stDefault;
  if (DSession.Execute()) then
  begin
    SetFSessions(Sessions.SessionByName(DSession.SessionName));
    FBCancel.Caption := Preferences.LoadStr(231);
  end;

  ActiveControl := FSessions;
end;

procedure TDSessions.aOpenExecute(Sender: TObject);
begin
  if (Open) then
    FBOk.Click()
  else
    if (Boolean(SendMessage(Application.MainForm.Handle, CM_ADDTAB, 0, LPARAM(Sessions.SessionByName(FSessions.Selected.Caption).Desktop.Address)))) then
      FBOk.Click();
end;

procedure TDSessions.CMApplyAutosize(var Message: TMessage);
begin
  ListViewShowSortDirection(FSessions);
end;

procedure TDSessions.CMChangePreferences(var Message: TMessage);
begin
  FSessions.Canvas.Font := Font;

  Preferences.SmallImages.GetIcon(40, Icon);

  GSessions.Caption := ReplaceStr(Preferences.LoadStr(25), '&', '');
  FSessions.Columns.Items[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FSessions.Columns.Items[1].Caption := ReplaceStr(Preferences.LoadStr(693), '&', '');
  aOpen.Caption := ReplaceStr(Preferences.LoadStr(581), '&', '');
  aNew.Caption := Preferences.LoadStr(26) + '...';
  aEdit.Caption := Preferences.LoadStr(97) + '...';
  aDelete.Caption := Preferences.LoadStr(559);

  FBOk.Caption := Preferences.LoadStr(581);
end;

function TDSessions.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDSessions.FBOkEnabledCheck(Sender: TObject);
begin
  FBOk.Enabled := Assigned(FSessions.Selected);
end;

procedure TDSessions.FormActivate(Sender: TObject);
begin
  if (FSessions.Items.Count = 0) then
    aNew.Execute();
end;

procedure TDSessions.FormCreate(Sender: TObject);
begin
  FSessions.SmallImages := Preferences.SmallImages;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Sessions.Width >= Width) and (Preferences.Sessions.Height >= Height)) then
  begin
    Width := Preferences.Sessions.Width;
    Height := Preferences.Sessions.Height;
  end;

  SetWindowLong(ListView_GetHeader(FSessions.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FSessions.Handle), GWL_STYLE) or HDS_NOSIZING);
end;

procedure TDSessions.FormHide(Sender: TObject);
begin
  if (ModalResult <> mrCancel) then
  begin
    FSelected := Sessions.SessionByName(FSessions.Selected.Caption);

    Sessions.Default := Selected;
  end;

  Preferences.Sessions.Height := Height;
  Preferences.Sessions.SelectOrder := FSessions.Tag;
  Preferences.Sessions.Width := Width;
end;

procedure TDSessions.FormShow(Sender: TObject);
begin
  if (not Open) then
    Caption := ReplaceStr(Preferences.LoadStr(25), '&', '')
  else
    Caption := Preferences.LoadStr(1);

  FSessions.Tag := Preferences.Sessions.SelectOrder;
  if (FSessions.Tag = 1) then
    FSessions.Column[FSessions.Tag].Tag := -1
  else
    FSessions.Column[FSessions.Tag].Tag := 1;

  SetFSessions(Sessions.Default);

  FBOk.Visible := Open;
  if (not Open) then
    FBCancel.Caption := Preferences.LoadStr(231)
  else
    FBCancel.Caption := Preferences.LoadStr(30);

  FBOk.Default := Open;
  FBCancel.Default := not FBOk.Default;

  ActiveControl := FSessions;

  FBOkEnabledCheck(Sender);
end;

procedure TDSessions.FSessionsColumnClick(Sender: TObject;
  Column: TListColumn);
var
  I: Integer;
begin
  if (Assigned(Sender)) then
    for I := 0 to FSessions.Columns.Count - 1 do
      if (FSessions.Column[I] <> Column) then
        FSessions.Column[I].Tag := 0
      else if (FSessions.Column[I].Tag < 0) then
        FSessions.Column[I].Tag := 1
      else if (FSessions.Column[I].Tag > 0) then
        FSessions.Column[I].Tag := -1
      else if (I = 1) then
        FSessions.Column[I].Tag := -1
      else
        FSessions.Column[I].Tag := 1;

  FSessions.Tag := Column.Index;
  FSessions.AlphaSort();

  ListViewShowSortDirection(FSessions);
end;

procedure TDSessions.FSessionsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Column: TListColumn;
  DateTime1: TDateTime;
  DateTime2: TDateTime;
begin
  Column := FSessions.Column[FSessions.Tag];

  if (Column.Index = 0) then
    Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)))
  else
  begin
    if (Item1.SubItems[Column.Index - 1] = '???') then
      DateTime1 := 0
    else
      DateTime1 := StrToDateTime(Item1.SubItems[Column.Index - 1], LocaleFormatSettings);
    if (Item2.SubItems[Column.Index - 1] = '???') then
      DateTime2 := 0
    else
      DateTime2 := StrToDateTime(Item2.SubItems[Column.Index - 1], LocaleFormatSettings);
    Compare := Sign(DateTime1 - DateTime2);
  end;

  if (Column.Tag < 0) then
    Compare := - Compare;
end;

procedure TDSessions.FSessionsDblClick(Sender: TObject);
begin
  if (Open and FBOk.Enabled) then
    FBOk.Click()
  else if (not Open and aEdit.Enabled) then
    aEdit.Execute();
end;

procedure TDSessions.FSessionsResize(Sender: TObject);
begin
  if (Assigned(FSessions.ItemFocused) and (FSessions.Items.Count > 1) and (FSessions.ItemFocused.Position.Y - FSessions.ClientHeight + (FSessions.Items[1].Top - FSessions.Items[0].Top) > 0)) then
    FSessions.Scroll(0, FSessions.ItemFocused.Position.Y - FSessions.ClientHeight + (FSessions.Items[1].Top - FSessions.Items[0].Top));

  if (not (csDestroying in ComponentState)) then
    PostMessage(Handle, CM_APPLYAUTOSIZE, 0, 0);
end;

procedure TDSessions.FSessionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Session: TSSession;
begin
  if (not Assigned(Item)) then
    Session := nil
  else
    Session := Sessions.SessionByName(Item.Caption);

  aEdit.Enabled := Assigned(Item) and Selected;
  aDelete.Enabled := Assigned(Item) and Selected and Assigned(Session) and (Session.DesktopCount = 0);

  FBOkEnabledCheck(Sender);
  FBOk.Default := FBOk.Enabled;
end;

procedure TDSessions.ListViewShowSortDirection(const ListView: TListView);
var
  Column: TListColumn;
  HDItem: THDItem;
  I: Integer;
  MaxWidth: Integer;
begin
  Column := ListView.Column[ListView.Tag];

  MaxWidth := 0;
  for I := 0 to FSessions.Items.Count - 1 do
    if (MaxWidth < FSessions.Canvas.TextWidth(FSessions.Items[I].SubItems[0])) then
      MaxWidth := FSessions.Canvas.TextWidth(FSessions.Items[I].SubItems[0]);
  if (MaxWidth = 0) then
    MaxWidth := FSessions.Width div 2;

  HDItem.Mask := HDI_WIDTH or HDI_FORMAT;
  for I := 0 to ListView.Columns.Count - 1 do
    if (BOOL(SendMessage(ListView_GetHeader(ListView.Handle), HDM_GETITEM, I, LParam(@HDItem)))) then
    begin
      case (ListView.Column[I].Tag) of
        -1: HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        1: HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP;
        else HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN;
      end;

      case (I) of
        0: HDItem.cxy := FSessions.ClientWidth - (MaxWidth + 20);
        1: HDItem.cxy := MaxWidth + 20;
      end;
      SendMessage(ListView_GetHeader(ListView.Handle), HDM_SETITEM, I, LParam(@HDItem));
    end;

  if (ComCtl32MajorVersion >= 6) then
    SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, Column.Index, 0);
end;

procedure TDSessions.PopupMenuPopup(Sender: TObject);
begin
  aOpen.Enabled := Assigned(FSessions.Selected) and Assigned(Sessions.SessionByURI(Sessions.SessionByName(FSessions.Selected.Caption).Desktop.Address));
  miOpen.Default := Open;
  miEdit.Default := not miOpen.Default;
  ShowEnabledItems(PopupMenu.Items);
end;

procedure TDSessions.SetFSessions(const ASelected: TSSession);
var
  I: Integer;
  Item: TListItem;
begin
  FSessions.DisableAlign(); FSessions.Items.BeginUpdate();

  FSessions.Items.Clear();

  if (Sessions.Count = 0) then
    FSessionsSelectItem(FSessions, nil, False)
  else
    for I := 0 to Sessions.Count - 1 do
    begin
      Item := FSessions.Items.Add();
      Item.Caption := Sessions[I].Name;
      if (Sessions[I].LastLogin = 0) then
        Item.SubItems.Add('???')
      else
        Item.SubItems.Add(DateTimeToStr(Sessions[I].LastLogin, LocaleFormatSettings));
      if (Sessions[I].ImageIndex < 0) then
        Item.ImageIndex := iiServer
      else
        Item.ImageIndex := Sessions[I].ImageIndex;
    end;

  FSessionsColumnClick(nil, FSessions.Column[FSessions.Tag]);

  if (ASelected = nil) and (FSessions.Items.Count > 0) then
    FSessions.Selected := FSessions.Items.Item[0]
  else
    for I := 0 to FSessions.Items.Count - 1 do
      if (ASelected <> nil) and (FSessions.Items.Item[I].Caption = ASelected.Name) then
        FSessions.Selected := FSessions.Items.Item[I];

  FSessions.ItemFocused := FSessions.Selected;
  FSessionsResize(nil);

  FSessions.EnableAlign(); FSessions.Items.EndUpdate();

  Perform(CM_APPLYAUTOSIZE, 0, 0);
end;

initialization
  FSessions := nil;
end.

