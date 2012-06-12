unit fDHost;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  SynEdit, SynMemo,
  Forms_Ext, StdCtrls_Ext,
  fClient, fBase, Menus, Vcl.ExtCtrls;

type
  TDHost = class(TForm_Ext)
    FAll: TRadioButton;
    FBCancel: TButton;
    FBDelete: TButton;
    FBEdit: TButton;
    FBHelp: TButton;
    FBNew: TButton;
    FBOk: TButton;
    FDatabases: TListView;
    FHost: TRadioButton;
    FHosts: TEdit;
    FSource: TSynMemo;
    GBasics: TGroupBox_Ext;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    PageControl: TPageControl;
    PSQLWait: TPanel;
    TSBasics: TTabSheet;
    TSDatabases: TTabSheet;
    TSSource: TTabSheet;
    procedure FBDeleteClick(Sender: TObject);
    procedure FBEditClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FBNewClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FDatabasesDblClick(Sender: TObject);
    procedure FDatabasesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FHostsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TSDatabasesShow(Sender: TObject);
    procedure TSSourceShow(Sender: TObject);
  private
    NewHost: TCHost;
    procedure FDatabasesRefresh(Sender: TObject);
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Client: TCClient;
    Host: TCHost;
    function Execute(): Boolean;
  end;

function DHost(): TDHost;

implementation {***************************************************************}

uses
  StrUtils,
  fPreferences, fDHostDatabase;

{$R *.dfm}

var
  FHosts: TDHost;

function DHost(): TDHost;
begin
  if (not Assigned(FHosts)) then
  begin
    Application.CreateForm(TDHost, FHosts);
    FHosts.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FHosts;
end;

{ TDHost **********************************************************************}

procedure TDHost.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiHost, Icon);

  TSBasics.Caption := ReplaceStr(Preferences.LoadStr(108), '&', '');
  GBasics.Caption := Preferences.LoadStr(85);
  FAll.Caption := Preferences.LoadStr(327);
  FHost.Caption := Preferences.LoadStr(305) + ':';

  TSDatabases.Caption := Preferences.LoadStr(265);
  FBNew.Caption := Preferences.LoadStr(26) + '...';
  FBDelete.Caption := Preferences.LoadStr(559);
  FBEdit.Caption := Preferences.LoadStr(97) + '...';

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

function TDHost.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDHost.FBDeleteClick(Sender: TObject);
begin
  NewHost.Databases.DeleteDatabase(NewHost.Databases[FDatabases.ItemIndex]);
  FDatabasesRefresh(Sender);

  FBOkCheckEnabled(Sender);
end;

procedure TDHost.FBEditClick(Sender: TObject);
begin
  DHostDatabase.Host := NewHost;
  DHostDatabase.HostDatabase := NewHost.Databases[FDatabases.ItemIndex];
  if (DHostDatabase.Execute()) then
  begin
    FDatabasesRefresh(Sender);

    FBOkCheckEnabled(Sender);
  end;

  ActiveControl := FDatabases;
end;

procedure TDHost.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDHost.FBNewClick(Sender: TObject);
var
  Selected: TListItem;
begin
  DHostDatabase.Host := NewHost;
  DHostDatabase.HostDatabase := nil;
  if (DHostDatabase.Execute()) then
  begin
    FDatabasesRefresh(Sender);

    Selected := FDatabases.Selected;
    FDatabases.Selected := Selected;

    FBOkCheckEnabled(Sender);
  end;

  ActiveControl := FDatabases;
end;

procedure TDHost.FBOkCheckEnabled(Sender: TObject);
begin
  FBOk.Enabled := (FAll.Checked or FHost.Checked and (FHosts.Text <> '')) and True;

  if (FAll.Checked) then
    FBOk.Enabled := FBOk.Enabled and not Assigned(Client.HostByName('')) or (Client.HostByName('') = Host)
  else
    FBOk.Enabled := FBOk.Enabled and not Assigned(Client.HostByName(FHosts.Text)) or (Client.HostByName(FHosts.Text) = Host);

  TSSource.TabVisible := False;
end;

procedure TDHost.FDatabasesDblClick(Sender: TObject);
begin
  FBEdit.Click();
end;

procedure TDHost.FDatabasesRefresh(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin
  FDatabases.Clear();

  for I := 0 to NewHost.Databases.Count - 1 do
  begin
    Item := FDatabases.Items.Add();
    if (NewHost.Databases[I].Name = '') then
      Item.Caption := '<' + ReplaceStr(Preferences.LoadStr(300), '&', '') + '>'
    else
      Item.Caption := NewHost.Databases[I].Name;
    Item.ImageIndex := iiDatabase;
  end;

  if (FDatabases.Items.Count > 0) then
    FDatabases.Items[0].Selected := True;
end;

procedure TDHost.FDatabasesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  FBDelete.Enabled := FDatabases.Items.Count > 1;
  FBEdit.Enabled := Selected;
end;

procedure TDHost.FHostsChange(Sender: TObject);
begin
  if (FHosts.Text = '') then
    FAll.Checked := True
  else
    FHost.Checked := True;

  FBOkCheckEnabled(Sender);
end;

procedure TDHost.FormClientEvent(const Event: TCClient.TEvent);
begin
  if ((Event.EventType in [ceItemCreated, ceItemAltered]) and (Event.CItem is TCHost)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Client.ErrorCode <> 0)) then
  begin
    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end;
end;

procedure TDHost.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
  begin
    if (FAll.Checked) then
      NewHost.Name := ''
    else
      NewHost.Name := Trim(FHosts.Text);


    if (not Assigned(Host)) then
      CanClose := Client.AddHost(NewHost)
    else
      CanClose := Client.UpdateHost(Host, NewHost);

    PageControl.Visible := CanClose or not Client.Asynchron;
    PSQLWait.Visible := not PageControl.Visible;
    if (PSQLWait.Visible) then
      ModalResult := mrNone;

    FBOk.Enabled := False;
  end;
end;

procedure TDHost.FormCreate(Sender: TObject);
begin
  NewHost := nil;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Host.Width >= Width) and (Preferences.Host.Height >= Height)) then
  begin
    Width := Preferences.Host.Width;
    Height := Preferences.Host.Height;
  end;

  FDatabases.SmallImages := Preferences.SmallImages;
  FSource.Highlighter := MainHighlighter;

  PageControl.ActivePage := TSBasics;
end;

procedure TDHost.FormHide(Sender: TObject);
begin
  Client.UnRegisterEventProc(FormClientEvent);

  if (Assigned(NewHost)) then
    FreeAndNil(NewHost);

  FSource.Lines.Clear();

  Preferences.Host.Width := Width;
  Preferences.Host.Height := Height;

  PageControl.ActivePage := TSBasics;
end;

procedure TDHost.FormShow(Sender: TObject);
var
  NewHostDatabase: TCHostDatabase;
begin
  Client.RegisterEventProc(FormClientEvent);

  NewHost := TCHost.Create(Client.Hosts);
  if (Assigned(Host)) then
    NewHost.Assign(Host);

  if (not Assigned(Host)) then
    Caption := Preferences.LoadStr(296)
  else
    Caption := Preferences.LoadStr(842, Host.Caption);

  if (not Assigned(Host)) then
    HelpContext := 1076
  else
    HelpContext := 1058;

  if (not Assigned(Host)) then
  begin
    NewHostDatabase := TCHostDatabase.Create(Host.Databases);
    NewHost.Databases.AddDatabase(NewHostDatabase);
    FreeAndNil(NewHostDatabase);

    FAll.Checked := True;
    FHosts.Text := '';
  end
  else
  begin
    FAll.Checked := Host.Name = '';
    FHost.Checked := Host.Name <> '';
    FHosts.Text := Host.Name;
  end;

  TSSource.TabVisible := Assigned(Host);

  PageControl.ActivePage := TSBasics;
  ActiveControl := FBCancel;
  if (FAll.Checked) then
    ActiveControl := FAll
  else
    ActiveControl := FHosts;
end;

procedure TDHost.TSDatabasesShow(Sender: TObject);
var
  Selected: TListItem;
begin
  FDatabasesRefresh(Sender);

  ActiveControl := FDatabases;

  Selected := FDatabases.Selected;
  FDatabases.Selected := Selected;
end;

procedure TDHost.TSSourceShow(Sender: TObject);
begin
  if (FSource.Lines.Text = '') then
    FSource.Text := Host.Source;
end;

initialization
  FHosts := nil;
end.

