unit fDDatabases;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  ODBCAPI,
  Forms_Ext,
  fBase, fClient, StdCtrls_Ext, Vcl.ExtCtrls;

type
  TDDatabases = class (TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FDatabases: TListView;
    GroupBox: TGroupBox_Ext;
    PSQLWait: TPanel;
    procedure FDatabasesDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Built();
    procedure FormClientEvent(const Event: TCClient.TEvent);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Client: TCClient;
    ODBCEnv: SQLHENV;
    SelectedDatabases: string;
    function Execute(): Boolean;
  end;

function DDatabases(): TDDatabases;

implementation {***************************************************************}

{$R *.dfm}

uses
  fPreferences, CSVUtils;

const
  STR_LEN = 128 + 1;

var
  FDatabases: TDDatabases;

function DDatabases(): TDDatabases;
begin
  if (not Assigned(FDatabases)) then
  begin
    Application.CreateForm(TDDatabases, FDatabases);
    FDatabases.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDatabases;
end;

{ TDDatabases *****************************************************************}

procedure TDDatabases.Built();
var
  DatabaseNames: TCSVStrings;
  I: Integer;
  Item: TListItem;
  J: Integer;
begin
  for I := 0 to Client.Databases.Count - 1 do
  begin
    if (not (Client.Databases[I] is TCSystemDatabase)) then
    begin
      Item := FDatabases.Items.Add();
      Item.Caption := Client.Databases[I].Name;
      Item.ImageIndex := iiDatabase;
    end;
  end;

  SetLength(DatabaseNames, 0);
  CSVSplitValues(SelectedDatabases, ',', '"', DatabaseNames);
  for I := 0 to Length(DatabaseNames) - 1 do
    for J := 0 to FDatabases.Items.Count - 1 do
      FDatabases.Items[J].Selected := FDatabases.Items[J].Selected
        or (Client.Databases.NameCmp(FDatabases.Items[J].Caption, DatabaseNames[I]) = 0);
  SetLength(DatabaseNames, 0);

  FDatabases.SortType := stText;

  FDatabases.ItemFocused := nil;
  for I := FDatabases.Items.Count - 1 downto 0 do
    if (FDatabases.Items[I].Selected) then
      FDatabases.ItemFocused := FDatabases.Items[I];
  if (not Assigned(FDatabases.ItemFocused) and (FDatabases.Items.Count > 0)) then
    FDatabases.ItemFocused := FDatabases.Items[0];

  GroupBox.Visible := True;
  PSQLWait.Visible := not GroupBox.Visible;
  FBOk.Enabled := GroupBox.Visible;

  ActiveControl := FDatabases;
end;

procedure TDDatabases.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiServer, Icon);

  Caption := Preferences.LoadStr(264);

  PSQLWait.Caption := Preferences.LoadStr(882);

  GroupBox.Caption := Preferences.LoadStr(265) + ':';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDDatabases.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDDatabases.FDatabasesDblClick(Sender: TObject);
begin
  FBOk.Click();
end;

procedure TDDatabases.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (Event.EventType in [ceBuild]) then
    Built();
end;

procedure TDDatabases.FormCreate(Sender: TObject);
begin
  FDatabases.SmallImages := Preferences.SmallImages;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  Preferences.Databases.Left := Left;
  Preferences.Databases.Top := Top;

  Preferences.ODBC.Left := Left;
  Preferences.ODBC.Top := Top;

  BorderStyle := bsSizeable;
end;

procedure TDDatabases.FormHide(Sender: TObject);
var
  I: Integer;
begin
  if (Assigned(Client)) then
    Client.UnRegisterEventProc(FormClientEvent);

  if (ModalResult = mrOk) then
  begin
    SelectedDatabases := '';
    for I := 0 to FDatabases.Items.Count - 1 do
      if (FDatabases.Items[I].Selected) then
      begin
        if (SelectedDatabases <> '') then SelectedDatabases := SelectedDatabases + ',';
        SelectedDatabases := SelectedDatabases + CSVEscape(FDatabases.Items[I].Caption);
      end;
  end;

  FDatabases.Items.BeginUpdate();
  FDatabases.Items.Clear();
  FDatabases.Items.EndUpdate();

  if (Assigned(Client)) then
  begin
    Preferences.Databases.Height := Height;
    Preferences.Databases.Left := Left;
    Preferences.Databases.Top := Top;
    Preferences.Databases.Width := Width;
  end
  else
  begin
    Preferences.ODBC.Height := Height;
    Preferences.ODBC.Left := Left;
    Preferences.ODBC.Top := Top;
    Preferences.ODBC.Width := Width;
  end;
end;

procedure TDDatabases.FormShow(Sender: TObject);
var
  cbServerName: SQLSMALLINT;
  I: Integer;
  Item: TListItem;
  List: TList;
  ServerName: array [0 .. STR_LEN - 1] of SQLTCHAR;
begin
  if (Assigned(Client)) then
  begin
    Client.RegisterEventProc(FormClientEvent);

    Left := Preferences.Databases.Left;
    Top := Preferences.Databases.Top;
    if ((Preferences.ODBC.Width >= Width) and (Preferences.ODBC.Height >= Height)) then
    begin
      Height := Preferences.Databases.Height;
      Width := Preferences.Databases.Width;
    end;

    List := TList.Create();
    List.Add(Client.Databases);
    GroupBox.Visible := not Client.Update(List);
    PSQLWait.Visible := not GroupBox.Visible;
    List.Free();

    if (GroupBox.Visible) then
      Built();

    FDatabases.MultiSelect := True;
  end
  else
  begin
    Left := Preferences.ODBC.Left;
    Top := Preferences.ODBC.Top;
    if ((Preferences.ODBC.Width >= Width) and (Preferences.ODBC.Height >= Height)) then
    begin
      Height := Preferences.ODBC.Height;
      Width := Preferences.ODBC.Width;
    end;

    if (SQL_SUCCEEDED(SQLDataSources(ODBCEnv, SQL_FETCH_FIRST, @ServerName, STR_LEN, @cbServerName, nil, 0, nil))) then
      repeat
        Item := FDatabases.Items.Add();
        Item.Caption := ServerName;
        Item.ImageIndex := iiDatabase;
      until (not SQL_SUCCEEDED(SQLDataSources(ODBCEnv, SQL_FETCH_NEXT, @ServerName, STR_LEN, @cbServerName, nil, 0, nil)));

    FDatabases.MultiSelect := False;

    FDatabases.SortType := stText;

    FDatabases.ItemFocused := nil;
    for I := FDatabases.Items.Count - 1 downto 0 do
      if (FDatabases.Items[I].Selected) then
        FDatabases.ItemFocused := FDatabases.Items[I];
    if (not Assigned(FDatabases.ItemFocused) and (FDatabases.Items.Count > 0)) then
      FDatabases.ItemFocused := FDatabases.Items[0];
  end;

  FBOk.Enabled := GroupBox.Visible;
  if (not GroupBox.Visible) then
    ActiveControl := FBCancel
  else
    ActiveControl := FDatabases;
end;

initialization
  FDatabases := nil;
end.
