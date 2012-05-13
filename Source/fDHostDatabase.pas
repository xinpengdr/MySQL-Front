unit fDHostDatabase;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  Forms_Ext, StdCtrls_Ext,
  fClient, fBase;

type
  TDHostDatabase = class (TForm_Ext)
    FAll: TRadioButton;
    FAlter: TCheckBox;
    FBCancel: TButton;
    FBOk: TButton;
    FCreate: TCheckBox;
    FCreateView: TCheckBox;
    FDatabase: TRadioButton;
    FDatabases: TComboBox_Ext;
    FDelete: TCheckBox;
    FDrop: TCheckBox;
    FGrant: TCheckBox;
    FIndex: TCheckBox;
    FInsert: TCheckBox;
    FLockTable: TCheckBox;
    FReferences: TCheckBox;
    FSelect: TCheckBox;
    FShowView: TCheckBox;
    FTmpTable: TCheckBox;
    FUpdate: TCheckBox;
    GRights: TGroupBox_Ext;
    GWhat: TGroupBox_Ext;
    procedure FAllClick(Sender: TObject);
    procedure FDatabaseClick(Sender: TObject);
    procedure FDatabasesChange(Sender: TObject);
    procedure FDatabasesDropDown(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FullWidth, DiffButtonsLeft: Integer;
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Host: TCHost;
    HostDatabase: TCHostDatabase;
    function Execute(): Boolean;
  end;

function DHostDatabase(): TDHostDatabase;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  fPreferences;

var
  FHost: TDHostDatabase;

function DHostDatabase(): TDHostDatabase;
begin
  if (not Assigned(FHost)) then
  begin
    Application.CreateForm(TDHostDatabase, FHost);
    FHost.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FHost;
end;

{ TDHostDatabase **************************************************************}

procedure TDHostDatabase.CMChangePreferences(var Message: TMessage);
begin
  GWhat.Caption := Preferences.LoadStr(299);
  FAll.Caption := Preferences.LoadStr(300);
  FDatabase.Caption := Preferences.LoadStr(301) + ':';

  GRights.Caption := Preferences.LoadStr(284);
  FSelect.Caption := Preferences.LoadStr(307);
  FInsert.Caption := ReplaceStr(Preferences.LoadStr(308), '&', '');
  FUpdate.Caption := ReplaceStr(Preferences.LoadStr(309), '&', '');
  FDelete.Caption := ReplaceStr(Preferences.LoadStr(310), '&', '');

  FCreate.Caption := Preferences.LoadStr(311);
  FDrop.Caption := Preferences.LoadStr(312);
  FAlter.Caption := Preferences.LoadStr(313);
  FIndex.Caption := Preferences.LoadStr(314);
  FReferences.Caption := Preferences.LoadStr(315);

  FLockTable.Caption := Preferences.LoadStr(316);
  FTmpTable.Caption := Preferences.LoadStr(318);

  FCreateView.Caption := Preferences.LoadStr(763);
  FShowView.Caption := Preferences.LoadStr(764);

  FGrant.Caption := Preferences.LoadStr(332);

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDHostDatabase.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDHostDatabase.FAllClick(Sender: TObject);
begin
  FDatabases.Clear(); FDatabasesChange(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDHostDatabase.FBOkCheckEnabled(Sender: TObject);
begin
  FBOk.Enabled := FAll.Checked or (FDatabase.Checked and (FDatabases.Text <> ''));

  if (FAll.Checked) then
    FBOk.Enabled := FBOk.Enabled and not Assigned(Host.DatabaseByName('')) or (Host.DatabaseByName('') = HostDatabase)
  else
    FBOk.Enabled := FBOk.Enabled and not Assigned(Host.DatabaseByName(FDatabases.Text)) or (Host.DatabaseByName(FDatabases.Text) = HostDatabase);
end;

procedure TDHostDatabase.FDatabaseClick(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDHostDatabase.FDatabasesChange(Sender: TObject);
begin
  if (FDatabases.Text <> '') then FDatabase.Checked := True;
  FBOkCheckEnabled(Sender);
end;

procedure TDHostDatabase.FDatabasesDropDown(Sender: TObject);
var
  I: Integer;
begin
  if (FDatabases.Items.Count = 0) then
    for I := 0 to Host.Hosts.Client.Databases.Count - 1 do
      if (not (Host.Hosts.Client.Databases[I] is TCSystemDatabase)) then
        FDatabases.Items.Add(Host.Hosts.Client.Databases[I].Name);
end;

procedure TDHostDatabase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewHostDatabase: TCHostDatabase;
begin
  if (ModalResult = mrOk) then
  begin
    NewHostDatabase := TCHostDatabase.Create();
    if (Assigned(HostDatabase)) then
      NewHostDatabase.Assign(HostDatabase);

    NewHostDatabase.Name := Trim(FDatabases.Text);

    NewHostDatabase.RSelect := FSelect.Checked;
    NewHostDatabase.RInsert := FInsert.Checked;
    NewHostDatabase.RUpdate := FUpdate.Checked;
    NewHostDatabase.RDelete := FDelete.Checked;
    NewHostDatabase.RReferences := FReferences.Checked;

    NewHostDatabase.RCreate := FCreate.Checked;
    NewHostDatabase.RDrop := FDrop.Checked;
    NewHostDatabase.RAlter := FAlter.Checked;
    NewHostDatabase.RIndex := FIndex.Checked;

    NewHostDatabase.RLockTables := FLockTable.Checked;
    NewHostDatabase.RCreateTempTable := FTmpTable.Checked;

    NewHostDatabase.RCreateView := FCreateView.Checked;
    NewHostDatabase.RShowView := FShowView.Checked;

    NewHostDatabase.RGrant := FGrant.Checked;

    if (not Assigned(HostDatabase)) then
      CanClose := Host.Databases.AddDatabase(NewHostDatabase)
    else
      CanClose := Host.Databases.UpdateDatabase(HostDatabase, NewHostDatabase);

    NewHostDatabase.Free();
  end;
end;

procedure TDHostDatabase.FormCreate(Sender: TObject);
begin
  FullWidth := Width; DiffButtonsLeft := FBCancel.Left - FBOk.Left;
end;

procedure TDHostDatabase.FormShow(Sender: TObject);
begin
  if (not Assigned(HostDatabase)) then
    Caption := Preferences.LoadStr(147)
  else
    Caption := Preferences.LoadStr(842, HostDatabase.Name);

  if (not Assigned(HostDatabase)) then
  begin
    FAll.Checked := True;
    FDatabases.Text := '';

    FSelect.Checked := False;
    FInsert.Checked := False;
    FUpdate.Checked := False;
    FDelete.Checked := False;
    FReferences.Checked := False;

    FCreate.Checked := False;
    FDrop.Checked := False;
    FAlter.Checked := False;
    FIndex.Checked := False;

    FLockTable.Checked := False;
    FTmpTable.Checked := False;

    FCreateView.Checked := False;
    FShowView.Checked := False;

    FGrant.Checked := False;
  end
  else
  begin
    FAll.Checked := HostDatabase.Name = '';
    FDatabase.Checked := not FAll.Checked;
    if (FDatabase.Checked) then
    begin
      FDatabases.Text := HostDatabase.Name;
      FDatabasesChange(Sender);
    end;

    FSelect.Checked := HostDatabase.RSelect;
    FInsert.Checked := HostDatabase.RInsert;
    FUpdate.Checked := HostDatabase.RUpdate;
    FDelete.Checked := HostDatabase.RDelete;
    FReferences.Checked := HostDatabase.RReferences;

    FCreate.Checked := HostDatabase.RCreate;
    FDrop.Checked := HostDatabase.RDrop;
    FAlter.Checked := HostDatabase.RAlter;
    FIndex.Checked := HostDatabase.RIndex;

    FLockTable.Checked := HostDatabase.RLockTables;
    FTmpTable.Checked := HostDatabase.RCreateTempTable;

    FCreateView.Checked := HostDatabase.RCreateView;
    FShowView.Checked := HostDatabase.RShowView;

    FGrant.Checked := HostDatabase.RGrant;
  end;

  ActiveControl := FBCancel;
  if (FAll.Checked) then ActiveControl := FAll;
  if (FDatabase.Checked) then ActiveControl := FDatabase;
end;

initialization
  FHost := nil;
end.
