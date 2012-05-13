unit fDUserRight;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  StdCtrls_Ext, Forms_Ext,
  fClient, fBase;

type
  TDUserRight = class (TForm_Ext)
    FAll: TRadioButton;
    FAlter: TCheckBox;
    FAlterRoutine: TCheckBox;
    FBCancel: TButton;
    FBOk: TButton;
    FCreate: TCheckBox;
    FCreateRoutine: TCheckBox;
    FCreateTempTable: TCheckBox;
    FCreateUser: TCheckBox;
    FCreateView: TCheckBox;
    FDatabase: TRadioButton;
    FDatabases: TComboBox_Ext;
    FDelete: TCheckBox;
    FDrop: TCheckBox;
    FEvent: TCheckBox;
    FExecute: TCheckBox;
    FField: TRadioButton;
    FFields: TComboBox_Ext;
    FFile: TCheckBox;
    FFunction: TRadioButton;
    FFunctions: TComboBox;
    FGrant: TCheckBox;
    FIndex: TCheckBox;
    FInsert: TCheckBox;
    FLockTable: TCheckBox;
    FProcedure: TRadioButton;
    FProcedures: TComboBox;
    FProcess: TCheckBox;
    FReferences: TCheckBox;
    FReload: TCheckBox;
    FReplClient: TCheckBox;
    FReplSlave: TCheckBox;
    FSelect: TCheckBox;
    FShowDatabase: TCheckBox;
    FShowView: TCheckBox;
    FShutdown: TCheckBox;
    FSuper: TCheckBox;
    FTable: TRadioButton;
    FTables: TComboBox_Ext;
    FTrigger: TCheckBox;
    FUpdate: TCheckBox;
    GRights: TGroupBox_Ext;
    GWhat: TGroupBox_Ext;
    procedure FAllClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FDatabaseClick(Sender: TObject);
    procedure FDatabasesChange(Sender: TObject);
    procedure FDatabasesDropDown(Sender: TObject);
    procedure FFieldClick(Sender: TObject);
    procedure FFieldsChange(Sender: TObject);
    procedure FFieldsDropDown(Sender: TObject);
    procedure FFunctionsChange(Sender: TObject);
    procedure FFunctionsDropDown(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FProceduresChange(Sender: TObject);
    procedure FProceduresDropDown(Sender: TObject);
    procedure FRoutineClick(Sender: TObject);
    procedure FTableClick(Sender: TObject);
    procedure FTablesChange(Sender: TObject);
    procedure FTablesDropDown(Sender: TObject);
  private
    procedure EnableElements(Sender: TObject);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Client: TCClient;
    User: TCUser;
    UserRight: TCUserRight;
    function Execute(): Boolean;
  end;

function DUserRight(): TDUserRight;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  fPreferences;

var
  FUserRight: TDUserRight;

function DUserRight(): TDUserRight;
begin
  if (not Assigned(FUserRight)) then
  begin
    Application.CreateForm(TDUserRight, FUserRight);
    FUserRight.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FUserRight;
end;

{ TDUserRight *****************************************************************}

procedure TDUserRight.CMChangePreferences(var Message: TMessage);
begin
  GWhat.Caption := Preferences.LoadStr(299);
  FAll.Caption := Preferences.LoadStr(300);
  FDatabase.Caption := Preferences.LoadStr(301) + ':';
  FTable.Caption := Preferences.LoadStr(302) + ':';
  FField.Caption := Preferences.LoadStr(164) + ':';
  FProcedure.Caption := Preferences.LoadStr(768) + ':';
  FFunction.Caption := Preferences.LoadStr(769) + ':';

  GRights.Caption := Preferences.LoadStr(284);

  FAlter.Caption := Preferences.LoadStr(313);
  FAlterRoutine.Caption := Preferences.LoadStr(766);
  FCreate.Caption := Preferences.LoadStr(311);
  FCreateRoutine.Caption := Preferences.LoadStr(765);
  FCreateTempTable.Caption := Preferences.LoadStr(318);
  FCreateUser.Caption := Preferences.LoadStr(767);
  FCreateView.Caption := Preferences.LoadStr(763);
  FDelete.Caption := ReplaceStr(Preferences.LoadStr(310), '&', '');
  FDrop.Caption := Preferences.LoadStr(312);
  FEvent.Caption := Preferences.LoadStr(809);
  FExecute.Caption := Preferences.LoadStr(317);
  FFile.Caption := Preferences.LoadStr(324);
  FGrant.Caption := Preferences.LoadStr(332);
  FIndex.Caption := Preferences.LoadStr(314);
  FInsert.Caption := ReplaceStr(Preferences.LoadStr(308), '&', '');
  FLockTable.Caption := Preferences.LoadStr(316);
  FProcess.Caption := Preferences.LoadStr(320);
  FReferences.Caption := Preferences.LoadStr(315);
  FReload.Caption := Preferences.LoadStr(321);
  FReplClient.Caption := Preferences.LoadStr(325);
  FReplSlave.Caption := Preferences.LoadStr(326);
  FSelect.Caption := Preferences.LoadStr(307);
  FShowDatabase.Caption := Preferences.LoadStr(319);
  FShowView.Caption := Preferences.LoadStr(764);
  FShutdown.Caption := Preferences.LoadStr(323);
  FSuper.Caption := Preferences.LoadStr(322);
  FTrigger.Caption := ReplaceStr(Preferences.LoadStr(810), '&', '');
  FUpdate.Caption := ReplaceStr(Preferences.LoadStr(309), '&', '');

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

procedure TDUserRight.EnableElements(Sender: TObject);
begin
  FAlter.Enabled           := FAlter.Visible           and (FAll.Checked or FDatabase.Checked);
  FAlterRoutine.Enabled    := FAlterRoutine.Visible    and (FAll.Checked or FDatabase.Checked or FProcedure.Checked or FFunction.Checked);
  FCreate.Enabled          := FCreate.Visible          and (FAll.Checked or FDatabase.Checked);
  FCreateRoutine.Enabled   := FCreateRoutine.Visible   and (FAll.Checked or FDatabase.Checked);
  FCreateTempTable.Enabled := FCreateTempTable.Visible and (FAll.Checked or FDatabase.Checked);
  FCreateUser.Enabled      := FCreateUser.Visible      and (FAll.Checked);
  FCreateView.Enabled      := FCreateView.Visible      and (FAll.Checked or FDatabase.Checked);
  FDelete.Enabled          := FDelete.Visible          and (FAll.Checked or FDatabase.Checked or FTable.Checked);
  FDrop.Enabled            := FDrop.Visible            and (FAll.Checked or FDatabase.Checked);
  FEvent.Enabled           := FEvent.Visible           and (FAll.Checked or FDatabase.Checked);
  FExecute.Enabled         := FExecute.Visible         and (FAll.Checked or FDatabase.Checked or FProcedure.Checked or FFunction.Checked);
  FFile.Enabled            := FFile.Visible            and  FAll.Checked;
  FIndex.Enabled           := FIndex.Visible           and  FAll.Checked or FDatabase.Checked;
  FInsert.Enabled          := FInsert.Visible          and  not FProcedure.Checked and not FFunction.Checked;
  FGrant.Enabled           := FGrant.Visible           and  FAll.Checked or FDatabase.Checked or FProcedure.Checked or FFunction.Checked;
  FLockTable.Enabled       := FLockTable.Visible       and (FAll.Checked or FDatabase.Checked);
  FProcess.Enabled         := FProcess.Visible         and  FAll.Checked;
  FReferences.Enabled      := FReferences.Visible      and  not FProcedure.Checked and not FFunction.Checked;
  FReload.Enabled          := FReload.Visible          and  FAll.Checked;
  FReplClient.Enabled      := FReplClient.Visible      and  FAll.Checked;
  FReplSlave.Enabled       := FReplSlave.Visible       and  FAll.Checked;
  FSelect.Enabled          := FSelect.Visible          and  not FProcedure.Checked and not FFunction.Checked;
  FShowDatabase.Enabled    := FShowDatabase.Visible    and  FAll.Checked;
  FShowView.Enabled        := FShowView.Visible        and (FAll.Checked or FDatabase.Checked);
  FShutdown.Enabled        := FShutdown.Visible        and  FAll.Checked;
  FSuper.Enabled           := FSuper.Visible           and  FAll.Checked;
  FTrigger.Enabled         := FTrigger.Visible         and (FAll.Checked or FDatabase.Checked);
  FUpdate.Enabled          := FUpdate.Visible          and  not FProcedure.Checked and not FFunction.Checked;
end;

function TDUserRight.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult <> mrCancel;
end;

procedure TDUserRight.FAllClick(Sender: TObject);
begin
  FDatabases.Clear(); FDatabasesChange(Sender);
  EnableElements(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FBOkCheckEnabled(Sender: TObject);
var
  I: Integer;
begin
  FBOk.Enabled := (FAll.Checked
    or (FDatabase.Checked and (Trim(FDatabases.Text) <> ''))
    or (FTable.Checked and (Trim(FTables.Text) <> ''))
    or (FField.Checked and (Trim(FFields.Text) <> ''))
    or (FProcedure.Checked and (Trim(FProcedures.Text) <> ''))
    or (FFunction.Checked and (Trim(FFunctions.Text) <> '')))
    and True;

  for I := 0 to User.RightCount - 1 do
    if (not Assigned(UserRight)
      or (User.Right[I].DatabaseName <> UserRight.DatabaseName)
      or (User.Right[I].TableName <> UserRight.TableName)
      or (User.Right[I].FieldName <> UserRight.FieldName)
      or (User.Right[I].ProcedureName <> UserRight.ProcedureName)
      or (User.Right[I].FunctionName <> UserRight.FunctionName)) then
    begin
      if (FAll.Checked and (User.Right[I].DatabaseName = '')) then
        FBOk.Enabled := Assigned(UserRight);
      if (FDatabase.Checked and (lstrcmpi(PChar(User.Right[I].DatabaseName), PChar(FDatabases.Text)) = 0)) then
        FBOk.Enabled := Assigned(UserRight);
      if (FTable.Checked and (lstrcmpi(PChar(User.Right[I].DatabaseName), PChar(FDatabases.Text)) = 0) and (lstrcmpi(PChar(User.Right[I].TableName), PChar(FTables.Text)) = 0)) then
        FBOk.Enabled := Assigned(UserRight);
      if (FField.Checked and (lstrcmpi(PChar(User.Right[I].DatabaseName), PChar(FDatabases.Text)) = 0) and (lstrcmpi(PChar(User.Right[I].TableName), PChar(FTables.Text)) = 0) and (lstrcmpi(PChar(User.Right[I].FieldName), PChar(FFields.Text)) = 0)) then
        FBOk.Enabled := Assigned(UserRight);
      if (FProcedure.Checked and (lstrcmpi(PChar(User.Right[I].DatabaseName), PChar(FDatabases.Text)) = 0) and (lstrcmpi(PChar(User.Right[I].ProcedureName), PChar(FProcedures.Text)) = 0)) then
        FBOk.Enabled := Assigned(UserRight);
      if (FFunction.Checked and (lstrcmpi(PChar(User.Right[I].DatabaseName), PChar(FDatabases.Text)) = 0) and (lstrcmpi(PChar(User.Right[I].FunctionName), PChar(FFunctions.Text)) = 0)) then
        FBOk.Enabled := Assigned(UserRight);
    end;

  if (not FAll.Checked
    and not FAlter.Checked
    and not FAlterRoutine.Checked
    and not FCreate.Checked
    and not FCreateRoutine.Checked
    and not FCreateTempTable.Checked
    and not FCreateUser.Checked
    and not FCreateView.Checked
    and not FDelete.Checked
    and not FDrop.Checked
    and not FEvent.Checked
    and not FExecute.Checked
    and not FFile.Checked
    and not FIndex.Checked
    and not FInsert.Checked
    and not FGrant.Checked
    and not FLockTable.Checked
    and not FProcess.Checked
    and not FReferences.Checked
    and not FReload.Checked
    and not FReplClient.Checked
    and not FReplSlave.Checked
    and not FSelect.Checked
    and not FShowDatabase.Checked
    and not FShowView.Checked
    and not FShutdown.Checked
    and not FSuper.Checked
    and not FTrigger.Checked
    and not FUpdate.Checked) then
    FBOk.Enabled := False;
end;

procedure TDUserRight.FDatabaseClick(Sender: TObject);
begin
  FTables.Clear(); FTablesChange(Sender);
  EnableElements(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FDatabasesChange(Sender: TObject);
begin
  FTables.Clear(); FTablesChange(Sender);
  FTables.Enabled := FDatabases.Text <> ''; FTable.Enabled := FTables.Enabled;
  FFields.Enabled := False; FField.Enabled := FFields.Enabled;

  FProcedures.Clear(); FProceduresChange(Sender);
  FProcedures.Enabled := FDatabases.Text <> ''; FProcedure.Enabled := FProcedures.Enabled;

  FFunctions.Clear(); FFunctionsChange(Sender);
  FFunctions.Enabled := FDatabases.Text <> ''; FFunction.Enabled := FFunctions.Enabled;


  if (FDatabases.Text <> '') then FDatabase.Checked := True;

  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FDatabasesDropDown(Sender: TObject);
var
  I: Integer;
begin
  if (FDatabases.Items.Count = 0) then
  begin
    FDatabases.Items.BeginUpdate();
    for I := 0 to Client.Databases.Count - 1 do
      if (not (Client.Databases[I] is TCSystemDatabase)) then
        FDatabases.Items.Add(Client.Databases[I].Name);
    FDatabases.Items.EndUpdate();
  end;
end;

procedure TDUserRight.FFieldClick(Sender: TObject);
begin
  EnableElements(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FFieldsChange(Sender: TObject);
begin
  if (FFields.Text <> '') then FField.Checked := True;
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FFieldsDropDown(Sender: TObject);
var
  Database: TCDatabase;
  I: Integer;
  Table: TCBaseTable;
begin
  if (FFields.Items.Count = 0) then
  begin
    Database := Client.DatabaseByName(FDatabases.Text);
    if (Assigned(Database)) then
    begin
      Table := Database.BaseTableByName(FTables.Text);
      if (Assigned(Table)) then
      begin
        FFields.Items.BeginUpdate();
        for I := 0 to Table.Fields.Count - 1 do
          FFields.Items.Add(Table.Fields[I].Name);
        FFields.Items.EndUpdate();
      end;
    end;
  end;
end;

procedure TDUserRight.FFunctionsChange(Sender: TObject);
begin
  if (FFunctions.Text <> '') then FFunction.Checked := True;
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FFunctionsDropDown(Sender: TObject);
var
  Database: TCDatabase;
  I: Integer;
begin
  if (FFunctions.Items.Count = 0) then
  begin
    Database := Client.DatabaseByName(FDatabases.Text);
    if (Assigned(Database)) then
    begin
      FFunctions.Items.BeginUpdate();
      for I := 0 to Client.DatabaseByName(FDatabases.Text).Routines.Count - 1 do
        if (Client.DatabaseByName(FDatabases.Text).Routines[I].RoutineType = rtFunction) then
          FFunctions.Items.Add(Client.DatabaseByName(FDatabases.Text).Routines[I].Name);
      FFunctions.Items.EndUpdate();
    end;
  end;
end;

procedure TDUserRight.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  NewUserRight: TCUserRight;
begin
  if (ModalResult = mrOk) then
  begin
    NewUserRight := TCUserRight.Create();
    if (Assigned(UserRight)) then
      NewUserRight.Assign(UserRight);

    NewUserRight.DatabaseName := FDatabases.Text;
    NewUserRight.TableName := FTables.Text;
    NewUserRight.ProcedureName := FProcedures.Text;
    NewUserRight.FunctionName := FFunctions.Text;
    NewUserRight.FieldName := FFields.Text;


    NewUserRight.RAlter := FAlter.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RAlterRoutine := FAlterRoutine.Checked and (FAll.Checked or FDatabase.Checked or FProcedure.Checked or FFunction.Checked);
    NewUserRight.RCreate := FCreate.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RCreateTempTable := FCreateTempTable.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RCreateRoutine := FCreateRoutine.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RCreateUser := FCreateUser.Checked and FAll.Checked;
    NewUserRight.RCreateView := FCreateView.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RDelete := FDelete.Checked and (FAll.Checked or FDatabase.Checked or FTable.Checked);
    NewUserRight.RDrop := FDrop.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.REvent := FEvent.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RExecute := FExecute.Checked and (FAll.Checked or FDatabase.Checked or FProcedure.Checked or FFunction.Checked);
    NewUserRight.RFile := FFile.Checked and FAll.Checked;
    NewUserRight.RGrant := FGrant.Checked and (FAll.Checked or FDatabase.Checked or FProcedure.Checked or FFunction.Checked);
    NewUserRight.RIndex := FIndex.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RInsert := FInsert.Checked;
    NewUserRight.RLockTables := FLockTable.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RProcess := FProcess.Checked and FAll.Checked;
    NewUserRight.RReferences := FReferences.Checked;
    NewUserRight.RReplClient := FReplClient.Checked and FAll.Checked;
    NewUserRight.RReplSlave := FReplSlave.Checked and FAll.Checked;
    NewUserRight.RReload := FReload.Checked and FAll.Checked;
    NewUserRight.RSelect := FSelect.Checked;
    NewUserRight.RShowDatabases := FShowDatabase.Checked and FAll.Checked;
    NewUserRight.RShowView := FShowView.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RShutdown := FShutdown.Checked and FAll.Checked;
    NewUserRight.RSuper := FSuper.Checked and FAll.Checked;
    NewUserRight.RTrigger := FTrigger.Checked and (FAll.Checked or FDatabase.Checked);
    NewUserRight.RUpdate := FUpdate.Checked;

    if (not Assigned(UserRight)) then
      User.AddRight(NewUserRight)
    else
      User.UpdateRight(UserRight, NewUserRight);

    CanClose := True;

    NewUserRight.Free();
  end;
end;

procedure TDUserRight.FormHide(Sender: TObject);
begin
  FDatabases.Items.Clear();
  FTables.Items.Clear();
  FFields.Items.Clear();
  FProcedures.Items.Clear();
  FFunctions.Items.Clear();
end;

procedure TDUserRight.FormShow(Sender: TObject);
begin
  if (not Assigned(UserRight)) then
    Caption := Preferences.LoadStr(298)
  else
    Caption := Preferences.LoadStr(842, UserRight.Caption);

  FDatabases.Clear();
  FTables.Clear();
  FFields.Clear();


  FProcedure.Visible := Client.ServerVersion >= 50006; FProcedures.Visible := FProcedure.Visible;
  FFunction.Visible := Client.ServerVersion >= 50006; FFunctions.Visible := FFunction.Visible;

  FAlterRoutine.Visible := Client.ServerVersion >= 50003;
  FCreateRoutine.Visible := Client.ServerVersion >= 50003;
  FCreateTempTable.Visible := Client.ServerVersion >= 40002;
  FCreateUser.Visible := Client.ServerVersion >= 50003;
  FCreateView.Visible := Client.ServerVersion >= 50001;
  FEvent.Visible := Client.ServerVersion >= 50106;
  FExecute.Visible := Client.ServerVersion >= 50003;
  FLockTable.Visible := Client.ServerVersion >= 40002;
  FReplClient.Visible := Client.ServerVersion >= 40002;
  FReplSlave.Visible := Client.ServerVersion >= 40002;
  FShowDatabase.Visible := Client.ServerVersion >= 40002;
  FShowView.Visible := Client.ServerVersion >= 50001;
  FSuper.Visible := Client.ServerVersion >= 40002;
  FTrigger.Visible := Client.ServerVersion >= 50106;


  FAll.Checked := not Assigned(UserRight) or (UserRight.DatabaseName = '');
  FDatabase.Checked := Assigned(UserRight) and (UserRight.DatabaseName <> '');
  if (FDatabase.Checked) then
  begin
    FDatabasesDropDown(Sender);
    FDatabases.Text := UserRight.DatabaseName;
    FDatabasesChange(Sender);
  end;
  FTable.Checked := Assigned(UserRight) and (UserRight.TableName <> '');
  if (FTable.Checked) then
  begin
    FTablesDropDown(Sender);
    FTables.Text := UserRight.TableName;
    FTablesChange(Sender);
  end;
  FField.Checked := Assigned(UserRight) and (UserRight.FieldName <> '');
  if (FField.Checked) then
  begin
    FFieldsDropDown(Sender);
    FFields.Text := UserRight.FieldName;
    FFieldsChange(Sender);
  end;
  FProcedure.Checked := Assigned(UserRight) and (UserRight.ProcedureName <> '');
  if (FProcedure.Checked) then
  begin
    FProceduresDropDown(Sender);
    FProcedures.Text := UserRight.ProcedureName;
    FProceduresChange(Sender);
  end;
  FFunction.Checked := Assigned(UserRight) and (UserRight.FunctionName <> '');
  if (FFunction.Checked) then
  begin
    FFunctionsDropDown(Sender);
    FFunctions.Text := UserRight.FunctionName;
    FFunctionsChange(Sender);
  end;

  FAlter.Checked := Assigned(UserRight) and UserRight.RAlter and FAlter.Enabled;
  FAlterRoutine.Checked := Assigned(UserRight) and UserRight.RAlterRoutine and FAlterRoutine.Enabled;
  FCreate.Checked := Assigned(UserRight) and UserRight.RCreate and FCreate.Enabled;
  FCreateRoutine.Checked := Assigned(UserRight) and UserRight.RCreateRoutine and FCreateRoutine.Enabled;
  FCreateTempTable.Checked := Assigned(UserRight) and UserRight.RCreateTempTable and FCreateTempTable.Enabled;
  FCreateUser.Checked := Assigned(UserRight) and UserRight.RCreateUser and FCreateUser.Enabled;
  FCreateView.Checked := Assigned(UserRight) and UserRight.RCreateView and FCreateView.Enabled;
  FDelete.Checked := Assigned(UserRight) and UserRight.RDelete and FDelete.Enabled;
  FDrop.Checked := Assigned(UserRight) and UserRight.RDrop and FDrop.Enabled;
  FEvent.Checked := Assigned(UserRight) and UserRight.REvent and FEvent.Enabled;
  FExecute.Checked := Assigned(UserRight) and UserRight.RExecute and FExecute.Enabled;
  FFile.Checked := Assigned(UserRight) and UserRight.RFile and FFile.Enabled;
  FGrant.Checked := Assigned(UserRight) and UserRight.RGrant and FGrant.Enabled;
  FIndex.Checked := Assigned(UserRight) and UserRight.RIndex and FIndex.Enabled;
  FInsert.Checked := Assigned(UserRight) and UserRight.RInsert and FInsert.Enabled;
  FLockTable.Checked := Assigned(UserRight) and UserRight.RLockTables and FLockTable.Enabled;
  FProcess.Checked := Assigned(UserRight) and UserRight.RProcess and FProcess.Enabled;
  FReferences.Checked := Assigned(UserRight) and UserRight.RReferences and FReferences.Enabled;
  FReload.Checked := Assigned(UserRight) and UserRight.RReload and FReload.Enabled;
  FReplClient.Checked := Assigned(UserRight) and UserRight.RReplClient and FReplClient.Enabled;
  FReplSlave.Checked := Assigned(UserRight) and UserRight.RReplSlave and FReplSlave.Enabled;
  FSelect.Checked := Assigned(UserRight) and UserRight.RSelect and FSelect.Enabled;
  FShowDatabase.Checked := Assigned(UserRight) and UserRight.RShowDatabases and FShowDatabase.Enabled;
  FShowView.Checked := Assigned(UserRight) and UserRight.RShowView and FShowView.Enabled;
  FShutdown.Checked := Assigned(UserRight) and UserRight.RShutdown and FShutdown.Enabled;
  FSuper.Checked := Assigned(UserRight) and UserRight.RSuper and FSuper.Enabled;
  FTrigger.Checked := Assigned(UserRight) and UserRight.RTrigger and FTrigger.Enabled;
  FUpdate.Checked := Assigned(UserRight) and UserRight.RUpdate and FUpdate.Enabled;

  ActiveControl := FBCancel;
  if (FAll.Checked) then ActiveControl := FAll
  else if (FDatabase.Checked) then ActiveControl := FDatabase
  else if (FTable.Checked) then ActiveControl := FTable
  else if (FField.Checked) then ActiveControl := FField
  else if (FProcedure.Checked) then ActiveControl := FProcedure
  else if (FFunction.Checked) then ActiveControl := FFunction;

  FBOk.Enabled := False;
end;

procedure TDUserRight.FProceduresChange(Sender: TObject);
begin
  if (FProcedures.Text <> '') then FProcedure.Checked := True;
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FProceduresDropDown(Sender: TObject);
var
  Database: TCDatabase;
  I: Integer;
begin
  if (FProcedures.Items.Count = 0) then
  begin
    Database := Client.DatabaseByName(FDatabases.Text);
    if (Assigned(Database)) then
    begin
      FProcedures.Items.BeginUpdate();
      for I := 0 to Client.DatabaseByName(FDatabases.Text).Routines.Count - 1 do
        if (Client.DatabaseByName(FDatabases.Text).Routines[I].RoutineType = rtProcedure) then
          FProcedures.Items.Add(Client.DatabaseByName(FDatabases.Text).Routines[I].Name);
      FProcedures.Items.EndUpdate();
    end;
  end;
end;

procedure TDUserRight.FRoutineClick(Sender: TObject);
begin
  EnableElements(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FTableClick(Sender: TObject);
begin
  FFields.Clear(); FFieldsChange(Sender);
  EnableElements(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FTablesChange(Sender: TObject);
begin
  FFields.Clear();
  FFields.Enabled := FTables.Text <> ''; FField.Enabled := FFields.Enabled;

  if (FTables.Text <> '') then FTable.Checked := True;
  FBOkCheckEnabled(Sender);
end;

procedure TDUserRight.FTablesDropDown(Sender: TObject);
var
  Database: TCDatabase;
  I: Integer;
begin
  if (FTables.Items.Count = 0) then
  begin
    Database := Client.DatabaseByName(FDatabases.Text);
    if (Assigned(Database)) then
    begin
      FTables.Items.BeginUpdate();
      for I := 0 to Database.Tables.Count - 1 do
        FTables.Items.Add(Database.Tables[I].Name);
      FTables.Items.EndUpdate();
    end;
  end;
end;

initialization
  FUserRight := nil;
end.
