unit fDTransfer;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB,
  ComCtrls_Ext, Forms_Ext, ExtCtrls_Ext,
  fClient, fBase, MySQLDB, fAccount, fTools, Menus,
  StdCtrls_Ext;

type
  TDTransfer = class(TForm_Ext)
    FBBack: TButton;
    FBCancel: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FLDone: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLTransferGeneral: TLabel;
    FLTransferWhat: TLabel;
    FMaster: TTreeView_Ext;
    FProgressBar: TProgressBar;
    FSlave: TTreeView_Ext;
    FTransferData: TCheckBox;
    FTransferDisableKeys: TCheckBox;
    FTransferStructure: TCheckBox;
    GErrorMessages: TGroupBox_Ext;
    GMaster: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GSlave: TGroupBox_Ext;
    GTransferOptions: TGroupBox_Ext;
    GTransferWhat: TGroupBox_Ext;
    miSelectAll: TMenuItem;
    MMaster: TPopupMenu;
    PageControl: TPageControl;
    PErrorMessages: TPanel_Ext;
    PMaster: TPanel_Ext;
    PSlave: TPanel_Ext;
    TSExecute: TTabSheet;
    TSSelect: TTabSheet;
    TSTransferOptions: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FTransferDataClick(Sender: TObject);
    procedure FTransferDataKeyPress(Sender: TObject; var Key: Char);
    procedure FTransferStructureClick(Sender: TObject);
    procedure FTransferStructureKeyPress(Sender: TObject; var Key: Char);
    procedure miSelectAllClick(Sender: TObject);
    procedure MMasterPopup(Sender: TObject);
    procedure PageControlResize(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
    procedure TSTransferOptionsShow(Sender: TObject);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    Clients: array of TCClient;
    MouseDownNode: TTreeNode;
    Transfer: TTTransfer;
    WantedExecute: Boolean;
    WantedNodeExpand: TTreeNode;
    procedure FormClientEvent(const Event: TCClient.TEvent);
    function GetClient(const Index: Integer): TCClient;
    procedure InitTSSelect(Sender: TObject);
    procedure OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutedDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    MasterClient: TCClient;
    MasterDatabaseName: string;
    MasterTableName: string;
    SlaveClient: TCClient;
    SlaveDatabaseName: string;
    SlaveTableName: string;
    function Execute(): Boolean;
  end;

function DTransfer(): TDTransfer;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils, CommCtrl, RichEdit,
  SQLUtils,
  fPreferences,
  FDConnecting;

var
  FTransfer: TDTransfer;

function DTransfer(): TDTransfer;
begin
  if (not Assigned(FTransfer)) then
  begin
    Application.CreateForm(TDTransfer, FTransfer);
    FTransfer.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FTransfer;
end;

function TreeViewSelCount(const TreeView: TTreeView_Ext): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to TreeView.Items.Count - 1 do
    if (TreeView.Items[I].Selected) then
      Inc(Result);
end;

{ TDTransfer ******************************************************************}

procedure TDTransfer.CMChangePreferences(var Message: TMessage);
begin
  miSelectAll.Caption := Preferences.LoadStr(572);

  Caption := ReplaceStr(Preferences.LoadStr(753), '&', '');;

  GMaster.Caption := ReplaceStr(Preferences.LoadStr(754), '&', '');
  GSlave.Caption := ReplaceStr(Preferences.LoadStr(755), '&', '');

  GTransferWhat.Caption := Preferences.LoadStr(227);
  FLTransferWhat.Caption := Preferences.LoadStr(227) + ':';
  FTransferStructure.Caption := Preferences.LoadStr(215);
  FTransferData.Caption := Preferences.LoadStr(216);

  GTransferOptions.Caption := Preferences.LoadStr(238);
  FLTransferGeneral.Caption := Preferences.LoadStr(108) + ':';
  FTransferDisableKeys.Caption := Preferences.LoadStr(621);

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GErrorMessages.Caption := Preferences.LoadStr(392);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDTransfer.CMExecutedDone(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  FreeAndNil(Transfer);

  FMaster.Items.BeginUpdate();
  FMaster.Items.Clear();
  FMaster.Items.EndUpdate();
  FSlave.Items.BeginUpdate();
  FSlave.Items.Clear();
  FSlave.Items.EndUpdate();

  FBBack.Enabled := False;
  if (Success) then
  begin
    FBCancel.Caption := Preferences.LoadStr(231);
    FBCancel.ModalResult := mrOk;
  end;
end;

procedure TDTransfer.CMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTools.PProgressInfos;
begin
  Infos := TTools.PProgressInfos(Message.LParam);

  if (Infos^.TablesSum < 0) then
    FEntieredTables.Caption := '???'
  else
    FEntieredTables.Caption := FormatFloat('#,##0', Infos^.TablesSum, LocaleFormatSettings);
  if (Infos^.TablesDone < 0) then
    FDoneTables.Caption := '???'
  else
    FDoneTables.Caption := FormatFloat('#,##0', Infos^.TablesDone, LocaleFormatSettings);

  if (Infos^.RecordsSum < 0) then
    FEntieredRecords.Caption := '???'
  else
    FEntieredRecords.Caption := FormatFloat('#,##0', Infos^.RecordsSum, LocaleFormatSettings);
  if (Infos^.RecordsDone < 0) then
    FDoneRecords.Caption := '???'
  else
    FDoneRecords.Caption := FormatFloat('#,##0', Infos^.RecordsDone, LocaleFormatSettings);

  FEntieredTime.Caption := TimeToStr(Infos^.TimeSum, DurationFormatSettings);
  FDoneTime.Caption := TimeToStr(Infos^.TimeDone, DurationFormatSettings);

  FProgressBar.Position := Infos^.Progress;

  if (Assigned(Transfer) and Transfer.Suspended) then
    Application.ProcessMessages();
end;

function TDTransfer.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDTransfer.FBBackClick(Sender: TObject);
begin
  if (PageControl.ActivePage = TSTransferOptions)then
    PageControl.ActivePage := TSSelect
  else if (PageControl.ActivePage = TSExecute) then
    PageControl.ActivePage := TSTransferOptions;
end;

procedure TDTransfer.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Transfer)) then
  begin
    SetEvent(Transfer.UserAbort);
    if (not Transfer.Suspended) then
      Transfer.WaitFor();
  end;
end;

procedure TDTransfer.FBForwardClick(Sender: TObject);
begin
  if (PageControl.ActivePage = TSSelect) then
    PageControl.ActivePage := TSTransferOptions
  else if (PageControl.ActivePage = TSTransferOptions)then
    PageControl.ActivePage := TSExecute;
end;

procedure TDTransfer.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDTransfer.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (Event.EventType in [ceAfterExecuteSQL]) then
    if (Assigned(WantedNodeExpand)) then
      WantedNodeExpand.Expand(False)
    else if (WantedExecute) then
      TSExecuteShow(Event.Sender);
end;

procedure TDTransfer.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  Transfer := nil;
  BorderStyle := bsSizeable;

  FMaster.Images := Preferences.SmallImages;
  FSlave.Images := Preferences.SmallImages;

  FTransferStructure.Checked := Preferences.Transfer.Structure;
  FTransferData.Checked := Preferences.Transfer.Data;
  FTransferDisableKeys.Checked := toDisableForeignKeys in Preferences.Transfer.Options;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDTransfer.FormHide(Sender: TObject);
var
  I: Integer;
begin
  if (ModalResult in [mrNone, mrOk]) then
  begin
    if (FTransferData.Enabled) then
      Preferences.Transfer.Structure := FTransferStructure.Checked;
    if (FTransferData.Enabled) then
      Preferences.Transfer.Data := FTransferData.Checked;
    if (FTransferDisableKeys.Enabled) then
      if (FTransferDisableKeys.Checked) then Include(Preferences.Transfer.Options, toDisableForeignKeys) else Exclude(Preferences.Transfer.Options, toDisableForeignKeys);
  end;

  for I := 0 to Length(Clients) - 1 do
    if (Assigned(Clients[I])) then
    begin
      Clients[I].UnRegisterEventProc(FormClientEvent);
      if (Assigned(Clients[I]) and (Clients[I] <> MasterClient) and (Clients[I] <> MasterClient)) then
        FreeAndNil(Clients[I]);
    end;
  SetLength(Clients, 0);

  Preferences.Transfer.Height := Height;
  Preferences.Transfer.Width := Width;
  Preferences.Transfer.Left := Left;
  Preferences.Transfer.Top := Top;
end;

procedure TDTransfer.FormShow(Sender: TObject);
var
  I: Integer;
begin
  HelpContext := 1094;

  if ((Preferences.Transfer.Width > 0) and (Preferences.Transfer.Height > 0)) then
  begin
    Width := Preferences.Transfer.Width;
    Height := Preferences.Transfer.Height;
  end
  else
  begin
    Width := Constraints.MinWidth;
    Height := Constraints.MinHeight;
  end;
  if ((Preferences.Transfer.Left > 0) and (Preferences.Transfer.Top > 0)) then
  begin
    Left := Preferences.Transfer.Left;
    Top := Preferences.Transfer.Top;
  end;

  WantedExecute := False;
  WantedNodeExpand := nil;

  SetLength(Clients, Accounts.Count);
  for I := 0 to Accounts.Count - 1 do
  begin
    if (Assigned(MasterClient) and (Accounts[I] = MasterClient.Account)) then
      Clients[I] := MasterClient
    else if (Assigned(MasterClient) and (Accounts[I] = MasterClient.Account)) then
      Clients[I] := MasterClient
    else
      Clients[I] := nil;
  end;

  InitTSSelect(Sender);

  TSSelectShow(Sender);
  if (not Assigned(SlaveClient)) then
  begin
    PageControl.ActivePage := TSSelect;
    if (Assigned(MasterClient)) then
      ActiveControl := FMaster
    else
      ActiveControl := FBCancel;
  end
  else
  begin
    PageControl.ActivePage := TSTransferOptions;
    ActiveControl := FTransferData;
  end;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;
end;

procedure TDTransfer.FTransferDataClick(Sender: TObject);
begin
  FTransferStructure.Checked := FTransferStructure.Checked or FTransferData.Checked;

  FBForward.Enabled := FTransferStructure.Checked or FTransferData.Checked;
end;

procedure TDTransfer.FTransferDataKeyPress(Sender: TObject; var Key: Char);
begin
  FTransferDataClick(Sender);
end;

procedure TDTransfer.FTransferStructureClick(Sender: TObject);
begin
  FTransferData.Checked := FTransferData.Checked and FTransferStructure.Checked;

  FBForward.Enabled := FTransferStructure.Checked or FTransferData.Checked;
end;

procedure TDTransfer.FTransferStructureKeyPress(Sender: TObject;
  var Key: Char);
begin
  FTransferStructureClick(Sender);
end;

function TDTransfer.GetClient(const Index: Integer): TCClient;
begin
  if (not Assigned(Clients[Index])) then
  begin
    Clients[Index] := TCClient.Create(fClient.Clients, Accounts[Index]);
    DConnecting.Client := Clients[Index];
    if (not DConnecting.Execute()) then
      FreeAndNil(Clients[Index]);
  end;

  Result := Clients[Index];

  if (Assigned(Result)) then
    Result.RegisterEventProc(FormClientEvent);
end;

procedure TDTransfer.InitTSSelect(Sender: TObject);
var
  DatabaseNames: TStringList;
  DatabaseNode: TTreeNode;
  FMasterOnChange: TTVChangedEvent;
  FSlaveOnChange: TTVChangedEvent;
  I: Integer;
  Node: TTreeNode;
  SelectedNodes: TList;
  AccountNode: TTreeNode;
  TableNames: TStringList;
  TableNode: TTreeNode;
begin
  FMasterOnChange := FMaster.OnChange;
  FMaster.OnChange := nil;
  FSlaveOnChange := FSlave.OnChange;
  FSlave.OnChange := nil;

  FMaster.Items.BeginUpdate();
  FMaster.Items.Clear();
  FMaster.Items.EndUpdate();
  FSlave.Items.BeginUpdate();
  FSlave.Items.Clear();
  FSlave.Items.EndUpdate();

  for I := 0 to Accounts.Count - 1 do
  begin
    Node := FMaster.Items.Add(nil, Accounts[I].Name);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    Node := FSlave.Items.Add(nil, Accounts[I].Name);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;
  end;

  if (Assigned(MasterClient)) then
  begin
    MasterClient.BeginSynchro();

    SelectedNodes := TList.Create();
    DatabaseNames := TStringList.Create();
    TableNames := TStringList.Create();

    DatabaseNames.Text := ReplaceStr(MasterDatabaseName, ',', #13#10);
    TableNames.Text := ReplaceStr(MasterTableName, ',', #13#10);

    AccountNode := FMaster.TopItem;
    while (Assigned(AccountNode)) do
    begin
      if (AccountNode.Text = MasterClient.Account.Name) then
      begin
        if (DatabaseNames.Count = 0) then
          AccountNode.Selected := True
        else
        begin
          AccountNode.Expand(False);
          DatabaseNode := AccountNode.getFirstChild();
          while (Assigned(DatabaseNode)) do
          begin
            if ((DatabaseNames.IndexOf(DatabaseNode.Text) < 0) or (TableNames.Count = 0)) then
              DatabaseNode.Selected := DatabaseNames.IndexOf(DatabaseNode.Text) >= 0
            else
            begin
              DatabaseNode.Expand(False);
              TableNode := DatabaseNode.getFirstChild();
              while (Assigned(TableNode)) do
              begin
                if (TableNames.IndexOf(TableNode.Text) >= 0) then
                  SelectedNodes.Add(TableNode);
                TableNode := TableNode.getNextSibling();
              end;
            end;
            DatabaseNode := DatabaseNode.getNextSibling();
          end;
        end;
      end;
      AccountNode := AccountNode.getNextSibling();
    end;
    if (SelectedNodes.Count = 1) then
    begin
      FMaster.Selected := SelectedNodes[0];
      if (Assigned(FMaster.Selected) and FMaster.AutoExpand) then
        FMaster.Selected.Expand(False);
    end
    else if (SelectedNodes.Count > 1) then
      FMaster.Select(SelectedNodes);

    SelectedNodes.Free();
    DatabaseNames.Free();
    TableNames.Free();

    if (Assigned(MasterClient)) then
    begin
      AccountNode := FSlave.TopItem;
      while (Assigned(AccountNode)) do
      begin
        if (AccountNode.Text = MasterClient.Account.Name) then
        begin
          AccountNode.Selected := True;
          if (SlaveDatabaseName <> '') then
          begin
            AccountNode.Expand(False);
            DatabaseNode := AccountNode.getFirstChild();
            while (Assigned(DatabaseNode)) do
            begin
              if (DatabaseNode.Text = SlaveDatabaseName) then
              begin
                DatabaseNode.Selected := True;
                if (SlaveTableName <> '') then
                begin
                  DatabaseNode.Expand(False);
                  TableNode := DatabaseNode.getFirstChild();
                  while (Assigned(TableNode)) do
                  begin
                    if (TableNode.Text = SlaveTableName) then
                      TableNode.Selected := True;
                    TableNode := DatabaseNode.getNextChild(TableNode);
                  end;
                end;
              end;
              DatabaseNode := DatabaseNode.getNextSibling();
            end;
          end;
        end;
        AccountNode := AccountNode.getNextSibling();
      end;
      MasterClient.EndSynchro();
    end;
    if (Assigned(FSlave.Selected) and FSlave.AutoExpand) then
      FSlave.Selected.Expand(False);
  end;

  TreeViewChange(Sender, nil);

  FMaster.OnChange := FMasterOnChange;
  FSlave.OnChange := FSlaveOnChange;
end;

procedure TDTransfer.miSelectAllClick(Sender: TObject);
var
  I: Integer;
  Nodes: TList;
begin
  Nodes := TList.Create();
  if (Assigned(MouseDownNode)) then
    for I := 0 to MouseDownNode.Count - 1 do
      Nodes.Add(MouseDownNode.Item[I]);
  FMaster.Select(Nodes);
  Nodes.Free();
end;

procedure TDTransfer.MMasterPopup(Sender: TObject);
begin
  miSelectAll.Enabled := FMaster.MultiSelect and Assigned(MouseDownNode) and (MouseDownNode.ImageIndex <> iiBaseTable);

  ShowEnabledItems(MMaster.Items);
end;

procedure TDTransfer.OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
var
  ErrorMsg: string;
  Flags: Integer;
  Msg: string;
  Text: string;
begin
  ErrorMsg := '';
  case (Error.ErrorType) of
    TE_Database:
      begin
        Msg := Preferences.LoadStr(165, IntToStr(Item.Client.ErrorCode), Item.Client.ErrorMessage);
        ErrorMsg := SQLUnwrapStmt(Item.Client.ErrorMessage);
        if (Item.Client.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (' + IntToStr(Item.Client.ErrorCode) + ')';
        ErrorMsg := ErrorMsg + '  -  ' + SQLUnwrapStmt(Item.Client.CommandText);
      end;
    TE_File:
      begin
        Msg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
        ErrorMsg := Msg;
      end;
    TE_NoPrimaryIndex:
      Msg := Preferences.LoadStr(722, Item.TableName);
    TE_DifferentPrimaryIndex:
      Msg := Preferences.LoadStr(723, Item.TableName);
    else
      Msg := Error.ErrorMessage;
  end;

  Flags := MB_CANCELTRYCONTINUE + MB_ICONERROR;
  case (MsgBox(Msg, Preferences.LoadStr(45), Flags, Handle)) of
    IDCANCEL,
    IDABORT: Success := daAbort;
    IDRETRY,
    IDTRYAGAIN: Success := daRetry;
    IDCONTINUE,
    IDIGNORE: Success := daFail;
  end;

  if ((Success in [daAbort, daFail]) and (ErrorMsg <> '')) then
  begin
    Text := FErrorMessages.Text;
    if (Text <> '') then
      Text := Text + #13#10;
    Text := Text + Trim(ErrorMsg);

    try
      SendMessage(FErrorMessages.Handle, WM_SETTEXT, 0, LPARAM(PChar(Text)));
    except
    end;

    PostMessage(FErrorMessages.Handle, WM_VSCROLL, SB_BOTTOM, 0);

    FErrors.Caption := IntToStr(TTools(Sender).ErrorCount);
  end;
end;

procedure TDTransfer.PageControlResize(Sender: TObject);
begin
  GMaster.Width := PageControl.Width div 2 - 3 * GMaster.Left;
  GSlave.Width := GMaster.Width;
  GSlave.Left := PageControl.Width - GSlave.Width - 3 * GMaster.Left;
end;

procedure TDTransfer.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if (ModalResult = mrNone) then
  begin
    if ((Sender = FMaster) and Assigned(Node)) then
      FMaster.MultiSelect := Assigned(Node.Parent);

    FBForward.Enabled := Assigned(FMaster.Selected) and Assigned(FMaster.Selected.Parent) and Assigned(FSlave.Selected)
      and (FMaster.Selected.Parent.Level = FSlave.Selected.Level)
      and ((FMaster.Selected.ImageIndex <> iiDatabase) or (FMaster.Selected.Parent.Text <> FSlave.Selected.Text))
      and ((FMaster.Selected.ImageIndex <> iiBaseTable) or (FMaster.Selected.Parent.Text <> FSlave.Selected.Text) or (FMaster.Selected.Parent.Parent.Text <> FSlave.Selected.Parent.Text));
  end;
end;

procedure TDTransfer.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Client: TCClient;
  Database: TCDatabase;
  I: Integer;
  NewNode: TTreeNode;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (Assigned(WantedNodeExpand)) then
  begin
    for I := 0 to Length(Clients) - 1 do
      if (Assigned(Clients[I])) then
        Clients[I].UnRegisterEventProc(FormClientEvent);
    WantedNodeExpand := nil;
  end;

  if (Assigned(Node)) then
    if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
    begin
      case (Node.ImageIndex) of
        iiServer:
          begin
            Client := GetClient(Node.Index);
            if (Assigned(Client)) then
              if (not Client.Update() and Client.Asynchron) then
                WantedNodeExpand := Node
              else
              begin
                for I := 0 to Client.Databases.Count - 1 do
                  if (not (Client.Databases[I] is TCSystemDatabase)) then
                  begin
                    NewNode := TreeView.Items.AddChild(Node, Client.Databases[I].Name);
                    NewNode.ImageIndex := iiDatabase;
                    NewNode.HasChildren := TreeView = FMaster;
                  end;
                Node.HasChildren := Assigned(Node.getFirstChild());
              end;
          end;
        iiDatabase:
          begin
            Client := GetClient(Node.Parent.Index);
            Database := Client.DatabaseByName(Node.Text);
            if ((not Database.Tables.Update() or not Client.Update(Database.Tables)) and Client.Asynchron) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
                if ((Database.Tables[I] is TCBaseTable) and Assigned(TCBaseTable(Database.Tables[I]).Engine) and not TCBaseTable(Database.Tables[I]).Engine.IsMerge and (RightStr(Database.Tables[I].Name, Length(BackupExtension)) <> BackupExtension)) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                  NewNode.ImageIndex := iiBaseTable;
                end;
            end;
          end;
      end;
    end;
end;

procedure TDTransfer.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDTransfer.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TTreeView_Ext) then
    MouseDownNode := TTreeView_Ext(Sender).GetNodeAt(X, Y)
  else
    MouseDownNode := nil;
end;

procedure TDTransfer.TSExecuteShow(Sender: TObject);
var
  Answer: Integer;

  procedure AddTable(const MasterClient: TCClient; const MasterDatabaseName, MasterTableName: string; const SlaveClient: TCClient; const SlaveDatabaseName, SlaveTableName: string);
  begin
    if ((Answer <> IDYESALL) and Assigned(SlaveClient.DatabaseByName(SlaveDatabaseName)) and Assigned(SlaveClient.DatabaseByName(SlaveDatabaseName).TableByName(SlaveTableName))) then
      Answer := MsgBox(Preferences.LoadStr(700, SlaveDatabaseName + '.' + SlaveTableName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);

    if (Answer in [IDYES, IDYESALL]) then
      Transfer.Add(
        MasterClient, MasterDatabaseName, MasterTableName,
        SlaveClient, SlaveDatabaseName, SlaveTableName
      )
    else if (Answer = IDCANCEL) then
      FreeAndNil(Transfer);
  end;

  function InitializeNode(const Client: TCClient; const Node: TTreeNode): Boolean;
  var
    Database: TCDatabase;
    I: Integer;
    J: Integer;
    Objects: TList;
  begin
    Objects := TList.Create();
    case (Node.ImageIndex) of
      iiServer:
        begin
          Result := not Client.Update() and Client.Asynchron;
          if (not Result) then
            for I := 0 to Client.Databases.Count - 1 do
              if (not Result and not (Client.Databases[I] is TCSystemDatabase)) then
              begin
                Database := Client.Databases[I];
                Result := not Database.Tables.Update() and Client.Asynchron;
                if (not Result) then
                begin
                  for J := 0 to Database.Tables.Count - 1 do
                    if (Database.Tables[J] is TCBaseTable) then
                      Objects.Add(Database.Tables[J]);
                  Result := not Client.Update(Objects);
                end;
              end;
        end;
      iiDatabase:
        begin
          Database := Client.DatabaseByName(Node.Text);
          Result := not Database.Tables.Update() and Client.Asynchron;
          if (not Result) then
          begin
            for J := 0 to Database.Tables.Count - 1 do
              if (Database.Tables[J] is TCBaseTable) then
                Objects.Add(Database.Tables[J]);
            Result := not Client.Update(Objects);
          end;
        end;
      iiBaseTable:
        begin
          Database := Client.DatabaseByName(Node.Parent.Text);
          for J := 0 to Database.Tables.Count - 1 do
            if (Node.Parent.Item[J].Selected) then
              Objects.Add(Database.Tables[J]);
          Result := not Client.Update(Objects);
        end;
      else
        Result := False;
    end;
    Objects.Free();
  end;

var
  I: Integer;
  J: Integer;
  Database: TCDatabase;
  MasterClient: TCClient;
  Node: TTreeNode;
  ProgressInfos: TTools.TProgressInfos;
  SlaveClient: TCClient;
begin
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  ProgressInfos.TablesDone := 0;
  ProgressInfos.TablesSum := 0;
  ProgressInfos.RecordsDone := 0;
  ProgressInfos.RecordsSum := 0;
  ProgressInfos.TimeDone := 0;
  ProgressInfos.TimeSum := 0;
  ProgressInfos.Progress := 0;
  SendMessage(Self.Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));

  FBForward.Enabled := False;
  FBCancel.Default := True;
  ActiveControl := FBCancel;

  Node := FMaster.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  MasterClient := GetClient(Node.Index);
  WantedExecute := InitializeNode(MasterClient, FMaster.Selected);

  if (WantedExecute) then
    SlaveClient := nil
  else
  begin
    Node := FSlave.Selected;
    while (Assigned(Node.Parent)) do Node := Node.Parent;
    SlaveClient := GetClient(Node.Index);
    WantedExecute := InitializeNode(SlaveClient, FSlave.Selected);
  end;

  if (not WantedExecute) then
  begin
    Answer := IDYES;

    Transfer := TTTransfer.Create();
    Transfer.Wnd := Self.Handle;
    Transfer.UpdateMessage := CM_UPDATEPROGRESSINFO;
    Transfer.ExecutedMessage := CM_EXECUTIONDONE;
    Transfer.Backup := False;
    Transfer.Data := FTransferData.Checked;
    Transfer.DisableKeys := FTransferDisableKeys.Checked;
    Transfer.Structure := FTransferStructure.Checked;
    Transfer.UpdateData := False;
    Transfer.UpdateStructure := False;
    Transfer.OnError := OnError;

    for I := 0 to FMaster.Selected.Parent.Count - 1 do
      if (FMaster.Selected.Parent[I].Selected) then
        case (FMaster.Selected.Parent[I].ImageIndex) of
          iiDatabase:
            begin
              Database := MasterClient.DatabaseByName(FMaster.Selected.Parent[I].Text);
              for J := 0 to Database.Tables.Count - 1 do
                if ((Database.Tables[J] is TCBaseTable) and Assigned(TCBaseTable(Database.Tables[J]).Engine) and not TCBaseTable(Database.Tables[J]).Engine.IsMerge and (RightStr(Database.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
                  AddTable(
                    MasterClient, Database.Name, Database.Tables[J].Name,
                    SlaveClient, Database.Name, Database.Tables[J].Name
                  );
            end;
          iiBaseTable:
            AddTable(
              MasterClient, FMaster.Selected.Parent.Text, FMaster.Selected.Parent[I].Text,
              SlaveClient, FSlave.Selected.Text, FMaster.Selected.Parent[I].Text
            );
        end;

    {$IFNDEF Debug}
      Transfer.Start();
    {$ELSE}
      Transfer.Execute();
    {$ENDIF}
  end;
end;

procedure TDTransfer.TSSelectShow(Sender: TObject);
begin
  FBBack.Enabled := False;
  FBForward.Caption := Preferences.LoadStr(229) + ' >';

  TreeViewChange(Sender, nil);
end;

procedure TDTransfer.TSTransferOptionsShow(Sender: TObject);
begin
  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(230);
  FBForward.Enabled := True;
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;
end;

initialization
  FTransfer := nil;
end.
