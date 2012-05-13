unit fDTransfer;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB,
  ComCtrls_Ext, Forms_Ext, ExtCtrls_Ext,
  fClient, fBase, MySQLDB, fSession, fTools, Menus,
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
  private
    Connections: array of TCClient;
    MouseDownNode: TTreeNode;
    Transfer: TTTransfer;
    procedure InitTSSelect(Sender: TObject);
    procedure OnConvertError(Sender: TObject; Text: string);
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
  fPreferences;

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

  ModalResult := mrOk;

  InitTSSelect(nil); // Löschen falls zurück auf TSSelect

  ModalResult := mrNone;

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

  for I := 0 to Length(Connections) - 1 do
    if (Assigned(Connections[I]) and (Connections[I] <> MasterClient) and (Connections[I] <> MasterClient)) then
      fClient.Clients.ReleaseClient(Connections[I]);
  SetLength(Connections, 0);

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

  SetLength(Connections, Sessions.Count);
  for I := 0 to Sessions.Count - 1 do
  begin
    if (Assigned(MasterClient) and (Sessions[I] = MasterClient.Session)) then
      Connections[I] := MasterClient
    else if (Assigned(MasterClient) and (Sessions[I] = MasterClient.Session)) then
      Connections[I] := MasterClient
    else
      Connections[I] := nil;
  end;

  InitTSSelect(Sender);

  TSSelectShow(Sender);
  if (not Assigned(MasterClient)) then
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

procedure TDTransfer.InitTSSelect(Sender: TObject);
var
  DatabaseNames: TStringList;
  DatabaseNode: TTreeNode;
  FMasterOnChange: TTVChangedEvent;
  FSlaveOnChange: TTVChangedEvent;
  I: Integer;
  Node: TTreeNode;
  SelectedNodes: TList;
  SessionNode: TTreeNode;
  TableNames: TStringList;
  TableNode: TTreeNode;
begin
  FMasterOnChange := FMaster.OnChange;
  FMaster.OnChange := nil;
  FSlaveOnChange := FSlave.OnChange;
  FSlave.OnChange := nil;

  FMaster.Items.Clear();
  FSlave.Items.Clear();

  for I := 0 to Sessions.Count - 1 do
  begin
    Node := FMaster.Items.Add(nil, Sessions[I].Name);
    Node.ImageIndex := Sessions[I].ImageIndex;
    if (Node.ImageIndex < 0) then Node.ImageIndex := iiServer;
    Node.SelectedIndex := Node.ImageIndex;
    Node.HasChildren := True;

    Node := FSlave.Items.Add(nil, Sessions[I].Name);
    Node.ImageIndex := Sessions[I].ImageIndex;
    if (Node.ImageIndex < 0) then Node.ImageIndex := iiServer;
    Node.SelectedIndex := Node.ImageIndex;
    Node.HasChildren := True;
  end;

  if (Assigned(MasterClient)) then
  begin
    SelectedNodes := TList.Create();
    DatabaseNames := TStringList.Create();
    TableNames := TStringList.Create();

    DatabaseNames.Text := ReplaceStr(MasterDatabaseName, ',', #13#10);
    TableNames.Text := ReplaceStr(MasterTableName, ',', #13#10);

    SessionNode := FMaster.TopItem;
    while (Assigned(SessionNode)) do
    begin
      if (SessionNode.Text = MasterClient.Session.Name) then
      begin
        if (DatabaseNames.Count = 0) then
          SessionNode.Selected := True
        else
        begin
          SessionNode.Expand(False);
          DatabaseNode := SessionNode.getFirstChild();
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
      SessionNode := SessionNode.getNextSibling();
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
      SessionNode := FSlave.TopItem;
      while (Assigned(SessionNode)) do
      begin
        if (SessionNode.Text = MasterClient.Session.Name) then
        begin
          SessionNode.Selected := True;
          if (SlaveDatabaseName <> '') then
          begin
            SessionNode.Expand(False);
            DatabaseNode := SessionNode.getFirstChild();
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
        SessionNode := SessionNode.getNextSibling();
      end;
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

procedure TDTransfer.OnConvertError(Sender: TObject; Text: string);
begin
  ConvertError(Sender, Text);
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
var
  AllowExpansion: Boolean;
begin
  if (ModalResult = mrNone) then
  begin
    if ((Sender = FMaster) and Assigned(Node)) then
      FMaster.MultiSelect := Assigned(Node.Parent);

    AllowExpansion := True;
    if (Assigned(Node) and Node.Selected and not Node.Expanded and not Assigned(Node.Parent)) then
      TreeViewExpanding(Sender, Node, AllowExpansion);

    FBForward.Enabled := Assigned(FMaster.Selected) and Assigned(FMaster.Selected.Parent) and Assigned(FSlave.Selected)
      and (FMaster.Selected.Parent.Level = FSlave.Selected.Level)
      and ((FMaster.Selected.ImageIndex <> iiDatabase) or (FMaster.Selected.Parent.Text <> FSlave.Selected.Text))
      and ((FMaster.Selected.ImageIndex <> iiBaseTable) or (FMaster.Selected.Parent.Text <> FSlave.Selected.Text) or (FMaster.Selected.Parent.Parent.Text <> FSlave.Selected.Parent.Text));
  end;
end;

procedure TDTransfer.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  B: Boolean;
  Client: TCClient;
  I: Integer;
  NewNode: TTreeNode;
  NodeSelected: Boolean;
  TempOnChange: TTVChangedEvent;
  //  Database: TCDatabase;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (not Assigned(TreeView.Selected)) then
    TreeView.Selected := Node;

  if (Assigned(TreeView.Selected)) then
  begin
    if (Sender is TTreeView_Ext) then
    begin
      TempOnChange := TTreeView_Ext(Sender).OnChange;
      TTreeView_Ext(Sender).OnChange := nil;
      NodeSelected := Node.Selected;

      if (Assigned(Node) and NodeSelected) then
        for I := 0 to TTreeView_Ext(Sender).Items.Count - 1 do
          if (TTreeView_Ext(Sender).Items[I].Selected and (TTreeView_Ext(Sender).Items[I] <> Node) and (TTreeView_Ext(Sender).Items[I].Parent <> Node.Parent)) then
            TTreeView_Ext(Sender).Items[I].Selected := False;

      TTreeView_Ext(Sender).OnChange := TempOnChange;
    end;

    if (not Assigned(Node.Parent)) then
    begin
      I := Sessions.SessionByName(Node.Text).Index;
      if ((0 <= I) and (I < Length(Connections)) and not Assigned(Connections[I])) then
      begin
        Connections[I] := fClient.Clients.CreateClient(Sessions.SessionByName(Node.Text), B);
        if (Assigned(Connections[I])) then
          Connections[I].OnConvertError := OnConvertError;
      end;

      Client := Connections[I];
    end
    else
    begin
      if (not Assigned(Node.Parent.Parent)) then
        Client := Connections[Sessions.SessionByName(Node.Parent.Text).Index]
      else
        Client := Connections[Sessions.SessionByName(Node.Parent.Parent.Text).Index];
    end;

    if (Assigned(Client)) then
    begin
      if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
        if (not Assigned(Node.Parent)) then
        begin
          for I := 0 to Client.Databases.Count - 1 do
            if (not (Client.Databases[I] is TCSystemDatabase)) then
            begin
              NewNode := TreeView.Items.AddChild(Node, Client.Databases[I].Name);
              NewNode.ImageIndex := iiDatabase; NewNode.SelectedIndex := NewNode.ImageIndex;
              NewNode.HasChildren := (Sender = FMaster);
            end;
        end
        else if ((Node.ImageIndex = iiDatabase) and (Sender = FMaster)) then
        begin
// ToDo
//          Database := Client.DatabaseByName(Node.Text);
//          if (Database.Initialize()) then
//            for I := 0 to Database.Tables.Count - 1 do
//              if ((Database.Tables[I] is TCBaseTable) and Assigned(TCBaseTable(Database.Tables[I]).Engine) and not TCBaseTable(Database.Tables[I]).Engine.IsMerge and (RightStr(Database.Tables[I].Name, Length(BackupExtension)) <> BackupExtension)) then
//              begin
//                NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
//                NewNode.ImageIndex := iiBaseTable; NewNode.SelectedIndex := NewNode.ImageIndex;
//                NewNode.HasChildren := False;
//              end;
        end;
      Node.HasChildren := Assigned(Node.getFirstChild());
    end;
  end;
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
    if ((Answer <> IDYESALL) and Assigned(MasterClient.DatabaseByName(SlaveDatabaseName)) and Assigned(MasterClient.DatabaseByName(SlaveDatabaseName).TableByName(SlaveTableName))) then
      Answer := MsgBox(Preferences.LoadStr(700, SlaveDatabaseName + '.' + SlaveTableName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);

    if (Answer in [IDYES, IDYESALL]) then
      Transfer.Add(MasterClient, MasterDatabaseName, MasterTableName, MasterClient, SlaveDatabaseName, SlaveTableName)
    else if (Answer = IDCANCEL) then
      FreeAndNil(Transfer);
  end;

var
  I: Integer;
  //  J: Integer;
  //  MClient: TCClient;
  //  MDatabase: TCDatabase;
  //  SSession: TSSession;
  ProgressInfos: TTools.TProgressInfos;
begin
  FBForward.Enabled := False;
  FBCancel.Default := True;
  ActiveControl := FBCancel;

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

  for I := 0 to FMaster.Items.Count - 1 do
    if (FMaster.Items[I].Selected) then
      case (FMaster.Items[I].ImageIndex) of
        iiBaseTable:
          if (Assigned(Transfer)) then
            AddTable(
              Connections[Sessions.SessionByName(FMaster.Items[I].Parent.Parent.Text).Index], FMaster.Items[I].Parent.Text, FMaster.Items[I].Text,
              Connections[Sessions.SessionByName(FSlave.Selected.Parent.Text).Index], FSlave.Selected.Text, FMaster.Items[I].Text
            );
        iiDatabase:
          begin
// ToDo
//            MClient := Connections[Sessions.SessionByName(FMaster.Items[I].Parent.Text).Index];
//            MDatabase := MClient.DatabaseByName(FMaster.Items[I].Text);
//            SSession := Sessions.SessionByName(FSlave.Selected.Text);
//            if (MDatabase.Initialize()) then
//              for J := 0 to MDatabase.Tables.Count - 1 do
//                if (Assigned(Transfer) and (MDatabase.Tables[J] is TCBaseTable) and Assigned(TCBaseTable(MDatabase.Tables[J]).Engine) and not TCBaseTable(MDatabase.Tables[J]).Engine.IsMerge and (RightStr(MDatabase.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
//                  AddTable(
//                    MClient, MDatabase.Name, MDatabase.Tables[J].Name,
//                    Connections[SSession.Index], MDatabase.Name, MDatabase.Tables[J].Name
//                  );
          end;
      end;

  if (not Assigned(Transfer)) then
    Perform(CM_EXECUTIONDONE, WPARAM(False), 0)
  else
    {$IFNDEF Debug}
      Transfer.Start();
    {$ELSE}
      Transfer.Execute();
    {$ENDIF}
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
