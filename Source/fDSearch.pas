unit fDSearch;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls, DB, ExtCtrls,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext,
  fBase, fClient, MySQLDB, fAccount, fTools, fFClient;

type
  TDSTableItem = record
    Account: TSAccount;
    DatabaseName: string;
    TableName: string;
  end;

  TDSearch = class(TForm_Ext)
    FBackup: TCheckBox;
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
    FFFindText: TComboBox_Ext;
    FFMatchCase: TCheckBox;
    FFRegExpr: TCheckBox;
    FFWholeValue: TCheckBox;
    FLBackup: TLabel;
    FLDone: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLFFindText: TLabel;
    FLFSearchOptions: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLReplaceText: TLabel;
    FLRFindText: TLabel;
    FLRSearchOptions: TLabel;
    FProgressBar: TProgressBar;
    FReplaceText: TComboBox_Ext;
    FRFindText: TComboBox_Ext;
    FRMatchCase: TCheckBox;
    FRRegExpr: TCheckBox;
    FRWholeValue: TCheckBox;
    FSelect: TTreeView_Ext;
    FTables: TListView_Ext;
    GBackup: TGroupBox_Ext;
    GFOptions: TGroupBox_Ext;
    GFWhat: TGroupBox_Ext;
    GMessages: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GROptions: TGroupBox_Ext;
    GRWhat: TGroupBox_Ext;
    GSelect: TGroupBox_Ext;
    MTables: TPopupMenu;
    mTCopy: TMenuItem;
    PageControl: TPageControl;
    PErrorMessages: TPanel_Ext;
    PSelect: TPanel_Ext;
    TSExecute: TTabSheet;
    TSFOptions: TTabSheet;
    TSROptions: TTabSheet;
    TSSelect: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FFFindTextChange(Sender: TObject);
    procedure FFRegExprClick(Sender: TObject);
    procedure FFRegExprKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FRFindTextChange(Sender: TObject);
    procedure FRRegExprClick(Sender: TObject);
    procedure FRRegExprKeyPress(Sender: TObject; var Key: Char);
    procedure FSelectChange(Sender: TObject; Node: TTreeNode);
    procedure FSelectDblClick(Sender: TObject);
    procedure FSelectExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FTablesDblClick(Sender: TObject);
    procedure mTCopyClick(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFOptionsShow(Sender: TObject);
    procedure TSROptionsShow(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
    procedure FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
  private
    Clients: array of TCClient;
    ExecuteClient: TCClient;
    Find: TTFind;
    SQLWait: Boolean;
    ReplaceClient: TCClient;
    Tables: array of TDSTableItem;
    WantedExecute: Boolean;
    WantedNodeExpand: TTreeNode;
    procedure FormClientEvent(const Event: TCClient.TEvent);
    function GetClient(const Index: Integer): TCClient;
    procedure OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutedDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    Client: TCClient;
    DatabaseName: string;
    FieldName: string;
    Frame: TFClient;
    SearchOnly: Boolean;
    TableName: string;
    function Execute(): Boolean;
  end;

function DSearch(): TDSearch;

implementation {***************************************************************}

{$R *.dfm}

uses
  Consts, StrUtils, CommCtrl, RichEdit,
  SQLUtils,
  fPreferences, fURI;

var
  FSearch: TDSearch;

function DSearch(): TDSearch;
begin
  if (not Assigned(FSearch)) then
  begin
    Application.CreateForm(TDSearch, FSearch);
    FSearch.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FSearch;
end;

{ TDSearch ********************************************************************}

procedure TDSearch.CMChangePreferences(var Message: TMessage);
begin
  GSelect.Caption := Preferences.LoadStr(721);

  GFWhat.Caption := Preferences.LoadStr(227);
  FLFFindText.Caption := Preferences.LoadStr(719) + ':';

  GFOptions.Caption := Preferences.LoadStr(238);
  FLFSearchOptions.Caption := Preferences.LoadStr(715) + ':';
  FFMatchCase.Caption := Preferences.LoadStr(716);
  FFWholeValue.Caption := Preferences.LoadStr(717);
  FFRegExpr.Caption := Preferences.LoadStr(718);

  GRWhat.Caption := Preferences.LoadStr(227);
  FLRFindText.Caption := Preferences.LoadStr(719) + ':';
  FLRFindText.Caption := Preferences.LoadStr(719) + ':';
  FLReplaceText.Caption := Preferences.LoadStr(720) + ':';

  GROptions.Caption := Preferences.LoadStr(238);
  FLRSearchOptions.Caption := Preferences.LoadStr(715) + ':';
  FRMatchCase.Caption := Preferences.LoadStr(716);
  FRWholeValue.Caption := Preferences.LoadStr(717);
  FRRegExpr.Caption := Preferences.LoadStr(718);

  GBackup.Caption := Preferences.LoadStr(713);
  FLBackup.Caption := Preferences.LoadStr(713) + ':';
  FBackup.Caption := Preferences.LoadStr(714);

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211);
  FLDone.Caption := Preferences.LoadStr(232);
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  mtCopy.Caption := MainAction('aECopy').Caption;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDSearch.CMExecutedDone(var Message: TMessage);
begin
  if (SearchOnly and (FTables.Items.Count = 0)) then
    MsgBox(Preferences.LoadStr(533, Find.FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

  FreeAndNil(Find);
  if (Assigned(ExecuteClient)) then
    ExecuteClient := nil;
  if (Assigned(ReplaceClient)) then
  begin
    fClient.Clients.ReleaseClient(ReplaceClient);
    ReplaceClient := nil;
  end;

  ModalResult := mrNone;

  FBCancel.Caption := Preferences.LoadStr(231);
  FBCancel.ModalResult := mrOk;

  ActiveControl := FBCancel;
end;

procedure TDSearch.CMUpdateProgressInfo(var Message: TMessage);
var
  CurrentItem: TTFind.PItem;
  Found: Boolean;
  I: Integer;
  Infos: TTools.PProgressInfos;
  Item: TListItem;
begin
  CurrentItem := TTFind.PItem(Message.WParam);
  Infos := TTools.PProgressInfos(Message.LParam);

  if (Infos.TablesSum < 0) then
    FEntieredTables.Caption := '???'
  else
    FEntieredTables.Caption := FormatFloat('#,##0', Infos.TablesSum, LocaleFormatSettings);
  if (Infos.TablesDone < 0) then
    FDoneTables.Caption := '???'
  else
    FDoneTables.Caption := FormatFloat('#,##0', Infos.TablesDone, LocaleFormatSettings);

  if (Infos.RecordsSum < 0) then
    FEntieredRecords.Caption := '???'
  else
    FEntieredRecords.Caption := FormatFloat('#,##0', Infos.RecordsSum, LocaleFormatSettings);
  if (Infos.RecordsDone < 0) then
    FDoneRecords.Caption := '???'
  else
    FDoneRecords.Caption := FormatFloat('#,##0', Infos.RecordsDone, LocaleFormatSettings);

  FEntieredTime.Caption := TimeToStr(Infos.TimeSum, DurationFormatSettings);
  FDoneTime.Caption := TimeToStr(Infos.TimeDone, DurationFormatSettings);

  FProgressBar.Position := Infos.Progress;

  if (Assigned(CurrentItem) and CurrentItem^.Done and (CurrentItem^.RecordsFound > 0)) then
  begin
    Found := False;
    for I := 0 to Length(Tables) - 1 do
      if ((Tables[I].Account = ExecuteClient.Account) and (Tables[I].DatabaseName = CurrentItem^.DatabaseName) and (Tables[I].TableName = CurrentItem^.TableName)) then
        Found := True;

    if (not Found) then
    begin
      SetLength(Tables, Length(Tables) + 1);

      Tables[Length(Tables) - 1].Account := ExecuteClient.Account;
      Tables[Length(Tables) - 1].DatabaseName := CurrentItem^.DatabaseName;
      Tables[Length(Tables) - 1].TableName := CurrentItem^.TableName;

      Item := FTables.Items.Add();
      if (not (FSelect.Selected.ImageIndex in [iiDatabase, iiBaseTable, iiField])) then
        Item.Caption := Item.Caption + ExecuteClient.Account.Name + '.';
      Item.Caption := Item.Caption + CurrentItem^.DatabaseName + '.';
      Item.Caption := Item.Caption + CurrentItem^.TableName + ' (' + IntToStr(CurrentItem^.RecordsFound) + ')';
    end;
  end;

  if (Assigned(Find) and Find.Suspended) then
    Application.ProcessMessages();
end;

function TDSearch.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDSearch.FBBackClick(Sender: TObject);
begin
  if ((PageControl.ActivePage = TSFOptions) or (PageControl.ActivePage = TSROptions)) then
    PageControl.ActivePage := TSSelect
  else if ((PageControl.ActivePage = TSExecute) and SearchOnly) then
    PageControl.ActivePage := TSFOptions
  else if ((PageControl.ActivePage = TSExecute) and not SearchOnly) then
    PageControl.ActivePage := TSROptions;
end;

procedure TDSearch.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Find)) then
  begin
    SetEvent(Find.UserAbort);
    if (not Find.Suspended) then
      Find.WaitFor();
  end;
end;

procedure TDSearch.FBForwardClick(Sender: TObject);
begin
  if (PageControl.ActivePage = TSSelect) then
    if (SearchOnly) then
      PageControl.ActivePage := TSFOptions
    else
      PageControl.ActivePage := TSROptions
  else if (PageControl.ActivePage = TSFOptions) then
    PageControl.ActivePage := TSExecute
  else if (PageControl.ActivePage = TSROptions) then
    PageControl.ActivePage := TSExecute;
end;

procedure TDSearch.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDSearch.FFFindTextChange(Sender: TObject);
begin
  FBForward.Enabled := (FFFindText.Text <> '') and not SQLWait;
end;

procedure TDSearch.FFRegExprClick(Sender: TObject);
begin
  FFWholeValue.Enabled := not FFRegExpr.Checked;
end;

procedure TDSearch.FFRegExprKeyPress(Sender: TObject; var Key: Char);
begin
  FFRegExprClick(Sender);
end;

procedure TDSearch.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (Event.EventType in [ceAfterExecuteSQL]) then
    if (Assigned(WantedNodeExpand)) then
      WantedNodeExpand.Expand(False)
    else if (WantedExecute) then
      TSExecuteShow(Event.Sender);
end;

procedure TDSearch.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  Find := nil;

  FSelect.Images := Preferences.SmallImages;

  FFFindText.Items.Clear();
  for I := Preferences.Find.FindTextMRU.Count - 1 downto 0 do
    FFFindText.Items.Insert(0, Preferences.Find.FindTextMRU.Values[I]);

  FFMatchCase.Checked := foMatchCase in Preferences.Find.Options;
  FFWholeValue.Checked := foWholeValue in Preferences.Find.Options;
  FFRegExpr.Checked := foRegExpr in Preferences.Find.Options;
  FFRegExprClick(Sender);

  FRFindText.Items.Clear();
  for I := Preferences.Replace.FindTextMRU.Count - 1 downto 0 do
    FRFindText.Items.Insert(0, Preferences.Replace.FindTextMRU.Values[I]);
  FReplaceText.Items.Clear();
  for I := Preferences.Replace.ReplaceTextMRU.Count - 1 downto 0 do
    FReplaceText.Items.Insert(0, Preferences.Replace.ReplaceTextMRU.Values[I]);

  FRMatchCase.Checked := roMatchCase in Preferences.Replace.Options;
  FRWholeValue.Checked := roWholeValue in Preferences.Replace.Options;
  FRRegExpr.Checked := roRegExpr in Preferences.Replace.Options;
  FRRegExprClick(Sender);

  FBackup.Checked := Preferences.Replace.Backup;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDSearch.FormHide(Sender: TObject);
var
  I: Integer;
begin
  FSelect.Selected := nil; // Make sure, not to call FSelectedChange with a selcted node
  FSelect.Items.BeginUpdate();
  FSelect.Items.Clear();
  FSelect.Items.EndUpdate();

  for I := 0 to Length(Clients) - 1 do
    if (Assigned(Clients[I])) then
    begin
      Clients[I].UnRegisterEventProc(FormClientEvent);
      if (Clients[I] <> Client) then
        fClient.Clients.ReleaseClient(Clients[I]);
    end;
  SetLength(Clients, 0);

  if (SearchOnly) then
  begin
    Preferences.Find.Height := Height;
    Preferences.Find.Width := Width;
    Preferences.Find.Left := Left;
    Preferences.Find.Top := Top;
  end
  else
  begin
    Preferences.Replace.Height := Height;
    Preferences.Replace.Width := Width;
    Preferences.Replace.Left := Left;
    Preferences.Replace.Top := Top;
  end;
end;

procedure TDSearch.FormShow(Sender: TObject);
var
  DatabaseNames: TStringList;
  DatabaseNode: TTreeNode;
  FieldNames: TStringList;
  FieldNode: TTreeNode;
  I: Integer;
  Node: TTreeNode;
  SelectedNodes: TList;
  AccountNode: TTreeNode;
  TableNames: TStringList;
  TableNode: TTreeNode;
begin
  if (SearchOnly) then
  begin
    Caption := ReplaceStr(Preferences.LoadStr(187), '&', '');
    Preferences.SmallImages.GetIcon(12, Icon);
    HelpContext := 1093;

    GMessages.Caption := Preferences.LoadStr(234);

    if ((Preferences.Find.Width > 0) and (Preferences.Find.Height > 0)) then
    begin
      Width := Preferences.Find.Width;
      Height := Preferences.Find.Height;
    end
    else
    begin
      Width := Constraints.MinWidth;
      Height := Constraints.MinHeight;
    end;
    if ((Preferences.Find.Left > 0) and (Preferences.Find.Top > 0)) then
    begin
      Left := Preferences.Find.Left;
      Top := Preferences.Find.Top;
    end;
  end
  else
  begin
    Caption := ReplaceStr(Preferences.LoadStr(416), '&', '');
    Preferences.SmallImages.GetIcon(29, Icon);
    HelpContext := 1090;

    GMessages.Caption := Preferences.LoadStr(392);

    if ((Preferences.Replace.Width > 0) and (Preferences.Replace.Height > 0)) then
    begin
      Width := Preferences.Replace.Width;
      Height := Preferences.Replace.Height;
    end
    else
    begin
      Width := Constraints.MinWidth;
      Height := Constraints.MinHeight;
    end;
    if ((Preferences.Replace.Left > 0) and (Preferences.Replace.Top > 0)) then
    begin
      Left := Preferences.Replace.Left;
      Top := Preferences.Replace.Top;
    end;
  end;

  WantedExecute := False;
  WantedNodeExpand := nil;
  FErrorMessages.Visible := not SearchOnly;
  FTables.Visible := SearchOnly;
  FFFindTextChange(Sender);

  SetLength(Clients, Accounts.Count);

  for I := 0 to Accounts.Count - 1 do
  begin
    if (Assigned(Client) and (Accounts[I] = Client.Account)) then
      Clients[I] := Client
    else
      Clients[I] := nil;

    Node := FSelect.Items.Add(nil, Accounts[I].Name);
    Node.ImageIndex := Accounts[I].ImageIndex;
    if (Node.ImageIndex < 0) then Node.ImageIndex := iiServer;
    Node.HasChildren := True;
  end;

  if (Assigned(Client)) then
  begin
    SelectedNodes := TList.Create();
    DatabaseNames := TStringList.Create();
    TableNames := TStringList.Create();
    FieldNames := TStringList.Create();

    DatabaseNames.Text := ReplaceStr(DatabaseName, ',', #13#10);
    TableNames.Text := ReplaceStr(TableName, ',', #13#10);
    FieldNames.Text := ReplaceStr(FieldName, ',', #13#10);

    AccountNode := FSelect.TopItem;
    while (Assigned(AccountNode)) do
    begin
      if (AccountNode.Text = Client.Account.Name) then
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
                if ((TableNames.IndexOf(TableNode.Text) < 0) or (FieldNames.Count = 0)) then
                  TableNode.Selected := TableNames.IndexOf(TableNode.Text) >= 0
                else
                begin
                  TableNode.Expand(False);
                  FieldNode := TableNode.getFirstChild();
                  while (Assigned(FieldNode)) do
                  begin
                    if (FieldNames.IndexOf(FieldNode.Text) >= 0) then
                      SelectedNodes.Add(FieldNode);
                    FieldNode := FieldNode.getNextSibling();
                  end;
                end;
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
      FSelect.Selected := SelectedNodes[0];
      if (Assigned(FSelect.Selected) and FSelect.AutoExpand) then
        FSelect.Selected.Expand(False);
    end
    else if (SelectedNodes.Count > 1) then
      FSelect.Select(SelectedNodes);

    SelectedNodes.Free();
    DatabaseNames.Free();
    TableNames.Free();
    FieldNames.Free();
  end;

  FFFindText.Text := '';

  FRFindText.Text := '';
  FReplaceText.Text := '';

  PageControl.ActivePage := TSSelect;

  FBForward.Cursor := crDefault;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  ActiveControl := FSelect;
end;

procedure TDSearch.FRFindTextChange(Sender: TObject);
begin
  FBForward.Enabled := (FRFindText.Text <> '') and (FRFindText.Text <> FReplaceText.Text) and not SQLWait;
end;

procedure TDSearch.FRRegExprClick(Sender: TObject);
begin
  FRWholeValue.Enabled := not FRRegExpr.Checked;
end;

procedure TDSearch.FRRegExprKeyPress(Sender: TObject; var Key: Char);
begin
  FRRegExprClick(Sender);
end;

procedure TDSearch.FSelectChange(Sender: TObject; Node: TTreeNode);
begin
  if ((ModalResult = mrNone) and Assigned(Node)) then
    FSelect.MultiSelect := Assigned(Node.Parent);

  FBForward.Enabled := Assigned(FSelect.Selected);
end;

procedure TDSearch.FSelectDblClick(Sender: TObject);
begin
  if (FBForward.Enabled) then
    FBForward.Click();
end;

procedure TDSearch.FSelectExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Client: TCClient;
  Database: TCDatabase;
  I: Integer;
  NewNode: TTreeNode;
  Table: TCBaseTable;
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
              if (Client.Initialize()) then
                WantedNodeExpand := Node
              else
              begin
                for I := 0 to Client.Databases.Count - 1 do
                  if (not (Client.Databases[I] is TCSystemDatabase)) then
                  begin
                    NewNode := TreeView.Items.AddChild(Node, Client.Databases[I].Name);
                    NewNode.ImageIndex := iiDatabase;
                    NewNode.HasChildren := True;
                  end;
                Node.HasChildren := Assigned(Node.getFirstChild());
              end;
          end;
        iiDatabase:
          begin
            Client := GetClient(Node.Parent.Index);
            Database := Client.DatabaseByName(Node.Text);
            if (Database.Initialize(Database.Tables)) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
                if ((Database.Tables[I] is TCBaseTable) and Assigned(TCBaseTable(Database.Tables[I]).Engine) and not TCBaseTable(Database.Tables[I]).Engine.IsMerge and (RightStr(Database.Tables[I].Name, Length(BackupExtension)) <> BackupExtension)) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                  NewNode.ImageIndex := iiBaseTable;
                  NewNode.HasChildren := True;
                end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
        iiBaseTable:
          begin
            Client := GetClient(Node.Parent.Parent.Index);
            Database := Client.DatabaseByName(Node.Parent.Text);
            Table := Database.BaseTableByName(Node.Text);
            if (Table.Initialize()) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Table.Fields.Count - 1 do
              begin
                NewNode := TreeView.Items.AddChild(Node, Table.Fields[I].Name);
                NewNode.ImageIndex := iiField;
              end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
      end;
    end;
end;

procedure TDSearch.FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDSearch.FTablesDblClick(Sender: TObject);
var
  Result: Boolean;
  URI: TUURI;
  ViewFrame: TFClient;
begin
  if (Assigned(FTables.Selected)) then
  begin
    URI := TUURI.Create('');

    URI.Scheme := 'mysql';
    URI.Host := Tables[FTables.Selected.Index].Account.Connection.Host;
    URI.Port := Tables[FTables.Selected.Index].Account.Connection.Port;
    URI.Database := Tables[FTables.Selected.Index].DatabaseName;
    URI.Table := Tables[FTables.Selected.Index].TableName;
    URI.Param['view'] := 'browser';

    if (Assigned(Frame)) then
      ViewFrame := Frame
    else
      ViewFrame := TFClient(Tables[FTables.Selected.Index].Account.Frame());

    Result := True;
    if (Assigned(ViewFrame)) then
      ViewFrame.Address := URI.Address
    else
      Result := Boolean(SendMessage(Application.MainForm.Handle, CM_ADDTAB, 0, LPARAM(URI.Address)));

    URI.Free();

    if (Result) then
      FBCancel.Click();
  end;
end;

function TDSearch.GetClient(const Index: Integer): TCClient;
var
  B: Boolean;
begin
  if (not Assigned(Clients[Index])) then
    Clients[Index] := fClient.Clients.CreateClient(Accounts[Index], B);

  Result := Clients[Index];

  if (Assigned(Result)) then
    Result.RegisterEventProc(FormClientEvent);
end;

procedure TDSearch.mTCopyClick(Sender: TObject);
var
  ClipboardData: HGLOBAL;
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to FTables.Items.Count - 1 do
    if ((FTables.SelCount = 0) or FTables.Items[I].Selected) then
      S := S + FTables.Items[I].Caption + #13#10;

  if ((S <> '') and OpenClipboard(Handle)) then
  begin
    EmptyClipboard();

    ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
    StrPCopy(GlobalLock(ClipboardData), S);
    SetClipboardData(CF_UNICODETEXT, ClipboardData);
    GlobalUnlock(ClipboardData);

    CloseClipboard();
  end;
end;

procedure TDSearch.OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
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

procedure TDSearch.TSExecuteShow(Sender: TObject);
var
  B: Boolean;
  Client: TCClient;
  Database: TCDatabase;
  I: Integer;
  J: Integer;
  K: Integer;
  Node: TTreeNode;
  Objects: TList;
  ProgressInfos: TTools.TProgressInfos;
  Table: TCBaseTable;
begin
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();
  FTables.Items.BeginUpdate();
  FTables.Items.Clear();
  FTables.Items.EndUpdate();

  ProgressInfos.TablesDone := 0;
  ProgressInfos.TablesSum := 0;
  ProgressInfos.RecordsDone := 0;
  ProgressInfos.RecordsSum := 0;
  ProgressInfos.TimeDone := 0;
  ProgressInfos.TimeSum := 0;
  ProgressInfos.Progress := 0;
  SendMessage(Self.Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));

  FBForward.Enabled := False;
  FBForward.Default := False;
  FBCancel.Default := True;


  Node := FSelect.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  Client := GetClient(Node.Index);

  Objects := TList.Create();
  case (FSelect.Selected.ImageIndex) of
    iiServer:
      begin
        WantedExecute := Client.Initialize();
        if (not WantedExecute) then
          for I := 0 to Client.Databases.Count - 1 do
            if (not WantedExecute and not (Client.Databases[I] is TCSystemDatabase)) then
            begin
              Database := Client.Databases[I];
              WantedExecute := Database.Initialize(Database.Tables);
              if (not WantedExecute) then
              begin
                for J := 0 to Database.Tables.Count - 1 do
                  if (Database.Tables[J] is TCBaseTable) then
                    Objects.Add(Database.Tables[J]);
                WantedExecute := Database.InitializeSources(Objects);
              end;
            end;
      end;
    iiDatabase:
      begin
        Database := Client.DatabaseByName(FSelect.Selected.Text);
        WantedExecute := Database.Initialize(Database.Tables);
        if (not WantedExecute) then
        begin
          for J := 0 to Database.Tables.Count - 1 do
            if (Database.Tables[J] is TCBaseTable) then
              Objects.Add(Database.Tables[J]);
          WantedExecute := Database.InitializeSources(Objects);
        end;
      end;
    iiBaseTable:
      begin
        Database := Client.DatabaseByName(FSelect.Selected.Parent.Text);
        for J := 0 to Database.Tables.Count - 1 do
          if (FSelect.Selected.Parent.Item[J].Selected) then
            Objects.Add(Database.Tables[J]);
        WantedExecute := Database.InitializeSources(Objects);
      end;
  end;
  Objects.Free();

  if (not WantedExecute) then
  begin
    Preferences.Find.FindTextMRU.Add(Trim(FFFindText.Text));

    Preferences.Find.Options := [];
    if (FFMatchCase.Checked) then
      Include(Preferences.Find.Options, foMatchCase);
    if (FFWholeValue.Checked) then
      Include(Preferences.Find.Options, foWholeValue);
    if (FFRegExpr.Checked) then
      Include(Preferences.Find.Options, foRegExpr);

    Preferences.Replace.FindTextMRU.Add(Trim(FRFindText.Text));
    Preferences.Replace.ReplaceTextMRU.Add(Trim(FReplaceText.Text));

    Preferences.Replace.Options := [];
    if (FRMatchCase.Checked) then
      Include(Preferences.Replace.Options, roMatchCase);
    if (FRWholeValue.Checked) then
      Include(Preferences.Replace.Options, roWholeValue);
    if (FRRegExpr.Checked) then
      Include(Preferences.Replace.Options, roRegExpr);

    Preferences.Replace.Backup := FBackup.Checked;

    ExecuteClient := Client;

    if (SearchOnly) then
    begin
      SetLength(Tables, 0);

      Find := TTFind.Create(ExecuteClient);

      Find.Wnd := Self.Handle;
      Find.UpdateMessage := CM_UPDATEPROGRESSINFO;
      Find.ExecutedMessage := CM_EXECUTIONDONE;
      Find.FindText := FFFindText.Text;
      Find.MatchCase := FFMatchCase.Checked;
      Find.WholeValue := FFWholeValue.Checked;
      Find.RegExpr := FFRegExpr.Checked;
    end
    else
    begin
      ReplaceClient := fClient.Clients.CreateClient(ExecuteClient.Account, B);

      if (Assigned(ReplaceClient)) then
      begin
        Find := TTReplace.Create(ExecuteClient, ReplaceClient);

        TTReplace(Find).Wnd := Self.Handle;
        TTReplace(Find).UpdateMessage := CM_UPDATEPROGRESSINFO;
        TTReplace(Find).ExecutedMessage := CM_EXECUTIONDONE;
        TTReplace(Find).OnError := OnError;
        TTReplace(Find).FindText := FRFindText.Text;
        TTReplace(Find).ReplaceText := FReplaceText.Text;
        TTReplace(Find).MatchCase := FRMatchCase.Checked;
        TTReplace(Find).WholeValue := FRWholeValue.Checked;
        TTReplace(Find).RegExpr := FRRegExpr.Checked;
        TTReplace(Find).Backup := FBackup.Checked;
      end;
    end;

    for I := 0 to FSelect.Items.Count - 1 do
      if (FSelect.Items[I].Selected) then
      begin
        if (FSelect.Selected.ImageIndex = iiField) then
        begin
          Database := ExecuteClient.DatabaseByName(FSelect.Items[I].Parent.Parent.Text);
          Table := Database.BaseTableByName(FSelect.Items[I].Parent.Text);
          Find.Add(Table, Table.FieldByName(FSelect.Items[I].Text));
        end
        else if (FSelect.Items[I].ImageIndex = iiBaseTable) then
        begin
          Database := ExecuteClient.DatabaseByName(FSelect.Items[I].Parent.Text);
          Find.Add(Database.BaseTableByName(FSelect.Items[I].Text), nil);
        end
        else if (FSelect.Items[I].ImageIndex = iiDatabase) then
        begin
          Database := ExecuteClient.DatabaseByName(FSelect.Items[I].Text);
          for J := 0 to Database.Tables.Count - 1 do
            if ((Database.Tables[J] is TCBaseTable)  and (RightStr(Database.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
              Find.Add(Database.Tables.BaseTable[J], nil);
        end
        else // iiConnection
        begin
          for K := 0 to ExecuteClient.Databases.Count - 1 do
          begin
            Database := ExecuteClient.Databases[K];
            if (not (Database is TCSystemDatabase)) then
              for J := 0 to Database.Tables.Count - 1 do
                if ((Database.Tables[J] is TCBaseTable)  and (RightStr(Database.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
                  Find.Add(Database.Tables.BaseTable[J], nil);
          end;
        end;
      end;

    if (ExecuteClient.Asynchron) then
      Find.Start()
    else
      Find.Execute();
  end;
end;

procedure TDSearch.TSFOptionsShow(Sender: TObject);
begin
  FFFindTextChange(Sender);

  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(230);
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  ActiveControl := FFFindText;
end;

procedure TDSearch.TSROptionsShow(Sender: TObject);
begin
  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(230);
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  FRFindTextChange(Sender);

  ActiveControl := FRFindText;
end;

procedure TDSearch.TSSelectShow(Sender: TObject);
begin
  FBBack.Enabled := False;
  FBForward.Caption := Preferences.LoadStr(229) + ' >';
  FBForward.Enabled := True;
end;

initialization
  FSearch := nil;
end.

