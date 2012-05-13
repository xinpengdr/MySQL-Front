unit fDSearch;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls, DB, ExtCtrls,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext,
  fBase, fClient, MySQLDB, fSession, fTools, fFClient;

type
  TDSTableItem = record
    Session: TSSession;
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
    procedure FSelectMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FSelectMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FTablesDblClick(Sender: TObject);
    procedure mTCopyClick(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFOptionsShow(Sender: TObject);
    procedure TSROptionsShow(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
  private
    Connections: array of TCClient;
    ExecuteClient: TCClient;
    Find: TTFind;
    MouseDownNode: TTreeNode;
    Replace: TTReplace;
    ReplaceClient: TCClient;
    Tables: array of TDSTableItem;
    procedure CMExecutenDoneF(var Message: TMessage);
    procedure CMExecutenDoneR(var Message: TMessage);
    procedure OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
  protected
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
  if (SearchOnly) then
    CMExecutenDoneF(Message)
  else
    CMExecutenDoneR(Message);
end;

procedure TDSearch.CMExecutenDoneF(var Message: TMessage);
begin
  if (FTables.Items.Count = 0) then
    MsgBox(Preferences.LoadStr(533, Find.FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

  FreeAndNil(Find);

  if (Assigned(ExecuteClient)) then
    ExecuteClient := nil;

  ModalResult := mrNone;

  FBCancel.Caption := Preferences.LoadStr(231);
  FBCancel.ModalResult := mrOk;
end;

procedure TDSearch.CMExecutenDoneR(var Message: TMessage);
begin
  FreeAndNil(Replace);

  if (Assigned(ReplaceClient)) then
  begin
    fClient.Clients.ReleaseClient(ReplaceClient);
    ReplaceClient := nil;
  end;
  if (Assigned(ExecuteClient)) then
  begin
    ExecuteClient := nil;
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
      if ((Tables[I].Session = Client.Session) and (Tables[I].DatabaseName = CurrentItem^.DatabaseName) and (Tables[I].TableName = CurrentItem^.TableName)) then
        Found := True;

    if (not Found) then
    begin
      SetLength(Tables, Length(Tables) + 1);

      Tables[Length(Tables) - 1].Session := Client.Session;
      Tables[Length(Tables) - 1].DatabaseName := CurrentItem^.DatabaseName;
      Tables[Length(Tables) - 1].TableName := CurrentItem^.TableName;

      Item := FTables.Items.Add();
      if (not (FSelect.Selected.ImageIndex in [iiDatabase, iiBaseTable, iiField])) then
        Item.Caption := Item.Caption + Client.Session.Name + '.';
      Item.Caption := Item.Caption + CurrentItem^.DatabaseName + '.';
      Item.Caption := Item.Caption + CurrentItem^.TableName + ' (' + IntToStr(CurrentItem^.RecordsFound) + ')';
    end;
  end;

  if (Assigned(Find) and Find.Suspended or Assigned(Replace) and Replace.Suspended) then
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
  if (Assigned(Replace)) then
  begin
    SetEvent(Replace.UserAbort);
    if (not Replace.Suspended) then
      Replace.WaitFor();
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
  FBForward.Enabled := (FFFindText.Text <> '') and True;
end;

procedure TDSearch.FFRegExprClick(Sender: TObject);
begin
  FFWholeValue.Enabled := not FFRegExpr.Checked;
end;

procedure TDSearch.FFRegExprKeyPress(Sender: TObject; var Key: Char);
begin
  FFRegExprClick(Sender);
end;

procedure TDSearch.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  Find := nil;
  Replace := nil;

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
  FSelect.Items.Clear();

  for I := 0 to Length(Connections) - 1 do
    if (Assigned(Connections[I]) and (Connections[I] <> Client)) then
      fClient.Clients.ReleaseClient(Connections[I]);
  SetLength(Connections, 0);

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
  SessionNode: TTreeNode;
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

  FSelect.Items.Clear();
  FErrorMessages.Visible := not SearchOnly;
  FTables.Visible := SearchOnly;
  FFFindTextChange(Sender);

  SetLength(Connections, Sessions.Count);

  for I := 0 to Sessions.Count - 1 do
  begin
    if (Assigned(Client) and (Sessions[I] = Client.Session)) then
      Connections[I] := Client
    else
      Connections[I] := nil;

    Node := FSelect.Items.Add(nil, Sessions[I].Name);
    Node.ImageIndex := Sessions[I].ImageIndex;
    if (Node.ImageIndex < 0) then Node.ImageIndex := iiServer;
    Node.SelectedIndex := Node.ImageIndex;
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

    SessionNode := FSelect.TopItem;
    while (Assigned(SessionNode)) do
    begin
      if (SessionNode.Text = Client.Session.Name) then
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
      SessionNode := SessionNode.getNextSibling();
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

  ActiveControl := FBCancel;
  if (Assigned(Client)) then
    ActiveControl := FSelect
  else
    ActiveControl := FBCancel;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;
end;

procedure TDSearch.FRFindTextChange(Sender: TObject);
begin
  FBForward.Enabled := (FRFindText.Text <> '') and (FRFindText.Text <> FReplaceText.Text) and True;
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
var
  AllowExpansion: Boolean;
begin
  if ((ModalResult = mrNone) and Assigned(Node)) then
    FSelect.MultiSelect := Assigned(Node.Parent);

  if (Assigned(Node) and Node.Selected and not Node.Expanded and not Assigned(Node.Parent)) then
  begin
    AllowExpansion := True;
    FSelectExpanding(Sender, Node, AllowExpansion);
  end;

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
  B: Boolean;
  //  Table: TCBaseTable;
  Client: TCClient;
  Database: TCDatabase;
  I: Integer;
  Index: Integer;
  NewNode: TTreeNode;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (not Assigned(TreeView.Selected)) then
    TreeView.Selected := Node;

  if (Assigned(TreeView.Selected)) then
  begin
    if (not Assigned(Node.Parent)) then
    begin
      Index := Sessions.IndexOf(Sessions.SessionByName(Node.Text));
      if (not Assigned(Connections[Index])) then
        Connections[Index] := fClient.Clients.CreateClient(Sessions.SessionByName(Node.Text), B);

      Client := Connections[Index];
    end
    else
      if (not Assigned(Node.Parent.Parent)) then
        Client := Connections[Sessions.IndexOf(Sessions.SessionByName(Node.Parent.Text))]
      else
        Client := Connections[Sessions.IndexOf(Sessions.SessionByName(Node.Parent.Parent.Text))];

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
                NewNode.HasChildren := True;
              end;
        end
        else if (Node.ImageIndex = iiDatabase) then
        begin
          Database := Client.DatabaseByName(Node.Text);
            for I := 0 to Database.Tables.Count - 1 do
              if ((Database.Tables[I] is TCBaseTable) and Assigned(TCBaseTable(Database.Tables[I]).Engine) and not TCBaseTable(Database.Tables[I]).Engine.IsMerge and (RightStr(Database.Tables[I].Name, Length(BackupExtension)) <> BackupExtension)) then
              begin
                NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                NewNode.ImageIndex := iiBaseTable; NewNode.SelectedIndex := NewNode.ImageIndex;
                NewNode.HasChildren := True;
              end;
        end
        else if (Node.ImageIndex = iiBaseTable) then
        begin
// ToDo
//          Database := Client.DatabaseByName(Node.Parent.Text);
//          Table := Database.BaseTableByName(Node.Text);
//          if (Database.Initialize()) then
//            for I := 0 to Table.Fields.Count - 1 do
//            begin
//              NewNode := TreeView.Items.AddChild(Node, Table.Fields[I].Name);
//              NewNode.ImageIndex := iiField; NewNode.SelectedIndex := NewNode.ImageIndex;
//            end;
        end;
      Node.HasChildren := Assigned(Node.getFirstChild());
    end;
  end;
end;

procedure TDSearch.FSelectMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TTreeView_Ext) then
    MouseDownNode := TTreeView_Ext(Sender).GetNodeAt(X, Y)
  else
    MouseDownNode := nil;
end;

procedure TDSearch.FSelectMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDownNode := nil;
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
    URI.Host := Tables[FTables.Selected.Index].Session.Connection.Host;
    URI.Port := Tables[FTables.Selected.Index].Session.Connection.Port;
    URI.Database := Tables[FTables.Selected.Index].DatabaseName;
    URI.Table := Tables[FTables.Selected.Index].TableName;
    URI.Param['view'] := 'browser';

    if (Assigned(Frame)) then
      ViewFrame := Frame
    else
      ViewFrame := TFClient(Tables[FTables.Selected.Index].Session.Frame());

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
  Database: TCDatabase;
  I: Integer;
  J: Integer;
  K: Integer;
  ProgressInfos: TTools.TProgressInfos;
  Table: TCBaseTable;
begin
  FBForward.Enabled := False;
  FBForward.Default := False;
  FBCancel.Default := True;


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


  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();
  FTables.Items.Clear();

  ProgressInfos.TablesDone := 0;
  ProgressInfos.TablesSum := 0;
  ProgressInfos.RecordsDone := 0;
  ProgressInfos.RecordsSum := 0;
  ProgressInfos.TimeDone := 0;
  ProgressInfos.TimeSum := 0;
  ProgressInfos.Progress := 0;
  SendMessage(Self.Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));

  ExecuteClient := nil;
  I := 0;
  while (not Assigned(ExecuteClient) and (I < FSelect.Items.Count)) do
  begin
    if (FSelect.Items[I].Selected) then
      if (FSelect.Selected.ImageIndex = iiField) then
        ExecuteClient := Connections[Sessions.IndexOf(Sessions.SessionByName(FSelect.Items[I].Parent.Parent.Parent.Text))]
      else if (FSelect.Items[I].ImageIndex = iiBaseTable) then
        ExecuteClient := Connections[Sessions.IndexOf(Sessions.SessionByName(FSelect.Items[I].Parent.Parent.Text))]
      else if (FSelect.Items[I].ImageIndex = iiDatabase) then
        ExecuteClient := Connections[Sessions.IndexOf(Sessions.SessionByName(FSelect.Items[I].Parent.Text))]
      else // iiConnection
        ExecuteClient := Connections[Sessions.IndexOf(Sessions.SessionByName(FSelect.Items[I].Text))];
    Inc(I);
  end;

  if (Assigned(ExecuteClient)) then
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
    end
    else
    begin
      ReplaceClient := fClient.Clients.CreateClient(ExecuteClient.Session, B);

      if (Assigned(ReplaceClient)) then
      begin
        Replace := TTReplace.Create(ExecuteClient, ReplaceClient);

        Replace.Wnd := Self.Handle;
        Replace.UpdateMessage := CM_UPDATEPROGRESSINFO;
        Replace.ExecutedMessage := CM_EXECUTIONDONE;
        Replace.OnError := OnError;
        Replace.FindText := FRFindText.Text;
        Replace.ReplaceText := FReplaceText.Text;
        Replace.MatchCase := FRMatchCase.Checked;
        Replace.WholeValue := FRWholeValue.Checked;
        Replace.RegExpr := FRRegExpr.Checked;
        Replace.Backup := FBackup.Checked;

        if (FSelect.Selected.ImageIndex = iiField) then
        begin
          Database := ExecuteClient.DatabaseByName(FSelect.Selected.Parent.Parent.Text);
          Table := Database.BaseTableByName(FSelect.Selected.Parent.Text);
          for I := 0 to FSelect.Items.Count - 1 do
            if (FSelect.Items[I].Selected) then
              Replace.Add(Table, Table.FieldByName(FSelect.Items[I].Text));
        end
        else
        begin
          for I := 0 to FSelect.Items.Count - 1 do
            if (FSelect.Items[I].Selected) then
            begin
              if (FSelect.Items[I].ImageIndex = iiBaseTable) then
              begin
                Database := ExecuteClient.DatabaseByName(FSelect.Items[I].Parent.Text);
                Replace.Add(Database.BaseTableByName(FSelect.Items[I].Text), nil);
              end
              else if (FSelect.Items[I].ImageIndex = iiDatabase) then
              begin
                Database := ExecuteClient.DatabaseByName(FSelect.Items[I].Text);
                for J := 0 to Database.Tables.Count - 1 do
                  if ((Database.Tables[J] is TCBaseTable)  and (RightStr(Database.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
                    Replace.Add(Database.Tables.BaseTable[J], nil);
              end
              else // iiConnection
              begin
                for K := 0 to ExecuteClient.Databases.Count - 1 do
                begin
                  Database := ExecuteClient.Databases.Database[K];
                  for J := 0 to Database.Tables.Count - 1 do
                    if ((Database.Tables[J] is TCBaseTable)  and (RightStr(Database.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
                      Replace.Add(Database.Tables.BaseTable[J], nil);
                end;
              end;
            end;
        end;

        if (ExecuteClient.Asynchron) then
          Replace.Start()
        else
          Replace.Execute();
      end;
    end;
end;

procedure TDSearch.TSFOptionsShow(Sender: TObject);
begin
  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(230);
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  FFFindTextChange(Sender);

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

