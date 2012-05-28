unit fDExport;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB, ShDocVw, DBGrids,
  ODBCAPI,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext, Dialogs_Ext,
  fClient, fAccount, fBase, fTools, fCWorkbench;

type
  TExportType = (etSQLFile, etTextFile, etExcelFile, etAccessFile, etSQLiteFile, etODBC, etHTMLFile, etXMLFile, etPrint);

  TSaveToStream = procedure(Stream: TStream) of object;

  TDExport = class (TForm_Ext)
    FAllQuote: TRadioButton;
    FBBack: TButton;
    FBCancel: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FCreateDatabase: TCheckBox;
    FCSVHeadline: TCheckBox;
    FDatabaseAttribute: TEdit;
    FDatabaseTag: TEdit;
    FDatabaseTagDisabled: TRadioButton;
    FDatabaseTagFree: TRadioButton;
    FDatabaseTagName: TRadioButton;
    FDestField1: TEdit;
    FDisableKeys: TCheckBox;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FDrop: TCheckBox;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FField1: TComboBox_Ext;
    FField2: TComboBox_Ext;
    FFieldAttribute: TEdit;
    FFieldTag: TEdit;
    FFieldTagFree: TRadioButton;
    FFieldTagName: TRadioButton;
    FHTMLData: TCheckBox;
    FHTMLIndexBGColorEnabled: TCheckBox;
    FHTMLNullText: TCheckBox;
    FHTMLRowBGColorEnabled: TCheckBox;
    FHTMLShowMemoContent: TCheckBox;
    FHTMLStructure: TCheckBox;
    FL1DatabaseTagFree: TLabel;
    FL1FieldTagFree: TLabel;
    FL1TableTagFree: TLabel;
    FL2DatabaseTagFree: TLabel;
    FL2FieldTagFree: TLabel;
    FL2RecordTag: TLabel;
    FL2RootTag: TLabel;
    FL2TableTagFree: TLabel;
    FL3RecordTag: TLabel;
    FL3RootTag: TLabel;
    FLCSVHeadline: TLabel;
    FLDatabaseHandling: TLabel;
    FLDatabaseTag: TLabel;
    FLDestFields: TLabel;
    FLEntiered: TLabel;
    FLDrop: TLabel;
    FLDone: TLabel;
    FLErrors: TLabel;
    FLFields: TLabel;
    FLFieldTag: TLabel;
    FLGeneral: TLabel;
    FLHTMLBGColorEnabled: TLabel;
    FLHTMLNullValues: TLabel;
    FLHTMLViewDatas: TLabel;
    FLHTMLWhat: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLQuoteChar: TLabel;
    FLQuoteValues: TLabel;
    FLRecordTag: TLabel;
    FLReferrer1: TLabel;
    FLRootTag: TLabel;
    FLSeparator: TLabel;
    FLSQLWhat: TLabel;
    FLTableTag: TLabel;
    FNoQuote: TRadioButton;
    FODBCSelect: TListView_Ext;
    FProgressBar: TProgressBar;
    FQuoteChar: TEdit;
    FRecordTag: TEdit;
    FReplaceData: TCheckBox;
    FRootTag: TEdit;
    FSeparator: TEdit;
    FSeparatorChar: TRadioButton;
    FSeparatorTab: TRadioButton;
    FSQLData: TCheckBox;
    FSQLStructure: TCheckBox;
    FStringQuote: TRadioButton;
    FTableAttribute: TEdit;
    FTableTag: TEdit;
    FTableTagDisabled: TRadioButton;
    FTableTagFree: TRadioButton;
    FTableTagName: TRadioButton;
    FUseDatabase: TCheckBox;
    GCSVOptions: TGroupBox_Ext;
    GErrors: TGroupBox_Ext;
    GFields: TGroupBox_Ext;
    GHTMLOptions: TGroupBox_Ext;
    GHTMLWhat: TGroupBox_Ext;
    GODBCSelect: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GSQLOptions: TGroupBox_Ext;
    GSQLWhat: TGroupBox_Ext;
    GXMLHow: TGroupBox_Ext;
    PageControl: TPageControl;
    PDatabaseTag: TPanel_Ext;
    PErrorMessages: TPanel_Ext;
    PFieldTag: TPanel_Ext;
    PODBCSelect: TPanel_Ext;
    PQuote: TPanel_Ext;
    PSeparator: TPanel_Ext;
    PTableTag: TPanel_Ext;
    ScrollBox: TScrollBox;
    TSCSVOptions: TTabSheet;
    TSExecute: TTabSheet;
    TSFields: TTabSheet;
    TSHTMLOptions: TTabSheet;
    TSODBCSelect: TTabSheet;
    TSSQLOptions: TTabSheet;
    TSXMLOptions: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FDatabaseDblClick(Sender: TObject);
    procedure FDatabaseTagClick(Sender: TObject);
    procedure FDatabaseTagKeyPress(Sender: TObject; var Key: Char);
    procedure FDestField1Change(Sender: TObject);
    procedure FField1Change(Sender: TObject);
    procedure FField1Exit(Sender: TObject);
    procedure FFieldTagClick(Sender: TObject);
    procedure FFieldTagKeyPress(Sender: TObject; var Key: Char);
    procedure FHTMLDataClick(Sender: TObject);
    procedure FHTMLDataKeyPress(Sender: TObject; var Key: Char);
    procedure FHTMLStructureClick(Sender: TObject);
    procedure FHTMLStructureKeyPress(Sender: TObject; var Key: Char);
    procedure FODBCSelectChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FODBCSelectDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FQuoteCharExit(Sender: TObject);
    procedure FQuoteClick(Sender: TObject);
    procedure FQuoteKeyPress(Sender: TObject; var Key: Char);
    procedure FSeparatorClick(Sender: TObject);
    procedure FSeparatorKeyPress(Sender: TObject; var Key: Char);
    procedure FSQLOptionClick(Sender: TObject);
    procedure FSQLOptionKeyPress(Sender: TObject; var Key: Char);
    procedure FTableTagClick(Sender: TObject);
    procedure FTableTagKeyPress(Sender: TObject; var Key: Char);
    procedure TSCSVOptionsShow(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFieldsShow(Sender: TObject);
    procedure TSHTMLOptionsShow(Sender: TObject);
    procedure TSODBCSelectShow(Sender: TObject);
    procedure TSOptionsHide(Sender: TObject);
    procedure TSSQLOptionsShow(Sender: TObject);
    procedure TSXMLOptionChange(Sender: TObject);
    procedure TSXMLOptionsHide(Sender: TObject);
    procedure TSXMLOptionsShow(Sender: TObject);
  private
    Export: TTExport;
    FDBObjects: TList;
    FDestFields: array of TEdit;
    FFields: array of TComboBox_Ext;
    FLReferrers: array of TLabel;
    ODBC: SQLHDBC;
    ODBCEnv: SQLHENV;
    SQLWait: Boolean;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure ClearTSFields(Sender: TObject);
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure InitializeDBObjects();
    procedure InitTSFields(Sender: TObject);
    procedure OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutionDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    Client: TCClient;
    CodePage: Cardinal;
    DBGrid: TDBGrid;
    ExportType: TExportType;
    Filename: TFileName;
    Window: TForm;
    function Execute(): Boolean;
    property DBObjects: TList read FDBObjects;
  end;

function DExport(): TDExport;

implementation {***************************************************************}

{$R *.dfm}

uses
  Registry, Math, StrUtils, RichEdit, DBCommon,
  MySQLDB, SQLUtils,
  fDLogin,
  fPreferences;

var
  FExport: TDExport;

function DExport(): TDExport;
begin
  if (not Assigned(FExport)) then
  begin
    Application.CreateForm(TDExport, FExport);
    FExport.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FExport;
end;

function DBObjectsSortItem(Item1, Item2: Pointer): Integer;
var
  Client: TCClient;
  Index1: Integer;
  Index2: Integer;
begin
  Client := TCDBObject(Item1).Database.Client;

  if (Client.LowerCaseTableNames = 0) then
    Result := Sign(lstrcmp(PChar(TCDBObject(Item1).Database.Name), PChar(TCDBObject(Item2).Database.Name)))
  else
    Result := Sign(lstrcmpi(PChar(TCDBObject(Item1).Database.Name), PChar(TCDBObject(Item2).Database.Name)));

  if (Result = 0) then
  begin
    if ((TCDBObject(Item1) is TCBaseTable) and not TCBaseTable(Item1).Engine.IsMerge) then
      Index1 := 0
    else if (TCDBObject(Item1) is TCBaseTable) then
      Index1 := 1
    else if (TCDBObject(Item1) is TCView) then
      Index1 := 2
    else if (TCDBObject(Item1) is TCTrigger) then
      Index1 := 3
    else if (TCDBObject(Item1) is TCRoutine) then
      Index1 := 4
    else if (TCDBObject(Item1) is TCEvent) then
      Index1 := 5
    else
      Index1 := 6;
    if ((TCDBObject(Item2) is TCBaseTable) and not TCBaseTable(Item2).Engine.IsMerge) then
      Index2 := 0
    else if (TCDBObject(Item2) is TCBaseTable) then
      Index2 := 1
    else if (TCDBObject(Item2) is TCView) then
      Index2 := 2
    else if (TCDBObject(Item2) is TCTrigger) then
      Index2 := 3
    else if (TCDBObject(Item2) is TCRoutine) then
      Index2 := 4
    else if (TCDBObject(Item2) is TCEvent) then
      Index2 := 5
    else
      Index2 := 6;
    Result := Sign(Index1 - Index2);
  end;

  if (Result = 0) then
    if ((TCDBObject(Item1) is TCTable) and (Client.LowerCaseTableNames = 0)) then
      Result := lstrcmp(PChar(TCDBObject(Item1).Name), PChar(TCDBObject(Item2).Name))
    else
      Result := lstrcmpi(PChar(TCDBObject(Item1).Name), PChar(TCDBObject(Item2).Name));
end;

{ TDExport ********************************************************************}

procedure TDExport.CheckActivePageChange(const ActivePageIndex: Integer);
var
  I: Integer;
  NextActivePageIndex: Integer;
begin
  FBBack.Enabled := False;
  for I := ActivePageIndex - 1 downto 0 do
    FBBack.Enabled := FBBack.Enabled or PageControl.Pages[I].Enabled;

  NextActivePageIndex := -1;
  for I := PageControl.PageCount - 1 downto ActivePageIndex + 1 do
    if (PageControl.Pages[I].Enabled) then
      NextActivePageIndex := I;

  if (NextActivePageIndex >= 0) then
    for I := NextActivePageIndex + 1 to PageControl.PageCount - 1 do
      PageControl.Pages[I].Enabled := False;

  if (NextActivePageIndex > 0) then
    if (NextActivePageIndex < TSExecute.PageIndex) then
      FBForward.Caption := Preferences.LoadStr(229) + ' >'
    else
      FBForward.Caption := Preferences.LoadStr(230);

  FBForward.Enabled := FBForward.Visible and (NextActivePageIndex >= 0) and ((NextActivePageIndex < TSExecute.PageIndex) or not SQLWait);
  FBForward.Default := True;

  if (not FBForward.Enabled and SQLWait) then
    FBForward.Cursor := crSQLWait
  else
    FBForward.Cursor := crDefault;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Default := False;

  if (not Assigned(ActiveControl) and FBForward.Enabled) then
    ActiveControl := FBForward;
end;

procedure TDExport.ClearTSFields(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FFields) - 1 do FFields[I].Free();
  for I := 0 to Length(FLReferrers) - 1 do FLReferrers[I].Free();
  for I := 0 to Length(FDestFields) - 1 do FDestFields[I].Free();
  SetLength(FFields, 0);
  SetLength(FLReferrers, 0);
  SetLength(FDestFields, 0);
end;

procedure TDExport.CMChangePreferences(var Message: TMessage);
begin
  GODBCSelect.Caption := Preferences.LoadStr(265) + ':';

  GSQLWhat.Caption := Preferences.LoadStr(227);
  FLSQLWhat.Caption := Preferences.LoadStr(218) + ':';
  FSQLStructure.Caption := Preferences.LoadStr(215);
  FSQLData.Caption := Preferences.LoadStr(216);

  GSQLOptions.Caption := Preferences.LoadStr(238);
  FLGeneral.Caption := Preferences.LoadStr(108) + ':';
  FLDrop.Caption := Preferences.LoadStr(242) + ':';
  FDrop.Caption := Preferences.LoadStr(243);
  FReplaceData.Caption := LowerCase(ReplaceStr(Preferences.LoadStr(416), '&', ''));
  FLDatabaseHandling.Caption := ReplaceStr(Preferences.LoadStr(38), '&', '') + ':';
  FCreateDatabase.Caption := Preferences.LoadStr(245);
  FUseDatabase.Caption := Preferences.LoadStr(246);
  FDisableKeys.Caption := Preferences.LoadStr(621);

  GCSVOptions.Caption := Preferences.LoadStr(238);
  FLCSVHeadline.Caption := Preferences.LoadStr(393) + ':';
  FCSVHeadline.Caption := Preferences.LoadStr(408);
  FLSeparator.Caption := Preferences.LoadStr(352) + ':';
  FSeparatorTab.Caption := Preferences.LoadStr(354);
  FSeparatorChar.Caption := Preferences.LoadStr(355) + ':';
  FLQuoteValues.Caption := Preferences.LoadStr(353) + ':';
  FNoQuote.Caption := Preferences.LoadStr(359);
  FStringQuote.Caption := Preferences.LoadStr(360);
  FAllQuote.Caption := Preferences.LoadStr(361);
  FLQuoteChar.Caption := Preferences.LoadStr(356) + ':';

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GXMLHow.Caption := Preferences.LoadStr(238);
  FLRootTag.Caption := 'Root:';
  FLDatabaseTag.Caption := ReplaceStr(Preferences.LoadStr(265), '&', '') + ':';
  FDatabaseTagDisabled.Caption := Preferences.LoadStr(554);
  FLTableTag.Caption := ReplaceStr(Preferences.LoadStr(234), '&', '') + ':';
  FTableTagDisabled.Caption := Preferences.LoadStr(554);
  FLRecordTag.Caption := Preferences.LoadStr(124) + ':';
  FLFieldTag.Caption := ReplaceStr(Preferences.LoadStr(253), '&', '') + ':';

  GHTMLWhat.Caption := Preferences.LoadStr(227);
  FLHTMLWhat.Caption := Preferences.LoadStr(218) + ':';
  FHTMLData.Caption := Preferences.LoadStr(216);

  GHTMLOptions.Caption := Preferences.LoadStr(238);
  FLHTMLNullValues.Caption := Preferences.LoadStr(498) + ':';
  FHTMLNullText.Caption := Preferences.LoadStr(499);
  FLHTMLViewDatas.Caption := ReplaceStr(Preferences.LoadStr(574), '&', '') + ':';
  FHTMLShowMemoContent.Caption := Preferences.LoadStr(575);
  FLHTMLBGColorEnabled.Caption := Preferences.LoadStr(740) + ':';
  FHTMLRowBGColorEnabled.Caption := Preferences.LoadStr(600);
  FHTMLIndexBGColorEnabled.Caption := Preferences.LoadStr(458);

  GFields.Caption := Preferences.LoadStr(253);
  FLFields.Caption := Preferences.LoadStr(401) + ':';
  FLDestFields.Caption := Preferences.LoadStr(400) + ':';

  GErrors.Caption := Preferences.LoadStr(392);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDExport.CMExecutionDone(var Message: TMessage);
var
  Success: Boolean;
  vaIn: OleVariant;
  vaOut: OleVariant;
  WebBrowser: TWebBrowser;
begin
  Success := Boolean(Message.WParam);

  if (Assigned(Export)) then
    FreeAndNil(Export);

  if (ExportType = etPrint) then
  begin
    WebBrowser := TWebBrowser.Create(Self);
    WebBrowser.Navigate(Filename);

    WebBrowser.Left := 0; WebBrowser.Top := 0; WebBrowser.Width := 0; WebBrowser.Height := 0;
    WebBrowser.Visible := False;
    InsertControl(WebBrowser);
    while (WebBrowser.QueryStatusWB(OLECMDID_PRINT) and (OLECMDF_SUPPORTED + OLECMDF_ENABLED) <> OLECMDF_SUPPORTED + OLECMDF_ENABLED) do
      Application.ProcessMessages;
    if (ModalResult = mrNone) then
      WebBrowser.ControlInterface.ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
    RemoveControl(WebBrowser);
    FreeAndNil(WebBrowser);

    DeleteFile(Filename);
  end;

  FBBack.Enabled := ExportType <> etSQLiteFile;
  FBCancel.Caption := Preferences.LoadStr(231);

  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;

  ActiveControl := nil;
end;

procedure TDExport.CMPostShow(var Message: TMessage);
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount - 1 do
    if ((PageControl.ActivePageIndex < 0) and PageControl.Pages[I].Enabled) then
      PageControl.ActivePageIndex := I;
end;

procedure TDExport.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FDatabaseTag.Left := FL1DatabaseTagFree.Left + FL1DatabaseTagFree.Width;
  FDatabaseAttribute.Left := FDatabaseTag.Left + FDatabaseTag.Width + PDatabaseTag.Canvas.TextWidth('  ') + 2;
  FL2DatabaseTagFree.Left := FDatabaseAttribute.Left + FDatabaseAttribute.Width + 1;

  FTableTag.Left := FL1TableTagFree.Left + FL1TableTagFree.Width;
  FTableAttribute.Left := FTableTag.Left + FTableTag.Width + PTableTag.Canvas.TextWidth('  ') + 2;
  FL2TableTagFree.Left := FTableAttribute.Left + FTableAttribute.Width + 1;

  FFieldTag.Left := FL1FieldTagFree.Left + FL1FieldTagFree.Width;
  FFieldAttribute.Left := FFieldTag.Left + FFieldTag.Width + PFieldTag.Canvas.TextWidth('  ') + 2;
  FL2FieldTagFree.Left := FFieldAttribute.Left + FFieldAttribute.Width + 1;
end;

procedure TDExport.CMUpdateProgressInfo(var Message: TMessage);
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

  if (Assigned(Export) and Export.Suspended) then
    Application.ProcessMessages();
end;

function TDExport.Execute(): Boolean;
var
  FilenameP: array [0 .. MAX_PATH] of Char;
begin
  PageControl.ActivePageIndex := -1;

  if (ExportType <> etPrint) then
    Result := True
  else
  begin
    Result := (GetTempPath(MAX_PATH, @FilenameP) > 0) and (GetTempFileName(FilenameP, '~MF', 0, FilenameP) <> 0);
    if (Result) then
      Filename := FilenameP;
  end;

  if (Result) then
    Result := ShowModal() = mrOk;
end;

procedure TDExport.FBBackClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  for ActivePageIndex := PageControl.ActivePageIndex - 1 downto 0 do
    if (PageControl.Pages[ActivePageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := ActivePageIndex;
      Exit;
    end;
end;

procedure TDExport.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Export)) then
  begin
    SetEvent(Export.UserAbort);
    if (not Export.Suspended) then
      Export.WaitFor();
  end;
end;

procedure TDExport.FBForwardClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  for ActivePageIndex := PageControl.ActivePageIndex + 1 to PageControl.PageCount - 1 do
    if (PageControl.Pages[ActivePageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := ActivePageIndex;
      Exit;
    end;
end;

procedure TDExport.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDExport.FDatabaseDblClick(Sender: TObject);
begin
  FBForward.Click();
end;

procedure TDExport.FDatabaseTagClick(Sender: TObject);
begin
  FDatabaseTag.Enabled := FDatabaseTagFree.Checked;

  TSXMLOptionChange(Sender);
end;

procedure TDExport.FDatabaseTagKeyPress(Sender: TObject;
  var Key: Char);
begin
  FDatabaseTagClick(Sender);
end;

procedure TDExport.FDestField1Change(Sender: TObject);
var
  I: Integer;
  J: Integer;
begin
  TSExecute.Enabled := False;
  for I := 0 to Length(FFields) - 1 do
    if ((FFields[I].ItemIndex > 0) and (FDestFields[I].Text <> '')) then
      TSExecute.Enabled := True;

  for I := 0 to Length(FFields) - 1 do
    for J := 0 to I - 1 do
      if ((I <> J) and FDestFields[I].Enabled and FDestFields[J].Enabled and (lstrcmpi(PChar(FDestFields[J].Text), PChar(FDestFields[I].Text)) = 0)) then
        TSExecute.Enabled := False;

  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDExport.FField1Change(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FDestFields) - 1 do
    if (Sender = FFields[I]) then
    begin
      FDestFields[I].Enabled := (FFields[I].ItemIndex > 0) and (ExportType in [etTextFile, etExcelFile, etHTMLFile, etXMLFile, etPrint]);
      FDestFields[I].Text := FFields[I].Text;
    end;

  if (Length(FDestFields) > 0) then
    FDestField1Change(Sender);
end;

procedure TDExport.FField1Exit(Sender: TObject);
var
  I: Integer;
begin
  if (Sender is TComboBox_Ext) then
    for I := 0 to Length(FFields) - 1 do
      if ((FFields[I] <> Sender) and (FFields[I].ItemIndex = TComboBox_Ext(Sender).ItemIndex)) then
      begin
        FFields[I].ItemIndex := 0;
        FField1Change(FFields[I]);
      end;
end;

procedure TDExport.FFieldTagClick(Sender: TObject);
begin
  FFieldTag.Enabled := FFieldTagFree.Checked;

  TSXMLOptionChange(Sender);
end;

procedure TDExport.FFieldTagKeyPress(Sender: TObject;
  var Key: Char);
begin
  FFieldTagClick(Sender);
end;

procedure TDExport.FHTMLDataClick(Sender: TObject);
begin
  FLHTMLNullValues.Enabled := FHTMLData.Checked;
  FHTMLNullText.Enabled := FHTMLData.Checked;
  FLHTMLViewDatas.Enabled := FHTMLData.Checked;
  FHTMLShowMemoContent.Enabled := FHTMLData.Checked;
  FLHTMLBGColorEnabled.Enabled := FHTMLData.Checked;
  FHTMLRowBGColorEnabled.Enabled := FHTMLData.Checked;
  FHTMLIndexBGColorEnabled.Enabled := FHTMLData.Checked;

  TSExecute.Enabled := FHTMLStructure.Checked or FHTMLData.Checked;
  CheckActivePageChange(TSHTMLOptions.PageIndex);
end;

procedure TDExport.FHTMLDataKeyPress(Sender: TObject; var Key: Char);
begin
  FHTMLDataClick(Sender);
end;

procedure TDExport.FHTMLStructureClick(Sender: TObject);
begin
  TSExecute.Enabled := FHTMLStructure.Checked or FHTMLData.Checked;
  CheckActivePageChange(TSHTMLOptions.PageIndex);
end;

procedure TDExport.FHTMLStructureKeyPress(Sender: TObject; var Key: Char);
begin
  FHTMLStructureClick(Sender);
end;

procedure TDExport.FODBCSelectChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  Cancel: Boolean;
  cbMessageText: SQLSMALLINT;
  MessageText: PSQLTCHAR;
  SQLState: array [0 .. SQL_SQLSTATE_SIZE] of SQLTCHAR;
  Success: Boolean;
begin
  if (ODBC <> SQL_NULL_HANDLE) then
  begin
    SQLDisconnect(ODBC);
    SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
  end;

  if ((Change = ctState) and Assigned(Item) and Item.Selected) then
    repeat
      DLogin.Account := nil;
      DLogin.Filename := Item.Caption;
      DLogin.Window := Window;
      Success := DLogin.Execute();
      Cancel := not Success;
      if (Success) then
      begin
        Success := SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBCEnv, @ODBC));
        Success := Success and SQL_SUCCEEDED(SQLConnect(ODBC, PSQLTCHAR(PChar(Item.Caption)), SQL_NTS, PSQLTCHAR(DLogin.Username), SQL_NTS, PSQLTCHAR(DLogin.Password), SQL_NTS));
        if (not Success and (ODBC <> SQL_NULL_HANDLE)) then
        begin
          if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
          begin
            GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTCHAR));
            if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, @SQLState, nil, @MessageText, cbMessageText + 1, nil))) then
              MsgBox(PChar(MessageText) + ' (' + SQLState + ')', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
            FreeMem(MessageText);
          end;
          SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
        end;
      end;
    until (Success or Cancel);

  TSExecute.Enabled := ODBC <> SQL_NULL_HANDLE;
  FBForward.Enabled := TSExecute.Enabled;
end;

procedure TDExport.FODBCSelectDblClick(Sender: TObject);
begin
  FBForward.Click();
end;

procedure TDExport.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (Event.EventType in [ceAfterExecuteSQL]) then
    InitializeDBObjects();
end;

procedure TDExport.FormCreate(Sender: TObject);
begin
  Export := nil;

  FDBObjects := TList.Create();
  FODBCSelect.SmallImages := Preferences.SmallImages;

  FCSVHeadline.Checked := Preferences.Export.CSVHeadline;
  FSeparatorTab.Checked := Preferences.Export.CSVSeparatorType = stTab;
  FSeparatorChar.Checked := Preferences.Export.CSVSeparatorType = stChar;
  FSeparator.Text := Preferences.Export.CSVSeparator;
  FNoQuote.Checked := Preferences.Export.CSVQuote = 0;
  FStringQuote.Checked := Preferences.Export.CSVQuote = 1;
  FAllQuote.Checked := Preferences.Export.CSVQuote = 2;
  FQuoteChar.Text := Preferences.Export.CSVQuoteChar;

  FSQLStructure.Checked := Preferences.Export.SQLStructure;
  FSQLData.Checked := Preferences.Export.SQLData;
  FUseDatabase.Checked := Preferences.Export.SQLUseDatabase;
  FDrop.Checked := Preferences.Export.SQLDropBeforeCreate;
  FReplaceData.Checked := Preferences.Export.SQLReplaceData;
  FDisableKeys.Checked := Preferences.Export.SQLDisableKeys;

  FSQLOptionClick(Sender);

  FHTMLStructure.Checked := Preferences.Export.HTMLStructure;
  FHTMLData.Checked := Preferences.Export.HTMLData;
  FHTMLNullText.Checked := Preferences.GridNullText;
  FHTMLShowMemoContent.Checked := Preferences.GridShowMemoContent;
  FHTMLRowBGColorEnabled.Checked := Preferences.GridRowBGColorEnabled;
  FHTMLIndexBGColorEnabled.Checked := True;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed

  ExportType := etSQLFile;
end;

procedure TDExport.FormDestroy(Sender: TObject);
begin
  FDBObjects.Free();
end;

procedure TDExport.FormHide(Sender: TObject);
begin
  Client.UnRegisterEventProc(FormClientEvent);

  if (Assigned(DBGrid)) then
    DBGrid.DataSource.DataSet.EnableControls();

  if (ModalResult = mrOk) then
  begin
    Preferences.Export.CSVHeadline := FCSVHeadline.Checked;
    if (ExportType = etTextFile) then
    begin
      if (FSeparatorTab.Checked) then
        Preferences.Export.CSVSeparatorType := stTab
      else if (FSeparatorChar.Checked) then
        Preferences.Export.CSVSeparatorType := stChar;
      Preferences.Export.CSVSeparator := FSeparator.Text;
      if (FNoQuote.Checked) then
        Preferences.Export.CSVQuote := 0
      else if (FAllQuote.Checked) then
        Preferences.Export.CSVQuote := 0
      else
        Preferences.Export.CSVQuote := 1;
      Preferences.Export.CSVQuoteChar := FQuoteChar.Text;
    end;

    Preferences.Export.SQLStructure := FSQLStructure.Checked;
    Preferences.Export.SQLData := FSQLData.Checked;
    Preferences.Export.SQLDropBeforeCreate := FDrop.Checked;
    Preferences.Export.SQLReplaceData := FReplaceData.Checked;
    Preferences.Export.SQLDisableKeys := FDisableKeys.Checked;
    if (DBObjects.Count > 1) then
      Preferences.Export.SQLCreateDatabase := FCreateDatabase.Checked;
    Preferences.Export.SQLUseDatabase := FUseDatabase.Checked;

    Preferences.Export.HTMLStructure := FHTMLStructure.Checked;
    Preferences.Export.HTMLData := FHTMLData.Checked;
  end;

  FODBCSelect.Items.BeginUpdate();
  FODBCSelect.Items.Clear();
  FODBCSelect.Items.EndUpdate();
  ClearTSFields(Sender);
  PageControl.ActivePage := nil;

  if (ODBC <> SQL_NULL_HANDLE) then
  begin
    SQLDisconnect(ODBC);
    SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
  end;
  if (ODBCEnv <> SQL_NULL_HANDLE) then
    begin SQLFreeHandle(SQL_HANDLE_ENV, ODBCEnv); ODBCEnv := SQL_NULL_HANDLE; end;

  PageControl.ActivePage := nil;
end;

procedure TDExport.FormShow(Sender: TObject);
begin
  ModalResult := mrNone;
  if (ExportType = etPrint) then
    Caption := Preferences.LoadStr(577)
  else if (ExtractFileName(Filename) = '') then
    Caption := Preferences.LoadStr(210)
  else
    Caption := Preferences.LoadStr(210) + ' ' + ExtractFileName(Filename);

  case (ExportType) of
    etSQLFile: HelpContext := 1014;
    etTextFile: HelpContext := 1134;
    etExcelFile: HelpContext := 1107;
    etAccessFile: HelpContext := 1129;
    etSQLiteFile: HelpContext := 1128;
    etHTMLFile: HelpContext := 1016;
    etXMLFile: HelpContext := 1017;
    etPrint: HelpContext := 1018;
    else HelpContext := -1;
  end;

  if (Assigned(DBGrid)) then
    FHTMLStructure.Caption := Preferences.LoadStr(794)
  else
    FHTMLStructure.Caption := Preferences.LoadStr(215);

  TSODBCSelect.Enabled := ExportType in [etODBC];
  TSSQLOptions.Enabled := ExportType in [etSQLFile];
  FCreateDatabase.Checked := (DBObjects.Count > 1) and Preferences.Export.SQLCreateDatabase;
  TSCSVOptions.Enabled := ExportType in [etTextFile];
  TSXMLOptions.Enabled := (ExportType in [etXMLFile]) and not Assigned(DBGrid);
  TSHTMLOptions.Enabled := ExportType in [etHTMLFile, etPrint];
  TSFields.Enabled := (ExportType in [etExcelFile]) and ((DBObjects.Count = 1) or Assigned(DBGrid)) or (ExportType in [etXMLFile]) and Assigned(DBGrid);
  TSExecute.Enabled := not TSODBCSelect.Enabled and not TSSQLOptions.Enabled and not TSCSVOptions.Enabled and not TSHTMLOptions.Enabled and not TSFields.Enabled;

  if (TSFields.Enabled) then
    InitTSFields(Sender);

  FBBack.Visible := TSODBCSelect.Enabled or TSSQLOptions.Enabled or TSCSVOptions.Enabled or TSXMLOptions.Enabled or TSHTMLOptions.Enabled or TSFields.Enabled;
  FBForward.Visible := FBBack.Visible;
  FBForward.Enabled := FBForward.Visible and (not TSODBCSelect.Enabled or Assigned(FODBCSelect.Selected));

  FBCancel.ModalResult := mrCancel;

  if (Assigned(DBGrid)) then
    DBGrid.DataSource.DataSet.DisableControls();

  Client.RegisterEventProc(FormClientEvent);

  DBObjects.Sort(DBObjectsSortItem);
  InitializeDBObjects();

  PostMessage(Handle, CM_POSTSHOW, 0, 0);
end;

procedure TDExport.FQuoteCharExit(Sender: TObject);
begin
  if (FQuoteChar.Text = '') then
    FNoQuote.Checked := True;
end;

procedure TDExport.FQuoteClick(Sender: TObject);
begin
  FQuoteChar.Enabled := not FNoQuote.Checked;
  FLQuoteChar.Enabled := FQuoteChar.Enabled;

  if (not FNoQuote.Checked and (FQuoteChar.Text = '')) then
    FQuoteChar.Text := '"';
end;

procedure TDExport.FQuoteKeyPress(Sender: TObject; var Key: Char);
begin
  FQuoteClick(Sender);
end;

procedure TDExport.FSeparatorClick(Sender: TObject);
begin
  if (not FStringQuote.Enabled and FStringQuote.Checked) then
    FAllQuote.Checked := True;
end;

procedure TDExport.FSeparatorKeyPress(Sender: TObject; var Key: Char);
begin
  FSeparatorClick(Sender);
end;

procedure TDExport.FSQLOptionClick(Sender: TObject);
begin
  FCreateDatabase.Enabled := FSQLStructure.Checked;
  FCreateDatabase.Checked := FCreateDatabase.Checked and FCreateDatabase.Enabled;
  FUseDatabase.Enabled := not FCreateDatabase.Checked;
  FUseDatabase.Checked := FUseDatabase.Checked or FCreateDatabase.Checked;

  FDrop.Enabled := FSQLStructure.Checked;
  FDrop.Checked := FDrop.Checked and FDrop.Enabled;
  FReplaceData.Enabled := not FCreateDatabase.Checked and FSQLData.Checked and not FDrop.Checked;
  FReplaceData.Checked := FReplaceData.Checked and FReplaceData.Enabled;

  FDisableKeys.Enabled := FSQLData.Checked;
  FDisableKeys.Checked := FDisableKeys.Checked and FDisableKeys.Enabled;

  TSExecute.Enabled := FSQLStructure.Checked or FSQLData.Checked;
  CheckActivePageChange(TSSQLOptions.PageIndex);
end;

procedure TDExport.FSQLOptionKeyPress(Sender: TObject; var Key: Char);
begin
  FSQLOptionClick(Sender);
end;

procedure TDExport.FTableTagClick(Sender: TObject);
begin
  FTableTag.Enabled := FTableTagFree.Checked;

  TSXMLOptionChange(Sender);
end;

procedure TDExport.FTableTagKeyPress(Sender: TObject;
  var Key: Char);
begin
  FTableTagClick(Sender);
end;

procedure TDExport.InitializeDBObjects();
var
  Database: TCDatabase;
  Index: Integer;
  Objects: TList;
begin
  SQLWait := False;

  if (DBObjects.Count > 0) then
  begin
    Objects := TList.Create();

    Index := 0;
    Database := TCDBObject(DBObjects[0]).Database;
    repeat
      if ((Index = DBObjects.Count) or (TCDBObject(DBObjects[Index]).Database <> Database)) then
      begin
        SQLWait := Database.InitializeSources(Objects);
        Objects.Clear();
      end
      else
        Objects.Add(DBObjects[Index]);

      Inc(Index);
    until (SQLWait or (Index > DBObjects.Count));

    Objects.Free();
  end;

  if (not SQLWait) then
    CheckActivePageChange(PageControl.ActivePageIndex);
end;

procedure TDExport.InitTSFields(Sender: TObject);
var
  I: Integer;
  J: Integer;
  K: Integer;
begin
  ClearTSFields(Sender);

  if ((DBObjects.Count > 0) and (TCDBObject(DBObjects[0]) is TCTable)) then
    SetLength(FFields, TCTable(DBObjects[0]).Fields.Count)
  else if (Assigned(DBGrid)) then
    SetLength(FFields, DBGrid.FieldCount);

  FLDestFields.Visible :=
    (ExportType = etTextFile) and FCSVHeadline.Checked
    or (ExportType = etExcelFile)
    or (ExportType in [etXMLFile, etHTMLFile, etPrint]) and not FHTMLStructure.Checked;

  if (FLDestFields.Visible) then
  begin
    SetLength(FLReferrers, Length(FFields));
    SetLength(FDestFields, Length(FFields));
  end;

  ScrollBox.DisableAlign();

  for I := 0 to Length(FFields) - 1 do
  begin
    FFields[I] := TComboBox_Ext.Create(ScrollBox);
    FFields[I].Parent := ScrollBox;
    FFields[I].Left := FField1.Left;
    FFields[I].Top := FField1.Top + I * (FField2.Top - FField1.Top);
    FFields[I].Width := FField1.Width;
    FFields[I].Height := FField1.Height;
    FFields[I].Style := FField1.Style;
    FFields[I].Items.Add('');
    if ((DBObjects.Count > 0) and (TCDBObject(DBObjects[0]) is TCTable)) then
      for J := 0 to TCTable(DBObjects[0]).Fields.Count - 1 do
        FFields[I].Items.Add(TCTable(DBObjects[0]).Fields[J].Name)
    else if (Assigned(DBGrid)) then
      for J := 0 to DBGrid.FieldCount - 1 do
        FFields[I].Items.Add(DBGrid.Fields[J].DisplayName);
    FFields[I].ItemIndex := I + 1;
    FFields[I].OnChange := FField1.OnChange;
    FFields[I].OnExit := FField1.OnExit;

    if (FLDestFields.Visible) then
    begin
      FLReferrers[I] := TLabel.Create(ScrollBox);
      FLReferrers[I].Parent := ScrollBox;
      FLReferrers[I].Left := FLReferrer1.Left;
      FLReferrers[I].Top := FLReferrer1.Top + I * (FField2.Top - FField1.Top);
      FLReferrers[I].Width := FLReferrer1.Width;
      FLReferrers[I].Height := FLReferrer1.Height;
      FLReferrers[I].Caption := FLReferrer1.Caption;

      FDestFields[I] := TEdit.Create(ScrollBox);
      FDestFields[I].Parent := ScrollBox;
      FDestFields[I].Left := FDestField1.Left;
      FDestFields[I].Top := FDestField1.Top + I * (FField2.Top - FField1.Top);
      FDestFields[I].Width := FDestField1.Width;
      FDestFields[I].Height := FDestField1.Height;
      FDestFields[I].Enabled := ExportType in [etTextFile, etExcelFile, etHTMLFile, etXMLFile, etPrint];
      TEdit(FDestFields[I]).Text := FFields[I].Text;
      K := 2;
      for J := 0 to I - 1 do
        if (FFields[I].Text = FFields[J].Text) then
        begin
          TEdit(FDestFields[I]).Text := FFields[J].Text + '_' + IntToStr(K);
          Inc(K);
        end;
      FDestFields[I].OnChange := FDestField1.OnChange;
    end;
  end;

  ScrollBox.EnableAlign();
end;

procedure TDExport.OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; var Success: TDataAction);
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
    else
    begin
      Msg := Error.ErrorMessage;
      ErrorMsg := Msg;
    end;
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

procedure TDExport.TSCSVOptionsShow(Sender: TObject);
begin
  ClearTSFields(Sender);

  FSeparatorClick(Self);

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  TSFields.Enabled := (DBObjects.Count = 1) or Assigned(DBGrid);
  TSExecute.Enabled := not TSFields.Enabled;
  CheckActivePageChange(TSCSVOptions.PageIndex);
end;

procedure TDExport.TSExecuteShow(Sender: TObject);
var
  ExportExcel: TTExportExcel;
  ExportHTML: TTExportHTML;
  ExportSQL: TTExportSQL;
  ExportText: TTExportText;
  ExportXML: TTExportXML;
  I: Integer;
  ProgressInfos: TTools.TProgressInfos;
begin
  CheckActivePageChange(TSExecute.PageIndex);
  FBBack.Enabled := False;

  FBForward.Default := False;
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
  SendMessage(Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));

  if (Assigned(DBGrid)) then
  begin
    case (ExportType) of
      etSQLFile:
        try
          ExportSQL := TTExportSQL.Create(Client, Filename, CodePage);
          ExportSQL.CreateDatabaseStmts := FCreateDatabase.Checked;
          ExportSQL.Data := FHTMLData.Checked;
          ExportSQL.DisableKeys := FDisableKeys.Checked;
          ExportSQL.IncludeDropStmts := FDrop.Checked;
          ExportSQL.ReplaceData := FReplaceData.Checked;
          ExportSQL.Structure := FSQLStructure.Checked;
          ExportSQL.UseDatabaseStmts := FUseDatabase.Checked;

          Export := ExportSQL;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etTextFile:
        try
          ExportText := TTExportText.Create(Client, Filename, CodePage);
          ExportText.Data := True;
          if (FSeparatorTab.Checked) then
            ExportText.Delimiter := #9;
          if (FSeparatorChar.Checked) then
            ExportText.Delimiter := FSeparator.Text;
          ExportText.QuoteStringValues := FStringQuote.Checked;
          ExportText.QuoteValues := FAllQuote.Checked;
          ExportText.Quoter := FQuoteChar.Text[1];
          ExportText.Structure := FCSVHeadline.Checked;

          Export := ExportText;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etExcelFile:
        try
          ExportExcel := TTExportExcel.Create(Client, Filename);
          ExportExcel.Structure := True;

          Export := ExportExcel;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etXMLFile:
        try
          ExportXML := TTExportXML.Create(Client, Filename, CodePage);
          if (FDatabaseTagName.Checked) then
          begin
            ExportXML.DatabaseTag := 'database';
            ExportXML.DatabaseAttribute := '';
          end
          else if (FDatabaseTagFree.Checked) then
          begin
            ExportXML.DatabaseTag := FDatabaseTag.Text;
            ExportXML.DatabaseAttribute := FDatabaseAttribute.Text;
          end
          else
          begin
            ExportXML.DatabaseTag := '';
            ExportXML.DatabaseAttribute := '';
          end;
          ExportXML.RootTag := FRootTag.Text;
          ExportXML.Structure := True;
          if (FTableTagName.Checked) then
          begin
            ExportXML.TableTag := 'Table';
            ExportXML.TableAttribute := '';
          end
          else if (FTableTagFree.Checked) then
          begin
            ExportXML.TableTag := FTableTag.Text;
            ExportXML.TableAttribute := FTableAttribute.Text;
          end
          else
          begin
            ExportXML.TableTag := '';
            ExportXML.TableAttribute := '';
          end;
          ExportXML.RecordTag := FRecordTag.Text;
          if (FFieldTagName.Checked) then
          begin
            ExportXML.FieldTag := 'database';
            ExportXML.FieldAttribute := '';
          end
          else
          begin
            ExportXML.FieldTag := FFieldTag.Text;
            ExportXML.FieldAttribute := FFieldAttribute.Text;
          end;

          Export := ExportXML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etHTMLFile,
      etPrint:
        try
          ExportHTML := TTExportHTML.Create(Client, Filename, CodePage);
          ExportHTML.Data := FHTMLData.Checked;
          ExportHTML.IndexBackground := FHTMLIndexBGColorEnabled.Checked;
          ExportHTML.TextContent := FHTMLShowMemoContent.Checked;
          ExportHTML.NULLText := FHTMLNullText.Checked;
          ExportHTML.RowBackground := FHTMLRowBGColorEnabled.Checked;
          ExportHTML.Structure := FHTMLStructure.Checked;

          Export := ExportHTML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
    end;

    if (Assigned(Export)) then
    begin
      if (Length(FFields) = 0) then
      begin
        for I := 0 to DBGrid.Columns.Count - 1 do
          if (DBGrid.Columns[I].Visible) then
            begin
              SetLength(Export.Fields, Length(Export.Fields) + 1);
              Export.Fields[Length(Export.Fields) - 1] := DBGrid.Columns[I].Field;
              SetLength(Export.TargetFields, Length(Export.TargetFields) + 1);
              Export.TargetFields[Length(Export.TargetFields) - 1].Name := DBGrid.Columns[I].DisplayName;
            end;
      end
      else
        for I := 0 to Length(FFields) - 1 do
          if (FFields[I].ItemIndex > 0) then
          begin
            SetLength(Export.Fields, Length(Export.Fields) + 1);
            Export.Fields[Length(Export.Fields) - 1] := DBGrid.Fields[FFields[I].ItemIndex - 1];
            SetLength(Export.TargetFields, Length(Export.TargetFields) + 1);
            Export.TargetFields[Length(Export.TargetFields) - 1].Name := FDestFields[I].Text;
          end;

      Export.Wnd := Handle;
      Export.UpdateMessage := CM_UPDATEPROGRESSINFO;
      Export.ExecutedMessage := CM_EXECUTIONDONE;
      Export.OnError := OnError;
      Export.Add(DBGrid);
      if (Export.Client.Asynchron) then
        Export.Start()
      else
        Export.Execute();
    end;
  end
  else
  begin
    case (ExportType) of
      etSQLFile:
        try
          ExportSQL := TTExportSQL.Create(Client, Filename, CodePage);
          ExportSQL.CreateDatabaseStmts := FCreateDatabase.Checked;
          ExportSQL.Data := FSQLData.Checked;
          ExportSQL.DisableKeys := FDisableKeys.Checked;
          ExportSQL.IncludeDropStmts := FDrop.Checked;
          ExportSQL.ReplaceData := FReplaceData.Checked;
          ExportSQL.Structure := FSQLStructure.Checked;
          ExportSQL.UseDatabaseStmts := FUseDatabase.Checked;
          for I := 0 to DBObjects.Count - 1 do
            ExportSQL.Add(TCDBObject(DBObjects[I]));

          Export := ExportSQL;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etTextFile:
        try
          ExportText := TTExportText.Create(Client, Filename, CodePage);
          ExportText.Data := True;
          if (FSeparatorTab.Checked) then
            ExportText.Delimiter := #9;
          if (FSeparatorChar.Checked) then
            ExportText.Delimiter := FSeparator.Text;
          ExportText.QuoteStringValues := FStringQuote.Checked;
          ExportText.QuoteValues := FAllQuote.Checked;
          ExportText.Quoter := FQuoteChar.Text[1];
          ExportText.Structure := FCSVHeadline.Checked;
          for I := 0 to DBObjects.Count - 1 do
            ExportText.Add(TCDBObject(DBObjects[I]));

          Export := ExportText;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etExcelFile:
        try
          Export := TTExportExcel.Create(Client, Filename);
          Export.Data := True;
          Export.Structure := True;
          for I := 0 to DBObjects.Count - 1 do
            Export.Add(TCDBObject(DBObjects[I]));
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etAccessFile:
        try
          Export := TTExportAccess.Create(Client, Filename);
          Export.Structure := True;
          Export.Data := True;
          for I := 0 to DBObjects.Count - 1 do
            if (TCDBObject(DBObjects[I]) is TCBaseTable) then
              Export.Add(TCDBObject(DBObjects[I]));
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etODBC:
        begin
          Export := TTExportODBC.Create(Client, ODBCEnv, ODBC);
          Export.Data := True;
          Export.Structure := True;
          for I := 0 to DBObjects.Count - 1 do
            if (TCDBObject(DBObjects[I]) is TCBaseTable) then
              Export.Add(TCDBObject(DBObjects[I]));
        end;
      etSQLiteFile:
        try
          Export := TTExportSQLite.Create(Client, Filename, CP_ACP);
          Export.Data := True;
          Export.Structure := True;
          for I := 0 to DBObjects.Count - 1 do
            if (TCDBObject(DBObjects[I]) is TCBaseTable) then
              Export.Add(TCDBObject(DBObjects[I]));
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etXMLFile:
        try
          ExportXML := TTExportXML.Create(Client, Filename, CodePage);
          ExportXML.Data := True;
          if (FDatabaseTagName.Checked) then
          begin
            ExportXML.DatabaseTag := 'database';
            ExportXML.DatabaseAttribute := '';
          end
          else if (FDatabaseTagFree.Checked) then
          begin
            ExportXML.DatabaseTag := FDatabaseTag.Text;
            ExportXML.DatabaseAttribute := FDatabaseAttribute.Text;
          end
          else
          begin
            ExportXML.DatabaseTag := '';
            ExportXML.DatabaseAttribute := '';
          end;
          ExportXML.RootTag := FRootTag.Text;
          ExportXML.Structure := True;
          if (FTableTagName.Checked) then
          begin
            ExportXML.TableTag := 'Table';
            ExportXML.TableAttribute := '';
          end
          else if (FTableTagFree.Checked) then
          begin
            ExportXML.TableTag := FTableTag.Text;
            ExportXML.TableAttribute := FTableAttribute.Text;
          end
          else
          begin
            ExportXML.TableTag := '';
            ExportXML.TableAttribute := '';
          end;
          ExportXML.RecordTag := FRecordTag.Text;
          if (FFieldTagName.Checked) then
          begin
            ExportXML.FieldTag := 'database';
            ExportXML.FieldAttribute := '';
          end
          else
          begin
            ExportXML.FieldTag := FFieldTag.Text;
            ExportXML.FieldAttribute := FFieldAttribute.Text;
          end;
          for I := 0 to DBObjects.Count - 1 do
            ExportXML.Add(TCDBObject(DBObjects[I]));

          Export := ExportXML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etHTMLFile,
      etPrint:
        try
          ExportHTML := TTExportHTML.Create(Client, Filename, CodePage);
          ExportHTML.Data := FHTMLData.Checked;
          ExportHTML.IndexBackground := FHTMLIndexBGColorEnabled.Checked;
          ExportHTML.TextContent := FHTMLShowMemoContent.Checked;
          ExportHTML.NULLText := FHTMLNullText.Checked;
          ExportHTML.RowBackground := FHTMLRowBGColorEnabled.Checked;
          ExportHTML.Structure := FHTMLStructure.Checked;
          for I := 0 to DBObjects.Count - 1 do
            ExportHTML.Add(TCDBObject(DBObjects[I]));

          Export := ExportHTML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
    end;

    if (Assigned(Export)) then
    begin
      if ((DBObjects.Count = 1) and (TCDBObject(DBObjects[0]) is TCTable)) then
        for I := 0 to Length(FFields) - 1 do
          if (FFields[I].ItemIndex > 0) then
          begin
            SetLength(Export.TableFields, Length(Export.TableFields) + 1);
            Export.TableFields[Length(Export.TableFields) - 1] := TCTable(DBObjects[0]).Fields[FFields[I].ItemIndex - 1];
            SetLength(Export.TargetFields, Length(Export.TargetFields) + 1);
            Export.TargetFields[Length(Export.TargetFields) - 1].Name := FDestFields[I].Text;
          end;

      Export.Wnd := Handle;
      Export.UpdateMessage := CM_UPDATEPROGRESSINFO;
      Export.ExecutedMessage := CM_EXECUTIONDONE;
      Export.OnError := OnError;

      if (Export.Client.Asynchron) then
        Export.Start()
      else
        Export.Execute();
    end;
  end;
end;

procedure TDExport.TSFieldsShow(Sender: TObject);
begin
  TSExecute.Enabled := True;
  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDExport.TSHTMLOptionsShow(Sender: TObject);
begin
  FHTMLStructure.Enabled := not Assigned(DBGrid) or not (DBGrid.DataSource.DataSet is TMySQLTable);
  FHTMLStructure.Checked := FHTMLStructure.Checked and FHTMLStructure.Enabled;
  FHTMLStructureClick(Sender);
  FHTMLDataClick(Sender);
end;

procedure TDExport.TSODBCSelectShow(Sender: TObject);
var
  DataSourceName: array [0 .. SQL_MAX_DSN_LENGTH] of SQLTCHAR;
  Item: TListItem;
begin
  if (ODBCEnv = SQL_NULL_HANDLE) then
  begin
    if (SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, @ODBCEnv))
      and SQL_SUCCEEDED(SQLSetEnvAttr(ODBCEnv, SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), SQL_IS_UINTEGER))
      and SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBCEnv, @ODBC))
      and SQL_SUCCEEDED(SQLDataSources(ODBCEnv, SQL_FETCH_FIRST, @DataSourceName, Length(DataSourceName), nil, nil, 0, nil))) then
      repeat
        Item := FODBCSelect.Items.Add();
        Item.Caption := DataSourceName;
        Item.ImageIndex := iiDatabase;
      until (not SQL_SUCCEEDED(SQLDataSources(ODBCEnv, SQL_FETCH_NEXT, @DataSourceName, Length(DataSourceName), nil, nil, 0, nil)));
  end;

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  CheckActivePageChange(TSODBCSelect.PageIndex);
end;

procedure TDExport.TSOptionsHide(Sender: TObject);
begin
  if (TSFields.Enabled) then
    InitTSFields(Sender);
end;

procedure TDExport.TSSQLOptionsShow(Sender: TObject);
begin
  FSQLStructure.Enabled := not Assigned(DBGrid) or (DBGrid.DataSource.DataSet is TMySQLDataSet) and (TMySQLDataSet(DBGrid.DataSource.DataSet).TableName <> '');
  FSQLData.Enabled := not Assigned(DBGrid);

  FSQLStructure.Checked := FSQLStructure.Checked and FSQLStructure.Enabled;
  FSQLData.Checked := FSQLData.Checked or Assigned(DBGrid);

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  FSQLOptionClick(Sender);
end;

procedure TDExport.TSXMLOptionChange(Sender: TObject);
begin
  FBForward.Enabled :=
    (FRootTag.Text <> '')
    and (not FDatabaseTagDisabled.Checked or FDatabaseTagDisabled.Enabled)
    and (not FDatabaseTagName.Checked or FDatabaseTagName.Enabled)
    and (not FDatabaseTagFree.Checked or FDatabaseTagFree.Enabled and (FDatabaseTag.Text <> '') and (FDatabaseAttribute.Text <> ''))
    and (not FTableTagDisabled.Checked or FTableTagDisabled.Enabled)
    and (not FTableTagName.Checked or FTableTagName.Enabled)
    and (not FTableTagFree.Checked or FTableTagFree.Enabled and (FTableTag.Text <> '') and (FTableAttribute.Text <> ''))
    and (FRecordTag.Text <> '')
    and (not FFieldTagName.Checked or FFieldTagName.Enabled)
    and (not FFieldTagFree.Checked or FFieldTagFree.Enabled and (FFieldTag.Text <> '') and (FFieldAttribute.Text <> ''));

  FDatabaseTag.Enabled := FDatabaseTagFree.Checked;
  FDatabaseAttribute.Enabled := FDatabaseTagFree.Checked;

  FTableTag.Enabled := FTableTagFree.Checked;
  FTableAttribute.Enabled := FTableTagFree.Checked;
end;

procedure TDExport.TSXMLOptionsHide(Sender: TObject);
begin
  if (TSFields.Enabled) then
    InitTSFields(Sender);
end;

procedure TDExport.TSXMLOptionsShow(Sender: TObject);
var
  DatabaseCount: Integer;
  I: Integer;
  OldDatabase: TCDatabase;
begin
  DatabaseCount := 0;
  OldDatabase := nil;
  for I := 0 to DBObjects.Count - 1 do
  begin
    if (TCDBObject(DBObjects[I]).Database <> OldDatabase) then
      Inc(DatabaseCount);
    OldDatabase := TCDBObject(DBObjects[I]).Database;
  end;

  FDatabaseTagDisabled.Enabled := DatabaseCount <= 1;
  if (FDatabaseTagDisabled.Checked and not FDatabaseTagDisabled.Enabled) then
    FDatabaseTagFree.Checked := True;

  FTableTagDisabled.Enabled := DBObjects.Count <= 1;
  if (FTableTagDisabled.Checked and not FTableTagDisabled.Enabled) then
    FTableTagFree.Checked := True;

  TSFields.Enabled := (DBObjects.Count = 1) or Assigned(DBGrid);
  CheckActivePageChange(TSXMLOptions.PageIndex);
  TSXMLOptionChange(Sender);
end;

initialization
  FExport := nil;
end.

