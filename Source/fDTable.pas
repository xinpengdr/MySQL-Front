unit fDTable;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, ToolWin, ActnList,
  SynEdit, SynMemo,
  StdCtrls_Ext, Forms_Ext,
  fClient, fBase, MySQLDB, ComCtrls_Ext, ExtCtrls, 
  ExtCtrls_Ext;

type
  TDTable = class (TForm_Ext)
    ActionList: TActionList;
    aPCreateField: TAction;
    aPCreateForeignKey: TAction;
    aPCreateIndex: TAction;
    aPCreatePartition: TAction;
    aPCreateTrigger: TAction;
    aPDeleteField: TAction;
    aPDeleteForeignKey: TAction;
    aPDeleteIndex: TAction;
    aPDeletePartition: TAction;
    aPDeleteTrigger: TAction;
    aPDown: TAction;
    aPDown1: TMenuItem;
    aPEditField: TAction;
    aPEditForeignKey: TAction;
    aPEditIndex: TAction;
    aPEditPartition: TAction;
    aPEditTrigger: TAction;
    aPUp: TAction;
    aPUp1: TMenuItem;
    FAutoIncrement: TEdit;
    FBCancel: TButton;
    FBCheck: TButton;
    FBFlush: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FBOptimize: TButton;
    FChecked: TLabel;
    FCollation: TComboBox_Ext;
    FComment: TEdit;
    FCreated: TLabel;
    FDatabase: TEdit;
    FDataSize: TLabel;
    FDefaultCharset: TComboBox_Ext;
    FEngine: TComboBox_Ext;
    FFields: TListView;
    FForeignKeys: TListView;
    FIndexSize: TLabel;
    FIndices: TListView;
    FLAutoIncrement: TLabel;
    FLChecked: TLabel;
    FLCollation: TLabel;
    FLComment: TLabel;
    FLCreated: TLabel;
    FLDatabase: TLabel;
    FLDataSize: TLabel;
    FLDefaultCharset: TLabel;
    FLEngine: TLabel;
    FLIndexSize: TLabel;
    FLinear: TCheckBox;
    FLMaxDataSize: TLabel;
    FLName: TLabel;
    FLPartitionCount: TLabel;
    FLPartitionExpr: TLabel;
    FLPartitions: TLabel;
    FLPartitionType: TLabel;
    FLRecordCount: TLabel;
    FLRowType: TLabel;
    FLTablesCharset: TLabel;
    FLTablesCollation: TLabel;
    FLTablesCount: TLabel;
    FLTablesEngine: TLabel;
    FLTablesRowType: TLabel;
    FLUnusedSize: TLabel;
    FLUpdated: TLabel;
    FMaxDataSize: TLabel;
    FName: TEdit;
    FPartitionCount: TEdit;
    FPartitionExpr: TEdit;
    FPartitions: TListView_Ext;
    FPartitionType: TComboBox_Ext;
    FRecordCount: TLabel;
    FReferenced: TListView;
    FRowType: TComboBox_Ext;
    FSource: TSynMemo;
    FTablesCharset: TComboBox_Ext;
    FTablesCollation: TComboBox_Ext;
    FTablesCount: TLabel;
    FTablesEngine: TComboBox_Ext;
    FTablesRowType: TComboBox_Ext;
    FTriggers: TListView;
    FUDPartitionCount: TUpDown;
    FUnusedSize: TLabel;
    FUpdated: TLabel;
    GBasics: TGroupBox_Ext;
    GCheck: TGroupBox_Ext;
    GDates: TGroupBox_Ext;
    GFlush: TGroupBox_Ext;
    GMemory: TGroupBox_Ext;
    GOptimize: TGroupBox_Ext;
    GPartitions: TGroupBox_Ext;
    GRecordCount: TGroupBox_Ext;
    GRecords: TGroupBox_Ext;
    GTablesBasics: TGroupBox_Ext;
    GTablesRecords: TGroupBox_Ext;
    mlDCreate: TMenuItem;
    mlDDelete: TMenuItem;
    mlDProperties: TMenuItem;
    MList: TPopupMenu;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PageControl: TPageControl;
    PPartitions: TPanel_Ext;
    tbCreateField: TToolButton;
    tbCreateForeignKey: TToolButton;
    tbCreateIndex: TToolButton;
    tbDeleteField: TToolButton;
    tbDeleteForeignKey: TToolButton;
    tbDeleteIndex: TToolButton;
    tbFieldDown: TToolButton;
    TBFields: TToolBar;
    tbFieldUp: TToolButton;
    TBForeignKeys: TToolBar;
    TBIndices: TToolBar;
    tbPropertiesField: TToolButton;
    tbPropertiesForeignKey: TToolButton;
    tbPropertiesIndex: TToolButton;
    tbSeparator: TToolButton;
    TSExtras: TTabSheet;
    TSFields: TTabSheet;
    TSForeignKeys: TTabSheet;
    TSIndices: TTabSheet;
    TSInformations: TTabSheet;
    TSPartitions: TTabSheet;
    TSReferenced: TTabSheet;
    TSSource: TTabSheet;
    TSTable: TTabSheet;
    TSTables: TTabSheet;
    TSTriggers: TTabSheet;
    PSQLWait: TPanel;
    procedure aPCreateFieldExecute(Sender: TObject);
    procedure aPCreateForeignKeyExecute(Sender: TObject);
    procedure aPCreateIndexExecute(Sender: TObject);
    procedure aPCreatePartitionExecute(Sender: TObject);
    procedure aPDeleteFieldExecute(Sender: TObject);
    procedure aPDeleteForeignKeyExecute(Sender: TObject);
    procedure aPDeleteIndexExecute(Sender: TObject);
    procedure aPDeletePartitionExecute(Sender: TObject);
    procedure aPDownExecute(Sender: TObject);
    procedure aPEditFieldExecute(Sender: TObject);
    procedure aPEditForeignKeyExecute(Sender: TObject);
    procedure aPEditIndexExecute(Sender: TObject);
    procedure aPEditPartitionExecute(Sender: TObject);
    procedure aPUpExecute(Sender: TObject);
    procedure FAutoIncrementExit(Sender: TObject);
    procedure FBCheckClick(Sender: TObject);
    procedure FBFlushClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FBOptimizeClick(Sender: TObject);
    procedure FCollationDropDown(Sender: TObject);
    procedure FDefaultCharsetChange(Sender: TObject);
    procedure FDefaultCharsetExit(Sender: TObject);
    procedure FEngineChange(Sender: TObject);
    procedure FFieldsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FFieldsEnter(Sender: TObject);
    procedure FForeignKeysChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FForeignKeysEnter(Sender: TObject);
    procedure FIndicesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FIndicesEnter(Sender: TObject);
    procedure FLinearClick(Sender: TObject);
    procedure FLinearKeyPress(Sender: TObject; var Key: Char);
    procedure FListDblClick(Sender: TObject);
    procedure FListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FPartitionCountChange(Sender: TObject);
    procedure FPartitionExprChange(Sender: TObject);
    procedure FPartitionsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FPartitionTypeChange(Sender: TObject);
    procedure FTablesCharsetChange(Sender: TObject);
    procedure FTriggersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FTriggersEnter(Sender: TObject);
    procedure msCopyClick(Sender: TObject);
    procedure TSExtrasShow(Sender: TObject);
    procedure TSFieldsShow(Sender: TObject);
    procedure TSForeignKeysShow(Sender: TObject);
    procedure TSIndicesShow(Sender: TObject);
    procedure TSInformationsShow(Sender: TObject);
    procedure TSPartitionsShow(Sender: TObject);
    procedure TSReferencedShow(Sender: TObject);
    procedure TSSourceShow(Sender: TObject);
    procedure TSTriggersShow(Sender: TObject);
  private
    FCreatedName: string;
    NewTable: TCBaseTable;
    RecordCount: Integer;
    procedure Built();
    procedure FFieldsRefresh(Sender: TObject);
    procedure FForeignKeysRefresh(Sender: TObject);
    procedure FIndicesRefresh(Sender: TObject);
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure FPartitionsRefresh(Sender: TObject);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Charset: string;
    Collation: string;
    Database: TCDatabase;
    Engine: string;
    RowType: TMySQLRowType;
    Table: TCBaseTable;
    Tables: TList;
    function Execute(): Boolean;
    property CreatedName: string read FCreatedName;
  end;

function DTable(): TDTable;

implementation {***************************************************************}

{$R *.dfm}

uses
  Clipbrd, StrUtils,
  SQLUtils,
  fDField, fDIndex, fDForeignKey, fDTableService, fPreferences,
  fDTrigger, fDPartition;

var
  FTable: TDTable;

function DTable(): TDTable;
begin
  if (not Assigned(FTable)) then
  begin
    Application.CreateForm(TDTable, FTable);
    FTable.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FTable;
end;

{ TDTable *********************************************************************}

procedure TDTable.aPCreateFieldExecute(Sender: TObject);
begin
  DField.Database := nil;
  DField.Table := NewTable;
  DField.Field := nil;
  if (DField.Execute()) then
  begin
    FFieldsRefresh(Sender);

    FIndices.Items.Clear();

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPCreateForeignKeyExecute(Sender: TObject);
begin
  DForeignKey.Database := nil;
  DForeignKey.Table := NewTable;
  DForeignKey.ForeignKey := nil;
  if (DForeignKey.Execute()) then
  begin
    FForeignKeysRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPCreateIndexExecute(Sender: TObject);
begin
  DIndex.Database := nil;
  DIndex.Table := NewTable;
  DIndex.Index := nil;
  if (DIndex.Execute()) then
  begin
    FIndicesRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPCreatePartitionExecute(Sender: TObject);
begin
  DPartition.Table := NewTable;
  DPartition.Partition := nil;
  if (DPartition.Execute()) then
  begin
    FPartitionsRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPDeleteFieldExecute(Sender: TObject);
var
  I: Integer;
  Msg: string;
begin
  if (FFields.SelCount = 1) then
    Msg := Preferences.LoadStr(100, FFields.Selected.Caption)
  else
    Msg := Preferences.LoadStr(413);
  if (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
  begin
    for I := FFields.Items.Count - 1 downto 0 do
      if (FFields.Items.Item[I].Selected) then
        NewTable.Fields.DeleteField(NewTable.Fields[I]);

    FFieldsRefresh(Sender);
  end;
end;

procedure TDTable.aPDeleteForeignKeyExecute(Sender: TObject);
var
  I: Integer;
  Msg: string;
begin
  if (FForeignKeys.SelCount = 1) then
    Msg := Preferences.LoadStr(692, FForeignKeys.Selected.Caption)
  else
    Msg := Preferences.LoadStr(413);
  if (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
  begin
    for I := FForeignKeys.Items.Count - 1 downto 0 do
      if (FForeignKeys.Items.Item[I].Selected) then
        NewTable.ForeignKeys.DeleteForeignKey(NewTable.ForeignKeys[I]);

    FForeignKeysRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPDeleteIndexExecute(Sender: TObject);
var
  I: Integer;
  Msg: string;
begin
  if (FIndices.SelCount = 1) then
    Msg := Preferences.LoadStr(162, FIndices.Selected.Caption)
  else
    Msg := Preferences.LoadStr(413);
  if (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
  begin
    for I := FIndices.Items.Count - 1 downto 0 do
      if (FIndices.Items.Item[I].Selected) then
        NewTable.Indices.DeleteIndex(NewTable.Indices.Index[I]);

    FIndicesRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPDeletePartitionExecute(Sender: TObject);
var
  I: Integer;
  Msg: string;
begin
  if (FPartitions.SelCount = 1) then
    Msg := Preferences.LoadStr(841, FPartitions.Selected.Caption)
  else
    Msg := Preferences.LoadStr(413);
  if (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
    for I := FPartitions.Items.Count - 1 downto 0 do
      if (FPartitions.Items.Item[I].Selected) then
        NewTable.Partitions.DeletePartition(NewTable.Partitions.Partition[I]);

  FPartitionsRefresh(Sender);
end;

procedure TDTable.aPDownExecute(Sender: TObject);
var
  Fields: array of TCTableField;
  FocusedField: TCTableField;
  I: Integer;
  Index: Integer;
  NewListItem: TListItem;
  OldListItem: TListItem;
begin
  if ((PageControl.ActivePage = TSFields) and Assigned(FFields.ItemFocused)) then
  begin
    FocusedField := NewTable.Fields[FFields.ItemFocused.Index];

    SetLength(Fields, FFields.SelCount);

    Index := 0;
    for I := 0 to FFields.Items.Count - 1 do
      if (FFields.Items[I].Selected) then
      begin
        Fields[Index] := NewTable.Fields[I];
        Inc(Index);
      end;

    for Index := Length(Fields) - 1 downto 0 do
    begin
      OldListItem := FFields.Items[Fields[Index].Index];

      TCBaseTableFields(NewTable.Fields).MoveField(Fields[Index], Fields[Index]);

      NewListItem := FFields.Items.Insert(Fields[Index].Index + 1);
      NewListItem.Caption := OldListItem.Caption;
      NewListItem.ImageIndex := OldListItem.ImageIndex;
      NewListItem.SubItems.Text := OldListItem.SubItems.Text;
      NewListItem.Selected := OldListItem.Selected;

      FFields.Items.Delete(OldListItem.Index);
    end;

    FFields.ItemFocused := FFields.Items[FocusedField.Index];

    FListSelectItem(FFields, FFields.ItemFocused, True);

    SetLength(Fields, FFields.SelCount);
  end
  else if (PageControl.ActivePage = TSPartitions) then
  begin
    NewTable.Partitions.MovePartition(NewTable.Partitions[FPartitions.Selected.Index], FPartitions.Selected.Index + 1);

    FPartitions.Items.BeginUpdate(); FPartitions.DisableAlign();

    OldListItem := FPartitions.Items[FPartitions.Selected.Index];

    NewListItem := FPartitions.Items.Insert(FPartitions.Selected.Index + 2);
    NewListItem.Caption := OldListItem.Caption;
    NewListItem.ImageIndex := OldListItem.ImageIndex;
    NewListItem.SubItems.Text := OldListItem.SubItems.Text;
    NewListItem.Selected := OldListItem.Selected;

    FPartitions.Items.Delete(OldListItem.Index);

    FPartitions.Items.EndUpdate(); FPartitions.EnableAlign();
  end;
end;

procedure TDTable.aPEditFieldExecute(Sender: TObject);
begin
  DField.Database := nil;
  DField.Table := NewTable;
  DField.Field := TCBaseTableField(NewTable.Fields[FFields.ItemIndex]);
  if (DField.Execute()) then
  begin
    FFieldsRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPEditForeignKeyExecute(Sender: TObject);
var
  ForeignKey: TCForeignKey;
begin
  ForeignKey := NewTable.ForeignKeys[FForeignKeys.ItemIndex];

  DForeignKey.Database := nil;
  DForeignKey.Table := NewTable;
  DForeignKey.ForeignKey := ForeignKey;
  if (DForeignKey.Execute()) then
  begin
    FForeignKeysRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPEditIndexExecute(Sender: TObject);
begin
  DIndex.Database := nil;
  DIndex.Table := NewTable;
  DIndex.Index := NewTable.Indices.Index[FIndices.ItemIndex];
  if (DIndex.Execute()) then
  begin
    FIndicesRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPEditPartitionExecute(Sender: TObject);
var
  Partition: TCPartition;
begin
  Partition := NewTable.Partitions[FPartitions.ItemIndex];

  DPartition.Table := NewTable;
  DPartition.Partition := Partition;
  if (DPartition.Execute()) then
  begin
    FPartitionsRefresh(Sender);

    TSSource.TabVisible := False;
  end;
end;

procedure TDTable.aPUpExecute(Sender: TObject);
var
  Fields: array of TCTableField;
  FocusedField: TCTableField;
  I: Integer;
  Index: Integer;
  NewListItem: TListItem;
  OldListItem: TListItem;
begin
  if ((PageControl.ActivePage = TSFields) and Assigned(FFields.ItemFocused)) then
  begin
    FocusedField := NewTable.Fields[FFields.ItemFocused.Index];

    SetLength(Fields, FFields.SelCount);

    Index := 0;
    for I := 0 to FFields.Items.Count - 1 do
      if (FFields.Items[I].Selected) then
      begin
        Fields[Index] := NewTable.Fields[I];
        Inc(Index);
      end;

    for Index := 0 to Length(Fields) - 1 do
    begin
      OldListItem := FFields.Items[Fields[Index].Index];

      TCBaseTableFields(NewTable.Fields).MoveField(Fields[Index], Fields[Index].FieldBefore.FieldBefore);

      NewListItem := FFields.Items.Insert(Fields[Index].Index);
      NewListItem.Caption := OldListItem.Caption;
      NewListItem.ImageIndex := OldListItem.ImageIndex;
      NewListItem.SubItems.Text := OldListItem.SubItems.Text;
      NewListItem.Selected := OldListItem.Selected;

      FFields.Items.Delete(OldListItem.Index);
    end;

    FFields.ItemFocused := FFields.Items[FocusedField.Index];

    FListSelectItem(FFields, FFields.ItemFocused, True);

    SetLength(Fields, FFields.SelCount);
  end
  else if (PageControl.ActivePage = TSPartitions) then
  begin
    NewTable.Partitions.MovePartition(NewTable.Partitions[FPartitions.Selected.Index], FPartitions.Selected.Index - 1);

    FPartitions.Items.BeginUpdate(); FPartitions.DisableAlign();

    OldListItem := FPartitions.Items[FPartitions.Selected.Index];

    NewListItem := FPartitions.Items.Insert(FPartitions.Selected.Index - 1);
    NewListItem.Caption := OldListItem.Caption;
    NewListItem.ImageIndex := OldListItem.ImageIndex;
    NewListItem.SubItems.Text := OldListItem.SubItems.Text;
    NewListItem.Selected := OldListItem.Selected;

    FPartitions.Items.Delete(OldListItem.Index);

    FPartitions.Items.EndUpdate(); FPartitions.EnableAlign();
  end;
end;

procedure TDTable.Built();
var
  Engine: TCEngine;
  I: Integer;
  Index: Integer;
begin
  if (not Assigned(Tables)) then
  begin
    NewTable.Assign(Table);

    FName.Text := NewTable.Name;

    FDefaultCharset.ItemIndex := FDefaultCharset.Items.IndexOf(NewTable.DefaultCharset); FDefaultCharsetChange(Self);
    FCollation.ItemIndex := FCollation.Items.IndexOf(NewTable.Collation);

    FComment.Text := SQLUnwrapStmt(NewTable.Comment);

    if (not Assigned(NewTable.Engine)) then
      FEngine.ItemIndex := -1
    else
      FEngine.ItemIndex := FEngine.Items.IndexOf(NewTable.Engine.Name);
    FEngineChange(Self);

    FRowType.ItemIndex := Integer(NewTable.RowType);
    FAutoIncrement.Visible := NewTable.AutoIncrement > 0; FLAutoIncrement.Visible := FAutoIncrement.Visible;
    FAutoIncrement.Text := IntToStr(NewTable.AutoIncrement);

    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end
  else
  begin
    FTablesCount.Caption := IntToStr(Tables.Count);
    FDatabase.Text := Database.Name;

    Engine := TCBaseTable(Tables[0]).Engine;
    for I := 1 to Tables.Count - 1 do
      if (TCBaseTable(Tables[I]).Engine <> Engine) then
        Engine := nil;
    if (FTablesEngine.Style = csDropDown) then
      FTablesEngine.Text := NewTable.Engine.Name
    else if (Assigned(Engine)) then
      FTablesEngine.ItemIndex := FTablesEngine.Items.IndexOf(Engine.Name);

    Index := -1;
    for I := 0 to FDefaultCharset.Items.Count - 1 do
      if (Database.Client.Charsets.NameCmp(FDefaultCharset.Items[I], TCBaseTable(Tables[0]).DefaultCharset) = 0) then
        Index := I;
    for I := 1 to Tables.Count - 1 do
      if ((Index >= 0) and (Database.Client.Charsets.NameCmp(FDefaultCharset.Items[Index], TCBaseTable(Tables[I]).DefaultCharset) <> 0)) then
        Index := -1;
    FTablesCharset.ItemIndex := Index;

    Index := -1;
    for I := 0 to FCollation.Items.Count - 1 do
      if (Database.Client.Collations.NameCmp(FCollation.Items[I], TCBaseTable(Tables[0]).Collation) = 0) then
        Index := FCollation.Items.IndexOf(FCollation.Items[I]);
    for I := 1 to Tables.Count - 1 do
      if ((Index >= 0) and (Database.Client.Collations.NameCmp(FCollation.Items[Index], TCBaseTable(Tables[I]).Collation) <> 0)) then
        Index := -1;
    if (Assigned(Database.Client.CharsetByName(FTablesCharset.Text)) and Assigned(Database.Client.CharsetByName(FTablesCharset.Text).DefaultCollation) and (FTablesCollation.Items[Index] <> Database.Client.CharsetByName(FTablesCharset.Text).DefaultCollation.Name)) then
      FTablesCollation.ItemIndex := Index;

    Index := FTablesRowType.Items.IndexOf(TCBaseTable(Tables[0]).DBRowTypeStr());
    for I := 1 to Tables.Count - 1 do
      if (FTablesRowType.Items.IndexOf(TCBaseTable(Tables[I]).DBRowTypeStr()) <> Index) then
        Index := 0;
    FTablesRowType.ItemIndex := Index;
  end;

  TSInformations.TabVisible := Assigned(Table) and (Table.DataSize >= 0) or Assigned(Tables);
  TSFields.TabVisible := not Assigned(Tables);
  TSIndices.TabVisible := not Assigned(Tables);
  TSTriggers.TabVisible := Assigned(Table)  and Assigned(Database.Triggers);
  TSReferenced.TabVisible := Assigned(Table) and Assigned(NewTable.Engine) and (UpperCase(NewTable.Engine.Name) = 'INNODB');
  TSPartitions.TabVisible := not Assigned(Tables) and Assigned(NewTable.Partitions);
  TSExtras.TabVisible := Assigned(Table) or Assigned(Tables);
  TSSource.TabVisible := Assigned(Table) or Assigned(Tables);

  if (not Assigned(Tables)) then
    ActiveControl := FName
  else
    ActiveControl := FTablesEngine;
end;

procedure TDTable.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiBaseTable, Icon);

  PSQLWait.Caption := Preferences.LoadStr(882);

  aPUp.Caption := Preferences.LoadStr(563);
  aPDown.Caption := Preferences.LoadStr(564);

  TSTable.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLEngine.Caption := Preferences.LoadStr(110) + ':';
  FLDefaultCharset.Caption := Preferences.LoadStr(682) + ':';
  FLCollation.Caption := Preferences.LoadStr(702) + ':';
  FLComment.Caption := Preferences.LoadStr(111) + ':';
  GRecords.Caption := Preferences.LoadStr(124);
  FLAutoIncrement.Caption := Preferences.LoadStr(117) + ':';
  FLRowType.Caption := Preferences.LoadStr(129) + ':';

  TSTables.Caption := Preferences.LoadStr(108);
  GTablesBasics.Caption := Preferences.LoadStr(85);
  FLTablesCount.Caption := Preferences.LoadStr(617) + ':';
  FLDatabase.Caption := ReplaceStr(Preferences.LoadStr(38), '&', '') + ':';
  FLTablesEngine.Caption := Preferences.LoadStr(110) + ':';
  FLTablesCharset.Caption := Preferences.LoadStr(682) + ':';
  FLTablesCollation.Caption := Preferences.LoadStr(702) + ':';
  GTablesRecords.Caption := Preferences.LoadStr(124);
  FLTablesRowType.Caption := Preferences.LoadStr(129) + ':';

  TSInformations.Caption := Preferences.LoadStr(121);
  GDates.Caption := Preferences.LoadStr(122);
  FLCreated.Caption := Preferences.LoadStr(118) + ':';
  FLUpdated.Caption := Preferences.LoadStr(119) + ':';
  GMemory.Caption := Preferences.LoadStr(125);
  FLIndexSize.Caption := ReplaceStr(Preferences.LoadStr(163), '&', '') + ':';
  FLDataSize.Caption := Preferences.LoadStr(127) + ':';
  FLMaxDataSize.Caption := Preferences.LoadStr(844) + ':';
  GRecordCount.Caption := Preferences.LoadStr(170);
  FLRecordCount.Caption := Preferences.LoadStr(116) + ':';

  TSFields.Caption := Preferences.LoadStr(253);
  tbCreateField.Hint := Preferences.LoadStr(87) + '...';
  tbDeleteField.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbPropertiesField.Hint := ReplaceStr(Preferences.LoadStr(97), '&', '') + '...';
  tbFieldUp.Hint := ReplaceStr(Preferences.LoadStr(545), '&', '');
  tbFieldDown.Hint := ReplaceStr(Preferences.LoadStr(547), '&', '');
  FFields.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FFields.Column[1].Caption := Preferences.LoadStr(69);
  FFields.Column[2].Caption := Preferences.LoadStr(71);
  FFields.Column[3].Caption := Preferences.LoadStr(72);
  FFields.Column[4].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
  FFields.Column[5].Caption := ReplaceStr(Preferences.LoadStr(111), '&', '');

  TSIndices.Caption := Preferences.LoadStr(458);
  tbCreateIndex.Hint := Preferences.LoadStr(160) + '...';
  tbDeleteIndex.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbPropertiesIndex.Hint := ReplaceStr(Preferences.LoadStr(97), '&', '') + '...';
  FIndices.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FIndices.Column[1].Caption := Preferences.LoadStr(69);
  FIndices.Column[2].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');

  TSForeignKeys.Caption := Preferences.LoadStr(459);
  tbCreateForeignKey.Hint := Preferences.LoadStr(249) + '...';
  tbDeleteForeignKey.Hint := ReplaceStr(Preferences.LoadStr(559), '&', '');
  tbPropertiesForeignKey.Hint := ReplaceStr(Preferences.LoadStr(97), '&', '') + '...';
  FForeignKeys.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FForeignKeys.Column[1].Caption := Preferences.LoadStr(69);
  FForeignKeys.Column[2].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');

  TSTriggers.Caption := Preferences.LoadStr(797);
  FTriggers.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FTriggers.Column[1].Caption := Preferences.LoadStr(69);

  TSReferenced.Caption := Preferences.LoadStr(782);
  FReferenced.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FReferenced.Column[1].Caption := Preferences.LoadStr(69);
  FReferenced.Column[2].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');

  TSPartitions.Caption := ReplaceStr(Preferences.LoadStr(830), '&', '');
  GPartitions.Caption := Preferences.LoadStr(85);
  FLPartitionType.Caption := Preferences.LoadStr(110) + ':';
  FPartitionType.Items.Clear();
  FPartitionType.Items.Add(Preferences.LoadStr(831));
  FPartitionType.Items.Add(Preferences.LoadStr(832));
  FPartitionType.Items.Add(Preferences.LoadStr(833));
  FPartitionType.Items.Add(Preferences.LoadStr(834));
  FLinear.Caption := Preferences.LoadStr(835);
  FLPartitionExpr.Caption := Preferences.LoadStr(836) + ':';
  FLPartitionCount.Caption := Preferences.LoadStr(617) + ':';
  FLPartitions.Caption := Preferences.LoadStr(830) + ':';
//  tbPartitionUp.Hint := ReplaceStr(Preferences.LoadStr(545), '&', '');
//  tbPartitionDown.Hint := ReplaceStr(Preferences.LoadStr(547), '&', '');
  FPartitions.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FPartitions.Column[1].Caption := ReplaceStr(Preferences.LoadStr(836), '&', '');
  FPartitions.Column[2].Caption := ReplaceStr(Preferences.LoadStr(837), '&', '');
  FPartitions.Column[3].Caption := ReplaceStr(Preferences.LoadStr(838), '&', '');
  FPartitions.Column[4].Caption := ReplaceStr(Preferences.LoadStr(111), '&', '');

  TSExtras.Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
  GOptimize.Caption := Preferences.LoadStr(171);
  FLUnusedSize.Caption := Preferences.LoadStr(128) + ':';
  FBOptimize.Caption := Preferences.LoadStr(130);
  GCheck.Caption := Preferences.LoadStr(172);
  FLChecked.Caption := Preferences.LoadStr(120) + ':';
  FBCheck.Caption := Preferences.LoadStr(131);
  GFlush.Caption := Preferences.LoadStr(328);
  FBFlush.Caption := Preferences.LoadStr(329);

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

  aPCreateIndex.Caption := Preferences.LoadStr(26) + '...';
  aPDeleteIndex.Caption := Preferences.LoadStr(559);
  aPEditIndex.Caption := Preferences.LoadStr(97) + '...';
  aPCreateField.Caption := Preferences.LoadStr(26) + '...';
  aPDeleteField.Caption := Preferences.LoadStr(559);
  aPEditField.Caption := Preferences.LoadStr(97) + '...';
  aPCreateForeignKey.Caption := Preferences.LoadStr(26) + '...';
  aPDeleteForeignKey.Caption := Preferences.LoadStr(559);
  aPEditForeignKey.Caption := Preferences.LoadStr(97) + '...';
  aPCreateTrigger.Caption := Preferences.LoadStr(26) + '...';
  aPDeleteTrigger.Caption := Preferences.LoadStr(559);
  aPEditTrigger.Caption := Preferences.LoadStr(97) + '...';
  aPCreatePartition.Caption := Preferences.LoadStr(26) + '...';
  aPDeletePartition.Caption := Preferences.LoadStr(559);
  aPEditPartition.Caption := Preferences.LoadStr(97) + '...';

  msCopy.Action := MainAction('aECopy');
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
end;

function TDTable.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDTable.FAutoIncrementExit(Sender: TObject);
var
  Value: Int64;
begin
  if (TryStrToInt64(FAutoIncrement.Text, Value)) then
    NewTable.AutoIncrement := Value;
end;

procedure TDTable.FBCheckClick(Sender: TObject);
var
  I: Byte;
  Success: Boolean;
begin
  DTableService.Database := Database;

  if (not Assigned(Tables)) then
  begin
    SetLength(DTableService.TableNames, 1);
    DTableService.TableNames[0] := NewTable.Name;
    DTableService.ServiceMode := smCheck;

    Success := DTableService.Execute();
    if (not Success and (UpperCase(NewTable.Engine.Name) = 'MYISAM')) then
    begin
      DTableService.ServiceMode := smRepair;
      DTableService.Execute();
    end;

    if (Success) then
      NewTable.Checked := Database.BaseTableByName(DTableService.TableNames[0]).Checked;
  end
  else
  begin
    Success := True;
    SetLength(DTableService.TableNames, Tables.Count);

    for I := 0 to Tables.Count - 1 do
      if (Success) then
      begin
        DTableService.TableNames[I] := TCBaseTable(Tables[I]).Name;
        DTableService.ServiceMode := smCheck;

        Success := DTableService.Execute();
        if (not Success and (Database.Client.Engines.NameCmp(TCBaseTable(Tables[I]).Engine.Name, 'MYISAM') = 0)) then
        begin
          DTableService.ServiceMode := smRepair;
          Success := DTableService.Execute();
        end;
      end;
  end;

  TSExtrasShow(Sender);

  FBCheck.Enabled := False;
  ActiveControl := FBCancel;

  FBCancel.Caption := Preferences.LoadStr(231);
end;

procedure TDTable.FBFlushClick(Sender: TObject);
var
  I: Byte;
begin
  DTableService.ServiceMode := smFlush;
  DTableService.Database := Database;

  if (not Assigned(Tables)) then
  begin
    SetLength(DTableService.TableNames, 1);
    DTableService.TableNames[0] := NewTable.Name;
  end
  else
  begin
    SetLength(DTableService.TableNames, Tables.Count);
    for I := 0 to Tables.Count - 1 do
      DTableService.TableNames[I] := TCBaseTable(Tables[I]).Name;
  end;
  DTableService.Execute();

  FBFlush.Enabled := False;
  ActiveControl := FBCancel;

  FBCancel.Caption := Preferences.LoadStr(231);
end;

procedure TDTable.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDTable.FBOkCheckEnabled(Sender: TObject);
begin
  FBOk.Enabled := PageControl.Visible
    and (FName.Text <> '')
    and (not Assigned(Database.TableByName(FName.Text)) or (Assigned(Table) and (((Database.Client.LowerCaseTableNames = 0) and (FName.Text = Table.Name)) or ((Database.Client.LowerCaseTableNames > 0) and ((lstrcmpi(PChar(FName.Text), PChar(Table.Name)) = 0))))))
    and True;

  TSSource.TabVisible := Assigned(Table) and not Active;
end;

procedure TDTable.FBOptimizeClick(Sender: TObject);
var
  I: Byte;
begin
  DTableService.ServiceMode := smOptimize;
  DTableService.Database := Database;

  if (not Assigned(Tables)) then
  begin
    SetLength(DTableService.TableNames, 1);
    DTableService.TableNames[0] := NewTable.Name;
    if (DTableService.Execute()) then
      NewTable.UnusedSize := Database.BaseTableByName(DTableService.TableNames[0]).UnusedSize;
  end
  else
  begin
    SetLength(DTableService.TableNames, Tables.Count);
    for I := 0 to Tables.Count - 1 do
      DTableService.TableNames[I] := TCBaseTable(Tables[I]).Name;

    DTableService.Execute();
  end;

  TSExtrasShow(Sender);

  FBOptimize.Enabled := False;
  ActiveControl := FBCancel;

  FBCancel.Caption := Preferences.LoadStr(231);
end;

procedure TDTable.FCollationDropDown(Sender: TObject);
var
  I: Integer;
  J: Integer;
begin
  if (Assigned(Database.Client.Collations) and (FCollation.ItemIndex < 0)) then
    for I := 0 to Database.Client.Collations.Count - 1 do
      if ((lstrcmpi(PChar(Database.Client.Collations[I].Charset.Name), PChar(FDefaultCharset.Text)) = 0) and Database.Client.Collations[I].Default) then
        for J := 1 to FCollation.Items.Count - 1 do
          if (lstrcmpi(PChar(FCollation.Items[J]), PChar(Database.Client.Collations[I].Name)) = 0) then
            FCollation.ItemIndex := FCollation.Items.IndexOf(FCollation.Items[J]);
end;

procedure TDTable.FDefaultCharsetChange(Sender: TObject);
var
  Charset: TCCharset;
  I: Integer;
begin
  Charset := Database.Client.CharsetByName(FDefaultCharset.Text);

  FCollation.Items.Clear();
  FCollation.Items.Add('');
  if (Assigned(Database.Client.Collations)) then
    for I := 0 to Database.Client.Collations.Count - 1 do
      if (Assigned(Charset) and (Database.Client.Collations[I].Charset = Charset)) then
        FCollation.Items.Add(Database.Client.Collations[I].Name);

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FDefaultCharsetExit(Sender: TObject);
begin
  if (FDefaultCharset.Text = '') then
    FDefaultCharset.Text := NewTable.DefaultCharset;
end;

procedure TDTable.FEngineChange(Sender: TObject);
begin
  NewTable.Engine := Database.Client.EngineByName(Trim(FEngine.Text));

  TSForeignKeys.TabVisible := not Assigned(Tables) and Assigned(Database.Client.EngineByName(FEngine.Text)) and Database.Client.EngineByName(FEngine.Text).ForeignKeyAllowed;

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FFieldsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (ctText = Change) then
    FBOkCheckEnabled(Sender);
end;

procedure TDTable.FFieldsEnter(Sender: TObject);
begin
  FListSelectItem(FFields, FFields.Selected, Assigned(FFields.Selected));
end;

procedure TDTable.FFieldsRefresh(Sender: TObject);
var
  I: Integer;
  SelectedField: string;
begin
  SelectedField := '';
  if (Assigned(FFields.Selected)) then SelectedField := FFields.Selected.Caption;

  FFields.Items.Clear();
  TSFieldsShow(Sender);
  for I := 0 to FFields.Items.Count - 1 do
    if (FFields.Items.Item[I].Caption = SelectedField) then
    begin
      FFields.Selected := FFields.Items.Item[I];
      FFields.ItemFocused := FFields.Selected;

      if (Assigned(FFields.ItemFocused) and (FFields.ItemFocused.Position.Y > FFields.ClientHeight)) then
        FFields.Scroll(0, (FFields.ItemFocused.Index + 2) * (FFields.Items[1].Top - FFields.Items[0].Top) - (FFields.ClientHeight - GetSystemMetrics(SM_CYHSCROLL)));
    end;

  FIndices.Items.Clear();

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FForeignKeysChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (ctText = Change) then
    FBOkCheckEnabled(Sender);
end;

procedure TDTable.FForeignKeysEnter(Sender: TObject);
begin
  FListSelectItem(FForeignKeys, FForeignKeys.Selected, Assigned(FForeignKeys.Selected));
end;

procedure TDTable.FForeignKeysRefresh(Sender: TObject);
var
  I: Integer;
  SelectedField: string;
begin
  SelectedField := '';
  if (Assigned(FForeignKeys.Selected)) then SelectedField := FForeignKeys.Selected.Caption;

  FForeignKeys.Items.Clear();
  TSForeignKeysShow(Sender);
  for I := 0 to FForeignKeys.Items.Count - 1 do
    if (FForeignKeys.Items.Item[I].Caption = SelectedField) then
    begin
      FForeignKeys.Selected := FForeignKeys.Items.Item[I];
      FForeignKeys.ItemFocused := FForeignKeys.Selected;

      if (Assigned(FForeignKeys.ItemFocused) and (FForeignKeys.ItemFocused.Position.Y > FForeignKeys.ClientHeight)) then
        FForeignKeys.Scroll(0, (FForeignKeys.ItemFocused.Index + 2) * (FForeignKeys.Items[1].Top - FForeignKeys.Items[0].Top) - (FForeignKeys.ClientHeight - GetSystemMetrics(SM_CYHSCROLL)));
    end;

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FIndicesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (ctText = Change) then
    FBOkCheckEnabled(Sender);
end;

procedure TDTable.FIndicesEnter(Sender: TObject);
begin
  FListSelectItem(FIndices, FIndices.Selected, Assigned(FIndices.Selected));
end;

procedure TDTable.FIndicesRefresh(Sender: TObject);
var
  I: Integer;
  SelectedField: string;
begin
  SelectedField := '';
  if (Assigned(FIndices.Selected)) then SelectedField := FIndices.Selected.Caption;

  FIndices.Items.Clear();
  TSIndicesShow(Sender);
  for I := 0 to FIndices.Items.Count - 1 do
    if (FIndices.Items.Item[I].Caption = SelectedField) then
    begin
      FIndices.Selected := FIndices.Items.Item[I];
      FIndices.ItemFocused := FIndices.Selected;

      if (Assigned(FIndices.ItemFocused) and (FIndices.ItemFocused.Position.Y > FIndices.ClientHeight)) then
        FIndices.Scroll(0, (FIndices.ItemFocused.Index + 2) * (FIndices.Items[1].Top - FIndices.Items[0].Top) - (FIndices.ClientHeight - GetSystemMetrics(SM_CYHSCROLL)));
    end;

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FLinearClick(Sender: TObject);
begin
  NewTable.Partitions.Linear := FLinear.Checked;
end;

procedure TDTable.FLinearKeyPress(Sender: TObject; var Key: Char);
begin
  FLinearClick(Sender);
end;

procedure TDTable.FListDblClick(Sender: TObject);
var
  I: Integer;
  ListView: TListView;
begin
  ListView := TListView(Sender);
  if (Assigned(ListView)) and (Assigned(ListView.PopupMenu)) then
    for I := 0 to MList.Items.Count - 1 do
      if (ListView.PopupMenu.Items.Items[I].Default) and (ListView.PopupMenu.Items.Items[I].Enabled) then
        ListView.PopupMenu.Items.Items[I].Click();
end;

procedure TDTable.FListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  ListView: TListView;
  I: Integer;
  Page: TTabSheet;
begin
  Page := TTabSheet(PageControl.ActivePage);
  for I := 0 to PageControl.PageCount - 1 do
    if (PageControl.Pages[I].Visible and (PageControl.Pages[I] <> Page)) then
      Page := TTabSheet(PageControl.Pages[I]);

  ListView := TListView(Sender);

  aPCreateField.Enabled := not Selected and (Page = TSFields);
  aPDeleteField.Enabled := Selected and (ListView.SelCount >= 1) and (Item.ImageIndex = iiField) and (NewTable.Fields.Count > 1);
  aPEditField.Enabled := Selected and (ListView.SelCount = 1) and (Page = TSFields);
  aPCreateIndex.Enabled := not Selected and (Page = TSIndices);
  aPDeleteIndex.Enabled := Selected and (ListView.SelCount >= 1) and (Item.ImageIndex = iiIndex);
  aPEditIndex.Enabled := Selected and (ListView.SelCount = 1) and (Page = TSIndices);
  aPCreateForeignKey.Enabled := not Selected and (Page = TSForeignKeys);
  aPDeleteForeignKey.Enabled := Selected and (ListView.SelCount >= 1) and (Item.ImageIndex = iiForeignKey);
  aPEditForeignKey.Enabled := Selected and (ListView.SelCount = 1) and (Page = TSForeignKeys);
  aPCreateTrigger.Enabled := not Selected and (Page = TSTriggers);
  aPDeleteTrigger.Enabled := Selected and (ListView.SelCount >= 1) and (Item.ImageIndex = iiTrigger);
  aPEditTrigger.Enabled := Selected and (ListView.SelCount = 1) and (Page = TSTriggers);
  aPCreatePartition.Enabled := not Selected and (Page = TSPartitions);
  aPDeletePartition.Enabled := Selected and (ListView.SelCount >= 1);
  aPEditPartition.Enabled := Selected and (ListView.SelCount = 1) and (Page = TSPartitions);

  aPUp.Enabled := Selected and (ListView = FFields) and not ListView.Items[0].Selected and (NewTable.Database.Client.ServerVersion >= 40001);
  aPDown.Enabled := Selected and (ListView = FFields) and not ListView.Items[ListView.Items.Count - 1].Selected and (NewTable.Database.Client.ServerVersion >= 40001);

  ShowEnabledItems(MList.Items);

  mlDProperties.Default := mlDProperties.Enabled;
end;

procedure TDTable.FormClientEvent(const Event: TCClient.TEvent);
begin
  if ((Event.EventType = ceItemValid) and (Event.CItem = Table)) then
    Built()
  else if ((Event.EventType in [ceItemCreated, ceItemAltered]) and (Event.CItem is TCTable)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Client.ErrorCode <> 0)) then
  begin
    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end;
end;

procedure TDTable.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
  I64: Int64;
  UpdateTableNames: TStringList;
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
    if (not Assigned(Tables)) then
    begin
      NewTable.Name := Trim(FName.Text);
      NewTable.DefaultCharset := Trim(FDefaultCharset.Text);
      NewTable.Collation := Trim(FCollation.Text);
      if (not Assigned(Table) or (Trim(FComment.Text) <> SQLUnwrapStmt(NewTable.Comment))) then
        NewTable.Comment := Trim(FComment.Text);

      if (GRecords.Visible) then
      begin
        NewTable.RowType := TMySQLRowType(FRowType.ItemIndex);
        if (TryStrToInt64(FAutoIncrement.Text, I64)) then NewTable.AutoIncrement := I64;
      end
      else
      begin
        NewTable.RowType := mrUnknown;
        NewTable.AutoIncrement := 0;
      end;

      if (not Assigned(Table)) then
        CanClose := Database.AddTable(NewTable)
      else
        CanClose := Database.UpdateTable(Table, NewTable);

      if (Assigned(Table) or not CanClose) then
        FCreatedName := ''
      else
        FCreatedName := NewTable.Name;

      PageControl.Visible := CanClose or not Database.Client.Asynchron;
      PSQLWait.Visible := not PageControl.Visible;

      FBOk.Enabled := False;
    end
    else
    begin
      UpdateTableNames := TStringList.Create();
      for I := 0 to Tables.Count - 1 do
        UpdateTableNames.Add(TCBaseTable(Tables[I]).Name);

      CanClose := Database.UpdateTables(UpdateTableNames, FTablesCharset.Text, FTablesCollation.Text, FTablesEngine.Text, TMySQLRowType(FTablesRowType.ItemIndex));

      UpdateTableNames.Free();
    end;
end;

procedure TDTable.FormCreate(Sender: TObject);
begin
  Tables := nil;

  FFields.SmallImages := Preferences.SmallImages;
  FIndices.SmallImages := Preferences.SmallImages;
  FForeignKeys.SmallImages := Preferences.SmallImages;
  FTriggers.SmallImages := Preferences.SmallImages;
  FReferenced.SmallImages := Preferences.SmallImages;

  TBFields.Images := Preferences.SmallImages;
  TBIndices.Images := Preferences.SmallImages;
  TBForeignKeys.Images := Preferences.SmallImages;
//  TBPartitions.Images := Preferences.SmallImages;

  FSource.Highlighter := MainHighlighter;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Table.Width >= Width) and (Preferences.Table.Height >= Height)) then
  begin
    Width := Preferences.Table.Width;
    Height := Preferences.Table.Height;
  end;

  PageControl.ActivePage := nil; // TSInformationsShow soll nicht vorzeitig aufgerufen werden

  SetWindowLong(FAutoIncrement.Handle, GWL_STYLE, GetWindowLong(FAutoIncrement.Handle, GWL_STYLE) or ES_NUMBER);
  FFields.RowSelect := CheckWin32Version(6);
  FIndices.RowSelect := CheckWin32Version(6);
  FForeignKeys.RowSelect := CheckWin32Version(6);
  FReferenced.RowSelect := CheckWin32Version(6);
  FPartitions.RowSelect := CheckWin32Version(6);
end;

procedure TDTable.FormDestroy(Sender: TObject);
begin
  NewTable.Free();
end;

procedure TDTable.FormHide(Sender: TObject);
begin
  Database.Client.UnRegisterEventProc(FormClientEvent);

  PageControl.ActivePage := nil; // TSInformationsShow soll beim nächsten Öffnen nicht vorzeitig aufgerufen werden

  if (Assigned(NewTable)) then
    FreeAndNil(NewTable);
  FreeAndNil(Tables);

  Preferences.Table.Width := Width;
  Preferences.Table.Height := Height;

  FFields.Items.BeginUpdate();
  FFields.Items.Clear();
  FFields.Items.EndUpdate();
  FIndices.Items.BeginUpdate();
  FIndices.Items.Clear();
  FIndices.Items.EndUpdate();
  FForeignKeys.Items.BeginUpdate();
  FForeignKeys.Items.Clear();
  FForeignKeys.Items.EndUpdate();
  FTriggers.Items.BeginUpdate();
  FTriggers.Items.Clear();
  FTriggers.Items.EndUpdate();
  FReferenced.Items.BeginUpdate();
  FReferenced.Items.Clear();
  FReferenced.Items.EndUpdate();

  FSource.Lines.Clear();
end;

procedure TDTable.FormShow(Sender: TObject);
var
  I: Integer;
  NewColumn: TCIndexColumn;
  NewField: TCBaseTableField;
  NewIndex: TCIndex;
  TableName: string;
begin
  Database.Client.RegisterEventProc(FormClientEvent);

  if (not Assigned(Tables) and not Assigned(Table)) then
  begin
    Caption := Preferences.LoadStr(383);
    HelpContext := 1045;
  end
  else if (not Assigned(Table)) then
  begin
    Caption := Preferences.LoadStr(107);
    HelpContext := 1054;
  end
  else
  begin
    Caption := Preferences.LoadStr(842, Table.Name);
    HelpContext := 1054;
  end;

  RecordCount := -1;

  if (not Assigned(Table) and (Database.Client.LowerCaseTableNames = 1)) then
    FName.CharCase := ecLowerCase
  else
    FName.CharCase := ecNormal;

  FEngine.Items.Clear();
  for I := 0 to Database.Client.Engines.Count - 1 do
    if (not (Database.Client.Engines[I] is TCSystemEngine)) then
      FEngine.Items.Add(Database.Client.Engines[I].Name);

  FTablesEngine.Items.Clear();
  FTablesEngine.Items.Add('');
  for I := 0 to Database.Client.Engines.Count - 1 do
    FTablesEngine.Items.Add(Database.Client.Engines[I].Name);

  FDefaultCharset.Items.Clear();
  FDefaultCharset.Items.Add('');
  for I := 0 to Database.Client.Charsets.Count - 1 do
    FDefaultCharset.Items.Add(Database.Client.Charsets[I].Name);
  FDefaultCharset.Text := ''; FDefaultCharsetChange(Sender);

  FCollation.Text := '';

  FTablesCharset.Items.Text := FDefaultCharset.Items.Text;
  FTablesCharset.Text := ''; FTablesCharsetChange(Sender);
  FTablesCollation.Text := '';

  FPartitionType.ItemIndex := -1;


  FBOptimize.Enabled := True;
  FBCheck.Enabled := True;
  FBFlush.Enabled := True;

  if (not Assigned(Tables)) then
  begin
    NewTable := TCBaseTable.Create(Database.Tables);

    TSTable.TabVisible := not Assigned(Tables);
    TSTables.TabVisible := not TSTable.TabVisible;
    PageControl.ActivePage := TSTable;

    if (not Assigned(Table)) then
    begin
      NewTable.DefaultCharset := Database.DefaultCharset;
      NewTable.Collation := Database.Collation;
      NewTable.Engine := Database.Client.Engines.DefaultEngine;

      NewField := TCBaseTableField.Create(NewTable.Fields);
      NewField.Name := 'Id';
      NewField.FieldType := mfInt;
      NewField.Size := 11;
      NewField.Unsigned := False;
      NewField.NullAllowed := False;
      NewField.AutoIncrement := True;
      NewTable.Fields.AddField(NewField);
      FreeAndNil(NewField);

      NewIndex := TCIndex.Create(NewTable.Indices);
      NewIndex.Primary := True;

      NewColumn := TCIndexColumn.Create(NewIndex.Columns);
      NewColumn.Field := TCBaseTableField(NewTable.Fields[0]);
      NewIndex.Columns.AddColumn(NewColumn);
      FreeAndNil(NewColumn);

      NewTable.Indices.AddIndex(NewIndex);
      FreeAndNil(NewIndex);

      if (NewTable.Name = '') then
        FName.Text := Preferences.LoadStr(114)
      else
        FName.Text := NewTable.Name;
      while (Assigned(Database.TableByName(FName.Text))) do
      begin
        TableName := FName.Text;
        Delete(TableName, 1, Length(Preferences.LoadStr(114)));
        if (TableName = '') then TableName := '1';
        TableName := Preferences.LoadStr(114) + IntToStr(StrToInt(TableName) + 1);
        FName.Text := TableName;
      end;

      FDefaultCharset.ItemIndex := FDefaultCharset.Items.IndexOf(Database.DefaultCharset); FDefaultCharsetChange(Sender);
      FCollation.ItemIndex := FCollation.Items.IndexOf(Database.Collation);

      FComment.Text := '';

      if (not Assigned(NewTable.Engine)) then
        FEngine.ItemIndex := -1
      else
        FEngine.ItemIndex := FEngine.Items.IndexOf(NewTable.Engine.Name);
      FEngineChange(Sender);

      FRowType.ItemIndex := Integer(NewTable.RowType);
      FAutoIncrement.Visible := NewTable.AutoIncrement > 0; FLAutoIncrement.Visible := FAutoIncrement.Visible;
      FAutoIncrement.Text := IntToStr(NewTable.AutoIncrement);

      PageControl.Visible := True;
      PSQLWait.Visible := not PageControl.Visible;
    end
    else
    begin
      PageControl.Visible := not Table.Update();
      PSQLWait.Visible := not PageControl.Visible;

      if (PageControl.Visible) then
        Built();
    end;
  end
  else
  begin
    TSTable.TabVisible := (not Assigned(Tables));
    TSTables.TabVisible := not TSTable.TabVisible;
    PageControl.ActivePage := TSTables;
    PageControl.Visible := not Database.Client.Update(Tables);
    PSQLWait.Visible := not PageControl.Visible;

    if (PageControl.Visible) then
      Built();
  end;

  DTableService.Parent := TForm(Self);


  FDefaultCharset.Visible := Database.Client.ServerVersion >= 40101; FLDefaultCharset.Visible := FDefaultCharset.Visible;
  FCollation.Visible := Database.Client.ServerVersion >= 40101; FLCollation.Visible := FCollation.Visible;
  FTablesCharset.Visible := Database.Client.ServerVersion >= 40101; FLTablesCharset.Visible := FTablesCharset.Visible;
  FTablesCollation.Visible := Database.Client.ServerVersion >= 40101; FLTablesCollation.Visible := FTablesCollation.Visible;
  GRecords.Visible := Assigned(Table);

  TSInformations.TabVisible := Assigned(Table) and (Table.DataSize >= 0) or Assigned(Tables);
  TSFields.TabVisible := not Assigned(Tables);
  TSIndices.TabVisible := not Assigned(Tables);
  TSTriggers.TabVisible := Assigned(Table)  and Assigned(Database.Triggers);
  TSReferenced.TabVisible := Assigned(Table) and Assigned(NewTable.Engine) and (UpperCase(NewTable.Engine.Name) = 'INNODB');
  TSPartitions.TabVisible := not Assigned(Tables) and Assigned(NewTable.Partitions);
  TSExtras.TabVisible := Assigned(Table) or Assigned(Tables);
  TSSource.TabVisible := Assigned(Table) or Assigned(Tables);

  FBOk.Enabled := PageControl.Visible and not Assigned(Tables) and not Assigned(Table);
  FBCancel.Caption := Preferences.LoadStr(30);

  ActiveControl := FBCancel;
  if (PageControl.Visible) then
    if (not Assigned(Tables)) then
      ActiveControl := FName
    else
      ActiveControl := FTablesEngine;
end;

procedure TDTable.FPartitionCountChange(Sender: TObject);
begin
  FPartitionExpr.Enabled := FPartitions.Items.Count = 0;
  FLPartitionExpr.Enabled := FPartitionExpr.Enabled;

  FPartitionType.Enabled := FPartitions.Items.Count = 0;
  FLPartitionType.Enabled := FPartitionType.Enabled;

  FPartitionCount.Enabled := FPartitions.Items.Count = 0;
  FUDPartitionCount.Enabled := FPartitionCount.Enabled;
  FLPartitionCount.Enabled := FPartitionCount.Enabled;
end;

procedure TDTable.FPartitionExprChange(Sender: TObject);
begin
  NewTable.Partitions.Expression := FPartitionExpr.Text;
end;

procedure TDTable.FPartitionsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  FPartitionCountChange(Sender);

  FUDPartitionCount.Position := FPartitions.Items.Count;
end;

procedure TDTable.FPartitionsRefresh(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
  SelectedPartition: string;
begin
  if (not Assigned(FPartitions.Selected)) then
    SelectedPartition := ''
  else
    SelectedPartition := FPartitions.Selected.Caption;

  FPartitions.Items.Clear();
  if (NewTable.Partitions.PartitionType in [ptRange, ptList]) then
    for I := 0 to NewTable.Partitions.Count - 1 do
    begin
      Item := FPartitions.Items.Add();
      Item.ImageIndex := iiPartition;
      Item.Caption := NewTable.Partitions[I].Name;
      Item.SubItems.Add(NewTable.Partitions[I].ValuesExpr);
      if (NewTable.Partitions[I].MinRows < 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(IntToStr(NewTable.Partitions[I].MinRows));
      if (NewTable.Partitions[I].MaxRows < 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(IntToStr(NewTable.Partitions[I].MaxRows));
      Item.SubItems.Add(NewTable.Partitions[I].Comment);
    end;

  for I := 0 to FPartitions.Items.Count - 1 do
    if (FPartitions.Items.Item[I].Caption = SelectedPartition) then
    begin
      FPartitions.Selected := FPartitions.Items.Item[I];
      FPartitions.ItemFocused := FPartitions.Selected;

      if (Assigned(FPartitions.ItemFocused) and (FPartitions.ItemFocused.Position.Y > FPartitions.ClientHeight)) then
        FPartitions.Scroll(0, (FPartitions.ItemFocused.Index + 2) * (FPartitions.Items[1].Top - FPartitions.Items[0].Top) - (FPartitions.ClientHeight - GetSystemMetrics(SM_CYHSCROLL)));
    end;

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FPartitionTypeChange(Sender: TObject);
begin
  case (FPartitionType.ItemIndex) of
    0: NewTable.Partitions.PartitionType := ptHash;
    1: NewTable.Partitions.PartitionType := ptKey;
    2: NewTable.Partitions.PartitionType := ptRange;
    3: NewTable.Partitions.PartitionType := ptList;
    else NewTable.Partitions.PartitionType := ptUnknown;
  end;

  FLinear.Enabled := NewTable.Partitions.PartitionType in [ptHash, ptKey];

  FPartitionExpr.Text := '';

  FPartitions.Items.Clear(); FPartitionsChange(Sender, nil, ctState);
  FPartitions.Enabled := FPartitionType.ItemIndex in [2, 3];
  FLPartitions.Enabled := FPartitions.Enabled;
end;

procedure TDTable.FTablesCharsetChange(Sender: TObject);
var
  Charset: TCCharset;
  I: Integer;
begin
  Charset := Database.Client.CharsetByName(FDefaultCharset.Text);

  FTablesCollation.Items.Clear();
  FTablesCollation.Items.Add('');
  if (Assigned(Database.Client.Collations)) then
    for I := 0 to Database.Client.Collations.Count - 1 do
      if (Assigned(Charset) and (Database.Client.Collations[I].Charset = Charset)) then
        FTablesCollation.Items.Add(Database.Client.Collations[I].Name);

  FBOkCheckEnabled(Sender);
end;

procedure TDTable.FTriggersChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (ctText = Change) then
    FBOkCheckEnabled(Sender);
end;

procedure TDTable.FTriggersEnter(Sender: TObject);
begin
  FListSelectItem(FTriggers, FTriggers.Selected, Assigned(FTriggers.Selected));
end;

procedure TDTable.msCopyClick(Sender: TObject);
begin
  try
    FSource.CopyToClipboard();
  except
    on E: Exception do
      raise Exception.Create(E.Message + ' - ' + SysErrorMessage(GetLastError()));
  end;
end;

procedure TDTable.TSExtrasShow(Sender: TObject);
var
  DateTime: TDateTime;
  I: Integer;
  Size: Int64;
begin
  if (not Assigned(Tables)) then
  begin
    FUnusedSize.Caption := SizeToStr(NewTable.UnusedSize);

    if (NewTable.Checked <= 0) then
      FChecked.Caption := '???'
    else
      FChecked.Caption := SysUtils.DateTimeToStr(NewTable.Checked, LocaleFormatSettings);
  end
  else
  begin
    Size := 0;
    for I := 0 to Tables.Count - 1 do
      Inc(Size, TCBaseTable(Tables[I]).UnusedSize);
    FUnusedSize.Caption := SizeToStr(Size);

    DateTime := Now();
    for I := 0 to Tables.Count - 1 do
      if (TCBaseTable(Tables[I]).Checked < DateTime) then
        DateTime := TCBaseTable(Tables[I]).Checked;
    if (DateTime <= 0) then
      FChecked.Caption := '???'
    else
      FChecked.Caption := SysUtils.DateTimeToStr(DateTime, LocaleFormatSettings);
  end;
end;

procedure TDTable.TSFieldsShow(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
  S: string;
  TempOnChange: TLVChangeEvent;
begin
  TempOnChange := FFields.OnChange;
  FFields.OnChange := nil;

  mlDCreate.Action := aPCreateField;
  mlDDelete.Action := aPDeleteField;
  mlDProperties.Action := aPEditField;

  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(559);
  mlDCreate.ShortCut := VK_INSERT;
  mlDDelete.ShortCut := VK_DELETE;

  if (FFields.Items.Count = 0) then
    for I := 0 to NewTable.Fields.Count - 1 do
    begin
      ListItem := FFields.Items.Add();
      ListItem.Caption := NewTable.Fields[I].Name;
      ListItem.SubItems.Add(NewTable.Fields[I].DBTypeStr());
      if (NewTable.Fields[I].NullAllowed) then
        ListItem.SubItems.Add(Preferences.LoadStr(74))
      else
        ListItem.SubItems.Add(Preferences.LoadStr(75));
      if (NewTable.Fields[I].AutoIncrement) then
        ListItem.SubItems.Add('<auto_increment>')
      else if (NewTable.Fields[I].Default = 'NULL') then
        ListItem.SubItems.Add('<' + Preferences.LoadStr(71) + '>')
      else if (NewTable.Fields[I].Default = 'CURRENT_TIMESTAMP') then
        ListItem.SubItems.Add('<INSERT-TimeStamp>')
      else
        ListItem.SubItems.Add(NewTable.Fields[I].UnescapeValue(NewTable.Fields[I].Default));
      S := NewTable.Fields[I].Charset;
      if (NewTable.Fields[I].Collation <> '') then
      begin
        if (S <> '') then S := S + ', ';
        S := S + NewTable.Fields[I].Collation;
      end;
      ListItem.SubItems.Add(S);
      ListItem.SubItems.Add(NewTable.Fields[I].Comment);

      ListItem.ImageIndex := iiField;
    end;

  FListSelectItem(FFields, FFields.Selected, Assigned(FFields.Selected));

  FFields.OnChange := TempOnChange;
end;

procedure TDTable.TSForeignKeysShow(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
  S: string;
  S2: string;
  TempOnChange: TLVChangeEvent;
begin
  TempOnChange := FForeignKeys.OnChange;
  FForeignKeys.OnChange := nil;

  mlDCreate.Action := aPCreateForeignKey;
  mlDDelete.Action := aPDeleteForeignKey;
  mlDProperties.Action := aPEditForeignKey;

  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(559);

  mlDCreate.ShortCut := VK_INSERT;
  mlDDelete.ShortCut := VK_DELETE;

  if (FForeignKeys.Items.Count = 0) then
    for I := 0 to NewTable.ForeignKeys.Count - 1 do
    begin
      ListItem := FForeignKeys.Items.Add();
      ListItem.Caption := NewTable.ForeignKeys[I].Name;
      ListItem.SubItems.Add(NewTable.ForeignKeys[I].DBTypeStr());

      S := '';
      if (NewTable.ForeignKeys[I].OnDelete = dtCascade) then S := 'cascade on delete';
      if (NewTable.ForeignKeys[I].OnDelete = dtSetNull) then S := 'set NULL on delete';
      if (NewTable.ForeignKeys[I].OnDelete = dtSetDefault) then S := 'set default on delete';
      if (NewTable.ForeignKeys[I].OnDelete = dtNoAction) then S := 'no action on delete';

      S2 := '';
      if (NewTable.ForeignKeys[I].OnUpdate = utCascade) then S2 := 'cascade on update';
      if (NewTable.ForeignKeys[I].OnUpdate = utSetNull) then S2 := 'set NULL on update';
      if (NewTable.ForeignKeys[I].OnUpdate = utSetDefault) then S2 := 'set default on update';
      if (NewTable.ForeignKeys[I].OnUpdate = utNoAction) then S2 := 'no action on update';

      if (S <> '') and (S2 <> '') then S := S + ', ';
      S := S + S2;
      ListItem.SubItems.Add(S);

      ListItem.ImageIndex := iiForeignKey;
    end;

  FListSelectItem(FForeignKeys, FForeignKeys.Selected, Assigned(FForeignKeys.Selected));

  FForeignKeys.OnChange := TempOnChange;
end;

procedure TDTable.TSIndicesShow(Sender: TObject);
var
  FieldNames: string;
  I: Integer;
  J: Integer;
  ListItem: TListItem;
  TempOnChange: TLVChangeEvent;
begin
  TempOnChange := FIndices.OnChange;
  FIndices.OnChange := nil;

  mlDCreate.Action := aPCreateIndex;
  mlDDelete.Action := aPDeleteIndex;
  mlDProperties.Action := aPEditIndex;

  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(559);
  mlDCreate.ShortCut := VK_INSERT;
  mlDDelete.ShortCut := VK_DELETE;

  if (FIndices.Items.Count = 0) then
    for I := 0 to NewTable.Indices.Count - 1 do
    begin
      ListItem := FIndices.Items.Add();
      ListItem.Caption := NewTable.Indices.Index[I].Caption;
      FieldNames := '';
      for J := 0 to NewTable.Indices.Index[I].Columns.Count - 1 do
        begin if (FieldNames <> '') then FieldNames := FieldNames + ','; FieldNames := FieldNames + NewTable.Indices.Index[I].Columns.Column[J].Field.Name; end;
      ListItem.SubItems.Add(FieldNames);
      if (NewTable.Indices.Index[I].Unique) then
        ListItem.SubItems.Add('unique')
      else if (NewTable.Indices.Index[I].Fulltext) then
        ListItem.SubItems.Add('fulltext')
      else
        ListItem.SubItems.Add('');
      ListItem.ImageIndex := iiIndex;
    end;

  FListSelectItem(FIndices, FIndices.Selected, Assigned(FIndices.Selected));

  FIndices.OnChange := TempOnChange;
end;

procedure TDTable.TSInformationsShow(Sender: TObject);
var
  DateTime: TDateTime;
  I: Integer;
  Size: Int64;
begin
  FCreated.Caption := '???';
  FUpdated.Caption := '???';
  FIndexSize.Caption := '???';
  FDataSize.Caption := '???';
  FMaxDataSize.Caption := '???';
  FRecordCount.Caption := '???';

  if (not Assigned(Tables)) then
  begin
    if (NewTable.Created = 0) then FCreated.Caption := '???' else FCreated.Caption := SysUtils.DateTimeToStr(NewTable.Created, LocaleFormatSettings);
    if (NewTable.Updated = 0) then FUpdated.Caption := '???' else FUpdated.Caption := SysUtils.DateTimeToStr(NewTable.Updated, LocaleFormatSettings);

    FIndexSize.Caption := SizeToStr(NewTable.IndexSize);
    FDataSize.Caption := SizeToStr(NewTable.DataSize);
    FMaxDataSize.Visible := NewTable.MaxDataSize > 0; FLMaxDataSize.Visible := FMaxDataSize.Visible;
    if (FMaxDataSize.Visible) then
      FMaxDataSize.Caption := SizeToStr(NewTable.MaxDataSize);

    if (RecordCount < 0) then RecordCount := NewTable.CountRecords();

    FRecordCount.Caption := FormatFloat('#,##0', RecordCount, LocaleFormatSettings);
  end
  else
  begin
    DateTime := Now();
    for I := 0 to Tables.Count - 1 do
      if (TCBaseTable(Tables[I]).Created < DateTime) then
        DateTime := TCBaseTable(Tables[I]).Created;
    if (DateTime <= 0) then FCreated.Caption := '???' else FCreated.Caption := SysUtils.DateTimeToStr(DateTime, LocaleFormatSettings);

    DateTime := 0;
    for I := 0 to Tables.Count - 1 do
      if (TCBaseTable(Tables[I]).Updated > DateTime) then
        DateTime := TCBaseTable(Tables[I]).Updated;
    if (DateTime = 0) then FUpdated.Caption := '???' else FUpdated.Caption := SysUtils.DateTimeToStr(DateTime, LocaleFormatSettings);

    Size := 0;
    for I := 0 to Tables.Count - 1 do
      Inc(Size, TCBaseTable(Tables[I]).IndexSize);
    FIndexSize.Caption := SizeToStr(Size);

    Size := 0;
    for I := 0 to Tables.Count - 1 do
      Inc(Size, TCBaseTable(Tables[I]).DataSize);
    FDataSize.Caption := SizeToStr(Size);

    if (RecordCount < 0) then
    begin
      RecordCount := 0;
      for I := 0 to Tables.Count - 1 do
        Inc(RecordCount, TCBaseTable(Tables[I]).CountRecords());
    end;
    FRecordCount.Caption := FormatFloat('#,##0', RecordCount, LocaleFormatSettings);
  end;
end;

procedure TDTable.TSPartitionsShow(Sender: TObject);
begin
  mlDCreate.Action := aPCreatePartition;
  mlDDelete.Action := aPDeletePartition;
  mlDProperties.Action := aPEditPartition;

  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(559);
  mlDCreate.ShortCut := VK_INSERT;
  mlDDelete.ShortCut := VK_DELETE;

  if (FPartitionType.ItemIndex < 0) then
  begin
    FPartitionType.OnChange := nil;
    FPartitionExpr.OnChange := nil;

    case (NewTable.Partitions.PartitionType) of
      ptHash: FPartitionType.ItemIndex := 0;
      ptKey: FPartitionType.ItemIndex := 1;
      ptRange: FPartitionType.ItemIndex := 2;
      ptList: FPartitionType.ItemIndex := 3;
      else FPartitionType.ItemIndex := -1
    end; FPartitionTypeChange(Sender);
    FLinear.Checked := NewTable.Partitions.Linear;

    FPartitionExpr.Text := NewTable.Partitions.Expression;
    FUDPartitionCount.Position := NewTable.Partitions.Count;

    FPartitionsRefresh(Sender);

    FPartitionType.OnChange := FPartitionTypeChange;
    FPartitionExpr.OnChange := FPartitionExprChange;
  end;

  FListSelectItem(FPartitions, FPartitions.Selected, Assigned(FPartitions.Selected));
end;

procedure TDTable.TSReferencedShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  K: Integer;
  ListItem: TListItem;
  S: string;
  S2: string;
  Source: TCBaseTable;
  TempOnChange: TLVChangeEvent;
begin
  TempOnChange := FReferenced.OnChange;
  FReferenced.OnChange := nil;

  mlDCreate.Action := aPCreateForeignKey;
  mlDDelete.Action := aPDeleteForeignKey;
  mlDProperties.Action := aPEditForeignKey;

  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(559);

  mlDCreate.ShortCut := VK_INSERT;
  mlDDelete.ShortCut := VK_DELETE;

  if (Assigned(Table) and (FReferenced.Items.Count = 0)) then
    for I := 0 to NewTable.Database.Tables.Count - 1 do
      if ((NewTable.Database.Tables[I] is TCBaseTable) and (NewTable.Database.Tables[I].Name <> NewTable.Name)) then
      begin
        Source := TCBaseTable(NewTable.Database.Tables[I]);

        for J := 0 to Source.ForeignKeys.Count - 1 do
          if (Source.ForeignKeys[J].Parent.TableName = NewTable.Name) then
          begin
            ListItem := FReferenced.Items.Add();
            ListItem.Caption := Source.Name + '.' + Source.ForeignKeys[J].Name;
            S := '';
            if (Source.Name <> NewTable.Name) then
              S := S + Source.Name + '.';
            if (Length(Source.ForeignKeys[J].Fields) > 1) then S := S + '(';
            for K := 0 to Length(Source.ForeignKeys[J].Fields) - 1 do
            begin
              if (K > 0) then S := S + ', ';
              S := S + Source.ForeignKeys[J].Fields[K].Name;
            end;
            if (Length(Source.ForeignKeys[J].Fields) > 1) then S := S + ')';
            S := S + ' -> ';
            if (Length(Source.ForeignKeys[J].Fields) > 1) then S := S + '(';
            for K := 0 to Length(Source.ForeignKeys[J].Parent.FieldNames) - 1 do
            begin
              if (K > 0) then S := S + ', ';
                S := S + Source.ForeignKeys[J].Parent.FieldNames[K];
            end;
            if (Length(Source.ForeignKeys[J].Fields) > 1) then S := S + ')';
            ListItem.SubItems.Add(S);

            S := '';
            if (Source.ForeignKeys[J].OnDelete = dtCascade) then S := 'cascade on delete';
            if (Source.ForeignKeys[J].OnDelete = dtSetNull) then S := 'set nil on delete';
            if (Source.ForeignKeys[J].OnDelete = dtSetDefault) then S := 'set default on delete';
            if (Source.ForeignKeys[J].OnDelete = dtNoAction) then S := 'no action on delete';

            S2 := '';
            if (Source.ForeignKeys[J].OnUpdate = utCascade) then S2 := 'cascade on update';
            if (Source.ForeignKeys[J].OnUpdate = utSetNull) then S2 := 'set nil on update';
            if (Source.ForeignKeys[J].OnUpdate = utSetDefault) then S2 := 'set default on update';
            if (Source.ForeignKeys[J].OnUpdate = utNoAction) then S2 := 'no action on update';
            if (S <> '') and (S2 <> '') then S := S + ', ';

            ListItem.SubItems.Add(S + S2);
            ListItem.ImageIndex := iiForeignKey;
          end;
      end;

  FListSelectItem(FReferenced, FReferenced.Selected, Assigned(FReferenced.Selected));

  FReferenced.OnChange := TempOnChange;
end;

procedure TDTable.TSSourceShow(Sender: TObject);
var
  I: Integer;
begin
  if (FSource.Lines.Count = 0) then
    if (not Assigned(Tables) and Assigned(NewTable)) then
      FSource.Lines.Text := NewTable.Source + #13#10
    else if (Assigned(Tables)) then
      for I := 0 to Tables.Count - 1 do
      begin
        if (I > 0) then FSource.Lines.Text := FSource.Lines.Text + #13#10;
        FSource.Lines.Text := FSource.Lines.Text + TCBaseTable(Tables[I]).Source + #13#10;
      end;
end;

procedure TDTable.TSTriggersShow(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
  S: string;
  TempOnChange: TLVChangeEvent;
begin
  TempOnChange := FTriggers.OnChange;
  FTriggers.OnChange := nil;

  mlDCreate.Action := aPCreateTrigger;
  mlDDelete.Action := aPDeleteTrigger;
  mlDProperties.Action := aPEditTrigger;

  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(559);
  mlDCreate.ShortCut := VK_INSERT;
  mlDDelete.ShortCut := VK_DELETE;

  if (FTriggers.Items.Count = 0) then
    for I := 0 to NewTable.Database.Triggers.Count - 1 do
      if (NewTable.Database.Triggers[I].TableName = NewTable.Name) then
      begin
        ListItem := FTriggers.Items.Add();
        ListItem.ImageIndex := iiTrigger;
        ListItem.Caption := NewTable.Database.Triggers[I].Name;
        S := '';
        case (NewTable.Database.Triggers[I].Timing) of
          ttBefore: S := S + 'before ';
          ttAfter: S := S + 'after ';
        end;
        case (NewTable.Database.Triggers[I].Event) of
          teInsert: S := S + 'insert';
          teUpdate: S := S + 'update';
          teDelete: S := S + 'delete';
        end;
        ListItem.SubItems.Add(S);
      end;

  FListSelectItem(FTriggers, FTriggers.Selected, Assigned(FTriggers.Selected));

  FTriggers.OnChange := TempOnChange;
end;

initialization
  FTable := nil;
end.
