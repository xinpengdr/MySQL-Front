unit fDIndex;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ComCtrls, StdCtrls, ToolWin, ExtCtrls,
  Forms_Ext, ComCtrls_Ext,
  fClient, fPreferences, fBase, StdCtrls_Ext;

type
  TDIndex = class (TForm_Ext)
    FAvailableFields: TListView;
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FFulltext: TCheckBox;
    FIndexedFields: TListView;
    FLAvailableFields: TLabel;
    FLength: TEdit;
    FLengthUD: TUpDown;
    FLIndexedFields: TLabel;
    FLLength: TLabel;
    FLName: TLabel;
    FName: TEdit;
    FOther: TRadioButton;
    FPrimary: TRadioButton;
    FUnique: TCheckBox;
    GAttributes: TGroupBox_Ext;
    GBasics: TGroupBox_Ext;
    Panel: TPanel;
    PSQLWait: TPanel;
    tbAddAll: TToolButton;
    tbAddOne: TToolButton;
    tbDown: TToolButton;
    tbRemoveAll: TToolButton;
    tbRemoveOne: TToolButton;
    tbUp: TToolButton;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
    procedure FAvailableFieldsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FAvailableFieldsDeletion(Sender: TObject; Item: TListItem);
    procedure FAvailableFieldsEnter(Sender: TObject);
    procedure FAvailableFieldsExit(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FFulltextClick(Sender: TObject);
    procedure FIndexedFieldsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FIndexedFieldsDeletion(Sender: TObject; Item: TListItem);
    procedure FIndexedFieldsEnter(Sender: TObject);
    procedure FIndexedFieldsExit(Sender: TObject);
    procedure FLengthExit(Sender: TObject);
    procedure FNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FUniqueClick(Sender: TObject);
    procedure IndexTypeChange(Sender: TObject);
    procedure tbAddAllClick(Sender: TObject);
    procedure tbAddOneClick(Sender: TObject);
    procedure tbRemoveAllClick(Sender: TObject);
    procedure tbRemoveOneClick(Sender: TObject);
    procedure tbUpDownClick(Sender: TObject);
  private
    Lengths: array of Integer;
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Database: TCDatabase;
    Index: TCIndex;
    Table: TCBaseTable;
    function Execute(): Boolean;
  end;

function DIndex(): TDIndex;

implementation {***************************************************************}

{$R *.dfm}

uses
  MySQLDB;

var
  FIndex: TDIndex;

function DIndex(): TDIndex;
begin
  if (not Assigned(FIndex)) then
  begin
    Application.CreateForm(TDIndex, FIndex);
    FIndex.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FIndex;
end;

{ TDIndex *********************************************************************}

procedure TDIndex.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiIndex, Icon);

  PSQLWait.Caption := Preferences.LoadStr(882);

  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FPrimary.Caption := Preferences.LoadStr(154);
  FLIndexedFields.Caption := Preferences.LoadStr(155) + ':';
  FLLength.Caption := Preferences.LoadStr(630) + ':';
  FLAvailableFields.Caption := Preferences.LoadStr(156) + ':';

  GAttributes.Caption := Preferences.LoadStr(157);
  FUnique.Caption := Preferences.LoadStr(158);
  FFulltext.Caption := Preferences.LoadStr(159);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30)
end;

function TDIndex.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDIndex.FAvailableFieldsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  tbAddAll.Enabled := FAvailableFields.Items.Count > 0;
  tbAddOne.Enabled := Item.Selected;

  FAvailableFields.Enabled := FAvailableFields.Items.Count > 0; FLAvailableFields.Enabled := FAvailableFields.Enabled;
end;

procedure TDIndex.FAvailableFieldsDeletion(Sender: TObject; Item: TListItem);
begin
  FAvailableFields.Enabled := FAvailableFields.Items.Count > 1; FLAvailableFields.Enabled := FAvailableFields.Enabled;
  tbAddAll.Enabled := FAvailableFields.Enabled;
end;

procedure TDIndex.FAvailableFieldsEnter(Sender: TObject);
begin
  FAvailableFieldsChange(Sender, FAvailableFields.Selected, ctState);
end;

procedure TDIndex.FAvailableFieldsExit(Sender: TObject);
begin
  tbAddAll.Enabled := False;
  tbAddOne.Enabled := False;
end;

procedure TDIndex.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDIndex.FBOkCheckEnabled(Sender: TObject);
var
  I: Integer;
begin
  FBOk.Enabled := (FIndexedFields.Items.Count > 0)
    and (not FLength.Enabled or (FLengthUD.Position > 0))
    and True;

  if (not Assigned(Index) and Visible) then
    if (FPrimary.Checked or (FName.Text = 'PRIMARY')) then
      FBOk.Enabled := FBOk.Enabled and ((Table.Indices.Count = 0) or Table.Indices[0].Primary)
    else
      for I := 0 to Table.Indices.Count - 1 do
        if (not Table.Indices[I].Primary and (lstrcmpi(PChar(Table.Indices[I].Name), PChar(FName.Text)) = 0)) then
          FBOk.Enabled := False;
end;

procedure TDIndex.FFulltextClick(Sender: TObject);
begin
  if (FFulltext.Checked) then
    FUnique.Checked := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDIndex.FIndexedFieldsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  Field: TCBaseTableField;
  I: Integer;
begin
  if (Visible and Assigned(Item) and Assigned(Table.FieldByName(Item.Caption))) then
  begin
    FIndexedFields.Enabled := (FIndexedFields.Items.Count > 0);
    FLIndexedFields.Enabled := FIndexedFields.Enabled;

    Field := Table.FieldByName(Item.Caption);

    FLength.Enabled := Field.FieldType in [mfChar, mfVarChar, mfTinyText, mfText, mfMediumText, mfLongText, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob, mfPoint];
    FLLength.Enabled := FLength.Enabled;
    FLengthUD.Enabled := FLength.Enabled;

    if (Field.FieldType in [mfChar, mfVarChar]) then
      FLengthUD.Max := Field.Size
    else
      FLengthUD.Max := 255;

    if (Table.Fields.IndexOf(Field) < Length(Lengths)) then
      FLengthUD.Position := Lengths[Table.Fields.IndexOf(Field)]
    else
      FLengthUD.Position := 0;

    if (FLengthUD.Position = 0) then
      FLength.Text := '';

    FFulltext.Enabled := not Assigned(Table.Engine) or (UpperCase(Table.Engine.Name) = 'MYISAM') and (FIndexedFields.Items.Count > 0) and (Table.Database.Client.ServerVersion >= 32323);
  end
  else
    FLength.Enabled := False;

  for I := 0 to FIndexedFields.Items.Count - 1 do
    FFulltext.Enabled := FFulltext.Enabled and Assigned(Table) and (Table.FieldByName(FIndexedFields.Items[I].Caption).FieldType in [mfChar, mfVarChar, mfTinyText, mfText, mfMediumText, mfLongText]);
  FFulltext.Checked := FFulltext.Enabled and FFulltext.Checked;

  tbUp.Enabled := Assigned(FIndexedFields.Selected) and (FIndexedFields.Items.IndexOf(Item) > 0);
  tbDown.Enabled := Assigned(FIndexedFields.Selected) and (FIndexedFields.Items.IndexOf(Item) + 1 < FIndexedFields.Items.Count);

  tbRemoveAll.Enabled := (FIndexedFields.Items.Count > 0);
  tbRemoveOne.Enabled := Assigned(FIndexedFields.Selected);

  FBOkCheckEnabled(Sender);
end;

procedure TDIndex.FIndexedFieldsDeletion(Sender: TObject; Item: TListItem);
begin
  tbRemoveAll.Enabled := FIndexedFields.Enabled;
  FBOkCheckEnabled(Sender);
end;

procedure TDIndex.FIndexedFieldsEnter(Sender: TObject);
begin
  FIndexedFieldsChange(Sender, FIndexedFields.Selected, ctState);
end;

procedure TDIndex.FIndexedFieldsExit(Sender: TObject);
begin
  tbUp.Enabled := False;
  tbDown.Enabled := False;

  tbRemoveAll.Enabled := False;
  tbRemoveOne.Enabled := False;
end;

procedure TDIndex.FLengthExit(Sender: TObject);
begin
  if (Assigned(FIndexedFields.Selected)) then
    Lengths[Table.Fields.IndexOf(Table.FieldByName(FIndexedFields.Selected.Caption))] := FLengthUD.Position;
end;

procedure TDIndex.FNameChange(Sender: TObject);
begin
  if (FName.Text <> '') then
    FOther.Checked := True
  else if (FPrimary.Enabled) then
    FPrimary.Checked := True;

  FBOkCheckEnabled(Sender);
end;

procedure TDIndex.FormClientEvent(const Event: TCClient.TEvent);
begin
  if ((Event.EventType = ceItemAltered) and (Event.CItem = Table)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Client.ErrorCode <> 0)) then
  begin
    GBasics.Visible := True;
    GAttributes.Visible := GBasics.Visible;
    PSQLWait.Visible := not GBasics.Visible;
  end;
end;

procedure TDIndex.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
  NewColumn: TCIndexColumn;
  NewIndex: TCIndex;
  NewTable: TCBaseTable;
begin
  FLengthExit(Sender);

  if ((ModalResult = mrOk) and GBasics.Visible) then
  begin
    NewIndex := TCIndex.Create(Table.Indices);
    if (Assigned(Index)) then
      NewIndex.Assign(Index);

    NewIndex.Primary := FPrimary.Checked;
    if (not NewIndex.Primary) then
      NewIndex.Name := Trim(FName.Text);

    NewIndex.Columns.Clear();
    for I := 0 to FIndexedFields.Items.Count - 1 do
    begin
      NewColumn := TCIndexColumn.Create(NewIndex.Columns);
      NewColumn.Field := Table.FieldByName(FIndexedFields.Items[I].Caption);
      NewColumn.Length := Lengths[Table.Fields.IndexOf(NewColumn.Field)];
      NewIndex.Columns.AddColumn(NewColumn);
      FreeAndNil(NewColumn);
    end;

    NewIndex.Unique := FUnique.Checked;
    NewIndex.Fulltext := FFulltext.Checked;

    if (not Assigned(Database)) then
    begin
      if (not Assigned(Index)) then
        Table.Indices.AddIndex(NewIndex)
      else
        Table.Indices[Index.Index].Assign(NewIndex);
    end
    else
    begin
      NewTable := TCBaseTable.Create(Database.Tables);
      NewTable.Assign(Table);

      if (not Assigned(Index)) then
        NewTable.Indices.AddIndex(NewIndex)
      else
        NewTable.Indices[Index.Index].Assign(NewIndex);

      CanClose := Database.UpdateTable(Table, NewTable);

      NewTable.Free();

      GBasics.Visible := CanClose or not Database.Client.Asynchron;
      GAttributes.Visible := GBasics.Visible;
      PSQLWait.Visible := not GBasics.Visible;
      if (PSQLWait.Visible) then
        ModalResult := mrNone;

      FBOk.Enabled := False;
    end;

    NewIndex.Free();
  end;
end;

procedure TDIndex.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Index.Width >= Width) and (Preferences.Index.Height >= Height)) then
  begin
    Width := Preferences.Index.Width;
    Height := Preferences.Index.Height;
  end;

  ToolBar1.Images := Preferences.SmallImages;
  ToolBar2.Images := Preferences.SmallImages;
  ToolBar3.Images := Preferences.SmallImages;
  ToolBar4.Images := Preferences.SmallImages;
  ToolBar5.Images := Preferences.SmallImages;
  ToolBar6.Images := Preferences.SmallImages;

  Panel.Left := GBasics.Width div 2 - Panel.Width div 2;
  Panel.Top := FAvailableFields.Top;
  Panel.Height := FAvailableFields.Height;
end;

procedure TDIndex.FormHide(Sender: TObject);
begin
  Database.Client.UnRegisterEventProc(FormClientEvent);

  Preferences.Index.Width := Width;
  Preferences.Index.Height := Height;

  SetLength(Lengths, 0);
  Table := nil;

  FIndexedFields.Items.Clear();
end;

procedure TDIndex.FormResize(Sender: TObject);
begin
  DisableAlign();

  FIndexedFields.Width := (GBasics.ClientWidth - Panel.Width) div 2 - 2 * FIndexedFields.Left;
  Panel.Left := (GBasics.ClientWidth - Panel.Width) div 2;
  FLAvailableFields.Left := (GBasics.ClientWidth + Panel.Width) div 2 + FIndexedFields.Left;
  FAvailableFields.Left := FLAvailableFields.Left;
  FAvailableFields.Width := (GBasics.ClientWidth - Panel.Width) div 2 - 2 * FIndexedFields.Left;

  EnableAlign();
end;

procedure TDIndex.FormShow(Sender: TObject);
var
  Found: Boolean;
  I: Integer;
  J: Integer;
begin
  Database.Client.RegisterEventProc(FormClientEvent);

  if (not Assigned(Index)) then
  begin
    Caption := Preferences.LoadStr(160);
    HelpContext := 1046;
  end
  else
  begin
    Caption := Preferences.LoadStr(842, Index.Caption);
    HelpContext := 1055;
  end;

  FIndexedFields.Items.Clear();

  SetLength(Lengths, Table.Fields.Count);
  for I := 0 to Length(Lengths) - 1 do
    if (Table.Fields.Field[I].FieldType in [mfChar, mfVarChar]) then
      Lengths[I] := Table.Fields.Field[I].Size
    else if (Table.Fields.Field[I].FieldType in [mfTinyText, mfText, mfMediumText, mfLongText, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob]) then
      Lengths[I] := 10
    else
      Lengths[I] := 0;

  if (not Assigned(Index)) then
  begin
    FPrimary.Enabled := (Table.Indices.Count = 0) or not Table.Indices.Index[0].Primary;
    FPrimary.Checked := FPrimary.Enabled;
    FOther.Checked := not FPrimary.Checked;

    FName.Text := '';
    FLength.Text := '';

    FUnique.Checked := False;
    FFulltext.Checked := False;
  end
  else
  begin
    FPrimary.Enabled := Index.Primary or (Table.Indices.Count = 0) or not Table.Indices.Index[0].Primary;
    FPrimary.Checked := Index.Primary;
    FOther.Checked := not FPrimary.Checked;
    if (FOther.Checked) then FName.Text := Index.Name else FName.Text := '';

    for I := 0 to Index.Columns.Count - 1 do
      if (Index.Columns.Column[I].Length > 0) then
        Lengths[Table.Fields.IndexOf(Index.Columns.Column[I].Field)] := Index.Columns.Column[I].Length
      else if (Index.Columns.Column[I].Field.FieldType in [mfChar, mfVarChar]) then
        Lengths[Table.Fields.IndexOf(Index.Columns.Column[I].Field)] := Index.Columns.Column[I].Field.Size;

    for I := 0 to Index.Columns.Count - 1 do
      FIndexedFields.Items.Add().Caption := Index.Columns.Column[I].Field.Name;
    FIndexedFields.Selected := FIndexedFields.Items[0];

    FUnique.Checked := Index.Unique;
    FFulltext.Checked := Index.Fulltext;
  end;

  FAvailableFields.Items.Clear();
  for I := 0 to Table.Fields.Count - 1 do
  begin
    Found := False;
    if (Assigned(Index)) then
      for J := 0 to Index.Columns.Count - 1 do
        if (Index.Columns.Column[J].Field = Table.Fields.Field[I]) then
          Found := True;
    if (not Found) then
      FAvailableFields.Items.Add().Caption := Table.Fields.Field[I].Name;
  end;
  if (Assigned(FAvailableFields.Items[0])) then
    FAvailableFields.Items[0].Selected := True;

  FIndexedFieldsChange(Sender, nil, ctState);
  IndexTypeChange(Sender);
  FIndexedFieldsExit(Sender);
  FAvailableFieldsExit(Sender);

  FBOk.Enabled := False;

  ActiveControl := FBCancel;
  ActiveControl := FLName.FocusControl;
end;

procedure TDIndex.FUniqueClick(Sender: TObject);
begin
  if (FUnique.Checked) then
    FFulltext.Checked := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDIndex.IndexTypeChange(Sender: TObject);
begin
  if (FPrimary.Checked) then
    FLName.FocusControl := FPrimary
  else
    FLName.FocusControl := FName;

  FUnique.Enabled := not FPrimary.Checked;
  FUnique.Checked := FPrimary.Checked or FUnique.Checked;

  FBOkCheckEnabled(Sender);
end;

procedure TDIndex.tbAddAllClick(Sender: TObject);
begin
  while (FAvailableFields.Items.Count > 0) do
  begin
    FAvailableFields.Items[0].Selected := True;
    tbAddOneClick(Sender);
  end;
end;

procedure TDIndex.tbAddOneClick(Sender: TObject);
var
  Index: Integer;
  Item: TListItem;
begin
  Index := FAvailableFields.Items.IndexOf(FAvailableFields.Selected);

  if (Index >= 0) then
  begin
    Item := FIndexedFields.Items.Add();
    Item.Caption := FAvailableFields.Selected.Caption;
    FIndexedFields.Selected := Item;

    FAvailableFields.Selected.Delete();

    if (Index < FAvailableFields.Items.Count) then
      FAvailableFields.Items[Index].Selected := True
    else if (FAvailableFields.Items.Count > 0) then
      FAvailableFields.Items[FAvailableFields.Items.Count - 1].Selected := True;
  end;
end;

procedure TDIndex.tbRemoveAllClick(Sender: TObject);
begin
  while (FIndexedFields.Items.Count > 0) do
  begin
    FIndexedFields.Items[0].Selected := True;
    tbRemoveOneClick(Sender);
  end;
end;

procedure TDIndex.tbRemoveOneClick(Sender: TObject);
var
  Field: TCTableField;
  I: Integer;
  Index: Integer;
  Item: TListItem;
begin
  if (Assigned(FIndexedFields.Selected)) then
  begin
    Field := Table.FieldByName(FIndexedFields.Selected.Caption);

    Index := FIndexedFields.Items.IndexOf(FIndexedFields.Selected);
    FIndexedFields.Items[Index].Delete();
    FIndexedFieldsChange(FIndexedFields, nil, ctState);
    if (Index = FIndexedFields.Items.Count) then Dec(Index);
    if (Index >= 0) then FIndexedFields.Items[Index].Selected := True;

    Index := 0;
    for I := 0 to FAvailableFields.Items.Count - 1 do
      if (Table.Fields.IndexOf(Field) > Table.Fields.IndexOf(Table.FieldByName(FAvailableFields.Items[I].Caption))) then
        Index := I + 1;
    if (Index >= 0) then
      Item := FAvailableFields.Items.Insert(Index)
    else
      Item := FAvailableFields.Items.Add();
    Item.Caption := Field.Name;
    Item.Selected := True;
  end;
end;

procedure TDIndex.tbUpDownClick(Sender: TObject);
var
  Index: Integer;
  OldCaption: string;
  OldIndex: Integer;
begin
  OldCaption := FIndexedFields.Selected.Caption;
  OldIndex := FIndexedFields.Items.IndexOf(FIndexedFields.Selected);
  FIndexedFields.Items.Delete(OldIndex);

  if (Sender = tbUp) then
    Index := OldIndex - 1
  else
    Index := OldIndex + 1;

  FIndexedFields.Items.Insert(Index).Caption := OldCaption;
  FIndexedFields.Selected := FIndexedFields.Items[Index];
  FIndexedFields.ItemFocused := FIndexedFields.Selected;
end;

initialization
  FIndex := nil;
end.

