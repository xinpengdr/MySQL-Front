unit fDField;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Mask,
  ComCtrls_Ext, StdCtrls_Ext, Forms_Ext,
  fClient, fBase,
  MySQLDB, Vcl.ExtCtrls;

type
  TDField = class (TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FCharset: TComboBox_Ext;
    FCollation: TComboBox_Ext;
    FComment: TEdit;
    FDefault: TEdit;
    FDefaultEnum: TComboBox_Ext;
    FDefaultSet: TListBox;
    FFieldType: TComboBox_Ext;
    FFlagAscii: TCheckBox;
    FFlagBinary: TCheckBox;
    FFlagNational: TCheckBox;
    FFlagNullAllowed: TCheckBox;
    FFlagUnicode: TCheckBox;
    FFlagUnsigned: TCheckBox;
    FFlagZerofill: TCheckBox;
    FFormatDate: TEdit;
    FFormatDecimals: TEdit;
    FFormatSize: TEdit;
    FFormatTimestamp: TComboBox_Ext;
    FFormatUnion: TEdit;
    FFormatYear: TComboBox_Ext;
    FLCharset: TLabel;
    FLCollation: TLabel;
    FLComment: TLabel;
    FLDefault: TLabel;
    FLFormat: TLabel;
    FLFormatDecimals: TLabel;
    FLFormatSize: TLabel;
    FLName: TLabel;
    FLPosition: TLabel;
    FLType: TLabel;
    FLUpdateTime: TLabel;
    FName: TEdit;
    FPosition: TComboBox_Ext;
    FRDefault: TRadioButton;
    FRDefaultAutoIncrement: TRadioButton;
    FRDefaultInsertTime: TRadioButton;
    FRDefaultNull: TRadioButton;
    FUDFormatDecimals: TUpDown;
    FUDFormatSize: TUpDown;
    FUpdateTime: TCheckBox;
    GAttributes: TGroupBox_Ext;
    GBasics: TGroupBox_Ext;
    PSQLWait: TPanel;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FCharsetChange(Sender: TObject);
    procedure FCollationDropDown(Sender: TObject);
    procedure FDefaultChange(Sender: TObject);
    procedure FDefaultEnter(Sender: TObject);
    procedure FDefaultExit(Sender: TObject);
    procedure FFieldTypeChange(Sender: TObject);
    procedure FFieldTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FFieldTypeExit(Sender: TObject);
    procedure FFlagCharClick(Sender: TObject);
    procedure FFlagCharPress(Sender: TObject; var Key: Char);
    procedure FFlagNationalKeyPress(Sender: TObject; var Key: Char);
    procedure FFlagNullAllowedClick(Sender: TObject);
    procedure FFlagNullAllowedKeyPress(Sender: TObject; var Key: Char);
    procedure FFlagUnsignedClick(Sender: TObject);
    procedure FFlagUnsignedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FFlagUnsignedKeyPress(Sender: TObject; var Key: Char);
    procedure FFlagZerofillClick(Sender: TObject);
    procedure FFlagZerofillKeyPress(Sender: TObject; var Key: Char);
    procedure FFormatDecimalsChange(Sender: TObject);
    procedure FFormatSizeChange(Sender: TObject);
    procedure FFormatSizeExit(Sender: TObject);
    procedure FFormatTimestampChange(Sender: TObject);
    procedure FFormatUnionChange(Sender: TObject);
    procedure FFormatYearChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FRDefaultClick(Sender: TObject);
    procedure FRDefaultNullKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FUDClick(Sender: TObject; Button: TUDBtnType);
  private
    function GetDefault(): string;
    function GetDefaultDecimals(): Integer;
    function GetDefaultSize(): Integer;
    function GetMaxLength(): Integer;
    function GetType(): TMySQLFieldType;
    procedure FormClientEvent(const Event: TCClient.TEvent);
    function IsBinaryType(): Boolean;
    function IsCharType(): Boolean;
    function IsDateType(): Boolean;
    function IsFloatType(): Boolean;
    function IsIntType(): Boolean;
    function IsMemoType(): Boolean;
    function IsUnionType(): Boolean;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Database: TCDatabase;
    Field: TCBaseTableField;
    Table: TCBaseTable;
    function Execute(): Boolean;
  end;

function DField(): TDField;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  SQLUtils,
  fPreferences;

var
  FField: TDField;

function DField(): TDField;
begin
  if (not Assigned(FField)) then
  begin
    Application.CreateForm(TDField, FField);
    FField.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FField;
end;

{ TDField *********************************************************************}

function DisplayFormatToEditMask(const DisplayFormat: string): string;
var
  I: Integer;
begin
  Result := DisplayFormat;

  Result := ReplaceStr(Result, 'Y', '0');
  Result := ReplaceStr(Result, 'y', '0');
  Result := ReplaceStr(Result, 'M', '0');
  Result := ReplaceStr(Result, 'm', '0');
  Result := ReplaceStr(Result, 'D', '0');
  Result := ReplaceStr(Result, 'd', '0');
  Result := ReplaceStr(Result, 'H', '0');
  Result := ReplaceStr(Result, 'h', '0');
  Result := ReplaceStr(Result, 'S', '0');
  Result := ReplaceStr(Result, 's', '0');

  I := Length(Result);
  while (I >= 1) do
  begin
    if (Result[I] <> '0') then
      Insert('\', Result, I);
    Dec(I);
  end;
end;

procedure TDField.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiField, Icon);

  PSQLWait.Caption := Preferences.LoadStr(882);

  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLType.Caption := Preferences.LoadStr(91) + ':';
  FLFormatSize.Caption := Preferences.LoadStr(104) + ':';
  FLFormatDecimals.Caption := Preferences.LoadStr(78) + ':';
  FLFormat.Caption := Preferences.LoadStr(93) + ':';
  FLDefault.Caption := Preferences.LoadStr(92) + ':';
  FRDefaultNull.Caption := '<' + Preferences.LoadStr(71) + '>';
  FRDefaultInsertTime.Caption := '<INSERT-TimeStamp>';
  FLUpdateTime.Caption := Preferences.LoadStr(261) + ':';
  FUpdateTime.Caption := '<UPDATE-TimeStamp>';
  FLPosition.Caption := Preferences.LoadStr(79) + ':';
  FLCharset.Caption := Preferences.LoadStr(682) + ':';
  FLCollation.Caption := Preferences.LoadStr(702) + ':';
  FLComment.Caption := Preferences.LoadStr(111) + ':';

  GAttributes.Caption := Preferences.LoadStr(86);
  FFlagBinary.Caption := Preferences.LoadStr(80);
  FFlagUnsigned.Caption := Preferences.LoadStr(81);
  FFlagNational.Caption := Preferences.LoadStr(94);
  FFlagZerofill.Caption := Preferences.LoadStr(82);
  FFlagNullAllowed.Caption := Preferences.LoadStr(83);
  FRDefaultAutoIncrement.Caption := Preferences.LoadStr(84);
  FFlagAscii.Caption := Preferences.LoadStr(669);
  FFlagUnicode.Caption := Preferences.LoadStr(683);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDField.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDField.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDField.FBOkCheckEnabled(Sender: TObject);
begin
  FBOk.Enabled := Active and (FName.Text <> '')
    and (not IsFloatType() or (FUDFormatSize.Position = 0) and (FUDFormatDecimals.Position = 0) or (FUDFormatSize.Position <> 0))
    and ((GetType() <> mfEnum) or ((FFormatUnion.Text <> '') and (FDefaultEnum.ItemIndex >= 0)))
    and (not Assigned(Table.FieldByName(FName.Text)) or (Assigned(Field) and (Table.Fields.NameCmp(FName.Text, Field.Name) = 0)));
end;

procedure TDField.FCharsetChange(Sender: TObject);
var
  Charset: TCCharset;
  I: Integer;
begin
  if (FCharset.Text <> '') then
    Charset := Table.Database.Client.CharsetByName(FCharset.Text)
  else if (Assigned(Table) and (Table.DefaultCharset <> '')) then
    Charset := Table.Database.Client.CharsetByName(Table.DefaultCharset)
  else if (Assigned(Table) and (Table.Database.DefaultCharset <> '')) then
    Charset := Table.Database.Client.CharsetByName(Table.Database.DefaultCharset)
  else
    Charset := nil;

  FCollation.Items.Clear();
  FCollation.Items.Add('');
  if (Assigned(Charset) and Assigned(Table.Database.Client.Collations)) then
    for I := 0 to Table.Database.Client.Collations.Count - 1 do
      if (Table.Database.Client.Collations[I].Charset = Charset) then
        FCollation.Items.Add(Table.Database.Client.Collations[I].Name);

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FCollationDropDown(Sender: TObject);
var
  I: Integer;
  J: Integer;
begin
  if (Assigned(Table.Database.Client.Collations) and (FCollation.ItemIndex < 0)) then
    for I := 0 to Table.Database.Client.Collations.Count - 1 do
      if ((lstrcmpi(PChar(Table.Database.Client.Collations[I].Charset.Name), PChar(FCharset.Text)) = 0) and Table.Database.Client.Collations[I].Default) then
        for J := 1 to FCollation.Items.Count - 1 do
          if (lstrcmpi(PChar(FCollation.Items[J]), PChar(Table.Database.Client.Collations[I].Name)) = 0) then
            FCollation.ItemIndex := FCollation.Items.IndexOf(FCollation.Items[J]);
end;

procedure TDField.FDefaultChange(Sender: TObject);
begin
  if (FDefault.Text <> '') then
    FRDefault.Checked := True;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FDefaultEnter(Sender: TObject);
begin
  if (IsIntType()) or (IsFloatType()) then
    FDefault.MaxLength := 30
  else if (IsCharType() or IsBinaryType()) then
    FDefault.MaxLength := FUDFormatSize.Position
  else if ((GetType() = mfTimeStamp) and (Table.Database.Client.ServerVersion < 40100)) then
    FDefault.MaxLength := Length(FFormatTimestamp.Text)
  else
    FDefault.MaxLength := 0;

  if (FUDFormatDecimals.Position > 0) and (FUDFormatDecimals.Visible) then
    FDefault.MaxLength := FDefault.MaxLength + 1 + FUDFormatDecimals.Position;
end;

procedure TDField.FDefaultExit(Sender: TObject);
begin
  FDefaultChange(Sender);

  if (FRDefault.Checked and (Trim(FDefault.Text) = '')
    and (IsIntType() or IsFloatType() or (GetType() in [mfDate, mfDateTime, mfTime, mfYear, mfTimeStamp]))) then
    FDefault.Text := GetDefault();

  if (FFlagZerofill.Visible) then
    if (FFlagZerofill.Checked) then
      while (Length(FDefault.Text) < FUDFormatSize.Position) do
        FDefault.Text := '0' + FDefault.Text;
end;

procedure TDField.FFieldTypeChange(Sender: TObject);
var
  I: Integer;
  J: Integer;
begin
  FFormatSize.Visible := (GetType() = mfBit) or IsIntType() or IsFloatType() or IsCharType() or IsBinaryType(); FUDFormatSize.Visible := FFormatSize.Visible;
  FLFormatSize.Visible := (GetType() = mfBit) or IsIntType() or IsCharType() or IsBinaryType();
  FFormatDecimals.Visible := IsFloatType(); FUDFormatDecimals.Visible := FFormatDecimals.Visible;
  FLFormatDecimals.Visible := IsFloatType();
  FFormatDate.Visible := GetType() = mfTime;
  FFormatYear.Visible := GetType() = mfYear;
  FFormatTimestamp.Visible := (GetType() = mfTimestamp) and (Table.Database.Client.ServerVersion < 40100);
  FFormatUnion.Visible := IsUnionType();
  FLFormat.Visible := FFormatTimestamp.Visible or FFormatYear.Visible or FFormatDate.Visible or FFormatUnion.Visible;
  FDefault.Visible := ((GetType() = mfBit) or IsIntType() or IsFloatType() or IsCharType() or IsBinaryType() or IsDateType());
  FRDefaultNull.Visible := FDefault.Visible;
  FRDefault.Visible := FDefault.Visible;
  FRDefaultInsertTime.Visible := FDefault.Visible and (GetType() = mfTimeStamp) and (Table.Database.Client.ServerVersion >= 40102);
  FRDefaultAutoIncrement.Visible := IsIntType();
  FUpdateTime.Visible := FDefault.Visible and (GetType() = mfTimeStamp) and (Table.Database.Client.ServerVersion >= 40102); FLUpdateTime.Visible := FUpdateTime.Visible;
  FDefaultEnum.Visible := GetType() = mfEnum;
  FDefaultSet.Visible := GetType() = mfSet;
  FLDefault.Visible := FDefault.Visible or FDefaultEnum.Visible or FDefaultSet.Visible;
  FCharset.Visible := (IsCharType() or IsMemoType() or (GetType() in [mfEnum, mfSet])) and (Table.Database.Client.ServerVersion >= 40101); FLCharset.Visible := FCharset.Visible;
  FCollation.Visible := (IsCharType() or IsMemoType() or (GetType() in [mfEnum, mfSet])) and (Table.Database.Client.ServerVersion >= 40101); FLCollation.Visible := FCollation.Visible;
  FFlagBinary.Visible := IsCharType() and (Table.Database.Client.ServerVersion < 40101);
  FFlagNational.Visible := IsCharType() and (Table.Database.Client.ServerVersion < 40101);
  FFlagUnsigned.Visible := IsIntType() or IsFloatType();
  FFlagZerofill.Visible := IsIntType();
  FFlagAscii.Visible := (GetType() = mfChar) and (Table.Database.Client.ServerVersion < 40101);
  FFlagUnicode.Visible := (GetType() = mfChar) and (Table.Database.Client.ServerVersion < 40101);

  FDefault.Enabled := True; FLDefault.Enabled := FDefault.Enabled;
  FRDefaultNull.Enabled := FDefault.Enabled;
  FRDefault.Enabled := FDefault.Enabled;
  FRDefaultInsertTime.Enabled := FDefault.Enabled;
  FRDefaultAutoIncrement.Enabled := FDefault.Enabled;
  for I := 0 to Table.Keys.Count - 1 do
    if (Table.Keys[I].Unique) then
      for J := 0 to Table.Keys[I].Columns.Count - 1 do
        if (Assigned(Field) and (Table.Keys[I].Columns[J].Field.Name = Field.Name)) then
          FRDefaultAutoIncrement.Enabled := True;
  for I := 0 to Table.Fields.Count - 1 do
    if (not Assigned(Field) or (Table.Fields[I].Name <> Field.Name)) then
      if (Table.Fields[I].AutoIncrement) then
        FRDefaultAutoIncrement.Enabled := False;
  if (Assigned(Table.PrimaryKey) and (not Assigned(Field) or not Assigned(Table.PrimaryKey.ColumnByField(Field)))) then
    FRDefaultAutoIncrement.Enabled := False;

  FUpdateTime.Enabled := FDefault.Enabled; FLUpdateTime.Enabled := FUpdateTime.Enabled;

  if (GetType() = mfDate) then FFormatDate.Text := UpperCase(Table.Database.Client.FormatSettings.ShortDateFormat);
  if (GetType() = mfDateTime) then FFormatDate.Text := UpperCase(Table.Database.Client.FormatSettings.ShortDateFormat + ' ' + Table.Database.Client.FormatSettings.LongTimeFormat);
  if (GetType() = mfTime) then FFormatDate.Text := UpperCase(Table.Database.Client.FormatSettings.LongTimeFormat);

  if ((GetType() = mfBit) or IsFloatType() or (IsIntType() or IsCharType() or IsBinaryType()) and (FUDFormatSize.Position = 0)) then FUDFormatSize.Position := GetDefaultSize();
  if (IsFloatType()) then FUDFormatDecimals.Position := GetDefaultDecimals();
  if (GetType() = mfChar) then FUDFormatSize.Position := 1;

  case (GetType()) of
    mfTimestamp: FFormatTimestampChange(Sender);
    mfYear: FFormatYearChange(Sender);
  end;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFieldTypeDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ComboBox: TComboBox;
begin
  if (Control is TComboBox) then
  begin
    ComboBox := TComboBox(Control);

    if (not (odComboBoxEdit in State) and Table.Database.Client.FieldTypeByCaption(ComboBox.Items[Index]).Highlighted) then
      ComboBox.Canvas.Font.Style := ComboBox.Canvas.Font.Style + [fsBold];
    ComboBox.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top, ComboBox.Items[Index]);
    ComboBox.Canvas.Font.Style := ComboBox.Canvas.Font.Style - [fsBold];
  end;
end;

procedure TDField.FFieldTypeExit(Sender: TObject);
begin
  FUDFormatSize.Max := GetMaxLength();
  if (FUDFormatSize.Position = 0) then
    FUDFormatSize.Position := GetDefaultSize();

  if (FRDefault.Checked and (Trim(FDefault.Text) = '')
    and (IsIntType() or IsFloatType() or (GetType() in [mfDate, mfDateTime, mfTime, mfYear, mfTimeStamp]))) then
    FDefault.Text := GetDefault();
end;

procedure TDField.FFlagCharClick(Sender: TObject);
begin
  if ((Sender <> FFlagBinary) and TCheckBox(Sender).Checked) then FFlagBinary.Checked := False;
  if ((Sender <> FFlagAscii) and TCheckBox(Sender).Checked) then FFlagAscii.Checked := False;
  if ((Sender <> FFlagUnicode) and TCheckBox(Sender).Checked) then FFlagUnicode.Checked := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFlagCharPress(Sender: TObject; var Key: Char);
begin
  FFlagCharClick(Sender);
end;

procedure TDField.FFlagNationalKeyPress(Sender: TObject; var Key: Char);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFlagNullAllowedClick(Sender: TObject);
begin
  if (FFlagNullAllowed.Checked and FRDefaultAutoIncrement.Checked) then
    FRDefaultNull.Checked := True
  else if (not FFlagNullAllowed.Checked and FRDefaultNull.Checked) then
  begin
    FRDefault.Checked := True;
    if (FDefault.Text = '') then
      FDefault.Text := GetDefault();
  end;

  FFormatUnionChange(Sender);
  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFlagNullAllowedKeyPress(Sender: TObject; var Key: Char);
begin
  FFlagNullAllowedClick(Sender);
end;

procedure TDField.FFlagUnsignedClick(Sender: TObject);
begin
  if (not FFlagUnsigned.Checked) then FFlagZerofill.Checked := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFlagUnsignedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FFlagUnsignedClick(Sender);
end;

procedure TDField.FFlagUnsignedKeyPress(Sender: TObject; var Key: Char);
begin
  FFlagZerofillClick(Sender);
end;

procedure TDField.FFlagZerofillClick(Sender: TObject);
var
  S: string;
begin
  S := FDefault.Text;

  if (FFlagZerofill.Checked) then
    while (Length(S) < FUDFormatSize.Position) do
      S := '0' + S
    else
      while (Length(S) > 1) and (S[1] = '0') do
        Delete(S, 1, 1);
  FDefault.Text := S;

  if (FFlagZerofill.Checked) then FFlagUnsigned.Checked := True;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFlagZerofillKeyPress(Sender: TObject; var Key: Char);
begin
  FFlagZerofillClick(Sender);
end;

procedure TDField.FFormatDecimalsChange(Sender: TObject);
var
  Format: string;
  Value: Double;
begin
  if (FFormatDecimals.Visible and (FUDFormatDecimals.Position > 0) and (FUDFormatDecimals.Position > FUDFormatSize.Position - 1)) then
    FUDFormatSize.Position := FUDFormatDecimals.Position + 1;

  if (Active and IsFloatType() and (FDefault.Text <> '') and (FUDFormatSize.Position <> 0)) then
  begin
    Format := StringOfChar('#', FUDFormatSize.Position);
    Format := Format + '.';
    Format := Format + StringOfChar('0', FUDFormatDecimals.Position);
    if (TryStrToFloat(FDefault.Text, Value)) then
      FDefault.Text := FormatFloat(Format, Value);
  end;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFormatSizeChange(Sender: TObject);
var
  Format: string;
  Value: Double;
begin
  if (Active and IsFloatType() and (FDefault.Text <> '') and (FUDFormatSize.Position <> 0)) then
  begin
    Format := StringOfChar('#', FUDFormatSize.Position - 1);
    Format := Format + '0.';
    Format := Format + StringOfChar('0', FUDFormatDecimals.Position);
    if (TryStrToFloat(FDefault.Text, Value)) then
      FDefault.Text  := FormatFloat(Format, Value);
  end;

  FFormatDecimals.Enabled := FUDFormatSize.Position <> 0; FUDFormatDecimals.Enabled := FFormatDecimals.Enabled;

  if (FUDFormatSize.Position = 0) then FUDFormatDecimals.Position := 0;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFormatSizeExit(Sender: TObject);
begin
  if (FUDFormatSize.Position = 0) and ((GetType() = mfBit) or IsIntType() or IsFloatType()) then
    FUDFormatSize.Position := GetDefaultSize();
end;

procedure TDField.FFormatTimestampChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFormatUnionChange(Sender: TObject);
var
  I: Integer;
  OldText: string;
  Values: TSQLStrings;
begin
  OldText := FDefaultEnum.Text;

  FDefaultEnum.Items.Clear();
  FDefaultSet.Items.Clear();

  SetLength(Values, 0);
  SQLSplitValues(Trim(FFormatUnion.Text), Values);
  if (FFlagNullAllowed.Checked) then
    FDefaultEnum.Items.Add('<' + Preferences.LoadStr(71) + '>');
  for I := 0 to Length(Values) - 1 do
    FDefaultEnum.Items.Add(SQLUnescape(Values[I]));

  if (FDefaultEnum.Items.IndexOf(OldText) < 0) then
    FDefaultEnum.ItemIndex := 0
  else
    FDefaultEnum.ItemIndex := FDefaultEnum.Items.IndexOf(OldText);

  for I := 0 to Length(Values) - 1 do
    FDefaultSet.Items.Add(SQLUnescape(Values[I]));

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FFormatYearChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDField.FormClientEvent(const Event: TCClient.TEvent);
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

procedure TDField.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  FieldName: string;
  I: Integer;
  Index: Integer;
  NewField: TCBaseTableField;
  NewTable: TCBaseTable;
begin
  if ((ModalResult = mrOk) and GBasics.Visible) then
  begin
    if (not Assigned(Database)) then
      NewTable := Table
    else
    begin
      NewTable := TCBaseTable.Create(Database.Tables);
      NewTable.Assign(Table);
    end;

    if (IsIntType()) or (GetType() = mfTimestamp) then
      for I := 1 to Length(FDefault.Text) do
        if (not CharInSet(FDefault.Text[I], ['0'..'9', FormatSettings.DecimalSeparator]) and (FDefault.Text[I] = '-') and FFlagUnsigned.Checked) then
          begin MessageBeep(MB_ICONERROR); ActiveControl := FDefault; CanClose := False; end;
    if (IsFloatType()) then
      for I := 1 to Length(FDefault.Text) do
        if (not CharInSet(FDefault.Text[I], ['0'..'9', FormatSettings.ThousandSeparator, FormatSettings.DecimalSeparator]) and (FDefault.Text[I] = '-') and FFlagUnsigned.Checked) then
          begin MessageBeep(MB_ICONERROR); ActiveControl := FDefault; CanClose := False; end;
    if (GetType() = mfVarChar) then
      if (FUDFormatSize.Position = 0) then
        begin MessageBeep(MB_ICONERROR); ActiveControl := FFormatSize; CanClose := False; end;

    if (CanClose) then
    begin
      NewField := TCBaseTableField.Create(NewTable.Fields);
      if (Assigned(Field)) then
        NewField.Assign(Field);

      NewField.Name := Trim(FName.Text);
      NewField.FieldType := GetType();
      if ((GetType() = mfBit) or IsIntType() or IsFloatType() or IsCharType() or IsBinaryType()) then
        if (FUDFormatSize.Position = 0) then NewField.Size := -1 else NewField.Size := FUDFormatSize.Position
      else if (NewField.FieldType = mfTimeStamp) then NewField.Size := Length(FFormatTimestamp.Text)
      else if (NewField.FieldType = mfYear) then NewField.Size := Length(FFormatYear.Text)
      else NewField.Size := 0;
      if (IsFloatType()) then NewField.Decimals := FUDFormatDecimals.Position else NewField.Decimals := 0;
      SetLength(NewField.Items, 0);

      if (NewField.FieldType = mfEnum) then
      begin
        if (FFlagNullAllowed.Checked) then
          Index := 1
        else
          Index := 0;
        for I := Index to FDefaultEnum.Items.Count - 1 do
          begin SetLength(NewField.Items, Length(NewField.Items) + 1); NewField.Items[Length(NewField.Items) - 1] := FDefaultEnum.Items.Strings[I]; end;
      end;
      if (NewField.FieldType = mfSet) then
        for I := 0 to FDefaultSet.Items.Count - 1 do
          begin SetLength(NewField.Items, Length(NewField.Items) + 1); NewField.Items[Length(NewField.Items) - 1] := FDefaultSet.Items.Strings[I]; end;

      NewField.Default := '';
      if (NewField.FieldType = mfTimestamp) then
        if (FRDefaultInsertTime.Checked) then
          NewField.Default := 'CURRENT_TIMESTAMP'
        else if (FRDefaultNull.Checked) then
          NewField.Default := 'NULL'
        else
          NewField.Default := NewField.EscapeValue(Trim(FDefault.Text))
      else if (((GetType() = mfBit) or IsIntType() or IsFloatType() or IsCharType() or IsBinaryType() or IsDateType()) and (FDefault.Visible) and not FRDefaultNull.Checked) then
        NewField.Default := NewField.EscapeValue(Trim(FDefault.Text))
      else if ((NewField.FieldType = mfEnum) and (not FFlagNullAllowed.Checked or (FDefaultENum.ItemIndex > 0))) then
        NewField.Default := NewField.EscapeValue(FDefaultENum.Text)
      else if (NewField.FieldType = mfSet) then
      begin
        for I := 0 to FDefaultSet.Count - 1 do
          if (FDefaultSet.Selected[I]) then
          begin
            if (NewField.Default <> '') then
              NewField.Default := NewField.Default + ',';
            NewField.Default := NewField.Default + FDefaultSet.Items.Strings[I];
          end;
        NewField.Default := NewField.EscapeValue(NewField.Default);
      end
      else if (FFlagNullAllowed.Checked) then
        NewField.Default := 'NULL';
      if (FUpdateTime.Checked) then
        NewField.OnUpdate := 'CURRENT_TIMESTAMP'
      else
        NewField.OnUpdate := '';

      NewField.NullAllowed := FFlagNullAllowed.Checked;
      NewField.Unsigned := FFlagUnsigned.Checked and (IsIntType() or IsFloatType());
      NewField.Zerofill := FFlagZerofill.Checked and (IsIntType() or IsFloatType());
      NewField.Binary := FFlagBinary.Checked and IsCharType();
      NewField.National := FFlagNational.Checked and IsCharType();
      NewField.AutoIncrement := FRDefaultAutoIncrement.Checked and IsIntType();
      NewField.Ascii := FFlagAscii.Checked and (GetType() = mfChar);
      NewField.Unicode := FFlagUnicode.Checked and (GetType() = mfChar);

      NewField.Moved := NewField.Moved or (FPosition.ItemIndex <> NewTable.Fields.IndexOf(NewField.FieldBefore) + 1);
      if (FPosition.ItemIndex = 0) then
        NewField.FieldBefore := nil
      else
      begin
        FieldName := FPosition.Text;
        Delete(FieldName, 1, Pos('"', FieldName));
        Delete(FieldName, Pos('"', FieldName), Length(FieldName) - Pos('"', FieldName) + 1);
        NewField.FieldBefore := NewTable.FieldByName(FieldName);
      end;

      if (FCharset.Visible and FCharset.Enabled) then
        NewField.Charset := Trim(FCharset.Text)
      else
        NewField.Charset := '';
      if (FCollation.Visible) then
        NewField.Collation := Trim(FCollation.Text)
      else
        NewField.Collation := '';
      if (not Assigned(Field) or (Trim(FComment.Text) <> SQLUnwrapStmt(NewField.Comment))) then
        NewField.Comment := Trim(FComment.Text);

      if (not Assigned(Database)) then
      begin
        if (not Assigned(Field)) then
          NewTable.Fields.AddField(NewField)
        else
        begin
          TCBaseTableFields(NewTable.Fields).MoveField(Field, NewField.FieldBefore);
          NewTable.Fields[Field.Index].Assign(NewField)
        end;
      end
      else
      begin
        if (not Assigned(Field)) then
          NewTable.Fields.AddField(NewField)
        else
        begin
          NewTable.Fields[Field.Index].Assign(NewField);
          TCBaseTableFields(NewTable.Fields).MoveField(NewTable.Fields[Field.Index], NewField.FieldBefore);
        end;

        CanClose := Database.UpdateTable(Table, NewTable);

        GBasics.Visible := CanClose or not Database.Client.Asynchron;
        GAttributes.Visible := GBasics.Visible;
        PSQLWait.Visible := not GBasics.Visible;
        if (PSQLWait.Visible) then
          ModalResult := mrNone;

        FBOk.Enabled := False;
      end;

      NewField.Free();

      if (Assigned(Database)) then
        NewTable.Free();
    end;
  end;
end;

procedure TDField.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Field.Width >= Width) and (Preferences.Field.Height >= Height)) then
  begin
    Width := Preferences.Field.Width;
    Height := Preferences.Field.Height;
  end;
end;

procedure TDField.FormHide(Sender: TObject);
begin
  Table.Client.UnRegisterEventProc(FormClientEvent);

  Preferences.Field.Width := Width;
  Preferences.Field.Height := Height;
end;

procedure TDField.FormShow(Sender: TObject);
var
  DefaultList: TStringList;
  I: Integer;
  S: string;
begin
  Table.Client.RegisterEventProc(FormClientEvent);

  if (not Assigned(Field)) then
  begin
    Caption := Preferences.LoadStr(87);
    HelpContext := 1047;
  end
  else
  begin
    Caption := Preferences.LoadStr(842, Field.Name);
    HelpContext := 1056;
  end;

  FFieldType.Clear();
  for I := 0 to Table.Database.Client.FieldTypes.Count - 1 do
    if (Table.Engine.FieldAvailable(Table.Database.Client.FieldTypes[I].MySQLFieldType)) then
      FFieldType.Items.Add(Table.Database.Client.FieldTypes[I].Caption);

  FPosition.Items.Clear();
  FPosition.Items.Add(Preferences.LoadStr(95));
  for I := 0 to Table.Fields.Count - 1 do
    if (not Assigned(Field) or (Table.Fields[I].Name <> Field.Name)) then
      FPosition.Items.Add(Preferences.LoadStr(96) + ' "' + Table.Fields[I].Name + '"');
  FPosition.Enabled := not Assigned(Field) or (Table.Database.Client.ServerVersion >= 40001);

  FCharset.Items.Clear();
  FCharset.Items.Add('');
  for I := 0 to Table.Database.Client.Charsets.Count - 1 do
    FCharset.Items.Add(Table.Database.Client.Charsets[I].Name);

  if (not Assigned(Field)) then
    FPosition.ItemIndex := FPosition.Items.Count - 1
  else if (Assigned(Field.FieldBefore)) then
    FPosition.ItemIndex := Table.Fields.IndexOf(Field.FieldBefore) + 1
  else
    FPosition.ItemIndex := 0;

  if (not Assigned(Field)) then
  begin
    FName.Text := Preferences.LoadStr(105);
    while (Assigned(Table.FieldByName(FName.Text))) do
    begin
      S := FName.Text;
      Delete(S, 1, Length(Preferences.LoadStr(105)));
      if (S = '') then S := '1';
      S := Preferences.LoadStr(105) + IntToStr(StrToInt(S) + 1);
      FName.Text := S;
    end;

    FFieldType.ItemIndex := FFieldType.Items.IndexOf(Table.Database.Client.FieldTypeByMySQLFieldType(mfVarChar).Caption); FFieldTypeChange(Sender); FFieldTypeExit(Sender);
    FUDFormatSize.Position := 255; FFormatSizeChange(Sender);

    FFormatUnion.Text := '';

    FRDefaultNull.Checked := True;
    FDefault.Text := ''; FRDefaultClick(Sender);

    FCharset.ItemIndex := FCharset.Items.IndexOf(Table.DefaultCharset); FCharsetChange(Sender);
    FCollation.ItemIndex := FCollation.Items.IndexOf(Table.Collation);

    FComment.Text := '';

    FFlagBinary.Checked := False; FFlagCharClick(Sender);
    FFlagNullAllowed.Checked := True; FFlagNullAllowedClick(Sender);
    FFlagNational.Checked := Table.Database.Client.ServerVersion < 40101;
    FFlagAscii.Checked := False; FFlagCharClick(Sender);
    FFlagUnicode.Checked := False; FFlagCharClick(Sender);
  end
  else
  begin
    FName.Text := Field.Name;

    FFieldType.ItemIndex := FFieldType.Items.IndexOf(Table.Database.Client.FieldTypeByMySQLFieldType(Field.FieldType).Caption); FFieldTypeChange(Sender); FFieldTypeExit(Sender);
    if (Field.Size >= 0) then FUDFormatSize.Position := Field.Size; FFormatSizeChange(Sender);
    if (Field.Decimals >= 0) then FUDFormatDecimals.Position := Field.Decimals; FFormatDecimalsChange(Sender);

    FFormatUnion.Text := '';
    for I := 0 to Length(Field.Items) - 1 do
      begin if (I > 0) then FFormatUnion.Text := FFormatUnion.Text + ','; FFormatUnion.Text := FFormatUnion.Text + Field.Items[I]; end;
    FFormatTimestamp.ItemIndex := 0;
    for I := 0 to FFormatTimestamp.Items.Count - 1 do
      if (FUDFormatSize.Position = Length(FFormatTimestamp.Items.Strings[I])) then
        FFormatTimestamp.ItemIndex := I;
    FFormatYear.ItemIndex := 0;
    for I := 0 to FFormatYear.Items.Count - 1 do
      if (FUDFormatSize.Position = Length(FFormatYear.Items.Strings[I])) then
        FFormatYear.ItemIndex := I;

    FCharset.ItemIndex := FCharset.Items.IndexOf(Field.Charset); FCharsetChange(Sender);
    FCollation.ItemIndex := FCollation.Items.IndexOf(Field.Collation);

    FFlagUnsigned.Checked := Field.Unsigned;
    FFlagZerofill.Checked := Field.Zerofill;
    FFlagBinary.Checked := Field.Binary; FFlagCharClick(Sender);
    FFlagNullAllowed.Checked := Field.NullAllowed; FFlagNullAllowedClick(Sender);
    FFlagNational.Checked := Field.National;
    FFlagAscii.Checked := Field.Ascii; FFlagCharClick(Sender);
    FFlagUnicode.Checked := Field.Unicode; FFlagCharClick(Sender);

    FDefault.Text := '';
    if (Field.AutoIncrement) then
      FRDefaultAutoIncrement.Checked := True
    else if (GetType() = mfEnum) then
    begin
      if (Field.Default = 'NULL') then
        FDefaultEnum.ItemIndex := 0
      else
        FDefaultEnum.ItemIndex := FDefaultEnum.Items.IndexOf(Field.UnescapeValue(Field.Default));
    end
    else if (GetType() = mfSet) then
    begin
      DefaultList := TStringList.Create();
      DefaultList.Text := ReplaceStr(Field.UnescapeValue(Field.Default), ',', #13#10);
      for I := 0 to DefaultList.Count - 1 do
        FDefaultSet.Selected[FDefaultSet.Items.IndexOf(DefaultList.Strings[I])] := True;
      FreeAndNil(DefaultList);
    end
    else
    begin
      FRDefaultNull.Checked := Field.Default = 'NULL';
      FRDefaultInsertTime.Checked := UpperCase(Field.Default) = 'CURRENT_TIMESTAMP';
      FUpdateTime.Checked := UpperCase(Field.OnUpdate) = 'CURRENT_TIMESTAMP';
      FRDefault.Checked := not FRDefaultNull.Checked and not FRDefaultInsertTime.Checked;
      if (not FRDefaultNull.Checked and not FRDefaultInsertTime.Checked) then
        FDefault.Text := Field.UnescapeValue(Field.Default);
    end;
    FRDefaultClick(Sender);

    FComment.Text := SQLUnwrapStmt(Field.Comment);
  end;

  FComment.Visible := Table.Database.Client.ServerVersion >= 40100; FLComment.Visible := FComment.Visible;

  GBasics.Visible := True;
  GAttributes.Visible := GBasics.Visible;
  PSQLWait.Visible := not GBasics.Visible;

  FBOk.Enabled := GBasics.Visible and not Assigned(Field);

  ActiveControl := FBCancel;
  if (GBasics.Visible) then
    ActiveControl := FName;
end;

procedure TDField.FRDefaultClick(Sender: TObject);
begin
  if (FRDefaultNull.Checked) then
    FFlagNullAllowed.Checked := True
  else if (FRDefaultAutoIncrement.Checked) then
    FFlagNullAllowed.Checked := False
  else if (FRDefault.Checked) then
    FDefaultExit(Sender);
  FFlagNullAllowedClick(Sender);

  if (FDefault.Visible and FDefault.Enabled and (Sender = FRDefault)) then
    ActiveControl := FDefault;

  FBOkCheckEnabled(Sender);
end;

procedure TDField.FRDefaultNullKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FRDefaultClick(Sender);
end;

procedure TDField.FUDClick(Sender: TObject; Button: TUDBtnType);
begin
  ActiveControl := TUpDown(Sender).Associate;
end;

function TDField.GetDefault(): string;
var
  Day: Word;
  Month: Word;
  Year: Word;
begin
  DecodeDate(Date(), Year, Month, Day);

  Result := '';
  if ((IsIntType() or IsFloatType()) and not FRDefaultAutoIncrement.Checked) then Result := '0';
  if (IsCharType() or IsBinaryType()) then Result := '';
  if (GetType() = mfYear) then Result := IntToStr(Year);
  if (GetType() = mfDate) then Result := MySQLDB.DateToStr(MySQLZeroDate, Table.Database.Client.FormatSettings);
  if (GetType() = mfDateTime) then Result := MySQLDB.DateTimeToStr(MySQLZeroDate, Table.Database.Client.FormatSettings);
  if (GetType() = mfTime) then Result := TimeToStr(0, Table.Database.Client.FormatSettings);
  if (GetType() = mfTimeStamp) then if (Table.Database.Client.ServerVersion >= 40100) then Result := MySQLDB.DateTimeToStr(MySQLZeroDate, Table.Database.Client.FormatSettings);
end;

function TDField.GetDefaultDecimals(): Integer;
begin
  case (GetType()) of
    mfFloat: Result := 0;
    mfDouble: Result := 0;
    mfDecimal: Result := 2;
    else Result := 0;
  end;
end;

function TDField.GetDefaultSize(): Integer;
begin
  case (GetType()) of
    mfBit: Result := 1;
    mfTinyInt: Result := 3;
    mfSmallInt: if (FFlagUnsigned.Checked) then Result := 5 else Result := 6;
    mfMediumInt: if (FFlagUnsigned.Checked) then Result := 8 else Result := 9;
    mfInt: Result := 11;
    mfBigInt: if (FFlagUnsigned.Checked) then Result := 20 else Result := 20;

    mfFloat: Result := 0;
    mfDouble: Result := 0;
    mfDecimal: Result := 10;

    mfChar,
    mfBinary: Result := 1;
    mfVarChar,
    mfVarBinary: Result := 255;
    else Result := 0;
  end
end;

function TDField.GetMaxLength(): Integer;
begin
  Result := 0;
  case (GetType()) of
    mfBit: Result := 64;
    mfTinyInt: Result := 3;
    mfSmallInt: if (FFlagUnsigned.Checked) then Result := 5 else Result := 6;
    mfMediumInt: if (FFlagUnsigned.Checked) then Result := 8 else Result := 9;
    mfInt: Result := 11;
    mfBigInt: if (FFlagUnsigned.Checked) then Result := 20 else Result := 20;

    mfFloat: Result := 16;
    mfDouble: Result := 24;
    mfDecimal: Result := 254;

    mfTime: Result := 10;
    mfYear: Result := 4;

    mfChar,
    mfBinary,
    mfVarChar,
    mfVarBinary: if (Table.Database.Client.ServerVersion < 50003) then Result := 255 else Result := 65535;
  end
end;

function TDField.GetType(): TMySQLFieldType;
begin
  if (FFieldType.ItemIndex < 0) then
    Result := mfUnknown
  else
    Result := Table.Database.Client.FieldTypeByCaption(FFieldType.Text).MySQLFieldType;
end;

function TDField.IsBinaryType(): Boolean;
begin
  Result := GetType() in [mfBinary, mfVarBinary];
end;

function TDField.IsCharType(): Boolean;
begin
  Result := GetType() in [mfChar, mfVarChar];
end;

function TDField.IsDateType(): Boolean;
begin
  Result := GetType() in [mfDate, mfDateTime, mfTimestamp, mfTime, mfYear];
end;

function TDField.IsFloatType(): Boolean;
begin
  Result := GetType() in [mfFloat, mfDouble, mfDecimal];
end;

function TDField.IsIntType(): Boolean;
begin
  Result := GetType() in [mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt];
end;

function TDField.IsMemoType(): Boolean;
begin
  Result := GetType() in [mfTinyText, mfText, mfMediumText, mfLongText];
end;

function TDField.IsUnionType(): Boolean;
begin
  Result := GetType() in [mfEnum, mfSet];
end;

initialization
  FField := nil;
end.
