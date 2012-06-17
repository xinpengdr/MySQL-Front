unit fDView;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, Menus,
  SynEdit, SynMemo,
  Forms_Ext,
  fBase, fClient, StdCtrls_Ext, Vcl.ExtCtrls;

type
  TDView = class(TForm_Ext)
    FAlgorithm: TComboBox;
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FCheckOption: TCheckBox;
    FCheckOptionCascade: TCheckBox;
    FCheckOptionLocal: TCheckBox;
    FDefiner: TLabel;
    FFields: TListView;
    FLAlgorithm: TLabel;
    FLCheckOption: TLabel;
    FLDefiner: TLabel;
    FLName: TLabel;
    FLRecordCount: TLabel;
    FLSecurity: TLabel;
    FLStmt: TLabel;
    FName: TEdit;
    FRecordCount: TLabel;
    FSecurityDefiner: TRadioButton;
    FSecurityInvoker: TRadioButton;
    FSource: TSynMemo;
    FStmt: TSynMemo;
    GBasics: TGroupBox_Ext;
    GDefiner: TGroupBox_Ext;
    GRecordCount: TGroupBox_Ext;
    msCopy: TMenuItem;
    msCut: TMenuItem;
    msDelete: TMenuItem;
    MSource: TPopupMenu;
    msPaste: TMenuItem;
    msSelectAll: TMenuItem;
    msUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PageControl: TPageControl;
    PSQLWait: TPanel;
    TSBasics: TTabSheet;
    TSInformations: TTabSheet;
    TSSource: TTabSheet;
    TSFields: TTabSheet;
    procedure FAlgorithmSelect(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FCheckOptionCascadeClick(Sender: TObject);
    procedure FCheckOptionCascadeKeyPress(Sender: TObject; var Key: Char);
    procedure FCheckOptionClick(Sender: TObject);
    procedure FCheckOptionKeyPress(Sender: TObject; var Key: Char);
    procedure FCheckOptionLocalClick(Sender: TObject);
    procedure FCheckOptionLocalKeyPress(Sender: TObject; var Key: Char);
    procedure FNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSecurityClick(Sender: TObject);
    procedure FSecurityKeyPress(Sender: TObject; var Key: Char);
    procedure FSourceStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure FStmtChange(Sender: TObject);
    procedure TSInformationsShow(Sender: TObject);
    procedure FSourceChange(Sender: TObject);
  private
    RecordCount: Integer;
    procedure Built();
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Database: TCDatabase;
    View: TCView;
    function Execute(): Boolean;
  end;

function DView(): TDView;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  fPreferences, SQLUtils;

var
  FView: TDView;

function DView(): TDView;
begin
  if (not Assigned(FView)) then
  begin
    Application.CreateForm(TDView, FView);
    FView.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FView;
end;

{ TDView **********************************************************************}

procedure TDView.Built();
var
  I: Integer;
  Item: TListItem;
  ViewField: TCViewField;
begin
  FName.Text := View.Name;

  case (View.Algorithm) of
    vaUndefined: FAlgorithm.ItemIndex := 0;
    vaMerge: FAlgorithm.ItemIndex := 1;
    vaTemptable: FAlgorithm.ItemIndex := 2;
    else FAlgorithm.Text := '';
  end;

  case (View.Security) of
    seDefiner: FSecurityDefiner.Checked := True;
    seInvoker: FSecurityInvoker.Checked := True;
  end;

  FCheckOption.Checked := View.CheckOption <> voNone;
  FCheckOptionCascade.Checked := View.CheckOption = voCascaded;
  FCheckOptionLocal.Checked := View.CheckOption = voLocal;

  FStmt.Lines.Text := Trim(SQLWrapStmt(View.Stmt, ['from', 'where', 'group by', 'having', 'order by', 'limit', 'procedure'], 0)) + #13#10;

  FDefiner.Caption := View.Definer;

  FFields.Items.BeginUpdate();
  for I := 0 to View.Fields.Count - 1 do
  begin
    ViewField := TCViewField(View.Fields[I]);
    Item := FFields.Items.Add();
    Item.Caption := ViewField.Name;
    if (ViewField.FieldType <> mfUnknown) then
    begin
      Item.SubItems.Add(ViewField.DBTypeStr());
      if (ViewField.NullAllowed) then
        Item.SubItems.Add(Preferences.LoadStr(74))
      else
        Item.SubItems.Add(Preferences.LoadStr(75));
      if (ViewField.AutoIncrement) then
        Item.SubItems.Add('<auto_increment>')
      else
        Item.SubItems.Add(ViewField.Default);
      if (ViewField.Charset <> View.Database.DefaultCharset) then
        Item.SubItems.Add(ViewField.Charset);
    end;
    Item.ImageIndex := iiViewField;
  end;
  FFields.Items.EndUpdate();

  FSource.Lines.Text := View.Source + #13#10;

  TSSource.TabVisible := Assigned(View) and (View.Source <> '');

  PageControl.Visible := True;
  PSQLWait.Visible := not PageControl.Visible;

  ActiveControl := FName;
end;

procedure TDView.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiView, Icon);

  PSQLWait.Caption := Preferences.LoadStr(882);

  TSBasics.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLAlgorithm.Caption := Preferences.LoadStr(743) + ':';
  FAlgorithm.Items.Add('<' + Preferences.LoadStr(744) + '>');
  FAlgorithm.Items.Add(Preferences.LoadStr(745));
  FAlgorithm.Items.Add(Preferences.LoadStr(318));
  FLSecurity.Caption := Preferences.LoadStr(798) + ':';
  FSecurityDefiner.Caption := Preferences.LoadStr(799);
  FSecurityInvoker.Caption := Preferences.LoadStr(561);
  FLCheckOption.Caption := Preferences.LoadStr(248) + ':';
  FCheckOption.Caption := Preferences.LoadStr(529);
  FCheckOptionCascade.Caption := Preferences.LoadStr(256);
  FCheckOptionLocal.Caption := Preferences.LoadStr(746);
  FLStmt.Caption := Preferences.LoadStr(307) + ':';

  FStmt.Font.Name := Preferences.SQLFontName;
  FStmt.Font.Style := Preferences.SQLFontStyle;
  FStmt.Font.Color := Preferences.SQLFontColor;
  FStmt.Font.Size := Preferences.SQLFontSize;
  FStmt.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FStmt.Gutter.Font.Color := clWindowText
  else
    FStmt.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FStmt.Gutter.Color := clBtnFace
  else
    FStmt.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FStmt.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;
  if (Preferences.Editor.AutoIndent) then
    FStmt.Options := FStmt.Options + [eoAutoIndent, eoSmartTabs]
  else
    FStmt.Options := FStmt.Options - [eoAutoIndent, eoSmartTabs];
  if (Preferences.Editor.TabToSpaces) then
    FStmt.Options := FStmt.Options + [eoTabsToSpaces]
  else
    FStmt.Options := FStmt.Options - [eoTabsToSpaces];
  FStmt.RightEdge := Preferences.Editor.RightEdge;
  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FStmt.ActiveLineColor := clNone
  else
    FStmt.ActiveLineColor := Preferences.Editor.CurrRowBGColor;

  TSInformations.Caption := Preferences.LoadStr(121);
  GDefiner.Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
  FLDefiner.Caption := ReplaceStr(Preferences.LoadStr(799), '&', '') + ':';
  GRecordCount.Caption := Preferences.LoadStr(170);
  FLRecordCount.Caption := Preferences.LoadStr(116) + ':';

  TSFields.Caption := Preferences.LoadStr(253);
  FFields.Column[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');
  FFields.Column[1].Caption := Preferences.LoadStr(69);
  FFields.Column[2].Caption := Preferences.LoadStr(71);
  FFields.Column[3].Caption := Preferences.LoadStr(72);
  FFields.Column[4].Caption := ReplaceStr(Preferences.LoadStr(73), '&', '');
  FFields.Column[5].Caption := ReplaceStr(Preferences.LoadStr(111), '&', '');

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

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDView.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDView.FAlgorithmSelect(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDView.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDView.FBOkCheckEnabled(Sender: TObject);
var
  I: Integer;
  Parse: TSQLParse;
begin
  FBOk.Enabled := PageControl.Visible
    and (FName.Text <> '')
    and SQLSingleStmt(FStmt.Text) and SQLCreateParse(Parse, PChar(FStmt.Text), Length(FStmt.Text), Database.Client.ServerVersion) and SQLParseKeyword(Parse, 'SELECT');
  for I := 0 to Database.Tables.Count - 1 do
    if (Database.Client.TableNameCmp(FName.Text, Database.Tables[I].Name) = 0) and not (not Assigned(View) or (Database.Client.TableNameCmp(FName.Text, View.Name) = 0)) then
      FBOk.Enabled := False;
end;

procedure TDView.FCheckOptionCascadeClick(Sender: TObject);
begin
  if (FCheckOptionCascade.Checked) then
  begin
    FCheckOption.Checked := True;
    FCheckOptionLocal.Checked := False;
  end;

  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDView.FCheckOptionCascadeKeyPress(Sender: TObject;
  var Key: Char);
begin
  FCheckOptionCascadeClick(Sender);
end;

procedure TDView.FCheckOptionClick(Sender: TObject);
begin
  if (not FCheckOption.Checked) then
  begin
    FCheckOptionCascade.Checked := False;
    FCheckOptionLocal.Checked := False;
  end;

  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDView.FCheckOptionKeyPress(Sender: TObject; var Key: Char);
begin
  FCheckOptionClick(Sender);
end;

procedure TDView.FCheckOptionLocalClick(Sender: TObject);
begin
  if (FCheckOptionLocal.Checked) then
  begin
    FCheckOption.Checked := True;
    FCheckOptionCascade.Checked := False;
  end;

  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDView.FCheckOptionLocalKeyPress(Sender: TObject; var Key: Char);
begin
  FCheckOptionLocalClick(Sender);
end;

procedure TDView.FNameChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDView.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewView: TCView;
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
  begin
    NewView := TCView.Create(Database.Tables);
    if (Assigned(View)) then
      NewView.Assign(View);

    NewView.Name := Trim(FName.Text);
    case (FAlgorithm.ItemIndex) of
      0: NewView.Algorithm := vaUndefined;
      1: NewView.Algorithm := vaMerge;
      2: NewView.Algorithm := vaTemptable;
    end;
    if (FSecurityDefiner.Checked) then
      NewView.Security := seDefiner
    else if (FSecurityInvoker.Checked) then
      NewView.Security := seInvoker;
    if (not FCheckOption.Checked) then
      NewView.CheckOption := voNone
    else if (FCheckOptionCascade.Checked) then
      NewView.CheckOption := voCascaded
    else if (FCheckOptionLocal.Checked) then
      NewView.CheckOption := voLocal
    else
      NewView.CheckOption := voDefault;
    NewView.Stmt := Trim(FStmt.Lines.Text);

    if (not Assigned(View)) then
      CanClose := Database.AddView(NewView)
    else
      CanClose := Database.UpdateView(View, NewView);

    NewView.Free();

    PageControl.Visible := CanClose or not Database.Client.Asynchron;
    PSQLWait.Visible := not PageControl.Visible;
    if (PSQLWait.Visible) then
      ModalResult := mrNone;

    FBOk.Enabled := False;
  end;
end;

procedure TDView.FormClientEvent(const Event: TCClient.TEvent);
begin
  if ((Event.EventType = ceItemValid) and (Event.CItem = View)) then
    Built()
  else if ((Event.EventType in [ceItemCreated, ceItemAltered]) and (Event.CItem is TCView)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Client.ErrorCode <> 0)) then
  begin
    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end;
end;

procedure TDView.FormCreate(Sender: TObject);
begin
  FFields.SmallImages := Preferences.SmallImages;

  FStmt.Highlighter := MainHighlighter;
  FSource.Highlighter := MainHighlighter;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.View.Width >= Width) and (Preferences.View.Height >= Height)) then
  begin
    Width := Preferences.View.Width;
    Height := Preferences.View.Height;
  end;

  msUndo.Action := MainAction('aEUndo'); msCut.ShortCut := 0;
  msCut.Action := MainAction('aECut'); msCut.ShortCut := 0;
  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msPaste.Action := MainAction('aEPaste'); msPaste.ShortCut := 0;
  msDelete.Action := MainAction('aEDelete'); msDelete.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  PageControl.ActivePage := TSBasics;
end;

procedure TDView.FormHide(Sender: TObject);
begin
  Database.Client.UnRegisterEventProc(FormClientEvent);

  Preferences.View.Width := Width;
  Preferences.View.Height := Height;

  FStmt.Lines.Clear();

  FFields.Items.BeginUpdate();
  FFields.Items.Clear();
  FFields.Items.EndUpdate();

  FSource.Lines.Clear();
  PageControl.ActivePage := TSBasics;
end;

procedure TDView.FormShow(Sender: TObject);
var
  TableName: string;
begin
  Database.Client.RegisterEventProc(FormClientEvent);

  if (not Assigned(View)) then
  begin
    Caption := Preferences.LoadStr(741);
    HelpContext := 1096;
  end
  else
  begin
    Caption := Preferences.LoadStr(842, View.Name);
    HelpContext := 1098;
  end;

  if (not Assigned(View) and (Database.Client.LowerCaseTableNames = 1)) then
    FName.CharCase := ecLowerCase
  else
    FName.CharCase := ecNormal;

  RecordCount := -1;

  if (not Assigned(View)) then
  begin
    FName.Text := Preferences.LoadStr(747);
    while (not Assigned(View) and Assigned(Database.TableByName(FName.Text))) do
    begin
      TableName := FName.Text;
      Delete(TableName, 1, Length(Preferences.LoadStr(747)));
      if (TableName = '') then TableName := '1';
      TableName := Preferences.LoadStr(747) + IntToStr(StrToInt(TableName) + 1);
      FName.Text := TableName;
    end;

    FAlgorithm.ItemIndex := 0;

    FSecurityDefiner.Checked := True;

    FCheckOption.Checked := False;
    FCheckOptionCascade.Checked := False;
    FCheckOptionLocal.Checked := False;

    FStmt.Lines.Text := 'SELECT 1;';

    TSSource.TabVisible := False;

    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end
  else
  begin
    PageControl.Visible := View.Update();
    PSQLWait.Visible := not PageControl.Visible;

    if (PageControl.Visible) then
      Built();
  end;

  TSInformations.TabVisible := Assigned(View);
  TSFields.TabVisible := Assigned(View);

  FBOk.Enabled := PageControl.Visible and not Assigned(View);

  ActiveControl := FBCancel;
  if (PageControl.Visible) then
    ActiveControl := FName;
end;

procedure TDView.FSecurityClick(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDView.FSecurityKeyPress(Sender: TObject; var Key: Char);
begin
  FSecurityClick(Sender);
end;

procedure TDView.FSourceChange(Sender: TObject);
begin
  TSFields.TabVisible := False;
end;

procedure TDView.FSourceStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  MainAction('aECopyToFile').Enabled := FSource.SelText <> '';
end;

procedure TDView.FStmtChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  TSFields.TabVisible := False;
  TSSource.TabVisible := False;
end;

procedure TDView.TSInformationsShow(Sender: TObject);
begin
  FRecordCount.Caption := '???';

  if (RecordCount < 0) then
    RecordCount := View.CountRecords;

  FRecordCount.Caption := FormatFloat('#,##0', RecordCount, LocaleFormatSettings);
end;

initialization
  FView := nil;
end.
