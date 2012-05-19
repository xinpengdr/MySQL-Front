unit fDView;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, Menus,
  SynEdit, SynMemo,
  Forms_Ext,
  fBase, fClient, StdCtrls_Ext;

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
    TSBasics: TTabSheet;
    TSInformations: TTabSheet;
    TSSource: TTabSheet;
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
    procedure HideTSSource(Sender: TObject);
    procedure TSInformationsShow(Sender: TObject);
  private
    RecordCount: Integer;
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

procedure TDView.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiView, Icon);

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
  HideTSSource(Sender);
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
  FBOk.Enabled := (FName.Text <> '') and SQLSingleStmt(FStmt.Text) and SQLCreateParse(Parse, PChar(FStmt.Text), Length(FStmt.Text), Database.Client.ServerVersion) and SQLParseKeyword(Parse, 'SELECT');
  for I := 0 to Database.Tables.Count - 1 do
    if (lstrcmpi(PChar(FName.Text), PChar(Database.Tables[I].Name)) = 0) and not (not Assigned(View) or (lstrcmpi(PChar(FName.Text), PChar(View.Name)) = 0)) then
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
  HideTSSource(Sender);
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
  HideTSSource(Sender);
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
  HideTSSource(Sender);
end;

procedure TDView.FCheckOptionLocalKeyPress(Sender: TObject; var Key: Char);
begin
  FCheckOptionLocalClick(Sender);
end;

procedure TDView.FNameChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  HideTSSource(Sender);
end;

procedure TDView.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewView: TCView;
begin
  if (ModalResult = mrOk) then
  begin
    NewView := TCView.Create(Database);
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
  end;
end;

procedure TDView.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (Event.EventType in [ceBuild, ceUpdated]) then
  begin
    FStmt.Cursor := crDefault;
    FStmt.Lines.Text := Trim(SQLWrapStmt(View.Stmt, ['from', 'where', 'group by', 'having', 'order by', 'limit', 'procedure'], 0)) + #13#10;
    FSource.Cursor := crDefault;
    FSource.Lines.Text := View.Source + #13#10;
  end;
end;

procedure TDView.FormCreate(Sender: TObject);
begin
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

    FSource.Lines.Clear();
  end
  else
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

    if (Database.Initialize(View)) then
    begin
      FStmt.Cursor := crSQLWait;
      FStmt.Lines.Clear();
      FSource.Cursor := crSQLWait;
      FSource.Lines.Clear();
    end
    else
    begin
      FStmt.Cursor := crDefault;
      FStmt.Lines.Text := Trim(SQLWrapStmt(View.Stmt, ['from', 'where', 'group by', 'having', 'order by', 'limit', 'procedure'], 0)) + #13#10;
      FSource.Cursor := crDefault;
      FSource.Lines.Text := View.Source + #13#10;
    end;

    FDefiner.Caption := View.Definer;
  end;

  TSInformations.TabVisible := Assigned(View);
  TSSource.TabVisible := Assigned(View);

  FBOk.Enabled := not Assigned(View);

  ActiveControl := FBCancel;
  ActiveControl := FName;
end;

procedure TDView.FSecurityClick(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  HideTSSource(Sender);
end;

procedure TDView.FSecurityKeyPress(Sender: TObject; var Key: Char);
begin
  FSecurityClick(Sender);
end;

procedure TDView.FSourceStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  MainAction('aECopyToFile').Enabled := FSource.SelText <> '';
end;

procedure TDView.FStmtChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  HideTSSource(Sender);
end;

procedure TDView.HideTSSource(Sender: TObject);
begin
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
