unit fDRoutine;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls,
  Forms_Ext,
  SynEdit, SynMemo,
  fBase, fClient, StdCtrls_Ext;

type
  TDRoutine = class(TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FComment: TEdit;
    FCreated: TLabel;
    FDefiner: TLabel;
    FLComment: TLabel;
    FLCreated: TLabel;
    FLDefiner: TLabel;
    FLName: TLabel;
    FLSecurity: TLabel;
    FLSize: TLabel;
    FLUpdated: TLabel;
    FName: TEdit;
    FSecurityDefiner: TRadioButton;
    FSecurityInvoker: TRadioButton;
    FSize: TLabel;
    FSource: TSynMemo;
    FUpdated: TLabel;
    GBasics: TGroupBox_Ext;
    GDates: TGroupBox_Ext;
    GDefiner: TGroupBox_Ext;
    GSize: TGroupBox_Ext;
    msCopy: TMenuItem;
    msCut: TMenuItem;
    msDelete: TMenuItem;
    MSource: TPopupMenu;
    msPaste: TMenuItem;
    msSelectAll: TMenuItem;
    msUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PageControl_DRoutine: TPageControl;
    TSBasics: TTabSheet;
    TSInformations: TTabSheet;
    TSSource: TTabSheet;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FCommentChange(Sender: TObject);
    procedure FNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSecurityClick(Sender: TObject);
    procedure FSecurityKeyPress(Sender: TObject; var Key: Char);
    procedure FSourceChange(Sender: TObject);
  private
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Database: TCDatabase;
    Routine: TCRoutine;
    RoutineType: TCRoutine.TRoutineType;
    function Execute(): Boolean;
  end;

function DRoutine(): TDRoutine;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  SQLUtils,
  fPreferences;

var
  FRoutine: TDRoutine;

function DRoutine(): TDRoutine;
begin
  if (not Assigned(FRoutine)) then
  begin
    Application.CreateForm(TDRoutine, FRoutine);
    FRoutine.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FRoutine;
end;

{ TDRoutine *******************************************************************}

procedure TDRoutine.CMChangePreferences(var Message: TMessage);
begin
  TSBasics.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLSecurity.Caption := Preferences.LoadStr(798) + ':';
  FSecurityDefiner.Caption := Preferences.LoadStr(799);
  FSecurityInvoker.Caption := Preferences.LoadStr(561);
  FLComment.Caption := Preferences.LoadStr(111) + ':';

  TSInformations.Caption := Preferences.LoadStr(121);
  GDates.Caption := Preferences.LoadStr(122);
  FLCreated.Caption := Preferences.LoadStr(118) + ':';
  FLUpdated.Caption := Preferences.LoadStr(119) + ':';
  GDefiner.Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
  FLDefiner.Caption := ReplaceStr(Preferences.LoadStr(799), '&', '') + ':';
  GSize.Caption := Preferences.LoadStr(67);
  FLSize.Caption := Preferences.LoadStr(67) + ':';

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
  FSource.Gutter.Visible := Preferences.Editor.LineNumbers;
  if (Preferences.Editor.AutoIndent) then
    FSource.Options := FSource.Options + [eoAutoIndent, eoSmartTabs]
  else
    FSource.Options := FSource.Options - [eoAutoIndent, eoSmartTabs];
  if (Preferences.Editor.TabToSpaces) then
    FSource.Options := FSource.Options + [eoTabsToSpaces]
  else
    FSource.Options := FSource.Options - [eoTabsToSpaces];
  FSource.TabWidth := Preferences.Editor.TabWidth;
  FSource.RightEdge := Preferences.Editor.RightEdge;
  FSource.WantTabs := Preferences.Editor.TabAccepted;
  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FSource.ActiveLineColor := clNone
  else
    FSource.ActiveLineColor := Preferences.Editor.CurrRowBGColor;

  msUndo.Action := MainAction('aEUndo'); msCut.ShortCut := 0;
  msCut.Action := MainAction('aECut'); msCut.ShortCut := 0;
  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msPaste.Action := MainAction('aEPaste'); msPaste.ShortCut := 0;
  msDelete.Action := MainAction('aEDelete'); msDelete.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDRoutine.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDRoutine.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDRoutine.FBOkCheckEnabled(Sender: TObject);
var
  DDLStmt: TSQLDDLStmt;
  SQL: string;
begin
  SQL := Trim(FSource.Text);

  FBOk.Enabled := (not Assigned(Routine) or Assigned(Routine) and (Routine.Source <> ''))
    and (not TSBasics.Visible or not Assigned(Routine) or (FName.Text <> '') and ((lstrcmpi(PChar(FName.Text), PChar(Routine.Name)) = 0) or ((Routine.RoutineType = rtProcedure) and not Assigned(Database.ProcedureByName(FName.Text)) or ((Routine.RoutineType = rtFunction) and not Assigned(Database.FunctionByName(FName.Text))))))
    and (not TSSource.Visible or SQLSingleStmt(FSource.Text) and SQLParseDDLStmt(DDLStmt, PChar(FSource.Text), Length(FSource.Text), Database.Client.ServerVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction]) and ((DDLStmt.DatabaseName = '') or (Database.Client.DatabaseByName(DDLStmt.DatabaseName) = Database)));

  TSInformations.TabVisible := False;
end;

procedure TDRoutine.FCommentChange(Sender: TObject);
begin
  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FNameChange(Sender: TObject);
begin
  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewRoutine: TCRoutine;
begin
  if (ModalResult = mrOk) then
  begin
    NewRoutine := TCRoutine.Create(Database.Routines);
    if (Assigned(Routine)) then
      NewRoutine.Assign(Routine);

    NewRoutine.Name := Trim(FName.Text);
    if (FSecurityDefiner.Checked) then
      NewRoutine.Security := seDefiner
    else if (FSecurityInvoker.Checked) then
      NewRoutine.Security := seInvoker;
    if (not Assigned(Routine) or (Trim(FComment.Text) <> SQLUnwrapStmt(Routine.Comment))) then
      NewRoutine.Comment := Trim(FComment.Text);
    NewRoutine.Source := Trim(FSource.Text);

    if (not Assigned(Routine)) then
      CanClose := Database.AddRoutine(NewRoutine)
    else
      CanClose := Database.UpdateRoutine(Routine, NewRoutine);

    NewRoutine.Free();
  end;
end;

procedure TDRoutine.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Routine.Width >= Width) and (Preferences.Routine.Height >= Height)) then
  begin
    Width := Preferences.Routine.Width;
    Height := Preferences.Routine.Height;
  end;

  FSource.Highlighter := MainHighlighter;
end;

procedure TDRoutine.FormHide(Sender: TObject);
begin
  Preferences.Routine.Width := Width;
  Preferences.Routine.Height := Height;
end;

procedure TDRoutine.FormShow(Sender: TObject);
var
  I: Integer;
  RoutineName: string;
begin
  if (not Assigned(Routine)) then
  begin
    Caption := Preferences.LoadStr(775);
    Preferences.SmallImages.GetIcon(iiProcedure, Icon);
    HelpContext := 1097;
  end
  else if (Routine.RoutineType = rtProcedure) then
  begin
    Caption := Preferences.LoadStr(842, Routine.Name);
    Preferences.SmallImages.GetIcon(iiProcedure, Icon);
    HelpContext := 1099;
  end
  else if (Routine.RoutineType = rtFunction) then
  begin
    Caption := Preferences.LoadStr(842, Routine.Name);
    Preferences.SmallImages.GetIcon(iiFunction, Icon);
    HelpContext := 1099;
  end;

  FComment.Enabled := True; FLComment.Enabled := FComment.Enabled;

  if (not Assigned(Routine)) then
  begin
    if (RoutineType = rtProcedure) then
    begin
      RoutineName := Preferences.LoadStr(863);
      I := 2;
      while (Assigned(Database.ProcedureByName(RoutineName))) do
      begin
        RoutineName := Preferences.LoadStr(863) + IntToStr(I);
        Inc(I);
      end;

      FSource.Lines.Clear();
      FSource.Lines.Add('CREATE PROCEDURE ' + Database.Client.EscapeIdentifier(RoutineName) + '(' + Database.Client.EscapeIdentifier('Param') + ' int(11))');
      FSource.Lines.Add('BEGIN');
      FSource.Lines.Add('END;');
    end
    else if (RoutineType = rtFunction) then
    begin
      RoutineName := Preferences.LoadStr(864);
      I := 2;
      while (Assigned(Database.FunctionByName(RoutineName))) do
      begin
        RoutineName := Preferences.LoadStr(864) + IntToStr(I);
        Inc(I);
      end;

      FSource.Lines.Clear();
      FSource.Lines.Add('CREATE FUNCTION ' + Database.Client.EscapeIdentifier(RoutineName) + '(' + Database.Client.EscapeIdentifier('Param') + ' int(11)) RETURNS int(11)');
      FSource.Lines.Add('BEGIN');
      FSource.Lines.Add('  RETURN Param;');
      FSource.Lines.Add('END;');
    end
    else
      FSource.Lines.Clear();
  end
  else
  begin
    FName.Text := Routine.Name;

    case (Routine.Security) of
      seDefiner: FSecurityDefiner.Checked := True;
      seInvoker: FSecurityInvoker.Checked := True;
    end;
    FComment.Text := SQLUnwrapStmt(Routine.Comment);

    if (Double(Routine.Created) = 0) then FCreated.Caption := '???' else FCreated.Caption := SysUtils.DateTimeToStr(Routine.Created, LocaleFormatSettings);
    if (Double(Routine.Modified) = 0) then FUpdated.Caption := '???' else FUpdated.Caption := SysUtils.DateTimeToStr(Routine.Modified, LocaleFormatSettings);
    FDefiner.Caption := Routine.Definer;
    FSize.Caption := FormatFloat('#,##0', Length(Routine.Source), LocaleFormatSettings);

    FSource.Text := Trim(Routine.Source) + #13#10;
  end;

  TSBasics.TabVisible := Assigned(Routine);
  TSInformations.TabVisible := Assigned(Routine);
  TSSource.TabVisible := not Assigned(Routine) or (Routine.Source <> '');

  ActiveControl := FBCancel;
  if (TSBasics.TabVisible) then
  begin
    PageControl_DRoutine.ActivePage := TSBasics;
    ActiveControl := FComment;
  end
  else if (TSSource.TabVisible) then
  begin
    PageControl_DRoutine.ActivePage := TSSource;
    ActiveControl := FSource;
  end;

  FBOk.Enabled := not Assigned(Routine);
end;

procedure TDRoutine.FSecurityClick(Sender: TObject);
begin
  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FSecurityKeyPress(Sender: TObject;
  var Key: Char);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FSourceChange(Sender: TObject);
begin
  MainAction('aECopyToFile').Enabled := FSource.SelText <> '';

  FName.Enabled := False; FLName.Enabled := FName.Enabled;
  FComment.Enabled := False; FLComment.Enabled := FComment.Enabled;

  TSBasics.TabVisible := False;
  TSInformations.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

initialization
  FRoutine := nil;
end.
