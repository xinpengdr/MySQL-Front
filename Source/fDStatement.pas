unit fDStatement;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  SynEdit, SynMemo,
  Forms_Ext,
  fBase, StdCtrls_Ext;

type
  TDStatementViewType = (vtQuery, vtStatement, vtProcess);

  TDStatement = class(TForm_Ext)
    FBClose: TButton;
    FDatabase: TLabel;
    FExecutionTime: TLabel;
    FFieldCount: TLabel;
    FHost: TLabel;
    FId: TLabel;
    FInfo: TLabel;
    FInsertId: TLabel;
    FLDatabase: TLabel;
    FLExecutionTime: TLabel;
    FLFieldCount: TLabel;
    FLHost: TLabel;
    FLId: TLabel;
    FLInfo: TLabel;
    FLInsertId: TLabel;
    FLQueryTime: TLabel;
    FLRecordCount: TLabel;
    FLRowsAffected: TLabel;
    FLStatementTime: TLabel;
    FLUser: TLabel;
    FQueryTime: TLabel;
    FRecordCount: TLabel;
    FRowsAffected: TLabel;
    FSource: TSynMemo;
    FStatementTime: TLabel;
    FUser: TLabel;
    GBasics: TGroupBox_Ext;
    GProcess: TGroupBox_Ext;
    GQuery: TGroupBox_Ext;
    GStatement: TGroupBox_Ext;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    PageControl: TPageControl;
    TSInformations: TTabSheet;
    TSSource: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    DatabaseName: string;
    DateTime: TDateTime;
    FieldCount: Integer;
    Host: string;
    Id: Int64;
    Info: string;
    RecordCount: Integer;
    RowsAffected: Integer;
    SQL: string;
    StatementTime: TDateTime;
    UserName: string;
    ViewType: TDStatementViewType;
    function Execute(): Boolean;
  end;

function DStatement(): TDStatement;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  MySQLDB, fPreferences;

var
  FStatement: TDStatement;

function DStatement(): TDStatement;
begin
  if (not Assigned(FStatement)) then
  begin
    Application.CreateForm(TDStatement, FStatement);
    FStatement.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FStatement;
end;

{ TDStatement *****************************************************************}

procedure TDStatement.CMChangePreferences(var Message: TMessage);
begin
  TSInformations.Caption := Preferences.LoadStr(121);
  GBasics.Caption := Preferences.LoadStr(85);
  FLExecutionTime.Caption := ReplaceStr(Preferences.LoadStr(520), '&', '') + ':';
  FLDatabase.Caption := ReplaceStr(Preferences.LoadStr(38), '&', '') + ':';

  GStatement.Caption := ReplaceStr(Preferences.LoadStr(662), '&', '');
  FLStatementTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLRowsAffected.Caption := Preferences.LoadStr(808) + ':';
  FLInfo.Caption := ReplaceStr(Preferences.LoadStr(274), '&', '') + ':';
  FLInsertId.Caption := ReplaceStr(Preferences.LoadStr(84), '&', '') + ':';

  GQuery.Caption := ReplaceStr(Preferences.LoadStr(662), '&', '');
  FLQueryTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLFieldCount.Caption := Preferences.LoadStr(253) + ':';
  FLRecordCount.Caption := Preferences.LoadStr(66) + ':';

  GProcess.Caption := ReplaceStr(Preferences.LoadStr(684), '&', '');
  FLId.Caption := Preferences.LoadStr(269) + ':';
  FLUser.Caption := ReplaceStr(Preferences.LoadStr(561), '&', '') + ':';
  FLHost.Caption := ReplaceStr(Preferences.LoadStr(271), '&', '') + ':';

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

  FBClose.Caption := Preferences.LoadStr(231);
end;

function TDStatement.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDStatement.FormCreate(Sender: TObject);
begin
  FSource.Highlighter := MainHighlighter;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Statement.Width >= Width) and (Preferences.Statement.Height >= Height)) then
  begin
    Width := Preferences.Statement.Width;
    Height := Preferences.Statement.Height;
  end;

  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll');
end;

procedure TDStatement.FormHide(Sender: TObject);
begin
  Preferences.Statement.Width := Width;
  Preferences.Statement.Height := Height;
end;

procedure TDStatement.FormShow(Sender: TObject);
begin
  case (ViewType) of
    vtQuery:
      begin
        Preferences.SmallImages.GetIcon(iiStatement, Icon);
        Caption := ReplaceStr(Preferences.LoadStr(794), '&', '');
      end;
    vtStatement:
      begin
        Preferences.SmallImages.GetIcon(iiQuery, Icon);
        Caption := ReplaceStr(Preferences.LoadStr(794), '&', '');
      end;
    vtProcess:
      begin
        Preferences.SmallImages.GetIcon(iiProcess, Icon);
        Caption := ReplaceStr(Preferences.LoadStr(562), '&', '');
      end;
  end;

  if (DateTime = MySQLZeroDate) then
    FExecutionTime.Caption := '???'
  else
    FExecutionTime.Caption := SysUtils.DateTimeToStr(DateTime, LocaleFormatSettings);
  FDatabase.Caption := DatabaseName;

  GStatement.Visible := ViewType = vtStatement;
  if (StatementTime = MySQLZeroDate) then
    FStatementTime.Caption := '???'
  else
    FStatementTime.Caption := ExecutionTimeToStr(StatementTime);
  FRowsAffected.Visible := RowsAffected >= 0; FLRowsAffected.Visible := FRowsAffected.Visible;
  if (FRowsAffected.Visible) then
    FRowsAffected.Caption := IntToStr(RowsAffected);
  FInfo.Visible := Info <> ''; FLInfo.Visible := FInfo.Visible;
  if (FInfo.Visible) then
    FInfo.Caption := Info;
  FInsertId.Visible := Id > 0; FLInsertId.Visible := FInsertId.Visible;
  if (FInsertId.Visible) then
    FInsertId.Caption := IntToStr(Id);

  GQuery.Visible := ViewType = vtQuery;
  FQueryTime.Caption := FStatementTime.Caption;
  FFieldCount.Visible := FieldCount >= 0; FLFieldCount.Visible := FFieldCount.Visible;
  if (FFieldCount.Visible) then
    FFieldCount.Caption := IntToStr(FieldCount);
  FRecordCount.Visible := RecordCount >= 0; FLRecordCount.Visible := FRecordCount.Visible;
  if (FRecordCount.Visible) then
    FRecordCount.Caption := IntToStr(RecordCount);

  GProcess.Visible := ViewType = vtProcess;
  FId.Caption := IntToStr(Id);
  FUser.Caption := UserName;
  FHost.Caption := Host;

  TSSource.TabVisible := SQL <> '';
  if (TSSource.TabVisible) then
  begin
    FSource.Lines.Text := SQL + #13#10;
    FSource.ReadOnly := True;
  end;

  PageControl.ActivePage := TSInformations;
  ActiveControl := FBClose;
end;

initialization
  FStatement := nil;
end.

