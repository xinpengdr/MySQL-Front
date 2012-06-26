unit fDSQLHelp;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Menus,
  Forms_Ext, ExtCtrls_Ext,
  fClient,
  fBase;

type
  TDSQLHelp = class(TForm_Ext)
    FBDescription: TButton;
    FBExample: TButton;
    FBManual: TButton;
    FDescription: TRichEdit;
    FExample: TRichEdit;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    Panel: TPanel_Ext;
    procedure FBDescriptionClick(Sender: TObject);
    procedure FBExampleClick(Sender: TObject);
    procedure FBManualClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    Description: string;
    Example: string;
    ManualURL: string;
    Title: string;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  public
    Client: TCClient;
    Keyword: string;
    function Execute(): Boolean;
  end;

function DSQLHelp(): TDSQLHelp;

implementation {***************************************************************}

{$R *.dfm}

uses
  ShellAPI, RichEdit,
  StrUtils,
  MySQLDB, SQLUtils,
  fPreferences, fDSelection;

var
  FSQLHelp: TDSQLHelp;

function DSQLHelp(): TDSQLHelp;
begin
  if (not Assigned(FSQLHelp)) then
  begin
    Application.CreateForm(TDSQLHelp, FSQLHelp);
    FSQLHelp.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FSQLHelp;
end;

{ TDSQLHelp *******************************************************************}

procedure TDSQLHelp.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(14, Icon);

  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBDescription.Caption := '&' + Preferences.LoadStr(85);
  FBExample.Caption := Preferences.LoadStr(849);
  FBManual.Caption := Preferences.LoadStr(573);

  FDescription.Font.Name := Preferences.SQLFontName;
  FDescription.Font.Style := Preferences.SQLFontStyle;
  FDescription.Font.Size := Preferences.SQLFontSize;
  FDescription.Font.Charset := Preferences.SQLFontCharset;

  FExample.Font.Name := Preferences.SQLFontName;
  FExample.Font.Style := Preferences.SQLFontStyle;
  FExample.Font.Size := Preferences.SQLFontSize;
  FExample.Font.Charset := Preferences.SQLFontCharset;

  Perform(CM_SYSFONTCHANGED, 0, 0);
end;

procedure TDSQLHelp.CMPostShow(var Message: TMessage);
begin
  ActiveControl := nil;
  BringToFront();
end;

procedure TDSQLHelp.CMSysFontChanged(var Message: TMessage);
begin
  FBDescription.Width := Canvas.TextWidth(FBDescription.Caption) + FBDescription.Height - Canvas.TextHeight(FBDescription.Caption);
  FBExample.Width := Canvas.TextWidth(FBExample.Caption) + FBExample.Height - Canvas.TextHeight(FBExample.Caption);
  FBManual.Width := Canvas.TextWidth(FBManual.Caption) + FBManual.Height - Canvas.TextHeight(FBManual.Caption);
  FBExample.Left := FBDescription.Left + FBDescription.Width;
  FBManual.Left := FBExample.Left + FBExample.Width;
end;

function TDSQLHelp.Execute(): Boolean;
var
  Cancel: Boolean;
  DataSet: TMySQLQuery;
begin
  Result := False;

  DataSet := TMySQLQuery.Create(Self);
  DataSet.Connection := Client;
  if (Trim(Keyword) = '') then
    DataSet.CommandText := 'HELP ' + SQLEscape('Contents')
  else
    DataSet.CommandText := 'HELP ' + SQLEscape(SQLUnwrapStmt(Keyword));
  DataSet.Open();

  if (not DataSet.Active or DataSet.IsEmpty() or not Assigned(DataSet.FindField('name'))) then
    MessageBeep(MB_ICONERROR)
  else
  begin
    Cancel := False;
    while (not Cancel and not Assigned(DataSet.FindField('description')) and not DataSet.IsEmpty()) do
    begin
      Hide();

      repeat
        SetLength(DSelection.Values, Length(DSelection.Values) + 1);
        DSelection.Values[Length(DSelection.Values) - 1] := DataSet.FieldByName('name').AsString;
      until (not DataSet.FindNext());
      Cancel := not DSelection.Execute();
      if (not Cancel) then
      begin
        DataSet.Close();
        DataSet.CommandText := 'HELP ' + SQLEscape(DSelection.Selected);
        DataSet.Open();
      end;
    end;

    if (not Cancel and Assigned(DataSet.FindField('description'))) then
    begin
      Title := DataSet.FieldByName('name').AsString;
      Description := Trim(DataSet.FieldByName('description').AsString);
      Example := Trim(DataSet.FieldByName('example').AsString);
      ManualURL := Client.Account.ManualURL;

      Result := True;
    end;
  end;

  DataSet.Free();

  if (Result) then
    if (Visible) then
      FormShow(nil)
    else
      Show();
end;

procedure TDSQLHelp.FBDescriptionClick(Sender: TObject);
begin
  FDescription.BringToFront();
  ActiveControl := nil;
end;

procedure TDSQLHelp.FBExampleClick(Sender: TObject);
begin
  FExample.BringToFront();
  ActiveControl := nil;
end;

procedure TDSQLHelp.FBManualClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(ManualURL), '', '', SW_SHOW);

  Close();
end;

procedure TDSQLHelp.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.SQLHelp.Width >= Width) and (Preferences.SQLHelp.Height >= Height)) then
  begin
    Width := Preferences.SQLHelp.Width;
    Height := Preferences.SQLHelp.Height;
  end;
  if ((0 <= Preferences.SQLHelp.Left) and (Preferences.SQLHelp.Left + Width <= Screen.Width)
    and (0 <= Preferences.SQLHelp.Top) and (Preferences.SQLHelp.Top + Height <= Screen.Height)) then
  begin
    Left := Preferences.SQLHelp.Left;
    Top := Preferences.SQLHelp.Top;
  end
  else
  begin
    Left := Application.MainForm.Left + Application.MainForm.Width div 2 - Width div 2;
    Top := Application.MainForm.Top + Application.MainForm.Height div 2 - Height div 2;
  end;

  SendMessage(FDescription.Handle, EM_SETEVENTMASK, 0, SendMessage(FDescription.Handle, EM_GETEVENTMASK, 0, 0) or ENM_LINK);
  SendMessage(FDescription.Handle, EM_AUTOURLDETECT, Integer(True), 0);
end;

procedure TDSQLHelp.FormHide(Sender: TObject);
begin
  FDescription.Lines.Clear();
  FExample.Lines.Clear();

  Preferences.SQLHelp.Left := Left;
  Preferences.SQLHelp.Top := Top;
  Preferences.SQLHelp.Width := Width;
  Preferences.SQLHelp.Height := Height;
end;

procedure TDSQLHelp.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
    Hide();
end;

procedure TDSQLHelp.FormShow(Sender: TObject);
var
  URL: string;
begin
  Caption := ReplaceStr(Preferences.LoadStr(883), '&', '') + ': ' + Title;

  FDescription.Text := Description;
  if (Pos('URL: ', FDescription.Lines[FDescription.Lines.Count - 1]) = 1) then
  begin
    URL := FDescription.Lines[FDescription.Lines.Count - 1];
    Delete(URL, 1, Length('URL: '));
    URL := Trim(URL);
    ManualURL := URL;
    FDescription.Lines.Delete(FDescription.Lines.Count - 1);
    while ((FDescription.Lines.Count > 0) and (Trim(FDescription.Lines[FDescription.Lines.Count - 1]) = '')) do
      FDescription.Lines.Delete(FDescription.Lines.Count - 1);
  end;

  FExample.Lines.Text := Example;

  FBExample.Enabled := Example <> '';
  FBManual.Enabled := ManualURL <> '';

  FBDescription.Click();

  PostMessage(Handle, CM_POSTSHOW, 0, 0);
end;

procedure TDSQLHelp.WMNotify(var Message: TWMNotify);
var
  ENLink: TENLink;
  SelStart: Integer;
  URL: string;
begin
  if (Message.NMHdr.code = EN_LINK) then
  begin
    ENLink := TENLink(Pointer(Message.NMHdr)^);
    if (ENLink.Msg = WM_LBUTTONDOWN) then
    begin
      SelStart := FDescription.SelStart;
      SendMessage(FDescription.Handle, EM_EXSETSEL, 0, LPARAM(@(ENLink.chrg)));
      URL := FDescription.SelText;
      FDescription.SelStart := SelStart;
      FDescription.SelLength := 0;

      ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
    end
  end
end;

initialization
  FSQLHelp := nil;
end.
