unit fDSQLHelp;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Menus,
  Forms_Ext, ExtCtrls_Ext,
  MySQLDB,
  fClient,
  fBase;

type
  TDSQLHelp = class(TForm_Ext)
    FBDescription: TButton;
    FBExample: TButton;
    FBManual: TButton;
    FDescription: TRichEdit;
    FExample: TRichEdit;
    FQuickSearch: TEdit;
    FQuickSearchEnabled: TToolButton;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    Panel: TPanel_Ext;
    TBQuickSearchEnabled: TToolBar;
    FBContent: TButton;
    procedure FBDescriptionClick(Sender: TObject);
    procedure FBExampleClick(Sender: TObject);
    procedure FBManualClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FQuickSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FQuickSearchEnabledClick(Sender: TObject);
    procedure FBContentClick(Sender: TObject);
  const
    CM_SEND_SQL = WM_USER + 300;
  private
    ManualURL: string;
    function ClientResult(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMSendSQL(var Message: TMessage); message CM_SEND_SQL;
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
  ShellAPI, RichEdit, CommCtrl,
  StrUtils,
  SQLUtils,
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

function TDSQLHelp.ClientResult(const Connection: TMySQLConnection; const Data: Boolean): Boolean;
var
  DataSet: TMySQLQuery;
begin
  if (Connection.ErrorCode = 0) then
  begin
    DataSet := TMySQLQuery.Create(Owner);
    DataSet.Connection := Connection;
    DataSet.Open();

    if (Assigned(DataSet.FindField('description')) and not DataSet.IsEmpty()) then
    begin
      ManualURL := Client.Account.ManualURL;

      Caption := ReplaceStr(Preferences.LoadStr(883), '&', '') + ': ' + DataSet.FieldByName('name').AsString;

      FDescription.Lines.Text := Trim(DataSet.FieldByName('description').AsString);
      FBDescription.Enabled := True;

      FExample.Lines.Text := Trim(DataSet.FieldByName('example').AsString);
      FBExample.Enabled := Trim(DataSet.FieldByName('example').AsString) <> '';

      if (Pos('URL: ', FDescription.Lines[FDescription.Lines.Count - 1]) <> 1) then
        ManualURL := Client.Account.ManualURL
      else
      begin
        ManualURL := FDescription.Lines[FDescription.Lines.Count - 1];
        Delete(ManualURL, 1, Length('URL: '));
        ManualURL := Trim(ManualURL);
        FBManual.Enabled := ManualURL <> '';
        FDescription.Lines.Delete(FDescription.Lines.Count - 1);
        while ((FDescription.Lines.Count > 0) and (Trim(FDescription.Lines[FDescription.Lines.Count - 1]) = '')) do
          FDescription.Lines.Delete(FDescription.Lines.Count - 1);
      end;
      FBManual.Enabled := ManualURL <> '';

      FBDescription.Click();
    end
    else if (Assigned(DataSet.FindField('name')) and not DataSet.IsEmpty()) then
    begin
      repeat
        SetLength(DSelection.Values, Length(DSelection.Values) + 1);
        DSelection.Values[Length(DSelection.Values) - 1] := DataSet.FieldByName('name').AsString;
      until (not DataSet.FindNext());
      if (DSelection.Execute()) then
      begin
        Keyword := DSelection.Selected;
        PostMessage(Handle, CM_SEND_SQL, 0, 0);
      end
      else if (FDescription.Lines.Count < 1) then
        Hide();
    end
    else
    begin
      MsgBox(Preferences.LoadStr(533, Keyword), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
      if (FDescription.Lines.Count < 1) then
        Hide();
    end;

    DataSet.Free();
  end;

  Result := False;
end;

procedure TDSQLHelp.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(14, Icon);

  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBContent.Caption := Preferences.LoadStr(653);
  FBDescription.Caption := Preferences.LoadStr(85);
  FBExample.Caption := Preferences.LoadStr(849);
  FBManual.Caption := Preferences.LoadStr(573);
  FQuickSearch.Hint := ReplaceStr(Preferences.LoadStr(424), '&', '');
  if (CheckWin32Version(6)) then
    SendMessage(FQuickSearch.Handle, EM_SETCUEBANNER, 0, LParam(PChar(ReplaceStr(Preferences.LoadStr(424), '&', ''))));
  FQuickSearchEnabled.Hint := ReplaceStr(Preferences.LoadStr(424), '&', '');

  FDescription.Font.Name := Preferences.SQLFontName;
  FDescription.Font.Style := Preferences.SQLFontStyle;
  FDescription.Font.Size := Preferences.SQLFontSize;
  FDescription.Font.Charset := Preferences.SQLFontCharset;
  FQuickSearch.Font := FDescription.Font;

  FExample.Font.Name := Preferences.SQLFontName;
  FExample.Font.Style := Preferences.SQLFontStyle;
  FExample.Font.Size := Preferences.SQLFontSize;
  FExample.Font.Charset := Preferences.SQLFontCharset;

  Perform(CM_SYSFONTCHANGED, 0, 0);
end;

procedure TDSQLHelp.CMSendSQL(var Message: TMessage);
begin
  Client.SendSQL('HELP ' + SQLEscape(Keyword), ClientResult);
end;

procedure TDSQLHelp.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FBContent.Width := Canvas.TextWidth(FBContent.Caption) + FBContent.Height - Canvas.TextHeight(FBContent.Caption);
  FBDescription.Left := FBContent.Left + FBContent.Width;
  FBDescription.Width := Canvas.TextWidth(FBDescription.Caption) + FBDescription.Height - Canvas.TextHeight(FBDescription.Caption);
  FBExample.Left := FBDescription.Left + FBDescription.Width;
  FBExample.Width := Canvas.TextWidth(FBExample.Caption) + FBExample.Height - Canvas.TextHeight(FBExample.Caption);
  FBManual.Left := FBExample.Left + FBExample.Width;
  FBManual.Width := Canvas.TextWidth(FBManual.Caption) + FBManual.Height - Canvas.TextHeight(FBManual.Caption);

  Constraints.MinWidth := 2 * GetSystemMetrics(SM_CXFRAME) + 2 * Panel.BevelWidth + FBDescription.Width + FBExample.Width + FBManual.Width + FQuickSearch.Width + TBQuickSearchEnabled.Width + 50;
end;

function TDSQLHelp.Execute(): Boolean;
begin
  Show();

  Keyword := Trim(SQLUnwrapStmt(Keyword));
  if (Keyword = '') then
    Keyword := 'Contents';
  Perform(CM_SEND_SQL, 0, 0);

  Result := False;
end;

procedure TDSQLHelp.FBContentClick(Sender: TObject);
begin
  Keyword := 'Contents';
  Perform(CM_SEND_SQL, 0, 0);
end;

procedure TDSQLHelp.FBDescriptionClick(Sender: TObject);
begin
  FDescription.Visible := True;
  FExample.Visible := False;
  ActiveControl := nil;
end;

procedure TDSQLHelp.FBExampleClick(Sender: TObject);
begin
  FDescription.Visible := False;
  FExample.Visible := True;
  ActiveControl := nil;
end;

procedure TDSQLHelp.FBManualClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(ManualURL), '', '', SW_SHOW);

  Close();
end;

procedure TDSQLHelp.FormCreate(Sender: TObject);
begin
  ShowGripper := False;

  TBQuickSearchEnabled.Images := Preferences.SmallImages;

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
begin
  Caption := ReplaceStr(Preferences.LoadStr(883), '&', '');

  FDescription.Lines.Clear();
  FDescription.Visible := False;
  FBDescription.Enabled := False;
  FExample.Visible := False;
  FBExample.Enabled := False;
  FBManual.Enabled := False;

  FDescription.BringToFront();
end;

procedure TDSQLHelp.FQuickSearchEnabledClick(Sender: TObject);
begin
  Keyword := FQuickSearch.Text;
  Perform(CM_SEND_SQL, 0, 0);
end;

procedure TDSQLHelp.FQuickSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(VK_ESCAPE)) then
  begin
    FQuickSearch.Text := '';

    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FQuickSearchEnabled.Down) then
  begin
    FQuickSearchEnabled.Click();

    FQuickSearch.SelStart := 0;
    FQuickSearch.SelLength := Length(FQuickSearch.Text);
    Key := #0;
  end;
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
