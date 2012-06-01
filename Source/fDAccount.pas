unit fDAccount;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext, Dialogs_Ext,
  fAccount, fBase, fClient, SynEdit, SynMemo, Menus;

type
  TDAccountShowType = (stDefault, stLogin);

type
  TDAccount = class (TForm_Ext)
    FBCancel: TButton;
    FBDatabase: TButton;
    FBHelp: TButton;
    FBIcon: TButton;
    FBOk: TButton;
    FCacheSize: TEdit;
    FCharset: TComboBox_Ext;
    FCompression: TCheckBox;
    FConnectionType: TComboBox_Ext;
    FDatabase: TEdit;
    FDefaultSorting: TCheckBox;
    FHost: TEdit;
    FHTTPTunnelURI: TEdit;
    FIcon: TImage;
    FL2CacheSize: TLabel;
    FLCacheSize: TLabel;
    FLCharset: TLabel;
    FLCompression: TLabel;
    FLConnectionType: TLabel;
    FLDatabase: TLabel;
    FLDefaultSorting: TLabel;
    FLHost: TLabel;
    FLHTTPTunnelURI: TLabel;
    FLibraryFilename: TEdit;
    FLIcon: TLabel;
    FLimitOff: TRadioButton;
    FLimitOn: TRadioButton;
    FLimitRemember: TRadioButton;
    FLLibraryFilename: TLabel;
    FLLimit: TLabel;
    FLMultiStatements: TLabel;
    FLAsynchron: TLabel;
    FLName: TLabel;
    FLPassword: TLabel;
    FLPort: TLabel;
    FLPrefetch: TLabel;
    FLUseInformationSchema: TLabel;
    FLUser: TLabel;
    FMultiStatements: TCheckBox;
    FAsynchron: TCheckBox;
    FName: TEdit;
    FPassword: TEdit;
    FPort: TEdit;
    FPrefetch: TComboBox;
    FStartup: TSynMemo;
    FUDCacheSize: TUpDown;
    FUDPort: TUpDown;
    FUseInformationSchema: TCheckBox;
    FUser: TEdit;
    GBasics: TGroupBox_Ext;
    GBrowser: TGroupBox_Ext;
    GDebug: TGroupBox_Ext;
    GLogin: TGroupBox_Ext;
    GServer: TGroupBox_Ext;
    msCopy: TMenuItem;
    msCut: TMenuItem;
    msDelete: TMenuItem;
    MSource: TPopupMenu;
    msPaste: TMenuItem;
    msSelectAll: TMenuItem;
    msUndo: TMenuItem;
    N2: TMenuItem;
    OpenDialog: TOpenDialog_Ext;
    PageControl: TPageControl;
    PIcon: TPanel_Ext;
    TSBasics: TTabSheet;
    TSConnection: TTabSheet;
    TSDataBrowser: TTabSheet;
    TSDebug: TTabSheet;
    TSLogin: TTabSheet;
    TSStartup: TTabSheet;
    procedure FBDatabaseClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FBIconClick(Sender: TObject);
    procedure FCharsetDropDown(Sender: TObject);
    procedure FConnectionTypeChange(Sender: TObject);
    procedure FEditChange(Sender: TObject);
    procedure FHostExit(Sender: TObject);
    procedure FHTTPTunnelURIChange(Sender: TObject);
    procedure FHTTPTunnelURIEnter(Sender: TObject);
    procedure FIconClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetAccountName(): string;
    procedure TSBasicsShow(Sender: TObject);
  private
    IconChanged: Boolean;
    function CheckConnectInfos(): Boolean;
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Password: string;
    Account: TAAccount;
    ShowType: TDAccountShowType;
    Username: string;
    function Execute(): Boolean;
    property AccountName: string read GetAccountName;
  end;

function DAccount(): TDAccount;

implementation {***************************************************************}

{$R *.dfm}

uses
  WinINet, UITypes,
  StrUtils,
  MySQLConsts,
  MySQLDB,
  fDDatabases, fPreferences;

var
  FAccount: TDAccount;

function DAccount(): TDAccount;
begin
  if (not Assigned(FAccount)) then
  begin
    Application.CreateForm(TDAccount, FAccount);
    FAccount.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FAccount;
end;

{ TDAccount *******************************************************************}

function TDAccount.CheckConnectInfos(): Boolean;
begin
  Result := False;

  if (FHost.Text = '') then
    begin MessageBeep(MB_ICONERROR); PageControl.ActivePage := TSConnection; ActiveControl := FHost; end
  else if ((FConnectionType.ItemIndex = 1) and (FLibraryFilename.Text = '')) then
    begin MessageBeep(MB_ICONERROR); PageControl.ActivePage := TSConnection; ActiveControl := FLibraryFilename; end
  else if ((FConnectionType.ItemIndex = 2) and (FHTTPTunnelURI.Text = '')) then
    begin MessageBeep(MB_ICONERROR); PageControl.ActivePage := TSConnection; ActiveControl := FHTTPTunnelURI; end
  else
    Result := True;
end;

procedure TDAccount.CMChangePreferences(var Message: TMessage);
begin
  TSBasics.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLIcon.Caption := Preferences.LoadStr(761) + ':';

  TSConnection.Caption := Preferences.LoadStr(486);
  GServer.Caption := ReplaceStr(Preferences.LoadStr(37), '&', '');
  FLHost.Caption := Preferences.LoadStr(37) + ':';
  FLPort.Caption := Preferences.LoadStr(436) + ':';
  FLConnectionType.Caption := Preferences.LoadStr(648) + ':';
  FConnectionType.Items.Text := '';
  FConnectionTypeChange(nil);
  FLLibraryFilename.Caption := Preferences.LoadStr(568) + ':';
  FLHTTPTunnelURI.Caption := Preferences.LoadStr(652) + ':';
  FLCharset.Caption := Preferences.LoadStr(682) + ':';

  TSLogin.Caption := Preferences.LoadStr(487);
  GLogin.Caption := Preferences.LoadStr(34);
  FLUser.Caption := Preferences.LoadStr(561) + ':';
  FLPassword.Caption := Preferences.LoadStr(40) + ':';
  FLDatabase.Caption := Preferences.LoadStr(38) + ':';

  TSStartup.Caption := Preferences.LoadStr(805);
  FStartup.Font.Name := Preferences.SQLFontName;
  FStartup.Font.Style := Preferences.SQLFontStyle;
  FStartup.Font.Color := Preferences.SQLFontColor;
  FStartup.Font.Size := Preferences.SQLFontSize;
  FStartup.Font.Charset := Preferences.SQLFontCharset;
  FStartup.Gutter.Font.Name := FStartup.Font.Name;
  FStartup.Gutter.Font.Style := FStartup.Font.Style;
  FStartup.Gutter.Font.Size := FStartup.Font.Size;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FStartup.Gutter.Font.Color := clWindowText
  else
    FStartup.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FStartup.Gutter.Color := clBtnFace
  else
    FStartup.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FStartup.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;
  FStartup.Gutter.Visible := Preferences.Editor.LineNumbers;
  if (Preferences.Editor.AutoIndent) then
    FStartup.Options := FStartup.Options + [eoAutoIndent, eoSmartTabs]
  else
    FStartup.Options := FStartup.Options - [eoAutoIndent, eoSmartTabs];
  if (Preferences.Editor.TabToSpaces) then
    FStartup.Options := FStartup.Options + [eoTabsToSpaces]
  else
    FStartup.Options := FStartup.Options - [eoTabsToSpaces];
  FStartup.TabWidth := Preferences.Editor.TabWidth;
  FStartup.RightEdge := Preferences.Editor.RightEdge;
  FStartup.WantTabs := Preferences.Editor.TabAccepted;
  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FStartup.ActiveLineColor := clNone
  else
    FStartup.ActiveLineColor := Preferences.Editor.CurrRowBGColor;

  TSDataBrowser.Caption := Preferences.LoadStr(17);
  GBrowser.Caption := Preferences.LoadStr(17);
  FLLimit.Caption := ReplaceStr(Preferences.LoadStr(197), '&', '') + ':';
  FLimitOff.Caption := Preferences.LoadStr(697);
  FLimitRemember.Caption := Preferences.LoadStr(698);
  FLimitOn.Caption := Preferences.LoadStr(699);
  FLDefaultSorting.Caption := ReplaceStr(Preferences.LoadStr(9), '&', '') + ':';
  FDefaultSorting.Caption := Preferences.LoadStr(611);

  TSDebug.Caption := 'Debug';
  GDebug.Caption := '';
  FLCacheSize.Caption := 'Cache' + ':';
  FL2CacheSize.Caption := 'MB';
  FLCompression.Caption := Preferences.LoadStr(644) + ':';
  FCompression.Caption := Preferences.LoadStr(645);
  FLMultiStatements.Caption := 'Multi Statements' + ':';
  FMultiStatements.Caption := 'enabled';
  FLAsynchron.Caption := 'Asynchron' + ':';
  FAsynchron.Caption := 'enabled';
  FLUseInformationSchema.Caption := 'Fetch informations' + ':';
  FUseInformationSchema.Caption := 'information_schema';
  FLPrefetch.Caption := 'Prefetch' + ':';
  FPrefetch.Items.Text := 'disabled' + #13#10 + 'automatic' + #13#10 + 'enabled';

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);

  OpenDialog.Title := ReplaceStr(Preferences.LoadStr(581), '&', '');
end;

function TDAccount.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDAccount.FBDatabaseClick(Sender: TObject);
var
  Client: TCClient;
  LibraryName: string;
begin
  if (CheckConnectInfos()) then
  begin
    Client := TCClient.CreateConnection();
    if (Assigned(Client)) then
    begin
      case (FConnectionType.ItemIndex) of
        0: LibraryName := '';
        1: LibraryName := FLibraryFilename.Text;
        2: LibraryName := FHTTPTunnelURI.Text;
      end;

      Client.BeginSilent();
      Client.FirstConnect(FConnectionType.ItemIndex, LibraryName, FHost.Text, FUser.Text, FPassword.Text, '', FUDPort.Position, FCompression.Checked, False);
      if (Client.ErrorCode <> 0) then
        Accounts.OnSQLError(Client, Client.ErrorCode, Client.ErrorMessage)
      else if (Client.Connected) then
      begin
        DDatabases.Client := Client;
        DDatabases.SelectedDatabases := FDatabase.Text;
        if (DDatabases.Execute()) then
          FDatabase.Text := DDatabases.SelectedDatabases;
      end;
      Client.EndSilent();

      Client.Free();
    end;

    ActiveControl := FDatabase;
  end;
end;

procedure TDAccount.FBHelpClick(Sender: TObject);
begin
  if (GetKeyState(VK_MENU) >= 0) then
    Application.HelpContext(HelpContext)
  else
  begin
    TSDataBrowser.TabVisible := not TSDataBrowser.TabVisible;
    TSDebug.TabVisible := not TSDebug.TabVisible;
    if (TSDebug.TabVisible) then
    begin
      PageControl.ActivePage := nil; // needed for Vista Themes
      PageControl.ActivePage := TSDebug;
    end;
  end;
end;

procedure TDAccount.FBIconClick(Sender: TObject);
begin
  OpenDialog.FileName := '';

  if (OpenDialog.Execute()) then
  begin
    Icon := TIcon.Create();
    try
      Icon.LoadFromFile(OpenDialog.FileName);
      Preferences.SmallImages.AddIcon(Icon);
      Preferences.SmallImages.GetIcon(Preferences.SmallImages.Count - 1, FIcon.Picture.Icon);
      IconChanged := True;
    except
      MsgBox(Preferences.LoadStr(523, OpenDialog.FileName), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
    end;
  end;

  if (FBOk.Enabled) then
    ActiveControl := FBOk;
end;

procedure TDAccount.FCharsetDropDown(Sender: TObject);
var
  Client: TCClient;
  I: Integer;
  LibraryName: string;
  List: TList;
begin
  if ((FCharset.Items.Count = 0) and CheckConnectInfos()) then
  begin
    Client := TCClient.CreateConnection();
    if (Assigned(Client)) then
    begin
      case (FConnectionType.ItemIndex) of
        0: LibraryName := '';
        1: LibraryName := FLibraryFilename.Text;
        2: LibraryName := FHTTPTunnelURI.Text;
      end;

      Client.BeginSilent();
      Client.FirstConnect(FConnectionType.ItemIndex, LibraryName, FHost.Text, FUser.Text, FPassword.Text, '', FUDPort.Position, FCompression.Checked, False);
      if (Client.ErrorCode <> 0) then
        Accounts.OnSQLError(Client, Client.ErrorCode, Client.ErrorMessage)
      else if (Client.Connected) then
      begin
        List := TList.Create();
        List.Add(Client.Charsets);
        if (not Client.Update(List)) then
          for I := 0 to Client.Charsets.Count - 1 do
            FCharset.Items.Add(Client.Charsets.Charset[I].Name);
        FCharset.ItemIndex := FCharset.Items.IndexOf(Client.Charset);
        List.Free();
      end;
      Client.EndSilent();

      Client.Free();
    end;
  end;
end;

procedure TDAccount.FConnectionTypeChange(Sender: TObject);
begin
  FLibraryFilename.Visible := FConnectionType.ItemIndex = 1;
  FHTTPTunnelURI.Visible := FConnectionType.ItemIndex = 2;
  FLLibraryFilename.Visible := FLibraryFilename.Visible;
  FLHTTPTunnelURI.Visible := FHTTPTunnelURI.Visible;

  if (FConnectionType.ItemIndex = 2) then
    FHTTPTunnelURIChange(Sender);
end;

procedure TDAccount.FEditChange(Sender: TObject);
var
  Name: string;
begin
  if (FName.Text = '') then
    Name := FHost.Text
  else
    Name := FName.Text;
  FBOk.Enabled := (FHost.Text <> '')
    and (not Assigned(Accounts.AccountByName(Name)) or Assigned(Account) and (Accounts.AccountByName(Name) = Account));
  FBDatabase.Enabled := FHost.Text <> '';
end;

procedure TDAccount.FHostExit(Sender: TObject);
begin
  if (FName.Text = '') then
    FName.Text := FHost.Text;
end;

procedure TDAccount.FHTTPTunnelURIChange(Sender: TObject);
begin
  FBOk.Enabled := (FHTTPTunnelURI.Text = '') or (Copy(FHTTPTunnelURI.Text, 1, 7) = 'http://') or (Copy(FHTTPTunnelURI.Text, 1, 8) = 'https://');
end;

procedure TDAccount.FHTTPTunnelURIEnter(Sender: TObject);
begin
  if ((FHTTPTunnelURI.Text = '') and (Trim(FHost.Text) <> '') and (lstrcmpi(PChar(Trim(FHost.Text)), LOCAL_HOST) <> 0)) then
    FHTTPTunnelURI.Text := 'http://' + Trim(FHost.Text) + '/libMySQL.php';
end;

procedure TDAccount.FIconClick(Sender: TObject);
begin
  FBIcon.Click();
end;

procedure TDAccount.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewAccount: TAAccount;
begin
  if (ModalResult = mrOk) then
  begin
    ActiveControl := FBOk;

    if ((FConnectionType.ItemIndex = 1) and (FLibraryFilename.Text = '')) then
      begin PageControl.ActivePage := TSConnection; ActiveControl := FLibraryFilename; MessageBeep(MB_ICONERROR); CanClose := False; end
    else if ((FConnectionType.ItemIndex = 2) and not ((Copy(FHTTPTunnelURI.Text, 1, 7) = 'http://') or (Copy(FHTTPTunnelURI.Text, 1, 8) = 'https://'))) then
      begin PageControl.ActivePage := TSConnection; ActiveControl := FHTTPTunnelURI; MessageBeep(MB_ICONERROR); CanClose := False; end;

    if (CanClose) then
    begin
      NewAccount := TAAccount.Create(Accounts);
      if (Assigned(Account)) then
        NewAccount.Assign(Account);

      NewAccount.Name := Trim(FName.Text);
      if (IconChanged) then
      begin
        try
          ForceDirectories(ExtractFilePath(NewAccount.IconFilename));
          FIcon.Picture.Icon.SaveToFile(NewAccount.IconFilename);
          NewAccount.ImageIndex := Preferences.SmallImages.Count - 1;
        except
          MsgBox(Preferences.LoadStr(522, NewAccount.IconFilename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      end;
      NewAccount.Connection.Host := Trim(FHost.Text);
      NewAccount.Connection.Port := FUDPort.Position;
      case (FConnectionType.ItemIndex) of
        0: NewAccount.Connection.LibraryType := ltBuiltIn;
        1: NewAccount.Connection.LibraryType := ltDLL;
        2: NewAccount.Connection.LibraryType := ltHTTP;
      end;
      NewAccount.Connection.LibraryFilename := Trim(FLibraryFilename.Text);
      NewAccount.Connection.HTTPTunnelURI := Trim(FHTTPTunnelURI.Text);
      NewAccount.Connection.Charset := Trim(FCharset.Text);

      NewAccount.Connection.User := Trim(FUser.Text);
      NewAccount.Connection.Password := Trim(FPassword.Text);
      NewAccount.Connection.SavePassword := (NewAccount.Connection.User <> '') and ((NewAccount.Connection.Password <> '') or Assigned(Account) and (Account.Connection.User = NewAccount.Connection.User) and (Account.Connection.Password = ''));
      NewAccount.Connection.Database := ReplaceStr(Trim(FDatabase.Text), ';', ',');

      NewAccount.Startup := Trim(FStartup.Text);

      if (FLimitOff.Checked) then
        NewAccount.DefaultLimit := dlOff
      else if (FLimitOn.Checked) then
        NewAccount.DefaultLimit := dlOn
      else
        NewAccount.DefaultLimit := dlRemember;
      NewAccount.DefaultSorting := FDefaultSorting.Checked;

      NewAccount.CacheSize := FUDCacheSize.Position;
      NewAccount.Connection.ASynchron := FAsynchron.Checked;
      NewAccount.Connection.Compression := FCompression.Checked;
      NewAccount.Connection.MultiStatements := FMultiStatements.Checked;
      NewAccount.Connection.UseInformationSchema := FUseInformationSchema.Checked;
      NewAccount.Connection.Prefetch := FPrefetch.ItemIndex;

      Username := NewAccount.Connection.User;
      Password := NewAccount.Connection.Password;

      if (not Assigned(Account)) then
        Accounts.AddAccount(NewAccount)
      else
        Accounts.UpdateAccount(Account, NewAccount);

      NewAccount.Free();
    end;
  end;
end;

procedure TDAccount.FormCreate(Sender: TObject);
begin
  FStartup.Highlighter := MainHighlighter;

  FIcon.Picture.Icon.Height := 18;
  FIcon.Picture.Icon.Width := 18;

  FBIcon.Height := 20;
  FBIcon.Width := 20;

  FBDatabase.Height := FDatabase.Height; FBDatabase.Width := FBDatabase.Height;
  FBDatabase.Left := FDatabase.Left + FDatabase.Width;

  OpenDialog.InitialDir := GetCurrentDir();
  OpenDialog.FileName := '';
  OpenDialog.DefaultExt := 'ico';
  OpenDialog.Filter := FilterDescription('ico') + ' (*.ico)|*.ico';

  msUndo.Action := MainAction('aEUndo'); msCut.ShortCut := 0;
  msCut.Action := MainAction('aECut'); msCut.ShortCut := 0;
  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msPaste.Action := MainAction('aEPaste'); msPaste.ShortCut := 0;
  msDelete.Action := MainAction('aEDelete'); msDelete.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  TSDataBrowser.TabVisible := False;
  TSDebug.TabVisible := False;
end;

procedure TDAccount.FormShow(Sender: TObject);
begin
  if (not Assigned(Account)) then
    Caption := Preferences.LoadStr(204)
  else
    Caption := Preferences.LoadStr(842, Account.Name);

  FCharset.Items.Clear();

  FConnectionType.Items.Text := Preferences.LoadStr(649) + #13#10 + Preferences.LoadStr(650) + #13#10 + Preferences.LoadStr(651);

  if (not Assigned(Account)) then
  begin
    FName.Text := '';
    IconChanged := False;

    FHost.Text := '';
    FUDPort.Position := MYSQL_PORT;
    FConnectionType.ItemIndex := 0;
    FLibraryFilename.Text := 'libMySQL.dll';
    FHTTPTunnelURI.Text := '';
    FCharset.Text := '';

    FUser.Text := 'root';
    FPassword.Text := '';
    FDatabase.Text := '';

    FStartup.Lines.Clear();
    FLimitRemember.Checked := True;
    FDefaultSorting.Checked := True;

    FUDCacheSize.Position := 50;
    FCompression.Checked := True;
    FMultiStatements.Checked := True;
    FAsynchron.Checked := True;
    FUseInformationSchema.Checked := True;
    FPrefetch.ItemIndex := 1;
  end
  else
  begin
    FName.Text := Account.Name;

    IconChanged := False;

    FHost.Text := Account.Connection.Host;
    if (Account.Connection.Port = 0) then
      FUDPort.Position := MYSQL_PORT
    else
      FUDPort.Position := Account.Connection.Port;
    case (Account.Connection.LibraryType) of
      ltBuiltIn: FConnectionType.ItemIndex := 0;
      ltDLL: FConnectionType.ItemIndex := 1;
      ltHTTP: FConnectionType.ItemIndex := 2;
    end;
    FLibraryFilename.Text := Account.Connection.LibraryFilename;
    FHTTPTunnelURI.Text := Account.Connection.HTTPTunnelURI;
    FCharset.Text := Account.Connection.Charset;

    FUser.Text := Username;
    FPassword.Text := Password;
    FDatabase.Text := Account.Connection.Database;

    if (Account.Startup = '') then
      FStartup.Lines.Clear
    else
      FStartup.Lines.Text := Account.Startup + #13#10;

    case (Account.DefaultLimit) of
      dlOff: FLimitOff.Checked := True;
      dlRemember: FLimitRemember.Checked := True;
      dlOn: FLimitOn.Checked := True;
    end;
    FDefaultSorting.Checked := Account.DefaultSorting;

    FUDCacheSize.Position := Account.CacheSize;
    FCompression.Checked := Account.Connection.Compression;
    FMultiStatements.Checked := Account.Connection.MultiStatements;
    FAsynchron.Checked := Account.Connection.ASynchron;
    FUseInformationSchema.Checked := Account.Connection.UseInformationSchema;
    FPrefetch.ItemIndex := Account.Connection.Prefetch;
  end;


  FEditChange(nil);
  FConnectionTypeChange(nil);

  ActiveControl := FBCancel;
  if (ShowType = stLogin) then
  begin
    PageControl.ActivePage := TSLogin;
    ActiveControl := FUser;
  end
  else
  begin
    PageControl.ActivePage := TSConnection;
    ActiveControl := FHost;
  end;
end;

function TDAccount.GetAccountName(): string;
begin
  Result := Trim(FName.Text);
end;

procedure TDAccount.TSBasicsShow(Sender: TObject);
begin
  if (Assigned(Account) and FileExists(Account.IconFilename)) then
    FIcon.Picture.Icon.LoadFromFile(Account.IconFilename)
  else
    Preferences.SmallImages.GetIcon(iiServer, FIcon.Picture.Icon);
end;

initialization
  FAccount := nil;
end.
