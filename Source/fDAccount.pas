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
    FBOk: TButton;
    FCharset: TComboBox_Ext;
    FConnectionType: TComboBox_Ext;
    FDatabase: TEdit;
    FHost: TEdit;
    FHTTPTunnelURI: TEdit;
    FLCharset: TLabel;
    FLConnectionType: TLabel;
    FLDatabase: TLabel;
    FLHost: TLabel;
    FLHTTPTunnelURI: TLabel;
    FLibraryFilename: TEdit;
    FLLibraryFilename: TLabel;
    FLName: TLabel;
    FLPassword: TLabel;
    FLPort: TLabel;
    FLUser: TLabel;
    FName: TEdit;
    FPassword: TEdit;
    FPort: TEdit;
    FUDPort: TUpDown;
    FUser: TEdit;
    GBasics: TGroupBox_Ext;
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
    procedure FBDatabaseClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FCharsetDropDown(Sender: TObject);
    procedure FConnectionTypeChange(Sender: TObject);
    procedure FEditChange(Sender: TObject);
    procedure FHostExit(Sender: TObject);
    procedure FHTTPTunnelURIChange(Sender: TObject);
    procedure FHTTPTunnelURIEnter(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetAccountName(): string;
  private
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
    begin MessageBeep(MB_ICONERROR); ActiveControl := FHost; end
  else if ((FConnectionType.ItemIndex = 1) and (FLibraryFilename.Text = '')) then
    begin MessageBeep(MB_ICONERROR); ActiveControl := FLibraryFilename; end
  else if ((FConnectionType.ItemIndex = 2) and (FHTTPTunnelURI.Text = '')) then
    begin MessageBeep(MB_ICONERROR); ActiveControl := FHTTPTunnelURI; end
  else
    Result := True;
end;

procedure TDAccount.CMChangePreferences(var Message: TMessage);
begin
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';

  GServer.Caption := ReplaceStr(Preferences.LoadStr(37), '&', '');
  FLHost.Caption := Preferences.LoadStr(37) + ':';
  FLPort.Caption := Preferences.LoadStr(436) + ':';
  FLConnectionType.Caption := Preferences.LoadStr(648) + ':';
  FConnectionType.Items.Text := '';
  FConnectionTypeChange(nil);
  FLLibraryFilename.Caption := Preferences.LoadStr(568) + ':';
  FLHTTPTunnelURI.Caption := Preferences.LoadStr(652) + ':';
  FLCharset.Caption := Preferences.LoadStr(682) + ':';

  GLogin.Caption := Preferences.LoadStr(34);
  FLUser.Caption := Preferences.LoadStr(561) + ':';
  FLPassword.Caption := Preferences.LoadStr(40) + ':';
  FLDatabase.Caption := Preferences.LoadStr(38) + ':';

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
    Client := TCClient.Create(Clients);
    if (Assigned(Client)) then
    begin
      case (FConnectionType.ItemIndex) of
        0: LibraryName := '';
        1: LibraryName := FLibraryFilename.Text;
        2: LibraryName := FHTTPTunnelURI.Text;
      end;

      Client.BeginSilent();
      Client.FirstConnect(FConnectionType.ItemIndex, LibraryName, FHost.Text, FUser.Text, FPassword.Text, '', FUDPort.Position, True);
      if (Client.ErrorCode <> 0) then
        Client.OnSQLError(Client, Client.ErrorCode, Client.ErrorMessage)
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
  Application.HelpContext(HelpContext)
end;

procedure TDAccount.FCharsetDropDown(Sender: TObject);
var
  Client: TCClient;
  I: Integer;
  LibraryName: string;
begin
  if ((FCharset.Items.Count = 0) and CheckConnectInfos()) then
  begin
    Client := TCClient.Create(Clients);
    if (Assigned(Client)) then
    begin
      case (FConnectionType.ItemIndex) of
        0: LibraryName := '';
        1: LibraryName := FLibraryFilename.Text;
        2: LibraryName := FHTTPTunnelURI.Text;
      end;

      Client.BeginSilent();
      Client.FirstConnect(FConnectionType.ItemIndex, LibraryName, FHost.Text, FUser.Text, FPassword.Text, '', FUDPort.Position, False);
      if (Client.ErrorCode <> 0) then
        Client.OnSQLError(Client, Client.ErrorCode, Client.ErrorMessage)
      else if (Client.Connected) then
      begin
        if (Client.Charsets.Update()) then
          for I := 0 to Client.Charsets.Count - 1 do
            FCharset.Items.Add(Client.Charsets.Charset[I].Name);
        FCharset.ItemIndex := FCharset.Items.IndexOf(Client.Charset);
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
var
  Index: Integer;
begin
  if (FName.Text = '') then
    if (not Assigned(Accounts.AccountByName(FHost.Text))) then
      FName.Text := FHost.Text
    else
    begin
      Index := 2;
      while (Assigned(Accounts.AccountByName(FHost.Text + ' (' + IntToStr(Index) + ')'))) do
        Inc(Index);
      FName.Text := FHost.Text + ' (' + IntToStr(Index) + ')';
    end;
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

procedure TDAccount.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewAccount: TAAccount;
begin
  if (ModalResult = mrOk) then
  begin
    ActiveControl := FBOk;

    if ((FConnectionType.ItemIndex = 1) and (FLibraryFilename.Text = '')) then
      begin ActiveControl := FLibraryFilename; MessageBeep(MB_ICONERROR); CanClose := False; end
    else if ((FConnectionType.ItemIndex = 2) and not ((Copy(FHTTPTunnelURI.Text, 1, 7) = 'http://') or (Copy(FHTTPTunnelURI.Text, 1, 8) = 'https://'))) then
      begin ActiveControl := FHTTPTunnelURI; MessageBeep(MB_ICONERROR); CanClose := False; end;

    if (CanClose) then
    begin
      NewAccount := TAAccount.Create(Accounts);
      if (Assigned(Account)) then
        NewAccount.Assign(Account);

      NewAccount.Name := Trim(FName.Text);
      if (Trim(FHost.Text) = LOCAL_HOST_NAMEDPIPE) then
      begin
        NewAccount.Connection.Host := LOCAL_HOST;
        NewAccount.Connection.Port := MYSQL_PORT;
        NewAccount.Connection.PipeName := MYSQL_NAMEDPIPE;
        NewAccount.Connection.LibraryType := ltNamedPipe;
      end
      else
      begin
        NewAccount.Connection.Host := Trim(FHost.Text);
        NewAccount.Connection.Port := FUDPort.Position;
        case (FConnectionType.ItemIndex) of
          0: NewAccount.Connection.LibraryType := ltBuiltIn;
          1: NewAccount.Connection.LibraryType := ltDLL;
          2: NewAccount.Connection.LibraryType := ltHTTP;
        end;
      end;
      NewAccount.Connection.LibraryFilename := Trim(FLibraryFilename.Text);
      NewAccount.Connection.HTTPTunnelURI := Trim(FHTTPTunnelURI.Text);
      NewAccount.Connection.Charset := Trim(FCharset.Text);

      NewAccount.Connection.User := Trim(FUser.Text);
      NewAccount.Connection.Password := Trim(FPassword.Text);
      NewAccount.Connection.SavePassword := (NewAccount.Connection.User <> '') and ((NewAccount.Connection.Password <> '') or Assigned(Account) and (Account.Connection.User = NewAccount.Connection.User) and (Account.Connection.Password = ''));
      NewAccount.Connection.Database := ReplaceStr(Trim(FDatabase.Text), ';', ',');

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

    FHost.Text := '';
    FUDPort.Position := MYSQL_PORT;
    FConnectionType.ItemIndex := 0;
    FLibraryFilename.Text := 'libMySQL.dll';
    FHTTPTunnelURI.Text := '';
    FCharset.Text := '';

    FUser.Text := 'root';
    FPassword.Text := '';
    FDatabase.Text := '';
  end
  else
  begin
    FName.Text := Account.Name;

    if (Account.Connection.LibraryType = ltNamedPipe) then
      FHost.Text := LOCAL_HOST_NAMEDPIPE
    else
      FHost.Text := Account.Connection.Host;
    if (Account.Connection.Port = 0) then
      FUDPort.Position := MYSQL_PORT
    else
      FUDPort.Position := Account.Connection.Port;
    case (Account.Connection.LibraryType) of
      ltBuiltIn: FConnectionType.ItemIndex := 0;
      ltDLL: FConnectionType.ItemIndex := 1;
      ltHTTP: FConnectionType.ItemIndex := 2;
      ltNamedPipe: FConnectionType.ItemIndex := 0;
    end;
    FLibraryFilename.Text := Account.Connection.LibraryFilename;
    FHTTPTunnelURI.Text := Account.Connection.HTTPTunnelURI;
    FCharset.Text := Account.Connection.Charset;

    FUser.Text := Username;
    FPassword.Text := Password;
    FDatabase.Text := Account.Connection.Database;
  end;


  FEditChange(nil);
  FConnectionTypeChange(nil);

  ActiveControl := FBCancel;
  if (ShowType = stLogin) then
    ActiveControl := FUser
  else
    ActiveControl := FHost;
end;

function TDAccount.GetAccountName(): string;
begin
  Result := Trim(FName.Text);
end;

initialization
  FAccount := nil;
end.
