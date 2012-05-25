unit fDLogin;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, WinCred,
  Dialogs, StdCtrls,
  Forms_Ext,
  fClient, fBase, fAccount, ExtCtrls, StdCtrls_Ext;

const
  CREDUI_MAX_MESSAGE_LENGTH        = 32767;
  {$EXTERNALSYM CREDUI_MAX_MESSAGE_LENGTH}
  CREDUI_MAX_CAPTION_LENGTH        = 128;
  {$EXTERNALSYM CREDUI_MAX_CAPTION_LENGTH}
  CREDUI_MAX_GENERIC_TARGET_LENGTH = CRED_MAX_GENERIC_TARGET_NAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_GENERIC_TARGET_LENGTH}
  CREDUI_MAX_DOMAIN_TARGET_LENGTH  = CRED_MAX_DOMAIN_TARGET_NAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_DOMAIN_TARGET_LENGTH}
  CREDUI_MAX_USERNAME_LENGTH       = CRED_MAX_USERNAME_LENGTH;
  {$EXTERNALSYM CREDUI_MAX_USERNAME_LENGTH}
  CREDUI_MAX_PASSWORD_LENGTH       = CRED_MAX_CREDENTIAL_BLOB_SIZE div 2;
  {$EXTERNALSYM CREDUI_MAX_PASSWORD_LENGTH}

type
  TDLogin = class (TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FBSettings: TButton;
    FLPassword: TLabel;
    FLUsername: TLabel;
    FPassword: TEdit;
    FSavePassword: TCheckBox;
    FUsername: TEdit;
    GAccount: TGroupBox_Ext;
    procedure FBSettingsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    PasswordC: array [0 .. CREDUI_MAX_PASSWORD_LENGTH] of Char;
    UsernameC: array [0 .. CRED_MAX_USERNAME_LENGTH] of Char;
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Filename: TFileName;
    Password: string;
    Account: TAAccount;
    Username: string;
    Window: TForm;
    function Execute(): Boolean;
  end;

function DLogin(): TDLogin;

implementation {***************************************************************}

{$R *.dfm}

uses
  fDAccount, fPreferences;

var
  FLogin: TDLogin;

function DLogin(): TDLogin;
begin
  if (not Assigned(FLogin)) then
  begin
    Application.CreateForm(TDLogin, FLogin);
    FLogin.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FLogin;
end;

type
  TCredUIPromptForCredentials = function (pUiInfo: PCredUIInfoW;
    pszTargetName: PCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD;
    pszUserName: PWSTR; ulUserNameBufferSize: ULONG; pszPassword: PWSTR;
    ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;

{ TDLogin *********************************************************************}

procedure TDLogin.CMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(49);

  GAccount.Caption := Preferences.LoadStr(34);
  FLUsername.Caption := Preferences.LoadStr(561) + ':';
  FLPassword.Caption := Preferences.LoadStr(40) + ':';
  FSavePassword.Caption := Preferences.LoadStr(50);
  FBSettings.Caption := Preferences.LoadStr(27) + '...';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDLogin.Execute(): Boolean;
var
  Handle: THandle;
  CredUIPromptForCredentials: TCredUIPromptForCredentials;
  Info: TCredUIInfoW;
  Save: BOOL;
  Flags: DWORD;
begin
  if (Assigned(Account) or not CheckWin32Version(5, 1)) then
  begin
    Handle := 0;
    CredUIPromptForCredentials := nil;
  end
  else
  begin
    Handle := LoadLibrary('CredUI.dll');
    CredUIPromptForCredentials := GetProcAddress(Handle, 'CredUIPromptForCredentialsW');
  end;

  if (not Assigned(CredUIPromptForCredentials)) then
  begin
    ShowModal();
    Result := ModalResult = mrOk;
  end
  else
  begin
    ZeroMemory(@Info, SizeOf(TCredUIInfo));
    Info.cbSize := SizeOf(TCredUIInfo);
    Info.hwndParent := Window.Handle;
    Info.pszMessageText := PChar(Filename);
    Info.pszCaptionText := PChar(Caption);

    Save := False;
    Flags := CREDUI_FLAGS_ALWAYS_SHOW_UI or CREDUI_FLAGS_DO_NOT_PERSIST or CREDUI_FLAGS_EXCLUDE_CERTIFICATES or CREDUI_FLAGS_GENERIC_CREDENTIALS;

    Result := CredUIPromptForCredentials(@Info, PChar(Filename), nil, 0,
      UsernameC, CRED_MAX_USERNAME_LENGTH,
      PasswordC, CREDUI_MAX_PASSWORD_LENGTH,
      Save, Flags) = NO_ERROR;

    if (Result) then
    begin
      Username := UsernameC;
      Password := PasswordC;
    end;
  end;

  if (Handle <> 0) then
    FreeLibrary(Handle);
end;

procedure TDLogin.FBSettingsClick(Sender: TObject);
begin
  DAccount.Account := Account;
  DAccount.Username := Trim(FUsername.Text);
  DAccount.Password := Trim(FPassword.Text);
  DAccount.ShowType := stLogin;
  if (not DAccount.Execute()) then
    ActiveControl := FPassword
  else
    FormShow(Sender);
end;

procedure TDLogin.FormCreate(Sender: TObject);
begin
  UsernameC := 'Admin';
  PasswordC := '';
end;

procedure TDLogin.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
  begin
    if (Assigned(Account)) then
    begin
      Account.Connection.SavePassword := FSavePassword.Checked;
      Account.Connection.User := Trim(FUsername.Text);
      Account.Connection.Password := Trim(FPassword.Text);
    end;
    Username := Trim(FUsername.Text);
    Password := Trim(FPassword.Text);
  end;
end;

procedure TDLogin.FormShow(Sender: TObject);
begin
  if (not Assigned(Account)) then
  begin
    FUsername.Text := 'Admin';
    FPassword.Text := '';
    FSavePassword.Checked := False;
  end
  else
  begin
    FUsername.Text := Account.Connection.User;
    FPassword.Text := Account.Connection.Password;
    FSavePassword.Checked := Account.Connection.SavePassword;
  end;
  FSavePassword.Visible := Assigned(Account);
  FBSettings.Visible := Assigned(Account);

  ActiveControl := FBCancel;
  if (not Assigned(Account)) then
    ActiveControl := FUsername
  else
    ActiveControl := FPassword;
end;

initialization
  FLogin := nil;
end.
