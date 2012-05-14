unit fDConnecting;

interface {********************************************************************}

uses
  Messages, Classes,
  Forms, Controls,StdCtrls,
  Forms_Ext,
  fClient, fBase, ComCtrls, ToolWin, ImgList;

type
  TDConnecting = class (TForm_Ext)
    FBCancel: TButton;
    FInfo: TLabel;
    procedure FBCancelClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure AfterConnect(Sender: TObject);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
  public
    Client: TCClient;
    function Execute(): Boolean;
  end;

function DConnecting(): TDConnecting;

implementation {***************************************************************}

{$R *.dfm}

uses
  Windows, SysUtils, 
  MySQLDB,
  fPreferences, fAccount, fDAccount;

var
  FConnecting: TDConnecting;

function DConnecting(): TDConnecting;
begin
  if (not Assigned(FConnecting)) then
  begin
    Application.CreateForm(TDConnecting, FConnecting);
    FConnecting.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FConnecting;
end;

{ TDOpenConnection ************************************************************}

procedure TDConnecting.AfterConnect(Sender: TObject);
begin
  if (Client.Connected) then
    ModalResult := mrOk
  else
    ModalResult := mrCancel;
end;

procedure TDConnecting.CMChangePreferences(var Message: TMessage);
begin
  FInfo.Caption := Preferences.LoadStr(195) + '...';

  FBCancel.Caption := Preferences.LoadStr(30);
end;

procedure TDConnecting.CMPostShow(var Message: TMessage);
begin
  if (not Client.Account.Connection.SavePassword and (Client.Account.Connection.Password = '') and not Accounts.DBLogin(Client.Account)) then
    ModalResult := mrCancel
  else
    Client.FirstConnect();
end;

function TDConnecting.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDConnecting.FBCancelClick(Sender: TObject);
begin
  Client.Terminate();
  ModalResult := mrCancel;
end;

procedure TDConnecting.FormHide(Sender: TObject);
begin
  Client.AfterConnect := nil;
end;

procedure TDConnecting.FormShow(Sender: TObject);
begin
  Caption := Client.Account.Name;

  FBCancel.Enabled := Client.Account.Connection.Asynchron;

  Client.AfterConnect := AfterConnect;

  PostMessage(Handle, CM_POSTSHOW, 0, 0);
end;

initialization
  FConnecting := nil;
end.

