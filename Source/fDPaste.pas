unit fDPaste;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  Forms_Ext,
  fBase;

type
  TDPaste = class(TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FData: TCheckBox;
    FStructure: TCheckBox;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Data: Boolean;
    Structure: Boolean;
    function Execute(): Boolean;
  end;

function DPaste(): TDPaste;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  fPreferences;

var
  FPaste: TDPaste;

function DPaste(): TDPaste;
begin
  if (not Assigned(FPaste)) then
  begin
    Application.CreateForm(TDPaste, FPaste);
    FPaste.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FPaste;
end;

{ TDPaste *********************************************************************}

procedure TDPaste.CMChangePreferences(var Message: TMessage);
begin
  Caption := ReplaceStr(Preferences.LoadStr(65), '&', '');

  FStructure.Caption := Preferences.LoadStr(215);
  FData.Caption := Preferences.LoadStr(216);

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDPaste.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDPaste.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
  begin
    Structure := FStructure.Checked;
    Data := FData.Checked;

    Preferences.Paste.Data := FData.Checked;
  end;
end;

procedure TDPaste.FormShow(Sender: TObject);
begin
  FData.Checked := Preferences.Paste.Data;

  ActiveControl := FData;
end;

initialization
  FPaste := nil;
end.
