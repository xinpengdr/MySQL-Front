unit fDQuickFilter;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  Forms_Ext,
  fBase;

type
  TDQuickFilter = class (TForm_Ext)
    FBOk: TButton;
    FBCancel: TButton;
    FLValue: TLabel;
    FValue: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Data: string;
    function Execute(): Boolean;
  end;

function DQuickFilter(): TDQuickFilter;

implementation {***************************************************************}

{$R *.dfm}

uses
  fPreferences;

var
  FQuickFilter: TDQuickFilter;

function DQuickFilter(): TDQuickFilter;
begin
  if (not Assigned(FQuickFilter)) then
  begin
    Application.CreateForm(TDQuickFilter, FQuickFilter);
    FQuickFilter.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FQuickFilter;
end;

{ TDQuickFilter ***************************************************************}

procedure TDQuickFilter.CMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(489);

  FLValue.Caption := Preferences.LoadStr(490) + ':';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDQuickFilter.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDQuickFilter.FormShow(Sender: TObject);
begin
  FValue.Text := Data;

  ActiveControl := FBCancel;
  ActiveControl := FValue;
end;

procedure TDQuickFilter.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
  begin
    Data := Trim(FValue.Text);
  end;
end;

initialization
  FQuickFilter := nil;
end.
