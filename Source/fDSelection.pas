unit fDSelection;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  Forms_Ext,
  fBase;

type
  TDSelection = class(TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FSelection: TListView;
    FManual: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSelectionChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FSelectionCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FSelectionDblClick(Sender: TObject);
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Selected: string;
    Values: array of string;
    function Execute(): Boolean;
  end;

function DSelection(): TDSelection;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommCtrl,
  StrUtils, Math,
  fPreferences;

var
  FSelection: TDSelection;

function DSelection(): TDSelection;
begin
  if (not Assigned(FSelection)) then
  begin
    Application.CreateForm(TDSelection, FSelection);
    FSelection.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FSelection;
end;

{ TDSelect ********************************************************************}

procedure TDSelection.CMChangePreferences(var Message: TMessage);
begin
  Caption := ReplaceStr(Preferences.LoadStr(721), '&', '');

  if (CheckWin32Version(6)) then
    SendMessage(FManual.Handle, EM_SETCUEBANNER, 0, LParam(PChar(ReplaceStr(Preferences.LoadStr(424), '&', ''))));

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDSelection.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDSelection.FormCreate(Sender: TObject);
begin
  SetLength(Values, 0);
end;

procedure TDSelection.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
    if (FManual.Text <> '') then
      Selected := FManual.Text
    else
      Selected := FSelection.Selected.Caption;

  SetLength(Values, 0);
end;

procedure TDSelection.FormShow(Sender: TObject);
var
  I: Integer;
begin
  FSelection.Items.Clear();

  for I := 0 to Length(Values) - 1 do
    FSelection.Items.Add().Caption := Values[I];
  if (FSelection.Items.Count > 0) then
  begin
    FSelection.ItemFocused := FSelection.Items[0];
    FSelection.Selected := FSelection.ItemFocused;
  end;

  FManual.Text := '';

  ActiveControl := FSelection;
end;

procedure TDSelection.FSelectionChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  FBOk.Enabled := Assigned(FSelection.Selected);
end;

procedure TDSelection.FSelectionCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)));
end;

procedure TDSelection.FSelectionDblClick(Sender: TObject);
begin
  if (FBOk.Enabled) then
    FBOk.Click();
end;

initialization
  FSelection := nil;
end.
