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
    ListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListViewDblClick(Sender: TObject);
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
    Selected := ListView.Selected.Caption;

  SetLength(Values, 0);
end;

procedure TDSelection.FormShow(Sender: TObject);
var
  I: Integer;
begin
  ListView.Items.Clear();

  for I := 0 to Length(Values) - 1 do
    ListView.Items.Add().Caption := Values[I];
  if (ListView.Items.Count > 0) then
  begin
    ListView.ItemFocused := ListView.Items[0];
    ListView.Selected := ListView.ItemFocused;
  end;

  ActiveControl := ListView;
end;

procedure TDSelection.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  FBOk.Enabled := Assigned(ListView.Selected);
end;

procedure TDSelection.ListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)));
end;

procedure TDSelection.ListViewDblClick(Sender: TObject);
begin
  if (FBOk.Enabled) then
    FBOk.Click();
end;

initialization
  FSelection := nil;
end.
