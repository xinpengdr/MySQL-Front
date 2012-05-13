unit fDSegment;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  Forms_Ext, ExtCtrls_Ext, StdCtrls_Ext,
  fBase, fCWorkbench;

type
  TDSegment = class(TForm_Ext)
    ColorDialog: TColorDialog;
    FBCancel: TButton;
    FBOk: TButton;
    FColor: TButton;
    FLColor: TLabel;
    FLName: TLabel;
    FName: TEdit;
    GBasics: TGroupBox_Ext;
    PColor: TPanel_Ext;
    procedure FColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PColorClick(Sender: TObject);
  private
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Section: TWSection;
    function Execute(): Boolean;
  end;

function DSegment(): TDSegment;

implementation {***************************************************************}

{$R *.dfm}

uses
  fPreferences;

var
  FSegment: TDSegment;

function DSegment(): TDSegment;
begin
  if (not Assigned(FSegment)) then
  begin
    Application.CreateForm(TDSegment, FSegment);
    FSegment.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FSegment;
end;

{ TDSegment *******************************************************************}

procedure TDSegment.CMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(842, Preferences.LoadStr(877));

  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLColor.Caption := Preferences.LoadStr(740) + ':';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDSegment.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDSegment.FColorClick(Sender: TObject);
begin
  ColorDialog.Color := PColor.Color;

  if (ColorDialog.Execute()) then
    PColor.Color := ColorDialog.Color;
end;

procedure TDSegment.FormCreate(Sender: TObject);
begin
  ColorDialog.CustomColors.Add('ColorA=000080');
  ColorDialog.CustomColors.Add('ColorB=008000');
  ColorDialog.CustomColors.Add('ColorC=008080');
  ColorDialog.CustomColors.Add('ColorD=800000');
  ColorDialog.CustomColors.Add('ColorE=800080');
  ColorDialog.CustomColors.Add('ColorF=808000');
  ColorDialog.CustomColors.Add('ColorG=808080');
  ColorDialog.CustomColors.Add('ColorH=C0C0C0');
  ColorDialog.CustomColors.Add('ColorI=0000FF');
  ColorDialog.CustomColors.Add('ColorJ=00FF00');
  ColorDialog.CustomColors.Add('ColorK=00FFFF');
  ColorDialog.CustomColors.Add('ColorL=FF0000');
  ColorDialog.CustomColors.Add('ColorM=FF00FF');
  ColorDialog.CustomColors.Add('ColorN=FFFF00');
  ColorDialog.CustomColors.Add('ColorO=C0C0C0');
  ColorDialog.CustomColors.Add('ColorP=808080');
end;

procedure TDSegment.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
  begin
    Section.Workbench.BeginUpdate();

    Section.Caption := FName.Text;
    Section.Color := PColor.Color;

    Section.Workbench.EndUpdate();
  end;
end;

procedure TDSegment.FormShow(Sender: TObject);
begin
  FName.Text := Section.Caption;
  PColor.Color := Section.Color;

  ActiveControl := FBCancel;
  ActiveControl := FName;
end;

procedure TDSegment.PColorClick(Sender: TObject);
begin
  FColor.Click();
end;

end.

