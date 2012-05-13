unit fDGoto;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  Forms_Ext,
  fBase;

type
  TDGoto = class(TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FLPrimary: TLabel;
    FLSecondary: TLabel;
    FPrimary: TEdit;
    FSecondary: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FPrimaryChange(Sender: TObject);
  private
    FFLabels: array of TLabel;
    FFValues: array of TEdit;
    InitialHeight: Integer;
    LastCaptions: string;
  protected
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Captions: string;
    Values: array of string;
    function Execute(): Boolean;
  end;

function DGoto(): TDGoto;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  fPreferences;

var
  FGoto: TDGoto;

function DGoto(): TDGoto;
begin
  if (not Assigned(FGoto)) then
  begin
    Application.CreateForm(TDGoto, FGoto);
    FGoto.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FGoto;
end;

{ TDGoto **********************************************************************}

procedure TDGoto.CMChangePreferences(var Message: TMessage);
begin
  Caption := ReplaceStr(Preferences.LoadStr(676), '&', '');

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDGoto.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDGoto.FormCreate(Sender: TObject);
begin
  InitialHeight := Height;
  LastCaptions := '';
end;

procedure TDGoto.FormHide(Sender: TObject);
var
  I: Integer;
begin
  if (ModalResult = mrOk) then
  begin
    SetLength(Values, Length(FFValues));

    for I := 0 to Length(Values) - 1 do
      Values[I] := Trim(FFValues[I].Text);
  end;

  LastCaptions := Captions;
end;

procedure TDGoto.FormShow(Sender: TObject);
var
  I: Integer;
  StringList: TStringList;
begin
  if (Captions <> LastCaptions) then
  begin
    for I := 0 to Length(FFLabels) - 1 do
    begin
      FFValues[I].Free();
      FFLabels[I].Free();
    end;

    StringList := TStringList.Create();
    StringList.Text := ReplaceStr(Captions, ';', #13#10);

    SetLength(FFLabels, StringList.Count);
    SetLength(FFValues, StringList.Count);
    for I := 0 to Length(FFLabels) - 1 do
    begin
      FFValues[I] := TEdit.Create(Self);
      FFValues[I].Parent := Self;
      FFValues[I].Left := FPrimary.Left;
      FFValues[I].Top := FPrimary.Top + I * (FSecondary.Top - FPrimary.Top);
      FFValues[I].Height := FPrimary.Height;
      FFValues[I].Width := FPrimary.Width;
      FFValues[I].OnChange := FPrimary.OnChange;

      FFLabels[I] := TLabel.Create(Self);
      FFLabels[I].Parent := Self;
      FFLabels[I].Left := FLPrimary.Left;
      FFLabels[I].Top := FLPrimary.Top + I * (FLSecondary.Top - FLPrimary.Top);
      FFLabels[I].Height := FLPrimary.Height;
      FFLabels[I].Caption := StringList[I] + ':';
      FFLabels[I].FocusControl := FFValues[I];
    end;

    if (StringList.Count > 2) then
      Height := InitialHeight + (StringList.Count - 2) * (FSecondary.Top - FPrimary.Top)
    else
      Height := InitialHeight;

    StringList.Free();
  end;

  FPrimaryChange(Sender);
  ActiveControl := FFValues[0];
  FFValues[0].SelectAll();
end;

procedure TDGoto.FPrimaryChange(Sender: TObject);
var
  I: Integer;
begin
  FBOk.Enabled := True;

  for I := 0 to Length(FFValues) - 1 do
    if (FFValues[I].Text = '') then
      FBOk.Enabled := False;
end;

initialization
  FGoto := nil;
end.
