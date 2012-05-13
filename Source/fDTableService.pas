unit fDTableService;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  Forms_Ext,
  fClient, fBase;

type
  TDTableServiceMode = (smOptimize, smCheck, smAnalyze, smRepair, smFlush);

  TDTableService = class (TForm_Ext)
    FBCancel: TButton;
    FInformation: TLabel;
    procedure FBCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPostShow(var Message: TMessage); message CM_POSTSHOW;
  public
    Database: TCDatabase;
    ServiceMode: TDTableServiceMode;
    TableNames: array of string;
    function Execute(): Boolean;
  end;

function DTableService(): TDTableService;

implementation {***************************************************************}

{$R *.dfm}

uses
  fPreferences;

var
  FTableServiceSmall: TDTableService;

function DTableService(): TDTableService;
begin
  if (not Assigned(FTableServiceSmall)) then
  begin
    Application.CreateForm(TDTableService, FTableServiceSmall);
    FTableServiceSmall.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FTableServiceSmall;
end;

{ TDTableService **************************************************************}

procedure TDTableService.CMChangePreferences(var Message: TMessage);
begin
  FBCancel.Caption := Preferences.LoadStr(231);
end;

procedure TDTableService.CMPostShow(var Message: TMessage);
begin
  Hide();
end;

function TDTableService.Execute(): Boolean;
begin
  ShowModal();

  Result := ModalResult = mrOk;
end;

procedure TDTableService.FBCancelClick(Sender: TObject);
begin
  Database.Client.Terminate();
end;

procedure TDTableService.FormDestroy(Sender: TObject);
begin
  SetLength(TableNames, 0);
end;

procedure TDTableService.FormHide(Sender: TObject);
var
  Success: Boolean;
  Table: TCBaseTable;
begin
  if (Length(TableNames) = 1) then
  begin
    Table := Database.BaseTableByName(TableNames[0]);
    Success := Assigned(Table);
    case (ServiceMode) of
      smOptimize:
        Success := Success and Table.Optimize();
      smCheck:
        Success := Success and Table.Check();
      smFlush:
        Success := Success and Table.Flush();
      smRepair:
        Success := Success and Table.Repair();
      else
        Success := False;
    end;
  end
  else
  begin
    case (ServiceMode) of
      smOptimize:
        Success := Database.OptimizeTables(TableNames);
      smFlush:
        Success := Database.FlushTables(TableNames);
      else
        Success := False;
    end;
  end;

  if (Success) then ModalResult := mrOk else ModalResult := mrCancel;
end;

procedure TDTableService.FormShow(Sender: TObject);
begin
  case (ServiceMode) of
    smOptimize:
    begin
      Caption := Preferences.LoadStr(137);
      FInformation.Caption := Preferences.LoadStr(879);
    end;
    smCheck:
    begin
      Caption := Preferences.LoadStr(143);
      FInformation.Caption := Preferences.LoadStr(879);
    end;
    smFlush:
    begin
      Caption := Preferences.LoadStr(330);
      FInformation.Caption := Preferences.LoadStr(879);
    end;
    smRepair:
    begin
      Caption := Preferences.LoadStr(143);
      FInformation.Caption := Preferences.LoadStr(879);
    end;
  end;

  PostMessage(Self.Handle, CM_POSTSHOW, 0, 0);
end;

initialization
  FTableServiceSmall := nil;
end.
