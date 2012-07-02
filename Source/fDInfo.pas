unit fDInfo;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Graphics, Forms, Dialogs, Jpeg, StdCtrls,
  Controls, Classes, ExtCtrls,
  ComCtrls_Ext, ExtCtrls_Ext, StdCtrls_Ext,
  fBase;

type
  TDInfo = class(TForm)
    FBHelp: TButton;
    FBOk: TButton;
    FBuild: TLabel;
    FLine: TPanel_Ext;
    FURI: TLabel;
    FVersion: TLabel;
    Image: TImage;
    procedure FBHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FURIClick(Sender: TObject);
  private
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    function Execute(): Boolean;
  end;

function DInfo(): TDInfo;

implementation {***************************************************************}

{$R *.dfm}

uses
  ShellAPI, RichEdit,
  StrUtils, IniFiles,
  fPreferences,
  fDInstallUpdate;

var
  FInfo: TDInfo;

function DInfo(): TDInfo;
begin
  if (not Assigned(FInfo)) then
  begin
    Application.CreateForm(TDInfo, FInfo);
    FInfo.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FInfo;
end;

{ TDInfo **********************************************************************}

procedure TDInfo.CMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(367) + ' ' + LoadStr(1000);

  FVersion.Caption := Preferences.LoadStr(169) + ' ' + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor);
  FBuild.Caption := '(' + Preferences.LoadStr(737) + ': ' + IntToStr(Preferences.VerPatch) + '.' + IntToStr(Preferences.VerBuild) + ')';
  FVersion.Left := FBuild.Left - FVersion.Width - 8;

  FURI.Caption := SysUtils.LoadStr(1004);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(231);
end;

function TDInfo.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDInfo.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDInfo.FormCreate(Sender: TObject);
var
  JPEGImage: TJPEGImage;
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(HInstance, 1, RT_RCDATA);
  JPEGImage := TJPEGImage.Create();
  JPEGImage.LoadFromStream(Stream);
  Image.Picture.Graphic := JPEGImage;
  JPEGImage.Free();
  Stream.Free();
end;

procedure TDInfo.FormDestroy(Sender: TObject);
begin
  if (Assigned(Image.Picture.Graphic)) then
    Image.Picture.Graphic := nil;
end;

procedure TDInfo.FormShow(Sender: TObject);
begin
  ActiveControl := FBOk;
end;

procedure TDInfo.FURIClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(FURI.Caption), '', '', SW_SHOW);
end;

initialization
  FInfo := nil;
end.

