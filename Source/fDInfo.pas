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
    FMail: TLabel;
    procedure FBHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FURIClick(Sender: TObject);
    procedure FMailClick(Sender: TObject);
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
  Caption := Preferences.LoadStr(367) + ' ' + Application.Title;

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

procedure TDInfo.FMailClick(Sender: TObject);
var
  Filename: string;
begin
  if (IsConnectedToInternet()) then
  begin
    CheckUpdateThread := TCheckUpdateThread.Create(True);
    CheckUpdateThread.Stream := TStringStream.Create('');
    CheckUpdateThread.Execute();
    CheckUpdateThread.Stream.Free();

    if (CheckUpdateThread.UpdateAvailable) then
    begin
      MsgBox('An update is available. Please use the latest update before send a feedback.'
        + #13#10#13#10 + 'You can install the update with menu: Help -> Install Update.',
        Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
    end
    else
    begin
      Filename := 'mailto:' + SysUtils.LoadStr(1006)
        + '?subject=' + SysUtils.LoadStr(1000) + '%20Feedback%20'
        + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor) + '.' + IntToStr(Preferences.VerPatch) + '.' + IntToStr(Preferences.VerBuild);
      ShellExecute(0, PChar('open'), PChar(Filename), nil, nil, SW_SHOWNORMAL);
    end;

    CheckUpdateThread.Free();
  end;
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

  FMail.Caption := SysUtils.LoadStr(1006);
end;

procedure TDInfo.FormDestroy(Sender: TObject);
begin
  if (Assigned(Image.Picture.Graphic)) then
    Image.Picture.Graphic := nil;
end;

procedure TDInfo.FormShow(Sender: TObject);
begin
  ActiveControl := FBOk;

  FMail.Visible := IsConnectedToInternet();
end;

procedure TDInfo.FURIClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(FURI.Caption), '', '', SW_SHOW);
end;

initialization
  FInfo := nil;
end.

