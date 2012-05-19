unit fDInstallUpdate;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, WinInet,
  Dialogs, StdCtrls, ComCtrls, Forms,
  Forms_Ext, StdCtrls_Ext,
  fBase;

const
  CM_PAD_FILE_RECEIVED = WM_USER + 300;
  CM_PROGRAM_FILE_RECEIVED = WM_USER + 302;
  CM_UPDATE_PROGRESSBAR = WM_USER + 303;

type
  THTTPThread = class (TThread)
  private
    Buffer: Pointer;
    FSize: Longint;
    URI: string;
    URLComponents: TURLComponents;
  protected
    procedure DoTerminate(); override;
    procedure Execute(); override;
  public
    Stream: TStream;
    Wnd: THandle;
    procedure AfterConstruction(); override;
    property Size: Longint read FSize;
  end;

  THTTPMessagedThread = class (THTTPThread)
  public
    SuccessMessage: UINT;
    procedure AfterConstruction(); override;
    procedure Execute(); override;
  end;

  TCheckUpdateThread = class (THTTPThread)
  protected
    procedure DoTerminate(); override;
  public
    SuccessMessage: UINT;
    UpdateAvailable: Boolean;
    procedure AfterConstruction(); override;
    procedure Execute(); override;
  end;

type
  TDInstallUpdate = class (TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FProgram: TLabel;
    FProgressBar: TProgressBar;
    FVersionInfo: TLabel;
    GroupBox: TGroupBox_Ext;
    procedure FBCancelClick(Sender: TObject);
    procedure FBOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    EXE_Stream: TFileStream;
    EXE_URI: string;
    FullHeight: Integer;
    HTTPThread: THTTPMessagedThread;
    PAD_Stream: TStringStream;
    SetupPrgFilename: TFileName;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPADFileReceived(var Message: TMessage); message CM_PAD_FILE_RECEIVED;
    procedure CMProgramFileReceived(var Message: TMessage); message CM_PROGRAM_FILE_RECEIVED;
    procedure CMUpdateProgressBarReceived(var Message: TMessage); message CM_UPDATE_PROGRESSBAR;
  public
    function Execute(): Boolean;
  end;

function CheckActualVersion(const Stream: TStringStream; var VersionStr: string; var UpdateAvailable: Boolean; var EXE_URI: string): Boolean;

var
  CheckUpdateThread: TCheckUpdateThread;

function DInstallUpdate(): TDInstallUpdate;

implementation {***************************************************************}

{$R *.dfm}

uses
  XMLIntf, XMLDoc, ShellApi, ActiveX, RTLConsts, StrUtils,
  fPreferences;

const
  BufferSize = 10 * 1024;

var
  FInstallUpdate: TDInstallUpdate;

function DInstallUpdate(): TDInstallUpdate;
begin
  if (not Assigned(FInstallUpdate)) then
  begin
    Application.CreateForm(TDInstallUpdate, FInstallUpdate);
    FInstallUpdate.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FInstallUpdate;
end;

{******************************************************************************}

function CheckActualVersion(const Stream: TStringStream; var VersionStr: string; var UpdateAvailable: Boolean; var EXE_URI: string): Boolean;
var
  Build: Integer;
  Infos: IXMLNode;
  Major: Integer;
  Minor: Integer;
  PAD: IXMLNode;
  Patch: Integer;
  Success: Boolean;
  VerBuild: IXMLNode;
  VerMajor: IXMLNode;
  VerMinor: IXMLNode;
  VerPatch: IXMLNode;
  XML: IXMLDocument;
begin
  Success := True;
  Major := -1; Minor := -1; Patch := -1; Build := -1;
  UpdateAvailable := False; EXE_URI := '';

  XML := NewXMLDocument();
  XML.LoadFromStream(Stream, xetUnknown);
  if (XML.Active and (Assigned(XML.Node))) then
  begin
    PAD := XML.Node.ChildNodes.FindNode('XML_DIZ_INFO');
    if (Assigned(PAD)) then
    begin
      Infos := PAD.ChildNodes.FindNode('Program_Info');
      if (Assigned(Infos)) then
      begin
        VerMajor := Infos.ChildNodes.FindNode('Program_Version_Major');
        if (Assigned(VerMajor)) and (VerMajor.IsTextElement) then
          Major := StrToInt(VerMajor.GetText());
        VerMinor := Infos.ChildNodes.FindNode('Program_Version_Minor');
        if (Assigned(VerMinor)) and (VerMinor.IsTextElement) then
          Minor := StrToInt(VerMinor.GetText());
        VerPatch := Infos.ChildNodes.FindNode('Program_Version_Patch');
        if (Assigned(VerPatch)) and (VerPatch.IsTextElement) then
          Patch := StrToInt(VerPatch.GetText());                                   
        VerBuild := Infos.ChildNodes.FindNode('Program_Version_Build');
        if (Assigned(VerBuild)) and (VerBuild.IsTextElement) then
          Build := StrToInt(VerBuild.GetText());
      end;

      UpdateAvailable := EncodeVersion(Major, Minor, Patch, Build) > Preferences.Version;

      VersionStr := IntToStr(Major) + '.' + IntToStr(Minor) + '  (Build ' + IntToStr(Patch) + '.' + IntToStr(Build) + ')';

      Infos := PAD.ChildNodes.FindNode('Web_Info');

      if (Assigned(Infos)) then
        if (Assigned(Infos.ChildNodes.FindNode('Download_URLs'))) then
          if (Assigned(Infos.ChildNodes.FindNode('Download_URLs'))) then
            EXE_URI := Infos.ChildNodes.FindNode('Download_URLs').ChildNodes.FindNode('Primary_Download_URL').Text;
    end;
  end;

  Result := (Success) and (Major > 0) and (Minor >= 0) and (Patch >= 0) and (Build >= 0);
end;

{ THTTPThread *****************************************************************}

procedure THTTPThread.AfterConstruction();
begin
  ReturnValue := 0;
  Stream := nil;
  URI := '';

  URLComponents.dwStructSize := SizeOf(URLComponents);

  URLComponents.dwHostNameLength := INTERNET_MAX_SCHEME_LENGTH;
  URLComponents.dwHostNameLength := INTERNET_MAX_HOST_NAME_LENGTH;
  URLComponents.dwUserNameLength := INTERNET_MAX_USER_NAME_LENGTH;
  URLComponents.dwPasswordLength := INTERNET_MAX_PASSWORD_LENGTH;
  URLComponents.dwUrlPathLength := INTERNET_MAX_PATH_LENGTH;
  URLComponents.dwExtraInfoLength := INTERNET_MAX_PATH_LENGTH;

  GetMem(URLComponents.lpszScheme, URLComponents.dwSchemeLength);
  GetMem(URLComponents.lpszHostName, URLComponents.dwHostNameLength);
  GetMem(URLComponents.lpszUserName, URLComponents.dwUserNameLength);
  GetMem(URLComponents.lpszPassword, URLComponents.dwPasswordLength);
  GetMem(URLComponents.lpszUrlPath, URLComponents.dwUrlPathLength);
  GetMem(URLComponents.lpszExtraInfo, URLComponents.dwExtraInfoLength);

  GetMem(Buffer, BufferSize);

  inherited;
end;

procedure THTTPThread.DoTerminate();
begin
  inherited;

  FreeMem(Buffer);

  FreeMem(URLComponents.lpszScheme);
  FreeMem(URLComponents.lpszHostName);
  FreeMem(URLComponents.lpszUserName);
  FreeMem(URLComponents.lpszPassword);
  FreeMem(URLComponents.lpszUrlPath);
  FreeMem(URLComponents.lpszExtraInfo);
end;

procedure THTTPThread.Execute();
var
  Client: HInternet;
  DownloadTry: Integer;
  Error: Boolean;
  Headers: string;
  Index: Cardinal;
  Internet: HInternet;
  L: Longint;
  Request: HInternet;
  Size: Cardinal;
  StatusCode: Word;
  Success: Boolean;
begin
  ReturnValue := 0; DownloadTry := 0; Error := False;

  if (not InternetCrackUrl(PChar(URI), Length(URI), ICU_DECODE, URLComponents)) then
    Internet := nil
  else
    Internet := InternetOpen(PChar(Application.Title + '/' + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor)), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if (not Assigned(Internet)) then
    ReturnValue := 1
  else
  begin
    L := 120000;
    InternetSetOption(Internet, INTERNET_OPTION_SEND_TIMEOUT, @L, SizeOf(L));
    InternetSetOption(Internet, INTERNET_OPTION_RECEIVE_TIMEOUT, @L, SizeOf(L));

    Client := InternetConnect(Internet, URLComponents.lpszHostName, URLComponents.nPort, URLComponents.lpszUserName, URLComponents.lpszPassword, INTERNET_SERVICE_HTTP, 0, 0);

    if (not Assigned(Client)) then
      ReturnValue := 1
    else
    begin
      Request := HttpOpenRequest(Client, 'GET', PChar(string(URLComponents.lpszUrlPath) + string(URLComponents.lpszExtraInfo)), 'HTTP/1.1', nil, nil, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);

      if (not Assigned(Request)) then
        ReturnValue := 1
      else
      begin
        repeat
          Stream.Size := 0; FSize := -1;

          if (not HttpSendRequest(Request, nil, 0, nil, 0)) then
            StatusCode := 1
          else
          begin
            Size := BufferSize; Index := 0;
            if (HttpQueryInfo(Request, HTTP_QUERY_CONTENT_LENGTH, Buffer, Size, Index)) then
              FSize := StrToInt(PChar(Buffer));

            repeat
              Success := InternetReadFile(Request, Buffer, BufferSize, Size);
              if (Success and (Size > 0)) then
                Stream.Write(Buffer^, Size);

              PostMessage(Wnd, CM_UPDATE_PROGRESSBAR, Stream.Size, FSize);
            until (Terminated or (Success and (Size = 0)));

            FSize := Stream.Size;

            Size := BufferSize; Index := 0;
            if (Terminated or not HttpQueryInfo(Request, HTTP_QUERY_STATUS_CODE, Buffer, Size, Index)) then
              StatusCode := 1
            else
              StatusCode := StrToInt(PChar(Buffer));

            if (StatusCode = HTTP_STATUS_PROXY_AUTH_REQ) then
            begin
              Headers := 'Proxy-Connection: Keep-Alive';
              HttpAddRequestHeaders(Request, PChar(Headers), Length(Headers), HTTP_ADDREQ_FLAG_ADD_IF_NEW);

              Error := InternetErrorDlg(Application.Handle, Request, ERROR_INTERNET_INCORRECT_PASSWORD, FLAGS_ERROR_UI_FILTER_FOR_ERRORS or FLAGS_ERROR_UI_FLAGS_GENERATE_DATA or FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS, Pointer(nil^)) <> ERROR_INTERNET_FORCE_RETRY;
              if (not Error) then
                DownloadTry := 0;
            end;
          end;

          Inc(DownloadTry);
        until (Error or Terminated or (StatusCode = HTTP_STATUS_OK) or (Stream.Size = 0) or (DownloadTry >= 3));

        ReturnValue := StatusCode;
      end;
      InternetCloseHandle(Request);
    end;

    InternetCloseHandle(Internet);
  end;
end;

{ THTTPMessagedThread *********************************************************}

procedure THTTPMessagedThread.AfterConstruction();
begin
  SuccessMessage := 0;

  inherited;
end;

procedure THTTPMessagedThread.Execute();
begin
  inherited;

  if ((ReturnValue = HTTP_STATUS_OK) and (Wnd > 0) and (SuccessMessage > 0)) then
    PostMessage(Wnd, SuccessMessage, 0, 0);
end;

{ TCheckUpdateThread **********************************************************}

procedure TCheckUpdateThread.AfterConstruction();
begin
  SuccessMessage := 0;

  inherited;
end;

procedure TCheckUpdateThread.DoTerminate();
begin
  if (FreeOnTerminate) then
  begin
    Stream.Free();
    CheckUpdateThread := nil;
  end;

  inherited;
end;

procedure TCheckUpdateThread.Execute();
var
  EXE_URI: string;
  VersionStr: string;
begin
  CoInitialize(nil);

  URI := SysUtils.LoadStr(1005);

  inherited;

  if (ReturnValue = HTTP_STATUS_OK) then
    if (CheckActualVersion(TStringStream(Stream), VersionStr, UpdateAvailable, EXE_URI) and (SuccessMessage <> 0)) then
    begin
      Preferences.UpdateChecked := Now();
      if (UpdateAvailable) then
        PostMessage(Wnd, SuccessMessage, 0, 0);
    end;

  CoUninitialize();
end;

procedure TDInstallUpdate.CMChangePreferences(var Message: TMessage);
begin
  Caption := ReplaceStr(Preferences.LoadStr(666), '&', '');

  GroupBox.Caption := ReplaceStr(Preferences.LoadStr(224), '&', '');

  FBOk.Caption := Preferences.LoadStr(230);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

procedure TDInstallUpdate.CMPadFileReceived(var Message: TMessage);
var
  UpdateAvailable: Boolean;
  Version: string;
begin
  Preferences.UpdateChecked := Now();

  HTTPThread.WaitFor();
  FreeAndNil(HTTPThread);

  SendMessage(Handle, CM_UPDATE_PROGRESSBAR, PAD_Stream.Position, PAD_Stream.Size);

  if (not CheckActualVersion(PAD_Stream, Version, UpdateAvailable, EXE_URI)) then
  begin
    FVersionInfo.Caption := Preferences.LoadStr(663) + ': ' + Preferences.LoadStr(384);
    MsgBox(Preferences.LoadStr(508), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
    FBCancel.Click();
  end
  else
  begin
    FVersionInfo.Caption := Preferences.LoadStr(663) + ': ' + Version;

    if (not UpdateAvailable) then
    begin
      MsgBox(Preferences.LoadStr(507), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
      FBCancel.Click();
    end
    else
    begin
      SendMessage(Handle, CM_UPDATE_PROGRESSBAR, 0, 0);

      FBOk.Enabled := True;
      FBCancel.OnClick := nil;
      ActiveControl := FBOk;
    end;
  end;
end;

procedure TDInstallUpdate.CMProgramFileReceived(var Message: TMessage);
begin
  HTTPThread.WaitFor();
  FreeAndNil(HTTPThread);

  SendMessage(Handle, CM_UPDATE_PROGRESSBAR, EXE_Stream.Position, EXE_Stream.Size);

  FProgram.Caption := Preferences.LoadStr(665) + ': ' + Preferences.LoadStr(138);
  FBCancel.OnClick := nil;

  FreeAndNil(EXE_Stream);

  Preferences.SetupProgram := SetupPrgFilename;
  Preferences.SetupProgramInstalled := False;

  MsgBox(Preferences.LoadStr(848), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

  ModalResult := mrOk;
end;

procedure TDInstallUpdate.CMUpdateProgressBarReceived(var Message: TMessage);
begin
  if (Message.LParam <= 0) then
  begin
    FProgressBar.Position := 0;
    FProgressBar.Max := 0;
  end
  else
  begin
    FProgressBar.Position := (Integer(Message.WParam) * 100) div Integer(Message.LParam);
    FProgressBar.Max := 100;
  end;
end;

function TDInstallUpdate.Execute(): Boolean;
begin
  Result := ShowModal() = mrOk;
end;

procedure TDInstallUpdate.FBCancelClick(Sender: TObject);
begin
  if (Assigned(HTTPThread)) then
  begin
    HTTPThread.FreeOnTerminate := True;
    HTTPThread.Terminate();
  end;
end;

procedure TDInstallUpdate.FBOkClick(Sender: TObject);
var
  Ext: string;
  FilenameP: array [0 .. MAX_PATH] of Char;
  I: Integer;
begin
  if (GetTempPath(MAX_PATH, FilenameP) > 0) then
  begin
    SetupPrgFilename := EXE_URI;
    while (Pos('/', SetupPrgFilename) > 0) do Delete(SetupPrgFilename, 1, Pos('/', SetupPrgFilename));

    if (not FileExists(FilenameP + SetupPrgFilename)) then
      SetupPrgFilename := FilenameP + SetupPrgFilename
    else
    begin
      Ext := ExtractFileExt(SetupPrgFilename);
      Delete(SetupPrgFilename, Length(SetupPrgFilename) - Length(Ext) + 1, Length(Ext));
      I := 2;
      while (FileExists(FilenameP + SetupPrgFilename + ' (' + IntToStr(I) + ')' + Ext)) do Inc(I);
      SetupPrgFilename := FilenameP + SetupPrgFilename + ' (' + IntToStr(I) + ')' + Ext;
    end;

    FProgram.Caption := Preferences.LoadStr(665) + ' ...';
    FProgram.Enabled := True;
    FBOk.Enabled := False;
    FBCancel.OnClick := FBCancelClick;
    ActiveControl := FBCancel;

    EXE_Stream := TFileStream.Create(SetupPrgFilename, fmCreate);

    HTTPThread := THTTPMessagedThread.Create(True);
    HTTPThread.URI := PChar(EXE_URI);
    HTTPThread.Stream := EXE_Stream;
    HTTPThread.Wnd := Handle;
    HTTPThread.SuccessMessage := CM_PROGRAM_FILE_RECEIVED;

    SendMessage(Handle, CM_UPDATE_PROGRESSBAR, 2, 100);

    HTTPThread.Start();
  end;
end;

{ TDInstallUpdate *************************************************************}

procedure TDInstallUpdate.FormCreate(Sender: TObject);
begin
  FullHeight := Height;
  HTTPThread := nil;
end;

procedure TDInstallUpdate.FormHide(Sender: TObject);
begin
  if (Assigned(HTTPThread) and not HTTPThread.FreeOnTerminate) then
    FreeAndNil(HTTPThread);

  FreeAndNil(PAD_Stream);
  if (Assigned(EXE_Stream)) then
    FreeAndNil(EXE_Stream);
end;

procedure TDInstallUpdate.FormShow(Sender: TObject);
begin
  FVersionInfo.Caption := Preferences.LoadStr(663);
  FVersionInfo.Enabled := False;

  FBOk.Enabled := False;

  FProgram.Caption := Preferences.LoadStr(665);
  FProgram.Enabled := False;

  SendMessage(Handle, CM_UPDATE_PROGRESSBAR, 0, 100);

  PAD_Stream := TStringStream.Create('');
  EXE_Stream := nil;

  FBCancel.OnClick := FBCancelClick;

  FVersionInfo.Caption := Preferences.LoadStr(663) + ' ...';
  FVersionInfo.Enabled := True;

  HTTPThread := THTTPMessagedThread.Create(True);
  HTTPThread.URI := SysUtils.LoadStr(1005);
  HTTPThread.Stream := PAD_Stream;
  HTTPThread.Wnd := Handle;
  HTTPThread.SuccessMessage := CM_PAD_FILE_RECEIVED;

  SendMessage(Handle, CM_UPDATE_PROGRESSBAR, 10, 100);

  HTTPThread.Start();
end;

initialization
  CheckUpdateThread := nil;
  FInstallUpdate := nil;
end.
