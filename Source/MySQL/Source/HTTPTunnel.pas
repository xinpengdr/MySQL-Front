unit HTTPTunnel;

interface {********************************************************************}

uses
  Windows, SyncObjs, WinInet,
  MySQLClient, MySQLConsts;

const
  CR_HTTPTUNNEL_UNKNOWN_ERROR              = 2200;
  CR_HTTPTUNNEL_OLD                        = 2201;
  CR_HTTPTUNNEL_CONN_ERROR                 = 2202;
  CR_HTTPTUNNEL_ACCESS_DENIED_ERROR        = 2203;
  CR_HTTPTUNNEL_NOT_FOUND                  = 2204;
  CR_HTTPTUNNEL_UNKNOWN_HOST               = 2205;
  CR_HTTPTUNNEL_INVALID_CONTENT_TYPE_ERROR = 2206;
  CR_HTTPTUNNEL_INVALID_SERVER_RESPONSE    = 2207;
  CR_HTTPTUNNEL_HTTP_CONNECTION            = 2208;
  CR_HTTPTUNNEL_SERVER_ERROR               = 2209;
  CR_HTTPTUNNEL_REDIRECT                   = 2210;

  MYSQL_OPT_HTTPTUNNEL_URL   = 240;
  MYSQL_OPT_HTTPTUNNEL_AGENT = 241;

type
  PPChar = ^PChar;

  MYSQL = class (MySQLClient.MYSQL)
  private
    Agent: string;
    Connection: HInternet;
    Handle: HInternet;
    LastRequest: TDateTime;
    SendBuffer: TMySQL_File.TFileBuffer;
    Request: HInternet;
    RequestComplete: TEvent;
    ResponseReceived: TEvent;
    SecurityFlags: ULONG;
    SID: string;
    URL: string;
    URLComponents: TURLComponents;
    function ExecuteHTTPRequest(const Connect: Boolean): Boolean;
  protected
    procedure Close(); override;
    function ExecuteCommand(const Command: enum_server_command;
      const Bin: my_char; const Size: my_int; const Retry: Boolean): my_int; override;
    function Open(const AType: TMYSQL_IO.TType; const AHost, AUnixSocket: RawByteString;
      const APort, ATimeout: my_uint): Boolean; override;
    function Receive(var Buffer; const BytesToRead: my_uint; out BytesRead: my_uint): Boolean; override;
    function Send(const Buffer; const BytesToWrite: my_uint): Boolean; override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    function get_host_info(): my_char; override;
    function options(option: enum_mysql_option; const arg: my_char): my_int; override;
  end;
  TMYSQL = MYSQL;

function mysql_init(mysql: MYSQL): MYSQL; stdcall;

implementation {***************************************************************}

uses
  SysUtils, Forms,
  SQLUtils;

const
  RequiredTunnelVersion = 8;

const
  HTTPTTUNNEL_ERRORS: array [0..8] of PChar = (
    'Unknown HTTP Tunnel error (%d)',                                        {0}
    'HTTP Tunnel script (%s) is too old - please update',                    {1}
    'Can''t connect to MySQL server through HTTP Tunnel ''%s'' (%d)',        {2}
    'Access denied (403)',                                                   {3}
    'File Not Found (404):  ''%s''',                                         {4}
    'Unknown HTTP Server Host ''%s'' (%d)',                                  {5}
    'Invalid HTTP content type (''%s'').',                                   {6}
    'The HTTP server response could not be parsed (%s):' + #10#10 + '%s',    {7}
    '%-.64s via HTTP'                                                        {8}
  );

{******************************************************************************}

procedure InternetStatusCallback(Handle: HINTERNET; Context, dwInternetStatus: DWord; lpvStatusInformation: PInternetAsyncResult; StatusInformationLength: DWord); stdcall;
var
  mysql: TMYSQL;
begin
  mysql := TMYSQL(Context);

  case (dwInternetStatus) of
    INTERNET_STATUS_RESPONSE_RECEIVED: mysql.ResponseReceived.SetEvent();
    INTERNET_STATUS_REQUEST_COMPLETE: mysql.RequestComplete.SetEvent();
  end;
end;

{ C API ***********************************************************************}

function mysql_init(mysql: MYSQL): MYSQL;
begin
  if (Assigned(mysql)) then
    Result := mysql
  else
    Result := HTTPTunnel.MYSQL.Create();
end;

{ MYSQL ***********************************************************************}

procedure MYSQL.Close();
begin
  if (SendBuffer.Size - SendBuffer.Offset > 0) then
    ExecuteHTTPRequest(False);

  if (Assigned(Request)) then
    InternetCloseHandle(Request);

  if (Assigned(Connection)) then
    InternetCloseHandle(Connection);

  if (Assigned(Handle)) then
  begin
    InternetSetStatusCallback(Handle, nil);
    InternetCloseHandle(Handle);
  end;

  ReallocBuffer(SendBuffer, 0);

  SID := '';

  inherited;
end;

constructor MYSQL.Create();
begin
  inherited;

  Agent := LoadStr(1000);
  Connection := nil;
  Handle := nil;
  LastRequest := 0;
  SecurityFlags := SECURITY_FLAG_IGNORE_REVOCATION or SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_WRONG_USAGE or SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;

  SID := '';
  URL := '';

  FillChar(SendBuffer, SizeOf(SendBuffer), #0);

  URLComponents.dwStructSize := SizeOf(URLComponents);
  GetMem(URLComponents.lpszScheme, INTERNET_MAX_SCHEME_LENGTH);
  GetMem(URLComponents.lpszHostName, INTERNET_MAX_HOST_NAME_LENGTH);
  GetMem(URLComponents.lpszUserName, INTERNET_MAX_USER_NAME_LENGTH);
  GetMem(URLComponents.lpszPassword, INTERNET_MAX_PASSWORD_LENGTH);
  GetMem(URLComponents.lpszUrlPath, INTERNET_MAX_PATH_LENGTH);
  GetMem(URLComponents.lpszExtraInfo, 256);

  ResponseReceived := TEvent.Create(nil, False, False, '');
  RequestComplete := TEvent.Create(nil, True, False, '');
end;

destructor MYSQL.Destroy();
begin
  inherited;

  ResponseReceived.Free();
  RequestComplete.Free();

  FreeMem(URLComponents.lpszScheme);
  FreeMem(URLComponents.lpszHostName);
  FreeMem(URLComponents.lpszUserName);
  FreeMem(URLComponents.lpszPassword);
  FreeMem(URLComponents.lpszUrlPath);
  FreeMem(URLComponents.lpszExtraInfo);
end;

var
  Nils: Boolean = False;

function MYSQL.ExecuteHTTPRequest(const Connect: Boolean): Boolean;
var
  Buffer: array [0..2048] of Char;
  Flags: Cardinal;
  Headers: string;
  HttpRequestError: Longint;
  Index: DWord;
  InternetError: Longint;
  ObjectName: string;
  pvData: Pointer;
  S: string;
  Size: DWord;
  StatusCode: array [0..4] of Char;
begin
  if (Assigned(Request)) then
    InternetCloseHandle(Request);

  ResponseReceived.ResetEvent();
  RequestComplete.ResetEvent();

  if (errno() = 0) then
  begin
    ObjectName := '';
    if (URLComponents.dwUrlPathLength > 0) then
      ObjectName := ObjectName + URLComponents.lpszUrlPath;
    if (URLComponents.dwExtraInfoLength > 0) then
      ObjectName := ObjectName + URLComponents.lpszExtraInfo;
    if (SID <> '') then
      if (URLComponents.dwExtraInfoLength = 0) then
        ObjectName := ObjectName + '?SID=' + SID
      else
        ObjectName := ObjectName + '&SID=' + SID;

    Flags := INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_NO_UI or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_COOKIES;
    if (URLComponents.lpszScheme = 'https') then
      Flags := Flags or INTERNET_FLAG_SECURE;

    Request := HttpOpenRequest(Connection, 'POST', PChar(ObjectName), 'HTTP/1.1', nil, nil, Flags, Cardinal(Self));
    if (not Assigned(Request)) then
      Seterror(CR_IPSOCK_ERROR)
    else
    begin
      InternetSetOption(Request, INTERNET_OPTION_SECURITY_FLAGS, @SecurityFlags, SizeOf(SecurityFlags));

      Headers := 'Content-Type: application/binary' + #10;
      Headers := Headers + 'Keep-Alive: 300' + #10;
      if (not Connect) then
        Headers := Headers + 'Referer: ' + URL + #10;

      repeat
        if (not HttpSendRequest(Request, PChar(Headers), Length(Headers), @SendBuffer.Mem[SendBuffer.Offset], SendBuffer.Size - SendBuffer.Offset)) then
        begin
          HttpRequestError := GetLastError();

          InternetError := InternetErrorDlg(Application.MainForm.Handle, Request, HttpRequestError,
            FLAGS_ERROR_UI_FILTER_FOR_ERRORS or FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or FLAGS_ERROR_UI_FLAGS_GENERATE_DATA, pvData);
        end
        else
        begin
          HttpRequestError := 0;
          InternetError := 0;
        end;
      until ((InternetError <> ERROR_INTERNET_FORCE_RETRY) and not ((InternetError = 0) and ((HttpRequestError = ERROR_INTERNET_SEC_CERT_DATE_INVALID) or (HttpRequestError = ERROR_INTERNET_INVALID_CA) or (HttpRequestError = 12057))));

      case (HttpRequestError) of
        NOERROR:
          begin
            SendBuffer.Offset := 0;
            SendBuffer.Size := 0;

            SecurityFlags := 0; Size := SizeOf(SecurityFlags);
            InternetQueryOption(Request, INTERNET_OPTION_SECURITY_FLAGS, @SecurityFlags, Size);

            ZeroMemory(@StatusCode, SizeOf(StatusCode));
            repeat
              ResponseReceived.WaitFor(50);
              Size := SizeOf(StatusCode); Index := 0;
            until (HttpQueryInfo(Request, HTTP_QUERY_STATUS_CODE, @StatusCode, Size, Index) or (Size = 0) or (SysUtils.StrToInt(StatusCode) <> 0));

            if (HttpRequestError <> NOERROR) then
              Seterror(CR_HTTPTUNNEL_UNKNOWN_ERROR, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_UNKNOWN_ERROR - CR_HTTPTUNNEL_UNKNOWN_ERROR], [GetLastError()])))
            else
              case (SysUtils.StrToInt(StatusCode)) of
                HTTP_STATUS_FORBIDDEN:
                  begin
                    Seterror(CR_HTTPTUNNEL_ACCESS_DENIED_ERROR, RawByteString(StatusCode));
                    Size := SizeOf(Buffer); Index := 0;
                    if (HttpQueryInfo(Request, HTTP_QUERY_STATUS_TEXT, @Buffer, Size, Index)) then
                      Seterror(CR_HTTPTUNNEL_ACCESS_DENIED_ERROR, error() + ' ' + RawByteString(Buffer));
                  end;
                HTTP_STATUS_REDIRECT,
                HTTP_STATUS_REDIRECT_METHOD:
                  begin
                    Size := SizeOf(Buffer); Index := 0;
                    if (HttpQueryInfo(Request, HTTP_QUERY_LOCATION, @Buffer, Size, Index)) then
                    begin
                      SetString(ObjectName, PChar(@Buffer), Size);
                      SetString(S, URLComponents.lpszExtraInfo, URLComponents.dwExtraInfoLength);
                      if (S <> '') then
                        ObjectName := ObjectName + S;
                      Seterror(CR_HTTPTUNNEL_REDIRECT);
                    end;
                  end;
                HTTP_STATUS_NOT_FOUND:
                  Seterror(CR_HTTPTUNNEL_NOT_FOUND, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_NOT_FOUND - CR_HTTPTUNNEL_UNKNOWN_ERROR], [ObjectName])));
                HTTP_STATUS_OK:
                  begin
                    Size := SizeOf(Buffer);
                    if (HttpQueryInfo(Request, HTTP_QUERY_CONTENT_TYPE, @Buffer, Size, Index) and (LowerCase(Buffer) <> 'application/mysql-front')) then
                      if (not Receive(Buffer, SizeOf(Buffer), Size) or (Size = 0)) then
                        Seterror(CR_HTTPTUNNEL_INVALID_CONTENT_TYPE_ERROR, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_INVALID_CONTENT_TYPE_ERROR - CR_HTTPTUNNEL_UNKNOWN_ERROR], [Buffer])))
                      else
                      begin
                        SetString(S, PChar(@Buffer), Size);
                        Seterror(CR_HTTPTUNNEL_INVALID_SERVER_RESPONSE, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_INVALID_SERVER_RESPONSE - CR_HTTPTUNNEL_UNKNOWN_ERROR], [ObjectName, S])));
                      end;
                  end;
                else
                  begin
                    Seterror(CR_HTTPTUNNEL_SERVER_ERROR, RawByteString(StatusCode));
                    Size := SizeOf(Buffer); Index := 0;
                    if (HttpQueryInfo(Request, HTTP_QUERY_STATUS_TEXT, @Buffer, Size, Index)) then
                      Seterror(CR_HTTPTUNNEL_SERVER_ERROR, error() + ' ' + RawByteString(Buffer));
                  end;
              end;
          end;
        ERROR_IO_PENDING: ResponseReceived.WaitFor(NET_WAIT_TIMEOUT * 1000);
        ERROR_INTERNET_CANNOT_CONNECT,
        ERROR_INTERNET_CONNECTION_RESET,
        ERROR_INTERNET_TIMEOUT:
          if (IOType = itNone) then
            Seterror(CR_CONN_HOST_ERROR, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_CONN_ERROR - CR_HTTPTUNNEL_UNKNOWN_ERROR], [ObjectName, GetLastError()])))
          else
            Seterror(CR_SERVER_GONE_ERROR);
        ERROR_INTERNET_NAME_NOT_RESOLVED:
          Seterror(CR_UNKNOWN_HOST, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_UNKNOWN_HOST - CR_HTTPTUNNEL_UNKNOWN_ERROR], [URLComponents.lpszHostName, GetLastError()])));
        ERROR_HTTP_INVALID_SERVER_RESPONSE:
          Seterror(CR_SERVER_HANDSHAKE_ERR);
        else
          Seterror(CR_HTTPTUNNEL_UNKNOWN_ERROR, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_UNKNOWN_ERROR - CR_HTTPTUNNEL_UNKNOWN_ERROR], [HttpRequestError])));
      end;
    end;

    LastRequest := Now();
  end;

  Result := errno() = 0;
end;

function MYSQL.get_host_info(): my_char;
begin
  if (fhost_info = '') then
    fhost_info := AnsiString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_HTTP_CONNECTION - CR_HTTPTUNNEL_UNKNOWN_ERROR], [fhost]));

  Result := my_char(fhost_info);
end;

function MYSQL.Open(const AType: TMYSQL_IO.TType; const AHost, AUnixSocket: RawByteString;
  const APort, ATimeout: my_uint): Boolean;
var
  Buffer: array [0..127] of Char;
  Command: AnsiChar;
  Index: DWord;
  L: Longint;
  Size: DWord;
begin
  Result := AType = itTCPIP;
  if (Result) then
  begin
    Handle := InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

    if (not Assigned(Handle)) then
      Seterror(CR_SOCKET_CREATE_ERROR)
    else
    begin
      InternetSetStatusCallback(Handle, @InternetStatusCallback);

      L := ftimeout * 1000;
      InternetSetOption(Handle, INTERNET_OPTION_CONNECT_TIMEOUT, @L, SizeOf(L));

      L := NET_WAIT_TIMEOUT * 1000;
      InternetSetOption(Handle, INTERNET_OPTION_RECEIVE_TIMEOUT, @L, SizeOf(L));
      L := NET_WRITE_TIMEOUT * 1000;
      InternetSetOption(Handle, INTERNET_OPTION_SEND_TIMEOUT, @L, SizeOf(L));

      repeat
        Seterror(0);
        if (Assigned(Connection)) then
          InternetCloseHandle(Connection);

        URLComponents.dwSchemeLength := INTERNET_MAX_SCHEME_LENGTH;
        URLComponents.dwHostNameLength := INTERNET_MAX_HOST_NAME_LENGTH;
        URLComponents.dwUserNameLength := INTERNET_MAX_USER_NAME_LENGTH;
        URLComponents.dwPasswordLength := INTERNET_MAX_PASSWORD_LENGTH;
        URLComponents.dwUrlPathLength := INTERNET_MAX_PATH_LENGTH;
        URLComponents.dwExtraInfoLength := 256;
        InternetCrackUrl(PChar(URL), Length(URL), ICU_DECODE, URLComponents);

        Connection := InternetConnect(Handle, URLComponents.lpszHostName, URLComponents.nPort, URLComponents.lpszUserName, URLComponents.lpszPassword, INTERNET_SERVICE_HTTP, 0, Cardinal(Self));
        if (not Assigned(Connection)) then
          Seterror(CR_HTTPTUNNEL_CONN_ERROR, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_CONN_ERROR - CR_HTTPTUNNEL_UNKNOWN_ERROR], [URL, 0])))
        else
        begin
          Direction := idWrite;

          Command := AnsiChar(COM_CONNECT);
          WriteFile(@Command, SizeOf(Command));
          if (lstrcmpi(URLComponents.lpszHostName, PChar(string(fhost))) = 0) then
            WriteFile(LOCAL_HOST)
          else
            WriteFile(RawByteString(fhost));
          WriteFile(RawByteString(fuser));
          WriteFile(RawByteString(fpasswd));
          WriteFile(RawByteString(fdb));
          WriteFile(RawByteString(fcharacter_set_name));
          WriteFile(fport, 2);
          WriteFile(fclient_flag, 4);
          WriteFile(ftimeout, 2);
          FlushFileBuffers();

          if (ExecuteHTTPRequest(True)) then
          begin
            StrPCopy(@Buffer, 'MF-Version'); Size := SizeOf(Buffer); Index := 0;
            if (not HttpQueryInfo(Request, HTTP_QUERY_CUSTOM, @Buffer, Size, Index) or (StrToInt(Buffer) < RequiredTunnelVersion)) then
              Seterror(CR_HTTPTUNNEL_OLD, RawByteString(Format(HTTPTTUNNEL_ERRORS[CR_HTTPTUNNEL_OLD - CR_HTTPTUNNEL_UNKNOWN_ERROR], [URL])))
            else
            begin
              Size := SizeOf(Buffer); Index := 0;

              StrPCopy(@Buffer, 'MF-SID'); Size := SizeOf(Buffer); Index := 0;
              if (HttpQueryInfo(Request, HTTP_QUERY_CUSTOM, @Buffer, Size, Index)) then
                SID := PChar(@Buffer);


              Direction := idRead;

              IOType := TMYSQL_IO.TType(Integer(High(TMYSQL_IO.TType)) + 1);
            end;
          end;
        end;
      until (errno() <> CR_HTTPTUNNEL_REDIRECT);
    end;

    Result := errno() = 0;
  end;
end;

function MYSQL.options(option: enum_mysql_option; const arg: my_char): my_int;
begin
  Result := 0;

  case (Integer(option)) of
    MYSQL_OPT_HTTPTUNNEL_URL: URL := DecodeString(RawByteString(arg));
    MYSQL_OPT_HTTPTUNNEL_AGENT: Agent := DecodeString(RawByteString(arg));
    else Result := inherited options(option, arg);
  end;
end;

function MYSQL.ExecuteCommand(const Command: enum_server_command; const Bin: my_char; const Size: my_int; const Retry: Boolean): my_int;
var
  Index: Integer;
  Len: Integer;
  SQL: string;
  SQLIndex: Integer;
  SQLLen: Integer;
begin
  if (Command = COM_PING) then
    Result := 0
  else
  begin
    SendBuffer.Offset := 0;
    SendBuffer.Size := 0;

    Seterror(0);
    next_command();

    if (Command <> COM_QUERY) then
      WriteFile(Bin, Size)
    else
    begin
      SQL := DecodeString(Bin); SQLIndex := 1;
      Index := 0;
      while (Index < Size) do
      begin
        SQLLen := SQLStmtLength(SQL, SQLIndex);
        Len := WideCharToMultiByte(CodePage, 0, PChar(@SQL[SQLIndex]), SQLLen, nil, 0, nil, nil);

        if (GetFileSize() > 0) then
          SetFilePointer(1, PACKET_CURRENT);
        WriteFile(@Command, 1);
        WriteFile(my_char(@Bin[Index]), Len);

        Inc(SQLIndex, SQLLen);
        Inc(Index, Len);
      end;
    end;

    if ((FlushFileBuffers() or (errno() = CR_SERVER_GONE_ERROR)) and ExecuteHTTPRequest(False) and (next_result() <= 0)) then
      Result := 0
    else
    begin
      if (errno() = 0) then
        Seterror(CR_SERVER_LOST);
      Result := 1;
    end;
  end;
end;

function MYSQL.Receive(var Buffer; const BytesToRead: my_uint; out BytesRead: my_uint): Boolean;
var
  Size: DWord;
begin
  BytesRead := 0;
  repeat
    Result := InternetReadFile(Request, @my_char(@Buffer)[BytesRead], BytesToRead - BytesRead, Size);
    if (not Result) then
      Seterror(CR_SERVER_LOST)
    else
      Inc(BytesRead, Size);
  until (not Result or (Size = 0) or (BytesRead = BytesToRead));
end;

function MYSQL.Send(const Buffer; const BytesToWrite: my_uint): Boolean;
begin
  Result := (SendBuffer.Size + BytesToWrite <= SendBuffer.MemSize) or ReallocBuffer(SendBuffer, SendBuffer.Size + BytesToWrite);

  if (Result) then
  begin
    MoveMemory(@SendBuffer.Mem[SendBuffer.Size], @Buffer, BytesToWrite);
    Inc(SendBuffer.Size, BytesToWrite);
  end;
end;

end.

