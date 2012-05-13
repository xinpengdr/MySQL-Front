unit MySQLClient;

interface {********************************************************************}

uses
  SyncObjs, WinSock,
  SysUtils,
  MySQLConsts;

type
  MYSQL_RES = class;
  MYSQL = class;

  TMySQL_IO = class
  type
    TType = (itNone, itNamedPipe, itTCPIP);
    TDirection = (idRead, idWrite);
  private
    FErrNo: my_uint;
    FError: RawByteString;
    FDirection: TDirection;
    Pipe: THandle;
    Socket: TSocket;
  protected
    IOType: TType;
    function GetCodePage(): Cardinal; virtual;
    property CodePage: Cardinal read GetCodePage;
    procedure SetFileAccess(ADirection: TDirection); virtual;
    property Direction: TDirection read FDirection write SetFileAccess;
    procedure Close(); virtual;
    function DecodeString(const Str: RawByteString): string; virtual;
    function EncodeString(const Str: string): RawByteString; virtual;
    function Open(const AType: TType; const Host, PipeName: RawByteString;
      const Port, Timeout: my_uint): Boolean; virtual;
    function Receive(var Buffer; const BytesToRead: my_uint; out BytesRead: my_uint): Boolean; virtual;
    function Send(const Buffer; const BytesToWrite: my_uint): Boolean; virtual;
    procedure Seterror(const AErrNo: my_uint; const AError: RawByteString = ''); virtual;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;
    function errno(): my_uint; virtual;
    function error(): RawByteString; virtual;
  end;

  TMySQL_File = class (TMySQL_IO)
  type
    TFileBuffer = record
      Mem: my_char;
      Offset: my_uint;
      Size: my_uint;
      TotalSize: my_uint;
    end;
    TClientStatus = (
      MYSQL_STATUS_READY,
      MYSQL_STATUS_GET_RESULT,
      MYSQL_STATUS_USE_RESULT
    );
  private
    CompPacketNr: Byte;
    FCompress: Boolean;
    FReadFileBuffer: TFileBuffer;
    PacketBuffer: TFileBuffer;
    PacketNr: Byte;
    function ReceivePacket(): Boolean;
  protected
    property Compress: Boolean read FCompress write FCompress;
    property ReadFileBuffer: TFileBuffer read FReadFileBuffer;
    function CreateFile(const AIOType: TMySQL_IO.TType;
      const Host, UnixSocket: RawByteString; const Port, Timeout: my_uint): Boolean; virtual;
    procedure CloseFile(); virtual;
    function FlushFileBuffers(): Boolean; virtual;
    function GetFileSize(): my_int; virtual;
    procedure next_command(); virtual;
    function next_result(): my_int; virtual;
    function ReadFile(const Buffer: my_char; const Size: my_uint): Boolean; overload; virtual;
    function ReadFile(out Value: my_int; const Size: Byte = 0): Boolean; overload; virtual;
    function ReadFile(out Value: my_uint; const Size: Byte = 0): Boolean; overload; virtual;
    function ReadFile(out Value: my_ulonglong; const Size: Byte = 0): Boolean; overload; virtual;
    function ReadFile(out Value: RawByteString; const NTS: Boolean = True; const Size: Byte = 0): Boolean; overload; virtual;
    function ReallocBuffer(var Buffer: TFileBuffer; const Size: my_uint): Boolean;
    function SetFilePointer(const DistanceToMove: my_int; const MoveMethod: my_int): my_int; virtual;
    procedure SetFileAccess(ADirection: TMySQL_IO.TDirection); override;
    function WriteFile(const Buffer: my_char; const Size: my_uint): Boolean; overload; virtual;
    function WriteFile(const Value: my_ulonglong; const Size: my_uint): Boolean; overload; virtual;
    function WriteFile(const Value: RawByteString; const NTS: Boolean = True): Boolean; overload; virtual;
  public
    constructor Create(); override;
  end;

  MYSQL_RES = class
  type
    PRow = ^TRow;
    TRow = record
      MemSize: my_uint;
      Lengths: MYSQL_LENGTHS;
      Row: MYSQL_ROW;
      Next: PRow;
    end;
  private
    CurrentRow: PRow;
    FieldCount: my_uint;
    FieldIndex: my_int;
    FirstRow: PRow;
    mysql: MYSQL;
    ResultType: (rtUsed, rtStored);
    RowCount: my_ulonglong;
    RowIndex: my_ulonglong;
  protected
    Fields: MYSQL_FIELDS;
    Lengths: MYSQL_LENGTHS;
    property MysqlClient: MYSQL read mysql;
  public
    constructor Create(const Amysql: MYSQL; const AFieldCount: my_uint); virtual;
    procedure data_seek(offset: my_ulonglong); virtual;
    destructor Destroy(); override;
    function fetch_field(): MYSQL_FIELD; virtual;
    function fetch_field_direct(fieldnr: my_uint): MYSQL_FIELD; virtual;
    function fetch_fields(): MYSQL_FIELDS; virtual;
    function fetch_lengths(): MYSQL_LENGTHS; virtual;
    function fetch_row(): MYSQL_ROW; virtual;
    function num_fields(): my_uint; virtual;
    function num_rows(): my_ulonglong; virtual;
  end;

  MYSQL = class (TMySQL_File)
  private
    CriticalSection: TCriticalSection;
    FieldCount: my_uint;
    FSQLState: array [0 .. SQLSTATE_LENGTH - 1] of AnsiChar;
    UseNamedPipe: Boolean;
    function Reconnect(): Boolean;
    function SendFile(const Filename: RawByteString): Boolean;
  protected
    faffected_rows: my_ulonglong;
    fca: RawByteString;
    fca_path: RawByteString;
    fcert: RawByteString;
    fcharacter_set_name: RawByteString;
    fcipher: RawByteString;
    fclient_flag: my_uint;
    fclient_status: TMySQL_File.TClientStatus;
    fcompress: Boolean;
    fdb: RawByteString;
    fhost: RawByteString;
    fhost_info: RawByteString;
    finfo: my_char;
    finsert_id: my_ulonglong;
    fkey: RawByteString;
    flocal_infile_end: Tlocal_infile_end;
    flocal_infile_error: Tlocal_infile_error;
    flocal_infile_init: Tlocal_infile_init;
    flocal_infile_read: Tlocal_infile_read;
    flocal_infile_userdata: Pointer;
    fpasswd: RawByteString;
    fpipe_name: RawByteString;
    fport: Cardinal;
    freconnect: Boolean;
    fres: MYSQL_RES;
    fserver_capabilities: my_uint;
    fserver_info: my_char;
    fserver_status: my_int;
    fserver_version: my_uint;
    fstat: RawByteString;
    fthread_id: my_uint;
    ftimeout: my_uint;
    fuser: RawByteString;
    fwarning_count: my_uint;
    procedure CloseFile(); override;
    function ExecuteCommand(const Command: enum_server_command; const Bin: my_char; const Size: my_int; const Retry: Boolean): my_int; virtual;
    function GetCodePage(): Cardinal; override;
    function ReadRow(var Row: MYSQL_RES.PRow): my_int; virtual;
    procedure ReadRows(const Ares: MYSQL_RES); virtual;
    function ServerError(): Boolean; virtual;
    procedure Seterror(const AErrNo: my_uint; const AError: RawByteString = ''); override;
    property ClientStatus: TMySQL_File.TClientStatus read fclient_status;
    property CodePage: Cardinal read GetCodePage;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    function affected_rows(): my_ulonglong; virtual;
    function character_set_name(): my_char; virtual;
    function dump_debug_info(): my_int; virtual;
    function eof(): my_bool; virtual;
    function get_client_info(): my_char; virtual;
    function get_client_version(): my_uint; virtual;
    function get_host_info(): my_char; virtual;
    function get_server_info(): my_char; virtual;
    function get_server_status(): my_uint; virtual;
    function get_server_version(): my_int; virtual;
    function info(): my_char; virtual;
    function insert_id(): my_ulonglong; virtual;
    function kill(pid: my_uint): my_int; virtual;
    function more_results(): my_bool; virtual;
    function next_result(): my_int; override;
    function options(option: enum_mysql_option; const arg: my_char): my_int; virtual;
    function ping(): my_int; virtual;
    function real_connect(host, user, passwd, db: my_char; port: my_uint; unix_socket: my_char; client_flag: my_uint): MYSQL; virtual;
    function real_escape_string(_to: my_char; const from: my_char; length: my_uint): my_uint; virtual;
    function real_query(query: my_char; length: my_int): my_int; virtual;
    function refresh(options: my_int): my_int; virtual;
    function select_db(db: my_char): my_int; virtual;
    function set_character_set(const csname: my_char): my_int; virtual;
    procedure set_local_infile_default(); virtual;
    procedure set_local_infile_handler(local_infile_init: Tlocal_infile_init; local_infile_read: Tlocal_infile_read; local_infile_end: Tlocal_infile_end; local_infile_error: Tlocal_infile_error; userdata: Pointer); virtual;
    function set_server_option(option: enum_mysql_set_option): my_int; virtual;
    function shutdown(shutdown_level: mysql_enum_shutdown_level): my_int; virtual;
    function sqlstate(): my_char; virtual;
    function store_result(): MYSQL_RES; virtual;
    function thread_id(): my_uint; virtual;
    function use_result(): MYSQL_RES; virtual;
    function warning_count(): my_uint; virtual;
    property client_status: TMySQL_File.TClientStatus read fclient_status;
    property res: MYSQL_RES read fres;
  end;

function mysql_affected_rows(mysql: MYSQL): my_ulonglong; stdcall;
// function mysql_change_user(mysql: MYSQL; const user, passwd, db: my_char): my_bool; stdcall;
function mysql_character_set_name(mysql: MYSQL): my_char; stdcall;
procedure mysql_close(mysql: MYSQL); stdcall;
// function mysql_connect(mysql: MYSQL; const host, user, passwd: my_char): MYSQL; stdcall;
// function mysql_create_db(mysql: MYSQL; const DB: my_char): my_int; stdcall;
procedure mysql_data_seek(res: MYSQL_RES; offset: my_ulonglong); stdcall;
// procedure mysql_debug(const debug: my_char); stdcall;
// function mysql_drop_db(mysql: MYSQL; const DB: my_char): my_int; stdcall;
// function mysql_dump_debug_info(mysql: MYSQL): my_int; stdcall;
function mysql_eof(mysql: MYSQL): my_bool; stdcall;
function mysql_errno(mysql: MYSQL): my_uint; stdcall;
function mysql_error(mysql: MYSQL): my_char; stdcall;
// function mysql_escape_string(_to: my_char; const from: my_char; from_length: my_uint): my_uint; stdcall;
function mysql_fetch_field(res: MYSQL_RES): MYSQL_FIELD; stdcall;
function mysql_fetch_fields(res: MYSQL_RES): MYSQL_FIELDS; stdcall;
function mysql_fetch_field_direct(res: MYSQL_RES; fieldnr: my_uint): MYSQL_FIELD; stdcall;
function mysql_fetch_lengths(res: MYSQL_RES): MYSQL_LENGTHS; stdcall;
function mysql_fetch_row(res: MYSQL_RES): MYSQL_ROW; stdcall;
function mysql_field_count(mysql: MYSQL): my_uint; stdcall;
procedure mysql_free_result(res: MYSQL_RES); stdcall;
function mysql_get_client_info: my_char; stdcall;
function mysql_get_client_version: my_uint; stdcall;
function mysql_get_host_info(mysql: MYSQL): my_char; stdcall;
function mysql_get_proto_info(mysql: MYSQL): my_uint; stdcall;
function mysql_get_server_info(mysql: MYSQL): my_char; stdcall;
function mysql_get_server_status(mysql: MYSQL): my_uint; stdcall;
function mysql_get_server_version(mysql: MYSQL): my_uint; stdcall;
function mysql_info(mysql: MYSQL): my_char; stdcall;
function mysql_init(mysql: MYSQL): MYSQL; stdcall;
function mysql_insert_id(mysql: MYSQL): my_ulonglong; stdcall;
function mysql_kill(mysql: MYSQL; pid: my_uint): my_int; stdcall;
// function mysql_list_dbs(mysql: MYSQL; const wild: my_char): MYSQL_RES; stdcall;
// function mysql_list_fields(mysql: MYSQL; const table, wild: my_char): MYSQL_RES; stdcall;
// function mysql_list_processes(mysql: MYSQL): MYSQL_RES; stdcall;
// function mysql_list_tables(mysql: MYSQL; const wild: my_char): MYSQL_RES; stdcall;
function mysql_num_fields(res: MYSQL_RES): my_uint; stdcall;
function mysql_num_rows(res: MYSQL_RES): my_ulonglong; stdcall;
function mysql_options(mysql: MYSQL; option: enum_mysql_option; const arg: my_char): my_int; stdcall;
function mysql_ping(mysql: MYSQL): my_int; stdcall;
function mysql_query(mysql: MYSQL; const q: my_char): my_int; stdcall;
function mysql_real_connect(mysql: MYSQL; host, user, passwd, db: my_char; port: my_uint; unix_socket: my_char; client_flag: my_uint): MYSQL; stdcall;
function mysql_real_escape_string(mysql: MYSQL; _to: my_char; const from: my_char; length: my_uint): my_uint; stdcall;
function mysql_real_query(mysql: MYSQL; query: my_char; length: my_int): my_int; stdcall;
// function mysql_reload(mysql: MYSQL): my_int; stdcall;
// function mysql_row_seek(res: MYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; stdcall;
// function mysql_row_tell(res: MYSQL_RES): MYSQL_ROW_OFFSET; stdcall;
function mysql_select_db(mysql: MYSQL; const db: my_char): my_int; stdcall;
function mysql_set_character_set(mysql: MYSQL; const csname: my_char): my_int; stdcall;
function mysql_set_server_option(mysql: MYSQL; option: enum_mysql_set_option): my_int; stdcall;
function mysql_shutdown(mysql: MYSQL; shutdown_level: mysql_enum_shutdown_level): my_int; stdcall;
function mysql_sqlstate(mysql: MYSQL): my_char; stdcall;
// function mysql_ssl_set(mysql: MYSQL; key, cert, ca, capath, cipher: my_char): my_int; stdcall;
// function mysql_stat(mysql: MYSQL): my_char; stdcall;
function mysql_store_result(mysql: MYSQL): MYSQL_RES; stdcall;
function mysql_thread_id(mysql: MYSQL): my_uint; stdcall;
function mysql_use_result(mysql: MYSQL): MYSQL_RES; stdcall;
function mysql_warning_count(mysql: MYSQL): my_uint; stdcall;
// function mysql_commit(mysql: MYSQL): my_bool; stdcall;
// function mysql_rollback(mysql: MYSQL): my_bool; stdcall;
// function mysql_autocommit(mysql: MYSQL; mode: my_bool): my_bool; stdcall;
function mysql_more_results(mysql: MYSQL): my_bool; stdcall;
function mysql_next_result(mysql: MYSQL): my_int; stdcall;
procedure mysql_set_local_infile_default(mysql: MYSQL); cdecl;
procedure mysql_set_local_infile_handler(mysql: MYSQL; local_infile_init: Tlocal_infile_init; local_infile_read: Tlocal_infile_read; local_infile_end: Tlocal_infile_end; local_infile_error: Tlocal_infile_error; userdata: Pointer); cdecl;

const
  PACKET_CURRENT = 4;

implementation {***************************************************************}

uses
  Windows,
  ZLib, StrUtils;

const
  MYSQL_CLIENT_INFO    = '4.1.1';
  MYSQL_CLIENT_VERSION = 40101;
  CLIENT_CAPABILITIES  = CLIENT_LONG_PASSWORD or
                         CLIENT_LONG_FLAG or
                         CLIENT_LOCAL_FILES or
                         CLIENT_PROTOCOL_41 or
                         CLIENT_TRANSACTIONS or
                         CLIENT_SECURE_CONNECTION;

  AF_INET6 = 23;

  ctSHA1HashSize = 20;
  ctSHAKeys: array[0..4] of Longint =
    (Longint($67452301),
     Longint($EFCDAB89),
     Longint($98BADCFE),
     Longint($10325476),
     Longint($C3D2E1F0));

type
  TWSAConnectByNameA = function(
    s: TSocket;
    nodename: LPSTR;
    servicename: LPSTR;
    LocalAddressLength: LPDWORD;
    LocalAddress: PSockAddrIn;
    RemoteAddressLength: LPDWORD;
    RemoteAddress: PSockAddrIn;
    timeout: PTimeVal;
    Reserved: POverlapped): Boolean; stdcall;

  TSHA1Context = record
    FLength: int64;
    FInterimHash: array[0..4] of Longint;
    FComputed: boolean;
    FCorrupted: boolean;
    FMsgBlockIndex: byte;
    FMsgBlock: array[0..63] of Byte
  end;

var
  WSAData: WinSock.WSADATA;
  WS2_32: THandle;
  WSAConnectByNameA: TWSAConnectByNameA;

{$Q-}

procedure sha1_ProcessMessageBlock(var context: TSHA1Context);
const
  ctKeys: array[0..3] of Longint =
    (Longint($5A827999), Longint($6ED9EBA1), Longint($8F1BBCDC), Longint($CA62C1D6));
var
  a: Longint;
  b: Longint;
  c: Longint;
  d: Longint;
  e: Longint;
  i: Integer;
  j: Integer;
  temp: Longint;
  w: array [0..79] of Longint;
begin
  for i := 0 to 15 do
    begin
      j := i*4;
      W[i] := context.FMsgBlock[j] shl 24;
      W[i] := W[i] or context.FMsgBlock[j+1] shl 16;
      W[i] := W[i] or context.FMsgBlock[j+2] shl 8;
      W[i] := W[i] or context.FMsgBlock[j+3];
    end;
  for i := 16 to 79 do
    begin
      W[i] := W[i-3] xor W[i-8] xor W[i-14] xor W[i-16];
      W[i] := (W[i] shl 1) or (W[i] shr 31);
    end;
  A := context.FInterimHash[0];
  B := context.FInterimHash[1];
  C := context.FInterimHash[2];
  D := context.FInterimHash[3];
  E := context.FInterimHash[4];
  for i := 0 to 19 do
    begin
      temp := ((A shl 5) or (A shr 27))+((B and C)or((not B)and D))+E+W[i]+ctKeys[0];
      E := D;
      D := C;
      C := (B shl 30) or (B shr 2);
      B := A;
      A := temp;
    end;
  for i := 20 to 39 do
    begin
      temp := ((A shl 5) or (A shr 27))+(B xor C xor D)+E+W[i]+ctKeys[1];
      E := D;
      D := C;
      C := (B shl 30) or (B shr 2);
      B := A;
      A := temp;
    end;
  for i := 40 to 59 do
    begin
      temp := ((A shl 5) or (A shr 27))+((B and C)or(B and D)or(C and D))+E+W[i]+ctKeys[2];
      E := D;
      D := C;
      C := (B shl 30) or (B shr 2);
      B := A;
      A := temp;
    end;
  for i := 60 to 79 do
    begin
      temp := ((A shl 5) or (A shr 27))+(B xor C xor D)+E+W[i]+ctKeys[3];
      E := D;
      D := C;
      C := (B shl 30) or (B shr 2);
      B := A;
      A := temp;
    end;
  context.FInterimHash[0] := context.FInterimHash[0]+A;
  context.FInterimHash[1] := context.FInterimHash[1]+B;
  context.FInterimHash[2] := context.FInterimHash[2]+C;
  context.FInterimHash[3] := context.FInterimHash[3]+D;
  context.FInterimHash[4] := context.FInterimHash[4]+E;
  context.FMsgBlockIndex := 0;
end;

procedure sha1_reset(var context: TSHA1Context);
begin
  context.FLength := 0;
  context.FMsgBlockIndex := 0;
  context.FInterimHash[0] := ctSHAKeys[0];
  context.FInterimHash[1] := ctSHAKeys[1];
  context.FInterimHash[2] := ctSHAKeys[2];
  context.FInterimHash[3] := ctSHAKeys[3];
  context.FInterimHash[4] := ctSHAKeys[4];
  context.FComputed := false;
  context.FCorrupted := false;
  FillChar(context.FMsgBlock[0], 64, #0);
end;

procedure sha1_input(var context: TSHA1Context; msgArray :PAnsiChar; msgLen:cardinal);
begin
  assert(assigned(msgArray), 'Empty array paased to sha1Input');
  if context.FComputed then
    context.FCorrupted := true;
  if not context.FCorrupted then
    while msgLen>0 do
      begin
        context.FMsgBlock[context.FMsgBlockIndex] := byte(msgArray[0]);
        inc(context.FMsgBlockIndex);
        context.FLength := context.FLength+8;
        if context.FMsgBlockIndex=64 then
          sha1_ProcessMessageBlock(context);
        dec(msgLen);
        inc(msgArray);
      end;
end;

procedure sha1_result(var context: TSHA1Context; msgDigest: PAnsiChar);
var
  i: Integer;
begin
  assert(assigned(msgDigest), 'Empty array passed to sha1Result');
  if not context.FCorrupted then
    begin
      if not context.FComputed then
        begin
          i := context.FMsgBlockIndex;
          if i>55 then
            begin
              context.FMsgBlock[i] := $80;
              inc(i);
              FillChar(context.FMsgBlock[i], (64-i), #0);
              context.FMsgBlockIndex := 64;
              sha1_ProcessMessageBlock(context);
              FillChar(context.FMsgBlock[0], 56, #0);
              context.FMsgBlockIndex := 56;
            end
          else
            begin
              context.FMsgBlock[i] := $80;
              inc(i);
              FillChar(context.FMsgBlock[i], (56-i), #0);
              context.FMsgBlockIndex := 56;
            end;
          context.FMsgBlock[56] := (context.FLength shr 56) and $FF;
          context.FMsgBlock[57] := (context.FLength shr 48) and $FF;
          context.FMsgBlock[58] := (context.FLength shr 40) and $FF;
          context.FMsgBlock[59] := (context.FLength shr 32) and $FF;
          context.FMsgBlock[60] := (context.FLength shr 24) and $FF;
          context.FMsgBlock[61] := (context.FLength shr 16) and $FF;
          context.FMsgBlock[62] := (context.FLength shr  8) and $FF;
          context.FMsgBlock[63] := (context.FLength       ) and $FF;

          sha1_ProcessMessageBlock(context);

          FillChar(context.FMsgBlock, SizeOf(context.FMsgBlock), #0);
          context.FLength := 0;
          context.FComputed := True;
        end;
      for i := 0 to ctSHA1HashSize -1 do
        msgDigest[i] := AnsiChar(context.FInterimHash[i shr 2] shr (8 * (3 - (i and 3))) and $FF);
    end;
end;

function Scramble(const Password: my_char; const Salt: my_char): RawByteString;

  {$Q-}
  procedure hashPassword(const pass: my_char; var res0, res1: my_int);
  var
    nr, add, nr2, tmp: my_ulonglong;
    I: my_int;
    e1: my_ulonglong;
    len: my_int;
  begin
    nr := 1345345333;
    add := 7;
    nr2 := $12345671;
    len := length(pass)-1;
    for I := 0 to len do
    begin
      if (Pass[I] = #20) or (Pass[I] = #9)then
        continue;
      tmp := $ff and Byte(Pass[I]);
      e1 := (((nr and 63) +add)*tmp)+(nr shl 8);
      nr := nr xor e1;
      nr2 := nr2+((nr2 shl 8) xor nr);
      add := add+tmp;
    end;
    res0 := nr and $7fffffff;
    res1 := nr2 and $7fffffff;
  end;
  {$Q+}

  function Floor(X: Extended): my_int;
  begin
    Result := Trunc(X);
    if ((X < 0) and (Result <> X)) then
      Dec(Result);
  end;

var
  dRes: Double;
  e: Byte;
  hm0: my_int;
  hm1: my_int;
  hp0: my_int;
  hp1: my_int;
  I: my_int;
  maxValue: my_ulonglong;
  Scramled: array [0..7] of AnsiChar;
  Seed: my_ulonglong;
  Seed2: my_ulonglong;
begin
  hashPassword(Password, hp0, hp1);
  hashPassword(Salt, hm0, hm1);
  MaxValue := $3FFFFFFF;
  Seed  := (hp0 xor hm0) mod maxValue ;
  Seed2 := (hp1 xor hm1) mod maxValue ;
  for I := 0 to StrLen(Salt) - 1 do
  begin
    Seed  := (Seed * 3 + Seed2) mod MaxValue;
    Seed2 := (Seed + Seed2 + 33) mod MaxValue;
    dRes := Seed / maxValue;
    Scramled[I] := AnsiChar(Floor(dRes * 31) + 64);
  end;
  dRes := (Seed * 3 + Seed2) mod MaxValue / MaxValue;
  e := Floor(dRes * 31);
  for I := 0 to StrLen(Salt) - 1 do
    Scramled[I] := AnsiChar(Byte(Scramled[I]) xor e);

  SetString(Result, PAnsiChar(@Scramled), StrLen(Salt));
end;

function SecureScramble(const Password: my_char; const Salt: my_char): RawByteString;
var
  hash_stage1: array [0 .. ctSHA1HashSize - 1] of AnsiChar;
  hash_stage2: array [0 .. ctSHA1HashSize - 1] of AnsiChar;
  I: my_int;
  Scramled: array [0 .. ctSHA1HashSize - 1] of AnsiChar;
  sha1_context: TSHA1Context;
begin
  sha1_reset(sha1_context);
  //* stage 1: hash Password */
  sha1_input(sha1_context, Password, StrLen(Password));
  sha1_result(sha1_context, @hash_stage1[0]);
  //* stage 2: hash stage 1; note that hash_stage2 is stored in the database */
  sha1_reset(sha1_context);
  sha1_input(sha1_context, @hash_stage1[0], ctSHA1HashSize);
  sha1_result(sha1_context, @hash_stage2[0]);
  //* create crypt AnsiString as sha1(message, hash_stage2) */;
  sha1_reset(sha1_context);
  sha1_input(sha1_context, PAnsiChar(Salt), ctSHA1HashSize);
  sha1_input(sha1_context, @hash_stage2[0], ctSHA1HashSize);
  //* xor allows 'from' and 'to' overlap: lets take advantage of it */
  sha1_result(sha1_context, @scramled);

  for I := 0 to ctSHA1HashSize - 1 do
    Scramled[I] := AnsiChar(Byte(Scramled[I]) xor Byte(hash_stage1[I]));

  SetString(Result, PAnsiChar(@Scramled), ctSHA1HashSize);
end;

{ C API functions *************************************************************}

function mysql_affected_rows(mysql: MYSQL): my_ulonglong; stdcall;
begin
  Result := mysql.affected_rows();
end;

// function mysql_change_user(mysql: MYSQL; const user, passwd, db: my_char): my_bool; stdcall;

function mysql_character_set_name(mysql: MYSQL): my_char; stdcall;
begin
  Result := mysql.character_set_name();
end;

procedure mysql_close(mysql: MYSQL); stdcall;
begin
  mysql.Free();
end;

// function mysql_connect(mysql: MYSQL; const host, user, passwd: my_char): MYSQL; stdcall;
// function mysql_create_db(mysql: MYSQL; const DB: my_char): my_int; stdcall;

procedure mysql_data_seek(res: MYSQL_RES; offset: my_ulonglong); stdcall;
begin
  res.data_seek(offset);
end;

// procedure mysql_debug(const debug: my_char); stdcall;
// function mysql_drop_db(mysql: MYSQL; const DB: my_char): my_int; stdcall;
// function mysql_dump_debug_info(mysql: MYSQL): my_int; stdcall;

function mysql_eof(mysql: MYSQL): my_bool; stdcall;
begin
  Result := mysql.eof();
end;

function mysql_errno(mysql: MYSQL): my_uint; stdcall;
begin
  Result := mysql.errno();
end;

function mysql_error(mysql: MYSQL): my_char; stdcall;
begin
  Result := my_char(RawByteString(mysql.error()));
end;

// function mysql_escape_string(_to: my_char; const from: my_char; from_length: my_uint): my_uint; stdcall;

function mysql_fetch_field(res: MYSQL_RES): MYSQL_FIELD; stdcall;
begin
  Result := res.fetch_field();
end;

function mysql_fetch_fields(res: MYSQL_RES): MYSQL_FIELDS; stdcall;
begin
  Result := res.fetch_fields();
end;

function mysql_fetch_field_direct(res: MYSQL_RES; fieldnr: my_uint): MYSQL_FIELD; stdcall;
begin
  Result := res.fetch_field_direct(fieldnr);
end;

function mysql_fetch_lengths(res: MYSQL_RES): MYSQL_LENGTHS; stdcall;
begin
  Result := res.fetch_lengths();
end;

function mysql_fetch_row(res: MYSQL_RES): MYSQL_ROW; stdcall;
begin
  Result := res.fetch_row();
end;

function mysql_field_count(mysql: MYSQL): my_uint; stdcall;
begin
  Result := mysql.res.FieldCount;
end;

procedure mysql_free_result(res: MYSQL_RES); stdcall;
begin
  res.Free();
end;

function mysql_get_client_info(): my_char; stdcall;
begin
  Result := MYSQL_CLIENT_INFO;
end;

function mysql_get_client_version: my_uint;
begin
  Result := MYSQL_CLIENT_VERSION;
end;

function mysql_get_host_info(mysql: MYSQL): my_char; stdcall;
begin
  Result := mysql.get_host_info();
end;

function mysql_get_proto_info(mysql: MYSQL): my_uint; stdcall;
begin
  Result := PROTOCOL_VERSION;
end;

function mysql_get_server_info(mysql: MYSQL): my_char; stdcall;
begin
  Result := mysql.get_server_info();
end;

function mysql_get_server_status(mysql: MYSQL): my_uint; stdcall;
begin
  Result := mysql.get_server_status();
end;

function mysql_get_server_version(mysql: MYSQL): my_uint; stdcall;
begin
  Result := mysql.get_server_version();
end;

function mysql_info(mysql: MYSQL): my_char; stdcall;
begin
  Result := mysql.info();
end;

function mysql_init(mysql: MYSQL): MYSQL; stdcall;
begin
  if (Assigned(mysql)) then
    Result := mysql
  else
    Result := MySQLClient.MYSQL.Create();
end;

function mysql_insert_id(mysql: MYSQL): my_ulonglong; stdcall;
begin
  Result := mysql.insert_id();
end;

function mysql_kill(mysql: MYSQL; pid: my_uint): my_int; stdcall;
begin
  Result := mysql.kill(pid);
end;

// function mysql_list_dbs(mysql: MYSQL; const wild: my_char): MYSQL_RES; stdcall;
// function mysql_list_fields(mysql: MYSQL; const table, wild: my_char): MYSQL_RES; stdcall;
// function mysql_list_processes(mysql: MYSQL): MYSQL_RES; stdcall;
// function mysql_list_tables(mysql: MYSQL; const wild: my_char): MYSQL_RES; stdcall;

function mysql_num_fields(res: MYSQL_RES): my_uint; stdcall;
begin
  Result := res.num_fields();
end;

function mysql_num_rows(res: MYSQL_RES): my_ulonglong; stdcall;
begin
  Result := res.num_rows();
end;

function mysql_options(mysql: MYSQL; option: enum_mysql_option; const arg: my_char): my_int; stdcall;
begin
  Result := mysql.options(option, arg);
end;

function mysql_ping(mysql: MYSQL): my_int; stdcall;
begin
  Result := mysql.ping();
end;

function mysql_query(mysql: MYSQL; const q: my_char): my_int; stdcall;
begin
  Result := mysql_real_query(mysql, q, StrLen(q));
end;

function mysql_real_connect(mysql: MYSQL; host, user, passwd, db: my_char; port: my_uint; unix_socket: my_char; client_flag: my_uint): MYSQL; stdcall;
begin
  Result := mysql.real_connect(host, user, passwd, db, port, unix_socket, client_flag);
end;

function mysql_real_escape_string(mysql: MYSQL; _to: my_char; const from: my_char; length: my_uint): my_uint; stdcall;
begin
  Result := mysql.real_escape_string(_to, from, length);
end;

function mysql_real_query(mysql: MYSQL; query: my_char; length: my_int): my_int; stdcall;
begin
  Result := mysql.real_query(query, length);
end;

// function mysql_reload(mysql: MYSQL): my_int; stdcall;
// function mysql_row_seek(res: MYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; stdcall;
// function mysql_row_tell(res: MYSQL_RES): MYSQL_ROW_OFFSET; stdcall;

function mysql_select_db(mysql: MYSQL; const db: my_char): my_int; stdcall;
begin
  Result := mysql.select_db(db);
end;

function mysql_set_character_set(mysql: MYSQL; const csname: my_char): my_int; stdcall;
begin
  Result := mysql.set_character_set(csname);
end;

function mysql_set_server_option(mysql: MYSQL; option: enum_mysql_set_option): my_int; stdcall;
begin
  Result := mysql.set_server_option(MySQLConsts.enum_mysql_set_option(option));
end;

function mysql_shutdown(mysql: MYSQL; shutdown_level: mysql_enum_shutdown_level): my_int; stdcall;
begin
  Result := mysql.shutdown(shutdown_level);
end;

function mysql_sqlstate(mysql: MYSQL): my_char; stdcall;
begin
  Result := mysql.sqlstate();
end;

// function mysql_ssl_set(mysql: MYSQL; key, cert, ca, capath, cipher: my_char): my_int; stdcall;
// function mysql_stat(mysql: MYSQL): my_char; stdcall;

function mysql_store_result(mysql: MYSQL): MYSQL_RES; stdcall;
begin
  Result := mysql.store_result();
end;

function mysql_thread_id(mysql: MYSQL): my_uint; stdcall;
begin
  Result := mysql.thread_id();
end;

function mysql_use_result(mysql: MYSQL): MYSQL_RES; stdcall;
begin
  Result := mysql.use_result();
end;

function mysql_warning_count(mysql: MYSQL): my_uint; stdcall;
begin
  Result := mysql.warning_count();
end;

// function mysql_commit(mysql: MYSQL): my_bool; stdcall;
// function mysql_rollback(mysql: MYSQL): my_bool; stdcall;
// function mysql_autocommit(mysql: MYSQL; mode: my_bool): my_bool; stdcall;

function mysql_more_results(mysql: MYSQL): my_bool; stdcall;
begin
  Result := mysql.more_results();
end;

function mysql_next_result(mysql: MYSQL): my_int; stdcall;
begin
  Result := mysql.next_result();
end;

procedure mysql_set_local_infile_default(mysql: MYSQL); cdecl;
begin
  mysql.set_local_infile_default();
end;

procedure mysql_set_local_infile_handler(mysql: MYSQL; local_infile_init: Tlocal_infile_init; local_infile_read: Tlocal_infile_read; local_infile_end: Tlocal_infile_end; local_infile_error: Tlocal_infile_error; userdata: Pointer); cdecl;
begin
  mysql.set_local_infile_handler(local_infile_init, local_infile_read, local_infile_end, local_infile_error, userdata);
end;

{ TMySQL_IO *******************************************************************}

procedure TMySQL_IO.Close();
begin
  case (IOType) of
    itNamedPipe:
      begin
        CloseHandle(Pipe); Pipe := INVALID_HANDLE_VALUE;
      end;
    itTCPIP:
      begin
        shutdown(Socket, SD_BOTH);
        closesocket(Socket); Socket := INVALID_SOCKET;
      end;
  end;

  IOType := itNone;
end;

constructor TMySQL_IO.Create();
begin
  FError := '';
  FErrno := 0;
  FDirection := idRead;
  Pipe := INVALID_HANDLE_VALUE;
  Socket := INVALID_SOCKET;
  IOType := itNone;
end;

destructor TMySQL_IO.Destroy();
begin
  if (IOType <> itNone) then
    Close();

  inherited;
end;

function TMySQL_IO.errno(): my_uint;
begin
  Result := FErrNo;
end;

function TMySQL_IO.error(): RawByteString;
begin
  Result := FError;
end;

function TMySQL_IO.GetCodePage(): Cardinal;
begin
  Result := 1252; // Base on the default character set "latin1"
end;

function TMySQL_IO.DecodeString(const Str: RawByteString): string;
var
  Len: Integer;
begin
  if (Str = '') then
    Result := ''
  else
  begin
    Len := MultiByteToWideChar(CodePage, 0, PAnsiChar(Str), Length(Str), nil, 0);
    SetLength(Result, Len);
    MultiByteToWideChar(CodePage, 0, PAnsiChar(Str), Length(Str), PChar(@Result[1]), Len);
  end;
end;

function TMySQL_IO.EncodeString(const Str: string): RawByteString;
var
  Len: Integer;
begin
  if (Str = '') then
    Result := ''
  else
  begin
    Len := WideCharToMultiByte(CodePage, 0, PChar(Str), Length(Str), nil, 0, nil, nil);
    SetLength(Result, Len);
    WideCharToMultiByte(CodePage, 0, PChar(Str), Length(Str), PAnsiChar(@Result[1]), Len, nil, nil);
  end;
end;

function TMySQL_IO.Open(const AType: TMYSQL_IO.TType;
  const Host, PipeName: RawByteString; const Port, Timeout: my_uint): Boolean;
var
  Filename: string;
  HostEnt: PHostEnt;
  ip_addr: u_long;
  KeepAlive: BOOL;
  Mode: ULONG;
  RcvBuf: u_int;
  ReadFDS: TFDSet;
  sock_addr: sockaddr_in;
  Time: timeval;
begin
  Seterror(0);

  case (AType) of
    itNamedPipe:
      begin
        if (Host = LOCAL_HOST) then
          Filename := DecodeString('\\' + LOCAL_HOST_NAMEDPIPE + '\pipe\' + PipeName)
        else
          Filename := DecodeString('\\' + Host + '\pipe\' + PipeName);
        if (not WaitNamedPipe(PChar(Filename), Timeout * 1000)) then
          if (GetLastError() = 2) then
            Seterror(CR_NAMEDPIPEOPEN_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_NAMEDPIPEOPEN_ERROR - CR_MIN_ERROR], [LOCAL_HOST, PipeName, GetLastError()])))
          else
            Seterror(CR_NAMEDPIPEWAIT_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_NAMEDPIPEWAIT_ERROR - CR_MIN_ERROR], [LOCAL_HOST, PipeName, GetLastError()])))
        else
        begin
          Pipe := CreateFile(PChar(Filename), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
          if (Pipe = INVALID_HANDLE_VALUE) then
            if (GetLastError() = ERROR_PIPE_BUSY) then
              Seterror(CR_NAMEDPIPEWAIT_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_NAMEDPIPEWAIT_ERROR - CR_MIN_ERROR], [LOCAL_HOST, PipeName, GetLastError()])))
            else
              Seterror(CR_NAMEDPIPEOPEN_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_NAMEDPIPEOPEN_ERROR - CR_MIN_ERROR], [LOCAL_HOST, PipeName, GetLastError()])))
          else
          begin
            Mode := PIPE_READMODE_BYTE or PIPE_WAIT;
            if (not SetNamedPipeHandleState(Pipe, Mode, nil, nil)) then
            begin
              CloseHandle(Pipe); Pipe := INVALID_HANDLE_VALUE;

              Seterror(CR_NAMEDPIPESETSTATE_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_NAMEDPIPESETSTATE_ERROR - CR_MIN_ERROR], [Host, PipeName, GetLastError()])));
            end
            else
              IOType := itNamedPipe;
          end;
        end;
      end;
    itTCPIP:
      if ((WSAData.wVersion = 0) and (WSAStartup($0101, WSAData) <> 0)) then
        Seterror(CR_UNKNOWN_ERROR)
      else
      begin
        ip_addr := inet_addr(PAnsiChar(Host));
        if (ip_addr = u_long(INADDR_NONE)) then
        begin
          HostEnt := gethostbyname(PAnsiChar(Host));
          if (not Assigned(HostEnt)) then
            ip_addr := u_long(INADDR_NONE)
          else
            Move(HostEnt^.h_addr^[0], ip_addr, SizeOf(ip_addr));
        end;

        if (ip_addr = u_long(INADDR_NONE)) then
          Seterror(CR_UNKNOWN_HOST, EncodeString(Format(CLIENT_ERRORS[CR_UNKNOWN_HOST - CR_MIN_ERROR], [Host, WSAGetLastError()])))
        else
        begin
          FillChar(sock_addr, SizeOf(sock_addr), #0);
          sock_addr.sin_family := AF_INET;
          sock_addr.sin_addr := in_addr(ip_addr);
          sock_addr.sin_port := htons(Port);

          Socket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
          if (Socket = INVALID_SOCKET) then
            Seterror(CR_IPSOCK_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_IPSOCK_ERROR - CR_MIN_ERROR], [WSAGetLastError()])))
          else if (connect(Socket, sock_addr, SizeOf(sock_addr)) = SOCKET_ERROR) then
          begin
            Seterror(CR_CONN_HOST_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_CONN_HOST_ERROR - CR_MIN_ERROR], [Host, WSAGetLastError()])));

            closesocket(Socket); Socket := INVALID_SOCKET;
          end;
        end;

        if ((Socket = INVALID_SOCKET) and (Win32MajorVersion >= 6)) then
        begin
          WS2_32 := LoadLibrary('WS2_32.DLL');
          if (WS2_32 <> 0) then
          begin
            WSAConnectByNameA := GetProcAddress(WS2_32, 'WSAConnectByNameA');

            if (Assigned(WSAConnectByNameA)) then
            begin
              Socket := WinSock.socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP);
              if (Socket = INVALID_SOCKET) then
                Seterror(CR_IPSOCK_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_IPSOCK_ERROR - CR_MIN_ERROR], [WSAGetLastError()])))
              else
              begin
                Time.tv_sec := Timeout; Time.tv_usec := Time.tv_sec * 1000;
                if (not WSAConnectByNameA(Socket, PAnsiChar(Host), PAnsiChar(RawByteString(IntToStr(Port))), nil, nil, nil, nil, @Time, nil)) then
                begin
                  closesocket(Socket); Socket := INVALID_SOCKET;
                end
                else
                  Seterror(0);
              end;
            end;
          end;
        end;

        if (Socket <> INVALID_SOCKET) then
        begin
          KeepAlive := TRUE; // Sends keep-alives.
          setsockopt(Socket, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@KeepAlive), SizeOf(KeepAlive));

          RcvBuf := 2 * NET_BUFFER_LENGTH;
          setsockopt(Socket, SOL_SOCKET, SO_RCVBUF, PAnsiChar(@RcvBuf), SizeOf(RcvBuf));

          Direction := idRead;

          FD_ZERO(ReadFDS); FD_SET(Socket, ReadFDS);
          Time.tv_sec := Timeout; Time.tv_usec := Time.tv_sec * 1000;
          if (select(0, @ReadFDS, nil, nil, @Time) <> 1) then
          begin
            Seterror(CR_CONN_HOST_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_CONN_HOST_ERROR - CR_MIN_ERROR], [Host, WSAGetLastError()])));

            shutdown(Socket, SD_BOTH);
            closesocket(Socket); Socket := INVALID_SOCKET;
          end
          else
            IOType := itTCPIP;
        end;
      end;
    else
      Seterror(CR_UNKNOWN_ERROR);
  end;

  Result := errno() = 0;
end;

function TMySQL_IO.Receive(var Buffer; const BytesToRead: my_uint; out BytesRead: my_uint): Boolean;
var
  arg: u_long;
  Len: my_int;
  ReadFDS: TFDSet;
  Size: Cardinal;
  Time: timeval;
begin
  BytesRead := 0;
  repeat
    case (IOType) of
      itNamedPipe:
        begin
          Result := ReadFile(Pipe, PAnsiChar(@AnsiChar(Buffer))[BytesRead], BytesToRead - BytesRead, Size, nil);
          if (not Result) then
            Len := -1
          else
            Len := Size;
       end;
      itTCPIP:
        begin
          if (FErrNo = 0) then
            Size := BytesToRead
          else if (ioctlsocket(Socket, FIONREAD, arg) <> SOCKET_ERROR) then
            Size := arg
          else
            Size := 0;

          Result := Size > 0;
          if (not Result) then
            Len := -1
          else
          begin
            FD_ZERO(ReadFDS); FD_SET(Socket, ReadFDS);
            Time.tv_sec := NET_WAIT_TIMEOUT; Time.tv_usec := Time.tv_sec * 1000;
            Result := select(0, @ReadFDS, nil, nil, @Time) > 0;
            if (not Result) then
              Len := -1
            else
            begin
              Len := recv(Socket, PAnsiChar(@AnsiChar(Buffer))[BytesRead], BytesToRead - BytesRead, 0);
              Result := Len <> SOCKET_ERROR;
            end;
          end;
        end;
      else
        begin
          Len := -1;
          Result := False;
        end;
    end;

    if (Result) then
      Inc(BytesRead, Len);
  until (not Result or (Len = 0) or (BytesRead = BytesToRead));

  if (not Result) then
    Seterror(CR_SERVER_LOST);
end;

function TMySQL_IO.Send(const Buffer; const BytesToWrite: my_uint): Boolean;
var
  BytesWritten: my_uint;
  Len: my_int;
  Size: DWORD;
  Time: timeval;
  WriteFDS: TFDSet;
begin
  case (IOType) of
    itNamedPipe:
    begin
      BytesWritten := 0;
      repeat
        Result := WriteFile(Pipe, PAnsiChar(@AnsiChar(Buffer))[BytesWritten], BytesToWrite - BytesWritten, Size, nil);
        if (Result) then
          Inc(BytesWritten, Size);
      until (not Result or (BytesWritten = BytesToWrite));
    end;
    itTCPIP:
    begin
      FD_ZERO(WriteFDS); FD_SET(Socket, WriteFDS);
      Time.tv_sec := NET_WRITE_TIMEOUT; Time.tv_usec := Time.tv_sec * 1000;
      Result := (select(0, nil, @WriteFDS, nil, @Time) >= 1);
      if (Result) then
      begin
        Len := WinSock.send(Socket, Pointer(@Buffer)^, BytesToWrite, 0);
        Result := (Len >= SOCKET_ERROR) and (my_uint(Len) = BytesToWrite);
      end;
    end;
    else
      Result := False;
  end;

  if (not Result) then
    Seterror(CR_SERVER_GONE_ERROR);
end;

procedure TMySQL_IO.Seterror(const AErrNo: my_uint; const AError: RawByteString = '');
begin
  FErrNo := AErrNo;

  if (AError <> '') then
    FError := AError
  else if ((CR_MIN_ERROR <= FErrNo) and (FErrNo <= CR_MIN_ERROR + Length(CLIENT_ERRORS))) then
    FError := EncodeString(CLIENT_ERRORS[FErrNo - CR_MIN_ERROR])
  else
    FError := '';
end;

procedure TMySQL_IO.SetFileAccess(ADirection: TMySQL_IO.TDirection);
var
  arg: Integer;
begin
  if (ADirection <> FDirection) then
  begin
    case (IOType) of
      itTCPIP:
        begin
          if (ADirection = idRead) then
            arg := 0 // disable Nonblocking mode
          else
            arg := not 0; // enable Nonblocking mode
          ioctlsocket(Socket, FIONBIO, arg);
        end;
    end;

    FDirection := ADirection;
  end;
end;

{ TMySQL_File *******************************************************************}

procedure TMySQL_File.CloseFile();
var
  C: AnsiChar;
begin
  if ((IOType <> itNone) and (errno() <> CR_SERVER_GONE_ERROR) and (errno() <> CR_SERVER_LOST)) then
  begin
    next_command();
    C := AnsiChar(COM_QUIT); // inform the server we're gone
    WriteFile(@C, 1);
    FlushFileBuffers();
  end;

  Close();

  ReallocBuffer(PacketBuffer, 0);
  FillChar(FReadFileBuffer, SizeOf(FReadFileBuffer), #0);
end;

constructor TMySQL_File.Create();
begin
  inherited;

  FillChar(PacketBuffer, SizeOf(PacketBuffer), #0);
  FillChar(FReadFileBuffer, SizeOf(FReadFileBuffer), #0);
end;

function TMySQL_File.CreateFile(const AIOType: TMYSQL_IO.TType;
  const Host, UnixSocket: RawByteString; const Port, Timeout: my_uint): Boolean;
begin
  Result := IOType = itNone;
  if (Result) then
  begin
    ReallocBuffer(PacketBuffer, NET_BUFFER_LENGTH);

    FCompress := False;

    PacketNr := 0;
    CompPacketNr := 0;

    Result := Open(AIOType, Host, UnixSocket, Port, Timeout);
  end;
end;

function TMySQL_File.FlushFileBuffers(): Boolean;
var
  CompressBuffer: Pointer;
  CompressedSize: Integer;
  Offset: my_uint;
  Size: my_uint;
begin
  Result := (errno() <> CR_SERVER_GONE_ERROR) and (errno() <> CR_SERVER_LOST);
  if (Result and Assigned(PacketBuffer.Mem)) then
  begin
    SetFilePointer(1, PACKET_CURRENT);
    Dec(PacketBuffer.Size, NET_HEADER_SIZE);

    if (not Compress) then
      Result := Send(PacketBuffer.Mem[0], PacketBuffer.Size)
    else
    begin
      Result := True;
      Offset := 0;

      repeat
        Size := PacketBuffer.Size - Offset;
        if (Size > MAX_PACKET_LENGTH) then
          Size := MAX_PACKET_LENGTH;

        CompressBuffer := nil;
        CompressedSize := Size;
        if (Size >= MIN_COMPRESS_LENGTH) then
          try
            ZCompress(@PacketBuffer.Mem[Offset], Size, CompressBuffer, CompressedSize);
          except
            on E: EOutOfMemory do
              begin Seterror(CR_OUT_OF_MEMORY); Result := False; end;
            else
              begin Seterror(CR_UNKNOWN_ERROR); Result := False; end;
          end;

        if (Result) then
          if (my_uint(CompressedSize) >= Size) then
          begin
            if (Offset + NET_HEADER_SIZE + COMP_HEADER_SIZE + Size > PacketBuffer.TotalSize) then
              ReallocBuffer(PacketBuffer, Offset + NET_HEADER_SIZE + COMP_HEADER_SIZE + Size);

            Move(PacketBuffer.Mem[Offset], PacketBuffer.Mem[Offset + NET_HEADER_SIZE + COMP_HEADER_SIZE], Size);
            Inc(PacketBuffer.Size, NET_HEADER_SIZE + COMP_HEADER_SIZE);

            Move(Size, PacketBuffer.Mem[Offset + 0], 3);
            Move(CompPacketNr, PacketBuffer.Mem[Offset + 3], 1);
            FillChar(PacketBuffer.Mem[Offset + NET_HEADER_SIZE], COMP_HEADER_SIZE, #0);

            Result := Send(PacketBuffer.Mem[Offset], NET_HEADER_SIZE + COMP_HEADER_SIZE + Size);

            Inc(Offset, NET_HEADER_SIZE + COMP_HEADER_SIZE + Size);
          end
          else
          begin
            Move(CompressedSize, PacketBuffer.Mem[Offset + 0], 3);
            Move(CompPacketNr, PacketBuffer.Mem[Offset + 3], 1);
            Move(Size, PacketBuffer.Mem[Offset + NET_HEADER_SIZE], 3);

            if (NET_HEADER_SIZE + COMP_HEADER_SIZE + my_uint(CompressedSize) > Size) then
            begin
              Result := Send(PacketBuffer.Mem[Offset], NET_HEADER_SIZE + COMP_HEADER_SIZE);
              Result := Result and Send(CompressBuffer^, CompressedSize);
            end
            else
            begin
              Move(CompressBuffer^, PacketBuffer.Mem[Offset + NET_HEADER_SIZE + COMP_HEADER_SIZE], CompressedSize);
              Result := Send(PacketBuffer.Mem[Offset], NET_HEADER_SIZE + COMP_HEADER_SIZE + CompressedSize);
            end;

            Inc(Offset, Size);
          end;

        CompPacketNr := (CompPacketNr + 1) and $FF;

        if (Assigned(CompressBuffer)) then
          FreeMem(CompressBuffer);
      until (not Result or (Offset = PacketBuffer.Size));
    end;

    PacketBuffer.Offset := 0;
    PacketBuffer.Size := NET_HEADER_SIZE; // Reservate space for packet header
  end;
end;

function TMySQL_File.GetFileSize(): my_int;
begin
  if (Direction = idRead) then
    Result := FReadFileBuffer.Size
  else
    Result := PacketBuffer.Size - (NET_HEADER_SIZE + PacketBuffer.Offset);
end;

procedure TMySQL_File.next_command();
begin
  Direction := idWrite;

  PacketNr := 0;
  CompPacketNr := 0;
end;

function TMySQL_File.next_result(): my_int;
begin
  Direction := idRead;

  if (Compress) then
    PacketNr := CompPacketNr;

  Result := 0;
end;

function TMySQL_File.ReadFile(const Buffer: my_char; const Size: my_uint): Boolean;
begin
  Assert(Direction = idRead);
  

  Result := FReadFileBuffer.Offset + Size <= FReadFileBuffer.Size;
  if (not Result) then
    Seterror(CR_SERVER_HANDSHAKE_ERR)
  else
  begin
    Move(FReadFileBuffer.Mem[FReadFileBuffer.Offset], Buffer^, Size);
    Inc(FReadFileBuffer.Offset, Size);
  end;
end;

function TMySQL_File.ReadFile(out Value: my_int; const Size: Byte = 0): Boolean;
var
  LL: my_ulonglong;
begin
  Result := ReadFile(LL, Size);
  if (Result) then
    Move(LL, Value, SizeOf(Value));
end;

function TMySQL_File.ReadFile(out Value: my_uint; const Size: Byte = 0): Boolean;
var
  LL: my_ulonglong;
begin
  Result := ReadFile(LL, Size);
  if (Result) then
    Move(LL, Value, SizeOf(Value));
end;

function TMySQL_File.ReadFile(out Value: my_ulonglong; const Size: Byte = 0): Boolean;
begin
  FillChar(Value, SizeOf(Value), #0);

  if ((errno() <> 0) and (errno() <> CR_SERVER_LOST)) then
    Result := False
  else if (Size > 0) then
  begin
    Result := FReadFileBuffer.Offset + Size <= FReadFileBuffer.Size;
    if (Result) then
    begin
      Move(FReadFileBuffer.Mem[FReadFileBuffer.Offset], Value, Size);
      Inc(FReadFileBuffer.Offset, Size);
    end;
  end
  else if (FReadFileBuffer.Offset + 1 > FReadFileBuffer.Size) then
    Result := False
  else if (Byte(FReadFileBuffer.Mem[FReadFileBuffer.Offset]) = $FB) then
  begin
    Result := True;
    Value := NULL_LENGTH;
    Inc(FReadFileBuffer.Offset);
  end
  else if (Byte(FReadFileBuffer.Mem[FReadFileBuffer.Offset]) < $FB) then
  begin
    Result := True;
    Move(FReadFileBuffer.Mem[FReadFileBuffer.Offset], Value, 1);
    Inc(FReadFileBuffer.Offset, 1);
  end
  else if (Byte(FReadFileBuffer.Mem[FReadFileBuffer.Offset]) = $FC) then
  begin
    Result := FReadFileBuffer.Offset + 2 <= FReadFileBuffer.Size;
    if (Result) then
    begin
      Move(FReadFileBuffer.Mem[FReadFileBuffer.Offset + 1], Value, 2);
      Inc(FReadFileBuffer.Offset, 3);
    end;
  end
  else if (Byte(FReadFileBuffer.Mem[FReadFileBuffer.Offset]) = $FD) then
  begin
    Result := FReadFileBuffer.Offset + 3 <= FReadFileBuffer.Size;
    if (Result) then
    begin
      Move(FReadFileBuffer.Mem[FReadFileBuffer.Offset + 1], Value, 3);
      Inc(FReadFileBuffer.Offset, 4);
    end;
  end
  else
  begin
    Result := FReadFileBuffer.Offset + 8 <= FReadFileBuffer.Size;
    if (Result) then
    begin
      Move(FReadFileBuffer.Mem[FReadFileBuffer.Offset + 1], Value, 8);
      Inc(FReadFileBuffer.Offset, 9);
    end;
  end;
end;

function TMySQL_File.ReadFile(out Value: RawByteString; const NTS: Boolean = True; const Size: Byte = 0): Boolean;
var
  Len: my_ulonglong;
begin
  if ((errno() <> 0) and (errno() <> CR_SERVER_GONE_ERROR)) then
    Result := False
  else if (not NTS and (Size > 0)) then
    if (FReadFileBuffer.Offset + Size > FReadFileBuffer.Size) then
      Result := False
    else
    begin
      SetString(Value, PAnsiChar(@FReadFileBuffer.Mem[FReadFileBuffer.Offset]), Size);
      Inc(FReadFileBuffer.Offset, Size);
      Result := True;
    end
  else if (not NTS and ReadFile(Len)) then
    if ((Len = NULL_LENGTH) or (FReadFileBuffer.Offset + Len > FReadFileBuffer.Size)) then
      Result := False
    else
    begin
      SetString(Value, PAnsiChar(@FReadFileBuffer.Mem[FReadFileBuffer.Offset]), Len);
      Inc(FReadFileBuffer.Offset, Len);
      Result := True;
    end
  else if (NTS and (FReadFileBuffer.Offset + 1 <= FReadFileBuffer.Size)) then
  begin
    Len := 0;
    while ((FReadFileBuffer.Offset + Len < FReadFileBuffer.Size) and (FReadFileBuffer.Mem[FReadFileBuffer.Offset + Len] <> #0)) do
      Inc(Len);
    SetString(Value, PAnsiChar(@FReadFileBuffer.Mem[FReadFileBuffer.Offset]), Len);
    Inc(FReadFileBuffer.Offset, Len);
    if (Size = 0) then
      Inc(FReadFileBuffer.Offset);
    Result := True;
  end
  else
    Result := False;
end;

function TMySQL_File.ReallocBuffer(var Buffer: TFileBuffer; const Size: my_uint): Boolean;
var
  NewSize: my_uint;
begin
  Result := (errno() = 0) or (Size <= Buffer.TotalSize);

  if (Result) then
    if (Size > 0) then
    begin
      NewSize := (((Size - 1) div NET_BUFFER_LENGTH) + 1) * NET_BUFFER_LENGTH;

      if (Buffer.Size > NewSize) then
      begin
        Move(Buffer.Mem[Buffer.Offset], Buffer.Mem[0], Buffer.Size - Buffer.Offset);
        Dec(Buffer.Size, Buffer.Offset);
        Buffer.Offset := 0;
      end;

      if (Buffer.Size > NewSize) then
        NewSize := (((Buffer.Size - 1) div NET_BUFFER_LENGTH) + 1) * NET_BUFFER_LENGTH;

      try
        ReallocMem(Buffer.Mem, NewSize);
        Buffer.TotalSize := NewSize;
      except
        on E: EOutOfMemory do
        begin
          Result := False;
          Seterror(CR_OUT_OF_MEMORY);
        end;
      end;
    end
    else
    begin
      if (Assigned(Buffer.Mem)) then
        FreeMem(Buffer.Mem);
      FillChar(Buffer, SizeOf(Buffer), #0);
    end
end;

function TMySQL_File.ReceivePacket(): Boolean;

  function ReceivePacketBuffer(const BytesToRead: my_uint; out BytesRead: my_uint): Boolean;
  begin
    if (PacketBuffer.Size + BytesToRead > PacketBuffer.TotalSize) then
    begin
      Move(PacketBuffer.Mem[PacketBuffer.Offset], PacketBuffer.Mem[0], PacketBuffer.Size - PacketBuffer.Offset);
      Dec(PacketBuffer.Size, PacketBuffer.Offset);
      PacketBuffer.Offset := 0;
    end;

    if (PacketBuffer.Size + BytesToRead > PacketBuffer.TotalSize) then
      ReallocBuffer(PacketBuffer, PacketBuffer.Size + BytesToRead);

    Result := Receive(PacketBuffer.Mem[PacketBuffer.Size], BytesToRead, BytesRead);

    if (Result) then
      Inc(PacketBuffer.Size, BytesRead);
  end;

  function ReceiveCompressed(const BytesToRead: my_uint; out BytesRead: my_uint): Boolean;
  var
    PacketOffset, Size, VIOSize: my_uint;
    UncompressedSize: my_uint;
    Nr: Byte;
    DecompressBuffer: Pointer;
    DecompressedSize: Integer;
  begin
    BytesRead := 0;

    Move(PacketBuffer.Mem[PacketBuffer.Offset], PacketBuffer.Mem[0], PacketBuffer.Size - PacketBuffer.Offset);
    Dec(PacketBuffer.Size, PacketBuffer.Offset);
    PacketBuffer.Offset := 0;

    repeat
      PacketOffset := PacketBuffer.Size;

      Result := ReceivePacketBuffer(NET_HEADER_SIZE, VIOSize) and (PacketBuffer.Offset + NET_HEADER_SIZE <= PacketBuffer.Size);
      if (Result) then
      begin
        FillChar(Size, SizeOf(Size), #0);
        Move(PacketBuffer.Mem[PacketOffset + 0], Size, 3);
        Move(PacketBuffer.Mem[PacketOffset + 3], Nr, 1);

        if (Nr <> CompPacketNr) then
          Seterror(CR_SERVER_LOST)
        else if (NET_HEADER_SIZE + COMP_HEADER_SIZE + Size > MAX_PACKET_LENGTH) then
          Seterror(CR_NET_PACKET_TOO_LARGE)
        else if (ReceivePacketBuffer(COMP_HEADER_SIZE + Size, VIOSize) and (PacketBuffer.Offset + NET_HEADER_SIZE + Size <= PacketBuffer.Size)) then
        begin
          FillChar(UncompressedSize, SizeOf(UncompressedSize), #0);
          Move(PacketBuffer.Mem[PacketOffset + NET_HEADER_SIZE], UncompressedSize, 3);

          if (UncompressedSize = 0) then
          begin
            Move(PacketBuffer.Mem[PacketOffset + NET_HEADER_SIZE + COMP_HEADER_SIZE], PacketBuffer.Mem[PacketOffset], Size);

            Inc(BytesRead, Size);
            PacketBuffer.Size := PacketOffset + Size;
          end
          else
          begin
            if (PacketOffset + UncompressedSize > PacketBuffer.TotalSize) then
              Result := Result and ReallocBuffer(PacketBuffer, PacketOffset + UncompressedSize);

            try
              ZDecompress(@PacketBuffer.Mem[PacketOffset + NET_HEADER_SIZE + COMP_HEADER_SIZE], UncompressedSize, DecompressBuffer, DecompressedSize);
              Move(DecompressBuffer^, PacketBuffer.Mem[PacketOffset], UncompressedSize);
              FreeMem(DecompressBuffer);
            except
              on E: EOutOfMemory do
                begin Seterror(CR_OUT_OF_MEMORY); Result := False; end;
              else
                begin Seterror(CR_UNKNOWN_ERROR); Result := False; end;
            end;

            Inc(BytesRead, UncompressedSize);
            PacketBuffer.Size := PacketOffset + UncompressedSize;
          end;
          CompPacketNr := (CompPacketNr + 1) and $FF;
        end;
      end;
    until (not Result or (BytesRead >= BytesToRead));
  end;

  function Receive(const BytesToRead: my_uint; out BytesRead: my_uint): Boolean;
  begin
    if (not Compress) then
      Result := ReceivePacketBuffer(BytesToRead, BytesRead)
    else
      Result := ReceiveCompressed(BytesToRead, BytesRead);
  end;

var
  Index: my_int;
  Nr: Byte;
  Offset: my_uint;
  Size: my_uint;
  VIOSize: my_uint;
begin
  Result := IOType <> itNone;

  if (not Result) then
    Seterror(CR_SERVER_LOST)
  else
  begin
    FillChar(FReadFileBuffer, SizeOf(FReadFileBuffer), #0);

    Index := -1;
    repeat
      Inc(Index);
      Offset := PacketBuffer.Offset; if (Index > 0) then Inc(Offset,  NET_HEADER_SIZE + my_uint(Index) * MAX_PACKET_LENGTH);
      Result := (Offset + NET_HEADER_SIZE <= PacketBuffer.Size);
      if (Result) then
        VIOSize := NET_HEADER_SIZE
      else
      begin
        Result := Receive(NET_HEADER_SIZE, VIOSize);
        Offset := PacketBuffer.Offset; if (Index > 0) then Inc(Offset,  NET_HEADER_SIZE + my_uint(Index) * MAX_PACKET_LENGTH);
      end;

      if (Offset + NET_HEADER_SIZE > PacketBuffer.Size) then
        Size := 0
      else
      begin
        FillChar(Size, SizeOf(Size), #0);
        Move(PacketBuffer.Mem[Offset + 0], Size, 3);
        Move(PacketBuffer.Mem[Offset + 3], Nr, 1);

        if (not Compress and (Nr <> PacketNr)) then
          Seterror(CR_SERVER_LOST)
        else if (Size > MAX_PACKET_LENGTH) then
          Seterror(CR_NET_PACKET_TOO_LARGE)
        else
        begin
          PacketNr := (PacketNr + 1) and $FF;

          if (Offset + NET_HEADER_SIZE + Size > PacketBuffer.TotalSize) then
            ReallocBuffer(PacketBuffer, Offset + NET_HEADER_SIZE + Size);

          if (Offset + NET_HEADER_SIZE + Size > PacketBuffer.Size) then
          begin
            Receive(Offset + NET_HEADER_SIZE + Size - PacketBuffer.Size, VIOSize);
            Offset := PacketBuffer.Offset; if (Index > 0) then Inc(Offset,  NET_HEADER_SIZE + my_uint(Index) * MAX_PACKET_LENGTH);
          end;

          if (Offset + NET_HEADER_SIZE + Size <= PacketBuffer.Size) then
            if (Index > 0) then
            begin
              Move(PacketBuffer.Mem[Offset + NET_HEADER_SIZE], PacketBuffer.Mem[Offset], PacketBuffer.Size - Offset);
              Dec(PacketBuffer.Size, NET_HEADER_SIZE);
            end;
        end;
      end;
    until (not Result or (VIOSize = 0) or (Size <> MAX_PACKET_LENGTH));

    if (Result) then
    begin
      FReadFileBuffer.Mem := @PacketBuffer.Mem[PacketBuffer.Offset + NET_HEADER_SIZE];
      FReadFileBuffer.Size := my_uint(Index) * MAX_PACKET_LENGTH + Size;
      Inc(PacketBuffer.Offset, NET_HEADER_SIZE + FReadFileBuffer.Size);
    end;
  end;
end;

function TMySQL_File.SetFilePointer(const DistanceToMove: my_int; const MoveMethod: my_int): my_int;
var
  I: my_int;
  Size: my_uint;
begin
  if (Direction = idRead) then
  begin
    if (MoveMethod = PACKET_CURRENT) then // Switch to next packet
    begin
      if ((DistanceToMove < 1) or ((errno() <> 0) and (errno() <> CR_SERVER_GONE_ERROR))) then
        Result := -1
      else
        Result := 0;
      for I := 0 to DistanceToMove - 1 do
        if (Result = 0) then
          if (not ReceivePacket()) then
            Result := -1
          else if (FReadFileBuffer.Size = 0) then
          begin
            Seterror(CR_SERVER_LOST);
            Result := -1;
          end
          else
            Result := 0;
    end
    else if ((errno() <> 0) and (errno() <> CR_SERVER_GONE_ERROR)) then
      Result := -1
    else // Move inside a packet
    begin
      case (MoveMethod) of
        FILE_BEGIN: Result := DistanceToMove;
        FILE_CURRENT: Result := my_int(FReadFileBuffer.Offset) + DistanceToMove;
        FILE_END: Result := my_int(FReadFileBuffer.Size) + DistanceToMove;
        else Result := -1;
      end;
      if ((Result < 0) or (my_int(FReadFileBuffer.Size) < Result)) then
        Seterror(CR_SERVER_HANDSHAKE_ERR)
      else
        FReadFileBuffer.Offset := Result;
    end;
  end
  else
  begin
    if (DistanceToMove <= 0) then
    begin
      Seterror(CR_UNKNOWN_ERROR);
      Result := -1;
    end
    else
      case (MoveMethod) of
        FILE_CURRENT:
          begin
            for I := 0 to DistanceToMove - 1 do WriteFile(0, 1);
            Result := PacketBuffer.Size - PacketBuffer.Offset;
          end;
        PACKET_CURRENT:
          begin
            for I := 0 to DistanceToMove - 1 do
            begin
              Size := PacketBuffer.Size - (PacketBuffer.Offset + NET_HEADER_SIZE);
              Move(Size, PacketBuffer.Mem[PacketBuffer.Offset], 3);
              Move(PacketNr, PacketBuffer.Mem[PacketBuffer.Offset + 3], 1);
              PacketNr := (PacketNr + 1) and $FF;

              PacketBuffer.Offset := PacketBuffer.Size;
              Inc(PacketBuffer.Size, NET_HEADER_SIZE); // Reserve space for packet header
            end;

            Result := 0;
          end;
        else
          begin
            Seterror(CR_UNKNOWN_ERROR);
            Result := -1;
          end;
      end;
  end;                                                             
end;

procedure TMySQL_File.SetFileAccess(ADirection: TMySQL_IO.TDirection);
const
  ReducedBufferSized = 4 * NET_BUFFER_LENGTH;
begin
  if (ADirection <> Direction) then
  begin
    if (PacketBuffer.TotalSize > ReducedBufferSized) then
      ReallocBuffer(PacketBuffer, ReducedBufferSized);

    PacketBuffer.Offset := 0;
    if (ADirection = idRead) then
      PacketBuffer.Size := 0
    else
      PacketBuffer.Size := NET_HEADER_SIZE; // Resere space for packet header
  end;

  inherited;
end;

function TMySQL_File.WriteFile(const Buffer: my_char; const Size: my_uint): Boolean;
var
  Offset: my_uint;
  PartSize: my_uint;
begin
  Assert(Direction = idWrite);
  

  Offset := 0;
  repeat
    PartSize := Size - Offset;
    if (PartSize > NET_HEADER_SIZE + MAX_PACKET_LENGTH - (PacketBuffer.Size - PacketBuffer.Offset)) then
      PartSize := NET_HEADER_SIZE + MAX_PACKET_LENGTH - (PacketBuffer.Size - PacketBuffer.Offset);

    if (PacketBuffer.Size + PartSize > PacketBuffer.TotalSize) then
      ReallocBuffer(PacketBuffer, PacketBuffer.Size + PartSize);

    if (errno() = 0) then
    begin
      Move(Buffer[Offset], PacketBuffer.Mem[PacketBuffer.Size], PartSize);
      Inc(PacketBuffer.Size, PartSize);
      Inc(Offset, PartSize);
    end;

    if (PacketBuffer.Size = NET_HEADER_SIZE + MAX_PACKET_LENGTH) then
      SetFilePointer(1, PACKET_CURRENT);

    Result := errno() = 0;
  until (not Result or (Offset = Size) and (PartSize <> MAX_PACKET_LENGTH));
end;

function TMySQL_File.WriteFile(const Value: my_ulonglong; const Size: my_uint): Boolean;
begin
  Result := WriteFile(@Value, Size);
end;

function TMySQL_File.WriteFile(const Value: RawByteString; const NTS: Boolean = True): Boolean;
begin
  Result := True;
  if (not NTS) then
    Result := Result and WriteFile(Length(Value), 1);
  if (Value <> '') then
    Result := Result and WriteFile(my_char(Value), Length(Value));
  if (NTS) then
    Result := Result and WriteFile(0, 1);
end;

{ MYSQL ****************************************************************}

function MYSQL.affected_rows(): my_ulonglong;
begin
  Result := faffected_rows;
end;

function MYSQL.character_set_name(): my_char;
begin
  Result := my_char(fcharacter_set_name);
end;

procedure MYSQL.CloseFile();
begin
  CriticalSection.Enter();

  if (Assigned(fres)) then
    FreeAndNil(fres);

  inherited;

  fclient_status := MYSQL_STATUS_READY;
  if (Assigned(finfo)) then
    begin FreeMem(finfo); finfo := nil; end;
  fserver_capabilities := 0;
  if (Assigned(fserver_info)) then
    begin FreeMem(fserver_info); fserver_info := nil; end;
  fserver_status := 0;
  FillChar(FSQLState, SizeOf(FSQLState), #0);
  fthread_id := 0;

  CriticalSection.Leave();
end;

constructor MYSQL.Create();
begin
  inherited;

  fclient_status := MYSQL_STATUS_READY;
  CriticalSection := TCriticalSection.Create();;
  fres := nil;
  UseNamedPipe := False;

  faffected_rows := 0;
  fclient_flag := CLIENT_CAPABILITIES;
  fcharacter_set_name := '';
  fcompress := False;
  ftimeout := NET_READ_TIMEOUT;
  fdb := '';
  fhost := '';
  fhost_info := '';
  finfo := nil;
  finsert_id := 0;
  flocal_infile_end := nil;
  flocal_infile_error := nil;
  flocal_infile_init := nil;
  flocal_infile_read := nil;
  flocal_infile_userdata := nil;
  fpasswd := '';
  fport := MYSQL_PORT;
  freconnect := False;
  fserver_capabilities := 0;
  fserver_info := nil;
  fthread_id := 0;
  fuser := '';
  fpipe_name := '';
  fwarning_count := 0;
end;

destructor MYSQL.Destroy();
begin
  CloseFile();

  CriticalSection.Free();

  inherited;
end;

function MYSQL.dump_debug_info(): my_int;
begin
  CriticalSection.Enter();

  Result := ExecuteCommand(COM_DEBUG, nil, 0, freconnect);

  CriticalSection.Leave();
end;

function MYSQL.eof(): my_bool;
begin
  if (not Assigned(res) or (res.ResultType = rtUsed) and (fclient_status <> MYSQL_STATUS_READY)) then
    Result := 0
  else
    Result := 1;
end;

function MYSQL.ExecuteCommand(const Command: enum_server_command;
  const Bin: my_char; const Size: my_int; const Retry: Boolean): my_int;
begin
  if ((fclient_status <> MYSQL_STATUS_READY) or (more_results() <> 0)) then
  begin
    Seterror(CR_COMMANDS_OUT_OF_SYNC);
    Result := -1;
  end
  else if ((IOType = itNone) and not (Retry and Reconnect())) then
  begin
    Seterror(CR_SERVER_GONE_ERROR);
    Result := -1;
  end
  else
  begin
    Seterror(0);
    next_command();

    if (WriteFile(@Command, 1) and WriteFile(Bin, Size) and (FlushFileBuffers() or (errno() = CR_SERVER_GONE_ERROR)) and (next_result() <= 0)) then
      if (errno() = 0) then
        Result := 0
      else
        Result := -1
    else if (Retry and Reconnect()) then
      Result := ExecuteCommand(Command, Bin, Size, False)
    else
    begin
      if (errno() = 0) then
        Seterror(CR_SERVER_LOST);
      Result := -1;
    end;
  end;
end;

function MYSQL.GetCodePage(): Cardinal;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to Length(MySQL_Collations) - 1 do
    if (lstrcmpa(MySQL_Collations[I].CharsetName, PAnsiChar(fcharacter_set_name)) = 0) then
      Result := MySQL_Collations[I].CodePage;

  if (Result = 0) then
    inherited GetCodePage();
end;

function MYSQL.get_client_info(): my_char;
begin
  Result := MYSQL_CLIENT_INFO;
end;

function MYSQL.get_client_version(): my_uint;
begin
  Result := MYSQL_CLIENT_VERSION;
end;

function MYSQL.get_host_info(): my_char;
begin
  if (fhost_info = '') then
    case (IOType) of
      itNamedPipe: fhost_info := RawByteString(Format(CLIENT_ERRORS[CR_NAMEDPIPE_CONNECTION - CR_MIN_ERROR], [LOCAL_HOST]));
      itTCPIP: fhost_info := RawByteString(Format(CLIENT_ERRORS[CR_TCP_CONNECTION - CR_MIN_ERROR], [fhost]));
    end;

  Result := my_char(fhost_info);
end;

function MYSQL.get_server_info(): my_char;
begin
  Result := fserver_info;
end;

function MYSQL.get_server_status(): my_uint;
begin
  Result := fserver_status;
end;

function MYSQL.get_server_version(): my_int;
begin
  if (not Assigned(fserver_info)) then
    Result := 0
  else
    Result := fserver_version;
end;

function MYSQL.info(): my_char;
begin
  Result := finfo;
end;

function MYSQL.insert_id(): my_ulonglong;
begin
  Result := finsert_id;
end;

function MYSQL.kill(pid: my_uint): my_int;
begin
  CriticalSection.Enter();

  Result := ExecuteCommand(COM_PROCESS_KILL, @pid, SizeOf(pid), freconnect);

  CriticalSection.Leave();
end;

function MYSQL.more_results(): my_bool;
begin
  if ((fclient_flag and CLIENT_MULTI_RESULTS = 0) or (fserver_status and SERVER_MORE_RESULTS_EXISTS = 0)) then
    Result := 0
  else
    Result := 1;
end;

function MYSQL.next_result(): my_int;
// -1  Successful and there are no more results
//  0  Successful and there are more results
// >0  An error occurred
var
  FileSent: Boolean;
  RBS: RawByteString;
begin
  if (fclient_status <> MYSQL_STATUS_READY)  then
  begin
    Seterror(CR_COMMANDS_OUT_OF_SYNC);
    Result := 1;
  end
  else
  begin
    faffected_rows := -1;
    if (Assigned(finfo)) then
      begin FreeMem(finfo); finfo := nil; end;
    finsert_id := -1;
    fwarning_count := 0;
    fres := nil;

    repeat
      FileSent := False;

      if ((inherited next_result() <> 0) or (SetFilePointer(1, PACKET_CURRENT) < 0)) then
        Result := 1
      else if (GetFileSize() = 0) then
      begin
        Seterror(CR_SERVER_HANDSHAKE_ERR);
        Result := 1;
      end
      else if (ServerError()) then
        Result := 1
      else if (Byte(ReadFileBuffer.Mem[ReadFileBuffer.Offset]) = $FB) then // NULL_LENGTH
      begin
        SetFilePointer(1, FILE_CURRENT); // $FB

        FileSent := True;
        if (not ReadFile(RBS) or not SendFile(RBS)) then
          Result := 1
        else
          Result := 0;
      end
      else if ((Byte(ReadFileBuffer.Mem[ReadFileBuffer.Offset]) = $FE) and (ReadFileBuffer.Size - ReadFileBuffer.Offset < 9)) then
      begin
        SetFilePointer(1, FILE_CURRENT); // $FE

        if (fclient_flag and CLIENT_PROTOCOL_41 <> 0) then
        begin
          ReadFile(fserver_status, 2);
          ReadFile(fwarning_count, 2);
        end;

        Result := 0;
      end
      else if (not ReadFile(FieldCount)) then
      begin
        if (errno() = 0) then
          Seterror(CR_SERVER_HANDSHAKE_ERR);
        Result := 1;
      end
      else if (FieldCount = 0) then
      begin
        ReadFile(faffected_rows);
        ReadFile(finsert_id);

        if (fclient_flag and CLIENT_PROTOCOL_41 <> 0) then
        begin
          ReadFile(fserver_status, 2);
          ReadFile(fwarning_count, 2);
        end
        else if (fserver_capabilities and CLIENT_TRANSACTIONS <> 0) then
        begin
          ReadFile(fserver_status, 2);
          fwarning_count := 0;
        end;

        if ((ReadFileBuffer.Offset < ReadFileBuffer.Size) and ReadFile(RBS, False)) then
        begin
          GetMem(finfo, Length(RBS) + 1);
          StrPCopy(finfo, RBS);
        end;

        fclient_status := MYSQL_STATUS_READY;

        Result := 0;
      end
      else
      begin
        faffected_rows := 0;
        finsert_id := 0;

        // we can switch the server in transaction
        if (fserver_status and SERVER_STATUS_AUTOCOMMIT = 0) then
          fserver_status := fserver_status or SERVER_STATUS_IN_TRANS;
        fwarning_count := 0;

        fclient_status := MYSQL_STATUS_GET_RESULT;

        Result := 0;
      end;
    until (not FileSent);
  end;
end;

function MYSQL.options(option: enum_mysql_option; const arg: my_char): my_int;
var
  I: my_int;
begin
  case option of
    MYSQL_OPT_CONNECT_TIMEOUT: if (TryStrToInt(string(arg), I) and (I > 0)) then ftimeout := I;
    MYSQL_OPT_COMPRESS: fcompress := True;
    MYSQL_OPT_NAMED_PIPE: UseNamedPipe := True;
    MYSQL_OPT_PROTOCOL: if (TryStrToInt(string(arg), I)) then UseNamedPipe := I = Ord(MYSQL_PROTOCOL_PIPE);
    MYSQL_SET_CHARSET_NAME: fcharacter_set_name := RawByteString(arg);
    MYSQL_OPT_RECONNECT: freconnect := my_bool(arg) = 0;
  end;

  Result := 0;
end;

function MYSQL.ping(): my_int;
begin
  CriticalSection.Enter();

  Result := ExecuteCommand(COM_PING, nil, 0, False);

  CriticalSection.Leave();
end;

function MYSQL.ReadRow(var Row: MYSQL_RES.PRow): my_int;
// returns -1 on error
// returns 0 on EOF
// returns FieldCount on Success
var
  I: my_uint;
  Index: my_uint;
  Len: my_int;
  TotalSize: my_uint;
begin
  Assert(Direction = idRead);


  if ((SetFilePointer(1, PACKET_CURRENT) < 0) or ServerError()) then
    Result := -1
  else if ((Byte(ReadFileBuffer.Mem[ReadFileBuffer.Offset]) = $FE) and (ReadFileBuffer.Size - ReadFileBuffer.Offset < 9)) then
  begin
    SetFilePointer(1, FILE_CURRENT); // $FE

    if (fclient_flag and CLIENT_PROTOCOL_41 <> 0) then
    begin
      ReadFile(fwarning_count, 2);
      ReadFile(fserver_status, 2);
    end;

    if (fclient_status = MYSQL_STATUS_GET_RESULT) then
      fclient_status := MYSQL_STATUS_USE_RESULT
    else
      fclient_status := MYSQL_STATUS_READY;

    Result := 0;
  end
  else if (Assigned(res) and (res.ResultType = rtUsed)) then
  begin
    for I := 0 to FieldCount - 1 do
      if ((errno() = 0) and ReadFile(res.CurrentRow^.Lengths^[I])) then
        if (res.CurrentRow^.Lengths^[I] = NULL_LENGTH) then
        begin
          res.CurrentRow^.Lengths^[I] := 0;
          res.CurrentRow^.Row^[I] := nil;
        end
        else
        begin
          res.CurrentRow^.Row^[I] := @ReadFileBuffer.Mem[ReadFileBuffer.Offset];
          SetFilePointer(res.CurrentRow^.Lengths^[I], FILE_CURRENT);
        end;
    if (ReadFileBuffer.Offset <> ReadFileBuffer.Offset) then
      Seterror(CR_SERVER_HANDSHAKE_ERR);
    if (errno() <> 0) then
      Result := -1
    else
      Result := FieldCount;
  end
  else
  begin
    Result := 0;

    TotalSize := 0;
    while ((not Assigned(res) or (my_uint(Result) < FieldCount)) and (ReadFileBuffer.Offset < ReadFileBuffer.Size) and ReadFile(Len)) do
    begin
      if (Len <> NULL_LENGTH) then
      begin
        SetFilePointer(Len, FILE_CURRENT);
        Inc(TotalSize, Len);
      end;

      Inc(Result);
    end;

    if (Assigned(res) and (my_uint(Result) <> FieldCount)) then
      Seterror(CR_SERVER_HANDSHAKE_ERR)
    else if (ReadFileBuffer.Offset <> ReadFileBuffer.Size) then
      Seterror(CR_SERVER_HANDSHAKE_ERR)
    else
    begin
      Inc(TotalSize, SizeOf(Row^) + Result * (SizeOf(Row^.Lengths^[0]) + SizeOf(Row^.Row^[0])));

      try
        if (not Assigned(Row)) then
        begin
          GetMem(Row, TotalSize);
          Row^.MemSize := TotalSize;
        end
        else if (TotalSize > Row^.MemSize) then
        begin
          ReallocMem(Row, TotalSize);
          Row^.MemSize := TotalSize;
        end;
      except
        Seterror(CR_OUT_OF_MEMORY);
      end;
    end;

    if (errno() <> 0) then
      Result := -1
    else
    begin
      Row^.Lengths := Pointer(@PAnsiChar(Row)[SizeOf(Row^)]);
      Row^.Row := Pointer(@PAnsiChar(Row)[SizeOf(Row^) + Result * SizeOf(Row^.Lengths^[0])]);

      Index := SizeOf(Row^) + Result * (SizeOf(Row^.Lengths^[0]) + SizeOf(Row^.Row^[0]));
      SetFilePointer(0, FILE_BEGIN);
      for I := 0 to Result - 1 do
        if (ReadFile(Row^.Lengths^[I])) then
          if (Row^.Lengths^[I] = NULL_LENGTH) then
          begin
            Row^.Lengths^[I] := 0;
            Row^.Row^[I] := nil;
          end
          else
          begin
            Row^.Row^[I] := @PAnsiChar(Row)[Index];
            if (ReadFile(Row^.Row^[I], Row^.Lengths^[I])) then
              Inc(Index, Row^.Lengths^[I]);

            if (Assigned(res) and (res.Fields^[I]^.max_length < my_uint(Row^.Lengths^[I]))) then
              res.Fields^[I]^.max_length := my_uint(Row^.Lengths^[I]);
          end;
    end;
  end;
end;

procedure MYSQL.ReadRows(const Ares: MYSQL_RES);
var
  PreviousRow: MYSQL_RES.PRow;
begin
  PreviousRow := nil;

  repeat
    if (Ares.ResultType = rtStored) then
    begin
      PreviousRow := Ares.CurrentRow;
      Ares.CurrentRow := nil;
    end;

    if (ReadRow(Ares.CurrentRow) = 0) then
      begin FreeMem(Ares.CurrentRow); Ares.CurrentRow := nil; end
    else
    begin
      Inc(Ares.RowIndex);
      Inc(Ares.RowCount);
    end;

    if (Ares.ResultType = rtStored) then
      if (not Assigned(Ares.FirstRow)) then
        Ares.FirstRow := Ares.CurrentRow
      else if (Assigned(PreviousRow)) then
        PreviousRow.Next := Ares.CurrentRow;
  until ((errno() <> 0) or not Assigned(Ares.CurrentRow));
end;

function MYSQL.real_connect(host, user, passwd, db: my_char; port: my_uint; unix_socket: my_char; client_flag: my_uint): MYSQL;
var
  CharsetNr: my_uint;
  I: my_int;
  NewCharsetNr: my_uint;
  ProtocolVersion: my_int;
  RBS: RawByteString;
  S: string;
  Salt: RawByteString;
begin
  CriticalSection.Enter();

  if (IOType = itNone) then
  begin
    if (host = '') then
      fhost := LOCAL_HOST
    else
      fhost := Copy(host, 1, HOSTNAME_LENGTH);
    fuser := Copy(user, 1, USERNAME_LENGTH + 1 + HOSTNAME_LENGTH);
    fpasswd := Copy(passwd, 1, 64);
    fdb := Copy(db, 1, NAME_LEN);
    if (port = 0) then
      fport := MYSQL_PORT
    else
      fport := port;
    if (StrLen(unix_socket) = 0) then
      fpipe_name := MYSQL_NAMEDPIPE
    else
      fpipe_name := unix_socket;
    fclient_flag := client_flag or CLIENT_CAPABILITIES or CLIENT_LONG_PASSWORD;
    if (fdb = '') then
      fclient_flag := fclient_flag and not CLIENT_CONNECT_WITH_DB;

    if ((host = LOCAL_HOST_NAMEDPIPE) or (StrLen(unix_socket) > 0)) then
      CreateFile(itNamedPipe, host, fpipe_name, fport, ftimeout)
    else
      CreateFile(itTCPIP, fhost, '', fport, ftimeout);

    if (IOType = itNone) then
      // errno() has been set by CreateFile()
    else if (SetFilePointer(1, PACKET_CURRENT) < 0) then
      // errno() has been set by SetFilePointer()
    else if (ServerError()) then
      // errno() has been set by ServerError()
    else if (not ReadFile(ProtocolVersion, 1)) then
      // errno() has been set by ReadFile()
    else if (ProtocolVersion <> PROTOCOL_VERSION) then
      Seterror(CR_VERSION_ERROR, EncodeString(Format(CLIENT_ERRORS[CR_VERSION_ERROR - CR_MIN_ERROR], [ProtocolVersion, PROTOCOL_VERSION])))
    else
    begin
      ReadFile(RBS); ReallocMem(fserver_info, Length(RBS) + 1); StrPCopy(fserver_info, RBS);
      ReadFile(fthread_id, 4);
      ReadFile(Salt);
      ReadFile(fserver_capabilities, 2);
      ReadFile(CharsetNr, 1);
      ReadFile(fserver_status, 2);

      if ((SetFilePointer(13, FILE_CURRENT) + 1 < GetFileSize()) and ReadFile(RBS)) then
        Salt := Salt + RBS
      else if (get_server_version() <> 40100) then
        fserver_capabilities := fserver_capabilities and not CLIENT_SECURE_CONNECTION;

      if (errno() = 0) then
      begin
        S := DecodeString(fserver_info);
        if (Pos('-', S) > 0) then
          S := copy(S, 1, Pos('-', S) - 1);
        if (S[2] = '.') and (S[4] = '.') then
          Insert('0', S, 3);
        if (S[2] = '.') and (Length(S) = 6) then
          Insert('0', S, 6);
        Val(StringReplace(S, '.', '', [rfReplaceAll	]), fserver_version, I);

        fclient_flag := fclient_flag and ($FFFF2481 or ($0000DB7E and fserver_capabilities));
        if (get_server_version() < 40101) then
          fclient_flag := fclient_flag and $FFFFF
        else if ((fserver_capabilities and CLIENT_RESERVED <> 0) and (get_server_version() < 50000)) then
          fclient_flag := fclient_flag or CLIENT_PROTOCOL_41 or CLIENT_RESERVED; //  CLIENT_PROTOCOL_41 has in some older 4.1.xx versions the value $04000 instead of $00200
        fclient_flag := fclient_flag and not CLIENT_SSL;

        if (fcharacter_set_name = '') then
        begin
          if (get_server_version() < 40101) then
            fcharacter_set_name := MySQL_Character_Sets[CharsetNr].CharsetName
          else
            for I := 0 to Length(MySQL_Collations) - 1 do
              if (MySQL_Collations[I].CharsetNr = CharsetNr) then
                fcharacter_set_name := MySQL_Collations[I].CharsetName;
        end
        else if (fclient_flag and CLIENT_PROTOCOL_41 <> 0) then
        begin
          NewCharsetNr := 0;
          for I := 0 to Length(MySQL_Collations) - 1 do
            if ((lstrcmpiA(MySQL_Collations[I].CharsetName, PAnsiChar(fcharacter_set_name)) = 0) and MySQL_Collations[I].Default) then
              NewCharsetNr := MySQL_Collations[I].CharsetNr;
          if (NewCharsetNr = 0) then
            Seterror(CR_CANT_READ_CHARSET, EncodeString(Format(CLIENT_ERRORS[CR_CANT_READ_CHARSET - CR_MIN_ERROR], [fcharacter_set_name])))
          else
          begin
            CharsetNr := NewCharsetNr;
            for I := 0 to Length(MySQL_Collations) - 1 do
              if (MySQL_Collations[I].CharsetNr = CharsetNr) then
                fcharacter_set_name := MySQL_Collations[I].CharsetName;
          end;
        end;

        Direction := idWrite;
        if (fclient_flag and CLIENT_PROTOCOL_41 = 0) then
        begin
          WriteFile(fclient_flag and $FFFF, 2);
          WriteFile(MAX_ALLOWED_PACKET, 3); // Max allowed packet size (Client)
        end
        else
        begin
          WriteFile(fclient_flag, 4);
          WriteFile($40000000, 4); // Max allowed packet size (Client)
          WriteFile(CharsetNr, 1);
          WriteFile(RawByteString(StringOfChar(#0, 22))); // unused space
        end;

        if (errno() = 0) then
        begin
          WriteFile(fuser);
          if (fpasswd = '') then
            WriteFile('')
          else if (fserver_capabilities and CLIENT_SECURE_CONNECTION = 0) then
            WriteFile(Scramble(my_char(fpasswd), my_char(Salt)))
          else
            WriteFile(SecureScramble(my_char(fpasswd), my_char(Salt)), False);
          if (fclient_flag and CLIENT_CONNECT_WITH_DB <> 0) then
            WriteFile(fdb);
          FlushFileBuffers();


          Direction := idRead;
          if (SetFilePointer(1, PACKET_CURRENT) = 0) then
          begin
            if ((Byte(ReadFileBuffer.Mem[ReadFileBuffer.Offset]) = $FE) and (GetFileSize() < 9) and (fserver_capabilities and CLIENT_SECURE_CONNECTION <> 0)) then
            begin
              Direction := idWrite;
              WriteFile(Scramble(my_char(fpasswd), my_char(RawByteString(Copy(Salt, 1, SCRAMBLE_LENGTH_323)))));
              if (not FlushFileBuffers()) then
                Seterror(CR_SERVER_GONE_ERROR)
              else
              begin
                Direction := idRead;
                SetFilePointer(1, PACKET_CURRENT);
              end;
            end;

            if (errno() = 0) then
              if (GetFileSize() = 0) then
                Seterror(CR_SERVER_LOST)
              else if (not ServerError()) then
              begin
                SetFilePointer(1, FILE_CURRENT); // $00
                ReadFile(faffected_rows);
                ReadFile(finsert_id);

                if (fclient_flag and CLIENT_PROTOCOL_41 <> 0) then
                begin
                  ReadFile(fserver_status, 2);
                  ReadFile(fwarning_count, 2);
                end
                else if (fserver_capabilities and CLIENT_TRANSACTIONS <> 0) then
                begin
                  ReadFile(fserver_status, 2);
                  fwarning_count := 0;
                end;

                Compress := fclient_flag and CLIENT_COMPRESS <> 0;
              end;
          end;
        end;
      end;
    end;

    // if server does not support connect with db, so we need to select the db
    if ((errno() = 0) and (fserver_capabilities and CLIENT_CONNECT_WITH_DB = 0) and (client_flag and CLIENT_CONNECT_WITH_DB <> 0)) then
      select_db(my_char(fdb));
  end;

  if (errno() <> 0) then
  begin
    CloseFile();
    Result := nil;
  end
  else
    Result := Self;

  CriticalSection.Leave();
end;

function MYSQL.real_escape_string(_to: my_char; const from: my_char; length: my_uint): my_uint;
var
  RBS: RawByteString;
begin
  SetString(RBS, from, length);

  RBS := EncodeString(StringReplace(StringReplace(StringReplace(StringReplace(StringReplace(StringReplace(DecodeString(RBS), '\', '\\', [rfReplaceAll]), #0, '\0', [rfReplaceAll]), #10, '\n', [rfReplaceAll]), #13, '\r', [rfReplaceAll]), '''', '\''', [rfReplaceAll]), '"', '\"', [rfReplaceAll]));

  StrPCopy(_to, RBS);
  Result := System.Length(RBS);
end;

function MYSQL.real_query(query: my_char; length: my_int): my_int;
begin
  if (StrLen(query) = 0) then
    Result := -1
  else
  begin
    CriticalSection.Enter();

    Result := ExecuteCommand(COM_QUERY, query, length, False);

    CriticalSection.Leave();
  end;
end;

function MYSQL.Reconnect(): Boolean;
begin
  if (fserver_status and SERVER_STATUS_IN_TRANS <> 0) then
  begin
    fserver_status := fserver_status and not SERVER_STATUS_IN_TRANS;
    Result := False;
  end
  else
    Result := Assigned(real_connect(my_char(fhost), my_char(fuser), my_char(fpasswd), my_char(fdb), fport, my_char(fpipe_name), fclient_flag));
end;

function MYSQL.refresh(options: my_int): my_int;
begin
  CriticalSection.Enter();

  Result := ExecuteCommand(COM_REFRESH, @options, SizeOf(options), freconnect);

  CriticalSection.Leave();
end;

function MYSQL.select_db(db: my_char): my_int;
begin
  if (db = '') then
    Result := 1
  else
  begin
    CriticalSection.Enter();

    Result := ExecuteCommand(COM_INIT_DB, db, StrLen(db), freconnect);
    if (Result = 0) then
      fdb := db;

    CriticalSection.Leave();
  end;
end;

function MYSQL.SendFile(const Filename: RawByteString): Boolean;
// send file, initiated by LOAD DATA LOCAL INFILE
const
  MaxFileBufferSize = 2 * NET_BUFFER_LENGTH;
var
  Buffer: PAnsiChar;
  BufferSize: DWord;
  BytesPerSector: DWord;
  ErrMsg: PAnsiChar;
  Handle: THandle;
  NumberofFreeClusters: DWord;
  ptr: Pointer;
  ReadSize: DWord;
  SectorsPerCluser: DWord;
  Size: my_int;
  TotalNumberOfClusters: DWord;
begin
  if (not Assigned(flocal_infile_init) or not Assigned(flocal_infile_read) or not Assigned(flocal_infile_end) or not Assigned(flocal_infile_error)) then
  begin
    Handle := Windows.CreateFile(PChar(DecodeString(Filename)),
                                  GENERIC_READ,
                                  FILE_SHARE_READ,
                                  nil, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);

    Result := Handle <> INVALID_HANDLE_VALUE;
    if (not Result) then
      Seterror(EE_FILENOTFOUND, EncodeString(Format('%s  (%s)', [SysErrorMessage(GetLastError()), Filename])))
    else
    begin
      BufferSize := MaxFileBufferSize;
      if (BufferSize > 0) then
      begin
        if (GetDiskFreeSpace(PChar(DecodeString(Filename)), SectorsPerCluser, BytesPerSector, NumberofFreeClusters, TotalNumberOfClusters)) then
          if (BufferSize mod BytesPerSector > 0) then
            Inc(BufferSize, BytesPerSector - BufferSize mod BytesPerSector);
        Buffer := VirtualAlloc(nil, BufferSize, MEM_COMMIT, PAGE_READWRITE);

        Result := Assigned(Buffer);
        if (not Result) then
          Seterror(CR_OUT_OF_MEMORY)
        else
        begin
          repeat
            Result := Windows.ReadFile(Handle, Buffer^, BufferSize, ReadSize, nil);
            if (not Result) then
              Seterror(EE_READ, EncodeString(Format('%s  (%s)', [SysErrorMessage(GetLastError()), Filename])))
            else
            begin
              if ((GetFileSize() > 0) and (GetFileSize() + Integer(ReadSize) > 2 * NET_BUFFER_LENGTH)) then
                Result := FlushFileBuffers();
              if (Result) then
                Result := WriteFile(Buffer, ReadSize);
            end;
          until (not Result or (ReadSize = 0));

          VirtualFree(Buffer, BufferSize, MEM_RELEASE);
        end;
      end;

      CloseHandle(Handle);
    end;
  end
  else
  begin
    try
      GetMem(ErrMsg, MYSQL_ERRMSG_SIZE);
    except
      Seterror(CR_OUT_OF_MEMORY);
      ErrMsg := nil;
    end;
    ptr := nil;
    Result := Assigned(ErrMsg);
    if (Result) then
    begin
      Result := flocal_infile_init(@ptr, my_char(Filename), flocal_infile_userdata^) = 0;
      if (not Result) then
        Seterror(flocal_infile_error(ptr, ErrMsg, MYSQL_ERRMSG_SIZE), RawByteString(ErrMsg))
      else
      begin
        BufferSize := MaxFileBufferSize;
        try
          GetMem(Buffer, BufferSize);
        except
          Seterror(CR_OUT_OF_MEMORY);
          Buffer := nil;
        end;

        Result := Assigned(Buffer);
        if (Result) then
        begin
          Direction := idWrite;
          repeat
            Size := flocal_infile_read(ptr, Buffer, BufferSize);
            Result := Size >= 0;

            if (not Result) then
              Seterror(flocal_infile_error(ptr, ErrMsg, MYSQL_ERRMSG_SIZE), RawByteString(ErrMsg))
            else
            begin
              if ((GetFileSize() > 0) and (GetFileSize() + Size > 2 * NET_BUFFER_LENGTH)) then
                Result := FlushFileBuffers();
              if (Result) then
                Result := WriteFile(Buffer, Size);
            end;
          until (not Result or (Size <= 0));

          FreeMem(Buffer);
        end;

        flocal_infile_end(ptr);
      end;

      FreeMem(ErrMsg);
    end;
  end;

  if (Result) then
  begin
    if (GetFileSize() > 0) then
      SetFilePointer(1, PACKET_CURRENT);
    FlushFileBuffers();
  end;
end;

function MYSQL.ServerError(): Boolean;
var
  I: my_uint;
  RBS: RawByteString;
begin
  Result := Byte(ReadFileBuffer.Mem[ReadFileBuffer.Offset]) = $FF;

  if (Result) then
  begin
    SetFilePointer(1, FILE_CURRENT);
    ReadFile(I, 2);
    ReadFile(RBS);
    if ((Length(RBS) < 6) or (RBS[1] <> '#')) then
      Seterror(I, RBS)
    else
    begin
      Move(PAnsiChar(RawByteString(Copy(RBS, 2, SQLSTATE_LENGTH)))^, FSQLState, SQLSTATE_LENGTH);
      Seterror(I, Copy(RBS, 7, Length(RBS) - SQLSTATE_LENGTH - 1));
    end;

    fserver_status := fserver_status and not SERVER_MORE_RESULTS_EXISTS;
  end;
end;

procedure MYSQL.Seterror(const AErrNo: my_uint; const AError: RawByteString = '');
begin
  inherited;

  FillChar(FSQLState, SizeOf(FSQLState), #0);

  {$IFDEF EurekaLog}
    if (AErrNo = CR_COMMANDS_OUT_OF_SYNC) then
      raise Exception.Create(DecodeString(error()) + ' (' + IntToStr(Byte(fclient_status)) + ')')
    else if (AErrNo = CR_OUT_OF_MEMORY) then
      raise Exception.Create(DecodeString(error()));
  {$ENDIF}
end;

function MYSQL.set_character_set(const csname: my_char): my_int;
var
  I: my_int;
  SQL: RawByteString;
begin
  if (get_server_version() < 40101) then
    Result := 0
  else
  begin
    SQL := '';
    for I := 0 to Length(MySQL_Collations) - 1 do
      if ((lstrcmpiA(MySQL_Collations[I].CharsetName, PAnsiChar(fcharacter_set_name)) = 0) and MySQL_Collations[I].Default) then
        SQL := MySQL_Collations[I].CharsetName;

    if (SQL = '') then
      Seterror(CR_CANT_READ_CHARSET, RawByteString(Format(CLIENT_ERRORS[CR_CANT_READ_CHARSET - CR_MIN_ERROR], [csname])))
    else
    begin
      SQL := 'SET NAMES ' + SQL + ';';
      if (real_query(my_char(RawByteString(SQL)), Length(SQL)) = 0) then
        fcharacter_set_name := RawByteString(LowerCase(string(csname)));
    end;

    Result := errno();
  end;
end;

procedure MYSQL.set_local_infile_default();
begin
  flocal_infile_end := nil;
  flocal_infile_error := nil;
  flocal_infile_init := nil;
  flocal_infile_read := nil;
  flocal_infile_userdata := nil;
end;

procedure MYSQL.set_local_infile_handler(local_infile_init: Tlocal_infile_init; local_infile_read: Tlocal_infile_read; local_infile_end: Tlocal_infile_end; local_infile_error: Tlocal_infile_error; userdata: Pointer);
begin
  flocal_infile_init := local_infile_init;
  flocal_infile_read := local_infile_read;
  flocal_infile_end := local_infile_end;
  flocal_infile_error := local_infile_error;
  flocal_infile_userdata := userdata;
end;

function MYSQL.set_server_option(option: enum_mysql_set_option): my_int;
var
  W: Word;
begin
  CriticalSection.Enter();

  W := Word(option);
  Result := ExecuteCommand(COM_SET_OPTION, @W, SizeOf(W), freconnect);

  CriticalSection.Leave();
end;

function MYSQL.shutdown(shutdown_level: mysql_enum_shutdown_level): my_int;
begin
  CriticalSection.Enter();

  Result := ExecuteCommand(COM_SHUTDOWN, @shutdown_level, SizeOf(shutdown_level), freconnect);

  CriticalSection.Leave();
end;

function MYSQL.sqlstate(): my_char;
begin
  Result := @FSQLState;
end;

function MYSQL.store_result(): MYSQL_RES;
begin
  Result := use_result();

  if (Assigned(Result) and (errno() = 0)) then
  begin
    Result.ResultType := rtStored;

    ReadRows(Result);
    Result.RowIndex := -1;
    Result.CurrentRow := nil;

    faffected_rows := result.RowCount;
  end;
end;

function MYSQL.thread_id(): my_uint;
begin
  Result := fthread_id;
end;

function MYSQL.use_result(): MYSQL_RES;
begin
  if ((errno() <> 0) or (fclient_status = MYSQL_STATUS_READY)) then
    Result := nil
  else if (fclient_status <> MYSQL_STATUS_GET_RESULT) then
  begin
    Seterror(CR_COMMANDS_OUT_OF_SYNC);
    Result := nil;
  end
  else if (FieldCount = 0) then
  begin
    Seterror(CR_UNKNOWN_ERROR);
    Result := nil;
  end
  else
  begin
    fres := MYSQL_RES.Create(Self, FieldCount);

    if (errno() <> 0) then
      FreeAndNil(fres);

    Result := res;
  end;
end;

function MYSQL.warning_count(): my_uint;
begin
  Result := fwarning_count;
end;

{ MYSQL_RES *******************************************************************}

constructor MYSQL_RES.Create(const Amysql: MYSQL; const AFieldCount: my_uint);
var
  Field: MYSQL_FIELD;
  Index: Integer;
  ItemCount: Integer;
  MemSize: Integer;
  Row: MYSQL_RES.PRow;
begin
  inherited Create();

  mysql := Amysql;

  CurrentRow := nil;
  FieldCount := 0;
  FieldIndex := -1;
  Fields := nil;
  RowCount := 0;
  RowIndex := -1;
  ResultType := rtUsed;

  if (AFieldCount = 0) then
    mysql.Seterror(CR_UNKNOWN_ERROR)
  else
    try
      Fields := AllocMem(AFieldCount * SizeOf(Fields^[0]));
    except
      mysql.Seterror(CR_OUT_OF_MEMORY);
    end;

  if (mysql.errno() = 0) then
  begin
    Row := nil; Field := nil;

    repeat
      ItemCount := mysql.ReadRow(Row);
      if (ItemCount <= 0) then
        Field := nil
      else
      begin
        MemSize := SizeOf(Field^);
        if (mysql.fclient_flag and CLIENT_PROTOCOL_41 = 0) then
          if (ItemCount < 5) then
            mysql.Seterror(CR_SERVER_HANDSHAKE_ERR)
          else
          begin
            Inc(MemSize, Row^.Lengths^[0] + 1);
            Inc(MemSize, Row^.Lengths^[1] + 1);
            if (ItemCount > 5) then Inc(MemSize, Row^.Lengths^[5] + 1);
          end
        else
          if (ItemCount < 7) then
            mysql.Seterror(CR_SERVER_HANDSHAKE_ERR)
          else
          begin
            Inc(MemSize, Row^.Lengths^[0] + 1);
            Inc(MemSize, Row^.Lengths^[1] + 1);
            Inc(MemSize, Row^.Lengths^[2] + 1);
            Inc(MemSize, Row^.Lengths^[3] + 1);
            Inc(MemSize, Row^.Lengths^[4] + 1);
            Inc(MemSize, Row^.Lengths^[5] + 1);
            if (ItemCount > 7) then Inc(MemSize, Row^.Lengths^[7] + 1);
          end;

        if (mysql.errno() = 0) then
          try
            Field := AllocMem(MemSize);
          except
            mysql.Seterror(CR_OUT_OF_MEMORY);
          end;

        if (mysql.errno() = 0) then
        begin
          Index := SizeOf(Field^);
          if (mysql.fclient_flag and CLIENT_PROTOCOL_41 = 0) then
          begin
            if (Assigned(Row^.Row^[0])) then begin Field^.table := @PAnsiChar(Field)[Index]; MoveMemory(Field^.table, Row^.Row^[0], Row^.Lengths^[0]); end; Inc(Index, Row^.Lengths^[0] + 1);
            Field^.table_length := Row^.Lengths^[0];
            if (Assigned(Row^.Row^[1])) then begin Field^.name := @PAnsiChar(Field)[Index]; MoveMemory(Field^.name, Row^.Row^[1], Row^.Lengths^[1]); end; Inc(Index, Row^.Lengths^[1] + 1);
            Field^.name_length := Row^.Lengths^[1];
            if (Assigned(Row^.Row^[2])) then MoveMemory(@Field^.length, @Row^.Row^[2][0], 3);
            if (Assigned(Row^.Row^[3])) then MoveMemory(@Field^.field_type, @Row^.Row^[3][0], 1);
            if (Assigned(Row^.Row^[4])) then
              if (mysql.fserver_capabilities and CLIENT_LONG_FLAG = 0) then
                MoveMemory(@Field^.flags, @Row^.Row^[4][0], 1)
              else
                MoveMemory(@Field^.flags, @Row^.Row^[4][0], 2);
            if (Assigned(Row^.Row^[4])) then
              if (mysql.fserver_capabilities and CLIENT_LONG_FLAG = 0) then
                MoveMemory(@Field^.decimals, @Row^.Row^[4][1], 1)
              else
                MoveMemory(@Field^.decimals, @Row^.Row^[4][2], 1);
            if ((ItemCount > 5) and Assigned(Row^.Row^[5])) then begin Field^.def := @PAnsiChar(Field)[Index]; MoveMemory(Field^.def, Row^.Row^[5], Row^.Lengths^[5] + 1); end;
            if (ItemCount > 5) then if (Assigned(Field^.def)) then Field^.def_length := Row^.Lengths^[5];
          end
          else
          begin
            if (Assigned(Row^.Row^[0])) then begin Field^.catalog := @PAnsiChar(Field)[Index]; MoveMemory(Field^.catalog, Row^.Row^[0], Row^.Lengths^[0]); end; Inc(Index, Row^.Lengths^[0] + 1);
            Field^.catalog_length := Row^.Lengths^[0];
            if (Assigned(Row^.Row^[1])) then begin Field^.db := @PAnsiChar(Field)[Index]; MoveMemory(Field^.db, Row^.Row^[1], Row^.Lengths^[1]); end; Inc(Index, Row^.Lengths^[1] + 1);
            Field^.db_length := Row^.Lengths^[1];
            if (Assigned(Row^.Row^[2])) then begin Field^.table := @PAnsiChar(Field)[Index]; MoveMemory(Field^.table, Row^.Row^[2], Row^.Lengths^[2]); end; Inc(Index, Row^.Lengths^[2] + 1);
            Field^.table_length := Row^.Lengths^[2];
            if (Assigned(Row^.Row^[3])) then begin Field^.org_table := @PAnsiChar(Field)[Index]; MoveMemory(Field^.org_table, Row^.Row^[3], Row^.Lengths^[3]); end; Inc(Index, Row^.Lengths^[3] + 1);
            Field^.org_table_length := Row^.Lengths^[3];
            if (Assigned(Row^.Row^[4])) then begin Field^.name := @PAnsiChar(Field)[Index]; MoveMemory(Field^.name, Row^.Row^[4], Row^.Lengths^[4]); end; Inc(Index, Row^.Lengths^[4] + 1);
            Field^.name_length := Row^.Lengths^[4];
            if (Assigned(Row^.Row^[5])) then begin Field^.org_name := @PAnsiChar(Field)[Index]; MoveMemory(Field^.org_name, Row^.Row^[5], Row^.Lengths^[5]); end; Inc(Index, Row^.Lengths^[5] + 1);
            Field^.org_name_length := Row^.Lengths^[5];
            if (Row^.Lengths^[6] >= 2) then MoveMemory(@Field^.charsetnr, @Row^.Row^[6][0], 1);
            if (Row^.Lengths^[6] >= 6) then MoveMemory(@Field^.length, @Row^.Row^[6][2], 4);
            if (Row^.Lengths^[6] >= 7) then MoveMemory(@Field^.field_type, @Row^.Row^[6][6], 1);
            if (Row^.Lengths^[6] >= 9) then MoveMemory(@Field^.flags, @Row^.Row^[6][7], 2);
            if (Row^.Lengths^[6] >= 10) then MoveMemory(@Field^.decimals, @Row^.Row^[6][9], 1);
            if ((ItemCount > 7) and Assigned(Row^.Row^[7])) then begin Field^.def := @PAnsiChar(Field)[Index]; MoveMemory(Field^.def, Row^.Row^[7], Row^.Lengths^[7]); end;
            if (ItemCount > 7) then Field^.def_length := Row^.Lengths^[7];
          end;

          // NUM_FLAG is not sent by the server so it needs to be set
          if ((Field^.field_type <= MYSQL_TYPE_INT24) and ((Field^.field_type <> MYSQL_TYPE_TIMESTAMP) or (Field^.Length = 14) or (Field^.Length = 8)) or (Field^.field_type = MYSQL_TYPE_YEAR)) then
            Field^.flags := Field^.flags or NUM_FLAG;

          Fields^[FieldCount] := Field;
        end;
        Inc(FieldCount);
      end;
    until ((mysql.errno() <> 0) or not Assigned(Field) or (FieldCount = AFieldCount));

    if (Assigned(Row)) then
      FreeMem(Row);

    if ((mysql.errno() = 0) and (mysql.ReadRow(Row) > 0) or (FieldCount <> AFieldCount)) then
      mysql.Seterror(CR_SERVER_HANDSHAKE_ERR);

    if (mysql.errno() <> 0) then
    begin
      FreeMem(Fields); Fields := nil;
    end
    else if (ResultType = rtUsed) then
    begin
      try
        CurrentRow := AllocMem(SizeOf(CurrentRow^) + FieldCount * (SizeOf(CurrentRow^.Lengths^[0]) + SizeOf(CurrentRow^.Row^[0])));
        CurrentRow^.MemSize := SizeOf(CurrentRow^) + FieldCount * (SizeOf(CurrentRow^.Lengths^[0]) + SizeOf(CurrentRow^.Row^[0]));
        CurrentRow^.Lengths := Pointer(@PAnsiChar(CurrentRow)[SizeOf(CurrentRow^)]);
        CurrentRow^.Row := Pointer(@PAnsiChar(CurrentRow)[SizeOf(CurrentRow^) + FieldCount * SizeOf(CurrentRow^.Lengths^[0])]);
      except
        mysql.Seterror(CR_OUT_OF_MEMORY);
      end;
    end;
  end;
end;

procedure MYSQL_RES.data_seek(offset: my_ulonglong);
begin
  if ((ResultType = rtStored) and (0 <= offset) and (offset < RowCount)) then
  begin
    RowIndex := 0;
    CurrentRow := FirstRow;

    while (Assigned(CurrentRow) and (RowIndex < offset)) do
    begin
      CurrentRow := CurrentRow.Next;
      Inc(RowIndex);
    end;
  end;
end;

destructor MYSQL_RES.Destroy();
var
  I: my_int;
  Next_Row: MYSQL_RES.PRow;
begin
  if ((mysql.fclient_status = MYSQL_STATUS_USE_RESULT)) then
    MysqlClient.ReadRows(Self);

  if (mysql.fres = Self) then
    mysql.fres := nil;

  if (ResultType = rtStored) then
    CurrentRow := FirstRow;
  while (Assigned(CurrentRow)) do
  begin
    Next_Row := CurrentRow.Next;
    FreeMem(CurrentRow);
    CurrentRow := Next_Row;
  end;
  if (Assigned(Fields)) then
  begin
    for I := 0 to FieldCount - 1 do
      FreeMem(Fields^[I]);
    FreeMem(Fields);
  end;

  inherited;
end;

function MYSQL_RES.fetch_field(): MYSQL_FIELD;
begin
  if (FieldIndex >= my_int(FieldCount) - 1) then
    Result := nil
  else
  begin
    Inc(FieldIndex);
    Result := Fields^[FieldIndex];
  end;
end;

function MYSQL_RES.fetch_field_direct(fieldnr: my_uint): MYSQL_FIELD;
begin
  if (fieldnr >= FieldCount) then
    Result := nil
  else
  begin
    Result := Fields^[fieldnr];
    FieldIndex := fieldnr;
  end;
end;

function MYSQL_RES.fetch_fields(): MYSQL_FIELDS;
begin
  Result := Fields;
end;

function MYSQL_RES.fetch_lengths(): MYSQL_LENGTHS;
begin
  if (not Assigned(CurrentRow)) then
    Result := nil
  else
    Result := CurrentRow^.Lengths;
end;

function MYSQL_RES.fetch_row(): MYSQL_ROW;
begin
  if (ResultType = rtStored) then
    if (RowIndex >= RowCount - 1) then
      Result := nil
    else
    begin
      if (not Assigned(CurrentRow)) then
        CurrentRow := FirstRow
      else
        CurrentRow := CurrentRow^.Next;
      Result := CurrentRow^.Row;
      Inc(RowIndex);
    end
  else if (MysqlClient.fclient_status <> MYSQL_STATUS_USE_RESULT) then
  begin
    MysqlClient.Seterror(CR_COMMANDS_OUT_OF_SYNC);
    Result := nil;
  end
  else if (MysqlClient.ReadRow(CurrentRow) <= 0) then
  begin
    FreeMem(CurrentRow); CurrentRow := nil;
    mysql.fclient_status := MYSQL_STATUS_READY;
    Result := nil;
  end
  else
  begin
    Result := CurrentRow^.Row;

    Inc(RowIndex);
    Inc(RowCount);
  end;
end;

function MYSQL_RES.num_fields(): my_uint;
begin
  Result := FieldCount;
end;

function MYSQL_RES.num_rows(): my_ulonglong;
begin
  Result := RowCount;
end;

initialization
  WSAData.wVersion := 0;
  WS2_32 := 0;
  WSAConnectByNameA := nil;
finalization
  if (WSAData.wVersion <> 0) then
    WSACleanup();
  if (WS2_32 <> 0) then
    FreeLibrary(WS2_32);
end.

