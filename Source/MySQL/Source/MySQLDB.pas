unit MySQLDB;

interface {********************************************************************}

uses
  Classes, SysUtils, Windows, SyncObjs,
  DB, DBTables, DBCommon, SqlTimSt,
  MySQLConsts;

const
  CR_ASYNCHRON = -1;
  CR_SET_NAMES  = 2300;
  CR_SERVER_OLD = 2301;

type
  TMySQLMonitor = class;
  TMySQLConnection = class;                      
  TMySQLQuery = class;
  TMySQLDataSet = class;

  TMySQLLibrary = class
  type
    TLibraryType = (ltBuiltIn, ltDLL, ltHTTP, ltNamedPipe);
  private
    FHandle: HModule;
    FLibraryType: TLibraryType;
    FFilename: TFileName;
    FVersion: Integer;
    function GetVersionStr(): string;
  public
    my_init: Tmy_init;
    mysql_affected_rows: Tmysql_affected_rows;
    mysql_character_set_name: Tmysql_character_set_name;
    mysql_close: Tmysql_close;
    mysql_errno: Tmysql_errno;
    mysql_error: Tmysql_error;
    mysql_fetch_field: Tmysql_fetch_field;
    mysql_fetch_field_direct: Tmysql_fetch_field_direct;
    mysql_fetch_fields: Tmysql_fetch_fields;
    mysql_fetch_lengths: Tmysql_fetch_lengths;
    mysql_fetch_row: Tmysql_fetch_row;
    mysql_field_count: Tmysql_field_count;
    mysql_free_result: Tmysql_free_result;
    mysql_get_client_info: Tmysql_get_client_info;
    mysql_get_client_version: Tmysql_get_client_version;
    mysql_get_host_info: Tmysql_get_host_info;
    mysql_get_server_info: Tmysql_get_server_info;
    mysql_get_server_status: Tmysql_get_server_status;
    mysql_get_server_version: Tmysql_get_server_version;
    mysql_info: Tmysql_info;
    mysql_init: Tmysql_init;
    mysql_insert_id: Tmysql_insert_id;
    mysql_library_end: Tmysql_library_end;
    mysql_library_init: Tmysql_library_init;
    mysql_more_results: Tmysql_more_results;
    mysql_next_result: Tmysql_next_result;
    mysql_num_fields: Tmysql_num_fields;
    mysql_num_rows: Tmysql_num_rows;
    mysql_options: Tmysql_options;
    mysql_ping: Tmysql_ping;
    mysql_real_connect: Tmysql_real_connect;
    mysql_real_escape_string: Tmysql_real_escape_string;
    mysql_real_query: Tmysql_real_query;
    mysql_set_character_set: Tmysql_set_character_set;
    mysql_set_local_infile_default: Tmysql_set_local_infile_default;
    mysql_set_local_infile_handler: Tmysql_set_local_infile_handler;
    mysql_set_server_option: Tmysql_set_server_option;
    mysql_shutdown: Tmysql_shutdown;
    mysql_store_result: Tmysql_store_result;
    mysql_thread_end: Tmysql_thread_end;
    mysql_thread_id: Tmysql_thread_id;
    mysql_thread_init: Tmysql_thread_init;
    mysql_thread_save: Tmysql_thread_save;
    mysql_use_result: Tmysql_use_result;
    mysql_warning_count: Tmysql_warning_count;
    constructor Create(const ALibraryType: TLibraryType; const AFilename: TFileName); virtual;
    destructor Destroy(); override;
    function Field(const RawField: MYSQL_FIELD): TMYSQL_FIELD; virtual;
    property Filename: TFileName read FFilename;
    property Handle: HModule read FHandle;
    property LibraryType: TLibraryType read FLibraryType;
    property Version: Integer read FVersion;
    property VersionStr: string read GetVersionStr;
  end;

  EMySQLUserAbort = class(EAbort);
  EDatabasePostError = class(EDatabaseError);

  EMySQLError = class(EDatabaseError)
  protected
    FConnection: TMySQLConnection;
    FErrorCode: Integer;
  public
    constructor Create(const Msg: string; const AErrorCode: Integer; const AConnection: TMySQLConnection); virtual;
    property Connection: TMySQLConnection read FConnection;
    property ErrorCode: Integer read FErrorCode;
  end;

  TMySQLMonitor = class(TComponent)
  type
    TCache = record
      First: Integer;
      Items: TList;
      MaxSize: Integer;
      Mem: PChar;
      MemSize: Integer;
      UsedSize: Integer;
    end;
    TTraceType = (ttTime, ttRequest, ttResult, ttData, ttInfo);
    TTraceTypes = set of TTraceType;
    TMySQLOnMonitor = procedure (const Sender: TObject; const Text: PChar; const Length: Integer; const ATraceType: TTraceType) of object;
  private
    Cache: TCache;
    FCacheSize: Integer;
    FConnection: TMySQLConnection;
    FEnabled: Boolean;
    FOnMonitor: TMySQLOnMonitor;
    FTraceTypes: TTraceTypes;
    function GetCacheText(): string;
    procedure SetConnection(const AConnection: TMySQLConnection);
    procedure SetCacheSize(const ACacheSize: Integer);
    procedure SetOnMonitor(const AOnMonitor: TMySQLOnMonitor);
  protected
    procedure DoMonitor(const Sender: TObject; const Text: PChar; const Length: Integer; const ATraceType: TTraceType); virtual;
  public
    property CacheText: string read GetCacheText;
    procedure Clear(); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Connection: TMySQLConnection read FConnection write SetConnection;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property OnMonitor: TMySQLOnMonitor read FOnMonitor write SetOnMonitor default nil;
    property CacheSize: Integer read Cache.MaxSize write SetCacheSize;
    property TraceTypes: TTraceTypes read FTraceTypes write FTraceTypes default [ttRequest];
  end;

  TMySQLConnection = class(TCustomConnection)
  type
    TSynchroThread = class;
    TDataResult = TSynchroThread;

    TConvertErrorNotifyEvent = procedure (Sender: TObject; Text: string) of object;
    TErrorEvent = procedure (const Connection: TMySQLConnection; const ErrorCode: Integer; const ErrorMessage: string) of object;
    TOnUpdateIndexDefsEvent = procedure (const DataSet: TMySQLQuery; const IndexDefs: TIndexDefs) of object;
    TResultEvent = function (const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean of object;
    TSynchronizeEvent = procedure (const Data: Pointer) of object;

    TLibraryDataType = (ldtConnecting, ldtExecutingSQL, ldtDisconnecting);
    Plocal_infile = ^Tlocal_infile;
    Tlocal_infile = record
      Buffer: Pointer;
      BufferSize: DWord;
      Connection: TMySQLConnection;
      ErrorCode: Integer;
      Filename: array [0 .. MAX_PATH] of Char;
      Handle: THandle;
      LastError: DWord;
      Position: DWord;
    end;

    TSynchroThread = class(TThread)
    type
      TMode = (smSQL, smDataHandle, smDataSet);
      TState = (ssClose, ssConnecting, ssReady, ssExecutingSQL, ssResult, ssReceivingResult, ssNextResult, ssCancel, ssDisconnecting, ssError);
    private
      FConnection: TMySQLConnection;
      RunExecute: TEvent;
      SynchronizeStarted: TEvent;
      Time: TDateTime;
      function GetIsRunning(): Boolean;
    protected
      DataSet: TMySQLQuery;
      ErrorCode: Integer;
      ErrorMessage: string;
      LibHandle: MySQLConsts.MYSQL;
      Mode: TMode;
      OnResult: TResultEvent;
      ResultHandle: MySQLConsts.MYSQL_RES;
      SQL: string;
      SQLLastStmtInPacket: Integer;
      SQLPacket: Integer;
      SQLStmt: Integer;
      SQLStmtIndex: Integer;
      SQLStmtLengths: TList;
      SQLStmtInPacket: Integer;
      SQLStmtsInPackets: TList;
      SQLUseStmts: TList;
      State: TState;
      Success: Boolean;
      procedure BindDataSet(const ADataSet: TMySQLQuery); virtual;
      procedure Execute(); override;
      procedure ReleaseDataSet(); virtual;
      procedure RunAction(const AState: TState; const Synchron: Boolean); virtual;
      procedure Synchronize(); virtual;
      property IsRunning: Boolean read GetIsRunning;
    public
      constructor Create(const AConnection: TMySQLConnection); overload; virtual;
      destructor Destroy(); override;
      procedure Terminate(); reintroduce;
      property Connection: TMySQLConnection read FConnection;
    end;

    TTerminatedThreads = class(TList)
    private
      CriticalSection: TCriticalSection;
    public
      constructor Create(); overload;
      destructor Destroy(); override;
      function Add(const Item: Pointer): Integer; reintroduce;
      procedure Delete(const Item: Pointer); overload;
    end;

  private
    ExecuteSQLDone: TEvent;
    FAfterExecuteSQL: TNotifyEvent;
    FAsynchron: Boolean;
    FAutoCommit: Boolean;
    FBeforeExecuteSQL: TNotifyEvent;
    FCharset: string;
    FCodePage: Cardinal;
    FConnected: Boolean;
    FErrorCode: Integer;
    FErrorMessage: string;
    FHost: string;
    FHostInfo: string;
    FHTTPAgent: string;
    FIdentifierQuoted: Boolean;
    FIdentifierQuoter: Char;
    FInTransaction: Boolean;
    FLatestConnect: TDateTime;
    FLibraryName: string;
    FLibraryType: TMySQLLibrary.TLibraryType;
    FOnConvertError: TConvertErrorNotifyEvent;
    FOnSQLError: TErrorEvent;
    FOnUpdateIndexDefs: TOnUpdateIndexDefsEvent;
    FPassword: string;
    FPort: Word;
    FResultCount: Integer;
    FServerTimeout: Word;
    FServerVersion: Integer;
    FServerVersionStr: string;
    FSilentCount: Integer;
    FSynchronCount: Integer;
    FSQLMonitors: array of TMySQLMonitor;
    FSynchroThread: TSynchroThread;
    FTerminateCS: TCriticalSection;
    FTerminatedThreads: TTerminatedThreads;
    FThreadDeep: Integer;
    FThreadId: my_uint;
    FUsername: string;
    InMonitor: Boolean;
    InOnResult: Boolean;
    local_infile: Plocal_infile;
    function GetCommandText(): string;
    function UseCompression(): Boolean;
    function GetDateTime(): TDateTime;
    function GetHandle(): MySQLConsts.MYSQL;
    function GetInfo(): string;
    procedure SetDatabaseName(const ADatabaseName: string);
    procedure SetHost(const AHost: string);
    procedure SetIdentifierQuoter(const AIdentifierQuoter: Char);
    procedure SetLibraryName(const ALibraryName: string);
    procedure SetLibraryType(const ALibraryType: TMySQLLibrary.TLibraryType);
    procedure SetPassword(const APassword: string);
    procedure SetPort(const APort: Word);
    procedure SetUsername(const AUsername: string);
    function UseSynchroThread(): Boolean;
  protected
    FDatabaseName: string;
    FExecutedSQLLength: Integer;
    FExecutedStmts: Integer;
    FExecutionTime: TDateTime;
    FFormatSettings: TFormatSettings;
    FLib: TMySQLLibrary;
    FMultiStatements: Boolean;
    FRowsAffected: Integer;
    FWarningCount: Integer;
    TimeDiff: TDateTime;
    procedure DoAfterExecuteSQL(); virtual;
    procedure DoBeforeExecuteSQL(); virtual;
    procedure DoConnect(); override;
    procedure DoConvertError(const Sender: TObject; const Text: string; const Error: EConvertError); virtual;
    procedure DoDisconnect(); override;
    procedure DoError(const AErrorCode: Integer; const AErrorMessage: string); virtual;
    function ErrorMsg(const AHandle: MySQLConsts.MYSQL): string; virtual;
    function ExecuteSQL(const Mode: TSynchroThread.TMode; const Synchron: Boolean; const SQL: string; const OnResult: TResultEvent = nil): Boolean; overload; virtual;
    function GetAutoCommit(): Boolean; virtual;
    function GetConnected(): Boolean; override;
    function GetInsertId(): my_ulonglong; virtual;
    function GetLoadDataFile(): Boolean; virtual;
    function GetMaxAllowedPacket(): Integer; virtual;
    function LibDecode(const Data: my_char; const Length: my_int = -1): string; virtual;
    function LibUnpack(const Data: my_char; const Length: my_int = -1): string; virtual;
    function NextCommandText(): string;
    procedure RegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor); virtual;
    procedure SetAutoCommit(const AAutoCommit: Boolean); virtual;
    procedure SetCharset(const ACharset: string); virtual;
    procedure SetConnected(Value: Boolean); override;
    procedure SyncCancel(const SynchroThread: TSynchroThread); virtual;
    procedure SyncConnecting(const SynchroThread: TSynchroThread); virtual;
    procedure SyncConnected(const SynchroThread: TSynchroThread); virtual;
    procedure SyncDisconncting(const SynchroThread: TSynchroThread); virtual;
    procedure SyncDisconncted(const SynchroThread: TSynchroThread); virtual;
    procedure SyncExecutedSQL(const SynchroThread: TSynchroThread); virtual;
    procedure SyncExecutingSQL(const SynchroThread: TSynchroThread); virtual;
    procedure SyncHandleResult(const SynchroThread: TSynchroThread); virtual;
    procedure SyncHandledResult(const SynchroThread: TSynchroThread); virtual;
    procedure SyncHandlingResult(const SynchroThread: TSynchroThread); virtual;
    procedure SyncNextResult(const SynchroThread: TSynchroThread); virtual;
    procedure SyncPing(const SynchroThread: TSynchroThread); virtual;
    procedure SyncReceivingResult(SynchroThread: TSynchroThread); virtual;
    procedure UnRegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor); virtual;
    procedure WriteMonitor(const AText: PChar; const Length: Integer; const ATraceType: TMySQLMonitor.TTraceType); virtual;
    property Handle: MySQLConsts.MYSQL read GetHandle;
    property IdentifierQuoter: Char read FIdentifierQuoter write SetIdentifierQuoter;
    property IdentifierQuoted: Boolean read FIdentifierQuoted write FIdentifierQuoted;
    property SynchroThread: TSynchroThread read FSynchroThread;
    property SilentCount: Integer read FSilentCount;
    property TerminateCS: TCriticalSection read FTerminateCS;
    property TerminatedThreads: TTerminatedThreads read FTerminatedThreads;
  public
    procedure BeginSilent(); virtual;
    procedure BeginSynchron(); virtual;
    function CanShutdown(): Boolean; virtual;
    function CharsetToCodePage(const Charset: string): Cardinal; overload; virtual;
    function CharsetToCodePage(const Charset: Byte): Cardinal; overload; virtual;
    procedure CloseResult(const DataHandle: TDataResult); virtual;
    function CodePageToCharset(const CodePage: Cardinal): string; virtual;
    procedure CommitTransaction(); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure EndSilent(); virtual;
    procedure EndSynchron(); virtual;
    function EscapeIdentifier(const Identifier: string): string; virtual;
    function ExecuteSQL(const SQL: string; const OnResult: TResultEvent = nil): Boolean; overload; virtual;
    function FirstResult(out DataHandle: TDataResult; const SQL: string): Boolean; virtual;
    function InUse(): Boolean; virtual;
    function LibEncode(const Value: string): RawByteString; virtual;
    function LibPack(const Value: string): RawByteString; virtual;
    procedure local_infile_end(const local_infile: Plocal_infile); virtual;
    function local_infile_error(const local_infile: Plocal_infile; const error_msg: my_char; const error_msg_len: my_uint): my_int; virtual;
    function local_infile_init(var local_infile: Plocal_infile; const filename: my_char): my_int; virtual;
    function local_infile_read(const local_infile: Plocal_infile; buf: my_char; const buf_len: my_uint): my_int; virtual;
    function NextResult(const DataHandle: TDataResult): Boolean; virtual;
    procedure RollbackTransaction(); virtual;
    function SendSQL(const SQL: string; const OnResult: TResultEvent = nil): Boolean; virtual;
    function Shutdown(): Boolean; virtual;
    function SQLUse(const DatabaseName: string): string; virtual;
    procedure StartTransaction(); virtual;
    procedure Terminate(); virtual;
    property AutoCommit: Boolean read GetAutoCommit write SetAutoCommit;
    property CodePage: Cardinal read FCodePage;
    property CommandText: string read GetCommandText;
    property DateTime: TDateTime read GetDateTime;
    property HostInfo: string read FHostInfo;
    property HTTPAgent: string read FHTTPAgent write FHTTPAgent;
    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
    property ExecutedSQLLength: Integer read FExecutedSQLLength;
    property ExecutedStmts: Integer read FExecutedStmts;
    property ExecutionTime: TDateTime read FExecutionTime;
    property FormatSettings: TFormatSettings read FFormatSettings;
    property Info: string read GetInfo;
    property LatestConnect: TDateTime read FLatestConnect;
    property Lib: TMySQLLibrary read FLib;
    property LoadDataFile: Boolean read GetLoadDataFile;
    property MaxAllowedPacket: Integer read GetMaxAllowedPacket;
    property MultiStatements: Boolean read FMultiStatements;
    property ResultCount: Integer read FResultCount;
    property RowsAffected: Integer read FRowsAffected;
    property ServerVersion: Integer read FServerVersion;
    property ServerVersionStr: string read FServerVersionStr;
    property ThreadId: my_uint read FThreadId;
    property WarningCount: Integer read FWarningCount;
  published
    property Asynchron: Boolean read FAsynchron write FAsynchron default False;
    property AfterExecuteSQL: TNotifyEvent read FAfterExecuteSQL write FAfterExecuteSQL;
    property BeforeExecuteSQL: TNotifyEvent read FBeforeExecuteSQL write FBeforeExecuteSQL;
    property Charset: string read FCharset write SetCharset;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Host: string read FHost write SetHost;
    property InsertId: my_ulonglong read GetInsertId;
    property InTransaction: Boolean read FInTransaction;
    property LibraryName: string read FLibraryName write SetLibraryName;
    property LibraryType: TMySQLLibrary.TLibraryType read FLibraryType write SetLibraryType default ltBuiltIn;
    property OnConvertError: TConvertErrorNotifyEvent read FOnConvertError write FOnConvertError;
    property OnSQLError: TErrorEvent read FOnSQLError write FOnSQLError;
    property OnUpdateIndexDefs: TOnUpdateIndexDefsEvent read FOnUpdateIndexDefs write FOnUpdateIndexDefs;
    property Password: string read FPassword write SetPassword;
    property Port: Word read FPort write SetPort default MYSQL_PORT;
    property ServerTimeout: Word read FServerTimeout write FServerTimeout default 0;
    property Username: string read FUsername write SetUsername;
    // TCustomConnection Properties
    property Connected;
    property LoginPrompt;
    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
    property OnLogin;
  end;

  TMySQLQuery = class(TDataSet)
  type
    TCommandType = (ctQuery, ctTable);
    PRecordBufferData = ^TRecordBufferData;
    TRecordBufferData = packed record
      LibLengths: MYSQL_LENGTHS;
      LibRow: MYSQL_ROW;
    end;
  private
    FConnection: TMySQLConnection;
    FIndexDefs: TIndexDefs;
    FInformConvertError: Boolean;
    FRecNo: Integer;
    FRowsAffected: Integer;
    FWarningCount: Integer;
    function GetHandle(): MySQLConsts.MYSQL_RES;
  protected
    FCommandText: string;
    FCommandType: TCommandType;
    FDatabaseName: string;
    FTableName: string;
    SynchroThread: TMySQLConnection.TSynchroThread;
    function AllocRecordBuffer(): TRecordBuffer; override;
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetCanModify(): Boolean; override;
    function GetFieldData(const Field: TField; const Buffer: Pointer; const Data: PRecordBufferData): Boolean; overload; virtual;
    function GetLibLengths(): MYSQL_LENGTHS; virtual;
    function GetLibRow(): MYSQL_ROW; virtual;
    function GetRecNo(): Integer; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount(): Integer; override;
    function GetUniDirectional(): Boolean; virtual;
    procedure InternalClose(); override;
    procedure InternalHandleException(); override;
    procedure InternalInitFieldDefs(); override;
    procedure InternalOpen(); override;
    function IsCursorOpen(): Boolean; override;
    procedure SetActive(Value: Boolean); override;
    function SetActiveEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean; virtual;
    procedure SetCommandText(const ACommandText: string); virtual;
    procedure SetConnection(const AConnection: TMySQLConnection); virtual;
    procedure UpdateIndexDefs(); override;
    property Handle: MySQLConsts.MYSQL_RES read GetHandle;
    property IndexDefs: TIndexDefs read FIndexDefs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetAsString(const FieldNo: Integer): string; virtual;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure Open(const DataHandle: TMySQLConnection.TDataResult); overload; virtual;
    function SQLFieldValue(const Field: TField; Data: PRecordBufferData = nil): string; overload; virtual;
    property DatabaseName: string read FDatabaseName;
    property LibLengths: MYSQL_LENGTHS read GetLibLengths;
    property LibRow: MYSQL_ROW read GetLibRow;
    property RowsAffected: Integer read FRowsAffected;
    property TableName: string read FTableName;
    property UniDirectional: Boolean read GetUniDirectional;
    property Warnings: Integer read FWarningCount;
  published
    property CommandText: string read FCommandText write SetCommandText;
    property CommandType: TCommandType read FCommandType;
    property Connection: TMySQLConnection read FConnection write SetConnection;
    property Active;
    property AfterClose;
    property AfterOpen;
    property AfterRefresh;
    property AfterScroll;
    property BeforeClose;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnFilterRecord;
    property OnNewRecord;
  end;

  TMySQLDataSet = class(TMySQLQuery)
  type
    TTextWidth = function (const Text: string): Integer of object;
    PInternRecordBuffer = ^TInternRecordBuffer;
    TInternRecordBuffer = record
      NewData: TMySQLQuery.PRecordBufferData;
      OldData: TMySQLQuery.PRecordBufferData;
      VisibleInFilter: Boolean;
    end;
    PExternRecordBuffer = ^TExternRecordBuffer;
    TExternRecordBuffer = record
      InternRecordBuffer: PInternRecordBuffer;
      RecNo: Integer;
      BookmarkFlag: TBookmarkFlag;
    end;
    TInternRecordBuffers = class(TList)
    private
      FDataSet: TMySQLDataSet;
      FRecordReceived: TEvent;
      function Get(Index: Integer): PInternRecordBuffer; inline;
      procedure Put(Index: Integer; Buffer: PInternRecordBuffer); inline;
    public
      CriticalSection: TCriticalSection;
      FilteredRecordCount: Integer;
      Index: Integer;
      procedure Clear(); override;
      constructor Create(const ADataSet: TMySQLDataSet);
      destructor Destroy(); override;
      property Buffers[Index: Integer]: PInternRecordBuffer read Get write Put; default;
      property DataSet: TMySQLDataSet read FDataSet;
      property RecordReceived: TEvent read FRecordReceived;
    end;
  private
    BookmarkCounter: DWord;
    DeleteBookmarks: array of TBookmark;
    FCachedUpdates: Boolean;
    FCanModify: Boolean;
    FCursorOpen: Boolean;
    FDataSize: Int64;
    FilterParser: TExprParser;
    FLocateNext: Boolean;
    FReadOnly: Boolean;
    FRecordsReceived: TEvent;
    FSortDef: TIndexDef;
    InternRecordBuffers: TInternRecordBuffers;
    function AllocInternRecordBuffer(): PInternRecordBuffer;
    function BookmarkToInternBufferIndex(const Bookmark: TBookmark): Integer;
    procedure FreeInternRecordBuffer(const InternRecordBuffer: PInternRecordBuffer);
    procedure InternActivateFilter();
    function VisibleInFilter(const InternRecordBuffer: PInternRecordBuffer): Boolean;
  protected
    procedure ActivateFilter(); virtual;
    function AllocRecordBuffer(): TRecordBuffer; override;
    procedure DeactivateFilter(); virtual;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetCanModify(): Boolean; override;
    function GetIsIndexField(Field: TField): Boolean; override;
    function GetLibLengths(): MYSQL_LENGTHS; override;
    function GetLibRow(): MYSQL_ROW; override;
    function GetRecNo(): Integer; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount(): Integer; override;
    function GetUniDirectional(): Boolean; override;
    procedure InternAddRecord(const LibRow: MYSQL_ROW; const LibLengths: MYSQL_LENGTHS; const Index: Integer = -1); virtual;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel(); override;
    procedure InternalClose(); override;
    procedure InternalDelete(); override;
    procedure InternalFirst(); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalInsert(); override;
    procedure InternalLast(); override;
    procedure InternalOpen(); override;
    procedure InternalPost(); override;
    procedure InternalRefresh(); override;
    function InternalRefreshEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean; virtual;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen(): Boolean; override;
    procedure MoveRecordBufferData(var DestData: TMySQLQuery.PRecordBufferData; const SourceData: TMySQLQuery.PRecordBufferData);
    procedure SetActive(Value: Boolean); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(const Field: TField; const Buffer: Pointer; const Size: Integer); overload; virtual;
    procedure SetFieldsSortTag(); virtual;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetRecNo(Value: Integer); override;
    function SQLFieldValue(const Field: TField; Buffer: TRecordBuffer = nil): string; overload; virtual;
    function SQLTableClausel(): string; virtual;
    function SQLUpdate(Buffer: TRecordBuffer = nil): string; virtual;
    procedure UpdateIndexDefs(); override;
    property RecordsReceived: TEvent read FRecordsReceived;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure Delete(const Bookmarks: array of TBookmark); overload; virtual;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetMaxTextWidth(const Field: TField; const TextWidth: TTextWidth): Integer; virtual;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    procedure Sort(const ASortDef: TIndexDef); virtual;
    function SQLDelete(): string; virtual;
    function SQLInsert(): string; virtual;
    property DataSize: Int64 read FDataSize;
    property LocateNext: Boolean read FLocateNext write FLocateNext;
    property SortDef: TIndexDef read FSortDef;
  published
    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates default False;
    property AfterCancel;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterPost;
    property BeforeCancel;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforePost;
    property Filter;
    property Filtered;
    property FilterOptions;
    property OnDeleteError;
    property OnEditError;
    property OnPostError;
  end;

  TMySQLTable = class(TMySQLDataSet)
  private
    FAutomaticLoadNextRecords: Boolean;
    FLimit: Integer;
    FLimitedDataReceived: Boolean;
    FOffset: Integer;
  protected
    FFilterSQL: string;
    RequestedRecordCount: Integer;
    function GetCanModify(): Boolean; override;
    procedure InternalClose(); override;
    procedure InternalLast(); override;
    procedure InternalOpen(); override;
    procedure InternalRefresh(); override;
    procedure SetCommandText(const ACommandText: string); override;
    function SQLSelect(const IgnoreLimit: Boolean = False): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadNextRecords(const AllRecords: Boolean = False): Boolean; virtual;
    procedure Sort(const ASortDef: TIndexDef); override;
    property CommandText: string read FCommandText write SetCommandText;
    property LimitedDataReceived: Boolean read FLimitedDataReceived;
  published
    property AutomaticLoadNextRecords: Boolean read FAutomaticLoadNextRecords write FAutomaticLoadNextRecords default False;
    property FilterSQL: string read FFilterSQL write FFilterSQL;
    property Limit: Integer read FLimit write FLimit default 0;
    property Offset: Integer read FOffset write FOffset default 0;
  end;

  TMySQLBitField = class(TLargeintField)
  protected
    function GetAsString(): string; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
  end;

  TLargeWordField = class(TLargeintField)
  private
    FMaxValue: UInt64;
    FMinValue: UInt64;
    procedure CheckRange(Value, Min, Max: UInt64);
  protected
    function GetAsString(): string; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsString(const Value: string); override;
  published
    property MinValue: UInt64 read FMinValue write FMinValue default 0;
    property MaxValue: UInt64 read FMaxValue write FMaxValue default 0;
  end;

  TMySQLWideStringField = class(TWideStringField)
  protected
    function GetAsDateTime(): TDateTime; override;
    procedure SetAsDateTime(Value: TDateTime); override;
  end;

  TMySQLDateField = class(TDateField)
  private
    ZeroDateString: string;
  protected
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
  end;

  TMySQLDateTimeField = class(TDateTimeField)
  private
    ZeroDateString: string;
  protected
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
  end;

  TMySQLTimeField = class(TIntegerField)
  protected
    SQLFormat: string;
    function GetAsDateTime(): TDateTime; override;
    function GetAsString(): string; override;
    function GetAsVariant(): Variant; override;
    function GetDataSize(): Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TMySQLTimeStampField = class(TSQLTimeStampField)
  protected
    SQLFormat: string;
    function GetAsSQLTimeStamp(): TSQLTimeStamp; override;
    function GetAsVariant(): Variant; override;
    function GetDataSize(): Integer; override;
    function GetOldValue(): Variant;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure SetDataSet(ADataSet: TDataSet); override;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TMySQLBlobField = class(TBlobField)
  protected
    function GetAsVariant(): Variant; override;
    procedure SetAsString(const Value: string); override;
  end;

  TMySQLWideMemoField = class(TWideMemoField)
  public
    function GetAsVariant: Variant; override;
    procedure SetAsString(const Value: string); override;
  end;

const
  MySQLZeroDate = -693593;

  ftAscSortedField  = $10000;
  ftDescSortedField = $20000;
  ftSortedField     = $30000;
  ftBitField        = $40000;
  ftGeometryField   = $80000;

function BitField(const Field: TField): Boolean;
function DateTimeToStr(const DateTime: TDateTime; const FormatSettings: TFormatSettings): string; overload;
function DateToStr(const Date: TDateTime; const FormatSettings: TFormatSettings): string; overload;
function ExecutionTimeToStr(const Time: TDateTime; const Digits: Byte = 2): string;
function GetZeroDateString(const FormatSettings: TFormatSettings): string;
function GeometryField(const Field: TField): Boolean;
procedure MySQLConnectionSynchronize(const Data: Pointer);
function StrToDate(const S: string; const FormatSettings: TFormatSettings): TDateTime; overload;
function StrToDateTime(const S: string; const FormatSettings: TFormatSettings): TDateTime; overload;
function SQLFormatToDisplayFormat(const SQLFormat: string): string;

const
  UnquotedDataTypes = [ftShortInt, ftByte, ftSmallInt, ftWord, ftInteger, ftLongWord, ftLargeint, ftSingle, ftFloat, ftExtended];
  BinaryDataTypes = [ftString, ftBlob];
  TextDataTypes = [ftWideString, ftWideMemo];

var
  LocaleFormatSettings: TFormatSettings;
  MySQLConnectionOnSynchronize: TMySQLConnection.TSynchronizeEvent;

implementation {***************************************************************}

uses
  DBConsts, Forms, Variants, DateUtils, Registry, ActiveX,
  RTLConsts, Consts, SysConst, Masks, Controls, Math, StrUtils,
  UInt64Lib,
  MySQLClient,
  CSVUtils, SQLUtils, HTTPTunnel;

resourcestring
  SNoConnection = 'Database not connected';
  SInUse = 'Connection in use';
  SInvalidBuffer = 'Invalid buffer';
  SLibraryNotAvailable = 'Library can not be loaded ("%s")';
  SOutOfSync = 'Thread synchronization error';
  SWrongDataSet = 'Field doesn''t attached to a "%s" DataSet';
  CR_SET_NAMES_MSG = 'SET NAMES / SET CHARACTER SET statements are not supported';
  CR_SERVER_OLD_MSG = 'Require MySQL Server 3.23.20 or higher';

const
  // Field mappings needed for filtering. (What field type should be compared with what internal type).
  FldTypeMap: TFieldMap = (
    { ftUnknown          }  Ord(ftUnknown),
    { ftString           }  Ord(ftString),
    { ftSmallInt         }  Ord(ftSmallInt),
    { ftInteger          }  Ord(ftInteger),
    { ftWord             }  Ord(ftWord),
    { ftBoolean          }  Ord(ftBoolean),
    { ftFloat            }  Ord(ftFloat),
    { ftCurrency         }  Ord(ftFloat),
    { ftBCD              }  Ord(ftBCD),
    { ftDate             }  Ord(ftDate),
    { ftTime             }  Ord(ftTime),
    { ftDateTime         }  Ord(ftDateTime),
    { ftBytes            }  Ord(ftBytes),
    { ftVarBytes         }  Ord(ftVarBytes),
    { ftAutoInc          }  Ord(ftInteger),
    { fBlob              }  Ord(ftBlob),
    { ftMemo             }  Ord(ftBlob),
    { ftGraphic          }  Ord(ftBlob),
    { ftFmtMemo          }  Ord(ftBlob),
    { ftParadoxOle       }  Ord(ftBlob),
    { ftDBaseOle         }  Ord(ftBlob),
    { ftTypedBinary      }  Ord(ftBlob),
    { ftCursor           }  Ord(ftUnknown),
    { ftFixedChar        }  Ord(ftString),
    { ftWideString       }  Ord(ftWideString),
    { ftLargeInt         }  Ord(ftLargeInt),
    { ftADT              }  Ord(ftADT),
    { ftArray            }  Ord(ftArray),
    { ftReference        }  Ord(ftUnknown),
    { ftDataset          }  Ord(ftUnknown),
    { ftOraBlob          }  Ord(ftBlob),
    { ftOraClob          }  Ord(ftBlob),
    { ftVariant          }  Ord(ftUnknown),
    { ftInterface        }  Ord(ftUnknown),
    { ftIDispatch        }  Ord(ftUnknown),
    { ftGUID             }  Ord(ftGUID),
    { ftTimeStamp        }  Ord(ftTimeStamp),
    { ftFmtBCD           }  Ord(ftFmtBCD),
    { ftFixedWideChar    }  Ord(ftFixedWideChar),
    { ftWideMemo         }  Ord(ftWideMemo),
    { ftOraTimeStamp     }  Ord(ftOraTimeStamp),
    { ftOraInterval      }  Ord(ftOraInterval),
    { ftLongWord         }  Ord(ftLongWord),
    { ftShortint         }  Ord(ftShortint),
    { ftByte             }  Ord(ftByte),
    { ftExtended         }  Ord(ftExtended),
    { ftConnection       }  Ord(ftConnection),
    { ftParams           }  Ord(ftParams),
    { ftStream           }  Ord(ftStream),
    { ftTimeStampOffset  }  Ord(ftTimeStampOffset),
    { ftObject           }  Ord(ftObject),
    { ftSingle           }  Ord(ftSingle)
  );
                                                                            
type
  TMySQLQueryBlobStream = class(TMemoryStream)
  public
    constructor Create(const AField: TBlobField);
    function Write(const Buffer; Len: Integer): Longint; override;
    procedure WriteBuffer(const Buffer; Count: Longint);
  end;

  TMySQLQueryMemoStream = TMySQLQueryBlobStream;

  TMySQLDataSetBlobStream = class(TMemoryStream)
  private
    Empty: Boolean;
    Field: TBlobField;
    Mode: TBlobStreamMode;
  public
    constructor Create(const AField: TBlobField; AMode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Len: Integer): Longint; override;
  end;

var
  MySQLLibraries: array of TMySQLLibrary;

{******************************************************************************}

var
  SynchronizingThreadsCS: TCriticalSection;
  SynchronizingThreads: TList;

function BitField(const Field: TField): Boolean;
begin
  Result := Assigned(Field) and (Field.Tag and ftBitField <> 0);
end;

function DateTimeToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;
begin
  if (Trunc(DateTime) <= MySQLZeroDate) then
    Result := GetZeroDateString(FormatSettings) + ' ' + TimeToStr(DateTime, FormatSettings)
  else
    Result := SysUtils.DateTimeToStr(DateTime, FormatSettings);
end;

function DateToStr(const Date: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;
begin
  if (Trunc(Date) <= MySQLZeroDate) then
    Result := GetZeroDateString(FormatSettings)
  else
    Result := SysUtils.DateToStr(Date, FormatSettings);
end;

function DisplayFormatToSQLFormat(const DateTimeFormat: string): string;
var
  Index: Integer;
begin
  Result := DateTimeFormat;

  while (Pos('mm', Result) > 0) do
  begin
    Index := Pos('mm', Result);
    Result[Index] := '%';
    if ((Index > 1) and (Result[Index - 1] = 'h')) then
      Result[Index + 1] := 'i'
    else
      Result[Index + 1] := 'm';
  end;

  Result := ReplaceStr(Result, 'yyyy', '%Y');
  Result := ReplaceStr(Result, 'yy', '%y');
  Result := ReplaceStr(Result, 'MM', '%m');
  Result := ReplaceStr(Result, 'dd', '%d');
  Result := ReplaceStr(Result, 'hh', '%H');
  Result := ReplaceStr(Result, 'ss', '%s');
end;

function ExecutionTimeToStr(const Time: TDateTime; const Digits: Byte = 2): string;
var
  Hour: Word;
  Minute: Word;
  MSec: Word;
  Second: Word;
begin
  if (Time >= 1) then
    Result := IntToStr(Trunc(Time)) + ' days'
  else
  begin
    DecodeTime(Time, Hour, Minute, Second, MSec);
    if (Time < 0) then
      Result := '???'
    else if ((Hour > 0) or (Minute > 0)) then
      Result := TimeToStr(Time, LocaleFormatSettings)
    else
      Result := Format('%2.' + IntToStr(Digits) + 'f', [Second + MSec / 1000]);
  end;
end;

procedure FreeMySQLLibraries();
var
  I: Integer;
begin
  for I := 0 to Length(MySQLLibraries) - 1 do
    MySQLLibraries[I].Free();
  SetLength(MySQLLibraries, 0);
end;

function GeometryField(const Field: TField): Boolean;
begin
  Result := Assigned(Field) and (Field.Tag and ftGeometryField <> 0);
end;

function GetZeroDateString(const FormatSettings: TFormatSettings): string;
begin
  Result := FormatSettings.ShortDateFormat;
  Result := ReplaceStr(Result, 'Y', '0');
  Result := ReplaceStr(Result, 'y', '0');
  Result := ReplaceStr(Result, 'M', '0');
  Result := ReplaceStr(Result, 'm', '0');
  Result := ReplaceStr(Result, 'D', '0');
  Result := ReplaceStr(Result, 'd', '0');
  Result := ReplaceStr(Result, 'e', '0');
  Result := ReplaceStr(Result, '/', FormatSettings.DateSeparator);
end;

function LoadMySQLLibrary(const ALibraryType: TMySQLLibrary.TLibraryType; const AFileName: TFileName): TMySQLLibrary;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(MySQLLibraries) - 1 do
    if ((MySQLLibraries[I].LibraryType = ALibraryType) and (MySQLLibraries[I].FileName = AFileName)) then
      Result := MySQLLibraries[I];

  if (not Assigned(Result)) then
  begin
    Result := TMySQLLibrary.Create(ALibraryType, AFileName);
    if (Result.Version = 0) then
      FreeAndNil(Result);

    if (Assigned(Result)) then
    begin
      SetLength(MySQLLibraries, Length(MySQLLibraries) + 1);
      MySQLLibraries[Length(MySQLLibraries) - 1] := Result;
    end;
  end;
end;

procedure MySQLConnectionSynchronize(const Data: Pointer);
var
  Index: Integer;
  SynchroThread: TMySQLConnection.TSynchroThread;
begin
  SynchroThread := TMySQLConnection.TSynchroThread(Data);

  SynchronizingThreadsCS.Enter();
  Index := SynchronizingThreads.IndexOf(SynchroThread);
  if (Index < 0) then
    SynchroThread := nil
  else
    SynchronizingThreads.Delete(Index);
  SynchronizingThreadsCS.Leave();

  if (Assigned(SynchroThread)) then
    SynchroThread.Synchronize();
end;

procedure MySQLConnectionSynchronizeRequest(const SynchroThread: TMySQLConnection.TSynchroThread);
begin
  if (not Assigned(MySQLConnectionOnSynchronize)) then
    SynchroThread.Terminate()
  else
  begin
    SynchronizingThreadsCS.Enter();
    SynchronizingThreads.Add(SynchroThread);
    MySQLConnectionOnSynchronize(SynchroThread);
    SynchronizingThreadsCS.Leave();
  end;
end;

function MySQLTimeStampToStr(const SQLTimeStamp: TSQLTimeStamp; const DisplayFormat: string): string;
var
  I: Integer;
begin
  Result := LowerCase(DisplayFormat);

  for I := Length(DisplayFormat) - 1 downto 0 do
  begin
    if (DisplayFormat[I] = 'c') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.ShortDateFormat, Result, I);
    end;
    if (Copy(DisplayFormat, I, 2) = 'tt') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.LongTimeFormat, Result, I);
    end;
    if (DisplayFormat[I] = 't') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.ShortTimeFormat, Result, I);
    end;
    if (DisplayFormat[I] = '/') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.DateSeparator, Result, I);
    end;
    if (DisplayFormat[I] = ':') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.TimeSeparator, Result, I);
    end;
  end;

  for I := Length(Result) downto 1 do
    if (Result[I] = 'm') then
      if ((I > 1) and (Result[I - 1] = 'm')) then
      else if (((I > 1) and (Result[I - 1] = 'h')) or ((I > 2) and (Result[I - 1] = FormatSettings.TimeSeparator) and (Result[I - 2] = 'h'))) then
        if ((I < Length(Result)) and (Result[I + 1] = 'm')) then
          begin Delete(Result, I, 2); Insert(ReplaceStr(Format('%2d', [SQLTimeStamp.Minute]), ' ', '0'), Result, I); end
        else
          begin Delete(Result, I, 1); Insert(IntToStr(SQLTimeStamp.Minute), Result, I); end;

  while (Pos('yyyy', Result) > 0) do
    Result := ReplaceStr(Result, 'yyyy', ReplaceStr(Format('%4d', [SQLTimeStamp.Year]), ' ', '0'));
  while (Pos('yy', Result) > 0) do
    Result := ReplaceStr(Result, 'yy', ReplaceStr(Format('%2d', [SQLTimeStamp.Year mod 100]), ' ', '0'));
  while (Pos('mm', Result) > 0) do
    Result := ReplaceStr(Result, 'mm', ReplaceStr(Format('%2d', [SQLTimeStamp.Month]), ' ', '0'));
  while (Pos('dd', Result) > 0) do
    Result := ReplaceStr(Result, 'dd', ReplaceStr(Format('%2d', [SQLTimeStamp.Day]), ' ', '0'));
  while (Pos('d', Result) > 0) do
    Result := ReplaceStr(Result, 'd', IntToStr(SQLTimeStamp.Day));
  while (Pos('hh', Result) > 0) do
    Result := ReplaceStr(Result, 'hh', ReplaceStr(Format('%2d', [SQLTimeStamp.Hour]), ' ', '0'));
  while (Pos('h', Result) > 0) do
    Result := ReplaceStr(Result, 'h', IntToStr(SQLTimeStamp.Hour));
  while (Pos('ss', Result) > 0) do
    Result := ReplaceStr(Result, 'ss', ReplaceStr(Format('%2d', [SQLTimeStamp.Second]), ' ', '0'));
  while (Pos('s', Result) > 0) do
    Result := ReplaceStr(Result, 's', IntToStr(SQLTimeStamp.Second));
end;

function StrToDate(const S: string;
  const FormatSettings: TFormatSettings): TDateTime; overload;
begin
  if (S = GetZeroDateString(FormatSettings)) then
    Result := MySQLZeroDate
  else
    Result := SysUtils.StrToDate(S, FormatSettings);
end;

function StrToDateTime(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
var
  ZeroDateString: string;
begin
  ZeroDateString := GetZeroDateString(FormatSettings);

  if (Copy(S, 1, Length(ZeroDateString)) = ZeroDateString) then
    Result := MySQLZeroDate - StrToTime(Copy(S, Length(ZeroDateString) + 1, Length(S) - Length(ZeroDateString)), FormatSettings)
  else
    Result := SysUtils.StrToDateTime(S, FormatSettings);
end;

function StrToMySQLTimeStamp(const Str: string; const SQLFormat: string): TSQLTimeStamp;
var
  I: Integer;
  Pos: Integer;
  SQLTimeStamp: TSQLTimeStamp;
begin
  SQLTimeStamp := NullSQLTimeStamp;

  I := 0; Pos := 1;
  while ((Pos <= Length(Str)) and (I < Length(SQLFormat) - 1)) do
  begin
    if (SQLFormat[I + 1] <> '%') then
      Inc(Pos, 1)
    else
    begin
      case (SQLFormat[I + 2]) of
        'Y': begin SQLTimeStamp.Year := StrToInt(Copy(Str, Pos, 4)); Inc(Pos, 4); end;
        'y': begin
               SQLTimeStamp.Year := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2);
               if (SQLTimeStamp.Year <= 69) then
                 Inc(SQLTimeStamp.Year, 2000)
               else
                 Inc(SQLTimeStamp.Year, 1900);
             end;
        'm': begin SQLTimeStamp.Month := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        'd': begin SQLTimeStamp.Day := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        'H': begin SQLTimeStamp.Hour := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        'i': begin SQLTimeStamp.Minute := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        's': begin SQLTimeStamp.Second := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        else raise EConvertError.CreateFMT(SConvUnknownType, [SQLFormat[I + 2]]);
      end;
      Inc(I);
    end;
    Inc(I);
  end;

  Result := SQLTimeStamp;
end;

function StrToTime(const S, SQLFormat: string): Integer; overload;
var
  B: Byte;
  DisplayFormat: string;
  L: Byte;
  Str: string;
begin
  Result := 0;
  Str := S;
  if (S[1] = '-') then Delete(Str, 1, 1);

  DisplayFormat := SQLFormatToDisplayFormat(SQLFormat);
  DisplayFormat := ReplaceStr(DisplayFormat, 'hh', StringOfChar('h', Length(Str) - Length(DisplayFormat) + 2));

  if (Pos('ss', DisplayFormat) > 0) then
  begin
    B := StrToInt(Copy(Str, Pos('ss', DisplayFormat), 2));
    if (B >= 60) then
      raise EConvertError.CreateFmt(SInvalidTime, [S]);
    Result := Result + B;
  end;
  if (Pos('mm', DisplayFormat) > 0) then
  begin
    B := StrToInt(Copy(Str, Pos('mm', DisplayFormat), 2));
    if (B >= 60) then
      raise EConvertError.CreateFmt(SInvalidTime, [S]);
    Result := Result + B * 60;
  end;
  if (Pos('hh', DisplayFormat) > 0) then
  begin
    L := 2;
    while (DisplayFormat[Pos('hh', DisplayFormat) + L] = 'h') do Inc(L);
    Result := Result + StrToInt(Copy(Str, Pos('hh', DisplayFormat), L)) * 3600;
  end;
  if (S[1] = '-') then
    Result := - Result;

  if ((Result <= -3020400) or (3020400 <= Result))  then
    raise EConvertError.CreateFmt(SInvalidTime, [S]);
end;

function SQLFormatToDisplayFormat(const SQLFormat: string): string;
begin
  Result := SQLFormat;
  Result := ReplaceStr(Result, '%Y', 'yyyy');
  Result := ReplaceStr(Result, '%y', 'yy');
  Result := ReplaceStr(Result, '%M', 'mmmm');
  Result := ReplaceStr(Result, '%m', 'mm');
  Result := ReplaceStr(Result, '%c', 'm');
  Result := ReplaceStr(Result, '%D', 'dddd');
  Result := ReplaceStr(Result, '%d', 'dd');
  Result := ReplaceStr(Result, '%e', 'd');
  Result := ReplaceStr(Result, '%h', 'h');
  Result := ReplaceStr(Result, '%H', 'hh');
  Result := ReplaceStr(Result, '%p', 'x');
  Result := ReplaceStr(Result, '%i', 'mm');
  Result := ReplaceStr(Result, '%k', 'hh');
  Result := ReplaceStr(Result, '%l', 'n');
  Result := ReplaceStr(Result, '%S', 'ss');
  Result := ReplaceStr(Result, '%s', 'ss');
end;

function TimeToStr(const Time: Integer; const SQLFormat: string): string; overload;
var
  S: string;
  T: Integer;
begin
  T := Time;

  Result := SQLFormatToDisplayFormat(SQLFormat);

  if (T < 0) then
  begin
    T := -T;
    Result := ReplaceStr(Result, 'hh', '-hh');
  end;

  S := IntToStr(T mod 60); if (Length(S) = 1) then S := '0' + S; Result := ReplaceStr(Result, 'ss', S); T := T div 60;
  S := IntToStr(T mod 60); if (Length(S) = 1) then S := '0' + S; Result := ReplaceStr(Result, 'mm', S); T := T div 60;
  S := IntToStr(T       ); if (Length(S) = 1) then S := '0' + S; Result := ReplaceStr(Result, 'hh', S);
end;

{ Callback functions **********************************************************}

procedure local_infile_end(local_infile: TMySQLConnection.Plocal_infile); cdecl;
begin
  local_infile^.Connection.local_infile_end(local_infile);
end;

function local_infile_error(local_infile: TMySQLConnection.Plocal_infile; error_msg: my_char; error_msg_len: my_uint): my_int; cdecl;
begin
  Result := local_infile^.Connection.local_infile_error(local_infile, error_msg, error_msg_len);
end;

function local_infile_init(var local_infile: TMySQLConnection.Plocal_infile; filename: my_char; userdata: Pointer): my_int; cdecl;
begin
  Result := TMySQLConnection(userdata).local_infile_init(local_infile, filename);
end;

function local_infile_read(local_infile: TMySQLConnection.Plocal_infile; buf: my_char; buf_len: my_uint): my_int; cdecl;
begin
  Result := local_infile^.Connection.local_infile_read(local_infile, buf, buf_len);
end;

function TMySQLLibrary.GetVersionStr(): string;
begin
  Result := string(mysql_get_client_info());
end;

{ TMySQLLibrary ***************************************************************}

constructor TMySQLLibrary.Create(const ALibraryType: TLibraryType; const AFilename: TFileName);
var
  Code: Integer;
  S: string;
begin
  inherited Create();

  FHandle := 0;
  FLibraryType := ALibraryType;
  FFilename := AFilename;

  if (LibraryType = ltDLL) then
  begin
    Assert(FFilename <> '');

    FHandle := LoadLibrary(PChar(FFilename));

    if (Handle > 0) then
    begin
      my_init := GetProcAddress(Handle, 'my_init');
      mysql_affected_rows := GetProcAddress(Handle, 'mysql_affected_rows');
      mysql_character_set_name := GetProcAddress(Handle, 'mysql_character_set_name');
      mysql_close := GetProcAddress(Handle, 'mysql_close');
      mysql_errno := GetProcAddress(Handle, 'mysql_errno');
      mysql_error := GetProcAddress(Handle, 'mysql_error');
      mysql_fetch_field := GetProcAddress(Handle, 'mysql_fetch_field');
      mysql_fetch_fields := GetProcAddress(Handle, 'mysql_fetch_fields');
      mysql_fetch_field_direct := GetProcAddress(Handle, 'mysql_fetch_field_direct');
      mysql_fetch_lengths := GetProcAddress(Handle, 'mysql_fetch_lengths');
      mysql_fetch_row := GetProcAddress(Handle, 'mysql_fetch_row');
      mysql_field_count := GetProcAddress(Handle, 'mysql_field_count');
      mysql_free_result := GetProcAddress(Handle, 'mysql_free_result');
      mysql_get_client_info := GetProcAddress(Handle, 'mysql_get_client_info');
      mysql_get_client_version := GetProcAddress(Handle, 'mysql_get_client_version');
      mysql_get_host_info := GetProcAddress(Handle, 'mysql_get_host_info');
      mysql_get_server_info := GetProcAddress(Handle, 'mysql_get_server_info');
      mysql_get_server_version := GetProcAddress(Handle, 'mysql_get_server_version');
      mysql_info := GetProcAddress(Handle, 'mysql_info');
      mysql_init := GetProcAddress(Handle, 'mysql_init');
      mysql_insert_id := GetProcAddress(Handle, 'mysql_insert_id');
      mysql_more_results := GetProcAddress(Handle, 'mysql_more_results');
      mysql_next_result := GetProcAddress(Handle, 'mysql_next_result');
      mysql_library_end := GetProcAddress(Handle, 'mysql_library_end');
      mysql_library_init := GetProcAddress(Handle, 'mysql_library_init');
      mysql_num_fields := GetProcAddress(Handle, 'mysql_num_fields');
      mysql_num_rows := GetProcAddress(Handle, 'mysql_num_rows');
      mysql_options := GetProcAddress(Handle, 'mysql_options');
      mysql_ping := GetProcAddress(Handle, 'mysql_ping');
      mysql_real_connect := GetProcAddress(Handle, 'mysql_real_connect');
      mysql_real_escape_string := GetProcAddress(Handle, 'mysql_real_escape_string');
      mysql_real_query := GetProcAddress(Handle, 'mysql_real_query');
      mysql_set_character_set := GetProcAddress(Handle, 'mysql_set_character_set');
      mysql_set_local_infile_default := GetProcAddress(Handle, 'mysql_set_local_infile_default');
      mysql_set_local_infile_handler := GetProcAddress(Handle, 'mysql_set_local_infile_handler');
      mysql_set_server_option := GetProcAddress(Handle, 'mysql_set_server_option');
      mysql_shutdown := GetProcAddress(Handle, 'mysql_shutdown');
      mysql_store_result := GetProcAddress(Handle, 'mysql_store_result');
      mysql_thread_end := GetProcAddress(Handle, 'mysql_thread_end');
      mysql_thread_id := GetProcAddress(Handle, 'mysql_thread_id');
      mysql_thread_init := GetProcAddress(Handle, 'mysql_thread_init');
      mysql_thread_save := GetProcAddress(Handle, 'mysql_thread_save');
      mysql_use_result := GetProcAddress(Handle, 'mysql_use_result');
      mysql_warning_count := GetProcAddress(Handle, 'mysql_warning_count');
    end;
  end
  else
  begin
    my_init := nil;
    mysql_affected_rows := Tmysql_affected_rows(@MySQLClient.mysql_affected_rows);
    mysql_character_set_name := Tmysql_character_set_name(@MySQLClient.mysql_character_set_name);
    mysql_close := Tmysql_close(@MySQLClient.mysql_close);
    mysql_errno := Tmysql_errno(@MySQLClient.mysql_errno);
    mysql_error := Tmysql_error(@MySQLClient.mysql_error);
    mysql_fetch_field := Tmysql_fetch_field(@MySQLClient.mysql_fetch_field);
    mysql_fetch_fields := nil; // Tmysql_fetch_fields(@MySQLClient.mysql_fetch_fields);
    mysql_fetch_field_direct := Tmysql_fetch_field_direct(@MySQLClient.mysql_fetch_field_direct);
    mysql_fetch_lengths := Tmysql_fetch_lengths(@MySQLClient.mysql_fetch_lengths);
    mysql_fetch_row := Tmysql_fetch_row(@MySQLClient.mysql_fetch_row);
    mysql_field_count := Tmysql_field_count(@MySQLClient.mysql_field_count);
    mysql_free_result := Tmysql_free_result(@MySQLClient.mysql_free_result);
    mysql_get_client_info := Tmysql_get_client_info(@MySQLClient.mysql_get_client_info);
    mysql_get_client_version := Tmysql_get_client_version(@MySQLClient.mysql_get_client_version);
    mysql_get_host_info := Tmysql_get_host_info(@MySQLClient.mysql_get_host_info);
    mysql_get_server_info := Tmysql_get_server_info(@MySQLClient.mysql_get_server_info);
    mysql_get_server_status := Tmysql_get_server_status(@MySQLClient.mysql_get_server_status);
    mysql_get_server_version := Tmysql_get_server_version(@MySQLClient.mysql_get_server_version);
    mysql_info := Tmysql_info(@MySQLClient.mysql_info);
    if (LibraryType <> ltHTTP) then
      mysql_init := Tmysql_init(@MySQLClient.mysql_init)
    else
      mysql_init := Tmysql_init(@HTTPTunnel.mysql_init);
    mysql_insert_id := Tmysql_insert_id(@MySQLClient.mysql_insert_id);
    mysql_more_results := Tmysql_more_results(@MySQLClient.mysql_more_results);
    mysql_next_result := Tmysql_next_result(@MySQLClient.mysql_next_result);
    mysql_library_end := nil;
    mysql_library_init := nil;
    mysql_num_fields := Tmysql_num_fields(@MySQLClient.mysql_num_fields);
    mysql_num_rows := Tmysql_num_rows(@MySQLClient.mysql_num_rows);
    mysql_options := Tmysql_options(@MySQLClient.mysql_options);
    if (LibraryType = ltHTTP) then
      mysql_ping := nil
    else
      mysql_ping := Tmysql_ping(@MySQLClient.mysql_ping);
    mysql_real_connect := Tmysql_real_connect(@MySQLClient.mysql_real_connect);
    mysql_real_escape_string := Tmysql_real_escape_string(@MySQLClient.mysql_real_escape_string);
    mysql_real_query := Tmysql_real_query(@MySQLClient.mysql_real_query);
    mysql_set_character_set := Tmysql_set_character_set(@MySQLClient.mysql_set_character_set);
    mysql_set_local_infile_default := Tmysql_set_local_infile_default(@MySQLClient.mysql_set_local_infile_default);
    mysql_set_local_infile_handler := Tmysql_set_local_infile_handler(@MySQLClient.mysql_set_local_infile_handler);
    mysql_set_server_option := Tmysql_set_server_option(@MySQLClient.mysql_set_server_option);
    if (LibraryType <> ltHTTP) then
      mysql_shutdown := Tmysql_shutdown(@MySQLClient.mysql_shutdown)
    else
      mysql_shutdown := nil;
    mysql_store_result := Tmysql_store_result(@MySQLClient.mysql_store_result);
    mysql_thread_end := nil;
    mysql_thread_id := Tmysql_thread_id(@MySQLClient.mysql_thread_id);
    mysql_thread_init := nil;
    mysql_thread_save := nil;
    mysql_use_result := Tmysql_use_result(@MySQLClient.mysql_use_result);
    mysql_warning_count := Tmysql_warning_count(@MySQLClient.mysql_warning_count);
  end;

  if (Assigned(mysql_library_init)) then
    mysql_library_init(0, nil, nil);

  if (Assigned(mysql_get_client_version)) then
    FVersion := mysql_get_client_version()
  else if (Assigned(mysql_get_client_info)) then
  begin
    S := string(mysql_get_client_info());
    if (Pos('-', S) > 0) then
      S := Copy(S, 1, Pos('-', S) - 1);
    if (S[2] = '.') and (S[4] = '.') then
      Insert('0', S, 3);
    if (S[2] = '.') and (Length(S) = 6) then
      Insert('0', S, 6);
    Val(StringReplace(S, '.', '', [rfReplaceAll	]), FVersion, Code);
  end;
end;

destructor TMySQLLibrary.Destroy();
begin
  if (Assigned(mysql_library_end)) then
    mysql_library_end();
  if (Handle > 0) then
    FreeLibrary(Handle);

  inherited;
end;

function TMySQLLibrary.Field(const RawField: MYSQL_FIELD): TMYSQL_FIELD;
begin
  if (Version >= 40101) then
  begin
    Result.name := MYSQL_FIELD_40101(RawField)^.name;
    Result.org_name := MYSQL_FIELD_40101(RawField)^.org_name;
    Result.table := MYSQL_FIELD_40101(RawField)^.table;
    Result.org_table := MYSQL_FIELD_40101(RawField)^.org_table;
    Result.db := MYSQL_FIELD_40101(RawField)^.db;
    Result.catalog := MYSQL_FIELD_40101(RawField)^.catalog;
    Result.def := MYSQL_FIELD_40101(RawField)^.def;
    Result.length := MYSQL_FIELD_40101(RawField)^.length;
    Result.max_length := MYSQL_FIELD_40101(RawField)^.max_length;
    Result.name_length := MYSQL_FIELD_40101(RawField)^.name_length;
    Result.org_name_length := MYSQL_FIELD_40101(RawField)^.org_name_length;
    Result.table_length := MYSQL_FIELD_40101(RawField)^.table_length;
    Result.org_table_length := MYSQL_FIELD_40101(RawField)^.org_table_length;
    Result.db_length := MYSQL_FIELD_40101(RawField)^.db_length;
    Result.catalog_length := MYSQL_FIELD_40101(RawField)^.catalog_length;
    Result.def_length := MYSQL_FIELD_40101(RawField)^.def_length;
    Result.flags := MYSQL_FIELD_40101(RawField)^.flags;
    Result.decimals := MYSQL_FIELD_40101(RawField)^.decimals;
    Result.charsetnr := MYSQL_FIELD_40101(RawField)^.charsetnr;
    Result.field_type := MYSQL_FIELD_40101(RawField)^.field_type;
  end
  else if (Version >= 40100) then
  begin
    Result.name := MYSQL_FIELD_40100(RawField)^.name;
    Result.org_name := MYSQL_FIELD_40100(RawField)^.org_name;
    Result.table := MYSQL_FIELD_40100(RawField)^.table;
    Result.org_table := MYSQL_FIELD_40100(RawField)^.org_table;
    Result.db := MYSQL_FIELD_40100(RawField)^.db;
    Result.catalog := nil;
    Result.def := MYSQL_FIELD_40100(RawField)^.def;
    Result.length := MYSQL_FIELD_40100(RawField)^.length;
    Result.max_length := MYSQL_FIELD_40100(RawField)^.max_length;
    Result.name_length := MYSQL_FIELD_40100(RawField)^.name_length;
    Result.org_name_length := MYSQL_FIELD_40100(RawField)^.org_name_length;
    Result.table_length := MYSQL_FIELD_40100(RawField)^.table_length;
    Result.org_table_length := MYSQL_FIELD_40100(RawField)^.org_table_length;
    Result.db_length := MYSQL_FIELD_40100(RawField)^.db_length;
    Result.catalog_length := 0;
    Result.def_length := MYSQL_FIELD_40100(RawField)^.def_length;
    Result.flags := MYSQL_FIELD_40100(RawField)^.flags;
    Result.decimals := MYSQL_FIELD_40100(RawField)^.decimals;
    Result.charsetnr := MYSQL_FIELD_40100(RawField)^.charsetnr;
    Result.field_type := MYSQL_FIELD_40100(RawField)^.field_type;
  end
  else if (Version >= 40000) then
  begin
    Result.name := MYSQL_FIELD_40000(RawField)^.name;
    Result.org_name := nil;
    Result.table := MYSQL_FIELD_40000(RawField)^.table;
    Result.org_table := MYSQL_FIELD_40000(RawField)^.org_table;
    if (Version = 40017) then
      Result.db := nil // libMySQL.dll 4.0.17 gibt db einen fehlerhaften Zeiger zurück.
    else
      Result.db := MYSQL_FIELD_40000(RawField)^.db;
    Result.catalog := nil;
    Result.def := MYSQL_FIELD_40000(RawField)^.def;
    Result.length := MYSQL_FIELD_40000(RawField)^.length;
    Result.max_length := MYSQL_FIELD_40000(RawField)^.max_length;
    Result.name_length := 0;
    Result.org_name_length := 0;
    Result.table_length := 0;
    Result.org_table_length := 0;
    Result.db_length := 0;
    Result.catalog_length := 0;
    Result.def_length := 0;
    Result.flags := MYSQL_FIELD_40000(RawField)^.flags;
    Result.decimals := MYSQL_FIELD_40000(RawField)^.decimals;
    Result.charsetnr := 0;
    Result.field_type := MYSQL_FIELD_40000(RawField)^.field_type;
  end
  else
  begin
    Result.name := MYSQL_FIELD_32300(RawField)^.name;
    Result.org_name := nil;
    Result.table := MYSQL_FIELD_32300(RawField)^.table;
    Result.org_table := nil;
    Result.db := nil;
    Result.catalog := nil;
    Result.def := MYSQL_FIELD_32300(RawField)^.def;
    Result.length := MYSQL_FIELD_32300(RawField)^.length;
    Result.max_length := MYSQL_FIELD_32300(RawField)^.max_length;
    Result.name_length := 0;
    Result.org_name_length := 0;
    Result.table_length := 0;
    Result.org_table_length := 0;
    Result.db_length := 0;
    Result.catalog_length := 0;
    Result.def_length := 0;
    Result.flags := MYSQL_FIELD_32300(RawField)^.flags;
    Result.decimals := MYSQL_FIELD_32300(RawField)^.decimals;
    Result.charsetnr := 0;
    Result.field_type := MYSQL_FIELD_32300(RawField)^.field_type;
  end
end;

{ EMySQLError *****************************************************************}

constructor EMySQLError.Create(const Msg: string; const AErrorCode: Integer; const AConnection: TMySQLConnection);
begin
  inherited Create(Msg);

  FConnection := AConnection;
  FErrorCode := AErrorCode;
end;

function TMySQLMonitor.GetCacheText(): string;
begin
  SetLength(Result, Cache.UsedSize);
  if (Cache.UsedSize > 0) then
  begin
    if (Cache.First + Cache.UsedSize <= Cache.MemSize) then
      MoveMemory(@Result[1], @Cache.Mem[Cache.First], Cache.UsedSize * SizeOf(Cache.Mem[0]))
    else
    begin
      MoveMemory(@Result[1], @Cache.Mem[Cache.First], (Cache.MemSize - Cache.First) * SizeOf(Cache.Mem[0]));
      MoveMemory(@Result[1 + Cache.MemSize - Cache.First], Cache.Mem, (Cache.UsedSize - (Cache.MemSize - Cache.First)) * SizeOf(Cache.Mem[0]));
    end;
    while ((Length(Result) > 0) and CharInSet(Result[Length(Result)], [#10,#13])) do
      SetLength(Result, Length(Result) - 1);
  end;
end;

procedure TMySQLMonitor.SetConnection(const AConnection: TMySQLConnection);
begin
  if (Assigned(Connection)) then
    Connection.UnRegisterSQLMonitor(Self);

  FConnection := AConnection;

  if (Assigned(Connection)) then
    Connection.RegisterSQLMonitor(Self);
end;

procedure TMySQLMonitor.SetCacheSize(const ACacheSize: Integer);
begin
  while (Cache.UsedSize > ACacheSize) do
  begin
    Cache.First := (Cache.First + Integer(Cache.Items[0])) mod Cache.MemSize;
    Dec(Cache.UsedSize, Integer(Cache.Items[0]));
    Cache.Items.Delete(0);
  end;

  MoveMemory(Cache.Mem, @Cache.Mem[Cache.First], Cache.UsedSize * SizeOf(Cache.Mem[0]));
  Cache.First := 0;

  Cache.MaxSize := ACacheSize;

  Cache.MemSize := Cache.MaxSize;
  ReallocMem(Cache.Mem, Cache.MemSize * SizeOf(Cache.Mem[0]));
end;

procedure TMySQLMonitor.SetOnMonitor(const AOnMonitor: TMySQLOnMonitor);
begin
  FOnMonitor := AOnMonitor;
end;

procedure TMySQLMonitor.DoMonitor(const Sender: TObject; const Text: PChar; const Length: Integer; const ATraceType: TTraceType);
var
  CacheLength: Integer;
  CacheText: PChar;
  Pos: Integer;
begin
  if ((Cache.MaxSize > 0) and (Length > 0)) then
  begin
    CacheText := Text; CacheLength := Length;
    while ((CacheLength > 0) and CharInSet(CacheText[0], [#9,#10,#13,' '])) do
    begin
      CacheText := @CacheText[1];
      Dec(CacheLength);
    end;

    while ((CacheLength > 0) and CharInSet(CacheText[CacheLength - 1], [#9,#10,#13,' ',';'])) do
      Dec(CacheLength);

    if (ATraceType in [ttRequest, ttResult]) then
      Inc(CacheLength, 3) // ';' + New Line
    else
      Inc(CacheLength, 2); // New Line

    if ((CacheLength > Cache.MemSize) or (Cache.MemSize > Cache.MaxSize)) then
    begin
      Clear();

      Cache.MemSize := Max(Cache.MaxSize, CacheLength);
      ReallocMem(Cache.Mem, Cache.MemSize * SizeOf(Cache.Mem[0]));
      MoveMemory(Cache.Mem, CacheText, CacheLength * SizeOf(Cache.Mem[0]));
    end
    else
    begin
      while ((Cache.UsedSize + CacheLength > Cache.MemSize) and (Cache.Items.Count > 0)) do
      begin
        Inc(Cache.First, Integer(Cache.Items[0])); if (Cache.First >= Cache.MemSize) then Dec(Cache.First, Cache.MemSize);
        Dec(Cache.UsedSize, Integer(Cache.Items[0]));
        Cache.Items.Delete(0);
      end;

      Pos := (Cache.First + Cache.UsedSize) mod Cache.MemSize;
      if (Pos + CacheLength <= Cache.MemSize) then
        MoveMemory(@Cache.Mem[Pos], CacheText, CacheLength * SizeOf(Cache.Mem[0]))
      else
      begin
        MoveMemory(@Cache.Mem[Pos], CacheText, (Cache.MemSize - Pos) * SizeOf(Cache.Mem[0]));
        MoveMemory(Cache.Mem, @CacheText[Cache.MemSize - Pos], (CacheLength - (Cache.MemSize - Pos)) * SizeOf(Cache.Mem[0]));
      end;
    end;

    Inc(Cache.UsedSize, CacheLength);
    Cache.Items.Add(Pointer(CacheLength));

    Pos := (Cache.First + Cache.UsedSize) mod Cache.MemSize;
    case (Pos) of
      0:
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Cache.MemSize - 3] := ';';
          Cache.Mem[Cache.MemSize - 2] := #13;
          Cache.Mem[Cache.MemSize - 1] := #10;
        end;
      1:
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Cache.MemSize - 2] := ';';
          Cache.Mem[Cache.MemSize - 1] := #13;
          Cache.Mem[0] := #10;
        end;
      2:
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Cache.MemSize - 1] := ';';
          Cache.Mem[1] := #13;
          Cache.Mem[0] := #10;
        end;
      else
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Pos - 3] := ';';
          Cache.Mem[Pos - 2] := #13;
          Cache.Mem[Pos - 1] := #10;
        end;
    end;
  end;

  if (Enabled and Assigned(OnMonitor)) then
    OnMonitor(Sender, Text, Length, ATraceType);
end;

{ TMySQLMonitor ***************************************************************}

procedure TMySQLMonitor.Clear();
begin
  Cache.First := 0;
  Cache.UsedSize := 0;
  Cache.Items.Clear();
end;

constructor TMySQLMonitor.Create(AOwner: TComponent);
begin
  inherited;

  FConnection := nil;
  FEnabled := False;
  FOnMonitor := nil;
  FCacheSize := 0;
  FTraceTypes := [ttRequest];

  Cache.First := 0;
  Cache.Items := TList.Create();
  Cache.MemSize := FCacheSize;
  GetMem(Cache.Mem, Cache.MemSize * SizeOf(Cache.Mem[0]));
  Cache.UsedSize := 0;
end;

destructor TMySQLMonitor.Destroy();
begin
  if (Assigned(Connection)) then
    Connection.UnRegisterSQLMonitor(Self);

  FreeMem(Cache.Mem);
  Cache.Items.Free();

  inherited;
end;

{ TMySQLConnection.TTerminatedThreads *****************************************}

function TMySQLConnection.TTerminatedThreads.Add(const Item: Pointer): Integer;
begin
  CriticalSection.Enter();
  Result := inherited Add(Item);
  CriticalSection.Leave();
end;

constructor TMySQLConnection.TTerminatedThreads.Create();
begin
  inherited;

  CriticalSection := TCriticalSection.Create();
end;

procedure TMySQLConnection.TTerminatedThreads.Delete(const Item: Pointer);
var
  Index: Integer;
begin
  CriticalSection.Enter();

  Index := IndexOf(Item);
  if (Index >= 0) then
    Delete(Index);

  CriticalSection.Leave();
end;

destructor TMySQLConnection.TTerminatedThreads.Destroy();
begin
  CriticalSection.Enter();

  while (Count > 0) do
  begin
    TerminateThread(TThread(Items[0]).Handle, 1);
    inherited Delete(0);
  end;

  CriticalSection.Leave();

  CriticalSection.Free();

  inherited;
end;

{ TMySQLConnection.TSynchroThread *********************************************}

procedure TMySQLConnection.TSynchroThread.BindDataSet(const ADataSet: TMySQLQuery);
begin
  Assert(State = ssResult);
  Assert(not Assigned(DataSet));

  DataSet := ADataSet;

  if (not (DataSet is TMySQLDataSet)) then
    State := ssReceivingResult
  else
    RunAction(ssReceivingResult, False);
end;

constructor TMySQLConnection.TSynchroThread.Create(const AConnection: TMySQLConnection);
begin
  Assert(Assigned(AConnection));

  inherited Create(False);

  FConnection := AConnection;

  RunExecute := TEvent.Create(nil, True, False, '');
  SynchronizeStarted := TEvent.Create(nil, False, False, '');
  SQLStmtLengths := TList.Create();
  SQLStmtsInPackets := TList.Create();
  SQLUseStmts := TList.Create();
  State := ssClose;

  FreeOnTerminate := True;
end;

destructor TMySQLConnection.TSynchroThread.Destroy();
begin
  Assert(not Assigned(DataSet));

  RunExecute.Free();
  SynchronizeStarted.Free();
  SQLStmtLengths.Free();
  SQLStmtsInPackets.Free();
  SQLUseStmts.Free();

  inherited;
end;

procedure TMySQLConnection.TSynchroThread.Execute();
var
  Timeout: LongWord;
  WaitResult: TWaitResult;
  SynchronizeRequestSent: Boolean;
begin
  while (not Terminated) do
  begin
    if ((Connection.ServerTimeout = 0) or (Connection.LibraryType = ltHTTP)) then
      Timeout := INFINITE
    else
      Timeout := Connection.ServerTimeout * 1000;
    WaitResult := RunExecute.WaitFor(Timeout);
    if (not Terminated) then
      if (WaitResult = wrTimeout) then
        Connection.SyncPing(Self)
      else
      begin
        case (State) of
          ssConnecting:
            Connection.SyncConnecting(Self);
          ssExecutingSQL:
            Connection.SyncExecutingSQL(Self);
          ssReceivingResult:
            Connection.SyncReceivingResult(Self);
          ssNextResult:
            Connection.SyncNextResult(Self);
          ssCancel:
            Connection.SyncCancel(Self);
          ssDisconnecting:
            Connection.SyncDisconncting(Self);
        end;

        Connection.TerminateCS.Enter();
        RunExecute.ResetEvent();
        if (Terminated or (Mode in [smDataHandle]) and (State = ssReceivingResult)) then
          SynchronizeRequestSent := False
        else
        begin
          MySQLConnectionSynchronizeRequest(Self);
          SynchronizeRequestSent := True;
        end;
        Connection.TerminateCS.Leave();

        if (SynchronizeRequestSent) then
          SynchronizeStarted.WaitFor(INFINITE);
      end;
  end;

  if (Assigned(LibHandle)) then
  begin
    if (Assigned(ResultHandle)) then
    begin
      while ((Connection.Lib.mysql_errno(LibHandle) = 0) and Assigned(Connection.Lib.mysql_fetch_row(ResultHandle))) do ;
      Connection.Lib.mysql_free_result(ResultHandle);
      ResultHandle := nil;
    end;
    Connection.Lib.mysql_close(LibHandle);
    LibHandle := nil;
  end;

  Connection.TerminatedThreads.Delete(Self);
end;

function TMySQLConnection.TSynchroThread.GetIsRunning(): Boolean;
begin
  Result := (RunExecute.WaitFor(IGNORE) = wrSignaled) or not (State in [ssClose, ssReady]);
end;

procedure TMySQLConnection.TSynchroThread.ReleaseDataSet();
begin
  Assert(Assigned(DataSet));

  if (DataSet is TMySQLDataSet) then
    TMySQLDataSet(DataSet).RecordsReceived.SetEvent();

  DataSet := nil;

  Connection.SyncHandledResult(Self);
end;

procedure TMySQLConnection.TSynchroThread.RunAction(const AState: TState; const Synchron: Boolean);
begin
  Assert(RunExecute.WaitFor(IGNORE) <> wrSignaled);

  State := AState;

  if (Synchron) then
    case (State) of
      ssConnecting:
        begin
          Connection.SyncConnecting(Self);
          if (not Success) then
            Connection.DoError(ErrorCode, ErrorMessage)
          else
            Connection.SyncConnected(Self);
        end;
      ssExecutingSQL:
    		begin
          case (Mode) of
            smSQL:
              begin
                repeat
                  Connection.SyncExecutingSQL(Self);
                  Connection.SyncHandleResult(Self);
                  Connection.SyncHandlingResult(Self);
                  while (State = ssNextResult) do
                  begin
                    Connection.SyncNextResult(Self);
                    Connection.SyncHandleResult(Self);
                    Connection.SyncHandlingResult(Self);
                  end;
                until (State <> ssExecutingSQL);
                if (State = ssReady) then
                  Connection.SyncExecutedSQL(Self);
              end;
            smDataSet:
              begin
                Connection.SyncExecutingSQL(Self);
                Connection.SyncHandleResult(Self);
              end;
          end;
          if (State = ssReady) then
            Connection.SyncExecutedSQL(Self);
        end;
	    ssReceivingResult:
        raise Exception.Create(SOutOfSync);
      ssCancel:
        begin
          Connection.SyncCancel(Self);
          Connection.SyncExecutedSQL(Self);
        end;
      ssDisconnecting:
        begin
          Connection.SyncDisconncting(Self);
          Connection.SyncDisconncted(Self);
        end;
    end
  else
    RunExecute.SetEvent();
end;

procedure TMySQLConnection.TSynchroThread.Synchronize();
begin
  SynchronizeStarted.SetEvent();

  if (not Terminated) then
    case (State) of
      ssConnecting:
        Connection.SyncConnected(Self);
      ssExecutingSQL,
      ssNextResult:
        begin
          Connection.SyncHandleResult(Self);
          Connection.SyncHandlingResult(Self);
          if (Mode in [smSQL, smDataSet]) then
            if (State in [ssNextResult, ssExecutingSQL]) then
              RunExecute.SetEvent()
            else if (State in [ssReady, ssError]) then
              Connection.SyncExecutedSQL(Self);
        end;
	    ssReceivingResult:
        begin
          Connection.TerminateCS.Enter();
          if (Assigned(DataSet.SynchroThread)) then
          begin
            DataSet.SynchroThread := nil;
            ReleaseDataSet();
          end;
          Connection.TerminateCS.Leave();
          if (Mode in [smSQL, smDataSet]) then
            if (State in [ssNextResult, ssExecutingSQL]) then
              RunExecute.SetEvent()
            else if (State in [ssReady, ssError]) then
              Connection.SyncExecutedSQL(Self);
        end;
      ssCancel:
        Connection.SyncExecutedSQL(Self);
      ssDisconnecting:
        Connection.SyncDisconncted(Self);
    end;
end;

procedure TMySQLConnection.TSynchroThread.Terminate();
var
  Index: Integer;
begin
  SynchronizingThreadsCS.Enter();
  Index := SynchronizingThreads.IndexOf(Self);
  if (Index >= 0) then
    SynchronizingThreads.Delete(Index);
  SynchronizingThreadsCS.Leave();

  inherited Terminate;

  if (RunExecute.WaitFor(IGNORE) = wrSignaled) then
  begin
    SynchronizeStarted.SetEvent();
    Connection.TerminatedThreads.Add(Self);
  end
  else
    RunExecute.SetEvent();
end;

{ TMySQLConnection ************************************************************}

procedure TMySQLConnection.BeginSilent();
begin
  Inc(FSilentCount);
end;

procedure TMySQLConnection.BeginSynchron();
begin
  Inc(FSynchronCount);
end;

function TMySQLConnection.CanShutdown(): Boolean;
begin
  Result := Assigned(Lib.mysql_shutdown) and not InUse();
end;

function TMySQLConnection.CharsetToCodePage(const Charset: string): Cardinal;
var
  I: Integer;
begin
  Result := GetACP();

  if (ServerVersion < 40101) then
  begin
    for I := 0 to Length(MySQL_Character_Sets) - 1 do
      if (lstrcmpiA(PAnsiChar(AnsiString(Charset)), MySQL_Character_Sets[I].CharsetName) = 0) then
        Result := MySQL_Character_Sets[I].CodePage;
  end
  else
  begin
    for I := 0 to Length(MySQL_Collations) - 1 do
      if (lstrcmpiA(PAnsiChar(AnsiString(Charset)), MySQL_Collations[I].CharsetName) = 0) then
        Result := MySQL_Collations[I].CodePage;
  end;
end;

function TMySQLConnection.CharsetToCodePage(const Charset: Byte): Cardinal;
var
  Found: Boolean;
  I: Integer;
begin
  if (ServerVersion < 40101) then
    if (Charset < Length(MySQL_Character_Sets)) then
      Result := MySQL_Character_Sets[Charset].CodePage
    else
      raise ERangeError.CreateFmt(SPropertyOutOfRange, ['CodePage'])
  else
  begin
    Found := False;
    Result := 0;
    for I := 0 to Length(MySQL_Collations) - 1 do
      if (MySQL_Collations[I].CharsetNr = Charset) then
      begin
        Result := MySQL_Collations[I].CodePage;
        Found := True;
      end;
    if (not Found) then
      raise ERangeError.CreateFmt(SInvalidCodePage + ' (%d)', [Charset]);
  end;
end;

procedure TMySQLConnection.CloseResult(const DataHandle: TDataResult);
begin
  if (Asynchron and (DataHandle.State <> ssReady)) then
    DataHandle.RunAction(ssCancel, False)
  else
    SyncExecutedSQL(DataHandle);
end;

function TMySQLConnection.CodePageToCharset(const CodePage: Cardinal): string;
var
  I: Integer;
begin
  Result := '';

  if (ServerVersion < 40101) then
  begin
    for I := 0 to Length(MySQL_Character_Sets) - 1 do
      if ((Result = '') and (MySQL_Character_Sets[I].CodePage = CodePage)) then
        Result := string(StrPas(MySQL_Character_Sets[I].CharsetName));
  end
  else
  begin
    for I := 0 to Length(MySQL_Collations) - 1 do
      if ((Result = '') and (MySQL_Collations[I].CodePage = CodePage)) then
        Result := string(StrPas(MySQL_Collations[I].CharsetName));
  end;
end;

procedure TMySQLConnection.CommitTransaction();
begin
  if (Lib.LibraryType <> ltHTTP) then
  begin
    Assert(InTransaction);

    ExecuteSQL('COMMIT;');

    FInTransaction := False;
  end;
end;

constructor TMySQLConnection.Create(AOwner: TComponent);
begin
  inherited;

  FFormatSettings := TFormatSettings.Create(GetSystemDefaultLCID());
  FFormatSettings.ThousandSeparator := #0;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
  FFormatSettings.LongDateFormat := FFormatSettings.ShortDateFormat;
  FFormatSettings.LongTimeFormat := 'hh:mm:ss';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';

  ExecuteSQLDone := TEvent.Create(nil, False, False, 'ExecuteSQLDone');
  FAfterExecuteSQL := nil;
  FAsynchron := False;
  FAutoCommit := True;
  FBeforeExecuteSQL := nil;
  FCharset := 'utf8';
  FCodePage := CP_UTF8;
  FConnected := False;
  FDatabaseName := '';
  FExecutionTime := 0;
  FHost := '';
  FHTTPAgent := '';
  FIdentifierQuoted := True;
  FIdentifierQuoter := '`';
  FInTransaction := False;
  FLatestConnect := 0;
  FLib := nil;
  FSynchroThread := nil;
  FLibraryType := ltBuiltIn;
  FMultiStatements := True;
  FOnConvertError := nil;
  FOnSQLError := nil;
  FOnUpdateIndexDefs := nil;
  FPassword := '';
  FPort := MYSQL_PORT;
  FSilentCount := 0;
  FTerminateCS := TCriticalSection.Create();
  FTerminatedThreads := TTerminatedThreads.Create();
  FThreadDeep := 0;
  FThreadId := 0;
  FUserName := '';
  InMonitor := False;
  InOnResult := False;
  local_infile := nil;
  TimeDiff := 0;
end;

destructor TMySQLConnection.Destroy();
begin
  Asynchron := False;
  Close();

  while (DataSetCount > 0) do
    DataSets[0].Free();

  if (Assigned(SynchroThread)) then
  begin
    SynchroThread.FreeOnTerminate := False;
    SynchroThread.Terminate();
    SynchroThread.WaitFor();
    SynchroThread.Free();
  end;
  TerminatedThreads.Free();

  ExecuteSQLDone.Free();
  TerminateCS.Free();

  inherited;
end;

procedure TMySQLConnection.DoAfterExecuteSQL();
begin
  if (Assigned(AfterExecuteSQL)) then AfterExecuteSQL(Self);
end;

procedure TMySQLConnection.DoBeforeExecuteSQL();
begin
  if (Assigned(BeforeExecuteSQL)) then BeforeExecuteSQL(Self);
end;

procedure TMySQLConnection.DoConnect();
begin
  Assert(not Connected);

  FExecutionTime := 0;
  FErrorCode := CR_ASYNCHRON; FErrorMessage := '';

  if (Assigned(SynchroThread) and (SynchroThread.State <> ssClose)) then
    Terminate();
  if (not Assigned(FSynchroThread)) then
    FSynchroThread := TSynchroThread.Create(Self);

  SynchroThread.RunAction(ssConnecting, not UseSynchroThread());
end;

procedure TMySQLConnection.DoConvertError(const Sender: TObject; const Text: string; const Error: EConvertError);
begin
  if (Assigned(FOnConvertError)) then
    FOnConvertError(Sender, Text)
  else
    raise Error;
end;

procedure TMySQLConnection.DoDisconnect();
begin
  Assert(Connected);

  SendConnectEvent(False);

  if (Assigned(SynchroThread) and (SynchroThread.State <> ssReady)) then
    Terminate();

  FErrorCode := CR_ASYNCHRON; FErrorMessage := '';

  if (not Assigned(SynchroThread)) then
    SyncDisconncted(nil)
  else
    SynchroThread.RunAction(ssDisconnecting, not UseSynchroThread());
end;

procedure TMySQLConnection.DoError(const AErrorCode: Integer; const AErrorMessage: string);
begin
  if ((AErrorCode = CR_UNKNOWN_HOST)
    or (AErrorCode = CR_SERVER_OLD)) then
    Close()
  else if ((AErrorCode = CR_UNKNOWN_ERROR)
    or (AErrorCode = CR_IPSOCK_ERROR)
    or (AErrorCode = CR_SERVER_GONE_ERROR)
    or (AErrorCode = CR_SERVER_HANDSHAKE_ERR)
    or (AErrorCode = CR_SERVER_LOST)) then
    if (not Assigned(SynchroThread)) then
      SyncDisconncted(nil)
    else
      SynchroThread.RunAction(ssDisconnecting, not UseSynchroThread());

  FErrorCode := AErrorCode; FErrorMessage := AErrorMessage;

  if (SilentCount = 0) then
    if (not Assigned(FOnSQLError)) then
      raise EMySQLError.Create(AErrorMessage, AErrorCode, Self)
    else
      FOnSQLError(Self, AErrorCode, AErrorMessage);
end;

procedure TMySQLConnection.EndSilent();
begin
  if (FSilentCount > 0) then
    Dec(FSilentCount);
end;

procedure TMySQLConnection.EndSynchron();
begin
  if (FSynchronCount > 0) then
    Dec(FSynchronCount);
end;

function TMySQLConnection.ErrorMsg(const AHandle: MySQLConsts.MYSQL): string;
begin
  Result := LibDecode(my_char(SQLUnescape(Lib.mysql_error(AHandle))));
end;

function TMySQLConnection.EscapeIdentifier(const Identifier: string): string;
begin
  Result := IdentifierQuoter + ReplaceStr(Identifier, IdentifierQuoter, IdentifierQuoter + IdentifierQuoter) + IdentifierQuoter;
end;

function TMySQLConnection.ExecuteSQL(const SQL: string; const OnResult: TResultEvent = nil): Boolean;
begin
  Result := ExecuteSQL(smSQL, True, SQL, OnResult);
end;

function TMySQLConnection.ExecuteSQL(const Mode: TSynchroThread.TMode; const Synchron: Boolean;
  const SQL: string; const OnResult: TResultEvent = nil): Boolean;
var
  AlterTableAfterCreateTable: Boolean;
  AlterTableAfterCreateTableFix: Boolean;
  CLStmt: TSQLCLStmt;
  CreateTableInPacket: Boolean;
  DDLStmt: TSQLDDLStmt;
  OldStmt: Integer;
  PacketComplete: (pcNo, pcExclusiveCurrentStmt, pcInclusiveCurrentStmt, pcStmtTooLarge);
  ProcedureName: string;
  S: string;
  SetNames: Boolean;
  SQLPacketIndex: Integer;
  StmtLength: Integer;
begin
  Assert(SQL <> '');

  if (InOnResult) then
    raise Exception.Create(SOutOfSync + ' (in OnResult): ' + CommandText);
  if (InMonitor) then
    raise Exception.Create(SOutOfSync + ' (in Monitor): ' + CommandText);

  if (Assigned(SynchroThread) and not (SynchroThread.State in [ssClose, ssReady])) then
    Terminate();
  if (not Assigned(SynchroThread)) then
    FSynchroThread := TSynchroThread.Create(Self);

  SynchroThread.DataSet := nil;
  SynchroThread.Mode := Mode;
  SynchroThread.OnResult := OnResult;
  SynchroThread.SQL := SQL;
  SynchroThread.SQLStmtLengths.Clear();
  SynchroThread.SQLStmtsInPackets.Clear();
  SynchroThread.SQLUseStmts.Clear();
  SynchroThread.Time := 0;

  FErrorCode := CR_ASYNCHRON; FErrorMessage := '';
  FExecutedSQLLength := 0; FExecutedStmts := 0; FResultCount := 0;
  FRowsAffected := -1; FWarningCount := -1; FExecutionTime := 0;

  SynchroThread.SQLStmtIndex := 1;
  while (SynchroThread.SQLStmtIndex < Length(SynchroThread.SQL)) do
  begin
    StmtLength := SQLStmtLength(SynchroThread.SQL, SynchroThread.SQLStmtIndex);
    SynchroThread.SQLStmtLengths.Add(Pointer(StmtLength));
    Inc(SynchroThread.SQLStmtIndex, StmtLength);
  end;

  SQLPacketIndex := 1;
  SetNames := False; CreateTableInPacket := False; AlterTableAfterCreateTable := False;
  AlterTableAfterCreateTableFix := 50100 <= ServerVersion;
  PacketComplete := pcNo;
  SynchroThread.SQLStmt := 0; OldStmt := 0; SynchroThread.SQLStmtIndex := 1;
  while ((SynchroThread.SQLStmt < SynchroThread.SQLStmtLengths.Count) and not SetNames and (PacketComplete <> pcStmtTooLarge)) do
  begin
    SetNames := False;
    if (SQLParseCLStmt(CLStmt, @SynchroThread.SQL[SynchroThread.SQLStmtIndex], Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]), ServerVersion)) then
      case (CLStmt.CommandType) of
        ctSetNames,
        ctSetCharacterSet:
          SetNames := True;
        ctUse:
          SynchroThread.SQLUseStmts.Add(Pointer(SynchroThread.SQLStmt));
      end;
    if (AlterTableAfterCreateTableFix) then
      if (not CreateTableInPacket) then
        CreateTableInPacket := SQLParseDDLStmt(DDLStmt, @SynchroThread.SQL[SynchroThread.SQLStmtIndex], Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]), ServerVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType = otTable)
      else
        AlterTableAfterCreateTable := SQLParseDDLStmt(DDLStmt, @SynchroThread.SQL[SynchroThread.SQLStmtIndex], Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]), ServerVersion) and (DDLStmt.DefinitionType = dtAlter) and (DDLStmt.ObjectType = otTable);

    StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);

    if ((SynchroThread.SQLStmtIndex > 0) and not SetNames) then
    begin
      if (SetNames or AlterTableAfterCreateTable) then
        PacketComplete := pcExclusiveCurrentStmt
      else if ((SizeOf(COM_QUERY) + SynchroThread.SQLStmtIndex - 1 + StmtLength > MaxAllowedPacket) and (SizeOf(COM_QUERY) + WideCharToMultiByte(CodePage, 0, PChar(@SynchroThread.SQL[SQLPacketIndex]), SynchroThread.SQLStmtIndex - SQLPacketIndex + StmtLength, nil, 0, nil, nil) > MaxAllowedPacket)) then
        if (SynchroThread.SQLStmt > 0) then
          PacketComplete := pcExclusiveCurrentStmt
        else
          PacketComplete := pcStmtTooLarge
      else if (not MultiStatements or (SynchroThread.SQLStmtIndex - 1 + StmtLength = Length(SynchroThread.SQL))) then
        PacketComplete := pcInclusiveCurrentStmt
      else
        PacketComplete := pcNo;
      if ((PacketComplete = pcNo) and SQLParseCallStmt(@SynchroThread.SQL[SynchroThread.SQLStmtIndex], Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]), ProcedureName, ServerVersion) and (ProcedureName <> '')) then
        PacketComplete := pcInclusiveCurrentStmt;
    end;

    case (PacketComplete) of
      pcNo:
        begin
          Inc(SynchroThread.SQLStmt);
          Inc(SynchroThread.SQLStmtIndex, StmtLength);
        end;
      pcExclusiveCurrentStmt:
        begin
          if (SynchroThread.SQLStmt > OldStmt) then
            SynchroThread.SQLStmtsInPackets.Add(Pointer(SynchroThread.SQLStmt - OldStmt));
          OldStmt := SynchroThread.SQLStmt;
          SQLPacketIndex := SynchroThread.SQLStmtIndex;
          CreateTableInPacket := False;
          AlterTableAfterCreateTable := False;
        end;
      pcInclusiveCurrentStmt:
        begin
          Inc(SynchroThread.SQLStmt);
          Inc(SynchroThread.SQLStmtIndex, StmtLength);
          SynchroThread.SQLStmtsInPackets.Add(Pointer(SynchroThread.SQLStmt - OldStmt));
          OldStmt := SynchroThread.SQLStmt;
          SQLPacketIndex := SynchroThread.SQLStmtIndex;
          CreateTableInPacket := False;
          AlterTableAfterCreateTable := False;
        end;
    end;
  end;
  if (PacketComplete in [pcNo, pcExclusiveCurrentStmt]) then
    SynchroThread.SQLStmtsInPackets.Add(Pointer(SynchroThread.SQLStmt > OldStmt));

  if (SynchroThread.SQLStmtLengths.Count = 0) then
  begin
    DoError(ER_EMPTY_QUERY, ER_EMPTY_QUERY_MSG);
    Result := False;
  end
  else if (SetNames) then
  begin
    DoError(CR_SET_NAMES, CR_SET_NAMES_MSG);
    Result := False;
  end
  else if (PacketComplete = pcStmtTooLarge) then
  begin
    DoError(ER_NET_PACKET_TOO_LARGE, ER_NET_PACKET_TOO_LARGE_MSG);
    Result := False;
  end
  else
  begin
    DoBeforeExecuteSQL();

    SynchroThread.SQLStmt := 0;
    SynchroThread.SQLStmtIndex := 1;
    SynchroThread.SQLPacket := 0;
    SynchroThread.SQLStmtInPacket := 0;
    SynchroThread.SQLLastStmtInPacket := Integer(SynchroThread.SQLStmtsInPackets[SynchroThread.SQLPacket]) - 1;

    S := '# ' + SysUtils.DateTimeToStr(Now() + TimeDiff, FormatSettings);
    WriteMonitor(PChar(S), Length(S), ttTime);

    StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);
    WriteMonitor(@SynchroThread.SQL[SynchroThread.SQLStmtIndex], StmtLength, ttRequest);

    if (Mode = smDataHandle) then
    begin
      SyncExecutingSQL(SynchroThread);
      FErrorCode := SynchroThread.ErrorCode;
      FErrorMessage := SynchroThread.ErrorMessage;
      Result := ErrorCode = 0;
    end
    else
    begin
      SynchroThread.RunAction(ssExecutingSQL, Synchron or not UseSynchroThread());
      if ((Synchron or not UseSynchroThread()) and Assigned(SynchroThread.LibHandle)) then
        Result := ErrorCode = 0
      else
        Result := False;
    end;
  end;
end;

function TMySQLConnection.FirstResult(out DataHandle: TMySQLConnection.TDataResult; const SQL: string): Boolean;
begin
  Result := ExecuteSQL(smDataHandle, False, SQL);
  SyncHandleResult(SynchroThread);

  DataHandle := SynchroThread;
end;

function TMySQLConnection.GetAutoCommit(): Boolean;
begin
  Result := FAutoCommit;
end;

function TMySQLConnection.GetCommandText(): string;
var
  EndingCommentLength: Integer;
  Len: Integer;
  StartingCommentLength: Integer;
  StmtLength: Integer;
begin
  if ((SynchroThread.SQL = '') or (SynchroThread.SQLStmt > SynchroThread.SQLLastStmtInPacket)) then
    Result := ''
  else
  begin
    StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);
    Len := SQLTrimStmt(SynchroThread.SQL, SynchroThread.SQLStmtIndex, StmtLength, StartingCommentLength, EndingCommentLength);
    if (SynchroThread.SQL[SynchroThread.SQLStmtIndex + StartingCommentLength + Len - 1] = ';') then
      Dec(Len);
    Result := copy(SynchroThread.SQL, SynchroThread.SQLStmtIndex + StartingCommentLength, Len);
  end
end;

function TMySQLConnection.GetDateTime(): TDateTime;
begin
  Result := Now() + TimeDiff;
end;

function TMySQLConnection.GetHandle(): MySQLConsts.MYSQL;
begin
  if (not Assigned(SynchroThread)) then
    Result := nil
  else
    Result := SynchroThread.LibHandle;
end;

function TMySQLConnection.GetInfo(): string;
begin
  if (not Assigned(Handle)) then
    Result := ''
  else
    Result := LibDecode(Lib.mysql_info(Handle));
end;

function TMySQLConnection.GetConnected(): Boolean;
begin
  Result := FConnected;
end;

function TMySQLConnection.GetInsertId(): my_ulonglong;
begin
  if (not Assigned(SynchroThread)) then
    Result := 0
  else
    Result := Lib.mysql_insert_id(Handle);
end;

function TMySQLConnection.GetLoadDataFile(): Boolean;
begin
  Result := Assigned(Lib) and not (Lib.FLibraryType in [ltHTTP]);
end;

function TMySQLConnection.GetMaxAllowedPacket(): Integer;
begin
  // MAX_ALLOWED_PACKET Constante of the Server - SizeOf(COM_QUERY)
  Result := 1 * 1024 * 1024 - 1;
end;

function TMySQLConnection.InUse(): Boolean;
begin
  Result := Assigned(SynchroThread) and SynchroThread.IsRunning;
end;

function TMySQLConnection.LibDecode(const Data: my_char; const Length: my_int = -1): string;
label
  StringL;
var
  Len: Integer;
begin
  if (Length >= 0) then
    Len := Length
  else if (Assigned(Data)) then
    Len := lstrlenA(Data)
  else
    Len := 0;
  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, MultiByteToWideChar(CodePage, 0, Data, Len, nil, 0));
    if (Len > 0) then
      SetLength(Result, MultiByteToWideChar(CodePage, 0, Data, Len, PChar(Result), System.Length(Result)))
    else if (GetLastError() <> 0) then
      RaiseLastOSError();
  end;
end;

function TMySQLConnection.LibEncode(const Value: string): RawByteString;
var
  Len: Integer;
begin
  if (Value = '') then
    Result := ''
  else
  begin
    Len := WideCharToMultiByte(CodePage, 0, PChar(Value), Length(Value), nil, 0, nil, nil);
    if ((Len = 0) and (GetLastError() <> 0)) then
      RaiseLastOSError();

    SetLength(Result, Len);
    if (Len > 0) then
      SetLength(Result, WideCharToMultiByte(CodePage, 0, PChar(Value), Length(Value), PAnsiChar(Result), Len, nil, nil));
  end;
end;

function TMySQLConnection.LibPack(const Value: string): RawByteString;
label
  StringL;
var
  Len: Integer;
begin
  if (Value = '') then
    Result := ''
  else
  begin
    Len := Length(Value);
    SetLength(Result, Len);
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV ECX,Len
      StringL:
        LODSW                            // Load WideChar from Value
        STOSB                            // Store AnsiChar into Result
        LOOP StringL                     // Repeat for all characters

        POP EDI
        POP ESI
        POP ES
    end;
  end;
end;

function TMySQLConnection.LibUnpack(const Data: my_char; const Length: my_int = -1): string;
label
  StringL;
var
  Len: Integer;
begin
  if (Length = -1) then
    Len := lstrlenA(Data)
  else
    Len := Length;
  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, Len);
    asm
      PUSH ES
      PUSH ESI
      PUSH EDI

      PUSH DS                          // string operations uses ES
      POP ES
      CLD                              // string operations uses forward direction

      MOV ESI,Data                     // Copy characters from Data
      MOV EAX,Result                   //   to Result
      MOV EDI,[EAX]

      MOV AH,0                         // Clear AH, since AL will be load but AX stored
      MOV ECX,Len
    StringL:
      LODSB                            // Load AnsiChar from Data
      STOSW                            // Store WideChar into S
      LOOP StringL                     // Repeat for all characters

      POP EDI
      POP ESI
      POP ES
    end;
  end;
end;

procedure TMySQLConnection.local_infile_end(const local_infile: Plocal_infile);
begin
  if (local_infile^.Handle <> INVALID_HANDLE_VALUE) then
    CloseHandle(local_infile^.Handle);
  if (Assigned(local_infile^.Buffer)) then
    VirtualFree(local_infile^.Buffer, local_infile^.BufferSize, MEM_RELEASE);

  Self.local_infile := nil;
  FreeMem(local_infile);
end;

function TMySQLConnection.local_infile_error(const local_infile: Plocal_infile; const error_msg: my_char; const error_msg_len: my_uint): my_int;
var
  Buffer: PChar;
  Len: Integer;
begin
  Buffer := nil;
  Len := FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil, local_infile^.LastError, 0, @Buffer, 0, nil);
  if (Len > 0) then
  begin
    Len := WideCharToMultiByte(CodePage, 0, Buffer, Len, error_msg, error_msg_len, nil, nil);
    error_msg[Len] := #0;
    LocalFree(HLOCAL(Buffer));
  end
  else if (GetLastError() = 0) then
    RaiseLastOSError();
  Result := local_infile^.ErrorCode;
end;

function TMySQLConnection.local_infile_init(var local_infile: Plocal_infile; const filename: my_char): my_int;
begin
  GetMem(local_infile, SizeOf(local_infile^));
  Self.local_infile := local_infile;

  ZeroMemory(local_infile, SizeOf(local_infile^));
  local_infile^.Buffer := nil;
  local_infile^.Connection := Self;
  local_infile^.Position := 0;

  if ((MultiByteToWideChar(CodePage, 0, filename, lstrlenA(filename), @local_infile^.Filename, Length(local_infile^.Filename)) = 0) and (GetLastError() <> 0)) then
  begin
    local_infile^.LastError := GetLastError();
    local_infile^.ErrorCode := EE_FILENOTFOUND;
  end
  else
  begin
    local_infile^.Handle := CreateFile(@local_infile^.Filename, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);
    if (local_infile^.Handle = INVALID_HANDLE_VALUE) then
    begin
      local_infile^.ErrorCode := EE_FILENOTFOUND;
      local_infile^.LastError := GetLastError();
    end
    else
    begin
      local_infile^.ErrorCode := 0;
      local_infile^.LastError := 0;
    end;
  end;

  Result := local_infile^.ErrorCode;
end;

function TMySQLConnection.local_infile_read(const local_infile: Plocal_infile; buf: my_char; const buf_len: my_uint): my_int;
var
  BytesPerSector: DWord;
  NumberofFreeClusters: DWord;
  SectorsPerCluser: DWord;
  Size: DWord;
  TotalNumberOfClusters: DWord;
begin
  if (not Assigned(local_infile^.Buffer)) then
  begin
    local_infile^.BufferSize := buf_len;
    if (GetFileSize(local_infile^.Handle, nil) > 0) then
      local_infile^.BufferSize := Min(GetFileSize(local_infile^.Handle, nil), local_infile^.BufferSize);

    if (GetDiskFreeSpace(PChar(ExtractFileDrive(local_infile^.Filename)), SectorsPerCluser, BytesPerSector, NumberofFreeClusters, TotalNumberOfClusters)
      and (local_infile^.BufferSize mod BytesPerSector > 0)) then
      Inc(local_infile^.BufferSize, BytesPerSector - local_infile^.BufferSize mod BytesPerSector);
    local_infile^.Buffer := VirtualAlloc(nil, local_infile^.BufferSize, MEM_COMMIT, PAGE_READWRITE);

    if (not Assigned(local_infile^.Buffer)) then
      local_infile^.ErrorCode := EE_OUTOFMEMORY;
  end;

  if (not ReadFile(local_infile^.Handle, local_infile^.Buffer^, Min(buf_len, local_infile^.BufferSize), Size, nil)) then
  begin
    local_infile^.LastError := GetLastError();
    local_infile^.ErrorCode := EE_READ;
    Result := -1;
  end
  else
  begin
    MoveMemory(buf, local_infile^.Buffer, Size);
    Inc(local_infile^.Position, Size);
    Result := Size;
  end;
end;

function TMySQLConnection.NextCommandText(): string;
var
  EndingCommentLength: Integer;
  Len: Integer;
  StartingCommentLength: Integer;
  StmtLength: Integer;
begin
  if (not InUse() or not Assigned(SynchroThread) or (SynchroThread.SQLStmt = SynchroThread.SQLLastStmtInPacket)) then
    Result := ''
  else
  begin
    StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt + 1]);
    Len := SQLTrimStmt(SynchroThread.SQL, SynchroThread.SQLStmtIndex, StmtLength, StartingCommentLength, EndingCommentLength);
    Result := copy(SynchroThread.SQL, SynchroThread.SQLStmtIndex + Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]) + StartingCommentLength, Len);
  end;
end;

function TMySQLConnection.NextResult(const DataHandle: TMySQLConnection.TDataResult): Boolean;
begin
  SyncNextResult(DataHandle);
  SyncHandleResult(DataHandle);

  Result := DataHandle.Success;
end;

procedure TMySQLConnection.RegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor);
begin
  SetLength(FSQLMonitors, Length(FSQLMonitors) + 1);

  FSQLMonitors[Length(FSQLMonitors) - 1] := AMySQLMonitor;
end;

procedure TMySQLConnection.RollbackTransaction();
begin
  if (InTransaction and (Lib.LibraryType <> ltHTTP)) then
  begin
    Assert(InTransaction);

    ExecuteSQL('ROLLBACK;');

    FInTransaction := False;
  end;
end;

procedure TMySQLConnection.SetAutoCommit(const AAutoCommit: Boolean);
begin
  FAutoCommit := AAutoCommit;
end;

procedure TMySQLConnection.SetCharset(const ACharset: string);
begin
  if ((ACharset <> '') and (ACharset <> Charset)) then
  begin
    FCharset := LowerCase(ACharset);
    FCodePage := CharsetToCodePage(FCharset);

    if (Connected and Assigned(Lib.mysql_options) and Assigned(SynchroThread)) then
      Lib.mysql_options(Handle, MYSQL_SET_CHARSET_NAME, my_char(RawByteString(FCharset)));
  end;
end;

procedure TMySQLConnection.SetConnected(Value: Boolean);
begin
  if (csReading in ComponentState) and Value then
    inherited else
  begin
    if Value = GetConnected then Exit;
    if Value then
    begin
      if Assigned(BeforeConnect) then BeforeConnect(Self);
      if (not Assigned(FLib)) then
        FLib := LoadMySQLLibrary(FLibraryType, LibraryName);
      if (not Assigned(FLib)) then
      begin
        DoError(ER_CANT_OPEN_LIBRARY, Format(SLibraryNotAvailable, [LibraryName]));
        if Assigned(AfterConnect) then AfterConnect(Self);
      end
      else
      begin
        DoConnect();
        // Maybe we're using Asynchron. So the Events should be called after
        // thread execution in SyncConnected.
      end
    end else
    begin
      if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
      DoDisconnect();
      // Maybe we're using Asynchron. So the Events should be called after
      // thread execution in SyncDisconncted.
    end;
  end;
end;

procedure TMySQLConnection.SetDatabaseName(const ADatabaseName: string);
begin
  Assert(not Connected);


  FDatabaseName := ADatabaseName;
end;

procedure TMySQLConnection.SetHost(const AHost: string);
begin
  Assert(not Connected);


  FHost := AHost;
end;

procedure TMySQLConnection.SetIdentifierQuoter(const AIdentifierQuoter: Char);
begin
  FIdentifierQuoter := AIdentifierQuoter;
end;

procedure TMySQLConnection.SetLibraryName(const ALibraryName: string);
begin
  Assert(not Connected);


  FLibraryName := ALibraryName;
end;

procedure TMySQLConnection.SetLibraryType(const ALibraryType: TMySQLLibrary.TLibraryType);
begin
  Assert(not Connected);


  FLibraryType := ALibraryType;
end;

function TMySQLConnection.SendSQL(const SQL: string; const OnResult: TResultEvent = nil): Boolean;
begin
  Result := ExecuteSQL(smSQL, False, SQL, OnResult) and not UseSynchroThread();
end;

procedure TMySQLConnection.SetPassword(const APassword: string);
begin
  Assert(not Connected);


  FPassword := APassword;
end;

procedure TMySQLConnection.SetPort(const APort: Word);
begin
  Assert(not Connected);


  FPort := APort;
end;

procedure TMySQLConnection.SetUsername(const AUsername: string);
begin
  Assert(not Connected);


  FUserName := AUserName;
end;

function TMySQLConnection.Shutdown(): Boolean;
var
  Retry: Integer;
begin
  Assert(Connected and Assigned(Lib.mysql_shutdown));

  if (InOnResult) then
    raise Exception.Create(SOutOfSync);
  if (InMonitor) then
    raise Exception.Create(SOutOfSync);

  if (Assigned(SynchroThread) and (SynchroThread.State <> ssReady)) then
    Terminate();

  if (not Assigned(SynchroThread)) then
    FSynchroThread := TSynchroThread.Create(Self);

  Retry := 0;
  while (not Assigned(SynchroThread.LibHandle) and (Retry < RETRY_COUNT)) do
  begin
    SyncConnecting(SynchroThread);
    Inc(Retry);
  end;

  SendConnectEvent(False);

  if (not Assigned(SynchroThread.LibHandle)) then
    Result := False
  else if (Lib.mysql_shutdown(SynchroThread.LibHandle, SHUTDOWN_DEFAULT) <> 0) then
  begin
    FErrorCode := Lib.mysql_errno(SynchroThread.LibHandle);
    FErrorMessage := ErrorMsg(SynchroThread.LibHandle);
    DoError(FErrorCode, FErrorMessage);
    Result := False;
  end
  else
  begin
    SyncDisconncted(nil);
    Result := True;
  end;
end;

function TMySQLConnection.SQLUse(const DatabaseName: string): string;
begin
  Result := 'USE ' + EscapeIdentifier(DatabaseName) + ';' + #13#10;
end;

procedure TMySQLConnection.StartTransaction();
begin
  if (Lib.LibraryType <> ltHTTP) then
  begin
    Assert(not InTransaction);

    if (ServerVersion < 40011) then
      ExecuteSQL('BEGIN;')
    else
      ExecuteSQL('START TRANSACTION;');

    FInTransaction := True;
  end;
end;

procedure TMySQLConnection.SyncCancel(const SynchroThread: TSynchroThread);
begin
  Assert(not Assigned(SynchroThread.DataSet));

  repeat
    if (not SynchroThread.Terminated and not Assigned(SynchroThread.ResultHandle)) then
      SynchroThread.ResultHandle := Lib.mysql_use_result(SynchroThread.LibHandle);
    if (Assigned(SynchroThread.ResultHandle)) then
    begin
      while (not SynchroThread.Terminated and Assigned(Lib.mysql_fetch_row(SynchroThread.ResultHandle))) do ;
      if (not SynchroThread.Terminated) then
        begin Lib.mysql_free_result(SynchroThread.ResultHandle); SynchroThread.ResultHandle := nil; end;
    end;
  until (SynchroThread.Terminated or not MultiStatements or (Lib.mysql_next_result(SynchroThread.LibHandle) > 0));
end;

procedure TMySQLConnection.SyncConnecting(const SynchroThread: TSynchroThread);
var
  ClientFlag: my_uint;
begin
  if (not Assigned(SynchroThread.LibHandle)) then
  begin
    if (Assigned(Lib.my_init)) then
      Lib.my_init();
    SynchroThread.LibHandle := Lib.mysql_init(nil);
  end;

  Lib.mysql_options(SynchroThread.LibHandle, MYSQL_OPT_READ_TIMEOUT, my_char(RawByteString(IntToStr(NET_WAIT_TIMEOUT))));
  Lib.mysql_options(SynchroThread.LibHandle, MYSQL_OPT_WRITE_TIMEOUT, my_char(RawByteString(IntToStr(NET_WAIT_TIMEOUT))));
  Lib.mysql_options(SynchroThread.LibHandle, MYSQL_SET_CHARSET_NAME, my_char(RawByteString(FCharset)));
  if (UseCompression()) then
    Lib.mysql_options(SynchroThread.LibHandle, MYSQL_OPT_COMPRESS, nil);
  if (LibraryType = ltHTTP) then
  begin
    Lib.mysql_options(SynchroThread.LibHandle, enum_mysql_option(MYSQL_OPT_HTTPTUNNEL_URL), my_char(LibEncode(LibraryName)));
    if (HTTPAgent <> '') then
      Lib.mysql_options(SynchroThread.LibHandle, enum_mysql_option(MYSQL_OPT_HTTPTUNNEL_AGENT), my_char(LibEncode(HTTPAgent)));
  end;

  if (Assigned(Lib.mysql_set_local_infile_handler)) then
    Lib.mysql_set_local_infile_handler(SynchroThread.LibHandle, @MySQLDB.local_infile_init, @MySQLDB.local_infile_read, @MySQLDB.local_infile_end, @MySQLDB.local_infile_error, Self);

  ClientFlag := CLIENT_INTERACTIVE or CLIENT_LOCAL_FILES;
  if (DatabaseName <> '') then
    ClientFlag := ClientFlag or CLIENT_CONNECT_WITH_DB;
  if (Assigned(Lib.mysql_more_results) and Assigned(Lib.mysql_next_result)) then
    ClientFlag := ClientFlag or CLIENT_MULTI_STATEMENTS or CLIENT_MULTI_RESULTS;
  if (UseCompression()) then
    ClientFlag := ClientFlag or CLIENT_COMPRESS;

  if (LibraryType <> ltNamedPipe) then
    SynchroThread.Success := Assigned(Lib.mysql_real_connect(SynchroThread.LibHandle,
      my_char(LibEncode(Host)),
      my_char(LibEncode(Username)), my_char(LibEncode(Password)),
      my_char(LibEncode(DatabaseName)), Port, '', ClientFlag))
  else
  begin
    Lib.mysql_options(SynchroThread.LibHandle, enum_mysql_option(MYSQL_OPT_NAMED_PIPE), nil);
    SynchroThread.Success := Assigned(Lib.mysql_real_connect(SynchroThread.LibHandle,
      my_char(LibEncode(LOCAL_HOST_NAMEDPIPE)),
      my_char(LibEncode(Username)), my_char(LibEncode(Password)),
      my_char(LibEncode(DatabaseName)), Port, '', ClientFlag));
  end;

  SynchroThread.ErrorCode := Lib.mysql_errno(SynchroThread.LibHandle);
  SynchroThread.ErrorMessage := ErrorMsg(SynchroThread.LibHandle);
end;

procedure TMySQLConnection.SyncConnected(const SynchroThread: TSynchroThread);
var
  I: Integer;
  S: string;
  SQL: string;
begin
  FConnected := SynchroThread.Success;
  FErrorCode := SynchroThread.ErrorCode;
  FErrorMessage := SynchroThread.ErrorMessage;

  if (Assigned(SynchroThread.LibHandle)) then
  begin
    if (FErrorCode > 0) then
    begin
      DoError(FErrorCode, FErrorMessage);
      mysql_close(SynchroThread.LibHandle);
      SynchroThread.LibHandle := nil;
    end
    else
    begin
      FLatestConnect := Now();

      if (ServerVersionStr = '') then
      begin
        FServerVersionStr := string(Lib.mysql_get_server_info(SynchroThread.LibHandle));
        if (Assigned(Lib.mysql_get_server_version)) then
          FServerVersion := Lib.mysql_get_server_version(SynchroThread.LibHandle)
        else
        begin
          S := FServerVersionStr;
          if (Pos('-', S) > 0) then
            S := Copy(S, 1, Pos('-', S) - 1);
          if (S[2] = '.') and (S[4] = '.') then
            Insert('0', S, 3);
          if (S[2] = '.') and (Length(S) = 6) then
            Insert('0', S, 6);
          Val(StringReplace(S, '.', '', [rfReplaceAll	]), FServerVersion, I);
        end;
      end;

      if (ServerVersion < 32320) then
        DoError(CR_SERVER_OLD, CR_SERVER_OLD_MSG)
      else
      begin
        if ((ServerVersion < 40101) or not Assigned(Lib.mysql_character_set_name)) then
        begin
          FCharset := '';
          FCodePage := CP_ACP;
        end
        else
        begin
          FCharset := string(Lib.mysql_character_set_name(SynchroThread.LibHandle));
          FCodePage := CharsetToCodePage(FCharset);
        end;

        FHostInfo := LibDecode(Lib.mysql_get_host_info(SynchroThread.LibHandle));
        FMultiStatements := FMultiStatements and Assigned(Lib.mysql_more_results) and Assigned(Lib.mysql_next_result) and ((ServerVersion > 40100) or (Lib.FLibraryType = ltHTTP)) and not ((50000 <= ServerVersion) and (ServerVersion < 50007));

        if (FThreadId = 0) then
          SQL := ''
        else if (ServerVersion < 50000) then
          SQL := SQL + 'KILL ' + IntToStr(FThreadId) + ';' + #13#10
        else
          SQL := SQL + 'KILL CONNECTION ' + IntToStr(FThreadId) + ';' + #13#10;

        if (not Assigned(Lib.mysql_thread_id)) then
        begin
          FThreadId := 0;
          S := '# ' + SysUtils.DateTimeToStr(FLatestConnect + TimeDiff, FormatSettings) + ': Connected';
        end
        else
        begin
          FThreadId := Lib.mysql_thread_id(SynchroThread.LibHandle);
          S := '# ' + SysUtils.DateTimeToStr(FLatestConnect + TimeDiff, FormatSettings) + ': Connected (Id: ' + IntToStr(FThreadId) + ')';
        end;
        WriteMonitor(PChar(S), Length(S), ttInfo);

        if ((SQL <> '') and Assigned(SynchroThread.LibHandle)) then
          SendSQL(SQL);
      end;
    end;
  end;

  SynchroThread.State := ssReady;

  if (Connected) then
    SendConnectEvent(True);
  if Assigned(AfterConnect) then AfterConnect(Self);
end;

procedure TMySQLConnection.SyncDisconncting(const SynchroThread: TSynchroThread);
begin
  if (not Assigned(SynchroThread.LibHandle)) then
  begin
    SynchroThread.ErrorCode := 0;
    SynchroThread.ErrorMessage := '';
  end
  else
  begin
    SynchroThread.ErrorCode := Lib.mysql_errno(SynchroThread.LibHandle);
    SynchroThread.ErrorMessage := ErrorMsg(SynchroThread.LibHandle);

    Lib.mysql_close(SynchroThread.LibHandle);
    SynchroThread.LibHandle := nil;
  end;
end;

procedure TMySQLConnection.SyncDisconncted(const SynchroThread: TSynchroThread);
begin
  FThreadId := 0;
  FConnected := False;

  if (Assigned(SynchroThread)) then
  begin
    FErrorCode := SynchroThread.ErrorCode;
    FErrorMessage := SynchroThread.ErrorMessage;
    SynchroThread.State := ssClose;
  end
  else
  begin
    FErrorCode := 0;
    FErrorMessage := '';
  end;

  if Assigned(AfterDisconnect) then AfterDisconnect(Self);
end;

procedure TMySQLConnection.SyncExecutedSQL(const SynchroThread: TSynchroThread);
begin
  FErrorCode := SynchroThread.ErrorCode;
  FErrorMessage := SynchroThread.ErrorMessage;
  FExecutionTime := SynchroThread.Time;

  if (Assigned(Lib.mysql_get_server_status)) then
    FAutoCommit := Lib.mysql_get_server_status(SynchroThread.LibHandle) and SERVER_STATUS_AUTOCOMMIT <> 0;

  if (FErrorCode = 0) then
    SynchroThread.SQL := '';

  SynchroThread.State := ssReady;

  DoAfterExecuteSQL();

  if ((FErrorCode = CR_SERVER_HANDSHAKE_ERR)) then
    SyncDisconncting(SynchroThread);
end;

procedure TMySQLConnection.SyncExecutingSQL(const SynchroThread: TSynchroThread);
var
  I: Integer;
  LibLength: Integer;
  LibSQL: AnsiString;
  PacketLength: Integer;
  Retry: Integer;
  StartTime: TDateTime;
  TrimmedPacketLength: Integer;
begin
  SynchroThread.Success := True;

  Retry := 0;
  while (not Assigned(SynchroThread.LibHandle) and (Retry < RETRY_COUNT)) do
  begin
    SyncConnecting(SynchroThread);
    Inc(Retry);
  end;

  if (SynchroThread.Success) then
  begin
    PacketLength := 0;
    for I := 0 to Integer(SynchroThread.SQLStmtsInPackets[SynchroThread.SQLPacket]) - 1 do
      Inc(PacketLength, Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt + I]));
    TrimmedPacketLength := PacketLength;
    if (not MultiStatements) then
      while ((TrimmedPacketLength > 0) and CharInSet(SynchroThread.SQL[SynchroThread.SQLStmtIndex + TrimmedPacketLength - 1], [#9, #10, #13, ' ', ';'])) do
        Dec(TrimmedPacketLength);

    LibLength := WideCharToMultiByte(CodePage, 0, PChar(@SynchroThread.SQL[SynchroThread.SQLStmtIndex]), TrimmedPacketLength, nil, 0, nil, nil);
    if (LibLength < 0) then RaiseLastOSError();
    SetLength(LibSQL, LibLength);
    WideCharToMultiByte(CodePage, 0, PChar(@SynchroThread.SQL[SynchroThread.SQLStmtIndex]), TrimmedPacketLength, PAnsiChar(LibSQL), LibLength, nil, nil);

    if (not SynchroThread.Terminated and SynchroThread.Success) then
    begin
      StartTime := Now();
      SynchroThread.Success := Lib.mysql_real_query(SynchroThread.LibHandle, my_char(LibSQL), LibLength) = 0;
      SynchroThread.Time := SynchroThread.Time + Now() - StartTime;
    end;

    if (SynchroThread.Success and not SynchroThread.Terminated) then
      SynchroThread.ResultHandle := Lib.mysql_use_result(SynchroThread.LibHandle);
  end;

  SynchroThread.ErrorCode := Lib.mysql_errno(SynchroThread.LibHandle);
  SynchroThread.ErrorMessage := ErrorMsg(SynchroThread.LibHandle);
end;

procedure TMySQLConnection.SyncHandleResult(const SynchroThread: TSynchroThread);
var
  CLStmt: TSQLCLStmt;
  S: String;
  StmtLength: Integer;
begin
  FErrorCode := SynchroThread.ErrorCode;
  FErrorMessage := SynchroThread.ErrorMessage;

  if (FErrorCode > 0) then
  begin
    S := '--> Error #' + IntToStr(FErrorCode) + ': ' + FErrorMessage;
    WriteMonitor(PChar(S), Length(S), ttInfo);
  end
  else
  begin
    Inc(FResultCount);

    if (Assigned(SynchroThread.ResultHandle)) then
    begin
      StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);
      WriteMonitor(@SynchroThread.SQL[SynchroThread.SQLStmtIndex], StmtLength, ttData);
    end
    else
    begin
      StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);
      WriteMonitor(@SynchroThread.SQL[SynchroThread.SQLStmtIndex], StmtLength, ttResult);
    end;

    if (SynchroThread.SQLUseStmts.IndexOf(Pointer(SynchroThread.SQLStmt)) >= 0) then
    begin
      SQLParseCLStmt(CLStmt, @SynchroThread.SQL[SynchroThread.SQLStmtIndex], StmtLength, ServerVersion);
      FDatabaseName := CLStmt.ObjectName;
      S := '--> Database selected: ' + DatabaseName;
      WriteMonitor(PChar(S), Length(S), ttInfo);
    end
    else if (not Assigned(SynchroThread.ResultHandle)) then
    begin
      if (Lib.mysql_affected_rows(SynchroThread.LibHandle) >= 0) then
      begin
        if (FRowsAffected < 0) then FRowsAffected := 0;
        Inc(FRowsAffected, Lib.mysql_affected_rows(SynchroThread.LibHandle));
      end;

      if (Assigned(Lib.mysql_info) and Assigned(Lib.mysql_info(SynchroThread.LibHandle))) then
      begin
        S := '--> ' + LibDecode(Lib.mysql_info(SynchroThread.LibHandle));
        WriteMonitor(PChar(S), Length(S), ttInfo);
      end
      else if (Lib.mysql_affected_rows(SynchroThread.LibHandle) > 0) then
      begin
        S := '--> ' + IntToStr(Lib.mysql_affected_rows(SynchroThread.LibHandle)) + ' Record(s) affected';
        WriteMonitor(PChar(S), Length(S), ttInfo);
      end
      else
      begin
        S := '--> Ok';
        WriteMonitor(PChar(S), Length(S), ttInfo);
      end;

      if ((ServerVersion > 40100) and Assigned(Lib.mysql_warning_count)) then
      begin
        if (FWarningCount < 0) then FWarningCount := 0;
        Inc(FWarningCount, Lib.mysql_warning_count(SynchroThread.LibHandle));

        if (Lib.mysql_warning_count(SynchroThread.LibHandle) > 0) then
        begin
          S := '--> ' + IntToStr(Lib.mysql_warning_count(SynchroThread.LibHandle)) + ' warning(s) available';
          WriteMonitor(PChar(S), Length(S), ttInfo);
        end;
      end;
    end;
  end;

  if (FErrorCode > 0) then
    SynchroThread.State := ssReady
  else
    SynchroThread.State := ssResult;
end;

procedure TMySQLConnection.SyncHandledResult(const SynchroThread: TSynchroThread);
var
  S: String;
  StmtLength: Integer;
begin
  FErrorCode := SynchroThread.ErrorCode;
  FErrorMessage := SynchroThread.ErrorMessage;

  if (not SynchroThread.Terminated and (SynchroThread.State = ssResult)) then
  begin
    if (Assigned(SynchroThread.ResultHandle)) then
      while (Assigned(Lib.mysql_fetch_row(SynchroThread.ResultHandle))) do ;
  end;

  if (not SynchroThread.Terminated) then
    if (FErrorCode > 0) then
    begin
      S := '--> Error #' + IntToStr(FErrorCode) + ': ' + FErrorMessage + ' while receiving Record(s)';
      WriteMonitor(PChar(S), Length(S), ttInfo);

      SynchroThread.State := ssReady;
    end
    else
    begin
      if (Assigned(SynchroThread.ResultHandle)) then
      begin
        S := '--> ' + IntToStr(Lib.mysql_num_rows(SynchroThread.ResultHandle)) + ' Record(s) received';
        WriteMonitor(PChar(S), Length(S), ttInfo);

        Lib.mysql_free_result(SynchroThread.ResultHandle);
        SynchroThread.ResultHandle := nil;
      end;

      if (MultiStatements and Assigned(SynchroThread.LibHandle) and (Lib.mysql_more_results(SynchroThread.LibHandle) <> 0)) then
      begin
        if (SynchroThread.SQLStmtInPacket + 1 < Integer(SynchroThread.SQLStmtsInPackets[SynchroThread.SQLPacket])) then
        begin
          Inc(FExecutedSQLLength, Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]));
          Inc(FExecutedStmts);

          Inc(SynchroThread.SQLStmtIndex, Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]));
          Inc(SynchroThread.SQLStmt);
          Inc(SynchroThread.SQLStmtInPacket);

          StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);
          WriteMonitor(@SynchroThread.SQL[SynchroThread.SQLStmtIndex], StmtLength, ttRequest);
        end;

        SynchroThread.State := ssNextResult;
      end
      else
      begin
        Inc(FExecutedSQLLength, Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]));
        Inc(FExecutedStmts);

        Inc(SynchroThread.SQLStmtIndex, Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]));
        Inc(SynchroThread.SQLStmt);
        Inc(SynchroThread.SQLPacket);
        SynchroThread.SQLStmtInPacket := 0;

        if (SynchroThread.SQLPacket < SynchroThread.SQLStmtsInPackets.Count) then
        begin
          SynchroThread.SQLLastStmtInPacket := SynchroThread.SQLStmt + Integer(SynchroThread.SQLStmtsInPackets[SynchroThread.SQLPacket]) - 1;

          S := '# ' + SysUtils.DateTimeToStr(Now() + TimeDiff, FormatSettings);
          WriteMonitor(PChar(S), Length(S), ttTime);

          StmtLength := Integer(SynchroThread.SQLStmtLengths[SynchroThread.SQLStmt]);
          WriteMonitor(@SynchroThread.SQL[SynchroThread.SQLStmtIndex], StmtLength, ttRequest);

          SynchroThread.State := ssExecutingSQL;
        end
        else
          SynchroThread.State := ssReady;
      end;
    end;
end;

procedure TMySQLConnection.SyncHandlingResult(const SynchroThread: TSynchroThread);
begin
  InOnResult := True;
  if ((not Assigned(SynchroThread.OnResult) or not SynchroThread.OnResult(SynchroThread, Assigned(SynchroThread.ResultHandle)))
    and (ErrorCode > 0)) then
    DoError(ErrorCode, ErrorMessage);
  InOnResult := False;

  if (SynchroThread.State = ssResult) then
  begin
    SynchroThread.State := ssReceivingResult;
    SyncHandledResult(SynchroThread);
  end;
end;

procedure TMySQLConnection.SyncNextResult(const SynchroThread: TSynchroThread);
var
  Time: TDateTime;
begin
  Assert(SynchroThread.State = ssNextResult);

  Time := Now();
  SynchroThread.Success := MultiStatements and (Lib.mysql_next_result(SynchroThread.LibHandle) <= 0);
  SynchroThread.Time := SynchroThread.Time + Now() - Time;
  if (SynchroThread.Success) then
  begin
    SynchroThread.ResultHandle := Lib.mysql_use_result(SynchroThread.LibHandle);
    SynchroThread.Success := Assigned(SynchroThread.ResultHandle);
  end;

  SynchroThread.ErrorCode := Lib.mysql_errno(SynchroThread.LibHandle);
  SynchroThread.ErrorMessage := ErrorMsg(SynchroThread.LibHandle);
end;

procedure TMySQLConnection.SyncPing(const SynchroThread: TSynchroThread);
begin
  if (Assigned(Lib.mysql_ping)) then
    Lib.mysql_ping(SynchroThread.LibHandle);
end;

procedure TMySQLConnection.SyncReceivingResult(SynchroThread: TSynchroThread);
var
  LibRow: MYSQL_ROW;
begin
  Assert(Assigned(SynchroThread.DataSet));


  repeat
    if ((SynchroThread.Terminated) or not Assigned(SynchroThread.DataSet)) then
      LibRow := nil
    else
      LibRow := Lib.mysql_fetch_row(SynchroThread.ResultHandle);

    TerminateCS.Enter();
    TMySQLDataSet(SynchroThread.DataSet).InternAddRecord(LibRow, Lib.mysql_fetch_lengths(SynchroThread.ResultHandle));
    TerminateCS.Leave();
  until (not Assigned(LibRow));

  SynchroThread.Success := Lib.mysql_errno(SynchroThread.LibHandle) = 0;

  SynchroThread.ErrorCode := Lib.mysql_errno(SynchroThread.LibHandle);
  SynchroThread.ErrorMessage := ErrorMsg(SynchroThread.LibHandle);
end;

procedure TMySQLConnection.Terminate();
var
  S: string;
begin
  TerminateCS.Enter();

  if (Assigned(SynchroThread)) then
  begin
    if (SynchroThread.IsRunning) then
    begin
      S := '----> Connection Terminated <----';
      WriteMonitor(PChar(S), Length(S), ttInfo);
    end;
    SynchroThread.Terminate();
    FSynchroThread := nil;
  end;

  TerminateCS.Leave();
end;

procedure TMySQLConnection.UnRegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor);
var
  I: Integer;
  Index: Integer;
begin
  Index := -1;
  for I := 0 to Length(FSQLMonitors) - 1 do
    if (AMySQLMonitor = FSQLMonitors[I]) then
      Index := I;

  if (Index >= 0) then
  begin
    for I := Length(FSQLMonitors) - 2 downto Index do
      FSQLMonitors[I] := FSQLMonitors[I + 1];

    SetLength(FSQLMonitors, Length(FSQLMonitors) - 1);
  end;
end;

function TMySQLConnection.UseCompression(): Boolean;
begin
  Result := True or (LibraryType = ltHTTP) or not ((lstrcmpi(PChar(Host), LOCAL_HOST) = 0) or (Host = '127.0.0.1') or (Host = '::1'));
end;

function TMySQLConnection.UseSynchroThread(): Boolean;
begin
  Result := Asynchron and Assigned(MySQLConnectionOnSynchronize) and (FSynchronCount = 0);
end;

procedure TMySQLConnection.WriteMonitor(const AText: PChar; const Length: Integer; const ATraceType: TMySQLMonitor.TTraceType);
var
  I: Integer;
begin
  InMonitor := True;
  for I := 0 to System.Length(FSQLMonitors) - 1 do
    if (ATraceType in FSQLMonitors[I].TraceTypes) then
      FSQLMonitors[I].DoMonitor(Self, AText, Length, ATraceType);
  InMonitor := False;
end;

{ TMySQLBitField **************************************************************}

function TMySQLBitField.GetAsString(): string;
begin
  if (not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1])) then
    Result := ''
  else
    Result := IntToBitString(UInt64(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]^));
end;

procedure TMySQLBitField.GetText(var Text: string; DisplayText: Boolean);
var
  FmtStr: string;
  L: Largeint;
begin
  if (not GetValue(L)) then
    Text := ''
  else
  begin
    if (DisplayText or (EditFormat = '')) then
      FmtStr := DisplayFormat
    else
      FmtStr := EditFormat;
    while ((Length(FmtStr) > 0) and (FmtStr[1] = '#')) do Delete(FmtStr, 1, 1);
    Text := IntToBitString(L, Length(FmtStr));
  end;
end;

procedure TMySQLBitField.SetAsString(const Value: string);
begin
  SetAsLargeInt(BitStringToInt(PChar(Value), Length(Value)));
end;

{ TLargeWordField *************************************************************}

procedure TLargeWordField.CheckRange(Value, Min, Max: UInt64);
begin
  if ((Value < Min) or (Value > Max)) then RangeError(Value, Min, Max);
end;

function TLargeWordField.GetAsString(): string;
begin
  Result := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TLargeWordField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TLargeWordField.SetAsLargeInt(Value: Largeint);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
    CheckRange(UInt64(Value), UInt64(FMinValue), UInt64(FMaxValue));
  SetData(@Value);
end;

procedure TLargeWordField.SetAsString(const Value: string);
begin
  if (Value = '') then
    Clear()
  else
    SetAsLargeint(StrToUInt64(Value));
end;

{ TMySQLWideStringField *******************************************************}

function TMySQLWideStringField.GetAsDateTime(): TDateTime;
begin
  Result := MySQLDB.StrToDateTime(GetAsString(), TMySQLQuery(DataSet).Connection.FormatSettings);
end;

procedure TMySQLWideStringField.SetAsDateTime(Value: TDateTime);
begin
  SetAsString(MySQLDB.DateTimeToStr(Value, TMySQLQuery(DataSet).Connection.FormatSettings));
end;

{ TMySQLDateField *************************************************************}

procedure TMySQLDateField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLDateField.SetAsString(const Value: string);
begin
  try
    AsDateTime := MySQLDB.StrToDate(Value, TMySQLQuery(DataSet).Connection.FormatSettings);
  except
    on E: EConvertError do
      TMySQLQuery(DataSet).Connection.DoConvertError(Self, Value, E);
  end;
end;

procedure TMySQLDateField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  ZeroDateString := GetZeroDateString(TMySQLQuery(DataSet).Connection.FormatSettings);

  if (DataSet is TMySQLQuery) then
    ValidChars := ['0'..'9', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.DateSeparator]
  else
    ValidChars := ['0'..'9', '-'];
end;

{ TMySQLDateTimeField *********************************************************}

procedure TMySQLDateTimeField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLDateTimeField.SetAsString(const Value: string);
begin
  if (Value = '') then
    AsDateTime := -1
  else
    AsDateTime := MySQLDB.StrToDateTime(Value, TMySQLQuery(DataSet).Connection.FormatSettings);
end;

procedure TMySQLDateTimeField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  if (DataSet is TMySQLQuery) then
    ValidChars := ['0'..'9', ' ', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.DateSeparator, TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.TimeSeparator]
  else
    ValidChars := ['0'..'9', ' ', '-', ':'];

  ZeroDateString := GetZeroDateString(TMySQLQuery(DataSet).Connection.FormatSettings);
end;

function TMySQLTimeField.GetAsDateTime(): TDateTime;
var
  Hour: Word;
  Min: Word;
  Sec: Word;
  Time: Integer;
begin
  if (not GetData(@Time)) then
    Result := Null
  else
  begin
    Sec := Time mod 60; Time := Time div 60;
    Min := Time mod 60; Time := Time div 60;
    Hour := Time mod 60;
    Result := EncodeTime(Hour, Min, Sec, 0);
  end;
end;

function TMySQLTimeField.GetAsString(): string;
begin
  Result := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

function TMySQLTimeField.GetAsVariant(): Variant;
var
  Time: Integer;
begin
  if (not GetData(@Time)) then
    Result := Null
  else
    Result := TimeToStr(Time, SQLFormat);
end;

function TMySQLTimeField.GetDataSize(): Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TMySQLTimeField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLTimeField.SetAsString(const Value: string);
begin
  try
    AsInteger := StrToTime(Value, SQLFormat);
  except
    on E: EConvertError do
      if (DataSet is TMySQLQuery) then
        TMySQLQuery(DataSet).Connection.DoConvertError(Self, Value, E)
      else
        raise EConvertError.CreateFmt(SInvalidTime, [Value]);
  end;
end;

procedure TMySQLTimeField.SetAsVariant(const Value: Variant);
begin
  AsString := Value;
end;

procedure TMySQLTimeField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  if (DataSet is TMySQLQuery) then
    ValidChars := ['-', '0'..'9', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.TimeSeparator]
  else
    ValidChars := ['-', '0'..'9', ':'];
end;

{ TMySQLTimeField *************************************************************}

constructor TMySQLTimeField.Create(AOwner: TComponent);
begin
  inherited;

  SetDataType(ftTime);
end;

{ TMySQLTimeStampField ********************************************************}

function TMySQLTimeStampField.GetAsSQLTimeStamp(): TSQLTimeStamp;
begin
  if (not GetData(@Result)) then
    Result := NULLSQLTimeStamp;
end;

function TMySQLTimeStampField.GetAsVariant: Variant;
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  if (not GetData(@SQLTimeStamp)) then
    Result := ''
  else
    Result := MySQLTimeStampToStr(SQLTimeStamp, SQLFormatToDisplayFormat(SQLFormat));
end;

function TMySQLTimeStampField.GetDataSize: Integer;
begin
  Result := SizeOf(TSQLTimeStamp);
end;

function TMySQLTimeStampField.GetOldValue: Variant;
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  if (not GetData(@SQLTimeStamp)) then
    Result := ''
  else
    Result := MySQLTimeStampToStr(SQLTimeStamp, SQLFormatToDisplayFormat(SQLFormat));
end;

procedure TMySQLTimeStampField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLTimeStampField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  SetData(@Value);
end;

procedure TMySQLTimeStampField.SetAsString(const Value: string);
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  if (Value = '') then
  begin
    SQLTimeStamp.Year := Word(-32767);
    SQLTimeStamp.Month := 0;
    SQLTimeStamp.Day := 0;
    SQLTimeStamp.Hour := 0;
    SQLTimeStamp.Minute := 0;
    SQLTimeStamp.Second := 0;
    SQLTimeStamp.Fractions := 0;
    AsSQLTimeStamp := SQLTimeStamp;
  end
  else if (DisplayFormat <> '') then
    AsSQLTimeStamp := StrToMySQLTimeStamp(Value, DisplayFormatToSQLFormat(DisplayFormat))
  else
    AsSQLTimeStamp := StrToMySQLTimeStamp(Value, SQLFormat);
end;

procedure TMySQLTimeStampField.SetAsVariant(const Value: Variant);
begin
  SetAsSQLTimeStamp(StrToMySQLTimeStamp(Value, SQLFormat));
end;

procedure TMySQLTimeStampField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  if (DataSet is TMySQLQuery) then
    if (TMySQLConnection(TMySQLQuery(DataSet).Connection).ServerVersion >= 40100) then
      ValidChars := ['0'..'9', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.DateSeparator, TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.TimeSeparator, ' ']
    else
      ValidChars := ['0'..'9']
  else
    ValidChars := ['0'..'9', '-', ':', ' '];
end;

{ TMySQLBlobField *************************************************************}

function TMySQLBlobField.GetAsVariant(): Variant;
begin
  if (IsNull) then
    Result := Null
  else
    Result := GetAsString();
end;

procedure TMySQLBlobField.SetAsString(const Value: string);
begin
  if (Length(Value) > 0) then
    inherited SetAsString(Value)
  else
    with DataSet.CreateBlobStream(Self, bmWrite) do
      try
        Write(Value, 0);
      finally
        Free();
      end;
end;

{ TMySQLWideMemoField *************************************************************}

function TMySQLWideMemoField.GetAsVariant(): Variant;
begin
  if (IsNull) then
    Result := Null
  else
    Result := GetAsString();
end;

procedure TMySQLWideMemoField.SetAsString(const Value: string);
begin
  if (Length(Value) > 0) then
    inherited SetAsString(Value)
  else
    with DataSet.CreateBlobStream(Self, bmWrite) do
      try
        Write(Value, 0);
      finally
        Free();
      end;
end;

{ TMySQLQueryBlobStream *******************************************************}

constructor TMySQLQueryBlobStream.Create(const AField: TBlobField);
begin
  Assert(AField.DataSet is TMySQLQuery);
  

  inherited Create();

  case (AField.DataType) of
    ftBlob:
      begin
        SetSize(TMySQLQuery.PRecordBufferData(AField.DataSet.ActiveBuffer())^.LibLengths^[AField.FieldNo - 1]);
        Move(TMySQLQuery.PRecordBufferData(AField.DataSet.ActiveBuffer())^.LibRow^[AField.FieldNo - 1]^, Memory^, TMySQLQuery.PRecordBufferData(AField.DataSet.ActiveBuffer())^.LibLengths^[AField.FieldNo - 1]);
      end;
    ftWideMemo: TMySQLQuery(AField.DataSet).DataConvert(AField, AField.DataSet.ActiveBuffer(), Self, False);
  end;
end;

function TMySQLQueryBlobStream.Write(const Buffer; Len: Integer): Integer;
begin
  Result := inherited Write(Buffer, Len);
end;

procedure TMySQLQueryBlobStream.WriteBuffer(const Buffer; Count: Longint);
begin
  inherited WriteBuffer(Buffer, Count);
end;

{ TMySQLQuery *****************************************************************}

function TMySQLQuery.AllocRecordBuffer(): TRecordBuffer;
var
  Data: PRecordBufferData;
begin
  New(Data);
  Data^.LibLengths := nil;
  Data^.LibRow := nil;

  Result := TRecordBuffer(Data);

  InitRecord(Result);
end;

constructor TMySQLQuery.Create(AOwner: TComponent);
begin
  inherited;

  FCommandText := '';
  FCommandType := ctQuery;
  FConnection := nil;
  FDatabaseName := '';
  FTableName := '';
  SynchroThread := nil;

  FIndexDefs := TIndexDefs.Create(Self);
  FRecNo := -1;

  SetUniDirectional(True);
end;

function TMySQLQuery.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  case (Field.DataType) of
    ftBlob: Result := TMySQLQueryBlobStream.Create(TMySQLBlobField(Field));
    ftWideMemo: Result := TMySQLQueryMemoStream.Create(TMySQLWideMemoField(Field));
    else Result := inherited CreateBlobStream(Field, Mode);
  end;
end;

procedure TMySQLQuery.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
var
  Len: Integer;
  S: string;
begin
  case (Field.DataType) of
    ftWideMemo:
      if (ToNative) then
      begin
        if (not Assigned(TMemoryStream(Source).Memory)) then
          Len := 0
        else
        begin
          Len := WideCharToMultiByte(Connection.CodePage, 0,
            TMemoryStream(Source).Memory, TMemoryStream(Source).Size div SizeOf(WideChar),
            nil, 0, nil, nil);
          if ((Len = 0) and (GetLastError() <> 0)) then
            DatabaseErrorFmt(SysErrorMessage(GetLastError()), [Field.DisplayName]);
        end;
        SetLength(RawByteString(Dest^), Len);
        if (Len > 0) then
          WideCharToMultiByte(Connection.CodePage, 0,
            TMemoryStream(Source).Memory, TMemoryStream(Source).Size div SizeOf(WideChar),
            PAnsiChar(RawByteString(Dest^)), Len, nil, nil);
      end
      else
      begin
        Len := PRecordBufferData(Source)^.LibLengths^[Field.FieldNo - 1];
        if (Len > 0) then
        begin
          Len := MultiByteToWideChar(Connection.CodePage, MB_ERR_INVALID_CHARS,
            PRecordBufferData(Source)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source)^.LibLengths^[Field.FieldNo - 1],
            nil, 0);
          TMemoryStream(Dest).SetSize(Len * SizeOf(WideChar));
          if (Len > 0) then
            MultiByteToWideChar(Connection.CodePage, 0,
              PRecordBufferData(Source)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source)^.LibLengths^[Field.FieldNo - 1],
              TMemoryStream(Dest).Memory, Len)
          else if (GetLastError() <> 0) then
            DatabaseErrorFmt(SysErrorMessage(GetLastError()) + ' (Field: %s)', [Field.Origin]);
        end;
      end;
    ftWideString:
      if (ToNative) then
      begin
        if (not Assigned(Source)) then
          Len := 0
        else
        begin
          Len := WideCharToMultiByte(Connection.CodePage, 0,
            PChar(Source), -1, nil, 0, nil, nil);
          if ((Len = 0) and (GetLastError() <> 0)) then
            DatabaseErrorFmt(SysErrorMessage(GetLastError()), [Field.DisplayName]);
        end;
        if (Len > 0) then
          WideCharToMultiByte(Connection.CodePage, 0,
            PChar(Source), -1, PAnsiChar(Dest), Field.DataSize, nil, nil);
      end
      else
      begin
        Len := PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1];
        if (Len > 0) then
        begin
          Len := MultiByteToWideChar(Connection.CodePage, MB_ERR_INVALID_CHARS,
            PRecordBufferData(Source^)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1],
            nil, 0);
          if (Len > Field.DataSize) then
            DatabaseErrorFmt(SInvalidFieldSize + ' (%s)', [Field.DisplayName])
          else if (Len > 0) then
            MultiByteToWideChar(Connection.CodePage, 0,
              PRecordBufferData(Source^)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1],
              PChar(Dest), Field.DataSize)
          else if (GetLastError() <> 0) then
          begin
            S := SQLEscape(PRecordBufferData(Source^)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1], False);
            DatabaseErrorFmt(SysErrorMessage(GetLastError()) + ' (%s - %s)', [IntToStr(Connection.CodePage), S]);
          end;
        end;
        PChar(Dest)[Len] := #0;
      end;
    else
      inherited DataConvert(Field, Source, Dest, ToNative);
  end;
end;

destructor TMySQLQuery.Destroy();
begin
  Close();
  Connection := nil;

  FIndexDefs.Free();

  inherited;
end;

function TMySQLQuery.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  Result := not Restart and GoForward and (MoveBy(1) <> 0);

  SetFound(Result);
end;

procedure TMySQLQuery.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Dispose(Buffer); Buffer := nil;
end;

function TMySQLQuery.GetAsString(const FieldNo: Integer): string;
begin
  if (not Assigned(LibRow^[FieldNo - 1])) then
    Result := ''
  else if (BitField(Fields[FieldNo - 1])) then
    Result := Fields[FieldNo - 1].AsString
  else if (Fields[FieldNo - 1].DataType in UnquotedDataTypes + BinaryDataTypes) then
    Result := Connection.LibUnpack(LibRow^[FieldNo - 1], LibLengths^[FieldNo - 1])
  else
    Result := Connection.LibDecode(LibRow^[FieldNo - 1], LibLengths^[FieldNo - 1]);
end;

function TMySQLQuery.GetCanModify(): Boolean;
begin
  Result := False;
end;

function TMySQLQuery.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Data: PRecordBufferData;
begin
  Data := PRecordBufferData(ActiveBuffer());
  Result := GetFieldData(Field, Buffer, Data);
end;

function TMySQLQuery.GetFieldData(const Field: TField; const Buffer: Pointer; const Data: PRecordBufferData): Boolean;
var
  DT: TDateTime;
  S: string;
begin
  Result := Assigned(Data) and (Field.FieldNo > 0) and Assigned(Data^.LibRow^[Field.FieldNo - 1]);
  if (Result and Assigned(Buffer)) then
    try
      if (BitField(Field)) then
        begin
          ZeroMemory(Buffer, Field.DataSize);
          MoveMemory(Buffer, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
        end
      else
        case (Field.DataType) of
          ftString: begin Move(Data^.LibRow^[Field.FieldNo - 1]^, Buffer^, Data^.LibLengths^[Field.FieldNo - 1]); PAnsiChar(Buffer)[Data^.LibLengths^[Field.FieldNo - 1]] := #0; end;
          ftShortInt: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); ShortInt(Buffer^) := StrToInt(S); end;
          ftByte: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Byte(Buffer^) := StrToInt(S); end;
          ftSmallInt: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Smallint(Buffer^) := StrToInt(S); end;
          ftWord: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Word(Buffer^) := StrToInt(S); end;
          ftInteger: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Longint(Buffer^) := StrToInt(S); end;
          ftLongWord: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); LongWord(Buffer^) := StrToInt64(S); end;
          ftLargeint:
            if (not (Field is TLargeWordField)) then
              begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Largeint(Buffer^) := StrToInt64(S); end
            else
              begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); UInt64(Buffer^) := StrToUInt64(S); end;
          ftSingle: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Single(Buffer^) := StrToFloat(S, Connection.FormatSettings); end;
          ftFloat: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Double(Buffer^) := StrToFloat(S, Connection.FormatSettings); end;
          ftExtended: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Extended(Buffer^) := StrToFloat(S, Connection.FormatSettings); end;
          ftDate: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); DT := MySQLDB.StrToDate(S, Connection.FormatSettings); DataConvert(Field, @DT, Buffer, True); end;
          ftDateTime: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); DT := MySQLDB.StrToDateTime(S, Connection.FormatSettings); DataConvert(Field, @DT, Buffer, True); end;
          ftTime: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Longint(Buffer^) := StrToTime(S, TMySQLTimeField(Field).SQLFormat); end;
          ftTimeStamp: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); PSQLTimeStamp(Buffer)^ := StrToMySQLTimeStamp(S, TMySQLTimeStampField(Field).SQLFormat); end;
          ftBlob: begin TMemoryStream(Buffer).SetSize(Data^.LibLengths^[Field.FieldNo - 1]); Move(Data^.LibRow^[Field.FieldNo - 1]^, TMemoryStream(Buffer).Memory^, Data^.LibLengths^[Field.FieldNo - 1]); end;
          ftWideString: Move(Data, Buffer^, SizeOf(Data));
          ftWideMemo: DataConvert(Field, Data, Buffer, False);
          else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
        end;
    except
      on E: EConvertError do
      begin
        if (FInformConvertError) then
        begin
          FInformConvertError := False;
          SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
          Connection.DoConvertError(Field, S, E);
        end;
        Result := False;
      end;
    end;
end;

function TMySQLQuery.GetHandle(): MySQLConsts.MYSQL_RES;
begin
  if (not Assigned(Connection)) then
    Result := nil
  else
  begin
    Connection.TerminateCS.Enter();
    if (not Assigned(SynchroThread)) then
      Result := nil
    else
      Result := SynchroThread.ResultHandle;
    Connection.TerminateCS.Leave();
  end;
end;

function TMySQLQuery.GetLibLengths(): MYSQL_LENGTHS;
begin
  Result := PRecordBufferData(ActiveBuffer())^.LibLengths;
end;

function TMySQLQuery.GetLibRow(): MYSQL_ROW;
begin
  Result := PRecordBufferData(ActiveBuffer())^.LibRow;
end;

function TMySQLQuery.GetRecNo(): Integer;
begin
  Result := FRecNo;
end;

function TMySQLQuery.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if (GetMode <> gmNext) then
    Result := grError
  else if (not Assigned(SynchroThread.ResultHandle)) then
    Result := grEOF
  else
  begin
    PRecordBufferData(ActiveBuffer())^.LibRow := Connection.Lib.mysql_fetch_row(SynchroThread.ResultHandle);
    if (Assigned(PRecordBufferData(ActiveBuffer())^.LibRow)) then
    begin
      PRecordBufferData(ActiveBuffer())^.LibLengths := Connection.Lib.mysql_fetch_lengths(SynchroThread.ResultHandle);

      Inc(FRecNo);
      Result := grOk;
    end
    else if (Connection.Lib.mysql_errno(Connection.SynchroThread.LibHandle) <> 0) then
    begin
      SynchroThread.ErrorCode := Connection.Lib.mysql_errno(Connection.SynchroThread.LibHandle);
      SynchroThread.ErrorMessage := Connection.ErrorMsg(Connection.SynchroThread.LibHandle);
      Result := grError;
    end
    else
    begin
      Result := grEOF;

      Connection.TerminateCS.Enter();
      SynchroThread.ReleaseDataSet();
      SynchroThread := nil;
      Connection.TerminateCS.Leave();
    end;
  end;
end;

function TMySQLQuery.GetRecordCount(): Integer;
begin
  Result := FRecNo + 1;
end;

function TMySQLQuery.GetUniDirectional(): Boolean;
begin
  Result := True;
end;

procedure TMySQLQuery.InternalClose();
begin
  Connection.TerminateCS.Enter();
  if (Assigned(SynchroThread)) then
  begin
    SynchroThread.ReleaseDataSet();
    SynchroThread := nil;
  end;
  Connection.TerminateCS.Leave();

  FIndexDefs.Clear();

  if (not (Self is TMySQLTable)) then
  begin
    FDatabaseName := '';
    FTableName := '';
  end;

  FieldDefs.Clear();
  Fields.Clear();
end;

procedure TMySQLQuery.InternalHandleException();
begin
  Application.HandleException(Self);
end;

procedure TMySQLQuery.InternalInitFieldDefs();
var
  Binary: Boolean;
  CreateField: Boolean;
  Decimals: Word;
  DName: string;
  Field: TField;
  I: Integer;
  Len: Longword;
  LibField: MYSQL_FIELD;
  S: string;
  UniqueDatabaseName: Boolean;
  UniqueTableName: Boolean;
begin
  if (FieldDefs.Count = 0) then
  begin
    if (not Assigned(Handle)) then
      for I := 0 to FieldCount - 1 do
      begin
        if (Fields[I].FieldName = '') then Fields[I].FieldName := Fields[I].Name;
        FieldDefs.Add(Fields[I].FieldName, Fields[I].DataType, Fields[I].Size, Fields[I].Required);
      end
    else
    begin
      FieldDefs.Clear();
      UniqueDatabaseName := True; UniqueTableName := True;

      repeat
        LibField := MYSQL_FIELD(Connection.Lib.mysql_fetch_field(Handle));

        if (Assigned(LibField)) then
        begin
          if ((Connection.ServerVersion < 40101) or (Connection.Lib.Version < 40101)) then
          begin
            Binary := Connection.Lib.Field(LibField).flags and BINARY_FLAG <> 0;
            if (not (Connection.Lib.Field(LibField).field_type in [MYSQL_TYPE_ENUM, MYSQL_TYPE_SET, MYSQL_TYPE_TINY_BLOB, MYSQL_TYPE_MEDIUM_BLOB, MYSQL_TYPE_LONG_BLOB, MYSQL_TYPE_BLOB, MYSQL_TYPE_VAR_STRING, MYSQL_TYPE_STRING]) or (Connection.Lib.Field(LibField).flags and BINARY_FLAG <> 0)) then
              Len := Connection.Lib.Field(LibField).length
            else if (Connection.ServerVersion <= 40109) then // In 40109 this is needed. In 40122 and higher the problem is fixed. What is the exact ServerVersion?
              Len := Connection.Lib.Field(LibField).length
            else
            begin
              Len := Connection.Lib.Field(LibField).length;
              for I := 0 to Length(MySQL_Character_Sets) - 1 do
                if (lstrcmpiA(MySQL_Character_Sets[I].CharsetName, PAnsiChar(AnsiString(Connection.Charset))) = 0) then
                  Len := Connection.Lib.Field(LibField).length div MySQL_Character_Sets[I].MaxLen;
            end;
          end
          else
          begin
            Binary := Connection.Lib.Field(LibField).charsetnr = 63;
            if (Binary) then
              Len := Connection.Lib.Field(LibField).length
            else if (Connection.ServerVersion <= 40109) then // In 40109 this is needed. In 40122 and higher the problem is fixed. What is the exact ServerVersion?
              Len := Connection.Lib.Field(LibField).length
            else
            begin
              Len := Connection.Lib.Field(LibField).length;
              for I := 0 to Length(MySQL_Collations) - 1 do
                if (MySQL_Collations[I].CharsetNr = Connection.Lib.Field(LibField).charsetnr) then
                  Len := Connection.Lib.Field(LibField).length div MySQL_Collations[I].MaxLen;
            end;
          end;
          Len := Len and $7FFFFFFF;

          case (Connection.Lib.Field(LibField).field_type) of
            MYSQL_TYPE_NULL:
              Field := TField.Create(Self);
            MYSQL_TYPE_BIT:
              begin Field := TMySQLBitField.Create(Self); Field.Tag := ftBitField; end;
            MYSQL_TYPE_TINY:
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                Field := TShortIntField.Create(Self)
              else
                Field := TByteField.Create(Self);
            MYSQL_TYPE_SHORT:
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                Field := TSmallIntField.Create(Self)
              else
                Field := TWordField.Create(Self);
            MYSQL_TYPE_INT24,
            MYSQL_TYPE_LONG:
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                Field := TIntegerField.Create(Self)
              else
                Field := TLongWordField.Create(Self);
            MYSQL_TYPE_LONGLONG:
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                Field := TLargeintField.Create(Self)
              else
                Field := TLargeWordField.Create(Self);
            MYSQL_TYPE_FLOAT:
              Field := TSingleField.Create(Self);
            MYSQL_TYPE_DOUBLE:
              Field := TFloatField.Create(Self);
            MYSQL_TYPE_DECIMAL,
            MYSQL_TYPE_NEWDECIMAL:
              Field := TExtendedField.Create(Self);
            MYSQL_TYPE_TIMESTAMP:
              if (Len in [2, 4, 6, 8, 10, 12, 14]) then
                Field := TMySQLTimeStampField.Create(Self)
              else
                Field := TMySQLDateTimeField.Create(Self);
            MYSQL_TYPE_DATE:
              Field := TMySQLDateField.Create(Self);
            MYSQL_TYPE_TIME:
              Field := TMySQLTimeField.Create(Self);
            MYSQL_TYPE_DATETIME,
            MYSQL_TYPE_NEWDATE:
              Field := TMySQLDateTimeField.Create(Self);
            MYSQL_TYPE_YEAR:
              if (Len = 2) then
                Field := TByteField.Create(Self)
              else
                Field := TWordField.Create(Self);
            MYSQL_TYPE_ENUM,
            MYSQL_TYPE_SET:
              if (Binary) then
                begin Field := TBytesField.Create(Self); if (Connection.ServerVersion < 40100) then Field.Size := Len + 1 else Field.Size := Len; end
              else
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_TINY_BLOB,
            MYSQL_TYPE_MEDIUM_BLOB,
            MYSQL_TYPE_LONG_BLOB,
            MYSQL_TYPE_BLOB:
              if (Binary) then
                begin Field := TMySQLBlobField.Create(Self); Field.Size := Len; end
              else
                begin Field := TMySQLWideMemoField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_VAR_STRING,
            MYSQL_TYPE_STRING:
              if (Binary) then
                begin Field := TStringField.Create(Self); if (Connection.ServerVersion < 40100) then Field.Size := Len + 1 else Field.Size := Len; end
              else if (Len <= 255) then
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := 255; end
              else if ((Len <= 65535) and (Connection.ServerVersion >= 50000)) then
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := 65535; end
              else
                begin Field := TMySQLWideMemoField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_GEOMETRY:
              begin Field := TMySQLBlobField.Create(Self); Field.Size := Len; Field.Tag := ftGeometryField; end;
            else
              raise EDatabaseError.CreateFMT(SBadFieldType + ' (%d)', [Connection.Lib.Field(LibField).name, Byte(Connection.Lib.Field(LibField).field_type)]);
          end;

          Field.FieldName := Connection.LibDecode(Connection.Lib.Field(LibField).name);
          if (FieldDefs.IndexOf(Field.FieldName) >= 0) then
          begin
            I := 2;
            while (FieldDefs.IndexOf(Field.FieldName + '_' + IntToStr(I)) >= 0) do Inc(I);
            Field.FieldName := Field.FieldName + '_' + IntToStr(I);
          end;

          case (Connection.Lib.Field(LibField).field_type) of
            MYSQL_TYPE_TINY:  // 8 bit
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                begin TShortIntField(Field).MinValue := -$80; TShortIntField(Field).MaxValue := $7F; end
              else
                begin TByteField(Field).MinValue := 0; TByteField(Field).MaxValue := $FF; end;
            MYSQL_TYPE_SHORT: // 16 bit
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                begin TSmallIntField(Field).MinValue := -$8000; TSmallIntField(Field).MaxValue := $7FFF; end
              else
                begin TWordField(Field).MinValue := 0; TWordField(Field).MaxValue := $FFFF; end;
            MYSQL_TYPE_INT24: // 24 bit
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                begin TIntegerField(Field).MinValue := -$800000; TIntegerField(Field).MaxValue := $7FFFFF; end
              else
                begin TLongWordField(Field).MinValue := 0; TLongWordField(Field).MaxValue := $FFFFFF; end;
            MYSQL_TYPE_LONG: // 32 bit
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                begin TIntegerField(Field).MinValue := -$80000000; TIntegerField(Field).MaxValue := $7FFFFFFF; end
              else
                begin TLongWordField(Field).MinValue := 0; TLongWordField(Field).MaxValue := $FFFFFFFF; end;
            MYSQL_TYPE_LONGLONG: // 64 bit
              if (Connection.Lib.Field(LibField).flags and UNSIGNED_FLAG = 0) then
                begin TLargeintField(Field).MinValue := -$8000000000000000; TLargeintField(Field).MaxValue := $7FFFFFFFFFFFFFFF; end
              else
                begin TLargeWordField(Field).MinValue := 0; TLargeWordField(Field).MaxValue := $FFFFFFFFFFFFFFFF; end;
            MYSQL_TYPE_YEAR:
              if (Len = 2) then
                begin TByteField(Field).MinValue := 0; TByteField(Field).MaxValue := 99; end
              else
                begin TWordField(Field).MinValue := 1901; TWordField(Field).MaxValue := 2155; end
          end;

          Field.Required := Connection.Lib.Field(LibField).flags and NOT_NULL_FLAG <> 0;

          CreateField := Pos('.', Field.Origin) = 0;
          if (CreateField) then
          begin
            if ((Connection.Lib.Version >= 40100) and (Connection.Lib.Field(LibField).org_name_length > 0)) then
              Field.Origin := '"' + Connection.LibDecode(Connection.Lib.Field(LibField).org_name) + '"'
            else if (Connection.Lib.Field(LibField).name_length > 0) then
              Field.Origin := '"' + Connection.LibDecode(Connection.Lib.Field(LibField).name) + '"'
            else
              Field.Origin := '';
            if (Field.Origin <> '') then
              if ((Connection.Lib.Version >= 40000) and (Connection.Lib.Field(LibField).org_table_length > 0)) then
              begin
                Field.Origin := '"' + Connection.LibDecode(Connection.Lib.Field(LibField).org_table) + '".' + Field.Origin;
                if ((Connection.Lib.Version >= 40101) and (Connection.Lib.Field(LibField).db_length > 0)) then
                  Field.Origin := '"' + Connection.LibDecode(Connection.Lib.Field(LibField).db) + '".' + Field.Origin;
              end
              else if (Connection.Lib.Field(LibField).table_length > 0) then
                Field.Origin := '"' + Connection.LibDecode(Connection.Lib.Field(LibField).table) + '".' + Field.Origin;
            Field.ReadOnly := Field.Origin = '';
            if ((Connection.Lib.Version >= 40101) and (Connection.Lib.Field(LibField).db_length > 0)) then
              if (DName = '') then
                DName := Connection.LibDecode(Connection.Lib.Field(LibField).db)
              else
                UniqueDatabaseName := UniqueDatabaseName and (Connection.LibDecode(Connection.Lib.Field(LibField).db) = DName);
            if (Connection.Lib.Field(LibField).table_length > 0) then
              if (FTableName = '') then
                FTableName := Connection.LibDecode(Connection.Lib.Field(LibField).table)
              else
                UniqueTableName := UniqueTableName and (Connection.LibDecode(Connection.Lib.Field(LibField).table) = FTableName);

            if (Connection.Lib.Field(LibField).flags and (AUTO_INCREMENT_FLAG) <> 0) then
              Field.AutoGenerateValue := arAutoInc
            else
              Field.AutoGenerateValue := arNone;
            if (Field.DataType = ftTime) then
              TMySQLTimeField(Field).SQLFormat := DisplayFormatToSQLFormat(Connection.FormatSettings.LongTimeFormat)
            else if (Field.DataType = ftTimeStamp) then
              case (Len) of
                2: TMySQLTimeStampField(Field).SQLFormat := '%y';
                4: TMySQLTimeStampField(Field).SQLFormat := '%y%m';
                6: TMySQLTimeStampField(Field).SQLFormat := '%y%m%d';
                8: TMySQLTimeStampField(Field).SQLFormat := '%Y%m%d';
                10: TMySQLTimeStampField(Field).SQLFormat := '%y%m%d%H%i';
                12: TMySQLTimeStampField(Field).SQLFormat := '%y%m%d%H%i%s';
                14: TMySQLTimeStampField(Field).SQLFormat := '%Y%m%d%H%i%s';
              end;

            case (Field.DataType) of
              ftShortInt,
              ftByte,
              ftSmallInt,
              ftWord,
              ftInteger,
              ftLongWord,
              ftSingle,
              ftFloat,
              ftExtended:
                begin
                  if (Len = 0) then
                    S := '0'
                  else if (BitField(Field) or (Connection.Lib.Field(LibField).flags and ZEROFILL_FLAG <> 0)) then
                    S := StringOfChar('0', Len)
                  else
                    S := StringOfChar('#', Len - 1) + '0';
                  Decimals := Connection.Lib.Field(LibField).decimals;
                  if (Decimals > Len) then Decimals := Len;
                  if ((Field.DataType in [ftSingle, ftFloat, ftExtended]) and (Decimals > 0)) then
                  begin
                    System.Delete(S, 1, Decimals + 1);
                    S := S + '.' + StringOfChar('0', Decimals);
                  end;
                  if (Length(S) > 256 - 32) then // Limit given because of the usage of SysUtils.FormatFloat
                    System.Delete(S, 1, Length(S) - 32);
                  TNumericField(Field).DisplayFormat := S;
                end;
              ftDate: TMySQLDateField(Field).DisplayFormat := Connection.FormatSettings.ShortDateFormat;
              ftTime: TMySQLTimeField(Field).DisplayFormat := Connection.FormatSettings.LongTimeFormat;
              ftDateTime: TMySQLDateTimeField(Field).DisplayFormat := Connection.FormatSettings.ShortDateFormat + ' ' + Connection.FormatSettings.LongTimeFormat;
              ftTimeStamp: TMySQLTimeStampField(Field).DisplayFormat := SQLFormatToDisplayFormat(TMySQLTimeStampField(Field).SQLFormat);
            end;

            if (Assigned(Handle)) then
            begin
              Field.DisplayLabel := Connection.LibDecode(Connection.Lib.Field(LibField).name);
              case (Field.DataType) of
                ftBlob: Field.DisplayWidth := 7;
                ftWideMemo: Field.DisplayWidth := 8;
                else
                  case (Field.Tag) of
                    ftBitField: Field.DisplayWidth := Len;
                    ftGeometryField: Field.DisplayWidth := 7;
                    else Field.DisplayWidth := Len;
                  end;
              end;
            end;

            if ((Field.Name = '') and IsValidIdent(ReplaceStr(ReplaceStr(Field.FieldName, ' ', '_'), '.', '_'))) then
              Field.Name := ReplaceStr(ReplaceStr(Field.FieldName, ' ', '_'), '.', '_');
            if (Field.Name = '') then
              Field.Name := 'Field' + '_' + IntToStr(FieldDefs.Count);
            if (Field.FieldName = '') then
              Field.FieldName := Field.Name;

            if (Connection.Lib.Field(LibField).flags and PRI_KEY_FLAG = 0) then
              Field.ProviderFlags := Field.ProviderFlags - [pfInKey]
            else
              Field.ProviderFlags := Field.ProviderFlags + [pfInKey];

            if (Assigned(Handle)) then
              Field.DataSet := Self;

            FieldDefs.Add(Field.FieldName, Field.DataType, Field.Size, Field.Required);

            if (not Assigned(Handle)) then
              FreeAndNil(Field)
          end;
        end;
      until (not Assigned(LibField));

      if (UniqueDatabaseName and (DName <> '')) then
        FDatabaseName := DName;
      if (Self is TMySQLTable) then
        FTableName := CommandText
      else if (not UniqueTableName) then
        FTableName := '';
    end;
  end;
end;

procedure TMySQLQuery.InternalOpen();
begin
  Assert(Assigned(Connection) and Assigned(Connection.Lib) and not Assigned(SynchroThread));


  FInformConvertError := True;
  FRowsAffected := -1;
  FRecNo := -1;

  SynchroThread := Connection.SynchroThread;

  InitFieldDefs();
  BindFields(True);

  SynchroThread.BindDataSet(Self);
end;

function TMySQLQuery.IsCursorOpen(): Boolean;
begin
  Result := Assigned(SynchroThread);
end;

procedure TMySQLQuery.Open(const DataHandle: TMySQLConnection.TDataResult);
begin
  Connection := DataHandle.Connection;

  FCommandText := Connection.CommandText;
  FDatabaseName := Connection.DatabaseName;

  SetState(dsOpening);
  SetActiveEvent(DataHandle, Assigned(DataHandle.ResultHandle));
end;

procedure TMySQLQuery.SetActive(Value: Boolean);
begin
  if (not Value) then
    inherited
  else if (not Active) then
  begin
    if (not (Self is TMySQLTable)) then
      Assert(SQLSingleStmt(CommandText))
    else
      Assert(CommandText <> '');

    if ((Self is TMySQLTable) and Connection.UseSynchroThread()) then
    begin
      SetState(dsOpening);
      Connection.ExecuteSQL(smDataSet, False, TMySQLTable(Self).SQLSelect(), SetActiveEvent);
    end
    else if (Connection.ExecuteSQL(smDataSet, True, CommandText)) then
      inherited;
  end;
end;

function TMySQLQuery.SetActiveEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
begin
  Assert(not Assigned(SynchroThread));
  Assert(DataHandle = Connection.SynchroThread);

  if (not Data) then
    SetState(dsInactive)
  else
  begin
    DoBeforeOpen();
    OpenCursorComplete();
  end;

  Result := False;
end;

procedure TMySQLQuery.SetCommandText(const ACommandText: string);
begin
  Assert(not Active);


  FCommandText := ACommandText;
end;

procedure TMySQLQuery.SetConnection(const AConnection: TMySQLConnection);
begin
  Assert(not IsCursorOpen());


  if (not Assigned(FConnection) and Assigned(AConnection)) then
    AConnection.RegisterClient(Self);
  if (Assigned(FConnection) and not Assigned(AConnection)) then
    FConnection.UnRegisterClient(Self);

  FConnection := AConnection;
end;

function TMySQLQuery.SQLFieldValue(const Field: TField; Data: PRecordBufferData = nil): string;
begin
  if (not Assigned(Data)) then
    if (not (Self is TMySQLDataSet)) then
      Data := PRecordBufferData(ActiveBuffer())
    else
      Data := TMySQLDataSet.PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData;

  if (not Assigned(Data) or not Assigned(Data^.LibRow^[Field.FieldNo - 1]) and (not Field.Required or (Field.AutoGenerateValue = arAutoInc))) then
    Result := 'NULL'
  else if (not Assigned(Data^.LibRow^[Field.FieldNo - 1])) then
    case (Field.DataType) of
      ftString,
      ftShortInt,
      ftByte,
      ftSmallInt,
      ftWord,
      ftInteger,
      ftLongWord,
      ftLargeint,
      ftSingle,
      ftFloat,
      ftExtended: Result := '0';
      ftDate,
      ftTime,
      ftDateTime: if (Field.DefaultExpression = '') then Result := 'NULL' else Result := SQLEscape(Field.DefaultExpression);
      ftBlob,
      ftWideMemo,
      ftWideString: Result := '''''';
      ftTimeStamp: Result := '0';
      else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
    end
  else if (BitField(Field)) then
    Result := 'b''' + IntToBitString(UInt64(Data^.LibRow^[Field.FieldNo - 1]^)) + ''''
  else
    case (Field.DataType) of
      ftString: Connection.LibDecode(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
      ftShortInt,
      ftByte,
      ftSmallInt,
      ftWord,
      ftInteger,
      ftLongWord,
      ftLargeint,
      ftSingle,
      ftFloat,
      ftExtended: Result := Connection.LibUnpack(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
      ftDate,
      ftDateTime,
      ftTime: Result := '''' + Connection.LibUnpack(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]) + '''';
      ftTimeStamp: Connection.LibUnpack(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
      ftBlob: Result := SQLEscape(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1], Connection.ServerVersion <= 40000);
      ftWideMemo,
      ftWideString: Result := SQLEscape(Connection.LibDecode(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]));
      else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
    end;
end;

procedure TMySQLQuery.UpdateIndexDefs();
begin
  if (not Assigned(Handle) and not FIndexDefs.Updated) then
  begin
    if (Assigned(Connection.OnUpdateIndexDefs)) then
      Connection.OnUpdateIndexDefs(Self, FIndexDefs);
    FIndexDefs.Updated := True;
  end;
end;

{ TMySQLDataSetBlobStream *****************************************************}

constructor TMySQLDataSetBlobStream.Create(const AField: TBlobField; AMode: TBlobStreamMode);
begin
  inherited Create();

  Empty := True;
  Field := AField;
  Mode := AMode;

  if (Mode in [bmRead, bmReadWrite]) then
  begin
    Empty := not Assigned(TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData^.LibRow^[Field.FieldNo - 1]);
    if (Empty or (TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData^.LibLengths^[Field.FieldNo - 1] = 0)) then
      SetSize(0)
    else
      TMySQLDataSet(Field.DataSet).GetFieldData(Field, Self, TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData);
  end;
end;

destructor TMySQLDataSetBlobStream.Destroy();
begin
  if (Mode in [bmWrite, bmReadWrite]) then
  begin
    if (Empty) then
      TMySQLDataSet(Field.DataSet).SetFieldData(Field, nil)
    else
      TMySQLDataSet(Field.DataSet).SetFieldData(Field, Self);

    TMySQLDataSet(Field.DataSet).DataEvent(deFieldChange, Longint(Field));
  end;

  inherited;
end;

function TMySQLDataSetBlobStream.Write(const Buffer; Len: Integer): Longint;
begin
  Empty := False;
  Result := inherited Write(Buffer, Len);
end;

{ TMySQLDataSet.TInternRecordBuffers ******************************************}

procedure TMySQLDataSet.TInternRecordBuffers.Clear();
var
  I: Integer;
begin
  CriticalSection.Enter();
  for I := 0 to Count - 1 do
    DataSet.FreeInternRecordBuffer(Items[I]);
  inherited Clear();
  FilteredRecordCount := 0;
  Index := -1;
  CriticalSection.Leave();
end;

constructor TMySQLDataSet.TInternRecordBuffers.Create(const ADataSet: TMySQLDataSet);
begin
  inherited Create();

  FDataSet := ADataSet;

  CriticalSection := TCriticalSection.Create();
  Index := -1;
  FRecordReceived := TEvent.Create(nil, True, False, '');
end;

destructor TMySQLDataSet.TInternRecordBuffers.Destroy();
begin
  inherited;

  CriticalSection.Free();
  FRecordReceived.Free();
end;

function TMySQLDataSet.TInternRecordBuffers.Get(Index: Integer): PInternRecordBuffer;
begin
  Result := PInternRecordBuffer(Items[Index]);
end;

procedure TMySQLDataSet.TInternRecordBuffers.Put(Index: Integer; Buffer: PInternRecordBuffer);
begin
  Items[Index] := Buffer;
end;

{ TMySQLDataSet ***************************************************************}

procedure TMySQLDataSet.ActivateFilter();
var
  OldBookmark: TBookmark;
begin
  CheckBrowseMode();

  DisableControls();
  DoBeforeScroll();

  OldBookmark := Bookmark;

  InternActivateFilter();

  if (not BookmarkValid(OldBookmark)) then
    First()
  else
    Bookmark := OldBookmark;

  DoAfterScroll();
  EnableControls();
end;

function TMySQLDataSet.AllocInternRecordBuffer(): PInternRecordBuffer;
begin
  New(Result);

  Result^.NewData := nil;
  Result^.OldData := nil;
  Result^.VisibleInFilter := True;
end;

function TMySQLDataSet.AllocRecordBuffer(): TRecordBuffer;
begin
  New(PExternRecordBuffer(Result));

  PExternRecordBuffer(Result)^.InternRecordBuffer := nil;
  PExternRecordBuffer(Result)^.RecNo := -1;
  PExternRecordBuffer(Result)^.BookmarkFlag := bfInserted;
end;

function TMySQLDataSet.BookmarkToInternBufferIndex(const Bookmark: TBookmark): Integer;
var
  I: Integer;
begin
  Result := -1;

  if (Length(Bookmark) = BookmarkSize) then
  begin
    I := 0;
    while ((Result < 0) and (I < InternRecordBuffers.Count)) do
    begin
      if (PPointer(@Bookmark[0])^ = InternRecordBuffers[I]) then
        Result := I;
      Inc(I);
    end;
  end;
end;

function TMySQLDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  Index: Integer;
begin
  Result := (Length(Bookmark) = BookmarkSize);
  if (Result) then
  begin
    Index := BookmarkToInternBufferIndex(Bookmark);
    Result := (Index >= 0) and (not Filtered or InternRecordBuffers[Index]^.VisibleInFilter);
  end;
end;

function TMySQLDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  Result := Sign(BookmarkToInternBufferIndex(Bookmark1) - BookmarkToInternBufferIndex(Bookmark2));
end;

constructor TMySQLDataSet.Create(AOwner: TComponent);
begin
  inherited;

  FCanModify := False;
  FCommandType := ctQuery;
  FCursorOpen := False;
  FDataSize := 0;
  FilterParser := nil;
  FLocateNext := False;
  FRecordsReceived := TEvent.Create(nil, True, False, '');
  FSortDef := TIndexDef.Create(nil, 'SortDef', '', []);
  InternRecordBuffers := TInternRecordBuffers.Create(Self);

  BookmarkSize := SizeOf(BookmarkCounter);

  SetUniDirectional(False);
  FilterOptions := [foNoPartialCompare];
end;

function TMySQLDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  case (Field.DataType) of
    ftBlob: Result := TMySQLDataSetBlobStream.Create(TMySQLBlobField(Field), Mode);
    ftWideMemo: Result := TMySQLDataSetBlobStream.Create(TMySQLWideMemoField(Field), Mode);
    else Result := inherited CreateBlobStream(Field, Mode);
  end;
end;

procedure TMySQLDataSet.DeactivateFilter();
var
  OldBookmark: TBookmark;
begin
  CheckBrowseMode();
  DisableControls();
  DoBeforeScroll();

  OldBookmark := Bookmark;

  if (Assigned(FilterParser)) then
    FreeAndNil(FilterParser);

  if (not BookmarkValid(OldBookmark)) then
    First()
  else
    Bookmark := OldBookmark;

  DoAfterScroll();
  EnableControls();
end;

procedure TMySQLDataSet.Delete(const Bookmarks: array of TBookmark);
var
  I: Integer;
begin
  SetLength(DeleteBookmarks, Length(Bookmarks));

  for I := 0 to Length(DeleteBookmarks) - 1 do
    DeleteBookmarks[I] := Bookmarks[I];

  Delete();

  SetLength(DeleteBookmarks, 0);
end;

destructor TMySQLDataSet.Destroy();
begin
  Close();

  FSortDef.Free();
  FRecordsReceived.Free();
  if (Assigned(FilterParser)) then
    FreeAndNil(FilterParser);
  InternRecordBuffers.Free();
  Connection := nil; // UnRegister Connection

  inherited;
end;

function TMySQLDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Distance: Integer;
begin
  if (Restart) then
  begin
    Result := RecordCount > 0;
    if (Result) then
      if (GoForward) then
        First()
      else
        Last();
  end
  else
  begin
    if (GoForward) then
      Distance := +1
    else
      Distance := -1;
    Result := MoveBy(Distance) <> 0;
  end;

  SetFound(Result);
end;

procedure TMySQLDataSet.FreeInternRecordBuffer(const InternRecordBuffer: PInternRecordBuffer);
begin
  if (Assigned(InternRecordBuffer^.NewData) and (InternRecordBuffer^.NewData <> InternRecordBuffer^.OldData)) then
    FreeMem(InternRecordBuffer^.NewData);
  if (Assigned(InternRecordBuffer^.OldData)) then
    FreeMem(InternRecordBuffer^.OldData);

  Dispose(InternRecordBuffer);
end;

procedure TMySQLDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Dispose(Buffer); Buffer := nil;
end;

procedure TMySQLDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PPointer(Data)^ := PExternRecordBuffer(Buffer)^.InternRecordBuffer;
end;

function TMySQLDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PExternRecordBuffer(Buffer)^.BookmarkFlag;
end;

function TMySQLDataSet.GetCanModify(): Boolean;
begin
  if (CachedUpdates) then
    Result := True
  else
  begin
    if (not IndexDefs.Updated) then
      UpdateIndexDefs();

    Result := FCanModify;
  end;
end;

function TMySQLDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
try
  Result := Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)
    and GetFieldData(Field, Buffer, PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
except
  Result := Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)
    and GetFieldData(Field, Buffer, PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
end;
end;

function TMySQLDataSet.GetIsIndexField(Field: TField): Boolean;
begin
  Result := pfInKey in Field.ProviderFlags;
end;

function TMySQLDataSet.GetLibLengths(): MYSQL_LENGTHS;
begin
  Result := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData^.LibLengths;
end;

function TMySQLDataSet.GetLibRow(): MYSQL_ROW;
begin
  Assert(Assigned(ActiveBuffer()));
  Assert(Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData));

  Result := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData^.LibRow;
end;

function TMySQLDataSet.GetMaxTextWidth(const Field: TField; const TextWidth: TTextWidth): Integer;
var
  I: Integer;
  Index: Integer;
begin
  if (InternRecordBuffers.Count = 0) then
    Result := TextWidth(StringOfChar('e', Field.DisplayWidth))
  else
    Result := 10;

  InternRecordBuffers.CriticalSection.Enter();
  Index := Field.FieldNo - 1;
  if ((not (Field.DataType in [ftWideString, ftWideMemo]))) then
    for I := 0 to InternRecordBuffers.Count - 1 do
      Result := Max(Result, TextWidth(Connection.LibUnpack(InternRecordBuffers[I]^.NewData^.LibRow^[Index], InternRecordBuffers[I]^.NewData^.LibLengths^[Index])))
  else
    for I := 0 to InternRecordBuffers.Count - 1 do
      Result := Max(Result, TextWidth(Connection.LibDecode(InternRecordBuffers[I]^.NewData^.LibRow^[Index], InternRecordBuffers[I]^.NewData^.LibLengths^[Index])));
  InternRecordBuffers.CriticalSection.Leave();
end;

function TMySQLDataSet.GetRecNo(): Integer;
begin
  if (PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag <> bfCurrent) then
    Result := -1
  else
    Result := PExternRecordBuffer(ActiveBuffer())^.RecNo;
end;

function TMySQLDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  NewIndex: Integer;
begin
  NewIndex := InternRecordBuffers.Index;
  case (GetMode) of
    gmPrior:
      begin
        Result := grError;
        while (Result = grError) do
          if (NewIndex < 0) then
            Result := grBOF
          else
          begin
            Dec(NewIndex);
            if ((NewIndex >= 0) and (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter)) then
              Result := grOk;
          end;
      end;
    gmNext:
      begin
        Result := grError;
        while (Result = grError) do
        begin
          if ((NewIndex + 1 = InternRecordBuffers.Count) and not Filtered
            and ((RecordsReceived.WaitFor(IGNORE) <> wrSignaled) or (Self is TMySQLTable) and TMySQLTable(Self).LimitedDataReceived and TMySQLTable(Self).AutomaticLoadNextRecords and TMySQLTable(Self).LoadNextRecords())) then
            InternRecordBuffers.RecordReceived.WaitFor(NET_WAIT_TIMEOUT * 1000);

          if (NewIndex >= InternRecordBuffers.Count - 1) then
            Result := grEOF
          else
          begin
            Inc(NewIndex);
            if (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter) then
              Result := grOk;
          end;
        end;

        if (Result <> grEOF) then
          InternRecordBuffers.RecordReceived.ResetEvent();
      end;
    else // gmCurrent
      if (Filtered) then
      begin
        if (NewIndex < 0) then
          Result := grBOF
        else if (NewIndex < InternRecordBuffers.Count) then
          repeat
            if (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter) then
              Result := grOk
            else if (NewIndex + 1 = InternRecordBuffers.Count) then
              Result := grEOF
            else
            begin
              Result := grError;
              Inc(NewIndex);
            end;
          until (Result <> grError)
        else
        begin
          Result := grEOF;
          NewIndex := InternRecordBuffers.Count - 1;
        end;
        while ((Result = grEOF) and (NewIndex > 0)) do
          if (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter) then
            Result := grOk
          else
            Dec(NewIndex);
      end
      else if ((0 <= InternRecordBuffers.Index) and (InternRecordBuffers.Index < InternRecordBuffers.Count)) then
        Result := grOk
      else
        Result := grEOF;
  end;

  if (Result = grOk) then
  begin
    InternRecordBuffers.Index := NewIndex;

    PExternRecordBuffer(Buffer)^.InternRecordBuffer := InternRecordBuffers[InternRecordBuffers.Index];
    PExternRecordBuffer(Buffer)^.RecNo := InternRecordBuffers.Index;
    PExternRecordBuffer(Buffer)^.BookmarkFlag := bfCurrent;
  end;
end;

function TMySQLDataSet.GetRecordCount(): Integer;
begin
  if (Filtered) then
    Result := InternRecordBuffers.FilteredRecordCount
  else
    Result := InternRecordBuffers.Count;
end;

function TMySQLDataSet.GetUniDirectional(): Boolean;
begin
  Result := False;
end;

procedure TMySQLDataSet.InternActivateFilter();
var
  I: Integer;
begin
  if (Assigned(FilterParser)) then
    FilterParser.Free();
  FilterParser := TExprParser.Create(Self, Filter, FilterOptions, [poExtSyntax], '', nil, FldTypeMap);

  InternRecordBuffers.CriticalSection.Enter();

  InternRecordBuffers.FilteredRecordCount := 0;
  for I := 0 to InternRecordBuffers.Count - 1 do
  begin
    InternRecordBuffers[I]^.VisibleInFilter := VisibleInFilter(InternRecordBuffers[I]);
    if (InternRecordBuffers[I]^.VisibleInFilter) then
      Inc(InternRecordBuffers.FilteredRecordCount);
  end;

  InternRecordBuffers.CriticalSection.Leave();
end;

procedure TMySQLDataSet.InternAddRecord(const LibRow: MYSQL_ROW; const LibLengths: MYSQL_LENGTHS; const Index: Integer = -1);
var
  Data: TMySQLQuery.TRecordBufferData;
  I: Integer;
  InternRecordBuffer: PInternRecordBuffer;
begin
  if (Assigned(LibRow)) then
  begin
    Data.LibLengths := LibLengths;
    Data.LibRow := LibRow;

    InternRecordBuffer := AllocInternRecordBuffer();

    MoveRecordBufferData(InternRecordBuffer^.OldData, @Data);
    InternRecordBuffer^.NewData := InternRecordBuffer^.OldData;
    InternRecordBuffer^.VisibleInFilter := not Filtered or VisibleInFilter(InternRecordBuffer);

    for I := 0 to FieldCount - 1 do
      Inc(FDataSize, Data.LibLengths^[I]);

    if (Filtered and InternRecordBuffer^.VisibleInFilter) then
      Inc(InternRecordBuffers.FilteredRecordCount);

    InternRecordBuffers.CriticalSection.Enter();
    if (Index >= 0) then
      InternRecordBuffers.Insert(Index, InternRecordBuffer)
    else
      InternRecordBuffers.Add(InternRecordBuffer);
    InternRecordBuffers.CriticalSection.Leave();
  end
  else
  begin
    if ((Self is TMySQLTable) and Assigned(SynchroThread)) then
      TMySQLTable(Self).FLimitedDataReceived :=
        not SynchroThread.Success or not Assigned(SynchroThread.DataSet)
        or (Connection.Lib.mysql_num_rows(SynchroThread.ResultHandle) = TMySQLTable(Self).RequestedRecordCount);

    RecordsReceived.SetEvent();
  end;

  InternRecordBuffers.RecordReceived.SetEvent();
end;

procedure TMySQLDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if (not Append and (InternRecordBuffers.Count > 0)) then
    InternAddRecord(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow, PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibLengths, InternRecordBuffers.Index)
  else
    InternAddRecord(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow, PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibLengths);
end;

procedure TMySQLDataSet.InternalCancel();
begin
  if (PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag <> bfCurrent) then
  begin
    FreeInternRecordBuffer(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer);
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := nil;
  end
  else if (PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData <> PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData) then
  begin
    FreeMem(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData;
  end;
end;

procedure TMySQLDataSet.InternalClose();
begin
  FSortDef.Fields := '';

  FCursorOpen := False;

  inherited;

  InternRecordBuffers.Clear();
  InternRecordBuffers.FilteredRecordCount := 0;
  FDataSize := 0;
end;

procedure TMySQLDataSet.InternalDelete();
var
  I: Integer;
  Index: Integer;
  J: Integer;
  SQL: string;
  Success: Boolean;
begin
  if (not CachedUpdates) then
  begin
    SQL := SQLDelete();
    if (Connection.DatabaseName <> DatabaseName) then
      SQL := Connection.SQLUse(DatabaseName) + SQL;
    Success := Connection.ExecuteSQL(SQL);
    if (Success and (Connection.RowsAffected = 0)) then
      raise EDatabasePostError.Create(SRecordChanged);

    InternRecordBuffers.CriticalSection.Enter();
    if (Length(DeleteBookmarks) = 0) then
    begin
      InternalSetToRecord(ActiveBuffer());
      FreeInternRecordBuffer(InternRecordBuffers[InternRecordBuffers.Index]);
      InternRecordBuffers.Delete(InternRecordBuffers.Index);
        for J := ActiveRecord + 1 to BufferCount - 1 do
          Dec(PExternRecordBuffer(Buffers[J])^.RecNo);
      if (Filtered) then
        Dec(InternRecordBuffers.FilteredRecordCount);
    end
    else
    begin
      for I := 0 to Max(1, Length(DeleteBookmarks)) - 1 do
      begin
        Index := BookmarkToInternBufferIndex(DeleteBookmarks[I]);
        if (Index < InternRecordBuffers.Index) then
          Dec(InternRecordBuffers.Index);
        for J := 0 to BufferCount - 1 do
          if (Assigned(PExternRecordBuffer(Buffers[I])) and (PExternRecordBuffer(Buffers[I])^.InternRecordBuffer = InternRecordBuffers[Index])) then
            PExternRecordBuffer(Buffers[I])^.InternRecordBuffer := nil;
        FreeInternRecordBuffer(InternRecordBuffers[Index]);
        InternRecordBuffers.Delete(Index);
        for J := ActiveRecord + 1 to BufferCount - 1 do
          Dec(PExternRecordBuffer(Buffers[J])^.RecNo);
        if (Filtered) then
          Dec(InternRecordBuffers.FilteredRecordCount);
      end;
    end;
    InternRecordBuffers.CriticalSection.Leave();

    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := nil;
  end;
end;

procedure TMySQLDataSet.InternalFirst();
begin
  InternRecordBuffers.Index := -1;
end;

procedure TMySQLDataSet.InternalGotoBookmark(Bookmark: Pointer);
var
  I: Integer;
  NewInternBuffersIndex: Integer;
begin
  NewInternBuffersIndex := -1;

  I := 0;
  while ((NewInternBuffersIndex < 0) and (I < InternRecordBuffers.Count)) do
  begin
    if (PPointer(@TBookmark(Bookmark)[0])^ = InternRecordBuffers[I]) then
      NewInternBuffersIndex := I;
    Inc(I);
  end;

  if (NewInternBuffersIndex >= 0) then
    InternRecordBuffers.Index := NewInternBuffersIndex;
end;

procedure TMySQLDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  PExternRecordBuffer(Buffer)^.InternRecordBuffer := nil;
  PExternRecordBuffer(Buffer)^.RecNo := -1;
  PExternRecordBuffer(Buffer)^.BookmarkFlag := bfCurrent;
end;

procedure TMySQLDataSet.InternalInsert();
var
  I: Integer;
  RBS: RawByteString;
begin
  InternalSetToRecord(ActiveBuffer());

  PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := AllocInternRecordBuffer();

  if (Filtered) then
    Inc(InternRecordBuffers.FilteredRecordCount);

  for I := 0 to FieldCount - 1 do
    if (Fields[I].DefaultExpression <> '') then
    begin
      RBS := Connection.LibEncode(SQLUnescape(Fields[I].DefaultExpression));
      SetFieldData(Fields[I], @RBS[1], Length(RBS));
    end
    else if (Fields[I].Required and (Fields[I].DataType in BinaryDataTypes * TextDataTypes)) then
      SetFieldData(Fields[I], Pointer(1), 0);
end;

procedure TMySQLDataSet.InternalLast();
begin
  InternRecordBuffers.Index := InternRecordBuffers.Count;
end;

procedure TMySQLDataSet.InternalOpen();
begin
  Assert(not IsCursorOpen());

  FCursorOpen := True;

  if (not CachedUpdates) then
  begin
    RecordsReceived.ResetEvent();

    inherited;

    SetFieldsSortTag();
  end
  else
  begin
    InternalInitFieldDefs();
    BindFields(True);
    UpdateBufferCount();

    InternRecordBuffers.Index := -1;
  end;
end;

procedure TMySQLDataSet.InternalPost();
var
  I: Integer;
  SQL: string;
  Success: Boolean;
  Update: Boolean;
begin
  Update := PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag = bfCurrent;

  if (CachedUpdates) then
    Success := True
  else
  begin
    if (Update) then
      SQL := SQLUpdate()
    else
      SQL := SQLInsert();

    if (Connection.DatabaseName <> DatabaseName) then
      SQL := Connection.SQLUse(DatabaseName) + SQL;
    Connection.BeginSilent();
    try
      Success := Connection.ExecuteSQL(SQL);
      if (not Success) then
        raise EMySQLError.Create(Connection.ErrorMessage, Connection.ErrorCode, Connection)
      else if (Update and (Connection.RowsAffected = 0)) then
        raise EDatabasePostError.Create(SRecordChanged);
    finally
      Connection.EndSilent();
    end;
  end;

  if (Success) then
  begin
    case (PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag) of
      bfInserted:
        InternRecordBuffers.Insert(InternRecordBuffers.Index, PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer);
      bfBOF,
      bfEOF:
        InternRecordBuffers.Index := InternRecordBuffers.Add(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer);
    end;

    if (not CachedUpdates and Connection.Connected) then
      for I := 0 to Fields.Count - 1 do
        if ((Fields[I].AutoGenerateValue = arAutoInc) and (Fields[I].IsNull or (Fields[I].AsInteger = 0))) then
          Fields[I].AsInteger := Connection.Lib.mysql_insert_id(Connection.Handle);

    if (PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData <> PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData) then
      FreeMem(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData);
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData;
  end;
end;

procedure TMySQLDataSet.InternalRefresh();
begin
  InternRecordBuffers.Clear();
  Connection.ExecuteSQL(CommandText, InternalRefreshEvent);
end;

function TMySQLDataSet.InternalRefreshEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
begin
  Assert(not Assigned(SynchroThread));


  InternRecordBuffers.RecordReceived.ResetEvent();

  SynchroThread := DataHandle;
  SynchroThread.BindDataSet(Self);

  Result := False;
end;

procedure TMySQLDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
var
  Direction: Integer;
  MaxIndex: Integer;
  NewIndex: Integer;
  NewInternBuffersIndex: Integer;
begin
  NewInternBuffersIndex := InternRecordBuffers.Index;

  NewIndex := Max(InternRecordBuffers.Index - BufferCount, 0);
  MaxIndex := Min(InternRecordBuffers.Index + BufferCount, RecordCount);
  while ((NewInternBuffersIndex = InternRecordBuffers.Index) and (NewIndex < MaxIndex)) do
  begin
    if (PExternRecordBuffer(Buffer)^.InternRecordBuffer = InternRecordBuffers[NewIndex]) then
      NewInternBuffersIndex := NewIndex;
    Inc(NewIndex);
  end;

  Direction := Sign(NewInternBuffersIndex - InternRecordBuffers.Index);
  while (InternRecordBuffers.Index <> NewInternBuffersIndex) do
    Inc(InternRecordBuffers.Index, Direction);
end;

function TMySQLDataSet.IsCursorOpen(): Boolean;
begin
  Result := FCursorOpen;
end;

function TMySQLDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
var
  Bookmark: TBookmark;
  FieldNames: TCSVStrings;
  Fields: array of TField;
  I: Integer;
  Index: Integer;
  Values: array of string;
begin
  CheckBrowseMode();

  SetLength(FieldNames, 0);
  CSVSplitValues(KeyFields, ';', #0, FieldNames);
  SetLength(Fields, 0);
  for I := 0 to Length(FieldNames) - 1 do
    if (Assigned(FindField(FieldNames[I]))) then
    begin
      SetLength(Fields, Length(Fields) + 1);
      Fields[Length(Fields) - 1] := FieldByName(FieldNames[I]);
    end;

  if (not VarIsArray(KeyValues)) then
  begin
    SetLength(Values, 1);
    Values[0] := KeyValues;
  end
  else
  begin
    SetLength(Values, Length(Fields));
    try
      for I := 0 to Length(Fields) - 1 do
        Values[I] := KeyValues[I];
    except
      SetLength(Values, 0);
    end;
  end;
  if ((loCaseInsensitive in Options) and (loPartialKey in Options)) then
    for I := 0 to Length(Fields) - 1 do
      Values[I] := LowerCase(Values[I]);

  Result := (Length(Fields) = Length(FieldNames)) and (Length(Fields) = Length(Values));

  if (Result) then
  begin
    if (not LocateNext) then
      Index := 0
    else
      Index := RecNo + 1;

    Result := False;
    while (not Result and (Index < InternRecordBuffers.Count)) do
    begin
      Result := True;
      for I := 0 to Length(Fields) - 1 do
        if (not Assigned(InternRecordBuffers[Index]^.NewData^.LibRow^[I])) then
          Result := False
        else if (BitField(Fields[I])) then
          if (loPartialKey in Options) then
            Result := Result and (Pos(Values[I], Fields[I].AsString) > 0)
          else
            Result := Result and (Values[I] = Fields[I].AsString)
        else if (loCaseInsensitive in Options) then
          if (loPartialKey in Options) then
            Result := Result and (Pos(Values[I], LowerCase(Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[I], InternRecordBuffers[Index]^.NewData^.LibLengths^[I]))) > 0)
          else
            Result := Result and (lstrcmpi(PChar(Values[I]), PChar(Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[I], InternRecordBuffers[Index]^.NewData^.LibLengths^[I]))) = 0)
        else
          if (loPartialKey in Options) then
            Result := Result and (Pos(Values[I], Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[I], InternRecordBuffers[Index]^.NewData^.LibLengths^[I])) > 0)
          else
            Result := Result and (lstrcmp(PChar(Values[I]), PChar(Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[I], InternRecordBuffers[Index]^.NewData^.LibLengths^[I]))) = 0);

      if (not Result) then
        Inc(Index);
    end;

    if (Result) then
    begin
      CheckBrowseMode();
      SetLength(Bookmark, BookmarkSize);
      PPointer(@Bookmark[0])^ := InternRecordBuffers[Index];
      GotoBookmark(Bookmark);
      SetLength(Bookmark, 0);
    end;
  end;

  SetLength(Values, 0);
  SetLength(FieldNames, 0);
end;

procedure TMySQLDataSet.MoveRecordBufferData(var DestData: TMySQLQuery.PRecordBufferData; const SourceData: TMySQLQuery.PRecordBufferData);
var
  I: Integer;
  Index: Integer;
  MemSize: Integer;
begin
  Assert(Assigned(SourceData));
  Assert(Assigned(SourceData^.LibLengths));


  if (Assigned(DestData)) then
    FreeMem(DestData);

  MemSize := SizeOf(DestData^) + FieldCount * (SizeOf(DestData^.LibLengths^[0]) + SizeOf(DestData^.LibRow^[0]));
  for I := 0 to FieldCount - 1 do
    Inc(MemSize, SourceData^.LibLengths^[I]);
  GetMem(DestData, MemSize);

  DestData^.LibLengths := Pointer(@PAnsiChar(DestData)[SizeOf(DestData^)]);
  DestData^.LibRow := Pointer(@PAnsiChar(DestData)[SizeOf(DestData^) + FieldCount * SizeOf(DestData^.LibLengths^[0])]);

  MoveMemory(DestData^.LibLengths, SourceData^.LibLengths, FieldCount * SizeOf(DestData^.LibLengths^[0]));
  Index := SizeOf(DestData^) + FieldCount * (SizeOf(DestData^.LibLengths^[0]) + SizeOf(DestData^.LibRow^[0]));
  for I := 0 to FieldCount - 1 do
    if (not Assigned(SourceData^.LibRow^[I])) then
      DestData^.LibRow^[I] := nil
    else
    begin
      DestData^.LibRow^[I] := @PAnsiChar(DestData)[Index];
      MoveMemory(DestData^.LibRow^[I], SourceData^.LibRow^[I], DestData^.LibLengths^[I]);
      Inc(Index, DestData^.LibLengths^[I]);
    end;
end;

procedure TMySQLDataSet.SetActive(Value: Boolean);
begin
  if (CachedUpdates and not Active and Value) then
  begin
    InternalInitFieldDefs();
    BindFields(True);
  end;

  inherited;
end;

procedure TMySQLDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PExternRecordBuffer(Buffer)^.InternRecordBuffer := PPointer(Data)^;
end;

procedure TMySQLDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PExternRecordBuffer(Buffer)^.BookmarkFlag := Value;
end;

procedure TMySQLDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  DT: TDateTime;
  RBS: RawByteString;
begin
  if (not Assigned(Buffer)) then
    SetFieldData(Field, nil, 0)
  else if (BitField(Field)) then
    SetFieldData(Field, Buffer, Field.DataSize)
  else
  begin
    case (Field.DataType) of
      ftString: SetString(RBS, PAnsiChar(Buffer), Field.DataSize);
      ftShortInt: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, ShortInt(Buffer^), Connection.FormatSettings));
      ftByte: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Byte(Buffer^), Connection.FormatSettings));
      ftSmallInt: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, SmallInt(Buffer^), Connection.FormatSettings));
      ftWord: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Word(Buffer^), Connection.FormatSettings));
      ftInteger: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Integer(Buffer^), Connection.FormatSettings));
      ftLongWord: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, LongWord(Buffer^), Connection.FormatSettings));
      ftLargeint:
        if (not (Field is TLargeWordField) or (UInt64(Buffer^) and $80000000 = 0)) then
          RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Largeint(Buffer^), Connection.FormatSettings))
        else
          RBS := Connection.LibPack(UInt64ToStr(UInt64(Buffer^)));
      ftSingle: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Single(Buffer^), Connection.FormatSettings));
      ftFloat: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Double(Buffer^), Connection.FormatSettings));
      ftExtended: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Extended(Buffer^), Connection.FormatSettings));
      ftDate: begin DataConvert(Field, Buffer, @DT, False); RBS := Connection.LibPack(MySQLDB.DateToStr(DT, Connection.FormatSettings)); end;
      ftTime: RBS := Connection.LibPack(TimeToStr(Integer(Buffer^), TMySQLTimeField(Field).SQLFormat));
      ftTimeStamp: RBS := Connection.LibPack(MySQLTimeStampToStr(PSQLTimeStamp(Buffer)^, TMySQLTimeStampField(Field).DisplayFormat));
      ftDateTime: begin DataConvert(Field, Buffer, @DT, False); RBS := Connection.LibPack(MySQLDB.DateTimeToStr(DT, Connection.FormatSettings)); end;
      ftBytes: SetString(RBS, PAnsiChar(Buffer), Field.DataSize);
      ftBlob: begin SetLength(RBS, TMemoryStream(Buffer).Size); Move(TMemoryStream(Buffer).Memory^, PAnsiChar(RBS)^, TMemoryStream(Buffer).Size); end;
      ftWideMemo: DataConvert(Field, Buffer, @RBS, True);
      ftWideString: RBS := PAnsiChar(Buffer);
      else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
    end;
    if (RBS = '') then
      SetFieldData(Field, Pointer(-1), 0)
    else
      SetFieldData(Field, PAnsiChar(RBS), Length(RBS));
  end;

  DataEvent(deFieldChange, Longint(Field));
end;

procedure TMySQLDataSet.SetFieldData(const Field: TField; const Buffer: Pointer; const Size: Integer);
var
  I: Integer;
  Index: Integer;
  MemSize: Integer;
  NewData: TMySQLQuery.PRecordBufferData;
  OldData: TMySQLQuery.PRecordBufferData;
begin
  OldData := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData;

  MemSize := SizeOf(NewData^) + FieldCount * (SizeOf(NewData^.LibLengths^[0]) + SizeOf(NewData^.LibLengths^[0]));
  for I := 0 to FieldCount - 1 do
    if (I = Field.FieldNo - 1) then
      Inc(MemSize, Size)
    else if (Assigned(OldData)) then
      Inc(MemSize, OldData^.LibLengths^[I]);
  GetMem(NewData, MemSize);

  NewData^.LibLengths := Pointer(@PAnsiChar(NewData)[SizeOf(NewData^)]);
  NewData^.LibRow := Pointer(@PAnsiChar(NewData)[SizeOf(NewData^) + FieldCount * SizeOf(NewData^.LibLengths^[0])]);

  Index := SizeOf(NewData^) + FieldCount * (SizeOf(NewData^.LibLengths^[0]) + SizeOf(NewData^.LibRow^[0]));
  for I := 0 to FieldCount - 1 do
    if (I = Field.FieldNo - 1) then
      if (not Assigned(Buffer)) then
      begin
        NewData^.LibLengths^[I] := 0;
        NewData^.LibRow^[I] := nil;
      end
      else
      begin
        NewData^.LibLengths^[I] := Size;
        NewData^.LibRow^[I] := Pointer(@PAnsiChar(NewData)[Index]);
        MoveMemory(NewData^.LibRow^[I], Buffer, Size);
        Inc(Index, NewData^.LibLengths^[I]);
      end
    else
      if (not Assigned(OldData) or not Assigned(OldData^.LibRow^[I])) then
      begin
        NewData^.LibLengths^[I] := 0;
        NewData^.LibRow^[I] := nil;
      end
      else
      begin
        NewData^.LibLengths^[I] := OldData^.LibLengths^[I];
        NewData^.LibRow^[I] := Pointer(@PAnsiChar(NewData)[Index]);
        MoveMemory(NewData^.LibRow^[I], OldData^.LibRow^[I], NewData^.LibLengths^[I]);
        Inc(Index, NewData^.LibLengths^[I]);
      end;

  if (not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)) then
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := AllocInternRecordBuffer()
  else if (PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData <> PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData) then
    FreeMem(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
  PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData := NewData;
end;

procedure TMySQLDataSet.SetFieldsSortTag();
var
  Field: TField;
  FieldName: string;
  Pos: Integer;
begin
  for Field in Fields do
    Field.Tag := Field.Tag and not ftSortedField;
  Pos := 1;
  repeat
    FieldName := ExtractFieldName(SortDef.Fields, Pos);
    if (FieldName <> '') then
    begin
      Field := FindField(FieldName);
      if (Assigned(Field)) then
        Field.Tag := Field.Tag or ftAscSortedField;
    end;
  until (FieldName = '');
  Pos := 1;
  repeat
    FieldName := ExtractFieldName(SortDef.DescFields, Pos);
    if (FieldName <> '') then
    begin
      Field := FindField(FieldName);
      if (Assigned(Field)) then
        Field.Tag := (Field.Tag and not ftAscSortedField) or ftDescSortedField;
    end;
  until (FieldName = '');
end;

procedure TMySQLDataSet.SetFiltered(Value: Boolean);
begin
  if (Value <> Filtered) then
  begin
    inherited;

    if (Active) then
      if (Value) then
        ActivateFilter()
      else
        DeactivateFilter();
  end;
end;

procedure TMySQLDataSet.SetFilterText(const Value: string);
begin
  Filtered := Filtered and (Value <> '');

  if (Filtered) then
    DeactivateFilter();

  inherited;

  if (Filtered and (Filter <> '') and IsCursorOpen()) then
    ActivateFilter();
end;

procedure TMySQLDataSet.SetRecNo(Value: Integer);
var
  Bookmark: TBookmark;
  Index: Integer;
  VisibleRecords: Integer;
begin
  VisibleRecords := 0;

  Index := 0;
  while ((VisibleRecords < Value + 1) and (Index < InternRecordBuffers.Count)) do
  begin
    if (not Filtered or InternRecordBuffers[Index]^.VisibleInFilter) then
      Inc(VisibleRecords);
    Inc(Index);
  end;

  if ((VisibleRecords = Value + 1) and (Index < InternRecordBuffers.Count)) then
  begin
    SetLength(Bookmark, BookmarkSize);
    PPointer(@Bookmark[0])^ := InternRecordBuffers[Index - 1];
    GotoBookmark(Bookmark);
    SetLength(Bookmark, 0);
  end;
end;

procedure TMySQLDataSet.Sort(const ASortDef: TIndexDef);
var
  Ascending: array of Boolean;
  SortFields: array of TField;

  function Compare(const A, B: PInternRecordBuffer; const FieldIndex: Integer = 0): Integer;
  var
    Field: TField;
    ShortIntA, ShortIntB: ShortInt;
    ByteA, ByteB: Byte;
    SmallIntA, SmallIntB: SmallInt;
    WordA, WordB: Word;
    IntegerA, IntegerB: Integer;
    LongWordA, LongWordB: LongWord;
    LargeIntA, LargeIntB: LargeInt;
    UInt64A, UInt64B: UInt64;
    SingleA, SingleB: Single;
    DoubleA, DoubleB: Double;
    ExtendedA, ExtendedB: Extended;
    DateTimeA, DateTimeB: TDateTimeRec;
    StringA, StringB: string;
  begin
    Field := SortFields[FieldIndex];

    if (A = B) then
      Result := 0
    else if (not Assigned(A^.NewData^.LibRow[Field.FieldNo - 1]) and Assigned(B^.NewData^.LibRow[Field.FieldNo - 1])) then
      Result := -1
    else if (Assigned(A^.NewData^.LibRow[Field.FieldNo - 1]) and not Assigned(B^.NewData^.LibRow[Field.FieldNo - 1])) then
      Result := +1
    else if ((A^.NewData^.LibLengths[Field.FieldNo - 1] = 0) and (B^.NewData^.LibLengths[Field.FieldNo - 1] > 0)) then
      Result := -1
    else if ((A^.NewData^.LibLengths[Field.FieldNo - 1] > 0) and (B^.NewData^.LibLengths[Field.FieldNo - 1] = 0)) then
      Result := +1
    else if ((A^.NewData^.LibLengths[Field.FieldNo - 1] = 0) and (B^.NewData^.LibLengths[Field.FieldNo - 1] = 0)) then
      Result := 0
    else if (BitField(Fields[Field.FieldNo - 1])) then
    begin
      Result := Sign(UInt64(B^.NewData^.LibRow^[Field.FieldNo - 1]^) - UInt64(A^.NewData^.LibRow^[Field.FieldNo - 1]^));
    end
    else
    begin
      case (Field.DataType) of
        ftString: Result := lstrcmpA(A^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibRow^[Field.FieldNo - 1]);
        ftShortInt: begin GetFieldData(Field, @ShortIntA, A^.NewData); GetFieldData(Field, @ShortIntB, B^.NewData); Result := Sign(ShortIntA - ShortIntB); end;
        ftByte:
          begin
            GetFieldData(Field, @ByteA, A^.NewData);
            GetFieldData(Field, @ByteB, B^.NewData);
            if (ByteA < ByteB) then Result := -1 else if (ByteA > ByteB) then Result := +1 else Result := 0;
          end;
        ftSmallInt: begin GetFieldData(Field, @SmallIntA, A^.NewData); GetFieldData(Field, @SmallIntB, B^.NewData); Result := Sign(SmallIntA - SmallIntB); end;
        ftWord:
          begin
            GetFieldData(Field, @WordA, A^.NewData);
            GetFieldData(Field, @WordB, B^.NewData);
            if (WordA < WordB) then Result := -1 else if (WordA > WordB) then Result := +1 else Result := 0;
          end;
        ftInteger: begin GetFieldData(Field, @IntegerA, A^.NewData); GetFieldData(Field, @IntegerB, B^.NewData); Result := Sign(IntegerA - IntegerB); end;
        ftLongWord:
          begin
            GetFieldData(Field, @LongWordA, A^.NewData);
            GetFieldData(Field, @LongWordB, B^.NewData);
            if (LongWordA < LongWordB) then Result := -1 else if (LongWordA > LongWordB) then Result := +1 else Result := 0;
          end;
        ftLargeInt:
          if (not (Field is TLargeWordField)) then
            begin GetFieldData(Field, @LargeIntA, A^.NewData); GetFieldData(Field, @LargeIntB, B^.NewData); Result := Sign(LargeIntA - LargeIntB); end
          else
          begin
            GetFieldData(Field, @UInt64A, A^.NewData);
            GetFieldData(Field, @UInt64B, B^.NewData);
            if (UInt64A < UInt64B) then Result := -1 else if (UInt64A > UInt64B) then Result := +1 else Result := 0;
          end;
        ftSingle: begin GetFieldData(Field, @SingleA, A^.NewData); GetFieldData(Field, @SingleB, B^.NewData); Result := Sign(SingleA - SingleB); end;
        ftFloat: begin GetFieldData(Field, @DoubleA, A^.NewData); GetFieldData(Field, @DoubleB, B^.NewData); Result := Sign(DoubleA - DoubleB); end;
        ftExtended: begin GetFieldData(Field, @ExtendedA, A^.NewData); GetFieldData(Field, @ExtendedB, B^.NewData); Result := Sign(ExtendedA - ExtendedB); end;
        ftDate: begin GetFieldData(Field, @DateTimeA, A^.NewData); GetFieldData(Field, @DateTimeB, B^.NewData); Result := Sign(DateTimeA.Date - DateTimeB.Date); end;
        ftDateTime: begin GetFieldData(Field, @DateTimeA, A^.NewData); GetFieldData(Field, @DateTimeB, B^.NewData); Result := Sign(DateTimeA.DateTime - DateTimeB.DateTime); end;
        ftTime: begin GetFieldData(Field, @IntegerA, A^.NewData); GetFieldData(Field, @IntegerB, B^.NewData); Result := Sign(IntegerA - IntegerB); end;
        ftTimeStamp: Result := lstrcmpA(A^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibRow^[Field.FieldNo - 1]);
        ftWideString,
        ftWideMemo:
          begin
            StringA := Connection.LibDecode(A^.NewData^.LibRow^[Field.FieldNo - 1], A^.NewData^.LibLengths^[Field.FieldNo - 1]);
            StringB := Connection.LibDecode(B^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibLengths^[Field.FieldNo - 1]);
            Result := lstrcmpi(PChar(StringA), PChar(StringB));
          end;
        ftBlob: Result := lstrcmpA(A^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibRow^[Field.FieldNo - 1]);
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
      end;
    end;

    if (Result = 0) then
    begin
      if (FieldIndex + 1 < Length(SortFields)) then
        Result := Compare(A, B, FieldIndex + 1);
    end
    else if (not Ascending[FieldIndex]) then
      Result := -Result;
  end;

  procedure QuickSort(const Lo, Hi: Integer);
  var
    L, R: Integer;
    M: PInternRecordBuffer;
  begin
    L := Lo;
    R := Hi;
    M := InternRecordBuffers[(L + R + 1) div 2];

    while (L <= R) do
    begin
      while (Compare(InternRecordBuffers[L], M) < 0) do Inc(L);
      while (Compare(InternRecordBuffers[R], M) > 0) do Dec(R);
      if (L <= R) then
      begin
        InternRecordBuffers.Exchange(L, R);
        Inc(L); Dec(R);
      end;
    end;

    if (Lo < R) then QuickSort(Lo, R);
    if (L < Hi) then QuickSort(L, Hi);
  end;

var
  Field: TField;
  FieldName: string;
  I: Integer;
  OldBookmark: TBookmark;
  Pos: Integer;
begin
  Connection.Terminate();

  if ((ASortDef.Fields <> '') and (InternRecordBuffers.Count > 0)) then
  begin
    SortDef.Assign(ASortDef);

    CheckBrowseMode();
    DoBeforeScroll();

    OldBookmark := Bookmark;

    SetLength(SortFields, 0);
    SetLength(Ascending, 0);
    Pos := 1;
    repeat
      FieldName := ExtractFieldName(SortDef.Fields, Pos);
      if (FieldName <> '') then
      begin
        SetLength(SortFields, Length(SortFields) + 1);
        SortFields[Length(SortFields) - 1] := FieldByName(FieldName);
        SetLength(Ascending, Length(Ascending) + 1);
        Ascending[Length(SortFields) - 1] := True;
      end;
    until (FieldName = '');
    Pos := 1;
    repeat
      FieldName := ExtractFieldName(SortDef.DescFields, Pos);
      if (FieldName <> '') then
        for I := 0 to Length(SortFields) - 1 do
          if (SortFields[I].FieldName = FieldName) then
            Ascending[I] := False;
    until (FieldName = '');

    QuickSort(0, InternRecordBuffers.Count - 1);

    for Field in Fields do
      Field.Tag := Field.Tag and not ftSortedField;
    for Field in SortFields do
      Field.Tag := Field.Tag or ftSortedField;

    Bookmark := OldBookmark;

    DoAfterScroll();
  end;
end;

function TMySQLDataSet.SQLDelete(): string;
var
  I: Integer;
  InternRecordBuffer: PInternRecordBuffer;
  J: Integer;
  ValueHandled: Boolean;
  WhereField: TField;
  WhereFieldCount: Integer;
begin
  Result := 'DELETE FROM ' + SQLTableClausel() + ' WHERE ';

  if (Length(DeleteBookmarks) = 0) then
  begin
    InternRecordBuffer := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer;

    ValueHandled := False;
    for I := 0 to Fields.Count - 1 do
      if (pfInWhere in Fields[I].ProviderFlags) then
      begin
        if (ValueHandled) then Result := Result + ' AND ';
        if (not Assigned(InternRecordBuffer^.OldData) or not Assigned(InternRecordBuffer^.OldData^.LibRow^[I])) then
          Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + ' IS NULL'
        else
          Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], InternRecordBuffer^.OldData);
        ValueHandled := False;
      end;
  end
  else
  begin
    WhereFieldCount := 0; WhereField := nil;
    for I := 0 to FieldCount - 1 do
      if (pfInWhere in Fields[I].ProviderFlags) then
      begin
        WhereField := Fields[I];
        Inc(WhereFieldCount);
      end;

    if (WhereFieldCount = 1) then
    begin
      Result := Result + Connection.EscapeIdentifier(WhereField.FieldName) + ' IN (';
      for I := 0 to Length(DeleteBookmarks) - 1 do
      begin
        InternRecordBuffer := InternRecordBuffers[BookmarkToInternBufferIndex(TBookmark(DeleteBookmarks[I]))];
        if (I > 0) then Result := Result + ',';
        Result := Result + SQLFieldValue(WhereField, InternRecordBuffer^.OldData);
      end;
      Result := Result + ')';
    end
    else
      for I := 0 to Length(DeleteBookmarks) - 1 do
        for J := 0 to FieldCount - 1 do
          if (pfInWhere in Fields[I].ProviderFlags) then
          begin
            InternRecordBuffer := InternRecordBuffers[BookmarkToInternBufferIndex(TBookmark(DeleteBookmarks[I]))];
            if (I > 0) then Result := Result + ' OR ';
            if (not Assigned(InternRecordBuffer^.OldData^.LibRow^[I])) then
              Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + ' IS NULL'
            else
              Result := Result + '(' + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(WhereField, InternRecordBuffer^.OldData) + ')';
          end;
  end;
  Result := Result + ';' + #13#10;
end;

function TMySQLDataSet.SQLFieldValue(const Field: TField; Buffer: TRecordBuffer = nil): string;
begin
  if (not Assigned(Buffer)) then
    Buffer := ActiveBuffer();

  Result := SQLFieldValue(Field, PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData);
end;

function TMySQLDataSet.SQLInsert(): string;
var
  ExternRecordBuffer: PExternRecordBuffer;
  I: Integer;
  ValueHandled: Boolean;
begin
  ExternRecordBuffer := PExternRecordBuffer(ActiveBuffer());

  if (not Assigned(ExternRecordBuffer^.InternRecordBuffer^.NewData)) then
    Result := ''
  else
  begin
    Result := 'INSERT INTO ' + SQLTableClausel() + ' SET ';
    ValueHandled := False;
    for I := 0 to FieldCount - 1 do
      if (Assigned(ExternRecordBuffer^.InternRecordBuffer^.NewData^.LibRow^[I]) or Fields[I].Required and (Fields[I].AutoGenerateValue <> arAutoInc)) then
      begin
        if (ValueHandled) then Result := Result + ',';
        Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], TRecordBuffer(ExternRecordBuffer));
        ValueHandled := True;
      end;
    Result := Result + ';' + #13#10;
  end;
end;

function TMySQLDataSet.SQLTableClausel(): string;
begin
  if (DatabaseName = '') then
    Result := ''
  else
    Result := Connection.EscapeIdentifier(DatabaseName) + '.';
  if (TableName = '') then
    Result := ''
  else
    Result := Result + Connection.EscapeIdentifier(TableName);
end;

function TMySQLDataSet.SQLUpdate(Buffer: TRecordBuffer = nil): string;
var
  I: Integer;
  ValueHandled: Boolean;
begin
  if (not Assigned(Buffer)) then
    Buffer := ActiveBuffer();

  Result := 'UPDATE ' + SQLTableClausel() + ' SET ';
  ValueHandled := False;
  for I := 0 to FieldCount - 1 do
    if ((PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibLengths^[I] <> PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibLengths^[I])
      or (PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow^[I] <> PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibRow^[I])
      or (not CompareMem(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow^[I], PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibRow^[I], PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibLengths^[I]))) then
    begin
      if (ValueHandled) then Result := Result + ',';
      Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], Buffer);
      ValueHandled := True;
    end;
  Result := Result + ' WHERE ';
  ValueHandled := False;
  for I := 0 to FieldCount - 1 do
    if (pfInWhere in Fields[I].ProviderFlags) then
    begin
      if (ValueHandled) then Result := Result + ' AND ';
      if (not Assigned(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibRow^[I])) then
        Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + ' IS NULL'
      else
        Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData);
      ValueHandled := True;
    end;
  Result := Result + ';' + #13#10;
end;

procedure TMySQLDataSet.UpdateIndexDefs();
var
  DName: string;
  FieldName: string;
  Found: Boolean;
  I: Integer;
  Index: TIndexDef;
  Parse: TSQLParse;
  Pos: Integer;
  TName: string;
begin
  if (not Assigned(Handle)) then
  begin
    inherited;

    if (not (Self is TMySQLTable)) then
    begin
      Index := nil;
      for I := 0 to FIndexDefs.Count - 1 do
        if (not Assigned(Index) and (ixUnique in FIndexDefs[I].Options)) then
          Index := FIndexDefs[I];
      FCanModify := Assigned(Index);

      if (not FCanModify and SQLCreateParse(Parse, PChar(CommandText), Length(CommandText), Connection.ServerVersion) and SQLParseKeyword(Parse, 'SELECT') and SQLParseChar(Parse, '*') and SQLParseKeyword(Parse, 'FROM')) then
      begin
        TName := SQLParseValue(Parse);
        if (not SQLParseChar(Parse, '.')) then
          DName := DatabaseName
        else
        begin
          DName := TName;
          TName := SQLParseValue(Parse);
        end;
        if (((TName = TableName) or SQLParseKeyword(Parse, 'AS') and (SQLParseValue(Parse) = TableName))
          and ((SQLParseKeyword(Parse, 'FROM') or SQLParseKeyword(Parse, 'WHERE') or SQLParseKeyword(Parse, 'GROUP BY') or SQLParseKeyword(Parse, 'HAVING') or SQLParseKeyword(Parse, 'ORDER BY') or SQLParseKeyword(Parse, 'LIMIT') or SQLParseEnd(Parse)))) then
        begin
          FDatabaseName := DName;
          FTableName := TName;
          FCanModify := True;

          Found := False;
          for I := 0 to FieldCount - 1 do
            Found := Found or (pfInKey in Fields[I].ProviderFlags);

          if (Found) then
          begin
            Index := FIndexDefs.AddIndexDef();
            Index.Name := '';
            Index.Options := [ixPrimary, ixUnique, ixCaseInsensitive];
            for I := 0 to FieldCount - 1 do
              if (pfInKey in Fields[I].ProviderFlags) then
              begin
                if (Index.Fields <> '') then Index.Fields := Index.Fields + ';';
                Index.Fields := Index.Fields + Fields[I].FieldName;
              end;
          end;
        end;
      end;
    end;

    Index := nil;
    for I := 0 to FIndexDefs.Count - 1 do
      if (not Assigned(Index) and (ixUnique in FIndexDefs[I].Options)) then
        Index := FIndexDefs[I];

    if (Assigned(Index)) then
    begin
      FCanModify := True;

      for I := 0 to FieldCount - 1 do
        Fields[I].ProviderFlags := Fields[I].ProviderFlags - [pfInWhere];
      Pos := 1;
      repeat
        FieldName := ExtractFieldName(Index.Fields, Pos);
        for I := 0 to FieldCount - 1 do
          if (Fields[I].FieldName = FieldName) then
            Fields[I].ProviderFlags := Fields[I].ProviderFlags + [pfInWhere];
      until (FieldName = '');
    end
    else
    begin
      FCanModify := Self is TMySQLTable;

      for I := 0 to FieldCount - 1 do
        Fields[I].ProviderFlags := Fields[I].ProviderFlags + [pfInWhere];
    end;
  end;
end;

function TMySQLDataSet.VisibleInFilter(const InternRecordBuffer: PInternRecordBuffer): Boolean;

  type
    PCANExpr = ^TCANExpr;
    TCANExpr = packed record
      iVer: Word;
      iTotalSize: Word;
      iNodes: Word;
      iNodeStart: Word;
      iLiteralStart: Word;
    end;

    PCANHdr = ^TCANHdr;
    TCANHdr = packed record
      nodeClass: DBCommon.NODEClass;
      Reserved1: Byte;
      Reserved2: Word;
      coOp: DBCommon.TCANOperator;
      Reserved3: Byte;
      Reserved4: Word;
      case DBCommon.NODEClass of
        nodeUNARY: ( Unary: record
          NodeOfs: Word;
        end; );
        nodeBINARY: ( Binary: record
          LeftPosOfs: Word;
          RightPosOfs: Word;
        end; );
        nodeCOMPARE: ( Compare: record
          CaseInsensitive: WordBool;
          PartLength: Word;
          NodeOfs: Word;
          DataOfs: Word;
        end; );
        nodeFIELD: ( Field2: record
          FieldNo: Word;
          FieldNameOfs: Word;
        end; );
        nodeCONST: ( Const2: record
          FieldType: Word;
          Size: Word;
          DataOfs: Word;
        end; );
        nodeLIST: ( List: record
        end; );
        nodeFUNC: ( Func: record
          FunctionNameOfs: Word;
          ArgOfs: Word;
        end; );
        nodeLISTELEM: ( ListItem: record
        end; );
    end;

  var
    Expr: PCANExpr;

  function VIsNull(AVariant: Variant): Boolean;
  begin
    Result:= VarIsNull(AVariant) or VarIsEmpty(AVariant);
  end;

  function ParseNode(const Node: PCANHdr): Variant;
  type
    PLargeint = ^Largeint;
  var
    I, Z: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec:word;
    P: Pointer;
    Arg1, Arg2: Variant;
    S: string;
    TS: TTimeStamp;
    TempNode: PCANHdr;
  begin
    case (Node^.nodeClass) of
      nodeFIELD:
        case (Node^.coOp) of
          coFIELD2:
            if (not Assigned(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1])) then
              Result := Null
            else if (BitField(Fields[Node^.Field2.FieldNo - 1])) then
              Result := UInt64(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1]^)
            else if (Fields[Node^.Field2.FieldNo - 1].DataType in [ftWideString, ftWideMemo]) then
              Result := Connection.LibDecode(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1], InternRecordBuffer^.NewData^.LibLengths^[Node^.Field2.FieldNo - 1])
            else
              Result := Connection.LibUnpack(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1], InternRecordBuffer^.NewData^.LibLengths^[Node^.Field2.FieldNo - 1]);
          else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
        end;

      nodeCONST:
        case (Node^.coOp) of
          coCONST2:
            begin
              P := @FilterParser.FilterData[Expr^.iLiteralStart + Node^.Const2.DataOfs];

              if (Node^.Const2.FieldType = $1007) then
              begin
                SetString(S, PChar(@PAnsiChar(P)[2]), PWord(@PAnsiChar(P)[0])^ div SizeOf(Char));
                Result := S;
              end
              else
                case (TFieldType(Node^.Const2.FieldType)) of
                  ftShortInt: Result := PShortInt(P)^;
                  ftByte: Result := PByte(P)^;
                  ftSmallInt: Result := PSmallInt(P)^;
                  ftWord: Result := PWord(P)^;
                  ftInteger: Result := PInteger(P)^;
                  ftLongword: Result := PLongword(P)^;
                  ftLargeint: Result := PLargeint(P)^;
                  ftSingle: Result := PSingle(P)^;
                  ftFloat: Result := PDouble(P)^;
                  ftExtended: Result := PExtended(P)^;
                  ftWideString: Result := PString(P)^;
                  ftDate: begin TS.Date := PInteger(P)^; TS.Time := 0; Result := TimeStampToDateTime(TS); end;
                  ftDateTime: Result := TimeStampToDateTime(MSecsToTimeStamp(PDouble(P)^));
                  ftTime: begin TS.Date := 0; TS.Time := PInteger(P)^; Result := TimeStampToDateTime(TS); end;
                  ftTimeStamp: Result := VarSQLTimeStampCreate(PSQLTimeStamp(P)^);
                  ftString,
                  ftFixedChar: Result := string(PChar(P));
                  ftBoolean: Result := PWordBool(P)^;
                  else raise EDatabaseError.CreateFmt('FieldType not supported (%d)', [Ord(Node^.Const2.FieldType)]);
                end;
            end;
          else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
        end;

      nodeUNARY:
        begin
          Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Unary.NodeOfs]);

          case (Node^.coOp) of
            coISBLANK:
              Result := VIsNull(Arg1);
            coNOTBLANK:
              Result := not VIsNull(Arg1);
            coNOT:
              if (VIsNull(Arg1)) then Result := Null else Result := not Arg1;
            coMINUS:
              if (VIsNull(Arg1)) then Result := Null else Result := -Arg1;
            coUPPER:
              if (VIsNull(Arg1)) then Result := Null else Result := UpperCase(Arg1);
            coLOWER:
              if (VIsNull(Arg1)) then Result := Null else Result := LowerCase(Arg1);
            else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
          end;
        end;

      nodeBINARY:
        begin
          Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Binary.LeftPosOfs]);
          Arg2 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Binary.RightPosOfs]);

          case (Node^.coOp) of
            coEQ:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 = Arg2);
            coNE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 <> Arg2);
            coGT:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 > Arg2);
            coGE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 >= Arg2);
            coLT:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 < Arg2);
            coLE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 <= Arg2);
            coOR:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 or Arg2);
            coAND:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 and Arg2);
            coADD:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 + Arg2);
            coSUB:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 - Arg2);
            coMUL:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 * Arg2);
            coDIV:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 / Arg2);
            coMOD,
            coREM:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 mod Arg2);
            coIN:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then
                Result := False
              else if (VarIsArray(Arg2)) then
              begin
                Result := False;
                for i := 0 to VarArrayHighBound(Arg2, 1) do
                begin
                  if (VarIsEmpty(Arg2[i])) then break;
                  Result := (Arg1 = Arg2[i]);
                  if (Result) then break;
                end;
              end
              else
                Result := (Arg1 = Arg2);
            coLike:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then
                Result := False
              else if (Arg2 = '%') then
                Result := Arg1 <> ''
              else if ((LeftStr(Arg2, 1) = '%') and (RightStr(Arg2, 1) = '%')) then
                Result := ContainsText(Arg1, Copy(Arg2, 2, Length(Arg2) - 2))
              else if (LeftStr(Arg2, 1) = '%') then
                Result := EndsText(Copy(Arg2, 2, Length(Arg2) - 1), Arg1)
              else if (RightStr(Arg2, 1) = '%') then
                Result := StartsText(Copy(Arg2, 1, Length(Arg2) - 1), Arg1)
              else
                Result := UpperCase(Arg1) = UpperCase(Arg2);
            else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
          end;
        end;

      nodeCOMPARE:
        begin
          Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Compare.NodeOfs]);
          Arg2 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Compare.DataOfs]);

          case (Node^.coOp) of
            coEQ,
            coNE:
              begin
                if (VIsNull(Arg1) or VIsNull(Arg2)) then
                  Result := False
                else if (Node^.Compare.PartLength = 0) then
                  if (Node^.Compare.CaseInsensitive) then
                    Result := lstrcmpi(PChar(VarToStr(Arg1)), PChar(VarToStr(Arg2))) = 0
                  else
                    Result := lstrcmp(PChar(VarToStr(Arg1)), PChar(VarToStr(Arg2))) = 0
                else
                  if (Node^.Compare.CaseInsensitive) then
                    Result := lstrcmpi(PChar(LeftStr(Arg1, Node^.Compare.PartLength)), PChar(LeftStr(Arg2, Node^.Compare.PartLength))) = 0
                  else
                    Result := lstrcmp(PChar(LeftStr(Arg1, Node^.Compare.PartLength)), PChar(LeftStr(Arg2, Node^.Compare.PartLength))) = 0;
                if (Node^.coOp = coNE) then
                  Result := not Result;
              end;
            coLIKE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then
                Result := False
              else if (Arg2 = '%') then
                Result := Arg1 <> ''
              else if ((LeftStr(Arg2, 1) = '%') and (RightStr(Arg2, 1) = '%')) then
                Result := ContainsText(Arg1, Copy(Arg2, 2, Length(Arg2) - 2))
              else if (LeftStr(Arg2, 1) = '%') then
                Result := EndsText(Copy(Arg2, 2, Length(Arg2) - 1), Arg1)
              else if (RightStr(Arg2, 1) = '%') then
                Result := StartsText(Copy(Arg2, 1, Length(Arg2) - 1), Arg1)
              else
                Result := UpperCase(Arg1) = UpperCase(Arg2);
            else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
          end;
        end;

      nodeFUNC:
        case (Node^.coOp) of
          coFUNC2:
            begin
              P := PAnsiChar(@FilterParser.FilterData[Expr^.iLiteralStart + Node^.Func.FunctionNameOfs]);
              Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Func.ArgOfs]);

              if (lstrcmpiA(P, 'UPPER') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := UpperCase(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'LOWER') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := LowerCase(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'SUBSTRING') = 0) then
                if (VIsNull(Arg1)) then
                  Result := Null
                else
                begin
                  Result := Arg1;
                  try
                    Arg1 := VarToStr(Result[0]);
                  except
                    on EVariantError do // no Params for "SubString"
                      raise EDatabaseError.CreateFmt('InvMissParam',[Arg1]);
                  end;

                  if (Result[2] <> 0) then
                    Result := Copy(Arg1, Integer(Result[1]), Integer(Result[2]))
                  else if (Pos(',', Result[1]) > 0) then  // "From" and "To" entered without space!
                    Result := Copy(Arg1, Integer(Result[1]), StrToInt(Copy(Result[1], Pos(',', Result[1]) + 1, Length(Result[1]))))
                  else // No "To" entered so use all
                    Result := VarToStr(Arg1);
                end

              else if (lstrcmpiA(P, 'TRIM') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := Trim(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'TRIMLEFT') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := TrimLeft(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'TRIMRIGHT') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := TrimRight(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'GETDATE') = 0) then
                Result := Now()

              else if (lstrcmpiA(P, 'YEAR') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeDate(VarToDateTime(Arg1), Year, Month, Day); Result := Year; end

              else if (lstrcmpiA(P, 'MONTH') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeDate(VarToDateTime(Arg1), Year, Month, Day); Result := Month; end

              else if (lstrcmpiA(P, 'DAY') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeDate(VarToDateTime(Arg1), Year, Month, Day); Result := Day; end

              else if (lstrcmpiA(P, 'HOUR') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeTime(VarToDateTime(Arg1), Hour, Min, Sec, MSec); Result := Hour; end

              else if (lstrcmpiA(P, 'MINUTE') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeTime(VarToDateTime(Arg1), Hour, Min, Sec, MSec); Result := Min; end

              else if (lstrcmpiA(P, 'SECOND') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeTime(VarToDateTime(Arg1), Hour, Min, Sec, MSec); Result := Sec; end

              else if (lstrcmpiA(P, 'DATE') = 0) then  // Format: DATE('datestring','formatstring')
              begin                                    //   or    DATE(datevalue)
                Result := Arg1;
                if VarIsArray(Result) then
                begin
                  try
                    Arg1 := VarToStr(Result[0]);
                    Arg1 := VarToStr(Result[1]);
                  except
                    on EVariantError do // no Params for DATE
                      raise EDatabaseError.CreateFmt('Missing parameter', [Arg1]);
                  end;

                  S := FormatSettings.ShortDateFormat;
                  try
                    FormatSettings.ShortDateFormat := Arg1;
                    Result := StrToDate(Arg1);
                  finally
                    FormatSettings.ShortDateFormat := S;
                  end;
                end
                else
                  Result := Longint(Trunc(VarToDateTime(Result)));
              end

              else if (lstrcmpiA(P, 'TIME') = 0) then  // Format TIME('timestring','formatstring')
              begin                                               // or     TIME(datetimevalue)
                Result := Arg1;
                if (VarIsArray(Result)) then
                begin
                  try
                    Arg1 := VarToStr(Result[0]);
                    Arg1 := VarToStr(Result[1]);
                  except
                    on EVariantError do // no Params for TIME
                      raise EDatabaseError.CreateFmt('Missing parameter', [Arg1]);
                  end;

                  S := FormatSettings.ShortTimeFormat;
                  try
                    FormatSettings.ShortTimeFormat := Arg1;
                    Result := StrToTime(Arg1);
                  finally
                    FormatSettings.ShortTimeFormat := S;
                  end;
               end
               else
                 Result := Frac(VarToDateTime(Result));
              end

              else raise EDatabaseError.CreateFmt('Function not supported (%s)', [P]);
            end;

          else raise EDatabaseError.CreateFmt('coOp not supported', [Ord(Node^.coOp)]);
        end;

      nodeLISTELEM:
        case (Node^.coOp) of
          coLISTELEM2:
            begin
              Result := VarArrayCreate([0, 50], VarVariant); // Create VarArray for ListElements Values

              I := 0;
              TempNode := PCANHdr(@FilterParser.FilterData[CANEXPRSIZE + PWord(@PChar(Node)[CANHDRSIZE + I * 2])^]);
              while (TempNode^.nodeClass = nodeLISTELEM) do
              begin
                Arg1 := ParseNode(TempNode);
                if (not VarIsArray(Arg1)) then
                  Result[I] := Arg1
                else
                begin
                  Z := 0;
                  while (not VarIsEmpty(Arg1[Z])) do
                  begin
                    Result[I + Z] := Arg1[Z];
                    Inc(Z);
                  end;
                end;

                Inc(I);
                TempNode := PCANHdr(@FilterParser.FilterData[CANEXPRSIZE + PWord(@PChar(Node)[CANHDRSIZE + I * 2])^]);
             end;

             // Only one or no Value, so don't return as VarArray
             if (I < 2) then
               if (VIsNull(Result[0])) then
                 Result := False
               else
                 Result := VarAsType(Result[0], varString);
            end;
          else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
        end;

      else
        raise EDatabaseError.CreateFmt('nodeClass not supported', [Ord(Node^.nodeClass)]);
    end;
  end;

begin
  Expr := PCANExpr(@FilterParser.FilterData[0]);
  Result := ParseNode(@FilterParser.FilterData[Expr^.iNodeStart]);
end;

{ TMySQLTable *****************************************************************}

function TMySQLTable.GetCanModify(): Boolean;
begin
  Result := not FReadOnly and inherited;
end;

constructor TMySQLTable.Create(AOwner: TComponent);
begin
  inherited;

  FAutomaticLoadNextRecords := False;
  SetLength(DeleteBookmarks, 0);
  FCommandType := ctTable;
  FLimitedDataReceived := False;
  FFilterSQL := '';
end;

procedure TMySQLTable.InternalClose();
begin
  inherited;

  FLimitedDataReceived := False;
end;

procedure TMySQLTable.InternalLast();
begin
  if (LimitedDataReceived and AutomaticLoadNextRecords and LoadNextRecords(True)) then
    RecordsReceived.WaitFor(INFINITE);

  inherited;
end;

procedure TMySQLTable.InternalOpen();
begin
  Assert(CommandText <> '');

  inherited;

  if (IsCursorOpen()) then
    if (Filtered) then
      InternActivateFilter();
end;

procedure TMySQLTable.InternalRefresh();
begin
  InternRecordBuffers.Clear();

  RecordsReceived.ResetEvent();

  if (Connection.ExecuteSQL(smDataSet, True, TMySQLTable(Self).SQLSelect())) then
  begin
    SynchroThread := Connection.SynchroThread;
    SynchroThread.BindDataSet(Self);
  end;
end;

function TMySQLTable.LoadNextRecords(const AllRecords: Boolean = False): Boolean;
begin
  RecordsReceived.ResetEvent();

  Result := Connection.ExecuteSQL(smDataSet, True, SQLSelect(AllRecords));
  if (Result) then
  begin
    SynchroThread := Connection.SynchroThread;
    SynchroThread.BindDataSet(Self);
  end;
end;

procedure TMySQLTable.SetCommandText(const ACommandText: string);
var
  FieldInfo: TFieldInfo;
  I: Integer;
begin
  inherited;

  FTableName := ACommandText;

  for I := 0 to FieldCount - 1 do
    if (GetFieldInfo(Fields[I].Origin, FieldInfo)) then
      Fields[I].Origin := '"' + DatabaseName + '"."' + FTableName + '"."' + FieldInfo.OriginalFieldName + '"';
end;

procedure TMySQLTable.Sort(const ASortDef: TIndexDef);
var
  FieldName: string;
  Pos: Integer;
  StringFieldsEnclosed: Boolean;
begin
  StringFieldsEnclosed := False;
  Pos := 1;
  repeat
    FieldName := ExtractFieldName(ASortDef.Fields, Pos);
    if (Assigned(FindField(FieldName))) then
      StringFieldsEnclosed := StringFieldsEnclosed or (FieldByName(FieldName).DataType in [ftWideMemo, ftWideString]);
  until (FieldName = '');

  if (Active and (ASortDef.Fields <> FSortDef.Fields) or (ASortDef.DescFields <> FSortDef.DescFields)) then
  begin
    if ((ASortDef.Fields <> '') and not LimitedDataReceived and (InternRecordBuffers.Count < 1000) and not StringFieldsEnclosed) then
      inherited
    else
    begin
      FSortDef.Assign(ASortDef);


      CheckBrowseMode();

      FOffset := 0;
      InternRecordBuffers.Clear();
      Connection.ExecuteSQL(SQLSelect(), InternalRefreshEvent);

      SetFieldsSortTag();

      First();
    end;
  end;
end;

function TMySQLTable.SQLSelect(const IgnoreLimit: Boolean = False): string;
var
  DescFieldName: string;
  DescPos: Integer;
  FieldName: string;
  FirstField: Boolean;
  Pos: Integer;
begin
  Result := 'SELECT * FROM ';
  if (DatabaseName <> '') then
    Result := Result + Connection.EscapeIdentifier(DatabaseName) + '.';
  Result := Result + Connection.EscapeIdentifier(TableName);
  if (FilterSQL <> '') then
    Result := Result + ' WHERE ' + FilterSQL;
  if (SortDef.Fields <> '') then
  begin
    Result := Result + ' ORDER BY ';
    Pos := 1; FirstField := True;
    repeat
      FieldName := ExtractFieldName(SortDef.Fields, Pos);
      if (FieldName <> '') then
      begin
        if (not FirstField) then Result := Result + ',';
        Result := Result + Connection.EscapeIdentifier(FieldName);

        DescPos := 1;
        repeat
          DescFieldName := ExtractFieldName(SortDef.DescFields, DescPos);
          if (DescFieldName = FieldName) then
            Result := Result + ' DESC';
        until (DescFieldName = '');
      end;
      FirstField := False;
    until (FieldName = '');
  end;
  if (Limit > 0) then
  begin
    Result := Result + ' LIMIT ';
    if (Offset + InternRecordBuffers.Count > 0) then
      Result := Result + IntToStr(Offset + InternRecordBuffers.Count) + ',';
    if (IgnoreLimit) then
      RequestedRecordCount := $7fffffff - (Offset + InternRecordBuffers.Count)
    else if (InternRecordBuffers.Count = 0) then
      RequestedRecordCount := Limit
    else
      RequestedRecordCount := InternRecordBuffers.Count;
    Result := Result + IntToStr(RequestedRecordCount);
  end;
  Result := Result + ';' + #13#10;
end;

{******************************************************************************}

initialization
  MySQLConnectionOnSynchronize := nil;
  SynchronizingThreads := TList.Create();
  SynchronizingThreadsCS := TCriticalSection.Create();

  LocaleFormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
  SetLength(MySQLLibraries, 0);
finalization
  SynchronizingThreadsCS.Free();
  SynchronizingThreads.Free();

  FreeMySQLLibraries();
end.

