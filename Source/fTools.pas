unit fTools;

interface {********************************************************************}

uses
  Windows, XMLDoc, XMLIntf, DBGrids, msxml,
  SysUtils, DB, Classes, Graphics, SyncObjs,
  ODBCAPI,
  DISQLite3Api,
  MySQLDB, SQLUtils, CSVUtils,
  fClient;

const
  CP_UNICODE = 1200;
  BOM_UTF8: PAnsiChar = Chr($EF) + Chr($BB) + Chr($BF);
  BOM_UNICODE: PAnsiChar = Chr($FF) + Chr($FE);

type
  TTStringBuffer = class
  private
    FBuffer: PChar;
    FIndex: Integer;
    FMemSize: Integer;
  public
    procedure Clear(); virtual;
    constructor Create(const AMemSize: Integer); virtual;
    destructor Destroy(); override;
    function Read(): string; virtual;
    procedure Write(const Content: string); virtual;
    property Buffer: PChar read FBuffer;
    property Size: Integer read FIndex;
  end;

  TTools = class(TThread)
  type
    TItem = record
      Client: TCClient;
      DatabaseName: string;
      TableName: string;
    end;
    TErrorType = (TE_Database, TE_NoPrimaryIndex, TE_DifferentPrimaryIndex, TE_File, TE_ODBC, TE_SQLite, TE_XML, TE_Warning);
    TError = record
      ErrorType: TErrorType;
      ErrorCode: Integer;
      ErrorMessage: string;
    end;
    TErrorEvent = procedure(const Sender: TObject; const Error: TError; const Item: TItem; var Success: TDataAction) of object;
    TOnExecuted = procedure(const Success: Boolean) of object;
    PProgressInfos = ^TProgressInfos;
    TProgressInfos = record
      TablesDone, TablesSum: Integer;
      RecordsDone, RecordsSum: Int64;
      TimeDone, TimeSum: TDateTime;
      Progress: Byte;
    end;
    TOnUpdate = procedure(const ProgressInfos: TProgressInfos) of object;
    TSQLThread = class(TThread)
    private
      Client: TCClient;
      SQL: string;
    public
      constructor Create(const AClient: TCClient; const ASQL: string);
      procedure Execute(); override;
    end;
  private
    CriticalSection: TCriticalSection;
    FOnExecuted: TOnExecuted;
    FErrorCount: Integer;
    FOnError: TErrorEvent;
    FOnUpdate: TOnUpdate;
    FUserAbort: THandle;
    ProgressInfos: TProgressInfos;
  protected
    StartTime: TDateTime;
    Success: TDataAction;
    procedure AfterExecute(); virtual;
    procedure BackupTable(const Item: TItem; const Rename: Boolean = False); virtual;
    procedure BeforeExecute(); virtual;
    function DatabaseError(const Client: TCClient): TError; virtual;
    procedure DoError(const Error: TError; const Item: TItem); overload; virtual;
    procedure DoError(const Error: TError; const Item: TItem; var SQL: string); overload; virtual;
    procedure DoUpdateGUI(); virtual; abstract;
    function EmptyToolsItem(): TItem; virtual;
    function NoPrimaryIndexError(): TError; virtual;
    property OnError: TErrorEvent read FOnError write FOnError;
  public
    Wnd: HWND;
    constructor Create(); virtual;
    destructor Destroy(); override;
    property ErrorCount: Integer read FErrorCount;
    property OnExecuted: TOnExecuted read FOnExecuted write FOnExecuted;
    property OnUpdate: TOnUpdate read FOnUpdate write FOnUpdate;
    property UserAbort: THandle read FUserAbort;
  end;

  TTImport = class(TTools)
  type
    TImportType = (itInsert, itReplace, itUpdate);
    PItem = ^TItem;
    TItem = record
      TableName: string;
      RecordsDone, RecordsSum: Integer;
      Done: Boolean;
      SourceTableName: string;
    end;
  private
    FClient: TCClient;
    FDatabase: TCDatabase;
    FieldNames: string;
  protected
    Items: array of TItem;
    procedure AfterExecute(); override;
    procedure AfterExecuteData(var Item: TItem); virtual;
    procedure BeforeExecute(); override;
    procedure BeforeExecuteData(var Item: TItem); virtual;
    procedure Close(); virtual;
    procedure DoExecuteSQL(const Item: TItem; var SQL: string); virtual;
    procedure DoUpdateGUI(); override;
    procedure ExecuteData(var Item: TItem; const Table: TCTable); virtual;
    procedure ExecuteStructure(var Item: TItem); virtual;
    function GetValues(const Item: TItem; out Values: RawByteString): Boolean; overload; virtual;
    function GetValues(const Item: TItem; var Values: TSQLStrings): Boolean; overload; virtual;
    procedure Open(); virtual;
    function ToolsItem(const Item: TItem): TTools.TItem; virtual;
    property Client: TCClient read FClient;
    property Database: TCDatabase read FDatabase;
  public
    Fields: array of TCTableField;
    Data: Boolean;
    Error: Boolean;
    SourceFields: array of record
      Name: string;
    end;
    ImportType: TImportType;
    Structure: Boolean;
    constructor Create(const AClient: TCClient; const ADatabase: TCDatabase); reintroduce; virtual;
    procedure Execute(); override;
    property OnError;
  end;

  TTImportFile = class(TTImport)
  private
    BytesPerSector: DWord;
    FFilename: TFileName;
    FileBuffer: record
      Mem: PAnsiChar;
      Index: DWord;
      Size: DWord;
    end;
    FFileSize: DWord;
  protected
    FCodePage: Cardinal;
    FileContent: record
      Str: string;
      Index: Integer;
    end;
    FilePos: TLargeInteger;
    Handle: THandle;
    function DoOpenFile(const Filename: TFileName; out Handle: THandle; out Error: TTools.TError): Boolean; virtual;
    function ReadContent(const NewFilePos: TLargeInteger = -1): Boolean; virtual;
    procedure DoUpdateGUI(); override;
    procedure Open(); override;
    property FileSize: DWord read FFileSize;
  public
    procedure Close(); override;
    constructor Create(const AFilename: TFileName; const ACodePage: Cardinal; const AClient: TCClient; const ADatabase: TCDatabase); reintroduce; virtual;
    property CodePage: Cardinal read FCodePage;
    property Filename: TFileName read FFilename;
  end;

  TTImportSQL = class(TTImportFile)
  private
    FSetCharacterSetApplied: Boolean;
  public
    Text: PString;
    constructor Create(const AFilename: TFileName; const ACodePage: Cardinal; const AClient: TCClient; const ADatabase: TCDatabase); override;
    procedure Execute(); overload; override;
    property SetCharacterSetApplied: Boolean read FSetCharacterSetApplied;
  end;

  TTImportText = class(TTImportFile)
  private
    CSVColumns: array of Integer;
    CSVValues: TCSVValues;
    FileFields: array of record
      Name: string;
      FieldTypes: set of Byte;
    end;
    function GetHeadlineNameCount(): Integer;
    function GetHeadlineName(Index: Integer): string;
  protected
    procedure AfterExecuteData(var Item: TTImport.TItem); override;
    procedure BeforeExecuteData(var Item: TTImport.TItem); override;
    procedure ExecuteStructure(var Item: TTImport.TItem); override;
    function GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean; overload; override;
    function GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean; overload; override;
  public
    Charset: string;
    Collation: string;
    Delimiter: Char;
    Engine: string;
    Quoter: Char;
    RowType: TMySQLRowType;
    UseHeadline: Boolean;
    procedure Add(const TableName: string); virtual;
    procedure Close(); override;
    constructor Create(const AFilename: TFileName; const ACodePage: Cardinal; const AClient: TCClient; const ADatabase: TCDatabase); reintroduce; virtual;
    destructor Destroy(); override;
    function GetPreviewValues(var Values: TSQLStrings): Boolean; virtual;
    procedure Open(); override;
    property HeadlineNameCount: Integer read GetHeadlineNameCount;
    property HeadlineNames[Index: Integer]: string read GetHeadlineName;
  end;

  TTImportODBC = class(TTImport)
  private
    ColumnDesc: array of record
      ColumnName: PSQLTCHAR;
      SQLDataType: SQLSMALLINT;
      MaxDataSize: SQLUINTEGER;
      DecimalDigits: SQLSMALLINT;
      Nullable: SQLSMALLINT;
      SQL_C_TYPE: SQLSMALLINT;
    end;
    FHandle: SQLHANDLE;
    ODBCData: SQLPOINTER;
    Stmt: SQLHANDLE;
  protected
    procedure AfterExecuteData(var Item: TTImport.TItem); override;
    procedure BeforeExecute(); override;
    procedure BeforeExecuteData(var Item: TTImport.TItem); override;
    function GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean; overload; override;
    function GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean; overload; override;
    procedure ExecuteStructure(var Item: TTImport.TItem); override;
    function ODBCStmtException(const Handle: SQLHSTMT): Exception;
  public
    Charset: string;
    Collation: string;
    Engine: string;
    RowType: TMySQLRowType;
    procedure Add(const TableName: string; const SourceTableName: string); virtual;
    constructor Create(const AHandle: SQLHANDLE; const ADatabase: TCDatabase); reintroduce; virtual;
    destructor Destroy(); override;
  end;

  TTImportSQLite = class(TTImport)
  private
    Handle: sqlite3_ptr;
    Stmt: sqlite3_stmt_ptr;
  protected
    procedure AfterExecuteData(var Item: TTImport.TItem); override;
    procedure BeforeExecute(); override;
    procedure BeforeExecuteData(var Item: TTImport.TItem); override;
    procedure ExecuteStructure(var Item: TTImport.TItem); override;
    function GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean; overload; override;
    function GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean; overload; override;
  public
    Charset: string;
    Collation: string;
    Engine: string;
    RowType: TMySQLRowType;
    procedure Add(const TableName: string; const SheetName: string); virtual;
    constructor Create(const AHandle: sqlite3_ptr; const ADatabase: TCDatabase); reintroduce; virtual;
  end;

  TTImportXML = class(TTImport)
  private
    XMLDocument: IXMLDOMDocument;
    XMLNode: IXMLDOMNode;
  protected
    procedure BeforeExecute(); override;
    procedure BeforeExecuteData(var Item: TTImport.TItem); override;
    function GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean; overload; override;
    function GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean; overload; override;
  public
    RecordTag: string;
    procedure Add(const TableName: string); virtual;
    constructor Create(const AFilename: TFileName; const ATable: TCBaseTable); reintroduce; virtual;
    destructor Destroy(); override;
  end;

  TTExport = class(TTools)
  type
    PExportObject = ^TExportObject;
    TExportObject = record
      DBObject: TCDBObject;
      RecordsDone, RecordsSum: Integer;
      Done: Boolean;
    end;
    TExportDBGrid = record
      DBGrid: TDBGrid;
      RecordsDone, RecordsSum: Integer;
      Done: Boolean;
    end;
    TExportDBGrids = array of TExportDBGrid;
  private
    DataTables: TList;
    FDBGrids: TExportDBGrids;
    FClient: TCClient;
    ExportObjects: array of TExportObject;
  protected
    procedure AfterExecute(); override;
    procedure BeforeExecute(); override;
    procedure DoUpdateGUI(); override;
    function EmptyToolsItem(): TTools.TItem; override;
    procedure ExecuteDatabaseFooter(const Database: TCDatabase); virtual;
    procedure ExecuteDatabaseHeader(const Database: TCDatabase); virtual;
    procedure ExecuteDBGrid(var ExportDBGrid: TExportDBGrid); virtual;
    procedure ExecuteEvent(const Event: TCEvent); virtual;
    procedure ExecuteFooter(); virtual;
    procedure ExecuteHeader(); virtual;
    procedure ExecuteRoutine(const Routine: TCRoutine); virtual;
    procedure ExecuteTable(var ExportObject: TExportObject; const ResultHandle: TMySQLConnection.TResultHandle); virtual;
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); virtual;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); virtual;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); virtual; abstract;
    procedure ExecuteTrigger(const Trigger: TCTrigger); virtual;
    function ToolsItem(const ExportDBGrid: TExportDBGrid): TTools.TItem; overload; virtual;
    function ToolsItem(const ExportObject: TExportObject): TTools.TItem; overload; virtual;
    property DBGrids: TExportDBGrids read FDBGrids;
  public
    Data: Boolean;
    DestinationFields: array of record
      Name: string;
    end;
    Fields: array of TField;
    Structure: Boolean;
    TableFields: array of TCTableField;
    procedure Add(const ADBGrid: TDBGrid); overload; virtual;
    procedure Add(const ADBObject: TCDBObject); overload; virtual;
    constructor Create(const AClient: TCClient); reintroduce; virtual;
    destructor Destroy(); override;
    procedure Execute(); override;
    property Client: TCClient read FClient;
    property OnError;
  end;

  TTExportFile = class(TTExport)
  private
    ContentBuffer: TTStringBuffer;
    FCodePage: Cardinal;
    FileBuffer: record
      Mem: PAnsiChar;
      Size: Cardinal;
    end;
    FFilename: TFileName;
    Handle: THandle;
    procedure Flush();
  protected
    procedure CloseFile(); virtual;
    procedure DoFileCreate(const Filename: TFileName); virtual;
    function FileCreate(const Filename: TFileName; out Error: TTools.TError): Boolean; virtual;
    procedure WriteContent(const Content: string); virtual;
  public
    constructor Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal); reintroduce; virtual;
    destructor Destroy(); override;
    property CodePage: Cardinal read FCodePage;
    property Filename: TFileName read FFilename;
  end;

  TTExportSQL = class(TTExportFile)
  private
    ForeignKeySources: string;
    SQLInsertPacketLen: Integer;
    SQLInsertPostfix: string;
    SQLInsertPostfixPacketLen: Integer;
    SQLInsertPrefix: string;
    SQLInsertPrefixPacketLen: Integer;
  protected
    procedure ExecuteDatabaseFooter(const Database: TCDatabase); override;
    procedure ExecuteDatabaseHeader(const Database: TCDatabase); override;
    procedure ExecuteEvent(const Event: TCEvent); override;
    procedure ExecuteFooter(); override;
    procedure ExecuteHeader(); override;
    procedure ExecuteRoutine(const Routine: TCRoutine); override;
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTrigger(const Trigger: TCTrigger); override;
    function FileCreate(const Filename: TFileName; out Error: TTools.TError): Boolean; override;
  public
    CreateDatabaseStmts: Boolean;
    DisableKeys: Boolean;
    IncludeDropStmts: Boolean;
    ReplaceData: Boolean;
    UseDatabaseStmts: Boolean;
    constructor Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal); override;
  end;

  TTExportText = class(TTExportFile)
  protected
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    function FileCreate(const Filename: TFileName; out Error: TTools.TError): Boolean; override;
  public
    Quoter: Char;
    Delimiter: string;
    QuoteStringValues: Boolean;
    QuoteValues: Boolean;
    constructor Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal); override;
    destructor Destroy(); override;
  end;

  TTExportUML = class(TTExportFile)
  protected
    procedure ExecuteHeader(); override;
  end;

  TTExportHTML = class(TTExportUML)
  private
    CSS: array of string;
    FieldOfPrimaryIndex: array of Boolean;
    Font: TFont;
    SQLFont: TFont;
    RowOdd: Boolean;
    function Escape(const Str: string): string;
  protected
    procedure ExecuteDatabaseHeader(const Database: TCDatabase); override;
    procedure ExecuteFooter(); override;
    procedure ExecuteHeader(); override;
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
  public
    IndexBackground: Boolean;
    TextContent: Boolean;
    NULLText: Boolean;
    RowBackground: Boolean;
    constructor Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal); override;
    destructor Destroy(); override;
  end;

  TTExportXML = class(TTExportUML)
  private
    function Escape(const Str: string): string; virtual;
  protected
    procedure ExecuteDatabaseFooter(const Database: TCDatabase); override;
    procedure ExecuteDatabaseHeader(const Database: TCDatabase); override;
    procedure ExecuteFooter(); override;
    procedure ExecuteHeader(); override;
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
  public
    DatabaseTag, DatabaseAttribute: string;
    FieldTag, FieldAttribute: string;
    RecordTag: string;
    RootTag: string;
    TableTag, TableAttribute: string;
    constructor Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal); override;
  end;

  TTExportODBC = class(TTExport)
  private
    FHandle: SQLHDBC;
    FODBC: SQLHENV;
    FStmt: SQLHSTMT;
    Parameter: array of record
      Buffer: SQLPOINTER;
      BufferSize: SQLINTEGER;
      Size: SQLINTEGER;
    end;
  protected
    TableName: string;
    procedure ExecuteHeader(); override;
    procedure ExecuteFooter(); override;
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    property Handle: SQLHDBC read FHandle;
    property ODBC: SQLHENV read FODBC;
    property Stmt: SQLHSTMT read FStmt;
  public
    constructor Create(const AClient: TCClient; const AODBC: SQLHDBC = SQL_NULL_HANDLE; const AHandle: SQLHDBC = SQL_NULL_HANDLE); reintroduce; virtual;
  end;

  TTExportAccess = class(TTExportODBC)
  private
    Filename: TFileName;
  protected
    procedure ExecuteFooter(); override;
    procedure ExecuteHeader(); override;
  public
    constructor Create(const AClient: TCClient; const AFilename: TFileName); reintroduce; virtual;
  end;

  TTExportExcel = class(TTExportODBC)
  private
    Filename: TFileName;
    Sheet: Integer;
  protected
    procedure ExecuteFooter(); override;
    procedure ExecuteHeader(); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
  public
    constructor Create(const AClient: TCClient; const AFilename: TFileName); reintroduce; virtual;
  end;

  TTExportSQLite = class(TTExport)
  private
    Filename: TFileName;
    Handle: sqlite3_ptr;
    Stmt: sqlite3_stmt_ptr;
    Text: array of RawByteString;
  protected
    procedure ExecuteFooter(); override;
    procedure ExecuteHeader(); override;
    procedure ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
    procedure ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery); override;
  public
    constructor Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal); reintroduce; virtual;
  end;

  TTFind = class(TTools)
  type
    PItem = ^TItem;
    TItem = record
      DatabaseName: string;
      TableName: string;
      FieldNames: array of string;
      RecordsFound, RecordsDone, RecordsSum: Integer;
      Done: Boolean;
    end;
  private
    FClient: TCClient;
  protected
    FItem: PItem;
    Items: array of TItem;
    procedure AfterExecute(); override;
    procedure BeforeExecute(); override;
    function DoExecuteSQL(const Client: TCClient; var Item: TItem; var SQL: string): Boolean; virtual;
    procedure DoUpdateGUI(); override;
    procedure ExecuteDefault(var Item: TItem; const Table: TCBaseTable); virtual;
    procedure ExecuteMatchCase(var Item: TItem; const Table: TCBaseTable); virtual;
    procedure ExecuteWholeValue(var Item: TItem; const Table: TCBaseTable); virtual;
    function ToolsItem(const Item: TItem): TTools.TItem; virtual;
    property Client: TCClient read FClient;
  public
    FindText: string;
    MatchCase: Boolean;
    WholeValue: Boolean;
    RegExpr: Boolean;
    procedure Add(const Table: TCBaseTable; const Field: TCTableField = nil); virtual;
    constructor Create(const AClient: TCClient); reintroduce; virtual;
    destructor Destroy(); override;
    procedure Execute(); override;
  end;

  TTReplace = class(TTFind)
  private
    FReplaceClient: TCClient;
  protected
    procedure ExecuteMatchCase(var Item: TTFind.TItem; const Table: TCBaseTable); override;
    property ReplaceConnection: TCClient read FReplaceClient;
  public
    ReplaceText: string;
    Backup: Boolean;
    constructor Create(const AClient, AReplaceClient: TCClient); reintroduce; virtual;
    property OnError;
  end;

  TTTransfer = class(TTools)
  type
    TItem = record
      Client: TCClient;
      DatabaseName: string;
      TableName: string;
      RecordsSum, RecordsDone: Integer;
      Done: Boolean;
    end;
    TElement = record
      Source: TItem;
      Destination: TItem;
    end;
  private
    DataHandle: TMySQLConnection.TResultHandle;
    Elements: TList;
  protected
    procedure AfterExecute(); override;
    procedure BeforeExecute(); override;
    procedure CloneTable(var Source, Destination: TItem); virtual;
    function DifferentPrimaryIndexError(): TTools.TError; virtual;
    function DoExecuteSQL(var Item: TItem; const Client: TCClient; var SQL: string): Boolean; virtual;
    procedure DoUpdateGUI(); override;
    procedure ExecuteData(var Source, Destination: TItem); virtual;
    procedure ExecuteForeignKeys(var Source, Destination: TItem); virtual;
    procedure ExecuteStructure(const Source, Destination: TItem); virtual;
    procedure ExecuteTable(var Source, Destination: TItem); virtual;
    function ToolsItem(const Item: TItem): TTools.TItem; virtual;
  public
    Backup: Boolean;
    Data: Boolean;
    DisableKeys: Boolean;
    Structure: Boolean;
    procedure Add(const SourceClient: TCClient; const SourceDatabaseName, SourceTableName: string; const DestinationClient: TCClient; const DestinationDatabaseName, DestinationTableName: string); virtual;
    constructor Create(); override;
    destructor Destroy(); override;
    procedure Execute(); override;
    property OnError;
  end;

  EODBCError = EDatabaseError;

function SQLiteException(const Handle: sqlite3_ptr; const ReturnCode: Integer; const AState: PString = nil): SQLRETURN;
function ODBCException(const Stmt: SQLHSTMT; const ReturnCode: SQLRETURN; const AState: PString = nil): SQLRETURN;

const
  BackupExtension = '_bak';

implementation {***************************************************************}

uses
  ActiveX,
  Forms, Consts, DBConsts, Registry, DBCommon, StrUtils, Math, Variants,
  PerlRegEx,
  MySQLConsts,
  fPreferences;

resourcestring
  SSourceParseError = 'Source code of "%s" cannot be analyzed (%d):' + #10#10 + '%s';
  SInvalidQuoter = 'Quoter "%s" not supported for SQL Values import';

const
  SQLPacketSize = 100 * 1024;
  FilePacketSize = 500 * 1024;
  ODBCDataSize = 65536;

  daSuccess = daRetry;

  STR_LEN = 128;

function UMLEncoding(const Codepage: Cardinal): string;
var
  Reg: TRegistry;
begin
  Result := '';

  Reg := TRegistry.Create();
  Reg.RootKey := HKEY_CLASSES_ROOT;
  if (Reg.OpenKey('\MIME\Database\Codepage\' + IntToStr(Codepage), False)) then
  begin
    if (Reg.ValueExists('WebCharset')) then
      Result := Reg.ReadString('WebCharset')
    else if (Reg.ValueExists('BodyCharset')) then
      Result := Reg.ReadString('BodyCharset');
    Reg.CloseKey();
  end;
  Reg.Free();
end;

function GetUTCDateTime(Date: TDateTime): string;
const
  EnglishShortMonthNames : array[1..12] of string
    = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  EnglishShortDayNames : array[1..7] of string
    = ('Sun', 'Mon', 'Thu', 'Wed', 'Thu', 'Fri', 'Sat');
const
  TIME_ZONE_ID_UNKNOWN = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
var
  Day: Byte;
  Month: Byte;
  S: string;
  TempShortDayNames: array[1..12] of string;
  TempShortMonthNames: array[1..12] of string;
  TimeZoneInformation: TTimeZoneInformation;
  TZIBias: Integer;
  TZIName: string;
begin
  case GetTimeZoneInformation(TimeZoneInformation) of
    TIME_ZONE_ID_STANDARD:
      begin
        TZIName := TimeZoneInformation.StandardName;
        TZIBias := TimeZoneInformation.Bias + TimeZoneInformation.StandardBias;
      end;
    TIME_ZONE_ID_DAYLIGHT:
      begin
        TZIName := TimeZoneInformation.DaylightName;
        TZIBias := TimeZoneInformation.Bias + TimeZoneInformation.DaylightBias;
      end;
    else
      begin
        TZIName := '';
        TZIBias := TimeZoneInformation.Bias;
      end;
  end;
  S := TimeToStr(EncodeTime(Abs(TZIBias div 60), Abs(TZIBias mod 60), 0, 0), FileFormatSettings);
  S := Copy(S, 1, 2) + Copy(S, 4, 2);
  if TZIBias>0 then S := '-' + S else S := '+' + S;
  for Month := 1 to 12 do TempShortMonthNames[Month] := FormatSettings.ShortMonthNames[Month];
  for Month := 1 to 12 do FormatSettings.ShortMonthNames[Month] := EnglishShortMonthNames[Month];
  for Day := 1 to 7 do TempShortDayNames[Day] := FormatSettings.ShortDayNames[Day];
  for Day := 1 to 7 do FormatSettings.ShortDayNames[Day] := EnglishShortDayNames[Day];
  S := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "' + S + '"', Now());
  for Day := 1 to 7 do FormatSettings.ShortDayNames[Day] := TempShortDayNames[Day];
  for Month := 1 to 12 do FormatSettings.ShortMonthNames[Month] := TempShortMonthNames[Month];
  if (Pos('(', TZIName)>0) and (Pos(')', TZIName)>0) then
    S := S + ' ' + Copy(TZIName, Pos('(', TZIName), Pos(')', TZIName)-Pos('(', TZIName) + 1);
  Result := S;
end;

function SQLLoadDataInfile(const Database: TCDatabase; const IgnoreHeadline, Replace: Boolean; const Filename, FileCharset, DatabaseName, TableName: string; const FieldNames: string; const Quoter, FieldTerminator, LineTerminator: string): string;
var
  Client: TCClient;
begin
  Client := Database.Client;

  Result := 'LOAD DATA LOCAL INFILE ' + SQLEscape(Filename) + #13#10;
  if (Replace) then
    Result := Result + '  REPLACE' + #13#10;
  Result := Result + '  INTO TABLE ' + Client.EscapeIdentifier(DatabaseName) + '.' + Client.EscapeIdentifier(TableName) + #13#10;
  if (((50038 <= Client.ServerVersion) and (Client.ServerVersion < 50100) or (50117 <= Client.ServerVersion)) and (FileCharset <> '')) then
    Result := Result + '  CHARACTER SET ' + FileCharset + #13#10;
  Result := Result + '  FIELDS' + #13#10;
  Result := Result + '    TERMINATED BY ' + SQLEscape(FieldTerminator) + #13#10;
  if (Quoter <> '') then
    Result := Result + '    OPTIONALLY ENCLOSED BY ' + SQLEscape(Quoter) + #13#10;
  Result := Result + '    ESCAPED BY ' + SQLEscape('\') + #13#10;
  Result := Result + '  LINES' + #13#10;
  Result := Result + '    TERMINATED BY ' + SQLEscape(LineTerminator) + #13#10;
  if (IgnoreHeadline) then
    Result := Result + '  IGNORE 1 LINES' + #13#10;
  if (FieldNames <> '') then
    Result := Result + '  (' + FieldNames + ')' + #13#10;
  Result := Trim(Result) + ';' + #13#10;

  if (((Client.ServerVersion < 50038) or (50100 <= Client.ServerVersion)) and (Client.ServerVersion < 50117) and (FileCharset <> '')) then
    if ((Client.ServerVersion < 40100) or not Assigned(Client.VariableByName('character_set_database'))) then
      Client.Charset := FileCharset
    else if ((Client.VariableByName('character_set_database').Value <> FileCharset) and (Client.LibraryType <> ltHTTP)) then
      Result :=
        'SET SESSION character_set_database=' + SQLEscape(FileCharset) + ';' + #13#10
        + Result
        + 'SET SESSION character_set_database=' + SQLEscape(Client.VariableByName('character_set_database').Value) + ';' + #13#10;
end;

function SQLUpdate(const Table: TCTable; const Values, WhereClausel: string): string;
begin
  Result := 'UPDATE ' + Table.Database.Client.EscapeIdentifier(Table.Name)
    + ' SET ' + Values
    + ' WHERE ' + WhereClausel + ';' + #13#10;
end;

function SQLiteException(const Handle: sqlite3_ptr; const ReturnCode: Integer; const AState: PString = nil): SQLRETURN;
begin
  if ((ReturnCode = SQLITE_MISUSE)) then
    raise Exception.Create('Invalid SQLite Handle')
  else if ((ReturnCode <> SQLITE_OK) and (ReturnCode < SQLITE_ROW)) then
    raise EODBCError.Create(UTF8ToString(sqlite3_errmsg(@Handle)) + ' (' + IntToStr(ReturnCode) + ')');

  Result := ReturnCode;
end;

function DataFileEscape(const Value: PAnsiChar; const Length: Integer): RawByteString; overload;
label
  StartL,
  StringL, String2,
  PositionL, PositionE,
  FindPos, FindPos2,
  Finish;
const
  SearchLen = 7;
  Search: array [0 .. SearchLen - 1] of AnsiChar = (#0, #9, #10, #13, '''', '"', '\');
  Replace: array [0 .. 2 * SearchLen - 1] of AnsiChar = ('\','0', '\','t', '\','n', '\','r', '\','''', '\','"', '\','\');
var
  Len: Integer;
  Positions: packed array [0 .. SearchLen - 1] of Cardinal;
begin
  if (Length = 0) then
    Result := ''''''
  else
  begin
try
    SetLength(Result, 2 * Length + 2); // reserve space
except
    SetLength(Result, 2 * Length + 2); // reserve space
end;

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]
        MOV ECX,Length                   // Length of Value string

        MOV AL,''''                      // Start quoting
        STOSB

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindPos                     // Find Search character position
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!

      // -------------------

      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first position
        MOV EAX,0                        // Last character
        LEA EDX,Positions
      PositionL:
        CMP [EDX + ECX * 4],EAX          // Position before other positions?
        JB PositionE                     // No!
        MOV EBX,ECX                      // Index of first position
        MOV EAX,[EDX + EBX * 4]          // Value of first position
      PositionE:
        INC ECX                          // Next Position
        CMP ECX,SearchLen                // All Positions compared?
        JNE PositionL                    // No!

        POP ECX

        SUB ECX,EAX                      // Copy normal characters from Value
        CMP ECX,0                        // Is there something to copy?
        JE String2                       // No!
        REPNE MOVSB                      //   to Result

      String2:
        MOV ECX,EAX

        CMP ECX,0                        // Is there a character to replace?
        JE Finish                        // No!

        ADD ESI,1                        // Step of Search character
        LEA EDX,Replace                  // Insert Replace characters
        MOV AX,[EDX + EBX * 2]
        STOSW

        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!
        CALL FindPos                     // Find Search character
        JMP StringL

      // -------------------

      FindPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AL,[EDI + EBX * 1]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASB                      // Find Search character
        JNE FindPos2                     // Search character not found!
        INC ECX
      FindPos2:
        LEA EDI,Positions
        MOV [EDI + EBX * 4],ECX          // Store found position
        POP EDI
        POP ECX
        RET

      // -------------------

      Finish:
        MOV AL,''''                      // End quoting
        STOSB

        MOV EAX,Result                   // Calculate new length of Result
        MOV EAX,[EAX]
        SUB EDI,EAX
        MOV Len,EDI

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    SetLength(Result, Len);
  end;
end;

function DataFileEscape(const Value: RawByteString): RawByteString; overload;
begin
  if (Value = '') then
    Result := ''''''
  else
    Result := DataFileEscape(@Value[1], Length(Value));
end;

function DataFileValue(const Value: string; const Quote: LongBool = False): RawByteString; overload;
label
  StringL,
  Finish;
var
  Len: Cardinal;
begin
  Len := Length(Value);
  if (not Quote) then
    SetLength(Result, Len)
  else
    SetLength(Result, Len + 2);
  if (Len > 0) then
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

        CMP Quote,False                  // Quote Value?
        JE StringL                       // No!
        MOV AL,''''
        STOSB

      StringL:
        LODSW                            // Load WideChar from Value
        STOSB                            // Store AnsiChar into Result
        LOOP StringL                     // Repeat for all characters

        CMP Quote,False                  // Quote Value?
        JE Finish                        // No!
        MOV AL,''''
        STOSB

      Finish:
        POP EDI
        POP ESI
        POP ES
    end;
end;

function ODBCError(const HandleType: SQLSMALLINT; const Handle: SQLHSTMT): TTools.TError;
var
  cbMessageText: SQLSMALLINT;
  MessageText: PSQLTCHAR;
  SQLState: array [0 .. SQL_SQLSTATE_SIZE] of SQLTCHAR;
begin
  Result.ErrorType := TE_ODBC;
  Result.ErrorCode := 0;
  if (not SQL_SUCCEEDED(SQLGetDiagRec(HandleType, Handle, 1, @SQLState, nil, nil, 0, @cbMessageText))) then
    Result.ErrorMessage := 'Unknown ODBC Error.'
  else
  begin
    GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTCHAR));
    SQLGetDiagRec(HandleType, Handle, 1, nil, nil, MessageText, cbMessageText + 1, nil);
    Result.ErrorMessage := PChar(MessageText) + ' (' + SQLState + ')';
    FreeMem(MessageText);
  end;
end;

function ODBCException(const Stmt: SQLHSTMT; const ReturnCode: SQLRETURN; const AState: PString = nil): SQLRETURN;
var
  cbMessageText: SQLSMALLINT;
  MessageText: PSQLTCHAR;
  Msg: string;
  SQLState: array [0 .. SQL_SQLSTATE_SIZE] of SQLTCHAR;
begin
  ZeroMemory(@SQLState, SizeOf(SQLState));

  if ((ReturnCode < SQL_SUCCESS) or (ReturnCode = SQL_SUCCESS_WITH_INFO)) then
    if (SQLGetDiagRec(SQL_HANDLE_STMT, Stmt, 1, @SQLState, nil, nil, 0, @cbMessageText) = SQL_INVALID_HANDLE) then
      raise Exception.Create('Invalid ODBC Handle')
    else if ((SQLState <> '') and (SQLState <> '01004')) then
    begin
      GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTChar));
      SQLGetDiagRec(SQL_HANDLE_STMT, Stmt, 1, nil, nil, MessageText, cbMessageText + 1, nil);
      Msg := PChar(MessageText) + ' (' + SQLState + ')';
      FreeMem(MessageText);
      raise EODBCError.Create(Msg);
    end;

  if (Assigned(AState)) then
    AState^ := SQLState;

  Result := ReturnCode;
end;

{ TTStringBuffer *****************************************************************}

procedure TTStringBuffer.Clear();
begin
  FIndex := 0;
end;

constructor TTStringBuffer.Create(const AMemSize: Integer);
begin
  FIndex := 0;
  FMemSize := AMemSize;
  GetMem(FBuffer, (FMemSize + 1) * SizeOf(Buffer[0]));
end;

destructor TTStringBuffer.Destroy();
begin
  if (Assigned(Buffer)) then
    FreeMem(Buffer);

  inherited;
end;

function TTStringBuffer.Read(): string;
begin
  SetString(Result, FBuffer, FIndex);
  Clear();
end;

procedure TTStringBuffer.Write(const Content: string);
var
  Len: Integer;
begin
  Len := Length(Content);

  if (Len > 0) then
  begin
    if (FIndex + Len > FMemSize) then
    begin
      FMemSize := FIndex + Len + 1;
      ReallocMem(FBuffer, (FMemSize + 1) * SizeOf(FBuffer[0]));
    end;

    MoveMemory(@FBuffer[FIndex], @Content[1], Len * SizeOf(FBuffer[0]));
    Inc(FIndex, Len);
  end;
end;

{ TTools.TSQLThread ***********************************************************}

constructor TTools.TSQLThread.Create(const AClient: TCClient; const ASQL: string);
begin
  inherited Create(False);

  Client := AClient;
  SQL := ASQL;
end;

procedure TTools.TSQLThread.Execute();
begin
  Client.ExecuteSQL(SQL);
end;

{ TTools **********************************************************************}

procedure TTools.AfterExecute();
begin
  DoUpdateGUI();

  if (Assigned(OnExecuted)) then
    OnExecuted(Success = daSuccess);
end;

procedure TTools.BackupTable(const Item: TTools.TItem; const Rename: Boolean = False);
var
  Database: TCDatabase;
  NewTableName: string;
  Table: TCBaseTable;
begin
  Database := Item.Client.DatabaseByName(Item.DatabaseName);

  if (Assigned(Database)) then
  begin
    Table := Database.BaseTableByName(Item.TableName);
    if (Assigned(Table)) then
    begin
      NewTableName := Item.TableName + BackupExtension;

      if (Assigned(Database.BaseTableByName(NewTableName))) then
        while ((Success = daSuccess) and not Database.DeleteObject(Database.BaseTableByName(NewTableName))) do
          DoError(DatabaseError(Item.Client), Item);

      if (Rename) then
        while (Success = daSuccess) do
        begin
          Database.RenameTable(Table, NewTableName);
          if (Item.Client.ErrorCode <> 0) then
            DoError(DatabaseError(Item.Client), Item);
        end
      else
        while ((Success = daSuccess) and not Database.CloneTable(Table, NewTableName, True)) do
          DoError(DatabaseError(Item.Client), Item);
    end;
  end;
end;

procedure TTools.BeforeExecute();
begin
  StartTime := Now();
  Success := daSuccess;

  DoUpdateGUI();
end;

constructor TTools.Create();
begin
  inherited Create(True);

  Success := daSuccess;

  FErrorCount := 0;

  FUserAbort := CreateEvent(nil, False, False, '');
  CriticalSection := TCriticalSection.Create();
end;

function TTools.DatabaseError(const Client: TCClient): TTools.TError;
begin
  Result.ErrorType := TE_Database;
  Result.ErrorCode := Client.ErrorCode;
  Result.ErrorMessage := Client.ErrorMessage;
end;

destructor TTools.Destroy();
begin
  CriticalSection.Free();
  CloseHandle(FUserAbort);

  inherited;
end;

procedure TTools.DoError(const Error: TTools.TError; const Item: TTools.TItem);
var
  ErrorTime: TDateTime;
begin
  Inc(FErrorCount);
  if (Success <> daAbort) then
    if (not Assigned(OnError)) then
      Success := daAbort
    else
    begin
      ErrorTime := Now();
      OnError(Self, Error, Item, Success);
      StartTime := StartTime + ErrorTime - Now();
    end;
end;

procedure TTools.DoError(const Error: TTools.TError; const Item: TTools.TItem; var SQL: string);
begin
  DoError(Error, Item);
  if (Success = daFail) then
  begin
    Delete(SQL, 1, SQLStmtLength(SQL));
    Success := daSuccess;
  end;
end;

function TTools.EmptyToolsItem(): TTools.TItem;
begin
  Result.Client := nil;
  Result.DatabaseName := '';
  Result.TableName := '';
end;

function TTools.NoPrimaryIndexError(): TTools.TError;
begin
  Result.ErrorType := TE_NoPrimaryIndex;
end;

{ TTImport ********************************************************************}

procedure TTImport.AfterExecute();
begin
  Close();

  if (Success = daSuccess) then
    Client.CommitTransaction()
  else
    Client.RollbackTransaction();
  Client.EndSilent();
  Client.EndSynchron();

  inherited;
end;

procedure TTImport.AfterExecuteData(var Item: TItem);
begin
end;

procedure TTImport.BeforeExecute();
begin
  inherited;

  Client.BeginSilent();
  Client.BeginSynchron(); // We're still in a thread
  Client.StartTransaction();
end;

procedure TTImport.BeforeExecuteData(var Item: TItem);
var
  I: Integer;
begin
  FieldNames := '';
  if (not Structure and (Length(Fields) > 0)) then
    for I := 0 to Length(Fields) - 1 do
    begin
      if (I > 0) then FieldNames := FieldNames + ',';
      FieldNames := FieldNames + Client.EscapeIdentifier(Fields[I].Name);
    end;
end;

procedure TTImport.Close();
begin
end;

constructor TTImport.Create(const AClient: TCClient; const ADatabase: TCDatabase);
begin
  inherited Create();

  FClient := AClient;
  FDatabase := ADatabase;

  Data := False;
  Structure := False;
end;

procedure TTImport.DoExecuteSQL(const Item: TItem; var SQL: string);
var
  Result: Boolean;
begin
  while ((Success = daSuccess) and (SQL <> '')) do
  begin
    Result := Client.ExecuteSQL(SQL);
    Delete(SQL, 1, Client.ExecutedSQLLength);
    SQL := Trim(SQL);
    if (not Result) then
      DoError(DatabaseError(Client), ToolsItem(Item), SQL);
  end
end;

procedure TTImport.DoUpdateGUI();
var
  I: Integer;
begin
  CriticalSection.Enter();

  ProgressInfos.TablesDone := 0;
  ProgressInfos.TablesSum := Length(Items);
  ProgressInfos.RecordsDone := 0;
  ProgressInfos.RecordsSum := 0;
  ProgressInfos.TimeDone := 0;
  ProgressInfos.TimeSum := 0;

  for I := 0 to Length(Items) - 1 do
  begin
    if (Items[I].Done) then
      Inc(ProgressInfos.TablesDone);

    Inc(ProgressInfos.RecordsDone, Items[I].RecordsDone);
    Inc(ProgressInfos.RecordsSum, Items[I].RecordsSum);
  end;

  ProgressInfos.TimeDone := Now() - StartTime;

  if ((ProgressInfos.RecordsDone = 0) and (ProgressInfos.TablesDone = 0)) then
  begin
    ProgressInfos.Progress := 0;
    ProgressInfos.TimeSum := 0;
  end
  else if (ProgressInfos.RecordsDone = 0) then
  begin
    ProgressInfos.Progress := Round(ProgressInfos.TablesDone / ProgressInfos.TablesSum * 100);
    ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.TablesDone * ProgressInfos.TablesSum;
  end
  else if (ProgressInfos.RecordsDone < ProgressInfos.RecordsSum) then
  begin
    ProgressInfos.Progress := Round(ProgressInfos.RecordsDone / ProgressInfos.RecordsSum * 100);
    ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.RecordsDone * ProgressInfos.RecordsSum;
  end
  else
  begin
    ProgressInfos.Progress := 100;
    ProgressInfos.TimeSum := ProgressInfos.TimeDone;
  end;

  CriticalSection.Leave();

  if (Assigned(FOnUpdate)) then
    FOnUpdate(ProgressInfos);
end;

procedure TTImport.Execute();
var
  Error: TTools.TError;
  I: Integer;
begin
  BeforeExecute();

  Open();

  for I := 0 to Length(Items) - 1 do
    if (Success <> daAbort) then
    begin
      Success := daSuccess;

      if (Structure) then
      begin
        if (Assigned(Database.TableByName(Items[I].TableName))) then
          while ((Success = daSuccess) and not Database.DeleteObject(Database.TableByName(Items[I].TableName))) do
            DoError(DatabaseError(Client), ToolsItem(Items[I]));
        if (Success = daSuccess) then
          ExecuteStructure(Items[I]);
      end;

      if ((Success = daSuccess) and Data) then
      begin
        while ((Success = daSuccess) and not Assigned(Database.TableByName(Items[I].TableName))) do
        begin
          Error.ErrorType := TE_Database;
          Error.ErrorCode := 0;
          Error.ErrorMessage := 'Table "' + Items[I].TableName + '" does not exists.';
          DoError(DatabaseError(Client), ToolsItem(Items[I]));
        end;
        if (Success = daSuccess) then
          ExecuteData(Items[I], Database.TableByName(Items[I].TableName));
      end;

      Items[I].Done := True;
    end;

  AfterExecute();
end;

procedure TTImport.ExecuteData(var Item: TItem; const Table: TCTable);
var
  Buffer: TTStringBuffer;
  BytesToWrite: DWord;
  BytesWritten: DWord;
  DataSet: TMySQLQuery;
  DBValues: RawByteString;
  Error: TTools.TError;
  I: Integer;
  Pipe: THandle;
  Pipename: string;
  S: string;
  SQL: string;
  SQLThread: TSQLThread;
  SQLValues: TSQLStrings;
  Values: string;
  WhereClausel: string;
begin
  BeforeExecuteData(Item);

  if (Success = daSuccess) then
  begin
    SQL := '';
    if (Client.DatabaseName <> Database.Name) then
      SQL := SQL + Database.SQLUse() + #13#10;
    if (Structure and (Client.ServerVersion >= 40000)) then
      SQL := SQL + 'ALTER TABLE ' + Client.EscapeIdentifier(Table.Name) + ' DISABLE KEYS;' + #13#10;
    if (SQL <> '') then
      DoExecuteSQL(Item, SQL);

    if ((ImportType <> itUpdate) and Client.LoadDataFile) then
    begin
      Pipename := '\\.\pipe\' + LoadStr(1000);
      Pipe := CreateNamedPipe(PChar(Pipename),
                              PIPE_ACCESS_OUTBOUND, PIPE_TYPE_MESSAGE or PIPE_READMODE_BYTE or PIPE_WAIT,
                              1, NET_BUFFER_LENGTH, 0, NMPWAIT_USE_DEFAULT_WAIT, nil);
      if (Pipe = INVALID_HANDLE_VALUE) then
      begin
        Error.ErrorType := TE_File;
        Error.ErrorCode := GetLastError();
        Error.ErrorMessage := SysErrorMessage(GetLastError());
        DoError(Error, ToolsItem(Item));
        Success := daAbort;
      end
      else
      begin
        SQL := '';
        if (Database.Name <> Client.DatabaseName) then
          SQL := SQL + Database.SQLUse();
        SQL := SQL + SQLLoadDataInfile(Database, False, ImportType = itReplace, Pipename, Client.Charset, Database.Name, Table.Name, FieldNames, '''', ',', #13#10);

        SQLThread := TSQLThread.Create(Client, SQL);

        if (ConnectNamedPipe(Pipe, nil)) then
        begin
          Item.RecordsDone := 0;
          while ((Success = daSuccess) and GetValues(Item, DBValues)) do
          begin
            BytesToWrite := Length(DBValues);
            if (not WriteFile(Pipe, PAnsiChar(DBValues)^, BytesToWrite, BytesWritten, nil) or (BytesWritten < BytesToWrite)) then
            begin
              Error.ErrorType := TE_File;
              Error.ErrorCode := GetLastError();
              Error.ErrorMessage := SysErrorMessage(GetLastError());
              DoError(Error, ToolsItem(Item));
              Success := daAbort;
            end;

            Inc(Item.RecordsDone);
            if (Item.RecordsDone mod 100 = 0) then DoUpdateGUI();

            if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
              Success := daAbort;
          end;

          if (FlushFileBuffers(Pipe) and WriteFile(Pipe, PAnsiChar(DBValues)^, 0, BytesWritten, nil) and FlushFileBuffers(Pipe)) then
            SQLThread.WaitFor();
          DisconnectNamedPipe(Pipe);

          if (not Client.ErrorCode = 0) then
            DoError(DatabaseError(Client), ToolsItem(Item), SQL);
        end;

        FreeAndNil(SQLThread);
        CloseHandle(Pipe);
      end;

      if ((Success = daSuccess) and (ImportType = itInsert) and (Client.WarningCount > 0)) then
      begin
        DataSet := TMySQLQuery.Create(nil);
        DataSet.Connection := Client;
        DataSet.CommandText := 'SHOW WARNINGS';

        DataSet.Open();
        if (DataSet.Active and not DataSet.IsEmpty()) then
        begin
          Error.ErrorType := TE_Warning;
          Error.ErrorCode := 1;
          repeat
            Error.ErrorMessage := Error.ErrorMessage + Trim(DataSet.FieldByName('Message').AsString) + #13#10;
          until (not DataSet.FindNext());
          DoError(Error, ToolsItem(Item));
        end;
        DataSet.Free();
      end;
    end
    else
    begin
      Buffer := TTStringBuffer.Create(SQLPacketSize);
      if (Database.Name <> Client.DatabaseName) then
        Buffer.Write(Database.SQLUse());

      SetLength(SQLValues, Length(Fields));
      while ((Success = daSuccess) and GetValues(Item, SQLValues)) do
      begin
        Values := ''; WhereClausel := '';
        for I := 0 to Length(Fields) - 1 do
          if (ImportType <> itUpdate) then
          begin
            if (Values <> '') then Values := Values + ',';
            Values := Values + SQLValues[I];
          end
          else if (not Fields[I].InPrimaryKey) then
          begin
            if (Values <> '') then Values := Values + ',';
            Values := Client.EscapeIdentifier(Fields[I].Name) + '=' + SQLValues[I];
          end
          else
          begin
            if (WhereClausel <> '') then WhereClausel := WhereClausel + ' AND ';
            WhereClausel := Client.EscapeIdentifier(Fields[I].Name) + '=' + SQLValues[I];
          end;

        if (ImportType = itUpdate) then
          Buffer.Write(SQLUpdate(Table, Values, WhereClausel))
        else if (Buffer.Size = 0) then
        begin
          if (ImportType = itReplace) then
            SQL := 'REPLACE INTO '
          else
            SQL := 'INSERT INTO ';
          SQL := SQL + Client.EscapeIdentifier(Table.Name);
          if (FieldNames <> '') then
            SQL := SQL + ' (' + FieldNames + ')';
          SQL := SQL + ' VALUES (' + Values + ')';
          Buffer.Write(SQL);
        end
        else
          Values := ',(' + Values + ')';

        if ((Buffer.Size > 0) and ((ImportType = itUpdate) and not Client.MultiStatements or (Buffer.Size >= SQLPacketSize))) then
        begin
          if (ImportType = itUpdate) then
            Buffer.Write(';' + #13#10);
          S := Buffer.Read();
          DoExecuteSQL(Item, S);
          Buffer.Write(S);
        end;

        Inc(Item.RecordsDone);
        if (Item.RecordsDone mod 100 = 0) then DoUpdateGUI();

        if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
          Success := daAbort;
      end;
      SetLength(SQLValues, 0);

      if (Success = daSuccess) then
      begin
        if ((ImportType <> itUpdate) and (Buffer.Size > 0)) then
          Buffer.Write(';' + #13#10);

        S := Buffer.Read();
        DoExecuteSQL(Item, S);
      end;

      Buffer.Free();
    end;

    SQL := '';
    if (Structure and (Client.ServerVersion >= 40000)) then
      SQL := SQL + 'ALTER TABLE ' + Client.EscapeIdentifier(Table.Name) + ' ENABLE KEYS;' + #13#10;
    if (SQL <> '') then
      DoExecuteSQL(Item, SQL);
  end;

  AfterExecuteData(Item);
end;

procedure TTImport.ExecuteStructure(var Item: TItem);
begin
end;

function TTImport.GetValues(const Item: TItem; out Values: RawByteString): Boolean;
begin
  Result := False;
end;

function TTImport.GetValues(const Item: TItem; var Values: TSQLStrings): Boolean;
begin
  Result := False;
end;

procedure TTImport.Open();
begin
end;

function TTImport.ToolsItem(const Item: TItem): TTools.TItem;
begin
  Result.Client := FClient;
  if (not Assigned(FDatabase)) then
    Result.DatabaseName := ''
  else
    Result.DatabaseName := FDatabase.Name;
  Result.TableName := Item.TableName;
end;

{ TTImportFile ****************************************************************}

procedure TTImportFile.Close();
begin
  if (Handle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(Handle);
    Handle := INVALID_HANDLE_VALUE;
  end;

  if (Assigned(FileBuffer.Mem)) then
    VirtualFree(FileBuffer.Mem, FileBuffer.Size, MEM_RELEASE);
  FileBuffer.Index := 0;
  FileBuffer.Size := 0;

  FileContent.Str := '';
  FileContent.Index := 1;
end;

constructor TTImportFile.Create(const AFilename: TFileName; const ACodePage: Cardinal; const AClient: TCClient; const ADatabase: TCDatabase);
begin
  inherited Create(AClient, ADatabase);

  FFilename := AFilename;
  FCodePage := ACodePage;

  FilePos := 0;
  FileBuffer.Mem := nil;
  FileBuffer.Index := 0;
  FileBuffer.Size := 0;
  FileContent.Str := '';
  FileContent.Index := 1;
  FFileSize := 0;

  Handle := INVALID_HANDLE_VALUE;
end;

procedure TTImportFile.DoUpdateGUI();
begin
  CriticalSection.Enter();

  ProgressInfos.TablesDone := -1;
  ProgressInfos.TablesSum := -1;
  ProgressInfos.RecordsDone := FilePos;
  ProgressInfos.RecordsSum := FileSize;
  ProgressInfos.TimeDone := 0;
  ProgressInfos.TimeSum := 0;

  ProgressInfos.TimeDone := Now() - StartTime;

  if ((ProgressInfos.RecordsDone = 0) or (ProgressInfos.RecordsSum = 0)) then
  begin
    ProgressInfos.Progress := 0;
    ProgressInfos.TimeSum := 0;
  end
  else if (ProgressInfos.RecordsDone < ProgressInfos.RecordsSum) then
  begin
    ProgressInfos.Progress := Round(ProgressInfos.RecordsDone / ProgressInfos.RecordsSum * 100);
    ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.RecordsDone * ProgressInfos.RecordsSum;
  end
  else
  begin
    ProgressInfos.Progress := 100;
    ProgressInfos.TimeSum := ProgressInfos.TimeDone;
  end;

  CriticalSection.Leave();

  if (Assigned(OnUpdate)) then
    OnUpdate(ProgressInfos);
end;

function TTImportFile.DoOpenFile(const Filename: TFileName; out Handle: THandle; out Error: TTools.TError): Boolean;
var
  NumberofFreeClusters: DWord;
  SectorsPerCluser: DWord;
  TotalNumberOfClusters: DWord;
begin
  Result := True;

  try
    Handle := CreateFile(PChar(Filename),
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         nil,
                         OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0);

    if (Handle = INVALID_HANDLE_VALUE) then
    begin
      Error.ErrorType := TE_File;
      Error.ErrorCode := 0;
      Error.ErrorMessage := SysErrorMessage(GetLastError());
      DoError(Error, EmptyToolsItem());
    end
    else
    begin
      FFileSize := GetFileSize(Handle, nil);
      if (FFileSize = 0) then
        FileBuffer.Mem := nil
      else
      begin
        if (not GetDiskFreeSpace(PChar(ExtractFileDrive(Filename)), SectorsPerCluser, BytesPerSector, NumberofFreeClusters, TotalNumberOfClusters)) then
          raise EInOutError.Create(SysErrorMessage(GetLastError()));
        FileBuffer.Size := BytesPerSector + Min(FFileSize, FilePacketSize);
        Inc(FileBuffer.Size, BytesPerSector - FileBuffer.Size mod BytesPerSector);
        FileBuffer.Mem := VirtualAlloc(nil, FileBuffer.Size, MEM_COMMIT, PAGE_READWRITE);
        FileBuffer.Index := BytesPerSector;

        ReadContent();
      end;
    end;
  except
    Error.ErrorType := TE_File;
    Error.ErrorCode := GetLastError();
    Error.ErrorMessage := SysErrorMessage(GetLastError());

    Result := False;
  end;
end;

function TTImportFile.ReadContent(const NewFilePos: TLargeInteger = -1): Boolean;
var
  DistanceToMove: TLargeInteger;
  Error: TTools.TError;
  Index: Integer;
  Len: Integer;
  ReadSize: DWord;
  UTF8Bytes: Byte;
begin
  // The file will be read without buffering in Windows OS. Because of this,
  // we have to read complete sectors...

  if ((Success = daSuccess) and (NewFilePos >= 0)) then
  begin
    FileContent.Str := '';

    DistanceToMove := NewFilePos - NewFilePos mod BytesPerSector;
    if ((SetFilePointer(Handle, LARGE_INTEGER(DistanceToMove).LowPart, @LARGE_INTEGER(DistanceToMove).HighPart, FILE_BEGIN) = INVALID_FILE_SIZE) and (GetLastError() <> 0)) then
    begin
      Error.ErrorType := TE_File;
      Error.ErrorCode := GetLastError();
      Error.ErrorMessage := SysErrorMessage(GetLastError());
      DoError(Error, EmptyToolsItem());
    end;
    FileBuffer.Index := BytesPerSector + NewFilePos mod BytesPerSector;

    FilePos := NewFilePos;
  end
  else
  begin
    FileBuffer.Index := BytesPerSector;
    if (FileContent.Index > 1) then
      Delete(FileContent.Str, 1, FileContent.Index - 1);
  end;
  FileContent.Index := 1;

  if ((Success = daSuccess) and ReadFile(Handle, FileBuffer.Mem[BytesPerSector], FileBuffer.Size - BytesPerSector, ReadSize, nil) and (ReadSize > 0)) then
  begin
    if (FilePos = 0) then
    begin
      if (CompareMem(@FileBuffer.Mem[FileBuffer.Index + 0], BOM_UTF8, Length(BOM_UTF8))) then
      begin
        FCodePage := CP_UTF8;
        FilePos := Length(BOM_UTF8);
      end
      else if (CompareMem(@FileBuffer.Mem[FileBuffer.Index + 0], BOM_UNICODE, Length(BOM_UNICODE))) then
      begin
        FCodePage := CP_UNICODE;
        FilePos := Length(BOM_UNICODE);
      end
      else
        FilePos := 0;

      Inc(FileBuffer.Index, FilePos);
    end;

    case (CodePage) of
      CP_UNICODE:
        begin
          Index := 1 + Length(FileContent.Str);
          Len := Integer(ReadSize - (FileBuffer.Index - BytesPerSector));
          SetLength(FileContent.Str, Length(FileContent.Str) + Len div SizeOf(Char));
          MoveMemory(@FileContent.Str[Index], @FileBuffer.Mem[FileBuffer.Index], Len);
        end;
      else
        begin
          // UTF-8 coded bytes has to be separated well for the
          // MultiByteToWideChar function.

          UTF8Bytes := 0;
          if (CodePage = CP_UTF8) then
            while ((ReadSize > 0) and (Byte(FileBuffer.Mem[BytesPerSector + ReadSize - UTF8Bytes]) and $C0 = $80)) do
              Inc(UTF8Bytes);

          Len := MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS, @FileBuffer.Mem[FileBuffer.Index], BytesPerSector + ReadSize - FileBuffer.Index, nil, 0);
          if (Len > 0) then
          begin
            SetLength(FileContent.Str, Length(FileContent.Str) + Len);
            MultiByteToWideChar(CodePage, 0, @FileBuffer.Mem[FileBuffer.Index], BytesPerSector + ReadSize - FileBuffer.Index, @FileContent.Str[Length(FileContent.Str) - Len + 1], Len)
          end
          else if (GetLastError() <> 0) then
          begin
            Error.ErrorType := TE_File;
            Error.ErrorCode := GetLastError();
            Error.ErrorMessage := SysErrorMessage(GetLastError());
            DoError(Error, EmptyToolsItem());
          end;

          if (UTF8Bytes > 0) then
            MoveMemory(@FileBuffer.Mem[BytesPerSector - UTF8Bytes], @FileBuffer.Mem[BytesPerSector + ReadSize - UTF8Bytes], UTF8Bytes);
          FileBuffer.Index := BytesPerSector - UTF8Bytes;
        end;
    end;
  end;

  Result := (Success = daSuccess) and (ReadSize > 0);
end;

procedure TTImportFile.Open();
var
  Error: TTools.TError;
begin
  FilePos := 0;

  while ((Success = daSuccess) and not DoOpenFile(FFilename, Handle, Error)) do
    DoError(Error, EmptyToolsItem());
end;

{ TTImportSQL *************************************************************}

constructor TTImportSQL.Create(const AFilename: TFileName; const ACodePage: Cardinal; const AClient: TCClient; const ADatabase: TCDatabase);
begin
  inherited;

  SetLength(Items, 1);
  Items[0].TableName := '';
  Items[0].RecordsDone := 0;
  Items[0].RecordsSum := 0;
  Items[0].Done := False;

  FSetCharacterSetApplied := False;
  Text := nil
end;

procedure TTImportSQL.Execute();
var
  CLStmt: TSQLCLStmt;
  CompleteStmt: Boolean;
  Eof: Boolean;
  Index: Integer;
  Len: Integer;
  SetCharacterSet: Boolean;
  SQL: string;
begin
  if (not Assigned(Text)) then
    BeforeExecute();

  Open();

  if (Assigned(Text)) then
    Text^ := ''
  else if ((Success = daSuccess) and Assigned(Database) and (Client.DatabaseName <> Database.Name)) then
  begin
    SQL := Database.SQLUse();
    DoExecuteSQL(Items[0], SQL);
  end;

  Index := 1; Eof := False;
  while ((Success = daSuccess) and (not Eof or (Index <= Length(FileContent.Str)))) do
  begin
    repeat
      Len := SQLStmtLength(FileContent.Str, Index, @CompleteStmt);
      if (not CompleteStmt) then
      begin
        Eof := not ReadContent();
        if (not Eof) then
          Len := 0
        else
          Len := Length(FileContent.Str) - Index + 1;
      end;
    until ((Len > 0) or Eof);

    case (CodePage) of
      CP_UNICODE: Inc(FilePos, Len * SizeOf(Char));
      else if (Len > 0) then Inc(FilePos, WideCharToMultiByte(CodePage, 0, PChar(@FileContent.Str[Index]), Len, nil, 0, nil, nil));
    end;

    SetCharacterSet := not EOF
      and SQLParseCLStmt(CLStmt, @FileContent.Str[Index], Length(FileContent.Str), Client.ServerVersion)
      and (CLStmt.CommandType in [ctSetNames, ctSetCharacterSet]);

    if ((Index > 1) and (SetCharacterSet or (Index - 1 + Len >= SQLPacketSize))) then
    begin
      if (Assigned(Text)) then
        Text^ := Text^ + Copy(FileContent.Str, 1, Index - 1)
      else
      begin
        SQL := Copy(FileContent.Str, 1, Index - 1);
        DoExecuteSQL(Items[0], SQL);
      end;
      Delete(FileContent.Str, 1, Index - 1); Index := 1;

      DoUpdateGUI();
    end;

    if (Success = daSuccess) then
    begin
      if (not SetCharacterSet) then
        Inc(Index, Len)
      else
      begin
        FSetCharacterSetApplied := True;

        FCodePage := Client.CharsetToCodePage(CLStmt.ObjectName);

        ReadContent(FilePos); // Clear FileContent
      end;
    end;

    if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
      Success := daAbort;
  end;

  if (Success = daSuccess) then
    if (Assigned(Text)) then
      Text^ := Text^ + FileContent.Str
    else
    begin
      SQL := FileContent.Str;
      DoExecuteSQL(Items[0], SQL);
    end;

  Close();

  if (not Assigned(Text)) then
    AfterExecute();
end;

{ TTImportText ****************************************************************}

procedure TTImportText.AfterExecuteData(var Item: TTImport.TItem);
begin
  SetLength(CSVValues, 0);

  inherited;
end;

procedure TTImportText.BeforeExecuteData(var Item: TTImport.TItem);
var
  I: Integer;
  J: Integer;
begin
  inherited;

  SetLength(CSVColumns, Length(SourceFields));
  for I := 0 to Length(SourceFields) - 1 do
  begin
    CSVColumns[I] := -1;
    for J := 0 to HeadlineNameCount - 1 do
      if (SourceFields[I].Name = HeadlineNames[J]) then
        CSVColumns[I] := J;
  end;
end;

procedure TTImportText.Close();
begin
  inherited;

  SetLength(FileFields, 0);
end;

procedure TTImportText.Add(const TableName: string);
begin
  SetLength(Items, Length(Items) + 1);
  Items[Length(Items) - 1].TableName := TableName;
  Items[Length(Items) - 1].RecordsDone := 0;
  Items[Length(Items) - 1].RecordsSum := 0;
  Items[Length(Items) - 1].Done := False;
end;

constructor TTImportText.Create(const AFilename: TFileName; const ACodePage: Cardinal; const AClient: TCClient; const ADatabase: TCDatabase);
begin
  inherited Create(AFilename, ACodePage, AClient, ADatabase);

  SetLength(CSVValues, 0);
  Data := True;
  Delimiter := ',';
  Quoter := '"';
end;

destructor TTImportText.Destroy();
begin
  SetLength(Fields, 0);

  inherited;
end;

procedure TTImportText.ExecuteStructure(var Item: TTImport.TItem);
var
  I: Integer;
  NewField: TCBaseTableField;
  NewTable: TCBaseTable;
begin
  NewTable := TCBaseTable.Create(Database.Tables);

  for I := 0 to Length(FileFields) - 1 do
  begin
    NewField := TCBaseTableField.Create(NewTable.Fields);

    NewField.Name := HeadlineNames[I];

    if (SQL_INTEGER in FileFields[I].FieldTypes) then
      NewField.FieldType := mfInt
    else if (SQL_FLOAT in FileFields[I].FieldTypes) then
      NewField.FieldType := mfFloat
    else if (SQL_DATE in FileFields[I].FieldTypes) then
      NewField.FieldType := mfDate
    else
      NewField.FieldType := mfText;

    if (I > 0) then
      NewField.FieldBefore := NewTable.Fields[NewTable.Fields.Count - 1];

    NewTable.Fields.AddField(NewField);
    NewField.Free();
  end;

  NewTable.Name := Item.TableName;

  while ((Success = daSuccess) and not Database.AddTable(NewTable)) do
    DoError(DatabaseError(Client), ToolsItem(Item));
  while ((Success = daSuccess) and not Client.Update()) do
    DoError(DatabaseError(Client), ToolsItem(Item));

  NewTable.Free();

  if (Success = daSuccess) then
  begin
    NewTable := Database.BaseTableByName(Item.TableName);
    while ((Success = daSuccess) and not NewTable.Update()) do
      DoError(DatabaseError(Client), ToolsItem(Item));

    SetLength(Fields, NewTable.Fields.Count);
    for I := 0 to NewTable.Fields.Count - 1 do
      Fields[I] := NewTable.Fields[I];

    SetLength(SourceFields, NewTable.Fields.Count);
    for I := 0 to HeadlineNameCount - 1 do
      SourceFields[I].Name := HeadlineNames[I];
  end;
end;

function TTImportText.GetHeadlineNameCount(): Integer;
begin
  Result := Length(FileFields);
end;

function TTImportText.GetHeadlineName(Index: Integer): string;
begin
  Result := FileFields[Index].Name;
end;

function TTImportText.GetPreviewValues(var Values: TSQLStrings): Boolean;
var
  Eof: Boolean;
  I: Integer;
  RecordComplete: Boolean;
begin
  RecordComplete := False; Eof := False;
  while ((Success = daSuccess) and not RecordComplete and not Eof) do
  begin
    RecordComplete := CSVSplitValues(FileContent.Str, FileContent.Index, Delimiter, Quoter, CSVValues);
    if (not RecordComplete) then
      Eof := not ReadContent();
  end;

  Result := (Success = daSuccess) and RecordComplete;
  if (Result) then
  begin
    SetLength(Values, Length(CSVValues));
    for I := 0 to Length(CSVValues) - 1 do
      if (CSVValues[I].Length = 0) then
        if (not Preferences.GridNullText) then
          Values[I] := ''
        else
          Values[I] := '<NULL>'
      else
        Values[I] := CSVUnescape(CSVValues[I].Data, CSVValues[I].Length, Quoter);
  end;
end;

function TTImportText.GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean;
var
  EOF: Boolean;
  Error: TTools.TError;
  I: Integer;
  OldFileContentIndex: Integer;
  RecordComplete: Boolean;
begin
  RecordComplete := False; EOF := False; OldFileContentIndex := FileContent.Index;
  while ((Success = daSuccess) and not RecordComplete and not EOF) do
  begin
    RecordComplete := CSVSplitValues(FileContent.Str, FileContent.Index, Delimiter, Quoter, CSVValues);
    if (not RecordComplete) then
    begin
      FileContent.Index := OldFileContentIndex;
      EOF := not ReadContent();
    end;
  end;

  if (FileContent.Index - OldFileContentIndex > 0) then
    case (CodePage) of
      CP_UNICODE: Inc(FilePos, (FileContent.Index - OldFileContentIndex) * SizeOf(FileContent.Str[1]));
      else
        Inc(FilePos, WideCharToMultiByte(CodePage, 0,
          PChar(@FileContent.Str[OldFileContentIndex]), FileContent.Index - OldFileContentIndex, nil, 0, nil, nil));
    end;

  Result := RecordComplete;
  if (Result) then
  begin
    Values := '';
    if (Length(CSVValues) < HeadlineNameCount) then
    begin
      Error.ErrorType := TE_Warning;
      Error.ErrorCode := ER_WARN_TOO_FEW_RECORDS;
      Error.ErrorMessage := Format(ER_WARN_TOO_FEW_RECORDS_MSG, [Item.RecordsDone]);
      DoError(Error, ToolsItem(Item))
    end
    else if (Length(CSVValues) > HeadlineNameCount) then
    begin
      Error.ErrorType := TE_Warning;
      Error.ErrorCode := ER_WARN_TOO_MANY_RECORDS;
      Error.ErrorMessage := Format(ER_WARN_TOO_MANY_RECORDS_MSG, [Item.RecordsDone]);
      DoError(Error, ToolsItem(Item))
    end
    else
      for I := 0 to Length(Fields) - 1 do
      begin
        if (I > 0) then Values := Values + ',';
        if (CSVValues[CSVColumns[I]].Length = 0) then
          Values := Values + 'NULL'
        else if (Fields[I].FieldType = mfBit) then
          Values := Values + DataFileValue(IntToStr(BitStringToInt(CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter))))
        else if (Fields[I].FieldType in NotQuotedFieldTypes) then
          Values := Values + DataFileValue(CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter))
        else if (Fields[I].FieldType in BinaryFieldTypes) then
          Values := Values + DataFileEscape(CSVBinary(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter))
        else
          Values := Values + DataFileEscape(Client.LibEncode(CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter)));
      end;
    Values := Values + #13#10;
  end;
end;

function TTImportText.GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean;
var
  Eof: Boolean;
  Error: TTools.TError;
  I: Integer;
  OldFileContentIndex: Integer;
  RecordComplete: Boolean;
begin
  RecordComplete := False; Eof := False; OldFileContentIndex := FileContent.Index;
  while ((Success = daSuccess) and not RecordComplete and not Eof) do
  begin
    OldFileContentIndex := FileContent.Index;
    RecordComplete := CSVSplitValues(FileContent.Str, FileContent.Index, Delimiter, Quoter, CSVValues);
    if (not RecordComplete) then
      Eof := not ReadContent();
  end;

  if (FileContent.Index - OldFileContentIndex > 0) then
    case (CodePage) of
      CP_UNICODE: Inc(FilePos, (FileContent.Index - OldFileContentIndex) * SizeOf(FileContent.Str[1]));
      else
        Inc(FilePos, WideCharToMultiByte(CodePage, 0,
          PChar(@FileContent.Str[OldFileContentIndex]), FileContent.Index - OldFileContentIndex, nil, 0, nil, nil));
    end;

  Result := RecordComplete;
  if (Result) then
    if (Length(CSVValues) < HeadlineNameCount) then
    begin
      Error.ErrorType := TE_Warning;
      Error.ErrorCode := ER_WARN_TOO_FEW_RECORDS;
      Error.ErrorMessage := Format(ER_WARN_TOO_FEW_RECORDS_MSG, [Item.RecordsDone]);
      DoError(Error, ToolsItem(Item))
    end
    else if (Length(CSVValues) > HeadlineNameCount) then
    begin
      Error.ErrorType := TE_Warning;
      Error.ErrorCode := ER_WARN_TOO_MANY_RECORDS;
      Error.ErrorMessage := Format(ER_WARN_TOO_MANY_RECORDS_MSG, [Item.RecordsDone]);
      DoError(Error, ToolsItem(Item))
    end
    else
      for I := 0 to Length(Fields) - 1 do
        if (CSVValues[CSVColumns[I]].Length = 0) then
          Values[I] := 'NULL'
        else if (Fields[I].FieldType = mfBit) then
          Values[I] := IntToStr(BitStringToInt(CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter)))
        else if (Fields[I].FieldType in NotQuotedFieldTypes) then
          Values[I] := CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter)
        else if (Fields[I].FieldType in BinaryFieldTypes) then
          Values[I] := SQLEscape(CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter), Client.ServerVersion <= 40000)
        else
          Values[I] := SQLEscape(CSVUnescape(CSVValues[CSVColumns[I]].Data, CSVValues[CSVColumns[I]].Length, Quoter));
end;

procedure TTImportText.Open();
var
  DT: TDateTime;
  EOF: Boolean;
  F: Double;
  FirstRecordFilePos: Integer;
  I: Integer;
  Int: Integer;
  OldFileContentIndex: Integer;
  OldSuccess: TDataAction;
  RecNo: Integer;
  RecordComplete: Boolean;
  Value: string;
begin
  inherited;

  OldSuccess := Success; OldFileContentIndex := FileContent.Index; FirstRecordFilePos := FilePos;

  RecordComplete := False; Eof := False;
  while (not RecordComplete and not Eof) do
  begin
    RecordComplete := CSVSplitValues(FileContent.Str, FileContent.Index, Delimiter, Quoter, CSVValues);
    if (not RecordComplete) then
    begin
      Eof := not ReadContent();
      OldFileContentIndex := FileContent.Index;
    end;
  end;

  case (CodePage) of
    CP_UNICODE: Inc(FilePos, (FileContent.Index - OldFileContentIndex) * SizeOf(FileContent.Str[1]));
    else if (FileContent.Index - OldFileContentIndex > 0) then
      Inc(FilePos, WideCharToMultiByte(CodePage, 0,
        PChar(@FileContent.Str[OldFileContentIndex]), FileContent.Index - OldFileContentIndex, nil, 0, nil, nil));
  end;

  if (UseHeadline) then
  begin
    FirstRecordFilePos := FilePos;
    SetLength(FileFields, Length(CSVValues));
    for I := 0 to Length(FileFields) - 1 do
      FileFields[I].Name := CSVUnescape(CSVValues[I].Data, CSVValues[I].Length, Quoter);
  end
  else
  begin
    SetLength(FileFields, Length(CSVValues));
    for I := 0 to Length(FileFields) - 1 do
      FileFields[I].Name := 'Field_' + IntToStr(I);
  end;

  for I := 0 to Length(FileFields) - 1 do
    FileFields[I].FieldTypes := [SQL_INTEGER, SQL_FLOAT, SQL_DATE, Byte(SQL_LONGVARCHAR)];

  RecNo := 0; EOF := False;
  while ((RecNo < 20) and not EOF and RecordComplete) do
  begin
    RecordComplete := (RecNo = 0) and not UseHeadline;
    while (not RecordComplete and not Eof) do
    begin
      RecordComplete := CSVSplitValues(FileContent.Str, FileContent.Index, Delimiter, Quoter, CSVValues);
      if (not RecordComplete) then
        Eof := not ReadContent();
    end;

    if (RecordComplete and (Length(CSVValues) = Length(FileFields))) then
    begin
      for I := 0 to Length(CSVValues) - 1 do
        if (CSVValues[I].Length > 0) then
        begin
          Value := CSVUnescape(CSVValues[I].Data, CSVValues[I].Length, Quoter);
          if ((SQL_INTEGER in FileFields[I].FieldTypes) and not TryStrToInt(Value, Int)) then
            Exclude(FileFields[I].FieldTypes, SQL_INTEGER);
          if ((SQL_FLOAT in FileFields[I].FieldTypes) and not TryStrToFloat(Value, F, Client.FormatSettings)) then
            Exclude(FileFields[I].FieldTypes, SQL_FLOAT);
          if ((SQL_DATE in FileFields[I].FieldTypes) and (not TryStrToDate(Value, DT, Client.FormatSettings) or (DT < EncodeDate(1900, 1, 1)))) then
            Exclude(FileFields[I].FieldTypes, SQL_DATE);
        end;

      Inc(RecNo);
    end;
  end;

  Success := OldSuccess; ReadContent(FirstRecordFilePos);
end;

{ TTImportODBC ****************************************************************}

function SQLDataTypeToMySQLType(const SQLType: SQLSMALLINT; const Size: Integer; const FieldName: string): TMySQLFieldType;
begin
  case (SQLType) of
    SQL_CHAR: Result := mfChar;
    SQL_VARCHAR: Result := mfVarChar;
    SQL_LONGVARCHAR: Result := mfText;
    SQL_WCHAR: Result := mfChar;
    SQL_WVARCHAR: Result := mfVarChar;
    SQL_WLONGVARCHAR: Result := mfText;
    SQL_DECIMAL: Result := mfDecimal;
    SQL_NUMERIC: Result := mfDecimal;
    SQL_BIT: Result := mfBit;
    SQL_TINYINT: Result := mfTinyInt;
    SQL_SMALLINT: Result := mfSmallInt;
    SQL_INTEGER: Result := mfInt;
    SQL_BIGINT: Result := mfBigInt;
    SQL_REAL: Result := mfFloat;
    SQL_FLOAT: Result := mfFloat;
    SQL_DOUBLE: Result := mfDouble;
    SQL_BINARY: Result := mfBinary;
    SQL_VARBINARY: Result := mfVarBinary;
    SQL_LONGVARBINARY: Result := mfBlob;
    SQL_TYPE_DATE: Result := mfDate;
    SQL_TYPE_TIME: Result := mfTime;
    SQL_TYPE_TIMESTAMP: Result := mfDateTime;
    SQL_TIMESTAMP: Result := mfTimestamp;
    SQL_GUID: Result := mfChar;
    else raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [FieldName, SQLType]);
  end;
end;

procedure TTImportODBC.Add(const TableName: string; const SourceTableName: string);
begin
  SetLength(Items, Length(Items) + 1);

  Items[Length(Items) - 1].TableName := TableName;
  Items[Length(Items) - 1].RecordsDone := 0;
  Items[Length(Items) - 1].RecordsSum := 0;
  Items[Length(Items) - 1].Done := False;
  Items[Length(Items) - 1].SourceTableName := SourceTableName;
end;

procedure TTImportODBC.AfterExecuteData(var Item: TTImport.TItem);
var
  I: Integer;
begin
  Item.RecordsSum := Item.RecordsDone;

  for I := 0 to Length(ColumnDesc) - 1 do
    FreeMem(ColumnDesc[I].ColumnName);
  SetLength(ColumnDesc, 0);

  if (Assigned(ODBCData)) then
    FreeMem(ODBCData);
  if (Stmt <> SQL_NULL_HANDLE) then
    SQLFreeHandle(SQL_HANDLE_STMT, Stmt);
end;

procedure TTImportODBC.BeforeExecute();
var
  cbRecordsSum: SQLINTEGER;
  Handle: SQLHSTMT;
  I: Integer;
  RecordsSum: array [0..20] of SQLTCHAR;
  SQL: string;
begin
  inherited;

  for I := 0 to Length(Items) - 1 do
    if ((Success <> daAbort) and Data) then
    begin
      Success := daSuccess;

      if (SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @Handle))) then
      begin
        SQL := 'SELECT COUNT(*) FROM "' + Items[I].SourceTableName + '"';
        if (SQL_SUCCEEDED(SQLExecDirect(Handle, PSQLTCHAR(SQL), SQL_NTS))
          and SQL_SUCCEEDED(SQLFetch(Handle))
          and SQL_SUCCEEDED(SQLGetData(Handle, 1, SQL_C_WCHAR, @RecordsSum, SizeOf(RecordsSum) - 1, @cbRecordsSum))) then
            Items[I].RecordsSum := StrToInt(PChar(@RecordsSum));

        SQLFreeHandle(SQL_HANDLE_STMT, Handle);
      end;
    end;
end;

procedure TTImportODBC.BeforeExecuteData(var Item: TTImport.TItem);
var
  cbColumnName: SQLSMALLINT;
  ColumnNums: SQLSMALLINT;
  Error: TTools.TError;
  I: Integer;
  SQL: string;
  Unsigned: SQLINTEGER;
begin
  inherited;

  if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @Stmt))) then
  begin
    Error := ODBCError(SQL_HANDLE_DBC, FHandle);
    DoError(Error, ToolsItem(Item));
    Stmt := SQL_NULL_HANDLE;
    ODBCData := nil;
    SetLength(ColumnDesc, 0);
  end
  else
  begin
    GetMem(ODBCData, ODBCDataSize);

    SQL := '';
    if (not Structure and (Length(SourceFields) > 1)) then
      for I := 0 to Length(SourceFields) - 1 do
      begin
        if (I > 0) then SQL := SQL + ',';
        SQL := SQL + '"' + SourceFields[I].Name + '"';
      end
    else
      SQL := '*';
    SQL := 'SELECT ' + SQL + ' FROM "' + Item.SourceTableName + '"';

    while ((Success = daSuccess) and not SQL_SUCCEEDED(SQLExecDirect(Stmt, PSQLTCHAR(SQL), SQL_NTS))) do
      DoError(ODBCError(SQL_HANDLE_STMT, Stmt), ToolsItem(Item));

    if (Success = daSuccess) then
    begin
      ODBCException(Stmt, SQLNumResultCols(Stmt, @ColumnNums));

      SetLength(ColumnDesc, ColumnNums);
      if (Success = daSuccess) then
        for I := 0 to Length(ColumnDesc) - 1 do
        begin
          ODBCException(Stmt, SQLDescribeCol(Stmt, I + 1, nil, 0, @cbColumnName, nil, nil, nil, nil));
          GetMem(ColumnDesc[I].ColumnName, (cbColumnName + 1) * SizeOf(SQLTCHAR));

          ODBCException(Stmt, SQLDescribeCol(Stmt, I + 1, ColumnDesc[I].ColumnName, cbColumnName, nil, @ColumnDesc[I].SQLDataType, @ColumnDesc[I].MaxDataSize, @ColumnDesc[I].DecimalDigits, @ColumnDesc[I].Nullable));
          case (ColumnDesc[I].SQLDataType) of
            SQL_TINYINT,
            SQL_SMALLINT,
            SQL_INTEGER,
            SQL_BIGINT:
              begin
                ODBCException(Stmt, SQLColAttribute(Stmt, I + 1, SQL_DESC_UNSIGNED, nil, 0, nil, @Unsigned));
                if (Unsigned = SQL_TRUE) then
                  case (ColumnDesc[I].SQLDataType) of
                    SQL_TINYINT: ColumnDesc[I].SQL_C_TYPE := SQL_C_UTINYINT;
                    SQL_SMALLINT: ColumnDesc[I].SQL_C_TYPE := SQL_C_USHORT;
                    SQL_INTEGER: ColumnDesc[I].SQL_C_TYPE := SQL_C_ULONG;
                    SQL_BIGINT: ColumnDesc[I].SQL_C_TYPE := SQL_C_UBIGINT;
                  end
                else
                  case (ColumnDesc[I].SQLDataType) of
                    SQL_TINYINT: ColumnDesc[I].SQL_C_TYPE := SQL_C_STINYINT;
                    SQL_SMALLINT: ColumnDesc[I].SQL_C_TYPE := SQL_C_SSHORT;
                    SQL_INTEGER: ColumnDesc[I].SQL_C_TYPE := SQL_C_SLONG;
                    SQL_BIGINT: ColumnDesc[I].SQL_C_TYPE := SQL_C_SBIGINT;
                  end;
              end
          end;
        end;
    end;
  end;
end;

constructor TTImportODBC.Create(const AHandle: SQLHANDLE; const ADatabase: TCDatabase);
begin
  inherited Create(ADatabase.Client, ADatabase);

  FHandle := AHandle;

  SetLength(Items, 0);
end;

destructor TTImportODBC.Destroy();
begin
  SetLength(Items, 0);

  inherited;
end;

function TTImportODBC.GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean;
var
  cbData: SQLINTEGER;
  D: Double;
  I: Integer;
  RBS: RawByteString;
  ReturnCode: SQLRETURN;
  S: string;
  Timestamp: tagTIMESTAMP_STRUCT;
begin
  Result := SQL_SUCCEEDED(ODBCException(Stmt, SQLFetch(Stmt)));

  if (Result) then
  begin
    Values := '';
    for I := 0 to Length(Fields) - 1 do
    begin
      if (I > 0) then Values := Values + ',';

      case (ColumnDesc[I].SQLDataType) of
        SQL_BIT,
        SQL_TINYINT,
        SQL_SMALLINT,
        SQL_INTEGER,
        SQL_BIGINT:
          if (not SQL_SUCCEEDED(SQLGetData(Stmt, I + 1, SQL_C_WCHAR, ODBCData, ODBCDataSize, @cbData))) then
            ODBCException(Stmt, SQL_ERROR)
          else if (cbData = SQL_NULL_DATA) then
            Values := Values + 'NULL'
          else
          begin
            SetString(S, PChar(ODBCData), cbData div SizeOf(SQLWCHAR));
            Values := Values + DataFileValue(S, not (Fields[I].FieldType in NotQuotedFieldTypes));
          end;
        SQL_DECIMAL,
        SQL_NUMERIC,
        SQL_REAL,
        SQL_FLOAT,
        SQL_DOUBLE:
          if (not SQL_SUCCEEDED(SQLGetData(Stmt, I + 1, SQL_C_DOUBLE, @D, SizeOf(D), @cbData))) then
            ODBCException(Stmt, SQL_ERROR)
          else if (cbData = SQL_NULL_DATA) then
            Values := Values + 'NULL'
          else
            Values := Values + DataFileValue(FloatToStr(D, Client.FormatSettings), not (Fields[I].FieldType in NotQuotedFieldTypes));
        SQL_TYPE_DATE,
        SQL_TYPE_TIME,
        SQL_TYPE_TIMESTAMP:
          if (not SQL_SUCCEEDED(SQLGetData(Stmt, I + 1, SQL_C_TYPE_TIMESTAMP, @Timestamp, SizeOf(Timestamp), @cbData))) then
            ODBCException(Stmt, SQL_ERROR)
          else if (cbData = SQL_NULL_DATA) then
            Values := Values + 'NULL'
          else
            with (Timestamp) do
              if (ColumnDesc[I].SQLDataType = SQL_TYPE_TIME) then
                Values := Values + DataFileValue(TimeToStr(EncodeTime(hour, minute, second, 0), Client.FormatSettings), True)
              else if (ColumnDesc[I].SQLDataType = SQL_TYPE_TIMESTAMP) then
                Values := Values + DataFileValue(MySQLDB.DateTimeToStr(EncodeDate(year, month, day) + EncodeTime(hour, minute, second, 0), Client.FormatSettings), True)
              else if (ColumnDesc[I].SQLDataType = SQL_TYPE_DATE) then
                Values := Values + DataFileValue(MySQLDB.DateToStr(EncodeDate(year, month, day) + EncodeTime(hour, minute, second, 0), Client.FormatSettings), True);
        SQL_CHAR,
        SQL_VARCHAR,
        SQL_LONGVARCHAR,
        SQL_WCHAR,
        SQL_WVARCHAR,
        SQL_WLONGVARCHAR:
          begin
            SetLength(S, 0);
            repeat
              ReturnCode := SQLGetData(Stmt, I + 1, SQL_C_WCHAR, ODBCData, ODBCDataSize, @cbData);
              if (cbData <> SQL_NULL_DATA) then
              begin
                SetLength(S, Length(S) + cbData div SizeOf(SQLTCHAR));
                if (cbData > 0) then
                  MoveMemory(@S[1 + Length(S) - cbData div SizeOf(SQLWCHAR)], ODBCData, cbData);
              end;
            until (ReturnCode <> SQL_SUCCESS_WITH_INFO);
            if (not SQL_SUCCEEDED(ReturnCode)) then
              ODBCException(Stmt, ReturnCode)
            else if (cbData = SQL_NULL_DATA) then
              Values := Values + 'NULL'
            else
              Values := Values + DataFileEscape(Client.LibEncode(S));
          end;
        SQL_BINARY,
        SQL_VARBINARY,
        SQL_LONGVARBINARY:
          begin
            RBS := '';
            repeat
              ReturnCode := SQLGetData(Stmt, I + 1, SQL_C_BINARY, ODBCData, ODBCDataSize, @cbData);
              if (cbData > 0) then
              begin
                SetLength(RBS, Length(RBS) + cbData);
                MoveMemory(@RBS[1 + Length(RBS) - cbData], ODBCData, cbData);
              end;
            until (ReturnCode <> SQL_SUCCESS_WITH_INFO);
            if (not SQL_SUCCEEDED(ReturnCode)) then
              ODBCException(Stmt, ReturnCode)
            else if (cbData = SQL_NULL_DATA) then
              Values := Values + 'NULL'
            else
              Values := Values + DataFileEscape(RBS);
          end;
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].Name, ColumnDesc[I].SQLDataType]);
      end;
    end;
    Values := Values + #13#10;
  end;
end;

function TTImportODBC.GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean;
var
  Bytes: TBytes;
  cbData: SQLINTEGER;
  D: Double;
  I: Integer;
  ReturnCode: SQLRETURN;
  S: string;
  Timestamp: tagTIMESTAMP_STRUCT;
begin
  Result := SQL_SUCCEEDED(ODBCException(Stmt, SQLFetch(Stmt)));
  if (Result) then
    for I := 0 to Length(Fields) - 1 do
      case (ColumnDesc[I].SQLDataType) of
        SQL_BIT,
        SQL_TINYINT,
        SQL_SMALLINT,
        SQL_INTEGER,
        SQL_BIGINT:
          if (not SQL_SUCCEEDED(SQLGetData(Stmt, I + 1, SQL_C_WCHAR, ODBCData, ODBCDataSize, @cbData))) then
            ODBCException(Stmt, SQL_ERROR)
          else if (cbData = SQL_NULL_DATA) then
            Values[I] := 'NULL'
          else
          begin
            SetString(S, PChar(ODBCData), cbData div SizeOf(Char));
            Values[I] := Fields[I].EscapeValue(S);
          end;
        SQL_DECIMAL,
        SQL_NUMERIC,
        SQL_REAL,
        SQL_FLOAT,
        SQL_DOUBLE:
          if (not SQL_SUCCEEDED(SQLGetData(Stmt, I + 1, SQL_C_DOUBLE, @D, SizeOf(D), @cbData))) then
            ODBCException(Stmt, SQL_ERROR)
          else if (cbData = SQL_NULL_DATA) then
            Values[I] := 'NULL'
          else
            Values[I] := Fields[I].EscapeValue(FloatToStr(D, Client.FormatSettings));
        SQL_TYPE_DATE,
        SQL_TYPE_TIME,
        SQL_TYPE_TIMESTAMP:
          if (not SQL_SUCCEEDED(SQLGetData(Stmt, I + 1, SQL_C_TYPE_TIMESTAMP, @Timestamp, SizeOf(Timestamp), @cbData))) then
            ODBCException(Stmt, SQL_ERROR)
          else if (cbData = SQL_NULL_DATA) then
            Values[I] := 'NULL'
          else
            with (Timestamp) do
              if (ColumnDesc[I].SQLDataType = SQL_TYPE_TIME) then
                Values[I] := SQLEscape(TimeToStr(EncodeTime(hour, minute, second, 0), Client.FormatSettings))
              else if (ColumnDesc[I].SQLDataType = SQL_TYPE_TIMESTAMP) then
                Values[I] := SQLEscape(MySQLDB.DateTimeToStr(EncodeDate(year, month, day) + EncodeTime(hour, minute, second, 0), Client.FormatSettings))
              else if (ColumnDesc[I].SQLDataType = SQL_TYPE_DATE) then
                Values[I] := SQLEscape(MySQLDB.DateToStr(EncodeDate(year, month, day) + EncodeTime(hour, minute, second, 0), Client.FormatSettings));
        SQL_CHAR,
        SQL_VARCHAR,
        SQL_LONGVARCHAR,
        SQL_WCHAR,
        SQL_WVARCHAR,
        SQL_WLONGVARCHAR:
          begin
            SetLength(S, 0);
            repeat
              ReturnCode := SQLGetData(Stmt, I + 1, SQL_C_WCHAR, ODBCData, ODBCDataSize, @cbData);
              if (cbData <> SQL_NULL_DATA) then
              begin
                SetLength(S, Length(S) + cbData div SizeOf(SQLTCHAR));
                if (cbData > 0) then
                  MoveMemory(@S[1 + Length(S) - cbData div SizeOf(SQLTCHAR)], ODBCData, cbData);
              end;
            until (ReturnCode <> SQL_SUCCESS_WITH_INFO);
            if (not SQL_SUCCEEDED(ReturnCode)) then
              ODBCException(Stmt, ReturnCode)
            else if (cbData = SQL_NULL_DATA) then
              Values[I] := 'NULL'
            else
              Values[I] := Fields[I].EscapeValue(S);
          end;
        SQL_BINARY,
        SQL_VARBINARY,
        SQL_LONGVARBINARY:
          begin
            SetLength(Bytes, 0);
            repeat
              ReturnCode := SQLGetData(Stmt, I + 1, SQL_C_BINARY, ODBCData, ODBCDataSize, @cbData);
              if (cbData <> SQL_NULL_DATA) then
              begin
                SetLength(Bytes, Length(Bytes) + cbData);
                if (cbData > 0) then
                  MoveMemory(@Bytes[Length(Bytes) - cbData], ODBCData, cbData);
              end;
            until (ReturnCode <> SQL_SUCCESS_WITH_INFO);
            if (not SQL_SUCCEEDED(ReturnCode)) then
              ODBCException(Stmt, ReturnCode)
            else if (cbData = SQL_NULL_DATA) then
              Values[I] := 'NULL'
            else if (Length(Bytes) = 0) then
              Values[I] := SQLEscape(nil, 0, Client.ServerVersion <= 40000)
            else
              Values[I] := SQLEscape(@Bytes[0], Length(Bytes), Client.ServerVersion <= 40000);
          end;
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].Name, Ord(Fields[I].FieldType)]);
      end;
end;

procedure TTImportODBC.ExecuteStructure(var Item: TTImport.TItem);
var
  AscOrDesc: array [0 .. 2 - 1] of SQLTCHAR;
  AutoUniqueValue: SQLINTEGER;
  cbAscOrDesc: SQLINTEGER;
  cbColumnDef: SQLINTEGER;
  cbColumnName: SQLINTEGER;
  cbColumnSize: SQLINTEGER;
  cbDecimalDigits: SQLINTEGER;
  cbIndexName: SQLINTEGER;
  cbIndexType: SQLINTEGER;
  cbNonUnique: SQLINTEGER;
  cbNullable: SQLINTEGER;
  cbOrdinalPosition: SQLINTEGER;
  cbRemarks: SQLINTEGER;
  cbSQLDataType: SQLINTEGER;
  cbSQLDataType2: SQLINTEGER;
  ColumnDef: array [0 .. STR_LEN] of SQLTCHAR;
  ColumnName: array [0 .. STR_LEN] of SQLTCHAR;
  ColumnNumber: SQLINTEGER;
  ColumnSize: SQLINTEGER;
  DecimalDigits: SQLSMALLINT;
  Found: Boolean;
  I: Integer;
  Key: TCKey;
  IndexName: array [0 .. STR_LEN] of SQLTCHAR;
  IndexType: SQLSMALLINT;
  J: Integer;
  NewKeyColumn: TCKeyColumn;
  NewField: TCBaseTableField;
  NewTable: TCBaseTable;
  NonUnique: SQLSMALLINT;
  Nullable: SQLSMALLINT;
  OrdinalPosition: SQLSMALLINT;
  Remarks: array [0 .. 256 - 1] of SQLTCHAR;
  S: string;
  SQLDataType: SQLSMALLINT;
  SQLDataType2: SQLSMALLINT;
  Stmt: SQLHSTMT;
  Table: TCBaseTable;
  Unsigned: SQLINTEGER;
begin
  SetLength(SourceFields, 0);

  NewTable := TCBaseTable.Create(Database.Tables);
  NewTable.DefaultCharset := Charset;
  NewTable.Collation := Collation;
  NewTable.Engine := Client.EngineByName(Engine);
  NewTable.RowType := RowType;

  if (SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @Stmt))) then
  begin
    ODBCException(Stmt, SQLColumns(Stmt, nil, 0, nil, 0, PSQLTCHAR(Item.SourceTableName), SQL_NTS, nil, 0));

    ODBCException(Stmt, SQLBindCol(Stmt, 4, SQL_C_WCHAR, @ColumnName, SizeOf(ColumnName), @cbColumnName));

    ODBCException(Stmt, SQLBindCol(Stmt, 5, SQL_C_SSHORT, @SQLDataType, SizeOf(SQLDataType), @cbSQLDataType));
    ODBCException(Stmt, SQLBindCol(Stmt, 7, SQL_C_SLONG, @ColumnSize, SizeOf(ColumnSize), @cbColumnSize));
    ODBCException(Stmt, SQLBindCol(Stmt, 9, SQL_C_SSHORT, @DecimalDigits, SizeOf(DecimalDigits), @cbDecimalDigits));
    ODBCException(Stmt, SQLBindCol(Stmt, 11, SQL_C_SSHORT, @Nullable, SizeOf(Nullable), @cbNullable));
    if (not SQL_SUCCEEDED(SQLBindCol(Stmt, 12, SQL_C_WCHAR, @Remarks, SizeOf(Remarks), @cbRemarks))) then
      begin ZeroMemory(@Remarks, SizeOf(Remarks)); cbRemarks := 0; end;
    if (not SQL_SUCCEEDED(SQLBindCol(Stmt, 13, SQL_C_WCHAR, @ColumnDef, SizeOf(ColumnDef), @cbColumnDef))) then
      begin ZeroMemory(@ColumnDef, SizeOf(ColumnDef)); cbColumnDef := 0; end;
    if (not SQL_SUCCEEDED(SQLBindCol(Stmt, 14, SQL_C_SSHORT, @SQLDataType2, SizeOf(SQLDataType2), @cbSQLDataType2))) then
      begin ZeroMemory(@SQLDataType2, SizeOf(SQLDataType2)); cbSQLDataType2 := 0; end;

    while (SQL_SUCCEEDED(ODBCException(Stmt, SQLFetch(Stmt)))) do
      if (not Assigned(NewTable.FieldByName(ColumnName))) then
      begin
        SetLength(SourceFields, Length(SourceFields) + 1);
        SourceFields[Length(SourceFields) - 1].Name := ColumnName;


        NewField := TCBaseTableField.Create(NewTable.Fields);
        NewField.Name := ColumnName;
        if (NewTable.Fields.Count > 0) then
          NewField.FieldBefore := NewTable.Fields[NewTable.Fields.Count - 1];
        if (SQLDataType <> SQL_UNKNOWN_TYPE) then
          NewField.FieldType := SQLDataTypeToMySQLType(SQLDataType, ColumnSize, NewField.Name)
        else if (cbSQLDataType2 > 0) then
          NewField.FieldType := SQLDataTypeToMySQLType(SQLDataType2, ColumnSize, NewField.Name)
        else
          raise EODBCError.CreateFMT(SUnknownFieldType + ' (%d)', [ColumnName, SQLDataType]);
        if (not (NewField.FieldType in [mfFloat, mfDouble, mfDecimal]) or (DecimalDigits > 0)) then
        begin
          NewField.Size := ColumnSize;
          NewField.Decimals := DecimalDigits;
        end;
        if (cbColumnDef > 0) then
        begin
          SetString(S, ColumnDef, cbColumnDef);
          while ((Length(S) > 0) and (S[Length(S)] = #0)) do
            Delete(S, Length(S), 1);
          if (UpperCase(S) = 'NULL') then
            NewField.Default := 'NULL'
          else if ((NewField.FieldType in [mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt]) and (LowerCase(S) = '(newid())')) then
            NewField.AutoIncrement := True
          else if (LowerCase(S) = '(getdate())') then
          begin
            NewField.FieldType := mfTimestamp;
            NewField.Default := 'CURRENT_TIMESTAMP';
          end
          else if (NewField.FieldType in NotQuotedFieldTypes) then
          begin
            S := NewField.UnescapeValue(S);
            if ((LeftStr(S, 1) = '(') and (RightStr(S, 1) = ')')) then
              S := Copy(S, 2, Length(S) - 2);
            NewField.Default := S;
          end
          else if ((LeftStr(S, 1) <> '(') or (RightStr(S, 1) <> ')')) then
            NewField.Default := NewField.EscapeValue(NewField.UnescapeValue(S));
        end;
        NewField.NullAllowed := (Nullable <> SQL_NO_NULLS) or (NewField.Default = 'NULL');
        NewField.Comment := Remarks;

        NewTable.Fields.AddField(NewField);
        FreeAndNil(NewField);
      end;

    SQLFreeHandle(SQL_HANDLE_STMT, Stmt);


    if (SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @Stmt))) then
    begin
      if (SQL_SUCCEEDED(SQLExecDirect(Stmt, PSQLTCHAR(string('SELECT * FROM "' + Item.SourceTableName + '" WHERE 0<>0')), SQL_NTS))) then
      begin
        ColumnNumber := 1;
        while (SQL_SUCCEEDED(SQLColAttribute(Stmt, ColumnNumber, SQL_DESC_BASE_COLUMN_NAME, @ColumnName, SizeOf(ColumnName), @cbColumnName, nil))) do
        begin
          ODBCException(Stmt, SQLColAttribute(Stmt, ColumnNumber, SQL_DESC_AUTO_UNIQUE_VALUE, nil, 0, nil, @AutoUniqueValue));
          ODBCException(Stmt, SQLColAttribute(Stmt, ColumnNumber, SQL_DESC_UNSIGNED, nil, 0, nil, @Unsigned));
          NewField := NewTable.FieldByName(ColumnName);
          if (Assigned(NewField)) then
          begin
            NewField.AutoIncrement := AutoUniqueValue = SQL_TRUE;
            NewField.Unsigned := Unsigned = SQL_TRUE;
          end;

          Inc(ColumnNumber)
        end;
      end;
      SQLFreeHandle(SQL_HANDLE_STMT, Stmt);
    end;
  end;

  if (NewTable.Fields.Count = 0) then
    raise Exception.CreateFMT('Can''t read column definition from NewTable "%s"!', [Item.SourceTableName]);

  if (Success = daSuccess) and SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, FHandle, @Stmt)) then
  begin
    ODBCException(Stmt, SQLStatistics(Stmt, nil, 0, nil, 0, PSQLTCHAR(Item.SourceTableName), SQL_NTS, SQL_INDEX_UNIQUE, SQL_QUICK));

    ODBCException(Stmt, SQLBindCol(Stmt, 4, SQL_C_SSHORT, @NonUnique, SizeOf(NonUnique), @cbNonUnique));
    ODBCException(Stmt, SQLBindCol(Stmt, 6, SQL_C_WCHAR, @IndexName, SizeOf(IndexName), @cbIndexName));
    ODBCException(Stmt, SQLBindCol(Stmt, 7, SQL_C_SSHORT, @IndexType, SizeOf(IndexType), @cbIndexType));
    ODBCException(Stmt, SQLBindCol(Stmt, 8, SQL_C_SSHORT, @OrdinalPosition, SizeOf(OrdinalPosition), @cbOrdinalPosition));
    ODBCException(Stmt, SQLBindCol(Stmt, 9, SQL_C_WCHAR, @ColumnName, SizeOf(ColumnName), @cbColumnName));
    ODBCException(Stmt, SQLBindCol(Stmt, 10, SQL_C_WCHAR, @AscOrDesc[0], SizeOf(AscOrDesc), @cbAscOrDesc));

    while (SQL_SUCCEEDED(ODBCException(Stmt, SQLFetch(Stmt)))) do
      if ((IndexType in [SQL_INDEX_CLUSTERED, SQL_INDEX_HASHED, SQL_INDEX_OTHER])) then
      begin
        Key := NewTable.IndexByName(IndexName);

        if (not Assigned(Key)) then
        begin
          Key := TCKey.Create(NewTable.Keys);
          Key.Name := IndexName;
          Key.Unique := NonUnique = SQL_FALSE;
          NewTable.Keys.AddKey(Key);
          Key.Free();

          Key := NewTable.IndexByName(IndexName);
        end;

        NewKeyColumn := TCKeyColumn.Create(Key.Columns);
        NewKeyColumn.Field := NewTable.FieldByName(ColumnName);
        NewKeyColumn.Ascending := AscOrDesc[0] = 'A';
        Key.Columns.AddColumn(NewKeyColumn);
        FreeAndNil(NewKeyColumn);
      end;

    SQLFreeHandle(SQL_HANDLE_STMT, Stmt);


    if ((NewTable.Keys.Count > 0) and not Assigned(NewTable.IndexByName(''))) then
    begin
      Key := nil;
      for I := NewTable.Keys.Count - 1 downto 0 do
        if ((UpperCase(NewTable.Keys[I].Name) = 'PRIMARYKEY') and NewTable.Keys[0].Unique) then
          Key := NewTable.Keys[I];
      if (Assigned(Key)) then
      begin
        Key.Primary := True;
        Key.Name := '';
      end;
    end;

    if ((NewTable.Keys.Count > 0) and not NewTable.Keys[0].Primary and NewTable.Keys[0].Unique) then
    begin
      NewTable.Keys[0].Primary := True;
      NewTable.Keys[0].Name := '';
    end;

    for I := 0 to NewTable.Fields.Count -1 do
      if ((NewTable.Keys.Count = 0) and NewTable.Fields[I].AutoIncrement) then
      begin
        Key := TCKey.Create(NewTable.Keys);
        Key.Primary := True;
        NewTable.Keys.AddKey(Key);
        Key.Free();

        Key := NewTable.Keys[0];

        NewKeyColumn := TCKeyColumn.Create(Key.Columns);
        NewKeyColumn.Field := TCBaseTableField(NewTable.Fields[I]);
        Key.Columns.AddColumn(NewKeyColumn);
        FreeAndNil(NewKeyColumn);
      end;

    for I := NewTable.Keys.Count - 1 downto 1 do
      for J := I - 1 downto 0 do
        if (I <> J) then
          if (NewTable.Keys[J].Equal(NewTable.Keys[I])) then
            NewTable.Keys.DeleteKey(NewTable.Keys[J])
          else if (UpperCase(NewTable.Keys[I].Name) = UpperCase(NewTable.Keys[J].Name)) then
            NewTable.Keys[I].Name := 'Index_' + IntToStr(I);

    Found := False;
    for I := 0 to NewTable.Fields.Count - 1 do
    begin
      NewTable.Fields[I].AutoIncrement := not Found and NewTable.Fields[I].AutoIncrement and (NewTable.Keys.Count > 0) and (NewTable.Keys[0].Name = '') and (NewTable.Keys[0].Columns.KeyByField(NewTable.Fields[I]) >= 0);
      Found := Found or NewTable.Fields[I].AutoIncrement;
      if (NewTable.Fields[I].AutoIncrement) then
        NewTable.Fields[I].Default := '';
    end;

    Found := False;
    for I := 0 to NewTable.Fields.Count - 1 do
    begin
      if (Found and (NewTable.Fields[I].Default = 'CURRENT_TIMESTAMP')) then
        NewTable.Fields[I].Default := '';
      Found := Found or (NewTable.Fields[I].Default = 'CURRENT_TIMESTAMP');
    end;

    NewTable.Name := Item.TableName;

    while ((Success = daSuccess) and not Database.AddTable(NewTable)) do
      DoError(DatabaseError(Client), ToolsItem(Item));
    while ((Success = daSuccess) and not Client.Update()) do
      DoError(DatabaseError(Client), ToolsItem(Item));
  end;

  NewTable.Free();

  Table := Database.BaseTableByName(Item.TableName);
  if (not Assigned(Table)) then
    SetLength(Fields, 0)
  else
  begin
    SetLength(Fields, Table.Fields.Count);
    for I := 0 to Table.Fields.Count - 1 do
      Fields[I] := Table.Fields[I];
  end;
end;

function TTImportODBC.ODBCStmtException(const Handle: SQLHANDLE): Exception;
var
  MessageText: array [0 .. STR_LEN] of SQLTCHAR;
  NativeErrorPtr: SQLSMALLINT;
  SQLState: array [0 .. SQL_SQLSTATE_SIZE] of SQLTCHAR;
  TextLengthPtr: SQLSMALLINT;
begin
  SQLGetDiagRec(SQL_HANDLE_STMT, Handle, 1, @SQLState, @NativeErrorPtr, @MessageText, Length(MessageText), @TextLengthPtr);
  Result := Exception.Create(string(MessageText) + ' (' + SQLState + ')');
end;

{ TTImportSQLite *****************************************************************}

procedure TTImportSQLite.Add(const TableName: string; const SheetName: string);
begin
  SetLength(Items, Length(Items) + 1);

  Items[Length(Items) - 1].TableName := TableName;
  Items[Length(Items) - 1].RecordsDone := 0;
  Items[Length(Items) - 1].RecordsSum := 0;
  Items[Length(Items) - 1].Done := False;
  Items[Length(Items) - 1].SourceTableName := SheetName;
end;

procedure TTImportSQLite.AfterExecuteData(var Item: TTImport.TItem);
begin
  sqlite3_finalize(@Stmt); Stmt := nil;
end;

procedure TTImportSQLite.BeforeExecute();
var
  I: Integer;
begin
  inherited;

  if ((Success = daSuccess) and Data) then
    for I := 0 to Length(Items) - 1 do
    begin
      SQLiteException(Handle, sqlite3_prepare_v2(Handle, PAnsiChar(UTF8Encode('SELECT COUNT(*) FROM "' + Items[I].SourceTableName + '";')), -1, @Stmt, nil));
      if (sqlite3_step(Stmt) = SQLITE_ROW) then
        Items[I].RecordsSum := sqlite3_column_int(Stmt, 0);
      sqlite3_finalize(Stmt); Stmt := nil;
    end;
end;

procedure TTImportSQLite.BeforeExecuteData(var Item: TTImport.TItem);
var
  I: Integer;
  SQL: string;
begin
  inherited;

  SQL := '';
  if (not Structure and (Length(Items) = 1)) then
    for I := 0 to Length(SourceFields) - 1 do
    begin
      if (I > 0) then SQL := SQL + ',';
      SQL := SQL + '"' + SourceFields[I].Name + '"';
    end
  else
    SQL := '*';
  SQL := 'SELECT ' + SQL + ' FROM "' + Item.SourceTableName + '";';

  SQLiteException(Handle, sqlite3_prepare_v2(Handle, PAnsiChar(UTF8Encode(SQL)), -1, @Stmt, nil));
end;

constructor TTImportSQLite.Create(const AHandle: sqlite3_ptr; const ADatabase: TCDatabase);
begin
  inherited Create(ADatabase.Client, ADatabase);

  ImportType := itInsert;

  Handle := AHandle;
end;

function TTImportSQLite.GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean;
var
  I: Integer;
  RBS: RawByteString;
begin
  Result := sqlite3_step(Stmt) = SQLITE_ROW;
  if (Result) then
  begin
    Values := '';
    for I := 0 to Length(Fields) - 1 do
    begin
      if (I > 0) then Values := Values + ',';

      if (sqlite3_column_type(Stmt, I) = SQLITE_NULL) then
        Values := Values + 'NULL'
      else if ((Fields[I].FieldType in NotQuotedFieldTypes) and (sqlite3_column_type(Stmt, I) in [SQLITE_INTEGER, SQLITE_FLOAT])) then
      begin
        SetLength(Values, Length(Values) + sqlite3_column_bytes(Stmt, I));
        MoveMemory(@Values[1 + Length(Values) - sqlite3_column_bytes(Stmt, I)], sqlite3_column_text(Stmt, I), sqlite3_column_bytes(Stmt, I));
      end
      else if ((Client.CodePage = CP_UTF8) or (Fields[I].FieldType in BinaryFieldTypes) and (sqlite3_column_type(Stmt, I) in [SQLITE_BLOB])) then
        Values := Values + DataFileEscape(sqlite3_column_blob(Stmt, I), sqlite3_column_bytes(Stmt, I))
      else
      begin
        SetString(RBS, sqlite3_column_text(Stmt, I), sqlite3_column_bytes(Stmt, I));
        Values := Values + DataFileEscape(Client.LibEncode(UTF8ToString(RBS)));
      end;
    end;
    Values := Values + #13#10;
  end;
end;

function TTImportSQLite.GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean;
var
  I: Integer;
  RBS: RawByteString;
begin
  Result := sqlite3_step(Stmt) = SQLITE_ROW;
  if (Result) then
    for I := 0 to Length(Fields) - 1 do
      case (sqlite3_column_type(Stmt, I)) of
        SQLITE_NULL:
          if (Fields[I].NullAllowed) then
            Values[I] := 'NULL'
          else
            Values[I] := Fields[I].EscapeValue('');
        SQLITE_INTEGER:
          Values[I] := Fields[I].EscapeValue(IntToStr(sqlite3_column_int64(Stmt, I)));
        SQLITE_FLOAT:
          Values[I] := Fields[I].EscapeValue(FloatToStr(sqlite3_column_double(Stmt, I), Client.FormatSettings));
        SQLITE3_TEXT:
          begin
            SetString(RBS, sqlite3_column_text(Stmt, I), sqlite3_column_bytes(Stmt, I));
            Values[I] := SQLEscape(UTF8ToString(RBS));
          end;
        SQLITE_BLOB:
          Values[I] := SQLEscape(sqlite3_column_blob(Stmt, I), sqlite3_column_bytes(Stmt, I), Client.ServerVersion <= 40000);
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [SourceFields[I].Name, Integer(sqlite3_column_type(Stmt, I))]);
      end;
end;

procedure TTImportSQLite.ExecuteStructure(var Item: TTImport.TItem);
var
  I: Integer;
  Name: string;
  NewField: TCBaseTableField;
  NewKey: TCKey;
  NewKeyColumn: TCKeyColumn;
  NewTable: TCBaseTable;
  Parse: TSQLParse;
  ParseSQL: string;
  Primary: Boolean;
  RBS: RawByteString;
  SQL: string;
  Stmt: sqlite3_stmt_ptr;
  Table: TCBaseTable;
  Unique: Boolean;
begin
  SetLength(SourceFields, 0);


  NewTable := nil;

  SQLiteException(Handle, sqlite3_prepare_v2(Handle, PAnsiChar(UTF8Encode('SELECT "sql" FROM "sqlite_master" WHERE type=''table'' AND name="' + Item.TableName + '";')), -1, @Stmt, nil));
  ParseSQL := UTF8ToString(sqlite3_column_text(Stmt, 0));
  if ((sqlite3_step(Stmt) = SQLITE_ROW) and (SQLCreateParse(Parse, PChar(ParseSQL), Length(ParseSQL), 0))) then
  begin
    SQL := UTF8ToString(sqlite3_column_text(Stmt, 0));

    NewTable := TCBaseTable.Create(Database.Tables, Item.TableName);
    NewTable.DefaultCharset := Charset;
    NewTable.Collation := Collation;
    NewTable.Engine := Client.EngineByName(Engine);
    NewTable.RowType := RowType;

    if (not SQLParseKeyword(Parse, 'CREATE TABLE')) then raise EConvertError.CreateFmt(SSourceParseError, [Item.TableName, 1, SQL]);

    NewTable.Name := Database.Tables.ApplyMySQLTableName(SQLParseValue(Parse));

    if (not SQLParseChar(Parse, '(')) then raise EConvertError.CreateFmt(SSourceParseError, [Item.TableName, 2, SQL]);

    repeat
      Name := SQLParseValue(Parse);
      Primary := False;

      SetLength(SourceFields, Length(SourceFields) + 1);
      SourceFields[Length(SourceFields) - 1].Name := Name;


      NewField := TCBaseTableField.Create(NewTable.Fields);
      NewField.Name := Name;
      if (SQLParseKeyword(Parse, 'INTEGER PRIMARY KEY')) then
      begin
        Primary := True;
        NewField.FieldType := mfBigInt;
        NewField.Unsigned := False;
        NewField.AutoIncrement := SQLParseKeyword(Parse, 'AUTOINCREMENT');
      end
      else if (SQLParseKeyword(Parse, 'INTEGER')) then
        NewField.FieldType := mfBigInt
      else if (SQLParseKeyword(Parse, 'REAL')) then
        NewField.FieldType := mfDouble
      else if (SQLParseKeyword(Parse, 'TEXT')) then
        NewField.FieldType := mfLongText
      else if (SQLParseKeyword(Parse, 'BLOB')) then
        NewField.FieldType := mfLongBlob
      else
        raise EConvertError.CreateFmt(SSourceParseError, [Item.TableName, 3, SQL]);

      if (SQLParseChar(Parse, '(')) then
      begin
        if (TryStrToInt(SQLParseValue(Parse), I)) then
          NewField.Size := I;
        if (SQLParseChar(Parse, ',')) then
          if (TryStrToInt(SQLParseValue(Parse), I)) then
            NewField.Decimals := I;
        SQLParseChar(Parse, ')');
      end;

      // Ignore all further field properties like keys and foreign keys
      while (not SQLParseChar(Parse, ',', False) and not SQLParseChar(Parse, ')', False) and (SQLParseGetIndex(Parse) <= Length(SQL))) do
        SQLParseValue(Parse);

      if (NewTable.Fields.Count > 0) then
        NewField.FieldBefore := NewTable.Fields[NewTable.Fields.Count - 1];
      NewTable.Fields.AddField(NewField);
      NewField.Free();

      if (Primary) then
      begin
        NewKey := TCKey.Create(NewTable.Keys);
        NewKey.Primary := True;
        NewKeyColumn := TCKeyColumn.Create(NewKey.Columns);
        NewKeyColumn.Field := TCBaseTableField(NewTable.Fields[NewTable.Fields.Count - 1]);
        NewKeyColumn.Ascending := True;
        NewKey.Columns.AddColumn(NewKeyColumn);
        NewKeyColumn.Free();
        NewTable.Keys.AddKey(NewKey);
        NewKey.Free();
      end;
    until (not SQLParseChar(Parse, ',') and SQLParseChar(Parse, ')'));
  end;
  SQLiteException(Handle, sqlite3_finalize(Stmt));

  if (Assigned(NewTable)) then
  begin
    RBS := UTF8Encode('SELECT "sql" FROM "sqlite_master" WHERE type=''index'';');
    SQLiteException(Handle, sqlite3_prepare_v2(Handle, PAnsiChar(RBS), -1, @Stmt, nil));
    while (sqlite3_step(Stmt) = SQLITE_ROW) do
    begin
      SQL := UTF8ToString(sqlite3_column_text(Stmt, 0));

      if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Item.TableName, 3, SQL]);

      Unique := SQLParseKeyword(Parse, 'UNIQUE');

      if (not SQLParseKeyword(Parse, 'INDEX')) then raise EConvertError.CreateFmt(SSourceParseError, [Item.TableName, 4, SQL]);

      Name := SQLParseValue(Parse);

      if (not SQLParseKeyword(Parse, 'ON')) then raise EConvertError.CreateFmt(SSourceParseError, [Item.TableName, 5, SQL]);

      if (SQLParseValue(Parse) = NewTable.Name) then
      begin
        NewKey := TCKey.Create(NewTable.Keys);
        NewKey.Name := Name;
        NewKey.Unique := Unique;

        NewKeyColumn := TCKeyColumn.Create(NewKey.Columns);
        NewKeyColumn.Field := NewTable.FieldByName(SQLParseValue(Parse));
        if (SQLParseKeyword(Parse, 'COLLATE')) then
          SQLParseValue(Parse);
        NewKeyColumn.Ascending := SQLParseKeyword(Parse, 'ASC') or not SQLParseKeyword(Parse, 'DESC');
        NewKey.Columns.AddColumn(NewKeyColumn);
        NewKeyColumn.Free();

        NewTable.Keys.AddKey(NewKey);
        NewKey.Free();
      end;
    end;
    SQLiteException(Handle, sqlite3_finalize(Stmt));

    while ((Success = daSuccess) and not Database.AddTable(NewTable)) do
      DoError(DatabaseError(Client), ToolsItem(Item));
    while ((Success = daSuccess) and not Client.Update()) do
      DoError(DatabaseError(Client), ToolsItem(Item));

    NewTable.Free();
  end;

  if (Success = daSuccess) then
  begin
    Table := Database.BaseTableByName(Database.Tables.ApplyMySQLTableName(Item.TableName));
    if (Assigned(Table)) then
    begin
      SetLength(Fields, Table.Fields.Count);
      for I := 0 to Table.Fields.Count - 1 do
        Fields[I] := Table.Fields[I];
    end;
  end;
end;

{ TTImportXML *****************************************************************}

procedure TTImportXML.Add(const TableName: string);
begin
  SetLength(Items, Length(Items) + 1);

  Items[Length(Items) - 1].TableName := TableName;
  Items[Length(Items) - 1].RecordsDone := 0;
  Items[Length(Items) - 1].RecordsSum := 0;
  Items[Length(Items) - 1].Done := False;
end;

procedure TTImportXML.BeforeExecute();
var
  Error: TTools.TError;
begin
  inherited;

  XMLNode := XMLDocument.documentElement.selectSingleNode('//*/' + RecordTag);

  if (not Assigned(XMLNode)) then
  begin
    Error.ErrorType := TE_XML;
    Error.ErrorCode := 0;
    Error.ErrorMessage := 'Node not found.';
    DoError(Error, EmptyToolsItem());
  end;
end;

procedure TTImportXML.BeforeExecuteData(var Item: TTImport.TItem);
begin
  inherited;

  if (Assigned(XMLNode)) then
    Item.RecordsSum := XMLNode.parentNode.childNodes.length;
end;

constructor TTImportXML.Create(const AFilename: TFileName; const ATable: TCBaseTable);
begin
  inherited Create(ATable.Database.Client, ATable.Database);

  Add(ATable.Name);

  Data := True;
  RecordTag := 'row';

  CoInitialize(nil);

  XMLDocument := CoDOMDocument30.Create();
  if (not XMLDocument.load(AFilename)) then
    CoUninitialize();
end;

destructor TTImportXML.Destroy();
begin
  CoUninitialize();

  inherited;
end;

function TTImportXML.GetValues(const Item: TTImport.TItem; out Values: RawByteString): Boolean;
var
  I: Integer;
  J: Integer;
  XMLValueNode: IXMLDOMNode;
begin
  Result := Assigned(XMLNode);
  if (Result) then
  begin
    Values := '';
    for I := 0 to Length(Fields) - 1 do
    begin
      XMLValueNode := XMLNode.selectSingleNode('@' + LowerCase(SourceFields[I].Name));
      if (not Assigned(XMLValueNode)) then
      begin
        XMLValueNode := XMLNode.selectSingleNode(LowerCase(SourceFields[I].Name));
        for J := 0 to XMLNode.childNodes.length - 1 do
          if (not Assigned(XMLValueNode) and (XMLNode.childNodes[J].nodeName = 'field') and Assigned(XMLNode.childNodes[J].selectSingleNode('@name')) and (lstrcmpI(PChar(XMLNode.childNodes[J].selectSingleNode('@name').text), PChar(SourceFields[I].Name)) = 0)) then
            XMLValueNode := XMLNode.childNodes[J];
      end;

      if (I > 0) then Values := Values + ',';
      if (not Assigned(XMLValueNode) or (XMLValueNode.text = '') and Assigned(XMLValueNode.selectSingleNode('@xsi:nil')) and (XMLValueNode.selectSingleNode('@xsi:nil').text = 'true')) then
        Values := Values + 'NULL'
      else if (Fields[I].FieldType in NotQuotedFieldTypes + [mfDate, mfDateTime, mfTime, mfTimeStamp]) then
        Values := Values + DataFileValue(XMLValueNode.text, not (Fields[I].FieldType in NotQuotedFieldTypes))
      else if (Fields[I].FieldType in BinaryFieldTypes) then
        Values := Values + DataFileValue(XMLValueNode.text, True)
      else
        Values := Values + DataFileEscape(Client.LibEncode(XMLValueNode.text));
    end;
    Values := Values + #13#10;

    repeat
      XMLNode := XMLNode.nextSibling;
    until (not Assigned(XMLNode) or (XMLNode.nodeName = RecordTag));
  end;
end;

function TTImportXML.GetValues(const Item: TTImport.TItem; var Values: TSQLStrings): Boolean;
var
  I: Integer;
  J: Integer;
  XMLValueNode: IXMLDOMNode;
begin
  Result := Assigned(XMLNode);
  if (Result) then
  begin
    for I := 0 to Length(Fields) - 1 do
    begin
      XMLValueNode := XMLNode.selectSingleNode('@' + LowerCase(SourceFields[I].Name));
      if (not Assigned(XMLValueNode)) then
      begin
        XMLValueNode := XMLNode.selectSingleNode(LowerCase(SourceFields[I].Name));
        for J := 0 to XMLNode.childNodes.length - 1 do
          if (not Assigned(XMLValueNode) and (XMLNode.childNodes[J].nodeName = 'field') and Assigned(XMLNode.childNodes[J].selectSingleNode('@name')) and (XMLNode.childNodes[J].selectSingleNode('@name').text = SourceFields[I].Name)) then
            XMLValueNode := XMLNode.childNodes[J];
      end;

      if (not Assigned(XMLValueNode) or (XMLValueNode.text = '') and Assigned(XMLValueNode.selectSingleNode('@xsi:nil')) and (XMLValueNode.selectSingleNode('@xsi:nil').text = 'true')) then
        Values[I] := 'NULL'
      else
        Values[I] := Fields[I].EscapeValue(XMLValueNode.text);
    end;

    repeat
      XMLNode := XMLNode.nextSibling;
    until (not Assigned(XMLNode) or (XMLNode.nodeName = RecordTag));
  end;
end;

{ TTExport ********************************************************************}

procedure TTExport.Add(const ADBGrid: TDBGrid);
begin
  SetLength(FDBGrids, Length(DBGrids) + 1);

  DBGrids[Length(DBGrids) - 1].DBGrid := ADBGrid;
  DBGrids[Length(DBGrids) - 1].RecordsDone := 0;
  DBGrids[Length(DBGrids) - 1].RecordsSum := 0;
end;

procedure TTExport.Add(const ADBObject: TCDBObject);
begin
  SetLength(ExportObjects, Length(ExportObjects) + 1);
  ExportObjects[Length(ExportObjects) - 1].DBObject := ADBObject;
end;

procedure TTExport.AfterExecute();
var
  I: Integer;
begin
  for I := 0 to Length(DBGrids) - 1 do
    DBGrids[I].DBGrid.DataSource.DataSet.EnableControls();

  FClient.EndSilent();

  inherited;
end;

procedure TTExport.BeforeExecute();
var
  I: Integer;
begin
  inherited;

  FClient.BeginSilent();

  for I := 0 to Length(DBGrids) - 1 do
    DBGrids[I].DBGrid.DataSource.DataSet.DisableControls();
end;

constructor TTExport.Create(const AClient: TCClient);
begin
  inherited Create();

  FClient := AClient;

  Data := False;
  SetLength(FDBGrids, 0);
  SetLength(ExportObjects, 0);
  Structure := False;
end;

destructor TTExport.Destroy();
begin
  SetLength(FDBGrids, 0);
  SetLength(ExportObjects, 0);

  inherited;
end;

procedure TTExport.DoUpdateGUI();
var
  I: Integer;
begin
  if (Assigned(OnUpdate)) then
  begin
    CriticalSection.Enter();

    ProgressInfos.TablesDone := 0;
    ProgressInfos.TablesSum := Length(DBGrids) + Length(ExportObjects);
    ProgressInfos.RecordsDone := 0;
    ProgressInfos.RecordsSum := 0;
    ProgressInfos.TimeDone := 0;
    ProgressInfos.TimeSum := 0;

    for I := 0 to Length(DBGrids) - 1 do
    begin
      if (DBGrids[I].Done) then
        Inc(ProgressInfos.TablesDone);

      Inc(ProgressInfos.RecordsDone, DBGrids[I].RecordsDone);
      Inc(ProgressInfos.RecordsSum, DBGrids[I].RecordsSum);
    end;

    for I := 0 to Length(ExportObjects) - 1 do
    begin
      if (ExportObjects[I].Done) then
        Inc(ProgressInfos.TablesDone);

      Inc(ProgressInfos.RecordsDone, ExportObjects[I].RecordsDone);
      Inc(ProgressInfos.RecordsSum, ExportObjects[I].RecordsSum);
    end;

    ProgressInfos.TimeDone := Now() - StartTime;

    if ((ProgressInfos.RecordsDone = 0) and (ProgressInfos.TablesDone = 0)) then
    begin
      ProgressInfos.Progress := 0;
      ProgressInfos.TimeSum := 0;
    end
    else if (ProgressInfos.RecordsDone = 0) then
    begin
      ProgressInfos.Progress := Round(ProgressInfos.TablesDone / ProgressInfos.TablesSum * 100);
      ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.TablesDone * ProgressInfos.TablesSum;
    end
    else if (ProgressInfos.RecordsDone < ProgressInfos.RecordsSum) then
    begin
      ProgressInfos.Progress := Round(ProgressInfos.RecordsDone / ProgressInfos.RecordsSum * 100);
      ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.RecordsDone * ProgressInfos.RecordsSum;
    end
    else
    begin
      ProgressInfos.Progress := 100;
      ProgressInfos.TimeSum := ProgressInfos.TimeDone;
    end;

    CriticalSection.Leave();

    OnUpdate(ProgressInfos);
  end;
end;

function TTExport.EmptyToolsItem(): TTools.TItem;
begin
  Result.Client := Client;
  Result.DatabaseName := '';
  Result.TableName := '';
end;

procedure TTExport.Execute();
var
  ResultHandle: TMySQLConnection.TResultHandle;
  I: Integer;
  Index: Integer;
  J: Integer;
  S: string;
  SQL: string;
  Table: TCTable;
begin
  if (not Data) then
    DataTables := nil
  else
    DataTables := TList.Create();

  BeforeExecute();

  for I := 0 to Length(DBGrids) - 1 do
  begin
    DBGrids[I].Done := False;

    DBGrids[I].RecordsSum := DBGrids[I].DBGrid.DataSource.DataSet.RecordCount;
    DBGrids[I].RecordsDone := 0;
  end;

  SQL := '';
  for I := 0 to Length(ExportObjects) - 1 do
  begin
    if (not (ExportObjects[I].DBObject is TCBaseTable)) then
      ExportObjects[I].RecordsSum := 0
    else
      ExportObjects[I].RecordsSum := TCBaseTable(ExportObjects[I].DBObject).Rows;
    ExportObjects[I].RecordsDone := 0;
    ExportObjects[I].Done := False;

    if (Data and (ExportObjects[I].DBObject is TCTable) and (not (ExportObjects[I].DBObject is TCBaseTable) or not TCBaseTable(ExportObjects[I].DBObject).Engine.IsMerge)) then
      DataTables.Add(ExportObjects[I].DBObject);
  end;

  if ((Success <> daAbort) and Assigned(DataTables) and (DataTables.Count > 0)) then
  begin
    Success := daSuccess;

    for I := 0 to DataTables.Count - 1 do
    begin
      Table := TCTable(DataTables[I]);

      if (Length(TableFields) = 0) then
        S := '*'
      else
      begin
        S := '';
        for J := 0 to Length(TableFields) - 1 do
        begin
          if (S <> '') then S := S + ',';
          S := S + Client.EscapeIdentifier(TableFields[J].Name);
        end;
      end;

      SQL := SQL + 'SELECT ' + S + ' FROM ' + Client.EscapeIdentifier(Table.Database.Name) + '.' + Client.EscapeIdentifier(Table.Name);
      if ((Table is TCBaseTable) and Assigned(TCBaseTable(Table).PrimaryKey)) then
      begin
        SQL := SQL + ' ORDER BY ';
        for J := 0 to TCBaseTable(Table).PrimaryKey.Columns.Count - 1 do
        begin
          if (J > 0) then SQL := SQL + ',';
          SQL := SQL + Client.EscapeIdentifier(TCBaseTable(Table).PrimaryKey.Columns[J].Field.Name);
        end;
      end;
      SQL := SQL + ';' + #13#10;
    end;
  end;

  if (Success <> daAbort) then
  begin
    Success := daSuccess;
    ExecuteHeader();

    if (Success = daSuccess) then
      for I := 0 to Length(DBGrids) - 1 do
        if (Success <> daAbort) then
        begin
          Success := daSuccess;

          ExecuteDBGrid(DBGrids[I]);

          DBGrids[I].Done := True;

          if (Success = daFail) then Success := daSuccess;
        end;

    if (Success = daSuccess) then
      for I := 0 to Length(ExportObjects) - 1 do
      begin
        if ((Success <> daAbort) and Data) then
        begin
          Index := DataTables.IndexOf(ExportObjects[I].DBObject);
          if (Index >= 0) then
            if (Index = 0) then
              while ((Success = daSuccess) and not Client.FirstResult(ResultHandle, SQL)) do
                DoError(DatabaseError(Client), EmptyToolsItem(), SQL)
            else
              if ((Success = daSuccess) and not Client.NextResult(ResultHandle)) then
                DoError(DatabaseError(Client), EmptyToolsItem());
        end;

        if ((Success <> daAbort) and ((I = 0) or (ExportObjects[I - 1].DBObject.Database <> ExportObjects[I].DBObject.Database))) then
        begin
          Success := daSuccess;
          ExecuteDatabaseHeader(ExportObjects[I].DBObject.Database);
        end;

        if (Success <> daAbort) then
        begin
          Success := daSuccess;

          if (ExportObjects[I].DBObject is TCTable) then
            ExecuteTable(ExportObjects[I], ResultHandle)
          else if (ExportObjects[I].DBObject is TCRoutine) then
            ExecuteRoutine(TCRoutine(ExportObjects[I].DBObject))
          else if (ExportObjects[I].DBObject is TCEvent) then
            ExecuteEvent(TCEvent(ExportObjects[I].DBObject))
          else if (ExportObjects[I].DBObject is TCTrigger) then
            ExecuteTrigger(TCTrigger(ExportObjects[I].DBObject));

          if (Success = daFail) then Success := daSuccess;
        end;

        ExportObjects[I].Done := True;

        if (((I = Length(ExportObjects) - 1) or (ExportObjects[I + 1].DBObject.Database <> ExportObjects[I].DBObject.Database))) then
        begin
          if (Success <> daAbort) then
            Success := daSuccess;
          ExecuteDatabaseFooter(ExportObjects[I].DBObject.Database);
        end;
      end;

    if (Success <> daAbort) then
      Success := daSuccess;
    ExecuteFooter();
  end;

  AfterExecute();

  if (Data) then
  begin
    Client.CloseResult(ResultHandle);
    DataTables.Free();
  end;
end;

procedure TTExport.ExecuteDatabaseFooter(const Database: TCDatabase);
begin
end;

procedure TTExport.ExecuteDatabaseHeader(const Database: TCDatabase);
begin
end;

procedure TTExport.ExecuteDBGrid(var ExportDBGrid: TExportDBGrid);
var
  Database: TCDatabase;
  DataSet: TMySQLDataSet;
  Index: Integer;
  OldBookmark: TBookmark;
  OldLoadNextRecords: Boolean;
  Table: TCTable;
begin
  DataSet := TMySQLDataSet(ExportDBGrid.DBGrid.DataSource.DataSet);
  if (ExportDBGrid.DBGrid.DataSource.DataSet is TMySQLTable) then
  begin
    Database := Client.DatabaseByName(TMySQLTable(DataSet).DatabaseName);
    Table := Database.BaseTableByName(TMySQLTable(DataSet).TableName);
  end
  else
  begin
    Database := Client.DatabaseByName(DataSet.DatabaseName);
    if (not Assigned(Database)) then
      Table := nil
    else
      Table := Database.TableByName(DataSet.TableName);
  end;

  if (not (DataSet is TMySQLTable)) then
    OldLoadNextRecords := False // hide compiler warning
  else
  begin
    OldLoadNextRecords := TMySQLTable(DataSet).AutomaticLoadNextRecords;
    TMySQLTable(DataSet).AutomaticLoadNextRecords := False;
  end;
  OldBookmark := DataSet.Bookmark;

  if (DataSet.FindFirst()) then
  begin
    if (Success <> daAbort) then
    begin
      Success := daSuccess;
      ExecuteDatabaseHeader(Database);
      ExecuteTableHeader(Table, Fields, DataSet);
    end;

    Index := 0;
    if (Success <> daAbort) then
      repeat
        if (ExportDBGrid.DBGrid.SelectedRows.Count > Index) then
        begin
          DataSet.Bookmark := ExportDBGrid.DBGrid.SelectedRows[Index];
          Inc(Index);
        end;

        ExecuteTableRecord(Table, Fields, DataSet);

        Inc(ExportDBGrid.RecordsDone);
        if (ExportDBGrid.RecordsDone mod 100 = 0) then DoUpdateGUI();

        if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
          Success := daAbort;
      until ((Success <> daSuccess) or ((ExportDBGrid.DBGrid.SelectedRows.Count < 1) and not DataSet.FindNext()) or (ExportDBGrid.DBGrid.SelectedRows.Count >= 1) and (Index = ExportDBGrid.DBGrid.SelectedRows.Count));

    if (Success <> daAbort) then
    begin
      Success := daSuccess;
      ExecuteTableFooter(Table, Fields, DataSet);
      ExecuteDatabaseFooter(Database);
    end;
  end;

  DataSet.Bookmark := OldBookmark;
  if (DataSet is TMySQLTable) then
    TMySQLTable(DataSet).AutomaticLoadNextRecords := OldLoadNextRecords;

  if (Success = daSuccess) then
    ExportDBGrid.RecordsSum := ExportDBGrid.RecordsDone;
end;

procedure TTExport.ExecuteEvent(const Event: TCEvent);
begin
end;

procedure TTExport.ExecuteFooter();
begin
end;

procedure TTExport.ExecuteHeader();
begin
end;

procedure TTExport.ExecuteRoutine(const Routine: TCRoutine);
begin
end;

procedure TTExport.ExecuteTable(var ExportObject: TExportObject; const ResultHandle: TMySQLConnection.TResultHandle);
var
  DataSet: TMySQLQuery;
  Fields: array of TField;
  I: Integer;
  SQL: string;
  Table: TCTable;
begin
  Table := TCTable(ExportObject.DBObject);

  if (not Data or (Table is TCBaseTable) and TCBaseTable(Table).Engine.IsMerge) then
    DataSet := nil
  else
  begin
    DataSet := TMySQLQuery.Create(nil);
    DataSet.Connection := Client;
    while ((Success = daSuccess) and not DataSet.Active) do
    begin
      DataSet.Open(ResultHandle);
      if (not DataSet.Active) then
        DoError(DatabaseError(Client), ToolsItem(ExportObject), SQL);
    end;
  end;

  if (Success <> daSuccess) then
    SetLength(Fields, 0)
  else
  begin
    SetLength(Fields, DataSet.FieldCount);
    for I := 0 to DataSet.FieldCount - 1 do
      Fields[I] := DataSet.Fields[I];
  end;

  if (Success <> daAbort) then
  begin
    Success := daSuccess;

    ExecuteTableHeader(Table, Fields, DataSet);

    if ((Success <> daAbort) and Data and ((Table is TCBaseTable) or (Table is TCView) and (Length(ExportObjects) = 1)) and Assigned(DataSet) and not DataSet.IsEmpty()) then
      repeat
        ExecuteTableRecord(Table, Fields, DataSet);

        Inc(ExportObject.RecordsDone);
        if ((ExportObject.RecordsDone mod 1000 = 0)
          or ((ExportObject.RecordsDone mod 100 = 0) and ((Self is TTExportSQLite) or (Self is TTExportODBC)))) then
          DoUpdateGUI();

        if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
          Success := daAbort;
      until ((Success <> daSuccess) or not DataSet.FindNext());

    if (Success <> daAbort) then
      Success := daSuccess;
    ExecuteTableFooter(Table, Fields, DataSet);
  end;

  if (Success = daSuccess) then
    ExportObject.RecordsSum := ExportObject.RecordsDone;

  if ((Success <> daAbort) and Assigned(DataSet)) then
    DataSet.Free();
end;

procedure TTExport.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
end;

procedure TTExport.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
end;

procedure TTExport.ExecuteTrigger(const Trigger: TCTrigger);
begin
end;

function TTExport.ToolsItem(const ExportDBGrid: TExportDBGrid): TTools.TItem;
begin
  Result.Client := Client;
  Result.DatabaseName := '';
  Result.TableName := '';
end;

function TTExport.ToolsItem(const ExportObject: TExportObject): TTools.TItem;
begin
  Result.Client := Client;
  Result.DatabaseName := ExportObject.DBObject.Database.Name;
  Result.TableName := ExportObject.DBObject.Name;
end;

{ TTExportFile ****************************************************************}

procedure TTExportFile.CloseFile();
begin
  if (Handle <> INVALID_HANDLE_VALUE) then
  begin
    Flush();

    CloseHandle(Handle);
    Handle := INVALID_HANDLE_VALUE;
  end;
end;

constructor TTExportFile.Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal);
begin
  inherited Create(AClient);

  ContentBuffer := TTStringBuffer.Create(FilePacketSize);
  FCodePage := ACodePage;
  if (CodePage = CP_UNICODE) then
  begin
    FileBuffer.Size := 0;
    FileBuffer.Mem := nil;
  end
  else
  begin
    FileBuffer.Size := FilePacketSize;
    GetMem(FileBuffer.Mem, FileBuffer.Size);
  end;
  FFilename := AFilename;
  Handle := INVALID_HANDLE_VALUE;
end;

destructor TTExportFile.Destroy();
begin
  CloseFile();
  if (Assigned(FileBuffer.Mem)) then
    FreeMem(FileBuffer.Mem);
  ContentBuffer.Free();

  inherited;

  if (Success = daAbort) then
    DeleteFile(FFilename);
end;

procedure TTExportFile.DoFileCreate(const Filename: TFileName);
var
  Error: TTools.TError;
begin
  while ((Success = daSuccess) and not FileCreate(Filename, Error)) do
    DoError(Error, EmptyToolsItem());
end;

function TTExportFile.FileCreate(const Filename: TFileName; out Error: TTools.TError): Boolean;
begin
  Handle := CreateFile(PChar(Filename),
                       GENERIC_WRITE,
                       FILE_SHARE_READ,
                       nil,
                       CREATE_ALWAYS, 0, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;

  if (not Result) then
  begin
    Error.ErrorType := TE_File;
    Error.ErrorCode := GetLastError();
    Error.ErrorMessage := SysErrorMessage(GetLastError());
  end;
end;

procedure TTExportFile.Flush();
var
  Buffer: PAnsiChar;
  BytesToWrite: DWord;
  BytesWritten: DWord;
  Error: TTools.TError;
  Size: DWord;
begin
  case (CodePage) of
    CP_UNICODE:
      begin
        Buffer := Pointer(ContentBuffer.Buffer);
        BytesToWrite := ContentBuffer.Size * SizeOf(Char);
      end;
    else
      begin
        BytesToWrite := WideCharToMultiByte(CodePage, 0, ContentBuffer.Buffer, ContentBuffer.Size, nil, 0, nil, nil);
        if (BytesToWrite > FileBuffer.Size) then
        begin
          FileBuffer.Size := BytesToWrite;
          ReallocMem(FileBuffer.Mem, FileBuffer.Size);
        end;
        Buffer := FileBuffer.Mem;
        if (BytesToWrite > 0) then
          WideCharToMultiByte(CodePage, 0, ContentBuffer.Buffer, ContentBuffer.Size, Buffer, BytesToWrite, nil, nil);
      end;
  end;

  BytesWritten := 0;
  while ((Success = daSuccess) and (BytesWritten < BytesToWrite)) do
    if (not WriteFile(Handle, Buffer[BytesWritten], BytesToWrite - BytesWritten, Size, nil)) then
    begin
      Error.ErrorType := TE_File;
      Error.ErrorCode := GetLastError();
      Error.ErrorMessage := SysErrorMessage(GetLastError());
      DoError(Error, EmptyToolsItem());
    end
    else
      Inc(BytesWritten, Size);

  ContentBuffer.Clear();
end;

procedure TTExportFile.WriteContent(const Content: string);
begin
  if (Content <> '') then
  begin
    if ((ContentBuffer.Size > 0) and (ContentBuffer.Size + Length(Content) > FilePacketSize)) then
      Flush();

    ContentBuffer.Write(Content);
  end;
end;

{ TTExportSQL *****************************************************************}

constructor TTExportSQL.Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal);
begin
  inherited;

  CreateDatabaseStmts := False;
  DisableKeys := False;
  ForeignKeySources := '';
  IncludeDropStmts := False;
  UseDatabaseStmts := True;
end;

procedure TTExportSQL.ExecuteDatabaseFooter(const Database: TCDatabase);
begin
  if (ForeignKeySources <> '') then
    WriteContent(ForeignKeySources + #13#10);
end;

procedure TTExportSQL.ExecuteDatabaseHeader(const Database: TCDatabase);
var
  Content: string;
begin
  if (Assigned(Database)) then
  begin
    Content := '';

    if (UseDatabaseStmts or CreateDatabaseStmts) then
    begin
      Content := Content + #13#10;

      if (CreateDatabaseStmts) then
        Content := Content + Database.GetSourceEx(IncludeDropStmts) + #13#10;

      Content := Content + Database.SQLUse();
    end;

    WriteContent(Content);
  end;
end;

procedure TTExportSQL.ExecuteEvent(const Event: TCEvent);
var
  Content: string;
begin
  Content := #13#10;
  Content := Content + '#' + #13#10;
  Content := Content + '# Source for event "' + Event.Name + '"' + #13#10;
  Content := Content + '#' + #13#10;
  Content := Content + #13#10;
  Content := Content + ReplaceStr(Event.Source, Client.EscapeIdentifier(Event.Database.Name) + '.', '') + #13#10;

  WriteContent(Content);
end;

procedure TTExportSQL.ExecuteFooter();
var
  Content: string;
begin
  Content := '';

  if (DisableKeys and (Client.ServerVersion >= 40014)) then
  begin
    Content := Content + '/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;' + #13#10;
    Content := Content + '/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;' + #13#10;
  end;
  if (Assigned(Client.VariableByName('SQL_NOTES')) and (Client.ServerVersion >= 40111)) then
    Content := Content + '/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;' + #13#10;
  if (Assigned(Client.VariableByName('SQL_MODE')) and (Client.ServerVersion >= 40101)) then
    Content := Content + '/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;' + #13#10;
  if (Assigned(Client.VariableByName('TIME_ZONE')) and (Client.VariableByName('TIME_ZONE').Value <> 'SYSTEM') and (Client.ServerVersion >= 40103)) then
    Content := Content + '/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;' + #13#10;
  if ((CodePage <> CP_UNICODE) and (Client.CodePageToCharset(CodePage) <> '') and (Client.ServerVersion >= 40101)) then
  begin
    Content := Content + '/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;' + #13#10;
    Content := Content + '/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;' + #13#10;
    Content := Content + '/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;' + #13#10;
  end;

  WriteContent(#13#10 + Content);
end;

procedure TTExportSQL.ExecuteHeader();
var
  Content: string;
begin
  DoFileCreate(Filename);

  Content := Content + '# Host: ' + Client.Host;
  if (Client.Port <> MYSQL_PORT) then
    Content := Content + ':' + IntToStr(Client.Port);
  Content := Content + '  (Version: ' + Client.ServerVersionStr + ')' + #13#10;
  Content := Content + '# Date: ' + MySQLDB.DateTimeToStr(Now(), Client.FormatSettings) + #13#10;
  Content := Content + '# Generator: ' + LoadStr(1000) + ' ' + Preferences.VersionStr + #13#10;
  Content := Content + #13#10;

  if ((CodePage <> CP_UNICODE) and (Client.CodePageToCharset(CodePage) <> '') and (Client.ServerVersion >= 40101)) then
  begin
    Content := Content + '/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;' + #13#10;
    Content := Content + '/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;' + #13#10;
    Content := Content + '/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;' + #13#10;
    Content := Content + '/*!40101 SET NAMES ' + Client.CodePageToCharset(CodePage) + ' */;' + #13#10;
  end;
  if (Assigned(Client.VariableByName('TIME_ZONE')) and (Client.VariableByName('TIME_ZONE').Value <> 'SYSTEM')) then
  begin
    Content := Content + '/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;' + #13#10;
    Content := Content + '/*!40103 SET TIME_ZONE=' + SQLEscape(Client.VariableByName('TIME_ZONE').Value) + ' */;' + #13#10;
  end;
  if (Assigned(Client.VariableByName('SQL_MODE')) and (Client.ServerVersion >= 40101)) then
  begin
    Content := Content + '/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE */;' + #13#10;
    Content := Content + '/*!40101 SET SQL_MODE=' + SQLEscape(Client.VariableByName('SQL_MODE').Value) + ' */;' + #13#10;
  end;
  if (Assigned(Client.VariableByName('SQL_NOTES'))) then
  begin
    Content := Content + '/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES */;' + #13#10;
    Content := Content + '/*!40103 SET SQL_NOTES=' + SQLEscape(Client.VariableByName('SQL_NOTES').Value) + ' */;' + #13#10;
  end;
  if (DisableKeys and (Client.ServerVersion >= 40014)) then
  begin
    Content := Content + '/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS */;' + #13#10;
    Content := Content + '/*!40014 SET UNIQUE_CHECKS=0 */;' + #13#10;
    Content := Content + '/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS */;' + #13#10;
    Content := Content + '/*!40014 SET FOREIGN_KEY_CHECKS=0 */;' + #13#10;
  end;

  WriteContent(Content);
end;

procedure TTExportSQL.ExecuteRoutine(const Routine: TCRoutine);
var
  Content: string;
begin
  Content := #13#10;
  if (Routine is TCProcedure) then
  begin
    Content := Content + '#' + #13#10;
    Content := Content + '# Source for procedure "' + Routine.Name + '"' + #13#10;
    Content := Content + '#' + #13#10;
  end
  else if (Routine is TCFunction) then
  begin
    Content := Content + '#' + #13#10;
    Content := Content + '# Source for function "' + Routine.Name + '"' + #13#10;
    Content := Content + '#' + #13#10;
  end;
  Content := Content + #13#10;
  Content := Content + Routine.GetSourceEx(IncludeDropStmts) + #13#10;

  WriteContent(Content);
end;

procedure TTExportSQL.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Content: string;
begin
  if (SQLInsertPacketLen > 0) then
  begin
    WriteContent(SQLInsertPostfix);
    SQLInsertPacketLen := 0;
  end;

  if (Assigned(Table) and Data) then
  begin
    Content := '';

    if (DisableKeys and (Table is TCBaseTable)) then
      Content := Content + '/*!40000 ALTER TABLE ' + Client.EscapeIdentifier(Table.Name) + ' ENABLE KEYS */;' + #13#10;

    if (Content <> '') then
      WriteContent(Content);
  end;
end;

procedure TTExportSQL.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Content: string;
  ForeignKeySource: string;
  I: Integer;
begin
  Content := '';

  if (Structure and Assigned(Table)) then
  begin
    if (Table is TCBaseTable) then
    begin
      Content := Content + #13#10;
      Content := Content + '#' + #13#10;
      Content := Content + '# Source for table "' + Table.Name + '"' + #13#10;
      Content := Content + '#' + #13#10;
    end
    else if (Table is TCView) then
    begin
      Content := Content + #13#10;
      Content := Content + '#' + #13#10;
      Content := Content + '# Source for view "' + Table.Name + '"' + #13#10;
      Content := Content + '#' + #13#10;
    end;
    Content := Content + '' + #13#10;

    if (Table is TCBaseTable) then
    begin
      Content := Content + Table.GetSourceEx(IncludeDropStmts, False, @ForeignKeySource) + #13#10;

      if (ForeignKeySource <> '') then
      begin
        ForeignKeySources := ForeignKeySources + #13#10;
        ForeignKeySources := ForeignKeySources + '#' + #13#10;
        ForeignKeySources := ForeignKeySources + '#  Foreign keys for table ' + Table.Name + #13#10;
        ForeignKeySources := ForeignKeySources + '#' + #13#10;
        ForeignKeySources := ForeignKeySources + #13#10;
        ForeignKeySources := ForeignKeySources + ForeignKeySource + #13#10;
      end;
    end
    else if (Table is TCView) then
      Content := Content + Table.GetSourceEx(IncludeDropStmts, False) + #13#10;
  end;

  if (Assigned(Table) and Data) then
  begin
    if (Data) then
    begin
      Content := Content + #13#10;
      Content := Content + '#' + #13#10;
      Content := Content + '# Data for table "' + Table.Name + '"' + #13#10;
      Content := Content + '#' + #13#10;
    end;

    Content := Content + #13#10;

    if (DisableKeys and (Table is TCBaseTable)) then
      Content := Content + '/*!40000 ALTER TABLE ' + Client.EscapeIdentifier(Table.Name) + ' DISABLE KEYS */;' + #13#10;
  end;

  if (Content <> '') then
    WriteContent(Content);


  if (Data) then
  begin
    if (ReplaceData) then
      SQLInsertPrefix := 'REPLACE INTO '
    else
      SQLInsertPrefix := 'INSERT INTO ';

    SQLInsertPrefix := SQLInsertPrefix + Client.EscapeIdentifier(Table.Name);

    if (not Structure and Data and Assigned(Table)) then
    begin
      SQLInsertPrefix := SQLInsertPrefix + ' (';
      for I := 0 to Length(Fields) - 1 do
      begin
        if (I > 0) then SQLInsertPrefix := SQLInsertPrefix + ',';
        SQLInsertPrefix := SQLInsertPrefix + Client.EscapeIdentifier(Fields[I].FieldName);
      end;
      SQLInsertPrefix := SQLInsertPrefix + ')';
    end;

    SQLInsertPrefix := SQLInsertPrefix + ' VALUES ';
    SQLInsertPrefixPacketLen := SizeOf(COM_QUERY) + WideCharToMultiByte(Client.CodePage, 0, PChar(SQLInsertPrefix), Length(SQLInsertPrefix), nil, 0, nil, nil);

    SQLInsertPostfix := ';' + #13#10;
    SQLInsertPrefixPacketLen := WideCharToMultiByte(Client.CodePage, 0, PChar(SQLInsertPostfix), Length(SQLInsertPostfix), nil, 0, nil, nil);

    SQLInsertPacketLen := 0;
  end;
end;

procedure TTExportSQL.ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  I: Integer;
  Values: string;
  ValuesPacketLen: Integer;
begin
  Values := '(';
  for I := 0 to Length(Fields) - 1 do
  begin
    if (I > 0) then Values := Values + ',';
    Values := Values + DataSet.SQLFieldValue(Fields[I]);
  end;
  Values := Values + ')';

  ValuesPacketLen := WideCharToMultiByte(Client.CodePage, 0, PChar(Values), Length(Values), nil, 0, nil, nil);

  if ((SQLInsertPacketLen > 0) and (SQLInsertPacketLen + ValuesPacketLen + SQLInsertPostfixPacketLen >= SQLPacketSize)) then
  begin
    WriteContent(SQLInsertPostfix);
    SQLInsertPacketLen := 0;
  end;

  if (SQLInsertPacketLen = 0) then
  begin
    WriteContent(SQLInsertPrefix);
    Inc(SQLInsertPacketLen, SQLInsertPrefixPacketLen);
  end
  else
  begin
    WriteContent(',');
    Inc(SQLInsertPacketLen, 1);
  end;

  WriteContent(Values);
  Inc(SQLInsertPacketLen, ValuesPacketLen);
end;

procedure TTExportSQL.ExecuteTrigger(const Trigger: TCTrigger);
var
  Content: string;
begin
  Content := #13#10;
  Content := Content + '#' + #13#10;
  Content := Content + '# Source for trigger "' + Trigger.Name + '"' + #13#10;
  Content := Content + '#' + #13#10;
  Content := Content + #13#10;
  Content := Content + Trigger.GetSourceEx(IncludeDropStmts) + #13#10;

  WriteContent(Content);
end;

function TTExportSQL.FileCreate(const Filename: TFileName; out Error: TTools.TError): Boolean;
var
  Size: DWord;
begin
  Result := inherited FileCreate(Filename, Error);

  if (Result) then
  begin
    case (CodePage) of
      CP_UTF8: Result := WriteFile(Handle, BOM_UTF8^, Length(BOM_UTF8), Size, nil) and (Integer(Size) = Length(BOM_UTF8));
      CP_UNICODE: Result := WriteFile(Handle, BOM_UNICODE^, Length(BOM_UNICODE), Size, nil) and (Integer(Size) = Length(BOM_UNICODE));
    end;

    if (not Result) then
    begin
      Error.ErrorType := TE_File;
      Error.ErrorCode := GetLastError();
      Error.ErrorMessage := SysErrorMessage(GetLastError());
    end;
  end;
end;

{ TTExportText ****************************************************************}

constructor TTExportText.Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal);
begin
  inherited;

  Delimiter := ',';
  Quoter := '"';
  QuoteStringValues := True;
  QuoteValues := False;
end;

destructor TTExportText.Destroy();
begin
  SetLength(DestinationFields, 0);
  SetLength(Fields, 0);
  SetLength(TableFields, 0);

  inherited;
end;

procedure TTExportText.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
  CloseFile();
end;

procedure TTExportText.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Content: string;
  I: Integer;
  Value: string;
begin
  if (Length(ExportObjects) + Length(DBGrids) = 1) then
    DoFileCreate(Filename)
  else
    DoFileCreate(Filename + Table.Name + '.csv');

  if ((Success = daSuccess) and Structure) then
  begin
    Content := '';

    for I := 0 to Length(Fields) - 1 do
    begin
      if (I > 0) then Content := Content + Delimiter;

      if (not Assigned(Table)) then
        Value := Fields[I].DisplayName
      else if (Length(DestinationFields) > 0) then
        Value := DestinationFields[I].Name
      else
        Value := Table.Fields[I].Name;

      if (QuoteValues or QuoteStringValues) then
        Content := Content + Quoter + Value + Quoter
      else
        Content := Content + Value;
    end;

    WriteContent(Content + #13#10);
  end;
end;

procedure TTExportText.ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Content: string;
  I: Integer;
begin
  Content := '';
  for I := 0 to Length(Fields) - 1 do
  begin
    if (I > 0) then Content := Content + Delimiter;
    if (not Assigned(DataSet.LibRow^[Fields[I].FieldNo - 1])) then
      // NULL values are empty in MS Text files
    else if (Fields[I].DataType in BinaryDataTypes) then
      Content := Content + CSVEscape(DataSet.LibRow^[Fields[I].FieldNo - 1], DataSet.LibLengths^[Fields[I].FieldNo - 1], Quoter, QuoteStringValues)
    else
      Content := Content + CSVEscape(DataSet.GetAsString(Fields[I].FieldNo), Quoter, (BitField(Fields[I]) or (Fields[I].DataType in UnquotedDataTypes)) and QuoteValues or QuoteStringValues);
  end;
  WriteContent(Content + #13#10);
end;

function TTExportText.FileCreate(const Filename: TFileName; out Error: TTools.TError): Boolean;
var
  Size: DWord;
begin
  Result := inherited FileCreate(Filename, Error);

  if (Result) then
  begin
    case (CodePage) of
      CP_UTF8: Result := WriteFile(Handle, BOM_UTF8^, Length(BOM_UTF8), Size, nil) and (Integer(Size) = Length(BOM_UTF8));
      CP_UNICODE: Result := WriteFile(Handle, BOM_UNICODE^, Length(BOM_UNICODE), Size, nil) and (Integer(Size) = Length(BOM_UNICODE));
    end;

    if (not Result) then
    begin
      Error.ErrorType := TE_File;
      Error.ErrorCode := GetLastError();
      Error.ErrorMessage := SysErrorMessage(GetLastError());
    end;
  end;
end;

{ TTExportUML *****************************************************************}

procedure TTExportUML.ExecuteHeader();
begin
  DoFileCreate(Filename);
end;

{ TTExportHTML ****************************************************************}

constructor TTExportHTML.Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal);
begin
  inherited;

  IndexBackground := False;
  TextContent := False;
  NULLText := True;
  RowBackground := True;

  Font := TFont.Create();
  Font.Name := Preferences.GridFontName;
  Font.Style := Preferences.GridFontStyle;
  Font.Charset := Preferences.GridFontCharset;
  Font.Color := Preferences.GridFontColor;
  Font.Size := Preferences.GridFontSize;

  SQLFont := TFont.Create();
  SQLFont.Name := Preferences.SQLFontName;
  SQLFont.Style := Preferences.SQLFontStyle;
  SQLFont.Charset := Preferences.SQLFontCharset;
  SQLFont.Color := Preferences.SQLFontColor;
  SQLFont.Size := Preferences.SQLFontSize;
end;

destructor TTExportHTML.Destroy();
begin
  Font.Free();
  SQLFont.Free();

  inherited;
end;

function TTExportHTML.Escape(const Str: string): string;
label
  StartL,
  StringL, String2,
  PositionL, PositionE,
  MoveReplaceL, MoveReplaceE,
  FindPos, FindPos2,
  Finish;
const
  SearchLen = 6;
  Search: array [0 .. SearchLen - 1] of Char = (#0, #10, #13, '"', '<', '>');
  Replace: array [0 .. SearchLen - 1] of PChar = ('', '<br>' + #13#10, '', '&quot;', '&lt;', '&gt;');
var
  Len: Integer;
  Positions: packed array [0 .. SearchLen - 1] of Cardinal;
begin
  Len := Length(Str);

  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, 6 * Len); // reserve space

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Str)               // Copy characters from Str
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]
        MOV ECX,Len                      // Length of Str string

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindPos                     // Find Search character position
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!

      // -------------------

      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first position
        MOV EAX,0                        // Last character
        LEA EDX,Positions
      PositionL:
        CMP [EDX + ECX * 4],EAX          // Position before other positions?
        JB PositionE                     // No!
        MOV EBX,ECX                      // Index of first position
        MOV EAX,[EDX + EBX * 4]          // Value of first position
      PositionE:
        INC ECX                          // Next Position
        CMP ECX,SearchLen                // All Positions compared?
        JNE PositionL                    // No!

        POP ECX

        SUB ECX,EAX                      // Copy normal characters from Str
        CMP ECX,0                        // Is there something to copy?
        JE String2                       // No!
        REPNE MOVSW                      //   to Result

        MOV ECX,EAX

      String2:
        CMP ECX,0                        // Is there an character to replace?
        JE Finish                        // No!

        ADD ESI,2                        // Step of Search character

        PUSH ESI
        LEA EDX,Replace                  // Insert Replace string
        MOV ESI,[EDX + EBX * 4]
      MoveReplaceL:
        LODSW                            // Get Replace character
        CMP AX,0                         // End of Replace?
        JE MoveReplaceE                  // Yes!
        STOSW                            // Put character in Result
        JMP MoveReplaceL
      MoveReplaceE:
        POP ESI

        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!

        CALL FindPos                     // Find Search character
        JMP StringL

      // -------------------

      FindPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AX,[EDI + EBX * 2]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASW                      // Find Search character
        JNE FindPos2                     // Search character not found!
        INC ECX
      FindPos2:
        LEA EDI,Positions
        MOV [EDI + EBX * 4],ECX          // Store found position
        POP EDI
        POP ECX
        RET

      // -------------------

      Finish:
        MOV EAX,Result                   // Calculate new length of Result
        MOV EAX,[EAX]
        SUB EDI,EAX
        SHR EDI,1                        // 2 Bytes = 1 character
        MOV Len,EDI

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    SetLength(Result, Len);
  end;
end;

procedure TTExportHTML.ExecuteDatabaseHeader(const Database: TCDatabase);
begin
  if (Assigned(Database)) then
    WriteContent('<h1 class="DatabaseTitle">' + ReplaceStr(Preferences.LoadStr(38), '&', '') + ': ' + Escape(Database.Name) + '</h1>' + #13#10);
end;

procedure TTExportHTML.ExecuteFooter();
var
  Content: string;
begin
  Content := '';
  Content := Content + '</body>' + #13#10;
  Content := Content + '</html>' + #13#10;

  WriteContent(Content);

  inherited;
end;

procedure TTExportHTML.ExecuteHeader();
var
  Content: string;
  Title: string;
begin
  inherited;

  Title := ExtractFileName(Filename);
  if (Pos('.', Title) > 0) then
  begin
    while (Title[Length(Title)] <> '.') do Delete(Title, Length(Title), 1);
    Delete(Title, Length(Title), 1);
  end;

  Content := '';
  Content := Content + '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"' + #13#10;
  Content := Content + ' "http://www.w3.org/TR/html4/strict.dtd">' + #13#10;
  Content := Content + '<html>' + #13#10;
  Content := Content + '<head>' + #13#10;
  Content := Content + #9 + '<title>' + Escape(Title) + '</title>' + #13#10;
  if (UMLEncoding(CodePage) <> '') then
    Content := Content + #9 + '<meta http-equiv="Content-Type" content="text/html; charset=' + UMLEncoding(CodePage) + '">' + #13#10;
  Content := Content + #9 + '<meta name="date" content="' + GetUTCDateTime(Now()) + '">' + #13#10;
  Content := Content + #9 + '<meta name="generator" content="' + LoadStr(1000) + ' ' + Preferences.VersionStr + '">' + #13#10;
  Content := Content + #9 + '<style type="text/css"><!--' + #13#10;
  Content := Content + #9#9 + 'body {font-family: Arial,Helvetica,sans-serif; font-size: ' + IntToStr(-Font.Height) + 'px;}' + #13#10;
  Content := Content + #9#9 + 'h1 {font-size: ' + IntToStr(-Font.Height + 6) + 'px; text-decoration: bold;}' + #13#10;
  Content := Content + #9#9 + 'h2 {font-size: ' + IntToStr(-Font.Height + 4) + 'px; text-decoration: bold;}' + #13#10;
  Content := Content + #9#9 + 'h3 {font-size: ' + IntToStr(-Font.Height + 2) + 'px; text-decoration: bold;}' + #13#10;
  Content := Content + #9#9 + 'th {font-size: ' + IntToStr(-Font.Height) + 'px; border-style: solid; border-width: 1px; text-decoration: bold; padding: 1px;}' + #13#10;
  Content := Content + #9#9 + 'td {font-size: ' + IntToStr(-Font.Height) + 'px; border-style: solid; border-width: 1px; padding: 1px;}' + #13#10;
  Content := Content + #9#9 + 'code {font-size: ' + IntToStr(-SQLFont.Height) + 'px; white-space: pre;}' + #13#10;
  Content := Content + #9#9 + '.TableObject {border-collapse: collapse; border-color: #000000; font-family: ' + Escape(Font.Name) + '}' + #13#10;
  Content := Content + #9#9 + '.TableData {border-collapse: collapse; border-color: #000000; font-family: ' + Escape(Font.Name) + '}' + #13#10;
  Content := Content + #9#9 + '.TableHeader {border-color: #000000; text-decoration: bold; background-color: #e0e0e0;}' + #13#10;
  Content := Content + #9#9 + '.ObjectHeader {padding-left: 5px; text-align: left; border-color: #000000; text-decoration: bold;}' + #13#10;
  Content := Content + #9#9 + '.ObjectOfPrimaryKey {text-align: left; border-color: #aaaaaa; text-decoration: bold; background-color: #e0e0e0;}' + #13#10;
  Content := Content + #9#9 + '.ObjectOfUniqueKey {text-align: left; border-color: #aaaaaa; background-color: #e0e0e0;}' + #13#10;
  Content := Content + #9#9 + '.Object {text-align: left; border-color: #aaaaaa;}' + #13#10;
  Content := Content + #9#9 + '.odd {}' + #13#10;
  if (RowBackground) then
    Content := Content + #9#9 + '.even {background-color: #f0f0f0;}' + #13#10
  else
    Content := Content + #9#9 + '.even {}' + #13#10;
  Content := Content + #9#9 + '.DataHeader {padding-left: 5px; text-align: left; text-decoration: bold; border-color: #000000; background-color: #e0e0e0;}' + #13#10;
  if (IndexBackground) then
    Content := Content + #9#9 + '.DataOfPrimaryKey {text-align: left; text-decoration: bold; border-color: #aaaaaa; background-color: #e0e0e0;}' + #13#10
  else
    Content := Content + #9#9 + '.DataOfPrimaryKey {text-align: left; text-decoration: bold; border-color: #aaaaaa;}' + #13#10;
  Content := Content + #9#9 + '.DataOfUniqueKey {border-color: #aaaaaa;}' + #13#10;
  Content := Content + #9#9 + '.Data {border-color: #aaaaaa;}' + #13#10;
  Content := Content + #9#9 + '.DataRightAlign {text-align: right;}' + #13#10;
  Content := Content + #9#9 + '.DataNull {color: #999999; border-color: #aaaaaa;}' + #13#10;
  Content := Content + #9 + '--></style>' + #13#10;
  Content := Content + '</head>' + #13#10;
  Content := Content + '<body>' + #13#10;

  WriteContent(Content);
end;

procedure TTExportHTML.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
  if (Data) then
    WriteContent('</table><br style="page-break-after: always">' + #13#10);

  SetLength(CSS, 0);
  SetLength(FieldOfPrimaryIndex, 0);
end;

procedure TTExportHTML.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  ClassAttr: string;
  Content: string;
  FieldInfo: TFieldInfo;
  I: Integer;
  J: Integer;
  S: string;
begin
  Content := '';

  if (Assigned(DataSet) and Structure) then
  begin
    Content := Content + '<h2>' + ReplaceStr(Preferences.LoadStr(794), '&', '') + ':</h2>' + #13#10;
    Content := Content + '<code>' + DataSet.CommandText + '</code>' + #13#10;
  end;

  if (Table is TCBaseTable) then
  begin
    Content := '<h2 class="TableTitle">' + ReplaceStr(Preferences.LoadStr(302), '&', '') + ': ' + Escape(Table.Name) + '</h2>' + #13#10;
    if (TCBaseTable(Table).Comment <> '') then
      Content := Content + '<p>' + ReplaceStr(Preferences.LoadStr(111), '&', '') + ': ' + Escape(TCBaseTable(Table).Comment) + '</p>' + #13#10;
  end
  else if (Table is TCView) then
    Content := '<h2 class="TableTitle">' + ReplaceStr(Preferences.LoadStr(738), '&', '') + ': ' + Escape(Table.Name) + '</h2>' + #13#10
  else if (Structure) then
    Content := Content + '<h2 class="TableTitle">' + ReplaceStr(Preferences.LoadStr(216), '&', '') + ':</h2>' + #13#10;

  if (Assigned(Table) and (Table is TCBaseTable) and Structure) then
  begin
    if (TCBaseTable(Table).Keys.Count > 0) then
    begin
      Content := Content + '<h3>' + Preferences.LoadStr(458) + ':</h3>' + #13#10;

      Content := Content + '<table border="0" cellspacing="0" summary="' + Escape(Table.Name) + '" class="TableObject">' + #13#10;
      Content := Content + #9 + '<tr class="TableHeader">';
      Content := Content + '<th class="ObjectHeader">' + Escape(ReplaceStr(Preferences.LoadStr(35), '&', '')) + '</th>';
      Content := Content + '<th class="ObjectHeader">' + Escape(Preferences.LoadStr(69)) + '</th>';
      Content := Content + '</tr>' + #13#10;
      for I := 0 to TCBaseTable(Table).Keys.Count - 1 do
      begin
        if (TCBaseTable(Table).Keys[I].Primary) then
          ClassAttr := ' class="ObjectOfPrimaryKey"'
        else if (TCBaseTable(Table).Keys[I].Unique) then
          ClassAttr := ' class="ObjectOfUniqueKey"'
        else
          ClassAttr := ' class="Object"';

        Content := Content + #9 + '<tr class="TableHeader">';
        Content := Content + '<th ' + ClassAttr + '>' + Escape(TCBaseTable(Table).Keys[I].Caption) + '</th>';
        S := '';
        for J := 0 to TCBaseTable(Table).Keys[I].Columns.Count - 1 do
          begin
            if (S <> '') then S := S + ', ';
            S := S + TCBaseTable(Table).Keys[I].Columns[J].Field.Name;
          end;
        Content := Content + '<td' + ClassAttr + '>' + Escape(S) + '</td>';
        Content := Content + '</tr>' + #13#10;
      end;
      Content := Content + '</table><br>' + #13#10;
    end;

    Content := Content + '<h3>' + Preferences.LoadStr(253) + ':</h3>' + #13#10;

    Content := Content + '<table border="0" cellspacing="0" summary="' + Escape(Table.Name) + '" class="TableObject">' + #13#10;
    Content := Content + #9 + '<tr class="TableHeader">';
    Content := Content + '<th class="ObjectHeader">' + Escape(ReplaceStr(Preferences.LoadStr(35), '&', '')) + '</th>';
    Content := Content + '<th class="ObjectHeader">' + Escape(Preferences.LoadStr(69)) + '</th>';
    Content := Content + '<th class="ObjectHeader">' + Escape(Preferences.LoadStr(71)) + '</th>';
    Content := Content + '<th class="ObjectHeader">' + Escape(Preferences.LoadStr(72)) + '</th>';
    Content := Content + '<th class="ObjectHeader">' + Escape(ReplaceStr(Preferences.LoadStr(73), '&', '')) + '</th>';
    if (Client.ServerVersion >= 40100) then
      Content := Content + '<th class="ObjectHeader">' + Escape(ReplaceStr(Preferences.LoadStr(111), '&', '')) + '</th>';
    Content := Content + '</tr>' + #13#10;
    for I := 0 to Table.Fields.Count - 1 do
    begin
      if (Table.Fields[I].InPrimaryKey) then
        ClassAttr := ' class="ObjectOfPrimaryKey"'
      else if (Table.Fields[I].InUniqueKey) then
        ClassAttr := ' class="ObjectOfUniqueKey"'
      else
        ClassAttr := ' class="Object"';

      Content := Content + #9 + '<tr>';
      Content := Content + '<th' + ClassAttr + '>' + Escape(Table.Fields[I].Name) + '</th>';
      Content := Content + '<td' + ClassAttr + '>' + Escape(Table.Fields[I].DBTypeStr()) + '</td>';
      if (Table.Fields[I].NullAllowed) then
        Content := Content + '<td' + ClassAttr + '>NULL</td>'
      else
        Content := Content + '<td' + ClassAttr + '>&nbsp;</td>';
      if (Table.Fields[I].Default <> '') then
        Content := Content + '<td' + ClassAttr + '>' + Escape(Table.Fields[I].Default) + '</td>'
      else
        Content := Content + '<td' + ClassAttr + '>&nbsp;</td>';
      if (Table.Fields[I].AutoIncrement) then
        Content := Content + '<td' + ClassAttr + '>auto_increment</td>'
      else
        Content := Content + '<td' + ClassAttr + '>&nbsp;</td>';
      if (Client.ServerVersion >= 40100) then
        if (TCBaseTableField(Table.Fields[I]).Comment <> '') then
          Content := Content + '<td' + ClassAttr + '>' + Escape(TCBaseTableField(Table.Fields[I]).Comment) + '</td>'
        else
          Content := Content + '<td' + ClassAttr + '>&nbsp;</td>';
      Content := Content + #9 + '</tr>' + #13#10;
    end;
    Content := Content + '</table><br>' + #13#10;

    if (TCBaseTable(Table).ForeignKeys.Count > 0) then
    begin
      Content := Content + '<h3>' + Preferences.LoadStr(459) + ':</h3>' + #13#10;

      Content := Content + '<table border="0" cellspacing="0" summary="' + Escape(Table.Name) + '" class="TableObject">' + #13#10;
      Content := Content + #9 + '<tr class="TableHeader">';
      Content := Content + '<th class="ObjectHeader">' + Escape(ReplaceStr(Preferences.LoadStr(35), '&', '')) + '</th>';
      Content := Content + '<th class="ObjectHeader">' + Escape(Preferences.LoadStr(69)) + '</th>';
      Content := Content + '</tr>' + #13#10;
      for I := 0 to TCBaseTable(Table).ForeignKeys.Count - 1 do
      begin
        Content := Content + #9 + '<tr>';
        Content := Content + '<th class="ObjectHeader">' + Escape(TCBaseTable(Table).ForeignKeys.ForeignKey[I].Name) + '</th>';
        Content := Content + '<td class="ObjectHeader">' + Escape(TCBaseTable(Table).ForeignKeys.ForeignKey[I].DBTypeStr()) + '</td>';
        Content := Content + '</tr>' + #13#10;
      end;
      Content := Content + '</table><br>' + #13#10;
    end;
  end;

  if (Data) then
  begin
    if (Assigned(Table) and Structure) then
      Content := Content + '<h3>' + Preferences.LoadStr(580) + ':</h3>' + #13#10;

    if (Assigned(Table)) then
      Content := Content + '<table border="0" cellspacing="0" summary="' + Escape(Table.Name) + '" class="TableData">' + #13#10
    else
      Content := Content + '<table border="0" cellspacing="0" class="TableData">' + #13#10;
    Content := Content + #9 + '<tr class="TableHeader">';
    for I := 0 to Length(Fields) - 1 do
      if (I < Length(DestinationFields)) then
        Content := Content + '<th class="DataHeader">' + Escape(DestinationFields[I].Name) + '</th>'
      else
        Content := Content + '<th class="DataHeader">' + Escape(Fields[I].DisplayName) + '</th>';
    Content := Content + '</tr>' + #13#10;


    SetLength(CSS, Length(Fields));
    SetLength(FieldOfPrimaryIndex, Length(Fields));
    for I := 0 to Length(Fields) - 1 do
    begin
      FieldOfPrimaryIndex[I] := Fields[I].IsIndexField;

      CSS[I] := 'Data';
      if (FieldOfPrimaryIndex[I]) then
        CSS[I] := 'DataOfPrimaryKey'
      else if ((Table is TCBaseTable) and GetFieldInfo(Fields[I].Origin, FieldInfo)) then
        for J := 0 to TCBaseTable(Table).Keys.Count - 1 do
          if (Assigned(TCBaseTable(Table).Keys[J].ColumnByFieldName(FieldInfo.OriginalFieldName))) then
            CSS[I] := 'DataOfUniqueKey';

      if (Fields[I].Alignment = taRightJustify) then
        CSS[I] := CSS[I] + ' DataRightAlign';

      RowOdd := True;
    end;
  end;

  WriteContent(Content);
end;

procedure TTExportHTML.ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Content: string;
  I: Integer;
  Value: string;
begin
  Content := '';

  if (RowOdd) then
    Content := Content + #9 + '<tr class="odd">'
  else
    Content := Content + #9 + '<tr class="even">';
  RowOdd := not RowOdd;

  for I := 0 to Length(Fields) - 1 do
    if (Fields[I].IsNull) then
      if (NULLText) then
        Content := Content + '<td class="DataNull">&lt;NULL&gt;</td>'
      else
        Content := Content + '<td class="DataNull">&nbsp;</td>'
    else
    begin
      if (DataSet.LibLengths^[I] = 0) then
        Value := '&nbsp;'
      else if (GeometryField(Fields[I])) then
        Value := '&lt;GEO&gt;'
      else if (not TextContent and (Fields[I].DataType = ftWideMemo)) then
        Value := '&lt;MEMO&gt;'
      else if (Fields[I].DataType = ftBytes) then
        Value := '&lt;BINARY&gt;'
      else if (Fields[I].DataType = ftBlob) then
        Value := '&lt;BLOB&gt;'
      else if (Fields[I].DataType in TextDataTypes) then
        Value := Escape(DataSet.GetAsString(Fields[I].FieldNo))
      else
        Value := DataSet.GetAsString(Fields[I].FieldNo);

      if (FieldOfPrimaryIndex[I]) then
        Content := Content + '<th class="' + CSS[I] + '">' + Value + '</th>'
      else
        Content := Content + '<td class="' + CSS[I] + '">' + Value + '</td>';
    end;

  Content := Content + '</tr>' + #13#10;

  WriteContent(Content);
end;

{ TTExportXML *****************************************************************}

constructor TTExportXML.Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal);
begin
  inherited;

  DatabaseTag := '';
  RootTag := '';
  TableTag := '';
end;

function TTExportXML.Escape(const Str: string): string;
label
  StartL,
  StringL, String2,
  PositionL, PositionE,
  MoveReplaceL, MoveReplaceE,
  FindPos, FindPos2,
  Finish;
const
  SearchLen = 5;
  Search: array [0 .. SearchLen - 1] of Char = ('&', '"', '''', '<', '>');
  Replace: array [0 .. SearchLen - 1] of PChar = ('&amp;', '&quot;', '&apos;', '&lt;', '&gt;');
var
  Len: Integer;
  Positions: packed array [0 .. SearchLen - 1] of Cardinal;
begin
  Len := Length(Str);

  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, 6 * Len); // reserve space

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Str)               // Copy characters from Str
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]
        MOV ECX,Len                      // Length of Str string

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindPos                     // Find Search character position
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!

      // -------------------

      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first position
        MOV EAX,0                        // Last character
        LEA EDX,Positions
      PositionL:
        CMP [EDX + ECX * 4],EAX          // Position before other positions?
        JB PositionE                     // No!
        MOV EBX,ECX                      // Index of first position
        MOV EAX,[EDX + EBX * 4]          // Value of first position
      PositionE:
        INC ECX                          // Next Position
        CMP ECX,SearchLen                // All Positions compared?
        JNE PositionL                    // No!

        POP ECX

        SUB ECX,EAX                      // Copy normal characters from Str
        CMP ECX,0                        // Is there something to copy?
        JE String2                       // No!
        REPNE MOVSW                      //   to Result

        MOV ECX,EAX

      String2:
        CMP ECX,0                        // Is there an character to replace?
        JE Finish                        // No!

        ADD ESI,2                        // Step of Search character

        PUSH ESI
        LEA EDX,Replace                  // Insert Replace string
        MOV ESI,[EDX + EBX * 4]
      MoveReplaceL:
        LODSW                            // Get Replace character
        CMP AX,0                         // End of Replace?
        JE MoveReplaceE                  // Yes!
        STOSW                            // Put character in Result
        JMP MoveReplaceL
      MoveReplaceE:
        POP ESI

        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!

        CALL FindPos                     // Find Search character
        JMP StringL

      // -------------------

      FindPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AX,[EDI + EBX * 2]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASW                      // Find Search character
        JNE FindPos2                     // Search character not found!
        INC ECX
      FindPos2:
        LEA EDI,Positions
        MOV [EDI + EBX * 4],ECX          // Store found position
        POP EDI
        POP ECX
        RET

      // -------------------

      Finish:
        MOV EAX,Result                   // Calculate new length of Result
        MOV EAX,[EAX]
        SUB EDI,EAX
        SHR EDI,1                        // 2 Bytes = 1 character
        MOV Len,EDI

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    SetLength(Result, Len);
  end;
end;

procedure TTExportXML.ExecuteDatabaseFooter(const Database: TCDatabase);
begin
  if (Assigned(Database)) then
    if (DatabaseAttribute <> '') then
      WriteContent('</' + DatabaseTag + '>' + #13#10)
    else if (DatabaseTag <> '') then
      WriteContent('</' + LowerCase(Escape(Database.Name)) + '>' + #13#10);
end;

procedure TTExportXML.ExecuteDatabaseHeader(const Database: TCDatabase);
begin
  if (Assigned(Database)) then
    if (DatabaseAttribute <> '') then
      WriteContent('<' + DatabaseTag + ' ' + DatabaseAttribute + '="' + Escape(Database.Name) + '">' + #13#10)
    else if (DatabaseTag <> '') then
      WriteContent('<' + LowerCase(Escape(Database.Name)) + '>' + #13#10);
end;

procedure TTExportXML.ExecuteFooter();
begin
  WriteContent('</' + RootTag + '>' + #13#10);
end;

procedure TTExportXML.ExecuteHeader();
begin
  DoFileCreate(FFilename);

  if (UMLEncoding(CodePage) = '') then
    WriteContent('<?xml version="1.0"?>' + #13#10)
  else
    WriteContent('<?xml version="1.0" encoding="' + UMLEncoding(CodePage) + '"?>' + #13#10);
  WriteContent('<' + RootTag + ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' + #13#10);
end;

procedure TTExportXML.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
  if (Assigned(Table)) then
    if (TableAttribute <> '') then
      WriteContent('</' + TableTag + '>' + #13#10)
    else if (TableTag <> '') then
      WriteContent('</' + LowerCase(Escape(Table.Name)) + '>' + #13#10);
end;

procedure TTExportXML.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
  if (Assigned(Table)) then
    if (TableAttribute <> '') then
      WriteContent('<' + TableTag + ' ' + TableAttribute + '="' + Escape(Table.Name) + '">' + #13#10)
    else if (TableTag <> '') then
      WriteContent('<' + LowerCase(Escape(Table.Name)) + '>' + #13#10);
end;

procedure TTExportXML.ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Content: string;
  I: Integer;
begin
  Content := #9 + '<' + RecordTag + '>' + #13#10;

  for I := 0 to Length(Fields) - 1 do
  begin
    if (FieldAttribute = '') then
      if (Length(DestinationFields) > 0) then
        Content := Content + #9#9 + '<' + LowerCase(Escape(DestinationFields[I].Name)) + ''
      else
        Content := Content + #9#9 + '<' + LowerCase(Escape(Fields[I].DisplayName)) + ''
    else
      if (Length(DestinationFields) > 0) then
        Content := Content + #9#9 + '<' + FieldTag + ' ' + FieldAttribute + '="' + Escape(DestinationFields[I].Name) + '"'
      else
        Content := Content + #9#9 + '<' + FieldTag + ' ' + FieldAttribute + '="' + Escape(Fields[I].DisplayName) + '"';
    if (Fields[I].IsNull) then
      Content := Content + ' xsi:nil="true" />' + #13#10
    else
    begin
      if (Fields[I].DataType in TextDataTypes + BinaryDataTypes) then
        Content := Content + '>' + Escape(DataSet.GetAsString(Fields[I].FieldNo))
      else
        Content := Content + '>' + DataSet.GetAsString(Fields[I].FieldNo);

      if (FieldAttribute = '') then
        if (Length(DestinationFields) > 0) then
          Content := Content + '</' + LowerCase(Escape(DestinationFields[I].Name)) + '>' + #13#10
        else
          Content := Content + '</' + LowerCase(Escape(Fields[I].DisplayName)) + '>' + #13#10
      else
        Content := Content + '</' + FieldTag + '>' + #13#10;
    end;
  end;

  Content := Content + #9 + '</' + RecordTag + '>' + #13#10;

  WriteContent(Content);
end;

{ TTExportODBC ****************************************************************}

constructor TTExportODBC.Create(const AClient: TCClient; const AODBC: SQLHDBC = SQL_NULL_HANDLE; const AHandle: SQLHDBC = SQL_NULL_HANDLE);
begin
  inherited Create(AClient);

  FODBC := AODBC;
  FHandle := AHandle;

  FStmt := SQL_NULL_HANDLE;
  TableName := '';
end;

procedure TTExportODBC.ExecuteFooter();
begin
  inherited;

  if (Stmt <> SQL_NULL_HANDLE) then
    begin SQLFreeHandle(SQL_HANDLE_STMT, FStmt); FStmt := SQL_NULL_HANDLE; end;

  if (Handle <> SQL_NULL_HANDLE) then
  begin
    SQLEndTran(SQL_HANDLE_DBC, Handle, SQL_COMMIT);

    SQLDisconnect(Handle);
    SQLFreeHandle(SQL_HANDLE_DBC, FHandle); FHandle := SQL_NULL_HANDLE;
  end;
  if (ODBC <> SQL_NULL_HANDLE) then
    begin SQLFreeHandle(SQL_HANDLE_ENV, FODBC); FODBC := SQL_NULL_HANDLE; end;
end;

procedure TTExportODBC.ExecuteHeader();
begin
  if (Success = daSuccess) then
  begin
    SQLSetConnectAttr(Handle, SQL_ATTR_AUTOCOMMIT, SQLPOINTER(SQL_AUTOCOMMIT_OFF), 1);
    if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, Handle, @Stmt))) then
      DoError(ODBCError(SQL_HANDLE_DBC, Handle), EmptyToolsItem());
  end;

  inherited;
end;

procedure TTExportODBC.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  I: Integer;
begin
  inherited;

  TableName := '';

  for I := 0 to Length(Parameter) - 1 do
    case (Fields[I].DataType) of
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
      ftExtended,
      ftDate,
      ftDateTime,
      ftTimestamp,
      ftTime,
      ftWideString:
        FreeMem(Parameter[I].Buffer);
    end;
  SetLength(Parameter, 0);
end;

procedure TTExportODBC.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  ColumnSize: SQLUINTEGER;
  Error: TTools.TError;
  I: Integer;
  J: Integer;
  SQL: string;
  ValueType: SQLSMALLINT;
  ParameterType: SQLSMALLINT;
begin
  if (not (Self is TTExportExcel)) then
  begin
    if (TableName = '') then
      TableName := Table.Name;

    SQL := 'CREATE TABLE "' + TableName + '" (';
    for I := 0 to Length(Fields) - 1 do
    begin
      if (I > 0) then SQL := SQL + ',';
      SQL := SQL + '"' + Table.Fields[I].Name + '" ';

      if (Table.Fields[I].AutoIncrement and (Table.Fields[I].FieldType in [mfTinyInt, mfSmallInt, mfMediumInt, mfInt])) then
        SQL := SQL + 'COUNTER'
      else
        case (Table.Fields[I].FieldType) of
          mfBit:
            if ((Self is TTExportAccess) and (Table.Fields[I].Size = 1)) then
              SQL := SQL + 'BIT'
            else
              SQL := SQL + 'BINARY(' + IntToStr(Table.Fields[I].Size div 8) + ')';
          mfTinyInt:
            if (Table.Fields[I].Unsigned) then
              SQL := SQL + 'BYTE'
            else
              SQL := SQL + 'SMALLINT';
          mfSmallInt, mfYear:
            if (not Table.Fields[I].Unsigned) then
              SQL := SQL + 'SMALLINT'
            else
              SQL := SQL + 'INTEGER';
          mfMediumInt:
            SQL := SQL + 'INTEGER';
          mfInt:
            if (not Table.Fields[I].Unsigned) then
              SQL := SQL + 'INTEGER'
            else
              SQL := SQL + 'VARCHAR(10)';
          mfBigInt:
            SQL := SQL + 'VARCHAR(20)';
          mfFloat:
            SQL := SQL + 'REAL';
          mfDouble:
            SQL := SQL + 'FLOAT';
          mfDecimal:
            SQL := SQL + 'CURRENCY';
          mfDate:
            SQL := SQL + 'DATE';
          mfDateTime, mfTimeStamp:
            SQL := SQL + 'TIMESTAMP';
          mfTime:
            SQL := SQL + 'TIME';
          mfChar:
            SQL := SQL + 'CHAR(' + IntToStr(Table.Fields[I].Size) + ')';
          mfEnum, mfSet:
            SQL := SQL + 'VARCHAR';
          mfVarChar:
            if (Table.Fields[I].Size <= 255) then
              SQL := SQL + 'VARCHAR(' + IntToStr(Table.Fields[I].Size) + ')'
            else
              SQL := SQL + 'LONGTEXT';
          mfTinyText, mfText, mfMediumText, mfLongText:
            SQL := SQL + 'LONGTEXT';
          mfBinary:
            SQL := SQL + 'BINARY(' + IntToStr(Table.Fields[I].Size) + ')';
          mfVarBinary:
            if (Table.Fields[I].Size <= 255) then
              SQL := SQL + 'VARBINARY(' + IntToStr(Table.Fields[I].Size) + ')'
            else
              SQL := SQL + 'LONGBINARY';
          mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob,
          mfGeometry, mfPoint, mfLineString, mfPolygon, mfMultiPoint, mfMultiLineString, mfMultiPolygon, mfGeometryCollection:
            SQL := SQL + 'LONGBINARY';
          else
            raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Table.Fields[I].Name, Ord(Table.Fields[I].FieldType)]);
        end;
      if (not Table.Fields[I].NullAllowed) then
        SQL := SQL + ' NOT NULL';
    end;
    if ((Table is TCBaseTable) and Assigned(TCBaseTable(Table).PrimaryKey)) then
    begin
      SQL := SQL + ',PRIMARY KEY (';
      for I := 0 to TCBaseTable(Table).PrimaryKey.Columns.Count - 1 do
      begin
        if (I > 0) then SQL := SQL + ',';
        SQL := SQL + '"' + TCBaseTable(Table).PrimaryKey.Columns[I].Field.Name + '"';
      end;
      SQL := SQL + ')';
    end;
    SQL := SQL + ')';

    while ((Success = daSuccess) and not SQL_SUCCEEDED(SQLExecDirect(Stmt, PSQLTCHAR(SQL), SQL_NTS))) do
    begin
      Error := ODBCError(SQL_HANDLE_STMT, Stmt);
      Error.ErrorMessage := Error.ErrorMessage + ' - ' + SQL;
      DoError(Error, EmptyToolsItem());
    end;

    if (Table is TCBaseTable) then
      for I := 0 to TCBaseTable(Table).Keys.Count - 1 do
        if (not TCBaseTable(Table).Keys[I].Primary) then
        begin
          SQL := 'CREATE';
          if (TCBaseTable(Table).Keys[I].Unique) then
            SQL := SQL + ' UNIQUE';
          SQL := SQL + ' INDEX "' + TCBaseTable(Table).Keys[I].Name + '"';
          SQL := SQL + ' ON "' + Table.Name + '"';
          SQL := SQL + ' (';
          for J := 0 to TCBaseTable(Table).Keys[I].Columns.Count - 1 do
          begin
            if (J > 0) then SQL := SQL + ',';
            SQL := SQL + '"' + TCBaseTable(Table).Keys[I].Columns[J].Field.Name + '"';
          end;
          SQL := SQL + ');';

          // Execute silent, since some ODBC drivers doesn't support keys
          // and the user should know that...
          SQLExecDirect(Stmt, PSQLTCHAR(SQL), SQL_NTS);
        end;
  end;

  SetLength(Parameter, Length(Fields));

  for I := 0 to Length(Fields) - 1 do
  begin
    if (BitField(Fields[I])) then
      begin
        ValueType := SQL_C_BIT;
        ParameterType := SQL_BIT;
        ColumnSize := Fields[I].Size;
        Parameter[I].BufferSize := Fields[I].Size;
        GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
      end
    else
      case (Fields[I].DataType) of
        ftString:
          begin
            ValueType := SQL_C_BINARY;
            ParameterType := SQL_BINARY;
            ColumnSize := Fields[I].Size;
            Parameter[I].BufferSize := ColumnSize;
            GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
          end;
        ftShortInt,
        ftByte,
        ftSmallInt,
        ftWord,
        ftInteger,
        ftLongWord,
        ftLargeint,
        ftSingle,
        ftFloat,
        ftExtended,
        ftTimestamp:
          begin
            ValueType := SQL_C_CHAR;
            ParameterType := SQL_CHAR;
            ColumnSize := 100;
            Parameter[I].BufferSize := ColumnSize;
            GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
          end;
        ftDate:
          begin
            ValueType := SQL_C_CHAR;
            ParameterType := SQL_TYPE_DATE;
            ColumnSize := 10; // 'yyyy-mm-dd'
            Parameter[I].BufferSize := ColumnSize;
            GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
          end;
        ftDateTime:
          begin
            ValueType := SQL_C_CHAR;
            ParameterType := SQL_TYPE_TIMESTAMP;
            ColumnSize := 19; // 'yyyy-mm-dd hh:hh:ss'
            Parameter[I].BufferSize := ColumnSize;
            GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
          end;
        ftTime:
          begin
            ValueType := SQL_C_CHAR;
            ParameterType := -154; // SQL_SS_TIME2
            ColumnSize := 8; // 'hh:mm:ss'
            Parameter[I].BufferSize := ColumnSize;
            GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
          end;
        ftWideString:
          begin
            ValueType := SQL_C_WCHAR;
            ParameterType := SQL_WCHAR;
            ColumnSize := Fields[I].Size;
            Parameter[I].BufferSize := ColumnSize * SizeOf(Char);
            GetMem(Parameter[I].Buffer, Parameter[I].BufferSize);
          end;
        ftWideMemo:
          begin
            ValueType := SQL_C_WCHAR;
            ParameterType := SQL_WLONGVARCHAR;
            ColumnSize := Fields[I].Size;
            Parameter[I].BufferSize := ODBCDataSize;
            Parameter[I].Buffer := SQLPOINTER(I);
          end;
        ftBlob:
          begin
            ValueType := SQL_C_BINARY;
            ParameterType := SQL_LONGVARBINARY;
            ColumnSize := Fields[I].Size;
            Parameter[I].BufferSize := ODBCDataSize;
            Parameter[I].Buffer := SQLPOINTER(I);
          end;
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].DisplayName, Ord(Fields[I].DataType)]);
      end;

    while ((Success = daSuccess) and not SQL_SUCCEEDED(SQLBindParameter(Stmt, 1 + I, SQL_PARAM_INPUT, ValueType, ParameterType,
      ColumnSize, 0, Parameter[I].Buffer, Parameter[I].BufferSize, @Parameter[I].Size))) do
    begin
      Error := ODBCError(SQL_HANDLE_STMT, Stmt);
      Error.ErrorMessage := Error.ErrorMessage;
      DoError(Error, EmptyToolsItem());
    end;
  end;

  SQL := 'INSERT INTO "' + TableName + '" VALUES (';
  for I := 0 to Length(Fields) - 1 do
  begin
    if (I > 0) then SQL := SQL + ',';
    SQL := SQL + '?';
  end;
  SQL := SQL + ')';

  while ((Success = daSuccess) and not SQL_SUCCEEDED(SQLPrepare(Stmt, PSQLTCHAR(SQL), SQL_NTS))) do
  begin
    Error := ODBCError(SQL_HANDLE_STMT, Stmt);
    Error.ErrorMessage := Error.ErrorMessage + ' - ' + SQL;
    DoError(Error, EmptyToolsItem());
  end;
end;

procedure TTExportODBC.ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  DateTime: TDateTime;
  Error: TTools.TError;
  Field: SQLPOINTER;
  I: Integer;
  Index: Integer;
  ReturnCode: SQLRETURN;
  S: string;
  Size: Integer;
begin
  for I := 0 to Length(Fields) - 1 do
    if (Fields[I].IsNull) then
      Parameter[I].Size := SQL_NULL_DATA
    else if (BitField(Fields[I])) then
      begin
        Parameter[I].Size := Min(Parameter[I].BufferSize, DataSet.LibLengths^[I]);
        MoveMemory(Parameter[I].Buffer, DataSet.LibRow^[I], Parameter[I].Size);
      end
    else
      case (Fields[I].DataType) of
        ftString:
          begin
            Parameter[I].Size := Min(Parameter[I].BufferSize, DataSet.LibLengths^[I]);
            MoveMemory(Parameter[I].Buffer, DataSet.LibRow^[I], Parameter[I].Size);
          end;
        ftShortInt,
        ftByte,
        ftSmallInt,
        ftWord,
        ftInteger,
        ftLongWord,
        ftLargeint,
        ftSingle,
        ftFloat,
        ftExtended,
        ftTimestamp:
          begin
            Parameter[I].Size := Min(Parameter[I].BufferSize, DataSet.LibLengths^[I]);
            MoveMemory(Parameter[I].Buffer, DataSet.LibRow^[I], Parameter[I].Size);
          end;
        ftDate,
        ftTime,
        ftDateTime:
          begin
            SetString(S, DataSet.LibRow^[I], DataSet.LibLengths^[I]);
            if (not TryStrToDateTime(S, DateTime)) then // Dedect MySQL invalid dates like '0000-00-00' or '2012-02-30'
              Parameter[I].Size := SQL_NULL_DATA        // Handle them as NULL values
            else
            begin
              Parameter[I].Size := Min(Parameter[I].BufferSize, DataSet.LibLengths^[I]);
              MoveMemory(Parameter[I].Buffer, DataSet.LibRow^[I], Parameter[I].Size);
            end;
          end;
        ftWideString:
          begin
            Parameter[I].Size := Min(Parameter[I].BufferSize, MultiByteToWideChar(Client.CodePage, 0,
              DataSet.LibRow^[I], DataSet.LibLengths^[I], nil, 0) * SizeOf(Char));
            MultiByteToWideChar(Client.CodePage, 0,
              DataSet.LibRow^[I], DataSet.LibLengths^[I], Parameter[I].Buffer, Parameter[I].Size div SizeOf(Char));
          end;
        ftWideMemo:
          Parameter[I].Size := SQL_LEN_DATA_AT_EXEC(MultiByteToWideChar(Client.CodePage, 0,
            DataSet.LibRow^[I], DataSet.LibLengths^[I], nil, 0) * SizeOf(Char));
        ftBlob:
          Parameter[I].Size := SQL_LEN_DATA_AT_EXEC(DataSet.LibLengths^[I]);
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].DisplayName, Ord(Fields[I].DataType)]);
      end;

  ReturnCode := SQLExecute(Stmt);
  if (ReturnCode = SQL_NEED_DATA) then
    repeat
      ReturnCode := SQLParamData(Stmt, @Field);
      I := SQLINTEGER(Field);
      if (ReturnCode = SQL_NEED_DATA) then
        case (Fields[I].DataType) of
          ftWideMemo:
            begin
              Size := -(Parameter[I].Size - SQL_LEN_DATA_AT_EXEC_OFFSET);
              S := DataSet.GetAsString(Fields[I].FieldNo);
              Index := 0;
              repeat
                ODBCException(Stmt, SQLPutData(Stmt, @S[1 + Index div 2], Min(ODBCDataSize, Size - Index)));
                Inc(Index, Min(ODBCDataSize, Size - Index));
              until (Index = Size);
            end;
          ftBlob:
            begin
              Size := DataSet.LibLengths^[I];
              Index := 0;
              repeat
                ODBCException(Stmt, SQLPutData(Stmt, @DataSet.LibRow^[Fields[I].FieldNo - 1][Index], Min(ODBCDataSize, Size - Index)));
                Inc(Index, Min(ODBCDataSize, Size - Index));
              until (Index = Size);
            end;
          else
            raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].DisplayName, Ord(Fields[I].DataType)]);
        end;
    until (ReturnCode <> SQL_NEED_DATA);

  while ((Success = daSuccess) and not SQL_SUCCEEDED(ReturnCode)) do
  begin
    Error := ODBCError(SQL_HANDLE_STMT, Stmt);
    Error.ErrorMessage := Error.ErrorMessage;
    DoError(Error, EmptyToolsItem());
  end;
end;

{ TTExportAccess **************************************************************}

constructor TTExportAccess.Create(const AClient: TCClient; const AFilename: TFileName);
begin
  inherited Create(AClient);

  Filename := AFilename;
end;

procedure TTExportAccess.ExecuteHeader();
var
  ConnStrIn: string;
  Error: TTools.TError;
  ErrorCode: DWord;
  ErrorMsg: PChar;
  Size: Word;
begin
  ConnStrIn := 'Driver={Microsoft Access Driver (*.mdb)};' + 'DBQ=' + Filename + ';' + 'READONLY=FALSE';

  if (FileExists(Filename) and not DeleteFile(Filename)) then
  begin
    Error.ErrorType := TE_File;
    Error.ErrorCode := GetLastError();
    Error.ErrorMessage := SysErrorMessage(GetLastError());
    DoError(Error, EmptyToolsItem());
  end
  else if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, @ODBC))) then
    DoError(ODBCError(0, SQL_NULL_HANDLE), EmptyToolsItem())
  else if (not SQL_SUCCEEDED(SQLSetEnvAttr(ODBC, SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), SQL_IS_UINTEGER))) then
    DoError(ODBCError(SQL_HANDLE_ENV, ODBC), EmptyToolsItem())
  else if (not SQLConfigDataSource(Application.Handle, ODBC_ADD_DSN, 'Microsoft Access Driver (*.mdb)', PChar('CREATE_DB=' + Filename + ' General'))) then
  begin
    Error.ErrorType := TE_ODBC;
    GetMem(ErrorMsg, SQL_MAX_MESSAGE_LENGTH * SizeOf(Char));
    SQLInstallerError(1, ErrorCode, ErrorMsg, SQL_MAX_MESSAGE_LENGTH - 1, Size);
    Error.ErrorCode := ErrorCode;
    SetString(Error.ErrorMessage, ErrorMsg, Size);
    Error.ErrorMessage := Error.ErrorMessage + '  (' + ConnStrIn + ')';
    FreeMem(ErrorMsg);
    DoError(Error, EmptyToolsItem());
  end
  else if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBC, @Handle))) then
    DoError(ODBCError(SQL_HANDLE_ENV, ODBC), EmptyToolsItem())
  else if (not SQL_SUCCEEDED(SQLDriverConnect(Handle, Application.Handle, PSQLTCHAR(ConnStrIn), SQL_NTS, nil, 0, nil, SQL_DRIVER_COMPLETE))
    or not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, Handle, @Stmt))) then
    DoError(ODBCError(SQL_HANDLE_DBC, Handle), EmptyToolsItem());

  inherited;
end;

procedure TTExportAccess.ExecuteFooter();
begin
  inherited;

  if (Success = daAbort) then
    DeleteFile(Filename);
end;

{ TTExportExcel ***************************************************************}

constructor TTExportExcel.Create(const AClient: TCClient; const AFilename: TFileName);
begin
  inherited Create(AClient);

  Filename := AFilename;

  Sheet := 0;
end;

procedure TTExportExcel.ExecuteHeader();
var
  ConnStrIn: WideString;
  Error: TTools.TError;
begin
  ConnStrIn := 'Driver={Microsoft Excel Driver (*.xls)};' + 'DBQ=' + Filename + ';' + 'READONLY=FALSE';

  if (FileExists(Filename) and not DeleteFile(Filename)) then
  begin
    Error.ErrorType := TE_File;
    Error.ErrorCode := GetLastError();
    Error.ErrorMessage := SysErrorMessage(GetLastError());
    DoError(Error, EmptyToolsItem());
  end
  else if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, @ODBC))
    or not SQL_SUCCEEDED(SQLSetEnvAttr(ODBC, SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), SQL_IS_UINTEGER))) then
    DoError(ODBCError(0, SQL_NULL_HANDLE), EmptyToolsItem())
  else if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBC, @Handle))
    or not SQL_SUCCEEDED(SQLDriverConnect(Handle, Application.Handle, PSQLTCHAR(ConnStrIn), SQL_NTS, nil, 0, nil, SQL_DRIVER_COMPLETE))) then
    DoError(ODBCError(SQL_HANDLE_ENV, ODBC), EmptyToolsItem())
  else if (not SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, Handle, @Stmt))) then
    DoError(ODBCError(SQL_HANDLE_DBC, Handle), EmptyToolsItem());

  inherited;
end;

procedure TTExportExcel.ExecuteFooter();
begin
  inherited;

  if (Success = daAbort) then
    DeleteFile(Filename);
end;

procedure TTExportExcel.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Error: TTools.TError;
  I: Integer;
  SQL: string;
begin
  Inc(Sheet);
  if (not Assigned(Table)) then
    TableName := 'Sheet' + IntToStr(Sheet)
  else
    TableName := Table.Name;

  SQL := 'CREATE TABLE "' + TableName + '" (';
  for I := 0 to Length(Fields) - 1 do
  begin
    if (I > 0) then
      SQL := SQL + ',';
    if (Length(DestinationFields) > 0) then
      SQL := SQL + '"' + DestinationFields[I].Name + '" '
    else if (Assigned(Table)) then
      SQL := SQL + '"' + Table.Fields[I].Name + '" '
    else
      SQL := SQL + '"' + Fields[I].DisplayName + '" ';
    case (Fields[I].DataType) of
      ftString:
        SQL := SQL + 'BINARY';
      ftShortInt,
      ftByte,
      ftSmallInt,
      ftWord,
      ftInteger,
      ftLongWord,
      ftLargeint,
      ftSingle,
      ftFloat,
      ftExtended:
        SQL := SQL + 'NUMERIC';
      ftDate,
      ftDateTime,
      ftTimestamp,
      ftTime:
        SQL := SQL + 'DATETIME';
      ftWideString,
      ftWideMemo:
        if (Fields[I].Size <= 255) then
          SQL := SQL + 'STRING'
        else
          SQL := SQL + 'LONGTEXT';
      ftBlob:
        if (Fields[I].Size <= 255) then
          SQL := SQL + 'BINARY'
        else
          SQL := SQL + 'LONGBINARY';
      else
        raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].DisplayName, Ord(Fields[I].DataType)]);
    end;
  end;
  SQL := SQL + ')';

  while ((Success = daSuccess) and not SQL_SUCCEEDED(SQLExecDirect(Stmt, PSQLTCHAR(SQL), SQL_NTS))) do
  begin
    Error := ODBCError(SQL_HANDLE_STMT, Stmt);
    Error.ErrorMessage := Error.ErrorMessage + ' - ' + SQL;
    DoError(Error, EmptyToolsItem());
  end;

  if (Success = daSuccess) then
    inherited;
end;

{ TTExportSQLite **************************************************************}

constructor TTExportSQLite.Create(const AClient: TCClient; const AFilename: TFileName; const ACodePage: Cardinal);
begin
  inherited Create(AClient);

  Filename := AFilename;
end;

procedure TTExportSQLite.ExecuteHeader();
var
  Error: TTools.TError;
begin
  if (FileExists(Filename) and not DeleteFile(Filename)) then
  begin
    Error.ErrorType := TE_File;
    Error.ErrorCode := GetLastError();
    Error.ErrorMessage := SysErrorMessage(GetLastError());
    DoError(Error, EmptyToolsItem());
  end
  else if ((sqlite3_open_v2(PAnsiChar(UTF8Encode(Filename)), @Handle, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE, nil) <> SQLITE_OK)
    or (sqlite3_exec(Handle, PAnsiChar(UTF8Encode('BEGIN TRANSACTION;')), nil, nil, nil) <> SQLITE_OK)) then
  begin
    Error.ErrorType := TE_SQLite;
    Error.ErrorCode := sqlite3_errcode(Handle);
    Error.ErrorMessage := UTF8ToString(sqlite3_errmsg(Handle));
    DoError(Error, EmptyToolsItem());
  end;

  inherited;
end;

procedure TTExportSQLite.ExecuteFooter();
begin
  inherited;

  SQLiteException(Handle, sqlite3_exec(Handle, PAnsiChar(UTF8Encode('COMMIT;')), nil, nil, nil));
  sqlite3_close(Handle);

  if (Success = daAbort) then
    DeleteFile(Filename);
end;

procedure TTExportSQLite.ExecuteTableFooter(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
begin
  sqlite3_finalize(Stmt); Stmt := nil;

  SetLength(Text, 0);
end;

procedure TTExportSQLite.ExecuteTableHeader(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  Field: TCTableField;
  I: Integer;
  SQL: string;
begin
  SQL := 'CREATE TABLE "' + Table.Name + '" (';
  for I := 0 to Length(Fields) - 1 do
  begin
    Field := Table.FieldByName(Fields[I].Name);

    if (I > 0) then SQL := SQL + ', ';
    SQL := SQL + Field.Name + ' ';
    case (Field.FieldType) of
      mfBit, mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt:
        begin
          SQL := SQL + 'INTEGER';
          if ((Table is TCBaseTable)
            and Assigned(TCBaseTable(Table).PrimaryKey)
            and (TCBaseTable(Table).PrimaryKey.Columns.Count = 1)
            and (TCBaseTable(Table).PrimaryKey.Columns[0].Field = Table.Fields[I])) then
            SQL := SQL + ' PRIMARY KEY';
        end;
      mfFloat, mfDouble, mfDecimal:
        SQL := SQL + 'REAL';
      mfDate, mfDateTime, mfTimeStamp, mfTime, mfYear,
      mfEnum, mfSet,
      mfChar, mfVarChar, mfTinyText, mfText, mfMediumText, mfLongText:
        SQL := SQL + 'TEXT';
      mfBinary, mfVarBinary, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob,
      mfGeometry, mfPoint, mfLineString, mfPolygon, mfMultiPoint, mfMultiLineString, mfMultiPolygon, mfGeometryCollection:
        SQL := SQL + 'BLOB';
    end;
  end;
  SQL := SQL + ');';
  SQLiteException(Handle, sqlite3_exec(Handle, PAnsiChar(UTF8Encode(SQL)), nil, nil, nil));


  SQL := 'INSERT INTO "' + Table.Name + '"';
  if (Length(DestinationFields) > 0) then
  begin
    SQL := SQL + '(';
    for I := 0 to Length(DestinationFields) - 1 do
    begin
      if (I > 0) then SQL := SQL + ',';
      SQL := SQL + '"' + DestinationFields[I].Name + '"';
    end;
    SQL := SQL + ')';
  end;
  SQL := SQL + ' VALUES (';
  for I := 0 to Length(Fields) - 1 do
  begin
    if (I > 0) then SQL := SQL + ',';
    SQL := SQL + '?' + IntToStr(1 + I)
  end;
  SQL := SQL + ');';
  SQLiteException(Handle, sqlite3_prepare_v2(Handle, PAnsiChar(UTF8Encode(SQL)), -1, @Stmt, nil));

  SetLength(Text, Length(Fields));
end;

procedure TTExportSQLite.ExecuteTableRecord(const Table: TCTable; const Fields: array of TField; const DataSet: TMySQLQuery);
var
  I: Integer;
begin
  for I := 0 to Length(Fields) - 1 do
    if (not Assigned(DataSet.LibRow^[I])) then
      sqlite3_bind_null(Stmt, 1 + I)
    else if (BitField(Fields[I])) then
      sqlite3_bind_blob(Stmt, 1 + I, DataSet.LibRow^[I], DataSet.LibLengths^[I], SQLITE_STATIC)
    else
      case (Fields[I].DataType) of
        ftString:
          sqlite3_bind_blob(Stmt, 1 + I, DataSet.LibRow^[I], DataSet.LibLengths^[I], SQLITE_STATIC);
        ftShortInt,
        ftByte,
        ftSmallInt,
        ftWord,
        ftInteger,
        ftLongWord,
        ftLargeint,
        ftSingle,
        ftFloat,
        ftExtended,
        ftDate,
        ftDateTime,
        ftTimestamp,
        ftTime:
          sqlite3_bind_text(Stmt, 1 + I, DataSet.LibRow^[I], DataSet.LibLengths^[I], SQLITE_STATIC);
        ftWideString,
        ftWideMemo:
          if ((Client.CodePage = CP_UTF8) or (DataSet.LibLengths^[I] = 0)) then
            sqlite3_bind_text(Stmt, 1 + I, DataSet.LibRow^[I], DataSet.LibLengths^[I], SQLITE_STATIC)
          else
          begin
            Text[I] := UTF8Encode(DataSet.GetAsString(Fields[I].FieldNo));
            sqlite3_bind_text(Stmt, 1 + I, PAnsiChar(@Text[I][1]), Length(Text[I]), SQLITE_STATIC);
          end;
        ftBlob:
          sqlite3_bind_blob(Stmt, 1 + I, DataSet.LibRow^[I], DataSet.LibLengths^[I], SQLITE_STATIC);
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + ' (%d)', [Fields[I].Name, Integer(Fields[I].DataType)]);
      end;

  SQLiteException(Handle, sqlite3_step(Stmt));
  SQLiteException(Handle, sqlite3_reset(Stmt));
end;

{ TTFind **********************************************************************}

procedure TTFind.Add(const Table: TCBaseTable; const Field: TCTableField = nil);
var
  Found: Boolean;
  I: Integer;
begin
  SetLength(Items, Length(Items) + 1);

  Items[Length(Items) - 1].DatabaseName := Table.Database.Name;
  Items[Length(Items) - 1].TableName := Table.Name;
  SetLength(Items[Length(Items) - 1].FieldNames, 0);
  Items[Length(Items) - 1].RecordsSum := 0;
  Items[Length(Items) - 1].RecordsFound := 0;
  Items[Length(Items) - 1].RecordsDone := 0;
  Items[Length(Items) - 1].Done := False;

  if (Assigned(Field)) then
  begin
    Found := False;
    for I := 0 to Length(Items[Length(Items) - 1].FieldNames) - 1 do
      Found := Found or (Items[Length(Items) - 1].FieldNames[I] = Field.Name);
    if (not Found) then
    begin
      SetLength(Items[Length(Items) - 1].FieldNames, Length(Items[Length(Items) - 1].FieldNames) + 1);
      Items[Length(Items) - 1].FieldNames[Length(Items[Length(Items) - 1].FieldNames) - 1] := Field.Name;
    end;
  end;
end;

procedure TTFind.AfterExecute();
begin
  Client.EndSilent();
  Client.EndSynchron();

  inherited;
end;

procedure TTFind.BeforeExecute();
begin
  inherited;

  Client.BeginSilent();
  Client.BeginSynchron(); // We're still in a thread
end;

constructor TTFind.Create(const AClient: TCClient);
begin
  inherited Create();

  FClient := AClient;

  SetLength(Items, 0);
  FItem := nil;
end;

destructor TTFind.Destroy();
var
  I: Integer;
begin
  for I := 0 to Length(Items) - 1 do
    SetLength(Items[I].FieldNames, 0);
  SetLength(Items, 0);

  inherited;
end;

function TTFind.DoExecuteSQL(const Client: TCClient; var Item: TItem; var SQL: string): Boolean;
begin
  Result := (Success = daSuccess) and Client.ExecuteSQL(SQL);
  Delete(SQL, 1, Client.ExecutedSQLLength);
  SQL := Trim(SQL);
end;

procedure TTFind.Execute();
var
  Database: TCDatabase;
  I: Integer;
  J: Integer;
  Table: TCBaseTable;
begin
  BeforeExecute();

  for I := 0 to Length(Items) - 1 do
  begin
    FItem := @Items[I];

    if (Success = daSuccess) then
    begin
      Table := Client.DatabaseByName(Items[I].DatabaseName).BaseTableByName(Items[I].TableName);

      if (Length(Items[I].FieldNames) = 0) then
      begin
        SetLength(Items[I].FieldNames, Table.Fields.Count);
        for J := 0 to Table.Fields.Count - 1 do
          Items[I].FieldNames[J] := Table.Fields[J].Name;
      end;

      if (Table.Rows >= 0) then
        Items[I].RecordsSum := Table.Rows
      else
        Items[I].RecordsSum := Table.CountRecords();

      DoUpdateGUI();
    end;

    FItem := nil;
  end;

  for I := 0 to Length(Items) - 1 do
  begin
    FItem := @Items[I];

    if (Success = daSuccess) then
    begin
      if ((Self is TTReplace) and TTReplace(Self).Backup) then
      begin
        BackupTable(ToolsItem(Items[I]));
        if (Success = daFail) then Success := daSuccess;
      end;

      if (Success = daSuccess) then
      begin
        Database := Client.DatabaseByName(Items[I].DatabaseName);
        Table := Database.BaseTableByName(Items[I].TableName);

        if (RegExpr or (not WholeValue and not MatchCase)) then
          ExecuteDefault(Items[I], Table)
        else if (WholeValue) then
          ExecuteWholeValue(Items[I], Table)
        else
          ExecuteMatchCase(Items[I], Table);

        if (Self is TTReplace) then
          Table.InvalidateData();
      end;

      Items[I].Done := Success <> daAbort;

      if (Success = daFail) then Success := daSuccess;

      DoUpdateGUI();
    end;

    FItem := nil;
  end;

  AfterExecute();
end;

procedure TTFind.ExecuteDefault(var Item: TItem; const Table: TCBaseTable);
var
  Buffer: TTStringBuffer;
  DataSet: TMySQLQuery;
  Fields: array of TField;
  Found: Boolean;
  I: Integer;
  J: Integer;
  NewValue: string;
  PerlRegEx: TPerlRegEx;
  SQL: string;
  Value: string;
  WhereClausel: string;
begin
  if (Success = daSuccess) then
  begin
    if (not (Self is TTReplace) and not RegExpr) then
      SQL := 'COUNT(*)'
    else if (Length(Item.FieldNames) = Table.Fields.Count) then
      SQL := '*'
    else
    begin
      SQL := '';
      for I := 0 to Table.Fields.Count - 1 do
        if (Table.Fields[I].InPrimaryKey) then
        begin
          if (SQL <> '') then SQL := SQL + ',';
          SQL := SQL + Client.EscapeIdentifier(Table.Fields[I].Name);
        end
        else
          for J := 0 to Length(Item.FieldNames) - 1 do
            if (Item.FieldNames[J] = Table.Fields[I].Name) then
            begin
              if (SQL <> '') then SQL := SQL + ',';
              SQL := SQL + Client.EscapeIdentifier(Table.Fields[J].Name);
            end;
    end;

    WhereClausel := '';
    for I := 0 to Length(Item.FieldNames) - 1 do
    begin
      if (I > 0) then WhereClausel := WhereClausel + ' OR ';
      if (not RegExpr) then
        WhereClausel := WhereClausel + Client.EscapeIdentifier(Item.FieldNames[I]) + ' LIKE ' + SQLEscape('%' + FindText + '%')
      else
        WhereClausel := WhereClausel + Client.EscapeIdentifier(Item.FieldNames[I]) + ' REGEXP ' + SQLEscape(FindText);
    end;
    SQL := 'SELECT ' + SQL + ' FROM ' + Client.EscapeIdentifier(Item.DatabaseName) + '.' + Client.EscapeIdentifier(Item.TableName) + ' WHERE ' + WhereClausel;

    DataSet := TMySQLQuery.Create(nil);
    DataSet.Connection := Client;
    DataSet.CommandText := SQL;

    while ((Success = daSuccess) and not DataSet.Active) do
    begin
      DataSet.Open();
      if (not DataSet.Active) then
        DoError(DatabaseError(Client), ToolsItem(Item), SQL);
    end;

    if (Success = daSuccess) then
    begin
      if (DataSet.IsEmpty()) then
        Item.RecordsFound := 0
      else if (not (Self is TTReplace) and not RegExpr) then
        Item.RecordsFound := DataSet.Fields[0].AsInteger
      else
      begin
        SetLength(Fields, 0);
        for I := 0 to Length(Item.FieldNames) - 1 do
          if (Assigned(DataSet.FindField(Item.FieldNames[I]))) then
          begin
            SetLength(Fields, Length(Fields) + 1);
            Fields[Length(Fields) - 1] := DataSet.FindField(Item.FieldNames[I]);
          end;

        if (not RegExpr) then
          PerlRegEx := nil
        else
        begin
          PerlRegEx := TPerlRegEx.Create();
          PerlRegEx.RegEx := UTF8Encode(FindText);
          if (MatchCase) then
            PerlRegEx.Options := PerlRegEx.Options - [preCaseLess]
          else
            PerlRegEx.Options := PerlRegEx.Options + [preCaseLess];
          PerlRegEx.Study();
          if (Self is TTReplace) then
            PerlRegEx.Replacement := UTF8Encode(TTReplace(Self).ReplaceText);
        end;

        if (not (Self is TTReplace)) then
          Buffer := nil
        else
        begin
          TTReplace(Self).ReplaceConnection.StartTransaction();

          Buffer := TTStringBuffer.Create(SQLPacketSize);

          if (Item.DatabaseName <> TTReplace(Self).ReplaceConnection.DatabaseName) then
            Buffer.Write(TTReplace(Self).ReplaceConnection.SQLUse(Item.DatabaseName));
        end;

        repeat
          Found := False; SQL := '';
          for I := 0 to Length(Fields) - 1 do
            if (Assigned(DataSet.LibRow^[Fields[I].FieldNo - 1])) then
            begin
              Value := DataSet.GetAsString(Fields[I].FieldNo);

              if (not (Self is TTReplace)) then
                if (not RegExpr) then
                  // will never occur, since without RegExpr COUNT(*) will be used
                else
                begin
                  // not MatchCase, since otherwise ExecuteMatchCase will be used
                  PerlRegEx.Subject := UTF8Encode(Value);
                  Found := Found or PerlRegEx.Match();
                end
              else
              begin
                if (not RegExpr) then
                begin
                  // not MatchCase, since otherwise ExecuteMatchCase will be used
                  NewValue := StringReplace(Value, FindText, TTReplace(Self).ReplaceText, [rfReplaceAll, rfIgnoreCase]);
                  Found := NewValue <> Value;
                end
                else
                begin
                  PerlRegEx.Subject := UTF8Encode(Value);
                  Found := PerlRegEx.ReplaceAll();
                  if (Found) then
                    NewValue := UTF8ToWideString(PerlRegEx.Subject);
                end;

                if (Found) then
                  if (SQL <> '') then SQL := SQL + ',';
                  SQL := SQL + Client.EscapeIdentifier(Fields[I].FieldName) + '=';
                  if (BitField(Fields[I])) then
                    SQL := SQL + 'b''' + Fields[I].AsString + ''''
                  else if (Fields[I].DataType in UnquotedDataTypes) then
                    SQL := SQL + NewValue
                  else
                    SQL := SQL + SQLEscape(NewValue);
              end;
            end;

          if (not (Self is TTReplace)) then
          begin
            if (Found) then
              Inc(Item.RecordsFound);
          end
          else
            if (SQL <> '') then
            begin
              Inc(Item.RecordsFound);

              SQL := 'UPDATE ' + Client.EscapeIdentifier(Item.TableName) + ' SET ' + SQL + ' WHERE ';
              Found := False;
              for I := 0 to Length(Fields) - 1 do
                begin
                  if (Found) then SQL := SQL + ' AND ';
                  SQL := SQL + Client.EscapeIdentifier(Fields[I].FieldName) + '=';
                  if (not Assigned(DataSet.LibRow^[I])) then
                    SQL := SQL + 'NULL'
                  else if (BitField(Fields[I])) then
                    SQL := SQL + 'b''' + Fields[I].AsString + ''''
                  else if (Fields[I].DataType in UnquotedDataTypes + [ftTimestamp]) then
                    SQL := SQL + DataSet.GetAsString(Fields[I].FieldNo)
                  else if (Fields[I].DataType in [ftDate, ftDateTime, ftTime]) then
                    SQL := SQL + '''' + DataSet.GetAsString(Fields[I].FieldNo) + ''''
                  else
                    SQL := SQL + SQLEscape(DataSet.GetAsString(Fields[I].FieldNo));
                  Found := True;
                end;
              SQL := SQL + ';' + #13#10;

              Buffer.Write(SQL);

              if ((Buffer.Size > 0) and (not Client.MultiStatements or (Buffer.Size >= SQLPacketSize))) then
              begin
                SQL := Buffer.Read();
                DoExecuteSQL(TTReplace(Self).ReplaceConnection, Item, SQL);
                Buffer.Write(SQL);
              end;
            end;

          Inc(Item.RecordsDone);
          if (Item.RecordsDone mod 100 = 0) then DoUpdateGUI();

          if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
            Success := daAbort;
        until ((Success <> daSuccess) or not DataSet.FindNext());

        if (Buffer.Size > 0) then
        begin
          SQL := Buffer.Read();
          DoExecuteSQL(TTReplace(Self).ReplaceConnection, Item, SQL);
        end;

        Buffer.Free();

        if (Self is TTReplace) then
        begin
          if (Success = daSuccess) then
            TTReplace(Self).ReplaceConnection.CommitTransaction()
          else
            TTReplace(Self).ReplaceConnection.RollbackTransaction();
        end;

        if (Assigned(PerlRegEx)) then
          PerlRegEx.Free();
      end;
    end;

    DataSet.Free();
  end;
end;

procedure TTFind.ExecuteMatchCase(var Item: TItem; const Table: TCBaseTable);
var
  DataSet: TMySQLQuery;
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Length(Item.FieldNames) - 1 do
  begin
    if (I > 0) then SQL := SQL + ' OR ';
    SQL := SQL + 'BINARY(' + Client.EscapeIdentifier(Item.FieldNames[I]) + ') LIKE BINARY(' + SQLEscape('%' + FindText + '%') + ')';
  end;
  SQL := 'SELECT COUNT(*) FROM ' + Client.EscapeIdentifier(Table.Database.Name) + '.' + Client.EscapeIdentifier(Table.Name) + ' WHERE ' + SQL;

  DataSet := TMySQLQuery.Create(nil);
  DataSet.Connection := Client;
  DataSet.CommandText := SQL;
  while ((Success = daSuccess) and not DataSet.Active) do
  begin
    DataSet.Open();
    if (Client.ErrorCode > 0) then
      DoError(DatabaseError(Client), ToolsItem(Item), SQL);
  end;

  if ((Success = daSuccess) and not DataSet.IsEmpty()) then
    Item.RecordsFound := DataSet.Fields[0].AsInteger;

  DataSet.Free();
end;

procedure TTFind.ExecuteWholeValue(var Item: TItem; const Table: TCBaseTable);
var
  DataSet: TMySQLQuery;
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Length(Item.FieldNames) - 1 do
  begin
    if (I > 0) then SQL := SQL + ' OR ';
    if (MatchCase) then
      SQL := SQL + 'BINARY(' + Client.EscapeIdentifier(Item.FieldNames[I]) + ')=BINARY(' + SQLEscape(FindText) + ')'
    else
      SQL := SQL + Client.EscapeIdentifier(Item.FieldNames[I]) + '=' + SQLEscape(FindText)
  end;
  SQL := 'SELECT COUNT(*) FROM ' + Client.EscapeIdentifier(Table.Database.Name) + '.' + Client.EscapeIdentifier(Table.Name) + ' WHERE ' + SQL;

  DataSet := TMySQLQuery.Create(nil);
  DataSet.Connection := Client;
  DataSet.CommandText := SQL;

  while ((Success = daSuccess) and not DataSet.Active) do
  begin
    DataSet.Open();
    if (Client.ErrorCode > 0) then
      DoError(DatabaseError(Client), ToolsItem(Item), SQL);
  end;

  if ((Success = daSuccess) and not DataSet.IsEmpty()) then
    Item.RecordsFound := DataSet.Fields[0].AsInteger;

  FreeAndNil(DataSet);

  if (Self is TTReplace) then
  begin
    SQL := '';
    if (Client.DatabaseName = Table.Database.Name) then
      SQL := SQL + Table.Database.SQLUse();

    for I := 0 to Length(Item.FieldNames) - 1 do
    begin
      SQL := SQL + 'UPDATE ' + Client.EscapeIdentifier(Table.Database.Name) + '.' + Client.EscapeIdentifier(Item.TableName);
      SQL := SQL + ' SET ' + Client.EscapeIdentifier(Item.FieldNames[I]) + '=' + SQLEscape(TTReplace(Self).ReplaceText);
      if (MatchCase) then
        SQL := SQL + ' WHERE BINARY(' + Client.EscapeIdentifier(Item.FieldNames[I]) + ')=BINARY(' + SQLEscape(FindText) + ')'
      else
        SQL := SQL + ' WHERE ' + Client.EscapeIdentifier(Item.FieldNames[I]) + '=' + SQLEscape(FindText);
      SQL := SQL + ';' + #13#10;
    end;

    while ((Success = daSuccess) and not DoExecuteSQL(TTReplace(Self).ReplaceConnection, Item, SQL)) do
      if (Client.ErrorCode = ER_TRUNCATED_WRONG_VALUE) then
      begin
        Delete(SQL, 1, Length(Client.CommandText));
        Success := daSuccess;
      end
      else
        DoError(DatabaseError(Client), ToolsItem(Item), SQL);

    Item.RecordsDone := Item.RecordsSum;
  end;
end;

function TTFind.ToolsItem(const Item: TItem): TTools.TItem;
begin
  Result.Client := Client;
  Result.DatabaseName := Item.DatabaseName;
  Result.TableName := Item.TableName;
end;

procedure TTFind.DoUpdateGUI();
var
  I: Integer;
begin
  if (Assigned(OnUpdate)) then
  begin
    CriticalSection.Enter();

    ProgressInfos.TablesDone := 0;
    ProgressInfos.TablesSum := Length(Items);
    ProgressInfos.RecordsDone := 0;
    ProgressInfos.RecordsSum := 0;
    ProgressInfos.TimeDone := 0;
    ProgressInfos.TimeSum := 0;

    for I := 0 to Length(Items) - 1 do
    begin
      if (Items[I].Done) then
      begin
        Inc(ProgressInfos.TablesDone);
        Inc(ProgressInfos.RecordsDone, Items[I].RecordsSum);
      end;

      Inc(ProgressInfos.RecordsSum, Items[I].RecordsSum);
    end;

    ProgressInfos.TimeDone := Now() - StartTime;

    if ((ProgressInfos.RecordsDone = 0) and (ProgressInfos.TablesDone = 0)) then
    begin
      ProgressInfos.Progress := 0;
      ProgressInfos.TimeSum := 0;
    end
    else if (ProgressInfos.TablesDone < ProgressInfos.TablesSum) then
    begin
      ProgressInfos.Progress := Round(ProgressInfos.TablesDone / ProgressInfos.TablesSum * 100);
      ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.TablesDone * ProgressInfos.TablesSum;
    end
    else
    begin
      ProgressInfos.Progress := 100;
      ProgressInfos.TimeSum := ProgressInfos.TimeDone;
    end;

    CriticalSection.Leave();

    OnUpdate(ProgressInfos);
  end;
end;

{ TTReplace *******************************************************************}

constructor TTReplace.Create(const AClient, AReplaceClient: TCClient);
begin
  inherited Create(AClient);

  FReplaceClient := AReplaceClient;
end;

procedure TTReplace.ExecuteMatchCase(var Item: TTFind.TItem; const Table: TCBaseTable);
var
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Length(Item.FieldNames) - 1 do
  begin
    if (I > 0) then SQL := SQL + ',';
    SQL := SQL + Client.EscapeIdentifier(Item.FieldNames[I]) + '=REPLACE(' + Client.EscapeIdentifier(Item.FieldNames[I]) + ',' + SQLEscape(FindText) + ',' + SQLEscape(ReplaceText) + ')';
  end;
  SQL := 'UPDATE ' + Client.EscapeIdentifier(Item.DatabaseName) + '.' + Client.EscapeIdentifier(Item.TableName) + ' SET ' + SQL + ';';

  while ((Success = daSuccess) and not Client.ExecuteSQL(SQL)) do
    DoError(DatabaseError(Client), ToolsItem(Item), SQL);

  Item.RecordsDone := Client.RowsAffected;
  Item.RecordsSum := Item.RecordsDone;
end;

{ TTTransfer  ******************************************************************}

procedure TTTransfer.Add(const SourceClient: TCClient; const SourceDatabaseName, SourceTableName: string; const DestinationClient: TCClient; const DestinationDatabaseName, DestinationTableName: string);
var
  Element: ^TElement;
begin
  GetMem(Element, SizeOf(Element^));
  ZeroMemory(Element, SizeOf(Element^));

  Element^.Source.Client := SourceClient;
  Element^.Source.DatabaseName := SourceDatabaseName;
  Element^.Source.TableName := SourceTableName;
  Element^.Destination.Client := DestinationClient;
  Element^.Destination.DatabaseName := DestinationDatabaseName;
  Element^.Destination.TableName := DestinationTableName;

  Elements.Add(Element);
end;

procedure TTTransfer.AfterExecute();
begin
  if (Elements.Count > 0) then
  begin
    TElement(Elements[0]^).Source.Client.EndSilent();
    TElement(Elements[0]^).Source.Client.EndSynchron();

    TElement(Elements[0]^).Destination.Client.EndSilent();
    TElement(Elements[0]^).Destination.Client.EndSynchron();
  end;

  inherited;
end;

procedure TTTransfer.BeforeExecute();
begin
  inherited;

  if (Elements.Count > 0) then
  begin
    TElement(Elements[0]^).Source.Client.BeginSilent();
    TElement(Elements[0]^).Source.Client.BeginSynchron(); // We're still in a thread

    TElement(Elements[0]^).Destination.Client.BeginSilent();
    TElement(Elements[0]^).Destination.Client.BeginSynchron(); // We're still in a thread
  end;
end;

procedure TTTransfer.CloneTable(var Source, Destination: TItem);
var
  DestinationDatabase: TCDatabase;
  SourceDatabase: TCDatabase;
  SourceTable: TCBaseTable;
begin
  SourceDatabase := Source.Client.DatabaseByName(Source.DatabaseName);
  DestinationDatabase := Destination.Client.DatabaseByName(Destination.DatabaseName);

  SourceTable := SourceDatabase.BaseTableByName(Source.TableName);

  while ((Success = daSuccess) and not DestinationDatabase.CloneTable(SourceTable, Destination.TableName, Data)) do
    DoError(DatabaseError(Source.Client), ToolsItem(Destination));

  if (Success = daSuccess) then
  begin
    Destination.Done := True;
    if (Data) then
    begin
      Destination.RecordsSum := DestinationDatabase.BaseTableByName(Destination.TableName).CountRecords();
      Destination.RecordsDone := Destination.RecordsSum;
    end;
  end;

  DoUpdateGUI();
end;

constructor TTTransfer.Create();
begin
  inherited;

  Elements := TList.Create();
end;

destructor TTTransfer.Destroy();
begin
  while (Elements.Count > 0) do
  begin
    FreeMem(Elements[0]);
    Elements.Delete(0);
  end;
  Elements.Free();

  inherited;
end;

function TTTransfer.DifferentPrimaryIndexError(): TTools.TError;
begin
  Result.ErrorType := TE_DifferentPrimaryIndex;
end;

function TTTransfer.DoExecuteSQL(var Item: TItem; const Client: TCClient; var SQL: string): Boolean;
begin
  Result := (Success = daSuccess) and Client.ExecuteSQL(SQL);
  Delete(SQL, 1, Client.ExecutedSQLLength);
  SQL := Trim(SQL);
end;

procedure TTTransfer.DoUpdateGUI();
var
  I: Integer;
begin
  if (Assigned(OnUpdate)) then
  begin
    CriticalSection.Enter();

    ProgressInfos.TablesDone := 0;
    ProgressInfos.TablesSum := Elements.Count;
    ProgressInfos.RecordsDone := 0;
    ProgressInfos.RecordsSum := 0;
    ProgressInfos.TimeDone := 0;
    ProgressInfos.TimeSum := 0;

    for I := 0 to Elements.Count - 1 do
    begin
      if (TElement(Elements[I]^).Destination.Done) then
        Inc(ProgressInfos.TablesDone);
      Inc(ProgressInfos.RecordsDone, TElement(Elements[I]^).Destination.RecordsDone);
      Inc(ProgressInfos.RecordsSum, TElement(Elements[I]^).Destination.RecordsSum);
    end;

    ProgressInfos.TimeDone := Now() - StartTime;

    if ((ProgressInfos.RecordsDone = 0) and (ProgressInfos.TablesDone = 0)) then
    begin
      ProgressInfos.Progress := 0;
      ProgressInfos.TimeSum := 0;
    end
    else if (ProgressInfos.RecordsDone = 0) then
    begin
      ProgressInfos.Progress := Round(ProgressInfos.TablesDone / ProgressInfos.TablesSum * 100);
      ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.TablesDone * ProgressInfos.TablesSum;
    end
    else if (ProgressInfos.RecordsDone < ProgressInfos.RecordsSum) then
    begin
      ProgressInfos.Progress := Round(ProgressInfos.RecordsDone / ProgressInfos.RecordsSum * 100);
      ProgressInfos.TimeSum := ProgressInfos.TimeDone / ProgressInfos.RecordsDone * ProgressInfos.RecordsSum;
    end
    else
    begin
      ProgressInfos.Progress := 100;
      ProgressInfos.TimeSum := ProgressInfos.TimeDone;
    end;

    CriticalSection.Leave();

    OnUpdate(ProgressInfos);
  end;
end;

procedure TTTransfer.Execute();
var
  DataSet: TMySQLQuery;
  DestinationClient: TCClient;
  I: Integer;
  OLD_FOREIGN_KEY_CHECKS: string;
  OLD_UNIQUE_CHECKS: string;
  SourceClient: TCClient;
  SourceTable: TCBaseTable;
  SQL: string;
begin
  SourceClient := TElement(Elements[0]^).Source.Client;
  DestinationClient := TElement(Elements[0]^).Destination.Client;

  BeforeExecute();

  for I := 0 to Elements.Count - 1 do
  begin
    SourceTable := SourceClient.DatabaseByName(TElement(Elements[I]^).Source.DatabaseName).BaseTableByName(TElement(Elements[I]^).Source.TableName);

    TElement(Elements[I]^).Source.Done := False;
    TElement(Elements[I]^).Destination.Done := False;

    TElement(Elements[I]^).Source.RecordsSum := 0;
    TElement(Elements[I]^).Source.RecordsDone := 0;

    TElement(Elements[I]^).Destination.RecordsSum := SourceTable.Rows;
    TElement(Elements[I]^).Destination.RecordsDone := 0;

    DoUpdateGUI();
  end;

  if (DisableKeys) then
  begin
    if (DestinationClient.ServerVersion >= 40014) then
    begin
      DataSet := TMySQLQuery.Create(nil);
      DataSet.Connection := DestinationClient;
      DataSet.CommandText := 'SELECT @@UNIQUE_CHECKS, @@FOREIGN_KEY_CHECKS';

      while ((Success = daSuccess) and not DataSet.Active) do
      begin
        DataSet.Open();
        if (DestinationClient.ErrorCode > 0) then
          DoError(DatabaseError(DestinationClient), ToolsItem(TElement(Elements[0]^).Destination), SQL);
      end;

      if (Success = daSuccess) then
      begin
        OLD_UNIQUE_CHECKS := DataSet.Fields[0].AsString;
        OLD_FOREIGN_KEY_CHECKS := DataSet.Fields[1].AsString;
        DataSet.Close();

        SQL := 'SET UNIQUE_CHECKS=0, FOREIGN_KEY_CHECKS=0;';
        while ((Success = daSuccess) and not DestinationClient.ExecuteSQL(SQL)) do
          DoError(DatabaseError(DestinationClient), ToolsItem(TElement(Elements[0]^).Destination), SQL);
      end;

      FreeAndNil(DataSet);
    end;
  end;

  if (SourceClient = DestinationClient) then
  begin

    for I := 0 to Elements.Count - 1 do
      if (Success <> daAbort) then
      begin
        Success := daSuccess;

        CloneTable(TElement(Elements[I]^).Source, TElement(Elements[I]^).Destination);
      end;
  end
  else
  begin
    if (Success <> daAbort) then
    begin
      SQL := '';
      if (Data) then
        for I := 0 to Elements.Count - 1 do
        begin
          SourceTable := SourceClient.DatabaseByName(TElement(Elements[I]^).Source.DatabaseName).BaseTableByName(TElement(Elements[I]^).Source.TableName);

          SQL := SQL + 'SELECT * FROM ' + SourceClient.EscapeIdentifier(SourceTable.Database.Name) + '.' + SourceClient.EscapeIdentifier(SourceTable.Name) + ';' + #13#10;
        end;

      for I := 0 to Elements.Count - 1 do
        if (Success <> daAbort) then
        begin
          Success := daSuccess;

          if (Data) then
            if (I = 0) then
              while ((Success = daSuccess) and not SourceClient.FirstResult(DataHandle, SQL)) do
                DoError(DatabaseError(SourceClient), ToolsItem(TElement(Elements[I]^).Source), SQL)
            else
              if (not SourceClient.NextResult(DataHandle)) then
                DoError(DatabaseError(SourceClient), ToolsItem(TElement(Elements[I]^).Source));

          ExecuteTable(TElement(Elements[I]^).Source, TElement(Elements[I]^).Destination);
        end;

      if (Data) then
        SourceClient.CloseResult(DataHandle);
    end;

    // Handle Foreign Keys after tables executed to have more parent tables available
    for I := 0 to Elements.Count - 1 do
      if (Success <> daAbort) then
      begin
        Success := daSuccess;

        ExecuteForeignKeys(TElement(Elements[I]^).Source, TElement(Elements[I]^).Destination);
        if (Success = daFail) then Success := daSuccess;
      end;
  end;

  if (DisableKeys) then
  begin
    if (DestinationClient.ServerVersion >= 40014) then
    begin
      SQL := 'SET UNIQUE_CHECKS=' + OLD_UNIQUE_CHECKS + ', FOREIGN_KEY_CHECKS=' + OLD_FOREIGN_KEY_CHECKS + ';' + #13#10;
      while ((Success = daSuccess) and not DestinationClient.ExecuteSQL(SQL)) do
        DoError(DatabaseError(DestinationClient), ToolsItem(TElement(Elements[0]^).Destination), SQL);
    end;
  end;

  AfterExecute();
end;

procedure TTTransfer.ExecuteData(var Source, Destination: TItem);
var
  Buffer: TTStringBuffer;
  DBValues: RawByteString;
  DestinationDatabase: TCDatabase;
  DestinationFieldNames: string;
  DestinationPrimaryIndexFieldNames: string;
  DestinationTable: TCBaseTable;
  Error: TTools.TError;
  FieldCount: Integer;
  FieldInfo: TFieldInfo;
  FilenameP: array [0 .. MAX_PATH] of Char;
  I: Integer;
  InsertStmtInBuffer: Boolean;
  J: Integer;
  Pipe: THandle;
  Pipename: string;
  S: string;
  SourceDatabase: TCDatabase;
  SourceDataSet: TMySQLQuery;
  SourceFieldNames: string;
  SourceFields: array of Integer;
  SourceTable: TCBaseTable;
  SourceValues: string;
  SQL: string;
  SQLThread: TSQLThread;
  SQLValues: TSQLStrings;
  Update: Boolean;
  Values: string;
  WhereClausel: string;
  WrittenSize: Cardinal;
begin
  SourceValues := ''; FilenameP[0] := #0;
  SourceDatabase := Source.Client.DatabaseByName(Source.DatabaseName);
  DestinationDatabase := Destination.Client.DatabaseByName(Destination.DatabaseName);
  SourceTable := SourceDatabase.BaseTableByName(Source.TableName);
  DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);

  FieldCount := 0;
  for I := 0 to SourceTable.Fields.Count - 1 do
    for J := 0 to DestinationTable.Fields.Count - 1 do
      if (lstrcmpi(PChar(SourceTable.Fields[I].Name), PChar(DestinationTable.Fields[J].Name)) = 0) then
        Inc(FieldCount);

  if (FieldCount > 0) then
  begin
    if ((Success = daSuccess) and DisableKeys and (Destination.Client.ServerVersion >= 40000)) then
    begin
      SQL := 'ALTER TABLE ' + Destination.Client.EscapeIdentifier(DestinationTable.Name) + ' DISABLE KEYS;';
      if (not Destination.Client.ExecuteSQL(SQL)) then
        DoError(DatabaseError(Destination.Client), ToolsItem(Destination), SQL);
    end;

    SourceFieldNames := '';

    if (Success = daSuccess) then
    begin
      SourceDataSet := TMySQLQuery.Create(nil);
      SourceDataSet.Connection := Source.Client;
      SourceDataSet.Open(DataHandle);

      SetLength(SourceFields, DestinationTable.Fields.Count);
      for I := 0 to DestinationTable.Fields.Count - 1 do
      begin
        SourceFields[Length(SourceFields) - 1] := -1;
        for J := 0 to SourceDataSet.FieldCount - 1 do
          if (GetFieldInfo(SourceDataSet.Fields[J].Origin, FieldInfo) and (lstrcmpi(PChar(FieldInfo.OriginalFieldName), PChar(DestinationTable.Fields[I].Name)) = 0)) then
          begin
            SourceFields[I] := J;

            if (DestinationFieldNames <> '') then DestinationFieldNames := DestinationFieldNames + ',';
            DestinationFieldNames := DestinationFieldNames + Destination.Client.EscapeIdentifier(DestinationTable.Fields[I].Name);
          end;
      end;

      if ((Success = daSuccess) and (DestinationFieldNames <> '') and not SourceDataSet.IsEmpty()) then
      begin
        Destination.Client.StartTransaction();

        if (Destination.Client.LoadDataFile) then
        begin
          Pipename := '\\.\pipe\' + LoadStr(1000);
          Pipe := CreateNamedPipe(PChar(Pipename),
                                  PIPE_ACCESS_OUTBOUND, PIPE_TYPE_MESSAGE or PIPE_READMODE_BYTE or PIPE_WAIT,
                                  1, NET_BUFFER_LENGTH, 0, NMPWAIT_USE_DEFAULT_WAIT, nil);
          if (Pipe = INVALID_HANDLE_VALUE) then
          begin
            Error.ErrorType := TE_File;
            Error.ErrorCode := GetLastError();
            Error.ErrorMessage := SysErrorMessage(GetLastError());
            DoError(Error, ToolsItem(Destination));
            Success := daAbort;
          end
          else
          begin
            SQL := '';
            if (DestinationDatabase.Name <> Destination.Client.DatabaseName) then
              SQL := SQL + DestinationDatabase.SQLUse();
            SQL := SQL + SQLLoadDataInfile(DestinationDatabase, False, False, Pipename, Destination.Client.Charset, DestinationDatabase.Name, DestinationTable.Name, DestinationFieldNames, '''', ',', #13#10);

            SQLThread := TSQLThread.Create(Destination.Client, SQL);

            if (ConnectNamedPipe(Pipe, nil)) then
            begin
              repeat
                DBValues := '';
                for I := 0 to DestinationTable.Fields.Count - 1 do
                  if (SourceFields[I] >= 0) then
                  begin
                    if (DBValues <> '') then DBValues := DBValues + ',';
                    if (not Assigned(SourceDataSet.LibRow^[SourceFields[I]])) then
                      DBValues := DBValues + 'NULL'
                    else if (BitField(SourceDataSet.Fields[SourceFields[I]])) then
                      DBValues := DBValues + DataFileValue(SourceDataSet.Fields[SourceFields[I]].AsString, not (DestinationTable.Fields[I].FieldType in NotQuotedFieldTypes))
                    else if (DestinationTable.Fields[I].FieldType in NotQuotedFieldTypes) then
                    begin
                      SetLength(DBValues, Length(DBValues) + SourceDataSet.LibLengths^[SourceFields[I]]);
                      MoveMemory(@DBValues[1 + Length(DBValues) - SourceDataSet.LibLengths^[SourceFields[I]]], SourceDataSet.LibRow^[SourceFields[I]], SourceDataSet.LibLengths^[SourceFields[I]]);
                    end
                    else if (DestinationTable.Fields[I].FieldType in TextFieldTypes) then
                      if ((Destination.Client.CodePage = SourceDataSet.Connection.CodePage) or (DestinationTable.Fields[I].FieldType in BinaryFieldTypes)) then
                        DBValues := DBValues + DataFileEscape(SourceDataSet.LibRow^[SourceFields[I]], SourceDataSet.LibLengths^[SourceFields[I]])
                      else
                        DBValues := DBValues + DataFileEscape(Destination.Client.LibEncode(SourceDataSet.GetAsString(SourceDataSet.Fields[SourceFields[I]].FieldNo)))
                    else
                      DBValues := DBValues + DataFileEscape(SourceDataSet.LibRow^[SourceFields[I]], SourceDataSet.LibLengths^[SourceFields[I]]);
                  end;
                DBValues := DBValues + #13#10;

                if (not WriteFile(Pipe, PAnsiChar(DBValues)^, Length(DBValues), WrittenSize, nil) or (Abs(WrittenSize) < Length(DBValues))) then
                begin
                  Error.ErrorType := TE_File;
                  Error.ErrorCode := GetLastError();
                  Error.ErrorMessage := SysErrorMessage(GetLastError());
                  DoError(Error, ToolsItem(Destination));
                  Success := daAbort;
                end;

                Inc(Destination.RecordsDone);
                if (Destination.RecordsDone mod 100 = 0) then DoUpdateGUI();

                if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
                  Success := daAbort;
              until ((Success <> daSuccess) or not SourceDataSet.FindNext());

              if (FlushFileBuffers(Pipe) and WriteFile(Pipe, PAnsiChar(DBValues)^, 0, WrittenSize, nil) and FlushFileBuffers(Pipe)) then
                SQLThread.WaitFor();
              DisconnectNamedPipe(Pipe);

              if (Destination.Client.ErrorCode <> 0) then
                DoError(DatabaseError(Destination.Client), ToolsItem(Destination), SQL);
            end;

            FreeAndNil(SQLThread);
            CloseHandle(Pipe);
          end;
        end
        else
        begin
          DestinationPrimaryIndexFieldNames := '';
          for I := 0 to DestinationTable.Fields.Count - 1 do
            if (DestinationTable.Fields[I].InPrimaryKey) then
            begin
              if (DestinationPrimaryIndexFieldNames <> '') then DestinationPrimaryIndexFieldNames := DestinationPrimaryIndexFieldNames + ';';
              DestinationPrimaryIndexFieldNames := DestinationPrimaryIndexFieldNames + DestinationTable.Fields[I].Name;
            end;

          Buffer := TTStringBuffer.Create(SQLPacketSize);
          InsertStmtInBuffer := False;

          if (DestinationDatabase.Name <> Destination.Client.DatabaseName) then
            Buffer.Write(DestinationDatabase.SQLUse());

          SetLength(SQLValues, DestinationTable.Fields.Count);
          repeat
            for I := 0 to DestinationTable.Fields.Count - 1 do
              if (SourceFields[I] >= 0) then
                SQLValues[I] := SourceDataSet.SQLFieldValue(SourceDataSet.Fields[SourceFields[I]]);

            Update := False;
            Values := ''; WhereClausel := '';
            for I := 0 to DestinationTable.Fields.Count - 1 do
              if (SourceFields[I] >= 0) then
                if (not Update) then
                begin
                  if (Values <> '') then Values := Values + ',';
                  Values := Values + SQLValues[I];
                end
                else if (not DestinationTable.Fields[I].InPrimaryKey) then
                begin
                  if (Values <> '') then Values := Values + ',';
                  Values := Values + Destination.Client.EscapeIdentifier(DestinationTable.Fields[I].Name) + '=' + SQLValues[I];
                end
                else
                begin
                  if (WhereClausel <> '') then WhereClausel := WhereClausel + ' AND ';
                  WhereClausel := WhereClausel + Destination.Client.EscapeIdentifier(DestinationTable.Fields[I].Name) + '=' + SQLValues[I];
                end;

            if (Update) then
            begin
              if (InsertStmtInBuffer) then
              begin
                Buffer.Write(';' + #13#10);
                InsertStmtInBuffer := False;
              end;
              Buffer.Write(SQLUpdate(DestinationTable, Values, WhereClausel));
            end
            else if (not InsertStmtInBuffer) then
            begin
              SQL := 'INSERT INTO ';
              SQL := SQL + Destination.Client.EscapeIdentifier(DestinationTable.Name);
              if (DestinationFieldNames <> '') then
                SQL := SQL + ' (' + DestinationFieldNames + ')';
              SQL := SQL + ' VALUES (' + Values + ')';
              Buffer.Write(SQL);
              InsertStmtInBuffer := True;
            end
            else
              Buffer.Write(',(' + Values + ')');

            if ((Buffer.Size > 0) and (Update and not Destination.Client.MultiStatements or (Buffer.Size >= SQLPacketSize))) then
            begin
              if (InsertStmtInBuffer) then
              begin
                Buffer.Write(';' + #13#10);
                InsertStmtInBuffer := False;
              end;
              S := Buffer.Read();
              while ((Success = daSuccess) and not DoExecuteSQL(Destination, Destination.Client, S)) do
                DoError(DatabaseError(Destination.Client), ToolsItem(Destination), S);
              Buffer.Write(S);
            end;

            Inc(Destination.RecordsDone);
            if (Destination.RecordsDone mod 100 = 0) then DoUpdateGUI();

            if (WaitForSingleObject(UserAbort, 0) = WAIT_OBJECT_0) then
              Success := daAbort;
          until ((Success <> daSuccess) or not SourceDataSet.FindNext());

          if (InsertStmtInBuffer) then
            Buffer.Write(';' + #13#10);
          S := Buffer.Read();
          while ((Success = daSuccess) and (S <> '') and not DoExecuteSQL(Destination, Destination.Client, S)) do
            DoError(DatabaseError(Destination.Client), ToolsItem(Destination), S);

          FreeAndNil(Buffer);
        end;

        if (Success = daSuccess) then
          Destination.Client.CommitTransaction()
        else
          Destination.Client.RollbackTransaction();
      end;

      SourceDataSet.Free();
    end;

    if (DisableKeys and (Destination.Client.ServerVersion >= 40000)) then
    begin
      SQL := 'ALTER TABLE ' + Destination.Client.EscapeIdentifier(DestinationTable.Name) + ' ENABLE KEYS;';
      if (not Destination.Client.ExecuteSQL(SQL)) then
        DoError(DatabaseError(Destination.Client), ToolsItem(Destination), SQL);
    end;
  end;
end;

procedure TTTransfer.ExecuteForeignKeys(var Source, Destination: TItem);
var
  DestinationDatabase: TCDatabase;
  DestinationTable: TCBaseTable;
  I: Integer;
  NewTable: TCBaseTable;
  ParentTable: TCBaseTable;
  SourceDatabase: TCDatabase;
  SourceTable: TCBaseTable;
begin
  SourceDatabase := Source.Client.DatabaseByName(Source.DatabaseName);
  SourceTable := SourceDatabase.BaseTableByName(Source.TableName);
  DestinationDatabase := Destination.Client.DatabaseByName(Destination.DatabaseName);
  DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);
  NewTable := nil;

  if (Assigned(DestinationTable)) then
    for I := 0 to SourceTable.ForeignKeys.Count - 1 do
      if (not Assigned(DestinationTable.ForeignKeyByName(SourceTable.ForeignKeys[I].Name))) then
      begin
        if (not Assigned(NewTable)) then
        begin
          NewTable := TCBaseTable.Create(DestinationDatabase.Tables);
          NewTable.Assign(DestinationTable);
        end;

        ParentTable := DestinationDatabase.BaseTableByName(SourceTable.ForeignKeys[I].Parent.TableName);
        if (Assigned(ParentTable)) then
          NewTable.ForeignKeys.AddForeignKey(SourceTable.ForeignKeys[I]);
      end;

  if (Assigned(NewTable)) then
  begin
    while ((Success = daSuccess) and not DestinationDatabase.UpdateTable(DestinationTable, NewTable)) do
      DoError(DatabaseError(Destination.Client), ToolsItem(Destination));
    FreeAndNil(NewTable);
  end;
end;

procedure TTTransfer.ExecuteStructure(const Source, Destination: TItem);
var
  DeleteForeignKey: Boolean;
  DestinationDatabase: TCDatabase;
  DestinationTable: TCBaseTable;
  I: Integer;
  J: Integer;
  Modified: Boolean;
  NewDestinationTable: TCBaseTable;
  OldFieldBefore: TCTableField;
  SourceDatabase: TCDatabase;
  SourceTable: TCBaseTable;
begin
  SourceDatabase := Source.Client.DatabaseByName(Source.DatabaseName);
  SourceTable := SourceDatabase.BaseTableByName(Source.TableName);
  DestinationDatabase := Destination.Client.DatabaseByName(Destination.DatabaseName);
  DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);

  if (Assigned(DestinationTable)) then
  begin
    while ((Success = daSuccess) and not DestinationDatabase.DeleteObject(DestinationTable)) do
      DoError(DatabaseError(Destination.Client), ToolsItem(Destination));
    DestinationTable := nil;
  end;

  NewDestinationTable := TCBaseTable.Create(DestinationDatabase.Tables);

  if (not Assigned(DestinationTable)) then
  begin
    NewDestinationTable.Assign(SourceTable);

    for I := NewDestinationTable.ForeignKeys.Count - 1 downto 0 do
    begin
      DeleteForeignKey := (Destination.Client.TableNameCmp(NewDestinationTable.ForeignKeys[I].Parent.DatabaseName, SourceTable.Database.Name) <> 0)
        or not Assigned(DestinationDatabase.BaseTableByName(NewDestinationTable.ForeignKeys[I].Parent.TableName));

      if (not DeleteForeignKey) then
      begin
        NewDestinationTable.ForeignKeys[I].Parent.DatabaseName := NewDestinationTable.Database.Name;
        NewDestinationTable.ForeignKeys[I].Parent.TableName := NewDestinationTable.Database.BaseTableByName(NewDestinationTable.ForeignKeys[I].Parent.TableName).Name;
        DeleteForeignKey := not Assigned(DestinationDatabase.TableByName(NewDestinationTable.ForeignKeys[I].Parent.TableName));
        for J := 0 to Length(NewDestinationTable.ForeignKeys[I].Parent.FieldNames) - 1 do
          if (not DeleteForeignKey) then
          begin
            NewDestinationTable.ForeignKeys[I].Parent.FieldNames[J] := DestinationDatabase.TableByName(NewDestinationTable.ForeignKeys[I].Parent.TableName).FieldByName(NewDestinationTable.ForeignKeys[I].Parent.FieldNames[J]).Name;
            DeleteForeignKey := not Assigned(NewDestinationTable.FieldByName(NewDestinationTable.ForeignKeys[I].Parent.FieldNames[J]));
          end;
      end;

      if (DeleteForeignKey) then
        NewDestinationTable.ForeignKeys.DeleteForeignKey(NewDestinationTable.ForeignKeys[I]);
    end;

    NewDestinationTable.AutoIncrement := 0;
    while ((Success = daSuccess) and not DestinationDatabase.AddTable(NewDestinationTable)) do
      DoError(DatabaseError(Destination.Client), ToolsItem(Destination));
    while ((Success = daSuccess) and not Destination.Client.Update()) do
      DoError(DatabaseError(Destination.Client), ToolsItem(Destination));
  end;

  NewDestinationTable.Free();
end;

procedure TTTransfer.ExecuteTable(var Source, Destination: TItem);
var
  I: Integer;
  DestinationDatabase: TCDatabase;
  DestinationTable: TCBaseTable;
  NewTrigger: TCTrigger;
  SourceDatabase: TCDatabase;
  SourceTable: TCBaseTable;
begin
  DestinationDatabase := Destination.Client.DatabaseByName(Destination.DatabaseName);

  if ((Success = daSuccess) and Structure and not Assigned(DestinationDatabase)) then
  begin
    DestinationDatabase := TCDatabase.Create(Destination.Client, Destination.DatabaseName);
    while ((Success = daSuccess) and not Destination.Client.AddDatabase(DestinationDatabase)) do
      DoError(DatabaseError(Destination.Client), ToolsItem(Destination));
    FreeAndNil(DestinationDatabase);

    DestinationDatabase := Destination.Client.DatabaseByName(Destination.DatabaseName);
  end;

  if (Success = daSuccess) then
  begin
    DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);

    if (Backup and Assigned(DestinationTable)) then
    begin
      BackupTable(ToolsItem(Destination), True);
      if (Success = daFail) then Success := daSuccess;

      DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);
    end;

    if (Structure and Data and not Assigned(DestinationTable) and (Source.Client = Destination.Client) and (Source.Client.DatabaseByName(Source.DatabaseName).BaseTableByName(Source.TableName).ForeignKeys.Count = 0)) then
    begin
      while ((Success = daSuccess) and not DestinationDatabase.CloneTable(Source.Client.DatabaseByName(Source.DatabaseName).BaseTableByName(Source.TableName), Destination.TableName, True)) do
        DoError(DatabaseError(Destination.Client), ToolsItem(Destination));

      if (Success = daSuccess) then
      begin
        DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);

        Destination.RecordsDone := DestinationTable.Rows;
      end;
    end
    else
    begin
      if ((Success = daSuccess) and Structure) then
      begin
        ExecuteStructure(Source, Destination);
        DestinationTable := DestinationDatabase.BaseTableByName(Destination.TableName);
      end;

      if ((Success = daSuccess) and Data and Assigned(DestinationTable) and (DestinationTable.Source <> '')) then
        ExecuteData(Source, Destination);
    end;

    if (Success = daSuccess) then
      Destination.RecordsSum := Destination.RecordsDone;

    SourceDatabase := Source.Client.DatabaseByName(Source.DatabaseName);
    SourceTable := SourceDatabase.BaseTableByName(Source.TableName);
    if (Assigned(SourceDatabase.Triggers) and Assigned(DestinationDatabase.Triggers)) then
      for I := 0 to SourceDatabase.Triggers.Count - 1 do
        if ((Success = daSuccess) and (SourceDatabase.Triggers[I].Table = SourceTable) and not Assigned(DestinationDatabase.TriggerByName(SourceDatabase.Triggers[I].Name))) then
        begin
          NewTrigger := TCTrigger.Create(DestinationDatabase.Tables);
          NewTrigger.Assign(SourceDatabase.Triggers[I]);
          while (Success = daSuccess) do
          begin
            DestinationDatabase.AddTrigger(NewTrigger);
            if (Destination.Client.ErrorCode <> 0) then
              DoError(DatabaseError(Destination.Client), ToolsItem(Destination));
          end;
          NewTrigger.Free();
        end;

    Destination.Done := Success = daSuccess;

    DoUpdateGUI();
  end;
end;

function TTTransfer.ToolsItem(const Item: TItem): TTools.TItem;
begin
  Result.Client := Item.Client;
  Result.DatabaseName := Item.DatabaseName;
  Result.TableName := Item.TableName;
end;

end.

