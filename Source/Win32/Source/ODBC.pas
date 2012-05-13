unit ODBC;

//**************************************************************************
//
// ODBC API 3.51
//
// I have merged all the relevant header files into this one file,
// and removed all version specific IFDEFs assuming VERSION 3.51
//   sqltypes.h
//   sql.h
//   sqlext.h
//   sqlucode.h
//   sqltrace.h
//
//**************************************************************************

interface

uses
  Windows;

//##########################################################################
// sqltypes.h interface section starts here
// (N.B. no implementation section for sqltypes.h)
//##########################################################################

//******************************************************************
// SQLTYPES.H - This file defines the types used in ODBC
//
// (C) Copyright 1995-1998 By Microsoft Corp.
//
// Created 04/10/95 for 2.50 specification
// Updated 12/11/95 for 3.00 specification
//********************************************************************

const
  ODBCVER = $0351;

// API declaration data types
type
  {$EXTERNALSYM SQLCHAR}
  SQLCHAR = AnsiChar;
  {$EXTERNALSYM PSQLCHAR}
  PSQLCHAR = ^SQLCHAR;
  {$EXTERNALSYM SQLWCHAR}
  SQLWCHAR = WideChar;
  {$EXTERNALSYM SQLWCHAR}
  PSQLWCHAR = ^SQLWCHAR;
  {$IFNDEF UNICODE}
    {$EXTERNALSYM SQLTCHAR}
    SQLTCHAR = SQLCHAR;
  {$ELSE}
    {$EXTERNALSYM SQLTCHAR}
    SQLTCHAR = SQLWCHAR;
  {$ENDIF}
  {$EXTERNALSYM PSQLTCHAR}
  PSQLTCHAR = ^SQLTCHAR;
  {$EXTERNALSYM SQLSCHAR}
  SQLSCHAR = AnsiChar;
  {$EXTERNALSYM SQLDATE}
  SQLDATE = Byte; // ??? Defined in DBXpress SQLDATE = Longint;
  {$EXTERNALSYM SQLDECIMAL}
  SQLDECIMAL = Byte;
  {$EXTERNALSYM SQLDOUBLE}
  SQLDOUBLE = Double;
  {$EXTERNALSYM SQLFLOAT}
  SQLFLOAT = Double;
  {$EXTERNALSYM SQLINTEGER}
  SQLINTEGER = LongInt;
  PSQLINTEGER = ^SQLINTEGER;
  {$EXTERNALSYM SQLNUMERIC}
  SQLNUMERIC = Byte;
  {$EXTERNALSYM SQLPOINTER}
  SQLPOINTER = Pointer;
  PSQLPOINTER = ^SQLPOINTER;
  {$EXTERNALSYM SQLREAL}
  SQLREAL = Single;
  {$EXTERNALSYM SQLSMALLINT}
  SQLSMALLINT = SmallInt;
  PSQLSMALLINT = ^SQLSMALLINT;
  {$EXTERNALSYM SQLUSMALLINT}
  SQLUSMALLINT = Word;
  PSQLUSMALLINT = ^SQLUSMALLINT;
  {$EXTERNALSYM SQLTIME}
  SQLTIME = Byte; // ??? Defined in DBXpress SQLTIME = Longint;
  {$EXTERNALSYM SQLTIMESTAMP}
  SQLTIMESTAMP = Byte;
  {$EXTERNALSYM SQLVARCHAR}
  SQLVARCHAR = AnsiChar;
  {$EXTERNALSYM SQLVARWCHAR}
  SQLVARWCHAR = WideChar;
  {$IFNDEF UNICODE}
    {$EXTERNALSYM SQLVARTCHAR}
    SQLVARTCHAR = SQLVARCHAR;
  {$ELSE}
    {$EXTERNALSYM SQLVARTCHAR}
    SQLVARTCHAR = SQLVARWCHAR;
  {$ENDIF}

// function return type
  {$EXTERNALSYM SQLRETURN}
  SQLRETURN = SQLSMALLINT;

// SQL Handle types
  {$EXTERNALSYM SQLHANDLE}
  SQLHANDLE = Pointer;
  PSQLHANDLE = ^SQLHANDLE;
  {$EXTERNALSYM SQLHENV}
  SQLHENV = SQLHANDLE;
  PSQLHENV = ^SQLHENV;
  {$EXTERNALSYM SQLHDBC}
  SQLHDBC = SQLHANDLE;
  PSQLHDBC = ^SQLHDBC;
  {$EXTERNALSYM SQLHSTMT}
  SQLHSTMT = SQLHANDLE;
  PSQLHSTMT = ^SQLHSTMT;
  {$EXTERNALSYM SQLHDESC}
  SQLHDESC = SQLHANDLE;
  PSQLHDESC = ^SQLHDESC;

// SQL portable types
  {$EXTERNALSYM UCHAR}
  UCHAR = Byte;
  {$EXTERNALSYM SCHAR}
  SCHAR = AnsiChar;
  {$EXTERNALSYM SDWORD}
  SDWORD = LongInt;
  {$EXTERNALSYM SWORD}
  SWORD = SmallInt;
  {$EXTERNALSYM UDWORD}
  UDWORD = Longword;
  {$EXTERNALSYM UWORD}
  UWORD = Word;
  PUWORD = ^UWORD;
  {$EXTERNALSYM SQLUINTEGER}
  SQLUINTEGER = UDWORD;
  PSQLUINTEGER = ^SQLUINTEGER;
  {$EXTERNALSYM SLONG}
  SLONG = LongInt;
  {$EXTERNALSYM SSHORT}
  SSHORT = SmallInt;
  {$EXTERNALSYM ULONG}
  ULONG = LongWord;
  {$EXTERNALSYM USHORT}
  USHORT = Word;
  {$EXTERNALSYM SDOUBLE}
  SDOUBLE = Double;
  {$EXTERNALSYM LDOUBLE}
  LDOUBLE = Double;
  {$EXTERNALSYM SFLOAT}
  SFLOAT = Single;
  {$EXTERNALSYM PTR}
  PTR = Pointer;

  {$EXTERNALSYM HENV}
  HENV = Pointer;
  {$EXTERNALSYM HDBC}
  HDBC = Pointer;
  {$EXTERNALSYM HSTMT}
  HSTMT = Pointer;

  {$EXTERNALSYM RETCODE}
  RETCODE = SmallInt;

  {$EXTERNALSYM SQLHWND}
//  SQLHWND = HWND;
  SQLHWND = LongWord;

// transfer types for DATE, TIME, TIMESTAMP
  {$EXTERNALSYM tagDATE_STRUCT}
  tagDATE_STRUCT = record
    year:  SQLSMALLINT;
    month: SQLUSMALLINT;
    day:   SQLUSMALLINT;
  end;
  PSqlDateStruct = ^TSqlDateStruct;
  TSqlDateStruct = tagDATE_STRUCT;
  {$EXTERNALSYM SQL_DATE_STRUCT}
  SQL_DATE_STRUCT = tagDATE_STRUCT;

  {$EXTERNALSYM tagTIME_STRUCT}
  tagTIME_STRUCT = record
    hour:   SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
  end;
  PSqlTimeStruct = ^TSqlTimeStruct;
  TSqlTimeStruct = tagTIME_STRUCT;
  {$EXTERNALSYM SQL_TIME_STRUCT}
  SQL_TIME_STRUCT = tagTIME_STRUCT;

  {$EXTERNALSYM tagTIMESTAMP_STRUCT}
  tagTIMESTAMP_STRUCT = record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
    fraction: SQLUINTEGER;
  end;

  {$EXTERNALSYM SQL_TIMESTAMP_STRUCT}
  SQL_TIMESTAMP_STRUCT = tagTIMESTAMP_STRUCT;
  POdbcTimeStamp = ^TOdbcTimeStamp;
  TOdbcTimeStamp = tagTIMESTAMP_STRUCT;

// enumerations for DATETIME_INTERVAL_SUBCODE values for interval data types
// these values are from SQL-92
  {$EXTERNALSYM SQLINTERVAL}
  SQLINTERVAL = (
    SQL_IS_DUMMY {placeholder for 0},
    SQL_IS_YEAR {= 1},
    SQL_IS_MONTH {= 2},
    SQL_IS_DAY {= 3},
    SQL_IS_HOUR {= 4},
    SQL_IS_MINUTE {= 5},
    SQL_IS_SECOND {= 6},
    SQL_IS_YEAR_TO_MONTH {= 7},
    SQL_IS_DAY_TO_HOUR {= 8},
    SQL_IS_DAY_TO_MINUTE {= 9},
    SQL_IS_DAY_TO_SECOND {= 10},
    SQL_IS_HOUR_TO_MINUTE {= 11},
    SQL_IS_HOUR_TO_SECOND {= 12},
    SQL_IS_MINUTE_TO_SECOND {= 13});
  ESqlInterval = SQLINTERVAL;

  {$EXTERNALSYM tagSQL_YEAR_MONTH}
  tagSQL_YEAR_MONTH = record
    year: SQLUINTEGER;
    month: SQLUINTEGER;
  end;
  TSqlYearMonth = tagSQL_YEAR_MONTH;

  {$EXTERNALSYM tagSQL_DAY_SECOND}
  tagSQL_DAY_SECOND = record
    day: SQLUINTEGER;
    hour: SQLUINTEGER;
    minute: SQLUINTEGER;
    second: SQLUINTEGER;
    fraction: SQLUINTEGER;
  end;
  TSqlDaySecond = tagSQL_DAY_SECOND;

  {$EXTERNALSYM tagSQL_INTERVAL_STRUCT}
  tagSQL_INTERVAL_STRUCT = record
    interval_type: SQLINTERVAL;
    interval_sign: SQLSMALLINT;
    case ESqlInterval of
      SQL_IS_YEAR_TO_MONTH: (YearMonth: TSqlYearMonth);
      SQL_IS_DAY_TO_SECOND: (DaySecond: TSqlDaySecond);
  end;
  TSqlInterval = tagSQL_INTERVAL_STRUCT;

  {$EXTERNALSYM ODBCINT64}
  ODBCINT64 = Int64;
  {$EXTERNALSYM SQLBIGINT}
  SQLBIGINT = Int64;
  {$EXTERNALSYM SQLUBIGINT}
  SQLUBIGINT = UInt64;

// internal representation of numeric data type
const
  {$EXTERNALSYM SQL_MAX_NUMERIC_LEN}
  SQL_MAX_NUMERIC_LEN = 16;

type
  {$EXTERNALSYM tagSQL_NUMERIC_STRUCTA}
  tagSQL_NUMERIC_STRUCTA = record
    precision: SQLCHAR;
    scale: SQLSCHAR;
    sign: SQLCHAR; {= 1 if positive, 0 if negative }
    val: array[0..SQL_MAX_NUMERIC_LEN-1] of SQLCHAR;
  end;
  {$EXTERNALSYM tagSQL_NUMERIC_STRUCTW}
  tagSQL_NUMERIC_STRUCTW = record
    precision: SQLWCHAR;
    scale: SQLSCHAR;
    sign: SQLWCHAR; {= 1 if positive, 0 if negative }
    val: array[0..SQL_MAX_NUMERIC_LEN-1] of SQLWCHAR;
  end;
  {$IFNDEF UNICODE}
  {$EXTERNALSYM tagSQL_NUMERIC_STRUCT}
  tagSQL_NUMERIC_STRUCT = tagSQL_NUMERIC_STRUCTA;
  {$ELSE}
  {$EXTERNALSYM tagSQL_NUMERIC_STRUCT}
  tagSQL_NUMERIC_STRUCT = tagSQL_NUMERIC_STRUCTW;
  {$ENDIF}

  {$EXTERNALSYM SQLGUID}
  SQLGUID = TGUID;

  {$EXTERNALSYM BOOKMARK}
  BOOKMARK = LongInt;

  {$EXTERNALSYM SQLACHAR}
  SQLACHAR = AnsiChar;
  {$EXTERNALSYM SQLACHAR}
  PSQLACHAR = ^SQLACHAR;

//##########################################################################
// sqltypes.h interface section ends here
// (no implemetation section for sqltypes.h)
//##########################################################################

//##########################################################################
// sql.h interface part starts here
//##########################################################################

//****************************************************************
// SQL.H - This is the the main include for ODBC Core functions.
//
// preconditions:
// INCLUDE "windows.h"
//
// (C) Copyright 1990 - 1998 By Microsoft Corp.
//
// Updated 5/12/93 for 2.00 specification
// Updated 5/23/94 for 2.01 specification
// Updated 11/10/94 for 2.10 specification
// Updated 04/10/95 for 2.50 specification
// Updated 6/6/95 for 3.00 specification
// Updated 10/22/97 for 3.51 specification
//********************************************************************

// All version-specific IFDEFs removed, and assume ODBC Version 3.51

const
// special length/indicator values
  SQL_NULL_DATA = (-1); //??? Defined in DBXpress: SQL_NULL_DATA = 100;
  SQL_DATA_AT_EXEC = (-2);

// return values from functions
  SQL_SUCCESS = 0;  //??? Defined in DBXpress: SQL_SUCCESS = $0000;
  SQL_SUCCESS_WITH_INFO = 1;
  SQL_NO_DATA = 100;
  SQL_ERROR = (-1);  //??? Defined in DBXpress: SQL_ERROR = -1;
  SQL_INVALID_HANDLE = (-2);
  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

// MACRO
// test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO
{$EXTERNALSYM SQL_SUCCEEDED}
function SQL_SUCCEEDED(const rc: SQLRETURN): Boolean;

const
// flags for null-terminated string
  SQL_NTS = (-3);
  SQL_NTSL = (-3);

// maximum message length
  SQL_MAX_MESSAGE_LENGTH = 512;

// date/time length constants
  SQL_DATE_LEN = 10;
  SQL_TIME_LEN = 8; // add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19; // add P+1 if precision is nonzero

// handle type identifiers
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

// environment attribute
  SQL_ATTR_OUTPUT_NTS = 10001;

// connection attributes
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

// statement attributes
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = (-1);
  SQL_ATTR_CURSOR_SENSITIVITY = (-2);

// SQL_ATTR_CURSOR_SCROLLABLE values
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

// identifiers of fields in the SQL descriptor
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

// identifiers of fields in the diagnostics area*
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

// dynamic function codes
  SQL_DIAG_ALTER_DOMAIN = 3;
  SQL_DIAG_ALTER_TABLE = 4;
  SQL_DIAG_CALL = 7;
  SQL_DIAG_CREATE_ASSERTION = 6;
  SQL_DIAG_CREATE_CHARACTER_SET = 8;
  SQL_DIAG_CREATE_COLLATION = 10;
  SQL_DIAG_CREATE_DOMAIN = 23;
  SQL_DIAG_CREATE_INDEX = (-1);
  SQL_DIAG_CREATE_SCHEMA = 64;
  SQL_DIAG_CREATE_TABLE = 77;
  SQL_DIAG_CREATE_TRANSLATION = 79;
  SQL_DIAG_CREATE_VIEW = 84;
  SQL_DIAG_DELETE_WHERE = 19;
  SQL_DIAG_DROP_ASSERTION = 24;
  SQL_DIAG_DROP_CHARACTER_SET = 25;
  SQL_DIAG_DROP_COLLATION = 26;
  SQL_DIAG_DROP_DOMAIN = 27;
  SQL_DIAG_DROP_INDEX = (-2);
  SQL_DIAG_DROP_SCHEMA = 31;
  SQL_DIAG_DROP_TABLE = 32;
  SQL_DIAG_DROP_TRANSLATION = 33;
  SQL_DIAG_DROP_VIEW = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT = 48;
  SQL_DIAG_INSERT = 50;
  SQL_DIAG_REVOKE = 59;
  SQL_DIAG_SELECT_CURSOR = 85;
  SQL_DIAG_UNKNOWN_STATEMENT = 0;
  SQL_DIAG_UPDATE_WHERE = 82;

// SQL data type codes
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;
  SQL_DATETIME = 9;
  SQL_VARCHAR = 12;

// One-parameter shortcuts for date/time data types
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

// Statement attribute values for cursor sensitivity
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

// GetTypeInfo() request for all data types
  SQL_ALL_TYPES = 0;

// Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
  SQL_DEFAULT = 99;

// SQLGetData() code indicating that the application row descriptor
// specifies the data type
  SQL_ARD_TYPE = (-99);

// SQL date/time type subcodes
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

// CLI option values
  SQL_FALSE = 0;
  SQL_TRUE = 1;

// values of NULLABLE field in descriptor
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

// Value returned by SQLGetTypeInfo() to denote that it is
// not known whether or not a data type supports null values.
  SQL_NULLABLE_UNKNOWN = 2;

// Values returned by SQLGetTypeInfo() to show WHERE clause supported
  SQL_PRED_NONE = 0;
const
  SQL_PRED_CHAR = 1;
const
  SQL_PRED_BASIC = 2;

// values of UNNAMED field in descriptor
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

// values of ALLOC_TYPE field in descriptor
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

// FreeStmt() options
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;

// Codes used for FetchOrientation in SQLFetchScroll(), and in SQLDataSources()
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

// Other codes used for FetchOrientation in SQLFetchScroll()
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

// SQLEndTran() options
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;

// null handles returned by SQLAllocHandle()
  SQL_NULL_HENV = SQLHANDLE(0);
  SQL_NULL_HDBC = SQLHANDLE(0);
  SQL_NULL_HSTMT = SQLHANDLE(0);
  SQL_NULL_HDESC = SQLHANDLE(0);

// null handle used in place of parent handle when allocating HENV
  SQL_NULL_HANDLE = SQLHANDLE(0);

// Values that may appear in the result set of SQLSpecialColumns()
  SQL_SCOPE_CURROW = 0;
  SQL_SCOPE_TRANSACTION = 1;
  SQL_SCOPE_SESSION = 2;
  SQL_PC_UNKNOWN = 0;
  SQL_PC_NON_PSEUDO = 1;
  SQL_PC_PSEUDO = 2;

// Reserved value for the IdentifierType argument of SQLSpecialColumns()
  SQL_ROW_IDENTIFIER = 1;

// Reserved values for UNIQUE argument of SQLStatistics()
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;

// Values that may appear in the result set of SQLStatistics()
  SQL_INDEX_CLUSTERED = 1;
  SQL_INDEX_HASHED = 2;
  SQL_INDEX_OTHER = 3;

// SQLGetFunctions() values to identify ODBC APIs
  SQL_API_SQLALLOCCONNECT = 1;
  SQL_API_SQLALLOCENV = 2;
  SQL_API_SQLALLOCHANDLE = 1001;
  SQL_API_SQLALLOCSTMT = 3;
  SQL_API_SQLBINDCOL = 4;
  SQL_API_SQLBINDPARAM = 1002;
  SQL_API_SQLCANCEL = 5;
  SQL_API_SQLCLOSECURSOR = 1003;
  SQL_API_SQLCOLATTRIBUTE = 6;
  SQL_API_SQLCOLUMNS = 40;
  SQL_API_SQLCONNECT = 7;
  SQL_API_SQLCOPYDESC = 1004;
  SQL_API_SQLDATASOURCES = 57;
  SQL_API_SQLDESCRIBECOL = 8;
  SQL_API_SQLDISCONNECT = 9;
  SQL_API_SQLENDTRAN = 1005;
  SQL_API_SQLERROR = 10;
  SQL_API_SQLEXECDIRECT = 11;
  SQL_API_SQLEXECUTE = 12;
  SQL_API_SQLFETCH = 13;
  SQL_API_SQLFETCHSCROLL = 1021;
  SQL_API_SQLFREECONNECT = 14;
  SQL_API_SQLFREEENV = 15;
  SQL_API_SQLFREEHANDLE = 1006;
  SQL_API_SQLFREESTMT = 16;
  SQL_API_SQLGETCONNECTATTR = 1007;
  SQL_API_SQLGETCONNECTOPTION = 42;
  SQL_API_SQLGETCURSORNAME = 17;
  SQL_API_SQLGETDATA = 43;
  SQL_API_SQLGETDESCFIELD = 1008;
  SQL_API_SQLGETDESCREC = 1009;
  SQL_API_SQLGETDIAGFIELD = 1010;
  SQL_API_SQLGETDIAGREC = 1011;
  SQL_API_SQLGETENVATTR = 1012;
  SQL_API_SQLGETFUNCTIONS = 44;
  SQL_API_SQLGETINFO = 45;
  SQL_API_SQLGETSTMTATTR = 1014;
  SQL_API_SQLGETSTMTOPTION = 46;
  SQL_API_SQLGETTYPEINFO = 47;
  SQL_API_SQLNUMRESULTCOLS = 18;
  SQL_API_SQLPARAMDATA = 48;
  SQL_API_SQLPREPARE = 19;
  SQL_API_SQLPUTDATA = 49;
  SQL_API_SQLROWCOUNT = 20;
  SQL_API_SQLSETCONNECTATTR = 1016;
  SQL_API_SQLSETCONNECTOPTION = 50;
  SQL_API_SQLSETCURSORNAME = 21;
  SQL_API_SQLSETDESCFIELD = 1017;
  SQL_API_SQLSETDESCREC = 1018;
  SQL_API_SQLSETENVATTR = 1019;
  SQL_API_SQLSETPARAM = 22;
  SQL_API_SQLSETSTMTATTR = 1020;
  SQL_API_SQLSETSTMTOPTION = 51;
  SQL_API_SQLSPECIALCOLUMNS = 52;
  SQL_API_SQLSTATISTICS = 53;
  SQL_API_SQLTABLES = 54;
  SQL_API_SQLTRANSACT = 23;

// Information requested by SQLGetInfo()
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;
  SQL_XOPEN_CLI_YEAR = 10000;
  SQL_CURSOR_SENSITIVITY = 10001;
  SQL_DESCRIBE_PARAMETER = 10002;
  SQL_CATALOG_NAME = 10003;
  SQL_COLLATION_SEQ = 10004;
  SQL_MAX_IDENTIFIER_LEN = 10005;
  SQL_MAXIMUM_IDENTIFIER_LENGTH = SQL_MAX_IDENTIFIER_LEN;

// SQL_ALTER_TABLE bitmasks
  SQL_AT_ADD_COLUMN = $00000001;
  SQL_AT_DROP_COLUMN = $00000002;
  SQL_AT_ADD_CONSTRAINT = $00000008;

// The following bitmasks are ODBC extensions and defined in sqlext.h
  SQL_AT_COLUMN_SINGLE = $00000020;
  SQL_AT_ADD_COLUMN_DEFAULT = $00000040;
  SQL_AT_ADD_COLUMN_COLLATION = $00000080;
  SQL_AT_SET_COLUMN_DEFAULT = $00000100;
  SQL_AT_DROP_COLUMN_DEFAULT = $00000200;
  SQL_AT_DROP_COLUMN_CASCADE = $00000400;
  SQL_AT_DROP_COLUMN_RESTRICT = $00000800;
  SQL_AT_ADD_TABLE_CONSTRAINT = $00001000;
  SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE = $00002000;
  SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT = $00004000;
  SQL_AT_CONSTRAINT_NAME_DEFINITION = $00008000;
  SQL_AT_CONSTRAINT_INITIALLY_DEFERRED = $00010000;
  SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE = $00020000;
  SQL_AT_CONSTRAINT_DEFERRABLE = $00040000;
  SQL_AT_CONSTRAINT_NON_DEFERRABLE = $00080000;


// SQL_ASYNC_MODE values
  SQL_AM_NONE = 0;
  SQL_AM_CONNECTION = 1;
  SQL_AM_STATEMENT = 2;

// SQL_CURSOR_COMMIT_BEHAVIOR values
  SQL_CB_DELETE = 0;
  SQL_CB_CLOSE = 1;
  SQL_CB_PRESERVE = 2;

// SQL_FETCH_DIRECTION bitmasks
  SQL_FD_FETCH_NEXT = $00000001;
  SQL_FD_FETCH_FIRST = $00000002;
  SQL_FD_FETCH_LAST = $00000004;
  SQL_FD_FETCH_PRIOR = $00000008;
  SQL_FD_FETCH_ABSOLUTE = $00000010;
  SQL_FD_FETCH_RELATIVE = $00000020;

// SQL_GETDATA_EXTENSIONS bitmasks
  SQL_GD_ANY_COLUMN = $00000001;
  SQL_GD_ANY_ORDER = $00000002;

// SQL_IDENTIFIER_CASE values
  SQL_IC_UPPER = 1;
  SQL_IC_LOWER = 2;
  SQL_IC_SENSITIVE = 3;
  SQL_IC_MIXED = 4;

// SQL_OJ_CAPABILITIES bitmasks
// NB: this means 'outer join', not what you may be thinking
  SQL_OJ_LEFT = $00000001;
  SQL_OJ_RIGHT = $00000002;
  SQL_OJ_FULL = $00000004;
  SQL_OJ_NESTED = $00000008;
  SQL_OJ_NOT_ORDERED = $00000010;
  SQL_OJ_INNER = $00000020;
  SQL_OJ_ALL_COMPARISON_OPS = $00000040;

// SQL_SCROLL_CONCURRENCY bitmasks
  SQL_SCCO_READ_ONLY = $00000001;
  SQL_SCCO_LOCK = $00000002;
  SQL_SCCO_OPT_ROWVER = $00000004;
  SQL_SCCO_OPT_VALUES = $00000008;

// SQL_TXN_CAPABLE values
  SQL_TC_NONE = 0;
  SQL_TC_DML = 1;
  SQL_TC_ALL = 2;
  SQL_TC_DDL_COMMIT = 3;
  SQL_TC_DDL_IGNORE = 4;

// SQL_TXN_ISOLATION_OPTION bitmasks
  SQL_TXN_READ_UNCOMMITTED = $00000001;
  SQL_TRANSACTION_READ_UNCOMMITTED = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED = $00000002;
  SQL_TRANSACTION_READ_COMMITTED = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ = $00000004;
  SQL_TRANSACTION_REPEATABLE_READ = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE = $00000008;
  SQL_TRANSACTION_SERIALIZABLE = SQL_TXN_SERIALIZABLE;

// SQL_NULL_COLLATION values
  SQL_NC_HIGH = 0;
  SQL_NC_LOW = 1;

{$EXTERNALSYM SQLAllocConnect}
function SQLAllocConnect(
  EnvironmentHandle: SQLHENV;
  ConnectionHandle: PSQLHDBC): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLAllocEnv}
function SQLAllocEnv(
  EnvironmentHandle: PSQLHENV): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLAllocEnvA}
function SQLAllocEnvA(
  EnvironmentHandle: PSQLHENV): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLAllocEnvW}
function SQLAllocEnvW(
  EnvironmentHandle: PSQLHENV): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLAllocHandle}
function SQLAllocHandle(
  HandleType: SQLSMALLINT;
  InputHandle: SQLHANDLE;
  OutputHandle: PSQLHANDLE): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLAllocStmt}
function SQLAllocStmt(
  ConnectionHandle: SQLHDBC;
  StatementHandle: PSQLHSTMT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLBindCol}
function SQLBindCol(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  TargetType: SQLSMALLINT;
  TargetValue: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StrLen_or_Ind: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLBindParam}
function SQLBindParam(
  StatementHandle: SQLHSTMT;
  ParameterNumber: SQLUSMALLINT;
  ValueType: SQLSMALLINT;
  ParameterType: SQLSMALLINT;
  LengthPrecision: SQLUINTEGER;
  ParameterScale: SQLSMALLINT;
  ParameterValue: SQLPOINTER;
  StrLen_or_Ind: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLCancel}
function SQLCancel(
  StatementHandle: SQLHSTMT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLCloseCursor}
function SQLCloseCursor(
  StatementHandle: SQLHSTMT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLColAttribute}
function SQLColAttribute(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  FieldIdentifier: SQLUSMALLINT;
  CharacterAttributePtr: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLengthPtr: SQLPOINTER;
  NumericAttributePtr: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColAttributeA}
function SQLColAttributeA(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  FieldIdentifier: SQLUSMALLINT;
  CharacterAttributePtr: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLengthPtr: SQLPOINTER;
  NumericAttributePtr: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColAttributeW}
function SQLColAttributeW(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  FieldIdentifier: SQLUSMALLINT;
  CharacterAttributePtr: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLengthPtr: SQLPOINTER;
  NumericAttributePtr: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLColumns}
function SQLColumns(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLTCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLTCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLTCHAR;
  NameLength3: SQLSMALLINT;
  ColumnName: PSQLTCHAR;
  NameLength4: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColumnsA}
function SQLColumnsA(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR;
  NameLength4: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColumnsW}
function SQLColumnsW(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR;
  NameLength4: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLConnect}
function SQLConnect(
  ConnectionHandle: SQLHDBC;
  ServerName: PSQLTCHAR;
  NameLength1: SQLSMALLINT;
  UserName: PSQLTCHAR;
  NameLength2: SQLSMALLINT;
  Authentication: PSQLTCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLConnectA}
function SQLConnectA(
  ConnectionHandle: SQLHDBC;
  ServerName: PSQLCHAR;
  NameLength1: SQLSMALLINT;
  UserName: PSQLCHAR;
  NameLength2: SQLSMALLINT;
  Authentication: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLConnectW}
function SQLConnectW(
  ConnectionHandle: SQLHDBC;
  ServerName: PSQLWCHAR;
  NameLength1: SQLSMALLINT;
  UserName: PSQLWCHAR;
  NameLength2: SQLSMALLINT;
  Authentication: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLCopyDesc}
function SQLCopyDesc(
  SourceDescHandle: SQLHDESC;
  TargetDescHandle: SQLHDESC): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLDataSources}
function SQLDataSources(
  EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT;
  ServerName: PSQLTCHAR;
  BufferLength1: SQLSMALLINT;
  NameLength1: PSQLSMALLINT;
  Description: PSQLTCHAR;
  BufferLength2: SQLSMALLINT;
  NameLength2: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDataSourcesA}
function SQLDataSourcesA(
  EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT;
  ServerName: PSQLCHAR;
  BufferLength1: SQLSMALLINT;
  NameLength1: PSQLSMALLINT;
  Description: PSQLCHAR;
  BufferLength2: SQLSMALLINT;
  NameLength2: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDataSourcesW}
function SQLDataSourcesW(
  EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT;
  ServerName: PSQLWCHAR;
  BufferLength1: SQLSMALLINT;
  NameLength1: PSQLSMALLINT;
  Description: PSQLWCHAR;
  BufferLength2: SQLSMALLINT;
  NameLength2: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLDescribeCol}
function SQLDescribeCol(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  ColumnName: PSQLTCHAR;
  BufferLength: SQLSMALLINT;
  NameLength: PSQLSMALLINT;
  DataType: PSQLSMALLINT;
  ColumnSize: PSQLUINTEGER;
  DecimalDigits: PSQLSMALLINT;
  Nullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDescribeColA}
function SQLDescribeColA(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  ColumnName: PSQLCHAR;
  BufferLength: SQLSMALLINT;
  NameLength: PSQLSMALLINT;
  DataType: PSQLSMALLINT;
  ColumnSize: PSQLUINTEGER;
  DecimalDigits: PSQLSMALLINT;
  Nullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDescribeColW}
function SQLDescribeColW(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  ColumnName: PSQLWCHAR;
  BufferLength: SQLSMALLINT;
  NameLength: PSQLSMALLINT;
  DataType: PSQLSMALLINT;
  ColumnSize: PSQLUINTEGER;
  DecimalDigits: PSQLSMALLINT;
  Nullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLDisconnect}
function SQLDisconnect(
  ConnectionHandle: SQLHDBC): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLEndTran}
function SQLEndTran(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  CompletionType: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLError}
function SQLError(
  EnvironmentHandle: SQLHENV;
  ConnectionHandle: SQLHDBC;
  StatementHandle: SQLHSTMT;
  SQLState: PSQLTCHAR;
  NativeError: PSQLINTEGER;
  MessageText: PSQLTCHAR;
  BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLErrorA}
function SQLErrorA(
  EnvironmentHandle: SQLHENV;
  ConnectionHandle: SQLHDBC;
  StatementHandle: SQLHSTMT;
  SQLState: PSQLCHAR;
  NativeError: PSQLINTEGER;
  MessageText: PSQLCHAR;
  BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLErrorW}
function SQLErrorW(
  EnvironmentHandle: SQLHENV;
  ConnectionHandle: SQLHDBC;
  StatementHandle: SQLHSTMT;
  SQLState: PSQLWCHAR;
  NativeError: PSQLINTEGER;
  MessageText: PSQLWCHAR;
  BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLExecDirect}
function SQLExecDirect(
  StatementHandle: SQLHSTMT;
  StatementText: PSQLTCHAR;
  TextLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLExecDirectA}
function SQLExecDirectA(
  StatementHandle: SQLHSTMT;
  StatementText: PSQLCHAR;
  TextLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLExecDirectW}
function SQLExecDirectW(
  StatementHandle: SQLHSTMT;
  StatementText: PSQLWCHAR;
  TextLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLExecute}
function SQLExecute(
  StatementHandle: SQLHSTMT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLFetch}
function SQLFetch(
  StatementHandle: SQLHSTMT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLFetchScroll}
function SQLFetchScroll(
  StatementHandle: SQLHSTMT;
  FetchOrientation: SQLSMALLINT;
  FetchOffset: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLFreeConnect}
function SQLFreeConnect(
  ConnectionHandle: SQLHDBC): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLFreeEnv}
function SQLFreeEnv(
  EnvironmentHandle: SQLHENV): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLFreeEnvA}
function SQLFreeEnvA(
  EnvironmentHandle: SQLHENV): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLFreeEnvW}
function SQLFreeEnvW(
  EnvironmentHandle: SQLHENV): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLFreeHandle}
function SQLFreeHandle(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLFreeStmt}
function SQLFreeStmt(
  StatementHandle: SQLHSTMT;
  Option: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetConnectAttr}
function SQLGetConnectAttr(
  ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetConnectAttrA}
function SQLGetConnectAttrA(
  ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetConnectAttrW}
function SQLGetConnectAttrW(
  ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetConnectOption}
function SQLGetConnectOption(
  ConnectionHandle: SQLHDBC;
  Option: SQLUSMALLINT;
  Value: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetConnectOptionA}
function SQLGetConnectOptionA(
  ConnectionHandle: SQLHDBC;
  Option: SQLUSMALLINT;
  Value: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetConnectOptionW}
function SQLGetConnectOptionW(
  ConnectionHandle: SQLHDBC;
  Option: SQLUSMALLINT;
  Value: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetCursorName}
function SQLGetCursorName(
  StatementHandle: SQLHSTMT;
  CursorName: PSQLTCHAR;
  BufferLength: SQLSMALLINT;
  NameLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetCursorNameA}
function SQLGetCursorNameA(
  StatementHandle: SQLHSTMT;
  CursorName: PSQLCHAR;
  BufferLength: SQLSMALLINT;
  NameLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetCursorNameW}
function SQLGetCursorNameW(
  StatementHandle: SQLHSTMT;
  CursorName: PSQLWCHAR;
  BufferLength: SQLSMALLINT;
  NameLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetData}
function SQLGetData(
  StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT;
  TargetType: SQLSMALLINT;
  TargetValue: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StrLen_or_Ind: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetDescField}
function SQLGetDescField(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  FieldIdentifier: SQLSMALLINT;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDescFieldA}
function SQLGetDescFieldA(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  FieldIdentifier: SQLSMALLINT;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDescFieldW}
function SQLGetDescFieldW(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  FieldIdentifier: SQLSMALLINT;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetDescRec}
function SQLGetDescRec(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  Name: PSQLTCHAR;
  BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT;
  _Type: PSQLSMALLINT;
  SubType: PSQLSMALLINT;
  Length: PSQLINTEGER;
  Precision: PSQLSMALLINT;
  Scale: PSQLSMALLINT;
  Nullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDescRecA}
function SQLGetDescRecA(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  Name: PSQLCHAR;
  BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT;
  _Type: PSQLSMALLINT;
  SubType: PSQLSMALLINT;
  Length: PSQLINTEGER;
  Precision: PSQLSMALLINT;
  Scale: PSQLSMALLINT;
  Nullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDescRecW}
function SQLGetDescRecW(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  Name: PSQLWCHAR;
  BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT;
  _Type: PSQLSMALLINT;
  SubType: PSQLSMALLINT;
  Length: PSQLINTEGER;
  Precision: PSQLSMALLINT;
  Scale: PSQLSMALLINT;
  Nullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetDiagField}
function SQLGetDiagField(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT;
  DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDiagFieldA}
function SQLGetDiagFieldA(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT;
  DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDiagFieldW}
function SQLGetDiagFieldW(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT;
  DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetDiagRec}
function SQLGetDiagRec(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT;
  SQLState: PSQLTCHAR;
  NativeError: PSQLINTEGER;
  MessageText: PSQLTCHAR;
  BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDiagRecA}
function SQLGetDiagRecA(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT;
  SQLState: PSQLCHAR;
  NativeError: PSQLINTEGER;
  MessageText: PSQLCHAR;
  BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetDiagRecW}
function SQLGetDiagRecW(
  HandleType: SQLSMALLINT;
  Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT;
  SQLState: PSQLWCHAR;
  NativeError: PSQLINTEGER;
  MessageText: PSQLWCHAR;
  BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetEnvAttr}
function SQLGetEnvAttr(
  EnvironmentHandle: SQLHENV;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetFunctions}
function SQLGetFunctions(
  ConnectionHandle: SQLHDBC;
  FunctionId: SQLUSMALLINT;
  Supported: PSQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetInfo}
function SQLGetInfo(
  ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT;
  InfoValuePtr: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetInfoA}
function SQLGetInfoA(
  ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT;
  InfoValuePtr: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetInfoW}
function SQLGetInfoW(
  ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT;
  InfoValuePtr: SQLPOINTER;
  BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetStmtAttr}
function SQLGetStmtAttr(
  StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetStmtAttrA}
function SQLGetStmtAttrA(
  StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLGetStmtAttrW}
function SQLGetStmtAttrW(
  StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetStmtOption}
function SQLGetStmtOption(
  StatementHandle: SQLHSTMT;
  Option: SQLUSMALLINT;
  Value: SQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLGetTypeInfo}
function SQLGetTypeInfo(
  StatementHandle: SQLHSTMT;
  DataType: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLNumResultCols}
function SQLNumResultCols(
  StatementHandle: SQLHSTMT;
  ColumnCount: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLParamData}
function SQLParamData(
  StatementHandle: SQLHSTMT;
  Value: PSQLPOINTER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLPrepare}
function SQLPrepare(
  StatementHandle: SQLHSTMT;
  StatementText: PSQLTCHAR;
  TextLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLPrepareA}
function SQLPrepareA(
  StatementHandle: SQLHSTMT;
  StatementText: PSQLCHAR;
  TextLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLPrepareW}
function SQLPrepareW(
  StatementHandle: SQLHSTMT;
  StatementText: PSQLWCHAR;
  TextLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLPutData}
function SQLPutData(
  StatementHandle: SQLHSTMT;
  Data: SQLPOINTER;
  StrLen_or_Ind: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLRowCount}
function SQLRowCount(
  StatementHandle: SQLHSTMT;
  RowCountPtr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetConnectAttr}
function SQLSetConnectAttr(
  ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetConnectAttrA}
function SQLSetConnectAttrA(
  ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetConnectAttrW}
function SQLSetConnectAttrW(
  ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetConnectOption}
function SQLSetConnectOption(
  ConnectionHandle: SQLHDBC;
  Option: SQLUSMALLINT;
  Value: SQLUINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetConnectOptionA}
function SQLSetConnectOptionA(
  ConnectionHandle: SQLHDBC;
  Option: SQLUSMALLINT;
  Value: SQLUINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetConnectOptionW}
function SQLSetConnectOptionW(
  ConnectionHandle: SQLHDBC;
  Option: SQLUSMALLINT;
  Value: SQLUINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetCursorName}
function SQLSetCursorName(
  StatementHandle: SQLHSTMT;
  CursorName: PSQLTCHAR;
  NameLength: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetCursorNameA}
function SQLSetCursorNameA(
  StatementHandle: SQLHSTMT;
  CursorName: PSQLCHAR;
  NameLength: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetCursorNameW}
function SQLSetCursorNameW(
  StatementHandle: SQLHSTMT;
  CursorName: PSQLWCHAR;
  NameLength: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetDescField}
function SQLSetDescField(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  FieldIdentifier: SQLSMALLINT;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetDescFieldA}
function SQLSetDescFieldA(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  FieldIdentifier: SQLSMALLINT;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetDescFieldW}
function SQLSetDescFieldW(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  FieldIdentifier: SQLSMALLINT;
  Value: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetDescRec}
function SQLSetDescRec(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  _Type: SQLSMALLINT;
  SubType: SQLSMALLINT;
  Length: SQLINTEGER;
  Precision: SQLSMALLINT;
  Scale: SQLSMALLINT;
  Data: SQLPOINTER;
  StringLength: PSQLINTEGER;
  Indicator: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetDescRecA}
function SQLSetDescRecA(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  _Type: SQLSMALLINT;
  SubType: SQLSMALLINT;
  Length: SQLINTEGER;
  Precision: SQLSMALLINT;
  Scale: SQLSMALLINT;
  Data: SQLPOINTER;
  StringLength: PSQLINTEGER;
  Indicator: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetDescRecW}
function SQLSetDescRecW(
  DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT;
  _Type: SQLSMALLINT;
  SubType: SQLSMALLINT;
  Length: SQLINTEGER;
  Precision: SQLSMALLINT;
  Scale: SQLSMALLINT;
  Data: SQLPOINTER;
  StringLength: PSQLINTEGER;
  Indicator: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetEnvAttr}
function SQLSetEnvAttr(
  EnvironmentHandle: SQLHENV;
  Attribute: SQLINTEGER;
  ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetParam}
function SQLSetParam(
  StatementHandle: SQLHSTMT;
  ParameterNumber: SQLUSMALLINT;
  ValueType: SQLSMALLINT;
  ParameterType: SQLSMALLINT;
  LengthPrecision: SQLUINTEGER;
  ParameterScale: SQLSMALLINT;
  ParameterValue: SQLPOINTER;
  StrLen_or_Ind: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetStmtAttr}
function SQLSetStmtAttr(
  StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetStmtAttrA}
function SQLSetStmtAttrA(
  StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSetStmtAttrW}
function SQLSetStmtAttrW(
  StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER;
  Value: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetStmtOption}
function SQLSetStmtOption(
  StatementHandle: SQLHSTMT;
  Option: SQLUSMALLINT;
  Value: SQLUINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSpecialColumns}
function SQLSpecialColumns(
  StatementHandle: SQLHSTMT;
  IdentifierType: SQLUSMALLINT;
  CatalogName: PSQLTCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLTCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLTCHAR;
  NameLength3: SQLSMALLINT;
  Scope: SQLUSMALLINT;
  Nullable: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSpecialColumnsA}
function SQLSpecialColumnsA(
  StatementHandle: SQLHSTMT;
  IdentifierType: SQLUSMALLINT;
  CatalogName: PSQLCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT;
  Scope: SQLUSMALLINT;
  Nullable: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLSpecialColumnsW}
function SQLSpecialColumnsW(
  StatementHandle: SQLHSTMT;
  IdentifierType: SQLUSMALLINT;
  CatalogName: PSQLWCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT;
  Scope: SQLUSMALLINT;
  Nullable: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLStatistics}
function SQLStatistics(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLTCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLTCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLTCHAR;
  NameLength3: SQLSMALLINT;
  Unique: SQLUSMALLINT;
  Reserved: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLStatisticsA}
function SQLStatisticsA(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT;
  Unique: SQLUSMALLINT;
  Reserved: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLStatisticsW}
function SQLStatisticsW(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT;
  Unique: SQLUSMALLINT;
  Reserved: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLTables}
function SQLTables(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLTCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLTCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLTCHAR;
  NameLength3: SQLSMALLINT;
  TableType: PSQLTCHAR;
  NameLength4: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLTablesA}
function SQLTablesA(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT;
  TableType: PSQLCHAR;
  NameLength4: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLTablesW}
function SQLTablesW(
  StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR;
  NameLength1: SQLSMALLINT;
  SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT;
  TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT;
  TableType: PSQLWCHAR;
  NameLength4: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLTransact}
function SQLTransact(
  EnvironmentHandle: SQLHENV;
  ConnectionHandle: SQLHDBC;
  CompletionType: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

//##########################################################################
// sql.h interface part ends here
//##########################################################################

//##########################################################################
// sqlext.h interface part starts here
//##########################################################################

//****************************************************************
// SQLEXT.H - This is the include for applications using
// the Microsoft SQL Extensions
//
// (C) Copyright 1990 - 1998 By Microsoft Corp.
//
// Updated 05/12/93 for 2.00 specification
// Updated 05/23/94 for 2.01 specification
// Updated 10/27/94 for 2.10 specification
// Updated 04/10/95 for 2.50 specification
// Updated 07/25/95 for 3.00 specification
// Updated 01/12/96 for 3.00 preliminary release
// Updated 10/22/97 for 3.51 specification
//********************************************************************

// copied from sqlucode.h
const
SQL_WCHAR = (-8);
SQL_WVARCHAR = (-9);
SQL_WLONGVARCHAR = (-10);
SQL_C_WCHAR = SQL_WCHAR;

// generally useful constants
const
  SQL_SPEC_MAJOR = 3; // Major version of specification
  SQL_SPEC_MINOR = 51; // Minor version of specification
  SQL_SPEC_STRING = '03.51'; // String constant for version

  SQL_SQLSTATE_SIZE = 5; // size of SQLSTATE
  SQL_MAX_DSN_LENGTH = 32; // maximum data source name size

  SQL_MAX_OPTION_STRING_LENGTH = 256;

// return code SQL_NO_DATA_FOUND is the same as SQL_NO_DATA*
  SQL_NO_DATA_FOUND = SQL_NO_DATA;

// an env handle type
  SQL_HANDLE_SENV = 5;

// env attribute
  SQL_ATTR_ODBC_VERSION = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH = 202;

// values for SQL_ATTR_CONNECTION_POOLING
  SQL_CP_OFF = ULONG(0);
  SQL_CP_ONE_PER_DRIVER = ULONG(1);
  SQL_CP_ONE_PER_HENV = ULONG(2);
  SQL_CP_DEFAULT = SQL_CP_OFF;

// values for SQL_ATTR_CP_MATCH
  SQL_CP_STRICT_MATCH = ULONG(0);
  SQL_CP_RELAXED_MATCH = ULONG(1);
  SQL_CP_MATCH_DEFAULT = SQL_CP_STRICT_MATCH;

// values for SQL_ATTR_ODBC_VERSION
  SQL_OV_ODBC2 = ULONG(2);
  SQL_OV_ODBC3 = ULONG(3);

// connection attributes
  SQL_ACCESS_MODE = 101;
  SQL_AUTOCOMMIT = 102;
  SQL_LOGIN_TIMEOUT = 103;
  SQL_OPT_TRACE = 104;
  SQL_OPT_TRACEFILE = 105;
  SQL_TRANSLATE_DLL = 106;
  SQL_TRANSLATE_OPTION = 107;
  SQL_TXN_ISOLATION = 108;
  SQL_CURRENT_QUALIFIER = 109;
  SQL_ODBC_CURSORS = 110;
  SQL_QUIET_MODE = 111;
  SQL_PACKET_SIZE = 112;

// connection attributes with new names
  SQL_ATTR_ACCESS_MODE = SQL_ACCESS_MODE;
  SQL_ATTR_AUTOCOMMIT = SQL_AUTOCOMMIT;
  SQL_ATTR_CONNECTION_TIMEOUT = 113;
  SQL_ATTR_CURRENT_CATALOG = SQL_CURRENT_QUALIFIER;
  SQL_ATTR_DISCONNECT_BEHAVIOR = 114;
  SQL_ATTR_ENLIST_IN_DTC = 1207;
  SQL_ATTR_ENLIST_IN_XA = 1208;
  SQL_ATTR_LOGIN_TIMEOUT = SQL_LOGIN_TIMEOUT;
  SQL_ATTR_ODBC_CURSORS = SQL_ODBC_CURSORS;
  SQL_ATTR_PACKET_SIZE = SQL_PACKET_SIZE;
  SQL_ATTR_QUIET_MODE = SQL_QUIET_MODE;
  SQL_ATTR_TRACE = SQL_OPT_TRACE;
  SQL_ATTR_TRACEFILE = SQL_OPT_TRACEFILE;
  SQL_ATTR_TRANSLATE_LIB = SQL_TRANSLATE_DLL;
  SQL_ATTR_TRANSLATE_OPTION = SQL_TRANSLATE_OPTION;
  SQL_ATTR_TXN_ISOLATION = SQL_TXN_ISOLATION;
  SQL_ATTR_CONNECTION_DEAD = 1209; // GetConnectAttr only

{ ODBC Driver Manager sets this connection attribute to a unicode driver
  (which supports SQLConnectW) when the application is an ANSI application
  (which calls SQLConnect, SQLDriverConnect, or SQLBrowseConnect).
  This is SetConnectAttr only and application does not set this attribute
  This attribute was introduced because some unicode driver's some APIs may
  need to behave differently on ANSI or Unicode applications. A unicode
  driver, which has same behavior for both ANSI or Unicode applications,
  should return SQL_ERROR when the driver manager sets this connection
  attribute. When a unicode driver returns SQL_SUCCESS on this attribute,
  the driver manager treates ANSI and Unicode connections differently in
  connection pooling. }
  SQL_ATTR_ANSI_APP = 115;

// SQL_ACCESS_MODE options
  SQL_MODE_READ_WRITE = ULONG(0); // 1.0 FALSE
  SQL_MODE_READ_ONLY = ULONG(1); // 1.0 TRUE
  SQL_MODE_DEFAULT = SQL_MODE_READ_WRITE;

// SQL_AUTOCOMMIT options
  SQL_AUTOCOMMIT_OFF = ULONG(0); // 1.0 FALSE
  SQL_AUTOCOMMIT_ON = ULONG(1); // 1.0 TRUE
  SQL_AUTOCOMMIT_DEFAULT = SQL_AUTOCOMMIT_ON;

// SQL_LOGIN_TIMEOUT options
  SQL_LOGIN_TIMEOUT_DEFAULT = ULONG(15);

// SQL_OPT_TRACE options
  SQL_OPT_TRACE_OFF = ULONG(0);
  SQL_OPT_TRACE_ON = ULONG(1);
  SQL_OPT_TRACE_DEFAULT = SQL_OPT_TRACE_OFF;
  SQL_OPT_TRACE_FILE_DEFAULT = '\\SQL.LOG';

// SQL_ODBC_CURSORS options
  SQL_CUR_USE_IF_NEEDED = ULONG(0);
  SQL_CUR_USE_ODBC = ULONG(1);
  SQL_CUR_USE_DRIVER = ULONG(2);
  SQL_CUR_DEFAULT = SQL_CUR_USE_DRIVER;

// values for SQL_ATTR_DISCONNECT_BEHAVIOR
  SQL_DB_RETURN_TO_POOL = ULONG(0);
  SQL_DB_DISCONNECT = ULONG(1);
  SQL_DB_DEFAULT = SQL_DB_RETURN_TO_POOL;

// values for SQL_ATTR_ENLIST_IN_DTC
  SQL_DTC_DONE = 0;

// values for SQL_ATTR_CONNECTION_DEAD
  SQL_CD_TRUE = 1; // Connection is closed/dead
  SQL_CD_FALSE = 0; // Connection is open/available

// values for SQL_ATTR_ANSI_APP
  SQL_AA_TRUE = 1; // the application is an ANSI app
  SQL_AA_FALSE = 0; // the application is a Unicode app

// statement attributes
  SQL_QUERY_TIMEOUT = 0;
  SQL_MAX_ROWS = 1;
  SQL_NOSCAN = 2;
  SQL_MAX_LENGTH = 3;
  SQL_ASYNC_ENABLE = 4; // same as SQL_ATTR_ASYNC_ENABLE
  SQL_BIND_TYPE = 5;
  SQL_CURSOR_TYPE = 6;
  SQL_CONCURRENCY = 7;
  SQL_KEYSET_SIZE = 8;
  SQL_ROWSET_SIZE = 9;
  SQL_SIMULATE_CURSOR = 10;
  SQL_RETRIEVE_DATA = 11;
  SQL_USE_BOOKMARKS = 12;
  SQL_GET_BOOKMARK = 13; // GetStmtOption Only
  SQL_ROW_NUMBER = 14; // GetStmtOption Only

  SQL_ATTR_ASYNC_ENABLE = 4;
  SQL_ATTR_CONCURRENCY = SQL_CONCURRENCY;
  SQL_ATTR_CURSOR_TYPE = SQL_CURSOR_TYPE;
  SQL_ATTR_ENABLE_AUTO_IPD = 15;
  SQL_ATTR_FETCH_BOOKMARK_PTR = 16;
  SQL_ATTR_KEYSET_SIZE = SQL_KEYSET_SIZE;
  SQL_ATTR_MAX_LENGTH = SQL_MAX_LENGTH;
  SQL_ATTR_MAX_ROWS = SQL_MAX_ROWS;
  SQL_ATTR_NOSCAN = SQL_NOSCAN;
  SQL_ATTR_PARAM_BIND_OFFSET_PTR = 17;
  SQL_ATTR_PARAM_BIND_TYPE = 18;
  SQL_ATTR_PARAM_OPERATION_PTR = 19;
  SQL_ATTR_PARAM_STATUS_PTR = 20;
  SQL_ATTR_PARAMS_PROCESSED_PTR = 21;
  SQL_ATTR_PARAMSET_SIZE = 22;
  SQL_ATTR_QUERY_TIMEOUT = SQL_QUERY_TIMEOUT;
  SQL_ATTR_RETRIEVE_DATA = SQL_RETRIEVE_DATA;
  SQL_ATTR_ROW_BIND_OFFSET_PTR = 23;
  SQL_ATTR_ROW_BIND_TYPE = SQL_BIND_TYPE;
  SQL_ATTR_ROW_NUMBER = SQL_ROW_NUMBER; // GetStmtAttr
  SQL_ATTR_ROW_OPERATION_PTR = 24;
  SQL_ATTR_ROW_STATUS_PTR = 25;
  SQL_ATTR_ROWS_FETCHED_PTR = 26;
  SQL_ATTR_ROW_ARRAY_SIZE = 27;
  SQL_ATTR_SIMULATE_CURSOR = SQL_SIMULATE_CURSOR;
  SQL_ATTR_USE_BOOKMARKS = SQL_USE_BOOKMARKS;

//=====================================
// This block moved to here from below because of dependent decarations

// SQLColAttributes defines
  SQL_COLUMN_COUNT = 0;
  SQL_COLUMN_NAME = 1;
  SQL_COLUMN_TYPE = 2;
  SQL_COLUMN_LENGTH = 3;
  SQL_COLUMN_PRECISION = 4;
  SQL_COLUMN_SCALE = 5;
  SQL_COLUMN_DISPLAY_SIZE = 6;
  SQL_COLUMN_NULLABLE = 7;
  SQL_COLUMN_UNSIGNED = 8;
  SQL_COLUMN_MONEY = 9;
  SQL_COLUMN_UPDATABLE = 10;
  SQL_COLUMN_AUTO_INCREMENT = 11;
  SQL_COLUMN_CASE_SENSITIVE = 12;
  SQL_COLUMN_SEARCHABLE = 13;
  SQL_COLUMN_TYPE_NAME = 14;
  SQL_COLUMN_TABLE_NAME = 15;
  SQL_COLUMN_OWNER_NAME = 16;
  SQL_COLUMN_QUALIFIER_NAME = 17;
  SQL_COLUMN_LABEL = 18;

  SQL_COLATT_OPT_MAX = SQL_COLUMN_LABEL;
  SQL_COLATT_OPT_MIN = SQL_COLUMN_COUNT;

// SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
  SQL_ATTR_READONLY = 0;
  SQL_ATTR_WRITE = 1;
  SQL_ATTR_READWRITE_UNKNOWN = 2;

// SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
// These are also used by SQLGetInfo
  SQL_UNSEARCHABLE = 0;
  SQL_LIKE_ONLY = 1;
  SQL_ALL_EXCEPT_LIKE = 2;
  SQL_SEARCHABLE = 3;
  SQL_PRED_SEARCHABLE = SQL_SEARCHABLE;

// Special return values for SQLGetData
  SQL_NO_TOTAL = (-4);
// End of move
//=====================================

// New defines for SEARCHABLE column in SQLGetTypeInfo
  SQL_COL_PRED_CHAR = SQL_LIKE_ONLY;
  SQL_COL_PRED_BASIC = SQL_ALL_EXCEPT_LIKE;

// whether an attribute is a pointer or not
  SQL_IS_POINTER = (-4);

  SQL_IS_UINTEGER = (-5);
  SQL_IS_INTEGER = (-6);
  SQL_IS_USMALLINT = (-7);
  SQL_IS_SMALLINT = (-8);

// the value of SQL_ATTR_PARAM_BIND_TYPE
  SQL_PARAM_BIND_BY_COLUMN = ULONG(0);
  SQL_PARAM_BIND_TYPE_DEFAULT = SQL_PARAM_BIND_BY_COLUMN;

// SQL_QUERY_TIMEOUT options
  SQL_QUERY_TIMEOUT_DEFAULT = ULONG(0);

// SQL_MAX_ROWS options
  SQL_MAX_ROWS_DEFAULT = ULONG(0);

// SQL_NOSCAN options
  SQL_NOSCAN_OFF = ULONG(0); // 1.0 FALSE
  SQL_NOSCAN_ON = ULONG(1); // 1.0 TRUE
  SQL_NOSCAN_DEFAULT = SQL_NOSCAN_OFF;

// SQL_MAX_LENGTH options
  SQL_MAX_LENGTH_DEFAULT = ULONG(0);

// values for SQL_ATTR_ASYNC_ENABLE
  SQL_ASYNC_ENABLE_OFF = ULONG(0); // 1.0 FALSE
  SQL_ASYNC_ENABLE_ON = ULONG(1); // 1.0 TRUE
  SQL_ASYNC_ENABLE_DEFAULT = SQL_ASYNC_ENABLE_OFF;

// SQL_BIND_TYPE options
  SQL_BIND_BY_COLUMN = ULONG(0);
  SQL_BIND_TYPE_DEFAULT = SQL_BIND_BY_COLUMN; // Default value

// SQL_CONCURRENCY options
  SQL_CONCUR_READ_ONLY = 1;
  SQL_CONCUR_LOCK = 2;
  SQL_CONCUR_ROWVER = 3;
  SQL_CONCUR_VALUES = 4;
  SQL_CONCUR_DEFAULT = SQL_CONCUR_READ_ONLY; // Default value

// SQL_CURSOR_TYPE options
  SQL_CURSOR_FORWARD_ONLY = ULONG(0);
  SQL_CURSOR_KEYSET_DRIVEN = ULONG(1);
  SQL_CURSOR_DYNAMIC = ULONG(2);
  SQL_CURSOR_STATIC = ULONG(3);
  SQL_CURSOR_TYPE_DEFAULT = SQL_CURSOR_FORWARD_ONLY; // Default value

// SQL_ROWSET_SIZE options
  SQL_ROWSET_SIZE_DEFAULT = ULONG(1);

// SQL_KEYSET_SIZE options
  SQL_KEYSET_SIZE_DEFAULT = ULONG(0);

// SQL_SIMULATE_CURSOR options
  SQL_SC_NON_UNIQUE = ULONG(0);
  SQL_SC_TRY_UNIQUE = ULONG(1);
  SQL_SC_UNIQUE = ULONG(2);

// SQL_RETRIEVE_DATA options
  SQL_RD_OFF = ULONG(0);
  SQL_RD_ON = ULONG(1);
  SQL_RD_DEFAULT = SQL_RD_ON;

// SQL_USE_BOOKMARKS options
  SQL_UB_OFF = ULONG(0);
  SQL_UB_ON = ULONG(1);
  SQL_UB_DEFAULT = SQL_UB_OFF;

// New values for SQL_USE_BOOKMARKS attribute
  SQL_UB_FIXED = SQL_UB_ON;
  SQL_UB_VARIABLE = ULONG(2);

// extended descriptor field
  SQL_DESC_ARRAY_SIZE = 20;
  SQL_DESC_ARRAY_STATUS_PTR = 21;
  SQL_DESC_AUTO_UNIQUE_VALUE = SQL_COLUMN_AUTO_INCREMENT;
  SQL_DESC_BASE_COLUMN_NAME = 22;
  SQL_DESC_BASE_TABLE_NAME = 23;
  SQL_DESC_BIND_OFFSET_PTR = 24;
  SQL_DESC_BIND_TYPE = 25;
  SQL_DESC_CASE_SENSITIVE = SQL_COLUMN_CASE_SENSITIVE;
  SQL_DESC_CATALOG_NAME = SQL_COLUMN_QUALIFIER_NAME;
  SQL_DESC_CONCISE_TYPE = SQL_COLUMN_TYPE;
  SQL_DESC_DATETIME_INTERVAL_PRECISION = 26;
  SQL_DESC_DISPLAY_SIZE = SQL_COLUMN_DISPLAY_SIZE;
  SQL_DESC_FIXED_PREC_SCALE = SQL_COLUMN_MONEY;
  SQL_DESC_LABEL = SQL_COLUMN_LABEL;
  SQL_DESC_LITERAL_PREFIX = 27;
  SQL_DESC_LITERAL_SUFFIX = 28;
  SQL_DESC_LOCAL_TYPE_NAME = 29;
  SQL_DESC_MAXIMUM_SCALE = 30;
  SQL_DESC_MINIMUM_SCALE = 31;
  SQL_DESC_NUM_PREC_RADIX = 32;
  SQL_DESC_PARAMETER_TYPE = 33;
  SQL_DESC_ROWS_PROCESSED_PTR = 34;
  SQL_DESC_ROWVER = 35;
  SQL_DESC_SCHEMA_NAME = SQL_COLUMN_OWNER_NAME;
  SQL_DESC_SEARCHABLE = SQL_COLUMN_SEARCHABLE;
  SQL_DESC_TYPE_NAME = SQL_COLUMN_TYPE_NAME;
  SQL_DESC_TABLE_NAME = SQL_COLUMN_TABLE_NAME;
  SQL_DESC_UNSIGNED = SQL_COLUMN_UNSIGNED;
  SQL_DESC_UPDATABLE = SQL_COLUMN_UPDATABLE;

// defines for diagnostics fields
  SQL_DIAG_CURSOR_ROW_COUNT = (-1249);
  SQL_DIAG_ROW_NUMBER = (-1248);
  SQL_DIAG_COLUMN_NUMBER = (-1247);

// SQL extended datatypes
  SQL_DATE = 9;
  SQL_INTERVAL = 10;
  SQL_TIME = 10;
  SQL_TIMESTAMP = 11;
  SQL_LONGVARCHAR = (-1);
  SQL_BINARY = (-2);
  SQL_VARBINARY = (-3);
  SQL_LONGVARBINARY = (-4);
  SQL_BIGINT = (-5);
  SQL_TINYINT = (-6);
  SQL_BIT = (-7);
  SQL_GUID = (-11);

// interval code
  SQL_CODE_YEAR = 1;
  SQL_CODE_MONTH = 2;
  SQL_CODE_DAY = 3;
  SQL_CODE_HOUR = 4;
  SQL_CODE_MINUTE = 5;
  SQL_CODE_SECOND = 6;
  SQL_CODE_YEAR_TO_MONTH = 7;
  SQL_CODE_DAY_TO_HOUR = 8;
  SQL_CODE_DAY_TO_MINUTE = 9;
  SQL_CODE_DAY_TO_SECOND = 10;
  SQL_CODE_HOUR_TO_MINUTE = 11;
  SQL_CODE_HOUR_TO_SECOND = 12;
  SQL_CODE_MINUTE_TO_SECOND = 13;

  SQL_INTERVAL_YEAR = (100 + SQL_CODE_YEAR);
  SQL_INTERVAL_MONTH = (100 + SQL_CODE_MONTH);
  SQL_INTERVAL_DAY = (100 + SQL_CODE_DAY);
  SQL_INTERVAL_HOUR = (100 + SQL_CODE_HOUR);
  SQL_INTERVAL_MINUTE = (100 + SQL_CODE_MINUTE);
  SQL_INTERVAL_SECOND = (100 + SQL_CODE_SECOND);
  SQL_INTERVAL_YEAR_TO_MONTH = (100 + SQL_CODE_YEAR_TO_MONTH);
  SQL_INTERVAL_DAY_TO_HOUR = (100 + SQL_CODE_DAY_TO_HOUR);
  SQL_INTERVAL_DAY_TO_MINUTE = (100 + SQL_CODE_DAY_TO_MINUTE);
  SQL_INTERVAL_DAY_TO_SECOND = (100 + SQL_CODE_DAY_TO_SECOND);
  SQL_INTERVAL_HOUR_TO_MINUTE = (100 + SQL_CODE_HOUR_TO_MINUTE);
  SQL_INTERVAL_HOUR_TO_SECOND = (100 + SQL_CODE_HOUR_TO_SECOND);
  SQL_INTERVAL_MINUTE_TO_SECOND = (100 + SQL_CODE_MINUTE_TO_SECOND);

  SQL_UNICODE = SQL_WCHAR;
  SQL_UNICODE_VARCHAR = SQL_WVARCHAR;
  SQL_UNICODE_LONGVARCHAR = SQL_WLONGVARCHAR;
  SQL_UNICODE_CHAR = SQL_WCHAR;


// C datatype to SQL datatype mapping SQL types
// -------------------
  SQL_C_CHAR = SQL_CHAR; // CHAR, VARCHAR, DECIMAL, NUMERIC
  SQL_C_LONG = SQL_INTEGER; // INTEGER
  SQL_C_SHORT = SQL_SMALLINT; // SMALLINT
  SQL_C_FLOAT = SQL_REAL; // REAL
  SQL_C_DOUBLE = SQL_DOUBLE; // FLOAT, DOUBLE
  SQL_C_NUMERIC = SQL_NUMERIC;
  SQL_C_DEFAULT = 99;
  SQL_SIGNED_OFFSET = (-20);
  SQL_UNSIGNED_OFFSET = (-22);

// C datatype to SQL datatype mapping
  SQL_C_DATE = SQL_DATE;
  SQL_C_TIME = SQL_TIME;
  SQL_C_TIMESTAMP = SQL_TIMESTAMP;
  SQL_C_TYPE_DATE = SQL_TYPE_DATE;
  SQL_C_TYPE_TIME = SQL_TYPE_TIME;
  SQL_C_TYPE_TIMESTAMP = SQL_TYPE_TIMESTAMP;
  SQL_C_INTERVAL_YEAR = SQL_INTERVAL_YEAR;
  SQL_C_INTERVAL_MONTH = SQL_INTERVAL_MONTH;
  SQL_C_INTERVAL_DAY = SQL_INTERVAL_DAY;
  SQL_C_INTERVAL_HOUR = SQL_INTERVAL_HOUR;
  SQL_C_INTERVAL_MINUTE = SQL_INTERVAL_MINUTE;
  SQL_C_INTERVAL_SECOND = SQL_INTERVAL_SECOND;
  SQL_C_INTERVAL_YEAR_TO_MONTH = SQL_INTERVAL_YEAR_TO_MONTH;
  SQL_C_INTERVAL_DAY_TO_HOUR = SQL_INTERVAL_DAY_TO_HOUR;
  SQL_C_INTERVAL_DAY_TO_MINUTE = SQL_INTERVAL_DAY_TO_MINUTE;
  SQL_C_INTERVAL_DAY_TO_SECOND = SQL_INTERVAL_DAY_TO_SECOND;
  SQL_C_INTERVAL_HOUR_TO_MINUTE = SQL_INTERVAL_HOUR_TO_MINUTE;
  SQL_C_INTERVAL_HOUR_TO_SECOND = SQL_INTERVAL_HOUR_TO_SECOND;
  SQL_C_INTERVAL_MINUTE_TO_SECOND = SQL_INTERVAL_MINUTE_TO_SECOND;
  SQL_C_BINARY = SQL_BINARY;
  SQL_C_BIT = SQL_BIT;
  SQL_C_SBIGINT = (SQL_BIGINT+SQL_SIGNED_OFFSET); // SIGNED BIGINT
  SQL_C_UBIGINT = (SQL_BIGINT+SQL_UNSIGNED_OFFSET); // UNSIGNED BIGINT
  SQL_C_TINYINT = SQL_TINYINT;
  SQL_C_SLONG = (SQL_C_LONG+SQL_SIGNED_OFFSET); // SIGNED INTEGER
  SQL_C_SSHORT = (SQL_C_SHORT+SQL_SIGNED_OFFSET); // SIGNED SMALLINT
  SQL_C_STINYINT = (SQL_TINYINT+SQL_SIGNED_OFFSET); // SIGNED TINYINT
  SQL_C_ULONG = (SQL_C_LONG+SQL_UNSIGNED_OFFSET); // UNSIGNED INTEGER
  SQL_C_USHORT = (SQL_C_SHORT+SQL_UNSIGNED_OFFSET); // UNSIGNED SMALLINT
  SQL_C_UTINYINT = (SQL_TINYINT+SQL_UNSIGNED_OFFSET); // UNSIGNED TINYINT
  SQL_C_BOOKMARK = SQL_C_ULONG; // BOOKMARK
  SQL_C_GUID = SQL_GUID;

  SQL_TYPE_NULL = 0;
  SQL_C_VARBOOKMARK = SQL_C_BINARY;

// define for SQL_DIAG_ROW_NUMBER and SQL_DIAG_COLUMN_NUMBER
  SQL_NO_ROW_NUMBER = (-1);
  SQL_NO_COLUMN_NUMBER = (-1);
  SQL_ROW_NUMBER_UNKNOWN = (-2);
  SQL_COLUMN_NUMBER_UNKNOWN = (-2);

// SQLBindParameter extensions
  SQL_DEFAULT_PARAM = (-5);
  SQL_IGNORE = (-6);
  SQL_COLUMN_IGNORE = SQL_IGNORE;
  SQL_LEN_DATA_AT_EXEC_OFFSET = (-100);

{$EXTERNALSYM SQL_LEN_DATA_AT_EXEC}
function SQL_LEN_DATA_AT_EXEC(length: Integer): Integer;

const
// binary length for driver specific attributes
  SQL_LEN_BINARY_ATTR_OFFSET = (-100);

{$EXTERNALSYM SQL_LEN_BINARY_ATTR}
function SQL_LEN_BINARY_ATTR(length: Integer): Integer;

const
//=====================================
// SQLBindParameter block moved to here because of dependent decarations

// Defines for SQLBindParameter and
// SQLProcedureColumns (returned in the result set)
  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_INPUT = 1;
  SQL_PARAM_INPUT_OUTPUT = 2;
  SQL_RESULT_COL = 3;
  SQL_PARAM_OUTPUT = 4;
  SQL_RETURN_VALUE = 5;
// End of moved block
//=====================================

// Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter
  SQL_PARAM_TYPE_DEFAULT = SQL_PARAM_INPUT_OUTPUT;
  SQL_SETPARAM_VALUE_MAX = (-1);

// SQLColAttributes block
// WAS ORIGINALLY HERE
// Moved above because of dependent declarations

//*******************************************
// SQLGetFunctions: additional values for
// fFunction to represent functions that
// are not in the X/Open spec.
//*******************************************

  SQL_API_SQLALLOCHANDLESTD = 73;
  SQL_API_SQLBULKOPERATIONS = 24;
  SQL_API_SQLBINDPARAMETER = 72;
  SQL_API_SQLBROWSECONNECT = 55;
  SQL_API_SQLCOLATTRIBUTES = 6;
  SQL_API_SQLCOLUMNPRIVILEGES = 56;
  SQL_API_SQLDESCRIBEPARAM = 58;
  SQL_API_SQLDRIVERCONNECT = 41;
  SQL_API_SQLDRIVERS = 71;
  SQL_API_SQLEXTENDEDFETCH = 59;
  SQL_API_SQLFOREIGNKEYS = 60;
  SQL_API_SQLMORERESULTS = 61;
  SQL_API_SQLNATIVESQL = 62;
  SQL_API_SQLNUMPARAMS = 63;
  SQL_API_SQLPARAMOPTIONS = 64;
  SQL_API_SQLPRIMARYKEYS = 65;
  SQL_API_SQLPROCEDURECOLUMNS = 66;
  SQL_API_SQLPROCEDURES = 67;
  SQL_API_SQLSETPOS = 68;
  SQL_API_SQLSETSCROLLOPTIONS = 69;
  SQL_API_SQLTABLEPRIVILEGES = 70;


//--------------------------------------------
// SQL_API_ALL_FUNCTIONS returns an array
// of 'booleans' representing whether a
// function is implemented by the driver.
//
// CAUTION: Only functions defined in ODBC
// version 2.0 and earlier are returned, the
// new high-range function numbers defined by
// X/Open break this scheme. See the new
// method -- SQL_API_ODBC3_ALL_FUNCTIONS
//--------------------------------------------

  SQL_API_ALL_FUNCTIONS = 0; // See CAUTION above

//----------------------------------------------
// 2.X drivers export a dummy function with
// ordinal number SQL_API_LOADBYORDINAL to speed
// loading under the windows operating system.
//
// CAUTION: Loading by ordinal is not supported
// for 3.0 and above drivers.
//----------------------------------------------

  SQL_API_LOADBYORDINAL = 199; // See CAUTION above

//----------------------------------------------
// SQL_API_ODBC3_ALL_FUNCTIONS
// This returns a bitmap, which allows us to*
// handle the higher-valued function numbers.
// Use SQL_FUNC_EXISTS(bitmap,function_number)
// to determine if the function exists.
//----------------------------------------------

  SQL_API_ODBC3_ALL_FUNCTIONS = 999;
  SQL_API_ODBC3_ALL_FUNCTIONS_SIZE = 250; // array of 250 words

{$EXTERNALSYM SQL_FUNC_EXISTS}
function SQL_FUNC_EXISTS(pfExists: PUWORD; uwAPI: UWORD): SQLINTEGER;

const
//***********************************************
// Extended definitions for SQLGetInfo
//***********************************************

//---------------------------------
// Values in ODBC 2.0 that are not
// in the X/Open spec
//---------------------------------
  SQL_INFO_FIRST = 0;
  SQL_ACTIVE_CONNECTIONS = 0; // MAX_DRIVER_CONNECTIONS
  SQL_ACTIVE_STATEMENTS = 1; // MAX_CONCURRENT_ACTIVITIES
  SQL_DRIVER_HDBC = 3;
  SQL_DRIVER_HENV = 4;
  SQL_DRIVER_HSTMT = 5;
  SQL_DRIVER_NAME = 6;
  SQL_DRIVER_VER = 7;
  SQL_ODBC_API_CONFORMANCE = 9;
  SQL_ODBC_VER = 10;
  SQL_ROW_UPDATES = 11;
  SQL_ODBC_SAG_CLI_CONFORMANCE = 12;
  SQL_ODBC_SQL_CONFORMANCE = 15;
  SQL_PROCEDURES = 21;
  SQL_CONCAT_NULL_BEHAVIOR = 22;
  SQL_CURSOR_ROLLBACK_BEHAVIOR = 24;
  SQL_EXPRESSIONS_IN_ORDERBY = 27;
  SQL_MAX_OWNER_NAME_LEN = 32; // MAX_SCHEMA_NAME_LEN
  SQL_MAX_PROCEDURE_NAME_LEN = 33;
  SQL_MAX_QUALIFIER_NAME_LEN = 34; // MAX_CATALOG_NAME_LEN
  SQL_MULT_RESULT_SETS = 36;
  SQL_MULTIPLE_ACTIVE_TXN = 37;
  SQL_OUTER_JOINS = 38;
  SQL_OWNER_TERM = 39;
  SQL_PROCEDURE_TERM = 40;
  SQL_QUALIFIER_NAME_SEPARATOR = 41;
  SQL_QUALIFIER_TERM = 42;
  SQL_SCROLL_OPTIONS = 44;
  SQL_TABLE_TERM = 45;
  SQL_CONVERT_FUNCTIONS = 48;
  SQL_NUMERIC_FUNCTIONS = 49;
  SQL_STRING_FUNCTIONS = 50;
  SQL_SYSTEM_FUNCTIONS = 51;
  SQL_TIMEDATE_FUNCTIONS = 52;
  SQL_CONVERT_BIGINT = 53;
  SQL_CONVERT_BINARY = 54;
  SQL_CONVERT_BIT = 55;
  SQL_CONVERT_CHAR = 56;
  SQL_CONVERT_DATE = 57;
  SQL_CONVERT_DECIMAL = 58;
  SQL_CONVERT_DOUBLE = 59;
  SQL_CONVERT_FLOAT = 60;
  SQL_CONVERT_INTEGER = 61;
  SQL_CONVERT_LONGVARCHAR = 62;
  SQL_CONVERT_NUMERIC = 63;
  SQL_CONVERT_REAL = 64;
  SQL_CONVERT_SMALLINT = 65;
  SQL_CONVERT_TIME = 66;
  SQL_CONVERT_TIMESTAMP = 67;
  SQL_CONVERT_TINYINT = 68;
  SQL_CONVERT_VARBINARY = 69;
  SQL_CONVERT_VARCHAR = 70;
  SQL_CONVERT_LONGVARBINARY = 71;
  SQL_ODBC_SQL_OPT_IEF = 73; // SQL_INTEGRITY
  SQL_CORRELATION_NAME = 74;
  SQL_NON_NULLABLE_COLUMNS = 75;
  SQL_DRIVER_HLIB = 76;
  SQL_DRIVER_ODBC_VER = 77;
  SQL_LOCK_TYPES = 78;
  SQL_POS_OPERATIONS = 79;
  SQL_POSITIONED_STATEMENTS = 80;
  SQL_BOOKMARK_PERSISTENCE = 82;
  SQL_STATIC_SENSITIVITY = 83;
  SQL_FILE_USAGE = 84;
  SQL_COLUMN_ALIAS = 87;
  SQL_GROUP_BY = 88;
  SQL_KEYWORDS = 89;
  SQL_OWNER_USAGE = 91;
  SQL_QUALIFIER_USAGE = 92;
  SQL_QUOTED_IDENTIFIER_CASE = 93;
  SQL_SUBQUERIES = 95;
  SQL_UNION = 96;
  SQL_MAX_ROW_SIZE_INCLUDES_LONG = 103;
  SQL_MAX_CHAR_LITERAL_LEN = 108;
  SQL_TIMEDATE_ADD_INTERVALS = 109;
  SQL_TIMEDATE_DIFF_INTERVALS = 110;
  SQL_NEED_LONG_DATA_LEN = 111;
  SQL_MAX_BINARY_LITERAL_LEN = 112;
  SQL_LIKE_ESCAPE_CLAUSE = 113;
  SQL_QUALIFIER_LOCATION = 114;

//-----------------------------------------------
// ODBC 3.0 SQLGetInfo values that are not part
// of the X/Open standard at this time. X/Open
// standard values are in sql.h.
//-----------------------------------------------

  SQL_ACTIVE_ENVIRONMENTS = 116;
  SQL_ALTER_DOMAIN = 117;
  SQL_SQL_CONFORMANCE = 118;
  SQL_DATETIME_LITERALS = 119;
  SQL_ASYNC_MODE = 10021; // new X/Open spec
  SQL_BATCH_ROW_COUNT = 120;
  SQL_BATCH_SUPPORT = 121;
  SQL_CATALOG_LOCATION = SQL_QUALIFIER_LOCATION;
  SQL_CATALOG_NAME_SEPARATOR = SQL_QUALIFIER_NAME_SEPARATOR;
  SQL_CATALOG_TERM = SQL_QUALIFIER_TERM;
  SQL_CATALOG_USAGE = SQL_QUALIFIER_USAGE;
  SQL_CONVERT_WCHAR = 122;
  SQL_CONVERT_INTERVAL_DAY_TIME = 123;
  SQL_CONVERT_INTERVAL_YEAR_MONTH = 124;
  SQL_CONVERT_WLONGVARCHAR = 125;
  SQL_CONVERT_WVARCHAR = 126;
  SQL_CREATE_ASSERTION = 127;
  SQL_CREATE_CHARACTER_SET = 128;
  SQL_CREATE_COLLATION = 129;
  SQL_CREATE_DOMAIN = 130;
  SQL_CREATE_SCHEMA = 131;
  SQL_CREATE_TABLE = 132;
  SQL_CREATE_TRANSLATION = 133;
  SQL_CREATE_VIEW = 134;
  SQL_DRIVER_HDESC = 135;
  SQL_DROP_ASSERTION = 136;
  SQL_DROP_CHARACTER_SET = 137;
  SQL_DROP_COLLATION = 138;
  SQL_DROP_DOMAIN = 139;
  SQL_DROP_SCHEMA = 140;
  SQL_DROP_TABLE = 141;
  SQL_DROP_TRANSLATION = 142;
  SQL_DROP_VIEW = 143;
  SQL_DYNAMIC_CURSOR_ATTRIBUTES1 = 144;
  SQL_DYNAMIC_CURSOR_ATTRIBUTES2 = 145;
  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 = 146;
  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 = 147;
  SQL_INDEX_KEYWORDS = 148;
  SQL_INFO_SCHEMA_VIEWS = 149;
  SQL_KEYSET_CURSOR_ATTRIBUTES1 = 150;
  SQL_KEYSET_CURSOR_ATTRIBUTES2 = 151;
  SQL_MAX_ASYNC_CONCURRENT_STATEMENTS = 10022; // new X/Open spec
  SQL_ODBC_INTERFACE_CONFORMANCE = 152;
  SQL_PARAM_ARRAY_ROW_COUNTS = 153;
  SQL_PARAM_ARRAY_SELECTS = 154;
  SQL_SCHEMA_TERM = SQL_OWNER_TERM;
  SQL_SCHEMA_USAGE = SQL_OWNER_USAGE;
  SQL_SQL92_DATETIME_FUNCTIONS = 155;
  SQL_SQL92_FOREIGN_KEY_DELETE_RULE = 156;
  SQL_SQL92_FOREIGN_KEY_UPDATE_RULE = 157;
  SQL_SQL92_GRANT = 158;
  SQL_SQL92_NUMERIC_VALUE_FUNCTIONS = 159;
  SQL_SQL92_PREDICATES = 160;
  SQL_SQL92_RELATIONAL_JOIN_OPERATORS = 161;
  SQL_SQL92_REVOKE = 162;
  SQL_SQL92_ROW_VALUE_CONSTRUCTOR = 163;
  SQL_SQL92_STRING_FUNCTIONS = 164;
  SQL_SQL92_VALUE_EXPRESSIONS = 165;
  SQL_STANDARD_CLI_CONFORMANCE = 166;
  SQL_STATIC_CURSOR_ATTRIBUTES1 = 167;
  SQL_STATIC_CURSOR_ATTRIBUTES2 = 168;
  SQL_AGGREGATE_FUNCTIONS = 169;
  SQL_DDL_INDEX = 170;
  SQL_DM_VER = 171;
  SQL_INSERT_STATEMENT = 172;
  SQL_CONVERT_GUID = 173;
  SQL_UNION_STATEMENT = SQL_UNION;

  SQL_DTC_TRANSITION_COST = 1750;

// SQL_ALTER_TABLE bitmasks
// the following bitmasks are defined in sql.h
{
  SQL_AT_ADD_COLUMN = $00000001;
  SQL_AT_DROP_COLUMN = $00000002;
  SQL_AT_ADD_CONSTRAINT = $00000008;
//= ODBC 3
  SQL_AT_ADD_COLUMN_SINGLE = $00000020;
  SQL_AT_ADD_COLUMN_DEFAULT = $00000040;
  SQL_AT_ADD_COLUMN_COLLATION = $00000080;
  SQL_AT_SET_COLUMN_DEFAULT = $00000100;
  SQL_AT_DROP_COLUMN_DEFAULT = $00000200;
  SQL_AT_DROP_COLUMN_CASCADE = $00000400;
  SQL_AT_DROP_COLUMN_RESTRICT = $00000800;
  SQL_AT_ADD_TABLE_CONSTRAINT = $00001000;
  SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE = $00002000;
  SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT = $00004000;
  SQL_AT_CONSTRAINT_NAME_DEFINITION = $00008000;
  SQL_AT_CONSTRAINT_INITIALLY_DEFERRED = $00010000;
  SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE = $00020000;
  SQL_AT_CONSTRAINT_DEFERRABLE = $00040000;
  SQL_AT_CONSTRAINT_NON_DEFERRABLE = $00080000;
}
// SQL_CONVERT_* return value bitmasks
  SQL_CVT_CHAR = $00000001;
  SQL_CVT_NUMERIC = $00000002;
  SQL_CVT_DECIMAL = $00000004;
  SQL_CVT_INTEGER = $00000008;
  SQL_CVT_SMALLINT = $00000010;
  SQL_CVT_FLOAT = $00000020;
  SQL_CVT_REAL = $00000040;
  SQL_CVT_DOUBLE = $00000080;
  SQL_CVT_VARCHAR = $00000100;
  SQL_CVT_LONGVARCHAR = $00000200;
  SQL_CVT_BINARY = $00000400;
  SQL_CVT_VARBINARY = $00000800;
  SQL_CVT_BIT = $00001000;
  SQL_CVT_TINYINT = $00002000;
  SQL_CVT_BIGINT = $00004000;
  SQL_CVT_DATE = $00008000;
  SQL_CVT_TIME = $00010000;
  SQL_CVT_TIMESTAMP = $00020000;
  SQL_CVT_LONGVARBINARY = $00040000;
  SQL_CVT_INTERVAL_YEAR_MONTH = $00080000;
  SQL_CVT_INTERVAL_DAY_TIME = $00100000;
  SQL_CVT_WCHAR = $00200000;
  SQL_CVT_WLONGVARCHAR = $00400000;
  SQL_CVT_WVARCHAR = $00800000;
  SQL_CVT_GUID = $01000000;

// SQL_CONVERT_FUNCTIONS functions
  SQL_FN_CVT_CONVERT = $00000001;
  SQL_FN_CVT_CAST = $00000002;

// SQL_STRING_FUNCTIONS functions
  SQL_FN_STR_CONCAT = $00000001;
  SQL_FN_STR_INSERT = $00000002;
  SQL_FN_STR_LEFT = $00000004;
  SQL_FN_STR_LTRIM = $00000008;
  SQL_FN_STR_LENGTH = $00000010;
  SQL_FN_STR_LOCATE = $00000020;
  SQL_FN_STR_LCASE = $00000040;
  SQL_FN_STR_REPEAT = $00000080;
  SQL_FN_STR_REPLACE = $00000100;
  SQL_FN_STR_RIGHT = $00000200;
  SQL_FN_STR_RTRIM = $00000400;
  SQL_FN_STR_SUBSTRING = $00000800;
  SQL_FN_STR_UCASE = $00001000;
  SQL_FN_STR_ASCII = $00002000;
  SQL_FN_STR_CHAR = $00004000;
  SQL_FN_STR_DIFFERENCE = $00008000;
  SQL_FN_STR_LOCATE_2 = $00010000;
  SQL_FN_STR_SOUNDEX = $00020000;
  SQL_FN_STR_SPACE = $00040000;
  SQL_FN_STR_BIT_LENGTH = $00080000;
  SQL_FN_STR_CHAR_LENGTH = $00100000;
  SQL_FN_STR_CHARACTER_LENGTH = $00200000;
  SQL_FN_STR_OCTET_LENGTH = $00400000;
  SQL_FN_STR_POSITION = $00800000;

// SQL_SQL92_STRING_FUNCTIONS
  SQL_SSF_CONVERT = $00000001;
  SQL_SSF_LOWER = $00000002;
  SQL_SSF_UPPER = $00000004;
  SQL_SSF_SUBSTRING = $00000008;
  SQL_SSF_TRANSLATE = $00000010;
  SQL_SSF_TRIM_BOTH = $00000020;
  SQL_SSF_TRIM_LEADING = $00000040;
  SQL_SSF_TRIM_TRAILING = $00000080;

// SQL_NUMERIC_FUNCTIONS functions
  SQL_FN_NUM_ABS = $00000001;
  SQL_FN_NUM_ACOS = $00000002;
  SQL_FN_NUM_ASIN = $00000004;
  SQL_FN_NUM_ATAN = $00000008;
  SQL_FN_NUM_ATAN2 = $00000010;
  SQL_FN_NUM_CEILING = $00000020;
  SQL_FN_NUM_COS = $00000040;
  SQL_FN_NUM_COT = $00000080;
  SQL_FN_NUM_EXP = $00000100;
  SQL_FN_NUM_FLOOR = $00000200;
  SQL_FN_NUM_LOG = $00000400;
  SQL_FN_NUM_MOD = $00000800;
  SQL_FN_NUM_SIGN = $00001000;
  SQL_FN_NUM_SIN = $00002000;
  SQL_FN_NUM_SQRT = $00004000;
  SQL_FN_NUM_TAN = $00008000;
  SQL_FN_NUM_PI = $00010000;
  SQL_FN_NUM_RAND = $00020000;
  SQL_FN_NUM_DEGREES = $00040000;
  SQL_FN_NUM_LOG10 = $00080000;
  SQL_FN_NUM_POWER = $00100000;
  SQL_FN_NUM_RADIANS = $00200000;
  SQL_FN_NUM_ROUND = $00400000;
  SQL_FN_NUM_TRUNCATE = $00800000;

// SQL_SQL92_NUMERIC_VALUE_FUNCTIONS
  SQL_SNVF_BIT_LENGTH = $00000001;
  SQL_SNVF_CHAR_LENGTH = $00000002;
  SQL_SNVF_CHARACTER_LENGTH = $00000004;
  SQL_SNVF_EXTRACT = $00000008;
  SQL_SNVF_OCTET_LENGTH = $00000010;
  SQL_SNVF_POSITION = $00000020;

// SQL_TIMEDATE_FUNCTIONS functions
  SQL_FN_TD_NOW = $00000001;
  SQL_FN_TD_CURDATE = $00000002;
  SQL_FN_TD_DAYOFMONTH = $00000004;
  SQL_FN_TD_DAYOFWEEK = $00000008;
  SQL_FN_TD_DAYOFYEAR = $00000010;
  SQL_FN_TD_MONTH = $00000020;
  SQL_FN_TD_QUARTER = $00000040;
  SQL_FN_TD_WEEK = $00000080;
  SQL_FN_TD_YEAR = $00000100;
  SQL_FN_TD_CURTIME = $00000200;
  SQL_FN_TD_HOUR = $00000400;
  SQL_FN_TD_MINUTE = $00000800;
  SQL_FN_TD_SECOND = $00001000;
  SQL_FN_TD_TIMESTAMPADD = $00002000;
  SQL_FN_TD_TIMESTAMPDIFF = $00004000;
  SQL_FN_TD_DAYNAME = $00008000;
  SQL_FN_TD_MONTHNAME = $00010000;
  SQL_FN_TD_CURRENT_DATE = $00020000;
  SQL_FN_TD_CURRENT_TIME = $00040000;
  SQL_FN_TD_CURRENT_TIMESTAMP = $00080000;
  SQL_FN_TD_EXTRACT = $00100000;

// SQL_SQL92_DATETIME_FUNCTIONS
  SQL_SDF_CURRENT_DATE = $00000001;
  SQL_SDF_CURRENT_TIME = $00000002;
  SQL_SDF_CURRENT_TIMESTAMP = $00000004;

// SQL_SYSTEM_FUNCTIONS functions
  SQL_FN_SYS_USERNAME = $00000001;
  SQL_FN_SYS_DBNAME = $00000002;
  SQL_FN_SYS_IFNULL = $00000004;

// SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions
  SQL_FN_TSI_FRAC_SECOND = $00000001;
  SQL_FN_TSI_SECOND = $00000002;
  SQL_FN_TSI_MINUTE = $00000004;
  SQL_FN_TSI_HOUR = $00000008;
  SQL_FN_TSI_DAY = $00000010;
  SQL_FN_TSI_WEEK = $00000020;
  SQL_FN_TSI_MONTH = $00000040;
  SQL_FN_TSI_QUARTER = $00000080;
  SQL_FN_TSI_YEAR = $00000100;

// bitmasks for SQL_DYNAMIC_CURSOR_ATTRIBUTES1,
//- SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1,
//- SQL_KEYSET_CURSOR_ATTRIBUTES1, and SQL_STATIC_CURSOR_ATTRIBUTES1
//=

// supported SQLFetchScroll FetchOrientation's
  SQL_CA1_NEXT = $00000001;
  SQL_CA1_ABSOLUTE = $00000002;
  SQL_CA1_RELATIVE = $00000004;
  SQL_CA1_BOOKMARK = $00000008;

// supported SQLSetPos LockType's
  SQL_CA1_LOCK_NO_CHANGE = $00000040;
  SQL_CA1_LOCK_EXCLUSIVE = $00000080;
  SQL_CA1_LOCK_UNLOCK = $00000100;

// supported SQLSetPos Operations
  SQL_CA1_POS_POSITION = $00000200;
  SQL_CA1_POS_UPDATE = $00000400;
  SQL_CA1_POS_DELETE = $00000800;
  SQL_CA1_POS_REFRESH = $00001000;

// positioned updates and deletes
  SQL_CA1_POSITIONED_UPDATE = $00002000;
  SQL_CA1_POSITIONED_DELETE = $00004000;
  SQL_CA1_SELECT_FOR_UPDATE = $00008000;

// supported SQLBulkOperations operations
  SQL_CA1_BULK_ADD = $00010000;
  SQL_CA1_BULK_UPDATE_BY_BOOKMARK = $00020000;
  SQL_CA1_BULK_DELETE_BY_BOOKMARK = $00040000;
  SQL_CA1_BULK_FETCH_BY_BOOKMARK = $00080000;

// bitmasks for SQL_DYNAMIC_CURSOR_ATTRIBUTES2,
//- SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,
//- SQL_KEYSET_CURSOR_ATTRIBUTES2, and SQL_STATIC_CURSOR_ATTRIBUTES2
//=

// supported values for SQL_ATTR_SCROLL_CONCURRENCY
  SQL_CA2_READ_ONLY_CONCURRENCY = $00000001;
  SQL_CA2_LOCK_CONCURRENCY = $00000002;
  SQL_CA2_OPT_ROWVER_CONCURRENCY = $00000004;
  SQL_CA2_OPT_VALUES_CONCURRENCY = $00000008;

// sensitivity of the cursor to its own inserts, deletes, and updates
  SQL_CA2_SENSITIVITY_ADDITIONS = $00000010;
  SQL_CA2_SENSITIVITY_DELETIONS = $00000020;
  SQL_CA2_SENSITIVITY_UPDATES = $00000040;

// semantics of SQL_ATTR_MAX_ROWS
  SQL_CA2_MAX_ROWS_SELECT = $00000080;
  SQL_CA2_MAX_ROWS_INSERT = $00000100;
  SQL_CA2_MAX_ROWS_DELETE = $00000200;
  SQL_CA2_MAX_ROWS_UPDATE = $00000400;
  SQL_CA2_MAX_ROWS_CATALOG = $00000800;
  SQL_CA2_MAX_ROWS_AFFECTS_ALL = (SQL_CA2_MAX_ROWS_SELECT or
    SQL_CA2_MAX_ROWS_INSERT or SQL_CA2_MAX_ROWS_DELETE or
    SQL_CA2_MAX_ROWS_UPDATE or SQL_CA2_MAX_ROWS_CATALOG);

// semantics of SQL_DIAG_CURSOR_ROW_COUNT
  SQL_CA2_CRC_EXACT = $00001000;
  SQL_CA2_CRC_APPROXIMATE = $00002000;

// the kinds of positioned statements that can be simulated
  SQL_CA2_SIMULATE_NON_UNIQUE = $00004000;
  SQL_CA2_SIMULATE_TRY_UNIQUE = $00008000;
  SQL_CA2_SIMULATE_UNIQUE = $00010000;

// SQL_ODBC_API_CONFORMANCE values
  SQL_OAC_NONE = $0000;
  SQL_OAC_LEVEL1 = $0001;
  SQL_OAC_LEVEL2 = $0002;

// SQL_ODBC_SAG_CLI_CONFORMANCE values
  SQL_OSCC_NOT_COMPLIANT = $0000;
  SQL_OSCC_COMPLIANT = $0001;

// SQL_ODBC_SQL_CONFORMANCE values
  SQL_OSC_MINIMUM = $0000;
  SQL_OSC_CORE = $0001;
  SQL_OSC_EXTENDED = $0002;

// SQL_CONCAT_NULL_BEHAVIOR values
  SQL_CB_NULL = $0000;
  SQL_CB_NON_NULL = $0001;

// SQL_SCROLL_OPTIONS masks
  SQL_SO_FORWARD_ONLY = $00000001;
  SQL_SO_KEYSET_DRIVEN = $00000002;
  SQL_SO_DYNAMIC = $00000004;
  SQL_SO_MIXED = $00000008;
  SQL_SO_STATIC = $00000010;

// SQL_FETCH_DIRECTION masks
  SQL_FD_FETCH_RESUME = $00000040; // SQL_FETCH_RESUME is no longer supported
  SQL_FD_FETCH_BOOKMARK = $00000080;

// SQL_TXN_ISOLATION_OPTION masks
  SQL_TXN_VERSIONING = $00000010; // SQL_TXN_VERSIONING is no longer supported

// SQL_CORRELATION_NAME values
  SQL_CN_NONE = $0000;
  SQL_CN_DIFFERENT = $0001;
  SQL_CN_ANY = $0002;

// SQL_NON_NULLABLE_COLUMNS values
  SQL_NNC_NULL = $0000;
  SQL_NNC_NON_NULL = $0001;

// SQL_NULL_COLLATION values
  SQL_NC_START = $0002;
  SQL_NC_END = $0004;

// SQL_FILE_USAGE values
  SQL_FILE_NOT_SUPPORTED = $0000;
  SQL_FILE_TABLE = $0001;
  SQL_FILE_QUALIFIER = $0002;
  SQL_FILE_CATALOG = SQL_FILE_QUALIFIER; // ODBC 3.0

// SQL_GETDATA_EXTENSIONS values
  SQL_GD_BLOCK = $00000004;
  SQL_GD_BOUND = $00000008;

// SQL_POSITIONED_STATEMENTS masks
  SQL_PS_POSITIONED_DELETE = $00000001;
  SQL_PS_POSITIONED_UPDATE = $00000002;
  SQL_PS_SELECT_FOR_UPDATE = $00000004;

// SQL_GROUP_BY values
  SQL_GB_NOT_SUPPORTED = $0000;
  SQL_GB_GROUP_BY_EQUALS_SELECT = $0001;
  SQL_GB_GROUP_BY_CONTAINS_SELECT = $0002;
  SQL_GB_NO_RELATION = $0003;
  SQL_GB_COLLATE = $0004;

// SQL_OWNER_USAGE masks
  SQL_OU_DML_STATEMENTS = $00000001;
  SQL_OU_PROCEDURE_INVOCATION = $00000002;
  SQL_OU_TABLE_DEFINITION = $00000004;
  SQL_OU_INDEX_DEFINITION = $00000008;
  SQL_OU_PRIVILEGE_DEFINITION = $00000010;

// SQL_SCHEMA_USAGE masks
  SQL_SU_DML_STATEMENTS = SQL_OU_DML_STATEMENTS;
  SQL_SU_PROCEDURE_INVOCATION = SQL_OU_PROCEDURE_INVOCATION;
  SQL_SU_TABLE_DEFINITION = SQL_OU_TABLE_DEFINITION;
  SQL_SU_INDEX_DEFINITION = SQL_OU_INDEX_DEFINITION;
  SQL_SU_PRIVILEGE_DEFINITION = SQL_OU_PRIVILEGE_DEFINITION;

// SQL_QUALIFIER_USAGE masks
  SQL_QU_DML_STATEMENTS = $00000001;
  SQL_QU_PROCEDURE_INVOCATION = $00000002;
  SQL_QU_TABLE_DEFINITION = $00000004;
  SQL_QU_INDEX_DEFINITION = $00000008;
  SQL_QU_PRIVILEGE_DEFINITION = $00000010;

// SQL_CATALOG_USAGE masks
  SQL_CU_DML_STATEMENTS = SQL_QU_DML_STATEMENTS;
  SQL_CU_PROCEDURE_INVOCATION = SQL_QU_PROCEDURE_INVOCATION;
  SQL_CU_TABLE_DEFINITION = SQL_QU_TABLE_DEFINITION;
  SQL_CU_INDEX_DEFINITION = SQL_QU_INDEX_DEFINITION;
  SQL_CU_PRIVILEGE_DEFINITION = SQL_QU_PRIVILEGE_DEFINITION;

// SQL_SUBQUERIES masks
  SQL_SQ_COMPARISON = $00000001;
  SQL_SQ_EXISTS = $00000002;
  SQL_SQ_IN = $00000004;
  SQL_SQ_QUANTIFIED = $00000008;
  SQL_SQ_CORRELATED_SUBQUERIES = $00000010;

// SQL_UNION masks
  SQL_U_UNION = $00000001;
  SQL_U_UNION_ALL = $00000002;

// SQL_BOOKMARK_PERSISTENCE values
  SQL_BP_CLOSE = $00000001;
  SQL_BP_DELETE = $00000002;
  SQL_BP_DROP = $00000004;
  SQL_BP_TRANSACTION = $00000008;
  SQL_BP_UPDATE = $00000010;
  SQL_BP_OTHER_HSTMT = $00000020;
  SQL_BP_SCROLL = $00000040;

// SQL_STATIC_SENSITIVITY values
  SQL_SS_ADDITIONS = $00000001;
  SQL_SS_DELETIONS = $00000002;
  SQL_SS_UPDATES = $00000004;

// SQL_VIEW values
  SQL_CV_CREATE_VIEW = $00000001;
  SQL_CV_CHECK_OPTION = $00000002;
  SQL_CV_CASCADED = $00000004;
  SQL_CV_LOCAL = $00000008;

// SQL_LOCK_TYPES masks
  SQL_LCK_NO_CHANGE = $00000001;
  SQL_LCK_EXCLUSIVE = $00000002;
  SQL_LCK_UNLOCK = $00000004;

// SQL_POS_OPERATIONS masks
  SQL_POS_POSITION = $00000001;
  SQL_POS_REFRESH = $00000002;
  SQL_POS_UPDATE = $00000004;
  SQL_POS_DELETE = $00000008;
  SQL_POS_ADD = $00000010;

// SQL_QUALIFIER_LOCATION values
  SQL_QL_START = $0001;
  SQL_QL_END = $0002;


// Here start return values for ODBC 3.0 SQLGetInfo

// SQL_AGGREGATE_FUNCTIONS bitmasks
  SQL_AF_AVG = $00000001;
  SQL_AF_COUNT = $00000002;
  SQL_AF_MAX = $00000004;
  SQL_AF_MIN = $00000008;
  SQL_AF_SUM = $00000010;
  SQL_AF_DISTINCT = $00000020;
  SQL_AF_ALL = $00000040;

// SQL_SQL_CONFORMANCE bit masks
  SQL_SC_SQL92_ENTRY = $00000001;
  SQL_SC_FIPS127_2_TRANSITIONAL = $00000002;
  SQL_SC_SQL92_INTERMEDIATE = $00000004;
  SQL_SC_SQL92_FULL = $00000008;

// SQL_DATETIME_LITERALS masks
  SQL_DL_SQL92_DATE = $00000001;
  SQL_DL_SQL92_TIME = $00000002;
  SQL_DL_SQL92_TIMESTAMP = $00000004;
  SQL_DL_SQL92_INTERVAL_YEAR = $00000008;
  SQL_DL_SQL92_INTERVAL_MONTH = $00000010;
  SQL_DL_SQL92_INTERVAL_DAY = $00000020;
  SQL_DL_SQL92_INTERVAL_HOUR = $00000040;
  SQL_DL_SQL92_INTERVAL_MINUTE = $00000080;
  SQL_DL_SQL92_INTERVAL_SECOND = $00000100;
  SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH = $00000200;
  SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR = $00000400;
  SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE = $00000800;
  SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND = $00001000;
  SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE = $00002000;
  SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND = $00004000;
  SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND = $00008000;

// SQL_CATALOG_LOCATION values
  SQL_CL_START = SQL_QL_START;
  SQL_CL_END = SQL_QL_END;

// values for SQL_BATCH_ROW_COUNT
  SQL_BRC_PROCEDURES = $0000001;
  SQL_BRC_EXPLICIT = $0000002;
  SQL_BRC_ROLLED_UP = $0000004;

// bitmasks for SQL_BATCH_SUPPORT
  SQL_BS_SELECT_EXPLICIT = $00000001;
  SQL_BS_ROW_COUNT_EXPLICIT = $00000002;
  SQL_BS_SELECT_PROC = $00000004;
  SQL_BS_ROW_COUNT_PROC = $00000008;

// Values for SQL_PARAM_ARRAY_ROW_COUNTS getinfo
  SQL_PARC_BATCH = 1;
  SQL_PARC_NO_BATCH = 2;

// values for SQL_PARAM_ARRAY_SELECTS
  SQL_PAS_BATCH = 1;
  SQL_PAS_NO_BATCH = 2;
  SQL_PAS_NO_SELECT = 3;

// Bitmasks for SQL_INDEX_KEYWORDS
  SQL_IK_NONE = $00000000;
  SQL_IK_ASC = $00000001;
  SQL_IK_DESC = $00000002;
  SQL_IK_ALL = (SQL_IK_ASC or SQL_IK_DESC);

// Bitmasks for SQL_INFO_SCHEMA_VIEWS
  SQL_ISV_ASSERTIONS = $00000001;
  SQL_ISV_CHARACTER_SETS = $00000002;
  SQL_ISV_CHECK_CONSTRAINTS = $00000004;
  SQL_ISV_COLLATIONS = $00000008;
  SQL_ISV_COLUMN_DOMAIN_USAGE = $00000010;
  SQL_ISV_COLUMN_PRIVILEGES = $00000020;
  SQL_ISV_COLUMNS = $00000040;
  SQL_ISV_CONSTRAINT_COLUMN_USAGE = $00000080;
  SQL_ISV_CONSTRAINT_TABLE_USAGE = $00000100;
  SQL_ISV_DOMAIN_CONSTRAINTS = $00000200;
  SQL_ISV_DOMAINS = $00000400;
  SQL_ISV_KEY_COLUMN_USAGE = $00000800;
  SQL_ISV_REFERENTIAL_CONSTRAINTS = $00001000;
  SQL_ISV_SCHEMATA = $00002000;
  SQL_ISV_SQL_LANGUAGES = $00004000;
  SQL_ISV_TABLE_CONSTRAINTS = $00008000;
  SQL_ISV_TABLE_PRIVILEGES = $00010000;
  SQL_ISV_TABLES = $00020000;
  SQL_ISV_TRANSLATIONS = $00040000;
  SQL_ISV_USAGE_PRIVILEGES = $00080000;
  SQL_ISV_VIEW_COLUMN_USAGE = $00100000;
  SQL_ISV_VIEW_TABLE_USAGE = $00200000;
  SQL_ISV_VIEWS = $00400000;

// Bitmasks for SQL_ASYNC_MODE
// Already declared in sql.h
{
  SQL_AM_NONE = 0;
  SQL_AM_CONNECTION = 1;
  SQL_AM_STATEMENT = 2;
}

// Bitmasks for SQL_ALTER_DOMAIN
  SQL_AD_CONSTRAINT_NAME_DEFINITION = $00000001;
  SQL_AD_ADD_DOMAIN_CONSTRAINT = $00000002;
  SQL_AD_DROP_DOMAIN_CONSTRAINT = $00000004;
  SQL_AD_ADD_DOMAIN_DEFAULT = $00000008;
  SQL_AD_DROP_DOMAIN_DEFAULT = $00000010;
  SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED = $00000020;
  SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_AD_ADD_CONSTRAINT_DEFERRABLE = $00000080;
  SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE = $00000100;

// SQL_CREATE_SCHEMA bitmasks
  SQL_CS_CREATE_SCHEMA = $00000001;
  SQL_CS_AUTHORIZATION = $00000002;
  SQL_CS_DEFAULT_CHARACTER_SET = $00000004;

// SQL_CREATE_TRANSLATION bitmasks
  SQL_CTR_CREATE_TRANSLATION = $00000001;

// SQL_CREATE_ASSERTION bitmasks
  SQL_CA_CREATE_ASSERTION = $00000001;
  SQL_CA_CONSTRAINT_INITIALLY_DEFERRED = $00000010;
  SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE = $00000020;
  SQL_CA_CONSTRAINT_DEFERRABLE = $00000040;
  SQL_CA_CONSTRAINT_NON_DEFERRABLE = $00000080;

// SQL_CREATE_CHARACTER_SET bitmasks
  SQL_CCS_CREATE_CHARACTER_SET = $00000001;
  SQL_CCS_COLLATE_CLAUSE = $00000002;
  SQL_CCS_LIMITED_COLLATION = $00000004;

// SQL_CREATE_COLLATION bitmasks
  SQL_CCOL_CREATE_COLLATION = $00000001;

// SQL_CREATE_DOMAIN bitmasks
  SQL_CDO_CREATE_DOMAIN = $00000001;
  SQL_CDO_DEFAULT = $00000002;
  SQL_CDO_CONSTRAINT = $00000004;
  SQL_CDO_COLLATION = $00000008;
  SQL_CDO_CONSTRAINT_NAME_DEFINITION = $00000010;
  SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED = $00000020;
  SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_CDO_CONSTRAINT_DEFERRABLE = $00000080;
  SQL_CDO_CONSTRAINT_NON_DEFERRABLE = $00000100;

// SQL_CREATE_TABLE bitmasks
  SQL_CT_CREATE_TABLE = $00000001;
  SQL_CT_COMMIT_PRESERVE = $00000002;
  SQL_CT_COMMIT_DELETE = $00000004;
  SQL_CT_GLOBAL_TEMPORARY = $00000008;
  SQL_CT_LOCAL_TEMPORARY = $00000010;
  SQL_CT_CONSTRAINT_INITIALLY_DEFERRED = $00000020;
  SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_CT_CONSTRAINT_DEFERRABLE = $00000080;
  SQL_CT_CONSTRAINT_NON_DEFERRABLE = $00000100;
  SQL_CT_COLUMN_CONSTRAINT = $00000200;
  SQL_CT_COLUMN_DEFAULT = $00000400;
  SQL_CT_COLUMN_COLLATION = $00000800;
  SQL_CT_TABLE_CONSTRAINT = $00001000;
  SQL_CT_CONSTRAINT_NAME_DEFINITION = $00002000;

// SQL_DDL_INDEX bitmasks
  SQL_DI_CREATE_INDEX = $00000001;
  SQL_DI_DROP_INDEX = $00000002;

// SQL_DROP_COLLATION bitmasks
  SQL_DC_DROP_COLLATION = $00000001;

// SQL_DROP_DOMAIN bitmasks
  SQL_DD_DROP_DOMAIN = $00000001;
  SQL_DD_RESTRICT = $00000002;
  SQL_DD_CASCADE = $00000004;

// SQL_DROP_SCHEMA bitmasks
  SQL_DS_DROP_SCHEMA = $00000001;
  SQL_DS_RESTRICT = $00000002;
  SQL_DS_CASCADE = $00000004;

// SQL_DROP_CHARACTER_SET bitmasks
  SQL_DCS_DROP_CHARACTER_SET = $00000001;

// SQL_DROP_ASSERTION bitmasks
  SQL_DA_DROP_ASSERTION = $00000001;

// SQL_DROP_TABLE bitmasks
  SQL_DT_DROP_TABLE = $00000001;
  SQL_DT_RESTRICT = $00000002;
  SQL_DT_CASCADE = $00000004;

// SQL_DROP_TRANSLATION bitmasks
  SQL_DTR_DROP_TRANSLATION = $00000001;

// SQL_DROP_VIEW bitmasks
  SQL_DV_DROP_VIEW = $00000001;
  SQL_DV_RESTRICT = $00000002;
  SQL_DV_CASCADE = $00000004;

// SQL_INSERT_STATEMENT bitmasks
  SQL_IS_INSERT_LITERALS = $00000001;
  SQL_IS_INSERT_SEARCHED = $00000002;
  SQL_IS_SELECT_INTO = $00000004;

// SQL_ODBC_INTERFACE_CONFORMANCE values
  SQL_OIC_CORE = ULONG(1); // 1.0 TRUE
  SQL_OIC_LEVEL1 = ULONG(2);
  SQL_OIC_LEVEL2 = ULONG(3);

// SQL_SQL92_FOREIGN_KEY_DELETE_RULE bitmasks
  SQL_SFKD_CASCADE = $00000001;
  SQL_SFKD_NO_ACTION = $00000002;
  SQL_SFKD_SET_DEFAULT = $00000004;
  SQL_SFKD_SET_NULL = $00000008;

// SQL_SQL92_FOREIGN_KEY_UPDATE_RULE bitmasks
  SQL_SFKU_CASCADE = $00000001;
  SQL_SFKU_NO_ACTION = $00000002;
  SQL_SFKU_SET_DEFAULT = $00000004;
  SQL_SFKU_SET_NULL = $00000008;

// SQL_SQL92_GRANT bitmasks
  SQL_SG_USAGE_ON_DOMAIN = $00000001;
  SQL_SG_USAGE_ON_CHARACTER_SET = $00000002;
  SQL_SG_USAGE_ON_COLLATION = $00000004;
  SQL_SG_USAGE_ON_TRANSLATION = $00000008;
  SQL_SG_WITH_GRANT_OPTION = $00000010;
  SQL_SG_DELETE_TABLE = $00000020;
  SQL_SG_INSERT_TABLE = $00000040;
  SQL_SG_INSERT_COLUMN = $00000080;
  SQL_SG_REFERENCES_TABLE = $00000100;
  SQL_SG_REFERENCES_COLUMN = $00000200;
  SQL_SG_SELECT_TABLE = $00000400;
  SQL_SG_UPDATE_TABLE = $00000800;
  SQL_SG_UPDATE_COLUMN = $00001000;

// SQL_SQL92_PREDICATES bitmasks
  SQL_SP_EXISTS = $00000001;
  SQL_SP_ISNOTNULL = $00000002;
  SQL_SP_ISNULL = $00000004;
  SQL_SP_MATCH_FULL = $00000008;
  SQL_SP_MATCH_PARTIAL = $00000010;
  SQL_SP_MATCH_UNIQUE_FULL = $00000020;
  SQL_SP_MATCH_UNIQUE_PARTIAL = $00000040;
  SQL_SP_OVERLAPS = $00000080;
  SQL_SP_UNIQUE = $00000100;
  SQL_SP_LIKE = $00000200;
  SQL_SP_IN = $00000400;
  SQL_SP_BETWEEN = $00000800;
  SQL_SP_COMPARISON = $00001000;
  SQL_SP_QUANTIFIED_COMPARISON = $00002000;

// SQL_SQL92_RELATIONAL_JOIN_OPERATORS bitmasks
  SQL_SRJO_CORRESPONDING_CLAUSE = $00000001;
  SQL_SRJO_CROSS_JOIN = $00000002;
  SQL_SRJO_EXCEPT_JOIN = $00000004;
  SQL_SRJO_FULL_OUTER_JOIN = $00000008;
  SQL_SRJO_INNER_JOIN = $00000010;
  SQL_SRJO_INTERSECT_JOIN = $00000020;
  SQL_SRJO_LEFT_OUTER_JOIN = $00000040;
  SQL_SRJO_NATURAL_JOIN = $00000080;
  SQL_SRJO_RIGHT_OUTER_JOIN = $00000100;
  SQL_SRJO_UNION_JOIN = $00000200;

// SQL_SQL92_REVOKE bitmasks
  SQL_SR_USAGE_ON_DOMAIN = $00000001;
  SQL_SR_USAGE_ON_CHARACTER_SET = $00000002;
  SQL_SR_USAGE_ON_COLLATION = $00000004;
  SQL_SR_USAGE_ON_TRANSLATION = $00000008;
  SQL_SR_GRANT_OPTION_FOR = $00000010;
  SQL_SR_CASCADE = $00000020;
  SQL_SR_RESTRICT = $00000040;
  SQL_SR_DELETE_TABLE = $00000080;
  SQL_SR_INSERT_TABLE = $00000100;
  SQL_SR_INSERT_COLUMN = $00000200;
  SQL_SR_REFERENCES_TABLE = $00000400;
  SQL_SR_REFERENCES_COLUMN = $00000800;
  SQL_SR_SELECT_TABLE = $00001000;
  SQL_SR_UPDATE_TABLE = $00002000;
  SQL_SR_UPDATE_COLUMN = $00004000;

// SQL_SQL92_ROW_VALUE_CONSTRUCTOR bitmasks
  SQL_SRVC_VALUE_EXPRESSION = $00000001;
  SQL_SRVC_NULL = $00000002;
  SQL_SRVC_DEFAULT = $00000004;
  SQL_SRVC_ROW_SUBQUERY = $00000008;

// SQL_SQL92_VALUE_EXPRESSIONS bitmasks
  SQL_SVE_CASE = $00000001;
  SQL_SVE_CAST = $00000002;
  SQL_SVE_COALESCE = $00000004;
  SQL_SVE_NULLIF = $00000008;

// SQL_STANDARD_CLI_CONFORMANCE bitmasks
  SQL_SCC_XOPEN_CLI_VERSION1 = $00000001;
  SQL_SCC_ISO92_CLI = $00000002;

// SQL_UNION_STATEMENT bitmasks
  SQL_US_UNION = SQL_U_UNION;
  SQL_US_UNION_ALL = SQL_U_UNION_ALL;

// SQL_DTC_TRANSITION_COST bitmasks
  SQL_DTC_ENLIST_EXPENSIVE = $00000001;
  SQL_DTC_UNENLIST_EXPENSIVE = $00000002;

// additional SQLDataSources fetch directions
  SQL_FETCH_FIRST_USER = 31;
  SQL_FETCH_FIRST_SYSTEM = 32;

// Defines for SQLSetPos
  SQL_ENTIRE_ROWSET = 0;

// Operations in SQLSetPos
  SQL_POSITION = 0; // 1.0 FALSE
  SQL_REFRESH = 1; // 1.0 TRUE
  SQL_UPDATE = 2;
  SQL_DELETE = 3;

// Operations in SQLBulkOperations
  SQL_ADD = 4;
  SQL_SETPOS_MAX_OPTION_VALUE = SQL_ADD;
  SQL_UPDATE_BY_BOOKMARK = 5;
  SQL_DELETE_BY_BOOKMARK = 6;
  SQL_FETCH_BY_BOOKMARK = 7;

// Lock options in SQLSetPos
  SQL_LOCK_NO_CHANGE = 0; // 1.0 FALSE
  SQL_LOCK_EXCLUSIVE = 1; // 1.0 TRUE
  SQL_LOCK_UNLOCK = 2;

  SQL_SETPOS_MAX_LOCK_VALUE = SQL_LOCK_UNLOCK;

// Macros for SQLSetPos
{$EXTERNALSYM SQL_POSITION_TO}
function SQL_POSITION_TO(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
{$EXTERNALSYM SQL_LOCK_RECORD}
function SQL_LOCK_RECORD(hstmt: SQLHSTMT; irow, fLock: SQLUSMALLINT): SQLRETURN;
{$EXTERNALSYM SQL_REFRESH_RECORD}
function SQL_REFRESH_RECORD(hstmt: SQLHSTMT; irow, fLock: SQLUSMALLINT): SQLRETURN;
{$EXTERNALSYM SQL_UPDATE_RECORD}
function SQL_UPDATE_RECORD(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
{$EXTERNALSYM SQL_DELETE_RECORD}
function SQL_DELETE_RECORD(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
{$EXTERNALSYM SQL_ADD_RECORD}
function SQL_ADD_RECORD(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;

const

// Column types and scopes in SQLSpecialColumns.
  SQL_BEST_ROWID = 1;
  SQL_ROWVER = 2;

// Defines for SQLSpecialColumns (returned in the result set
//= SQL_PC_UNKNOWN and SQL_PC_PSEUDO are defined in sql.h
  SQL_PC_NOT_PSEUDO = 1;

// Defines for SQLStatistics
  SQL_QUICK = 0;
  SQL_ENSURE = 1;

// Defines for SQLStatistics (returned in the result set)
//-SQL_INDEX_CLUSTERED, SQL_INDEX_HASHED, and SQL_INDEX_OTHER are
//=defined in sql.h
  SQL_TABLE_STAT = 0;

// Defines for SQLTables
  SQL_ALL_CATALOGS = '%';
  SQL_ALL_SCHEMAS = '%';
  SQL_ALL_TABLE_TYPES = '%';

// Options for SQLDriverConnect
  SQL_DRIVER_NOPROMPT = 0;
  SQL_DRIVER_COMPLETE = 1;
  SQL_DRIVER_PROMPT = 2;
  SQL_DRIVER_COMPLETE_REQUIRED = 3;

{$EXTERNALSYM SQLDriverConnect}
function SQLDriverConnect(
  hdbc: SQLHDBC;
  hwnd: SQLHWND;
  szConnStrIn: PSQLTCHAR;
  cbConnStrIn: SQLSMALLINT;
  szConnStrOut: PSQLTCHAR;
  cbConnStrOutMax: SQLSMALLINT;
  pcbConnStrOut: PSQLSMALLINT;
  fDriverCompletion: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDriverConnectA}
function SQLDriverConnectA(
  hdbc: SQLHDBC;
  hwnd: SQLHWND;
  szConnStrIn: PSQLCHAR;
  cbConnStrIn: SQLSMALLINT;
  szConnStrOut: PSQLCHAR;
  cbConnStrOutMax: SQLSMALLINT;
  pcbConnStrOut: PSQLSMALLINT;
  fDriverCompletion: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDriverConnectW}
function SQLDriverConnectW(
  hdbc: SQLHDBC;
  hwnd: SQLHWND;
  szConnStrIn: PSQLWCHAR;
  cbConnStrIn: SQLSMALLINT;
  szConnStrOut: PSQLWCHAR;
  cbConnStrOutMax: SQLSMALLINT;
  pcbConnStrOut: PSQLSMALLINT;
  fDriverCompletion: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

// Level 2 Functions

// SQLExtendedFetch "fFetchType" values
const
  SQL_FETCH_BOOKMARK = 8;

// SQLExtendedFetch "rgfRowStatus" element values
  SQL_ROW_SUCCESS = 0;
  SQL_ROW_DELETED = 1;
  SQL_ROW_UPDATED = 2;
  SQL_ROW_NOROW = 3;
  SQL_ROW_ADDED = 4;
  SQL_ROW_ERROR = 5;
  SQL_ROW_SUCCESS_WITH_INFO = 6;
  SQL_ROW_PROCEED = 0;
  SQL_ROW_IGNORE = 1;

// value for SQL_DESC_ARRAY_STATUS_PTR
  SQL_PARAM_SUCCESS = 0;
  SQL_PARAM_SUCCESS_WITH_INFO = 6;
  SQL_PARAM_ERROR = 5;
  SQL_PARAM_UNUSED = 7;
  SQL_PARAM_DIAG_UNAVAILABLE = 1;
  SQL_PARAM_PROCEED = 0;
  SQL_PARAM_IGNORE = 1;

// Defines for SQLForeignKeys (UPDATE_RULE and DELETE_RULE)
  SQL_CASCADE = 0;
  SQL_RESTRICT = 1;
  SQL_SET_NULL = 2;
  SQL_NO_ACTION = 3;
  SQL_SET_DEFAULT = 4;

// Note that the following are in a different column of SQLForeignKeys than
// the previous #defines.   These are for DEFERRABILITY.
  SQL_INITIALLY_DEFERRED = 5;
  SQL_INITIALLY_IMMEDIATE = 6;
  SQL_NOT_DEFERRABLE = 7;

// SQLBindParameter block
// WAS ORIGINALLY HERE
// Moved above because of dependent declarations

// Defines for SQLProcedures (returned in the result set)
  SQL_PT_UNKNOWN = 0;
  SQL_PT_PROCEDURE = 1;
  SQL_PT_FUNCTION = 2;

// This define is too large for RC
  SQL_ODBC_KEYWORDS =
    'ABSOLUTE,ACTION,ADA,ADD,ALL,ALLOCATE,ALTER,AND,ANY,ARE,AS,' +
    'ASC,ASSERTION,AT,AUTHORIZATION,AVG,' +
    'BEGIN,BETWEEN,BIT,BIT_LENGTH,BOTH,BY,CASCADE,CASCADED,CASE,CAST,CATALOG,' +
    'CHAR,CHAR_LENGTH,CHARACTER,CHARACTER_LENGTH,CHECK,CLOSE,COALESCE,' +
    'COLLATE,COLLATION,COLUMN,COMMIT,CONNECT,CONNECTION,CONSTRAINT,' +
    'CONSTRAINTS,CONTINUE,CONVERT,CORRESPONDING,COUNT,CREATE,CROSS,CURRENT,' +
    'CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,CURSOR,' +
    'DATE,DAY,DEALLOCATE,DEC,DECIMAL,DECLARE,DEFAULT,DEFERRABLE,' +
    'DEFERRED,DELETE,DESC,DESCRIBE,DESCRIPTOR,DIAGNOSTICS,DISCONNECT,' +
    'DISTINCT,DOMAIN,DOUBLE,DROP,' +
    'ELSE,END,END-EXEC,ESCAPE,EXCEPT,EXCEPTION,EXEC,EXECUTE,' +
    'EXISTS,EXTERNAL,EXTRACT,' +
    'FALSE,FETCH,FIRST,FLOAT,FOR,FOREIGN,FORTRAN,FOUND,FROM,FULL,' +
    'GET,GLOBAL,GO,GOTO,GRANT,GROUP,HAVING,HOUR,' +
    'IDENTITY,IMMEDIATE,IN,INCLUDE,INDEX,INDICATOR,INITIALLY,INNER,' +
    'INPUT,INSENSITIVE,INSERT,INT,INTEGER,INTERSECT,INTERVAL,INTO,IS,ISOLATION,' +
    'JOIN,KEY,LANGUAGE,LAST,LEADING,LEFT,LEVEL,LIKE,LOCAL,LOWER,' +
    'MATCH,MAX,MIN,MINUTE,MODULE,MONTH,' +
    'NAMES,NATIONAL,NATURAL,NCHAR,NEXT,NO,NONE,NOT,NULL,NULLIF,NUMERIC,' +
    'OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,ORDER,OUTER,OUTPUT,OVERLAPS,' +
    'PAD,PARTIAL,PASCAL,PLI,POSITION,PRECISION,PREPARE,PRESERVE,' +
    'PRIMARY,PRIOR,PRIVILEGES,PROCEDURE,PUBLIC,' +
    'READ,REAL,REFERENCES,RELATIVE,RESTRICT,REVOKE,RIGHT,ROLLBACK,ROWS' +
    'SCHEMA,SCROLL,SECOND,SECTION,SELECT,SESSION,SESSION_USER,SET,SIZE,' +
    'SMALLINT,SOME,SPACE,SQL,SQLCA,SQLCODE,SQLERROR,SQLSTATE,SQLWARNING,' +
    'SUBSTRING,SUM,SYSTEM_USER,' +
    'TABLE,TEMPORARY,THEN,TIME,TIMESTAMP,TIMEZONE_HOUR,TIMEZONE_MINUTE,' +
    'TO,TRAILING,TRANSACTION,TRANSLATE,TRANSLATION,TRIM,TRUE,' +
    'UNION,UNIQUE,UNKNOWN,UPDATE,UPPER,USAGE,USER,USING,' +
    'VALUE,VALUES,VARCHAR,VARYING,VIEW,WHEN,WHENEVER,WHERE,WITH,WORK,WRITE,' +
    'YEAR,ZONE';

{$EXTERNALSYM SQLBrowseConnect}
function SQLBrowseConnect(
  hdbc: SQLHDBC;
  szConnStrIn: PSQLTCHAR;
  cbConnStrIn: SQLSMALLINT;
  szConnStrOut: PSQLTCHAR;
  cbConnStrOutMax: SQLSMALLINT;
  pcbConnStrOut: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLBrowseConnectA}
function SQLBrowseConnectA(
  hdbc: SQLHDBC;
  szConnStrIn: PSQLCHAR;
  cbConnStrIn: SQLSMALLINT;
  szConnStrOut: PSQLCHAR;
  cbConnStrOutMax: SQLSMALLINT;
  pcbConnStrOut: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLBrowseConnectW}
function SQLBrowseConnectW(
  hdbc: SQLHDBC;
  szConnStrIn: PSQLWCHAR;
  cbConnStrIn: SQLSMALLINT;
  szConnStrOut: PSQLWCHAR;
  cbConnStrOutMax: SQLSMALLINT;
  pcbConnStrOut: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLBulkOperations}
function SQLBulkOperations(
  StatementHandle: SQLHSTMT;
  Operation: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLColAttributes}
function SQLColAttributes(
  hstmt: SQLHSTMT;
  icol: SQLUSMALLINT;
  fDescType: SQLUSMALLINT;
  rgbDesc: SQLPOINTER;
  cbDescMax: SQLSMALLINT;
  pcbDesc: PSQLSMALLINT;
  pfDesc: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColAttributesA}
function SQLColAttributesA(
  hstmt: SQLHSTMT;
  icol: SQLUSMALLINT;
  fDescType: SQLUSMALLINT;
  rgbDesc: SQLPOINTER;
  cbDescMax: SQLSMALLINT;
  pcbDesc: PSQLSMALLINT;
  pfDesc: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColAttributesW}
function SQLColAttributesW(
  hstmt: SQLHSTMT;
  icol: SQLUSMALLINT;
  fDescType: SQLUSMALLINT;
  rgbDesc: SQLPOINTER;
  cbDescMax: SQLSMALLINT;
  pcbDesc: PSQLSMALLINT;
  pfDesc: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLColumnPrivileges}
function SQLColumnPrivileges(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLTCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLTCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLTCHAR;
  cbTableName: SQLSMALLINT;
  szColumnName: PSQLTCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColumnPrivilegesA}
function SQLColumnPrivilegesA(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLCHAR;
  cbTableName: SQLSMALLINT;
  szColumnName: PSQLCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLColumnPrivilegesW}
function SQLColumnPrivilegesW(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLWCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLWCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLWCHAR;
  cbTableName: SQLSMALLINT;
  szColumnName: PSQLWCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLDescribeParam}
function SQLDescribeParam(
  hstmt: SQLHSTMT;
  ipar: SQLUSMALLINT;
  pfSqlType: PSQLSMALLINT;
  pcbParamDef: PSQLUINTEGER;
  pibScale: PSQLSMALLINT;
  pfNullable: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLExtendedFetch}
function SQLExtendedFetch(
  hstmt: SQLHSTMT;
  fFetchType: SQLUSMALLINT;
  irow: SQLINTEGER;
  pcrow: PSQLUINTEGER;
  rgfRowStatus: PSQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLForeignKeys}
function SQLForeignKeys(
  hstmt: SQLHSTMT;
  szPkCatalogName: PSQLTCHAR;
  cbPkCatalogName: SQLSMALLINT;
  szPkSchemaName: PSQLTCHAR;
  cbPkSchemaName: SQLSMALLINT;
  szPkTableName: PSQLTCHAR;
  cbPkTableName: SQLSMALLINT;
  szFkCatalogName: PSQLTCHAR;
  cbFkCatalogName: SQLSMALLINT;
  szFkSchemaName: PSQLTCHAR;
  cbFkSchemaName: SQLSMALLINT;
  szFkTableName: PSQLTCHAR;
  cbFkTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLForeignKeysA}
function SQLForeignKeysA(
  hstmt: SQLHSTMT;
  szPkCatalogName: PSQLCHAR;
  cbPkCatalogName: SQLSMALLINT;
  szPkSchemaName: PSQLCHAR;
  cbPkSchemaName: SQLSMALLINT;
  szPkTableName: PSQLCHAR;
  cbPkTableName: SQLSMALLINT;
  szFkCatalogName: PSQLCHAR;
  cbFkCatalogName: SQLSMALLINT;
  szFkSchemaName: PSQLCHAR;
  cbFkSchemaName: SQLSMALLINT;
  szFkTableName: PSQLCHAR;
  cbFkTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLForeignKeysW}
function SQLForeignKeysW(
  hstmt: SQLHSTMT;
  szPkCatalogName: PSQLWCHAR;
  cbPkCatalogName: SQLSMALLINT;
  szPkSchemaName: PSQLWCHAR;
  cbPkSchemaName: SQLSMALLINT;
  szPkTableName: PSQLWCHAR;
  cbPkTableName: SQLSMALLINT;
  szFkCatalogName: PSQLWCHAR;
  cbFkCatalogName: SQLSMALLINT;
  szFkSchemaName: PSQLWCHAR;
  cbFkSchemaName: SQLSMALLINT;
  szFkTableName: PSQLWCHAR;
  cbFkTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLMoreResults}
function SQLMoreResults(
  hstmt: SQLHSTMT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLNativeSql}
function SQLNativeSql(
  hdbc: SQLHDBC;
  szSqlStrIn: PSQLTCHAR;
  cbSqlStrIn: SQLINTEGER;
  szSqlStr: PSQLTCHAR;
  cbSqlStrMax: SQLINTEGER;
  pcbSqlStr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLNativeSqlA}
function SQLNativeSqlA(
  hdbc: SQLHDBC;
  szSqlStrIn: PSQLCHAR;
  cbSqlStrIn: SQLINTEGER;
  szSqlStr: PSQLCHAR;
  cbSqlStrMax: SQLINTEGER;
  pcbSqlStr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLNativeSqlW}
function SQLNativeSqlW(
  hdbc: SQLHDBC;
  szSqlStrIn: PSQLWCHAR;
  cbSqlStrIn: SQLINTEGER;
  szSqlStr: PSQLWCHAR;
  cbSqlStrMax: SQLINTEGER;
  pcbSqlStr: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLNumParams}
function SQLNumParams(
  hstmt: SQLHSTMT;
  pcpar: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLParamOptions}
function SQLParamOptions(
  hstmt: SQLHSTMT;
  crow: SQLUINTEGER;
  pirow: PSQLUINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLPrimaryKeys}
function SQLPrimaryKeys(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLTCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLTCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLTCHAR;
  cbTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLPrimaryKeysA}
function SQLPrimaryKeysA(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLCHAR;
  cbTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLPrimaryKeysW}
function SQLPrimaryKeysW(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLWCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLWCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLWCHAR;
  cbTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLProcedureColumns}
function SQLProcedureColumns(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLTCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLTCHAR;
  cbSchemaName: SQLSMALLINT;
  szProcName: PSQLTCHAR;
  cbProcName: SQLSMALLINT;
  szColumnName: PSQLTCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLProcedureColumnsA}
function SQLProcedureColumnsA(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR;
  cbSchemaName: SQLSMALLINT;
  szProcName: PSQLCHAR;
  cbProcName: SQLSMALLINT;
  szColumnName: PSQLCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLProcedureColumnsW}
function SQLProcedureColumnsW(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLWCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLWCHAR;
  cbSchemaName: SQLSMALLINT;
  szProcName: PSQLWCHAR;
  cbProcName: SQLSMALLINT;
  szColumnName: PSQLWCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLProcedures}
function SQLProcedures(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLTCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLTCHAR;
  cbSchemaName: SQLSMALLINT;
  szProcName: PSQLTCHAR;
  cbProcName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLProceduresA}
function SQLProceduresA(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR;
  cbSchemaName: SQLSMALLINT;
  szProcName: PSQLCHAR;
  cbProcName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLProceduresW}
function SQLProceduresW(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLWCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLWCHAR;
  cbSchemaName: SQLSMALLINT;
  szProcName: PSQLWCHAR;
  cbProcName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLSetPos}
function SQLSetPos(
  hstmt: SQLHSTMT;
  irow: SQLUSMALLINT;
  fOption: SQLUSMALLINT;
  fLock: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLTablePrivileges}
function SQLTablePrivileges(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLCHAR;
  cbTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLTablePrivilegesA}
function SQLTablePrivilegesA(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLCHAR;
  cbTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLTablePrivilegesW}
function SQLTablePrivilegesW(
  hstmt: SQLHSTMT;
  szCatalogName: PSQLWCHAR;
  cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLWCHAR;
  cbSchemaName: SQLSMALLINT;
  szTableName: PSQLWCHAR;
  cbTableName: SQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLDrivers}
function SQLDrivers(
  henv: SQLHENV;
  fDirection: SQLUSMALLINT;
  szDriverDesc: PSQLTCHAR;
  cbDriverDescMax: SQLSMALLINT;
  pcbDriverDesc: PSQLSMALLINT;
  szDriverAttributes: PSQLTCHAR;
  cbDrvrAttrMax: SQLSMALLINT;
  pcbDrvrAttr: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDriversA}
function SQLDriversA(
  henv: SQLHENV;
  fDirection: SQLUSMALLINT;
  szDriverDesc: PSQLCHAR;
  cbDriverDescMax: SQLSMALLINT;
  pcbDriverDesc: PSQLSMALLINT;
  szDriverAttributes: PSQLCHAR;
  cbDrvrAttrMax: SQLSMALLINT;
  pcbDrvrAttr: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
{$EXTERNALSYM SQLDriversW}
function SQLDriversW(
  henv: SQLHENV;
  fDirection: SQLUSMALLINT;
  szDriverDesc: PSQLWCHAR;
  cbDriverDescMax: SQLSMALLINT;
  pcbDriverDesc: PSQLSMALLINT;
  szDriverAttributes: PSQLWCHAR;
  cbDrvrAttrMax: SQLSMALLINT;
  pcbDrvrAttr: PSQLSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM SQLBindParameter}
function SQLBindParameter(
  hstmt: SQLHSTMT;
  ipar: SQLUSMALLINT;
  fParamType: SQLSMALLINT;
  fCType: SQLSMALLINT;
  fSqlType: SQLSMALLINT;
  cbColDef: SQLUINTEGER;
  ibScale: SQLSMALLINT;
  rgbValue: SQLPOINTER;
  cbValueMax: SQLINTEGER;
  pcbValue: PSQLINTEGER): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

//---------------------------------------------------------
// SQLAllocHandleStd is implemented to make SQLAllocHandle
// compatible with X/Open standard. an application should
// not call SQLAllocHandleStd directly
//---------------------------------------------------------

// Internal type subcodes (ODBC_STD, ie X/OPEN)
const
  SQL_YEAR = SQL_CODE_YEAR;
  SQL_MONTH = SQL_CODE_MONTH;
  SQL_DAY = SQL_CODE_DAY;
  SQL_HOUR = SQL_CODE_HOUR;
  SQL_MINUTE = SQL_CODE_MINUTE;
  SQL_SECOND = SQL_CODE_SECOND;
  SQL_YEAR_TO_MONTH = SQL_CODE_YEAR_TO_MONTH;
  SQL_DAY_TO_HOUR = SQL_CODE_DAY_TO_HOUR;
  SQL_DAY_TO_MINUTE = SQL_CODE_DAY_TO_MINUTE;
  SQL_DAY_TO_SECOND = SQL_CODE_DAY_TO_SECOND;
  SQL_HOUR_TO_MINUTE = SQL_CODE_HOUR_TO_MINUTE;
  SQL_HOUR_TO_SECOND = SQL_CODE_HOUR_TO_SECOND;
  SQL_MINUTE_TO_SECOND = SQL_CODE_MINUTE_TO_SECOND;

{$EXTERNALSYM SQLAllocHandleStd}
function SQLAllocHandleStd(
  fHandleType: SQLSMALLINT;
  hInput: SQLHANDLE;
  phOutput: PSQLHANDLE): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};


// Deprecated defines from prior versions of ODBC
const
  SQL_DATABASE_NAME = 16; // Use SQLGetConnectOption/SQL_CURRENT_QUALIFIER
  SQL_FD_FETCH_PREV = SQL_FD_FETCH_PRIOR;
  SQL_FETCH_PREV = SQL_FETCH_PRIOR;
  SQL_CONCUR_TIMESTAMP = SQL_CONCUR_ROWVER;
  SQL_SCCO_OPT_TIMESTAMP = SQL_SCCO_OPT_ROWVER;
  SQL_CC_DELETE = SQL_CB_DELETE;
  SQL_CR_DELETE = SQL_CB_DELETE;
  SQL_CC_CLOSE = SQL_CB_CLOSE;
  SQL_CR_CLOSE = SQL_CB_CLOSE;
  SQL_CC_PRESERVE = SQL_CB_PRESERVE;
  SQL_CR_PRESERVE = SQL_CB_PRESERVE;
  SQL_FETCH_RESUME = 7; // is not supported by 2.0+ drivers
  SQL_SCROLL_FORWARD_ONLY = 0; //-SQL_CURSOR_FORWARD_ONLY
  SQL_SCROLL_KEYSET_DRIVEN = (-1); //-SQL_CURSOR_KEYSET_DRIVEN
  SQL_SCROLL_DYNAMIC = (-2); //-SQL_CURSOR_DYNAMIC
  SQL_SCROLL_STATIC = (-3); //*-SQL_CURSOR_STATIC

// Deprecated functions from prior versions of ODBC
// Use SQLSetStmtOptions
{$EXTERNALSYM SQLSetScrollOptions}
function SQLSetScrollOptions(
  hstmt: SQLHSTMT;
  fConcurrency: SQLUSMALLINT;
  crowKeyset: SQLINTEGER;
  crowRowset: SQLUSMALLINT): SQLRETURN {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

// Tracing section
const
  TRACE_VERSION = 1000; // Version of trace API

{$EXTERNALSYM TraceOpenLogFile}
function TraceOpenLogFile(
  _1: PWideChar;
  _2: PWideChar;
  _3: LongInt
  ): RETCODE {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM TraceCloseLogFile}
function TraceCloseLogFile: RETCODE {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM TraceReturn}
procedure TraceReturn(
  _1: RETCODE;
  _2: RETCODE) {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

{$EXTERNALSYM TraceVersion}
function TraceVersion: LongInt {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

// Functions for Visual Studio Analyzer
// to turn on/off tracing or VS events,
// call TraceVSControl by setting or clearing the following bits
const
  TRACE_ON = $00000001;
  TRACE_VS_EVENT_ON = $00000002;

{$EXTERNALSYM TraceVSControl}
function TraceVSControl(_1: LongInt): RETCODE {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};

// Functions for setting the connection pooling failure detection code
// The "TryWait" value is the time (in seconds) that the DM will wait
// between detecting that a connection is dead (using
// SQL_ATTR_CONNECTION_DEAD) and retrying the connection. During that
// interval, connection requests will get "The server appears to be
// dead" error returns.

{$EXTERNALSYM ODBCSetTryWaitValue}
function ODBCSetTryWaitValue(dwValue: LongInt): LongBool {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
//= In seconds

{$EXTERNALSYM ODBCGetTryWaitValue}
function ODBCGetTryWaitValue: LongInt {$ifdef MSWINDOWS} stdcall {$else} cdecl {$endif};
//= In Milliseconds(!)

// the flags in ODBC_VS_ARGS
const
  ODBC_VS_FLAG_UNICODE_ARG = $00000001; // the argument is unicode
  ODBC_VS_FLAG_UNICODE_COR = $00000002; // the correlation is unicode
  ODBC_VS_FLAG_RETCODE = $00000004; // RetCode field is set
  ODBC_VS_FLAG_STOP = $00000008; // Stop firing visual studio analyzer events

type
  tagODBC_VS_ARGS = record
    PGuidEvent: PGUID;  // the GUID for event
    dwFlags: LongWord;  // flags for the call
    case integer of
      0: (wszArg: PSQLWCHAR;
          szArg: PSQLCHAR;
          RetCode: RETCODE);
      1: (wszCorrelation: PSQLWCHAR;
          szCorrelation: PSQLCHAR;
          RetCode2: RETCODE);
    end;
  tagODBC_VS_ARGSW = record
    PGuidEvent: PGUID;  // the GUID for event
    dwFlags: LongWord;  // flags for the call
    case integer of
      0: (wszArg: PSQLWCHAR;
          szArg: PSQLCHAR;
          RetCode: RETCODE);
      1: (wszCorrelation: PSQLWCHAR;
          szCorrelation: PSQLCHAR;
          RetCode2: RETCODE);
    end;

  ODBC_VS_ARGS = tagODBC_VS_ARGS;
  PODBC_VS_ARGS = ^ODBC_VS_ARGS;
  ODBC_VS_ARGSW = tagODBC_VS_ARGSW;
  PODBC_VS_ARGSW = ^ODBC_VS_ARGSW;

{$EXTERNALSYM FireVSDebugEvent}
procedure FireVSDebugEvent(Args: PODBC_VS_ARGS);


//##########################################################################
// sqlext.h interface part ends here
//##########################################################################

//##########################################################################
// odbcinst.h interface section starts here
//##########################################################################

const
  // SQLConfigDataSource request flags
  {$EXTERNALSYM ODBC_ADD_DSN}
  ODBC_ADD_DSN            = 1;  // Add data source
  {$EXTERNALSYM ODBC_CONFIG_DSN}
  ODBC_CONFIG_DSN         = 2;  // Configure(edit) data source
  {$EXTERNALSYM ODBC_REMOVE_DSN}
  ODBC_REMOVE_DSN         = 3;  // Remove data source
  {$EXTERNALSYM ODBC_ADD_SYS_DSN}
  ODBC_ADD_SYS_DSN        = 4;  // add a system DSN
  {$EXTERNALSYM ODBC_CONFIG_SYS_DSN}
  ODBC_CONFIG_SYS_DSN     = 5;  // Configure a system DSN
  {$EXTERNALSYM ODBC_REMOVE_SYS_DSN}
  ODBC_REMOVE_SYS_DSN     = 6;  // remove a system DSN
  {$EXTERNALSYM ODBC_REMOVE_DEFAULT_DSN}
  ODBC_REMOVE_DEFAULT_DSN = 7;  // remove the default DSN

  // install request flags
  {$EXTERNALSYM ODBC_INSTALL_INQUIRY}
  ODBC_INSTALL_INQUIRY  = 1;
  {$EXTERNALSYM ODBC_INSTALL_COMPLETE}
  ODBC_INSTALL_COMPLETE = 2;

  // config driver flags
  {$EXTERNALSYM ODBC_INSTALL_DRIVER}
  ODBC_INSTALL_DRIVER    = 1;
  {$EXTERNALSYM ODBC_REMOVE_DRIVER}
  ODBC_REMOVE_DRIVER     = 2;
  {$EXTERNALSYM ODBC_CONFIG_DRIVER}
  ODBC_CONFIG_DRIVER     = 3;
  {$EXTERNALSYM ODBC_CONFIG_DRIVER_MAX}
  ODBC_CONFIG_DRIVER_MAX = 100;

  // SQLGetConfigMode and SQLSetConfigMode flags
  {$EXTERNALSYM ODBC_BOTH_DSN}
  ODBC_BOTH_DSN   = 0;
  {$EXTERNALSYM ODBC_USER_DSN}
  ODBC_USER_DSN   = 1;
  {$EXTERNALSYM ODBC_SYSTEM_DSN}
  ODBC_SYSTEM_DSN = 2;

  // SQLInstallerError code
  {$EXTERNALSYM ODBC_ERROR_GENERAL_ERR}
  ODBC_ERROR_GENERAL_ERR                  = 1;
  {$EXTERNALSYM ODBC_ERROR_INVALID_BUFF_LEN}
  ODBC_ERROR_INVALID_BUFF_LEN             = 2;
  {$EXTERNALSYM ODBC_ERROR_INVALID_HWND}
  ODBC_ERROR_INVALID_HWND                 = 3;
  {$EXTERNALSYM ODBC_ERROR_INVALID_STR}
  ODBC_ERROR_INVALID_STR                  = 4;
  {$EXTERNALSYM ODBC_ERROR_INVALID_REQUEST_TYPE}
  ODBC_ERROR_INVALID_REQUEST_TYPE         = 5;
  {$EXTERNALSYM ODBC_ERROR_COMPONENT_NOT_FOUND}
  ODBC_ERROR_COMPONENT_NOT_FOUND          = 6;
  {$EXTERNALSYM ODBC_ERROR_INVALID_NAME}
  ODBC_ERROR_INVALID_NAME                 = 7;
  {$EXTERNALSYM ODBC_ERROR_INVALID_KEYWORD_VALUE}
  ODBC_ERROR_INVALID_KEYWORD_VALUE        = 8;
  {$EXTERNALSYM ODBC_ERROR_INVALID_DSN}
  ODBC_ERROR_INVALID_DSN                  = 9;
  {$EXTERNALSYM ODBC_ERROR_INVALID_INF}
  ODBC_ERROR_INVALID_INF                  = 10;
  {$EXTERNALSYM ODBC_ERROR_REQUEST_FAILED}
  ODBC_ERROR_REQUEST_FAILED               = 11;
  {$EXTERNALSYM ODBC_ERROR_INVALID_PATH}
  ODBC_ERROR_INVALID_PATH                 = 12;
  {$EXTERNALSYM ODBC_ERROR_LOAD_LIB_FAILED}
  ODBC_ERROR_LOAD_LIB_FAILED              = 13;
  {$EXTERNALSYM ODBC_ERROR_INVALID_PARAM_SEQUENCE}
  ODBC_ERROR_INVALID_PARAM_SEQUENCE       = 14;
  {$EXTERNALSYM ODBC_ERROR_INVALID_LOG_FILE}
  ODBC_ERROR_INVALID_LOG_FILE             = 15;
  {$EXTERNALSYM ODBC_ERROR_USER_CANCELED}
  ODBC_ERROR_USER_CANCELED                = 16;
  {$EXTERNALSYM ODBC_ERROR_USAGE_UPDATE_FAILED}
  ODBC_ERROR_USAGE_UPDATE_FAILED          = 17;
  {$EXTERNALSYM ODBC_ERROR_CREATE_DSN_FAILED}
  ODBC_ERROR_CREATE_DSN_FAILED            = 18;
  {$EXTERNALSYM ODBC_ERROR_WRITING_SYSINFO_FAILED}
  ODBC_ERROR_WRITING_SYSINFO_FAILED       = 19;
  {$EXTERNALSYM ODBC_ERROR_REMOVE_DSN_FAILED}
  ODBC_ERROR_REMOVE_DSN_FAILED            = 20;
  {$EXTERNALSYM ODBC_ERROR_OUT_OF_MEM}
  ODBC_ERROR_OUT_OF_MEM                   = 21;
  {$EXTERNALSYM ODBC_ERROR_OUTPUT_STRING_TRUNCATED}
  ODBC_ERROR_OUTPUT_STRING_TRUNCATED      = 22;

// High level APIs

{$EXTERNALSYM SQLInstallODBC}
function SQLInstallODBC(hwndParent: HWND;
  lpszInfFile: LPCTSTR;
  lpszSrcPath: LPCTSTR;
  lpszDrivers: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLInstallODBCA}
function SQLInstallODBCA(hwndParent: HWND;
  lpszInfFile: LPCSTR;
  lpszSrcPath: LPCSTR;
  lpszDrivers: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLInstallODBCW}
function SQLInstallODBCW(hwndParent: HWND;
  lpszInfFile: LPCWSTR;
  lpszSrcPath: LPCWSTR;
  lpszDrivers: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLManageDataSources}
function SQLManageDataSources(hwndParent: HWND): BOOL; stdcall;

{$EXTERNALSYM SQLCreateDataSource}
function SQLCreateDataSource(hwndParent: HWND;
  lpszDSN: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLCreateDataSourceA}
function SQLCreateDataSourceA(hwndParent: HWND;
  lpszDSN: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLCreateDataSourceW}
function SQLCreateDataSourceW(hwndParent: HWND;
  lpszDSN: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLGetTranslator}
function SQLGetTranslator(hwnd: HWND;
  lpszName: LPTSTR;
  cbNameMax: WORD;
  var pcbNameOut: WORD;
  lpszPath: LPTSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD;
  var pvOption: DWORD): BOOL; stdcall;
{$EXTERNALSYM SQLGetTranslatorA}
function SQLGetTranslatorA(hwnd: HWND;
  lpszName: LPSTR;
  cbNameMax: WORD;
  var pcbNameOut: WORD;
  lpszPath: LPSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD;
  var pvOption: DWORD): BOOL; stdcall;
{$EXTERNALSYM SQLGetTranslatorW}
function SQLGetTranslatorW(hwnd: HWND;
  lpszName: LPWSTR;
  cbNameMax: WORD;
  var pcbNameOut: WORD;
  lpszPath: LPWSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD;
  var pvOption: DWORD): BOOL; stdcall;

// Low level APIs
// NOTE: The high-level APIs should always be used. These APIs
//       have been left for compatibility.

{$EXTERNALSYM SQLInstallDriver}
function SQLInstallDriver(lpszInfFile: LPCSTR;
  lpszDriver: LPCTSTR;
  lpszPath: LPTSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallDriverA}
function SQLInstallDriverA(lpszInfFile: LPCSTR;
  lpszDriver: LPCSTR;
  lpszPath: LPSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallDriverW}
function SQLInstallDriverW(lpszInfFile: LPCWSTR;
  lpszDriver: LPCWSTR;
  lpszPath: LPWSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD): BOOL; stdcall;

{$EXTERNALSYM SQLInstallDriverManager}
function SQLInstallDriverManager(lpszPath: LPTSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallDriverManagerA}
function SQLInstallDriverManagerA(lpszPath: LPSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallDriverManagerW}
function SQLInstallDriverManagerW(lpszPath: LPWSTR;
  cbPathMax: WORD;
  var pcbPathOut: WORD): BOOL; stdcall;

{$EXTERNALSYM SQLGetInstalledDrivers}
function SQLGetInstalledDrivers(lpszBuf: LPTSTR;
  cbBufMax: WORD;
  var pcbBufOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLGetInstalledDriversA}
function SQLGetInstalledDriversA(lpszBuf: LPSTR;
  cbBufMax: WORD;
  var pcbBufOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLGetInstalledDriversW}
function SQLGetInstalledDriversW(lpszBuf: LPWSTR;
  cbBufMax: WORD;
  var pcbBufOut: WORD): BOOL; stdcall;

{$EXTERNALSYM SQLGetAvailableDrivers}
function SQLGetAvailableDrivers(lpszInfFile: LPCTSTR;
  lpszBuf: LPSTR;
  cbBufMax: WORD;
  var pcbBufOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLGetAvailableDriversA}
function SQLGetAvailableDriversA(lpszInfFile: LPCSTR;
  lpszBuf: LPSTR;
  cbBufMax: WORD;
  var pcbBufOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLGetAvailableDriversW}
function SQLGetAvailableDriversW(lpszInfFile: LPCWSTR;
  lpszBuf: LPWSTR;
  cbBufMax: WORD;
  var pcbBufOut: WORD): BOOL; stdcall;

{$EXTERNALSYM SQLConfigDataSource}
function SQLConfigDataSource(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCTSTR;
  lpszAttributes: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLConfigDataSourceA}
function SQLConfigDataSourceA(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCSTR;
  lpszAttributes: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLConfigDataSourceW}
function SQLConfigDataSourceW(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCWSTR;
  lpszAttributes: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLRemoveDefaultDataSource():}
function SQLRemoveDefaultDataSource(): BOOL; stdcall;

{$EXTERNALSYM SQLWriteDSNToIni}
function SQLWriteDSNToIni(lpszDSN: LPCTSTR;
  lpszDriver: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLWriteDSNToIniA}
function SQLWriteDSNToIniA(lpszDSN: LPCSTR;
  lpszDriver: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLWriteDSNToIniW}
function SQLWriteDSNToIniW(lpszDSN: LPCWSTR;
  lpszDriver: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLRemoveDSNFromIni}
function SQLRemoveDSNFromIni(lpszDSN: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLRemoveDSNFromIniA}
function SQLRemoveDSNFromIniA(lpszDSN: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLRemoveDSNFromIniW}
function SQLRemoveDSNFromIniW(lpszDSN: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLValidDSN}
function SQLValidDSN(lpszDSN: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLValidDSNA}
function SQLValidDSNA(lpszDSN: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLValidDSNW}
function SQLValidDSNW(lpszDSN: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLWritePrivateProfileString}
function SQLWritePrivateProfileString(lpszSection: LPCTSTR;
  lpszEntry: LPCTSTR;
  lpszString: LPCTSTR;
  lpszFilename: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLWritePrivateProfileStringA}
function SQLWritePrivateProfileStringA(lpszSection: LPCSTR;
  lpszEntry: LPCSTR;
  lpszString: LPCSTR;
  lpszFilename: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLWritePrivateProfileStringW}
function SQLWritePrivateProfileStringW(lpszSection: LPCWSTR;
  lpszEntry: LPCWSTR;
  lpszString: LPCWSTR;
  lpszFilename: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLGetPrivateProfileString}
function SQLGetPrivateProfileString(lpszSection: LPCTSTR;
  lpszEntry: LPCTSTR;
  lpszDefault: LPCTSTR;
  lpszRetBuffer: LPTSTR;
  cbRetBuffer: Int32;
  lpszFilename: LPCSTR): Int32; stdcall;
{$EXTERNALSYM SQLGetPrivateProfileStringA}
function SQLGetPrivateProfileStringA(lpszSection: LPCSTR;
  lpszEntry: LPCSTR;
  lpszDefault: LPCSTR;
  lpszRetBuffer: LPSTR;
  cbRetBuffer: Int32;
  lpszFilename: LPCSTR): Int32; stdcall;
{$EXTERNALSYM SQLGetPrivateProfileStringW}
function SQLGetPrivateProfileStringW(lpszSection: LPCWSTR;
  lpszEntry: LPCWSTR;
  lpszDefault: LPCWSTR;
  lpszRetBuffer: LPWSTR;
  cbRetBuffer: Int32;
  lpszFilename: LPCWSTR): Int32; stdcall;

{$EXTERNALSYM SQLRemoveDriverManager}
function SQLRemoveDriverManager(lpdwUsageCount: LPDWORD): BOOL; stdcall;

{$EXTERNALSYM SQLInstallTranslator}
function SQLInstallTranslator(lpszInfFile: LPCTSTR;
  lpszTranslator: LPCTSTR;
  lpszPathIn: LPCTSTR;
  lpszPathOut: LPTSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallTranslatorA}
function SQLInstallTranslatorA(lpszInfFile: LPCSTR;
  lpszTranslator: LPCSTR;
  lpszPathIn: LPCSTR;
  lpszPathOut: LPSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallTranslatorW}
function SQLInstallTranslatorW(lpszInfFile: LPCWSTR;
  lpszTranslator: LPCWSTR;
  lpszPathIn: LPCWSTR;
  lpszPathOut: LPWSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;

{$EXTERNALSYM SQLRemoveTranslator}
function SQLRemoveTranslator(lpszTranslator: LPCTSTR;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLRemoveTranslatorA}
function SQLRemoveTranslatorA(lpszTranslator: LPCSTR;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLRemoveTranslatorW}
function SQLRemoveTranslatorW(lpszTranslator: LPCWSTR;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;

{$EXTERNALSYM SQLRemoveDriver}
function SQLRemoveDriver(lpszDriver: LPCTSTR;
  RemoveDSN: BOOL;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLRemoveDriverA}
function SQLRemoveDriverA(lpszDriver: LPCSTR;
  RemoveDSN: BOOL;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLRemoveDriverW}
function SQLRemoveDriverW(lpszDriver: LPCWSTR;
  RemoveDSN: BOOL;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;

{$EXTERNALSYM SQLConfigDriver}
function SQLConfigDriver(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCTSTR;
  lpszArgs: LPCTSTR;
  lpszMsg: LPTSTR;
  cbMsgMax: WORD;
  var pcbMsgOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLConfigDriverA}
function SQLConfigDriverA(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCSTR;
  lpszArgs: LPCSTR;
  lpszMsg: LPSTR;
  cbMsgMax: WORD;
  var pcbMsgOut: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLConfigDriverW}
function SQLConfigDriverW(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCWSTR;
  lpszArgs: LPCWSTR;
  lpszMsg: LPWSTR;
  cbMsgMax: WORD;
  var pcbMsgOut: WORD): BOOL; stdcall;

{$EXTERNALSYM SQLInstallerError}
function SQLInstallerError(iError: WORD;
  var pfErrorCode: DWORD;
  lpszErrorMsg: LPTSTR;
  cbErrorMsgMax: WORD;
  var pcbErrorMsg: WORD): SQLRETURN; stdcall;
{$EXTERNALSYM SQLInstallerErrorA}
function SQLInstallerErrorA(iError: WORD;
  var pfErrorCode: DWORD;
  lpszErrorMsg: LPSTR;
  cbErrorMsgMax: WORD;
  var pcbErrorMsg: WORD): SQLRETURN; stdcall;
{$EXTERNALSYM SQLInstallerErrorW}
function SQLInstallerErrorW(iError: WORD;
  var pfErrorCode: DWORD;
  lpszErrorMsg: LPWSTR;
  cbErrorMsgMax: WORD;
  var pcbErrorMsg: WORD): SQLRETURN; stdcall;

{$EXTERNALSYM SQLPostInstallerError}
function SQLPostInstallerError(dwErrorCode: DWORD;
  lpszErrMsg: LPCTSTR): SQLRETURN; stdcall;
{$EXTERNALSYM SQLPostInstallerErrorA}
function SQLPostInstallerErrorA(dwErrorCode: DWORD;
  lpszErrMsg: LPCSTR): SQLRETURN; stdcall;
{$EXTERNALSYM SQLPostInstallerErrorW}
function SQLPostInstallerErrorW(dwErrorCode: DWORD;
  lpszErrMsg: LPCWSTR): SQLRETURN; stdcall;

{$EXTERNALSYM SQLWriteFileDSN}
function SQLWriteFileDSN(lpszFileName: LPCTSTR;
  lpszAppName: LPCTSTR;
  lpszKeyName: LPCTSTR;
  lpszString: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM SQLWriteFileDSNA}
function SQLWriteFileDSNA(lpszFileName: LPCSTR;
  lpszAppName: LPCSTR;
  lpszKeyName: LPCSTR;
  lpszString: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM SQLWriteFileDSNW}
function SQLWriteFileDSNW(lpszFileName: LPCWSTR;
  lpszAppName: LPCWSTR;
  lpszKeyName: LPCWSTR;
  lpszString: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM SQLReadFileDSN}
function SQLReadFileDSN(lpszFileName: LPCTSTR;
  lpszAppName: LPCTSTR;
  lpszKeyName: LPCTSTR;
  lpszString: LPTSTR;
  cbString: WORD;
  var pcbString: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLReadFileDSNA}
function SQLReadFileDSNA(lpszFileName: LPCSTR;
  lpszAppName: LPCSTR;
  lpszKeyName: LPCSTR;
  lpszString: LPSTR;
  cbString: WORD;
  var pcbString: WORD): BOOL; stdcall;
{$EXTERNALSYM SQLReadFileDSNW}
function SQLReadFileDSNW(lpszFileName: LPCWSTR;
  lpszAppName: LPCWSTR;
  lpszKeyName: LPCWSTR;
  lpszString: LPWSTR;
  cbString: WORD;
  var pcbString: WORD): BOOL; stdcall;

{$EXTERNALSYM SQLInstallDriverEx}
function SQLInstallDriverEx(lpszDriver: LPCTSTR;
  lpszPathIn: LPCTSTR;
  lpszPathOut: LPTSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallDriverExA}
function SQLInstallDriverExA(lpszDriver: LPCSTR;
  lpszPathIn: LPCSTR;
  lpszPathOut: LPSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallDriverExW}
function SQLInstallDriverExW(lpszDriver: LPCWSTR;
  lpszPathIn: LPCWSTR;
  lpszPathOut: LPWSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;

{$EXTERNALSYM SQLInstallTranslatorEx}
function SQLInstallTranslatorEx(lpszTranslator: LPCTSTR;
  lpszPathIn: LPCTSTR;
  lpszPathOut: LPTSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallTranslatorExA}
function SQLInstallTranslatorExA(lpszTranslator: LPCSTR;
  lpszPathIn: LPCSTR;
  lpszPathOut: LPSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;
{$EXTERNALSYM SQLInstallTranslatorExW}
function SQLInstallTranslatorExW(lpszTranslator: LPCWSTR;
  lpszPathIn: LPCWSTR;
  lpszPathOut: LPWSTR;
  cbPathOutMax: WORD;
  var pcbPathOut: WORD;
  fRequest: WORD;
  lpdwUsageCount: LPDWORD): BOOL; stdcall;

{$EXTERNALSYM SQLGetConfigMode}
function SQLGetConfigMode(var pwConfigMode: UWORD): BOOL; stdcall;

{$EXTERNALSYM SQLSetConfigMode}
function SQLSetConfigMode(wConfigMode: UWORD): BOOL; stdcall;

//	Driver specific Setup APIs called by installer

{$EXTERNALSYM ConfigDSN}
function ConfigDSN(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCTSTR;
  lpszAttributes: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM ConfigDSNA}
function ConfigDSNA(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCSTR;
  lpszAttributes: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM ConfigDSNW}
function ConfigDSNW(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCWSTR;
  lpszAttributes: LPCWSTR): BOOL; stdcall;

{$EXTERNALSYM ConfigTranslator}
function ConfigTranslator(hwndParent: HWND;
  var pvOption: DWORD): BOOL; stdcall;

{$EXTERNALSYM ConfigDriver}
function ConfigDriver(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCTSTR;
  lpszArgs: LPCTSTR;
  lpszMsg: LPTSTR;
  cbMsgMax: WORD;
  var pcbMsgOut: WORD): BOOL; stdcall;
{$EXTERNALSYM ConfigDriverA}
function ConfigDriverA(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCSTR;
  lpszArgs: LPCSTR;
  lpszMsg: LPSTR;
  cbMsgMax: WORD;
  var pcbMsgOut: WORD): BOOL; stdcall;
{$EXTERNALSYM ConfigDriverW}
function ConfigDriverW(hwndParent: HWND;
  fRequest: WORD;
  lpszDriver: LPCWSTR;
  lpszArgs: LPCWSTR;
  lpszMsg: LPWSTR;
  cbMsgMax: WORD;
  var pcbMsgOut: WORD): BOOL; stdcall;

//##########################################################################
// odbcinst.h interface section ends here
//##########################################################################

implementation

//##########################################################################
// sql.h implementation part starts here
//##########################################################################

const
{$ifdef MSWINDOWS}
  ODBC32DLL = 'odbc32.dll';
{$else}
  ODBC32DLL = 'libodbc.so';
{$endif}

// Macro: test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO
function SQL_SUCCEEDED(const rc: SQLRETURN): Boolean;
begin
  Result := (rc and (not 1)) = 0;
end;

function SQLAllocConnect;        external ODBC32DLL name 'SQLAllocConnect';
{$IFNDEF UNICODE}
function SQLAllocEnv;            external ODBC32DLL name 'SQLAllocEnvA';
{$ELSE}
function SQLAllocEnv;            external ODBC32DLL name 'SQLAllocEnvW';
{$ENDIF}
function SQLAllocEnvA;           external ODBC32DLL name 'SQLAllocEnvA';
function SQLAllocEnvW;           external ODBC32DLL name 'SQLAllocEnvW';
function SQLAllocHandle;         external ODBC32DLL name 'SQLAllocHandle';
function SQLAllocStmt;           external ODBC32DLL name 'SQLAllocStmt';
function SQLBindCol;             external ODBC32DLL name 'SQLBindCol';
function SQLBindParam;           external ODBC32DLL name 'SQLBindParam';
function SQLCancel;              external ODBC32DLL name 'SQLCancel';
function SQLCloseCursor;         external ODBC32DLL name 'SQLCloseCursor';
{$IFNDEF UNICODE}
function SQLColAttribute;        external ODBC32DLL name 'SQLColAttributeA';
{$ELSE}
function SQLColAttribute;        external ODBC32DLL name 'SQLColAttributeW';
{$ENDIF}
function SQLColAttributeA;       external ODBC32DLL name 'SQLColAttributeA';
function SQLColAttributeW;       external ODBC32DLL name 'SQLColAttributeW';
{$IFNDEF UNICODE}
function SQLColumns;             external ODBC32DLL name 'SQLColumnsA';
{$ELSE}
function SQLColumns;             external ODBC32DLL name 'SQLColumnsW';
{$ENDIF}
function SQLColumnsA;            external ODBC32DLL name 'SQLColumnsA';
function SQLColumnsW;            external ODBC32DLL name 'SQLColumnsW';
{$IFNDEF UNICODE}
function SQLConnect;             external ODBC32DLL name 'SQLConnectA';
{$ELSE}
function SQLConnect;             external ODBC32DLL name 'SQLConnectW';
{$ENDIF}
function SQLConnectA;            external ODBC32DLL name 'SQLConnectA';
function SQLConnectW;            external ODBC32DLL name 'SQLConnectW';
function SQLCopyDesc;            external ODBC32DLL name 'SQLCopyDesc';
{$IFNDEF UNICODE}
function SQLDataSources;         external ODBC32DLL name 'SQLDataSourcesA';
{$ELSE}
function SQLDataSources;         external ODBC32DLL name 'SQLDataSourcesW';
{$ENDIF}
function SQLDataSourcesA;        external ODBC32DLL name 'SQLDataSourcesA';
function SQLDataSourcesW;        external ODBC32DLL name 'SQLDataSourcesW';
{$IFNDEF UNICODE}
function SQLDescribeCol;         external ODBC32DLL name 'SQLDescribeColA';
{$ELSE}
function SQLDescribeCol;         external ODBC32DLL name 'SQLDescribeColW';
{$ENDIF}
function SQLDescribeColA;        external ODBC32DLL name 'SQLDescribeColA';
function SQLDescribeColW;        external ODBC32DLL name 'SQLDescribeColW';
function SQLDisconnect;          external ODBC32DLL name 'SQLDisconnect';
function SQLEndTran;             external ODBC32DLL name 'SQLEndTran';
{$IFNDEF UNICODE}
function SQLError;               external ODBC32DLL name 'SQLErrorA';
{$ELSE}
function SQLError;               external ODBC32DLL name 'SQLErrorW';
{$ENDIF}
function SQLErrorA;              external ODBC32DLL name 'SQLErrorA';
function SQLErrorW;              external ODBC32DLL name 'SQLErrorW';
{$IFNDEF UNICODE}
function SQLExecDirect;          external ODBC32DLL name 'SQLExecDirectA';
{$ELSE}
function SQLExecDirect;          external ODBC32DLL name 'SQLExecDirectW';
{$ENDIF}
function SQLExecDirectA;         external ODBC32DLL name 'SQLExecDirectA';
function SQLExecDirectW;         external ODBC32DLL name 'SQLExecDirectW';
function SQLExecute;             external ODBC32DLL name 'SQLExecute';
function SQLFetch;               external ODBC32DLL name 'SQLFetch';
function SQLFetchScroll;         external ODBC32DLL name 'SQLFetchScroll';
function SQLFreeConnect;         external ODBC32DLL name 'SQLFreeConnect';
{$IFNDEF UNICODE}
function SQLFreeEnv;             external ODBC32DLL name 'SQLFreeEnvA';
{$ELSE}
function SQLFreeEnv;             external ODBC32DLL name 'SQLFreeEnvW';
{$ENDIF}
function SQLFreeEnvA;            external ODBC32DLL name 'SQLFreeEnvA';
function SQLFreeEnvW;            external ODBC32DLL name 'SQLFreeEnvW';
function SQLFreeHandle;          external ODBC32DLL name 'SQLFreeHandle';
function SQLFreeStmt;            external ODBC32DLL name 'SQLFreeStmt';
{$IFNDEF UNICODE}
function SQLGetConnectAttr;      external ODBC32DLL name 'SQLGetConnectAttrA';
{$ELSE}
function SQLGetConnectAttr;      external ODBC32DLL name 'SQLGetConnectAttrW';
{$ENDIF}
function SQLGetConnectAttrA;     external ODBC32DLL name 'SQLGetConnectAttrA';
function SQLGetConnectAttrW;     external ODBC32DLL name 'SQLGetConnectAttrW';
{$IFNDEF UNICODE}
function SQLGetConnectOption;    external ODBC32DLL name 'SQLGetConnectOptionA';
{$ELSE}
function SQLGetConnectOption;    external ODBC32DLL name 'SQLGetConnectOptionW';
{$ENDIF}
function SQLGetConnectOptionA;   external ODBC32DLL name 'SQLGetConnectOptionA';
function SQLGetConnectOptionW;   external ODBC32DLL name 'SQLGetConnectOptionW';
{$IFNDEF UNICODE}
function SQLGetCursorName;       external ODBC32DLL name 'SQLGetCursorNameA';
{$ELSE}
function SQLGetCursorName;       external ODBC32DLL name 'SQLGetCursorNameW';
{$ENDIF}
function SQLGetCursorNameA;      external ODBC32DLL name 'SQLGetCursorNameA';
function SQLGetCursorNameW;      external ODBC32DLL name 'SQLGetCursorNameW';
function SQLGetData;             external ODBC32DLL name 'SQLGetData';
{$IFNDEF UNICODE}
function SQLGetDescField;        external ODBC32DLL name 'SQLGetDescFieldA';
{$ELSE}
function SQLGetDescField;        external ODBC32DLL name 'SQLGetDescFieldW';
{$ENDIF}
function SQLGetDescFieldA;       external ODBC32DLL name 'SQLGetDescFieldA';
function SQLGetDescFieldW;       external ODBC32DLL name 'SQLGetDescFieldW';
{$IFNDEF UNICODE}
function SQLGetDescRec;          external ODBC32DLL name 'SQLGetDescRecA';
{$ELSE}
function SQLGetDescRec;          external ODBC32DLL name 'SQLGetDescRecW';
{$ENDIF}
function SQLGetDescRecA;         external ODBC32DLL name 'SQLGetDescRecA';
function SQLGetDescRecW;         external ODBC32DLL name 'SQLGetDescRecW';
{$IFNDEF UNICODE}
function SQLGetDiagField;        external ODBC32DLL name 'SQLGetDiagFieldA';
{$ELSE}
function SQLGetDiagField;        external ODBC32DLL name 'SQLGetDiagFieldW';
{$ENDIF}
function SQLGetDiagFieldA;       external ODBC32DLL name 'SQLGetDiagFieldA';
function SQLGetDiagFieldW;       external ODBC32DLL name 'SQLGetDiagFieldW';
{$IFNDEF UNICODE}
function SQLGetDiagRec;          external ODBC32DLL name 'SQLGetDiagRecA';
{$ELSE}
function SQLGetDiagRec;          external ODBC32DLL name 'SQLGetDiagRecW';
{$ENDIF}
function SQLGetDiagRecA;         external ODBC32DLL name 'SQLGetDiagRecA';
function SQLGetDiagRecW;         external ODBC32DLL name 'SQLGetDiagRecW';
function SQLGetEnvAttr;          external ODBC32DLL name 'SQLGetEnvAttr';
function SQLGetFunctions;        external ODBC32DLL name 'SQLGetFunctions';
{$IFNDEF UNICODE}
function SQLGetInfo;             external ODBC32DLL name 'SQLGetInfoA';
{$ELSE}
function SQLGetInfo;             external ODBC32DLL name 'SQLGetInfoW';
{$ENDIF}
function SQLGetInfoA;            external ODBC32DLL name 'SQLGetInfoA';
function SQLGetInfoW;            external ODBC32DLL name 'SQLGetInfoW';
{$IFNDEF UNICODE}
function SQLGetStmtAttr;         external ODBC32DLL name 'SQLGetStmtAttrA';
{$ELSE}
function SQLGetStmtAttr;         external ODBC32DLL name 'SQLGetStmtAttrW';
{$ENDIF}
function SQLGetStmtAttrA;        external ODBC32DLL name 'SQLGetStmtAttrA';
function SQLGetStmtAttrW;        external ODBC32DLL name 'SQLGetStmtAttrW';
function SQLGetStmtOption;       external ODBC32DLL name 'SQLGetStmtOption';
function SQLGetTypeInfo;         external ODBC32DLL name 'SQLGetTypeInfo';
function SQLNumResultCols;       external ODBC32DLL name 'SQLNumResultCols';
function SQLParamData;           external ODBC32DLL name 'SQLParamData';
{$IFNDEF UNICODE}
function SQLPrepare;             external ODBC32DLL name 'SQLPrepareA';
{$ELSE}
function SQLPrepare;             external ODBC32DLL name 'SQLPrepareW';
{$ENDIF}
function SQLPrepareA;            external ODBC32DLL name 'SQLPrepareA';
function SQLPrepareW;            external ODBC32DLL name 'SQLPrepareW';
function SQLPutData;             external ODBC32DLL name 'SQLPutData';
function SQLRowCount;            external ODBC32DLL name 'SQLRowCount';
{$IFNDEF UNICODE}
function SQLSetConnectAttr;      external ODBC32DLL name 'SQLSetConnectAttrA';
{$ELSE}
function SQLSetConnectAttr;      external ODBC32DLL name 'SQLSetConnectAttrW';
{$ENDIF}
function SQLSetConnectAttrA;     external ODBC32DLL name 'SQLSetConnectAttrA';
function SQLSetConnectAttrW;     external ODBC32DLL name 'SQLSetConnectAttrW';
{$IFNDEF UNICODE}
function SQLSetConnectOption;    external ODBC32DLL name 'SQLSetConnectOptionA';
{$ELSE}
function SQLSetConnectOption;    external ODBC32DLL name 'SQLSetConnectOptionW';
{$ENDIF}
function SQLSetConnectOptionA;   external ODBC32DLL name 'SQLSetConnectOptionA';
function SQLSetConnectOptionW;   external ODBC32DLL name 'SQLSetConnectOptionW';
{$IFNDEF UNICODE}
function SQLSetCursorName;       external ODBC32DLL name 'SQLSetCursorNameA';
{$ELSE}
function SQLSetCursorName;       external ODBC32DLL name 'SQLSetCursorNameW';
{$ENDIF}
function SQLSetCursorNameA;      external ODBC32DLL name 'SQLSetCursorNameA';
function SQLSetCursorNameW;      external ODBC32DLL name 'SQLSetCursorNameW';
{$IFNDEF UNICODE}
function SQLSetDescField;        external ODBC32DLL name 'SQLSetDescFieldA';
{$ELSE}
function SQLSetDescField;        external ODBC32DLL name 'SQLSetDescFieldW';
{$ENDIF}
function SQLSetDescFieldA;       external ODBC32DLL name 'SQLSetDescFieldA';
function SQLSetDescFieldW;       external ODBC32DLL name 'SQLSetDescFieldW';
{$IFNDEF UNICODE}
function SQLSetDescRec;          external ODBC32DLL name 'SQLSetDescRecA';
{$ELSE}
function SQLSetDescRec;          external ODBC32DLL name 'SQLSetDescRecW';
{$ENDIF}
function SQLSetDescRecA;         external ODBC32DLL name 'SQLSetDescRecA';
function SQLSetDescRecW;         external ODBC32DLL name 'SQLSetDescRecW';
function SQLSetEnvAttr;          external ODBC32DLL name 'SQLSetEnvAttr';
function SQLSetParam;            external ODBC32DLL name 'SQLSetParam';
{$IFNDEF UNICODE}
function SQLSetStmtAttr;         external ODBC32DLL name 'SQLSetStmtAttrA';
{$ELSE}
function SQLSetStmtAttr;         external ODBC32DLL name 'SQLSetStmtAttrW';
{$ENDIF}
function SQLSetStmtAttrA;        external ODBC32DLL name 'SQLSetStmtAttrA';
function SQLSetStmtAttrW;        external ODBC32DLL name 'SQLSetStmtAttrW';
function SQLSetStmtOption;       external ODBC32DLL name 'SQLSetStmtOption';
{$IFNDEF UNICODE}
function SQLSpecialColumns;      external ODBC32DLL name 'SQLSpecialColumnsA';
{$ELSE}
function SQLSpecialColumns;      external ODBC32DLL name 'SQLSpecialColumnsW';
{$ENDIF}
function SQLSpecialColumnsA;     external ODBC32DLL name 'SQLSpecialColumnsA';
function SQLSpecialColumnsW;     external ODBC32DLL name 'SQLSpecialColumnsW';
{$IFNDEF UNICODE}
function SQLStatistics;          external ODBC32DLL name 'SQLStatisticsA';
{$ELSE}
function SQLStatistics;          external ODBC32DLL name 'SQLStatisticsW';
{$ENDIF}
function SQLStatisticsA;         external ODBC32DLL name 'SQLStatisticsA';
function SQLStatisticsW;         external ODBC32DLL name 'SQLStatisticsW';
{$IFNDEF UNICODE}
function SQLTables;              external ODBC32DLL name 'SQLTablesA';
{$ELSE}
function SQLTables;              external ODBC32DLL name 'SQLTablesW';
{$ENDIF}
function SQLTablesA;             external ODBC32DLL name 'SQLTablesA';
function SQLTablesW;             external ODBC32DLL name 'SQLTablesW';
function SQLTransact;            external ODBC32DLL name 'SQLTransact';

//##########################################################################
// sql.h implementation part ends here
//##########################################################################

//##########################################################################
// sqlext.h implementation part starts here
//##########################################################################

const
  ODBCTRAC  = 'odbctrac.dll';
  ODBCINST  = 'odbcinst.dll';

// MACROs

function SQL_LEN_DATA_AT_EXEC(length: Integer): Integer;
begin
  result := -(length) + SQL_LEN_DATA_AT_EXEC_OFFSET;
end;

function SQL_LEN_BINARY_ATTR(length: Integer): Integer;
begin
  result := -(length) + SQL_LEN_BINARY_ATTR_OFFSET;
end;

function SQL_FUNC_EXISTS(pfExists: PUWORD; uwAPI: UWORD): SQLINTEGER;
begin
  Inc(pfExists, uwAPI shr 4);
  if (pfExists^ and (1 shl (uwAPI and $000F))) <> 0 then
    result := SQL_TRUE
  else
    result := SQL_FALSE;
end;

function SQL_POSITION_TO(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
begin
  result := SQLSetPos(hstmt,irow,SQL_POSITION,SQL_LOCK_NO_CHANGE);
end;

function SQL_LOCK_RECORD(hstmt: SQLHSTMT; irow, fLock: SQLUSMALLINT): SQLRETURN;
begin
  result := SQLSetPos(hstmt,irow,SQL_POSITION,fLock);
end;

function SQL_REFRESH_RECORD(hstmt: SQLHSTMT; irow, fLock: SQLUSMALLINT): SQLRETURN;
begin
  result := SQLSetPos(hstmt,irow,SQL_REFRESH,fLock);
end;

function SQL_UPDATE_RECORD(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
begin
  result := SQLSetPos(hstmt,irow,SQL_UPDATE,SQL_LOCK_NO_CHANGE);
end;

function SQL_DELETE_RECORD(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
begin
  result := SQLSetPos(hstmt,irow,SQL_DELETE,SQL_LOCK_NO_CHANGE);
end;

function SQL_ADD_RECORD(hstmt: SQLHSTMT; irow: SQLUSMALLINT): SQLRETURN;
begin
  result := SQLSetPos(hstmt,irow,SQL_ADD,SQL_LOCK_NO_CHANGE);
end;

function SQLAllocHandleStd;    external ODBC32DLL name 'SQLAllocHandleStd';
function SQLBindParameter;     external ODBC32DLL name 'SQLBindParameter';
{$IFNDEF UNICODE}
function SQLBrowseConnect;     external ODBC32DLL name 'SQLBrowseConnectA';
{$ELSE}
function SQLBrowseConnect;     external ODBC32DLL name 'SQLBrowseConnectW';
{$ENDIF}
function SQLBrowseConnectA;    external ODBC32DLL name 'SQLBrowseConnectA';
function SQLBrowseConnectW;    external ODBC32DLL name 'SQLBrowseConnectW';
function SQLBulkOperations;    external ODBC32DLL name 'SQLBulkOperations';
{$IFNDEF UNICODE}
function SQLColAttributes;     external ODBC32DLL name 'SQLColAttributesA';
{$ELSE}
function SQLColAttributes;     external ODBC32DLL name 'SQLColAttributesW';
{$ENDIF}
function SQLColAttributesA;    external ODBC32DLL name 'SQLColAttributesA';
function SQLColAttributesW;    external ODBC32DLL name 'SQLColAttributesW';
{$IFNDEF UNICODE}
function SQLColumnPrivileges;  external ODBC32DLL name 'SQLColumnPrivilegesA';
{$ELSE}
function SQLColumnPrivileges;  external ODBC32DLL name 'SQLColumnPrivilegesW';
{$ENDIF}
function SQLColumnPrivilegesA; external ODBC32DLL name 'SQLColumnPrivilegesA';
function SQLColumnPrivilegesW; external ODBC32DLL name 'SQLColumnPrivilegesW';
function SQLDescribeParam;     external ODBC32DLL name 'SQLDescribeParam';
{$IFNDEF UNICODE}
function SQLDriverConnect;     external ODBC32DLL name 'SQLDriverConnectA';
{$ELSE}
function SQLDriverConnect;     external ODBC32DLL name 'SQLDriverConnectW';
{$ENDIF}
function SQLDriverConnectA;    external ODBC32DLL name 'SQLDriverConnectA';
function SQLDriverConnectW;    external ODBC32DLL name 'SQLDriverConnectW';
{$IFNDEF UNICODE}
function SQLDrivers;           external ODBC32DLL name 'SQLDriversA';
{$ELSE}
function SQLDrivers;           external ODBC32DLL name 'SQLDriversW';
{$ENDIF}
function SQLDriversA;          external ODBC32DLL name 'SQLDriversA';
function SQLDriversW;          external ODBC32DLL name 'SQLDriversW';
function SQLExtendedFetch;     external ODBC32DLL name 'SQLExtendedFetch';
{$IFNDEF UNICODE}
function SQLForeignKeys;       external ODBC32DLL name 'SQLForeignKeysA';
{$ELSE}
function SQLForeignKeys;       external ODBC32DLL name 'SQLForeignKeysW';
{$ENDIF}
function SQLForeignKeysA;      external ODBC32DLL name 'SQLForeignKeysA';
function SQLForeignKeysW;      external ODBC32DLL name 'SQLForeignKeysW';
function SQLMoreResults;       external ODBC32DLL name 'SQLMoreResults';
{$IFNDEF UNICODE}
function SQLNativeSql;         external ODBC32DLL name 'SQLNativeSqlA';
{$ELSE}
function SQLNativeSql;         external ODBC32DLL name 'SQLNativeSqlW';
{$ENDIF}
function SQLNativeSqlA;        external ODBC32DLL name 'SQLNativeSqlA';
function SQLNativeSqlW;        external ODBC32DLL name 'SQLNativeSqlW';
function SQLNumParams;         external ODBC32DLL name 'SQLNumParams';
function SQLParamOptions;      external ODBC32DLL name 'SQLParamOptions';
{$IFNDEF UNICODE}
function SQLPrimaryKeys;       external ODBC32DLL name 'SQLPrimaryKeysA';
{$ELSE}
function SQLPrimaryKeys;       external ODBC32DLL name 'SQLPrimaryKeysW';
{$ENDIF}
function SQLPrimaryKeysA;      external ODBC32DLL name 'SQLPrimaryKeysA';
function SQLPrimaryKeysW;      external ODBC32DLL name 'SQLPrimaryKeysW';
{$IFNDEF UNICODE}
function SQLProcedureColumns;  external ODBC32DLL name 'SQLProcedureColumnsA';
{$ELSE}
function SQLProcedureColumns;  external ODBC32DLL name 'SQLProcedureColumnsW';
{$ENDIF}
function SQLProcedureColumnsA; external ODBC32DLL name 'SQLProcedureColumnsA';
function SQLProcedureColumnsW; external ODBC32DLL name 'SQLProcedureColumnsW';
{$IFNDEF UNICODE}
function SQLProcedures;        external ODBC32DLL name 'SQLProceduresA';
{$ELSE}
function SQLProcedures;        external ODBC32DLL name 'SQLProceduresW';
{$ENDIF}
function SQLProceduresA;       external ODBC32DLL name 'SQLProceduresA';
function SQLProceduresW;       external ODBC32DLL name 'SQLProceduresW';
function SQLSetPos;            external ODBC32DLL name 'SQLSetPos';
function SQLSetScrollOptions;  external ODBC32DLL name 'SQLSetScrollOptions';
{$IFNDEF UNICODE}
function SQLTablePrivileges;   external ODBC32DLL name 'SQLTablePrivilegesA';
{$ELSE}
function SQLTablePrivileges;   external ODBC32DLL name 'SQLTablePrivilegesW';
{$ENDIF}
function SQLTablePrivilegesA;  external ODBC32DLL name 'SQLTablePrivilegesA';
function SQLTablePrivilegesW;  external ODBC32DLL name 'SQLTablePrivilegesW';

function TraceOpenLogFile;     external ODBCTRAC name 'TraceOpenLogFile';
function TraceCloseLogFile;    external ODBCTRAC name 'TraceCloseLogFile';
procedure TraceReturn;         external ODBCTRAC name 'TraceReturn';
function TraceVersion;         external ODBCTRAC name 'TraceVersion';
function TraceVSControl;       external ODBCTRAC name 'TraceVSControl';
procedure FireVSDebugEvent;    external ODBCTRAC name 'FireVSDebugEvent';

function ODBCGetTryWaitValue;  external ODBCINST name 'ODBCGetTryWaitValue';
function ODBCSetTryWaitValue;  external ODBCINST name 'ODBCSetTryWaitValue';


//##########################################################################
// sqlext.h implementation part ends here
//##########################################################################

//##########################################################################
// odbcinst.h implementation part starts here
//##########################################################################

const
  ODBCCP32DLL = 'odbccp32.dll';

{$IFNDEF UNICODE}
function SQLInstallODBC;                external ODBCCP32DLL name 'SQLInstallODBCA';
{$ELSE}
function SQLInstallODBC;                external ODBCCP32DLL name 'SQLInstallODBCW';
{$ENDIF}
function SQLInstallODBCA;               external ODBCCP32DLL name 'SQLInstallODBCA';
function SQLInstallODBCW;               external ODBCCP32DLL name 'SQLInstallODBCW';
function SQLManageDataSources;          external ODBCCP32DLL name 'SQLManageDataSources';
{$IFNDEF UNICODE}
function SQLCreateDataSource;           external ODBCCP32DLL name 'SQLCreateDataSourceA';
{$ELSE}
function SQLCreateDataSource;           external ODBCCP32DLL name 'SQLCreateDataSourceW';
{$ENDIF}
function SQLCreateDataSourceA;          external ODBCCP32DLL name 'SQLCreateDataSourceA';
function SQLCreateDataSourceW;          external ODBCCP32DLL name 'SQLCreateDataSourceW';
{$IFNDEF UNICODE}
function SQLGetTranslator;              external ODBCCP32DLL name 'SQLGetTranslatorA';
{$ELSE}
function SQLGetTranslator;              external ODBCCP32DLL name 'SQLGetTranslatorW';
{$ENDIF}
function SQLGetTranslatorA;             external ODBCCP32DLL name 'SQLGetTranslatorA';
function SQLGetTranslatorW;             external ODBCCP32DLL name 'SQLGetTranslatorW';
{$IFNDEF UNICODE}
function SQLInstallDriver;              external ODBCCP32DLL name 'SQLInstallDriverA';
{$ELSE}
function SQLInstallDriver;              external ODBCCP32DLL name 'SQLInstallDriverW';
{$ENDIF}
function SQLInstallDriverA;             external ODBCCP32DLL name 'SQLInstallDriverA';
function SQLInstallDriverW;             external ODBCCP32DLL name 'SQLInstallDriverW';
{$IFNDEF UNICODE}
function SQLInstallDriverManager;       external ODBCCP32DLL name 'SQLInstallDriverManagerA';
{$ELSE}
function SQLInstallDriverManager;       external ODBCCP32DLL name 'SQLInstallDriverManagerW';
{$ENDIF}
function SQLInstallDriverManagerA;      external ODBCCP32DLL name 'SQLInstallDriverManagerA';
function SQLInstallDriverManagerW;      external ODBCCP32DLL name 'SQLInstallDriverManagerW';
{$IFNDEF UNICODE}
function SQLGetInstalledDrivers;        external ODBCCP32DLL name 'SQLGetInstalledDriversA';
{$ELSE}
function SQLGetInstalledDrivers;        external ODBCCP32DLL name 'SQLGetInstalledDriversW';
{$ENDIF}
function SQLGetInstalledDriversA;       external ODBCCP32DLL name 'SQLGetInstalledDriversA';
function SQLGetInstalledDriversW;       external ODBCCP32DLL name 'SQLGetInstalledDriversW';
{$IFNDEF UNICODE}
function SQLGetAvailableDrivers;        external ODBCCP32DLL name 'SQLGetAvailableDriversA';
{$ELSE}
function SQLGetAvailableDrivers;        external ODBCCP32DLL name 'SQLGetAvailableDriversW';
{$ENDIF}
function SQLGetAvailableDriversA;       external ODBCCP32DLL name 'SQLGetAvailableDriversA';
function SQLGetAvailableDriversW;       external ODBCCP32DLL name 'SQLGetAvailableDriversW';
{$IFNDEF UNICODE}
function SQLConfigDataSource;           external ODBCCP32DLL name 'SQLConfigDataSourceA';
{$ELSE}
function SQLConfigDataSource;           external ODBCCP32DLL name 'SQLConfigDataSourceW';
{$ENDIF}
function SQLConfigDataSourceA;          external ODBCCP32DLL name 'SQLConfigDataSourceA';
function SQLConfigDataSourceW;          external ODBCCP32DLL name 'SQLConfigDataSourceW';
function SQLRemoveDefaultDataSource;    external ODBCCP32DLL name 'SQLRemoveDefaultDataSource';
{$IFNDEF UNICODE}
function SQLWriteDSNToIni;              external ODBCCP32DLL name 'SQLWriteDSNToIniA';
{$ELSE}
function SQLWriteDSNToIni;              external ODBCCP32DLL name 'SQLWriteDSNToIniW';
{$ENDIF}
function SQLWriteDSNToIniA;             external ODBCCP32DLL name 'SQLWriteDSNToIniA';
function SQLWriteDSNToIniW;             external ODBCCP32DLL name 'SQLWriteDSNToIniW';
{$IFNDEF UNICODE}
function SQLRemoveDSNFromIni;           external ODBCCP32DLL name 'SQLRemoveDSNFromIniA';
{$ELSE}
function SQLRemoveDSNFromIni;           external ODBCCP32DLL name 'SQLRemoveDSNFromIniW';
{$ENDIF}
function SQLRemoveDSNFromIniA;          external ODBCCP32DLL name 'SQLRemoveDSNFromIniA';
function SQLRemoveDSNFromIniW;          external ODBCCP32DLL name 'SQLRemoveDSNFromIniW';
{$IFNDEF UNICODE}
function SQLValidDSN;                   external ODBCCP32DLL name 'SQLValidDSNA';
{$ELSE}
function SQLValidDSN;                   external ODBCCP32DLL name 'SQLValidDSNW';
{$ENDIF}
function SQLValidDSNA;                  external ODBCCP32DLL name 'SQLValidDSNA';
function SQLValidDSNW;                  external ODBCCP32DLL name 'SQLValidDSNW';
{$IFNDEF UNICODE}
function SQLWritePrivateProfileString;  external ODBCCP32DLL name 'SQLWritePrivateProfileStringA';
{$ELSE}
function SQLWritePrivateProfileString;  external ODBCCP32DLL name 'SQLWritePrivateProfileStringW';
{$ENDIF}
function SQLWritePrivateProfileStringA; external ODBCCP32DLL name 'SQLWritePrivateProfileStringA';
function SQLWritePrivateProfileStringW; external ODBCCP32DLL name 'SQLWritePrivateProfileStringW';
{$IFNDEF UNICODE}
function SQLGetPrivateProfileString;    external ODBCCP32DLL name 'SQLGetPrivateProfileStringA';
{$ELSE}
function SQLGetPrivateProfileString;    external ODBCCP32DLL name 'SQLGetPrivateProfileStringW';
{$ENDIF}
function SQLGetPrivateProfileStringA;   external ODBCCP32DLL name 'SQLGetPrivateProfileStringA';
function SQLGetPrivateProfileStringW;   external ODBCCP32DLL name 'SQLGetPrivateProfileStringW';
function SQLRemoveDriverManager;        external ODBCCP32DLL name 'SQLRemoveDriverManager';
{$IFNDEF UNICODE}
function SQLInstallTranslator;          external ODBCCP32DLL name 'SQLInstallTranslatorA';
{$ELSE}
function SQLInstallTranslator;          external ODBCCP32DLL name 'SQLInstallTranslatorW';
{$ENDIF}
function SQLInstallTranslatorA;         external ODBCCP32DLL name 'SQLInstallTranslatorA';
function SQLInstallTranslatorW;         external ODBCCP32DLL name 'SQLInstallTranslatorW';
{$IFNDEF UNICODE}
function SQLRemoveTranslator;           external ODBCCP32DLL name 'SQLRemoveTranslatorA';
{$ELSE}
function SQLRemoveTranslator;           external ODBCCP32DLL name 'SQLRemoveTranslatorW';
{$ENDIF}
function SQLRemoveTranslatorA;          external ODBCCP32DLL name 'SQLRemoveTranslatorA';
function SQLRemoveTranslatorW;          external ODBCCP32DLL name 'SQLRemoveTranslatorW';
{$IFNDEF UNICODE}
function SQLRemoveDriver;               external ODBCCP32DLL name 'SQLRemoveDriverA';
{$ELSE}
function SQLRemoveDriver;               external ODBCCP32DLL name 'SQLRemoveDriverW';
{$ENDIF}
function SQLRemoveDriverA;              external ODBCCP32DLL name 'SQLRemoveDriverA';
function SQLRemoveDriverW;              external ODBCCP32DLL name 'SQLRemoveDriverW';
{$IFNDEF UNICODE}
function SQLConfigDriver;               external ODBCCP32DLL name 'SQLConfigDriverA';
{$ELSE}
function SQLConfigDriver;               external ODBCCP32DLL name 'SQLConfigDriverW';
{$ENDIF}
function SQLConfigDriverA;              external ODBCCP32DLL name 'SQLConfigDriverA';
function SQLConfigDriverW;              external ODBCCP32DLL name 'SQLConfigDriverW';
{$IFNDEF UNICODE}
function SQLInstallerError;             external ODBCCP32DLL name 'SQLInstallerErrorA';
{$ELSE}
function SQLInstallerError;             external ODBCCP32DLL name 'SQLInstallerErrorW';
{$ENDIF}
function SQLInstallerErrorA;            external ODBCCP32DLL name 'SQLInstallerErrorA';
function SQLInstallerErrorW;            external ODBCCP32DLL name 'SQLInstallerErrorW';
{$IFNDEF UNICODE}
function SQLPostInstallerError;         external ODBCCP32DLL name 'SQLPostInstallerErrorA';
{$ELSE}
function SQLPostInstallerError;         external ODBCCP32DLL name 'SQLPostInstallerErrorW';
{$ENDIF}
function SQLPostInstallerErrorA;        external ODBCCP32DLL name 'SQLPostInstallerErrorA';
function SQLPostInstallerErrorW;        external ODBCCP32DLL name 'SQLPostInstallerErrorW';
{$IFNDEF UNICODE}
function SQLWriteFileDSN;               external ODBCCP32DLL name 'SQLWriteFileDSNA';
{$ELSE}
function SQLWriteFileDSN;               external ODBCCP32DLL name 'SQLWriteFileDSNW';
{$ENDIF}
function SQLWriteFileDSNA;              external ODBCCP32DLL name 'SQLWriteFileDSNA';
function SQLWriteFileDSNW;              external ODBCCP32DLL name 'SQLWriteFileDSNW';
{$IFNDEF UNICODE}
function SQLReadFileDSN;                external ODBCCP32DLL name 'SQLReadFileDSNA';
{$ELSE}
function SQLReadFileDSN;                external ODBCCP32DLL name 'SQLReadFileDSNW';
{$ENDIF}
function SQLReadFileDSNA;               external ODBCCP32DLL name 'SQLReadFileDSNA';
function SQLReadFileDSNW;               external ODBCCP32DLL name 'SQLReadFileDSNW';
{$IFNDEF UNICODE}
function SQLInstallDriverEx;            external ODBCCP32DLL name 'SQLInstallDriverExA';
{$ELSE}
function SQLInstallDriverEx;            external ODBCCP32DLL name 'SQLInstallDriverExW';
{$ENDIF}
function SQLInstallDriverExA;           external ODBCCP32DLL name 'SQLInstallDriverExA';
function SQLInstallDriverExW;           external ODBCCP32DLL name 'SQLInstallDriverExW';
{$IFNDEF UNICODE}
function SQLInstallTranslatorEx;        external ODBCCP32DLL name 'SQLInstallTranslatorExA';
{$ELSE}
function SQLInstallTranslatorEx;        external ODBCCP32DLL name 'SQLInstallTranslatorExW';
{$ENDIF}
function SQLInstallTranslatorExA;       external ODBCCP32DLL name 'SQLInstallTranslatorExA';
function SQLInstallTranslatorExW;       external ODBCCP32DLL name 'SQLInstallTranslatorExW';
function SQLGetConfigMode;              external ODBCCP32DLL name 'SQLGetConfigMode';
function SQLSetConfigMode;              external ODBCCP32DLL name 'SQLSetConfigMode';
{$IFNDEF UNICODE}
function ConfigDSN;                     external ODBCCP32DLL name 'ConfigDSNA';
{$ELSE}
function ConfigDSN;                     external ODBCCP32DLL name 'ConfigDSNW';
{$ENDIF}
function ConfigDSNA;                    external ODBCCP32DLL name 'ConfigDSNA';
function ConfigDSNW;                    external ODBCCP32DLL name 'ConfigDSNW';
function ConfigTranslator;              external ODBCCP32DLL name 'ConfigTranslator';
{$IFNDEF UNICODE}
function ConfigDriver;                  external ODBCCP32DLL name 'ConfigDriverA';
{$ELSE}
function ConfigDriver;                  external ODBCCP32DLL name 'ConfigDriverW';
{$ENDIF}
function ConfigDriverA;                 external ODBCCP32DLL name 'ConfigDriverA';
function ConfigDriverW;                 external ODBCCP32DLL name 'ConfigDriverW';

//##########################################################################
// odbcinst.h implementation part ends here
//##########################################################################

end.
