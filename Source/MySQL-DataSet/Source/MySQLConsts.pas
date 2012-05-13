unit MySQLConsts;

interface {********************************************************************}

const
  LOCAL_HOST            = 'localhost';
  MYSQL_PORT            = 3306;
  LOCAL_HOST_NAMEDPIPE  = '.';
  MYSQL_NAMEDPIPE       = 'MySQL';
  PROTOCOL_VERSION      = 10;
  NET_READ_TIMEOUT      = 30;
  NET_WRITE_TIMEOUT     = 60;
  NET_WAIT_TIMEOUT      = 8*60*60;
  NULL_LENGTH           = -1;
  NAME_LEN              = 64;
  HOSTNAME_LENGTH       = 60;
  USERNAME_LENGTH       = 16;
  MYSQL_ERRMSG_SIZE     = 512;
  SQLSTATE_LENGTH       = 5;
  SERVER_VERSION_LENGTH = 60;
  SCRAMBLE_LENGTH       = 20;
  SCRAMBLE_LENGTH_323   = 8;
  MAX_PACKET_LENGTH     = $FFFFFE;
  MAX_ALLOWED_PACKET    = $1000000;
  NET_BUFFER_LENGTH     = $2000;
  MIN_COMPRESS_LENGTH   = 50;
  NET_HEADER_SIZE       = 4;
  COMP_HEADER_SIZE      = 3;
  RETRY_COUNT           = 2;
  PACKET_ERROR          = -1;
  IO_SIZE               = $1000;

  // Server Status Flags
  SERVER_STATUS_IN_TRANS             = $0001;   // Transaction has started
  SERVER_STATUS_AUTOCOMMIT           = $0002;   // Server in auto_commit mode
  SERVER_STATUS_MORE_RESULTS         = $0004;   // More results on server
  SERVER_MORE_RESULTS_EXISTS         = $0008;   // Multi query - next query exists
  SERVER_QUERY_NO_GOOD_INDEX_USED    = $0010;
  SERVER_QUERY_NO_INDEX_USED         = $0020;   // The server was able to fulfill the clients request and opened a read-only non-scrollable cursor for a query. This flag comes in reply to COM_STMT_EXECUTE and COM_STMT_FETCH commands.
  SERVER_STATUS_CURSOR_EXISTS        = $0040;   // This flag is sent when a read-only cursor is exhausted, in reply to COM_STMT_FETCH command.
  SERVER_STATUS_LAST_ROW_SENT        = $0080;
  SERVER_STATUS_DB_DROPPED           = $0100;   // A database was dropped
  SERVER_STATUS_NO_BACKSLASH_ESCAPES = $0200;

  //client flags
  CLIENT_LONG_PASSWORD     = $00001;    // new more secure passwords
  CLIENT_FOUND_ROWS        = $00002;    // Found instead of affected rows
  CLIENT_LONG_FLAG         = $00004;    // Get all column flags
  CLIENT_CONNECT_WITH_DB   = $00008;    // One can specify db on connect
  CLIENT_NO_SCHEMA         = $00010;    // Don't allow database.table.column
  CLIENT_COMPRESS          = $00020;    // Can use compression protocol
  CLIENT_ODBC              = $00040;    // Odbc client
  CLIENT_LOCAL_FILES       = $00080;    // Can use LOAD DATA LOCAL
  CLIENT_IGNORE_SPACE      = $00100;    // Ignore spaces before '('
  CLIENT_PROTOCOL_41       = $00200;    // New 4.1 protocol
  CLIENT_INTERACTIVE       = $00400;    // This is an interactive client
  CLIENT_SSL               = $00800;    // Switch to SSL after handshake
  CLIENT_IGNORE_SIGPIPE    = $01000;    // IGNORE sigpipes
  CLIENT_TRANSACTIONS      = $02000;    // Client knows about transactions
  CLIENT_RESERVED          = $04000;    // Old flag for 4.1 protocol
  CLIENT_SECURE_CONNECTION = $08000;    // New 4.1 authentication
  CLIENT_MULTI_STATEMENTS  = $10000;    // Enable/disable multi-stmt support
  CLIENT_MULTI_RESULTS     = $20000;    // Enable/disable multi-results
  CLIENT_REMEMBER_OPTIONS  = $80000000;

  //field flags
  NOT_NULL_FLAG            = $000001;   // Field can't be NULL
  PRI_KEY_FLAG             = $000002;   // Field is part of a primary key
  UNIQUE_KEY_FLAG          = $000004;   // Field is part of a unique key
  MULTIPLE_KEY_FLAG        = $000008;   // Field is part of a key
  BLOB_FLAG                = $000010;   // Field is a blob
  UNSIGNED_FLAG            = $000020;   // Field is unsigned
  ZEROFILL_FLAG            = $000040;   // Field is zerofill
  BINARY_FLAG              = $000080;   // Field is binary
  ENUM_FLAG                = $000100;   // Field is an enum
  AUTO_INCREMENT_FLAG      = $000200;   // Field is a autoincrement field
  TIMESTAMP_FLAG           = $000400;   // Field is a timestamp
  SET_FLAG                 = $000800;   // field is a set
  NO_DEFAULT_VALUE_FLAG    = $001000;   // Field doesn't have default value
  NUM_FLAG                 = $002000;   // Field is num (for clients)
  PART_KEY_FLAG            = $004000;   // Intern; Part of some key
  GROUP_FLAG               = $008000;   // Intern: Group field
  UNIQUE_FLAG              = $010000;   // Intern: Used by sql_yacc
  BINCMP_FLAG              = $020000;   // Intern: Used by sql_yacc
  GET_FIXED_FIELDS_FLAG    = $040000;   // Used to get fields in item tree
  FIELD_IN_PART_FUNC_FLAG  = $080000;   // Field part of partition func
  FIELD_IN_ADD_INDEX       = $100000;   // Intern: Field used in ADD INDEX
  FIELD_IS_RENAMED         = $200000;   // Intern: Field is being renamed

  MAX_TINYINT_WIDTH        = 3;         // Max width for a TINY w.o. sign
  MAX_SMALLINT_WIDTH       = 5;         // Max width for a SHORT w.o. sign
  MAX_MEDIUMINT_WIDTH      = 8;         // Max width for a INT24 w.o. sign
  MAX_INT_WIDTH            = 10;        // Max width for a LONG w.o. sign
  MAX_BIGINT_WIDTH         = 20;        // Max width for a LONGLONG
  MAX_FLOAT_WIDTH          = 53;        // Max width for a FLOAT
  MAX_CHAR_WIDTH           = 255;       // Max length for a CHAR colum
  MAX_BLOB_WIDTH           = 8192;      // Default width for blob

type
  my_ulonglong = Int64;
  my_uint = Longword;
  my_int = Longint;
  my_bool = Byte;
  my_char = PAnsiChar;

  mysql_protocol_type = (
    MYSQL_PROTOCOL_DEFAULT = 0,
    MYSQL_PROTOCOL_TCP     = 1,
    MYSQL_PROTOCOL_SOCKET  = 2,
    MYSQL_PROTOCOL_PIPE    = 3,
    MYSQL_PROTOCOL_MEMORY  = 4
  );

  enum_server_command = (
    COM_SLEEP               = $00,  // (default, e.g. SHOW PROCESSLIST)
    COM_QUIT                = $01,  // mysql_close
    COM_INIT_DB             = $02,  // mysql_select_db
    COM_QUERY               = $03,  // mysql_real_query
    COM_FIELD_LIST          = $04,  // mysql_list_fields
    COM_CREATE_DB           = $05,  // mysql_create_db
    COM_DROP_DB             = $06,  // mysql_drop_db
    COM_REFRESH             = $07,  // mysql_refresh
    COM_SHUTDOWN            = $08,  // mysql_shutdown
    COM_STATISTICS          = $09,  // mysql_stat
    COM_PROCESS_INFO        = $0A,  // mysql_list_processes
    COM_CONNECT             = $0B,  // (during authentication handshake)
    COM_PROCESS_KILL        = $0C,  // mysql_kill
    COM_DEBUG               = $0D,  //
    COM_PING                = $0E,  // mysql_ping
    COM_TIME                = $0F,  // (special value for slow logs?)
    COM_DELAYED_INSERT      = $10,  //
    COM_CHANGE_USER         = $11,  // mysql_change_user
    COM_BINLOG_DUMP         = $12,  // (used by slave server / mysqlbinlog)
    COM_TABLE_DUMP          = $13,  // (used by slave server to get master table)
    COM_CONNECT_OUT         = $14,  // (used by slave to log connection to master)
    COM_REGISTER_SLAVE      = $15,  // (reports slave location to master)
    COM_STMT_PREPARE        = $16,  // see description of Prepare Packet
    COM_STMT_EXECUTE        = $17,  // see description of Execute Packet
    COM_STMT_SEND_LONG_DATA = $18,  // see description of Long Data Packet
    COM_STMT_CLOSE          = $19,  // new, for closing statement
    COM_STMT_RESET          = $1A,  //
    COM_SET_OPTION          = $1B,  // mysql_set_option
    COM_STMT_FETCH          = $1C   //
//    COM_DAEMON
//    COM_ERROR
  );

  enum_field_types = (
    MYSQL_TYPE_DECIMAL      = 0,
    MYSQL_TYPE_TINY         = 1,
    MYSQL_TYPE_SHORT        = 2,
    MYSQL_TYPE_LONG         = 3,
    MYSQL_TYPE_FLOAT        = 4,
    MYSQL_TYPE_DOUBLE       = 5,
    MYSQL_TYPE_NULL         = 6,
    MYSQL_TYPE_TIMESTAMP    = 7,
    MYSQL_TYPE_LONGLONG     = 8,
    MYSQL_TYPE_INT24        = 9,
    MYSQL_TYPE_DATE         = 10,
    MYSQL_TYPE_TIME         = 11,
    MYSQL_TYPE_DATETIME     = 12,
    MYSQL_TYPE_YEAR         = 13,
    MYSQL_TYPE_NEWDATE      = 14,
    MYSQL_TYPE_BIT          = 16,
    MYSQL_TYPE_NEWDECIMAL   = 246,
    MYSQL_TYPE_ENUM         = 247,
    MYSQL_TYPE_SET          = 248,
    MYSQL_TYPE_TINY_BLOB    = 249,
    MYSQL_TYPE_MEDIUM_BLOB  = 250,
    MYSQL_TYPE_LONG_BLOB    = 251,
    MYSQL_TYPE_BLOB         = 252,
    MYSQL_TYPE_VAR_STRING   = 253,
    MYSQL_TYPE_STRING       = 254,
    MYSQL_TYPE_GEOMETRY     = 255
  );

  enum_mysql_option = (
    MYSQL_OPT_CONNECT_TIMEOUT         = 0,
    MYSQL_OPT_COMPRESS                = 1,
    MYSQL_OPT_NAMED_PIPE              = 2,
    MYSQL_INIT_COMMAND                = 3,
    MYSQL_READ_DEFAULT_FILE           = 4,
    MYSQL_READ_DEFAULT_GROUP          = 5,
    MYSQL_SET_CHARSET_DIR             = 6,
    MYSQL_SET_CHARSET_NAME            = 7,
    MYSQL_OPT_LOCAL_INFILE            = 8,
    MYSQL_OPT_PROTOCOL                = 9,
    MYSQL_SHARED_MEMORY_BASE_NAME     = 10,
    MYSQL_OPT_READ_TIMEOUT            = 11,
    MYSQL_OPT_WRITE_TIMEOUT           = 12,
    MYSQL_OPT_USE_RESULT              = 13,
    MYSQL_OPT_USE_REMOTE_CONNECTION   = 14,
    MYSQL_OPT_USE_EMBEDDED_CONNECTION = 15,
    MYSQL_OPT_GUESS_CONNECTION        = 16,
    MYSQL_SET_CLIENT_IP               = 17,
    MYSQL_SECURE_AUTH                 = 18,
    MYSQL_REPORT_DATA_TRUNCATION      = 19,
    MYSQL_OPT_RECONNECT               = 20
  );

  enum_mysql_set_option = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON  = 0,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF = 1
  );

  mysql_enum_shutdown_level = (
    SHUTDOWN_DEFAULT = 0
  );

  MYSQL_FIELD_32300 = ^TMYSQL_FIELD_32300;  // MySQL 3.23.0 and higher
  TMYSQL_FIELD_32300 = record
    name: my_char;                // Name of column
    table: my_char;               // Table of column if column was a field
    def: my_char;                 // Default value (set by mysql_list_fields)
    field_type: enum_field_types; // Type of field. Se mysql_com.h for types
    length: my_uint;              // Width of column
    max_length: my_uint;          // Max width of selected set
    flags: my_uint;               // Div flags
    decimals: my_uint;            // Number of decimals in field
  end;

  MYSQL_FIELD_40000 = ^TMYSQL_FIELD_40000;  // MySQL 4.0.0 and higher
  TMYSQL_FIELD_40000 = packed record
    name: my_char;                // Name of column
    table: my_char;               // Table of column if column was a field
    org_table: my_char;           // Org table name if table was an alias
    db: my_char;                  // Database for table
    def: my_char;                 // Default value (set by mysql_list_fields)
    length: my_uint;              // Width of column
    max_length: my_uint;          // Max width of selected set
    flags: my_uint;               // Div flags
    decimals: my_uint;            // Number of decimals in field
    field_type: enum_field_types; // Type of field. Se mysql_com.h for types
  end;

  MYSQL_FIELD_40100 = ^TMYSQL_FIELD_40100;  // MySQL 4.1.0 and higher
  TMYSQL_FIELD_40100 = packed record
    name: my_char;                // Name of column
    org_name: my_char;            // Original column name, if an alias
    table: my_char;               // Table of column if column was a field
    org_table: my_char;           // Org table name if table was an alias
    db: my_char;                  // Database for table
    def: my_char;                 // Default value (set by mysql_list_fields)
    length: my_uint;              // Width of column
    max_length: my_uint;          // Max width of selected set
    name_length: my_uint;
    org_name_length: my_uint;
    table_length: my_uint;
    org_table_length: my_uint;
    db_length: my_uint;
    def_length: my_uint;
    flags: my_uint;               // Div flags
    decimals: my_uint;            // Number of decimals in field
    charsetnr: my_uint;           // Character set
    field_type: enum_field_types; // Type of field. Se mysql_com.h for types
  end;

  MYSQL_FIELD_40101 = ^TMYSQL_FIELD_40101;  // MySQL 4.1.1 and higher
  TMYSQL_FIELD_40101 = packed record
    name: my_char;                // Name of column
    org_name: my_char;            // Original column name, if an alias
    table: my_char;               // Table of column if column was a field
    org_table: my_char;           // Org table name if table was an  alias
    db: my_char;                  // Database for table
    catalog: my_char;             // Catalog for table
    def: my_char;                 // Default value (set by mysql_list_fields)
    length: my_uint;              // Width of column
    max_length: my_uint;          // Max width of selected set
    name_length: my_uint;
    org_name_length: my_uint;
    table_length: my_uint;
    org_table_length: my_uint;
    db_length: my_uint;
    catalog_length: my_uint;
    def_length: my_uint;
    flags: my_uint;               // Div flags
    decimals: my_uint;            // Number of decimals in field
    charsetnr: my_uint;           // Character set
    field_type: enum_field_types; // Type of field. Se mysql_com.h for types
  end;

  MYSQL_FIELD = MYSQL_FIELD_40101;
  TMYSQL_FIELD = TMYSQL_FIELD_40101;

  MYSQL_FIELDS = ^TMYSQL_FIELDS;
  TMYSQL_FIELDS = array[0 .. MaxInt div SizeOf(MYSQL_FIELD) - 1] of MYSQL_FIELD;

  MYSQL_FIELD_OFFSET = my_uint;

  MYSQL_ROW = ^TMYSQL_ROW;
  TMYSQL_ROW = array[0 .. MaxInt div SizeOf(MYSQL_ROW) - 1] of my_char;

  MYSQL_ROWS = Pointer;
  MYSQL_ROW_OFFSET = MYSQL_ROWS;

  MYSQL_LENGTHS = ^TMYSQL_LENGTHS;
  TMYSQL_LENGTHS = array[0 .. MaxInt div SizeOf(my_int) - 1] of my_int;

  MYSQL = Pointer;
  MYSQL_RES = Pointer;

type                                                                               
  Tlocal_infile_init = function(ptr: PPointer; filename: my_char; const userdata): my_int; cdecl;
  Tlocal_infile_read = function(ptr: Pointer; buf: my_char; buf_len: my_uint): my_int; cdecl;
  Tlocal_infile_end = procedure(ptr: Pointer); cdecl;
  Tlocal_infile_error = function(ptr: Pointer; error_msg: my_char; error_msg_len: my_uint): my_int; cdecl;

  Tmy_init = procedure(); stdcall;
  Tmysql_affected_rows = function(mysql: MYSQL): my_ulonglong; stdcall;
  Tmysql_change_user = function(mysql: MYSQL; const user, passwd, db: my_char): my_bool; stdcall;
  Tmysql_character_set_name = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_close = procedure(mysql: MYSQL); stdcall;
  Tmysql_connect = function(mysql: MYSQL; const host, user, passwd: my_char): MYSQL; stdcall;
  Tmysql_create_db = function(mysql: MYSQL; const DB: my_char): my_int; stdcall;
  Tmysql_data_seek = procedure(res: MYSQL_RES; offset: my_ulonglong); stdcall;
  Tmysql_debug = procedure(const debug: my_char); stdcall;
  Tmysql_drop_db = function(mysql: MYSQL; const DB: my_char): my_int; stdcall;
  Tmysql_dump_debug_info = function(mysql: MYSQL): my_int; stdcall;
  Tmysql_eof = function(res: MYSQL_RES): my_bool; stdcall;
  Tmysql_errno = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_error = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_escape_string = function(_to: my_char; const from: my_char; from_length: my_uint): my_uint; stdcall;
  Tmysql_fetch_field = function(res: MYSQL_RES): MYSQL_FIELD; stdcall;
  Tmysql_fetch_fields = function(res: MYSQL_RES): MYSQL_FIELDS; stdcall;
  Tmysql_fetch_field_direct = function(res: MYSQL_RES; fieldnr: my_uint): MYSQL_FIELD; stdcall;
  Tmysql_fetch_lengths = function(res: MYSQL_RES): MYSQL_LENGTHS; stdcall;
  Tmysql_fetch_row = function(res: MYSQL_RES): MYSQL_ROW; stdcall;
  Tmysql_field_count = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_field_seek = function(res: MYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; stdcall;
  Tmysql_field_tell = function(res: MYSQL_RES): my_uint; stdcall;
  Tmysql_free_result = procedure(res: MYSQL_RES); stdcall;
  Tmysql_get_client_info = function(): my_char; stdcall;
  Tmysql_get_client_version = function: my_uint; stdcall;
  Tmysql_get_host_info = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_get_proto_info = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_get_server_info = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_get_server_status = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_get_server_version = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_info = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_init = function(mysql: MYSQL): MYSQL; stdcall;
  Tmysql_insert_id = function(mysql: MYSQL): my_ulonglong; stdcall;
  Tmysql_kill = function(mysql: MYSQL; pid: my_uint): my_int; stdcall;
  Tmysql_library_end = procedure(); stdcall;
  Tmysql_library_init = function(argc: my_int; const argv, groups: my_char): my_int; stdcall;
  Tmysql_list_dbs = function(mysql: MYSQL; const wild: my_char): MYSQL_RES; stdcall;
  Tmysql_list_fields = function(mysql: MYSQL; const table, wild: my_char): MYSQL_RES; stdcall;
  Tmysql_list_processes = function(mysql: MYSQL): MYSQL_RES; stdcall;
  Tmysql_list_tables = function(mysql: MYSQL; const wild: my_char): MYSQL_RES; stdcall;
  Tmysql_num_fields = function(res: MYSQL_RES): my_uint; stdcall;
  Tmysql_num_rows = function(res: MYSQL_RES): my_ulonglong; stdcall;
  Tmysql_options = function(mysql: MYSQL; option: enum_mysql_option; const arg: my_char): my_int; stdcall;
  Tmysql_ping = function(mysql: MYSQL): my_int; stdcall;
  Tmysql_query = function(mysql: MYSQL; const q: my_char): my_int; stdcall;
  Tmysql_real_connect = function(mysql: MYSQL; const host, user, passwd, db: my_char; port: my_uint; const unix_socket: my_char; clientflag: my_uint): MYSQL; stdcall;
  Tmysql_real_escape_string = function(mysql: MYSQL; _to: my_char; const from: my_char; length: my_uint): my_uint; stdcall;
  Tmysql_real_query = function(mysql: MYSQL; const query: my_char; length: my_uint): my_int; stdcall;
  Tmysql_reload = function(mysql: MYSQL): my_int; stdcall;
  Tmysql_row_seek = function(res: MYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; stdcall;
  Tmysql_row_tell = function(res: MYSQL_RES): MYSQL_ROWS; stdcall;
  Tmysql_select_db = function(mysql: MYSQL; const db: my_char): my_int; stdcall;
  Tmysql_set_character_set = function(mysql: MYSQL; const csname: my_char): my_int; stdcall;
  Tmysql_set_server_option = function(mysql: MYSQL; option: enum_mysql_set_option): my_int; stdcall;
  Tmysql_shutdown = function(mysql: MYSQL; shutdown_level: mysql_enum_shutdown_level): my_int; stdcall;
  Tmysql_sqlstate = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_ssl_set = function(mysql: MYSQL; const key, cert, ca, capath: my_char): my_int; stdcall;
  Tmysql_stat = function(mysql: MYSQL): my_char; stdcall;
  Tmysql_store_result = function(mysql: MYSQL): MYSQL_RES; stdcall;
  Tmysql_thread_end = procedure(); stdcall;
  Tmysql_thread_id = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_thread_init = function(): my_bool; stdcall;
  Tmysql_thread_save = function(): my_uint; stdcall;
  Tmysql_use_result = function(mysql: MYSQL): MYSQL_RES; stdcall;
  Tmysql_warning_count = function(mysql: MYSQL): my_uint; stdcall;
  Tmysql_commit = function(mysql: MYSQL): my_bool; stdcall;
  Tmysql_rollback = function(mysql: MYSQL): my_bool; stdcall;
  Tmysql_autocommit = function(mysql: MYSQL; mode: my_bool): my_bool; stdcall;
  Tmysql_more_results = function(mysql: MYSQL): my_bool; stdcall;
  Tmysql_next_result = function(mysql: MYSQL): my_int; stdcall;
  Tmysql_set_local_infile_default = procedure(mysql: MYSQL); cdecl;
  Tmysql_set_local_infile_handler = procedure(mysql: MYSQL; local_infile_init: Tlocal_infile_init; local_infile_read: Tlocal_infile_read; local_infile_end: Tlocal_infile_end; local_infile_error: Tlocal_infile_error; userdata: Pointer); cdecl;

type
  TMySQL_Character_Sets = record
    CharsetName: PAnsiChar;
    CodePage: Word;
    MaxLen: Byte;
  end;
const
  MySQL_Character_Sets: array[0 .. 31 - 1] of TMySQL_Character_Sets = (
    (CharsetName: 'big5';       CodePage: 65000; MaxLen: 2; ),
    (CharsetName: 'czech';      CodePage: 28592; MaxLen: 1; ),
    (CharsetName: 'dec8';       CodePage:     0; MaxLen: 1; ),
    (CharsetName: 'dos';        CodePage:     0; MaxLen: 1; ),
    (CharsetName: 'german1';    CodePage:  1252; MaxLen: 1; ),
    (CharsetName: 'hp8';        CodePage:     0; MaxLen: 1; ),
    (CharsetName: 'koi8_ru';    CodePage: 21866; MaxLen: 1; ),
    (CharsetName: 'latin1';     CodePage:  1252; MaxLen: 1; ),
    (CharsetName: 'latin2';     CodePage: 28592; MaxLen: 1; ),
    (CharsetName: 'swe7';       CodePage:     0; MaxLen: 1; ),
    (CharsetName: 'usa7';       CodePage:  1252; MaxLen: 1; ),
    (CharsetName: 'ujis';       CodePage: 20932; MaxLen: 1; ),
    (CharsetName: 'sjis';       CodePage:   932; MaxLen: 1; ),
    (CharsetName: 'cp1251';     CodePage:  1251; MaxLen: 1; ),
    (CharsetName: 'danish';     CodePage:  1252; MaxLen: 1; ),
    (CharsetName: 'hebrew';     CodePage:  1255; MaxLen: 1; ),
    (CharsetName: 'win1251';    CodePage:  1251; MaxLen: 1; ),
    (CharsetName: 'tis620';     CodePage:   874; MaxLen: 1; ),
    (CharsetName: 'euc_kr';     CodePage:   949; MaxLen: 1; ),
    (CharsetName: 'estonia';    CodePage: 28603; MaxLen: 1; ),
    (CharsetName: 'hungarian';  CodePage: 28592; MaxLen: 1; ),
    (CharsetName: 'koi8_ukr';   CodePage: 20866; MaxLen: 1; ),
    (CharsetName: 'win1251ukr'; CodePage:  1251; MaxLen: 1; ),
    (CharsetName: 'gb2312';     CodePage:   936; MaxLen: 1; ),
    (CharsetName: 'greek';      CodePage: 28597; MaxLen: 1; ),
    (CharsetName: 'win1250';    CodePage:  1250; MaxLen: 1; ),
    (CharsetName: 'croat';      CodePage: 28592; MaxLen: 1; ),
    (CharsetName: 'gbk';        CodePage:   936; MaxLen: 1; ),
    (CharsetName: 'cp1257';     CodePage:  1257; MaxLen: 1; ),
    (CharsetName: 'latin5';     CodePage:  1254; MaxLen: 1; ),
    (CharsetName: 'latin1_de';  CodePage:  1252; MaxLen: 1; )
  );

type
  TMySQL_Collations = packed record
    CharsetNr: Byte;
    CharsetName: PAnsiChar;
    Default: Boolean;
    CodePage: Word;
    MaxLen: Byte;
  end;
const
  MySQL_Collations: array[0 .. 195 - 1] of TMySQL_Collations = (
    (CharsetNr:   1; CharsetName: 'big5';    Default: True ; CodePage: 65000; MaxLen: 2; ),
    (CharsetNr:   2; CharsetName: 'latin2';  Default: False; CodePage: 28592; MaxLen: 1; ),
    (CharsetNr:   3; CharsetName: 'dec8';    Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:   4; CharsetName: 'cp850';   Default: True ; CodePage:   850; MaxLen: 1; ),
    (CharsetNr:   5; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:   6; CharsetName: 'hp8';     Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:   7; CharsetName: 'koi8r';   Default: True ; CodePage: 20866; MaxLen: 1; ),
    (CharsetNr:   8; CharsetName: 'latin1';  Default: True ; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:   9; CharsetName: 'latin2';  Default: True ; CodePage: 28592; MaxLen: 1; ),
    (CharsetNr:  10; CharsetName: 'swe7';    Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  11; CharsetName: 'ascii';   Default: True ; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  12; CharsetName: 'ujis';    Default: True ; CodePage: 20932; MaxLen: 3; ),
    (CharsetNr:  13; CharsetName: 'sjis';    Default: True ; CodePage:   932; MaxLen: 2; ),
    (CharsetNr:  14; CharsetName: 'cp1251';  Default: False; CodePage:  1251; MaxLen: 1; ),
    (CharsetNr:  15; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  16; CharsetName: 'hebrew';  Default: True ; CodePage:  1255; MaxLen: 1; ),
    (CharsetNr:  18; CharsetName: 'tis620';  Default: True ; CodePage:   874; MaxLen: 1; ),
    (CharsetNr:  19; CharsetName: 'euckr';   Default: True ; CodePage: 51949; MaxLen: 2; ),
    (CharsetNr:  20; CharsetName: 'latin7';  Default: False; CodePage: 28603; MaxLen: 1; ),
    (CharsetNr:  21; CharsetName: 'latin2';  Default: False; CodePage: 28592; MaxLen: 1; ),
    (CharsetNr:  22; CharsetName: 'koi8u';   Default: True ; CodePage: 20866; MaxLen: 1; ),
    (CharsetNr:  23; CharsetName: 'cp1251';  Default: False; CodePage:  1251; MaxLen: 1; ),
    (CharsetNr:  24; CharsetName: 'gb2312';  Default: True ; CodePage:   936; MaxLen: 2; ),
    (CharsetNr:  25; CharsetName: 'greek';   Default: True ; CodePage: 28597; MaxLen: 1; ),
    (CharsetNr:  26; CharsetName: 'cp1250';  Default: True ; CodePage:  1250; MaxLen: 1; ),
    (CharsetNr:  27; CharsetName: 'latin2';  Default: False; CodePage: 28592; MaxLen: 1; ),
    (CharsetNr:  28; CharsetName: 'gbk';     Default: True ; CodePage:   936; MaxLen: 2; ),
    (CharsetNr:  29; CharsetName: 'cp1257';  Default: False; CodePage:  1257; MaxLen: 1; ),
    (CharsetNr:  30; CharsetName: 'latin5';  Default: True ; CodePage:  1254; MaxLen: 1; ),
    (CharsetNr:  31; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  32; CharsetName: 'armscii8';Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  33; CharsetName: 'utf8';    Default: True ; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr:  34; CharsetName: 'cp1250';  Default: False; CodePage:  1250; MaxLen: 1; ),
    (CharsetNr:  35; CharsetName: 'ucs2';    Default: True ; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr:  36; CharsetName: 'cp866';   Default: True ; CodePage:   866; MaxLen: 1; ),
    (CharsetNr:  37; CharsetName: 'keybcs2'; Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  38; CharsetName: 'macce';   Default: True ; CodePage: 10029; MaxLen: 1; ),
    (CharsetNr:  39; CharsetName: 'macroman';Default: True ; CodePage: 10000; MaxLen: 1; ),
    (CharsetNr:  40; CharsetName: 'cp852';   Default: True ; CodePage:   852; MaxLen: 1; ),
    (CharsetNr:  41; CharsetName: 'latin7';  Default: True ; CodePage: 28603; MaxLen: 1; ),
    (CharsetNr:  42; CharsetName: 'latin7';  Default: False; CodePage: 28603; MaxLen: 1; ),
    (CharsetNr:  43; CharsetName: 'macce';   Default: False; CodePage: 10029; MaxLen: 1; ),
    (CharsetNr:  44; CharsetName: 'cp1250';  Default: False; CodePage:  1250; MaxLen: 1; ),
    (CharsetNr:  45; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 1; ),
    (CharsetNr:  46; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 1; ),
    (CharsetNr:  47; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  48; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  49; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  50; CharsetName: 'cp1251';  Default: False; CodePage:  1251; MaxLen: 1; ),
    (CharsetNr:  51; CharsetName: 'cp1251';  Default: True ; CodePage:  1251; MaxLen: 1; ),
    (CharsetNr:  52; CharsetName: 'cp1251';  Default: False; CodePage:  1251; MaxLen: 1; ),
    (CharsetNr:  53; CharsetName: 'macroman';Default: False; CodePage: 10000; MaxLen: 1; ),
    (CharsetNr:  54; CharsetName: 'utf16';   Default: True ; CodePage:  1200; MaxLen: 1; ),
    (CharsetNr:  55; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 1; ),
    (CharsetNr:  57; CharsetName: 'cp1256';  Default: True ; CodePage:  1256; MaxLen: 1; ),
    (CharsetNr:  58; CharsetName: 'cp1257';  Default: False; CodePage:  1257; MaxLen: 1; ),
    (CharsetNr:  59; CharsetName: 'cp1257';  Default: True ; CodePage:  1257; MaxLen: 1; ),
    (CharsetNr:  60; CharsetName: 'utf32';   Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  61; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  63; CharsetName: 'binary';  Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  64; CharsetName: 'armscii8';Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  65; CharsetName: 'ascii';   Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  66; CharsetName: 'cp1250';  Default: False; CodePage:  1250; MaxLen: 1; ),
    (CharsetNr:  67; CharsetName: 'cp1256';  Default: False; CodePage:  1256; MaxLen: 1; ),
    (CharsetNr:  68; CharsetName: 'cp866';   Default: False; CodePage:   866; MaxLen: 1; ),
    (CharsetNr:  69; CharsetName: 'dec8';    Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  70; CharsetName: 'greek';   Default: False; CodePage: 28597; MaxLen: 1; ),
    (CharsetNr:  71; CharsetName: 'hebrew';  Default: False; CodePage:  1255; MaxLen: 1; ),
    (CharsetNr:  72; CharsetName: 'hp8';     Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  73; CharsetName: 'keybcs2'; Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  74; CharsetName: 'koi8r';   Default: False; CodePage: 20866; MaxLen: 1; ),
    (CharsetNr:  75; CharsetName: 'koi8u';   Default: False; CodePage: 21866; MaxLen: 1; ),
    (CharsetNr:  77; CharsetName: 'latin2';  Default: False; CodePage: 28592; MaxLen: 1; ),
    (CharsetNr:  78; CharsetName: 'latin5';  Default: False; CodePage:  1254; MaxLen: 1; ),
    (CharsetNr:  79; CharsetName: 'latin7';  Default: False; CodePage: 28603; MaxLen: 1; ),
    (CharsetNr:  80; CharsetName: 'cp850';   Default: False; CodePage:   850; MaxLen: 1; ),
    (CharsetNr:  81; CharsetName: 'cp852';   Default: False; CodePage:   852; MaxLen: 1; ),
    (CharsetNr:  82; CharsetName: 'swe7';    Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  83; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr:  84; CharsetName: 'big5';    Default: False; CodePage: 65000; MaxLen: 2; ),
    (CharsetNr:  85; CharsetName: 'euckr';   Default: False; CodePage: 51949; MaxLen: 2; ),
    (CharsetNr:  86; CharsetName: 'gb2312';  Default: False; CodePage:   936; MaxLen: 2; ),
    (CharsetNr:  87; CharsetName: 'gbk';     Default: False; CodePage:   936; MaxLen: 2; ),
    (CharsetNr:  88; CharsetName: 'sjis';    Default: False; CodePage:   932; MaxLen: 2; ),
    (CharsetNr:  89; CharsetName: 'tis620';  Default: False; CodePage:   874; MaxLen: 1; ),
    (CharsetNr:  90; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr:  91; CharsetName: 'ujis';    Default: False; CodePage: 20932; MaxLen: 3; ),
    (CharsetNr:  92; CharsetName: 'geostd8'; Default: True ; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  93; CharsetName: 'geostd8'; Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr:  94; CharsetName: 'latin1';  Default: False; CodePage:  1252; MaxLen: 1; ),
    (CharsetNr:  95; CharsetName: 'cp932';   Default: True ; CodePage:   932; MaxLen: 2; ),
    (CharsetNr:  96; CharsetName: 'cp932';   Default: False; CodePage:   932; MaxLen: 2; ),
    (CharsetNr:  97; CharsetName: 'eucjpms'; Default: True ; CodePage:     0; MaxLen: 3; ),
    (CharsetNr:  98; CharsetName: 'eucjpms'; Default: False; CodePage:     0; MaxLen: 3; ),
    (CharsetNr:  99; CharsetName: 'cp1250';  Default: False; CodePage:  1250; MaxLen: 1; ),
    (CharsetNr: 101; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 102; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 103; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 104; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 105; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 106; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 107; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 108; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 109; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 110; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 111; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 112; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 113; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 114; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 115; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 116; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 117; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 118; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 119; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 120; CharsetName: 'utf16';   Default: False; CodePage:  1200; MaxLen: 0; ),
    (CharsetNr: 128; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 129; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 130; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 131; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 132; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 133; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 134; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 135; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 136; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 137; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 138; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 139; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 140; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 141; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 142; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 143; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 144; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 145; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 146; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 147; CharsetName: 'ucs2';    Default: False; CodePage:  1200; MaxLen: 2; ),
    (CharsetNr: 160; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 161; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 162; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 163; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 164; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 165; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 166; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 167; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 168; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 169; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 170; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 171; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 172; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 173; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 174; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 175; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 176; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 177; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 178; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 179; CharsetName: 'utf32';   Default: False; CodePage:     0; MaxLen: 0; ),
    (CharsetNr: 192; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 193; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 194; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 195; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 196; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 197; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 198; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 199; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 200; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 201; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 202; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 203; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 204; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 205; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 206; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 207; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 208; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 209; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 210; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 211; CharsetName: 'utf8mb3'; Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 224; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 225; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 226; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 227; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 228; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 229; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 230; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 231; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 232; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 233; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 234; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 235; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 236; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 237; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 238; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 239; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 240; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 241; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 242; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; ),
    (CharsetNr: 243; CharsetName: 'utf8';    Default: False; CodePage: 65001; MaxLen: 3; )
  );
const
  EE_CANTCREATEFILE                          =    1;
  EE_READ                                    =    2;
  EE_WRITE                                   =    3;
  EE_BADCLOSE                                =    4;
  EE_OUTOFMEMORY                             =    5;
  EE_DELETE                                  =    6;
  EE_LINK                                    =    7;
  EE_EOFERR                                  =    9;
  EE_CANTLOCK                                =   10;
  EE_CANTUNLOCK                              =   11;
  EE_DIR                                     =   12;
  EE_STAT                                    =   13;
  EE_CANT_CHSIZE                             =   14;
  EE_CANT_OPEN_STREAM                        =   15;
  EE_GETWD                                   =   16;
  EE_SETWD                                   =   17;
  EE_LINK_WARNING                            =   18;
  EE_OPEN_WARNING                            =   19;
  EE_DISK_FULL                               =   20;
  EE_CANT_MKDIR                              =   21;
  EE_UNKNOWN_CHARSET                         =   22;
  EE_OUT_OF_FILERESOURCES                    =   23;
  EE_CANT_READLINK                           =   24;
  EE_CANT_SYMLINK                            =   25;
  EE_REALPATH                                =   26;
  EE_SYNC                                    =   27;
  EE_UNKNOWN_COLLATION                       =   28;
  EE_FILENOTFOUND                            =   29;
  EE_FILE_NOT_CLOSED                         =   30;

  ER_ERROR_FIRST                             = 1000;
  ER_HASHCHK                                 = 1000;
  ER_NISAMCHK                                = 1001;
  ER_NO                                      = 1002;
  ER_YES                                     = 1003;
  ER_CANT_CREATE_FILE                        = 1004;
  ER_CANT_CREATE_TABLE                       = 1005;
  ER_CANT_CREATE_DB                          = 1006;
  ER_DB_CREATE_EXISTS                        = 1007;
  ER_DB_DROP_EXISTS                          = 1008;
  ER_DB_DROP_DELETE                          = 1009;
  ER_DB_DROP_RMDIR                           = 1010;
  ER_CANT_DELETE_FILE                        = 1011;
  ER_CANT_FIND_SYSTEM_REC                    = 1012;
  ER_CANT_GET_STAT                           = 1013;
  ER_CANT_GET_WD                             = 1014;
  ER_CANT_LOCK                               = 1015;
  ER_CANT_OPEN_FILE                          = 1016;
  ER_FILE_NOT_FOUND                          = 1017;
  ER_CANT_READ_DIR                           = 1018;
  ER_CANT_SET_WD                             = 1019;
  ER_CHECKREAD                               = 1020;
  ER_DISK_FULL                               = 1021;
  ER_DUP_KEY                                 = 1022;
  ER_ERROR_ON_CLOSE                          = 1023;
  ER_ERROR_ON_READ                           = 1024;
  ER_ERROR_ON_RENAME                         = 1025;
  ER_ERROR_ON_WRITE                          = 1026;
  ER_FILE_USED                               = 1027;
  ER_FILSORT_ABORT                           = 1028;
  ER_FORM_NOT_FOUND                          = 1029;
  ER_GET_ERRNO                               = 1030;
  ER_ILLEGAL_HA                              = 1031;
  ER_KEY_NOT_FOUND                           = 1032;
  ER_NOT_FORM_FILE                           = 1033;
  ER_NOT_KEYFILE                             = 1034;
  ER_OLD_KEYFILE                             = 1035;
  ER_OPEN_AS_READONLY                        = 1036;
  ER_OUTOFMEMORY                             = 1037;
  ER_OUT_OF_SORTMEMORY                       = 1038;
  ER_UNEXPECTED_EOF                          = 1039;
  ER_CON_COUNT_ERROR                         = 1040;
  ER_OUT_OF_RESOURCES                        = 1041;
  ER_BAD_HOST_ERROR                          = 1042;
  ER_HANDSHAKE_ERROR                         = 1043;
  ER_DBACCESS_DENIED_ERROR                   = 1044;
  ER_ACCESS_DENIED_ERROR                     = 1045;
  ER_NO_DB_ERROR                             = 1046;
  ER_UNKNOWN_COM_ERROR                       = 1047;
  ER_BAD_NULL_ERROR                          = 1048;
  ER_BAD_DB_ERROR                            = 1049;
  ER_TABLE_EXISTS_ERROR                      = 1050;
  ER_BAD_TABLE_ERROR                         = 1051;
  ER_NON_UNIQ_ERROR                          = 1052;
  ER_SERVER_SHUTDOWN                         = 1053;
  ER_BAD_FIELD_ERROR                         = 1054;
  ER_WRONG_FIELD_WITH_GROUP                  = 1055;
  ER_WRONG_GROUP_FIELD                       = 1056;
  ER_WRONG_SUM_SELECT                        = 1057;
  ER_WRONG_VALUE_COUNT                       = 1058;
  ER_TOO_LONG_IDENT                          = 1059;
  ER_DUP_FIELDNAME                           = 1060;
  ER_DUP_KEYNAME                             = 1061;
  ER_DUP_ENTRY                               = 1062;
  ER_WRONG_FIELD_SPEC                        = 1063;
  ER_PARSE_ERROR                             = 1064;
  ER_EMPTY_QUERY                             = 1065;
  ER_NONUNIQ_TABLE                           = 1066;
  ER_INVALID_DEFAULT                         = 1067;
  ER_MULTIPLE_PRI_KEY                        = 1068;
  ER_TOO_MANY_KEYS                           = 1069;
  ER_TOO_MANY_KEY_PARTS                      = 1070;
  ER_TOO_LONG_KEY                            = 1071;
  ER_KEY_COLUMN_DOES_NOT_EXITS               = 1072;
  ER_BLOB_USED_AS_KEY                        = 1073;
  ER_TOO_BIG_FIELDLENGTH                     = 1074;
  ER_WRONG_AUTO_KEY                          = 1075;
  ER_READY                                   = 1076;
  ER_NORMAL_SHUTDOWN                         = 1077;
  ER_GOT_SIGNAL                              = 1078;
  ER_SHUTDOWN_COMPLETE                       = 1079;
  ER_FORCING_CLOSE                           = 1080;
  ER_IPSOCK_ERROR                            = 1081;
  ER_NO_SUCH_INDEX                           = 1082;
  ER_WRONG_FIELD_TERMINATORS                 = 1083;
  ER_BLOBS_AND_NO_TERMINATED                 = 1084;
  ER_TEXTFILE_NOT_READABLE                   = 1085;
  ER_FILE_EXISTS_ERROR                       = 1086;
  ER_LOAD_INFO                               = 1087;
  ER_ALTER_INFO                              = 1088;
  ER_WRONG_SUB_KEY                           = 1089;
  ER_CANT_REMOVE_ALL_FIELDS                  = 1090;
  ER_CANT_DROP_FIELD_OR_KEY                  = 1091;
  ER_INSERT_INFO                             = 1092;
  ER_UPDATE_TABLE_USED                       = 1093;
  ER_NO_SUCH_THREAD                          = 1094;
  ER_KILL_DENIED_ERROR                       = 1095;
  ER_NO_TABLES_USED                          = 1096;
  ER_TOO_BIG_SET                             = 1097;
  ER_NO_UNIQUE_LOGFILE                       = 1098;
  ER_TABLE_NOT_LOCKED_FOR_WRITE              = 1099;
  ER_TABLE_NOT_LOCKED                        = 1100;
  ER_BLOB_CANT_HAVE_DEFAULT                  = 1101;
  ER_WRONG_DB_NAME                           = 1102;
  ER_WRONG_TABLE_NAME                        = 1103;
  ER_TOO_BIG_SELECT                          = 1104;
  ER_UNKNOWN_ERROR                           = 1105;
  ER_UNKNOWN_PROCEDURE                       = 1106;
  ER_WRONG_PARAMCOUNT_TO_PROCEDURE           = 1107;
  ER_WRONG_PARAMETERS_TO_PROCEDURE           = 1108;
  ER_UNKNOWN_TABLE                           = 1109;
  ER_FIELD_SPECIFIED_TWICE                   = 1110;
  ER_INVALID_GROUP_FUNC_USE                  = 1111;
  ER_UNSUPPORTED_EXTENSION                   = 1112;
  ER_TABLE_MUST_HAVE_COLUMNS                 = 1113;
  ER_RECORD_FILE_FULL                        = 1114;
  ER_UNKNOWN_CHARACTER_SET                   = 1115;
  ER_TOO_MANY_TABLES                         = 1116;
  ER_TOO_MANY_FIELDS                         = 1117;
  ER_TOO_BIG_ROWSIZE                         = 1118;
  ER_STACK_OVERRUN                           = 1119;
  ER_WRONG_OUTER_JOIN                        = 1120;
  ER_NULL_COLUMN_IN_INDEX                    = 1121;
  ER_CANT_FIND_UDF                           = 1122;
  ER_CANT_INITIALIZE_UDF                     = 1123;
  ER_UDF_NO_PATHS                            = 1124;
  ER_UDF_EXISTS                              = 1125;
  ER_CANT_OPEN_LIBRARY                       = 1126;
  ER_CANT_FIND_DL_ENTRY                      = 1127;
  ER_FUNCTION_NOT_DEFINED                    = 1128;
  ER_HOST_IS_BLOCKED                         = 1129;
  ER_HOST_NOT_PRIVILEGED                     = 1130;
  ER_PASSWORD_ANONYMOUS_USER                 = 1131;
  ER_PASSWORD_NOT_ALLOWED                    = 1132;
  ER_PASSWORD_NO_MATCH                       = 1133;
  ER_UPDATE_INFO                             = 1134;
  ER_CANT_CREATE_THREAD                      = 1135;
  ER_WRONG_VALUE_COUNT_ON_ROW                = 1136;
  ER_CANT_REOPEN_TABLE                       = 1137;
  ER_INVALID_USE_OF_NULL                     = 1138;
  ER_REGEXP_ERROR                            = 1139;
  ER_MIX_OF_GROUP_FUNC_AND_FIELDS            = 1140;
  ER_NONEXISTING_GRANT                       = 1141;
  ER_TABLEACCESS_DENIED_ERROR                = 1142;
  ER_COLUMNACCESS_DENIED_ERROR               = 1143;
  ER_ILLEGAL_GRANT_FOR_TABLE                 = 1144;
  ER_GRANT_WRONG_HOST_OR_USER                = 1145;
  ER_NO_SUCH_TABLE                           = 1146;
  ER_NONEXISTING_TABLE_GRANT                 = 1147;
  ER_NOT_ALLOWED_COMMAND                     = 1148;
  ER_SYNTAX_ERROR                            = 1149;
  ER_DELAYED_CANT_CHANGE_LOCK                = 1150;
  ER_TOO_MANY_DELAYED_THREADS                = 1151;
  ER_ABORTING_CONNECTION                     = 1152;
  ER_NET_PACKET_TOO_LARGE                    = 1153;
  ER_NET_READ_ERROR_FROM_PIPE                = 1154;
  ER_NET_FCNTL_ERROR                         = 1155;
  ER_NET_PACKETS_OUT_OF_ORDER                = 1156;
  ER_NET_UNCOMPRESS_ERROR                    = 1157;
  ER_NET_READ_ERROR                          = 1158;
  ER_NET_READ_INTERRUPTED                    = 1159;
  ER_NET_ERROR_ON_WRITE                      = 1160;
  ER_NET_WRITE_INTERRUPTED                   = 1161;
  ER_TOO_LONG_STRING                         = 1162;
  ER_TABLE_CANT_HANDLE_BLOB                  = 1163;
  ER_TABLE_CANT_HANDLE_AUTO_INCREMENT        = 1164;
  ER_DELAYED_INSERT_TABLE_LOCKED             = 1165;
  ER_WRONG_COLUMN_NAME                       = 1166;
  ER_WRONG_KEY_COLUMN                        = 1167;
  ER_WRONG_MRG_TABLE                         = 1168;
  ER_DUP_UNIQUE                              = 1169;
  ER_BLOB_KEY_WITHOUT_LENGTH                 = 1170;
  ER_PRIMARY_CANT_HAVE_NULL                  = 1171;
  ER_TOO_MANY_ROWS                           = 1172;
  ER_REQUIRES_PRIMARY_KEY                    = 1173;
  ER_NO_RAID_COMPILED                        = 1174;
  ER_UPDATE_WITHOUT_KEY_IN_SAFE_MODE         = 1175;
  ER_KEY_DOES_NOT_EXITS                      = 1176;
  ER_CHECK_NO_SUCH_TABLE                     = 1177;
  ER_CHECK_NOT_IMPLEMENTED                   = 1178;
  ER_CANT_DO_THIS_DURING_AN_TRANSACTION      = 1179;
  ER_ERROR_DURING_COMMIT                     = 1180;
  ER_ERROR_DURING_ROLLBACK                   = 1181;
  ER_ERROR_DURING_FLUSH_LOGS                 = 1182;
  ER_ERROR_DURING_CHECKPOINT                 = 1183;
  ER_NEW_ABORTING_CONNECTION                 = 1184;
  ER_DUMP_NOT_IMPLEMENTED                    = 1185;
  ER_FLUSH_MASTER_BINLOG_CLOSED              = 1186;
  ER_INDEX_REBUILD                           = 1187;
  ER_MASTER                                  = 1188;
  ER_MASTER_NET_READ                         = 1189;
  ER_MASTER_NET_WRITE                        = 1190;
  ER_FT_MATCHING_KEY_NOT_FOUND               = 1191;
  ER_LOCK_OR_ACTIVE_TRANSACTION              = 1192;
  ER_UNKNOWN_SYSTEM_VARIABLE                 = 1193;
  ER_CRASHED_ON_USAGE                        = 1194;
  ER_CRASHED_ON_REPAIR                       = 1195;
  ER_WARNING_NOT_COMPLETE_ROLLBACK           = 1196;
  ER_TRANS_CACHE_FULL                        = 1197;
  ER_SLAVE_MUST_STOP                         = 1198;
  ER_SLAVE_NOT_RUNNING                       = 1199;
  ER_BAD_SLAVE                               = 1200;
  ER_MASTER_INFO                             = 1201;
  ER_SLAVE_THREAD                            = 1202;
  ER_TOO_MANY_USER_CONNECTIONS               = 1203;
  ER_SET_CONSTANTS_ONLY                      = 1204;
  ER_LOCK_WAIT_TIMEOUT                       = 1205;
  ER_LOCK_TABLE_FULL                         = 1206;
  ER_READ_ONLY_TRANSACTION                   = 1207;
  ER_DROP_DB_WITH_READ_LOCK                  = 1208;
  ER_CREATE_DB_WITH_READ_LOCK                = 1209;
  ER_WRONG_ARGUMENTS                         = 1210;
  ER_NO_PERMISSION_TO_CREATE_USER            = 1211;
  ER_UNION_TABLES_IN_DIFFERENT_DIR           = 1212;
  ER_LOCK_DEADLOCK                           = 1213;
  ER_TABLE_CANT_HANDLE_FT                    = 1214;
  ER_CANNOT_ADD_FOREIGN                      = 1215;
  ER_NO_REFERENCED_ROW                       = 1216;
  ER_ROW_IS_REFERENCED                       = 1217;
  ER_CONNECT_TO_MASTER                       = 1218;
  ER_QUERY_ON_MASTER                         = 1219;
  ER_ERROR_WHEN_EXECUTING_COMMAND            = 1220;
  ER_WRONG_USAGE                             = 1221;
  ER_WRONG_NUMBER_OF_COLUMNS_IN_SELECT       = 1222;
  ER_CANT_UPDATE_WITH_READLOCK               = 1223;
  ER_MIXING_NOT_ALLOWED                      = 1224;
  ER_DUP_ARGUMENT                            = 1225;
  ER_USER_LIMIT_REACHED                      = 1226;
  ER_SPECIFIC_ACCESS_DENIED_ERROR            = 1227;
  ER_LOCAL_VARIABLE                          = 1228;
  ER_GLOBAL_VARIABLE                         = 1229;
  ER_NO_DEFAULT                              = 1230;
  ER_WRONG_VALUE_FOR_VAR                     = 1231;
  ER_WRONG_TYPE_FOR_VAR                      = 1232;
  ER_VAR_CANT_BE_READ                        = 1233;
  ER_CANT_USE_OPTION_HERE                    = 1234;
  ER_NOT_SUPPORTED_YET                       = 1235;
  ER_MASTER_FATAL_ERROR_READING_BINLOG       = 1236;
  ER_SLAVE_IGNORED_TABLE                     = 1237;
  ER_INCORRECT_GLOBAL_LOCAL_VAR              = 1238;
  ER_WRONG_FK_DEF                            = 1239;
  ER_KEY_REF_DO_NOT_MATCH_TABLE_REF          = 1240;
  ER_OPERAND_COLUMNS                         = 1241;
  ER_SUBQUERY_NO_1_ROW                       = 1242;
  ER_UNKNOWN_STMT_HANDLER                    = 1243;
  ER_CORRUPT_HELP_DB                         = 1244;
  ER_CYCLIC_REFERENCE                        = 1245;
  ER_AUTO_CONVERT                            = 1246;
  ER_ILLEGAL_REFERENCE                       = 1247;
  ER_DERIVED_MUST_HAVE_ALIAS                 = 1248;
  ER_SELECT_REDUCED                          = 1249;
  ER_TABLENAME_NOT_ALLOWED_HERE              = 1250;
  ER_NOT_SUPPORTED_AUTH_MODE                 = 1251;
  ER_SPATIAL_CANT_HAVE_NULL                  = 1252;
  ER_COLLATION_CHARSET_MISMATCH              = 1253;
  ER_SLAVE_WAS_RUNNING                       = 1254;
  ER_SLAVE_WAS_NOT_RUNNING                   = 1255;
  ER_TOO_BIG_FOR_UNCOMPRESS                  = 1256;
  ER_ZLIB_Z_MEM_ERROR                        = 1257;
  ER_ZLIB_Z_BUF_ERROR                        = 1258;
  ER_ZLIB_Z_DATA_ERROR                       = 1259;
  ER_CUT_VALUE_GROUP_CONCAT                  = 1260;
  ER_WARN_TOO_FEW_RECORDS                    = 1261;
  ER_WARN_TOO_MANY_RECORDS                   = 1262;
  ER_WARN_NULL_TO_NOTNULL                    = 1263;
  ER_WARN_DATA_OUT_OF_RANGE                  = 1264;
  EARN_DATA_TRUNCATED                        = 1265;
  ER_WARN_USING_OTHER_HANDLER                = 1266;
  ER_CANT_AGGREGATE_2COLLATIONS              = 1267;
  ER_DROP_USER                               = 1268;
  ER_REVOKE_GRANTS                           = 1269;
  ER_CANT_AGGREGATE_3COLLATIONS              = 1270;
  ER_CANT_AGGREGATE_NCOLLATIONS              = 1271;
  ER_VARIABLE_IS_NOT_STRUCT                  = 1272;
  ER_UNKNOWN_COLLATION                       = 1273;
  ER_SLAVE_IGNORED_SSL_PARAMS                = 1274;
  ER_SERVER_IS_IN_SECURE_AUTH_MODE           = 1275;
  ER_WARN_FIELD_RESOLVED                     = 1276;
  ER_BAD_SLAVE_UNTIL_COND                    = 1277;
  ER_MISSING_SKIP_SLAVE                      = 1278;
  ER_UNTIL_COND_IGNORED                      = 1279;
  ER_WRONG_NAME_FOR_INDEX                    = 1280;
  ER_WRONG_NAME_FOR_CATALOG                  = 1281;
  ER_WARN_QC_RESIZE                          = 1282;
  ER_BAD_FT_COLUMN                           = 1283;
  ER_UNKNOWN_KEY_CACHE                       = 1284;
  ER_WARN_HOSTNAME_WONT_WORK                 = 1285;
  ER_UNKNOWN_STORAGE_ENGINE                  = 1286;
  ER_WARN_DEPRECATED_SYNTAX                  = 1287;
  ER_NON_UPDATABLE_TABLE                     = 1288;
  ER_FEATURE_DISABLED                        = 1289;
  ER_OPTION_PREVENTS_STATEMENT               = 1290;
  ER_DUPLICATED_VALUE_IN_TYPE                = 1291;
  ER_TRUNCATED_WRONG_VALUE                   = 1292;
  ER_TOO_MUCH_AUTO_TIMESTAMP_COLS            = 1293;
  ER_INVALID_ON_UPDATE                       = 1294;
  ER_UNSUPPORTED_PS                          = 1295;
  ER_GET_ERRMSG                              = 1296;
  ER_GET_TEMPORARY_ERRMSG                    = 1297;
  ER_UNKNOWN_TIME_ZONE                       = 1298;
  ER_WARN_INVALID_TIMESTAMP                  = 1299;
  ER_INVALID_CHARACTER_STRING                = 1300;
  ER_WARN_ALLOWED_PACKET_OVERFLOWED          = 1301;
  ER_CONFLICTING_DECLARATIONS                = 1302;
  ER_SP_NO_RECURSIVE_CREATE                  = 1303;
  ER_SP_ALREADY_EXISTS                       = 1304;
  ER_SP_DOES_NOT_EXIST                       = 1305;
  ER_SP_DROP_FAILED                          = 1306;
  ER_SP_STORE_FAILED                         = 1307;
  ER_SP_LILABEL_MISMATCH                     = 1308;
  ER_SP_LABEL_REDEFINE                       = 1309;
  ER_SP_LABEL_MISMATCH                       = 1310;
  ER_SP_UNINIT_VAR                           = 1311;
  ER_SP_BADSELECT                            = 1312;
  ER_SP_BADRETURN                            = 1313;
  ER_SP_BADSTATEMENT                         = 1314;
  ER_UPDATE_LOG_DEPRECATED_IGNORED           = 1315;
  ER_UPDATE_LOG_DEPRECATED_TRANSLATED        = 1316;
  ER_QUERY_INTERRUPTED                       = 1317;
  ER_SP_WRONG_NO_OF_ARGS                     = 1318;
  ER_SP_COND_MISMATCH                        = 1319;
  ER_SP_NORETURN                             = 1320;
  ER_SP_NORETURNEND                          = 1321;
  ER_SP_BAD_CURSOR_QUERY                     = 1322;
  ER_SP_BAD_CURSOR_SELECT                    = 1323;
  ER_SP_CURSOR_MISMATCH                      = 1324;
  ER_SP_CURSOR_ALREADY_OPEN                  = 1325;
  ER_SP_CURSOR_NOT_OPEN                      = 1326;
  ER_SP_UNDECLARED_VAR                       = 1327;
  ER_SP_WRONG_NO_OF_FETCH_ARGS               = 1328;
  ER_SP_FETCH_NO_DATA                        = 1329;
  ER_SP_DUP_PARAM                            = 1330;
  ER_SP_DUP_VAR                              = 1331;
  ER_SP_DUP_COND                             = 1332;
  ER_SP_DUP_CURS                             = 1333;
  ER_SP_CANT_ALTER                           = 1334;
  ER_SP_SUBSELECT_NYI                        = 1335;
  ER_SP_NO_USE                               = 1336;
  ER_SP_VARCOND_AFTER_CURSHNDLR              = 1337;
  ER_SP_CURSOR_AFTER_HANDLER                 = 1338;
  ER_SP_CASE_NOT_FOUND                       = 1339;
  ER_FPARSER_TOO_BIG_FILE                    = 1340;
  ER_FPARSER_BAD_HEADER                      = 1341;
  ER_FPARSER_EOF_IN_COMMENT                  = 1342;
  ER_FPARSER_ERROR_IN_PARAMETER              = 1343;
  ER_FPARSER_EOF_IN_UNKNOWN_PARAMETER        = 1344;
  ER_VIEW_NO_EXPLAIN                         = 1345;
  ER_FRM_UNKNOWN_TYPE                        = 1346;
  ER_WRONG_OBJECT                            = 1347;
  ER_NONUPDATEABLE_COLUMN                    = 1348;
  ER_VIEW_SELECT_DERIVED                     = 1349;
  ER_VIEW_SELECT_CLAUSE                      = 1350;
  ER_VIEW_SELECT_VARIABLE                    = 1351;
  ER_VIEW_SELECT_TMPTABLE                    = 1352;
  ER_VIEW_WRONG_LIST                         = 1353;
  ER_WARN_VIEW_MERGE                         = 1354;
  ER_WARN_VIEW_WITHOUT_KEY                   = 1355;
  ER_VIEW_INVALID                            = 1356;
  ER_SP_NO_DROP_SP                           = 1357;
  ER_SP_GOTO_IN_HNDLR                        = 1358;
  ER_TRG_ALREADY_EXISTS                      = 1359;
  ER_TRG_DOES_NOT_EXIST                      = 1360;
  ER_TRG_ON_VIEW_OR_TEMP_TABLE               = 1361;
  ER_TRG_CANT_CHANGE_ROW                     = 1362;
  ER_TRG_NO_SUCH_ROW_IN_TRG                  = 1363;
  ER_NO_DEFAULT_FOR_FIELD                    = 1364;
  ER_DIVISION_BY_ZERO                        = 1365;
  ER_TRUNCATED_WRONG_VALUE_FOR_FIELD         = 1366;
  ER_ILLEGAL_VALUE_FOR_TYPE                  = 1367;
  ER_VIEW_NONUPD_CHECK                       = 1368;
  ER_VIEW_CHECK_FAILED                       = 1369;
  ER_PROCACCESS_DENIED_ERROR                 = 1370;
  ER_RELAY_LOG_FAIL                          = 1371;
  ER_PASSWD_LENGTH                           = 1372;
  ER_UNKNOWN_TARGET_BINLOG                   = 1373;
  ER_IO_ERR_LOG_INDEX_READ                   = 1374;
  ER_BINLOG_PURGE_PROHIBITED                 = 1375;
  ER_FSEEK_FAIL                              = 1376;
  ER_BINLOG_PURGE_FATAL_ERR                  = 1377;
  ER_LOG_IN_USE                              = 1378;
  ER_LOG_PURGE_UNKNOWN_ERR                   = 1379;
  ER_RELAY_LOG_INIT                          = 1380;
  ER_NO_BINARY_LOGGING                       = 1381;
  ER_RESERVED_SYNTAX                         = 1382;
  ER_WSAS_FAILED                             = 1383;
  ER_DIFF_GROUPS_PROC                        = 1384;
  ER_NO_GROUP_FOR_PROC                       = 1385;
  ER_ORDER_WITH_PROC                         = 1386;
  ER_LOGGING_PROHIBIT_CHANGING_OF            = 1387;
  ER_NO_FILE_MAPPING                         = 1388;
  ER_WRONG_MAGIC                             = 1389;
  ER_PS_MANY_PARAM                           = 1390;
  ER_KEY_PART_0                              = 1391;
  ER_VIEW_CHECKSUM                           = 1392;
  ER_VIEW_MULTIUPDATE                        = 1393;
  ER_VIEW_NO_INSERT_FIELD_LIST               = 1394;
  ER_VIEW_DELETE_MERGE_VIEW                  = 1395;
  ER_CANNOT_USER                             = 1396;
  ER_XAER_NOTA                               = 1397;
  ER_XAER_INVAL                              = 1398;
  ER_XAER_RMFAIL                             = 1399;
  ER_XAER_OUTSIDE                            = 1400;
  ER_XAER_RMERR                              = 1401;
  ER_XA_RBROLLBACK                           = 1402;
  ER_NONEXISTING_PROC_GRANT                  = 1403;
  ER_PROC_AUTO_GRANT_FAIL                    = 1404;
  ER_PROC_AUTO_REVOKE_FAIL                   = 1405;
  ER_DATA_TOO_LONG                           = 1406;
  ER_SP_BAD_SQLSTATE                         = 1407;
  ER_STARTUP                                 = 1408;
  ER_LOAD_FROM_FIXED_SIZE_ROWS_TO_VAR        = 1409;
  ER_CANT_CREATE_USER_WITH_GRANT             = 1410;
  ER_WRONG_VALUE_FOR_TYPE                    = 1411;
  ER_TABLE_DEF_CHANGED                       = 1412;
  ER_SP_DUP_HANDLER                          = 1413;
  ER_SP_NOT_VAR_ARG                          = 1414;
  ER_SP_NO_RETSET_IN_FUNC                    = 1415;
  ER_CANT_CREATE_GEOMETRY_OBJECT             = 1416;
  ER_FAILED_ROUTINE_BREAK_BINLOG             = 1417;
  ER_BINLOG_UNSAFE_ROUTINE                   = 1418;
  ER_BINLOG_CREATE_ROUTINE_NEED_SUPER        = 1419;
  ER_EXEC_STMT_WITH_OPEN_CURSOR              = 1420;
  ER_STMT_HAS_NO_OPEN_CURSOR                 = 1421;
  ER_COMMIT_NOT_ALLOWED_IN_SF_OR_TRG         = 1422;
  ER_NO_DEFAULT_FOR_VIEW_FIELD               = 1423;
  ER_SP_NO_RECURSION                         = 1424;
  ER_TOO_BIG_SCALE                           = 1425;
  ER_TOO_BIG_PRECISION                       = 1426;
  ER_M_BIGGER_THAN_D                         = 1427;
  ER_WRONG_LOCK_OF_SYSTEM_TABLE              = 1428;
  ER_CONNECT_TO_FOREIGN_DATA_SOURCE          = 1429;
  ER_QUERY_ON_FOREIGN_DATA_SOURCE            = 1430;
  ER_FOREIGN_DATA_SOURCE_DOESNT_EXIST        = 1431;
  ER_FOREIGN_DATA_STRING_INVALID_CANT_CREATE = 1432;
  ER_FOREIGN_DATA_STRING_INVALID             = 1433;
  ER_CANT_CREATE_FEDERATED_TABLE             = 1434;
  ER_TRG_IN_WRONG_SCHEMA                     = 1435;
  ER_STACK_OVERRUN_NEED_MORE                 = 1436;
  ER_TOO_LONG_BODY                           = 1437;
  ER_WARN_CANT_DROP_DEFAULT_KEYCACHE         = 1438;
  ER_TOO_BIG_DISPLAYWIDTH                    = 1439;
  ER_XAER_DUPID                              = 1440;
  ER_DATETIME_FUNCTION_OVERFLOW              = 1441;
  ER_CANT_UPDATE_USED_TABLE_IN_SF_OR_TRG     = 1442;
  ER_VIEW_PREVENT_UPDATE                     = 1443;
  ER_PS_NO_RECURSION                         = 1444;
  ER_SP_CANT_SET_AUTOCOMMIT                  = 1445;
  ER_MALFORMED_DEFINER                       = 1446;
  ER_VIEW_FRM_NO_USER                        = 1447;
  ER_VIEW_OTHER_USER                         = 1448;
  ER_NO_SUCH_USER                            = 1449;
  ER_FORBID_SCHEMA_CHANGE                    = 1450;
  ER_ROW_IS_REFERENCED_2                     = 1451;
  ER_NO_REFERENCED_ROW_2                     = 1452;
  ER_SP_BAD_VAR_SHADOW                       = 1453;
  ER_TRG_NO_DEFINER                          = 1454;
  ER_OLD_FILE_FORMAT                         = 1455;
  ER_SP_RECURSION_LIMIT                      = 1456;
  ER_SP_PROC_TABLE_CORRUPT                   = 1457;
  ER_SP_WRONG_NAME                           = 1458;
  ER_TABLE_NEEDS_UPGRADE                     = 1459;
  ER_SP_NO_AGGREGATE                         = 1460;
  ER_MAX_PREPARED_STMT_COUNT_REACHED         = 1461;
  ER_VIEW_RECURSIVE                          = 1462;
  ER_NON_GROUPING_FIELD_USED                 = 1463;
  ER_TABLE_CANT_HANDLE_SPKEYS                = 1464;
  ER_NO_TRIGGERS_ON_SYSTEM_SCHEMA            = 1465;
  ER_REMOVED_SPACES                          = 1466;
  ER_AUTOINC_READ_FAILED                     = 1467;
  ER_USERNAME                                = 1468;
  ER_HOSTNAME                                = 1469;
  ER_WRONG_STRING_LENGTH                     = 1470;
  ER_NON_INSERTABLE_TABLE                    = 1471;
  ER_ADMIN_WRONG_MRG_TABLE                   = 1472;
  ER_TOO_HIGH_LEVEL_OF_NESTING_FOR_SELECT    = 1473;
  ER_NAME_BECOMES_EMPTY                      = 1474;
  ER_AMBIGUOUS_FIELD_TERM                    = 1475;
  ER_LOAD_DATA_INVALID_COLUMN                = 1476;
  ER_LOG_PURGE_NO_FILE                       = 1477;
  ER_XA_RBTIMEOUT                            = 1478;
  ER_XA_RBDEADLOCK                           = 1479;
  ER_TOO_MANY_CONCURRENT_TRXS                = 1480;

  CR_MIN_ERROR                               = 2000;
  CR_UNKNOWN_ERROR                           = 2000;
  CR_SOCKET_CREATE_ERROR                     = 2001;
  CR_CONNECTION_ERROR                        = 2002;
  CR_CONN_HOST_ERROR                         = 2003;
  CR_IPSOCK_ERROR                            = 2004;
  CR_UNKNOWN_HOST                            = 2005;
  CR_SERVER_GONE_ERROR                       = 2006;
  CR_VERSION_ERROR                           = 2007;
  CR_OUT_OF_MEMORY                           = 2008;
  CR_WRONG_HOST_INFO                         = 2009;
  CR_LOCALHOST_CONNECTION                    = 2010;
  CR_TCP_CONNECTION                          = 2011;
  CR_SERVER_HANDSHAKE_ERR                    = 2012;
  CR_SERVER_LOST                             = 2013;
  CR_COMMANDS_OUT_OF_SYNC                    = 2014;
  CR_NAMEDPIPE_CONNECTION                    = 2015;
  CR_NAMEDPIPEWAIT_ERROR                     = 2016;
  CR_NAMEDPIPEOPEN_ERROR                     = 2017;
  CR_NAMEDPIPESETSTATE_ERROR                 = 2018;
  CR_CANT_READ_CHARSET                       = 2019;
  CR_NET_PACKET_TOO_LARGE                    = 2020;
  CR_EMBEDDED_CONNECTION                     = 2021;
  CR_PROBE_SLAVE_STATUS                      = 2022;
  CR_PROBE_SLAVE_HOSTS                       = 2023;
  CR_PROBE_SLAVE_CONNECT                     = 2024;
  CR_PROBE_MASTER_CONNECT                    = 2025;
  CR_SSL_CONNECTION_ERROR                    = 2026;
  CR_MALFORMED_PACKET                        = 2027;
  CR_WRONG_LICENSE                           = 2028;
  CR_NULL_POINTER                            = 2029;
  CR_NO_PREPARE_STMT                         = 2030;
  CR_PARAMS_NOT_BOUND                        = 2031;
  CR_DATA_TRUNCATED                          = 2032;
  CR_NO_PARAMETERS_EXISTS                    = 2033;
  CR_INVALID_PARAMETER_NO                    = 2034;
  CR_INVALID_BUFFER_USE                      = 2035;
  CR_UNSUPPORTED_PARAM_TYPE                  = 2036;

const
  ER_NET_PACKET_TOO_LARGE_MSG = 'Got a packet bigger than ''max_allowed_packet'' bytes';
  ER_WARN_TOO_FEW_RECORDS_MSG = 'Row %d doesn''t contain data for all columns';
  ER_WARN_TOO_MANY_RECORDS_MSG = 'Row %d was truncated; it contained more data than there were input columns';

var
  CLIENT_ERRORS: array [0..20] of PChar = (
    'Unknown MySQL error',                                                   {0}
    'Can''t create UNIX socket (%d)',                                        {1}
    'Can''t connect to local MySQL server through socket ''%-.64s'' (%d)',   {2}
    'Can''t connect to MySQL server on ''%-.64s'' (%d)',                     {3}
    'Can''t create TCP/IP socket (%d)',                                      {4}
    'Unknown MySQL Server Host ''%-.64s'' (%d)',                             {5}
    'MySQL server has gone away',                                            {6}
    'Protocol mismatch. Server Version = %d Client Version = %d',            {7}
    'MySQL client run out of memory',                                        {8}
    'Wrong host info',                                                       {9}
    'Localhost via UNIX socket',                                            {10}
    '%-.64s via TCP/IP',                                                    {11}
    'Error in server handshake',                                            {12}
    'Lost connection to MySQL server during query',                         {13}
    'Commands out of sync;  You can''t run this command now',               {14}
    '%-.64s via named pipe',                                                {15}
    'Can''t wait for named pipe to host: %-.64s  pipe: %-.32s (%u)',        {16}
    'Can''t open named pipe to host: %-.64s  pipe: %-.32s (%u)',            {17}
    'Can''t set state of named pipe to host: %-.64s  pipe: %-.32s (%u)',    {18}
    'Can''t initialize character set %-.64s',                               {19}
    'Got packet bigger than ''max_allowed_packet'''                         {20}
  );
  
implementation {***************************************************************}

end.

