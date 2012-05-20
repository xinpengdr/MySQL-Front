unit SQLUtils;

interface {********************************************************************}

uses
  SysUtils;

type
  TSQLStrings = array of string;

  TSQLCLStmt = packed record // must be "packed", since asm code address it as packed
    CommandType: (ctSetNames, ctSetCharacterSet, ctUse);
    ObjectName: string;
  end;

  TSQLDDLStmt = packed record // must be "packed", since asm code address it as packed
    DefinitionType: (dtCreate, dtAlter, dtAlterRename, dtRename, dtDrop);
    ObjectType: (otDatabase, otEvent, otFunction, otProcedure, otTable, otTrigger, otView);
    DatabaseName: string;
    ObjectName: string;
    NewDatabaseName: string;
    NewObjectName: string;
  end;

  TSQLDMLStmt = packed record // must be "packed", since asm code address it as packed
    ManipulationType: (mtInsert, mtUpdate, mtDelete);
    DatabaseNames: array of string;
    TableNames: array of string;
  end;

  TSQLSLStmt = packed record // must be "packed", since asm code address it as packed
    SelectType: (stSelect, stShow);
    DatabaseName: string;
    ItemName: string;
  end;

  TSQLParse = packed record // must be "packed", since asm code address it as packed
    Pos: PChar;                          // Current point of parsing
    Len: Integer;
    EDX: Cardinal;                       // Version of MySQL conditional Code
    Start: PChar;                        // Complete parsed SQL
  end;

function BitStringToInt(const BitString: string): UInt64;
function IntToBitString(const Value: UInt64; const Width: Integer = 1): string;
function SQLCreateParse(out Handle: TSQLParse; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
function SQLEscape(const Value: string; const ODBCEncoding: Boolean): string; overload;
function SQLEscape(const Value: string; const Quoter: Char = ''''): string; overload;
function SQLEscape(const Value: PAnsiChar; const Length: Integer; const ODBCEncoding: Boolean): string; overload;
function SQLParseCallStmt(const SQL: PChar; const Len: Integer; out ProcedureName: string; const Version: Integer): Boolean;
function SQLParseChar(var Handle: TSQLParse; const Character: Char; const IncrementIndex: Boolean = True): Boolean;
function SQLParseCLStmt(out CLStmt: TSQLCLStmt; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
function SQLParseDDLStmt(out DDLStmt: TSQLDDLStmt; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
function SQLParseDMLStmt(out DMLStmt: TSQLDMLStmt; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
function SQLParseEnd(const Handle: TSQLParse): Boolean;
function SQLParseGetIndex(const Handle: TSQLParse): Integer;
function SQLParseKeyword(var Handle: TSQLParse; const Keyword: PChar; const IncrementIndex: Boolean = True): Boolean;
function SQLParseObjectName(var Handle: TSQLParse; var DatabaseName: string; out ObjectName: string): Boolean;
function SQLParseValue(var Handle: TSQLParse; const TrimAfterValue: Boolean = True): string;
function SQLSingleStmt(const SQL: string): Boolean;
procedure SQLSplitValues(const Text: string; out Values: TSQLStrings);
function SQLStmtLength(const SQL: string; const Index: Integer = 1; const CompleteStmt: PBoolean = nil): Integer;
function SQLStmtToCaption(const SQL: string; const Len: Integer = 50): string;
function SQLTrimStmt(const SQL: string): string; overload;
function SQLTrimStmt(const SQL: string; const Index, Length: Integer; var StartingCommentLength, EndingCommentLength: Integer): Integer; overload;
function SQLUnescape(const Value: RawByteString): RawByteString; overload;
function SQLUnescape(const Value: string): string; overload;
function SQLWrapStmt(const SQL: string; const WrapStrs: array of string; const Indent: Integer): string;
function SQLUnwrapStmt(const SQL: string): string;

implementation {***************************************************************}

uses
  RTLConsts, Classes;

resourcestring
  SInvalidSQLText = 'Invalid SQL text near "%s".';

const
  KAlgorithm: PChar = 'ALGORITHM';
  KAlter: PChar = 'ALTER';
  KBegin: PChar = 'BEGIN';
  KCall: PChar = 'CALL';
  KCase: PChar = 'CASE';
  KCreate: PChar = 'CREATE';
  KDatabase: PChar = 'DATABASE';
  KDefiner: PChar = 'DEFINER';
  KDelayed: PChar = 'DELAYED';
  KDelete: PChar = 'DELETE';
  KDrop: PChar = 'DROP';
  KEnd:PChar = 'END';
  KEvent: PChar = 'EVENT';
  KFrom: PChar = 'FROM';
  KFunction: PChar = 'FUNCTION';
  KHighPriority: PChar = 'HIGH_PRIORITY';
  KIf: PChar = 'IF';
  KIgnore: PChar = 'IGNORE';
  KInsert: PChar = 'INSERT';
  KInto: PChar = 'INTO';
  KLoop: PChar = 'LOOP';
  KLowPriority: PChar = 'LOW_PRIORITY';
  KOrReplace: PChar = 'OR REPLACE';
  KProcedure: PChar = 'PROCEDURE';
  KQuick: PChar = 'QUICK';
  KRename: PChar = 'RENAME';
  KRepeat: PChar = 'REPEAT';
  KSelect: PChar = 'SELECT';
  KSetNames: PChar = 'SET NAMES';
  KSetCharacterSet: PChar = 'SET CHARACTER SET';
  KShow: PChar = 'SHOW';
  KSQLSecurityDefiner: PChar = 'SQL SECURITY DEFINER';
  KSQLSecurityInvoker: PChar = 'SQL SECURITY INVOKER';
  KTable: PChar = 'TABLE';
  KTrigger: PChar = 'TRIGGER';
  KUpdate: PChar = 'UPDATE';
  KUse: PChar = 'USE';
  KView: PChar = 'VIEW';
  KWhile: PChar = 'WHILE';
  KWork: PChar = 'WORK';

procedure MoveString();
// ESI: Pointer to SQL
// ECX: Characters left in SQL
// EDI: Pointer to Result
// ESI will be moved to the next usable character inside SQL
// ECX will be decremened of the string length
// ZF if no string copied
label
  Quoted, Quoted1, QuotedL, QuotedL1, QuotedL2, QuotedLE,
  Finish;
asm
        PUSH EAX
        PUSH EBX

        CMP WORD PTR [ESI],''''          // Start quotation in SQL?
        JE Quoted                        // Yes!
        CMP WORD PTR [ESI],'"'           // Start quotation in SQL?
        JE Quoted                        // Yes!
        CMP WORD PTR [ESI],'`'           // Start quotation in SQL?
        JE Quoted                        // Yes!
        MOV EBX,False                    // string not found!
        JMP Finish

      Quoted:
        MOV EBX,True                     // string found!
        LODSW                            // Get used Quoter
        CMP EDI,0                        // Store the string somewhere?
        JE Quoted1                       // No!
        STOSW                            // Put character
      Quoted1:
        MOV DX,AX                        // Remember Quoter
        DEC ECX                          // Quoter handled
        JZ Finish                        // No further characters left in SQL!

      QuotedL:
        LODSW                            // Get character from SQL
        CMP EDI,0                        // Store the string somewhere?
        JE QuotedL1                      // No!
        STOSW                            // Put character
      QuotedL1:

        CMP AX,'\'                       // Character = Escaper?
        JNE QuotedL2                     // No!
        DEC ECX                          // Escaper handled
        JZ Finish                        // No further characters left in SQL!
        LODSW
        CMP EDI,0                        // Store the string somewhere?
        JE QuotedLE                      // No!
        STOSW                            // Put character
        JMP QuotedLE

      QuotedL2:
        CMP AX,DX                        // End of Quoted?
        JNE QuotedLE                     // No!
        DEC ECX                          // Ignore Quoter
        JMP Finish

      QuotedLE:
        LOOP QuotedL

      Finish:
        CMP EBX,True
        POP EBX
        POP EAX
end;

procedure Trim();
// ESI: Pointer to SQL
// ECX: Characters left in SQL
// EDI: Pointer to Result
// EDX: Version for MySQL conditional code
// ESI will be moved to the next usable character inside SQL
// ECX will be decremened by the ignored characters
// ZF if no string copied
label
  StringL,
  EndOfStatement,
  EmptyCharacter,
  LineComment, LineCommentL,
  EnclosedComment,
  Version, VersionL, VersionE,
  EnclosedCommentL, EnclosedCommentLE, EnclosedCommentE,
  CommendEnd,
  Finish;
asm
        PUSH EBX
        PUSH EDI

        MOV EBX,False                    // No empty character!
        XOR EDI,EDI                      // Don't copy comment

        CMP ECX,0                        // End of SQL?
        JE Finish                        // Yes!

      StringL:
        CMP WORD PTR [ESI],9             // Current character inside SQL?
        JE EmptyCharacter                // Tabulator!
        CMP WORD PTR [ESI],10            // New Line?
        JE EmptyCharacter                // Yes!
        CMP WORD PTR [ESI],13            // Carrige Return?
        JE EmptyCharacter                // Yes!
        CMP WORD PTR [ESI],' '           // Space?
        JE EmptyCharacter                // Yes!
        CMP WORD PTR [ESI],'#'           // End of line comment ("#") in SQL?
        JE LineComment                   // Yes!
        CMP WORD PTR [ESI],';'           // End of Statement?
        JE EndOfStatement                // Yes!
        CMP ECX,2                        // Are there two characters left in SQL?
        JB Finish                        // No!
        CMP DWORD PTR [ESI],$002D002D    // End of line comment ("--") in SQL?
        JE LineComment                   // Yes!
        CMP DWORD PTR [ESI],$002A002F    // Start of "/*" comment in SQL?
        JE EnclosedComment               // Yes!
        TEST EDX,$80000000               // Are we inside cond. MySQL code?
        JZ Finish                        // No!
        CMP DWORD PTR [ESI],$002F002A    // End of "*/" comment in SQL?
        JE CommendEnd                    // Yes!
        JMP Finish

      // -------------------

      EndOfStatement:
        AND EDX,NOT $80000000            // We're outside cond. MySQL code!
        JMP Finish

      // -------------------

      EmptyCharacter:
        MOV EBX,True                     // Empty characters found!
        ADD ESI,2                        // Step over empty character
        DEC ECX                          // One character handled
        JNZ StringL                      // There are still character left in SQL!
        JMP Finish

      // -------------------

      LineComment:
        MOV EBX,True                     // Comment found!
      LineCommentL:
        CMP WORD PTR [ESI],10            // End of line found in SQL?
        JE StringL                       // Yes!
        CMP WORD PTR [ESI],13            // End of line found in SQL?
        JE StringL                       // Yes!
        ADD ESI,2                        // Step over comment character
        LOOP LineCommentL
        JMP Finish

      // -------------------

      EnclosedComment:
        MOV EBX,True                     // Comment found!
        ADD ESI,4                        // Step over "/*"
        SUB ECX,2                        // "/*" handled
        JZ Finish                        // End of SQL!

        CMP WORD PTR [ESI],'!'           // Conditional MySQL code?
        JNE EnclosedCommentL             // No!
        CMP EDX,0                        // MySQL version given?
        JE EnclosedCommentL              // No!

      Version:
        ADD ESI,2                        // Step over "!"
        DEC ECX                          // "!" handled
        JZ Finish                        // End of SQL!

        PUSH EAX
        PUSH EBX
        PUSH EDX
        MOV EAX,6                        // Max. 6 version digits
        MOV EBX,0                        // Version inside SQL
      VersionL:
        CMP WORD PTR [ESI],'0'           // Version digit?
        JB VersionE                      // No!
        CMP WORD PTR [ESI],'9'           // Version digit?
        JA VersionE                      // No!

        PUSH EAX
        PUSH EBX
        PUSH EDX
        MOV EAX,EBX
        MOV BX,10
        MUL BX                           // Shift version one digi left
        MOV EBX,EAX
        MOV EAX,0
        LODSW                            // Get version digit
        SUB AX,'0'                       // Convert digit numerical
        ADD EBX,EAX                      // Add digit to version
        POP EDX
        POP EBX
        POP EAX

        ADD ESI,2                        // One character handled in SQL
        DEC EAX                          // One version digit handled
        JZ VersionE                      // All version digits handled!
        LOOP VersionL

      VersionE:
        POP EDX
        CMP EBX,EDX                      // Use cond. MySQL code?
        POP EBX
        POP EAX
        JGE EnclosedCommentL             // No!

        OR EDX,$80000000                 // We're inside cond. MySQL code!
        JECXZ Finish                     // End of SQL!
        JMP StringL

      EnclosedCommentL:
        CMP ECX,2                        // Are there two characters left in SQL?
        JB EnclosedCommentLE             // No!
        CMP DWORD PTR [ESI],$002F002A    // "*/" in SQL?
        JE EnclosedCommentE              // Yes!
      EnclosedCommentLE:
        CALL MoveString                  // Quoted string?
        JZ EnclosedCommentL              // Yes!
        ADD ESI,2                        // Step over commenct character in SQL
        LOOP EnclosedCommentL            // There are more characters left in SQL!
        JMP Finish
      EnclosedCommentE:
        ADD ESI,4                        // Ignore "*/"
        SUB ECX,2                        // "*/" handled in SQL
        JNZ StringL                      // There are more characters left in SQL!
        JMP Finish

      // -------------------

      CommendEnd:
        TEST EDX,$80000000               // Are we inside cond. MySQL code?
        JZ Finish                        // No!
        MOV EBX,True                     // Empty characters found!
        AND EDX,NOT $80000000            // Now we're outside cond. MySQL code!
        ADD ESI,4                        // Step over "*/"
        SUB ECX,2                        // "*/" handled in SQL
        JNZ StringL                      // There are more characters left in SQL!
        JMP Finish

      // -------------------

      Finish:
        CMP EBX,True                     // Empty characters found?

        POP EDI
        POP EBX
end;

procedure CompareKeyword();
// EAX: Pointer to Keyword
// ECX: Characters left in SQL
// ESI: Pointer to SQL
// ECX will be decremened by a found keyword length
// ZF if keyword found
label
  CharactersL, Characters2, CharactersLE,
  KeywordSpace,
  KeywordTerminated, KeywordTerminatedL,
  KeywordNotFound,
  KeywordFound,
  Finish;
const
  Terminators: PChar = #9#10#13' ''"`.,=();'; // Characters terminating the identifier
asm
        PUSH EDX                         // Conditional Code Marker, changed in Trim
        PUSH EDI
        PUSH ECX
        PUSH ESI

        MOV EDI,EAX

      CharactersL:
        CMP WORD PTR [EDI],0             // End of Keyword?
        JE KeywordTerminated             // Yes!
        CMP WORD PTR [EDI],' '           // Space in Keyword?
        JNE Characters2                  // No!
        CALL Trim                        // Empty characters in SQL?
        JNE KeywordNotFound              // No!
        ADD EDI,2                        // Step over space in Keyword
        JECXZ KeywordNotFound            // End of SQL!
        JMP CharactersL

      Characters2:
        MOV DX,[ESI]                     // Compare character inside SQL
        AND DX,not $20                   //   (upcased)
        CMP DX,[EDI]                     //   with character in keyword
        JNE KeywordNotFound              // Not equal!
        ADD ESI,2                        // Step over equal character inside SQL

      CharactersLE:
        ADD EDI,2                        // Next character inside SQL
        LOOP CharactersL

        CMP WORD PTR [EDI],0             // End of Keyword?
        JE KeywordFound                  // Yes!
        JMP KeywordNotFound

      // -------------------

      KeywordTerminated:
        MOV DX,[ESI]                     // Character in SQL
        MOV EDI,[Terminators]            // Terminating characters
      KeywordTerminatedL:
        CMP WORD PTR [EDI],0             // All terminators checked?
        JE KeywordNotFound               // Yes!
        CMP DX,[EDI]                     // Charcter in SQL = Terminator?
        JE KeywordFound                  // Yes!
        ADD EDI,2                        // Next terminator
        JMP KeywordTerminatedL

      // -------------------

      KeywordNotFound:
        POP ESI                          // Restore ESI, since keyword not found
        POP ECX                          // Restore ECX, since keyword not found
        MOV EDX,EDI
        JMP Finish

      // -------------------

      KeywordFound:
        POP EDX                          // Restore Stack
        POP EDX                          // Restore Stack
        MOV EDX,ESI
        JMP Finish

      // -------------------

      Finish:
        CMP EDX,ESI                      // Keyword found?
        POP EDI
        POP EDX
        RET
end;

procedure UnescapeString();
// ECX: Length of quoted SQL
// ESI: Pointer to quoted SQL
// EDI: Pointer to unquoted Result
// ESI will be moved to the next usable character inside SQL
// ECX will be decremened of the quoted string length
label
  Start, StringL, String1, String2, String3, String4, String5, StringLE,
  Hex, Hex1S, Hex1E, Hex2, Hex2C, Hex2S, HexE,
  Finish;
const
  HexDigits: PChar = 'FEDCBA9876543210';
asm
        PUSH EAX
        PUSH EBX
        PUSH EDX

        MOV DX,[ESI]                     // Quoter

      // -------------------

      Start:
        ADD ESI,2                        // Step over starting Quoter
        DEC ECX                          // Ignore starting Quoter
        JZ Finish                        // End of SQL!
      StringL:
        LODSW                            // Load character from ESI
        CMP AX,DX                        // End of quoted string?
        JNE String1                      // No!
        DEC ECX                          // Ignore Quoter
        JZ Finish                        // End of SQL!
        CMP [ESI],DX                     // Restart of quoted string?
        JE Start                         // Yes!
        JMP Finish
      String1:
        CMP AX,'\'                       // Escaped character?
        JNE StringLE                     // No!
        LODSW                            // Load next character from ESI
        DEC ECX                          // Ignore Escaper
        CMP AX,'0'                       // '\0'?
        JNE String2                      // No!
        MOV AX,0                         // replace with #0
        JMP StringLE
      String2:
        CMP AX,'t'                       // '\t'?
        JNE String3                      // No!
        MOV AX,9                         // replace with Tabulator
        JMP StringLE
      String3:
        CMP AX,'n'                       // '\n'?
        JNE String4                      // No!
        MOV AX,10                        // replace with LineFeed
        JMP StringLE
      String4:
        CMP AX,'r'                       // '\r'?
        JNE String5                      // No!
        MOV AX,13                        // replace with CarriageReturn
        JMP StringLE
      String5:
        CMP AX,'x'                       // '\x'?
        JNE StringLE                     // No!
        CMP ECX,3                        // Are there three character left in SQL?
        JB StringLE                      // No!
        JMP Hex
      StringLE:
        STOSW                            // Store character in EDI
        LOOP StringL
        JMP Finish

      // -------------------

      Hex:
        MOV AX,[ESI]                     // Get high digit
        CMP AX,'A'                       // character digit?
        JB Hex1S                         // No!
        AND AX,not $20                   // Upcase digit
      Hex1S:
        PUSH ECX
        PUSH EDI
        MOV EDI,Pointer(HexDigits)
        MOV ECX,16                       // Length(HexDigits)
        REPNE SCASW                      // Scan Digit
        MOV EBX,ECX
        POP EDI
        POP ECX
        JE Hex2                          // HexDigit!
        MOV AX,'x'                       // Restore hex initiating character
        JMP StringLE
      Hex2:
        MOV AX,[ESI + 2]                 // Get low digit
        CMP AX,'A'                       // character digit?
        JB Hex2S                         // No!
        AND AX,not $20                   // Upcase digit
      Hex2S:
        PUSH ECX
        PUSH EDI
        MOV EDI,Pointer(HexDigits)
        MOV ECX,16                       // Length(HexDigits)
        REPNE SCASW                      // Find Digit
        MOV EAX,ECX
        POP EDI
        POP ECX
        JE HexE                          // HexDigit!
        MOV AX,'x'                       // Restore hex initiating character
        JMP StringLE
      HexE:
        SHL BX,4
        ADD AX,BX
        ADD ESI,4                        // Step over hex digits
        SUB ECX,2                        // Ignore hex digits
        JMP StringLE

      // -------------------

      Finish:
        POP EDX
        POP EBX
        POP EAX
end;

{******************************************************************************}

function BitStringToInt(const BitString: string): UInt64;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(BitString) do
    if (BitString[I] = '1') then
      Result := Result + 1 shl (Length(BitString) - I)
    else if (BitString[I] <> '0') then
      raise EConvertError.CreateFmt(SConvStrParseError, [BitString]);
end;

function IntToBitString(const Value: UInt64; const Width: Integer = 1): string;
var
  I: Integer;
begin
  Result := '';
  for I := SizeOf(Value) * 8 - 1 downto 0 do
    if ((Value shr I and 1) = 0) then
      Result := Result + '0'
    else
      Result := Result + '1';
  while ((Result[1] = '0') and (Length(Result) > Width)) do Delete(Result, 1, 1);
end;

function SQLCreateParse(out Handle: TSQLParse; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
begin
  Result := True;
  if (Result) then
  begin
    Handle.Pos := SQL;
    Handle.Len := Len;
    Handle.EDX := Version;
    Handle.Start := Handle.Pos;
  end;
end;

function SQLEscape(const Value: string; const ODBCEncoding: Boolean): string;
const
  HexDigits: PChar = '0123456789ABCDEF';
label
  BinL;
var
  Len: Integer;
begin
  Len := Length(Value);

  if (Len = 0) then
    Result := ''''''
  else if (ODBCEncoding) then
  begin // ODBC notation
    SetLength(Result, 2 * Len + 2);
    Result[1] := '0';
    Result[2] := 'x';
  end
  else
  begin // Ansi SQL notation
    SetLength(Result, 2 * Len + 3);
    Result[1] := 'X';
    Result[2] := '''';
    Result[2 * Len + 3] := '''';
  end;

  if (Len > 0) then
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV EBX,Pointer(HexDigits)       // Hex digits

        MOV ESI,Pointer(Value)           // Read bytes from Value
        MOV EAX,Result                   // Store into Result
        MOV EDI,[EAX]
        ADD EDI,4                        // Step over "X'"

        MOV ECX,Len                      // Count of bytes

      // -------------------

      BinL:
        MOV EAX,0                        // Clear EAX since AX will be loaded, but be EAX used
        LODSW                            // Read byte
        AND AX,$00FF                     // Interpret value as binary
        PUSH EAX
        SHR AL,4                         // Use high octet
        MOV AX,[EBX + EAX * 2]           // Get hex character
        STOSW                            // Store character
        POP EAX
        AND AL,$F                        // Use low octet
        MOV AX,[EBX + EAX * 2]           // Get hex character
        STOSW                            // Store character

        LOOP BinL                        // Next Bin byte

      // -------------------

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;
end;

function SQLEscape(const Value: string; const Quoter: Char = ''''): string;
label
  StartL,
  StringL, String2,
  PositionL, PositionE,
  FindPos, FindPos2,
  Finish;
const
  SearchLen = 7;
  Search: array [0 .. SearchLen - 1] of Char = (#0, #9, #10, #13, '''', '"', '\');
  Replace: array [0 .. 2 * SearchLen - 1] of Char = ('\','0', '\','t', '\','n', '\','r', '\','''', '\','"', '\','\');
var
  Len: Integer;
  Positions: packed array [0 .. SearchLen - 1] of Cardinal;
begin
  Len := Length(Value);

  if (Len = 0) then
    Result := StringOfChar(Quoter, 2)
  else
  begin
    SetLength(Result, 2 * Len + 2); // reserve space

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
        MOV ECX,Len                      // Length of Value string

        MOV AX,Quoter                    // Start quoting
        STOSW

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
        JZ String2                       // End of Value!
        REPNE MOVSW                      // Copy normal characters to Result

      String2:
        MOV ECX,EAX
        JECXZ Finish                     // End of Value!

        ADD ESI,2                        // Step of Search character
        LEA EDX,Replace                  // Insert Replace characters
        MOV EAX,[EDX + EBX * 4]
        STOSD

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
        MOV AX,Quoter                    // End quoting
        STOSW

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

function SQLEscape(const Value: PAnsiChar; const Length: Integer; const ODBCEncoding: Boolean): string;
const
  HexDigits: PChar = '0123456789ABCDEF';
label
  BinL;
begin
  if (Length = 0) then
    Result := ''''''
  else if (ODBCEncoding) then
  begin // ODBC notation
    SetLength(Result, 2 * Length + 2);
    Result[1] := '0';
    Result[2] := 'x';
  end
  else
  begin // Ansi SQL notation
    SetLength(Result, 2 * Length + 3);
    Result[1] := 'X';
    Result[2] := '''';
    Result[2 * Length + 3] := '''';
  end;

  if (Length > 0) then
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV EBX,Pointer(HexDigits)       // Hex digits

        MOV ESI,Value                    // Read bytes from Value
        MOV EAX,Result                   // Store into Result
        MOV EDI,[EAX]
        ADD EDI,4                        // Step over "X'"
        MOV ECX,Length

      // -------------------

      BinL:
        MOV EAX,0                        // Clear EAX since AL will be loaded, but be EAX used
        LODSB                            // Read byte
        PUSH EAX
        SHR AL,4                         // Use high octet
        MOV AX,[EBX + EAX * 2]           // Get hex character
        STOSW                            // Store character
        POP EAX
        AND AL,$F                        // Use low octet
        MOV AX,[EBX + EAX * 2]           // Get hex character
        STOSW                            // Store character

        LOOP BinL                        // Next Bin byte

      // -------------------

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;
end;

function SQLParseCallStmt(const SQL: PChar; const Len: Integer; out ProcedureName: string; const Version: Integer): Boolean;
label
  Priority, Ignore, Into2,
  Found,
  Finish;
var
  Index: Integer;
  Parse: TSQLParse;
begin
  asm
      PUSH ES
      PUSH ESI
      PUSH EDI
      PUSH EBX

      PUSH DS                          // string operations uses ES
      POP ES

      MOV ESI,PChar(SQL)               // Scan characters from SQL
      MOV ECX,Len

    // -------------------

      MOV BYTE PTR [Result],False
      MOV EDI,0                        // Don't copy inside MoveString

      CALL Trim                        // Step over empty characters
      JECXZ Finish                     // End of SQL!

      MOV EAX,[KCall]
      CALL CompareKeyword              // 'CALL'?
      JNE Finish                       // No!
      CALL Trim                        // Step over empty characters
      JECXZ Finish                     // End of SQL!
      JMP Found

    // -------------------

    Found:
      MOV BYTE PTR [Result],True       // SQL is CALL!

    // -------------------

    Finish:
      MOV ECX,ESI
      SUB ECX,Pointer(SQL)
      SHR ECX,1                        // 2 Bytes = 1 character
      MOV Index,ECX

      POP EBX
      POP EDI
      POP ESI
      POP ES
  end;

  if (Result and SQLCreateParse(Parse, PChar(@SQL[Index]), Len - Index, Version)) then
    ProcedureName := SQLParseValue(Parse);
end;

function SQLParseChar(var Handle: TSQLParse; const Character: Char; const IncrementIndex: Boolean = True): Boolean;
label
  Finish;
begin
    asm
        PUSH ESI
        PUSH EBX

        MOV EBX,Handle
        MOV ESI,[EBX + 0]                // Position in SQL
        MOV ECX,[EBX + 4]                // Characters left in SQL
        MOV EDX,[EBX + 8]                // MySQL version

        MOV BYTE PTR [Result],False      // Character not found!

        JECXZ Finish                     // End of SQL!

        MOV AX,[ESI]
        CMP AX,Character                 // Character in SQL?
        JNE Finish                       // No!

        MOV BYTE PTR [Result],True       // Character found!

        CMP IncrementIndex,False         // Increment Index?
        JE Finish                        // No!

        ADD ESI,2                        // Step over Char
        DEC ECX                          // IgnoreCharacter
        CALL Trim                        // Step over empty characters

        MOV [EBX + 0],ESI                // New Position in SQL
        MOV [EBX + 4],ECX                // Characters left in SQL
        MOV [EBX + 8],EDX                // MySQL version

      Finish:
        POP EBX
        POP ESI
    end;
end;

function SQLParseCLStmt(out CLStmt: TSQLCLStmt; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
label
  Commands, SetNames, SetCharacterSet, Use,
  Found, FoundL, FoundE,
  Finish;
var
  Index: Integer;
  Parse: TSQLParse;
begin
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV BYTE PTR [Result],False

        MOV ESI,PChar(SQL)               // Scan characters from SQL

        MOV ECX,Len                      // Length
        JECXZ Finish                     // Empty SQL!

        MOV EDX,Version

      // -------------------

        MOV EBX,CLStmt

      Commands:
        CALL Trim                        // Empty characters?

      SetNames:
        MOV EAX,[KSetNames]
        CALL CompareKeyword              // 'SET NAMES'?
        JNE SetCharacterSet              // No!
        MOV BYTE PTR [EBX + 0],ctSetNames
        JMP Found

      SetCharacterSet:
        MOV EAX,[KSetCharacterSet]
        CALL CompareKeyword              // 'SET CHARACTER SET'?
        JNE Use                          // No!
        MOV BYTE PTR [EBX + 0],ctSetCharacterSet
        JMP Found

      Use:
        MOV EAX,[KUse]
        CALL CompareKeyword              // 'USE'?
        JNE Finish                       // No!
        MOV BYTE PTR [EBX + 0],ctUse

      Found:
        CALL Trim                        // Empty characters?

      FoundE:
        MOV BYTE PTR [Result],True

        MOV ECX,ESI                      // Calculate ObjectName position
        SUB ECX,Pointer(SQL)
        SHR ECX,1                        // 2 Bytes = 1 character
        MOV Index,ECX

      // -------------------

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

  if (not Result or not SQLCreateParse(Parse, PChar(@SQL[Index]), Len - Index, Version)) then
    CLStmt.ObjectName := ''
  else
    CLStmt.ObjectName := SQLParseValue(Parse);
end;

function SQLParseDDLStmt(out DDLStmt: TSQLDDLStmt; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
label
  Alter, Create, Drop, Rename,
  Algorithm, AlgorithmL,
  Definer,
  ObjType,
  Definer1, Definer2, Definer3,
  ODatabase, OEvent, OFunction, OProcedure, OTable, OTrigger, OView,
  Name,
  Found,
  RenameL, RenameLE, RenameC, RenameE,
  Finish;
var
  Index: Integer;
  IndexNewObjectName: Integer;
  Parse: TSQLParse;
begin
  asm
      PUSH ES
      PUSH ESI
      PUSH EDI
      PUSH EBX

      PUSH DS                          // string operations uses ES
      POP ES

      MOV ESI,SQL                      // Scan characters from SQL
      MOV ECX,Len

      MOV EBX,DDLStmt
      MOV EDX,Version

    // -------------------

      MOV BYTE PTR [Result],False
      MOV EDI,0                        // Don't copy inside MoveString

      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!

      MOV EAX,[KAlter]
      CALL CompareKeyword              // 'ALTER'?
      JNE Create                       // No!
      MOV BYTE PTR [EBX + 0],dtAlter
      JMP ObjType
    Create:
      MOV EAX,[KCreate]
      CALL CompareKeyword              // 'CREATE'?
      JNE Drop                         // No!
      MOV BYTE PTR [EBX + 0],dtCreate
      MOV EAX,[KOrReplace]
      CALL CompareKeyword              // Set over 'OR REPLACE'?
      JNE Algorithm
      MOV BYTE PTR [EBX + 0],dtAlter
      JMP Algorithm
    Drop:
      MOV EAX,[KDrop]
      CALL CompareKeyword              // 'DROP'?
      JNE Rename                       // No!
      MOV BYTE PTR [EBX + 0],dtDrop
      JMP ObjType
    Rename:
      MOV EAX,[KRename]
      CALL CompareKeyword              // 'RENAME'?
      JNE Finish                       // No!
      MOV BYTE PTR [EBX + 0],dtRename
      JMP ObjType

    // -------------------

    Algorithm:
      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV EAX,[KAlgorithm]
      CALL CompareKeyword              // 'ALGORITHM'?
      JNE Definer                      // No!

      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      CMP WORD PTR [ESI],'='           // '=' ?
      JNE Finish                       // No!

      ADD ESI,2                        // Next character
      DEC ECX                          // '=' handled
    AlgorithmL:
      CALL Trim                        // Empty character?
      JZ Definer                       // Yes!
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      ADD ESI,2                        // Next character in SQL
      DEC ECX                          // character handled
      JMP AlgorithmL

    // -------------------

    Definer:
      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!

      MOV EAX,[KDefiner]
      CALL CompareKeyword              // 'DEFINER'?
      JNE ObjType                      // No!

      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      CMP WORD PTR [ESI],'='           // '=' ?
      JNE Finish                       // No!
      ADD ESI,2                        // Next character
      DEC ECX                          // One character ('=') handled
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      CALL MoveString                  // Quoted identifier?
      JE Definer2                      // Yes!
    Definer1:
      CALL Trim                        // Empty character found?
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      JE ObjType                       // Yes!
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      CMP WORD PTR [ESI],'@'           // '@'?
      JE Definer2                      // Yes!
      CMP WORD PTR [ESI],''''          // Start quotation in SQL?
      JE ObjType                       // Yes!
      CMP WORD PTR [ESI],'"'           // Start quotation in SQL?
      JE ObjType                       // Yes!
      CMP WORD PTR [ESI],'`'           // Start quotation in SQL?
      JE ObjType                       // Yes!
      ADD ESI,2                        // Next character in SQL
      JMP Definer1
    Definer2:
      ADD ESI,2                        // Step over '@'
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      CALL MoveString                  // Quoted identifier?
      JE ObjType                       // Yes!
    Definer3:
      CALL Trim                        // Empty character?
      JE ObjType                       // Yes!
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      ADD ESI,2                        // Next character in SQL
      JMP Definer3

    ObjType:
      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV EAX,[KSQLSecurityDefiner]    // Step over
      CALL CompareKeyword              //   'SQL SECURITY DEFINER'
      MOV EAX,[KSQLSecurityInvoker]    // Step over
      CALL CompareKeyword              //   'SQL SECURITY INVOKER'
      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!

    ODatabase:
      MOV EAX,[KDatabase]
      CALL CompareKeyword              // 'DATABASE'?
      JNE OEvent                       // No!
      MOV BYTE PTR [EBX + 1],otDatabase
      JMP Found
    OEvent:
      MOV EAX,[KEvent]
      CALL CompareKeyword              // 'EVENT'?
      JNE OFunction                    // No!
      MOV BYTE PTR [EBX + 1],otEvent
      JMP Found
    OFunction:
      MOV EAX,[KFunction]
      CALL CompareKeyword              // 'FUNCTION'?
      JNE OProcedure                   // No!
      MOV BYTE PTR [EBX + 1],otFunction
      JMP Found
    OProcedure:
      MOV EAX,[KProcedure]
      CALL CompareKeyword              // 'PROCEDURE'?
      JNE OTable                       // No!
      MOV BYTE PTR [EBX + 1],otProcedure
      JMP Found
    OTable:
      MOV EAX,[KTable]
      CALL CompareKeyword              // 'TABLE'?
      JNE OTrigger                     // No!
      MOV BYTE PTR [EBX + 1],otTable
      JMP Found
    OTrigger:
      MOV EAX,[KTrigger]
      CALL CompareKeyword              // 'TRIGGER'?
      JNE OView                        // No!
      MOV BYTE PTR [EBX + 1],otTrigger
      JMP Found
    OView:
      MOV EAX,[KView]
      CALL CompareKeyword              // 'VIEW'?
      JNE Finish                       // No!
      MOV BYTE PTR [EBX + 1],otView
      JMP Found

    // -------------------

    Found:
      MOV BYTE PTR [Result],True       // SQL is DDL!

      CALL Trim                        // Step over empty characters
      JECXZ Finish                     // End of SQL!

      CMP BYTE PTR [EBX + 0],dtAlter   // Alter statement?
      JNE Finish                       // No!
      CMP BYTE PTR [EBX + 1],otTable   // Table object?
      JNE Finish                       // No!

      PUSH ESI

      MOV EAX,[KRename]
      MOV EDI,0                        // Don't copy inside MoveString
    RenameL:
      CMP WORD PTR [ESI],';'           // End of statement?
      JE RenameE                       // Yes!
      CALL Trim                        // Empty characters in SQL?
      JE RenameL                       // Yes!
      JECXZ Finish                     // End of SQL!
      CALL MoveString                  // Quoted string?
      JE RenameL                       // Yes!
      CALL CompareKeyword              // 'RENAME'?
      JNE RenameC                      // No!
      MOV BYTE PTR [EBX + 0],dtAlterRename
      PUSH ECX
      MOV ECX,ESI
      SUB ECX,Pointer(SQL)
      SHR ECX,1                        // 2 Bytes = 1 character
      MOV IndexNewObjectName,ECX
      POP ECX
      JMP RenameE
    RenameC:
      ADD ESI,2
    RenameLE:
      LOOP RenameL

    RenameE:
      POP ESI
      JMP Finish

    // -------------------

    Finish:
      MOV ECX,ESI
      SUB ECX,Pointer(SQL)
      SHR ECX,1                        // 2 Bytes = 1 character
      MOV Index,ECX

      POP EBX
      POP EDI
      POP ESI
      POP ES
  end;

  if (Result and SQLCreateParse(Parse, PChar(@SQL[Index]), Len - Index, Version)) then
  begin
    if (DDLStmt.DefinitionType = dtDrop) then SQLParseKeyword(Parse, 'IF EXISTS');
    DDLStmt.DatabaseName := '';
    if (DDLStmt.ObjectType = otDatabase) then
      DDLStmt.ObjectName := SQLParseValue(Parse)
    else
      Result := SQLParseObjectName(Parse, DDLStmt.DatabaseName, DDLStmt.ObjectName);
  end;
  if (Result and (DDLStmt.DefinitionType = dtAlterRename) and SQLCreateParse(Parse, PChar(@SQL[IndexNewObjectName]), Len - IndexNewObjectName, Version)) then
  begin
    SQLParseKeyword(Parse, 'TO');
    DDLStmt.NewDatabaseName := '';
    Result := SQLParseObjectName(Parse, DDLStmt.NewDatabaseName, DDLStmt.NewObjectName);
  end;
end;

function SQLParseDMLStmt(out DMLStmt: TSQLDMLStmt; const SQL: PChar; const Len: Integer; const Version: Integer): Boolean;
label
  Insert, Update, Delete,
  Priority, Ignore, Into2,
  Found,
  From,
  Finish;
var
  Index: Integer;
  Parse: TSQLParse;
  TableName: string;
begin
  asm
      PUSH ES
      PUSH ESI
      PUSH EDI
      PUSH EBX

      PUSH DS                          // string operations uses ES
      POP ES

      MOV ESI,PChar(SQL)               // Scan characters from SQL
      MOV ECX,Len

      MOV EBX,DMLStmt

    // -------------------

      MOV BYTE PTR [Result],False
      MOV EDI,0                        // Don't copy inside MoveString

      CALL Trim                        // Step over empty characters
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!

    Insert:
      MOV EAX,[KInsert]
      CALL CompareKeyword              // 'INSERT'?
      JNE Update                       // No!
      MOV BYTE PTR [EBX + 0],mtInsert
      JMP Found
    Update:
      MOV EAX,[KUpdate]
      CALL CompareKeyword              // 'UPDATE'?
      JNE Delete                       // No!
      MOV BYTE PTR [EBX + 0],mtUpdate
      JMP Found
    Delete:
      MOV EAX,[KDelete]
      CALL CompareKeyword              // 'DELETE'?
      JNE Finish                       // No!
      MOV BYTE PTR [EBX + 0],mtDelete
      JMP Found

    // -------------------

    Found:
      CALL Trim                        // Step over empty characters
      JECXZ Finish                     // End of SQL!

      MOV EAX,[KLowPriority]
      CALL CompareKeyword              // 'LOW_PRIORITY'?
      JE Ignore                        // No!
      MOV EAX,[KQuick]
      CALL CompareKeyword              // 'QUICK'?
      JE Ignore                        // No!
      MOV EAX,[KDelayed]
      CALL CompareKeyword              // 'DELAYED'?
      JE Ignore                        // No!
      MOV EAX,[KHighPriority]
      CALL CompareKeyword              // 'HIGH_PRIORITY'?
      JE Ignore                        // No!

    Ignore:
      CALL Trim                        // Step over empty characters
      JECXZ Finish                     // End of SQL!

      MOV EAX,[KIgnore]
      CALL CompareKeyword              // 'IGNORE'?

      CALL Trim                        // Step over empty characters
      JECXZ Finish                     // End of SQL!

      CMP BYTE PTR [EBX + 0],mtDelete  // DELETE?
      JE From

      MOV EAX,[KInto]
      CALL CompareKeyword              // 'INTO'?
      JNE Finish                       // No!
      MOV BYTE PTR [Result],True       // SQL is DML!
      CALL Trim                        // Step over empty characters
      JMP Finish

    From:
      MOV EAX,[KFrom]
      CALL CompareKeyword              // 'FROM'?
      JNE Finish                       // No!
      MOV BYTE PTR [Result],True       // SQL is DML!
      CALL Trim                        // Step over empty characters

    // -------------------

    Finish:
      MOV ECX,ESI
      SUB ECX,Pointer(SQL)
      SHR ECX,1                        // 2 Bytes = 1 character
      MOV Index,ECX

      POP EBX
      POP EDI
      POP ESI
      POP ES
  end;

  if (Result and SQLCreateParse(Parse, PChar(@SQL[Index]), Len - Index, Version)) then
    repeat
      TableName := SQLParseValue(Parse);
      if (TableName <> '') then
      begin
        SetLength(DMLStmt.DatabaseNames, System.Length(DMLStmt.DatabaseNames) + 1);
        SetLength(DMLStmt.TableNames, Length(DMLStmt.TableNames) + 1);
        DMLStmt.TableNames[Length(DMLStmt.TableNames) - 1] := TableName;

        if (not SQLParseChar(Parse, '.')) then
          DMLStmt.DatabaseNames[Length(DMLStmt.DatabaseNames) - 1] := ''
        else
        begin
          DMLStmt.DatabaseNames[Length(DMLStmt.DatabaseNames) - 1] := TableName;
          DMLStmt.TableNames[Length(DMLStmt.TableNames) - 1] := SQLParseValue(Parse);
        end;
      end;
    until (not SQLParseChar(Parse, ','));
end;

function SQLParseEnd(const Handle: TSQLParse): Boolean;
begin
  Result := Handle.Len = 0;
end;

function SQLParseGetIndex(const Handle: TSQLParse): Integer;
begin
  Result := 1 + Handle.Pos - Handle.Start;
end;

function SQLParseKeyword(var Handle: TSQLParse; const Keyword: PChar; const IncrementIndex: Boolean = True): Boolean;
label
  Finish;
begin
    asm
        PUSH ESI
        PUSH EBX

        MOV EBX,Handle
        MOV ESI,[EBX + 0]                // Position in SQL
        MOV ECX,[EBX + 4]                // Characters left in SQL
        MOV EDX,[EBX + 8]                // MySQL version

        MOV BYTE PTR [Result],False      // Keyword not found!

        MOV EAX,PChar(Keyword)
        CALL CompareKeyword              // Keyword?
        JNE Finish                       // No!

        MOV BYTE PTR [Result],True       // Keyword found!

        CMP IncrementIndex,False         // Increment Index?
        JE Finish                        // No!

        CALL Trim                        // Step over empty characters

        MOV [EBX + 0],ESI                // Position in SQL
        MOV [EBX + 4],ECX                // Characters left in SQL
        MOV [EBX + 8],EDX                // MySQL version

      Finish:
        POP EBX
        POP ESI
    end;
end;

function SQLParseObjectName(var Handle: TSQLParse; var DatabaseName: string; out ObjectName: string): Boolean;
begin
  ObjectName := SQLParseValue(Handle);
  if (SQLParseChar(Handle, '.')) then
  begin
    DatabaseName := ObjectName;
    ObjectName := SQLParseValue(Handle);
  end;
  Result := (ObjectName <> '');
end;

function SQLParseValue(var Handle: TSQLParse; const TrimAfterValue: Boolean = True): string;
label
  StringL,
  Quoted,
  Unquoted, UnquotedL, Unquoted1, Unquoted2, UnquotedTerminatorsL, UnquotedC, UnquotedLE,
  Finish, FinishE;
const
  Terminators: PChar = #9#10#13' ''"`.,=;'; // Characters, terminating the value
var
  BracketDeep: Integer;
  Len: Integer;
begin
  SetLength(Result, Handle.Len);

  if (Handle.Len > 0) then
  begin
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV EBX,Handle
        MOV ESI,[EBX + 0]                // Position in SQL
        MOV ECX,[EBX + 4]                // Characters left in SQL
        MOV EDX,[EBX + 8]                // MySQL Version
        MOV EAX,Result                   // Copy characters to Result
        MOV EDI,[EAX]

      // -------------------

        MOV BracketDeep,0                // Bracket deep
      StringL:
        MOV AX,[ESI]                     // First character
        CMP AX,''''                      // Start quotation in SQL?
        JE Quoted                        // Yes!
        CMP AX,'"'                       // Start quotation in SQL?
        JE Quoted                        // Yes!
        CMP AX,'`'                       // Start quotation in SQL?
        JE Quoted                        // Yes!

      Unquoted:
      UnquotedL:
        MOV AX,[ESI]                     // Character in SQL
        CMP AX,';'                       // End of SQL statement?
        JE Finish                        // Yes!

        CALL MoveString                  // If quoted string: Copy it
        JE UnquotedLE                    // Quoted string!

        CMP AX,'('                       // Start brackets?
        JNE Unquoted1                    // No!
        INC BracketDeep                  // Open bracket
        CMP ECX,[EBX + 4]                // First character?
        JE UnquotedC                     // Yes!
        CMP BracketDeep,1                // First bracket?
        JE Finish                        // Yes!
        JMP UnquotedC

      Unquoted1:
        CMP AX,')'                       // End brackets?
        JNE Unquoted2                    // No!
        CMP BracketDeep,0                // Are we inside an open brackt?
        JE Finish                        // No!
        DEC BracketDeep                  // Close bracket
        JNZ UnquotedC                    // We're still in open brackets!
        MOVSW                            // Copy closing bracket to Result
        JMP Finish

      Unquoted2:
        MOV EBX,Handle
        CMP ECX,[EBX + 4]                // First character?
        JE UnquotedC                     // Yes!

        MOV EBX,[Terminators]            // Terminating characters
      UnquotedTerminatorsL:
        CMP WORD PTR [EBX],0             // All terminators checked?
        JE UnquotedC                     // Yes!
        CMP AX,[EBX]                     // Charcter in SQL = Terminator?
        JE Finish                        // Yes!
        ADD EBX,2                        // Next character in Terminators
        JMP UnquotedTerminatorsL

      UnquotedC:
        MOVSW                            // Copy character from SQL to Result
      UnquotedLE:
        LOOP UnquotedL
        JMP Finish

      // -------------------

      Quoted:
        CALL UnescapeString              // Unquote and unescape string
        JECXZ Finish                     // End of SQL!
        CMP WORD PTR [ESI],'@'           // '@' in SQL?
        JNE Finish                       // No!
        MOVSW                            // Copy '@' from SQL to Result
        DEC ECX                          // '@' handled
        JMP StringL

      // -------------------

      Finish:
        MOV EAX,Result                   // Calculate new length of Result
        MOV EAX,[EAX]
        SUB EDI,EAX
        SHR EDI,1                        // 2 Bytes = 1 character
        MOV Len,EDI

        CMP TrimAfterValue,False
        JE FinishE
        CALL Trim                        // Step over emtpy characters

      FinishE:
        MOV EBX,Handle
        MOV [EBX + 0],ESI                // Position in SQL
        MOV [EBX + 4],ECX                // Characters left in SQL
        MOV [EBX + 8],EDX                // MySQL Version

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    if (Len <> Length(Result)) then
      SetLength(Result, Len);
  end;
end;

function SQLSingleStmt(const SQL: string): Boolean;
var
  CompleteStmt: Boolean;
  Len: Integer;
begin
  Len := SQLStmtLength(SQL, 1, @CompleteStmt);
  Result := (0 < Len) and (not CompleteStmt or (SQLStmtLength(SQL, Len + 1) = 0));
end;

procedure SQLSplitValues(const Text: string; out Values: TSQLStrings);
label
  Start,
  UnquotedL,
  Quoted, QuotedL, Quoted2, QuotedLE,
  Finish, FinishL, Finish2, Finish3, Finish4, Finish5, FinishE;
var
  EOF: Boolean;
  EOL: Boolean;
  Len: Integer;
  SQL: PChar;
  Value: Integer;
  ValueData: PChar;
  ValueLength: Integer;
begin
  if (Text = '') then
    SetLength(Values, 0)
  else
  begin
    Value := 0;
    SQL := PChar(Text);
    repeat
      if (Value >= Length(Values)) then
        SetLength(Values, 2 * Value + 1);

      Len := Length(Text);
      asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        MOV ESI,SQL                     // Get character from Text

        MOV ECX,Len
        CMP ECX,0                        // Are there characters left in SQL?
        JNE Start                        // Yes!
        MOV EOL,True
        MOV EOF,True
        JMP Finish

      // -------------------

      Start:
        MOV ValueData,ESI                // Start of value
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,''''                      // Character in Text = Quoter?
        JE Quoted                        // Yes!

      // -------------------

      UnquotedL:
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,','                       // Character = Delimiter?
        JE Finish                        // Yes!
        CMP AX,13                        // Character = CarrigeReturn?
        JE Finish                        // Yes!
        CMP AX,10                        // Character = NewLine?
        JE Finish                        // Yes!
        ADD ESI,2                        // Next character!
        LOOP UnquotedL
        JMP Finish

      // -------------------

      Quoted:
        ADD ESI,2                        // Step over starting Quoter
        DEC ECX                          // Starting Quoter handled
        JZ Finish                        // End of Text!
      QuotedL:
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,''''                      // Character = Quoter?
        JNE Quoted2                      // No!
        ADD ESI,2                        // Step over ending Quoter
        DEC ECX                          // Ending Quoter handled
        JMP Finish
      Quoted2:
        CMP AX,'\'                       // Character = Escaper?
        JNE QuotedLE                     // No!
        ADD ESI,2                        // Step over Escaper
        DEC ECX                          // Escaper handled
        JZ Finish                        // End of Text!
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,''''                      // Character = Quoter?
        JNE QuotedLE                     // No!
        ADD ESI,2                        // Step over ending Quoter
        DEC ECX                          // Ending Quoter handled
        JMP Finish                       // End of Text!
      QuotedLE:
        ADD ESI,2                        // Next character!
        LOOP QuotedL

      // -------------------

      Finish:
        MOV EAX,Len                      // Calculate length of value
        SUB EAX,ECX
        MOV ValueLength,EAX

      FinishL:
        CMP ECX,0                        // Are there characters left in SQL?
        JNE Finish2                      // Yes!
        MOV EOL,True
        MOV EOF,True
        JMP FinishE
      Finish2:
        MOV EOF,False
        MOV AX,[ESI]                     // Get character from Text
        CMP AX,','                       // Delimiter?
        JNE Finish3
        ADD ESI,2                        // Step over Delimiter
        DEC ECX                          // Ignore Delimiter
        MOV EOL,False
        JMP FinishE
      Finish3:
        MOV EOL,True
        CMP ECX,2                        // Are there two characters left in SQL?
        JB Finish4                       // No!
        MOV EAX,[ESI]
        CMP EAX,$000A000D                // CarriageReturn + LineFeed?
        JNE Finish4
        ADD ESI,4                        // Step over CarriageReturn + LineFeed
        SUB ECX,2                        // Ignore CarriageReturn + LineFeed
        JMP FinishE
      Finish4:
        CMP AX,13                        // CarriageReturn?
        JE Finish5
        CMP AX,10                        // LineFeed?
        JE Finish5
        ADD ESI,2                        // Step over unknow character
        DEC ECX                          // Ignore unknow character
        JMP FinishL
      Finish5:
        ADD ESI,2                        // Step over CarriageReturn / LineFeed
        DEC ECX                          // Ignore CarriageReturn / LineFeed
        JMP FinishE

      FinishE:
        MOV SQL,ESI

        POP EBX
        POP EDI
        POP ESI
        POP ES
      end;

      SetString(Values[Value], ValueData, ValueLength);
      Inc(Value);
    until (EOL or EOF);

    if (Value <> Length(Values)) then
      SetLength(Values, Value);
  end;
end;

function SQLStmtLength(const SQL: string; const Index: Integer = 1; const CompleteStmt: PBoolean = nil): Integer;
label
  StringL, String1, String2, String3, StringC, StringLE, StringEOS,
  Complete, Complete2, Complete3, Complete4,
  UncompleteStmt,
  Finish;
var
  BeforeTrim: PChar;
  CompoundDeep: Cardinal;
  Len: Integer;
begin
  if ((Index <= 0) or (Length(SQL) < Index)) then
  begin
    Result := 0;
    if (Assigned(CompleteStmt)) then
      CompleteStmt^ := False;
  end
  else
  begin
    Len := Length(SQL) - Index + 1;
    asm
      PUSH ESI
      PUSH EDI
      PUSH EBX

      MOV ESI,PChar(SQL)               // ESI := @SQL[Index]
      ADD ESI,Index                    // Add Index twice
      ADD ESI,Index                    //   since character = 2 byte
      SUB ESI,2                        // Index starts with 1 in string

      MOV ECX,Len

      PUSH ESI

      MOV EDI,0                        // Don't copy inside MoveString
      MOV EDX,$7FFFFFFF                // Enclose all condional MySQL code
      MOV CompoundDeep,0               // Compound deep

      CMP CompleteStmt,0               // Assigned(CompleteStmt)?
      JE StringL                       // No!
      MOV EAX,[CompleteStmt]
      MOV BYTE PTR [EAX],False                  // Uncomplete Statement!


    // -------------------

    StringL:
      CALL MoveString                  // Quoted string?
      JE StringLE                      // Yes!

      MOV BeforeTrim,ESI
      CALL Trim                        // Empty character(s)?
      JE StringLE                      // Yes!
      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!

      MOV EAX,[KBegin]
      CALL CompareKeyword              // 'BEGIN'?
      JNE String1                      // No!

      CMP ECX,0                        // End of SQL?
      JE Finish                        // Yes!
      MOV BeforeTrim,ESI
      CALL Trim                        // Empty character(s)?
      CMP ECX,0                        // End of SQL?
      JE UncompleteStmt                // Yes!

      MOV EAX,[KWork]
      CALL CompareKeyword              // 'WORK'?
      JE StringLE                      // Yes!

      INC CompoundDeep                 // Compound deep

    String1:
      CMP CompoundDeep,0               // Inside a Compound?
      JE String3                       // No!

      MOV EAX,[KEnd]
      CALL CompareKeyword              // 'END'?
      JNE StringC                      // No!

      MOV BeforeTrim,ESI
      CALL Trim                        // Ignore empty characters
      CMP ECX,0                        // End of SQL?
      JE UncompleteStmt                // Yes!

      CMP WORD PTR [ESI],';'           // End of statement?
      JNE String2                      // No!
      DEC CompoundDeep                 // Compound deep
      JMP StringEOS

    String2:
      MOV EAX,[KIf]
      CALL CompareKeyword              // 'IF'?
      JE String3                       // Yes!
      MOV EAX,[KCase]
      CALL CompareKeyword              // 'CASE'?
      JE String3                       // Yes!
      MOV EAX,[KLoop]
      CALL CompareKeyword              // 'LOOP'?
      JE String3                       // Yes!
      MOV EAX,[KRepeat]
      CALL CompareKeyword              // 'REPEAT'?
      JE String3                       // Yes!
      MOV EAX,[KWhile]
      CALL CompareKeyword              // 'WHILE'?
      JE String3                       // Yes!
      DEC CompoundDeep                 // Compound deep
      JMP StringC

    String3:
      CMP WORD PTR [ESI],';'           // Statement Delimiter?
      JE StringEOS

    StringC:
      ADD ESI,2                        // Next character inside SQL
      DEC ECX
      JMP StringLE

    StringEOS:
      ADD ESI,2                        // Step over Delimiter
      DEC ECX                          // Ignore Delimiter
      JZ Complete                      // End of SQL!
      CMP CompoundDeep,0               // Still in compound?
      JE Complete                      // No!

    StringLE:
      JECXZ Finish                     // End of SQL!
      JMP StringL

    // -------------------

    Complete:
      CMP CompleteStmt,0               // Assigned(CompleteStmt)?
      JE Complete2                     // No!
      MOV EAX,[CompleteStmt]
      MOV BYTE PTR [EAX],True          // Complete statement!

    Complete2:
      CMP ECX,2                        // Are there two characters left in SQL?
      JL Complete3                     // No!
      MOV EAX,[ESI]
      CMP EAX,$000A000D                // CarriageReturn + LineFeed?
      JNE Complete3                    // No!
      ADD ESI,4                        // Step over CarriageReturn + LineFeed
      SUB ECX,2                        // Ignore CarriageReturn + LineFeed
      JMP Finish

    Complete3:
      CMP ECX,1                        // Is there one characters left in SQL?
      JL Finish                        // No!
      MOV AX,[ESI]
      CMP AX,13                        // Ending CarriageReturn?
      JE Complete4                     // Yes!
      CMP AX,10                        // Ending LineFeed?
      JE Complete4                     // Yes!
      JMP Finish

    Complete4:
      ADD ESI,2                        // Step over CarriageReturn / LineFeed
      DEC ECX                          // Ignore CarriageReturn / LineFeed
      JMP Finish

    // -------------------

    UncompleteStmt:
      MOV ESI,BeforeTrim

    // -------------------

    Finish:
      POP EBX
      SUB ESI,EBX
      SHR ESI,1                        // 2 bytes = 1 character
      MOV DWORD PTR [Result],ESI

      POP EBX
      POP EDI
      POP ESI
    end;
  end;
end;

function SQLStmtToCaption(const SQL: string; const Len: Integer = 50): string;
begin
  if (Length(SQL) <= Len) then
    Result := SQL
  else
    Result := copy(SQL, 1, Len) + '...';
end;

function SQLTrimStmt(const SQL: string): string;
var
  EndingCommentLen: Integer;
  Len: Integer;
  StartingCommentLen: Integer;
begin
  Len := SQLTrimStmt(SQL, 1, Length(SQL), StartingCommentLen, EndingCommentLen);
  Result := Copy(SQL, 1 + StartingCommentLen, Len);
end;

function SQLTrimStmt(const SQL: string; const Index, Length: Integer; var StartingCommentLength, EndingCommentLength: Integer): Integer; overload;
label
  StringL, StringLE,
  Finish;
var
  First: Integer;
  Last: Integer;
begin
  if (SQL = '') then
  begin
    StartingCommentLength := 0;
    EndingCommentLength := 0;
    Result := 1;
  end
  else
  begin
    Last := SQLStmtLength(SQL, Index);
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        MOV ESI,PChar(SQL)               // Read characters from SQL
        ADD ESI,Index                    // Add Index twice
        ADD ESI,Index                    //   since 1 character = 2 byte
        SUB ESI,2                        // Index based on "1" in string

        MOV ECX,Length

        MOV EDI,ESI
        ADD EDI,Length                   // EDI := SQL[Index - 1 + Length]
        ADD EDI,Length
        SUB EDI,2

      // -------------------

        CALL Trim                        // Step over empty characters

        MOV EAX,ESI                      // Calculate Result:
        SUB EAX,PChar(SQL)               //   Result := (ESI - @SQL) DIV SizeOf(Char)
        SHR EAX,1                        // 2 bytes = 1 character
        INC EAX                          // Index based on "1" in string
        MOV First,EAX

      StringL:
        CMP EDI,ESI                      // Begin of Statement?
        JE Finish                        // Yes!
        CMP WORD PTR [EDI],9             // Tabulator?
        JE StringLE                      // Yes!
        CMP WORD PTR [EDI],10            // New Line?
        JE StringLE                      // Yes!
        CMP WORD PTR [EDI],13            // Carrige Return?
        JE StringLE                      // Yes!
        CMP WORD PTR [EDI],' '           // Space
        JE StringLE                      // Yes!
        ADD EDI,2                        // Next character
        JMP Finish
      StringLE:
        SUB EDI,2                        // Previous character
        JMP StringL

      Finish:
        SUB EDI,PChar(SQL)               // Last := Len - (EDI - @SQL[Index]) DIV SizeOf(Char)
        SHR EDI,1                        // 2 bytes = 1 character
        MOV Last,EDI

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    StartingCommentLength := First - Index;
    EndingCommentLength := Index - 1 + Length - Last;
    Result := Length - StartingCommentLength - EndingCommentLength;
  end;
end;

function SQLUnescape(const Value: string): string;
label
  StringL, Quoted, StringLE,
  Finish;
var
  Len: Integer;
begin
  Len := Length(Value);

  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, Len); // reserve space

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

        MOV ECX,Len

      // -------------------

      StringL:
        MOV AX,[ESI]                     // Get character from Data
        CMP AX,''''                      // Start quotation in SQL?
        JE Quoted                        // Yes!
        CMP AX,'"'                       // Start quotation in SQL?
        JE Quoted                        // Yes!
        CMP AX,'`'                       // Start quotation in SQL?
        JE Quoted                        // Yes!
        MOVSW                            // Copy character
        LOOP StringL
        JMP Finish

      Quoted:
        CALL UnescapeString              // Copy and unescape quoted string?
        JECXZ Finish                     // End of SQL!
        JMP StringL

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

    if (Len <> Length(Result)) then
      SetLength(Result, Len);
  end;
end;

function SQLUnescape(const Value: RawByteString): RawByteString;
label
  StringL, StringL2;
var
  Len: Cardinal;
  S: string;
begin
  Len := Length(Value);

  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(S, Len);
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EDI,S                        //   to Result

        MOV ECX,Len
        MOV AH,0                         // Clear AH, since AL will be loaded, but AX stored
      StringL:
        LODSB                            // Load AnsiChar from Value
        STOSW                            // Store WideChar into S
        LOOP StringL                     // Repeat for all characters

        POP EDI
        POP ESI
        POP ES
    end;

    S := SQLUnescape(S);

    Len := Length(S);
    SetLength(Result, Len);

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Pointer(S)               // Copy characters from S
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV ECX,Len
      StringL2:
        LODSW                            // Load WideChar form S
        STOSB                            // Store AnsiChar into Result
        LOOP StringL2                    // Repeat for all characters

        POP EDI
        POP ESI
        POP ES
    end;
  end;
end;

function SQLWrapStmt(const SQL: string; const WrapStrs: array of string; const Indent: Integer): string;
var
  I: Integer;
  InQuote: Char;
  Pattern: string;
  PatternLen: Integer;
  Pos: Integer;
  WrapStr: string;
begin
  Result := SQL;

  for I := 0 to Length(WrapStrs) - 1 do
  begin
    WrapStr := WrapStrs[I];

    Pattern := WrapStr;
    PatternLen := Length(Pattern);

    InQuote := #0;

    Pos := 1;
    while (Pos <= Length(Result)) do
    begin
      if (CharInSet(Result[Pos], ['''', '"', '`'])) then
        if (InQuote = #0) then
          InQuote := Result[Pos]
        else
          InQuote := #0
      else if ((Result[Pos] = '\') and (InQuote <> #0)) then
        Inc(Pos)
      else if ((InQuote = #0) and (1 < Pos) and (Pos < Length(Result) - PatternLen - 1) and CharInSet(Result[Pos - 1], [#9, #10, #13, ' ', ',', ')']) and (Copy(Result, Pos, PatternLen) = Pattern) and CharInSet(Result[Pos + PatternLen], [#9, #10, #13, ' ', ',', '(', '='])) then
      begin
        while ((1 < Pos) and CharInSet(Result[Pos - 1], [#9, #10, #13, ' '])) do
          begin Delete(Result, Pos - 1, 1); Dec(Pos); end;
        Insert(#13#10 + StringOfChar(' ', Indent), Result, Pos);
        Inc(Pos, 2 + Indent);
      end;

      Inc(Pos);
    end;
  end;
end;

function SQLUnwrapStmt(const SQL: string): string;
label
  StringL, String2, StringC, StringLE,
  Finish;
var
  Len: Integer;
begin
  Len := Length(SQL);

  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, Len);

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Pointer(SQL)             // Copy characters from SQL
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV ECX,Len

        MOV EDX,$7FFFFFFF                // Enclose all condional MySQL code

      // -------------------

      StringL:
        CALL MoveString                  // Copy string from SQL to Result?
        JNE String2                      // No!
        JECXZ Finish                     // End of SQL!

      String2:
        CALL Trim                        // Emtpy characters in SQL?
        JNE StringC                      // No!
        JECXZ Finish                     // End of SQL!
        MOV AX,' '                       // Store a space
        STOSW                            //   into Result
        JECXZ Finish                     // End of SQL!
        JMP StringL

      StringC:
        MOVSW                            // Copy character from SQL to Result

      StringLE:
        LOOP StringL

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

    if (Len <> Length(Result)) then
      SetLength(Result, Len);
    Result := SQLTrimStmt(Result);
  end;
end;

//var
//  SQL: string;
//  DDLStmt: TSQLDDLStmt;
//begin
//  SQL := 'CREATE PROCEDURE `NeueProzedur`(`Param` int(11))'#$D#$A'BEGIN'#$D#$A'END;';
//  SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), 50500);
end.

