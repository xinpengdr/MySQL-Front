unit fAccount;

interface {********************************************************************}

uses
  Forms, Classes, XMLIntf, XMLDoc, Controls, ComCtrls, SysUtils, IniFiles,
  fPreferences, MySQLDB;

type
  TLogin = function(const Account: Pointer): Boolean of object;

type
  TSBookmarks = class;
  TSDesktop = class;
  TSConnection = class;
  TSAccount = class;
  TSAccounts = class;

  TSBookmark = class
  private
    FBookmarks: TSBookmarks;
    FURI: string;
    FXML: IXMLNode;
    function GetXML(): IXMLNode;
  protected
    property XML: IXMLNode read GetXML;
  public
    Caption: string;
    property Bookmarks: TSBookmarks read FBookmarks;
    property URI: string read FURI write FURI;
    procedure Assign(const Source: TSBookmark); virtual;
    constructor Create(const ABookmarks: TSBookmarks; const AXML: IXMLNode = nil); virtual;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
  end;

  TSBookmarks = class
  private
    FXML: IXMLNode;
    FBookmarks: array of TSBookmark;
    FDesktop: TSDesktop;
    function GetBookmark(Index: Integer): TSBookmark;
    function GetDataPath(): TFileName;
    function GetXML(): IXMLNode;
  protected
    property Desktop: TSDesktop read FDesktop;
    property XML: IXMLNode read GetXML;
  public
    property Bookmark[Index: Integer]: TSBookmark read GetBookmark; default;
    property DataPath: TFileName read GetDataPath;
    function AddBookmark(const NewBookmark: TSBookmark): Boolean; virtual;
    function ByCaption(const Caption: string): TSBookmark; virtual;
    procedure Clear(); virtual;
    constructor Create(const ADesktop: TSDesktop);
    function Count(): Integer; virtual;
    function DeleteBookmark(const Bookmark: TSBookmark): Boolean; virtual;
    destructor Destroy(); override;
    function IndexOf(const Bookmark: TSBookmark): Integer; virtual;
    procedure LoadFromXML(); virtual;
    procedure MoveBookmark(const Bookmark: TSBookmark; const NewIndex: Integer); virtual;
    procedure SaveToXML(); virtual;
    function UpdateBookmark(const Bookmark, NewBookmark: TSBookmark): Boolean; virtual;
  end;

  TSDesktop = class
  private
    FAddress: string;
    FBookmarks: TSBookmarks;
    FAccount: TSAccount;
    FXML: IXMLNode;
    function GetAddress(): string;
    function GetXML(): IXMLNode;
    procedure SetAddress(AAddress: string);
  protected
    property Account: TSAccount read FAccount;
    property XML: IXMLNode read GetXML;
    procedure Assign(const Source: TSDesktop); virtual;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
  public
    AddressMRU: TMRUList;
    BookmarksVisible: Boolean;
    ContentWidths: array[0..7] of array [0..9] of Integer;
    DataHeight, BlobHeight: Integer;
    EditorContent: string;
    ExplorerVisible: Boolean;
    LogHeight: Integer;
    LogVisible: Boolean;
    FoldersHeight: Integer;
    FilesFilter: string;
    NavigatorVisible: Boolean;
    SelectorWitdth: Integer;
    SQLHistoryVisible: Boolean;
    ToolBarRefresh: Integer;
    property Address: string read GetAddress write SetAddress;
    property Bookmarks: TSBookmarks read FBookmarks;
    constructor Create(const AAccount: TSAccount); overload; virtual;
    destructor Destroy(); override;
  end;

  TSConnection = class
  private
    FXML: IXMLNode;
    FAccount: TSAccount;
    function GetXML(): IXMLNode;
  protected
    Section: string;
    property XML: IXMLNode read GetXML;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
  public
    Asynchron: Boolean;
    Charset: string;
    Compression: Boolean;
    Database: string;
    Host: string;
    HTTPTunnelURI: string;
    LibraryFilename: TFileName;
    LibraryType: TMySQLLibrary.TLibraryType;
    MultiStatements: Boolean;
    Password: string;
    Port: Integer;
    Prefetch: Integer;
    SavePassword: Boolean;
    UseInformationSchema: Boolean;
    User: string;
    property Account: TSAccount read FAccount;
    procedure Assign(const Source: TSConnection); virtual;
    constructor Create(const AAccount: TSAccount); virtual;
  end;

  TSAccount = class
  type
    TDefaultLimit = (dlOff = 0, dlRemember = 1, dlOn = 2);
    TEventProc = procedure (const ClassType: TClass) of object;
    TDesktop = record
      Control: Pointer;
      AccountEventProc: TEventProc;
    end;
  private
    FDesktop: TSDesktop;
    FDesktopXMLDocument: IXMLDocument;
    FHistoryXMLDocument: IXMLDocument;
    FLastLogin: TDateTime;
    FName: string;
    FDesktopCount: Integer;
    FDesktops: array of TDesktop;
    FXML: IXMLNode;
    FAccounts: TSAccounts;
    Modified: Boolean;
    function GetBookmarksFilename(): TFileName;
    function GetDataPath(): TFileName;
    function GetDesktop(): TSDesktop;
    function GetDesktopFilename(): TFileName;
    function GetDesktopXML(): IXMLNode;
    function GetHistoryFilename(): TFileName;
    function GetHistoryXML(): IXMLNode;
    function GetIconFilename(): TFileName;
    function GetName(): string;
    function GetXML(): IXMLNode;
    procedure SetLastLogin(const ALastLogin: TDateTime);
    procedure SetName(const AName: string);
  protected
    Section: string;
    property BookmarksFilename: TFileName read GetBookmarksFilename;
    property DesktopFilename: TFileName read GetDesktopFilename;
    property DesktopXMLDocument: IXMLDocument read FDesktopXMLDocument;
    property HistoryFilename: TFileName read GetHistoryFilename;
    property HistoryXMLDocument: IXMLDocument read FHistoryXMLDocument;
    property XML: IXMLNode read GetXML;
    function GetIndex(): Integer;
    procedure LoadFromXML();
    procedure SaveToXML(); virtual;
    procedure AccountEvent(const ClassType: TClass); virtual;
    function ValidDatabaseName(const ADatabaseName: string): Boolean;
  public
    Startup: string;
    CacheSize: Integer;
    Connection: TSConnection;
    DefaultLimit: TDefaultLimit;
    DefaultSorting: Boolean;
    IconFetched: Boolean;
    ImageIndex: Integer;
    ManualURL: string;
    ManualURLFetched: Boolean;
    property DataPath: TFileName read GetDataPath;
    property Desktop: TSDesktop read GetDesktop;
    property DesktopCount: Integer read FDesktopCount write FDesktopCount;
    property DesktopXML: IXMLNode read GetDesktopXML;
    function Frame(): Pointer; virtual;
    property HistoryXML: IXMLNode read GetHistoryXML;
    property IconFilename: TFileName read GetIconFilename;
    property Index: Integer read GetIndex;
    property LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property Name: string read GetName write SetName;
    property Accounts: TSAccounts read FAccounts;
    procedure Assign(const Source: TSAccount); virtual;
    constructor Create(const AAccounts: TSAccounts; const AXML: IXMLNode = nil); virtual;
    destructor Destroy(); override;
    function GetDefaultDatabase(): string; virtual;
    procedure RegisterDesktop(const AControl: Pointer; const AEventProc: TEventProc); virtual;
    procedure UnRegisterDesktop(const AControl: Pointer); virtual;
    function PackAddress(const AAddress: string): string; virtual;
    function FullAddress(const AAddress: string): string; virtual;
  end;

  TSAccounts = class(TList)
  private
    DefaultAccountName: string;
    FDBLogin: TLogin;
    FOnSQLError: TMySQLConnection.TErrorEvent;
    FXMLDocument: IXMLDocument;
    function GetDataPath(): TFileName;
    function GetDefault(): TSAccount; inline;
    function GetFilename(): TFileName;
    function GetXML(): IXMLNode;
    function GetFAccounts(Index: Integer): TSAccount; inline;
    procedure SetDefault(const AAccount: TSAccount);
  protected
    Section: string;
    property DataPath: TFileName read GetDataPath;
    property Filename: TFileName read GetFilename;
    property XML: IXMLNode read GetXML;
  public
    property Default: TSAccount read GetDefault write SetDefault;
    property DBLogin: TLogin read FDBLogin;
    property OnSQLError: TMySQLConnection.TErrorEvent read FOnSQLError;
    property Account[Index: Integer]: TSAccount read GetFAccounts; default;
    procedure AddAccount(const NewAccount: TSAccount); virtual;
    procedure AppendIconsToImageList(const AImageList: TImageList; const AAccount: TSAccount = nil); virtual;
    procedure Clear(); override;
    constructor Create(const ADBLogin: TLogin; const AOnSQLError: TMySQLConnection.TErrorEvent);
    function DeleteAccount(const AAccount: TSAccount): Boolean; virtual;
    destructor Destroy(); override;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
    function AccountByName(const AccountName: string): TSAccount; virtual;
    function AccountByURI(const AURI: string): TSAccount; virtual;
    procedure UpdateAccount(const Account, NewAccount: TSAccount); virtual;
  end;

function HostIsLocalhost(const Host: string): Boolean;

var
  Accounts: TSAccounts;

implementation {***************************************************************}

uses
  Windows, Graphics, CommCtrl, Consts, ShlObj, WinINet, ActiveX,
  Variants, RTLConsts, StrUtils, Registry, WinSock,
  MySQLConsts,
  CSVUtils,
  fURI;

resourcestring
  SDirNotExists = 'Verzeichnis "%s" nicht gefunden.';

var
  WSAData: WinSock.WSADATA;

function IntToLibraryType(const Value: Integer): TMySQLLibrary.TLibraryType;
begin
  case (Value) of
    1: Result := ltDLL;
    2: Result := ltHTTP;
    else Result := ltBuiltIn;
  end;
end;

function LibraryTypeToInt(const LibraryType: TMySQLLibrary.TLibraryType): Integer;
begin
  case (LibraryType) of
    ltDLL: Result := 1;
    ltHTTP: Result := 2;
    else Result := 0; // ltBuiltIn
  end;
end;

function ViewStyleToInteger(const ViewStyle: TViewStyle): Integer;
begin
  Result := 2;
  case (ViewStyle) of
    vsIcon: Result := 0;
    vsSmallIcon: Result := 1;
    vsList: Result := 2;
    vsReport: Result := 3;
  end;
end;

function TryStrToViewStyle(const Str: string; var ViewStyle: TViewStyle): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'ICON') then ViewStyle := vsIcon
  else if (UpperCase(Str) = 'SMALLICON') then ViewStyle := vsSmallIcon
  else if (UpperCase(Str) = 'LIST') then ViewStyle := vsList
  else if (UpperCase(Str) = 'REPORT') then ViewStyle := vsReport
  else Result := False;
end;

function ViewStyleToStr(const ViewStyle: TViewStyle): string;
begin
  case (ViewStyle) of
    vsIcon: Result := 'Icon';
    vsList: Result := 'List';
    vsReport: Result := 'Report';
    else Result := 'SmallIcon';
  end;
end;

function IntegerToViewStyle(const Int: Integer): TViewStyle;
begin
  Result := vsList;
  case (Int) of
    0: Result := vsIcon;
    1: Result := vsSmallIcon;
    2: Result := vsList;
    3: Result := vsReport;
  end;
end;

function HostIsLocalhost(const Host: string): Boolean;
type
  PPInAddr = ^PInAddr;
//var
//  HostEnt: PHostEnt;
//  HostName: array[0..255] of AnsiChar;
//  Addr: PPInAddr;
//  Addresses: TStringList;
begin
  Result := (lstrcmpi(PChar(Host), PChar(LOCAL_HOST)) = 0) or (Host = LOCAL_HOST_NAMEDPIPE);

//  if (not Result and ((WSAData.wVersion > 0) or (WSAStartup($0101, WSAData) = 0)) and (gethostname(@HostName[0], SizeOf(HostName)) = 0)) then
//  begin
//    Addresses := TStringList.Create();
//
//    HostEnt := gethostbyname(PAnsiChar(AnsiString(Host)));
//
//    if (Assigned(HostEnt)) then
//    begin
//      Addr := Pointer(HostEnt^.h_addr_list);
//      while (Assigned(Addr^)) do
//      begin
//        Addresses.Add(string(inet_ntoa(Addr^^)));
//        Result := Result or (string(inet_ntoa(Addr^^)) = '127.0.0.1');
//        Inc(Addr);
//      end;
//
//      HostEnt := GetHostByName(HostName);
//      if (Assigned(HostEnt)) then
//      begin
//        Addr := Pointer(HostEnt^.h_addr_list);
//        while (Assigned(Addr^)) do
//        begin
//          Result := Result or (Addresses.IndexOf(string(inet_ntoa(Addr^^))) >= 0);
//          Inc(Addr);
//        end;
//      end;
//    end;
//
//    Addresses.Free();
//  end;
end;

{ TSBookmark ******************************************************************}

procedure TSBookmark.Assign(const Source: TSBookmark);
begin
  if (not Assigned(Bookmarks) and Assigned(Source.Bookmarks)) then
    FBookmarks := Source.Bookmarks;

  Caption := Source.Caption;
  URI := Source.URI;

  if (Assigned(XML)) then
    XML.Attributes['name'] := Caption;
end;

constructor TSBookmark.Create(const ABookmarks: TSBookmarks; const AXML: IXMLNode = nil);
begin
  FBookmarks := ABookmarks;
  FXML := AXML;

  Caption := '';
  FURI := '';
end;

function TSBookmark.GetXML(): IXMLNode;
var
  I: Integer;
begin
  if (not Assigned(FXML)) then
    for I := 0 to Bookmarks.XML.ChildNodes.Count - 1 do
      if ((Bookmarks.XML.ChildNodes[I].NodeName = 'bookmark') and (lstrcmpi(PChar(string(Bookmarks.XML.ChildNodes[I].Attributes['name'])), PChar(Caption)) = 0)) then
        FXML := Bookmarks.XML.ChildNodes[I];

  Result := FXML;
end;

procedure TSBookmark.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    Caption := XML.Attributes['name'];
    if (Assigned(XMLNode(XML, 'uri'))) then FURI := Bookmarks.Desktop.Account.FullAddress(XMLNode(XML, 'uri').Text);
  end;
end;

procedure TSBookmark.SaveToXML();
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XML.Attributes['name'] := Caption;
  XMLNode(XML, 'uri').Text := Bookmarks.Desktop.Account.PackAddress(URI);

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];
end;

{ TSBookmarks *****************************************************************}

function TSBookmarks.AddBookmark(const NewBookmark: TSBookmark): Boolean;
begin
  Result := IndexOf(NewBookmark) < 0;

  if (Result) then
  begin
    SetLength(FBookmarks, Count + 1);

    FBookmarks[Count - 1] := TSBookmark.Create(Self, XML.AddChild('bookmark'));
    FBookmarks[Count - 1].Assign(NewBookmark);

    Desktop.Account.AccountEvent(ClassType);
  end;
end;

function TSBookmarks.ByCaption(const Caption: string): TSBookmark;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if (lstrcmpi(PChar(FBookmarks[I].Caption), PChar(Caption)) = 0) then
      Result := FBookmarks[I];
end;

procedure TSBookmarks.Clear();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FBookmarks[I].Free();
  SetLength(FBookmarks, 0);
end;

function TSBookmarks.Count(): Integer;
begin
  Result := Length(FBookmarks);
end;

constructor TSBookmarks.Create(const ADesktop: TSDesktop);
begin
  inherited Create();

  FDesktop := ADesktop;

  FXML := nil;
  SetLength(FBookmarks, 0);
end;

function TSBookmarks.DeleteBookmark(const Bookmark: TSBookmark): Boolean;
var
  I: Integer;
  Index: Integer;
begin
  Index := IndexOf(ByCaption(Bookmark.Caption));

  Result := Index >= 0;
  if (Result) then
  begin
    XML.ChildNodes.Delete(XML.ChildNodes.IndexOf(Bookmark.XML));

    FBookmarks[Index].Free();
    for I := Index to Count - 2 do
      FBookmarks[I] := FBookmarks[I + 1];

    SetLength(FBookmarks, Count - 1);

    Desktop.Account.AccountEvent(ClassType);
  end;
end;

destructor TSBookmarks.Destroy();
begin
  Clear();

  inherited;
end;

function TSBookmarks.GetBookmark(Index: Integer): TSBookmark;
begin
  Result := FBookmarks[Index];
end;

function TSBookmarks.GetDataPath(): TFileName;
begin
  Result := Desktop.Account.DataPath;
end;

function TSBookmarks.GetXML(): IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(Desktop.XML)) then
    FXML := XMLNode(Desktop.XML, 'bookmarks', True);

  Result := FXML;
end;

function TSBookmarks.IndexOf(const Bookmark: TSBookmark): Integer;
var
  I: Integer;
begin
  Result := -1;

  if (Assigned(Bookmark)) then
    for I := 0 to Count - 1 do
      if (lstrcmpi(PChar(FBookmarks[I].Caption), PChar(Bookmark.Caption)) = 0) then
        Result := I;
end;

procedure TSBookmarks.LoadFromXML();
var
  I: Integer;
begin
  Clear();

  if (Assigned(XML)) then
    for I := 0 to XML.ChildNodes.Count - 1 do
      if ((XML.ChildNodes[I].NodeName = 'bookmark') and not Assigned(ByCaption(XML.ChildNodes[I].Attributes['name']))) then
      begin
        SetLength(FBookmarks, Count + 1);
        FBookmarks[Count - 1] := TSBookmark.Create(Self, XML.ChildNodes[I]);

        FBookmarks[Count - 1].LoadFromXML();
      end;
end;

procedure TSBookmarks.MoveBookmark(const Bookmark: TSBookmark; const NewIndex: Integer);
var
  I: Integer;
  Index: Integer;
  TempBookmark: TSBookmark;
begin
  Index := IndexOf(Bookmark);
  TempBookmark := FBookmarks[Index];

  if (NewIndex <> Index) then
  begin
    XML.ChildNodes.Remove(Bookmark.XML);

    if (NewIndex < 0) then
    begin
      for I := Index to Count - 2 do
        FBookmarks[I] := FBookmarks[I + 1];

      XML.ChildNodes.Insert(0, Bookmark.XML);
    end
    else if (NewIndex < Index) then
    begin
      for I := Index downto NewIndex + 1 do
        FBookmarks[I] := FBookmarks[I - 1];

      XML.ChildNodes.Insert(NewIndex, Bookmark.XML);
    end
    else
    begin
      for I := Index to NewIndex - 1 do
        FBookmarks[I] := FBookmarks[I + 1];

      XML.ChildNodes.Insert(NewIndex, Bookmark.XML);
    end;
    if (NewIndex < 0) then
      FBookmarks[Count - 1] := TempBookmark
    else
      FBookmarks[NewIndex] := TempBookmark;

    Desktop.Account.AccountEvent(ClassType);
  end;
end;

procedure TSBookmarks.SaveToXML();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Bookmark[I].SaveToXML();
end;

function TSBookmarks.UpdateBookmark(const Bookmark, NewBookmark: TSBookmark): Boolean;
begin
  Result := Assigned(Bookmark) and Assigned(NewBookmark);

  if (Result) then
  begin
    if (Assigned(Bookmark.XML)) then
      Bookmark.XML.Attributes['name'] := NewBookmark.Caption;

    Bookmark.Assign(NewBookmark);

    Desktop.Account.AccountEvent(ClassType);
  end;
end;

{ TSDesktop *******************************************************************}

procedure TSDesktop.Assign(const Source: TSDesktop);
var
  I: Integer;
  J: Integer;
begin
  Address := Account.FullAddress(Source.Account.PackAddress(Source.Address));
  BlobHeight := Source.BlobHeight;
  BookmarksVisible := Source.BookmarksVisible;
  for I := 0 to Length(ContentWidths) - 1 do
    for J := 0 to Length(ContentWidths[0]) - 1 do
      ContentWidths[I][J] := Source.ContentWidths[I][J];
  DataHeight := Source.DataHeight;
  FilesFilter := Source.FilesFilter;
  EditorContent := Source.EditorContent;
  ExplorerVisible := Source.ExplorerVisible;
  FoldersHeight := Source.FoldersHeight;
  LogHeight := Source.LogHeight;
  LogVisible := Source.LogVisible;
  NavigatorVisible := Source.NavigatorVisible;
  SelectorWitdth := Source.SelectorWitdth;
  SQLHistoryVisible := Source.SQLHistoryVisible;
  ToolBarRefresh := Source.ToolBarRefresh;
end;

constructor TSDesktop.Create(const AAccount: TSAccount);
var
  I: Integer;
  J: Integer;
begin
  inherited Create();

  FAccount := AAccount;
  FXML := nil;

  FAddress := '/';
  BlobHeight := 100;
  BookmarksVisible := False;
  for I := 0 to Length(ContentWidths) - 1 do
    for J := 0 to Length(ContentWidths[0]) - 1 do
      ContentWidths[I][J] := -1;
  DataHeight := 150;
  EditorContent := '';
  ExplorerVisible := False;
  FilesFilter := '*.sql';
  FoldersHeight := 100;
  NavigatorVisible := True;
  LogHeight := 80;
  LogVisible := False;
  SelectorWitdth := 150;
  SQLHistoryVisible := False;
  ToolBarRefresh := 1;

  AddressMRU := TMRUList.Create(10);

  FBookmarks := TSBookmarks.Create(Self);
end;

destructor TSDesktop.Destroy();
begin
  AddressMRU.Free();
  FBookmarks.Free();

  inherited;
end;

function TSDesktop.GetAddress(): string;
begin
  Result := Account.FullAddress(FAddress);
end;

function TSDesktop.GetXML(): IXMLNode;
begin
  if (not Assigned(FXML)) then
    FXML := Account.DesktopXML;

  Result := FXML;
end;

procedure TSDesktop.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'address'))) then
    begin
      if (Assigned(XMLNode(XML, 'address/default')) and (XMLNode(XML, 'address/default').Text <> '')) then
        FAddress := XMLNode(XML, 'address/default').Text;
      AddressMRU.Clear();
    end;
    if (Assigned(XMLNode(XML, 'datagrid/height'))) then TryStrToInt(XMLNode(XML, 'datagrid/height').Text, DataHeight);
    if (Assigned(XMLNode(XML, 'datagrid/blob/height'))) then TryStrToInt(XMLNode(XML, 'datagrid/blob/height').Text, BlobHeight);
    if (Assigned(XMLNode(XML, 'editor/content'))) then EditorContent := XMLNode(XML, 'editor/content').Text;
    if (Assigned(XMLNode(XML, 'log/height'))) then TryStrToInt(XMLNode(XML, 'log/height').Text, LogHeight);
    if (Assigned(XMLNode(XML, 'log/visible'))) then TryStrToBool(XMLNode(XML, 'log/visible').Text, LogVisible);
    if (Assigned(XMLNode(XML, 'objects/server/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/name').Text, ContentWidths[0][0]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/size'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/size').Text, ContentWidths[0][1]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/count'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/count').Text, ContentWidths[0][2]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/created'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/created').Text, ContentWidths[0][3]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/extras'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/extras').Text, ContentWidths[0][4]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/name').Text, ContentWidths[1][0]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/type'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/type').Text, ContentWidths[1][1]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/recordcount'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/recordcount').Text, ContentWidths[1][2]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/size'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/size').Text, ContentWidths[1][3]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/updated'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/updated').Text, ContentWidths[1][4]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/extras'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/extras').Text, ContentWidths[1][5]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/comment'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/comment').Text, ContentWidths[1][6]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/name').Text, ContentWidths[2][0]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/type'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/type').Text, ContentWidths[2][1]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/null'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/null').Text, ContentWidths[2][2]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/default'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/default').Text, ContentWidths[2][3]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/extras'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/extras').Text, ContentWidths[2][4]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/comment'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/comment').Text, ContentWidths[2][5]);
    if (Assigned(XMLNode(XML, 'objects/hosts/widths/host'))) then TryStrToInt(XMLNode(XML, 'objects/hosts/widths/host').Text, ContentWidths[3][0]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/id'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/id').Text, ContentWidths[4][0]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/user'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/user').Text, ContentWidths[4][1]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/host'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/host').Text, ContentWidths[4][2]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/database'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/database').Text, ContentWidths[4][3]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/command'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/command').Text, ContentWidths[4][4]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/statement'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/statement').Text, ContentWidths[4][5]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/time'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/time').Text, ContentWidths[4][6]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/state'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/state').Text, ContentWidths[4][7]);
    if (Assigned(XMLNode(XML, 'objects/stati/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/stati/widths/name').Text, ContentWidths[5][0]);
    if (Assigned(XMLNode(XML, 'objects/stati/widths/value'))) then TryStrToInt(XMLNode(XML, 'objects/stati/widths/value').Text, ContentWidths[5][1]);
    if (Assigned(XMLNode(XML, 'objects/users/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/users/widths/name').Text, ContentWidths[6][0]);
    if (Assigned(XMLNode(XML, 'objects/users/widths/fullname'))) then TryStrToInt(XMLNode(XML, 'objects/users/widths/fullname').Text, ContentWidths[6][1]);
    if (Assigned(XMLNode(XML, 'objects/users/widths/comment'))) then TryStrToInt(XMLNode(XML, 'objects/users/widths/comment').Text, ContentWidths[6][2]);
    if (Assigned(XMLNode(XML, 'objects/variables/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/variables/widths/name').Text, ContentWidths[7][0]);
    if (Assigned(XMLNode(XML, 'objects/variables/widths/value'))) then TryStrToInt(XMLNode(XML, 'objects/variables/widths/value').Text, ContentWidths[7][1]);
    if (Assigned(XMLNode(XML, 'sidebar/explorer/folders/height'))) then TryStrToInt(XMLNode(XML, 'sidebar/explorer/folders/height').Text, FoldersHeight);
    if (Assigned(XMLNode(XML, 'sidebar/explorer/files/filter'))) then FilesFilter := XMLNode(XML, 'sidebar/explorer/files/filter').Text;
    if (Assigned(XMLNode(XML, 'sidebar/width'))) then TryStrToInt(XMLNode(XML, 'sidebar/width').Text, SelectorWitdth);
    if (Assigned(XMLNode(XML, 'sidebar/visible'))) then
    begin
      NavigatorVisible := UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'NAVIGATOR';
      BookmarksVisible := not NavigatorVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'BOOKMARKS');
      ExplorerVisible := not NavigatorVisible and not BookmarksVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'EXPLORER');
      SQLHistoryVisible := not NavigatorVisible and not BookmarksVisible and not ExplorerVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'SQL HISTORY');
    end;
    if (Assigned(XMLNode(XML, 'sidebar/visible'))) then
      if (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'ALL') then ToolBarRefresh := 1 else ToolBarRefresh := 0;

    Bookmarks.LoadFromXML();
  end;
end;

procedure TSDesktop.SaveToXML();
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XMLNode(XML, 'address').ChildNodes.Clear();
  XMLNode(XML, 'address/default').Text := FAddress;
  XMLNode(XML, 'datagrid/height').Text := IntToStr(DataHeight);
  XMLNode(XML, 'datagrid/blob/height').Text := IntToStr(BlobHeight);
  XMLNode(XML, 'editor/content').Text := EditorContent;
  XMLNode(XML, 'editor/filename/mru').ChildNodes.Clear();
  XMLNode(XML, 'log/height').Text := IntToStr(LogHeight);
  XMLNode(XML, 'log/visible').Text := BoolToStr(LogVisible, True);
  XMLNode(XML, 'objects/server/widths/name').Text := IntToStr(ContentWidths[0][0]);
  XMLNode(XML, 'objects/server/widths/count').Text := IntToStr(ContentWidths[0][1]);
  XMLNode(XML, 'objects/server/widths/size').Text := IntToStr(ContentWidths[0][2]);
  XMLNode(XML, 'objects/server/widths/created').Text := IntToStr(ContentWidths[0][3]);
  XMLNode(XML, 'objects/server/widths/extras').Text := IntToStr(ContentWidths[0][4]);
  XMLNode(XML, 'objects/database/widths/name').Text := IntToStr(ContentWidths[1][0]);
  XMLNode(XML, 'objects/database/widths/type').Text := IntToStr(ContentWidths[1][1]);
  XMLNode(XML, 'objects/database/widths/recordcount').Text := IntToStr(ContentWidths[1][2]);
  XMLNode(XML, 'objects/database/widths/size').Text := IntToStr(ContentWidths[1][3]);
  XMLNode(XML, 'objects/database/widths/updated').Text := IntToStr(ContentWidths[1][4]);
  XMLNode(XML, 'objects/database/widths/extras').Text := IntToStr(ContentWidths[1][5]);
  XMLNode(XML, 'objects/database/widths/comment').Text := IntToStr(ContentWidths[1][6]);
  XMLNode(XML, 'objects/table/widths/name').Text := IntToStr(ContentWidths[2][0]);
  XMLNode(XML, 'objects/table/widths/type').Text := IntToStr(ContentWidths[2][1]);
  XMLNode(XML, 'objects/table/widths/null').Text := IntToStr(ContentWidths[2][2]);
  XMLNode(XML, 'objects/table/widths/default').Text := IntToStr(ContentWidths[2][3]);
  XMLNode(XML, 'objects/table/widths/extras').Text := IntToStr(ContentWidths[2][4]);
  XMLNode(XML, 'objects/table/widths/comment').Text := IntToStr(ContentWidths[2][5]);
  XMLNode(XML, 'objects/hosts/widths/host').Text := IntToStr(ContentWidths[3][0]);
  XMLNode(XML, 'objects/processes/widths/id').Text := IntToStr(ContentWidths[4][0]);
  XMLNode(XML, 'objects/processes/widths/user').Text := IntToStr(ContentWidths[4][1]);
  XMLNode(XML, 'objects/processes/widths/host').Text := IntToStr(ContentWidths[4][2]);
  XMLNode(XML, 'objects/processes/widths/database').Text := IntToStr(ContentWidths[4][3]);
  XMLNode(XML, 'objects/processes/widths/command').Text := IntToStr(ContentWidths[4][4]);
  XMLNode(XML, 'objects/processes/widths/statement').Text := IntToStr(ContentWidths[4][5]);
  XMLNode(XML, 'objects/processes/widths/time').Text := IntToStr(ContentWidths[4][6]);
  XMLNode(XML, 'objects/processes/widths/state').Text := IntToStr(ContentWidths[4][7]);
  XMLNode(XML, 'objects/stati/widths/name').Text := IntToStr(ContentWidths[5][0]);
  XMLNode(XML, 'objects/stati/widths/value').Text := IntToStr(ContentWidths[5][1]);
  XMLNode(XML, 'objects/users/widths/name').Text := IntToStr(ContentWidths[6][0]);
  XMLNode(XML, 'objects/users/widths/fullname').Text := IntToStr(ContentWidths[6][1]);
  XMLNode(XML, 'objects/users/widths/comment').Text := IntToStr(ContentWidths[6][2]);
  XMLNode(XML, 'objects/variables/widths/name').Text := IntToStr(ContentWidths[5][0]);
  XMLNode(XML, 'objects/variables/widths/value').Text := IntToStr(ContentWidths[5][1]);
  XMLNode(XML, 'sidebar/explorer/folders/height').Text := IntToStr(FoldersHeight);
  XMLNode(XML, 'sidebar/explorer/files/filter').Text := FilesFilter;
  XMLNode(XML, 'sidebar/width').Text := IntToStr(SelectorWitdth);
  if (NavigatorVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Navigator'
  else if (BookmarksVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Bookmarks'
  else if (ExplorerVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Explorer'
  else if (SQLHistoryVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'SQL History'
  else
    XMLNode(XML, 'sidebar/visible').Text := BoolToStr(False, True);
  if (ToolBarRefresh = 0) then
    XMLNode(XML, 'toolbar/refresh').Text := ''
  else
    XMLNode(XML, 'toolbar/refresh').Text := 'All';

  Bookmarks.SaveToXML();

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];
end;

procedure TSDesktop.SetAddress(AAddress: string);
begin
  FAddress := Account.PackAddress(AAddress);
end;

{ TSConnection ****************************************************************}

procedure TSConnection.Assign(const Source: TSConnection);
begin
  Charset := Source.Charset;
  Compression := Source.Compression;
  Database := Source.Database;
  Host := Source.Host;
  HTTPTunnelURI := Source.HTTPTunnelURI;
  LibraryFilename := Source.LibraryFilename;
  LibraryType := Source.LibraryType;
  MultiStatements := Source.MultiStatements;
  Asynchron := Source.Asynchron;
  Password := Source.Password;
  Port := Source.Port;
  Prefetch := Source.Prefetch;
  SavePassword := Source.SavePassword;
  UseInformationSchema := Source.UseInformationSchema;
  User := Source.User;
end;

constructor TSConnection.Create(const AAccount: TSAccount);
begin
  inherited Create();

  FAccount := AAccount;
  FXML := nil;

  Charset := '';
  Compression := True;
  Database := '';
  Host := '';
  HTTPTunnelURI := '';
  LibraryFilename := 'libMySQL.dll';
  LibraryType := ltBuiltIn;
  MultiStatements := True;
  Asynchron := True;
  Password := '';
  Port := MYSQL_PORT;
  Prefetch := 1;
  SavePassword := False;
  UseInformationSchema := True;
  User := '';
end;

function TSConnection.GetXML(): IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(Account.XML)) then
    FXML := XMLNode(Account.XML, 'connection');

  Result := FXML;
end;

procedure TSConnection.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'character_set'))) then Charset := XMLNode(XML, 'character_set').Text;
    if (Assigned(XMLNode(XML, 'compression'))) then TryStrToBool(XMLNode(XML, 'compression').Text, Compression);
    if (Assigned(XMLNode(XML, 'database'))) then Database := XMLNode(XML, 'database').Text;
    if (Assigned(XMLNode(XML, 'host'))) then Host := XMLNode(XML, 'host').Text;
    if (Assigned(XMLNode(XML, 'library/type'))) then
      if (UpperCase(XMLNode(XML, 'library/type').Text) = 'FILE') then LibraryType := ltDLL
      else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'TUNNEL') then LibraryType := ltHTTP
      else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'HTTPTUNNEL') then LibraryType := ltHTTP
      else LibraryType := ltBuiltIn;
    if (Assigned(XMLNode(XML, 'library/filename'))) then LibraryFilename := XMLNode(XML, 'library/filename').Text;
    if (Assigned(XMLNode(XML, 'library/tunnel_url'))) then HTTPTunnelURI := XMLNode(XML, 'library/tunnel_url').Text;
    if (Assigned(XMLNode(XML, 'multistatements'))) then TryStrToBool(XMLNode(XML, 'multistatements').Text, MultiStatements);
    if (Assigned(XMLNode(XML, 'asynchron'))) then TryStrToBool(XMLNode(XML, 'asynchron').Text, Asynchron);
    if (Assigned(XMLNode(XML, 'password')) and (XMLNode(XML, 'password').Attributes['encode'] = 'none')) then Password := XMLNode(XML, 'password').Text;
    if (Assigned(XMLNode(XML, 'port'))) then TryStrToInt(XMLNode(XML, 'port').Text, Port);
    if (Assigned(XMLNode(XML, 'prefetch'))) then TryStrToInt(XMLNode(XML, 'prefetch').Text, Prefetch);
    if (Assigned(XMLNode(XML, 'savepassword'))) then TryStrToBool(XMLNode(XML, 'savepassword').Text, SavePassword);
    if (Assigned(XMLNode(XML, 'information_schema'))) then
      UseInformationSchema := UpperCase(XMLNode(XML, 'information_schema').Text) <> 'IGNORE';
    if (Assigned(XMLNode(XML, 'user'))) then User := XMLNode(XML, 'user').Text;
  end;
end;

procedure TSConnection.SaveToXML();
begin
  XMLNode(XML, 'character_set').Text := Charset;
  XMLNode(XML, 'compression').Text := BoolToStr(Compression, True);
  XMLNode(XML, 'database').Text := Database;
  XMLNode(XML, 'host').Text := Host;
  case (LibraryType) of
    ltDLL: XMLNode(XML, 'library/type').Text := 'File';
    ltHTTP: XMLNode(XML, 'library/type').Text := 'HTTPTunnel';
    else XMLNode(XML, 'library').ChildNodes.Delete('type');
  end;
  XMLNode(XML, 'library/filename').Text := LibraryFilename;
  XMLNode(XML, 'library/tunnel_url').Text := HTTPTunnelURI;
  XMLNode(XML, 'multistatements').Text := BoolToStr(MultiStatements, True);
  if (MultiStatements) then
    XML.ChildNodes.Delete('multistatements')
  else
    XMLNode(XML, 'multistatements').Text := BoolToStr(MultiStatements, True);
  if (Asynchron) then
    XML.ChildNodes.Delete('asynchron')
  else
    XMLNode(XML, 'asynchron').Text := BoolToStr(Asynchron, True);
  XMLNode(XML, 'password').Attributes['encode'] := 'none';
  XMLNode(XML, 'password').Text := Password;
  XMLNode(XML, 'port').Text := IntToStr(Port);
  if (Prefetch = 1) then
    XML.ChildNodes.Delete('prefetch')
  else
    XMLNode(XML, 'prefetch').Text := IntToStr(Prefetch);
  XMLNode(XML, 'savepassword').Text := BoolToStr(SavePassword, True);
  if (UseInformationSchema) then
    XML.ChildNodes.Delete('information_schema')
  else
    XMLNode(XML, 'information_schema').Text := 'ignore';
  XMLNode(XML, 'user').Text := User;
end;

{ TSAccount *******************************************************************}

procedure TSAccount.Assign(const Source: TSAccount);
begin
  if (not Assigned(Accounts)) then FAccounts := Source.Accounts;

  CacheSize := Source.CacheSize;
  DefaultLimit := Source.DefaultLimit;
  DefaultSorting := Source.DefaultSorting;
  FLastLogin := Source.LastLogin;
  IconFetched := Source.IconFetched;
  ImageIndex := Source.ImageIndex;
  ManualURL := Source.ManualURL;
  ManualURLFetched := Source.ManualURLFetched;
  Name := Source.Name;
  Startup := Source.Startup;

  Modified := True;

  Connection.Assign(Source.Connection);
  if (Assigned(Desktop) and Assigned(Source.Desktop)) then
    Desktop.Assign(Source.Desktop);
end;

constructor TSAccount.Create(const AAccounts: TSAccounts; const AXML: IXMLNode = nil);
begin
  FAccounts := AAccounts;
  FXML := AXML;

  FDesktopCount := 0;

  CacheSize := 50;
  DefaultLimit := dlRemember;
  DefaultSorting := True;
  FDesktopXMLDocument := nil;
  FHistoryXMLDocument := nil;
  FLastLogin := 0;
  IconFetched := False;
  ImageIndex := -1;
  ManualURL := '';
  ManualURLFetched := False;
  Modified := False;
  Startup := '';

  Connection := TSConnection.Create(Self);
  FDesktop := nil;
end;

destructor TSAccount.Destroy();
begin
  if (Assigned(FDesktop)) then FDesktop.Free();
  Connection.Free();

  inherited;
end;

function TSAccount.Frame(): Pointer;
begin
  if (Length(FDesktops) = 0) then
    Result := nil
  else
    Result := FDesktops[0].Control;
end;

function TSAccount.FullAddress(const AAddress: string): string;
var
  Buffer: array[0 .. INTERNET_MAX_URL_LENGTH] of Char;
  Size: Cardinal;
  URLComponents: URL_COMPONENTS;
begin
  Result := AAddress;

  if ((Copy(Result, 1, 1) = '/') and (Copy(Result, 1, 2) <> '//')) then
  begin
    ZeroMemory(@URLComponents, SizeOf(URLComponents));
    URLComponents.dwStructSize := SizeOf(URLComponents);

    URLComponents.lpszScheme := PChar('mysql');
    URLComponents.dwSchemeLength := StrLen(URLComponents.lpszScheme);
    URLComponents.lpszHostName := PChar(Connection.Host);
    URLComponents.dwHostNameLength := Length(Connection.Host);
    if (Connection.Port <> MYSQL_PORT) then
      URLComponents.nPort := Connection.Port;
    URLComponents.lpszUrlPath := PChar(AAddress);
    URLComponents.dwUrlPathLength := StrLen(URLComponents.lpszUrlPath);

    Size := SizeOf(Buffer);
    if (not InternetCreateUrl(URLComponents, ICU_ESCAPE, @Buffer, Size)) then
      raise EConvertError.CreateFmt(SConvStrParseError, [AAddress]);
    SetString(Result, PChar(@Buffer), Size);
  end;
  if (Copy(Result, 1, 2) = '//') then
    Result := 'mysql:' + Result;
end;

function TSAccount.GetBookmarksFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'Bookmarks.xml';
end;

function TSAccount.GetDataPath(): TFileName;
begin
  Result := Accounts.DataPath + ReplaceStr(Name, '/', '_') + PathDelim;
end;

function TSAccount.GetDefaultDatabase(): string;
var
  DatabaseNames: TCSVStrings;
  Found: Boolean;
  I: Integer;
  URI: TUURI;
begin
  Result := '';

  if (Assigned(Desktop)) then
  begin
    URI := TUURI.Create(Desktop.Address);

    if (ValidDatabaseName(URI.Database)) then
    begin
      Result := URI.Database;

      if (Connection.Database <> '') then
      begin
        SetLength(DatabaseNames, 0);
        CSVSplitValues(Connection.Database, ',', '"', DatabaseNames);

        Found := False;
        for I := 0 to Length(DatabaseNames) - 1 do
          if (lstrcmpi(PChar(DatabaseNames[I]), PChar(Result)) = 0) then
            Found := True;
        if (not Found) then
          Result := '';

        SetLength(DatabaseNames, 0);
      end;
    end;

    URI.Free();

    if (Result = '') then
    begin
      SetLength(DatabaseNames, 0);
      CSVSplitValues(Connection.Database, ',', '"', DatabaseNames);

      if (Length(DatabaseNames) = 0) then
        Result := ''
      else
        Result := DatabaseNames[0];

      SetLength(DatabaseNames, 0);
    end;
  end;
end;

function TSAccount.GetDesktop(): TSDesktop;
begin
  if (not Assigned(FDesktop) and Assigned(DesktopXML)) then
    FDesktop := TSDesktop.Create(Self);

  Result := FDesktop;
end;

function TSAccount.GetDesktopFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'Desktop.xml';
end;

function TSAccount.GetDesktopXML(): IXMLNode;
begin
  if (not Assigned(FDesktopXMLDocument)) then
  begin
    if (FileExists(DesktopFilename)) then
      try
        FDesktopXMLDocument := LoadXMLDocument(DesktopFilename);
      except
        FDesktopXMLDocument := nil;
      end;

    if (not Assigned(FDesktopXMLDocument)) then
    begin
      FDesktopXMLDocument := NewXMLDocument();
      FDesktopXMLDocument.Encoding := 'utf-8';
      FDesktopXMLDocument.Node.AddChild('desktop').Attributes['version'] := '1.1';
    end;

    FDesktopXMLDocument.Options := FDesktopXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FDesktopXMLDocument.DocumentElement;
end;

function TSAccount.GetHistoryFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'History.xml'
end;

function TSAccount.GetHistoryXML(): IXMLNode;
begin
  if (not Assigned(FHistoryXMLDocument)) then
  begin
    CoInitialize(nil);

    if (FileExists(HistoryFilename)) then
      try
        FHistoryXMLDocument := LoadXMLDocument(HistoryFilename);
      except
        FHistoryXMLDocument := nil;
      end;

    if (not Assigned(FHistoryXMLDocument)) then
    begin
      FHistoryXMLDocument := NewXMLDocument();
      FHistoryXMLDocument.Encoding := 'utf-8';
      FHistoryXMLDocument.Node.AddChild('history').Attributes['version'] := '1.0';
    end;

    FHistoryXMLDocument.Options := FHistoryXMLDocument.Options - [doAttrNull, doNodeAutoCreate];

    CoUninitialize();
  end;

  Result := FHistoryXMLDocument.DocumentElement;
end;

function TSAccount.GetIconFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'favicon.ico';
end;

function TSAccount.GetIndex(): Integer;
begin
  Result := Accounts.IndexOf(Self);
end;

function TSAccount.GetName(): string;
begin
  if (FName = '') and (Assigned(XML)) then
    FName := XML.Attributes['name'];

  Result := FName;
end;

function TSAccount.GetXML(): IXMLNode;
var
  I: Integer;
begin
  if (not Assigned(FXML) and Assigned(Accounts) and Assigned(Accounts.XML) and (FName <> '')) then
  begin
    for I := 0 to Accounts.XML.ChildNodes.Count - 1 do
      if ((Accounts.XML.ChildNodes[I].NodeName = 'account') and (lstrcmpi(PChar(string(Accounts.XML.ChildNodes[I].Attributes['name'])), PChar(FName)) = 0)) then
        FXML := Accounts.XML.ChildNodes[I];
    if (not Assigned(FXML) and (doNodeAutoCreate in Accounts.XML.OwnerDocument.Options)) then
    begin
      FXML := Accounts.XML.AddChild('account');
      FXML.Attributes['name'] := FName;
    end;
  end;

  Result := FXML;
end;

procedure TSAccount.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'cache/size'))) then TryStrToInt(XMLNode(XML, 'cache/size').Text, CacheSize);
    if (Assigned(XMLNode(XML, 'iconfetched'))) then TryStrToBool(XMLNode(XML, 'iconfetched').Text, IconFetched);
    if (Assigned(XMLNode(XML, 'lastlogin'))) then
      TryStrToFloat(ReplaceStr(XMLNode(XML, 'lastlogin').Text, '.', FormatSettings.DecimalSeparator), Double(FLastLogin));
    if (Assigned(XMLNode(XML, 'manualurl'))) then ManualURL := XMLNode(XML, 'manualurl').Text;
    if (Assigned(XMLNode(XML, 'manualurlfetched'))) then TryStrToBool(XMLNode(XML, 'manualurlfetched').Text, ManualURLFetched);
    if (Assigned(XMLNode(XML, 'limit'))) then
      if (UpperCase(XMLNode(XML, 'limit').Text) = 'REMEMBER') then DefaultLimit := dlRemember
      else if (UpperCase(XMLNode(XML, 'limit').Text) = 'ON') then DefaultLimit := dlOn
      else DefaultLimit := dlOff;
    if (Assigned(XMLNode(XML, 'sorting'))) then TryStrToBool(XMLNode(XML, 'sorting').Text, DefaultSorting);
    if (Assigned(XMLNode(XML, 'startup'))) then Startup := XMLNode(XML, 'startup').Text;

    Modified := False;

    Connection.LoadFromXML();
    if (Assigned(Desktop)) then
      Desktop.LoadFromXML(); // Client muss geladen sein, damit FullAddress funktioniert
  end;
end;

function TSAccount.PackAddress(const AAddress: string): string;
var
  URI: TUURI;
begin
  Result := AAddress;

  try
    URI := TUURI.Create(Result);

    if (URI.Scheme = 'mysql') then
      Delete(Result, 1, Length('mysql') + 1);
    if ((URI.Host = LowerCase(Connection.Host)) and ((URI.Port = 0) or (URI.Port = Connection.Port))) then
    begin
      Delete(Result, 1, 2);
      if (Pos('/', Result) = 0) then
        Result := ''
      else
        Delete(Result, 1, Pos('/', Result) - 1);
    end;

    URI.Free();
  except
  end;
end;

procedure TSAccount.RegisterDesktop(const AControl: Pointer; const AEventProc: TEventProc);
begin
  SetLength(FDesktops, Length(FDesktops) + 1);
  FDesktops[Length(FDesktops) - 1].Control := AControl;
  FDesktops[Length(FDesktops) - 1].AccountEventProc := AEventProc;
end;

procedure TSAccount.SaveToXML();
begin
  if (Assigned(XML)) then
  begin
    XMLNode(XML, 'cache/size').Text := IntToStr(CacheSize);
    XMLNode(XML, 'iconfetched').Text := BoolToStr(IconFetched, True);
    XMLNode(XML, 'lastlogin').Text := FloatToStr(LastLogin);
    case (DefaultLimit) of
      dlOff: XMLNode(XML, 'limit').Text := '';
      dlRemember: XMLNode(XML, 'limit').Text := 'Remember';
      else XMLNode(XML, 'limit').Text := 'On';
    end;
    XMLNode(XML, 'manualurl').Text := ManualURL;
    XMLNode(XML, 'manualurlfetched').Text := BoolToStr(ManualURLFetched, True);
    XMLNode(XML, 'sorting').Text := BoolToStr(DefaultSorting, True);
    XMLNode(XML, 'startup').Text := Startup;

    Connection.SaveToXML();
    if (Assigned(Desktop)) then
      Desktop.SaveToXML();

    if (ForceDirectories(DataPath)) then
    begin
      if (Assigned(DesktopXMLDocument) and DesktopXMLDocument.Modified) then
        if (ForceDirectories(ExtractFilePath(DesktopFilename))) then
          DesktopXMLDocument.SaveToFile(DesktopFilename);
      if (Assigned(HistoryXMLDocument) and HistoryXMLDocument.Modified) then
        if (ForceDirectories(ExtractFilePath(HistoryFilename))) then
          HistoryXMLDocument.SaveToFile(HistoryFilename);
    end;

    Modified := False;
 end;
end;

procedure TSAccount.AccountEvent(const ClassType: TClass);
var
  I: Integer;
begin
  for I := 0 to Length(FDesktops) - 1 do
    if (Assigned(FDesktops[I].AccountEventProc)) then
      FDesktops[I].AccountEventProc(ClassType);
end;

procedure TSAccount.SetLastLogin(const ALastLogin: TDateTime);
begin
  FLastLogin := ALastLogin;

  Modified := True;
end;

procedure TSAccount.SetName(const AName: string);
begin
  Assert(not Assigned(FXML) or (AName = FXML.Attributes['name']));

  
  FName := AName;
end;

procedure TSAccount.UnRegisterDesktop(const AControl: Pointer);
var
  I: Integer;
  J: Integer;
begin
  I := 0;
  while (I < Length(FDesktops)) do
    if (FDesktops[I].Control <> AControl) then
      Inc(I)
    else
    begin
      for J := I to Length(FDesktops) - 2 do
        FDesktops[J] := FDesktops[J + 1];
      SetLength(FDesktops, Length(FDesktops) - 1);
    end;
end;

function TSAccount.ValidDatabaseName(const ADatabaseName: string): Boolean;
var
  S: string;
  TempDatabaseName: string;
begin
  Result := False;
  S := Connection.Database;

  while (S <> '') do
    if (Pos(',', S) = 0) then
      begin
        if (S = ADatabaseName) then
          Result := True;
        S := '';
      end
    else
      begin
        TempDatabaseName := Copy(S, 1, Pos(',', S) - 1);
        Delete(S, 1, Pos(',', S));
        if (TempDatabaseName = ADatabaseName) then
          Result := True;
      end;
end;

{ TSAccounts ******************************************************************}

procedure TSAccounts.AddAccount(const NewAccount: TSAccount);
begin
  if (not Assigned(AccountByName(NewAccount.Name))) then
  begin
    Add(TSAccount.Create(Self));
    Account[Count - 1].Assign(NewAccount);

    AppendIconsToImageList(Preferences.SmallImages, NewAccount);
  end;
end;

procedure TSAccounts.AppendIconsToImageList(const AImageList: TImageList; const AAccount: TSAccount = nil);
var
  I: Integer;
  Icon: TIcon;
begin
  if (Assigned(AImageList)) then
  begin
    Icon := TIcon.Create();

    for I := 0 to Count - 1 do
      if (not Assigned(AAccount) or (Account[I] = AAccount)) then
        if (FileExists(Account[I].IconFilename)) then
          try
            Icon.LoadFromFile(Account[I].IconFilename);
            Account[I].ImageIndex := AImageList.Count;
            ImageList_AddIcon(AImageList.Handle, Icon.Handle);
          except
            Account[I].ImageIndex := -1;
          end
        else
          Account[I].ImageIndex := -1;

    Icon.Free();
  end;
end;

procedure TSAccounts.Clear();
begin
  while (Count > 0) do
  begin
    Account[0].Free();
    Delete(0);
  end;

  inherited;
end;

constructor TSAccounts.Create(const ADBLogin: TLogin; const AOnSQLError: TMySQLConnection.TErrorEvent);
begin
  inherited Create();

  FDBLogin := ADBLogin;
  FOnSQLError := AOnSQLError;

  Section := 'Accounts';

  LoadFromXML();

  AppendIconsToImageList(Preferences.SmallImages);
end;

function TSAccounts.DeleteAccount(const AAccount: TSAccount): Boolean;
var
  I: Integer;
  Index: Integer;
begin
  if (FileExists(AAccount.DesktopFilename)) then
    DeleteFile(PChar(AAccount.DesktopFilename));
  if (FileExists(AAccount.HistoryFilename)) then
    DeleteFile(PChar(AAccount.HistoryFilename));
  if (FileExists(AAccount.IconFilename)) then
    DeleteFile(PChar(AAccount.IconFilename));
  if (DirectoryExists(AAccount.DataPath)) then
    RemoveDirectory(PChar(AAccount.DataPath));

  for I := XML.ChildNodes.Count - 1 downto 0 do
    if ((XML.ChildNodes[I].NodeName = 'account') and (lstrcmpi(PChar(string(XML.ChildNodes[I].Attributes['name'])), PChar(AAccount.Name)) = 0)) then
      XML.ChildNodes.Remove(XML.ChildNodes[I]);

  Index := IndexOf(AAccount);

  Account[Index].Free();
  Delete(Index);

  SaveToXML();

  Result := True;
end;

destructor TSAccounts.Destroy();
begin
  SaveToXML();

  Clear();

  inherited;
end;

function TSAccounts.GetDataPath(): TFileName;
begin
  Result := Preferences.UserPath + 'Accounts' + PathDelim;
end;

function TSAccounts.GetDefault(): TSAccount;
begin
  Result := AccountByName(DefaultAccountName);
end;

function TSAccounts.GetFilename(): TFileName;
begin
  Result := DataPath + 'Accounts.xml';
end;

function TSAccounts.GetFAccounts(Index: Integer): TSAccount;
begin
  Result := TSAccount(Items[Index]);
end;

function TSAccounts.GetXML(): IXMLNode;
var
  I: Integer;
begin
  if (not Assigned(FXMLDocument)) then
  begin
    if (FileExists(Filename)) then
    begin
      FXMLDocument := LoadXMLDocument(Filename);

      if (VersionStrToVersion(FXMLDocument.DocumentElement.Attributes['version']) < 10003) then
      begin
        for I := 0 to FXMLDocument.DocumentElement.ChildNodes.Count - 1 do
          if ((FXMLDocument.DocumentElement.ChildNodes[I].NodeName = 'account')
            and Assigned(XMLNode(FXMLDocument.DocumentElement.ChildNodes[I], 'connection/library/type'))
            and (UpperCase(XMLNode(FXMLDocument.DocumentElement.ChildNodes[I], 'connection/library/type').Text) = 'HTTPTUNNEL')
            and (LowerCase(XMLNode(FXMLDocument.DocumentElement.ChildNodes[I], 'connection/host').Text) = LOCAL_HOST)) then
          XMLNode(FXMLDocument.DocumentElement.ChildNodes[I], 'iconfetched').Text := BoolToStr(False, True);

        FXMLDocument.DocumentElement.Attributes['version'] := '1.0.3';
      end;
    end
    else
    begin
      FXMLDocument := NewXMLDocument();
      FXMLDocument.Encoding := 'utf-8';
      FXMLDocument.Node.AddChild('accounts').Attributes['version'] := '1.0.3';
    end;

    FXMLDocument.Options := FXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FXMLDocument.DocumentElement;
end;

procedure TSAccounts.LoadFromXML();
var
  I: Integer;
  Index: Integer;
  J: Integer;
begin
  Clear();

  if (Assigned(XML)) then
  begin
    FXMLDocument.Options := FXMLDocument.Options - [doNodeAutoCreate];

    for I := 0 to XML.ChildNodes.Count - 1 do
    begin
      if ((XML.ChildNodes[I].NodeName = 'account') and (XML.ChildNodes[I].Attributes['name'] <> '')) then
      begin
        Index := TList(Self).Count;
        for J := TList(Self).Count - 1 downto 0 do
          if (lstrcmpi(PChar(string(XML.ChildNodes[I].Attributes['name'])), PChar(Account[J].Name)) <= 0) then
            Index := J;

        Insert(Index, TSAccount.Create(Self, XML.ChildNodes[I]));
        Account[Index].LoadFromXML();
      end;
    end;

    if (Assigned(XMLNode(XML, 'default'))) then
      DefaultAccountName := XMLNode(XML, 'default').Text;

    FXMLDocument.Options := FXMLDocument.Options + [doNodeAutoCreate];
  end;
end;

procedure TSAccounts.SaveToXML();
var
  I: Integer;
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  for I := 0 to Count - 1 do
    if (Account[I].Modified) then
      Account[I].SaveToXML();

  if (Assigned(XML)) then
    XMLNode(XML, 'default').Text := DefaultAccountName;

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];


  if (Assigned(FXMLDocument) and FXMLDocument.Modified) then
    if (ForceDirectories(ExtractFilePath(Filename))) then
      FXMLDocument.SaveToFile(Filename);
end;

function TSAccounts.AccountByName(const AccountName: string): TSAccount;
var
  I: Integer;
begin
  Result := nil;

  for I:=0 to Count - 1 do
    if (Account[I].Name = AccountName) then
      Result := Account[I];
end;

function TSAccounts.AccountByURI(const AURI: string): TSAccount;
var
  Found: Integer;
  Host: string;
  I: Integer;
  Name: string;
  NewAccount: TSAccount;
  NewAccountName: string;
  URI: TUURI;
  URLComponents: TURLComponents;
begin
  Result := nil;

  if (LowerCase(Copy(AURI, 1, 8)) = 'mysql://') then
    URI := TUURI.Create(AURI)
  else
    URI := nil;

  if (Assigned(URI)) then
  begin
    Found := 0;
    for I := 0 to Count - 1 do
    begin
      ZeroMemory(@URLComponents, SizeOf(URLComponents));
      URLComponents.dwStructSize := SizeOf(URLComponents);
      if ((Account[I].Connection.LibraryType <> ltHTTP) or (lstrcmpi(PChar(Account[I].Connection.Host), LOCAL_HOST) <> 0)) then
        Host := LowerCase(Account[I].Connection.Host)
      else if (Account[I].Connection.Host = LOCAL_HOST_NAMEDPIPE) then
        Host := LOCAL_HOST
      else if (InternetCrackUrl(PChar(Account[I].Connection.HTTPTunnelURI), Length(Account[I].Connection.HTTPTunnelURI), 0, URLComponents)) then
      begin
        Inc(URLComponents.dwHostNameLength);
        GetMem(URLComponents.lpszHostName, URLComponents.dwHostNameLength * SizeOf(URLComponents.lpszHostName[0]));
        InternetCrackUrl(PChar(Account[I].Connection.HTTPTunnelURI), Length(Account[I].Connection.HTTPTunnelURI), 0, URLComponents);
        SetString(Host, URLComponents.lpszHostName, URLComponents.dwHostNameLength);
        FreeMem(URLComponents.lpszHostName);
      end
      else
        Host := LOCAL_HOST;
      if ((lstrcmpi(PChar(Host), PChar(URI.Host)) = 0) and (URI.Port = Account[I].Connection.Port)
        and ((URI.Username = '') or (lstrcmpi(PChar(URI.Username), PChar(Account[I].Connection.User)) = 0))) then
      begin
        Result := Account[I];
        Inc(Found);
      end;
    end;

    if (Found = 0) then
    begin
      NewAccountName := URI.Host;
      if (URI.Username <> '') then
        NewAccountName := NewAccountName + ' (' + URI.Username + ')';
      Name := NewAccountName;
      I := 1;
      while (Assigned(AccountByName(Name))) do
      begin
        Inc(I);
        Name := NewAccountName + ' (' + IntToStr(I) + ')';
      end;

      NewAccount := TSAccount.Create(Self);
      NewAccount.Name := Name;
      NewAccount.Connection.Host := URI.Host;
      NewAccount.Connection.Port := URI.Port;
      NewAccount.Connection.User := URI.Username;
      NewAccount.Connection.Password := URI.Password;
      NewAccount.Connection.Database := URI.Database;
      AddAccount(NewAccount);
      NewAccount.Free();

      Result := AccountByName(NewAccountName);

      SaveToXML();
    end
    else if (Found > 1) then
      Result := nil;

    URI.Free();
  end;
end;

procedure TSAccounts.SetDefault(const AAccount: TSAccount);
begin
  if (not Assigned(AAccount)) then
    DefaultAccountName := ''
  else
    DefaultAccountName := AAccount.Name;
end;

procedure TSAccounts.UpdateAccount(const Account, NewAccount: TSAccount);
begin
  if (Assigned(Account) and Assigned(NewAccount) and (not Assigned(AccountByName(NewAccount.Name)) or (NewAccount.Name = Account.Name))) then
  begin
    if (Assigned(Account.XML)) then
      Account.XML.Attributes['name'] := NewAccount.Name;

    if (NewAccount.Name <> Account.Name) then
      if (DirectoryExists(Account.DataPath)) then
        RenameFile(Account.DataPath, NewAccount.DataPath);

    Account.Assign(NewAccount);

    AppendIconsToImageList(Preferences.SmallImages, NewAccount);

    SaveToXML();
  end;
end;

initialization
  WSAData.wVersion := 0;
finalization
  if (WSAData.wVersion <> 0) then
    WSACleanup();
end.
