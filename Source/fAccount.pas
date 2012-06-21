unit fAccount;

interface {********************************************************************}

uses
  Forms, Classes, XMLIntf, XMLDoc, Controls, ComCtrls, SysUtils, IniFiles,
  fPreferences, MySQLDB;

type
  TABookmarks = class;
  TADesktop = class;
  TAConnection = class;
  TAAccount = class;
  TAAccounts = class;

  TABookmark = class
  private
    FBookmarks: TABookmarks;
    FURI: string;
    FXML: IXMLNode;
    function GetXML(): IXMLNode;
  protected
    property XML: IXMLNode read GetXML;
  public
    Caption: string;
    procedure Assign(const Source: TABookmark); virtual;
    constructor Create(const ABookmarks: TABookmarks; const AXML: IXMLNode = nil); virtual;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
    property Bookmarks: TABookmarks read FBookmarks;
    property URI: string read FURI write FURI;
  end;

  TABookmarks = class
  private
    FXML: IXMLNode;
    FBookmarks: array of TABookmark;
    FDesktop: TADesktop;
    function GetBookmark(Index: Integer): TABookmark;
    function GetDataPath(): TFileName;
    function GetXML(): IXMLNode;
  protected
    property Desktop: TADesktop read FDesktop;
    property XML: IXMLNode read GetXML;
  public
    function AddBookmark(const NewBookmark: TABookmark): Boolean; virtual;
    function ByCaption(const Caption: string): TABookmark; virtual;
    procedure Clear(); virtual;
    constructor Create(const ADesktop: TADesktop);
    function Count(): Integer; virtual;
    function DeleteBookmark(const Bookmark: TABookmark): Boolean; virtual;
    destructor Destroy(); override;
    function IndexOf(const Bookmark: TABookmark): Integer; virtual;
    procedure LoadFromXML(); virtual;
    procedure MoveBookmark(const Bookmark: TABookmark; const NewIndex: Integer); virtual;
    procedure SaveToXML(); virtual;
    function UpdateBookmark(const Bookmark, NewBookmark: TABookmark): Boolean; virtual;
    property Bookmark[Index: Integer]: TABookmark read GetBookmark; default;
    property DataPath: TFileName read GetDataPath;
  end;

  TADesktop = class
  type
    TListViewKind = (lkServer, lkDatabase, lkTable, lkHosts, lkProcesses, lkStati, lkUsers, lkVariables);
  private
    FAccount: TAAccount;
    FBookmarks: TABookmarks;
    FPath: string;
    FXML: IXMLNode;
    function GetAddress(): string;
    function GetXML(): IXMLNode;
    procedure SetAddress(AAddress: string);
  protected
    procedure Assign(const Source: TADesktop); virtual;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
    property Account: TAAccount read FAccount;
    property XML: IXMLNode read GetXML;
  public
    AddressMRU: TMRUList;
    BookmarksVisible: Boolean;
    ColumnWidths: array [lkServer .. lkVariables] of array [0..7] of Integer;
    DataHeight, BlobHeight: Integer;
    EditorContent: string;
    ExplorerVisible: Boolean;
    FilesFilter: string;
    FoldersHeight: Integer;
    LogHeight: Integer;
    LogVisible: Boolean;
    NavigatorVisible: Boolean;
    SelectorWitdth: Integer;
    SQLHistoryVisible: Boolean;
    constructor Create(const AAccount: TAAccount); overload; virtual;
    destructor Destroy(); override;
    property Address: string read GetAddress write SetAddress;
    property Bookmarks: TABookmarks read FBookmarks;
  end;

  TAConnection = class
  private
    FXML: IXMLNode;
    FAccount: TAAccount;
    function GetXML(): IXMLNode;
  protected
    Section: string;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
    property XML: IXMLNode read GetXML;
  public
    Asynchron: Boolean;
    Charset: string;
    Database: string;
    Host: string;
    HTTPTunnelURI: string;
    LibraryFilename: TFileName;
    LibraryType: TMySQLLibrary.TLibraryType;
    MultiStatements: Boolean;
    Password: string;
    PipeName: string;
    Port: Integer;
    SavePassword: Boolean;
    UseInformationSchema: Boolean;
    User: string;
    procedure Assign(const Source: TAConnection); virtual;
    constructor Create(const AAccount: TAAccount); virtual;
    property Account: TAAccount read FAccount;
  end;

  TAAccount = class
  type
    TEventProc = procedure (const ClassType: TClass) of object;
    TDesktop = record
      Control: Pointer;
      AccountEventProc: TEventProc;
    end;
  private
    FDesktop: TADesktop;
    FDesktopXMLDocument: IXMLDocument;
    FHistoryXMLDocument: IXMLDocument;
    FLastLogin: TDateTime;
    FName: string;
    FDesktops: array of TDesktop;
    FXML: IXMLNode;
    FAccounts: TAAccounts;
    Modified: Boolean;
    function GetBookmarksFilename(): TFileName;
    function GetDataPath(): TFileName;
    function GetDesktop(): TADesktop;
    function GetDesktopCount(): Integer;
    function GetDesktopFilename(): TFileName;
    function GetDesktopXML(): IXMLNode;
    function GetHistoryFilename(): TFileName;
    function GetHistoryXML(): IXMLNode;
    function GetIconFilename(): TFileName;
    function GetImageIndex(): Integer;
    function GetName(): string;
    function GetXML(): IXMLNode;
    procedure SetLastLogin(const ALastLogin: TDateTime);
    procedure SetName(const AName: string);
  protected
    FImageIndex: Integer;
    Section: string;
    function GetIndex(): Integer;
    procedure LoadFromXML();
    procedure SaveToXML(); virtual;
    procedure AccountEvent(const ClassType: TClass); virtual;
    function ValidDatabaseName(const ADatabaseName: string): Boolean;
    property BookmarksFilename: TFileName read GetBookmarksFilename;
    property DesktopFilename: TFileName read GetDesktopFilename;
    property DesktopXMLDocument: IXMLDocument read FDesktopXMLDocument;
    property HistoryFilename: TFileName read GetHistoryFilename;
    property HistoryXMLDocument: IXMLDocument read FHistoryXMLDocument;
    property XML: IXMLNode read GetXML;
  public
    CacheSize: Integer;
    Connection: TAConnection;
    IconFetched: Boolean;
    ManualURL: string;
    ManualURLFetched: Boolean;
    procedure Assign(const Source: TAAccount); virtual;
    constructor Create(const AAccounts: TAAccounts; const AXML: IXMLNode = nil); virtual;
    destructor Destroy(); override;
    function ExtractPath(const AAddress: string): string; virtual;
    function FullAddress(const APath: string): string; virtual;
    function Frame(): Pointer; virtual;
    function GetDefaultDatabase(): string; virtual;
    procedure RegisterDesktop(const AControl: Pointer; const AEventProc: TEventProc); virtual;
    procedure UnRegisterDesktop(const AControl: Pointer); virtual;
    property Accounts: TAAccounts read FAccounts;
    property DataPath: TFileName read GetDataPath;
    property Desktop: TADesktop read GetDesktop;
    property DesktopCount: Integer read GetDesktopCount;
    property DesktopXML: IXMLNode read GetDesktopXML;
    property HistoryXML: IXMLNode read GetHistoryXML;
    property IconFilename: TFileName read GetIconFilename;
    property ImageIndex: Integer read GetImageIndex write FImageIndex;
    property Index: Integer read GetIndex;
    property LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property Name: string read GetName write SetName;
  end;

  TAAccounts = class(TList)
  type
    TDBLogin = function(const Account: Pointer): Boolean of object;
  private
    DefaultAccountName: string;
    FDBLogin: TDBLogin;
    FXMLDocument: IXMLDocument;
    function GetDataPath(): TFileName;
    function GetDefault(): TAAccount; inline;
    function GetFilename(): TFileName;
    function GetXML(): IXMLNode;
    function GetFAccounts(Index: Integer): TAAccount; inline;
    procedure SetDefault(const AAccount: TAAccount);
  protected
    Section: string;
    property DataPath: TFileName read GetDataPath;
    property Filename: TFileName read GetFilename;
    property XML: IXMLNode read GetXML;
  public
    function AccountByName(const AccountName: string): TAAccount; virtual;
    function AccountByURI(const AURI: string; const DefaultAccount: TAAccount = nil): TAAccount; virtual;
    procedure AddAccount(const NewAccount: TAAccount); virtual;
    procedure AppendIconsToImageList(const AImageList: TImageList; const AAccount: TAAccount = nil); virtual;
    procedure Clear(); override;
    constructor Create(const ADBLogin: TDBLogin);
    function DeleteAccount(const AAccount: TAAccount): Boolean; virtual;
    destructor Destroy(); override;
    procedure LoadFromXML(); virtual;
    procedure SaveToXML(); virtual;
    procedure UpdateAccount(const Account, NewAccount: TAAccount); virtual;
    property Account[Index: Integer]: TAAccount read GetFAccounts; default;
    property Default: TAAccount read GetDefault write SetDefault;
    property DBLogin: TDBLogin read FDBLogin;
  end;

var
  Accounts: TAAccounts;

implementation {***************************************************************}

uses
  Windows, Graphics, CommCtrl, Consts, SysConst, ShlObj, WinINet, ActiveX,
  Variants, RTLConsts, StrUtils, Registry, WinSock,
  MySQLConsts,
  CSVUtils,
  fURI;

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

{ TABookmark ******************************************************************}

procedure TABookmark.Assign(const Source: TABookmark);
begin
  if (not Assigned(Bookmarks) and Assigned(Source.Bookmarks)) then
    FBookmarks := Source.Bookmarks;

  Caption := Source.Caption;
  URI := Source.URI;

  if (Assigned(XML)) then
    XML.Attributes['name'] := Caption;
end;

constructor TABookmark.Create(const ABookmarks: TABookmarks; const AXML: IXMLNode = nil);
begin
  FBookmarks := ABookmarks;
  FXML := AXML;

  Caption := '';
  FURI := '';
end;

function TABookmark.GetXML(): IXMLNode;
var
  I: Integer;
begin
  if (not Assigned(FXML)) then
    for I := 0 to Bookmarks.XML.ChildNodes.Count - 1 do
      if ((Bookmarks.XML.ChildNodes[I].NodeName = 'bookmark') and (lstrcmpi(PChar(string(Bookmarks.XML.ChildNodes[I].Attributes['name'])), PChar(Caption)) = 0)) then
        FXML := Bookmarks.XML.ChildNodes[I];

  Result := FXML;
end;

procedure TABookmark.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    Caption := XML.Attributes['name'];
    if (Assigned(XMLNode(XML, 'uri'))) then FURI := Bookmarks.Desktop.Account.FullAddress(XMLNode(XML, 'uri').Text);
  end;
end;

procedure TABookmark.SaveToXML();
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XML.Attributes['name'] := Caption;
  XMLNode(XML, 'uri').Text := Bookmarks.Desktop.Account.ExtractPath(URI);

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];
end;

{ TABookmarks *****************************************************************}

function TABookmarks.AddBookmark(const NewBookmark: TABookmark): Boolean;
begin
  Result := IndexOf(NewBookmark) < 0;

  if (Result) then
  begin
    SetLength(FBookmarks, Count + 1);

    FBookmarks[Count - 1] := TABookmark.Create(Self, XML.AddChild('bookmark'));
    FBookmarks[Count - 1].Assign(NewBookmark);

    Desktop.Account.AccountEvent(ClassType);
  end;
end;

function TABookmarks.ByCaption(const Caption: string): TABookmark;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if (lstrcmpi(PChar(FBookmarks[I].Caption), PChar(Caption)) = 0) then
      Result := FBookmarks[I];
end;

procedure TABookmarks.Clear();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FBookmarks[I].Free();
  SetLength(FBookmarks, 0);
end;

function TABookmarks.Count(): Integer;
begin
  Result := Length(FBookmarks);
end;

constructor TABookmarks.Create(const ADesktop: TADesktop);
begin
  inherited Create();

  FDesktop := ADesktop;

  FXML := nil;
  SetLength(FBookmarks, 0);
end;

function TABookmarks.DeleteBookmark(const Bookmark: TABookmark): Boolean;
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

destructor TABookmarks.Destroy();
begin
  Clear();

  inherited;
end;

function TABookmarks.GetBookmark(Index: Integer): TABookmark;
begin
  Result := FBookmarks[Index];
end;

function TABookmarks.GetDataPath(): TFileName;
begin
  Result := Desktop.Account.DataPath;
end;

function TABookmarks.GetXML(): IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(Desktop.XML)) then
    FXML := XMLNode(Desktop.XML, 'bookmarks', True);

  Result := FXML;
end;

function TABookmarks.IndexOf(const Bookmark: TABookmark): Integer;
var
  I: Integer;
begin
  Result := -1;

  if (Assigned(Bookmark)) then
    for I := 0 to Count - 1 do
      if (lstrcmpi(PChar(FBookmarks[I].Caption), PChar(Bookmark.Caption)) = 0) then
        Result := I;
end;

procedure TABookmarks.LoadFromXML();
var
  I: Integer;
begin
  Clear();

  if (Assigned(XML)) then
    for I := 0 to XML.ChildNodes.Count - 1 do
      if ((XML.ChildNodes[I].NodeName = 'bookmark') and not Assigned(ByCaption(XML.ChildNodes[I].Attributes['name']))) then
      begin
        SetLength(FBookmarks, Count + 1);
        FBookmarks[Count - 1] := TABookmark.Create(Self, XML.ChildNodes[I]);

        FBookmarks[Count - 1].LoadFromXML();
      end;
end;

procedure TABookmarks.MoveBookmark(const Bookmark: TABookmark; const NewIndex: Integer);
var
  I: Integer;
  Index: Integer;
  TempBookmark: TABookmark;
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

procedure TABookmarks.SaveToXML();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Bookmark[I].SaveToXML();
end;

function TABookmarks.UpdateBookmark(const Bookmark, NewBookmark: TABookmark): Boolean;
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

{ TADesktop *******************************************************************}

procedure TADesktop.Assign(const Source: TADesktop);
var
  I: Integer;
  Kind: TListViewKind;
begin
  Address := Account.FullAddress(Source.Account.ExtractPath(Source.Address));
  BlobHeight := Source.BlobHeight;
  BookmarksVisible := Source.BookmarksVisible;
  for Kind := lkServer to lkVariables do
    for I := 0 to Length(ColumnWidths[Kind]) - 1 do
      ColumnWidths[Kind, I] := Source.ColumnWidths[Kind, I];
  DataHeight := Source.DataHeight;
  EditorContent := Source.EditorContent;
  ExplorerVisible := Source.ExplorerVisible;
  FilesFilter := Source.FilesFilter;
  FoldersHeight := Source.FoldersHeight;
  LogHeight := Source.LogHeight;
  LogVisible := Source.LogVisible;
  NavigatorVisible := Source.NavigatorVisible;
  SelectorWitdth := Source.SelectorWitdth;
  SQLHistoryVisible := Source.SQLHistoryVisible;
end;

constructor TADesktop.Create(const AAccount: TAAccount);
var
  I: Integer;
  Kind: TListViewKind;
begin
  inherited Create();

  FAccount := AAccount;
  FXML := nil;

  BlobHeight := 100;
  BookmarksVisible := False;
  for Kind := lkServer to lkVariables do
    for I := 0 to Length(ColumnWidths[Kind]) - 1 do
      ColumnWidths[Kind][I] := ColumnTextWidth;
  DataHeight := 150;
  EditorContent := '';
  ExplorerVisible := False;
  FilesFilter := '*.sql';
  FoldersHeight := 100;
  NavigatorVisible := True;
  LogHeight := 80;
  LogVisible := False;
  FPath := '/';
  SelectorWitdth := 150;
  SQLHistoryVisible := False;

  AddressMRU := TMRUList.Create(10);

  FBookmarks := TABookmarks.Create(Self);
end;

destructor TADesktop.Destroy();
begin
  AddressMRU.Free();
  FBookmarks.Free();

  inherited;
end;

function TADesktop.GetAddress(): string;
begin
  Result := Account.FullAddress(FPath);
end;

function TADesktop.GetXML(): IXMLNode;
begin
  if (not Assigned(FXML)) then
    FXML := Account.DesktopXML;

  Result := FXML;
end;

procedure TADesktop.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'datagrid/height'))) then TryStrToInt(XMLNode(XML, 'datagrid/height').Text, DataHeight);
    if (Assigned(XMLNode(XML, 'datagrid/blob/height'))) then TryStrToInt(XMLNode(XML, 'datagrid/blob/height').Text, BlobHeight);
    if (Assigned(XMLNode(XML, 'editor/content'))) then EditorContent := XMLNode(XML, 'editor/content').Text;
    if (Assigned(XMLNode(XML, 'log/height'))) then TryStrToInt(XMLNode(XML, 'log/height').Text, LogHeight);
    if (Assigned(XMLNode(XML, 'log/visible'))) then TryStrToBool(XMLNode(XML, 'log/visible').Text, LogVisible);
    if (Assigned(XMLNode(XML, 'objects/server/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/name').Text, ColumnWidths[lkServer][0]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/size'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/size').Text, ColumnWidths[lkServer][1]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/count'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/count').Text, ColumnWidths[lkServer][2]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/created'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/created').Text, ColumnWidths[lkServer][3]);
    if (Assigned(XMLNode(XML, 'objects/server/widths/extras'))) then TryStrToInt(XMLNode(XML, 'objects/server/widths/extras').Text, ColumnWidths[lkServer][4]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/name').Text, ColumnWidths[lkDatabase][0]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/type'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/type').Text, ColumnWidths[lkDatabase][1]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/recordcount'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/recordcount').Text, ColumnWidths[lkDatabase][2]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/size'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/size').Text, ColumnWidths[lkDatabase][3]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/updated'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/updated').Text, ColumnWidths[lkDatabase][4]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/extras'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/extras').Text, ColumnWidths[lkDatabase][5]);
    if (Assigned(XMLNode(XML, 'objects/database/widths/comment'))) then TryStrToInt(XMLNode(XML, 'objects/database/widths/comment').Text, ColumnWidths[lkDatabase][6]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/name').Text, ColumnWidths[lkTable][0]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/type'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/type').Text, ColumnWidths[lkTable][1]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/null'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/null').Text, ColumnWidths[lkTable][2]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/default'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/default').Text, ColumnWidths[lkTable][3]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/extras'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/extras').Text, ColumnWidths[lkTable][4]);
    if (Assigned(XMLNode(XML, 'objects/table/widths/comment'))) then TryStrToInt(XMLNode(XML, 'objects/table/widths/comment').Text, ColumnWidths[lkTable][5]);
    if (Assigned(XMLNode(XML, 'objects/hosts/widths/host'))) then TryStrToInt(XMLNode(XML, 'objects/hosts/widths/host').Text, ColumnWidths[lkHosts][0]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/id'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/id').Text, ColumnWidths[lkProcesses][0]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/user'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/user').Text, ColumnWidths[lkProcesses][1]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/host'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/host').Text, ColumnWidths[lkProcesses][2]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/database'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/database').Text, ColumnWidths[lkProcesses][3]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/command'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/command').Text, ColumnWidths[lkProcesses][4]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/statement'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/statement').Text, ColumnWidths[lkProcesses][5]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/time'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/time').Text, ColumnWidths[lkProcesses][6]);
    if (Assigned(XMLNode(XML, 'objects/processes/widths/state'))) then TryStrToInt(XMLNode(XML, 'objects/processes/widths/state').Text, ColumnWidths[lkProcesses][7]);
    if (Assigned(XMLNode(XML, 'objects/stati/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/stati/widths/name').Text, ColumnWidths[lkStati][0]);
    if (Assigned(XMLNode(XML, 'objects/stati/widths/value'))) then TryStrToInt(XMLNode(XML, 'objects/stati/widths/value').Text, ColumnWidths[lkStati][1]);
    if (Assigned(XMLNode(XML, 'objects/users/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/users/widths/name').Text, ColumnWidths[lkUsers][0]);
    if (Assigned(XMLNode(XML, 'objects/users/widths/fullname'))) then TryStrToInt(XMLNode(XML, 'objects/users/widths/fullname').Text, ColumnWidths[lkUsers][1]);
    if (Assigned(XMLNode(XML, 'objects/users/widths/comment'))) then TryStrToInt(XMLNode(XML, 'objects/users/widths/comment').Text, ColumnWidths[lkUsers][2]);
    if (Assigned(XMLNode(XML, 'objects/variables/widths/name'))) then TryStrToInt(XMLNode(XML, 'objects/variables/widths/name').Text, ColumnWidths[lkVariables][0]);
    if (Assigned(XMLNode(XML, 'objects/variables/widths/value'))) then TryStrToInt(XMLNode(XML, 'objects/variables/widths/value').Text, ColumnWidths[lkVariables][1]);
    if (Assigned(XMLNode(XML, 'path'))) then FPath := XMLNode(XML, 'path').Text;
    if (Assigned(XMLNode(XML, 'sidebar/explorer/folders/height'))) then TryStrToInt(XMLNode(XML, 'sidebar/explorer/folders/height').Text, FoldersHeight);
    if (Assigned(XMLNode(XML, 'sidebar/explorer/files/filter'))) then FilesFilter := XMLNode(XML, 'sidebar/explorer/files/filter').Text;
    if (Assigned(XMLNode(XML, 'sidebar/width'))) then TryStrToInt(XMLNode(XML, 'sidebar/width').Text, SelectorWitdth);
    if (Assigned(XMLNode(XML, 'sidebar/visible'))) then
    begin
      NavigatorVisible := UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'NAVIGATOR';
      BookmarksVisible := not NavigatorVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'BOOKMARKS');
      SQLHistoryVisible := not NavigatorVisible and not BookmarksVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'SQL HISTORY');
      ExplorerVisible := not ExplorerVisible and not BookmarksVisible and not SQLHistoryVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'EXPLORER');
    end;

    Bookmarks.LoadFromXML();
  end;
end;

procedure TADesktop.SaveToXML();
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XMLNode(XML, 'datagrid/height').Text := IntToStr(DataHeight);
  XMLNode(XML, 'datagrid/blob/height').Text := IntToStr(BlobHeight);
  try
    XMLNode(XML, 'editor/content').Text := EditorContent;
  except
    XMLNode(XML, 'editor/content').Text := '';
  end;
  XMLNode(XML, 'editor/filename/mru').ChildNodes.Clear();
  XMLNode(XML, 'log/height').Text := IntToStr(LogHeight);
  XMLNode(XML, 'log/visible').Text := BoolToStr(LogVisible, True);
  XMLNode(XML, 'objects/server/widths/name').Text := IntToStr(ColumnWidths[lkServer][0]);
  XMLNode(XML, 'objects/server/widths/count').Text := IntToStr(ColumnWidths[lkServer][1]);
  XMLNode(XML, 'objects/server/widths/size').Text := IntToStr(ColumnWidths[lkServer][2]);
  XMLNode(XML, 'objects/server/widths/created').Text := IntToStr(ColumnWidths[lkServer][3]);
  XMLNode(XML, 'objects/server/widths/extras').Text := IntToStr(ColumnWidths[lkServer][4]);
  XMLNode(XML, 'objects/database/widths/name').Text := IntToStr(ColumnWidths[lkDatabase][0]);
  XMLNode(XML, 'objects/database/widths/type').Text := IntToStr(ColumnWidths[lkDatabase][1]);
  XMLNode(XML, 'objects/database/widths/recordcount').Text := IntToStr(ColumnWidths[lkDatabase][2]);
  XMLNode(XML, 'objects/database/widths/size').Text := IntToStr(ColumnWidths[lkDatabase][3]);
  XMLNode(XML, 'objects/database/widths/updated').Text := IntToStr(ColumnWidths[lkDatabase][4]);
  XMLNode(XML, 'objects/database/widths/extras').Text := IntToStr(ColumnWidths[lkDatabase][5]);
  XMLNode(XML, 'objects/database/widths/comment').Text := IntToStr(ColumnWidths[lkDatabase][6]);
  XMLNode(XML, 'objects/table/widths/name').Text := IntToStr(ColumnWidths[lkTable][0]);
  XMLNode(XML, 'objects/table/widths/type').Text := IntToStr(ColumnWidths[lkTable][1]);
  XMLNode(XML, 'objects/table/widths/null').Text := IntToStr(ColumnWidths[lkTable][2]);
  XMLNode(XML, 'objects/table/widths/default').Text := IntToStr(ColumnWidths[lkTable][3]);
  XMLNode(XML, 'objects/table/widths/extras').Text := IntToStr(ColumnWidths[lkTable][4]);
  XMLNode(XML, 'objects/table/widths/comment').Text := IntToStr(ColumnWidths[lkTable][5]);
  XMLNode(XML, 'objects/hosts/widths/host').Text := IntToStr(ColumnWidths[lkHosts][0]);
  XMLNode(XML, 'objects/processes/widths/id').Text := IntToStr(ColumnWidths[lkProcesses][0]);
  XMLNode(XML, 'objects/processes/widths/user').Text := IntToStr(ColumnWidths[lkProcesses][1]);
  XMLNode(XML, 'objects/processes/widths/host').Text := IntToStr(ColumnWidths[lkProcesses][2]);
  XMLNode(XML, 'objects/processes/widths/database').Text := IntToStr(ColumnWidths[lkProcesses][3]);
  XMLNode(XML, 'objects/processes/widths/command').Text := IntToStr(ColumnWidths[lkProcesses][4]);
  XMLNode(XML, 'objects/processes/widths/statement').Text := IntToStr(ColumnWidths[lkProcesses][5]);
  XMLNode(XML, 'objects/processes/widths/time').Text := IntToStr(ColumnWidths[lkProcesses][6]);
  XMLNode(XML, 'objects/processes/widths/state').Text := IntToStr(ColumnWidths[lkProcesses][7]);
  XMLNode(XML, 'objects/stati/widths/name').Text := IntToStr(ColumnWidths[lkStati][0]);
  XMLNode(XML, 'objects/stati/widths/value').Text := IntToStr(ColumnWidths[lkStati][1]);
  XMLNode(XML, 'objects/users/widths/name').Text := IntToStr(ColumnWidths[lkUsers][0]);
  XMLNode(XML, 'objects/users/widths/fullname').Text := IntToStr(ColumnWidths[lkUsers][1]);
  XMLNode(XML, 'objects/users/widths/comment').Text := IntToStr(ColumnWidths[lkUsers][2]);
  XMLNode(XML, 'objects/variables/widths/name').Text := IntToStr(ColumnWidths[lkVariables][0]);
  XMLNode(XML, 'objects/variables/widths/value').Text := IntToStr(ColumnWidths[lkVariables][1]);
  if (FPath = '/.') then
    raise ERangeError.Create(SRangeError);
  XMLNode(XML, 'path').Text := FPath;
  XMLNode(XML, 'sidebar/explorer/folders/height').Text := IntToStr(FoldersHeight);
  XMLNode(XML, 'sidebar/explorer/files/filter').Text := FilesFilter;
  XMLNode(XML, 'sidebar/width').Text := IntToStr(SelectorWitdth);
  if (NavigatorVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Navigator'
  else if (BookmarksVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Bookmarks'
  else if (SQLHistoryVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'SQL History'
  else if (ExplorerVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Explorer'
  else
    XMLNode(XML, 'sidebar/visible').Text := BoolToStr(False, True);

  Bookmarks.SaveToXML();

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];
end;

procedure TADesktop.SetAddress(AAddress: string);
begin
  FPath := Account.ExtractPath(AAddress);

  if (FPath = '/.') then
    raise ERangeError.Create(SRangeError);
end;

{ TAConnection ****************************************************************}

procedure TAConnection.Assign(const Source: TAConnection);
begin
  Asynchron := Source.Asynchron;
  Charset := Source.Charset;
  Database := Source.Database;
  Host := Source.Host;
  HTTPTunnelURI := Source.HTTPTunnelURI;
  LibraryFilename := Source.LibraryFilename;
  LibraryType := Source.LibraryType;
  MultiStatements := Source.MultiStatements;
  Password := Source.Password;
  PipeName := Source.PipeName;
  Port := Source.Port;
  SavePassword := Source.SavePassword;
  UseInformationSchema := Source.UseInformationSchema;
  User := Source.User;
end;

constructor TAConnection.Create(const AAccount: TAAccount);
begin
  inherited Create();

  FAccount := AAccount;
  FXML := nil;

  Asynchron := True;
  Charset := '';
  Database := '';
  Host := '';
  HTTPTunnelURI := '';
  LibraryFilename := 'libMySQL.dll';
  LibraryType := ltBuiltIn;
  MultiStatements := True;
  Password := '';
  PipeName := MYSQL_NAMEDPIPE;
  Port := MYSQL_PORT;
  SavePassword := False;
  UseInformationSchema := True;
  User := '';
end;

function TAConnection.GetXML(): IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(Account.XML)) then
    FXML := XMLNode(Account.XML, 'connection');

  Result := FXML;
end;

procedure TAConnection.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'asynchron'))) then TryStrToBool(XMLNode(XML, 'asynchron').Text, Asynchron);
    if (Assigned(XMLNode(XML, 'character_set'))) then Charset := XMLNode(XML, 'character_set').Text;
    if (Assigned(XMLNode(XML, 'database'))) then Database := XMLNode(XML, 'database').Text;
    if (Assigned(XMLNode(XML, 'host'))) then Host := XMLNode(XML, 'host').Text;
    if (Assigned(XMLNode(XML, 'library/type'))) then
      if (UpperCase(XMLNode(XML, 'library/type').Text) = 'FILE') then LibraryType := ltDLL
      else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'TUNNEL') then LibraryType := ltHTTP
      else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'HTTPTUNNEL') then LibraryType := ltHTTP
      else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'NAMEDPIPE') then LibraryType := ltNamedPipe
      else LibraryType := ltBuiltIn;
    if (Assigned(XMLNode(XML, 'library/filename'))) then LibraryFilename := XMLNode(XML, 'library/filename').Text;
    if (Assigned(XMLNode(XML, 'library/pipename'))) then PipeName := XMLNode(XML, 'library/pipename').Text;
    if (Assigned(XMLNode(XML, 'library/tunnel_url'))) then HTTPTunnelURI := XMLNode(XML, 'library/tunnel_url').Text;
    if (Assigned(XMLNode(XML, 'multistatements'))) then TryStrToBool(XMLNode(XML, 'multistatements').Text, MultiStatements);
    if (Assigned(XMLNode(XML, 'password')) and (XMLNode(XML, 'password').Attributes['encode'] = 'none')) then Password := XMLNode(XML, 'password').Text;
    if (Assigned(XMLNode(XML, 'port'))) then TryStrToInt(XMLNode(XML, 'port').Text, Port);
    if (Assigned(XMLNode(XML, 'savepassword'))) then TryStrToBool(XMLNode(XML, 'savepassword').Text, SavePassword);
    if (Assigned(XMLNode(XML, 'information_schema'))) then
      UseInformationSchema := UpperCase(XMLNode(XML, 'information_schema').Text) <> 'IGNORE';
    if (Assigned(XMLNode(XML, 'user'))) then User := XMLNode(XML, 'user').Text;
  end;
end;

procedure TAConnection.SaveToXML();
begin
  if (Asynchron) then
    XML.ChildNodes.Delete('asynchron')
  else
    XMLNode(XML, 'asynchron').Text := BoolToStr(Asynchron, True);
  XMLNode(XML, 'character_set').Text := Charset;
  XMLNode(XML, 'database').Text := Database;
  XMLNode(XML, 'host').Text := Host;
  case (LibraryType) of
    ltDLL: XMLNode(XML, 'library/type').Text := 'File';
    ltHTTP: XMLNode(XML, 'library/type').Text := 'HTTPTunnel';
    ltNamedPipe: XMLNode(XML, 'library/type').Text := 'NamedPipe';
    else XMLNode(XML, 'library').ChildNodes.Delete('type');
  end;
  XMLNode(XML, 'library/filename').Text := LibraryFilename;
  XMLNode(XML, 'library/pipename').Text := PipeName;
  XMLNode(XML, 'library/tunnel_url').Text := HTTPTunnelURI;
  XMLNode(XML, 'multistatements').Text := BoolToStr(MultiStatements, True);
  if (MultiStatements) then
    XML.ChildNodes.Delete('multistatements')
  else
    XMLNode(XML, 'multistatements').Text := BoolToStr(MultiStatements, True);
  XMLNode(XML, 'password').Attributes['encode'] := 'none';
  XMLNode(XML, 'password').Text := Password;
  XMLNode(XML, 'port').Text := IntToStr(Port);
  XMLNode(XML, 'savepassword').Text := BoolToStr(SavePassword, True);
  if (UseInformationSchema) then
    XML.ChildNodes.Delete('information_schema')
  else
    XMLNode(XML, 'information_schema').Text := 'ignore';
  XMLNode(XML, 'user').Text := User;
end;

{ TAAccount *******************************************************************}

procedure TAAccount.Assign(const Source: TAAccount);
begin
  if (not Assigned(Accounts)) then FAccounts := Source.Accounts;

  CacheSize := Source.CacheSize;
  FLastLogin := Source.LastLogin;
  IconFetched := Source.IconFetched;
  ImageIndex := Source.ImageIndex;
  ManualURL := Source.ManualURL;
  ManualURLFetched := Source.ManualURLFetched;
  Name := Source.Name;

  Modified := True;

  Connection.Assign(Source.Connection);
  if (Assigned(Desktop) and Assigned(Source.Desktop)) then
    Desktop.Assign(Source.Desktop);
end;

constructor TAAccount.Create(const AAccounts: TAAccounts; const AXML: IXMLNode = nil);
begin
  FAccounts := AAccounts;
  FXML := AXML;

  CacheSize := 50;
  FDesktopXMLDocument := nil;
  FHistoryXMLDocument := nil;
  FLastLogin := 0;
  IconFetched := False;
  ImageIndex := -1;
  ManualURL := '';
  ManualURLFetched := False;
  Modified := False;

  Connection := TAConnection.Create(Self);
  FDesktop := nil;
end;

destructor TAAccount.Destroy();
begin
  if (Assigned(FDesktop)) then FDesktop.Free();
  Connection.Free();

  inherited;
end;

function TAAccount.ExtractPath(const AAddress: string): string;
var
  URI: TUURI;
begin
  Result := AAddress;

  try
    URI := TUURI.Create(Result);

    if (URI.Scheme = 'mysql') then
      Delete(Result, 1, Length('mysql') + 1);
    if (((URI.Host = LowerCase(Connection.Host)) or (URI.Host = LOCAL_HOST)) and ((URI.Port = 0) or (URI.Port = Connection.Port))) then
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

function TAAccount.Frame(): Pointer;
begin
  if (Length(FDesktops) = 0) then
    Result := nil
  else
    Result := FDesktops[0].Control;
end;

function TAAccount.FullAddress(const APath: string): string;
var
  Buffer: array[0 .. INTERNET_MAX_URL_LENGTH] of Char;
  Size: Cardinal;
  URLComponents: URL_COMPONENTS;
begin
  Result := APath;

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
    URLComponents.lpszUrlPath := PChar(APath);
    URLComponents.dwUrlPathLength := StrLen(URLComponents.lpszUrlPath);

    Size := SizeOf(Buffer);
    if (not InternetCreateUrl(URLComponents, ICU_ESCAPE, @Buffer, Size)) then
      raise EConvertError.CreateFmt(SConvStrParseError, [APath]);
    SetString(Result, PChar(@Buffer), Size);
  end;
  if (Copy(Result, 1, 2) = '//') then
    Result := 'mysql:' + Result;
end;

function TAAccount.GetBookmarksFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'Bookmarks.xml';
end;

function TAAccount.GetDataPath(): TFileName;
begin
  Result := Accounts.DataPath + ReplaceStr(Name, '/', '_') + PathDelim;
end;

function TAAccount.GetDefaultDatabase(): string;
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

function TAAccount.GetDesktop(): TADesktop;
begin
  if (not Assigned(FDesktop) and Assigned(DesktopXML)) then
    FDesktop := TADesktop.Create(Self);

  Result := FDesktop;
end;

function TAAccount.GetDesktopCount(): Integer;
begin
  Result := Length(FDesktops);
end;

function TAAccount.GetDesktopFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'Desktop.xml';
end;

function TAAccount.GetDesktopXML(): IXMLNode;
var
  I: Integer;
  Node: IXMLNode;
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
      FDesktopXMLDocument.Node.AddChild('desktop').Attributes['version'] := '1.3';
    end;

    if (FDesktopXMLDocument.DocumentElement.Attributes['version'] = '1.1') then
    begin
      Node := FDesktopXMLDocument.DocumentElement;
      if (Assigned(Node)) then
        for I := Node.ChildNodes.Count - 1 downto 0 do
          if (Node.ChildNodes[I].NodeName = 'browser') then
            Node.ChildNodes.Delete(I);
      FDesktopXMLDocument.DocumentElement.Attributes['version'] := '1.2';
    end;

    if (FDesktopXMLDocument.DocumentElement.Attributes['version'] = '1.2') then
    begin
      Node := FDesktopXMLDocument.DocumentElement;
      if (Assigned(Node) and Assigned(XMLNode(Node, 'address'))) then
        Node.ChildNodes.Remove(XMLNode(Node, 'address'));

      FDesktopXMLDocument.DocumentElement.Attributes['version'] := '1.3';
    end;

    FDesktopXMLDocument.Options := FDesktopXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FDesktopXMLDocument.DocumentElement;
end;

function TAAccount.GetHistoryFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'History.xml'
end;

function TAAccount.GetHistoryXML(): IXMLNode;
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

function TAAccount.GetIconFilename(): TFileName;
begin
  if (not ForceDirectories(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'favicon.ico';
end;

function TAAccount.GetImageIndex(): Integer;
begin
  if (FImageIndex >= 0) then
    Result := FImageIndex
  else if (Connection.Host = LOCAL_HOST) then
    Result := 13
  else
    Result := 23;
end;

function TAAccount.GetIndex(): Integer;
begin
  Result := Accounts.IndexOf(Self);
end;

function TAAccount.GetName(): string;
begin
  if (FName = '') and (Assigned(XML)) then
    FName := XML.Attributes['name'];

  Result := FName;
end;

function TAAccount.GetXML(): IXMLNode;
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

procedure TAAccount.LoadFromXML();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'cache/size'))) then TryStrToInt(XMLNode(XML, 'cache/size').Text, CacheSize);
    if (Assigned(XMLNode(XML, 'iconfetched'))) then TryStrToBool(XMLNode(XML, 'iconfetched').Text, IconFetched);
    if (Assigned(XMLNode(XML, 'lastlogin'))) then
      TryStrToFloat(ReplaceStr(XMLNode(XML, 'lastlogin').Text, '.', FormatSettings.DecimalSeparator), Double(FLastLogin));
    if (Assigned(XMLNode(XML, 'manualurl'))) then ManualURL := XMLNode(XML, 'manualurl').Text;
    if (Assigned(XMLNode(XML, 'manualurlfetched'))) then TryStrToBool(XMLNode(XML, 'manualurlfetched').Text, ManualURLFetched);

    Modified := False;

    Connection.LoadFromXML();
    if (Assigned(Desktop)) then
      Desktop.LoadFromXML(); // Client muss geladen sein, damit FullAddress funktioniert
  end;
end;

procedure TAAccount.RegisterDesktop(const AControl: Pointer; const AEventProc: TEventProc);
begin
  SetLength(FDesktops, Length(FDesktops) + 1);
  FDesktops[Length(FDesktops) - 1].Control := AControl;
  FDesktops[Length(FDesktops) - 1].AccountEventProc := AEventProc;
end;

procedure TAAccount.SaveToXML();
begin
  if (Assigned(XML)) then
  begin
    XMLNode(XML, 'cache/size').Text := IntToStr(CacheSize);
    XMLNode(XML, 'iconfetched').Text := BoolToStr(IconFetched, True);
    XMLNode(XML, 'lastlogin').Text := FloatToStr(LastLogin);
    XMLNode(XML, 'manualurl').Text := ManualURL;
    XMLNode(XML, 'manualurlfetched').Text := BoolToStr(ManualURLFetched, True);

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

procedure TAAccount.AccountEvent(const ClassType: TClass);
var
  I: Integer;
begin
  for I := 0 to Length(FDesktops) - 1 do
    if (Assigned(FDesktops[I].AccountEventProc)) then
      FDesktops[I].AccountEventProc(ClassType);
end;

procedure TAAccount.SetLastLogin(const ALastLogin: TDateTime);
begin
  FLastLogin := ALastLogin;

  Modified := True;
end;

procedure TAAccount.SetName(const AName: string);
begin
  Assert(not Assigned(FXML) or (AName = FXML.Attributes['name']));

  
  FName := AName;
end;

procedure TAAccount.UnRegisterDesktop(const AControl: Pointer);
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

function TAAccount.ValidDatabaseName(const ADatabaseName: string): Boolean;
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

{ TAAccounts ******************************************************************}

procedure TAAccounts.AddAccount(const NewAccount: TAAccount);
begin
  if (not Assigned(AccountByName(NewAccount.Name))) then
  begin
    Add(TAAccount.Create(Self));
    Account[Count - 1].Assign(NewAccount);

    AppendIconsToImageList(Preferences.SmallImages, NewAccount);
  end;
end;

procedure TAAccounts.AppendIconsToImageList(const AImageList: TImageList; const AAccount: TAAccount = nil);
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

procedure TAAccounts.Clear();
begin
  while (Count > 0) do
  begin
    Account[0].Free();
    Delete(0);
  end;

  inherited;
end;

constructor TAAccounts.Create(const ADBLogin: TDBLogin);
var
  StringList: TStringList;
begin
  inherited Create();

  FDBLogin := ADBLogin;

  Section := 'Accounts';

  // "Sessions" used up to version 5.1 // May 2012
  if (DirectoryExists(Preferences.UserPath + 'Sessions' + PathDelim)
    and not DirectoryExists(DataPath)) then
    CopyDir(PChar(Preferences.UserPath + 'Sessions' + PathDelim), PChar(DataPath));
  if (FileExists(DataPath + 'Sessions.xml') and not FileExists(Filename)) then
  begin
    if (CopyFile(PChar(DataPath + 'Sessions.xml'), PChar(Filename), False)) then
    begin
      StringList := TStringList.Create();
      StringList.LoadFromFile(Filename);
      StringList.Text := ReplaceStr(StringList.Text, '<session', '<account');
      StringList.Text := ReplaceStr(StringList.Text, '</session', '</account');
      StringList.SaveToFile(Filename);
      StringList.Free();
    end;
  end;

  LoadFromXML();

  AppendIconsToImageList(Preferences.SmallImages);
end;

function TAAccounts.DeleteAccount(const AAccount: TAAccount): Boolean;
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

destructor TAAccounts.Destroy();
begin
  SaveToXML();

  Clear();

  inherited;
end;

function TAAccounts.GetDataPath(): TFileName;
begin
  Result := Preferences.UserPath + 'Accounts' + PathDelim;
end;

function TAAccounts.GetDefault(): TAAccount;
begin
  Result := AccountByName(DefaultAccountName);
end;

function TAAccounts.GetFilename(): TFileName;
begin
  Result := DataPath + 'Accounts.xml';
end;

function TAAccounts.GetFAccounts(Index: Integer): TAAccount;
begin
  Result := TAAccount(Items[Index]);
end;

function TAAccounts.GetXML(): IXMLNode;
begin
  if (not Assigned(FXMLDocument)) then
  begin
    if (FileExists(Filename)) then
    begin
      FXMLDocument := LoadXMLDocument(Filename);
    end
    else
    begin
      FXMLDocument := NewXMLDocument();
      FXMLDocument.Encoding := 'utf-8';
      FXMLDocument.Node.AddChild('accounts').Attributes['version'] := '1.1.0';
    end;

    FXMLDocument.Options := FXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FXMLDocument.DocumentElement;
end;

procedure TAAccounts.LoadFromXML();
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

        Insert(Index, TAAccount.Create(Self, XML.ChildNodes[I]));
        Account[Index].LoadFromXML();
      end;
    end;

    if (Assigned(XMLNode(XML, 'default'))) then
      DefaultAccountName := XMLNode(XML, 'default').Text;

    FXMLDocument.Options := FXMLDocument.Options + [doNodeAutoCreate];
  end;
end;

procedure TAAccounts.SaveToXML();
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

function TAAccounts.AccountByName(const AccountName: string): TAAccount;
var
  I: Integer;
begin
  Result := nil;

  for I:=0 to Count - 1 do
    if (Account[I].Name = AccountName) then
      Result := Account[I];
end;

function TAAccounts.AccountByURI(const AURI: string; const DefaultAccount: TAAccount = nil): TAAccount;
var
  Found: Integer;
  Host: string;
  I: Integer;
  Name: string;
  NewAccount: TAAccount;
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
        if (Result = DefaultAccount) then
          break;
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

      NewAccount := TAAccount.Create(Self);
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
    end;

    URI.Free();
  end;
end;

procedure TAAccounts.SetDefault(const AAccount: TAAccount);
begin
  if (not Assigned(AAccount)) then
    DefaultAccountName := ''
  else
    DefaultAccountName := AAccount.Name;
end;

procedure TAAccounts.UpdateAccount(const Account, NewAccount: TAAccount);
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

end.
