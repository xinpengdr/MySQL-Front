unit fCWorkbench;

interface

uses
  SysUtils, Classes, Controls, Types, Graphics, Messages, Forms,
  Windows, XMLDoc, XMLIntf, Variants,
  fClient;

const
  CM_ENDLASSO = WM_USER + 400;

type
  TWPoint = class;
  TWLine = class;
  TWTable = class;
  TWTables = class;
  TWLinkPoint = class;
  TWLink = class;
  TWForeignKey = class;
  TWLinks = class;
  TWSection = class;
  TWWorkbench = class;

  TCoord = TPoint;
  TArea = TRect;

  TWControls = class(TList)
  private
    FWorkbench: TWWorkbench;
  public
    procedure Clear(); override;
    constructor Create(const AWorkbench: TWWorkbench); virtual;
    procedure Delete(Index: Integer); virtual;
    destructor Destroy(); override;
    property Workbench: TWWorkbench read FWorkbench;
  end;

  TWControl = class(TGraphicControl)
  private
    DoubleBuffer: Graphics.TBitmap;
    FDoubleBuffered: Boolean;
    FWorkbench: TWWorkbench;
    MouseMoveAlign: TAlign;
    MouseDownPosition: TCoord;
    MouseDownPoint: TPoint;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
  protected
    FPosition: TCoord;
    FSelected: Boolean;
    procedure ApplyPosition(); virtual; abstract;
    procedure DblClick(); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure LoadFromXML(const XML: IXMLNode); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MoveTo(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord); virtual;
    procedure Moving(const Sender: TWControl; const Shift: TShiftState; var NewPosition: TCoord); virtual;
    procedure Paint(); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); virtual; abstract;
    procedure SaveToXML(const XML: IXMLNode); virtual;
    procedure SetSelected(ASelected: Boolean); virtual;
    property DoubleBuffered: Boolean read FDoubleBuffered;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord); reintroduce; virtual;
    destructor Destroy(); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure Invalidate(); override;
    procedure Move(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord); virtual;
    property Position: TCoord read FPosition;
    property Selected: Boolean read FSelected write SetSelected;
    property Workbench: TWWorkbench read FWorkbench;
  end;

  TWAreaResizeMode = (rmNone, rmCreate, rmNW, rmN, rmNE, rmE, rmSE, rmS, rmSW, rmW);

  TWArea = class(TWControl)
  private
    FSize: TSize;
    FMouseDownSize: TSize;
    FResizeMode: TWAreaResizeMode;
    function GetArea(): TRect;
  protected
    procedure ApplyPosition(); override;
    procedure ChangeSize(const Sender: TWControl; const Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property MouseDownSize: TSize read FMouseDownSize;
    property ResizeMode: TWAreaResizeMode read FResizeMode;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord); override;
    property Area: TRect read GetArea;
    property Size: TSize read FSize;
  end;

  TWPointMoveState = (msNormal, msFixed, msAutomatic);

  TWPoint = class(TWControl)
  private
    function GetLastPoint(): TWPoint;
    function GetLineA(): TWLine;
    function GetLineB(): TWLine;
    procedure SetLineA(ALineA: TWLine);
    procedure SetLineB(ALineB: TWLine);
  protected
    ControlA: TWControl;
    ControlB: TWControl;
    Center: TPoint;
    MoveState: TWPointMoveState;
    procedure ApplyPosition(); override;
    function ControlAlign(const Control: TWControl): TAlign; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MoveTo(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure SetSelected(ASelected: Boolean); override;
    property LastPoint: TWPoint read GetLastPoint;
    property LineA: TWLine read GetLineA write SetLineA;
    property LineB: TWLine read GetLineB write SetLineB;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const PreviousPoint: TWPoint = nil); reintroduce; virtual;
    destructor Destroy(); override;
  end;

  TWLineOrientation = (foHorizontal, foVertical, foNone);

  TWLine = class(TWControl)
  private
    FOrientation: TWLineOrientation;
    FPointA: TWPoint;
    FPointB: TWPoint;
    function GetLength(): Integer;
    function GetOrientation(): TWLineOrientation;
    procedure SetPointA(APointA: TWPoint);
    procedure SetPointB(APointB: TWPoint);
  protected
    procedure ApplyPosition(); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure SetSelected(ASelected: Boolean); override;
    property Length: Integer read GetLength;
    property Orientation: TWLineOrientation read GetOrientation;
    property PointA: TWPoint read FPointA write SetPointA;
    property PointB: TWPoint read FPointB write SetPointB;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APointA, APointB: TWPoint); reintroduce; virtual;
    destructor Destroy(); override;
  end;

  TWTable = class(TWArea)
  private
    FData: TCustomData;
    FFocused: Boolean;
    FLinkPoints: array of TWLinkPoint;
    function GetCaption(): TCaption;
    function GetIndex(): Integer;
    function GetLinkPoint(AIndex: Integer): TWLinkPoint;
    function GetLinkPointCount(): Integer;
    procedure SetFocused(AFocused: Boolean);
  protected
    FBaseTable: TCBaseTable;
    procedure ApplyPosition(); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure LoadFromXML(const XML: IXMLNode); override;
    procedure MoveTo(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord); override;
    procedure Moving(const Sender: TWControl; const Shift: TShiftState; var NewPosition: TCoord); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure RegisterLinkPoint(const ALinkPoint: TWLinkPoint); virtual;
    procedure ReleaseLinkPoint(const ALinkPoint: TWLinkPoint); virtual;
    property LinkPoint[Index: Integer]: TWLinkPoint read GetLinkPoint;
    property LinkPointCount: Integer read GetLinkPointCount;
  public
    constructor Create(const ATables: TWTables; const APosition: TCoord; const ABaseTable: TCBaseTable = nil); reintroduce; virtual;
    destructor Destroy(); override;
    procedure Invalidate(); override;
    property BaseTable: TCBaseTable read FBaseTable;
    property Caption: TCaption read GetCaption;
    property Data: TCustomData read FData write FData;
    property Focused: Boolean read FFocused write SetFocused;
    property Index: Integer read GetIndex;
  end;

  TWTables = class(TWControls)
  private
    function GetSelCount(): Integer;
    function GetTable(Index: Integer): TWTable;
  protected
    procedure SaveToXML(const XML: IXMLNode); virtual;
  public
    property SelCount: Integer read GetSelCount;
    property Table[Index: Integer]: TWTable read GetTable; default;
  end;

  TWLinkPoint = class(TWPoint)
  private
    function GetIndex(): Integer;
    function GetLink(): TWLink;
    function GetTableA(): TWTable;
    function GetTableB(): TWTable;
    procedure SetTableA(ATableA: TWTable);
    procedure SetTableB(ATableB: TWTable);
  protected
    procedure ApplyPosition(); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Moving(const Sender: TWControl; const Shift: TShiftState; var NewPosition: TCoord); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    property Index: Integer read GetIndex;
    property TableA: TWTable read GetTableA write SetTableA;
    property TableB: TWTable read GetTableB write SetTableB;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const APreviousPoint: TWPoint = nil); reintroduce; virtual;
    destructor Destroy(); override;
    property Link: TWLink read GetLink;
  end;

  TWLinkLine = class(TWLine)
  private
    function GetLink(): TWLink;
  protected
    procedure ApplyPosition(); override;
  public
    property Link: TWLink read GetLink;
  end;

  TWLink = class(TWLinkPoint)
  private
    FCaption: TCaption;
    function GetLinkSelected(): Boolean;
    function GetPoint(Index: Integer): TWLinkPoint;
    function GetPointCount(): Integer;
    function GetTable(Index: Integer): TWTable;
    procedure SetLinkSelected(const ALinkSelected: Boolean);
    procedure SetTable(Index: Integer; ATable: TWTable);
  protected
    procedure Cleanup(const Sender: TWControl); virtual;
    function GetCaption(): TCaption; virtual;
    procedure LoadFromXML(const XML: IXMLNode); override;
    procedure SaveToXML(const XML: IXMLNode); override;
    procedure SetCaption(const ACaption: TCaption); virtual;
    property Points[Index: Integer]: TWLinkPoint read GetPoint;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const PreviousPoint: TWPoint = nil); override;
    destructor Destroy(); override;
    property Caption: TCaption read GetCaption write SetCaption;
    property ChildTable: TWTable index 0 read GetTable write SetTable;
    property LinkSelected: Boolean read GetLinkSelected write SetLinkSelected;
    property ParentTable: TWTable index 1 read GetTable write SetTable;
    property PointCount: Integer read GetPointCount;
  end;

  TWForeignKey = class(TWLink)
  private
    FBaseForeignKey: TCForeignKey;
  protected
    function GetCaption(): TCaption; override;
    procedure SetCaption(const ACaption: TCaption); override;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const PreviousPoint: TWPoint = nil); override;
    property BaseForeignKey: TCForeignKey read FBaseForeignKey write FBaseForeignKey;
  end;

  TWLinks = class(TWControls)
  private
    function GetLink(Index: Integer): TWLink; inline;
    function GetSelCount(): Integer;
  protected
    procedure SaveToXML(const XML: IXMLNode); virtual;
  public
    property Link[Index: Integer]: TWLink read GetLink; default;
    property SelCount: Integer read GetSelCount;
  end;

  TWSection = class(TWArea)
  private
    FColor: TColor;
    function GetCaption(): TCaption;
    procedure SetCaption(const ACaption: TCaption);
  protected
    procedure LoadFromXML(const XML: IXMLNode); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure SaveToXML(const XML: IXMLNode); override;
    procedure SetSelected(ASelected: Boolean); override;
    procedure SetZOrder(TopMost: Boolean); override;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord); reintroduce; virtual;
    property Caption: TCaption read GetCaption write SetCaption;
    property Color: TColor read FColor write FColor;
  end;

  TWSections = class(TWControls)
  private
    function GetSection(Index: Integer): TWSection;
  protected
    procedure LoadFromXML(const XML: IXMLNode); virtual;
    procedure SaveToXML(const XML: IXMLNode); virtual;
  public
    property Section[Index: Integer]: TWSection read GetSection; default;
  end;

  TWLasso = class(TWArea)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure SetSelected(ASelected: Boolean); override;
  public
    constructor Create(const AWorkbench: TWWorkbench; const APosition: TCoord); reintroduce; virtual;
  end;

  TMWorkbenchState = (wsNormal, wsCreateLink, wsCreateForeignKey, wsCreateSection, wsCreateTable, wsLoading, wsAutoCreate);

  TWWorkbenchChangeEvent = procedure(Sender: TObject; Control: TWControl) of object;
  TWWorkbenchCursorMoveEvent = procedure(Sender: TObject; X, Y: Integer) of object;
  TWWorkbenchValidateControlEvent = function(Sender: TObject; Control: TWControl): Boolean of Object;

  TWWorkbench = class(TScrollBox)
  private
    CreatedLink: TWLink;
    CreatedTable: TWTable;
    FDatabase: TCDatabase;
    FHideSelection: Boolean;
    FLinks: TWLinks;
    FMultiSelect: Boolean;
    FOnChange: TWWorkbenchChangeEvent;
    FOnCursorMove: TWWorkbenchCursorMoveEvent;
    FOnValidateControl: TWWorkbenchValidateControlEvent;
    FSections: TWSections;
    FSelected: TWControl;
    FTableFocused: TWTable;
    FTables: TWTables;
    Lasso: TWLasso;
    LastScrollTickCount: DWord;
    PendingUpdateControls: TList;
    UpdateCount: Integer;
    XML: IXMLNode;
    XMLDocument: IXMLDocument;
    function GetObjectCount(): Integer;
    function GetSelCount(): Integer;
    procedure SetMultiSelect(AMultiSelect: Boolean);
    procedure SetSelected(ASelected: TWControl);
    procedure SetTableFocused(ATableFocused: TWTable);
    procedure CMEndLasso(var Message: TMessage); message CM_ENDLASSO;
  protected
    FModified: Boolean;
    State: TMWorkbenchState;
    procedure Change(); virtual;
    procedure CursorMove(const X, Y: Integer); virtual;
    procedure DoEnter(); override;
    procedure DoExit(); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function Position(const X, Y: Integer): TCoord; inline;
    procedure ReleaseControl(const Control: TWControl); virtual;
    procedure UpdateControl(const Control: TWControl); virtual;
  public
    procedure AddExistingTable(const X, Y: Integer; const ABaseTable: TCBaseTable); virtual;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const ADatabase: TCDatabase); reintroduce; overload; virtual;
    destructor Destroy(); override;
    procedure BeginUpdate(); virtual;
    procedure CalcRange(const Reset: Boolean); virtual;
    procedure Clear(); virtual;
    procedure ClientUpdate(const Event: TCClient.TEvent);
    procedure EndUpdate(); virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function ForeignKeyByBaseForeignKey(const BaseForeignKey: TCForeignKey): TWForeignKey; virtual;
    procedure CreateNewForeignKey(const X, Y: Integer); virtual;
    procedure CreateNewLink(const X, Y: Integer); virtual;
    procedure CreateNewSection(const X, Y: Integer); virtual;
    procedure CreateNewTable(const X, Y: Integer); virtual;
    procedure KeyPress(var Key: Char); override;
    function LinkByCaption(const Caption: string): TWLink; virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure Print(const Title: string); virtual;
    procedure SaveToBMP(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    function TableAt(const Position: TCoord): TWTable;
    function TableByBaseTable(const ATable: TCBaseTable): TWTable; virtual;
    function TableByCaption(const Caption: string): TWTable; virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Database: TCDatabase read FDatabase;
    property HideSelection: Boolean read FHideSelection write FHideSelection default False;
    property Links: TWLinks read FLinks;
    property Modified: Boolean read FModified;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ObjectCount: Integer read GetObjectCount;
    property OnChange: TWWorkbenchChangeEvent read FOnChange write FOnChange;
    property OnCursorMove: TWWorkbenchCursorMoveEvent read FOnCursorMove write FOnCursorMove;
    property OnValidateControl: TWWorkbenchValidateControlEvent read FOnValidateControl write FOnValidateControl;
    property Sections: TWSections read FSections;
    property SelCount: Integer read GetSelCount;
    property Selected: TWControl read FSelected write SetSelected;
    property TableFocused: TWTable read FTableFocused write SetTableFocused;
    property Tables: TWTables read FTables;
  end;

implementation {***************************************************************}

uses
  ExtCtrls, Math, Dialogs, StdActns, Consts, Printers,
  fPreferences;

const
  BorderSize = 1;
  LineWidth = 1; // nur ungerade Werte
  ConnectorSize = LineWidth + 6; // nur gerade Werte
  PointSize = LineWidth + 2; // nur gerade Werte
  Padding = 2;

function TryStrToAlign(const Str: string; var Align: TAlign): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'LEFT') then Align := alLeft
  else if (UpperCase(Str) = 'TOP') then Align := alTop
  else if (UpperCase(Str) = 'RIGHT') then Align := alRight
  else if (UpperCase(Str) = 'BOTTOM') then Align := alBottom
  else Result := False;
end;

function AlignToStr(const Align: TAlign): string;
begin
  case (Align) of
    alLeft: Result := 'Left';
    alTop: Result := 'Top';
    alRight: Result := 'Right';
    alBottom: Result := 'Bottom';
    else Result := '';
  end;
end;

function InvertAlign(const Align: TAlign): TAlign;
begin
  case (Align) of
    alLeft: Result := alRight;
    alTop: Result := alBottom;
    alRight: Result := alLeft;
    alBottom: Result := alTop;
    else Result := alNone;
  end;
end;

function CreateSegment(const Sender: TWControl; const APosition: TCoord; const Point: TWPoint; const CreateBefore: Boolean = True): TWPoint;
var
  Line: TWLine;
  OldMoveState: TWPointMoveState;
begin
  OldMoveState := Point.MoveState;
  if (Point.MoveState = msNormal) then
    Point.MoveState := msFixed;

  if (CreateBefore) then
  begin
    if (Point is TWLinkPoint) then
      Result := TWLinkPoint.Create(Point.Workbench, Point.Position, nil)
    else
      Result := TWPoint.Create(Point.Workbench, Point.Position, nil);
    Result.LineA := Point.LineA;

    TWLinkLine.Create(Point.Workbench, Result, Point);
  end
  else
  begin
    Line := Point.LineB;

    if (Assigned(Line)) then
      Line.PointA := nil;

    if (Point is TWLinkPoint) then
      Result := TWLinkPoint.Create(Point.Workbench, Point.Position, Point)
    else
      Result := TWPoint.Create(Point.Workbench, Point.Position, Point);
    Result.LineB := Line;
  end;

  Point.MoveState := OldMoveState;

  Result.MoveTo(Sender, [], APosition);
  Result.Selected := Point.Selected;
end;

procedure FreeSegment(const Point: TWPoint; const Line: TWLine);
var
  TempPoint: TWPoint;
begin
  if (Line = Point.LineA) then
  begin
    if (Assigned(Point.ControlB)) then
      Point.LineA.PointA.ControlB := Point.ControlB;

    TempPoint := Line.PointA;
    Point.LineA := nil;
    Line.PointA := nil;
    TempPoint.LineB := Point.LineB;
  end
  else if (Line = Point.LineB) then
  begin
    TempPoint := Line.PointB;
    Point.LineB := nil;
    Line.PointB := nil;
    TempPoint.LineA := Point.LineA;
  end
  else
    raise ERangeError.Create('Line is not attached to Point.');

  Line.Free();
  Point.Free();
end;

function Coord(const X, Y: Integer): TCoord; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

function CoordInArea(const Position: TCoord; const Area: TRect): Boolean;
begin
  Result := PtInRect(Rect(Area.Left, Area.Top, Area.Right, Area.Bottom), Point(Position.X, Position.Y));
end;

{ TWObjects *******************************************************************}

procedure TWControls.Clear();
begin
  Workbench.BeginUpdate();

  while (Count > 0) do
    Delete(Count - 1);

  inherited;

  Workbench.EndUpdate();
end;

constructor TWControls.Create(const AWorkbench: TWWorkbench);
begin
  inherited Create();

  FWorkbench := AWorkbench;
end;

procedure TWControls.Delete(Index: Integer);
begin
  TWControl(Items[Index]).Free();

  inherited;
end;

destructor TWControls.Destroy();
begin
  Clear();

  inherited;
end;

{ TWControl *******************************************************************}

procedure TWControl.CMCursorChanged(var Message: TMessage);
var
  TempCursor: TCursor;
begin
  TempCursor := Workbench.Cursor;

  Workbench.Cursor := Cursor;
  Workbench.Perform(WM_SETCURSOR, Workbench.Handle, HTCLIENT);

  Workbench.Cursor := TempCursor;

  inherited;
end;

constructor TWControl.Create(const AWorkbench: TWWorkbench; const APosition: TCoord);
begin
  inherited Create(AWorkbench);
  Parent := AWorkbench;

  FWorkbench := AWorkbench;
  FPosition := APosition;

  FDoubleBuffered := False;
  FSelected := False;
  MouseDownPosition := Coord(-1, -1);
  MouseDownPoint := Point(-1, -1);
end;

procedure TWControl.DblClick();
begin
  Workbench.Selected := Self;
  if (Self is TWTable) then
    Workbench.TableFocused := TWTable(Self);

  MouseCapture := False;

  Workbench.DblClick();
end;

destructor TWControl.Destroy();
begin
  Workbench.ReleaseControl(Self);

  if (Assigned(DoubleBuffer)) then
    DoubleBuffer.Free();

  inherited;
end;

procedure TWControl.DragDrop(Source: TObject; X, Y: Integer);
begin
  Workbench.DragDrop(Source, Workbench.HorzScrollBar.Position + Left + X, Workbench.VertScrollBar.Position + Top + X);
end;

procedure TWControl.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Workbench.DragOver(Source, Workbench.HorzScrollBar.Position + Left + X, Workbench.VertScrollBar.Position + Top + X, State, Accept);
end;

procedure TWControl.Invalidate();
begin
  if (Assigned(DoubleBuffer)) then
    FreeAndNil(DoubleBuffer);

  inherited;
end;

procedure TWControl.LoadFromXML(const XML: IXMLNode);
var
  NewPosition: TCoord;
begin
  NewPosition := Position;
  if (Assigned(XMLNode(XML, 'coord/x'))) then TryStrToInt(XMLNode(XML, 'coord/x').Text, NewPosition.X);
  if (Assigned(XMLNode(XML, 'coord/y'))) then TryStrToInt(XMLNode(XML, 'coord/y').Text, NewPosition.Y);
  MoveTo(Self, [], NewPosition);

  Workbench.CalcRange(False);
end;

procedure TWControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Self is TWLinkLine) then
    MouseDownPosition := TWLinkLine(Self).PointA.Position
  else
    MouseDownPosition := Position;

  inherited;
  Workbench.SetFocus();

  if ((Button in [mbLeft, mbRight]) and (not (ssCtrl in Shift) and (not Selected or (Workbench.SelCount <= 1)) or (not Workbench.MultiSelect or (not (ssCtrl in Shift) and (Workbench.SelCount <= 1))))) then
  begin
    Workbench.Selected := Self;
    if (Self is TWLinkPoint) then
      TWLinkPoint(Self).Link.LinkSelected := not TWLinkPoint(Self).Link.LinkSelected
    else if (Self is TWLinkLine) then
      TWLinkLine(Self).Link.LinkSelected := not TWLinkLine(Self).Link.LinkSelected;

    if (Self is TWTable) then
      Workbench.TableFocused := TWTable(Self)
    else
      Workbench.TableFocused := nil;
  end;

  if (Button = mbLeft) then
  begin
    MouseMoveAlign := alNone;
    MouseDownPoint := Point(Workbench.HorzScrollBar.Position + Left + X, Workbench.VertScrollBar.Position + Top + Y);

    MouseCapture := True;
  end;
end;

procedure TWControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Delta: TPoint;
  Msg: TMsg;
  NewPosition: TCoord;
begin
  if (not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.wParam = MK_LBUTTON))) then
  begin
    if (ssLeft in Shift) then
      if (not (Self is TWTable)) then
        Workbench.TableFocused := nil
      else
        Workbench.TableFocused := TWTable(Self);

    inherited;

    X := Workbench.HorzScrollBar.Position + Left + X;
    Y := Workbench.VertScrollBar.Position + Top + Y;

    if (ssLeft in Shift) then
    begin
      Delta.X := 0;
      if (not (Self is TWArea) or (TWArea(Self).ResizeMode = rmNone)) then
      begin
        X := Max(X, MouseDownPoint.X - MouseDownPosition.X);
        if (MouseDownPosition.X + X - MouseDownPoint.X < Workbench.HorzScrollBar.Position) then
          Delta.X := (MouseDownPosition.X + X - MouseDownPoint.X) - Workbench.HorzScrollBar.Position
        else if (MouseDownPosition.X + Width + X - MouseDownPoint.X > Workbench.HorzScrollBar.Position + Workbench.ClientWidth) then
          Delta.X := MouseDownPosition.X + Width + X - MouseDownPoint.X - (Workbench.HorzScrollBar.Position + Workbench.ClientWidth);
      end
      else
      begin
        case (TWArea(Self).ResizeMode) of
          rmCreate:
            begin
              X := Max(X, MouseDownPoint.X - MouseDownPosition.X);
              X := Min(X, Max(Workbench.HorzScrollBar.Position + Workbench.ClientWidth, Workbench.HorzScrollBar.Range) - (MouseDownPosition.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X));
            end;
          rmN, rmS:
            X := MouseDownPoint.X;
          rmNW, rmW, rmSW:
            begin
              X := Max(X, MouseDownPoint.X - MouseDownPosition.X);
              X := Min(X, MouseDownPosition.X + TWArea(Self).MouseDownSize.cx + (MouseDownPoint.X - MouseDownPosition.X) - 2 * (BorderSize + Padding));
            end;
          rmNE, rmE, rmSE:
            begin
              X := Max(X, MouseDownPosition.X + 2 * (BorderSize + Padding) - (MouseDownPosition.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X));
              X := Min(X, Max(Workbench.HorzScrollBar.Position + Workbench.ClientWidth, Workbench.HorzScrollBar.Range) - (MouseDownPosition.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X));
            end;
        end;
        if ((TWArea(Self).ResizeMode in [rmCreate, rmNW, rmW, rmSW]) and (X < Workbench.HorzScrollBar.Position)) then
          Delta.X := X - Workbench.HorzScrollBar.Position - (MouseDownPoint.X - MouseDownPosition.X)
        else if ((TWArea(Self).ResizeMode in [rmCreate, rmNE, rmE, rmSE]) and (MouseDownPosition.X + TWArea(Self).MouseDownSize.cx + X - MouseDownPoint.X > Workbench.HorzScrollBar.Position + Workbench.ClientWidth)) then
          Delta.X := X - (Workbench.HorzScrollBar.Position + Workbench.ClientWidth) + (MouseDownPosition.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X);
      end;
      if (Abs(Delta.X) > Workbench.HorzScrollBar.Increment) then
      begin
        Dec(X, Sign(Delta.X) * (Abs(Delta.X) - Workbench.HorzScrollBar.Increment));
        Delta.X := Sign(Delta.X) * Workbench.HorzScrollBar.Increment;
      end;

      Delta.Y := 0;

      if (not (Self is TWArea) or (TWArea(Self).ResizeMode = rmNone)) then
      begin
        Y := Max(Y, MouseDownPoint.Y - MouseDownPosition.Y);
        if (MouseDownPosition.Y + Y - MouseDownPoint.Y < Workbench.VertScrollBar.Position) then
          Delta.Y := (MouseDownPosition.Y + Y - MouseDownPoint.Y) - Workbench.VertScrollBar.Position
        else if (MouseDownPosition.Y + Height + Y - MouseDownPoint.Y > Workbench.VertScrollBar.Position + Workbench.ClientHeight) then
          Delta.Y := MouseDownPosition.Y + Height + Y - MouseDownPoint.Y - (Workbench.VertScrollBar.Position + Workbench.ClientHeight);
      end
      else
      begin
        case (TWArea(Self).ResizeMode) of
          rmCreate:
            begin
              Y := Max(Y, MouseDownPoint.Y - MouseDownPosition.Y);
              Y := Min(Y, Max(Workbench.VertScrollBar.Position + Workbench.ClientHeight, Workbench.VertScrollBar.Range) - (MouseDownPosition.Y + TWArea(Self).MouseDownSize.cy - MouseDownPoint.Y));
            end;
          rmE, rmW:
            Y := MouseDownPoint.Y;
          rmNW, rmN, rmNE:
            begin
              Y := Max(Y, MouseDownPoint.Y - MouseDownPosition.Y);
              Y := Min(Y, MouseDownPosition.Y + TWArea(Self).MouseDownSize.cy + (MouseDownPoint.Y - MouseDownPosition.Y) - 2 * (BorderSize + Padding));
            end;
          rmSW, rmS, rmSE:
            begin
              Y := Max(Y, MouseDownPosition.Y + 2 * (BorderSize + Padding) - (MouseDownPosition.Y + TWArea(Self).MouseDownSize.cy - MouseDownPoint.Y));
              Y := Min(Y, Max(Workbench.VertScrollBar.Position + Workbench.ClientHeight, Workbench.VertScrollBar.Range) - (MouseDownPosition.Y + TWArea(Self).MouseDownSize.cy - MouseDownPoint.Y));
            end;
        end;
        if ((TWArea(Self).ResizeMode in [rmCreate, rmNE, rmN, rmNW]) and (Y < Workbench.VertScrollBar.Position)) then
          Delta.Y := Y - Workbench.VertScrollBar.Position
        else if ((TWArea(Self).ResizeMode in [rmCreate, rmSE, rmS, rmSW]) and (MouseDownPosition.Y + TWArea(Self).MouseDownSize.cy + Y - MouseDownPoint.Y > Workbench.VertScrollBar.Position + Workbench.ClientHeight)) then
          Delta.Y := Y - (Workbench.VertScrollBar.Position + Workbench.ClientHeight);
      end;
      if (Abs(Delta.Y) > Workbench.VertScrollBar.Increment) then
      begin
        Dec(Y, Sign(Delta.Y) * (Abs(Delta.Y) - Workbench.VertScrollBar.Increment));
        Delta.Y := Sign(Delta.Y) * Workbench.VertScrollBar.Increment;
      end;

      if (GetTickCount() - Workbench.LastScrollTickCount < 50) then
      begin
        Dec(X, Delta.X);
        Dec(Y, Delta.Y);
      end
      else if ((Delta.X <> 0) or (Delta.Y <> 0)) then
      begin
        if (Delta.X <> 0) then
        begin
          Workbench.HorzScrollBar.Range := Max(Workbench.HorzScrollBar.Range, Workbench.HorzScrollBar.Position + Workbench.ClientWidth + Delta.X);
          Workbench.HorzScrollBar.Position := Max(0, Workbench.HorzScrollBar.Position + Delta.X);
        end;

        if (Delta.Y <> 0) then
        begin
          Workbench.VertScrollBar.Range := Max(Workbench.VertScrollBar.Range, Workbench.VertScrollBar.Position + Workbench.ClientHeight + Delta.Y);
          Workbench.VertScrollBar.Position := Max(0, Workbench.VertScrollBar.Position + Delta.Y);
        end;

        Workbench.LastScrollTickCount := GetTickCount();
      end;
    end;

    if ((Self is TWArea) and (TWArea(Self).ResizeMode <> rmNone) and (ssLeft in Shift)) then
      TWArea(Self).ChangeSize(Self, Shift, X, Y)
    else
    begin
      NewPosition.X := MouseDownPosition.X + X - MouseDownPoint.X;
      NewPosition.Y := MouseDownPosition.Y + Y - MouseDownPoint.Y;
      Moving(Self, Shift, NewPosition);
      if ((Self is TWLinkPoint) and not Assigned(TWLinkPoint(Self).Link.ParentTable)) then
        MoveTo(Self, Shift, NewPosition)
      else if (ssLeft in Shift) then
        Move(Self, Shift, NewPosition);
    end;

    Workbench.CursorMove(X, Y);
  end;
end;

procedure TWControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if ((Button in [mbLeft]) and (not (ssCtrl in Shift) and (Workbench.SelCount <= 1) or (ssCtrl in Shift) and Workbench.MultiSelect and (Position.X = MouseDownPosition.X) and (Position.Y = MouseDownPosition.Y))) then
  begin
    if (ssCtrl in Shift) then
      Selected := not Selected
    else
    begin
      Workbench.Selected := Self;
      if (Self is TWLinkPoint) then
        TWLinkPoint(Self).Link.LinkSelected := True
      else if (Self is TWLinkLine) then
        TWLinkLine(Self).Link.LinkSelected := True;
    end;

    if (Self is TWTable) then
      Workbench.TableFocused := TWTable(Self)
    else
      Workbench.TableFocused := nil;
  end;

  MouseCapture := False;

  inherited;

  Workbench.CalcRange(True);
  MouseDownPosition.X := -1; MouseDownPosition.Y := -1;
  MouseDownPoint := Point(-1, -1);

  if (Self is TWLinkPoint) then
    TWLinkPoint(Self).Link.Cleanup(Self)
  else if (Self is TWLinkLine) then
    TWLinkLine(Self).Link.Cleanup(Self)
  else if (Self is TWTable) then
    for I := 0 to TWTable(Self).LinkPointCount - 1 do
      TWTable(Self).LinkPoint[I].Link.Cleanup(Self);
end;

procedure TWControl.Move(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord);
var
  Controls: array of TWControl;
  I: Integer;
  NewControlPosition: TCoord;
  WantedNewPosition: TCoord;
begin
  if ((NewPosition.X <> Position.X) or (NewPosition.Y <> Position.Y)) then
  begin
    if ((Workbench.SelCount = 1) or (Self is TWLinkPoint) and TWLinkPoint(Self).Link.LinkSelected and (Workbench.SelCount = TWLinkPoint(Self).Link.PointCount * 2 - 1)) then
    begin
      Moving(Self, Shift, NewPosition);
      MoveTo(Self, Shift, NewPosition);
    end
    else
    begin
      Workbench.BeginUpdate();

      for I := 0 to Workbench.ControlCount - 1 do
        if ((Workbench.Controls[I] is TWControl) and not (Workbench.Controls[I] is TWLine) and TWControl(Workbench.Controls[I]).Selected) then
        begin
          WantedNewPosition.X := TWControl(Workbench.Controls[I]).Position.X + NewPosition.X - Position.X;
          WantedNewPosition.Y := TWControl(Workbench.Controls[I]).Position.Y + NewPosition.Y - Position.Y;
          NewControlPosition := WantedNewPosition;
          TWControl(Workbench.Controls[I]).Moving(nil, Shift, NewControlPosition);
          Dec(NewPosition.X, WantedNewPosition.X - NewControlPosition.X);
          Dec(NewPosition.Y, WantedNewPosition.Y - NewControlPosition.Y);
        end;

      SetLength(Controls, 0);
      for I := 0 to Workbench.ControlCount - 1 do
        if ((Workbench.Controls[I] <> Self) and (Workbench.Controls[I] is TWControl) and TWControl(Workbench.Controls[I]).Selected) then
        begin
          SetLength(Controls, Length(Controls) + 1);
          Controls[Length(Controls) - 1] := TWControl(Workbench.Controls[I]);
        end;

      for I := 0 to Length(Controls) - 1 do
      begin
        NewControlPosition.X := Controls[I].Position.X + NewPosition.X - Position.X;
        NewControlPosition.Y := Controls[I].Position.Y + NewPosition.Y - Position.Y;
        Controls[I].MoveTo(nil, Shift, NewControlPosition);
      end;

      MoveTo(nil, [], NewPosition);

      Workbench.EndUpdate();
    end;

    Workbench.CalcRange(False);
  end;
end;

procedure TWControl.MoveTo(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord);
begin
  FPosition := NewPosition;

  Workbench.UpdateControl(Self);

  if (Workbench.State <> wsLoading) then
    Workbench.FModified := True;
end;

procedure TWControl.Moving(const Sender: TWControl; const Shift: TShiftState; var NewPosition: TCoord);
begin
  if (NewPosition.X < 0) then
    NewPosition.X := 0;
  if (NewPosition.Y < 0) then
    NewPosition.Y := 0;
end;

procedure TWControl.Paint();
begin
  if (not Workbench.DoubleBuffered or not DoubleBuffered) then
    PaintTo(Canvas, 0, 0)
  else
  begin
    if (not Assigned(DoubleBuffer)) then
    begin
      DoubleBuffer := Graphics.TBitmap.Create();
      DoubleBuffer.Handle := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
      DoubleBuffer.Canvas.Font := Canvas.Font;
      DoubleBuffer.Canvas.Brush := Canvas.Brush;
      DoubleBuffer.Canvas.Pen := Canvas.Pen;

      PaintTo(DoubleBuffer.Canvas, 0, 0);
    end;

    BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
      DoubleBuffer.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TWControl.SaveToXML(const XML: IXMLNode);
begin
  XMLNode(XML, 'coord/x').Text := IntToStr(Position.X);
  XMLNode(XML, 'coord/y').Text := IntToStr(Position.Y);
end;

procedure TWControl.SetSelected(ASelected: Boolean);
begin
  if (ASelected <> Selected) then
  begin
    FSelected := ASelected;

    if (Selected) then
      BringToFront();

    Invalidate();
  end;
end;

{ TWArea **********************************************************************}

procedure TWArea.ApplyPosition();
begin
  SetBounds(
    Position.X - Workbench.HorzScrollBar.Position,
    Position.Y - Workbench.VertScrollBar.Position,
    Size.cx,
    Size.cy
  );
end;

constructor TWArea.Create(const AWorkbench: TWWorkbench; const APosition: TCoord);
begin
  inherited;

  FSize.cx := 0;
  FSize.cy := 0;
end;

function TWArea.GetArea(): TRect;
begin
  Result.Left := FPosition.X;
  Result.Top := FPosition.Y;
  Result.Right := Result.Left + Size.cx;
  Result.Bottom := Result.Top + Size.cy;
end;

procedure TWArea.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ((Width = 0) and (Height = 0)) then
    FResizeMode := rmCreate;

  FMouseDownSize := FSize;

  inherited;
end;

procedure TWArea.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FResizeMode := rmNone;
end;

procedure TWArea.ChangeSize(const Sender: TWControl; const Shift: TShiftState; X, Y: Integer);
var
  NewPosition: TCoord;
begin
  case (ResizeMode) of
    rmCreate:
      begin
        FSize.cx := Abs(X - MouseDownPoint.X);
        FSize.cy := Abs(Y - MouseDownPoint.Y);

        NewPosition := Coord(Min(MouseDownPosition.X, X), Min(MouseDownPosition.Y, Y));
      end;
    rmN:
      begin
        FSize.cy := MouseDownSize.cy - (Y - MouseDownPoint.Y);

        NewPosition := Coord(MouseDownPosition.X, MouseDownPosition.Y + (Y - MouseDownPoint.Y));
      end;
    rmNE:
      begin
        FSize.cx := MouseDownSize.cx + (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy - (Y - MouseDownPoint.Y);

        NewPosition := Coord(MouseDownPosition.X, MouseDownPosition.Y + (Y - MouseDownPoint.Y));
      end;
    rmE:
      begin
        FSize.cx := MouseDownSize.cx + (X - MouseDownPoint.X);

        NewPosition := MouseDownPosition;
      end;
    rmSE:
      begin
        FSize.cx := MouseDownSize.cx + (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy + (Y - MouseDownPoint.Y);

        NewPosition := MouseDownPosition;
      end;
    rmS:
      begin
        FSize.cy := MouseDownSize.cy + (Y - MouseDownPoint.Y);

        NewPosition := MouseDownPosition;
      end;
    rmSW:
      begin
        FSize.cx := MouseDownSize.cx - (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy + (Y - MouseDownPoint.Y);

        NewPosition := Coord(MouseDownPosition.X + (X - MouseDownPoint.X), MouseDownPosition.Y);
      end;
    rmW:
      begin
        FSize.cx := MouseDownSize.cx - (X - MouseDownPoint.X);

        NewPosition := Coord(MouseDownPosition.X + (X - MouseDownPoint.X), MouseDownPosition.Y);
      end;
    rmNW:
      begin
        FSize.cx := MouseDownSize.cx - (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy - (Y - MouseDownPoint.Y);

        NewPosition := Coord(MouseDownPosition.X + (X - MouseDownPoint.X), MouseDownPosition.Y + (Y - MouseDownPoint.Y));
      end;
  end;

  Moving(Sender, Shift, NewPosition);
  MoveTo(Self, Shift, NewPosition);
end;

{ TWPoint *********************************************************************}

procedure TWPoint.ApplyPosition();
begin
  SetBounds(
    Position.X - (PointSize - 1) div 2 - Workbench.HorzScrollBar.Position,
    Position.Y - (PointSize - 1) div 2 - Workbench.VertScrollBar.Position,
    PointSize,
    PointSize);

  Center.X := (PointSize - 1) div 2;
  Center.Y := (PointSize - 1) div 2;
end;

function TWPoint.ControlAlign(const Control: TWControl): TAlign;
var
  ControlPosition: TCoord;
begin
  if (not Assigned(Control)) then
    Result := alNone
  else if (Control is TWTable) then
  begin
    if (Position.X > TWTable(Control).Area.Right) then
      Result := alLeft
    else if (Position.Y > TWTable(Control).Area.Bottom) then
      Result := alTop
    else if (Position.X < TWTable(Control).Area.Left) then
      Result := alRight
    else if (Position.Y < TWTable(Control).Area.Top) then
      Result := alBottom
    else
      Result := alNone;
  end
  else
  begin
    if (Control is TWPoint) then
      ControlPosition := Control.Position
    else if ((Control is TWLine) and Assigned(TWLine(Control).PointA) and (TWLine(Control).PointA <> Self)) then
      ControlPosition := TWLine(Control).PointA.Position
    else if ((Control is TWLine) and Assigned(TWLine(Control).PointB) and (TWLine(Control).PointB <> Self)) then
      ControlPosition := TWLine(Control).PointB.Position
    else
      begin ControlPosition.X := -1; ControlPosition.Y := -1; end;

    if ((ControlPosition.X < 0) or (ControlPosition.Y < 0)) then
      Result := alNone
    else if ((ControlPosition.X < Position.X) and (ControlPosition.Y = Position.Y)) then
      Result := alLeft
    else if ((ControlPosition.Y < Position.Y) and (ControlPosition.X = Position.X)) then
      Result := alTop
    else if ((ControlPosition.X > Position.X) and (ControlPosition.Y = Position.Y)) then
      Result := alRight
    else if ((ControlPosition.Y > Position.Y) and (ControlPosition.X = Position.X)) then
      Result := alBottom
    else
      Result := alNone;
  end;
end;

constructor TWPoint.Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const PreviousPoint: TWPoint = nil);
begin
  inherited Create(AWorkbench, APosition);
  Parent := AWorkbench;

  ControlA := nil;
  ControlB := nil;
  MoveState := msNormal;

  Canvas.Pen.Width := LineWidth;
  Canvas.Brush.Style := bsClear;

  if (Assigned(PreviousPoint)) then
    FSelected := PreviousPoint.Selected;

  MoveTo(nil, [], APosition);

  Workbench.UpdateControl(Self);
end;

destructor TWPoint.Destroy();
begin
  if (Assigned(LineB)) then
    LineB.Free();

  inherited;
end;

function TWPoint.GetLastPoint(): TWPoint;
begin
  Result := Self;
  while (Assigned(Result) and Assigned(Result.LineB) and Assigned(Result.LineB.PointB)) do
    Result := Result.LineB.PointB;
end;

function TWPoint.GetLineA(): TWLine;
begin
  if (not (ControlA is TWLine)) then
    Result := nil
  else
    Result := TWLine(ControlA);
end;

function TWPoint.GetLineB(): TWLine;
begin
  if (not (ControlB is TWLine)) then
    Result := nil
  else
    Result := TWLine(ControlB);
end;

procedure TWPoint.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (Workbench.State in [wsCreateLink, wsCreateForeignKey, wsCreateSection]) then
    Cursor := crDefault
  else if ((ssLeft in Shift) or not Assigned(ControlA) or not Assigned(ControlB)) then
    Cursor := crCross
  else if (ControlAlign(ControlA) = ControlAlign(ControlB)) then
    Cursor := crSizeAll
  else if ((ControlA is TWTable) and (ControlAlign(ControlA) in [alTop, alBottom]) or (ControlB is TWTable) and (ControlAlign(ControlB) in [alTop, alBottom])) then
    Cursor := crSizeWE
  else if ((ControlA is TWTable) and (ControlAlign(ControlA) in [alLeft, alRight]) or (ControlB is TWTable) and (ControlAlign(ControlB) in [alLeft, alRight])) then
    Cursor := crSizeNS
  else if ((ControlAlign(ControlA) in [alTop, alBottom]) and (ControlAlign(ControlB) in [alTop, alBottom])) then
    Cursor := crSizeWE
  else if ((ControlAlign(ControlA) in [alLeft, alRight]) and (ControlAlign(ControlB) in [alLeft, alRight])) then
    Cursor := crSizeNS
  else if ((ControlAlign(ControlA) = alBottom) and (ControlAlign(ControlB) = alLeft)
         or (ControlAlign(ControlA) = alTop) and (ControlAlign(ControlB) = alRight)
         or (ControlAlign(ControlA) = alRight) and (ControlAlign(ControlB) = alTop)
         or (ControlAlign(ControlA) = alLeft) and (ControlAlign(ControlB) = alBottom)) then
    Cursor := crSizeNESW
  else if ((ControlAlign(ControlA) = alBottom) and (ControlAlign(ControlB) = alRight)
         or (ControlAlign(ControlA) = alTop) and (ControlAlign(ControlB) = alLeft)
         or (ControlAlign(ControlA) = alRight) and (ControlAlign(ControlB) = alBottom)
         or (ControlAlign(ControlA) = alLeft) and (ControlAlign(ControlB) = alTop)) then
    Cursor := crSizeNWSE
  else
    Cursor := crDefault;

  inherited;
end;

procedure TWPoint.MoveTo(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord);

  procedure MovePointTo(const Point: TWLinkPoint);
  var
    Control, AntiControl: TWControl;
    Line, NextLine, AntiLine: TWLine;
    NextPoint: TWPoint;
  begin
    if (Assigned(LineA) and (Point = LineA.PointA)) then
    begin
      Control := ControlA;
      AntiControl := ControlB;
      Line := LineA;
      NextLine := Point.LineA;
      AntiLine := LineB;
      if (not Assigned(NextLine)) then
        NextPoint := nil
      else
        NextPoint := NextLine.PointA;
    end
    else
    begin
      Control := ControlB;
      AntiControl := ControlA;
      Line := LineB;
      NextLine := Point.LineB;
      AntiLine := LineA;
      if (not Assigned(NextLine)) then
        NextPoint := nil
      else
        NextPoint := NextLine.PointB;
    end;

    // move a point in a different orientation away from a line or from a fixed point
    if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alTop, alBottom]) and (NewPosition.X < Point.Position.X) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alLeft) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Coord(Point.Position.X, NewPosition.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewPosition.X := Position.X;
    end
    else if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alLeft, alRight]) and (NewPosition.Y < Point.Position.Y) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alTop) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Coord(NewPosition.X, Point.Position.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewPosition.Y := Position.Y;
    end
    else if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alTop, alBottom]) and (NewPosition.X > Point.Position.X) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alRight) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Coord(Point.Position.X, NewPosition.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewPosition.X := Position.X;
    end
    else if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alLeft, alRight]) and (NewPosition.Y > Point.Position.Y) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alBottom) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Coord(NewPosition.X, Point.Position.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewPosition.Y := Position.Y;
    end

    // move line away from a moved table
    else if (Assigned(NextLine) and (Point.MoveState <> msFixed) and
      ((ControlAlign(Line) = alLeft) and ((NewPosition.X = Point.Position.X) or (NewPosition.X <= Point.Position.X) and ((ControlAlign(AntiControl) = alRight)))
      or (ControlAlign(Line) = alTop) and ((NewPosition.Y = Point.Position.Y) or (NewPosition.Y <= Point.Position.Y) and ((ControlAlign(AntiControl) = alBottom)))
      or (ControlAlign(Line) = alRight) and ((NewPosition.X = Point.Position.X) or (NewPosition.X >= Point.Position.X) and ((ControlAlign(AntiControl) = alLeft)))
      or (ControlAlign(Line) = alBottom) and ((NewPosition.Y = Point.Position.Y) or (NewPosition.Y >= Point.Position.Y) and ((ControlAlign(AntiControl) = alTop))))) then
      Point.MoveTo(Self, Shift, NewPosition)

    // adjust other end of a line
    else if ((Sender <> Point) and (Self is TWLinkPoint) and Assigned(TWLinkPoint(Self).Link.ParentTable) and (Point.MoveState = msNormal)
      and ((ControlAlign(Line) in [alLeft, alRight]) or (Point.ControlAlign(NextLine) in [alTop, alBottom]) and (Point.Position.Y = Position.Y) and (Point.Position.X = Position.X))) then
      Point.MoveTo(Self, Shift, Coord(Point.Position.X, NewPosition.Y))
    else if ((Sender <> Point) and (Self is TWLinkPoint) and Assigned(TWLinkPoint(Self).Link.ParentTable) and (Point.MoveState = msNormal)
      and ((ControlAlign(Line) in [alTop, alBottom]) or (Point.ControlAlign(NextLine) in [alLeft, alRight]) and (Point.Position.X = Position.X) and (Point.Position.Y = Position.Y))) then
      Point.MoveTo(Self, Shift, Coord(NewPosition.X, Point.Position.Y));

    // remove a crushed line
    if ((Sender <> Self)
      and (Self is TWLinkPoint) and Assigned(TWLinkPoint(Self).Link.ParentTable)
      and Assigned(NextPoint) and (Point.Position.X = NextPoint.Position.X) and (Point.Position.Y = NextPoint.Position.Y)
      and (MoveState <> msAutomatic)) then
      FreeSegment(Point, NextLine);
  end;

var
  NewPoint: TWPoint;
  NewPoint2: TWPoint;
  OrgNewPosition: TCoord;
  TempOrientation: TWLineOrientation;
begin
  if (NewPosition <> Position) then
  begin
    NewPoint := nil;
    if ((Sender <> Self) and (MoveState <> msFixed)) then
    begin
      OrgNewPosition := NewPosition;
      Moving(Sender, Shift, NewPosition);
      if (NewPosition <> OrgNewPosition) then
        NewPoint := CreateSegment(Sender, OrgNewPosition, Self, Assigned(LineA) and ((Sender = LineA) or (Sender = LineA.PointA)));
    end;

    // Align "automatic" point
    if ((Sender = Self) and Assigned(LineA) and Assigned(LineA.PointA.LineA) and (LineA.PointA.MoveState = msAutomatic)) then
    begin
      if (ControlAlign(LineB) in [alLeft, alRight]) then
        TempOrientation := foVertical
      else if (ControlAlign(LineB) in [alTop, alBottom]) then
        TempOrientation := foHorizontal
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alLeft)
        and (NewPosition.X < LineA.PointA.LineA.PointA.Position.X)) then
        TempOrientation := foHorizontal
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alTop)
        and (NewPosition.Y < LineA.PointA.LineA.PointA.Position.Y)) then
        TempOrientation := foVertical
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alRight)
        and (NewPosition.X > LineA.PointA.LineA.PointA.Position.X)) then
        TempOrientation := foHorizontal
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alBottom)
        and (NewPosition.Y > LineA.PointA.LineA.PointA.Position.Y)) then
        TempOrientation := foVertical
      else if (Abs(NewPosition.X - LineA.PointA.LineA.PointA.Position.X) >= Abs(NewPosition.Y - LineA.PointA.LineA.PointA.Position.Y)) then
        TempOrientation := foVertical
      else
        TempOrientation := foHorizontal;

      case (TempOrientation) of
        foHorizontal: LineA.PointA.MoveTo(Self, [], Coord(LineA.PointA.LineA.PointA.Position.X, NewPosition.Y));
        foVertical: LineA.PointA.MoveTo(Self, [], Coord(NewPosition.X, LineA.PointA.LineA.PointA.Position.Y));
        else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Orientation']);
      end;
    end

    // build new point while creating a Foreign Key / or while inserting new point manually
    else if ((Sender = Self) and (Self is TWLinkPoint) and (Workbench.State <> wsLoading)
      and ((MoveState = msFixed)
      or (MoveState = msNormal) and (ssShift in Shift)
      or (MoveState = msAutomatic) and (Sender = Self) and (ControlAlign(LineA) in [alLeft, alRight]) and ((ControlAlign(LineA) in [alLeft, alTop]) and (NewPosition.X <> Position.X) or (ControlAlign(LineA) in [alTop, alBottom]) and (NewPosition.X <> Position.X)))) then
    begin
      if (Abs(NewPosition.X - Position.X) >= Abs(NewPosition.Y - Position.Y)) then
        NewPoint := CreateSegment(Self, Coord(NewPosition.X, Position.Y), Self, Assigned(TWLinkPoint(Self).Link.ParentTable) and Assigned(LineA) and not Assigned(LineB))
      else
        NewPoint := CreateSegment(Self, Coord(Position.X, NewPosition.Y), Self, Assigned(TWLinkPoint(Self).Link.ParentTable) and Assigned(LineA) and not Assigned(LineB));
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
      NewPoint.MoveState := msAutomatic;

      if ((not Assigned(TWLinkPoint(Self).Link.ParentTable) or Assigned(LineB))
        and ((NewPosition.X <> NewPoint.Position.X) or (NewPosition.Y <> NewPoint.Position.Y))) then
      begin
        NewPoint2 := CreateSegment(Self, NewPosition, NewPoint, Assigned(TWLinkPoint(Self).Link.ParentTable) and Assigned(LineA) and not Assigned(LineB));
        NewPoint2.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
        NewPosition := Position;
      end
      else if (Abs(NewPosition.X - Position.X) >= Abs(NewPosition.Y - Position.Y)) then
        NewPosition.X := Position.X
      else
        NewPosition.Y := Position.Y;
    end

    // move a point away from a fixed point
    else if ((Sender = Self) and Assigned(LineA) and (NewPosition.Y <> Position.Y) and (LineA.Orientation = foHorizontal) and (LineA.PointA.MoveState = msFixed)) then
    begin
      NewPoint := CreateSegment(Self, Coord(Position.X, NewPosition.Y), Self, False);
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
    end
    else if ((Sender = Self) and Assigned(LineA) and (NewPosition.X <> Position.X) and (LineA.Orientation = foVertical) and (LineA.PointA.MoveState = msFixed)) then
    begin
      NewPoint := CreateSegment(Self, Coord(NewPosition.X, Position.Y), Self, False);
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
    end

    else if (Assigned(LineA) and (Sender <> LineA.PointA) and (Assigned(Sender) or not LineA.PointA.Selected)) then
      MovePointTo(TWLinkPoint(LineA.PointA));

    if (Assigned(LineB) and (Sender <> LineB.PointB) and (Assigned(Sender) or not LineB.PointB.Selected)) then
      MovePointTo(TWLinkPoint(LineB.PointB));

    if (Assigned(NewPoint) and (NewPoint.MoveState = msFixed)) then
      NewPoint.MoveState := msNormal;

    if ((NewPosition.X <> Position.X) or (NewPosition.Y <> Position.Y)) then
    begin
      inherited;

      if (Assigned(LineA)) then
        Workbench.UpdateControl(LineA);
      if (Assigned(LineB)) then
        Workbench.UpdateControl(LineB);
    end;
  end;
end;

procedure TWPoint.PaintTo(const Canvas: TCanvas; const X, Y: Integer);

  procedure PaintToControl(const Control: TWControl; const Left, Top: Integer);
  begin
    if (Assigned(Control)) then
      case (ControlAlign(Control)) of
        alLeft:   begin Canvas.MoveTo(Left +        0, Top + Center.Y); Canvas.LineTo(Left + Center.X + 1, Top + Center.Y    ); end;
        alTop:    begin Canvas.MoveTo(Left + Center.X, Top +        0); Canvas.LineTo(Left + Center.X    , Top + Center.Y + 1); end;
        alRight:  begin Canvas.MoveTo(Left + Center.X, Top + Center.Y); Canvas.LineTo(Left + Width       , Top + Center.Y    ); end;
        alBottom: begin Canvas.MoveTo(Left + Center.X, Top + Center.Y); Canvas.LineTo(Left + Center.X    , Top + Height      ); end;
      end;
  end;

var
  Rect: TRect;
begin
  Rect := GetClientRect();
  OffsetRect(Rect, X, Y);

  if ((MoveState <> msFixed) and ((ControlAlign(LineA) = InvertAlign(ControlAlign(LineB))) and (ControlAlign(LineA) <> alNone)
    or not Assigned(ControlA) or not Assigned(ControlB)
    or Assigned(LineA) and (ControlAlign(LineA) = alNone)
    or Assigned(LineB) and (ControlAlign(LineB) = alNone))) then
  begin
    Canvas.Brush.Color := clRed;
    Canvas.FillRect(Rect);
  end
  else if (not Selected) then
    Canvas.Brush.Color := clWindow
  else
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.FillRect(Rect);
  end;

  if (Selected) then
    Canvas.Pen.Color := clHighlightText
  else if ((Self is TWLinkPoint) and Assigned(TWLinkPoint(Self).Link) and not (TWLinkPoint(Self).Link is TWForeignKey)) then
    Canvas.Pen.Color := clGrayText
  else
    Canvas.Pen.Color := clWindowText;

  if (ControlA is TWLine) then
    PaintToControl(ControlA, Rect.Left, Rect.Top);

  if (ControlB is TWLine) then
    PaintToControl(ControlB, Rect.Left, Rect.Top);
end;

procedure TWPoint.SetLineA(ALineA: TWLine);
begin
  if (Assigned(LineA)) then
    LineA.PointB := nil;

  ControlA := ALineA;

  if (Assigned(LineA)) then
  begin
    LineA.PointB := Self;
    if ((Workbench.SelCount = 1) or (Self is TWLinkPoint) and TWLinkPoint(Self).Link.LinkSelected and (Workbench.SelCount = TWLinkPoint(Self).Link.PointCount * 2 - 1)) then
      Selected := LineA.PointA.Selected;
    Workbench.UpdateControl(LineA);
  end;
end;

procedure TWPoint.SetLineB(ALineB: TWLine);
begin
  if (Assigned(LineB)) then
    LineB.PointA := nil;

  ControlB := ALineB;

  if (Assigned(LineB)) then
    LineB.PointA := Self;
end;

procedure TWPoint.SetSelected(ASelected: Boolean);
begin
  inherited;

  if (Assigned(LineA) and (LineA.PointA.Selected = Selected) and (LineA.Selected <> Selected)) then
    LineA.Selected := Selected;
  if (Assigned(LineB) and (LineB.PointB.Selected = Selected) and (LineB.Selected <> Selected)) then
    LineB.Selected := Selected;
end;

{ TWLine **********************************************************************}

procedure TWLine.ApplyPosition();
begin
  if ((FPointB.Position.X >= 0) and (FPointB.Position.Y >= 0) and (FPointB.Position.X >= 0) and (FPointB.Position.Y >= 0)) then
    case (Orientation) of
      foHorizontal:
        SetBounds(
          Min(PointA.Position.X, PointB.Position.X) + (PointSize - 1) div 2 - Workbench.HorzScrollBar.Position,
          PointA.Position.Y - (LineWidth - 1) div 2 - Workbench.VertScrollBar.Position,
          Length - PointSize + 1,
          LineWidth
        );
      foVertical:
        SetBounds(
          PointA.Position.X - (LineWidth - 1) div 2 - Workbench.HorzScrollBar.Position,
          Min(PointA.Position.Y, PointB.Position.Y) + (PointSize - 1) div 2 - Workbench.VertScrollBar.Position,
          LineWidth,
          Length - PointSize + 1
        );
    end;
end;

constructor TWLine.Create(const AWorkbench: TWWorkbench; const APointA, APointB: TWPoint);
begin
  inherited Create(AWorkbench, APointA.Position);
  Parent := AWorkbench;

  FWorkbench := AWorkbench;
  FPointA := APointA;
  FPointB := APointB;

  FPointA.ControlB := Self;
  FPointB.ControlA := Self;

  Canvas.Pen.Width := LineWidth;

  if (Assigned(PointA)) then
    FSelected := PointA.Selected;

  Workbench.UpdateControl(Self);
end;

destructor TWLine.Destroy();
begin
  if (Assigned(PointB)) then
    PointB.Free();
  if (Assigned(PointA)) then
    PointA.ControlB := nil;

  inherited;
end;

function TWLine.GetLength(): Integer;
begin
  Result := Max(Abs(PointA.Position.X - PointB.Position.X), Abs(PointA.Position.Y - PointB.Position.Y));
end;

function TWLine.GetOrientation(): TWLineOrientation;
begin
  if (Assigned(PointA) and Assigned(PointB)) then
    if ((PointA.Position.X = PointB.Position.X) and (PointA.Position.Y = PointB.Position.Y)) then
      FOrientation := foNone
    else if (PointA.Position.Y = PointB.Position.Y) then
      FOrientation := foHorizontal
    else if (PointA.Position.X = PointB.Position.X) then
      FOrientation := foVertical
    else
      FOrientation := foNone;

  Result := FOrientation;
end;

procedure TWLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  DiffPoint: TPoint;
  NewPosition: TCoord;
  NewPoint: TWPoint;
  PointANewPosition: TCoord;
begin
  if (Workbench.State in [wsCreateLink, wsCreateForeignKey, wsCreateSection]) then
    Cursor := crNo
  else if (Orientation = foHorizontal) then
    Cursor := crSizeNS
  else if (Orientation = foVertical) then
    Cursor := crSizeWE
  else
    Cursor := crDefault;

  if (not (ssLeft in Shift)) then
    inherited
  else
  begin
    DiffPoint := Point(
      Workbench.HorzScrollBar.Position + Left + X - MouseDownPoint.X,
      Workbench.VertScrollBar.Position + Top + Y - MouseDownPoint.Y);
    PointANewPosition := Coord(
      MouseDownPosition.X + DiffPoint.X,
      MouseDownPosition.Y + DiffPoint.Y);

    if (PointANewPosition.X < 0) then
      PointANewPosition.X := 0;
    if (PointANewPosition.Y < 0) then
      PointANewPosition.Y := 0;

    if ((ssShift in Shift) and (Self is TWLinkLine) and (PointA is TWLinkPoint)) then
    begin
      if ((Orientation = foHorizontal) and (PointANewPosition.Y <> PointA.Position.Y)
        or (Orientation = foVertical) and (PointANewPosition.X <> PointA.Position.X)) then
      begin
        MouseCapture := False;

        NewPosition := Workbench.Position(Left + X, Top + Y);

        TWLinkPoint(PointA).MoveState := msFixed;
        if (Orientation = foHorizontal) then
          NewPoint := CreateSegment(Self, Coord(NewPosition.X, PointA.Position.Y), PointA, False)
        else
          NewPoint := CreateSegment(Self, Coord(PointA.Position.X, NewPosition.Y), PointA, False);
        NewPoint.MoveState := msAutomatic;
        NewPoint := CreateSegment(Self, NewPosition, NewPoint, False);
        NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
      end;
    end
    else if ((Self is TWLinkLine) and (Workbench.SelCount <> TWLinkLine(Self).Link.PointCount * 2 - 1) and (Workbench.Links.SelCount <> 1)) then
      PointA.Move(Self, Shift, PointANewPosition)
    else
      case (Orientation) of
        foHorizontal:
          PointA.MoveTo(Self, Shift, Coord(PointA.Position.X, PointANewPosition.Y));
        foVertical:
          PointA.MoveTo(Self, Shift, Coord(PointANewPosition.X, PointA.Position.Y));
      end;
  end;

  Workbench.CursorMove(Workbench.HorzScrollBar.Position + Left + X, Workbench.VertScrollBar.Position + Top + Y);
end;

procedure TWLine.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := GetClientRect();
  OffsetRect(Rect, X, Y);

  if (Selected) then
  begin
    Canvas.Pen.Color := clWindow;
    Canvas.Brush.Color := clHighlight;
    Canvas.FillRect(Rect);
  end
  else if ((Self is TWLinkLine) and not (TWLinkLine(Self).Link is TWForeignKey)) then
  begin
    Canvas.Pen.Color := clGrayText;
    Canvas.Brush.Color := clWindow;
  end
  else
  begin
    Canvas.Pen.Color := clWindowText;
    Canvas.Brush.Color := clWindow;
  end;

  case (Orientation) of
    foHorizontal:
      begin
        Canvas.MoveTo(Rect.Left +     0, Rect.Top + (Height - 1) div 2);
        Canvas.LineTo(Rect.Left + Width, Rect.Top + (Height - 1) div 2);
      end;
    foVertical:
      begin
        Canvas.MoveTo(Rect.Left + (Width - 1) div 2, Rect.Top +      0);
        Canvas.LineTo(Rect.Left + (Width - 1) div 2, Rect.Top + Height);
      end;
  end;
end;

procedure TWLine.SetPointA(APointA: TWPoint);
begin
  if (Assigned(PointA)) then
    PointA.ControlB := nil;

  FPointA := APointA;

  if (Assigned(PointA)) then
  begin
    Workbench.UpdateControl(PointA);
    Workbench.UpdateControl(Self);
  end;
end;

procedure TWLine.SetPointB(APointB: TWPoint);
begin
  if (Assigned(PointB)) then
    PointB.ControlA := nil;

  FPointB := APointB;

  if (Assigned(PointB)) then
  begin
    Workbench.UpdateControl(PointB);
    Workbench.UpdateControl(Self);
  end;
end;

procedure TWLine.SetSelected(ASelected: Boolean);
begin
  inherited;

  if (Assigned(PointA) and (PointA.Selected <> Selected)) then
    PointA.Selected := FSelected;
  if (Assigned(PointB) and (PointB.Selected <> Selected)) then
    PointB.Selected := FSelected;
end;

{ TWTable *********************************************************************}

procedure TWTable.ApplyPosition();
begin
  AutoSize := True;

  inherited;
end;

function TWTable.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  I: Integer;
begin
  Result := Assigned(BaseTable);

  if (Result) then
  begin
    Canvas.Font.Style := [fsBold];
    NewWidth := Canvas.TextWidth(Caption);

    Canvas.Font.Style := [];
    for I := 0 to BaseTable.Fields.Count - 1 do
    begin
      if (not BaseTable.Fields[I].InPrimaryIndex) then
        Canvas.Font.Style := Canvas.Font.Style - [fsBold]
      else
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      NewWidth := Max(NewWidth, Canvas.TextWidth(BaseTable.Fields[I].Name));
    end;

    Inc(NewWidth, 2 * BorderSize + 2 * (Width - ClientWidth + Padding));

    NewHeight := 3 * BorderSize + (1 + BaseTable.Fields.Count) * -Canvas.Font.Height + (4 + BaseTable.Fields.Count) * Padding;
  end;
end;

constructor TWTable.Create(const ATables: TWTables; const APosition: TCoord; const ABaseTable: TCBaseTable = nil);
begin
  inherited Create(ATables.Workbench, APosition);

  FBaseTable := ABaseTable;

  Hint := BaseTable.Comment;

  FDoubleBuffered := True;
  SetLength(FLinkPoints, 0);

  Canvas.Font := Font;
  Canvas.Font.Color := Font.Color;
end;

destructor TWTable.Destroy();
begin
  while (Length(FLinkPoints) > 0) do
    Workbench.Links.Delete(Workbench.Links.IndexOf(FLinkPoints[0].Link));

  inherited;
end;

function TWTable.GetCaption(): TCaption;
begin
  Result := BaseTable.Name;
end;

function TWTable.GetLinkPoint(AIndex: Integer): TWLinkPoint;
begin
  Result := FLinkPoints[AIndex];
end;

function TWTable.GetLinkPointCount(): Integer;
begin
  Result := Length(FLinkPoints);
end;

function TWTable.GetIndex(): Integer;
begin
  Result := Workbench.Tables.IndexOf(Self);
end;

procedure TWTable.Invalidate();
begin
  if (CanAutoSize(FSize.cx, FSize.cy)) then
    ApplyPosition();

  inherited;

  if (not Assigned(BaseTable)) then
    Hint := ''
  else
    Hint := BaseTable.Comment;
end;

procedure TWTable.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  Workbench.UpdateControl(Self);
end;

procedure TWTable.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Workbench.State in [wsCreateLink, wsCreateForeignKey]) then
  begin
    if (Workbench.State = wsCreateLink) then
      Workbench.CreatedLink := TWLink.Create(Workbench, Workbench.Position(Left + X, Top + Y))
    else
      Workbench.CreatedLink := TWForeignKey.Create(Workbench, Workbench.Position(Left + X, Top + Y));
    Workbench.CreatedLink.TableA := Self;
    Workbench.CreatedLink.MoveState := msFixed;
    Workbench.CreatedLink.MouseDown(Button, Shift, Workbench.HorzScrollBar.Position + Left + X - Workbench.CreatedLink.Left, Workbench.VertScrollBar.Position + Top + Y - Workbench.CreatedLink.Top);

    Workbench.State := wsNormal;
  end
  else
    inherited;
end;

procedure TWTable.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (Workbench.State in [wsCreateLink, wsCreateForeignKey]) then
    Cursor := crCross
  else if (Workbench.State = wsCreateSection) then
    Cursor := crNo
  else
    Cursor := crDefault;

  inherited;
end;

procedure TWTable.MoveTo(const Sender: TWControl; const Shift: TShiftState; NewPosition: TCoord);
var
  I: Integer;
begin
  for I := 0 to Length(FLinkPoints) - 1 do
    if (not FLinkPoints[I].Selected) then
      FLinkPoints[I].MoveTo(Self, Shift, Coord(FLinkPoints[I].Position.X + NewPosition.X - Position.X, FLinkPoints[I].Position.Y + NewPosition.Y - Position.Y));

  inherited;
end;

procedure TWTable.Moving(const Sender: TWControl; const Shift: TShiftState; var NewPosition: TCoord);
var
  I: Integer;
  TempCoord: TCoord;
begin
  inherited;

  for I := 0 to Length(FLinkPoints) - 1 do
  begin
    TempCoord := Coord(FLinkPoints[I].Position.X + (NewPosition.X - Position.X), FLinkPoints[I].Position.Y + (NewPosition.Y - Position.Y));
    FLinkPoints[I].Moving(Self, Shift, TempCoord);
    NewPosition := Coord(Position.X + TempCoord.X - FLinkPoints[I].Position.X, Position.Y + TempCoord.Y - FLinkPoints[I].Position.Y);
  end;
end;

procedure TWTable.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
var
  BottomColor: TColor;
  TopColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

var
  Flags: Longint;
  I: Integer;
  Rect: TRect;
begin
  Rect := ClientRect;
  OffsetRect(Rect, X, Y);

  if (not Selected) then
  begin
    Canvas.Font.Color := clWindowText;

    Canvas.Pen.Color := clWindowText;
    Canvas.Pen.Style := psSolid;  
    Canvas.Brush.Color := clWindow;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(Rect);
  end
  else if (not Workbench.Focused() and Workbench.HideSelection) then
  begin
    Canvas.Font.Color := clWindowText;

    Canvas.Pen.Color := clWindowText;
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(Rect);
  end
  else
  begin
    Canvas.Font.Color := clHighlightText;

    Canvas.Pen.Color := clWindowText;
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Color := clHighlight;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(Rect);
  end;

  if (Workbench.Focused() and Focused) then
  begin
    Canvas.Pen.Color := clHighlight;
    Canvas.Pen.Mode := pmNotCopy;
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Color := clHighlight;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(Rect);

    Canvas.Pen.Mode := pmCopy;
  end;

  Canvas.Brush.Style := bsClear;

  Canvas.Pen.Color := Canvas.Font.Color;
  Canvas.Pen.Style := psSolid;
  for I := 0 to BorderSize - 1 do
  begin
    Canvas.MoveTo(Rect.Left + BorderSize, Rect.Top + BorderSize + 2 * Padding + -Canvas.Font.Height + I);
    Canvas.LineTo(Rect.Left + ClientWidth - BorderSize, Rect.Top + BorderSize + 2 * Padding + -Canvas.Font.Height + I);
  end;

  Inc(Rect.Left, BorderSize + Padding); Dec(Rect.Right, BorderSize - 1 + Padding);
  Inc(Rect.Top, BorderSize - 1 + Padding); Dec(Rect.Bottom, BorderSize - 1 + Padding);

  Flags := DrawTextBiDiModeFlags(DT_CENTER);
  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, PChar(Caption), -1, Rect, Flags);
  Inc(Rect.Top, 2 * BorderSize + 2 * Padding + -Canvas.Font.Height);

  Flags := DrawTextBiDiModeFlags(0);

  for I := 0 to BaseTable.Fields.Count - 1 do
  begin
    if (not BaseTable.Fields[I].InPrimaryIndex) then
      Canvas.Font.Style := Canvas.Font.Style - [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    DrawText(Canvas.Handle, PChar(BaseTable.Fields[I].Name), -1, Rect, Flags);
    Inc(Rect.Top, -Canvas.Font.Height + Padding);
  end;
end;

procedure TWTable.RegisterLinkPoint(const ALinkPoint: TWLinkPoint);
var
  Found: Boolean;
  I: Integer;
begin
  Found := False;
  for I := 0 to Length(FLinkPoints) - 1 do
    Found := Found or (FLinkPoints[I] = ALinkPoint);

  if (not Found) then
  begin
    SetLength(FLinkPoints, Length(FLinkPoints) + 1);
    FLinkPoints[Length(FLinkPoints) - 1] := ALinkPoint;
  end;
end;

procedure TWTable.ReleaseLinkPoint(const ALinkPoint: TWLinkPoint);
var
  I: Integer;
  Index: Integer;
begin
  Index := -1;
  for I := 0 to Length(FLinkPoints) - 1 do
    if (FLinkPoints[I] = ALinkPoint) then
      Index := I;

  if (Index >= 0) then
  begin
    for I := Index to Length(FLinkPoints) - 2 do
      FLinkPoints[I] := FLinkPoints[I + 1];

    SetLength(FLinkPoints, Length(FLinkPoints) - 1);
  end;
end;

procedure TWTable.SetFocused(AFocused: Boolean);
begin
  FFocused := AFocused;

  Invalidate();
end;

{ TWTables ********************************************************************}

function TWTables.GetSelCount(): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (TWTable(Items[I]).Selected) then
      Inc(Result);
end;

function TWTables.GetTable(Index: Integer): TWTable;
begin
  Result := TWTable(Items[Index]);
end;

procedure TWTables.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
  J: Integer;
  Node: IXMLNode;
begin
  for I := XML.ChildNodes.Count - 1 downto 0 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'table') and not Assigned(Workbench.TableByCaption(XML.ChildNodes.Nodes[I].Attributes['name'])) then
      XML.ChildNodes.Delete(I);

  for I := 0 to Count - 1 do
  begin
    Node := nil;
    for J := 0 to XML.ChildNodes.Count - 1 do
      if ((XML.ChildNodes.Nodes[J].NodeName = 'table') and (Workbench.TableByCaption(XML.ChildNodes.Nodes[J].Attributes['name']) = Table[I])) then
        Node := XML.ChildNodes.Nodes[J];
    if (not Assigned(Node)) then
    begin
      Node := XML.AddChild('table');
      Node.Attributes['name'] := Table[I].Caption;
    end;

    Table[I].SaveToXML(Node);
  end;
end;

{ TWLinkPoint ***********************************************************}

procedure TWLinkPoint.ApplyPosition();
var
  TempBottom: Integer;
  TempLeft: Integer;
  TempRight: Integer;
  TempTop: Integer;

  procedure ExpandTableAlign(const Table: TWTable);
  begin
    case (ControlAlign(Table)) of
      alLeft:
        begin
          Center.X   := Max(Center.X, ConnectorSize + (PointSize - 1) div 2);
          Center.Y   := Max(Center.Y, (ConnectorSize - 1) div 2            );

          TempLeft   := Min(TempLeft  , Position.X - (ConnectorSize + 1)      );
          TempTop    := Min(TempTop   , Position.Y - (ConnectorSize - 1) div 2);
          TempBottom := Max(TempBottom, Position.Y + (ConnectorSize - 1) div 2);
        end;
      alTop:
        begin
          Center.X   := Max(Center.X, (ConnectorSize - 1) div 2            );
          Center.Y   := Max(Center.Y, ConnectorSize + (PointSize - 1) div 2);

          TempLeft   := Min(TempLeft  , Position.X - (ConnectorSize - 1) div 2);
          TempTop    := Min(TempTop   , Position.Y - (ConnectorSize + 1)      );
          TempRight  := Max(TempRight , Position.X + (ConnectorSize - 1) div 2);
        end;
      alRight:
        begin
          Center.Y   := Max(Center.Y, (ConnectorSize - 1) div 2);

          TempTop    := Min(TempTop   , Position.Y - (ConnectorSize - 1) div 2);
          TempRight  := Max(TempRight , Position.X + (ConnectorSize + 1)      );
          TempBottom := Max(TempBottom, Position.Y + (ConnectorSize - 1) div 2);
        end;
      alBottom:
        begin
          Center.X   := Max(Center.X, (ConnectorSize - 1) div 2);

          TempLeft   := Min(TempLeft  , Position.X - (ConnectorSize - 1) div 2);
          TempRight  := Max(TempRight , Position.X + (ConnectorSize - 1) div 2);
          TempBottom := Max(TempBottom, Position.Y + (ConnectorSize + 1)      );
        end;
    end;
  end;

begin
  Center := Point((PointSize - 1) div 2, (PointSize - 1) div 2);

  TempLeft   := Position.X - (PointSize - 1) div 2;
  TempTop    := Position.Y - (PointSize - 1) div 2;
  TempRight  := Position.X + (PointSize - 1) div 2;
  TempBottom := Position.Y + (PointSize - 1) div 2;

  ExpandTableAlign(TableA);
  ExpandTableAlign(TableB);

  SetBounds(
    TempLeft - Workbench.HorzScrollBar.Position,
    TempTop - Workbench.VertScrollBar.Position,
    TempRight - TempLeft + 1,
    TempBottom - TempTop + 1
  );
end;

constructor TWLinkPoint.Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const APreviousPoint: TWPoint = nil);
begin
  inherited Create(AWorkbench, APosition, APreviousPoint);

  if (APreviousPoint is TWLinkPoint) then
    LineA := TWLinkLine.Create(Workbench, TWLinkPoint(APreviousPoint), Self);

  Workbench.UpdateControl(Self);
end;

destructor TWLinkPoint.Destroy();
begin
  TableA := nil;
  TableB := nil;

  inherited;
end;

function TWLinkPoint.GetLink(): TWLink;
var
  Point: TWLinkPoint;
begin
  Point := Self;
  while (Assigned(Point.LineA) and (Point.LineA.PointA is TWLinkPoint)) do
    Point := TWLinkPoint(Point.LineA.PointA);

  if (not Assigned(Point)) then
    raise Exception.Create('Point is not assigned')
  else if (not (Point is TWLink)) then
    raise Exception.Create('Point is not TWLink')
  else
    Result := TWLink(Point);
end;

function TWLinkPoint.GetIndex(): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Link.PointCount - 1 do
    if (Self = Link.Points[I]) then
      Result := I;
end;

function TWLinkPoint.GetTableA(): TWTable;
begin
  if (not (ControlA is TWTable)) then
    Result := nil
  else
    Result := TWTable(ControlA);
end;

function TWLinkPoint.GetTableB(): TWTable;
begin
  if (not (ControlB is TWTable)) then
    Result := nil
  else
    Result := TWTable(ControlB);
end;

procedure TWLinkPoint.Moving(const Sender: TWControl; const Shift: TShiftState; var NewPosition: TCoord);

  procedure MovingConnector(const Sender: TWControl; const Shift: TShiftState; const Table: TWTable; var NewPosition: TCoord);
  var
    TempAlign: TAlign;
  begin
    if ((NewPosition.X > Table.Position.X + Table.Width)
      and (Table.Position.Y + (ConnectorSize - 1) div 2 <= NewPosition.Y) and (NewPosition.Y < Table.Position.Y + Table.Height - (ConnectorSize - 1) div 2)) then
      TempAlign := alLeft
    else if ((NewPosition.Y > Table.Position.Y + Table.Height)
      and (Table.Position.X + (ConnectorSize - 1) div 2 <= NewPosition.X) and (NewPosition.X < Table.Position.X + Table.Width - (ConnectorSize - 1) div 2)) then
      TempAlign := alTop
    else if ((NewPosition.X < Table.Position.X)
      and (Table.Position.Y + (ConnectorSize - 1) div 2 <= NewPosition.Y) and (NewPosition.Y < Table.Position.Y + Table.Height - (ConnectorSize - 1) div 2)) then
      TempAlign := alRight
    else if ((NewPosition.Y < Table.Position.Y)
      and (Table.Position.X + (ConnectorSize - 1) div 2 <= NewPosition.X) and (NewPosition.X < Table.Position.X + Table.Width - (ConnectorSize - 1) div 2)) then
      TempAlign := alBottom
    else
      TempAlign := ControlAlign(Table);

    if (TempAlign in [alTop, alBottom]) then
      if (NewPosition.X < Table.Position.X + (ConnectorSize - 1) div 2) then
        NewPosition.X := Table.Position.X + (ConnectorSize - 1) div 2
      else if (NewPosition.X > Table.Position.X + Table.Width - (ConnectorSize - 1) div 2 - 1) then
        NewPosition.X := Table.Position.X + Table.Width - (ConnectorSize - 1) div 2 - 1;

    if (TempAlign in [alLeft, alRight]) then
      if (NewPosition.Y < Table.Position.Y + (ConnectorSize - 1) div 2) then
        NewPosition.Y := Table.Position.Y + (ConnectorSize - 1) div 2
      else if (NewPosition.Y > Table.Position.Y + Table.Height - (ConnectorSize - 1) div 2 - 1) then
        NewPosition.Y := Table.Position.Y + Table.Height - (ConnectorSize - 1) div 2 - 1;

    case (TempAlign) of
      alLeft: NewPosition.X := Table.Position.X + (Table.Width + ConnectorSize + (PointSize - 1) div 2);
      alTop: NewPosition.Y := Table.Position.Y + (Table.Height + ConnectorSize + (PointSize - 1) div 2);
      alRight: NewPosition.X := Table.Position.X - (ConnectorSize + (PointSize + 1) div 2);
      alBottom: NewPosition.Y := Table.Position.Y - (ConnectorSize + (PointSize + 1) div 2);
    end;
  end;

begin
  inherited;

  if ((Sender <> Self) and Assigned(LineA) and Assigned(LineB) and Assigned(Link.ParentTable)) then
    if ((ControlAlign(LineA) <> alNone) and (ControlAlign(LineA) = ControlAlign(LineB))) then
      NewPosition := Position
    else if ((ControlAlign(LineA) = alLeft) and (ControlAlign(LineB) = alRight)
      or (ControlAlign(LineA) = alRight) and (ControlAlign(LineB) = alLeft)) then
      NewPosition.Y := Position.Y
    else if ((ControlAlign(LineA) = alTop) and (ControlAlign(LineB) = alBottom)
      or (ControlAlign(LineA) = alBottom) and (ControlAlign(LineB) = alTop)) then
      NewPosition.X := Position.X;

  if ((Sender <> TableA) and Assigned(TableA) and not (Selected and TableA.Selected)) then
    MovingConnector(Sender, Shift, TableA, NewPosition);
  if ((Sender <> TableB) and Assigned(TableB) and not (Selected and TableB.Selected)) then
    MovingConnector(Sender, Shift, TableB, NewPosition);
end;

procedure TWLinkPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not Assigned(Link.ParentTable) and (Link.PointCount > 1)) then
    TableB := Workbench.TableAt(Coord(Position.X, Position.Y));

  inherited;

  if (not Assigned(Link.ParentTable)) then
  begin
    MouseDown(Button, Shift, X, Y);
    MoveState := msFixed;
    if ((Link.PointCount = 2) and not Assigned(Link.ParentTable) and Assigned(LineA)
      and (Workbench.TableAt(Position) = Link.ChildTable)) then
    begin
      LineA.PointA.MoveState := msFixed;
      MoveState := msAutomatic;
    end;
  end
  else if (Link = Workbench.CreatedLink) then
  begin
    if (not Workbench.OnValidateControl(Workbench, Workbench.CreatedLink)) then
      FreeAndNil(Workbench.CreatedLink)
    else if ((Workbench.CreatedLink is TWLink) and not (Workbench.CreatedLink is TWForeignKey)) then
    begin
      Workbench.Links.Add(Workbench.CreatedLink);
      Workbench.CreatedLink := nil;
    end;
  end;
end;

procedure TWLinkPoint.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
begin
  inherited;

  if (Assigned(TableA) and ((MoveState <> msFixed) or Assigned(Link.ParentTable))) then
    case (ControlAlign(TableA)) of
      alLeft:
        begin
          Canvas.MoveTo(X + (ConnectorSize - 1) div 2, Y + Center.Y); Canvas.LineTo(X + Center.X, Y + Center.Y);

          Canvas.Arc(X - (Height - 1) div 2, Y, X + (Height - 1) div 2 + 1, Y + Height, X + 1, Y + Height + 1, X - 1, Y - 1);
        end;
      alTop:
        begin
          Canvas.MoveTo(X + Center.X, Y + (ConnectorSize - 1) div 2); Canvas.LineTo(X + Center.X, Y + Center.Y);

          Canvas.Arc(X, Y - (Width - 1) div 2, X + Width, Y + (Width - 1) div 2 + 1, X - 1, Y, X + Width, Y - 1);
        end;
      alRight:
        begin
          Canvas.MoveTo(X + Center.X, Y + Center.Y); Canvas.LineTo(X + Width - (ConnectorSize - 1) div 2, Y + Center.Y);

          Canvas.Arc(X + Width - (Height - 1) div 2 - 1, Y, X + Width + (Height - 1) div 2, Y + Height, X + Width, Y, X + Width, Y + Height - 1);
        end;
      alBottom:
        begin
          Canvas.MoveTo(X + Center.X, Y + Center.Y); Canvas.LineTo(X + Center.X, Y + Height - (ConnectorSize - 1) div 2);

          Canvas.Arc(X, Y + Height - (Width - 1) div 2 - 1, X + Width, Y + Height + (Width - 1) div 2, X + Width, Y + Height, X - 1, Y + Height + 1);
        end;
    end;

  if (Assigned(TableB)) then
    case (ControlAlign(TableB)) of
      alLeft:
        begin
          Canvas.MoveTo(X + 0, Y + Center.Y); Canvas.LineTo(X + Center.X - 1, Y +       -1);
          Canvas.MoveTo(X + 0, Y + Center.Y); Canvas.LineTo(X + Center.X    , Y + Center.Y);
          Canvas.MoveTo(X + 0, Y + Center.Y); Canvas.LineTo(X + Center.X - 1, Y +  Height );
        end;
      alTop:
        begin
          Canvas.MoveTo(X + Center.X, Y + 0); Canvas.LineTo(X +       -1, Y + Center.Y - 1);
          Canvas.MoveTo(X + Center.X, Y + 0); Canvas.LineTo(X + Center.X, Y + Center.Y    );
          Canvas.MoveTo(X + Center.X, Y + 0); Canvas.LineTo(X +  Width  , Y + Center.Y - 1);
        end;
      alRight:
        begin
          Canvas.MoveTo(X + Width - 1, Y + Center.Y); Canvas.LineTo(X + Center.X + 1, Y +       -1);
          Canvas.MoveTo(X + Width - 1, Y + Center.Y); Canvas.LineTo(X + Center.X    , Y + Center.Y);
          Canvas.MoveTo(X + Width - 1, Y + Center.Y); Canvas.LineTo(X + Center.X + 1, Y +  Height );
        end;
      alBottom:
        begin
          Canvas.MoveTo(X + Center.X, Y + Height - 1); Canvas.LineTo(X +       -1, Y + Center.Y + 1);
          Canvas.MoveTo(X + Center.X, Y + Height - 1); Canvas.LineTo(X + Center.X, Y + Center.Y    );
          Canvas.MoveTo(X + Center.X, Y + Height - 1); Canvas.LineTo(X +  Width  , Y + Center.Y + 1);
        end;
    end;
end;

procedure TWLinkPoint.SetTableA(ATableA: TWTable);
begin
  if (ControlA is TWTable) then
    TWTable(ControlA).ReleaseLinkPoint(Self);

  ControlA := ATableA;

  if (Assigned(TableA)) then
    TableA.RegisterLinkPoint(Self);
end;

procedure TWLinkPoint.SetTableB(ATableB: TWTable);
begin
  if (Assigned(TableB)) then
    TableB.ReleaseLinkPoint(Self);

  ControlB := ATableB;

  if (Assigned(TableB)) then
    TableB.RegisterLinkPoint(Self);
end;

{ TWLinkLine ************************************************************}

procedure TWLinkLine.ApplyPosition();
begin
  if (Assigned(Workbench) and Assigned(PointA) and Assigned(PointB)) then
    case (Orientation) of
      foHorizontal:
        SetBounds(
          Min(PointA.Position.X, PointB.Position.X) + (PointSize + 1) div 2 - Workbench.HorzScrollBar.Position,
          PointA.Position.Y - (PointSize - 1) div 2 - Workbench.VertScrollBar.Position,
          Length - PointSize,
          PointSize
        );
      foVertical:
        SetBounds(
          PointA.Position.X - (PointSize - 1) div 2 - Workbench.HorzScrollBar.Position,
          Min(PointA.Position.Y, PointB.Position.Y) + (PointSize + 1) div 2 - Workbench.VertScrollBar.Position,
          PointSize,
          Length - PointSize
        );
    end;
end;

function TWLinkLine.GetLink(): TWLink;
begin
  if (not (PointA is TWLinkPoint)) then
    raise Exception.Create('No PointA')
  else
    Result := TWLinkPoint(PointA).Link;
end;

{ TWLink ****************************************************************}

procedure TWLink.Cleanup(const Sender: TWControl);

  function PointAlign(const TestPosition: TCoord; Area: TRect): TAlign;
  begin
    if (TestPosition.X < Area.Left) then
      Result := alLeft
    else if (TestPosition.Y < Area.Top) then
      Result := alTop
    else if (TestPosition.X > Area.Right) then
      Result := alRight
    else if (TestPosition.Y > Area.Bottom) then
      Result := alBottom
    else
      Result := alNone;
  end;

  procedure FixPointAlign(const Point: TWLinkPoint; const Table: TWTable);
  var
    Line: TWLine;
    LinePoint: TWPoint;
    NewPosition: TCoord;
  begin
    if (Table = Point.TableA) then
    begin
      Line := Point.LineB;
      LinePoint := Line.PointB;
    end
    else
    begin
      Line := Point.LineA;
      LinePoint := Line.PointA;
    end;

    if (Assigned(Line) and (CoordInArea(Point.Position, Table.Area) or (Point.ControlAlign(Line) = Point.ControlAlign(Table)))) then
    begin
      NewPosition := Point.Position;
      case (PointAlign(LinePoint.Position, Table.Area)) of
        alLeft: NewPosition.X := Table.Area.Left - ConnectorSize - (PointSize + 1) div 2;
        alTop: NewPosition.Y := Table.Area.Top - ConnectorSize - (PointSize + 1) div 2;
        alRight: NewPosition.X := Table.Area.Right + ConnectorSize + (PointSize + 1) div 2 - 1;
        alBottom: NewPosition.Y := Table.Area.Bottom + ConnectorSize + (PointSize + 1) div 2 - 1;
      end;
      Point.MoveTo(Point, [], NewPosition);
    end;
  end;

var
  I: Integer;
  NextPoint: TWLinkPoint;
  Point: TWLinkPoint;
  TempTable: TWTable;
begin
  for I := 0 to PointCount - 1 do
  begin
    Points[I].MoveState := msNormal;
    Points[I].MouseDownPosition := Coord(-1, -1);
    Points[I].MouseDownPoint := Types.Point(-1, -1);
  end;

  Point := TWLinkPoint(LastPoint);
  while (Assigned(Point) and (Point <> Self)) do
  begin
    NextPoint := TWLinkPoint(Point.LineA.PointA);

    if ((Workbench.TableAt(NextPoint.Position) = ParentTable)
      and (Assigned(ParentTable) and (ParentTable <> ChildTable))) then
    begin
      Point.MoveTo(Point, [], NextPoint.Position);
      if ((Sender = NextPoint) or (NextPoint = Self)) then
      begin
        NextPoint.TableB := Point.TableB;
        Point := NextPoint;
        FreeSegment(Point.LineB.PointB, Point.LineB);
      end
      else
        FreeSegment(Point.LineA.PointA, Point.LineA);
    end;

    if (not Assigned(Point.LineA) or not (Point.LineA.PointA is TWLinkPoint)) then
      Point := nil
    else
      Point := TWLinkPoint(Point.LineA.PointA);
  end;

  Point := Self;
  while (Assigned(Point) and (Point <> LastPoint)) do
  begin
    NextPoint := TWLinkPoint(Point.LineB.PointB);

    if ((Workbench.TableAt(NextPoint.Position) = ChildTable)
      and (Assigned(ParentTable) and (ParentTable <> ChildTable) or (Point.Index > 2))) then
    begin
      Point.MoveTo(Point, [], NextPoint.Position);
      if ((Sender = NextPoint) or (NextPoint = Self)) then
      begin
        TempTable := Point.TableA;
        Point := NextPoint;
        FreeSegment(Point.LineA.PointA, Point.LineA);
        if (Assigned(TempTable)) then
          Point.TableA := TempTable;
      end
      else
        FreeSegment(Point.LineB.PointB, Point.LineB);
    end;

    if (not Assigned(Point.LineB) or not (Point.LineB.PointB is TWLinkPoint)) then
      Point := nil
    else
      Point := TWLinkPoint(Point.LineB.PointB);
  end;

  Point := Self;
  repeat
    if (Assigned(Point.LineB) and (Point.LineB.PointB is TWLinkPoint)) then
    begin
      NextPoint := TWLinkPoint(Point.LineB.PointB);

      if ((NextPoint.ControlAlign(NextPoint.LineA) = InvertAlign(NextPoint.ControlAlign(NextPoint.LineB)))
        or (NextPoint.Position.X = Point.Position.X) and (NextPoint.Position.Y = Point.Position.Y)
        or Assigned(Point.TableB) and not Assigned(ParentTable)) then
      begin
        if ((Sender = NextPoint) or (NextPoint = LastPoint) and Assigned(ParentTable)) then
        begin
          TempTable := Point.TableB;
          Point := NextPoint;
          FreeSegment(Point.LineA.PointA, Point.LineA);
          if (Assigned(TempTable)) then
            Point.TableB := TempTable;
        end
        else
          FreeSegment(Point.LineB.PointB, Point.LineB);
      end;
    end;

    if (not Assigned(Point.LineB) or not (Point.LineB.PointB is TWLinkPoint)) then
      Point := nil
    else
      Point := TWLinkPoint(Point.LineB.PointB);
  until (not Assigned(Point));

  if (Assigned(Self.TableA) and Assigned(Self.LineB)) then
    FixPointAlign(Self, Self.TableA);
  if ((LastPoint is TWLinkPoint) and Assigned(TWLinkPoint(LastPoint).TableB) and Assigned(LastPoint.LineA)) then
    FixPointAlign(TWLinkPoint(LastPoint), TWLinkPoint(LastPoint).TableB);

  for I := 1 to PointCount - 1 do
  begin
    Points[I].LineA.BringToFront();
    Points[I].LineA.Hint := Caption;
  end;
  for I := 0 to PointCount - 1 do
  begin
    Points[I].MouseCapture := False;
    Points[I].BringToFront();
    Points[I].Hint := Caption;
  end;

  if ((LastPoint is TWLinkPoint) and not Assigned(TWLinkPoint(LastPoint).Link)) then
    raise Exception.Create('Unknown Link');
end;

constructor TWLink.Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const PreviousPoint: TWPoint = nil);
begin
  inherited;

  FCaption := '';
end;

destructor TWLink.Destroy();
var
  Point: TWPoint;
begin
  Point := LastPoint;
  while (Assigned(Point) and Assigned(Point.LineA)) do
  begin
    Point := Point.LineA.PointA;
    Point.LineB.Free();
  end;

  inherited;
end;

function TWLink.GetCaption(): TCaption;
begin
  Result := FCaption;
end;

function TWLink.GetLinkSelected(): Boolean;
var
  I: Integer;
begin
  Result := Selected;

  for I := 1 to PointCount - 1 do
    Result := Result and Points[I].LineA.Selected and Points[I].Selected;
end;

function TWLink.GetPoint(Index: Integer): TWLinkPoint;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Index - 1 do
    if (Assigned(Result)) then
      if (not Assigned(Result.LineB) or not (Result.LineB.PointB is TWLinkPoint)) then
        Result := nil
      else
        Result := TWLinkPoint(Result.LineB.PointB);
end;

function TWLink.GetPointCount(): Integer;
var
  Point: TWPoint;
begin
  Result := 1;

  Point := Self;
  while (Assigned(Point) and Assigned(Point.LineB)) do
  begin
    Inc(Result);

    Point := Point.LineB.PointB;
  end;
end;

function TWLink.GetTable(Index: Integer): TWTable;
begin
  case (Index) of
    0: Result := TableA;
    1:
      if (LastPoint is TWLinkPoint) then
        Result := TWLinkPoint(LastPoint).TableB
      else
        Result := nil;
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Index']);
  end;
end;

procedure TWLink.LoadFromXML(const XML: IXMLNode);

  function ConnectorPosition(const Table: TWTable; const Align: TAlign; const Position: Integer): TCoord;
  begin
    Result.X := Table.Area.Left;
    Result.Y := Table.Area.Top - (ConnectorSize + (PointSize - 1) div 2);

    case (Align) of
      alLeft:
        begin
          Result.X := Table.Area.Left - (ConnectorSize + (PointSize + 1) div 2);
          Result.Y := Table.Area.Top + Position;
        end;
      alTop:
        begin
          Result.X := Table.Area.Left + Position;
          Result.Y := Table.Area.Top - (ConnectorSize + (PointSize + 1) div 2);
        end;
      alRight:
        begin
          Result.X := Table.Area.Right + (ConnectorSize + (PointSize - 1) div 2);
          Result.Y := Table.Area.Top + Position;
        end;
      alBottom:
        begin
          Result.X := Table.Area.Left + Position;
          Result.Y := Table.Area.Bottom + (ConnectorSize + (PointSize - 1) div 2);
        end;
    end;
  end;

var
  Align: TAlign;
  Index: Integer;
  J: Integer;
  Point: TWLinkPoint;
  PointPosition: TCoord;
  PointIndex: Integer;
  PointsNode: IXMLNode;
  Position: Integer;
  Table: TWTable;
begin
  Workbench.State := wsLoading;

  if (XML.Attributes['name'] <> Null) then
    Caption := XML.Attributes['name'];

  if (Assigned(XMLNode(XML, 'tables/child')) and (XMLNode(XML, 'tables/child').Attributes['name'] <> Null)
    and Assigned(XMLNode(XML, 'tables/parent')) and (XMLNode(XML, 'tables/parent').Attributes['name'] <> Null)) then
  begin
    if (not (Self is TWForeignKey) or not Assigned(TWForeignKey(Self).BaseForeignKey)) then
    begin
      TableA := Workbench.TableByCaption(XMLNode(XML, 'tables/child').Attributes['name']);
      Table := Workbench.TableByCaption(XMLNode(XML, 'tables/parent').Attributes['name']);
    end
    else
    begin
      TableA := Workbench.TableByCaption(TWForeignKey(Self).BaseForeignKey.Table.Name);
      Table := Workbench.TableByCaption(TWForeignKey(Self).BaseForeignKey.Parent.TableName);
    end;

    if (Assigned(TableA) and Assigned(Table)) then
    begin
      if (Assigned(XMLNode(XML, 'tables/child/align'))
        and TryStrToAlign(XMLNode(XML, 'tables/child/align').Text, Align)
        and Assigned(XMLNode(XML, 'tables/child/position'))
        and TryStrToInt(XMLNode(XML, 'tables/child/position').Text, Position)) then
        MoveTo(Self, [], ConnectorPosition(TableA, Align, Position));

      PointIndex := 0;
      PointsNode := XMLNode(XML, 'points');
      if (Assigned(PointsNode)) then
        repeat
          for J := 0 to PointsNode.ChildNodes.Count - 1 do
            if ((PointsNode.ChildNodes[J].NodeName = 'point') and (TryStrToInt(PointsNode.ChildNodes[J].Attributes['index'], Index) and (Index = PointIndex))
              and Assigned(XMLNode(PointsNode.ChildNodes[J], 'coord/x')) and TryStrToInt(XMLNode(PointsNode.ChildNodes[J], 'coord/x').Text, PointPosition.X)
              and Assigned(XMLNode(PointsNode.ChildNodes[J], 'coord/y')) and TryStrToInt(XMLNode(PointsNode.ChildNodes[J], 'coord/y').Text, PointPosition.Y)) then
            begin
              Point := TWLinkPoint.Create(Workbench, Coord(-1, -1), LastPoint);
              Point.MoveTo(nil, [], PointPosition);
            end;
          Inc(PointIndex);
        until (PointCount < PointIndex);

      Point := TWLinkPoint.Create(Workbench, Coord(-1, -1), LastPoint);
      Point.TableB := Table;
      if (Assigned(XMLNode(XML, 'tables/parent/align'))
        and TryStrToAlign(XMLNode(XML, 'tables/parent/align').Text, Align)
        and Assigned(XMLNode(XML, 'tables/parent/position'))
        and TryStrToInt(XMLNode(XML, 'tables/parent/position').Text, Position)) then
        Point.MoveTo(nil, [], ConnectorPosition(Table, Align, Position));

      Cleanup(Self);
    end;

    if (Assigned(ChildTable) and (Self is TWForeignKey)) then
      TWForeignKey(Self).BaseForeignKey := ChildTable.BaseTable.ForeignKeyByName(Caption);
  end;

  Workbench.State := wsNormal;
end;

procedure TWLink.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
  Index: Integer;
  J: Integer;
  Node: IXMLNode;
  PointsNode: IXMLNode;
begin
  XMLNode(XML, 'tables/child').Attributes['name'] := ChildTable.Caption;
  XMLNode(XML, 'tables/child/align').Text := AlignToStr(InvertAlign(ControlAlign(TableA)));
  case (InvertAlign(ControlAlign(TableA))) of
    alLeft, alRight:
      XMLNode(XML, 'tables/child/position').Text := IntToStr(Position.Y - TableA.Position.Y);
    alTop, alBottom:
      XMLNode(XML, 'tables/child/position').Text := IntToStr(Position.X - TableA.Position.X);
  end;

  XMLNode(XML, 'tables/parent').Attributes['name'] := ParentTable.Caption;
  XMLNode(XML, 'tables/parent/align').Text := AlignToStr(InvertAlign(LastPoint.ControlAlign(TWLinkPoint(LastPoint).TableB)));
  case (InvertAlign(LastPoint.ControlAlign(TWLinkPoint(LastPoint).TableB))) of
    alLeft, alRight:
      XMLNode(XML, 'tables/parent/position').Text := IntToStr(LastPoint.Position.Y - TWLinkPoint(LastPoint).TableB.Position.Y);
    alTop, alBottom:
      XMLNode(XML, 'tables/parent/position').Text := IntToStr(LastPoint.Position.X - TWLinkPoint(LastPoint).TableB.Position.X);
  end;

  PointsNode := XMLNode(XML, 'points');

  for I := PointsNode.ChildNodes.Count - 1 downto 0 do
    if ((PointsNode.ChildNodes[I].NodeName = 'point') and (TryStrToInt(PointsNode.ChildNodes[I].Attributes['index'], Index) and ((Index < 1) or (PointCount - 2 < Index)))) then
      PointsNode.ChildNodes.Delete(I);

  for I := 1 to PointCount - 2 do
  begin
    Node := nil;
    for J := 0 to PointsNode.ChildNodes.Count - 1 do
      if ((PointsNode.ChildNodes[J].NodeName = 'point') and (TryStrToInt(PointsNode.ChildNodes[J].Attributes['index'], Index) and (Index = I - 1))) then
        Node := PointsNode.ChildNodes[J];
    if (not Assigned(Node)) then
    begin
      Node := PointsNode.AddChild('point');
      Node.Attributes['index'] := IntToStr(I - 1);
    end;

    Points[I].SaveToXML(Node);
  end;
end;

procedure TWLink.SetCaption(const ACaption: TCaption);
begin
  FCaption := ACaption;

  Workbench.FModified := True;
end;

procedure TWLink.SetLinkSelected(const ALinkSelected: Boolean);
var
  I: Integer;
begin
  Workbench.BeginUpdate();

  Selected := ALinkSelected;
  for I := 1 to PointCount - 1 do
    Points[I].Selected := ALinkSelected;

  Workbench.EndUpdate();
end;

procedure TWLink.SetTable(Index: Integer; ATable: TWTable);
begin
  Workbench.State := wsAutoCreate;

  case (Index) of
    0:
      begin
        TableA := ATable;
        if (Assigned(ATable)) then
          MoveTo(Self, [], Coord(ATable.Position.X + (ATable.Area.Right - ATable.Area.Left) div 2, ATable.Position.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
      end;
    1:
      begin
        if (Assigned(ATable) and (LastPoint is TWLinkPoint)) then
        begin
          if (ChildTable = ATable) then
          begin
            LastPoint.MoveTo(LastPoint, [], Coord(ATable.Area.Left + (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Position.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(LastPoint, [], Coord(ATable.Area.Left + (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Area.Top - 2 * ConnectorSize));
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(LastPoint, [], Coord(ATable.Area.Right - (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Area.Top - 2 * ConnectorSize));
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(LastPoint, [], Coord(ATable.Area.Right - (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Position.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
          end
          else
          begin
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(Self, [], Coord(ATable.Position.X + (ATable.Area.Right - ATable.Area.Left) div 2, ATable.Position.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
          end;
          TWLinkPoint(LastPoint).TableB := ATable;
        end;
      end;
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Index']);
  end;

  Cleanup(Self);

  Workbench.State := wsNormal;
end;

{ TWForeignKey ****************************************************************}

constructor TWForeignKey.Create(const AWorkbench: TWWorkbench; const APosition: TCoord; const PreviousPoint: TWPoint = nil);
begin
  inherited;

  FBaseForeignKey := nil;
end;

function TWForeignKey.GetCaption(): TCaption;
begin
  if (not Assigned(BaseForeignKey)) then
    inherited
  else
    Result := BaseForeignKey.Name;
end;

procedure TWForeignKey.SetCaption(const ACaption: TCaption);
begin
end;

{ TWLinks *********************************************************************}

function TWLinks.GetLink(Index: Integer): TWLink;
begin
  Result := TWLink(Items[Index]);
end;

function TWLinks.GetSelCount(): Integer;
var
  I: Integer;
  J: Integer;
  Selected: Boolean;
begin
  Result := 0;

  for I := 0 to Count - 1 do
  begin
    Selected := Link[I].Points[0].Selected;
    for J := 1 to Link[I].PointCount - 1 do
      Selected := Selected and Link[I].Points[J].LineA.Selected and Link[I].Points[J].Selected;
    if (Selected) then
      Inc(Result);
  end;
end;

procedure TWLinks.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
  J: Integer;
  Node: IXMLNode;
begin
  for I := XML.ChildNodes.Count - 1 downto 0 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'foreignkey') and not Assigned(Workbench.LinkByCaption(XML.ChildNodes.Nodes[I].Attributes['name'])) then
      XML.ChildNodes.Delete(I);

  for I := 0 to Count - 1 do
  begin
    Node := nil;
    for J := 0 to XML.ChildNodes.Count - 1 do
      if ((XML.ChildNodes.Nodes[J].NodeName = 'foreignkey') and (Workbench.LinkByCaption(XML.ChildNodes.Nodes[J].Attributes['name']) = Link[I])) then
        Node := XML.ChildNodes.Nodes[J];
    if (not Assigned(Node)) then
    begin
      Node := XML.AddChild('foreignkey');
      if (Link[I] is TWForeignKey) then
        Node.Attributes['name'] := Link[I].Caption;
    end;

    Link[I].SaveToXML(Node);
  end;
end;

{ TWSection *******************************************************************}

constructor TWSection.Create(const AWorkbench: TWWorkbench; const APosition: TCoord);
begin
  inherited;

  FColor := clGreen;

  Canvas.Font := Workbench.Font;
  Canvas.Font.Name := Workbench.Font.Name;
  Canvas.Font.Style := [];

  MoveTo(Self, [], APosition);
end;

function TWSection.GetCaption(): TCaption;
begin
  Result := inherited Caption;
end;

procedure TWSection.LoadFromXML(const XML: IXMLNode);
begin
  if (Assigned(XMLNode(XML, 'size/x'))) then TryStrToInt(XMLNode(XML, 'size/x').Text, FSize.cx);
  if (Assigned(XMLNode(XML, 'size/y'))) then TryStrToInt(XMLNode(XML, 'size/y').Text, FSize.cy);
  if (Assigned(XMLNode(XML, 'caption'))) then Caption := XMLNode(XML, 'caption').Text;
  if (Assigned(XMLNode(XML, 'color'))) then FColor := StringToColor(XMLNode(XML, 'color').Text);

  inherited;

  BringToFront();
end;

procedure TWSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ResizeMode = rmNone) then
    if ((X <= PointSize) and (Y <= PointSize)) then
      FResizeMode := rmNW
    else if ((ClientWidth - PointSize - 1 <= X) and (Y <= PointSize)) then
      FResizeMode := rmNE
    else if ((ClientWidth - PointSize - 1 <= X) and (ClientHeight - PointSize - 1 <= Y)) then
      FResizeMode := rmSE
    else if ((X <= PointSize) and (ClientHeight - PointSize - 1 <= Y)) then
      FResizeMode := rmSW
    else if (Y <= BorderSize) then
      FResizeMode := rmN
    else if (ClientHeight - BorderSize - 1 <= Y) then
      FResizeMode := rmS
    else if (ClientWidth - BorderSize - 1 <= X) then
      FResizeMode := rmE
    else if (X <= BorderSize) then
      FResizeMode := rmW
    else
      FResizeMode := rmNone;


  if ((Shift = [ssLeft]) and (ResizeMode = rmNone) and not Selected) then
    Workbench.MouseDown(Button, Shift, Left + X, Top + Y)
  else
  begin
    if (Assigned(Workbench.OnMouseDown)) then
      Workbench.OnMouseDown(Workbench, Button, Shift, Left + X, Top + Y);
    inherited;
  end;
end;

procedure TWSection.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ResizeMode = rmNone) then
  begin
    if ((X <= PointSize) and (Y <= PointSize) or (ClientWidth - PointSize - 1 <= X) and (ClientHeight - PointSize - 1 <= Y)) then
      Cursor := crSizeNWSE
    else if ((ClientWidth - PointSize - 1 <= X) and (Y <= PointSize) or (X <= PointSize) and (ClientHeight - PointSize - 1 <= Y)) then
      Cursor := crSizeNESW
    else if ((X <= BorderSize) or (ClientWidth - BorderSize - 1 <= X)) then
      Cursor := crSizeWE
    else if ((Y <= BorderSize) or (ClientHeight - BorderSize - 1 <= Y)) then
      Cursor := crSizeNS
    else
      Cursor := crDefault;
  end
  else if (ResizeMode = rmCreate) then
    Shift := Shift + [ssLeft];

  if ((Shift = [ssLeft]) and (ResizeMode = rmNone) and not Selected) then
    Workbench.MouseMove(Shift, Left + X, Top + Y)
  else
    inherited;
end;

procedure TWSection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ResizeMode = rmCreate) then
    Selected := False;

  inherited;
end;

procedure TWSection.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
var
  Bitmap: Graphics.TBitmap;
  BlendFunction: Windows.BLENDFUNCTION;
  Flags: Longint;
  Rect: TRect;
begin
  Canvas.Pen.Style := psDot;
  Canvas.Brush.Style := bsClear;

  if (Selected and (ResizeMode = rmNone)) then
    Canvas.Pen.Color := clHighlight
  else
    Canvas.Pen.Color := FColor;

  if (Workbench.DoubleBuffered) then
  begin
    Bitmap := Graphics.TBitmap.Create();
    Bitmap.Handle := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
    Bitmap.Canvas.Brush.Color := Canvas.Pen.Color;
    Bitmap.Canvas.FillRect(ClientRect);

    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := $20;
    BlendFunction.AlphaFormat := 0;

    AlphaBlend(Canvas.Handle, X, Y, ClientWidth, ClientHeight,
               Bitmap.Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
               BlendFunction);

    Bitmap.Free();
  end;

  Rect := ClientRect;
  OffsetRect(Rect, X, Y);

  Canvas.Rectangle(Rect);

  Inc(Rect.Left, BorderSize + Padding); Dec(Rect.Right, BorderSize + 1 + Padding);
  Rect.Top := Rect.Bottom + Canvas.Font.Height - BorderSize - 1 - Padding;

  Flags := DrawTextBiDiModeFlags(DT_RIGHT);
  Canvas.Font.Color := Canvas.Pen.Color;
  DrawText(Canvas.Handle, PChar(Caption), -1, Rect, Flags);
end;

procedure TWSection.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'size/x').Text := IntToStr(Size.cx);
  XMLNode(XML, 'size/y').Text := IntToStr(Size.cy);
  XMLNode(XML, 'caption').Text := Caption;
  XMLNode(XML, 'color').Text := ColorToString(Color);
end;

procedure TWSection.SetSelected(ASelected: Boolean);
begin
  if (ResizeMode <> rmCreate) then
    inherited;
end;

procedure TWSection.SetCaption(const ACaption: TCaption);
begin
  inherited Caption := ACaption;

  Workbench.UpdateControl(Self);

  if (Workbench.State <> wsLoading) then
    Workbench.FModified := True;
end;

procedure TWSection.SetZOrder(TopMost: Boolean);
var
  I: Integer;
  Order: Integer;
begin
  if (not TopMost) then
    inherited
  else
  begin
    Order := 0;
    for I := 0 to Workbench.ControlCount - 1 do
      if ((Workbench.Controls[I] <> Self) and (Workbench.Controls[I] is TWSection)) then
        Order := I + 1;
    Workbench.SetChildOrder(Self, Order);
  end;
end;

{ TWSections ******************************************************************}

function TWSections.GetSection(Index: Integer): TWSection;
begin
  Result := TWSection(Items[Index]);
end;

procedure TWSections.LoadFromXML(const XML: IXMLNode);
var
  I: Integer;
  Section: TWSection;
begin
  Workbench.State := wsLoading;
  for I := 0 to XML.ChildNodes.Count - 1 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'section') then
    begin
      Section := TWSection.Create(Workbench, Coord(-1, -1));
      Section.LoadFromXML(XML.ChildNodes.Nodes[I]);
      Add(Section);
    end;
  Workbench.State := wsNormal;
end;

procedure TWSections.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
begin
  for I := XML.ChildNodes.Count - 1 downto 0 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'section') then
      XML.ChildNodes.Delete(I);

  for I := 0 to Count - 1 do
    Section[I].SaveToXML(XML.AddChild('section'));
end;

{ TWLasso *********************************************************************}

constructor TWLasso.Create(const AWorkbench: TWWorkbench; const APosition: TCoord);
begin
  inherited;

  Canvas.Brush.Style := bsClear;

  MoveTo(Self, [], APosition);

  MouseDown(mbLeft, [], 0, 0);
end;

procedure TWLasso.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  LassoRect: TRect;
  R: TRect;
begin
  Workbench.BeginUpdate();

  inherited;

  LassoRect := BoundsRect;
  OffsetRect(LassoRect, Workbench.HorzScrollBar.Position, Workbench.VertScrollBar.Position);

  for I := 0 to Workbench.ControlCount - 1 do
    if (Workbench.Controls[I] is TWSection) then
      TWSection(Workbench.Controls[I]).Selected := PtInRect(LassoRect, TWSection(Workbench.Controls[I]).Area.TopLeft) and PtInRect(LassoRect, TWSection(Workbench.Controls[I]).Area.BottomRight)
    else if (Workbench.Controls[I] is TWArea) then
      TWArea(Workbench.Controls[I]).Selected := IntersectRect(R, LassoRect, TWArea(Workbench.Controls[I]).Area)
    else if (Workbench.Controls[I] is TWPoint) then
      TWPoint(Workbench.Controls[I]).Selected := CoordInArea(TWPoint(Workbench.Controls[I]).Position, LassoRect);

  BringToFront();

  Workbench.EndUpdate();
end;

procedure TWLasso.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PostMessage(Workbench.Handle, CM_ENDLASSO, 0, 0);
end;

procedure TWLasso.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
var
  Bitmap: Graphics.TBitmap;
  Bitmap2: Graphics.TBitmap;
  BlendFunction: Windows.BLENDFUNCTION;
begin
  if (not Workbench.DoubleBuffered) then
  begin
    Canvas.Pen.Color := clHighlight;
    Canvas.Pen.Style := psDot;
  end
  else
  begin
    Canvas.Pen.Color := clHighlight;
    Canvas.Pen.Style := psSolid;

    Bitmap := Graphics.TBitmap.Create();
    Bitmap.Handle := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
    Bitmap.Canvas.Brush.Color := clWindow;
    Bitmap.Canvas.FillRect(ClientRect);

    Bitmap2 := Graphics.TBitmap.Create();
    Bitmap2.Handle := CreateCompatibleBitmap(Canvas.Handle, ClientWidth, ClientHeight);
    Bitmap2.Canvas.Brush.Color := clHighlight;
    Bitmap2.Canvas.FillRect(ClientRect);

    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := $80;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    if (AlphaBlend(Bitmap2.Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
                   Bitmap.Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
                   BlendFunction)) then
    begin
      BlendFunction.BlendOp := AC_SRC_OVER;
      BlendFunction.BlendFlags := 0;
      BlendFunction.SourceConstantAlpha := $40;
      BlendFunction.AlphaFormat := 0;

      AlphaBlend(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
                 Bitmap2.Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
                 BlendFunction);
    end;

    Bitmap.Free();
    Bitmap2.Free();
  end;

  Canvas.Rectangle(ClientRect);
end;

procedure TWLasso.SetSelected(ASelected: Boolean);
begin
end;

{ TWWorkbench *****************************************************************}

procedure TWWorkbench.AddExistingTable(const X, Y: Integer; const ABaseTable: TCBaseTable);
var
  Table: TWTable;
begin
  Table := TWTable.Create(Tables, Position(X, Y), ABaseTable);
  Tables.Add(Table);
  if (Assigned(OnValidateControl)) then
  begin
    if (not OnValidateControl(Self, Table)) then
      Table.Selected := True
    else
      Selected := Table;
    FModified := True;
  end;
end;

procedure TWWorkbench.BeginUpdate();
begin
  Inc(UpdateCount);
end;

procedure TWWorkbench.CalcRange(const Reset: Boolean);
var
  I: Integer;
  NewRange: Integer;
begin
  if (Reset) then
    NewRange := 0
  else
    NewRange := HorzScrollBar.Range;
  for I := 0 to ControlCount - 1 do
    NewRange := Max(NewRange, HorzScrollBar.Position + Controls[I].Left + Controls[I].Width);
  HorzScrollBar.Range := Max(NewRange, HorzScrollBar.Range);

  if (Reset) then
    NewRange := 0
  else
    NewRange := VertScrollBar.Range;
  for I := 0 to ControlCount - 1 do
    NewRange := Max(NewRange, VertScrollBar.Position + Controls[I].Top + Controls[I].Height);
  VertScrollBar.Range := Max(NewRange, VertScrollBar.Range);
end;

procedure TWWorkbench.Change();
begin
  if Assigned(FOnChange) then FOnChange(Self, Selected);
end;

procedure TWWorkbench.Clear();
begin
  BeginUpdate();

  Links.Clear();
  Tables.Clear();
  Sections.Clear();

  EndUpdate();
end;

procedure TWWorkbench.ClientUpdate(const Event: TCClient.TEvent);
var
  BaseTable: TCBaseTable;
  ChildTable: TWTable;
  I: Integer;
  J: Integer;
  Link: TWLink;
  OldModified: Boolean;
  ParentTable: TWTable;
  S: string;
  Table: TWTable;
begin
  if ((Event.EventType = ceItemsValid) and (Event.Sender = Database) and (Event.CItems is TCTables)) then
  begin
    for I := Tables.Count - 1 downto 0 do
      if (Database.Tables.IndexOf(Tables[I].BaseTable) < 0) then
        Tables.Delete(I);
  end
  else if ((Event.EventType = ceItemValid) and (Event.Sender = Database) and (Event.CItem is TCBaseTable)) then
  begin
    BaseTable := TCBaseTable(Event.CItem);

    for I := Links.Count - 1 downto 0 do
      if ((Links[I].ChildTable.BaseTable = BaseTable)
        and not Assigned(Links[I].ChildTable.BaseTable.ForeignKeyByName(Links[I].Caption))) then
          Links.Delete(I);

    if (Assigned(CreatedTable)) then
    begin
      CreatedTable.FBaseTable := TCBaseTable(Event.CItem);
      Tables.Add(CreatedTable);
      Selected := CreatedTable;

      CreatedTable := nil;
      OldModified := True;
    end
    else if (Assigned(CreatedLink) and (CreatedLink is TWForeignKey)) then
    begin
      for J := 0 to BaseTable.ForeignKeys.Count - 1 do
        if (not Assigned(LinkByCaption(BaseTable.ForeignKeys[J].Name))) then
          TWForeignKey(CreatedLink).BaseForeignKey := BaseTable.ForeignKeys[J];
      if (not Assigned(TWForeignKey(CreatedLink).BaseForeignKey)) then
        FreeAndNil(CreatedLink)
      else
      begin
        Links.Add(CreatedLink);
        Selected := CreatedLink;

        CreatedLink := nil;
        OldModified := True;
      end;
    end
    else if (Assigned(TableByBaseTable(BaseTable))) then
    begin
      Table := TableByBaseTable(BaseTable);
      if (Assigned(Table)) then
      begin
        Table.Invalidate();
        if (not Assigned(Selected) and Table.Selected) then
          Selected := Table;
      end;
    end
    else if (Assigned(XML)) then
    begin
      OldModified := FModified;

      for I := 0 to XML.ChildNodes.Count - 1 do
        if ((XML.ChildNodes.Nodes[I].NodeName = 'table') and (Database.Tables.NameCmp(XML.ChildNodes.Nodes[I].Attributes['name'], BaseTable.Name) = 0)) then
        begin
          Table := TWTable.Create(Tables, Coord(-1, -1), BaseTable);
          Table.LoadFromXML(XML.ChildNodes.Nodes[I]);
          Tables.Add(Table);

          for J := 0 to XML.ChildNodes.Count - 1 do
            if ((XML.ChildNodes.Nodes[J].NodeName = 'foreignkey')
              and Assigned(XMLNode(XML.ChildNodes.Nodes[J], 'tables/child')) and (XMLNode(XML.ChildNodes.Nodes[J], 'tables/child').Attributes['name'] <> Null)
              and Assigned(XMLNode(XML.ChildNodes.Nodes[J], 'tables/parent')) and (XMLNode(XML.ChildNodes.Nodes[J], 'tables/parent').Attributes['name'] <> Null)) then
            begin
              ChildTable := TableByCaption(XMLNode(XML.ChildNodes.Nodes[J], 'tables/child').Attributes['name']);
              ParentTable := TableByCaption(XMLNode(XML.ChildNodes.Nodes[J], 'tables/parent').Attributes['name']);
              if (((Table = ChildTable) or (Table = ParentTable))
                and Assigned(ChildTable) and Assigned(ChildTable.BaseTable)
                and Assigned(ParentTable) and Assigned(ParentTable.BaseTable)) then
              begin
                if (XML.ChildNodes.Nodes[J].Attributes['name'] = Null) then
                  Link := TWLink.Create(Self, Coord(-1, -1))
                else if (not Assigned(LinkByCaption(XML.ChildNodes.Nodes[J].Attributes['name']))
                  and Assigned(ChildTable.BaseTable.ForeignKeyByName(XML.ChildNodes.Nodes[J].Attributes['name']))) then
                begin
                  S := XML.ChildNodes.Nodes[J].Attributes['name'];
                  Link := TWForeignKey.Create(Self, Coord(-1, -1));
                  TWForeignKey(Link).BaseForeignKey := ChildTable.BaseTable.ForeignKeyByName(XML.ChildNodes.Nodes[J].Attributes['name']);
                end
                else
                  Link := nil;
                if (Assigned(Link)) then
                begin
                  Link.LoadFromXML(XML.ChildNodes.Nodes[J]);
                  Links.Add(Link);
                end;
              end;
            end;
        end;

      FModified := OldModified;
    end;

    for J := 0 to BaseTable.ForeignKeys.Count - 1 do
      if (not Assigned(LinkByCaption(BaseTable.ForeignKeys[J].Name))
        and Assigned(TableByCaption(BaseTable.ForeignKeys[J].Parent.TableName))) then
        begin
          Link := TWForeignKey.Create(Self, Coord(-1, -1));
          TWForeignKey(Link).BaseForeignKey := BaseTable.ForeignKeys[J];
          Link.ChildTable := TableByBaseTable(BaseTable);
          Link.ParentTable := TableByCaption(BaseTable.ForeignKeys[J].Parent.TableName);
          Links.Add(Link);
        end;

    for I := 0 to Tables.Count - 1 do
      for J := 0 to Tables[I].BaseTable.ForeignKeys.Count - 1 do
        if ((Database.Tables.NameCmp(Tables[I].BaseTable.ForeignKeys[J].Parent.TableName, BaseTable.Name) = 0)
          and not Assigned(LinkByCaption(Tables[I].BaseTable.ForeignKeys[J].Name))) then
        begin
          Link := TWForeignKey.Create(Self, Coord(-1, -1));
          TWForeignKey(Link).BaseForeignKey := Tables[I].BaseTable.ForeignKeys[J];
          Link.ChildTable := Tables[I];
          Link.ParentTable := TableByBaseTable(BaseTable);
          Links.Add(Link);
        end;
  end
  else if ((Event.EventType = ceItemDropped) and (Event.Sender = Database) and (Event.CItem is TCBaseTable)) then
  begin
    for I := Tables.Count - 1 downto 0 do
      if (Tables[I].BaseTable = Event.CItem) then
        Tables.Delete(I);
  end;
end;

procedure TWWorkbench.CMEndLasso(var Message: TMessage);
begin
  FreeAndNil(Lasso);
end;

constructor TWWorkbench.Create(AOwner: TComponent);
begin
  inherited;

  AutoScroll := False;
  CreatedLink := nil;
  CreatedTable := nil;
  FDatabase := nil;
  FOnChange := nil;
  FOnCursorMove := nil;
  FOnValidateControl := nil;
  FLinks := TWLinks.Create(Self);
  FHideSelection := False;
  FModified := False;
  FMultiSelect := False;
  FSections := TWSections.Create(Self);
  FTables := TWTables.Create(Self);
  Lasso := nil;
  LastScrollTickCount := 0;
  State := wsNormal;
  PendingUpdateControls := TList.Create();
  UpdateCount := 0;
  XML := nil;
  XMLDocument := nil;

  ShowHint := True;

  CalcRange(False);
end;

constructor TWWorkbench.Create(const AOwner: TComponent; const ADatabase: TCDatabase);
begin
  Create(AOwner);

  FDatabase := ADatabase;
end;

procedure TWWorkbench.CreateNewForeignKey(const X, Y: Integer);
var
  Table: TWTable;
begin
  Selected := nil;
  State := wsCreateForeignKey;

  Table := TableAt(Position(X, Y));
  if (Assigned(Table)) then
    Table.MouseDown(mbLeft, [], X - Table.Left, Y - Table.Top);
end;

procedure TWWorkbench.CreateNewLink(const X, Y: Integer);
var
  Table: TWTable;
begin
  Selected := nil;
  State := wsCreateLink;

  Table := TableAt(Position(X, Y));
  if (Assigned(Table)) then
    Table.MouseDown(mbLeft, [], X - Table.Left, Y - Table.Top);
end;

procedure TWWorkbench.CreateNewSection(const X, Y: Integer);
var
  Section: TWSection;
begin
  Selected := nil;

  if ((X >= 0) or (Y >= 0)) then
  begin
    Section := TWSection.Create(Self, Position(X, Y));
    Sections.Add(Section);
    Section.MouseDown(mbLeft, [], 0, 0);
  end
  else
    State := wsCreateSection;
end;

procedure TWWorkbench.CreateNewTable(const X, Y: Integer);
begin
  CreatedTable := TWTable.Create(Tables, Position(X, Y));
  if (not Assigned(OnValidateControl) or not OnValidateControl(Self, CreatedTable)) then
    FreeAndNil(CreatedTable)
  else
    FModified := True;
end;

procedure TWWorkbench.CursorMove(const X, Y: Integer);
begin
  if Assigned(FOnCursorMove) then FOnCursorMove(Self, X, Y);
end;

destructor TWWorkbench.Destroy();
begin
  Clear();

  Links.Free();
  Tables.Free();
  Sections.Free();

  PendingUpdateControls.Free();

  inherited;
end;

procedure TWWorkbench.DoEnter();
begin
  if (Assigned(Selected)) then
    Selected.Invalidate();

  inherited;
end;

procedure TWWorkbench.DoExit();
begin
  inherited;

  if (Assigned(Selected)) then
    Selected.Invalidate();
end;

procedure TWWorkbench.EndUpdate();
var
  I: Integer;
begin
  if (UpdateCount > 0) then
    Dec(UpdateCount);

  if (UpdateCount = 0) then
  begin
    for I := 0 to PendingUpdateControls.Count - 1 do
      UpdateControl(TWControl(PendingUpdateControls[I]));
    PendingUpdateControls.Clear();
  end;
end;

function TWWorkbench.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditDelete) then
  begin
    Result := Assigned(Selected);
    if (Result and (Selected is TWTable)) then
      Tables.Delete(Tables.IndexOf(Selected))
    else if (Result and (Selected is TWLink)) then
      Links.Delete(Links.IndexOf(Selected))
    else if (Result and (Selected is TWSection)) then
      Sections.Delete(Sections.IndexOf(Selected));
  end
  else
    Result := inherited ExecuteAction(Action);
end;

function TWWorkbench.ForeignKeyByBaseForeignKey(const BaseForeignKey: TCForeignKey): TWForeignKey;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Links.Count - 1 do
    if ((Links[I] is TWForeignKey) and (TWForeignKey(Links[I]).BaseForeignKey = BaseForeignKey)) then
      Result := TWForeignKey(Links[I]);
end;

function TWWorkbench.GetObjectCount(): Integer;
begin
  Result := Tables.Count + Links.Count + Sections.Count;
end;

function TWWorkbench.GetSelCount(): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to ControlCount - 1 do
    if ((Controls[I] is TWControl) and TWControl(Controls[I]).Selected) then
      Inc(Result);
end;

procedure TWWorkbench.KeyPress(var Key: Char);
begin
  if ((Key = Chr(VK_ESCAPE)) and Assigned(Lasso)) then
    Perform(CM_ENDLASSO, 0, 0)
  else if ((Key = Chr(VK_ESCAPE)) and (Selected is TWLinkPoint) and Assigned(CreatedLink)) then
    FreeAndNil(CreatedLink)
  else if ((Key = Chr(VK_ESCAPE)) and (Selected is TWSection) and (TWSection(Selected).ResizeMode = rmCreate)) then
    Sections.Delete(Sections.IndexOf(Selected))
  else
    inherited;
end;

function TWWorkbench.LinkByCaption(const Caption: string): TWLink;
var
  I: Integer;
begin
  Result := nil;

  if (Caption <> '') then
    for I := 0 to Links.Count - 1 do
      if (lstrcmpI(PChar(Links[I].Caption), PChar(Caption)) = 0) then
        Result := Links[I];
end;

procedure TWWorkbench.LoadFromFile(const FileName: string);
var
  BaseTable: TCBaseTable;
  I: Integer;
  List: TList;
begin
  XMLDocument := LoadXMLDocument(FileName);
  XML := XMLDocument.DocumentElement;

  Clear();

  Sections.LoadFromXML(XML);

  List := TList.Create();
  for I := 0 to XML.ChildNodes.Count - 1 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'table') then
    begin
      BaseTable := Database.BaseTableByName(XML.ChildNodes.Nodes[I].Attributes['name']);
      if (Assigned(BaseTable)) then
        if (BaseTable.Valid) then
          BaseTable.PushBuildEvent()
        else
          List.Add(BaseTable);
    end;
  Database.Client.Update(List);
  List.Free();

  FModified := False;
end;

procedure TWWorkbench.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Section: TWSection;
begin
  inherited;

  SetFocus();

  if (State = wsCreateSection) then
  begin
    Section := TWSection.Create(Self, Position(X, Y));
    Sections.Add(Section);
    Section.MouseDown(mbLeft, [], 0, 0);

    State := wsNormal;
  end
  else
  begin
    if ((Button in [mbLeft, mbRight]) and not (MultiSelect and (ssCtrl in Shift))) then
      Selected := nil;
    TableFocused := nil;

    if ((Button = mbLeft) and not (MultiSelect and (ssCtrl in Shift))) then
      Lasso := TWLasso.Create(Self, Position(X, Y));
  end;
end;

procedure TWWorkbench.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (State in [wsCreateLink, wsCreateForeignKey]) then
    Cursor := crNo
  else if (State = wsCreateSection) then
    Cursor := crCross
  else
    Cursor := crDefault;

  inherited;

  CursorMove(HorzScrollBar.Position + X, VertScrollBar.Position + Y);
end;

function TWWorkbench.Position(const X, Y: Integer): TCoord;
begin
  Result := Coord(HorzScrollBar.Position + X, VertScrollBar.Position + Y);
end;

procedure TWWorkbench.Print(const Title: string);
var
  Bitmap: Graphics.TBitmap;
  I: Integer;
  Ratio: TPoint;
begin
  if (Tables.Count > 0) then
  begin
    Selected := nil;

    Bitmap := Graphics.TBitmap.Create();
    Bitmap.Width := HorzScrollBar.Range;
    Bitmap.Height := VertScrollBar.Range;

    for I := 0 to ControlCount - 1 do
      if (Controls[I].Visible and (Controls[I] is TWControl)) then
        TWControl(Controls[I]).PaintTo(Bitmap.Canvas, Controls[I].Left, Controls[I].Top);

    Printer().Title := Title;
    Printer().Fonts.Text;
    Printer().BeginDoc();

    Ratio.X := GetDeviceCaps(Printer().Canvas.Handle, LOGPIXELSX) div GetDeviceCaps(Tables[0].Canvas.Handle, LOGPIXELSX);
    Ratio.Y := GetDeviceCaps(Printer().Canvas.Handle, LOGPIXELSY) div GetDeviceCaps(Tables[0].Canvas.Handle, LOGPIXELSY);

    StretchBlt(
      Printer().Canvas.Handle, 0, 0, Ratio.X * Bitmap.Width, Ratio.Y * Bitmap.Height,
      Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, SRCCOPY);

    Bitmap.Free();

    Printer().EndDoc();
  end;
end;

procedure TWWorkbench.ReleaseControl(const Control: TWControl);
var
  Index: Integer;
begin
  if (FSelected = Control) then
    FSelected := nil;

  Index := PendingUpdateControls.IndexOf(Control);
  if (Index >= 0) then
    PendingUpdateControls.Delete(Index);

  FModified := True;
end;

procedure TWWorkbench.SaveToBMP(const FileName: string);
var
  Bitmap: Graphics.TBitmap;
  I: Integer;
begin
  Selected := nil;

  CalcRange(True);

  if (Tables.Count > 0) then
  begin
    Bitmap := Graphics.TBitmap.Create();
    Bitmap.Width := HorzScrollBar.Range;
    Bitmap.Height := VertScrollBar.Range;

    for I := 0 to ControlCount - 1 do
      if (Controls[I].Visible and (Controls[I] is TWControl)) then
        TWControl(Controls[I]).PaintTo(Bitmap.Canvas, Controls[I].Left, Controls[I].Top);

    Bitmap.SaveToFile(FileName);
    Bitmap.Free();
  end;
end;

procedure TWWorkbench.SaveToFile(const FileName: string);
var
  XMLDocument: IXMLDocument;
begin
  if (FileExists(FileName)) then
    XMLDocument := LoadXMLDocument(FileName)
  else
  begin
    XMLDocument := NewXMLDocument();
    XMLDocument.Encoding := 'utf-8';
    XMLDocument.Node.AddChild('workbench').Attributes['version'] := '1.0.0';
  end;

  XMLDocument.Options := XMLDocument.Options - [doAttrNull];
  XMLDocument.Options := XMLDocument.Options + [doNodeAutoCreate];

  Tables.SaveToXML(XMLDocument.DocumentElement);
  Links.SaveToXML(XMLDocument.DocumentElement);
  Sections.SaveToXML(XMLDocument.DocumentElement);

  if ((ExtractFilePath(FileName) = '') or ForceDirectories(ExtractFilePath(FileName))) then
    XMLDocument.SaveToFile(FileName);

  FModified := False;
end;

procedure TWWorkbench.SetMultiSelect(AMultiSelect: Boolean);
var
  I: Integer;
begin
  FMultiSelect := AMultiSelect;

  if (not FMultiSelect) then
    for I := 0 to Tables.Count - 1 do
      if (Tables[I] <> Selected) then
        Tables[I].Selected := False;
end;

procedure TWWorkbench.SetSelected(ASelected: TWControl);
var
  I: Integer;
begin
  Change();

  BeginUpdate();

  if (ASelected is TWLinkPoint) then
    FSelected := TWLinkPoint(ASelected).Link
  else if ((ASelected is TWLinkLine) and (TWLinkLine(ASelected).PointA is TWLinkPoint)) then
    FSelected := TWLinkPoint(TWLinkLine(ASelected).PointA).Link
  else if (not (ASelected is TWLasso)) then
    FSelected := ASelected;

  for I := 0 to ControlCount - 1 do
    if ((Controls[I] <> ASelected) and (Controls[I] is TWControl)) then
      TWControl(Controls[I]).Selected := False;

  if (Assigned(Selected)) then
  begin
    Selected.Selected := True;
    if (Selected is TWTable) then
      TWTable(Selected).Focused := True;
  end;

  EndUpdate();

  Change();
end;

procedure TWWorkbench.SetTableFocused(ATableFocused: TWTable);
var
  I: Integer;
begin
  if (ATableFocused <> FTableFocused) then
  begin
    FTableFocused := ATableFocused;

    for I := 0 to Tables.Count - 1 do
      Tables[I].Focused := False;

    if (FTableFocused is TWTable) then
      TWTable(FTableFocused).Focused := True;
  end;
end;

function TWWorkbench.TableAt(const Position: TCoord): TWTable;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if ((Controls[I] is TWTable) and CoordInArea(Position, TWTable(Controls[I]).Area)) then
      Result := TWTable(Controls[I]);
end;

function TWWorkbench.TableByBaseTable(const ATable: TCBaseTable): TWTable;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Tables.Count - 1 do
    if (Tables[I].BaseTable = ATable) then
      Result := Tables[I];
end;

function TWWorkbench.TableByCaption(const Caption: string): TWTable;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Tables.Count - 1 do
    if (Database.Tables.NameCmp(Tables[I].Caption, Caption) = 0) then
      Result := Tables[I];
end;

function TWWorkbench.UpdateAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditAction) then
  begin
    Result := Focused();
    if (Result) then
      if (Action is TEditCut) then
        TEditCut(Action).Enabled := False
      else if (Action is TEditDelete) then
        TEditDelete(Action).Enabled := (Selected is TWTable) or (Selected is TWLink) or (Selected is TWSection)
      else if (Action is TEditSelectAll) then
        TEditSelectAll(Action).Enabled := False
      else
        Result := False;
  end
  else
    Result := inherited UpdateAction(Action);
end;

procedure TWWorkbench.UpdateControl(const Control: TWControl);
var
  Index: Integer;
begin
  if (UpdateCount = 0) then
    Control.ApplyPosition()
  else
  begin
    Index := PendingUpdateControls.IndexOf(Control);

    if (Index < 0) then
      PendingUpdateControls.Add(Control)
    else
      PendingUpdateControls.Move(Index, PendingUpdateControls.Count - 1);
  end;
end;

end.

