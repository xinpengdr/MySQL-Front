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
  TWForeignKeyPoint = class;
  TWForeignKey = class;
  TWForeignKeys = class;
  TWSection = class;
  TWWorkbench = class;

  TWObjects = class(TList)
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
    FWorkbench: TWWorkbench;
    MouseMoveAlign: TAlign;
    MouseDownCoord: TPoint;
    MouseDownPoint: TPoint;
    procedure CMCursorChanged(var Message: TMessage); message CM_CURSORCHANGED;
  protected
    FCoord: TPoint;
    FSelected: Boolean;
    procedure ApplyCoord(); virtual; abstract;
    procedure DblClick(); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure LoadFromXML(const XML: IXMLNode); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MoveTo(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint); virtual;
    procedure Moving(const Sender: TWControl; const Shift: TShiftState; var NewCoord: TPoint); virtual;
    procedure Paint(); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); virtual; abstract;
    procedure SaveToXML(const XML: IXMLNode); virtual;
    procedure SetSelected(ASelected: Boolean); virtual;
  public
    constructor Create(const AWorkbench: TWWorkbench); reintroduce; virtual;
    destructor Destroy(); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure Move(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint); virtual;
    property Coord: TPoint read FCoord;
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
    procedure ApplyCoord(); override;
    procedure ChangeSize(const Sender: TWControl; const Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property MouseDownSize: TSize read FMouseDownSize;
    property ResizeMode: TWAreaResizeMode read FResizeMode;
  public
    constructor Create(const AWorkbench: TWWorkbench); override;
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
    procedure ApplyCoord(); override;
    function ControlAlign(const Control: TWControl): TAlign; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MoveTo(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure SetSelected(ASelected: Boolean); override;
    property LastPoint: TWPoint read GetLastPoint;
    property LineA: TWLine read GetLineA write SetLineA;
    property LineB: TWLine read GetLineB write SetLineB;
  public
    constructor Create(const AWorkbench: TWWorkbench; const ACoord: TPoint; const PreviousPoint: TWPoint = nil); reintroduce; virtual;
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
    procedure ApplyCoord(); override;
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
    FBaseTable: TCBaseTable;
    FData: TCustomData;
    FFocused: Boolean;
    FForeignKeyPoints: array of TWForeignKeyPoint;
    function GetCaption(): TCaption;
    function GetForeignKeyPoint(AIndex: Integer): TWForeignKeyPoint;
    function GetForeignKeyPointCount(): Integer;
    function GetIndex(): Integer;
    procedure SetFocused(AFocused: Boolean);
  protected
    procedure ApplyCoord(); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure LoadFromXML(const XML: IXMLNode); override;
    procedure MoveTo(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint); override;
    procedure Moving(const Sender: TWControl; const Shift: TShiftState; var NewCoord: TPoint); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    procedure RegisterForeignKeyPoint(const AForeignKeyPoint: TWForeignKeyPoint); virtual;
    procedure ReleaseForeignKeyPoint(const AForeignKeyPoint: TWForeignKeyPoint); virtual;
    property ForeignKeyPoint[Index: Integer]: TWForeignKeyPoint read GetForeignKeyPoint;
    property ForeignKeyPointCount: Integer read GetForeignKeyPointCount;
  public
    constructor Create(const ATables: TWTables; const ABaseTable: TCBaseTable); reintroduce; virtual;
    destructor Destroy(); override;
    procedure Invalidate(); override;
    property BaseTable: TCBaseTable read FBaseTable;
    property Caption: TCaption read GetCaption;
    property Data: TCustomData read FData write FData;
    property Focused: Boolean read FFocused write SetFocused;
    property Index: Integer read GetIndex;
  end;

  TWTables = class(TWObjects)
  private
    function GetSelCount(): Integer;
    function GetTable(Index: Integer): TWTable;
  protected
    procedure SaveToXML(const XML: IXMLNode); virtual;
  public
    property SelCount: Integer read GetSelCount;
    property Table[Index: Integer]: TWTable read GetTable; default;
  end;

  TWForeignKeyPoint = class(TWPoint)
  private
    function GetIndex(): Integer;
    function GetForeignKey(): TWForeignKey;
    function GetTableA(): TWTable;
    function GetTableB(): TWTable;
    procedure SetTableA(ATableA: TWTable);
    procedure SetTableB(ATableB: TWTable);
  protected
    procedure ApplyCoord(); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Moving(const Sender: TWControl; const Shift: TShiftState; var NewCoord: TPoint); override;
    procedure PaintTo(const Canvas: TCanvas; const X, Y: Integer); override;
    property Index: Integer read GetIndex;
    property TableA: TWTable read GetTableA write SetTableA;
    property TableB: TWTable read GetTableB write SetTableB;
  public
    constructor Create(const AWorkbench: TWWorkbench; const ACoord: TPoint; const PreviousPoint: TWPoint = nil); reintroduce; virtual;
    destructor Destroy(); override;
    property ForeignKey: TWForeignKey read GetForeignKey;
  end;

  TWForeignKeyLine = class(TWLine)
  private
    function GetForeignKey(): TWForeignKey;
  protected
    procedure ApplyCoord(); override;
  public
    property ForeignKey: TWForeignKey read GetForeignKey;
  end;

  TWForeignKey = class(TWForeignKeyPoint)
  private
    FCaption: TCaption;
    FBaseForeignKey: TCForeignKey;
    function GetCaption(): TCaption;
    function GetForeignKeySelected(): Boolean;
    function GetIsLine(): Boolean;
    function GetPoint(Index: Integer): TWForeignKeyPoint;
    function GetPointCount(): Integer;
    function GetTable(Index: Integer): TWTable;
    procedure SetCaption(const ACaption: TCaption);
    procedure SetForeignKeySelected(AForeignKeySelected: Boolean);
    procedure SetTable(Index: Integer; ATable: TWTable);
  protected
    procedure Cleanup(const Sender: TWControl); virtual;
    procedure LoadFromXML(const XML: IXMLNode); override;
    procedure SaveToXML(const XML: IXMLNode); override;
    property Points[Index: Integer]: TWForeignKeyPoint read GetPoint;
  public
    constructor Create(const AWorkbench: TWWorkbench; const ACoord: TPoint; const PreviousPoint: TWPoint = nil); override;
    destructor Destroy(); override;
    property Caption: TCaption read GetCaption write SetCaption;
    property ChildTable: TWTable index 0 read GetTable write SetTable;
    property BaseForeignKey: TCForeignKey read FBaseForeignKey write FBaseForeignKey;
    property ForeignKeySelected: Boolean read GetForeignKeySelected write SetForeignKeySelected;
    property IsLine: Boolean read GetIsLine;
    property ParentTable: TWTable index 1 read GetTable write SetTable;
    property PointCount: Integer read GetPointCount;
  end;

  TWForeignKeys = class(TWObjects)
  private
    function GetForeignKey(Index: Integer): TWForeignKey;
    function GetSelCount(): Integer;
  protected
    procedure SaveToXML(const XML: IXMLNode); virtual;
  public
    property ForeignKey[Index: Integer]: TWForeignKey read GetForeignKey; default;
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
    constructor Create(const AWorkbench: TWWorkbench; const ACoord: TPoint); reintroduce; virtual;
    property Caption: TCaption read GetCaption write SetCaption;
    property Color: TColor read FColor write FColor;
  end;

  TWSections = class(TWObjects)
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
    constructor Create(const AWorkbench: TWWorkbench; const X, Y: Integer); reintroduce; virtual;
  end;

  TMWorkbenchState = (wsNormal, wsInsertForeignKey, wsInsertSection, wsLoading, wsAutoCreate);

  TWWorkbenchChangeEvent = procedure(Sender: TObject; Control: TWControl) of object;
  TWWorkbenchCursorMoveEvent = procedure(Sender: TObject; X, Y: Integer) of object;
  TWWorkbenchValidateControlEvent = procedure (Sender: TObject; Control: TWControl) of Object;

  TWWorkbench = class(TScrollBox)
  private
    CreatedTable: record BaseTable: TCBaseTable; Point: TPoint; end;
    FClient: TCClient;
    FDatabase: TCDatabase;
    FDatabaseName: string;
    FForeignKeys: TWForeignKeys;
    FHideSelection: Boolean;
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
    function GetDatabase(): TCDatabase;
    function GetSelCount(): Integer;
    procedure SetMultiSelect(AMultiSelect: Boolean);
    procedure SetSelected(ASelected: TWControl);
    procedure SetTableFocused(ATableFocused: TWTable);
    procedure CMEndLasso(var Message: TMessage); message CM_ENDLASSO;
  protected
    FModified: Boolean;
    State: TMWorkbenchState;
    procedure Change(); virtual;
    procedure CursorMove(const Coord: TPoint); virtual;
    procedure DoEnter(); override;
    procedure DoExit(); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure ReleaseControl(const Control: TWControl); virtual;
    procedure UpdateControl(const Control: TWControl); virtual;
    procedure ValidateControl(const Sender: TComponent; const Control: TWControl); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AOwner: TComponent; const ADatabase: TCDatabase); reintroduce; overload; virtual;
    destructor Destroy(); override;
    procedure BeginUpdate(); virtual;
    procedure CalcRange(const Reset: Boolean); virtual;
    procedure Clear(); virtual;
    procedure ClientUpdate(const Event: TCClient.TEvent);
    procedure EndUpdate(); virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function ForeignKeyByCaption(const Caption: string): TWForeignKey; virtual;
    procedure InsertForeignKey(const X, Y: Integer); virtual;
    procedure InsertSection(const X, Y: Integer); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure Print(const Title: string); virtual;
    procedure SaveToBMP(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    function TableAtCoord(const Coord: TPoint): TWTable;
    function TableByBaseTable(const ATable: TCBaseTable): TWTable; virtual;
    function TableByCaption(const Caption: string): TWTable; virtual;
    procedure TableCreated(const ATable: TCBaseTable; const APoint: TPoint); virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Database: TCDatabase read GetDatabase;
    property ForeignKeys: TWForeignKeys read FForeignKeys;
    property HideSelection: Boolean read FHideSelection write FHideSelection default False;
    property Modified: Boolean read FModified;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
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
  ExtCtrls, Math, Dialogs, StdActns, Consts, Printers;

const
  BorderSize = 1;
  LineWidth = 1; // nur ungerade Werte
  ConnectorSize = LineWidth + 6; // nur gerade Werte
  PointSize = LineWidth + 2; // nur gerade Werte
  Padding = 2;

function XMLNode(const Parent: IXMLNode; const Key: string; const NodeAutoCreate: Boolean = False): IXMLNode;
var
  ChildKey: string;
  CurrentKey: string;
begin
  if (not Assigned(Parent)) then
    Result := nil
  else if (Key = '') then
    Result := Parent
  else
  begin
    if (Pos('/', Key) = 0) then
    begin
      CurrentKey := Key;
      ChildKey := '';
    end
    else
    begin
      CurrentKey := Copy(Key, 1, Pos('/', Key) - 1);
      ChildKey := Key; Delete(ChildKey, 1, Length(CurrentKey) + 1);
    end;

    if (Assigned(Parent.ChildNodes.FindNode(CurrentKey))) then
      Result := XMLNode(Parent.ChildNodes.FindNode(CurrentKey), ChildKey, NodeAutoCreate)
    else if (NodeAutoCreate or (doNodeAutoCreate in Parent.OwnerDocument.Options)) then
      Result := XMLNode(Parent.AddChild(CurrentKey), ChildKey, NodeAutoCreate)
    else
      Result := nil;
  end;
end;

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

function CreateSegment(const Sender: TWControl; const ACoord: TPoint; const Point: TWPoint; const CreateBefore: Boolean = True): TWPoint;
var
  Line: TWLine;
  OldMoveState: TWPointMoveState;
begin
  OldMoveState := Point.MoveState;
  if (Point.MoveState = msNormal) then
    Point.MoveState := msFixed;

  if (CreateBefore) then
  begin
    if (Point is TWForeignKeyPoint) then
      Result := TWForeignKeyPoint.Create(Point.Workbench, Point.Coord, nil)
    else
      Result := TWPoint.Create(Point.Workbench, Point.Coord, nil);
    Result.LineA := Point.LineA;

    TWForeignKeyLine.Create(Point.Workbench, Result, Point);
  end
  else
  begin
    Line := Point.LineB;

    if (Assigned(Line)) then
      Line.PointA := nil;

    if (Point is TWForeignKeyPoint) then
      Result := TWForeignKeyPoint.Create(Point.Workbench, Point.Coord, Point)
    else
      Result := TWPoint.Create(Point.Workbench, Point.Coord, Point);
    Result.LineB := Line;
  end;

  Point.MoveState := OldMoveState;

  Result.MoveTo(Sender, [], ACoord);
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

{ TWObjects *******************************************************************}

procedure TWObjects.Clear();
begin
  Workbench.BeginUpdate();

  while (Count > 0) do
    Delete(Count - 1);

  inherited;

  Workbench.EndUpdate();
end;

constructor TWObjects.Create(const AWorkbench: TWWorkbench);
begin
  inherited Create();

  FWorkbench := AWorkbench;
end;

procedure TWObjects.Delete(Index: Integer);
begin
  TWControl(Items[Index]).Free();

  inherited;
end;

destructor TWObjects.Destroy();
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

constructor TWControl.Create(const AWorkbench: TWWorkbench);
begin
  inherited Create(AWorkbench);
  Parent := AWorkbench;

  FWorkbench := AWorkbench;

  FCoord := Point(-1, -1);
  FSelected := False;
  MouseDownCoord := Point(-1, -1);
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

procedure TWControl.LoadFromXML(const XML: IXMLNode);
var
  NewCoord: TPoint;
begin
  NewCoord := Coord;
  if (Assigned(XMLNode(XML, 'coord/x'))) then TryStrToInt(XMLNode(XML, 'coord/x').Text, NewCoord.X);
  if (Assigned(XMLNode(XML, 'coord/y'))) then TryStrToInt(XMLNode(XML, 'coord/y').Text, NewCoord.Y);
  MoveTo(Self, [], NewCoord);
end;

procedure TWControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Self is TWForeignKeyLine) then
    MouseDownCoord := TWForeignKeyLine(Self).PointA.Coord
  else
    MouseDownCoord := Coord;

  inherited;
  Workbench.SetFocus();

  if ((Button in [mbLeft, mbRight]) and (not (ssCtrl in Shift) and (not Selected or (Workbench.SelCount <= 1)) or (not Workbench.MultiSelect or (not (ssCtrl in Shift) and (Workbench.SelCount <= 1))))) then
  begin
    Workbench.Selected := Self;
    if (Self is TWForeignKeyPoint) then
      TWForeignKeyPoint(Self).ForeignKey.ForeignKeySelected := not TWForeignKeyPoint(Self).ForeignKey.ForeignKeySelected
    else if (Self is TWForeignKeyLine) then
      TWForeignKeyLine(Self).ForeignKey.ForeignKeySelected := not TWForeignKeyLine(Self).ForeignKey.ForeignKeySelected;

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
  DeltaX: Integer;
  DeltaY: Integer;
  Msg: TMsg;
  NewCoord: TPoint;
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
      DeltaX := 0;
      if (not (Self is TWArea) or (TWArea(Self).ResizeMode = rmNone)) then
      begin
        X := Max(X, MouseDownPoint.X - MouseDownCoord.X);
        if (MouseDownCoord.X + X - MouseDownPoint.X < Workbench.HorzScrollBar.Position) then
          DeltaX := (MouseDownCoord.X + X - MouseDownPoint.X) - Workbench.HorzScrollBar.Position
        else if (MouseDownCoord.X + Width + X - MouseDownPoint.X > Workbench.HorzScrollBar.Position + Workbench.ClientWidth) then
          DeltaX := MouseDownCoord.X + Width + X - MouseDownPoint.X - (Workbench.HorzScrollBar.Position + Workbench.ClientWidth);
      end
      else
      begin
        case (TWArea(Self).ResizeMode) of
          rmCreate:
            begin
              X := Max(X, MouseDownPoint.X - MouseDownCoord.X);
              X := Min(X, Max(Workbench.HorzScrollBar.Position + Workbench.ClientWidth, Workbench.HorzScrollBar.Range) - (MouseDownCoord.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X));
            end;
          rmN, rmS:
            X := MouseDownPoint.X;
          rmNW, rmW, rmSW:
            begin
              X := Max(X, MouseDownPoint.X - MouseDownCoord.X);
              X := Min(X, MouseDownCoord.X + TWArea(Self).MouseDownSize.cx + (MouseDownPoint.X - MouseDownCoord.X) - 2 * (BorderSize + Padding));
            end;
          rmNE, rmE, rmSE:
            begin
              X := Max(X, MouseDownCoord.X + 2 * (BorderSize + Padding) - (MouseDownCoord.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X));
              X := Min(X, Max(Workbench.HorzScrollBar.Position + Workbench.ClientWidth, Workbench.HorzScrollBar.Range) - (MouseDownCoord.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X));
            end;
        end;
        if ((TWArea(Self).ResizeMode in [rmCreate, rmNW, rmW, rmSW]) and (X < Workbench.HorzScrollBar.Position)) then
          DeltaX := X - Workbench.HorzScrollBar.Position - (MouseDownPoint.X - MouseDownCoord.X)
        else if ((TWArea(Self).ResizeMode in [rmCreate, rmNE, rmE, rmSE]) and (MouseDownCoord.X + TWArea(Self).MouseDownSize.cx + X - MouseDownPoint.X > Workbench.HorzScrollBar.Position + Workbench.ClientWidth)) then
          DeltaX := X - (Workbench.HorzScrollBar.Position + Workbench.ClientWidth) + (MouseDownCoord.X + TWArea(Self).MouseDownSize.cx - MouseDownPoint.X);
      end;
      if (Abs(DeltaX) > Workbench.HorzScrollBar.Increment) then
      begin
        Dec(X, Sign(DeltaX) * (Abs(DeltaX) - Workbench.HorzScrollBar.Increment));
        DeltaX := Sign(DeltaX) * Workbench.HorzScrollBar.Increment;
      end;

      DeltaY := 0;

      if (not (Self is TWArea) or (TWArea(Self).ResizeMode = rmNone)) then
      begin
        Y := Max(Y, MouseDownPoint.Y - MouseDownCoord.Y);
        if (MouseDownCoord.Y + Y - MouseDownPoint.Y < Workbench.VertScrollBar.Position) then
          DeltaY := (MouseDownCoord.Y + Y - MouseDownPoint.Y) - Workbench.VertScrollBar.Position
        else if (MouseDownCoord.Y + Height + Y - MouseDownPoint.Y > Workbench.VertScrollBar.Position + Workbench.ClientHeight) then
          DeltaY := MouseDownCoord.Y + Height + Y - MouseDownPoint.Y - (Workbench.VertScrollBar.Position + Workbench.ClientHeight);
      end
      else
      begin
        case (TWArea(Self).ResizeMode) of
          rmCreate:
            begin
              Y := Max(Y, MouseDownPoint.Y - MouseDownCoord.Y);
              Y := Min(Y, Max(Workbench.VertScrollBar.Position + Workbench.ClientHeight, Workbench.VertScrollBar.Range) - (MouseDownCoord.Y + TWArea(Self).MouseDownSize.cy - MouseDownPoint.Y));
            end;
          rmE, rmW:
            Y := MouseDownPoint.Y;
          rmNW, rmN, rmNE:
            begin
              Y := Max(Y, MouseDownPoint.Y - MouseDownCoord.Y);
              Y := Min(Y, MouseDownCoord.Y + TWArea(Self).MouseDownSize.cy + (MouseDownPoint.Y - MouseDownCoord.Y) - 2 * (BorderSize + Padding));
            end;
          rmSW, rmS, rmSE:
            begin
              Y := Max(Y, MouseDownCoord.Y + 2 * (BorderSize + Padding) - (MouseDownCoord.Y + TWArea(Self).MouseDownSize.cy - MouseDownPoint.Y));
              Y := Min(Y, Max(Workbench.VertScrollBar.Position + Workbench.ClientHeight, Workbench.VertScrollBar.Range) - (MouseDownCoord.Y + TWArea(Self).MouseDownSize.cy - MouseDownPoint.Y));
            end;
        end;
        if ((TWArea(Self).ResizeMode in [rmCreate, rmNE, rmN, rmNW]) and (Y < Workbench.VertScrollBar.Position)) then
          DeltaY := Y - Workbench.VertScrollBar.Position
        else if ((TWArea(Self).ResizeMode in [rmCreate, rmSE, rmS, rmSW]) and (MouseDownCoord.Y + TWArea(Self).MouseDownSize.cy + Y - MouseDownPoint.Y > Workbench.VertScrollBar.Position + Workbench.ClientHeight)) then
          DeltaY := Y - (Workbench.VertScrollBar.Position + Workbench.ClientHeight);
      end;
      if (Abs(DeltaY) > Workbench.VertScrollBar.Increment) then
      begin
        Dec(Y, Sign(DeltaY) * (Abs(DeltaY) - Workbench.VertScrollBar.Increment));
        DeltaY := Sign(DeltaY) * Workbench.VertScrollBar.Increment;
      end;

      if (GetTickCount() - Workbench.LastScrollTickCount < 50) then
      begin
        Dec(X, DeltaX);
        Dec(Y, DeltaY);
      end
      else if ((DeltaX <> 0) or (DeltaY <> 0)) then
      begin
        if (DeltaX <> 0) then
        begin
          Workbench.HorzScrollBar.Range := Max(Workbench.HorzScrollBar.Range, Workbench.HorzScrollBar.Position + Workbench.ClientWidth + DeltaX);
          Workbench.HorzScrollBar.Position := Max(0, Workbench.HorzScrollBar.Position + DeltaX);
        end;

        if (DeltaY <> 0) then
        begin
          Workbench.VertScrollBar.Range := Max(Workbench.VertScrollBar.Range, Workbench.VertScrollBar.Position + Workbench.ClientHeight + DeltaY);
          Workbench.VertScrollBar.Position := Max(0, Workbench.VertScrollBar.Position + DeltaY);
        end;

        Workbench.LastScrollTickCount := GetTickCount();
      end;
    end;

    if ((Self is TWArea) and (TWArea(Self).ResizeMode <> rmNone) and (ssLeft in Shift)) then
      TWArea(Self).ChangeSize(Self, Shift, X, Y)
    else
    begin
      NewCoord.X := MouseDownCoord.X + X - MouseDownPoint.X;
      NewCoord.Y := MouseDownCoord.Y + Y - MouseDownPoint.Y;
      Moving(Self, Shift, NewCoord);
      if ((Self is TWForeignKeyPoint) and not Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable)) then
        MoveTo(Self, Shift, NewCoord)
      else if (ssLeft in Shift) then
        Move(Self, Shift, NewCoord);
    end;

    Workbench.CursorMove(Point(X, Y));
  end;
end;

procedure TWControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if ((Button in [mbLeft]) and (not (ssCtrl in Shift) and (Workbench.SelCount <= 1) or (ssCtrl in Shift) and Workbench.MultiSelect and (Coord.X = MouseDownCoord.X) and (Coord.Y = MouseDownCoord.Y))) then
  begin
    if (ssCtrl in Shift) then
      Selected := not Selected
    else
    begin
      Workbench.Selected := Self;
      if (Self is TWForeignKeyPoint) then
        TWForeignKeyPoint(Self).ForeignKey.ForeignKeySelected := True
      else if (Self is TWForeignKeyLine) then
        TWForeignKeyLine(Self).ForeignKey.ForeignKeySelected := True;
    end;

    if (Self is TWTable) then
      Workbench.TableFocused := TWTable(Self)
    else
      Workbench.TableFocused := nil;
  end;

  MouseCapture := False;

  inherited;

  Workbench.CalcRange(True);
  MouseDownPoint := Point(-1, -1);
  MouseDownCoord := Point(-1, -1);

  if (Self is TWForeignKeyPoint) then
    TWForeignKeyPoint(Self).ForeignKey.Cleanup(Self)
  else if (Self is TWForeignKeyLine) then
    TWForeignKeyLine(Self).ForeignKey.Cleanup(Self)
  else if (Self is TWTable) then
    for I := 0 to TWTable(Self).ForeignKeyPointCount - 1 do
      TWTable(Self).ForeignKeyPoint[I].ForeignKey.Cleanup(Self);
end;

procedure TWControl.Move(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint);
var
  Controls: array of TWControl;
  I: Integer;
  NewControlCoord: TPoint;
  WantedNewCoord: TPoint;
begin
  if ((NewCoord.X <> Coord.X) or (NewCoord.Y <> Coord.Y)) then
  begin
    if ((Workbench.SelCount = 1) or (Self is TWForeignKeyPoint) and TWForeignKeyPoint(Self).ForeignKey.ForeignKeySelected and (Workbench.SelCount = TWForeignKeyPoint(Self).ForeignKey.PointCount * 2 - 1)) then
    begin
      Moving(Self, Shift, NewCoord);
      MoveTo(Self, Shift, NewCoord);
    end
    else
    begin
      Workbench.BeginUpdate();

      for I := 0 to Workbench.ControlCount - 1 do
        if ((Workbench.Controls[I] is TWControl) and not (Workbench.Controls[I] is TWLine) and TWControl(Workbench.Controls[I]).Selected) then
        begin
          WantedNewCoord.X := TWControl(Workbench.Controls[I]).Coord.X + NewCoord.X - Coord.X;
          WantedNewCoord.Y := TWControl(Workbench.Controls[I]).Coord.Y + NewCoord.Y - Coord.Y;
          NewControlCoord := WantedNewCoord;
          TWControl(Workbench.Controls[I]).Moving(nil, Shift, NewControlCoord);
          Dec(NewCoord.X, WantedNewCoord.X - NewControlCoord.X);
          Dec(NewCoord.Y, WantedNewCoord.Y - NewControlCoord.Y);
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
        NewControlCoord.X := Controls[I].Coord.X + NewCoord.X - Coord.X;
        NewControlCoord.Y := Controls[I].Coord.Y + NewCoord.Y - Coord.Y;
        Controls[I].MoveTo(nil, Shift, NewControlCoord);
      end;

      MoveTo(nil, [], NewCoord);

      Workbench.EndUpdate();
    end;

    Workbench.CalcRange(False);
  end;
end;

procedure TWControl.MoveTo(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint);
begin
  FCoord := NewCoord;

  Workbench.UpdateControl(Self);

  Workbench.FModified := True;
end;

procedure TWControl.Moving(const Sender: TWControl; const Shift: TShiftState; var NewCoord: TPoint);
begin
  if (NewCoord.X < 0) then
    NewCoord.X := 0;
  if (NewCoord.Y < 0) then
    NewCoord.Y := 0;
end;

procedure TWControl.Paint();
begin
  PaintTo(Canvas, 0, 0);
end;

procedure TWControl.SaveToXML(const XML: IXMLNode);
begin
  XMLNode(XML, 'coord/x').Text := IntToStr(Coord.X);
  XMLNode(XML, 'coord/y').Text := IntToStr(Coord.Y);
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

procedure TWArea.ApplyCoord();
begin
  SetBounds(
    Coord.X - Workbench.HorzScrollBar.Position,
    Coord.Y - Workbench.VertScrollBar.Position,
    Size.cx,
    Size.cy
  );
end;

constructor TWArea.Create(const AWorkbench: TWWorkbench);
begin
  inherited Create(AWorkbench);

  FSize.cx := 0;
  FSize.cy := 0;
end;

function TWArea.GetArea(): TRect;
begin
  Result.Left := FCoord.X;
  Result.Top := FCoord.Y;
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
  NewCoord: TPoint;
begin
  case (ResizeMode) of
    rmCreate:
      begin
        FSize.cx := Abs(X - MouseDownPoint.X);
        FSize.cy := Abs(Y - MouseDownPoint.Y);

        NewCoord.X := Min(MouseDownCoord.X, X);
        NewCoord.Y := Min(MouseDownCoord.Y, Y);
      end;
    rmN:
      begin
        FSize.cy := MouseDownSize.cy - (Y - MouseDownPoint.Y);

        NewCoord.X := MouseDownCoord.X;
        NewCoord.Y := MouseDownCoord.Y + (Y - MouseDownPoint.Y);
      end;
    rmNE:
      begin
        FSize.cx := MouseDownSize.cx + (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy - (Y - MouseDownPoint.Y);

        NewCoord.X := MouseDownCoord.X;
        NewCoord.Y := MouseDownCoord.Y + (Y - MouseDownPoint.Y);
      end;
    rmE:
      begin
        FSize.cx := MouseDownSize.cx + (X - MouseDownPoint.X);

        NewCoord.X := MouseDownCoord.X;
        NewCoord.Y := MouseDownCoord.Y;
      end;
    rmSE:
      begin
        FSize.cx := MouseDownSize.cx + (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy + (Y - MouseDownPoint.Y);

        NewCoord.X := MouseDownCoord.X;
        NewCoord.Y := MouseDownCoord.Y;
      end;
    rmS:
      begin
        FSize.cy := MouseDownSize.cy + (Y - MouseDownPoint.Y);

        NewCoord.X := MouseDownCoord.X;
        NewCoord.Y := MouseDownCoord.Y;
      end;
    rmSW:
      begin
        FSize.cx := MouseDownSize.cx - (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy + (Y - MouseDownPoint.Y);

        NewCoord.X := MouseDownCoord.X + (X - MouseDownPoint.X);
        NewCoord.Y := MouseDownCoord.Y;
      end;
    rmW:
      begin
        FSize.cx := MouseDownSize.cx - (X - MouseDownPoint.X);

        NewCoord.X := MouseDownCoord.X + (X - MouseDownPoint.X);
        NewCoord.Y := MouseDownCoord.Y;
      end;
    rmNW:
      begin
        FSize.cx := MouseDownSize.cx - (X - MouseDownPoint.X);
        FSize.cy := MouseDownSize.cy - (Y - MouseDownPoint.Y);

        NewCoord.X := MouseDownCoord.X + (X - MouseDownPoint.X);
        NewCoord.Y := MouseDownCoord.Y + (Y - MouseDownPoint.Y);
      end;
  end;

  Moving(Sender, Shift, NewCoord);
  MoveTo(Self, Shift, NewCoord);
end;

{ TWPoint *********************************************************************}

procedure TWPoint.ApplyCoord();
begin
  SetBounds(
    Coord.X - (PointSize - 1) div 2 - Workbench.HorzScrollBar.Position,
    Coord.Y - (PointSize - 1) div 2 - Workbench.VertScrollBar.Position,
    PointSize,
    PointSize);

  Center := Point((PointSize - 1) div 2, (PointSize - 1) div 2);
end;

function TWPoint.ControlAlign(const Control: TWControl): TAlign;
var
  ControlCoord: TPoint;
begin
  if (not Assigned(Control)) then
    Result := alNone
  else if (Control is TWTable) then
  begin
    if (Coord.X > TWTable(Control).Area.Right) then
      Result := alLeft
    else if (Coord.Y > TWTable(Control).Area.Bottom) then
      Result := alTop
    else if (Coord.X < TWTable(Control).Area.Left) then
      Result := alRight
    else if (Coord.Y < TWTable(Control).Area.Top) then
      Result := alBottom
    else
      Result := alNone;
  end
  else
  begin
    if (Control is TWPoint) then
      ControlCoord := Control.Coord
    else if ((Control is TWLine) and Assigned(TWLine(Control).PointA) and (TWLine(Control).PointA <> Self)) then
      ControlCoord := TWLine(Control).PointA.Coord
    else if ((Control is TWLine) and Assigned(TWLine(Control).PointB) and (TWLine(Control).PointB <> Self)) then
      ControlCoord := TWLine(Control).PointB.Coord
    else
      ControlCoord := Point(-1, -1);

    if ((ControlCoord.X < 0) or (ControlCoord.Y < 0)) then
      Result := alNone
    else if ((ControlCoord.X < Coord.X) and (ControlCoord.Y = Coord.Y)) then
      Result := alLeft
    else if ((ControlCoord.Y < Coord.Y) and (ControlCoord.X = Coord.X)) then
      Result := alTop
    else if ((ControlCoord.X > Coord.X) and (ControlCoord.Y = Coord.Y)) then
      Result := alRight
    else if ((ControlCoord.Y > Coord.Y) and (ControlCoord.X = Coord.X)) then
      Result := alBottom
    else
      Result := alNone;
  end;
end;

constructor TWPoint.Create(const AWorkbench: TWWorkbench; const ACoord: TPoint; const PreviousPoint: TWPoint = nil);
begin
  inherited Create(AWorkbench);
  Parent := AWorkbench;

  ControlA := nil;
  ControlB := nil;
  MoveState := msNormal;

  Canvas.Pen.Width := LineWidth;
  Canvas.Brush.Style := bsClear;

  if (Assigned(PreviousPoint)) then
    FSelected := PreviousPoint.Selected;

  MoveTo(nil, [], ACoord);

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
  if (Workbench.State in [wsInsertForeignKey, wsInsertSection]) then
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

procedure TWPoint.MoveTo(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint);

  procedure MovePointTo(const Point: TWForeignKeyPoint);
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
      and (ControlAlign(Control) in [alTop, alBottom]) and (NewCoord.X < Point.Coord.X) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alLeft) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Types.Point(Point.Coord.X, NewCoord.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewCoord.X := Coord.X;
    end
    else if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alLeft, alRight]) and (NewCoord.Y < Point.Coord.Y) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alTop) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Types.Point(NewCoord.X, Point.Coord.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewCoord.Y := Coord.Y;
    end
    else if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alTop, alBottom]) and (NewCoord.X > Point.Coord.X) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alRight) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Types.Point(Point.Coord.X, NewCoord.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewCoord.X := Coord.X;
    end
    else if (Assigned(Sender) and (Sender <> Self)
      and (ControlAlign(Control) in [alLeft, alRight]) and (NewCoord.Y > Point.Coord.Y) and (MoveState = msNormal)
      and ((ControlAlign(Sender) = alBottom) or (Point.MoveState = msFixed) or (Sender = AntiLine))) then
    begin
      CreateSegment(Self, Types.Point(NewCoord.X, Point.Coord.Y), Self, (Line = LineA) and not Assigned(AntiLine));
      MoveState := msNormal;
      if (Assigned(AntiLine)) then
        NewCoord.Y := Coord.Y;
    end

    // move line away from a moved table
    else if (Assigned(NextLine) and (Point.MoveState <> msFixed) and
      ((ControlAlign(Line) = alLeft) and ((NewCoord.X = Point.Coord.X) or (NewCoord.X <= Point.Coord.X) and ((ControlAlign(AntiControl) = alRight)))
      or (ControlAlign(Line) = alTop) and ((NewCoord.Y = Point.Coord.Y) or (NewCoord.Y <= Point.Coord.Y) and ((ControlAlign(AntiControl) = alBottom)))
      or (ControlAlign(Line) = alRight) and ((NewCoord.X = Point.Coord.X) or (NewCoord.X >= Point.Coord.X) and ((ControlAlign(AntiControl) = alLeft)))
      or (ControlAlign(Line) = alBottom) and ((NewCoord.Y = Point.Coord.Y) or (NewCoord.Y >= Point.Coord.Y) and ((ControlAlign(AntiControl) = alTop))))) then
      Point.MoveTo(Self, Shift, NewCoord)

    // adjust other end of a line
    else if ((Sender <> Point) and (Self is TWForeignKeyPoint) and Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable) and (Point.MoveState = msNormal)
      and ((ControlAlign(Line) in [alLeft, alRight]) or (Point.ControlAlign(NextLine) in [alTop, alBottom]) and (Point.Coord.Y = Coord.Y) and (Point.Coord.X = Coord.X))) then
      Point.MoveTo(Self, Shift, Types.Point(Point.Coord.X, NewCoord.Y))
    else if ((Sender <> Point) and (Self is TWForeignKeyPoint) and Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable) and (Point.MoveState = msNormal)
      and ((ControlAlign(Line) in [alTop, alBottom]) or (Point.ControlAlign(NextLine) in [alLeft, alRight]) and (Point.Coord.X = Coord.X) and (Point.Coord.Y = Coord.Y))) then
      Point.MoveTo(Self, Shift, Types.Point(NewCoord.X, Point.Coord.Y));

    // remove a crushed line
    if ((Sender <> Self)
      and (Self is TWForeignKeyPoint) and Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable)
      and Assigned(NextPoint) and (Point.Coord.X = NextPoint.Coord.X) and (Point.Coord.Y = NextPoint.Coord.Y)
      and (MoveState <> msAutomatic)) then
      FreeSegment(Point, NextLine);
  end;

var
  NewPoint: TWPoint;
  NewPoint2: TWPoint;
  OrgNewCoord: TPoint;
  TempOrientation: TWLineOrientation;
begin
  if (NewCoord <> Coord) then
  begin
    NewPoint := nil;
    if ((Sender <> Self) and (MoveState <> msFixed)) then
    begin
      OrgNewCoord := NewCoord;
      Moving(Sender, Shift, NewCoord);
      if (NewCoord <> OrgNewCoord) then
        NewPoint := CreateSegment(Sender, OrgNewCoord, Self, Assigned(LineA) and ((Sender = LineA) or (Sender = LineA.PointA)));
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
        and (NewCoord.X < LineA.PointA.LineA.PointA.Coord.X)) then
        TempOrientation := foHorizontal
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alTop)
        and (NewCoord.Y < LineA.PointA.LineA.PointA.Coord.Y)) then
        TempOrientation := foVertical
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alRight)
        and (NewCoord.X > LineA.PointA.LineA.PointA.Coord.X)) then
        TempOrientation := foHorizontal
      else if (Assigned(LineA.PointA.LineA.PointA.LineA)
        and (LineA.PointA.LineA.PointA.ControlAlign(LineA.PointA.LineA.PointA.LineA) = alBottom)
        and (NewCoord.Y > LineA.PointA.LineA.PointA.Coord.Y)) then
        TempOrientation := foVertical
      else if (Abs(NewCoord.X - LineA.PointA.LineA.PointA.Coord.X) >= Abs(NewCoord.Y - LineA.PointA.LineA.PointA.Coord.Y)) then
        TempOrientation := foVertical
      else
        TempOrientation := foHorizontal;

      case (TempOrientation) of
        foHorizontal: LineA.PointA.MoveTo(Self, [], Point(LineA.PointA.LineA.PointA.Coord.X, NewCoord.Y));
        foVertical: LineA.PointA.MoveTo(Self, [], Point(NewCoord.X, LineA.PointA.LineA.PointA.Coord.Y));
        else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Orientation']);
      end;
    end

    // build new point while creating a Foreign Key / or while inserting new point manually
    else if ((Sender = Self) and (Self is TWForeignKeyPoint) and (Workbench.State <> wsLoading)
      and ((MoveState = msFixed)
      or (MoveState = msNormal) and (ssShift in Shift)
      or (MoveState = msAutomatic) and (Sender = Self) and (ControlAlign(LineA) in [alLeft, alRight]) and ((ControlAlign(LineA) in [alLeft, alTop]) and (NewCoord.X <> Coord.X) or (ControlAlign(LineA) in [alTop, alBottom]) and (NewCoord.X <> Coord.X)))) then
    begin
      if (Abs(NewCoord.X - Coord.X) >= Abs(NewCoord.Y - Coord.Y)) then
        NewPoint := CreateSegment(Self, Point(NewCoord.X, Coord.Y), Self, Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable) and Assigned(LineA) and not Assigned(LineB))
      else
        NewPoint := CreateSegment(Self, Point(Coord.X, NewCoord.Y), Self, Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable) and Assigned(LineA) and not Assigned(LineB));
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
      NewPoint.MoveState := msAutomatic;

      if ((not Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable) or Assigned(LineB))
        and ((NewCoord.X <> NewPoint.Coord.X) or (NewCoord.Y <> NewPoint.Coord.Y))) then
      begin
        NewPoint2 := CreateSegment(Self, NewCoord, NewPoint, Assigned(TWForeignKeyPoint(Self).ForeignKey.ParentTable) and Assigned(LineA) and not Assigned(LineB));
        NewPoint2.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
        NewCoord := Coord;
      end
      else if (Abs(NewCoord.X - Coord.X) >= Abs(NewCoord.Y - Coord.Y)) then
        NewCoord.X := Coord.X
      else
        NewCoord.Y := Coord.Y;
    end

    // move a point away from a fixed point
    else if ((Sender = Self) and Assigned(LineA) and (NewCoord.Y <> Coord.Y) and (LineA.Orientation = foHorizontal) and (LineA.PointA.MoveState = msFixed)) then
    begin
      NewPoint := CreateSegment(Self, Types.Point(Coord.X, NewCoord.Y), Self, False);
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
    end
    else if ((Sender = Self) and Assigned(LineA) and (NewCoord.X <> Coord.X) and (LineA.Orientation = foVertical) and (LineA.PointA.MoveState = msFixed)) then
    begin
      NewPoint := CreateSegment(Self, Types.Point(NewCoord.X, Coord.Y), Self, False);
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
    end

    else if (Assigned(LineA) and (Sender <> LineA.PointA) and (Assigned(Sender) or not LineA.PointA.Selected)) then
      MovePointTo(TWForeignKeyPoint(LineA.PointA));

    if (Assigned(LineB) and (Sender <> LineB.PointB) and (Assigned(Sender) or not LineB.PointB.Selected)) then
      MovePointTo(TWForeignKeyPoint(LineB.PointB));

    if (Assigned(NewPoint) and (NewPoint.MoveState = msFixed)) then
      NewPoint.MoveState := msNormal;

    if ((NewCoord.X <> Coord.X) or (NewCoord.Y <> Coord.Y)) then
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

  procedure PaintToControl(const Control: TWControl);
  begin
    if (Assigned(Control)) then
      case (ControlAlign(Control)) of
        alLeft:   begin Canvas.MoveTo(X +        0, Y + Center.Y); Canvas.LineTo(X + Center.X + 1, Y + Center.Y    ); end;
        alTop:    begin Canvas.MoveTo(X + Center.X, Y +        0); Canvas.LineTo(X + Center.X    , Y + Center.Y + 1); end;
        alRight:  begin Canvas.MoveTo(X + Center.X, Y + Center.Y); Canvas.LineTo(X + Width       , Y + Center.Y    ); end;
        alBottom: begin Canvas.MoveTo(X + Center.X, Y + Center.Y); Canvas.LineTo(X + Center.X    , Y + Height      ); end;
      end;
  end;

var
  Rect: TRect;
begin
  if ((MoveState <> msFixed) and ((ControlAlign(LineA) = InvertAlign(ControlAlign(LineB))) and (ControlAlign(LineA) <> alNone)
    or not Assigned(ControlA) or not Assigned(ControlB)
    or Assigned(LineA) and (ControlAlign(LineA) = alNone)
    or Assigned(LineB) and (ControlAlign(LineB) = alNone))) then
  begin
    Canvas.Brush.Color := clRed;
    Rect := GetClientRect();
    OffsetRect(Rect, X, Y);
    Canvas.FillRect(Rect);
  end
  else if (not Selected) then
    Canvas.Brush.Color := clWindow
  else
  begin
    Canvas.Brush.Color := clHighlight;
    Rect := GetClientRect();
    OffsetRect(Rect, X, Y);
    Canvas.FillRect(Rect);
  end;

  if (Selected) then
    Canvas.Pen.Color := clHighlightText
  else if ((Self is TWForeignKeyPoint) and Assigned(TWForeignKeyPoint(Self).ForeignKey) and TWForeignKeyPoint(Self).ForeignKey.IsLine) then
    Canvas.Pen.Color := clGrayText
  else
    Canvas.Pen.Color := clWindowText;

  if (ControlA is TWLine) then
    PaintToControl(ControlA);

  if (ControlB is TWLine) then
    PaintToControl(ControlB);
end;

procedure TWPoint.SetLineA(ALineA: TWLine);
begin
  if (Assigned(LineA)) then
    LineA.PointB := nil;

  ControlA := ALineA;

  if (Assigned(LineA)) then
  begin
    LineA.PointB := Self;
    if ((Workbench.SelCount = 1) or (Self is TWForeignKeyPoint) and TWForeignKeyPoint(Self).ForeignKey.ForeignKeySelected and (Workbench.SelCount = TWForeignKeyPoint(Self).ForeignKey.PointCount * 2 - 1)) then
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

procedure TWLine.ApplyCoord();
begin
  if ((FPointB.Coord.X >= 0) and (FPointB.Coord.Y >= 0) and (FPointB.Coord.X >= 0) and (FPointB.Coord.Y >= 0)) then
    case (Orientation) of
      foHorizontal:
        SetBounds(
          Min(PointA.Coord.X, PointB.Coord.X) + (PointSize - 1) div 2 - Workbench.HorzScrollBar.Position,
          PointA.Coord.Y - (LineWidth - 1) div 2 - Workbench.VertScrollBar.Position,
          Length - PointSize + 1,
          LineWidth
        );
      foVertical:
        SetBounds(
          PointA.Coord.X - (LineWidth - 1) div 2 - Workbench.HorzScrollBar.Position,
          Min(PointA.Coord.Y, PointB.Coord.Y) + (PointSize - 1) div 2 - Workbench.VertScrollBar.Position,
          LineWidth,
          Length - PointSize + 1
        );
    end;
end;

constructor TWLine.Create(const AWorkbench: TWWorkbench; const APointA, APointB: TWPoint);
begin
  inherited Create(AWorkbench);
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
  Result := Max(Abs(PointA.Coord.X - PointB.Coord.X), Abs(PointA.Coord.Y - PointB.Coord.Y));
end;

function TWLine.GetOrientation(): TWLineOrientation;
begin
  if (Assigned(PointA) and Assigned(PointB)) then
    if ((PointA.Coord.X = PointB.Coord.X) and (PointA.Coord.Y = PointB.Coord.Y)) then
      FOrientation := foNone
    else if (PointA.Coord.Y = PointB.Coord.Y) then
      FOrientation := foHorizontal
    else if (PointA.Coord.X = PointB.Coord.X) then
      FOrientation := foVertical
    else
      FOrientation := foNone;

  Result := FOrientation;
end;

procedure TWLine.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewCoord: TPoint;
  NewPoint: TWPoint;
  PointANewCoord: TPoint;
begin
  if (Workbench.State in [wsInsertForeignKey, wsInsertSection]) then
    Cursor := crNo
  else if (Orientation = foHorizontal) then
    Cursor := crSizeNS
  else if (Orientation = foVertical) then
    Cursor := crSizeWE
  else
    Cursor := crDefault;

  PointANewCoord := Point(MouseDownCoord.X + Workbench.HorzScrollBar.Position + Left + X - MouseDownPoint.X,
                          MouseDownCoord.Y + Workbench.VertScrollBar.Position + Top + Y - MouseDownPoint.Y);

  if (PointANewCoord.X < 0) then
    PointANewCoord.X := 0;
  if (PointANewCoord.Y < 0) then
    PointANewCoord.Y := 0;
    
  if (not (ssLeft in Shift)) then
    inherited
  else if ((ssShift in Shift) and (Self is TWForeignKeyLine) and (PointA is TWForeignKeyPoint)) then
  begin
    if ((Orientation = foHorizontal) and (PointANewCoord.Y <> PointA.Coord.Y)
      or (Orientation = foVertical) and (PointANewCoord.X <> PointA.Coord.X)) then
    begin
      MouseCapture := False;

      NewCoord := Point(Left + X, Top + Y);

      TWForeignKeyPoint(PointA).MoveState := msFixed;
      if (Orientation = foHorizontal) then
        NewPoint := CreateSegment(Self, Point(NewCoord.X, PointA.Coord.Y), PointA, False)
      else
        NewPoint := CreateSegment(Self, Point(PointA.Coord.X, NewCoord.Y), PointA, False);
      NewPoint.MoveState := msAutomatic;
      NewPoint := CreateSegment(Self, NewCoord, NewPoint, False);
      NewPoint.MouseDown(mbLeft, [], (PointSize - 1) div 2, (PointSize - 1) div 2);
    end;
  end
  else if ((Self is TWForeignKeyLine) and (Workbench.SelCount <> TWForeignKeyLine(Self).ForeignKey.PointCount * 2 - 1) and (Workbench.ForeignKeys.SelCount <> 1)) then
    PointA.Move(Self, Shift, PointANewCoord)
  else
    case (Orientation) of
      foHorizontal:
        PointA.MoveTo(Self, Shift, Point(PointA.Coord.X, PointANewCoord.Y));
      foVertical:
        PointA.MoveTo(Self, Shift, Point(PointANewCoord.X, PointA.Coord.Y));
    end;

  Workbench.CursorMove(Point(Left + Workbench.HorzScrollBar.Position + X, Top + Workbench.VertScrollBar.Position + Y));
end;

procedure TWLine.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
var
  Rect: TRect;
begin
  if (Selected) then
  begin
    Canvas.Pen.Color := clWindow;
    Canvas.Brush.Color := clHighlight;
    Rect := GetClientRect();
    OffsetRect(Rect, X, Y);
    Canvas.FillRect(Rect);
  end
  else if ((Self is TWForeignKeyLine) and TWForeignKeyLine(Self).ForeignKey.IsLine) then
    Canvas.Pen.Color := clGrayText
  else
    Canvas.Pen.Color := clWindowText;

  case (Orientation) of
    foHorizontal:
      begin
        Canvas.MoveTo(X +     0, Y + (Height - 1) div 2);
        Canvas.LineTo(X + Width, Y + (Height - 1) div 2);
      end;
    foVertical:
      begin
        Canvas.MoveTo(X + (Width - 1) div 2, Y +      0);
        Canvas.LineTo(X + (Width - 1) div 2, Y + Height);
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

procedure TWTable.ApplyCoord();
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

constructor TWTable.Create(const ATables: TWTables; const ABaseTable: TCBaseTable);
begin
  inherited Create(ATables.Workbench);

  FBaseTable := ABaseTable;

  SetLength(FForeignKeyPoints, 0);

  Canvas.Font := Font;
  Canvas.Font.Color := Font.Color;
end;

destructor TWTable.Destroy();
begin
  while (Length(FForeignKeyPoints) > 0) do
    Workbench.ForeignKeys.Delete(Workbench.ForeignKeys.IndexOf(FForeignKeyPoints[0].ForeignKey));

  inherited;
end;

function TWTable.GetCaption(): TCaption;
begin
  Result := BaseTable.Name;
end;

function TWTable.GetForeignKeyPoint(AIndex: Integer): TWForeignKeyPoint;
begin
  Result := FForeignKeyPoints[AIndex];
end;

function TWTable.GetForeignKeyPointCount(): Integer;
begin
  Result := Length(FForeignKeyPoints);
end;

function TWTable.GetIndex(): Integer;
begin
  Result := Workbench.Tables.IndexOf(Self);
end;

procedure TWTable.Invalidate();
begin
  if (CanAutoSize(FSize.cx, FSize.cy)) then
    ApplyCoord();
  inherited;
end;

procedure TWTable.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  Workbench.UpdateControl(Self);
end;

procedure TWTable.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ForeignKey: TWForeignKey;
begin
  if (Workbench.State = wsInsertForeignKey) then
  begin
    ForeignKey := TWForeignKey.Create(Workbench, Point(Workbench.HorzScrollBar.Position + Left + X, Workbench.VertScrollBar.Position + Top + Y));
    ForeignKey.TableA := Self;
    ForeignKey.MoveState := msFixed;
    ForeignKey.MouseDown(Button, Shift, Workbench.HorzScrollBar.Position + Left + X - ForeignKey.Left, Workbench.VertScrollBar.Position + Top + Y - ForeignKey.Top);
    Workbench.ForeignKeys.Add(ForeignKey);

    Workbench.State := wsNormal;
  end
  else
    inherited;
end;

procedure TWTable.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (Workbench.State = wsInsertForeignKey) then
    Cursor := crCross
  else if (Workbench.State = wsInsertSection) then
    Cursor := crNo
  else
    Cursor := crDefault;

  inherited;
end;

procedure TWTable.MoveTo(const Sender: TWControl; const Shift: TShiftState; NewCoord: TPoint);
var
  I: Integer;
begin
  for I := 0 to Length(FForeignKeyPoints) - 1 do
    if (not FForeignKeyPoints[I].Selected) then
      FForeignKeyPoints[I].MoveTo(Self, Shift, Point(FForeignKeyPoints[I].Coord.X + NewCoord.X - Coord.X, FForeignKeyPoints[I].Coord.Y + NewCoord.Y - Coord.Y));

  inherited;
end;

procedure TWTable.Moving(const Sender: TWControl; const Shift: TShiftState; var NewCoord: TPoint);
var
  I: Integer;
  TempCoord: TPoint;
begin
  inherited;

  for I := 0 to Length(FForeignKeyPoints) - 1 do
  begin
    TempCoord := Point(FForeignKeyPoints[I].Coord.X + (NewCoord.X - Coord.X), FForeignKeyPoints[I].Coord.Y + (NewCoord.Y - Coord.Y));
    FForeignKeyPoints[I].Moving(Self, Shift, TempCoord);
    NewCoord := Point(Coord.X + TempCoord.X - FForeignKeyPoints[I].Coord.X, Coord.Y + TempCoord.Y - FForeignKeyPoints[I].Coord.Y);
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
    Canvas.MoveTo(X + BorderSize, Y + BorderSize + 2 * Padding + -Canvas.Font.Height + I);
    Canvas.LineTo(X + ClientWidth - BorderSize, Y + BorderSize + 2 * Padding + -Canvas.Font.Height + I);
  end;

  Rect := GetClientRect();
  OffsetRect(Rect, X, Y);
  Inc(Rect.Left, BorderSize + Padding); Dec(Rect.Right, BorderSize - 1 + Padding);
  Inc(Rect.Top, BorderSize - 1 + Padding); Dec(Rect.Bottom, BorderSize - 1 + Padding);

  Flags := DrawTextBiDiModeFlags(DT_CENTER);
  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle, PChar(Caption), -1, Rect, Flags);

  Flags := DrawTextBiDiModeFlags(0);

  for I := 0 to BaseTable.Fields.Count - 1 do
  begin
    if (not BaseTable.Fields[I].InPrimaryIndex) then
      Canvas.Font.Style := Canvas.Font.Style - [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    Rect.Top := Y + 2 * BorderSize - 2 + (1 + I) * (-Canvas.Font.Height + Padding) + 3 * Padding;
    DrawText(Canvas.Handle, PChar(BaseTable.Fields[I].Name), -1, Rect, Flags);
  end;
end;

procedure TWTable.RegisterForeignKeyPoint(const AForeignKeyPoint: TWForeignKeyPoint);
var
  Found: Boolean;
  I: Integer;
begin
  Found := False;
  for I := 0 to Length(FForeignKeyPoints) - 1 do
    Found := Found or (FForeignKeyPoints[I] = AForeignKeyPoint);

  if (not Found) then
  begin
    SetLength(FForeignKeyPoints, Length(FForeignKeyPoints) + 1);
    FForeignKeyPoints[Length(FForeignKeyPoints) - 1] := AForeignKeyPoint;
  end;
end;

procedure TWTable.ReleaseForeignKeyPoint(const AForeignKeyPoint: TWForeignKeyPoint);
var
  I: Integer;
  Index: Integer;
begin
  Index := -1;
  for I := 0 to Length(FForeignKeyPoints) - 1 do
    if (FForeignKeyPoints[I] = AForeignKeyPoint) then
      Index := I;

  if (Index >= 0) then
  begin
    for I := Index to Length(FForeignKeyPoints) - 2 do
      FForeignKeyPoints[I] := FForeignKeyPoints[I + 1];

    SetLength(FForeignKeyPoints, Length(FForeignKeyPoints) - 1);
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

{ TWForeignKeyPoint ***********************************************************}

procedure TWForeignKeyPoint.ApplyCoord();
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

          TempLeft   := Min(TempLeft  , Coord.X - (ConnectorSize + 1)      );
          TempTop    := Min(TempTop   , Coord.Y - (ConnectorSize - 1) div 2);
          TempBottom := Max(TempBottom, Coord.Y + (ConnectorSize - 1) div 2);
        end;
      alTop:
        begin
          Center.X   := Max(Center.X, (ConnectorSize - 1) div 2            );
          Center.Y   := Max(Center.Y, ConnectorSize + (PointSize - 1) div 2);

          TempLeft   := Min(TempLeft  , Coord.X - (ConnectorSize - 1) div 2);
          TempTop    := Min(TempTop   , Coord.Y - (ConnectorSize + 1)      );
          TempRight  := Max(TempRight , Coord.X + (ConnectorSize - 1) div 2);
        end;
      alRight:
        begin
          Center.Y   := Max(Center.Y, (ConnectorSize - 1) div 2);

          TempTop    := Min(TempTop   , Coord.Y - (ConnectorSize - 1) div 2);
          TempRight  := Max(TempRight , Coord.X + (ConnectorSize + 1)      );
          TempBottom := Max(TempBottom, Coord.Y + (ConnectorSize - 1) div 2);
        end;
      alBottom:
        begin
          Center.X   := Max(Center.X, (ConnectorSize - 1) div 2);

          TempLeft   := Min(TempLeft  , Coord.X - (ConnectorSize - 1) div 2);
          TempRight  := Max(TempRight , Coord.X + (ConnectorSize - 1) div 2);
          TempBottom := Max(TempBottom, Coord.Y + (ConnectorSize + 1)      );
        end;
    end;
  end;

begin
  Center := Point((PointSize - 1) div 2, (PointSize - 1) div 2);

  TempLeft   := Coord.X - (PointSize - 1) div 2;
  TempTop    := Coord.Y - (PointSize - 1) div 2;
  TempRight  := Coord.X + (PointSize - 1) div 2;
  TempBottom := Coord.Y + (PointSize - 1) div 2;

  ExpandTableAlign(TableA);
  ExpandTableAlign(TableB);

  SetBounds(
    TempLeft - Workbench.HorzScrollBar.Position,
    TempTop - Workbench.VertScrollBar.Position,
    TempRight - TempLeft + 1,
    TempBottom - TempTop + 1
  );
end;

constructor TWForeignKeyPoint.Create(const AWorkbench: TWWorkbench; const ACoord: TPoint; const PreviousPoint: TWPoint = nil);
begin
  inherited Create(AWorkbench, ACoord);

  if (PreviousPoint is TWForeignKeyPoint) then
    LineA := TWForeignKeyLine.Create(Workbench, TWForeignKeyPoint(PreviousPoint), Self);

  Workbench.UpdateControl(Self);
end;

destructor TWForeignKeyPoint.Destroy();
begin
  TableA := nil;
  TableB := nil;

  inherited;
end;

function TWForeignKeyPoint.GetForeignKey(): TWForeignKey;
var
  Point: TWForeignKeyPoint;
begin
  Point := Self;
  while (Assigned(Point.LineA) and (Point.LineA.PointA is TWForeignKeyPoint)) do
    Point := TWForeignKeyPoint(Point.LineA.PointA);

  if (not Assigned(Point)) then
    raise Exception.Create('Point is not assigned')
  else if (not (Point is TWForeignKey)) then
    raise Exception.Create('Point is not TWForeignKey')
  else
    Result := TWForeignKey(Point);
end;

function TWForeignKeyPoint.GetIndex(): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to ForeignKey.PointCount - 1 do
    if (Self = ForeignKey.Points[I]) then
      Result := I;
end;

function TWForeignKeyPoint.GetTableA(): TWTable;
begin
  if (not (ControlA is TWTable)) then
    Result := nil
  else
    Result := TWTable(ControlA);
end;

function TWForeignKeyPoint.GetTableB(): TWTable;
begin
  if (not (ControlB is TWTable)) then
    Result := nil
  else
    Result := TWTable(ControlB);
end;

procedure TWForeignKeyPoint.Moving(const Sender: TWControl; const Shift: TShiftState; var NewCoord: TPoint);

  procedure MovingConnector(const Sender: TWControl; const Shift: TShiftState; const Table: TWTable; var NewCoord: TPoint);
  var
    TempAlign: TAlign;
  begin
    if ((NewCoord.X > Table.Coord.X + Table.Width)
      and (Table.Coord.Y + (ConnectorSize - 1) div 2 <= NewCoord.Y) and (NewCoord.Y < Table.Coord.Y + Table.Height - (ConnectorSize - 1) div 2)) then
      TempAlign := alLeft
    else if ((NewCoord.Y > Table.Coord.Y + Table.Height)
      and (Table.Coord.X + (ConnectorSize - 1) div 2 <= NewCoord.X) and (NewCoord.X < Table.Coord.X + Table.Width - (ConnectorSize - 1) div 2)) then
      TempAlign := alTop
    else if ((NewCoord.X < Table.Coord.X)
      and (Table.Coord.Y + (ConnectorSize - 1) div 2 <= NewCoord.Y) and (NewCoord.Y < Table.Coord.Y + Table.Height - (ConnectorSize - 1) div 2)) then
      TempAlign := alRight
    else if ((NewCoord.Y < Table.Coord.Y)
      and (Table.Coord.X + (ConnectorSize - 1) div 2 <= NewCoord.X) and (NewCoord.X < Table.Coord.X + Table.Width - (ConnectorSize - 1) div 2)) then
      TempAlign := alBottom
    else
      TempAlign := ControlAlign(Table);

    if (TempAlign in [alTop, alBottom]) then
      if (NewCoord.X < Table.Coord.X + (ConnectorSize - 1) div 2) then
        NewCoord.X := Table.Coord.X + (ConnectorSize - 1) div 2
      else if (NewCoord.X > Table.Coord.X + Table.Width - (ConnectorSize - 1) div 2 - 1) then
        NewCoord.X := Table.Coord.X + Table.Width - (ConnectorSize - 1) div 2 - 1;

    if (TempAlign in [alLeft, alRight]) then
      if (NewCoord.Y < Table.Coord.Y + (ConnectorSize - 1) div 2) then
        NewCoord.Y := Table.Coord.Y + (ConnectorSize - 1) div 2
      else if (NewCoord.Y > Table.Coord.Y + Table.Height - (ConnectorSize - 1) div 2 - 1) then
        NewCoord.Y := Table.Coord.Y + Table.Height - (ConnectorSize - 1) div 2 - 1;

    case (TempAlign) of
      alLeft: NewCoord.X := Table.Coord.X + (Table.Width + ConnectorSize + (PointSize - 1) div 2);
      alTop: NewCoord.Y := Table.Coord.Y + (Table.Height + ConnectorSize + (PointSize - 1) div 2);
      alRight: NewCoord.X := Table.Coord.X - (ConnectorSize + (PointSize + 1) div 2);
      alBottom: NewCoord.Y := Table.Coord.Y - (ConnectorSize + (PointSize + 1) div 2);
    end;
  end;

begin
  inherited;

  if ((Sender <> Self) and Assigned(LineA) and Assigned(LineB) and Assigned(ForeignKey.ParentTable)) then
    if ((ControlAlign(LineA) <> alNone) and (ControlAlign(LineA) = ControlAlign(LineB))) then
      NewCoord := Coord
    else if ((ControlAlign(LineA) = alLeft) and (ControlAlign(LineB) = alRight)
      or (ControlAlign(LineA) = alRight) and (ControlAlign(LineB) = alLeft)) then
      NewCoord.Y := Coord.Y
    else if ((ControlAlign(LineA) = alTop) and (ControlAlign(LineB) = alBottom)
      or (ControlAlign(LineA) = alBottom) and (ControlAlign(LineB) = alTop)) then
      NewCoord.X := Coord.X;

  if ((Sender <> TableA) and Assigned(TableA) and not (Selected and TableA.Selected)) then
    MovingConnector(Sender, Shift, TableA, NewCoord);
  if ((Sender <> TableB) and Assigned(TableB) and not (Selected and TableB.Selected)) then
    MovingConnector(Sender, Shift, TableB, NewCoord);
end;

procedure TWForeignKeyPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not Assigned(ForeignKey.ParentTable) and (ForeignKey.PointCount > 1)) then
    TableB := Workbench.TableAtCoord(Point(Coord.X, Coord.Y));

  inherited;

  if (not Assigned(ForeignKey.ParentTable)) then
  begin
    MouseDown(Button, Shift, X, Y);
    MoveState := msFixed;
    if ((ForeignKey.PointCount = 2) and not Assigned(ForeignKey.ParentTable) and Assigned(LineA)
      and (Workbench.TableAtCoord(Coord) = ForeignKey.ChildTable)) then
    begin
      LineA.PointA.MoveState := msFixed;
      MoveState := msAutomatic;
    end;
  end
  else if (ForeignKey.Caption = '') then
  begin
    Workbench.ValidateControl(Workbench, ForeignKey);
  end;
end;

procedure TWForeignKeyPoint.PaintTo(const Canvas: TCanvas; const X, Y: Integer);
begin
  inherited;

  if (Assigned(TableA) and ((MoveState <> msFixed) or Assigned(ForeignKey.ParentTable))) then
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

procedure TWForeignKeyPoint.SetTableA(ATableA: TWTable);
begin
  if (ControlA is TWTable) then
    TWTable(ControlA).ReleaseForeignKeyPoint(Self);

  ControlA := ATableA;

  if (Assigned(TableA)) then
    TableA.RegisterForeignKeyPoint(Self);
end;

procedure TWForeignKeyPoint.SetTableB(ATableB: TWTable);
begin
  if (Assigned(TableB)) then
    TableB.ReleaseForeignKeyPoint(Self);

  ControlB := ATableB;

  if (Assigned(TableB)) then
    TableB.RegisterForeignKeyPoint(Self);
end;

{ TWForeignKeyLine ************************************************************}

procedure TWForeignKeyLine.ApplyCoord();
begin
  if (Assigned(Workbench) and Assigned(PointA) and Assigned(PointB)) then
    case (Orientation) of
      foHorizontal:
        SetBounds(
          Min(PointA.Coord.X, PointB.Coord.X) + (PointSize + 1) div 2 - Workbench.HorzScrollBar.Position,
          PointA.Coord.Y - (PointSize - 1) div 2 - Workbench.VertScrollBar.Position,
          Length - PointSize,
          PointSize
        );
      foVertical:
        SetBounds(
          PointA.Coord.X - (PointSize - 1) div 2 - Workbench.HorzScrollBar.Position,
          Min(PointA.Coord.Y, PointB.Coord.Y) + (PointSize + 1) div 2 - Workbench.VertScrollBar.Position,
          PointSize,
          Length - PointSize
        );
    end;
end;

function TWForeignKeyLine.GetForeignKey(): TWForeignKey;
begin
  if (not (PointA is TWForeignKeyPoint)) then
    raise Exception.Create('No PointA')
  else
    Result := TWForeignKeyPoint(PointA).ForeignKey;
end;

{ TWForeignKey ****************************************************************}

procedure TWForeignKey.Cleanup(const Sender: TWControl);

  function PointAlign(const TestCoord: TPoint; Area: TRect): TAlign;
  begin
    if (TestCoord.X < Area.Left) then
      Result := alLeft
    else if (TestCoord.Y < Area.Top) then
      Result := alTop
    else if (TestCoord.X > Area.Right) then
      Result := alRight
    else if (TestCoord.Y > Area.Bottom) then
      Result := alBottom
    else
      Result := alNone;
  end;

  procedure FixPointAlign(const Point: TWForeignKeyPoint; const Table: TWTable);
  var
    Line: TWLine;
    LinePoint: TWPoint;
    NewCoord: TPoint;
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

    if (Assigned(Line) and (PtInRect(Table.Area, Point.Coord) or (Point.ControlAlign(Line) = Point.ControlAlign(Table)))) then
    begin
      NewCoord := Point.Coord;
      case (PointAlign(LinePoint.Coord, Table.Area)) of
        alLeft: NewCoord.X := Table.Area.Left - ConnectorSize - (PointSize + 1) div 2;
        alTop: NewCoord.Y := Table.Area.Top - ConnectorSize - (PointSize + 1) div 2;
        alRight: NewCoord.X := Table.Area.Right + ConnectorSize + (PointSize + 1) div 2 - 1;
        alBottom: NewCoord.Y := Table.Area.Bottom + ConnectorSize + (PointSize + 1) div 2 - 1;
      end;
      Point.MoveTo(Point, [], NewCoord);
    end;
  end;

var
  I: Integer;
  NextPoint: TWForeignKeyPoint;
  Point: TWForeignKeyPoint;
  TempTable: TWTable;
begin
  for I := 0 to PointCount - 1 do
  begin
    Points[I].MoveState := msNormal;
    Points[I].MouseDownCoord := Types.Point(-1, -1);
    Points[I].MouseDownPoint := Types.Point(-1, -1);
  end;

  Point := TWForeignKeyPoint(LastPoint);
  while (Assigned(Point) and (Point <> Self)) do
  begin
    NextPoint := TWForeignKeyPoint(Point.LineA.PointA);

    if ((Workbench.TableAtCoord(NextPoint.Coord) = ParentTable)
      and (Assigned(ParentTable) and (ParentTable <> ChildTable))) then
    begin
      Point.MoveTo(Point, [], NextPoint.Coord);
      if ((Sender = NextPoint) or (NextPoint = Self)) then
      begin
        NextPoint.TableB := Point.TableB;
        Point := NextPoint;
        FreeSegment(Point.LineB.PointB, Point.LineB);
      end
      else
        FreeSegment(Point.LineA.PointA, Point.LineA);
    end;

    if (not Assigned(Point.LineA) or not (Point.LineA.PointA is TWForeignKeyPoint)) then
      Point := nil
    else
      Point := TWForeignKeyPoint(Point.LineA.PointA);
  end;

  Point := Self;
  while (Assigned(Point) and (Point <> LastPoint)) do
  begin
    NextPoint := TWForeignKeyPoint(Point.LineB.PointB);

    if ((Workbench.TableAtCoord(NextPoint.Coord) = ChildTable)
      and (Assigned(ParentTable) and (ParentTable <> ChildTable) or (Point.Index > 2))) then
    begin
      Point.MoveTo(Point, [], NextPoint.Coord);
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

    if (not Assigned(Point.LineB) or not (Point.LineB.PointB is TWForeignKeyPoint)) then
      Point := nil
    else
      Point := TWForeignKeyPoint(Point.LineB.PointB);
  end;

  Point := Self;
  repeat
    if (Assigned(Point.LineB) and (Point.LineB.PointB is TWForeignKeyPoint)) then
    begin
      NextPoint := TWForeignKeyPoint(Point.LineB.PointB);

      if ((NextPoint.ControlAlign(NextPoint.LineA) = InvertAlign(NextPoint.ControlAlign(NextPoint.LineB)))
        or (NextPoint.Coord.X = Point.Coord.X) and (NextPoint.Coord.Y = Point.Coord.Y)
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

    if (not Assigned(Point.LineB) or not (Point.LineB.PointB is TWForeignKeyPoint)) then
      Point := nil
    else
      Point := TWForeignKeyPoint(Point.LineB.PointB);
  until (not Assigned(Point));

  if (Assigned(Self.TableA) and Assigned(Self.LineB)) then
    FixPointAlign(Self, Self.TableA);
  if ((LastPoint is TWForeignKeyPoint) and Assigned(TWForeignKeyPoint(LastPoint).TableB) and Assigned(LastPoint.LineA)) then
    FixPointAlign(TWForeignKeyPoint(LastPoint), TWForeignKeyPoint(LastPoint).TableB);

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

  if ((LastPoint is TWForeignKeyPoint) and not Assigned(TWForeignKeyPoint(LastPoint).ForeignKey)) then
    raise Exception.Create('Unknown ForeignKey');
end;

constructor TWForeignKey.Create(const AWorkbench: TWWorkbench; const ACoord: TPoint; const PreviousPoint: TWPoint = nil);
begin
  inherited;

  FCaption := '';
  FBaseForeignKey := nil;
end;

destructor TWForeignKey.Destroy();
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

function TWForeignKey.GetCaption(): TCaption;
begin
  if (not Assigned(BaseForeignKey)) then
    Result := FCaption
  else
    Result := BaseForeignKey.Name;
end;

function TWForeignKey.GetForeignKeySelected(): Boolean;
var
  I: Integer;
begin
  Result := Selected;

  for I := 1 to PointCount - 1 do
    Result := Result and Points[I].LineA.Selected and Points[I].Selected;
end;

function TWForeignKey.GetIsLine(): Boolean;
begin
  Result := Copy(Caption, 1, 7) = '__Line_';
end;

function TWForeignKey.GetPoint(Index: Integer): TWForeignKeyPoint;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Index - 1 do
    if (Assigned(Result)) then
      if (not Assigned(Result.LineB) or not (Result.LineB.PointB is TWForeignKeyPoint)) then
        Result := nil
      else
        Result := TWForeignKeyPoint(Result.LineB.PointB);
end;

function TWForeignKey.GetPointCount(): Integer;
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

function TWForeignKey.GetTable(Index: Integer): TWTable;
begin
  case (Index) of
    0: Result := TableA;
    1:
      if (LastPoint is TWForeignKeyPoint) then
        Result := TWForeignKeyPoint(LastPoint).TableB
      else
        Result := nil;
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Index']);
  end;
end;

procedure TWForeignKey.LoadFromXML(const XML: IXMLNode);

  function ConnectorCoord(const Table: TWTable; const Align: TAlign; const Position: Integer): TPoint;
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
  I: Integer;
  Index: Integer;
  J: Integer;
  Point: TWForeignKeyPoint;
  PointsNode: IXMLNode;
  Position: Integer;
  Table: TWTable;
begin
  Workbench.State := wsLoading;

  Caption := XML.Attributes['name'];

  if (Assigned(XMLNode(XML, 'tables/child')) and (XMLNode(XML, 'tables/child').Attributes['name'] <> Null)
    and Assigned(XMLNode(XML, 'tables/parent')) and (XMLNode(XML, 'tables/parent').Attributes['name'] <> Null)) then
  begin
    TableA := Workbench.TableByCaption(XMLNode(XML, 'tables/child').Attributes['name']);
    Table := Workbench.TableByCaption(XMLNode(XML, 'tables/parent').Attributes['name']);

    if (Assigned(TableA) and Assigned(Table)) then
    begin
      if (Assigned(XMLNode(XML, 'tables/child/align'))
        and TryStrToAlign(XMLNode(XML, 'tables/child/align').Text, Align)
        and Assigned(XMLNode(XML, 'tables/child/position'))
        and TryStrToInt(XMLNode(XML, 'tables/child/position').Text, Position)) then
        MoveTo(Self, [], ConnectorCoord(TableA, Align, Position));

      I := 0;
      PointsNode := XMLNode(XML, 'points');
      if (Assigned(PointsNode)) then
        repeat
          for J := 0 to PointsNode.ChildNodes.Count - 1 do
            if ((PointsNode.ChildNodes[J].NodeName = 'point') and (TryStrToInt(PointsNode.ChildNodes[J].Attributes['index'], Index) and (Index = I))) then
            begin
              Point := TWForeignKeyPoint.Create(Workbench, Types.Point(-1, -1), LastPoint);
              Point.LoadFromXML(PointsNode.ChildNodes[J]);
            end;
          Inc(I);
        until (PointCount < I);

      Point := TWForeignKeyPoint.Create(Workbench, Types.Point(-1, -1), LastPoint);
      Point.TableB := Table;
      if (Assigned(XMLNode(XML, 'tables/parent/align'))
        and TryStrToAlign(XMLNode(XML, 'tables/parent/align').Text, Align)
        and Assigned(XMLNode(XML, 'tables/parent/position'))
        and TryStrToInt(XMLNode(XML, 'tables/parent/position').Text, Position)) then
        Point.MoveTo(nil, [], ConnectorCoord(Table, Align, Position));

      Cleanup(Self);
    end;

    if (Assigned(ChildTable)) then
      BaseForeignKey := ChildTable.BaseTable.ForeignKeyByName(Caption);
  end;

  Workbench.State := wsNormal;
end;

procedure TWForeignKey.SaveToXML(const XML: IXMLNode);
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
      XMLNode(XML, 'tables/child/position').Text := IntToStr(Coord.Y - TableA.Coord.Y);
    alTop, alBottom:
      XMLNode(XML, 'tables/child/position').Text := IntToStr(Coord.X - TableA.Coord.X);
  end;

  XMLNode(XML, 'tables/parent').Attributes['name'] := ParentTable.Caption;
  XMLNode(XML, 'tables/parent/align').Text := AlignToStr(InvertAlign(LastPoint.ControlAlign(TWForeignKeyPoint(LastPoint).TableB)));
  case (InvertAlign(LastPoint.ControlAlign(TWForeignKeyPoint(LastPoint).TableB))) of
    alLeft, alRight:
      XMLNode(XML, 'tables/parent/position').Text := IntToStr(LastPoint.Coord.Y - TWForeignKeyPoint(LastPoint).TableB.Coord.Y);
    alTop, alBottom:
      XMLNode(XML, 'tables/parent/position').Text := IntToStr(LastPoint.Coord.X - TWForeignKeyPoint(LastPoint).TableB.Coord.X);
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

procedure TWForeignKey.SetCaption(const ACaption: TCaption);
begin
  FCaption := ACaption;

  if (not Assigned(BaseForeignKey)) then
    Workbench.FModified := True;
end;

procedure TWForeignKey.SetForeignKeySelected(AForeignKeySelected: Boolean);
var
  I: Integer;
begin
  Workbench.BeginUpdate();

  Selected := AForeignKeySelected;
  for I := 1 to PointCount - 1 do
    Points[I].Selected := AForeignKeySelected;

  Workbench.EndUpdate();
end;

procedure TWForeignKey.SetTable(Index: Integer; ATable: TWTable);
begin
  Workbench.State := wsAutoCreate;

  case (Index) of
    0:
      begin
        TableA := ATable;
        if (Assigned(ATable)) then
          MoveTo(Self, [], Point(ATable.Coord.X + (ATable.Area.Right - ATable.Area.Left) div 2, ATable.Coord.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
      end;
    1:
      begin
        if (Assigned(ATable) and (LastPoint is TWForeignKeyPoint)) then
        begin
          if (ChildTable = ATable) then
          begin
            LastPoint.MoveTo(LastPoint, [], Point(ATable.Area.Left + (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Coord.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(LastPoint, [], Point(ATable.Area.Left + (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Area.Top - 2 * ConnectorSize));
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(LastPoint, [], Point(ATable.Area.Right - (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Area.Top - 2 * ConnectorSize));
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(LastPoint, [], Point(ATable.Area.Right - (ATable.Area.Right - ATable.Area.Left) div 3, ATable.Coord.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
          end
          else
          begin
            LastPoint.MoveState := msFixed;
            LastPoint.MoveTo(Self, [], Point(ATable.Coord.X + (ATable.Area.Right - ATable.Area.Left) div 2, ATable.Coord.Y + (ATable.Area.Bottom - ATable.Area.Top) div 2));
          end;
          TWForeignKeyPoint(LastPoint).TableB := ATable;
        end;
      end;
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Index']);
  end;

  Cleanup(Self);

  Workbench.State := wsNormal;
end;

{ TWForeignKeys ***************************************************************}

function TWForeignKeys.GetForeignKey(Index: Integer): TWForeignKey;
begin
  Result := TWForeignKey(Items[Index]);
end;

function TWForeignKeys.GetSelCount(): Integer;
var
  I: Integer;
  J: Integer;
  Selected: Boolean;
begin
  Result := 0;

  for I := 0 to Count - 1 do
  begin
    Selected := ForeignKey[I].Points[0].Selected;
    for J := 1 to ForeignKey[I].PointCount - 1 do
      Selected := Selected and ForeignKey[I].Points[J].LineA.Selected and ForeignKey[I].Points[J].Selected;
    if (Selected) then
      Inc(Result);
  end;
end;

procedure TWForeignKeys.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
  J: Integer;
  Node: IXMLNode;
begin
  for I := XML.ChildNodes.Count - 1 downto 0 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'foreignkey') and not Assigned(Workbench.ForeignKeyByCaption(XML.ChildNodes.Nodes[I].Attributes['name'])) then
      XML.ChildNodes.Delete(I);

  for I := 0 to Count - 1 do
  begin
    Node := nil;
    for J := 0 to XML.ChildNodes.Count - 1 do
      if ((XML.ChildNodes.Nodes[J].NodeName = 'foreignkey') and (Workbench.ForeignKeyByCaption(XML.ChildNodes.Nodes[J].Attributes['name']) = ForeignKey[I])) then
        Node := XML.ChildNodes.Nodes[J];
    if (not Assigned(Node)) then
    begin
      Node := XML.AddChild('foreignkey');
      if (not ForeignKey[I].IsLine) then
        Node.Attributes['name'] := ForeignKey[I].Caption;
    end;

    ForeignKey[I].SaveToXML(Node);
  end;
end;

{ TWSection *******************************************************************}

constructor TWSection.Create(const AWorkbench: TWWorkbench; const ACoord: TPoint);
begin
  inherited Create(AWorkbench);

  FColor := clGreen;

  Canvas.Font := Workbench.Font;
  Canvas.Font.Name := Workbench.Font.Name;
  Canvas.Font.Style := [];

  MoveTo(Self, [], ACoord);
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
  for I := 0 to XML.ChildNodes.Count - 1 do
    if (XML.ChildNodes.Nodes[I].NodeName = 'section') then
    begin
      Section := TWSection.Create(Workbench, Point(-1, -1));
      Section.LoadFromXML(XML.ChildNodes.Nodes[I]);
      Add(Section);
    end;
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

constructor TWLasso.Create(const AWorkbench: TWWorkbench; const X, Y: Integer);
begin
  inherited Create(AWorkbench);

  Canvas.Brush.Style := bsClear;

  MoveTo(Self, [], Point(X, Y));

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
      TWPoint(Workbench.Controls[I]).Selected := PtInRect(LassoRect, TWPoint(Workbench.Controls[I]).Coord);

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

  ForeignKeys.Clear();
  Tables.Clear();
  Sections.Clear();

  EndUpdate();
end;

procedure TWWorkbench.ClientUpdate(const Event: TCClient.TEvent);
var
  BaseTable: TCBaseTable;
  ForeignKey: TWForeignKey;
  I: Integer;
  J: Integer;
  OldModified: Boolean;
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

    for I := ForeignKeys.Count - 1 downto 0 do
      if ((ForeignKeys[I].ChildTable.BaseTable = BaseTable)
        and not Assigned(ForeignKeys[I].ChildTable.BaseTable.ForeignKeyByName(ForeignKeys[I].Caption))) then
        ForeignKeys.Delete(I);

    if (BaseTable = CreatedTable.BaseTable) then
    begin
      BeginUpdate();
      Table := TWTable.Create(Tables, CreatedTable.BaseTable);
      Table.Move(nil, [], CreatedTable.Point);
      Tables.Add(Table);
      Selected := Table;
      EndUpdate();

      CreatedTable.BaseTable := nil;
    end
    else if (Assigned(TableByBaseTable(BaseTable))) then
    begin
      Table := TableByBaseTable(BaseTable);
      if (Assigned(Table)) then
        Table.Invalidate();
    end
    else if (Assigned(XML)) then
    begin
      OldModified := FModified;

      for I := 0 to XML.ChildNodes.Count - 1 do
        if ((XML.ChildNodes.Nodes[I].NodeName = 'table') and (Database.Tables.NameCmp(XML.ChildNodes.Nodes[I].Attributes['name'], BaseTable.Name) = 0)) then
        begin
          Table := TWTable.Create(Tables, BaseTable);
          Table.LoadFromXML(XML.ChildNodes.Nodes[I]);
          Tables.Add(Table);

          for J := 0 to XML.ChildNodes.Count - 1 do
            if ((XML.ChildNodes.Nodes[J].NodeName = 'foreignkey')
              and Assigned(XMLNode(XML.ChildNodes.Nodes[J], 'tables/child'))
              and (XMLNode(XML.ChildNodes.Nodes[J], 'tables/child').Attributes['name'] <> Null)
              and ((Database.Tables.NameCmp(XMLNode(XML.ChildNodes.Nodes[J], 'tables/child').Attributes['name'], BaseTable.Name) = 0)
                or (Database.Tables.NameCmp(XMLNode(XML.ChildNodes.Nodes[J], 'tables/parent').Attributes['name'], BaseTable.Name) = 0))) then
            begin
              ForeignKey := TWForeignKey.Create(Self, Point(-1, -1));
              ForeignKey.LoadFromXML(XML.ChildNodes.Nodes[J]);
              if (not Assigned(ForeignKey.ChildTable) or not Assigned(ForeignKey.ParentTable) or not Assigned(ForeignKey.BaseForeignKey)) then
                ForeignKey.Free()
              else
                ForeignKeys.Add(ForeignKey);
            end;
        end;

      FModified := OldModified;
    end;

    for J := 0 to BaseTable.ForeignKeys.Count - 1 do
      if (not Assigned(ForeignKeyByCaption(BaseTable.ForeignKeys[J].Name))
        and Assigned(TableByCaption(BaseTable.ForeignKeys[J].Parent.TableName))) then
        begin
          ForeignKey := TWForeignKey.Create(Self, Point(-1, -1));
          ForeignKey.Caption := BaseTable.ForeignKeys[J].Name;
          ForeignKey.ChildTable := TableByBaseTable(BaseTable);
          ForeignKey.ParentTable := TableByCaption(BaseTable.ForeignKeys[J].Parent.TableName);
          ForeignKeys.Add(ForeignKey);
        end;

    for I := 0 to Tables.Count - 1 do
      for J := 0 to Tables[I].BaseTable.ForeignKeys.Count - 1 do
        if ((Database.Tables.NameCmp(Tables[I].BaseTable.ForeignKeys[J].Parent.TableName, BaseTable.Name) = 0)
          and not Assigned(ForeignKeyByCaption(Tables[I].BaseTable.ForeignKeys[J].Name))) then
        begin
          ForeignKey := TWForeignKey.Create(Self, Point(-1, -1));
          ForeignKey.Caption := Tables[I].BaseTable.ForeignKeys[J].Name;
          ForeignKey.ChildTable := Tables[I];
          ForeignKey.ParentTable := TableByBaseTable(BaseTable);
          ForeignKeys.Add(ForeignKey);
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
  CreatedTable.BaseTable := nil;
  FDatabaseName := '';
  FOnChange := nil;
  FOnCursorMove := nil;
  FOnValidateControl := nil;
  FForeignKeys := TWForeignKeys.Create(Self);
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

  FClient := ADatabase.Client;
  FDatabase := ADatabase;
  FDatabaseName := ADatabase.Name;
end;

procedure TWWorkbench.CursorMove(const Coord: TPoint);
begin
  if Assigned(FOnCursorMove) then FOnCursorMove(Self, Coord.X, Coord.Y);
end;

destructor TWWorkbench.Destroy();
begin
  Clear();

  ForeignKeys.Free();
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
    else if (Result and (Selected is TWForeignKey)) then
      ForeignKeys.Delete(ForeignKeys.IndexOf(Selected))
    else if (Result and (Selected is TWSection)) then
      Sections.Delete(Sections.IndexOf(Selected));
  end
  else
    Result := inherited ExecuteAction(Action);
end;

function TWWorkbench.ForeignKeyByCaption(const Caption: string): TWForeignKey;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to ForeignKeys.Count - 1 do
    if (lstrcmpI(PChar(ForeignKeys[I].Caption), PChar(Caption)) = 0) then
      Result := ForeignKeys[I];
end;

function TWWorkbench.GetDatabase(): TCDatabase;
begin
  if (not Assigned(FDatabase)) then
    FDatabase := FClient.DatabaseByName(FDatabaseName);

  Result := FDatabase;
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

procedure TWWorkbench.InsertForeignKey(const X, Y: Integer);
var
  Table: TWTable;
begin
  Selected := nil;
  State := wsInsertForeignKey;

  Table := TableAtCoord(Point(HorzScrollBar.Position + X, VertScrollBar.Position + Y));
  if (Assigned(Table)) then
    Table.MouseDown(mbLeft, [], X - Table.Left, Y - Table.Top);
end;

procedure TWWorkbench.InsertSection(const X, Y: Integer);
var
  Section: TWSection;
begin
  Selected := nil;

  if ((X >= 0) or (Y >= 0)) then
  begin
    Section := TWSection.Create(Self, Point(HorzScrollBar.Position + X, VertScrollBar.Position + Y));
    Sections.Add(Section);
    Section.MouseDown(mbLeft, [], 0, 0);
  end
  else
    State := wsInsertSection;
end;

procedure TWWorkbench.KeyPress(var Key: Char);
begin
  if ((Key = Chr(VK_ESCAPE)) and Assigned(Lasso)) then
    Perform(CM_ENDLASSO, 0, 0)
  else if ((Key = Chr(VK_ESCAPE)) and (Selected is TWForeignKeyPoint) and not Assigned(TWForeignKeyPoint(Selected).ForeignKey.ParentTable)) then
    ForeignKeys.Delete(ForeignKeys.IndexOf(Selected))
  else if ((Key = Chr(VK_ESCAPE)) and (Selected is TWSection) and (TWSection(Selected).ResizeMode = rmCreate)) then
    Sections.Delete(Sections.IndexOf(Selected))
  else
    inherited;
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

  if (State = wsInsertSection) then
  begin
    Section := TWSection.Create(Self, Point(HorzScrollBar.Position + X, VertScrollBar.Position + Y));
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
      Lasso := TWLasso.Create(Self, HorzScrollBar.Position + X, VertScrollBar.Position + Y);
  end;
end;

procedure TWWorkbench.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (State = wsInsertForeignKey) then
    Cursor := crNo
  else if (State = wsInsertSection) then
    Cursor := crCross
  else
    Cursor := crDefault;

  inherited;

  CursorMove(Point(HorzScrollBar.Position + X, VertScrollBar.Position + Y));
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
  ForeignKeys.SaveToXML(XMLDocument.DocumentElement);
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

  if (ASelected is TWForeignKeyPoint) then
    FSelected := TWForeignKeyPoint(ASelected).ForeignKey
  else if ((ASelected is TWForeignKeyLine) and (TWForeignKeyLine(ASelected).PointA is TWForeignKeyPoint)) then
    FSelected := TWForeignKeyPoint(TWForeignKeyLine(ASelected).PointA).ForeignKey
  else if (not (ASelected is TWLasso)) then
    FSelected := ASelected;

  for I := 0 to ControlCount - 1 do
    if ((Controls[I] <> ASelected) and (Controls[I] is TWControl)) then
      TWControl(Controls[I]).Selected := False;

  if (Assigned(Selected)) then
    Selected.Selected := True;

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

function TWWorkbench.TableAtCoord(const Coord: TPoint): TWTable;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if ((Controls[I] is TWTable) and PtInRect(TWTable(Controls[I]).Area, Coord)) then
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

procedure TWWorkbench.TableCreated(const ATable: TCBaseTable; const APoint: TPoint);
begin
  CreatedTable.BaseTable := ATable;
  CreatedTable.Point := APoint;
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
        TEditDelete(Action).Enabled := (Selected is TWTable) or (Selected is TWForeignKey) and TWForeignKey(Selected).IsLine or (Selected is TWSection)
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
    Control.ApplyCoord()
  else
  begin
    Index := PendingUpdateControls.IndexOf(Control);

    if (Index < 0) then
      PendingUpdateControls.Add(Control)
    else
      PendingUpdateControls.Move(Index, PendingUpdateControls.Count - 1);
  end;
end;

procedure TWWorkbench.ValidateControl(const Sender: TComponent; const Control: TWControl);
var
  I: Integer;
begin
  if (Assigned(OnValidateControl)) then
  begin
    OnValidateControl(Self, Control);
    I := 0;
    while ((Control is TWForeignKey) and (TWForeignKey(Control).Caption = '')) do
      if (Assigned(ForeignKeyByCaption('__Line_' + IntToStr(I)))) then
        Inc(I)
      else
        TWForeignKey(Control).Caption := '__Line_' + IntToStr(I);
  end
  else if ((Control is TWForeignKey) and (TWForeignKey(Control).Caption = '')) then
    TWForeignKey(Control).Caption := InputBox('Name', 'How should the Foreign Key named?', 'ForeignKey_' + IntToStr(ForeignKeys.Count));
end;

end.

