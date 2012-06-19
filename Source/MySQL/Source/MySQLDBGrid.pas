unit MySQLDBGrid;

interface {********************************************************************}

uses
  Windows, Classes, Controls, Types, Grids, Messages, DB, Graphics, DBGrids,
  StdActns, DBCtrls,
  StdActns_Ext;

type
  TMySQLDBGrid = class(TDBGrid)
  type
    TSearchNotFoundEvent = procedure(Sender: TObject; FindText: string) of object;
    TFilterChange = procedure(Sender: TObject; Index: Integer) of object;
    TDBMySQLInplaceEdit = class(TInplaceEdit) // ToDo: TInplaceListEdit
    protected
//      procedure CloseUp(Accept: Boolean); override;
//      procedure DoEditButtonClick(); override;
//      procedure DropDown(); override;
      procedure KeyPress(var Key: Char); override;
    public
      property Font;
      constructor Create(Owner: TComponent); override;
    end;
  const
    tiShowHint = 1;
    tiHideHint = 2;
  private
    FIgnoreKeyPress: Boolean;
    FindNext: Boolean;
    FHeader: HWND;
    FHintWindow: THintWindow;
    FKeyDownShiftState: TShiftState;
    FListView: HWND;
    FMouseDownShiftState: TShiftState;
    FMouseDownPoint: TPoint;
    FMouseMoveCell: TGridCoord;
    FOnCanEditShow: TNotifyEvent;
    FOnCanEditShowExecuted: Boolean;
    FOnFilterChange: TFilterChange;
    FOnSelect: TNotifyEvent;
    FSearchNotFound: TSearchNotFoundEvent;
    IgnoreTitleClick: Boolean;
    IgnoreTitleChange: Boolean;
    SearchFindDialogOnCloseBeforeSearch: TNotifyEvent;
    SearchFindDialogOnFindBeforeSearch: TNotifyEvent;
    SearchFindDialogOnShowBeforeSearch: TNotifyEvent;
    procedure ActivateHint();
    function CanvasTextWidth(const Text: string): Integer;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    function EditCopyExecute(): Boolean;
    function EditCutExecute(): Boolean;
    function EditDeleteExecute(): Boolean;
    procedure FindDialogClose(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    function GetCurrentRow(): Boolean;
    function GetHeader(): HWND;
    procedure SearchFindExecute(const Action: TSearchFind);
    procedure SetHeaderColumnArrows();
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    property IgnoreKeyPress: Boolean read FIgnoreKeyPress;
    function CanEditShow(): Boolean; override;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; override;
    procedure ColEnter(); override;
    function CreateEditor(): TInplaceEdit; override;
    procedure CreateWnd(); override;
    procedure DblClick(); override;
    procedure DoEnter(); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function GetEditLimit(): Integer; override;
    function GetSelText(): string; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Resize(); override;
    procedure SetColumnAttributes(); override;
    procedure TitleClick(Column: TColumn); override;
    procedure TopLeftChanged(); override;
  public
    property CurrentRow: Boolean read GetCurrentRow;
    property Header: HWND read GetHeader;
    property KeyDownShiftState: TShiftState read FKeyDownShiftState;
    property MouseDownShiftState: TShiftState read FMouseDownShiftState;
    property SelText: string read GetSelText;
    procedure CopyToClipboard(); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LayoutChanged(); override;
    function PasteFromClipboard(): Boolean; virtual;
    procedure SelectAll(); virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UpdateHeader(); virtual;
    property DefaultRowHeight;
    property GridLineWidth;
    property InplaceEditor;
    property LeftCol;
    property Row;
    property RowCount;
    property TopRow;
  published
    property OnCanEditShow: TNotifyEvent read FOnCanEditShow write FOnCanEditShow;
    property OnFilterChange: TFilterChange read FOnFilterChange write FOnFilterChange;
    property OnSearchNotFound: TSearchNotFoundEvent
      read FSearchNotFound write FSearchNotFound;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation {***************************************************************}

uses
  Forms, SysUtils, Clipbrd, Dialogs, Consts, CommCtrl,
  DBActns, StrUtils, Math, Variants,
  MySQLDB, CSVUtils;

{ TDBMySQLGrid.TDBMySQLInplaceEdit ********************************************}

//procedure TMySQLDBGrid.TDBMySQLInplaceEdit.CloseUp(Accept: Boolean);
//begin
//  inherited;
//
//  if (Accept and Modified) then
//    TMySQLDBGrid(Grid).DataLink.Modified();
//end;

constructor TMySQLDBGrid.TDBMySQLInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
end;

//procedure TMySQLDBGrid.TDBMySQLInplaceEdit.DoEditButtonClick();
//begin
//  inherited;
//
//  TMySQLDBGrid(Grid).EditButtonClick();
//end;

//procedure TMySQLDBGrid.TDBMySQLInplaceEdit.DropDown();
//var
//  Column: TColumn;
//begin
//  if (not ListVisible) then
//  begin
//    Column := TMySQLDBGrid(Grid).Columns[TMySQLDBGrid(Grid).SelectedIndex];
//    if (ActiveList = PickList) then
//    begin
//      PickList.Items.Assign(Column.PickList);
//      DropDownRows := Column.DropDownRows;
//    end;
//  end;
//
//  inherited;
//end;

procedure TMySQLDBGrid.TDBMySQLInplaceEdit.KeyPress(var Key: Char);
begin
  if (TMySQLDBGrid(Grid).IgnoreKeyPress) then
    Key := #0
  else
    inherited;
end;

{ TMySQLDBGrid ****************************************************************}

procedure TMySQLDBGrid.ActivateHint();
var
  I: Integer;
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
  OldActiveRecord: Integer;
  Rect: TRect;
  StringList: TStringList;
begin
  if ((0 <= FMouseMoveCell.X) and (FMouseMoveCell.X < FieldCount) and not (Columns[FMouseMoveCell.X].Field.DataType in BinaryDataTypes) and not EditorMode) then
  begin
    if (not Assigned(FHintWindow)) then
    begin
      FHintWindow := THintWindow.Create(Self);
      FHintWindow.Color := clInfoBk;
    end;

    OldActiveRecord := DataLink.ActiveRecord;
    DataLink.ActiveRecord := FMouseMoveCell.Y - 1;

    StringList := TStringList.Create();
    if (FMouseMoveCell.Y = 0) then
      StringList.Text := Columns[FMouseMoveCell.X].Field.DisplayName
    else if (Columns[FMouseMoveCell.X].Field.IsNull) then
      StringList.Text := ''
    else if (Columns[FMouseMoveCell.X].Field.DataType = ftWideMemo) then
      StringList.Text := Columns[FMouseMoveCell.X].Field.AsString
    else
      StringList.Text := Columns[FMouseMoveCell.X].Field.DisplayText;

    if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)
      and (GetObject(Columns[FMouseMoveCell.X].Font.Handle, SizeOf(LogFont), @LogFont) <> 0)) then
    begin
      LogFont.lfQuality  := NonClientMetrics.lfMessageFont.lfQuality;
      FHintWindow.Canvas.Font.Handle := CreateFontIndirect(LogFont);
    end
    else
      FHintWindow.Canvas.Font := Columns[FMouseMoveCell.X].Font;

    Rect.Left := 0;
    for I := LeftCol to FMouseMoveCell.X - 1 do
      if (Columns[I].Visible) then
        if (dgRowLines in Options) then
          Inc(Rect.Left, Columns[I].Width + GridLineWidth)
        else
          Inc(Rect.Left, Columns[I].Width);
    Rect.Top := 0;
    for I := 0 to FMouseMoveCell.Y - 1 do
      if ((I > 0) and (dgRowLines in Options)) then
        Inc(Rect.Top, RowHeights[I] + GridLineWidth)
      else
        Inc(Rect.Top, RowHeights[I]);

    Rect.Left := ClientToScreen(Point(Rect.Left, Rect.Top)).X - 1;
    Rect.Top := ClientToScreen(Point(Rect.Left, Rect.Top)).Y;

    Rect.Right := 0;
    for I := 0 to StringList.Count - 1 do
      Rect.Right := Max(Rect.Right, Rect.Left + FHintWindow.Canvas.TextWidth(StringList[I]) + 6);
    Rect.Bottom := Rect.Top + FHintWindow.Canvas.TextHeight('H') * StringList.Count + 2;

    if ((Rect.Right - Rect.Left - 2 > Columns[FMouseMoveCell.X].Width)
      or (Columns[FMouseMoveCell.X].Field.DataType = ftWideMemo)
      or (StringList.Count > 1)) then
    begin
      FHintWindow.ActivateHint(Rect, StringList.Text);
      SetTimer(Handle, tiHideHint, Application.HintHidePause, nil);
    end
    else
      FreeAndNil(FHintWindow);

    StringList.Free();
    DataLink.ActiveRecord := OldActiveRecord;
  end;
end;

function TMySQLDBGrid.CanEditShow(): Boolean;
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  Result := inherited CanEditShow();

  if (Result and not FOnCanEditShowExecuted and Assigned(FOnCanEditShow)) then
  begin
    FOnCanEditShowExecuted := True;
    FOnCanEditShow(Self);
  end;
end;

function TMySQLDBGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := not ((Key in [VK_INSERT]))
      and not ((SelectedField = Columns[0].Field) and (Key in [VK_TAB]) and (ssShift in Shift))
      and not ((SelectedField = Columns[Columns.Count - 1].Field) and (Key in [VK_TAB]) and not (ssShift in Shift));
end;

function TMySQLDBGrid.CanvasTextWidth(const Text: string): Integer;
begin
  Result := Canvas.TextWidth(Text);
end;

procedure TMySQLDBGrid.CMFontChanged(var Message);
begin
  inherited;

  if (FListView > 0) then
    SendMessage(FListView, WM_SETFONT, WPARAM(Font.Handle), LPARAM(TRUE));
  if (FHeader > 0) then
    SendMessage(FHeader, WM_SETFONT, WPARAM(Font.Handle), LPARAM(TRUE));
  Resize();
end;

procedure TMySQLDBGrid.CopyToClipboard();
var
  ClipboardData: HGLOBAL;
  FormatSettings: TFormatSettings;
  I: Integer;
  J: Integer;
  Len: Cardinal;
  OldRecNo: Integer;
  S: string;
begin
  FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);

  if (OpenClipboard(Handle)) then
  begin
    EmptyClipboard();

    if (SelectedRows.Count = 0) then
    begin
      S := SelectedField.AsString;

      Len := Length(S);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1) * SizeOf(S[1]));
      Move(PChar(S)^, GlobalLock(ClipboardData)^, (Len + 1) * SizeOf(S[1]));
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);
    end
    else if (DataLink.DataSet is TMySQLDataSet) then
    begin
      S := SelText;

      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Length(S) + 1) * SizeOf(Char));
      Move(PChar(S)^, GlobalLock(ClipboardData)^, (Length(S) + 1) * SizeOf(Char));
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);


      DataLink.DataSet.DisableControls();
      OldRecNo := DataLink.DataSet.RecNo;


      S := '';
      for I := 0 to SelectedRows.Count - 1 do
      begin
        DataLink.DataSet.Bookmark := SelectedRows.Items[I];
        for J := 0 to Columns.Count - 1 do
          if (Columns[J].Visible) then
          begin
            if (S <> '') then S := S + FormatSettings.ListSeparator;

            if (Columns[J].Field.IsNull) then
              S := S + ''
            else if (Columns[J].Field.DataType in UnquotedDataTypes) then
              S := S + Columns[J].Field.AsString
            else
              S := S + CSVEscape(Columns[J].Field.AsString);
          end;
        S := S + #13#10;
      end;

      Len := WideCharToMultiByte(GetACP(), 0, PChar(S), Length(S), nil, 0, nil, nil);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1));
      WideCharToMultiByte(GetACP(), 0, PChar(S), Length(S), GlobalLock(ClipboardData), Len, nil, nil);
      PAnsiChar(GlobalLock(ClipboardData))[Len] := #0;
      SetClipboardData(49581, ClipboardData);
      GlobalUnlock(ClipboardData);


      S := '';
      for I := 0 to SelectedRows.Count - 1 do
      begin
        DataLink.DataSet.Bookmark := SelectedRows.Items[I];
        for J := 0 to Columns.Count - 1 do
          if (Columns[J].Visible) then
          begin
            if (S <> '') then S := S + #9;
            if (Columns[J].Field.DataType in UnquotedDataTypes) then
              S := S + Columns[J].Field.DisplayText
            else
              S := S + CSVEscape(Columns[J].Field.DisplayText);
          end;
        S := S + #13#10;
      end;

      Len := WideCharToMultiByte(GetACP(), 0, PChar(S), Length(S), nil, 0, nil, nil);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1));
      WideCharToMultiByte(GetACP(), 0, PChar(S), Length(S), GlobalLock(ClipboardData), Len, nil, nil);
      PAnsiChar(GlobalLock(ClipboardData))[Len] := #0;
      SetClipboardData(CF_DSPTEXT, ClipboardData);
      GlobalUnlock(ClipboardData);

      DataLink.DataSet.RecNo := OldRecNo;
      DataLink.DataSet.EnableControls();
    end;

    CloseClipboard();
  end;
end;

procedure TMySQLDBGrid.ColEnter();
begin
  if (Assigned(InplaceEditor)) then
  begin
    if (InplaceEditor is TDBMySQLInplaceEdit) then
      TDBMySQLInplaceEdit(InplaceEditor).Font := Columns[SelectedIndex].Font;
    if (Columns[SelectedIndex].Alignment <> taRightJustify) then
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) and not ES_RIGHT)
    else
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) or ES_RIGHT);
    if (Columns[SelectedIndex].Field.CanModify) then
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) and not ES_READONLY)
    else
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) or ES_READONLY);
  end;

  inherited;
end;

constructor TMySQLDBGrid.Create(AOwner: TComponent);
begin
  FIgnoreKeyPress := False;
  FHeader := 0;
  FListView := 0;
  IgnoreTitleClick := False;
  IgnoreTitleChange := False;
  FOnCanEditShowExecuted := False;

  FMouseMoveCell.X := -1;
  FMouseMoveCell.Y := -1;

  inherited;
end;

function TMySQLDBGrid.CreateEditor(): TInplaceEdit;
begin
  Result := TDBMySQLInplaceEdit.Create(Self);

  if (Assigned(Result)) then
  begin
    Result.Parent := Self;
    TDBMySQLInplaceEdit(Result).Font := Columns[SelectedIndex].Font;
    if (Columns[SelectedIndex].Alignment <> taRightJustify) then
      SetWindowLong(Result.Handle, GWL_STYLE, GetWindowLong(Result.Handle, GWL_STYLE) and not ES_RIGHT)
    else
      SetWindowLong(Result.Handle, GWL_STYLE, GetWindowLong(Result.Handle, GWL_STYLE) or ES_RIGHT);
  end;
end;

procedure TMySQLDBGrid.CreateWnd();
var
  LVColumn: TLVColumn;
  Style: DWORD;
begin
  inherited;

  if (FListView > 0) then CloseHandle(FListView);
  FListView := CreateWindow(WC_LISTVIEW, nil, WS_CHILD, 0, 0, 50, 50, Handle, 0, hInstance, nil);
  SendMessage(FListView, WM_SETFONT, WPARAM(Font.Handle), LPARAM(TRUE));
  LVColumn.mask := LVCF_TEXT;
  LVColumn.pszText := 'Test';
  LVColumn.cchTextMax := StrLen(LVColumn.pszText);
  SendMessage(FListView, LVM_INSERTCOLUMN, 0, LPARAM(@LVColumn));
  SendMessage(FHeader, LVM_SETUNICODEFORMAT, WPARAM(TRUE), 0);

  if (FHeader > 0) then CloseHandle(FHeader);
  Style := WS_CHILD or HDS_BUTTONS or HDS_FULLDRAG or HDS_DRAGDROP;
  if (not (dgColumnResize in Options) and CheckWin32Version(6)) then
    Style := Style or HDS_NOSIZING;
  FHeader := CreateWindow(WC_HEADER, nil, Style, 0, 0, ClientWidth, RowHeights[0], Handle, 0, hInstance, nil);
  SendMessage(FHeader, WM_SETFONT, WPARAM(Font.Handle), LPARAM(TRUE));
  SendMessage(FHeader, HDM_SETUNICODEFORMAT, WPARAM(TRUE), 0);
  SetColumnAttributes();
  Resize();
end;

procedure TMySQLDBGrid.DblClick();
var
  Coord: TGridCoord;
  DataCol: Integer;
begin
  Coord := MouseCoord(FMouseDownPoint.X - 3, FMouseDownPoint.Y);
  if (dgIndicator in Options) then
    DataCol := Coord.X - 1
  else
    DataCol := Coord.X;
  IgnoreTitleClick := Coord.Y = 0;

  if (not IgnoreTitleClick) then
    inherited
  else
  begin
    if ((DataLink.DataSet is TMySQLDataSet) and (DataCol >= 0) and not (Columns.Items[DataCol].Field.DataType in [ftWideMemo, ftBlob])) then
    begin
      Canvas.Font := Columns.Items[DataCol].Font;
      Columns.Items[DataCol].Width := TMySQLDataSet(DataLink.DataSet).GetMaxTextWidth(Columns.Items[DataCol].Field, CanvasTextWidth) + 5;
    end;
  end;
end;

destructor TMySQLDBGrid.Destroy();
begin
//  if (Assigned(FHintWindow)) then
//    FHintWindow.Free();

  inherited;
end;

procedure TMySQLDBGrid.DoEnter();
begin
  inherited;

  if (Assigned(InplaceEditor)) then
    if (Columns[SelectedIndex].Alignment <> taRightJustify) then
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) and not ES_RIGHT)
    else
      SetWindowLong(InplaceEditor.Handle, GWL_STYLE, GetWindowLong(InplaceEditor.Handle, GWL_STYLE) or ES_RIGHT);
end;

function TMySQLDBGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result :=  Assigned(DataSource) and Assigned(DataLink.DataSet) and DataLink.DataSet.Active and (not (ssShift in FMouseDownShiftState) and not (ssCtrl in FMouseDownShiftState));

  if (Result) then
    DataLink.DataSet.MoveBy(- WheelDelta div WHEEL_DELTA);
end;

procedure TMySQLDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if ((ARow > 0) or (FHeader = 0)) then // The header row has been replaced with the FHeader
    inherited;
end;

function TMySQLDBGrid.EditCopyExecute(): Boolean;
begin
  Result := True;

  if (EditorMode and Assigned(InplaceEditor)) then
    InplaceEditor.CopyToClipboard()
  else
    CopyToClipboard();
end;

function TMySQLDBGrid.EditCutExecute(): Boolean;
begin
  Result := CanEditModify();

  if (Result) then
  begin
    if (EditorMode and Assigned(InplaceEditor)) then
      InplaceEditor.CopyToClipboard()
    else
      CopyToClipboard();

    Result := EditDeleteExecute();
  end;
end;

function TMySQLDBGrid.EditDeleteExecute(): Boolean;
begin
  DataLink.DataSet.Edit();
  if (EditorMode and Assigned(InplaceEditor)) then
    InplaceEditor.SelText := ''
  else
    SelectedField.Clear();

  Result := True;
end;

function TMySQLDBGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditCut) then
    Result := EditCutExecute()
  else if (Action is TEditCopy) then
    Result := EditCopyExecute()
  else if (Action is TEditPaste) then
    Result := PasteFromClipboard()
  else if (Action is TEditDelete) then
    Result := EditDeleteExecute()
  else if (Action is TEditSelectAll) then
    begin Result := True; SelectAll(); end
  else if ((Action is TSearchFindFirst) or (Action is TSearchFind_Ext)) then
  begin
    SearchFindExecute(TSearchFind_Ext(Action));
    Result := True;
  end
  else if (Action is TSearchFindNext) then
  begin
    if (Assigned(TSearchFindNext(Action).SearchFind) and Assigned(TSearchFindNext(Action).SearchFind.Dialog) and Assigned(TSearchFindNext(Action).SearchFind.Dialog)) then
      FindDialogFind(TSearchFindNext(Action).SearchFind.Dialog);
    Result := True;
  end
  else if ((Action is TDataSetLast) and (DataLink.DataSet is TMySQLTable) and TMySQLTable(DataLink.DataSet).LimitedDataReceived and TMySQLTable(DataLink.DataSet).AutomaticLoadNextRecords and not DataLink.DataSet.Filtered and (DataLink.DataSet.State = dsBrowse)) then
  begin
    TMySQLTable(DataLink.DataSet).LoadNextRecords(True);
    Result := inherited ExecuteAction(Action);
  end
  else
    Result := inherited ExecuteAction(Action);
end;

procedure TMySQLDBGrid.FindDialogClose(Sender: TObject);
begin
  TFindDialog(Sender).OnClose := SearchFindDialogOnCloseBeforeSearch;
  TFindDialog(Sender).OnFind := SearchFindDialogOnFindBeforeSearch;
  TFindDialog(Sender).OnShow := SearchFindDialogOnShowBeforeSearch;
end;

procedure TMySQLDBGrid.FindDialogFind(Sender: TObject);
var
  LocateOptions: TLocateOptions;
begin
  LocateOptions := [loPartialKey];
  if (not (frMatchCase in TFindDialog(Sender).Options)) then
    LocateOptions := LocateOptions + [loCaseInsensitive];
  TMySQLDataSet(DataLink.DataSet).LocateNext := FindNext;
  if (TMySQLDataSet(DataLink.DataSet).Locate(SelectedField.FieldName, TFindDialog(Sender).FindText, LocateOptions)) then
    FindNext := True
  else
    if (Assigned(FSearchNotFound)) then
      FSearchNotFound(Sender, TFindDialog(Sender).FindText)
    else
      ShowMessage(Format(STextNotFound, [TFindDialog(Sender).FindText]));
end;

function TMySQLDBGrid.GetCurrentRow(): Boolean;
begin
  Result := Row - 1 = DataLink.ActiveRecord;
end;

function TMySQLDBGrid.GetEditLimit(): Integer;
begin
  if (not Assigned(SelectedField)) then
    Result := 0
  else
    case (SelectedField.DataType) of
      ftString: Result := SelectedField.DataSize;
      ftBytes,
      ftVarBytes:  Result := SelectedField.Size - 1;
      ftSmallInt,
      ftInteger:
        { Workaround, because TIntegerField.MinValue is sometimes not set }
        if ((TIntegerField(SelectedField).MinValue = 0) and (TIntegerField(SelectedField).MaxValue = $7FFFFFFF)) then
          Result := Length('-2147483648')
        else if ((TIntegerField(SelectedField).MinValue = 0) and (TIntegerField(SelectedField).MaxValue = 0)) then
          Result := 0
        else
          Result := Max(Length(IntToStr(TIntegerField(SelectedField).MinValue)), Length(IntToStr(TIntegerField(SelectedField).MaxValue)));
      ftWord: Result :=  max(Length(IntToStr(TWordField(SelectedField).MinValue)), Length(IntToStr(TWordField(SelectedField).MaxValue)));
      ftCurrency,
      ftFloat:
        if ((TFloatField(SelectedField).MinValue = 0) and (TFloatField(SelectedField).MaxValue = 0)) then
          Result := 0
        else
          Result := max(Length(FloatToStr(TFloatField(SelectedField).MinValue, TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings)), Length(FloatToStr(TFloatField(SelectedField).MaxValue, TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings)));
      ftDate: Result := Length(MySQLDB.DateToStr(Now(), TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings));
      ftTime: Result := Length(TimeToStr(Now(), TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings));
      ftDateTime: Result := Length(MySQLDB.DateTimeToStr(Now(), TMySQLDataSet(DataLink.DataSet).Connection.FormatSettings));
      ftBlob,
      ftWideMemo: Result := 0;
      ftLargeint:
        if ((TLargeintField(SelectedField).MinValue = 0) and (TLargeintField(SelectedField).MaxValue = 0)) then
          Result := 0
        else
          Result := Max(Length(IntToStr(TLargeintField(SelectedField).MinValue)), Length(IntToStr(TLargeintField(SelectedField).MaxValue)));
      ftTimeStamp: Result := SelectedField.Size;
      else Result := inherited GetEditLimit();
    end;
end;

function TMySQLDBGrid.GetHeader(): HWND;
begin
  if (FHeader = 0) then
    HandleNeeded();

  Result := FHeader;
end;

function TMySQLDBGrid.GetSelText(): string;
var
  FormatSettings: TFormatSettings;
  I: Integer;
  J: Integer;
  OldRecNo: Integer;
begin
  FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);

  if (SelectedRows.Count = 0) then
    Result := SelectedField.AsString
  else
  begin
    DataLink.DataSet.DisableControls();
    OldRecNo := DataLink.DataSet.RecNo;

    for I := 0 to SelectedRows.Count - 1 do
    begin
      DataLink.DataSet.Bookmark := SelectedRows.Items[I];
      for J := 0 to Columns.Count - 1 do
        if (Columns[J].Visible and not (Columns[J].Field.DataType = ftBlob)) then
        begin
          if (J > 0) then Result := Result + #9;
          if (Columns[J].Field.DataType in UnquotedDataTypes) then
            Result := Result + Columns[J].Field.DisplayText
          else
            Result := Result + CSVEscape(Columns[J].Field.DisplayText);
        end;
      Result := Result + #13#10;
    end;

    DataLink.DataSet.RecNo := OldRecNo;
    DataLink.DataSet.EnableControls();
  end;
end;

procedure TMySQLDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssShift, ssCtrl, ssAlt]);

  if ((Key = VK_UP) and (Shift = [ssCtrl]) and Assigned(DataLink.DataSet) and (DataLink.DataSet.State = dsBrowse)) then
  begin
    if (SelectedRows.Count = 0) then
      SelectedRows.CurrentRowSelected := True;
    if DataLink.DataSet.RecNo > 0 then
      DataLink.DataSet.MoveBy(-1);
    FIgnoreKeyPress := True;
  end
  else if ((Key = VK_DOWN) and (Shift = [ssCtrl]) and Assigned(DataLink.DataSet) and (DataLink.DataSet.State = dsBrowse)) then
  begin
    if (SelectedRows.Count = 0) then
      SelectedRows.CurrentRowSelected := True;
    if DataLink.DataSet.RecNo < DataLink.DataSet.RecordCount - 1 then
      DataLink.DataSet.MoveBy(+1);
    FIgnoreKeyPress := True;
  end
  else if (((Key = VK_HOME) or (Key = VK_END) or (Key = VK_LEFT) or (Key = VK_RIGHT)) and (Shift = [ssShift]) and not EditorMode) then
    SelectedRows.CurrentRowSelected := not SelectedRows.CurrentRowSelected
  else if ((Key = VK_END) and (ssCtrl in Shift) and Assigned(DataLink.DataSet) and (DataLink.DataSet is TMySQLTable) and TMySQLTable(DataLink.DataSet).LimitedDataReceived and TMySQLTable(DataLink.DataSet).AutomaticLoadNextRecords and not DataLink.DataSet.Filtered and (DataLink.DataSet.State = dsBrowse)) then
  begin
    TMySQLTable(DataLink.DataSet).LoadNextRecords(True);
    inherited;
  end
  else if ((Key = VK_SPACE) and (Shift = [ssCtrl]) and Assigned(DataLink.DataSet) and (DataLink.DataSet.State = dsBrowse)) then
  begin
    SelectedRows.CurrentRowSelected := not SelectedRows.CurrentRowSelected;
    FIgnoreKeyPress := True;
    Options := Options - [dgEditing];
  end
  else if ((Key = VK_ESCAPE) and (Shift = []) and EditorMode) then
  begin
    Datalink.Reset();
    if not (dgAlwaysShowEditor in Options) then
      HideEditor();
  end
  else if ((Key = Ord('A')) and (Shift = [ssCtrl])) then
    SelectAll()
  else if (((Key = Ord('X')) and (Shift = [ssCtrl]) or (Key = VK_DELETE) and (Shift = [ssShift])) and (SelectedRows.Count = 0)) then
    EditCutExecute()
  else if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
    EditCopyExecute()
  else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
    PasteFromClipboard()
  else if ((Key = VK_DELETE) and (Shift = [])) then
    EditDeleteExecute()
  else if ((Key = VK_DOWN) and (Shift = [ssAlt]) and (Columns[SelectedIndex].ButtonStyle = cbsEllipsis)) then
    EditButtonClick()
  else
    inherited;
end;

procedure TMySQLDBGrid.KeyPress(var Key: Char);
begin                                                                                
  if (IgnoreKeyPress) then
  begin
    Key := #0;
    FIgnoreKeyPress := False;
    Options := Options + [dgEditing];
  end;

  inherited;
end;

procedure TMySQLDBGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssShift, ssCtrl, ssAlt]);

  inherited;
end;

procedure TMySQLDBGrid.LayoutChanged();
begin
  inherited;

  FOnCanEditShowExecuted := False;
end;

procedure TMySQLDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  Coord: TGridCoord;
  I: Integer;
  NewBookmark: TBookmark;
  NewRecord: Integer;
  OldBookmark: TBookmark;
  OldRecord: Integer;
  OldSelectedRows: Integer;
begin
  if (Assigned(FHintWindow)) then
    FreeAndNil(FHintWindow);

  if (Assigned(DataSource) and Assigned(DataLink.DataSet) and DataLink.DataSet.Active) then
  begin
    FMouseDownPoint.X := X; FMouseDownPoint.Y := Y;
    FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssLeft, ssRight, ssMiddle, ssDouble]);

    Coord := MouseCoord(X, Y);
    if ((Coord.X = -1) and (Coord.Y = -1) and EditorMode) then
      try
        SelectedField.AsString := TDBMySQLInplaceEdit(InplaceEditor).Text;
      except
      end;

    if (Y <= RowHeights[0]) then
      OldRecord := -1
    else
      OldRecord := DataLink.ActiveRecord;
    OldBookmark := DataLink.DataSet.Bookmark;
    OldSelectedRows := SelectedRows.Count;

    inherited;

    if (Y <= RowHeights[0]) then
      NewRecord := -1
    else
      NewRecord := DataLink.ActiveRecord;

    Cell := MouseCoord(X, Y);
    if (((Cell.X > 0) or not (dgIndicator in Options)) and (Cell.Y > 0) and (Button = mbLeft)) then
    begin
      if (ssShift in Shift) then
      begin
        DataLink.DataSet.DisableControls();
        SelectedRows.CurrentRowSelected := True;
        if (NewRecord < OldRecord) then
          for I := OldRecord downto NewRecord do
          begin
            DataLink.ActiveRecord := I;
            SelectedRows.CurrentRowSelected := True;
          end;
        if (NewRecord > OldRecord) then
          for I := OldRecord to NewRecord do
          begin
            DataLink.ActiveRecord := I;
            SelectedRows.CurrentRowSelected := True;
          end;
        DataLink.DataSet.EnableControls();
      end
      else if (ssCtrl in Shift) then
      begin
        if (OldSelectedRows = 0) then
        begin
          DataLink.DataSet.DisableControls();
          NewBookmark := DataLink.DataSet.Bookmark;
          DataLink.DataSet.Bookmark := OldBookmark;
          SelectedRows.CurrentRowSelected := True;
          DataLink.DataSet.Bookmark := NewBookmark;
          DataLink.DataSet.EnableControls();
        end;
      end
      else
        SelectedRows.Clear();
    end;
  end;
end;

procedure TMySQLDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Cell: TGridCoord;
begin
  inherited;

  Cell := MouseCoord(X, Y);
  if (not ShowHint and not ParentShowHint or (Hint = '')) then
    if (((FMouseMoveCell.X >= 0) or (FMouseMoveCell.Y >= 1)) and ((Cell.X < 0) or (Cell.Y < 1))) then
    begin
      FMouseMoveCell.X := -1; FMouseMoveCell.Y := -1;
      ReleaseCapture();
      if (Assigned(FHintWindow)) then
        FreeAndNil(FHintWindow);
    end
    else if ((Cell.X >= 0) and (Cell.Y >= 1) and ((Cell.X <> FMouseMoveCell.X) or (Cell.Y <> FMouseMoveCell.Y))) then
    begin
      FMouseMoveCell := Cell;
      SetCapture(Handle);
      SetTimer(Handle, tiShowHint, Application.HintShortPause, nil);
    end;
end;

procedure TMySQLDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  FMouseDownShiftState := Shift + (FMouseDownShiftState - [ssShift, ssCtrl, ssAlt]);
end;

function TMySQLDBGrid.PasteFromClipboard(): Boolean;
var
  Bookmarks: array of TBookmark;
  ClipboardData: HGLOBAL;
  I: Integer;
  Index: Integer;
  RecNo: Integer;
  S: string;
  Values: TCSVValues;
begin
  Result := not ReadOnly;
  if (Result) then
    if (EditorMode and Assigned(InplaceEditor)) then
    begin
      InplaceEditor.PasteFromClipboard();
      Result := True;
    end
    else if ((DataLink.DataSet is TMySQLDataSet) and Clipboard.HasFormat(CF_UNICODETEXT) and OpenClipboard(Handle)) then
    begin
      ClipboardData := GetClipboardData(CF_UNICODETEXT);
      S := PChar(GlobalLock(ClipboardData)); // Make sure no error occurs before CloseClipboard
      GlobalUnlock(ClipboardData);
      CloseClipboard();

      Index := 1;
      if (CSVSplitValues(S, Index, #9, '"', Values) and ((Length(Values) > 1) or (Index <= Length(S)))) then
      begin
        if (DataLink.DataSet.State <> dsInsert) then
          DataLink.DataSet.CheckBrowseMode();
        DataLink.DataSet.DisableControls();
        try
          if (SelectedRows.Count > 0) then
          begin
            SetLength(Bookmarks, SelectedRows.Count);
            for I := 0 to Length(Bookmarks) - 1 do
              Bookmarks[I] := SelectedRows.Items[I];
            SelectedRows.Clear();
            TMySQLDataSet(DataLink.DataSet).Delete(Bookmarks);
            SetLength(Bookmarks, 0);
          end;

          RecNo := 0;
          try
            repeat
              if (Length(Values) > 0) then
              begin
                if ((DataLink.DataSet.State = dsInsert) or (RecNo > 0) and (DataLink.DataSet.RecNo = DataLink.DataSet.RecordCount - 1)) then
                  DataLink.DataSet.Append()
                else
                begin
                  if (RecNo > 1) then DataLink.DataSet.Next();
                  DataLink.DataSet.Insert();
                end;

                for I := 0 to Min(Length(Values), DataLink.DataSet.FieldCount) - 1 do
                  if ((Values[I].Length = 0) or (DataLink.DataSet.Fields[I].AutoGenerateValue = arAutoInc)) then
                    DataLink.DataSet.Fields[I].Clear()
                  else
                    DataLink.DataSet.Fields[I].AsString := CSVUnescape(Values[I].Data, Values[I].Length);

                if ((RecNo > 0) or (Index <= Length(S))) then
                  try
                    DataLink.DataSet.Post();
                  except
                    on Error: EDatabaseError do
                      begin
                        DataLink.DataSet.Cancel();
                        DataLink.DataSet.Prior();
                      end;
                  end;
                Inc(RecNo);
              end;
            until (not CSVSplitValues(S, Index, #9, '"', Values) or (Length(Values) = 0));
          finally
            SetLength(Values, 0);
          end;
        finally
          DataLink.DataSet.EnableControls();
        end;
      end
      else if (not SelectedField.ReadOnly) then
      begin
        ShowEditor();
        if (EditorMode and Assigned(InplaceEditor)) then
          InplaceEditor.PasteFromClipboard()
        else
        begin
          DataLink.DataSet.Edit();
          SelectedField.AsString := S;
        end;
      end;
    end
    else
      Result := False;
end;

procedure TMySQLDBGrid.Resize();
var
  HDLayout: THDLayout;
  HDRect: TRect;
  HDWindowPos: TWindowPos;
  Height: Integer;
  Rect: TRect;
begin
  inherited;

  if (Assigned(DataLink) and Assigned(DataSource) and Assigned(DataLink.DataSet)) then
    DataLink.BufferCount := RowCount - 1;

  if (GetWindowRect(FHeader, Rect)) then
  begin
    HDLayout.Rect := @HDRect;
    HDLayout.WindowPos := @HDWindowPos;
    if ((FListView > 0) and Header_Layout(ListView_GetHeader(FListView), @HDLayout)) then
      Height := HDWindowPos.cy
    else
      Height := Rect.Bottom - Rect.Top;
    SetWindowPos(FHeader, HWND_TOP, 0, 0, ClientWidth, Height, SWP_NOMOVE or SWP_SHOWWINDOW);

    if (EditorMode) then Perform(CM_Exit, 0, 0);
    if (GetWindowRect(FHeader, Rect)) then
      if (dgRowLines in Options) then
        RowHeights[0] := Height - GridLineWidth
      else
        RowHeights[0] := Height;
  end;
end;

procedure TMySQLDBGrid.SearchFindExecute(const Action: TSearchFind);
begin
  if (Assigned(Action.Dialog)) then
  begin
    FindNext := not (Action is TSearchFindFirst) and (Action.Dialog.FindText <> '');

    SearchFindDialogOnCloseBeforeSearch := Action.Dialog.OnClose;
    SearchFindDialogOnFindBeforeSearch := Action.Dialog.OnFind;
    SearchFindDialogOnShowBeforeSearch := Action.Dialog.OnShow;

    if (Assigned(Action.BeforeExecute)) then
      Action.BeforeExecute(Action);

    Action.Dialog.OnShow := nil;
    Action.Dialog.OnFind := FindDialogFind;
    Action.Dialog.OnClose := FindDialogClose;

    TSearchFind_Ext(Action).Dialog.Execute();
  end;
end;

procedure TMySQLDBGrid.SelectAll();
var
  OldActive: Boolean;
  OldCursor: TCursor;
  OldRecNo: Integer;
  OldVisible: Boolean;
begin
  DataLink.DataSet.CheckBrowseMode();

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  OldVisible := Visible;
  OldActive := Focused();
  Visible := False;
  OldRecNo := DataLink.DataSet.RecNo;

  DataLink.DataSet.DisableControls();
  SelectedRows.Clear();
  if (DataLink.DataSet.FindFirst()) then
  repeat
    SelectedRows.CurrentRowSelected := True;
  until (not DataLink.DataSet.FindNext());
  DataLink.DataSet.RecNo := OldRecNo;
  DataLink.DataSet.EnableControls();

  Screen.Cursor := OldCursor;
  Visible := OldVisible;
  if (Visible and OldActive) then
    SetFocus();
end;

procedure TMySQLDBGrid.SetColumnAttributes();
var
  HDItem: THDItem;
  I: Integer;
begin
  inherited;

  if (not IgnoreTitleChange) then
    for I := 0 to SendMessage(FHeader, HDM_GETITEMCOUNT, 0, 0) - 1 do
      SendMessage(FHeader, HDM_DELETEITEM, 0, 0);

  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
      TabStops[I + IndicatorOffset] := Showing and DataLink.Active and
        Assigned(Field) and not (Field.FieldKind = fkCalculated);

    if (not IgnoreTitleChange and (I >= LeftCol) and Assigned(Columns[I].Field)) then
    begin
      HDItem.Mask := HDI_FORMAT or HDI_TEXT or HDI_WIDTH;
      HDItem.fmt := HDF_STRING;
      if (dgColLines in Options) then
        HDItem.cxy := Columns[I].Width + GridLineWidth
      else
        HDItem.cxy := Columns[I].Width;
      HDItem.pszText := PChar(Columns[I].Field.DisplayName);
      HDItem.cchTextMax := Length(HDItem.pszText);

      SendMessage(FHeader, HDM_INSERTITEM, I - LeftCol, LPARAM(@HDItem));
    end;
  end;

  if (not Assigned(OnTitleClick) or not (dgTitleClick in Options)) then
    SetWindowLong(FHeader, GWL_STYLE, GetWindowLong(FHeader, GWL_STYLE) and not HDS_BUTTONS)
  else
    SetWindowLong(FHeader, GWL_STYLE, GetWindowLong(FHeader, GWL_STYLE) or HDS_BUTTONS);

  if (not IgnoreTitleChange) then
  begin
    SetHeaderColumnArrows();
    Resize();
  end;
end;

procedure TMySQLDBGrid.SetHeaderColumnArrows();
var
  HDItem: THDItem;
  I: Integer;
  Index: Integer;
begin
  Index := 0;
  HDItem.Mask := HDI_FORMAT;
  if (DataLink.DataSet is TMySQLDataSet) then
    for I := LeftCol to Columns.Count - 1 do
      if (Columns[I].Visible and BOOL(SendMessage(FHeader, HDM_GETITEM, Index, LParam(@HDItem)))) then
      begin
        if (Columns[I].Field.Tag and ftAscSortedField <> 0) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTUP
        else if (Columns[I].Field.Tag and ftDescSortedField <> 0) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTDOWN
        else
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN;
        SendMessage(FHeader, HDM_SETITEM, Index, LPARAM(@HDItem));
        Inc(Index);
      end;
end;

procedure TMySQLDBGrid.TitleClick(Column: TColumn);
begin
  if (not IgnoreTitleClick) then
  begin
    FMouseDownShiftState := [ssLeft];
    if (GetKeyState(VK_SHIFT) < 0) then FMouseDownShiftState := FMouseDownShiftState + [ssShift];
    if (GetKeyState(VK_CONTROL) < 0) then FMouseDownShiftState := FMouseDownShiftState + [ssCtrl];
    inherited;
  end;

  IgnoreTitleClick := False;
end;

procedure TMySQLDBGrid.TopLeftChanged();
begin
  inherited;

  SetColumnAttributes();
end;

function TMySQLDBGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditAction) then
  begin
    Result := Focused() and Assigned(DataLink.DataSet) and DataLink.DataSet.Active;

    if (Result) then
      if (Action is TEditCut) then
        TEditCut(Action).Enabled := Assigned(SelectedField) and not SelectedField.IsNull and not SelectedField.Required and SelectedField.CanModify and (not EditorMode or Assigned(InplaceEditor) and (InplaceEditor.SelText <> ''))
      else if (Action is TEditCopy) then
        TEditCopy(Action).Enabled := EditorMode and Assigned(InplaceEditor) and (InplaceEditor.SelText <> '') or not EditorMode and (not SelectedRows.CurrentRowSelected and Assigned(SelectedField) and not SelectedField.IsNull or SelectedRows.CurrentRowSelected and (DataLink.DataSet is TMySQLDataSet) and (DataLink.DataSet.State <> dsInsert))
      else if (Action is TEditPaste) then
        TEditPaste(Action).Enabled := Assigned(SelectedField) and not ReadOnly and SelectedField.CanModify and (EditorMode and Clipboard.HasFormat(CF_UNICODETEXT) or not EditorMode and Clipboard.HasFormat(CF_UNICODETEXT))
      else if (Action is TEditDelete) then
        TEditDelete(Action).Enabled := (SelectedRows.Count = 0) and Assigned(SelectedField) and not SelectedField.IsNull and not SelectedField.Required and SelectedField.CanModify and (not EditorMode or Assigned(InplaceEditor) and (InplaceEditor.SelText <> ''))
      else if (Action is TEditSelectAll) then
        TEditSelectAll(Action).Enabled := (DataLink.DataSet.RecordCount > 0)
      else
        Result := False;
  end
  else if ((Action is TSearchAction) or (Action is TSearchFindNext)) then
  begin
    Result := Focused();
    if (Action is TSearchFindFirst) then
      TSearchAction(Action).Enabled :=
        Result and Assigned(SelectedField) and not (SelectedField.DataType in [ftBlob, ftUnknown]) and (SelectedRows.Count <= 1)
    else if (Action is TSearchFind_Ext) then
      TSearchAction(Action).Enabled :=
        Result and Assigned(SelectedField) and not (SelectedField.DataType in [ftBlob, ftUnknown]) and (SelectedRows.Count <= 1)
    else if (Action is TSearchReplace) then
      TSearchReplace(Action).Enabled := False
    else if (Action is TSearchFindNext) then
      TSearchFindNext(Action).Enabled :=
        Result
          and (Assigned(TSearchFindNext(Action).SearchFind))
          and (TSearchFindNext(Action).SearchFind.Dialog.FindText <> '');
  end
  else
    Result := inherited UpdateAction(Action);
end;

procedure TMySQLDBGrid.UpdateHeader();
begin
  SetColumnAttributes();
end;

procedure TMySQLDBGrid.WMNotify(var Message: TWMNotify);
var
  Column: TColumn;
  HDItem: THDItem;
  HDNotify: PHDNotify;
begin
  HDNotify := PHDNotify(Message.NMHdr);
  if (HDNotify^.Hdr.hwndFrom <> FHeader) then
    inherited
  else
    case (HDNotify^.Hdr.code) of
      HDN_ITEMCLICK:
        TitleClick(Columns[HDNotify^.Item + LeftCol]);
      HDN_DIVIDERDBLCLICK:
        begin
          Column := Columns[HDNotify^.Item + LeftCol];
          if ((DataLink.DataSet is TMySQLDataSet) and not (Column.Field is TBlobField)) then
          begin
            HDItem.Mask := HDI_WIDTH;
            if (BOOL(SendMessage(FHeader, HDM_GETITEM, HDNotify^.Item, LPARAM(@HDItem)))) then
            begin
              IgnoreTitleChange := True;

              Canvas.Font := Column.Font;
              HDItem.cxy := TMySQLDataSet(DataLink.DataSet).GetMaxTextWidth(Column.Field, CanvasTextWidth) + 5;
              if (dgColLines in Options) then
                Inc(HDItem.cxy, GridLineWidth);
              while (not BOOL(SendMessage(FHeader, HDM_SETITEM, HDNotify^.Item, LPARAM(@HDItem)))) do
                Inc(HDItem.cxy, GridLineWidth);

              IgnoreTitleChange := False;

              Resize();
            end;
          end;
        end;
      HDN_ITEMCHANGING:
        Message.Result := LRESULT(not ((HDNotify^.PItem.Mask and HDI_WIDTH = 0) or (HDNotify^.PItem.cxy >= RowHeights[1])));
      HDN_ITEMCHANGED:
        begin
          Column := Columns[HDNotify^.Item + LeftCol];
          if (HDNotify^.PItem.Mask and HDI_WIDTH <> 0) then
          begin
            if (EditorMode) then Perform(CM_Exit, 0, 0);
            if (dgColLines in Options) then
              Column.Width := HDNotify^.PItem.cxy - GridLineWidth
            else
              Column.Width := HDNotify^.PItem.cxy;
            Resize();
          end;
        end;
      HDN_ENDDRAG:
        if ((HDNotify^.PItem.Mask and HDI_ORDER <> 0) and (HDNotify^.PItem.iOrder >= 0)) then
        begin
          Column := Columns[HDNotify^.Item + LeftCol];
          Column.Index := HDNotify^.PItem.iOrder + LeftCol;
          Message.Result := LRESULT(TRUE);
          Resize();
        end;
      else
        inherited;
    end;
end;

procedure TMySQLDBGrid.WMTimer(var Message: TWMTimer);
begin
  inherited;

  case (Message.TimerID) of
    tiShowHint:
      begin
        KillTimer(Handle, Message.TimerID);
        ActivateHint();
      end;
    tiHideHint:
      begin
        KillTimer(Handle, Message.TimerID);
        if (Assigned(FHintWindow)) then
          FreeAndNil(FHintWindow);
      end;
  end;
end;

end.

