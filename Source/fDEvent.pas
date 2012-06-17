unit fDEvent;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls,
  SynEdit, SynMemo,
  StdCtrls_Ext, ComCtrls_Ext, Forms_Ext,
  fBase, fClient, Vcl.ExtCtrls;

type
  TDEvent = class(TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FComment: TEdit;
    FCreated: TLabel;
    FDefiner: TLabel;
    FEnabled: TCheckBox;
    FEndDate: TDateTimePicker;
    FEndEnabled: TCheckBox;
    FEndTime: TDateTimePicker;
    FExecuteDate: TDateTimePicker;
    FExecuteTime: TDateTimePicker;
    FIntervalDay: TEdit;
    FIntervalHour: TEdit;
    FIntervalMinute: TEdit;
    FIntervalMonth: TEdit;
    FIntervalQuarter: TEdit;
    FIntervalSecond: TEdit;
    FIntervalWeek: TEdit;
    FIntervalYear: TEdit;
    FLComment: TLabel;
    FLCreated: TLabel;
    FLDefiner: TLabel;
    FLEnabled: TLabel;
    FLEndDateTime: TLabel;
    FLExecuteDateTime: TLabel;
    FLIntervalDate: TLabel;
    FLIntervalTime: TLabel;
    FLIntervalWeeks: TLabel;
    FLMultipleExecution: TLabel;
    FLName: TLabel;
    FLPreserve: TLabel;
    FLSingleExecution: TLabel;
    FLStartDateTime: TLabel;
    FLStatement: TLabel;
    FLUpdated: TLabel;
    FMultipleExecution: TRadioButton;
    FName: TEdit;
    FPreserve: TCheckBox;
    FSingleExecution: TRadioButton;
    FSource: TSynMemo;
    FStartDate: TDateTimePicker;
    FStartEnabled: TCheckBox;
    FStartTime: TDateTimePicker;
    FStatement: TSynMemo;
    FUDIntervalDay: TUpDown;
    FUDIntervalHour: TUpDown;
    FUDIntervalMinute: TUpDown;
    FUDIntervalMonth: TUpDown;
    FUDIntervalQuarter: TUpDown;
    FUDIntervalSecond: TUpDown;
    FUDIntervalWeek: TUpDown;
    FUDIntervalYear: TUpDown;
    FUpdated: TLabel;
    GBasics: TGroupBox_Ext;
    GDates: TGroupBox_Ext;
    GDefiner: TGroupBox_Ext;
    msCopy: TMenuItem;
    msCut: TMenuItem;
    msDelete: TMenuItem;
    MSource: TPopupMenu;
    msPaste: TMenuItem;
    msSelectAll: TMenuItem;
    msUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PageControl: TPageControl;
    PSQLWait: TPanel;
    TSBasics: TTabSheet;
    TSInformations: TTabSheet;
    TSSource: TTabSheet;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FEndEnabledClick(Sender: TObject);
    procedure FEndEnabledKeyPress(Sender: TObject; var Key: Char);
    procedure FExecutionClick(Sender: TObject);
    procedure FExecutionKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FStartEnabledClick(Sender: TObject);
    procedure FStartEnabledKeyPress(Sender: TObject; var Key: Char);
    procedure TSSourceShow(Sender: TObject);
  private
    procedure Built();
    procedure FormClientEvent(const Event: TCClient.TEvent);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Database: TCDatabase;
    Event: TCEvent;
    function Execute(): Boolean;
  end;

function DEvent(): TDEvent;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  SQLUtils,
  fPreferences;

var
  FEvent: TDEvent;

function DEvent(): TDEvent;
begin
  if (not Assigned(FEvent)) then
  begin
    Application.CreateForm(TDEvent, FEvent);
    FEvent.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FEvent;
end;

{ TDEvent *********************************************************************}

procedure TDEvent.Built();
var
  Day: Word;
  Hour: Word;
  Minute: Word;
  Month: Word;
  MSec: Word;
  Quarter: Word;
  Second: Word;
  Week: Word;
  Year: Word;
begin
  FName.Text := Event.Name;

  FSingleExecution.Checked := Event.EventType = etSingle;
  if (Event.Execute = 0) then FExecuteDate.Date := Now() else FExecuteDate.Date := Event.Execute;
  FExecuteTime.Time := Event.Execute;

  FMultipleExecution.Checked := Event.EventType = etMultiple;
  Database.Client.DecodeInterval(Event.IntervalValue, Event.IntervalType, Year, Month, Day, Quarter, Week, Hour, Minute, Second, MSec);
  FUDIntervalYear.Position := Year;
  FUDIntervalMonth.Position := Month;
  FUDIntervalDay.Position := Day;
  FUDIntervalQuarter.Position := Quarter;
  FUDIntervalQuarter.Position := Quarter;
  FUDIntervalWeek.Position := Week;
  FUDIntervalHour.Position := Hour;
  FUDIntervalMinute.Position := Minute;
  FUDIntervalSecond.Position := Second;
  if (Event.StartDateTime = 0) then FStartDate.Date := Now() else FStartDate.Date := Event.StartDateTime;
  FStartTime.Time := Event.StartDateTime;
  if (Event.EndDateTime = 0) then FEndDate.Date := Now() + 1 else FEndDate.Date := Event.EndDateTime;
  FEndTime.Time := Event.EndDateTime;

  FEnabled.Checked := Event.Enabled;
  FPreserve.Checked := Event.Preserve;
  FComment.Text := SQLUnwrapStmt(Event.Comment);
  FStatement.Lines.Text := Event.Stmt;

  FDefiner.Caption := Event.Definer;
  FCreated.Caption := SysUtils.DateTimeToStr(Event.Created, LocaleFormatSettings);
  FUpdated.Caption := SysUtils.DateTimeToStr(Event.Updated, LocaleFormatSettings);

  PageControl.Visible := True;
  PSQLWait.Visible := not PageControl.Visible;

  ActiveControl := FName;
end;

procedure TDEvent.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiEvent, Icon);

  PSQLWait.Caption := Preferences.LoadStr(882);

  TSBasics.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLSingleExecution.Caption := ReplaceStr(Preferences.LoadStr(174), '&', '') + ':';
  FSingleExecution.Caption := Preferences.LoadStr(815) + ' ...';
  FLExecuteDateTime.Caption := Preferences.LoadStr(520) + ':';
  FMultipleExecution.Caption := Preferences.LoadStr(816) + ' ...';
  FLMultipleExecution.Caption := ReplaceStr(Preferences.LoadStr(174), '&', '') + ':';
  FLIntervalDate.Caption := Preferences.LoadStr(822) + ' / ' + Preferences.LoadStr(823) + ' / ' + Preferences.LoadStr(824) + ':';
  FLIntervalWeeks.Caption := Preferences.LoadStr(825) + ' / ' + Preferences.LoadStr(826) + ':';
  FLIntervalTime.Caption := Preferences.LoadStr(827) + ' / ' + Preferences.LoadStr(828) + ' / ' + Preferences.LoadStr(829) + ':';
  FLStartDateTime.Caption := Preferences.LoadStr(817) + ':';
  FLEndDateTime.Caption := Preferences.LoadStr(818) + ':';
  FLEnabled.Caption := Preferences.LoadStr(812) + ':';
  FEnabled.Caption := Preferences.LoadStr(529);
  FLPreserve.Caption := Preferences.LoadStr(819) + ':';
  FPreserve.Caption := ReplaceStr(Preferences.LoadStr(28), '&', '');
  FLComment.Caption := Preferences.LoadStr(111) + ':';
  FLStatement.Caption := Preferences.LoadStr(794) + ':';

  FStatement.Font.Name := Preferences.SQLFontName;
  FStatement.Font.Style := Preferences.SQLFontStyle;
  FStatement.Font.Color := Preferences.SQLFontColor;
  FStatement.Font.Size := Preferences.SQLFontSize;
  FStatement.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FStatement.Gutter.Font.Color := clWindowText
  else
    FStatement.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FStatement.Gutter.Color := clBtnFace
  else
    FStatement.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FStatement.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;
  if (Preferences.Editor.AutoIndent) then
    FStatement.Options := FStatement.Options + [eoAutoIndent, eoSmartTabs]
  else
    FStatement.Options := FStatement.Options - [eoAutoIndent, eoSmartTabs];
  if (Preferences.Editor.TabToSpaces) then
    FStatement.Options := FStatement.Options + [eoTabsToSpaces]
  else
    FStatement.Options := FStatement.Options - [eoTabsToSpaces];
  FStatement.RightEdge := Preferences.Editor.RightEdge;
  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FStatement.ActiveLineColor := clNone
  else
    FStatement.ActiveLineColor := Preferences.Editor.CurrRowBGColor;

  TSInformations.Caption := Preferences.LoadStr(121);
  GDefiner.Caption := ReplaceStr(Preferences.LoadStr(561), '&', '');
  FLDefiner.Caption := ReplaceStr(Preferences.LoadStr(799), '&', '') + ':';
  GDates.Caption := Preferences.LoadStr(122);
  FLCreated.Caption := Preferences.LoadStr(118) + ':';
  FLUpdated.Caption := Preferences.LoadStr(119) + ':';

  TSSource.Caption := Preferences.LoadStr(198);
  FSource.Font.Name := Preferences.SQLFontName;
  FSource.Font.Style := Preferences.SQLFontStyle;
  FSource.Font.Color := Preferences.SQLFontColor;
  FSource.Font.Size := Preferences.SQLFontSize;
  FSource.Font.Charset := Preferences.SQLFontCharset;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDEvent.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDEvent.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDEvent.FBOkCheckEnabled(Sender: TObject);
var
  I: Integer;
  IntervalType: TMySQLIntervalType;
  Value: string;
begin
  FBOk.Enabled := (FName.Text <> '') and SQLSingleStmt(FStatement.Text);
  for I := 0 to Database.Events.Count - 1 do
    if (lstrcmpi(PChar(FName.Text), PChar(Database.Events[I].Name)) = 0) and not (not Assigned(Event) or (lstrcmpi(PChar(FName.Text), PChar(Event.Name)) = 0)) then
      FBOk.Enabled := False;

  if (FMultipleExecution.Checked) then
    FBOk.Enabled := FBOk.Enabled and Database.Client.EncodeInterval(FUDIntervalYear.Position, FUDIntervalMonth.Position, FUDIntervalDay.Position, FUDIntervalQuarter.Position, FUDIntervalWeek.Position, FUDIntervalHour.Position, FUDIntervalMinute.Position, FUDIntervalSecond.Position, 0, Value, IntervalType);

  TSSource.TabVisible := False;
end;

procedure TDEvent.FEndEnabledClick(Sender: TObject);
begin
  FEndDate.Enabled := FEndEnabled.Checked and FMultipleExecution.Checked;
  FEndTime.Enabled := FEndEnabled.Checked and FMultipleExecution.Checked;

  FBOkCheckEnabled(Sender);
end;

procedure TDEvent.FEndEnabledKeyPress(Sender: TObject; var Key: Char);
begin
  FEndEnabledClick(Sender);
end;

procedure TDEvent.FExecutionClick(Sender: TObject);
begin
  FExecuteDate.Enabled := FSingleExecution.Checked;
  FExecuteTime.Enabled := FSingleExecution.Checked;

  FIntervalYear.Enabled := FMultipleExecution.Checked;
  FIntervalMonth.Enabled := FMultipleExecution.Checked;
  FIntervalDay.Enabled := FMultipleExecution.Checked;
  FIntervalQuarter.Enabled := FMultipleExecution.Checked;
  FIntervalWeek.Enabled := FMultipleExecution.Checked;
  FIntervalHour.Enabled := FMultipleExecution.Checked;
  FIntervalMinute.Enabled := FMultipleExecution.Checked;
  FIntervalSecond.Enabled := FMultipleExecution.Checked;
  FStartEnabled.Enabled := FMultipleExecution.Checked; FStartEnabledClick(Sender);
  FEndEnabled.Enabled := FMultipleExecution.Checked; FEndEnabledClick(Sender);

  FBOkCheckEnabled(Sender);
end;

procedure TDEvent.FExecutionKeyPress(Sender: TObject;
  var Key: Char);
begin
  FExecutionClick(Sender);
end;

procedure TDEvent.FormClientEvent(const Event: TCClient.TEvent);
begin
  if ((Event.EventType = ceItemValid) and (Event.CItem = Self.Event)) then
    Built()
  else if ((Event.EventType in [ceItemCreated, ceItemAltered]) and (Event.CItem is TCEvent)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Client.ErrorCode <> 0)) then
  begin
    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end;
end;

procedure TDEvent.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  IntervalType: TMySQLIntervalType;
  IntervalValue: string;
  NewEvent: TCEvent;
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
  begin
    NewEvent := TCEvent.Create(Database.Events);
    if (Assigned(Event)) then
      NewEvent.Assign(Event);

    NewEvent.Name := Trim(FName.Text);
    if (FSingleExecution.Checked) then
      NewEvent.EventType := etSingle
    else if (FMultipleExecution.Checked) then
      NewEvent.EventType := etMultiple
    else
      NewEvent.EventType := etUnknown;

    if (NewEvent.EventType <> etSingle) then
      NewEvent.Execute := 0
    else
      NewEvent.Execute := Trunc(FExecuteDate.Date) + FExecuteTime.Time - Trunc(FExecuteTime.Time);

    if (NewEvent.EventType <> etMultiple) then
    begin
      NewEvent.StartDateTime := 0;
      NewEvent.EndDateTime := 0;
    end
    else
    begin
      Database.Client.EncodeInterval(FUDIntervalYear.Position, FUDIntervalMonth.Position, FUDIntervalDay.Position, FUDIntervalQuarter.Position, FUDIntervalWeek.Position, FUDIntervalHour.Position, FUDIntervalMinute.Position, FUDIntervalSecond.Position, 0, IntervalValue, IntervalType);
      NewEvent.IntervalValue := IntervalValue;
      NewEvent.IntervalType := IntervalType;
      if (not FStartEnabled.Checked) then
       NewEvent.StartDateTime := 0
      else
       NewEvent.StartDateTime := Trunc(FStartDate.Date) + FStartTime.Time - Trunc(FStartTime.Time);
      if (not FEndEnabled.Checked) then
       NewEvent.EndDateTime := 0
      else
       NewEvent.EndDateTime := Trunc(FEndDate.Date) + FEndTime.Time - Trunc(FEndTime.Time);
    end;
    NewEvent.Enabled := FEnabled.Checked;
    NewEvent.Preserve := FPreserve.Checked;
    if (not Assigned(Event) or (Trim(FComment.Text) <> SQLUnwrapStmt(NewEvent.Comment))) then
      NewEvent.Comment := Trim(FComment.Text);
    NewEvent.Stmt := FStatement.Lines.Text;

    if (not Assigned(Event)) then
      CanClose := Database.AddEvent(NewEvent)
    else
      CanClose := Database.UpdateEvent(Event, NewEvent);

    NewEvent.Free();

    PageControl.Visible := CanClose or not Database.Client.Asynchron;
    PSQLWait.Visible := not PageControl.Visible;
    if (PSQLWait.Visible) then
      ModalResult := mrNone;

    FBOk.Enabled := False;
  end;
end;

procedure TDEvent.FormCreate(Sender: TObject);
begin
  FStatement.Highlighter := MainHighlighter;
  FSource.Highlighter := MainHighlighter;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.View.Width >= Width) and (Preferences.View.Height >= Height)) then
  begin
    Width := Preferences.Event.Width;
    Height := Preferences.Event.Height;
  end;

  msUndo.Action := MainAction('aEUndo'); msCut.ShortCut := 0;
  msCut.Action := MainAction('aECut'); msCut.ShortCut := 0;
  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msPaste.Action := MainAction('aEPaste'); msPaste.ShortCut := 0;
  msDelete.Action := MainAction('aEDelete'); msDelete.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  PageControl.ActivePage := TSBasics;
end;

procedure TDEvent.FormHide(Sender: TObject);
begin
  Database.Client.UnRegisterEventProc(FormClientEvent);

  Preferences.Event.Width := Width;
  Preferences.Event.Height := Height;

  FSource.Lines.Clear();
  PageControl.ActivePage := TSBasics;
end;

procedure TDEvent.FormShow(Sender: TObject);
var
  EventName: string;
  I: Integer;
begin
  Database.Client.RegisterEventProc(FormClientEvent);

  if (not Assigned(Event)) then
    Caption := Preferences.LoadStr(820)
  else
    Caption := Preferences.LoadStr(842, Event.Name);

  if (not Assigned(Event)) then
    HelpContext := 1114
  else
    HelpContext := 1113;

  if (not Assigned(Event)) then
  begin
    EventName := Preferences.LoadStr(814);
    I := 2;
    while (Assigned(Database.EventByName(EventName))) do
    begin
      EventName := Preferences.LoadStr(814) + IntToStr(I);
      Inc(I);
    end;

    FName.Text := EventName;

    FSingleExecution.Checked := True;
    FExecuteDate.Date := Now() + 1; FExecuteTime.Time := 0;

    FUDIntervalYear.Position := 0;
    FUDIntervalMonth.Position := 0;
    FUDIntervalDay.Position := 0;
    FUDIntervalQuarter.Position := 0;
    FUDIntervalQuarter.Position := 0;
    FUDIntervalWeek.Position := 0;
    FUDIntervalHour.Position := 0;
    FUDIntervalMinute.Position := 0;
    FUDIntervalSecond.Position := 0;
    FStartDate.Date := Now() + 1; FStartTime.Time := 0;
    FEndDate.Date := Now() + 2; FEndTime.Time := 0;

    FEnabled.Checked := True;
    FPreserve.Checked := False;
    FComment.Text := '';
    FStatement.Lines.Text := 'SET @A = 1;';

    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end
  else
  begin
    PageControl.Visible := Event.Update();
    PSQLWait.Visible := not PageControl.Visible;

    if (PageControl.Visible) then
      Built();
  end;

  FSource.Lines.Clear();

  TSInformations.TabVisible := Assigned(Event);
  TSSource.TabVisible := Assigned(Event);

  FBOk.Enabled := PageControl.Visible and not Assigned(Event);

  ActiveControl := FBCancel;
  if (PageControl.Visible) then
    ActiveControl := FName;
end;

procedure TDEvent.FStartEnabledClick(Sender: TObject);
begin
  FStartDate.Enabled := FStartEnabled.Checked and FMultipleExecution.Checked;
  FStartTime.Enabled := FStartEnabled.Checked and FMultipleExecution.Checked;

  FBOkCheckEnabled(Sender);
end;

procedure TDEvent.FStartEnabledKeyPress(Sender: TObject; var Key: Char);
begin
  FStartEnabledClick(Sender);
end;

procedure TDEvent.TSSourceShow(Sender: TObject);
begin
  if (FSource.Lines.Count = 0) then
    FSource.Lines.Text := Event.Source + #13#10
end;

initialization
  FEvent := nil;
end.

