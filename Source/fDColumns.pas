unit fDColumns;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  Forms_Ext,
  DBGrids,
  fBase, ToolWin, StdCtrls_Ext;

type
  TDColumns = class(TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FColumns: TListView;
    FLWidth: TLabel;
    FUDWidth: TUpDown;
    FWidth: TEdit;
    GroupBox: TGroupBox_Ext;
    tbDown: TToolButton;
    tbUp: TToolButton;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    procedure FBHelpClick(Sender: TObject);
    procedure FColumnsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FColumnsResize(Sender: TObject);
    procedure FColumnsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FWidthChange(Sender: TObject);
    procedure tbUpDownClick(Sender: TObject);
  private
    OldChecked: Boolean;
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure CMApplyAutosize(var Message: TMessage); message CM_APPLYAUTOSIZE;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  public
    DBGrid: TDBGrid;
    function Execute(): Boolean;
  end;

function DColumns(): TDColumns;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommCtrl, UxTheme,
  StrUtils,
  CommCtrl_Ext,
  fPreferences;

var
  FColumns: TDColumns;

function DColumns(): TDColumns;
begin
  if (not Assigned(FColumns)) then
  begin
    Application.CreateForm(TDColumns, FColumns);
    FColumns.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FColumns;
end;

{ TDColumns *******************************************************************}

procedure TDColumns.CMApplyAutosize(var Message: TMessage);
var
  HDItem: THDItem;
begin
  HDItem.Mask := HDI_WIDTH or HDI_FORMAT;
  if (BOOL(SendMessage(ListView_GetHeader(FColumns.Handle), HDM_GETITEM, 0, LParam(@HDItem)))) then
  begin
    HDItem.cxy := FColumns.ClientWidth;
    HDItem.fmt := HDItem.fmt or HDF_CHECKBOX;
    if (OldChecked) then
      HDItem.fmt := HDItem.fmt or HDF_CHECKED;

    SendMessage(ListView_GetHeader(FColumns.Handle), HDM_SETITEM, 0, LParam(@HDItem));
  end;

  if (ComCtl32MajorVersion >= 6) then
    SendMessage(FColumns.Handle, LVM_SETSELECTEDCOLUMN, 0, 0);
end;

procedure TDColumns.CMChangePreferences(var Message: TMessage);
begin
  Caption := ReplaceStr(Preferences.LoadStr(684), '&', '');

  GroupBox.Caption := Preferences.LoadStr(253);
  FColumns.Columns[0].Caption := ReplaceStr(Preferences.LoadStr(35), '&', '');

  FLWidth.Caption := Preferences.LoadStr(854) + ':';

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDColumns.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOK;
end;

procedure TDColumns.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDColumns.FBOkCheckEnabled(Sender: TObject);
var
  Found: Integer;
  I: Integer;
begin
  Found := 0;
  for I := 0 to FColumns.Items.Count - 1 do
    if (FColumns.Items[I].Checked) then
      Inc(Found);

  FBOk.Enabled := Found > 0;
end;

procedure TDColumns.FColumnsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDColumns.FColumnsResize(Sender: TObject);
begin
  if (not (csDestroying in ComponentState)) then
    PostMessage(Handle, CM_APPLYAUTOSIZE, 0, 0);
end;

procedure TDColumns.FColumnsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  FWidth.Enabled := Assigned(Item) and Item.Selected;
  FUDWidth.Enabled := FWidth.Enabled;
  FLWidth.Enabled := FWidth.Enabled;

  if (not FWidth.Enabled) then
    FWidth.Text := ''
  else if (Item.SubItems.Count > 1) then
    FWidth.Text := Item.SubItems[1];

  tbUp.Enabled := Assigned(Item) and Item.Selected and (Item.Index > 0);
  tbDown.Enabled := Assigned(Item) and Item.Selected and (Item.Index < FColumns.Items.Count - 1);
end;

procedure TDColumns.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
  J: Integer;
  NewIndex: Integer;
  OldIndex: Integer;
begin
  if (ModalResult = mrOk) then
  begin
    for I := 0 to FColumns.Items.Count - 1 do
    if (Assigned(DBGrid)) then
    begin
      OldIndex := StrToInt(FColumns.Items[I].SubItems[0]);
      NewIndex := I;

      if (NewIndex < OldIndex) then
      begin
        for J := I + 1 to FColumns.Items.Count - 1 do
          if ((NewIndex <= StrToInt(FColumns.Items[J].SubItems[0])) and (StrToInt(FColumns.Items[J].SubItems[0]) <= OldIndex)) then
            FColumns.Items[J].SubItems[0] := IntToStr(StrToInt(FColumns.Items[J].SubItems[0]) + 1);
        DBGrid.Columns.Items[StrToInt(FColumns.Items[I].SubItems[0])].Index := I;
      end;
      DBGrid.Columns.Items[I].Visible := FColumns.Items[I].Checked;
      if (DBGrid.Columns.Items[I].Visible) then
        DBGrid.Columns.Items[I].Width := StrToInt(FColumns.Items[I].SubItems[1]);
    end;
  end;
end;

procedure TDColumns.FormCreate(Sender: TObject);
begin
  ToolBar1.Images := Preferences.SmallImages;
  ToolBar2.Images := Preferences.SmallImages;

  DBGrid := nil;

  FColumns.ShowColumnHeaders := ComCtl32MajorVersion >= 6;
  if (FColumns.ShowColumnHeaders) then
  begin
    SetWindowLong(ListView_GetHeader(FColumns.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FColumns.Handle), GWL_STYLE) or HDS_NOSIZING or HDS_CHECKBOXES);
    SendMessage(FColumns.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_CHECKBOXES, LVS_EX_CHECKBOXES);
  end;
end;

procedure TDColumns.FormHide(Sender: TObject);
begin
  DBGrid := nil;

  FColumns.Items.Clear();
end;

procedure TDColumns.FormShow(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
begin
  OldChecked := True;

  if (Assigned(DBGrid)) then
  begin
    for I := 0 to DBGrid.Columns.Count - 1 do
    begin
      ListItem := FColumns.Items.Add();
      ListItem.Checked := DBGrid.Columns[I].Visible;
      ListItem.Caption := DBGrid.Columns[I].Field.DisplayName;
      ListItem.SubItems.Add(IntToStr(I));
      ListItem.SubItems.Add(IntToStr(DBGrid.Columns[I].Width));
    end;

    FColumns.Items[DBGrid.SelectedField.Index].Selected := True;
    FColumns.Selected.Focused := True;

    FUDWidth.Max := DBGrid.Width;
  end;

  ActiveControl := FColumns;
end;

procedure TDColumns.FWidthChange(Sender: TObject);
var
  Width: Integer;
begin
  if (Assigned(FColumns.Selected) and TryStrToInt(Trim(FWidth.Text), Width)) then
    FColumns.Selected.SubItems[1] := IntToStr(Width);
end;

procedure TDColumns.tbUpDownClick(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
begin
  FColumns.Items.BeginUpdate();

  if (Sender = tbUp) then
    ListItem := FColumns.Items.Insert(FColumns.Selected.Index - 1)
  else if (Sender = tbDown) then
    ListItem := FColumns.Items.Insert(FColumns.Selected.Index + 2)
  else
    ListItem := nil;

  if (Assigned(ListItem)) then
  begin
    ListItem.Checked := FColumns.Selected.Checked;
    ListItem.Caption := FColumns.Selected.Caption;
    for I := 0 to FColumns.Selected.SubItems.Count - 1 do
      ListItem.SubItems.Add(FColumns.Selected.SubItems[I]);
    FColumns.Items.Delete(FColumns.Selected.Index);
    ListItem.Selected := True;

    FColumns.ItemFocused := FColumns.Selected;
  end;

  FColumns.Items.EndUpdate();

  ActiveControl := FColumns;
end;

procedure TDColumns.WMNotify(var Message: TWMNotify);
var
  FColumnsSelected: TListItem;
  I: Integer;
begin
  if (PHDNotify(Message.NMHdr)^.Hdr.hwndFrom <> ListView_GetHeader(FColumns.Handle)) then
    inherited
  else
    case (PHDNotify(Message.NMHdr)^.Hdr.code) of
      HDN_ITEMCHANGEDA,
      HDN_ITEMCHANGEDW:
        begin
          if (PHDNotify(Message.NMHdr)^.PItem^.mask and HDI_FORMAT = 0) then
            inherited
          else if ((PHDNotify(Message.NMHdr)^.PItem^.fmt and HDF_CHECKED <> 0) <> OldChecked) then
          begin
            FColumnsSelected := FColumns.Selected;
            FColumns.Items.BeginUpdate();
            for I := 0 to FColumns.Items.Count - 1 do
              FColumns.Items[I].Checked := PHDNotify(Message.NMHdr)^.PItem^.fmt and HDF_CHECKED <> 0;
            FColumns.Items.EndUpdate();
            FColumns.Selected := FColumnsSelected;
          end;
          OldChecked := (PHDNotify(Message.NMHdr)^.PItem^.fmt and HDF_CHECKED <> 0);
        end;
      else inherited;
    end;
end;

initialization
  FColumns := nil;
end.

