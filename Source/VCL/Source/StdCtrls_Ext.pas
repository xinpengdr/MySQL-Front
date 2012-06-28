unit StdCtrls_Ext;

interface {********************************************************************}

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows;

type
  TComboBox_Ext = class(TComboBox)
  public
    procedure CopyToClipboard(); virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure PasteFromClipboard(); virtual;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

  TGroupBox_Ext = class(TGroupBox)
  private
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
  end;

  TMemo_Ext = class(TMemo)
  private
    FInsertMode: Boolean;
  protected
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InsertMode: Boolean read FInsertMode write FInsertMode default True;
  end;

procedure Register();

implementation {***************************************************************}

uses
  StdActns, Clipbrd;

procedure Register();
begin
  RegisterComponents('VCL Extensions', [TComboBox_Ext]);
  RegisterComponents('VCL Extensions', [TGroupBox_Ext]);
  RegisterComponents('VCL Extensions', [TMemo_Ext]);
end;

{ TComboBox_Ext ***************************************************************}

procedure TComboBox_Ext.CopyToClipboard();
var
  ClipboardData: HGLOBAL;
  Len: Integer;
  S: String;
begin
  if (OpenClipboard(Handle)) then
  begin
    EmptyClipboard();

    S := SelText; Len := Length(S);
    ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1) * SizeOf(S[1]));
    Move(PChar(S)^, GlobalLock(ClipboardData)^, (Len + 1) * SizeOf(S[1]));
    SetClipboardData(CF_UNICODETEXT, ClipboardData);
    GlobalUnlock(ClipboardData);
    CloseClipboard();
  end;
end;

function TComboBox_Ext.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditAction) then
  begin
    Result := True;
    if (Action is TEditCut) then
      begin CopyToClipboard(); SelText := ''; end
    else if (Action is TEditCopy) then
      CopyToClipboard()
    else if (Action is TEditPaste) then
      PasteFromClipboard()
    else if (Action is TEditDelete) then
      SelText := ''
    else
      Result := inherited ExecuteAction(Action);
  end
  else
    Result := inherited ExecuteAction(Action);
end;

procedure TComboBox_Ext.PasteFromClipboard();
var
  ClipboardData: HGLOBAL;
begin
  if (OpenClipboard(Handle)) then
  begin
    ClipboardData := GetClipboardData(CF_UNICODETEXT);
    if (ClipboardData <> 0) then
      SelText := PChar(GlobalLock(ClipboardData));
    GlobalUnlock(ClipboardData);
    CloseClipboard();
  end;
end;

function TComboBox_Ext.UpdateAction(Action: TBasicAction): Boolean;
begin
  if (Action is TEditAction) then
  begin
    Result := Focused;
    if (Result) then
      if (Action is TEditCut) then
        TEditAction(Action).Enabled := SelText <> ''
      else if (Action is TEditCopy) then
        TEditAction(Action).Enabled := SelText <> ''
      else if (Action is TEditPaste) then
        TEditAction(Action).Enabled := not (Style in [csDropDownList]) and Clipboard.HasFormat(CF_UNICODETEXT)
      else if (Action is TEditDelete) then
        TEditAction(Action).Enabled := SelText <> ''
      else
        Result := False;
  end
  else
    Result := inherited UpdateAction(Action);
end;

{ TGroupBox_Ext ***************************************************************}

procedure TGroupBox_Ext.WMNotify(var Message: TWMNotify);
begin
  if (not Assigned(Parent)) then
    inherited
  else
    Message.Result := Parent.Perform(Message.Msg, TMessage(Message).WParam, TMessage(Message).LParam);
end;

{ TMemo_Ext *******************************************************************}

constructor TMemo_Ext.Create(AOwner: TComponent);
begin
  inherited;

  InsertMode := True;
end;

procedure TMemo_Ext.KeyPress(var Key: Char);
begin
  if ((SelLength = 0) and not InsertMode) then
    SelLength := 1;

 inherited;
end;

end.
