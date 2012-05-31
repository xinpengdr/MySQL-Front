unit StdActns_Ext;

interface {********************************************************************}

uses
  SysUtils, Classes, ActnList, StdActns, Dialogs, StrUtils, Consts, StdCtrls;

type
  TSearchFind_Ext = class(TSearchFind)
  private
    FOnNotFound: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Search(Sender: TObject); override;
    property Control: TCustomEdit read FControl write FControl;
    property FindFirst: Boolean read FFindFirst write FFindFirst;
  published
    property OnNotFound: TNotifyEvent read FOnNotFound write FOnNotFound;
  end;

  TSearchReplace_Ext = class(TSearchReplace)
  public
    property FindFirst: Boolean read FFindFirst write FFindFirst;
  end;

  TSearchFindNext_Ext = class(TSearchFindNext)
  end;

procedure Register();

implementation {***************************************************************}

uses
  ComCtrls, RichEdit, Windows;

procedure Register();
begin
  RegisterComponents('VCL Extensions', [TSearchFind_Ext, TSearchReplace_Ext, TSearchFindNext_Ext]);
end;

function RichEditTextLength(EditControl: TCustomEdit): Integer;
var
  LGetTextLengthEx: TGetTextLengthEx;
begin
  LGetTextLengthEx.flags := GTL_DEFAULT;
  LGetTextLengthEx.codepage := 1200; // Unicode
{$IF DEFINED(CLR)}
  Result := SendWPStructMessage(EditControl.Handle, EM_GETTEXTLENGTHEX, LGetTextLengthEx, 0);
{$ELSE}
  Result := SendMessage(EditControl.Handle, EM_GETTEXTLENGTHEX, WPARAM(@LGetTextLengthEx), 0);
{$IFEND}
end;

{$IF DEFINED(CLR)}
procedure GetEditTextBuf(EditControl: TCustomEdit; Size: Integer; var Buffer: string);
{$ELSE}
procedure GetEditTextBuf(EditControl: TCustomEdit; Size: Integer; Buffer: PChar);
{$IFEND}
var
  LGetTextEx: TGetTextEx;
{$IF DEFINED(CLR)}
  LString: IntPtr;
{$IFEND}
begin
  if EditControl is TCustomRichEdit then
  begin
    LGetTextEx.cb := (Size + 1) * SizeOf(Char);
    LGetTextEx.flags := GT_DEFAULT;
    LGetTextEx.codepage := 1200; // Unicode
    LGetTextEx.lpDefaultChar := nil;
    LGetTextEx.lpUsedDefChar := nil;
{$IF DEFINED(CLR)}
    LString := Marshal.AllocHGlobal(LGetTextEx.cb);
    try
      SendWPStructMessage(EditControl.Handle, EM_GETTEXTEX, LGetTextEx, LPARAM(LString));
      Buffer := Marshal.PtrToStringUni(LString);
    finally
      Marshal.FreeHGlobal(LString);
    end;
  end
  else
    Buffer := EditControl.Text;
{$ELSE}
    SendMessage(EditControl.Handle, EM_GETTEXTEX, WPARAM(@LGetTextEx), LPARAM(Buffer));
  end
  else
    EditControl.GetTextBuf(Buffer, Size + 1);
{$IFEND}
end;

function SearchEdit(EditControl: TCustomEdit; const SearchString: String;
  Options: TFindOptions; FindFirst: Boolean = False): Boolean;
var
  Size: Integer;
  SearchOptions: TStringSearchOptions;
{$IF DEFINED(CLR)}
  P: Integer;
  Buffer: string;
{$ELSE}
  Buffer, P: PChar;
{$IFEND}
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  if EditControl is TCustomRichEdit then
    Size := RichEditTextLength(EditControl)
  else
    Size := EditControl.GetTextLen;
  if (Size = 0) then Exit;

  SearchOptions := [];
  if frDown in Options then
    Include(SearchOptions, soDown);
  if frMatchCase in Options then
    Include(SearchOptions, soMatchCase);
  if frWholeWord in Options then
    Include(SearchOptions, soWholeWord);

{$IF DEFINED(CLR)}
  GetEditTextBuf(EditControl, Size + 1, Buffer);
  if FindFirst then
    P := SearchBuf(Buffer, 1, EditControl.SelLength, SearchString, SearchOptions)
  else
    P := SearchBuf(Buffer, EditControl.SelStart + 1, EditControl.SelLength, SearchString, SearchOptions);
  if P <> -1 then
  begin
    EditControl.SelStart := P - 1;
    EditControl.SelLength := Length(SearchString);
    Result := True;
  end;
{$ELSE}
  Buffer := StrAlloc(Size + 1);
  try
    GetEditTextBuf(EditControl, Size + 1, Buffer);
    if FindFirst then
      P := SearchBuf(Buffer, Size, 0, 1, SearchString, SearchOptions)
    else
      P := SearchBuf(Buffer, Size, 0, EditControl.SelStart + EditControl.SelLength, SearchString, SearchOptions);
    if P <> nil then
    begin
      EditControl.SelStart := P - Buffer;
      EditControl.SelLength := Length(SearchString);
      Result := True;
    end;
  finally
    StrDispose(Buffer);
  end;
{$IFEND}
end;

{ TSearchFind_Ext *************************************************************}

constructor TSearchFind_Ext.Create(AOwner: TComponent);
begin
  inherited;

  FFindFirst := True;
  FOnNotFound := nil;
end;

procedure TSearchFind_Ext.Search(Sender: TObject);
begin
  if (FControl is TCustomEdit) then
  begin
    if (not SearchEdit(TCustomEdit(FControl), Dialog.FindText, Dialog.Options, FFindFirst)) then
      if (not Assigned(OnNotFound)) then
        ShowMessage(Format(STextNotFound, [Dialog.FindText]))
      else
        FOnNotFound(Self);
  end
  else
    inherited;
  FFindFirst := False;
end;

end.

