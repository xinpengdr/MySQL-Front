unit fProfiling;

interface {********************************************************************}

procedure ProfilingPoint(const Index: Integer);
function ProfilingReport(const Filename: string = ''): string;
procedure ProfilingReset();

implementation {***************************************************************}

uses
  Windows, SysUtils;

var
  LastPoint: Integer;
  LocaleFormatSettings: TFormatSettings;
  Points: array of record
    Count: Int64;
    Sum: Int64;
  end;

procedure ProfilingPoint(const Index: Integer);
begin
  if (Index >= Length(Points)) then
  begin
    SetLength(Points, Index + 1);
    Points[Index].Sum := 0;
  end;

  QueryPerformanceCounter(Points[Index].Count);
  Inc(Points[Index].Sum, Points[Index].Count - Points[LastPoint].Count);
  LastPoint := Index;
end;

function ProfilingReport(const Filename: string = ''): string;
const
  BOM: PAnsiChar = #$FF + #$FE;
var
  Index: Integer;
  Handle: THandle;
  BytesWritten: DWord;
  Sum: Int64;
begin
  Result := '';

  Sum := 0;
  for Index := 1 to Length(Points) - 1 do
    Inc(Sum, Points[Index].Sum);

  for Index := 1 to Length(Points) - 1 do
    Result := Result + Format('%2d:  %17s  %3d %s' + #13#10, [Index, FormatFloat('#,##0', Points[Index].Sum, LocaleFormatSettings), Points[Index].Sum * 100 div Sum, '%']);

  Result := Result + Format('-----------------------------' + #13#10, []);
  Result := Result + Format('     %17s  %3d %s' + #13#10, [FormatFloat('#,##0', Sum, LocaleFormatSettings), 100, '%']);

  if (Filename <> '') then
  begin
    Handle := CreateFile(PChar(Filename),
                         GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         CREATE_ALWAYS, 0, 0);
    if (Handle = INVALID_HANDLE_VALUE) then
      MessageBox(0, PChar(SysErrorMessage(GetLastError())), 'Error', MB_OK)
    else
    begin
      WriteFile(Handle, BOM^, StrLen(BOM), BytesWritten, nil);
      WriteFile(Handle, Result[1], Length(Result) * SizeOf(Result[1]), BytesWritten, nil);
      CloseHandle(Handle);
    end;
  end;
end;

procedure ProfilingReset();
begin
  SetLength(Points, 1);

  QueryPerformanceCounter(Points[0].Count);
  Points[0].Sum := 0;
  LastPoint := 0;
end;

initialization
  LocaleFormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
  SetLength(Points, 0);
finalization
  SetLength(Points, 0);
end.
