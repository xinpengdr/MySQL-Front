unit CommCtrl_Ext;

interface {********************************************************************}

var
  ComCtl32MajorVersion: Integer = 0;
  ComCtl32MinorVersion: Integer = 0;

implementation {***************************************************************}

uses
  SysUtils, ShLwApi, Windows;

function PACKVERSION(const Major, Minor: Word): DWord;
begin
  Result := Major shl 16 + Minor;
end;

function GetDllVersion(const Filename: TFileName): DWord;
var
  DLLGetVersion: DllGetVersionProc;
  DllVersionInfo: TDllVersionInfo;
  Handle: THandle;
begin
  Result := 0;

  Handle := LoadLibrary(PChar(ExtractFileName(Filename)));
  if (Handle <> 0) then
  begin
    DllGetVersion := GetProcAddress(Handle, 'DllGetVersion');
    if (Assigned(DllGetVersion)) then
    begin
      ZeroMemory(@DllVersionInfo, SizeOf(TDllVersionInfo));
      DllVersionInfo.cbSize := SizeOf(DllVersionInfo);
      if (DllGetVersion(DllVersionInfo) = NOERROR) then
        Result := PACKVERSION(DllVersionInfo.dwMajorVersion, DllVersionInfo.dwMinorVersion);
    end;

    FreeLibrary(Handle);
  end;
end;

var
  DllVersion: DWord;
initialization
  DllVersion := GetDllVersion('ComCtl32.dll');
  ComCtl32MajorVersion := DllVersion shr 16;
  ComCtl32MinorVersion := DllVersion and $FFFF;
end.

