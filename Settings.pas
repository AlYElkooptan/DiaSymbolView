unit Settings;

{
  This module provides logic for persisting program settings.
}

interface

type
  TSettings = class abstract
  private
    class procedure SetSymbolSearchPath(const Value: String); static;
    type TSettingValue = (svMsdiaDllPath, svSymbolSearchPath, svEnableSorting,
      svShowStackTraces);
    class var FModified: array [TSettingValue] of Boolean;
    class var FSymbolSearchPath: String;
    class function GetMsdiaDllPath: String; static;
    class procedure SetMsdiaDllPath(const Value: String); static;
    class function GetShowStackTraces: Boolean; static;
    class procedure SetShowStackTraces(const Value: Boolean); static;
    class function GetEnableSorting: Boolean; static;
    class procedure SetEnableSorting(const Value: Boolean); static;
  public
    class property MsdiaDllPath: String read GetMsdiaDllPath write SetMsdiaDllPath;
    class property SymbolSearchPath: String read FSymbolSearchPath write SetSymbolSearchPath;
    class property EnableSorting: Boolean read GetEnableSorting write SetEnableSorting;
    class property ShowStackTraces: Boolean read GetShowStackTraces write SetShowStackTraces;
    class constructor Create;
    class procedure Load; static;
    class procedure Save; static;
  end;

implementation

uses
  Ntapi.ntregapi, Ntapi.ntseapi, NtUtils, NtUtils.Registry, NtUtils.Tokens.Info,
  NtUtils.Security.Sid, NtUtils.DbgHelp.Dia, NtUiLib.Errors.Dialog, DiaNodes;

function OpenSettingsKey(
  out hxKey: IHandle;
  WriteAccess: Boolean
): TNtxStatus;
var
  User: ISid;
  Path: String;
begin
  // Determine the SID for HKCU
  Result := NtxQuerySidToken(NtxCurrentProcessToken, TokenUser, User);

  if not Result.IsSuccess then
    Exit;

  Result := RtlxSidToString(User, Path);

  if not Result.IsSuccess then
    Exit;

  // Settings are per-user
  Path := REG_PATH_USER + '\' + Path + '\Software\DiaSymbolView';

  if WriteAccess then
    Result := NtxCreateKey(hxKey, Path, KEY_SET_VALUE)
  else
    Result := NtxOpenKey(hxKey, Path, KEY_QUERY_VALUE);
end;

const
  SETTING_NAMES: array [TSettings.TSettingValue] of String = ('MsdiaPath',
    'SymbolPath', 'EnableSorting', 'ShowStackTraces');

class constructor TSettings.Create;
begin
  // Defaults
  FSymbolSearchPath := 'C:\Symbols';
end;

class function TSettings.GetEnableSorting;
begin
  Result := DiaNodes.EnableSorting;
end;

class function TSettings.GetMsdiaDllPath;
begin
  Result := NtUtils.DbgHelp.Dia.MsdiaDllPath;
end;

class function TSettings.GetShowStackTraces;
begin
  Result := NtUtils.CaptureStackTraces;
end;

class procedure TSettings.Load;
var
  hxKey: IHandle;
  StringValue: String;
  CardinalValue: Cardinal;
begin
  if OpenSettingsKey(hxKey, False).IsSuccess then
  begin
    // Msdia DLL path
    if NtxQueryValueKeyString(hxKey, SETTING_NAMES[svMsdiaDllPath],
      StringValue).IsSuccess then
    begin
      MsdiaDllPath := StringValue;
      FModified[svMsdiaDllPath] := False;
    end;

    // Symbol search path
    if NtxQueryValueKeyString(hxKey, SETTING_NAMES[svSymbolSearchPath],
      StringValue).IsSuccess then
    begin
      SymbolSearchPath := StringValue;
      FModified[svSymbolSearchPath] := False;
    end;

    // Sorting
    if NtxQueryValueKeyUInt32(hxKey, SETTING_NAMES[svEnableSorting],
      CardinalValue).IsSuccess then
    begin
      EnableSorting := CardinalValue <> 0;
      FModified[svEnableSorting] := False;
    end;

    // Show stack traces
    if NtxQueryValueKeyUInt32(hxKey, SETTING_NAMES[svShowStackTraces],
      CardinalValue).IsSuccess then
    begin
      ShowStackTraces := CardinalValue <> 0;
      FModified[svShowStackTraces] := False;
    end;
  end;
end;

class procedure TSettings.Save;
var
  hxKey: IHandle;
begin
  // Is anything modifed?
  if not FModified[svMsdiaDllPath] and not FModified[svSymbolSearchPath] and
    not FModified[svEnableSorting] and not FModified[svShowStackTraces] then
    Exit;

  if OpenSettingsKey(hxKey, True).IsSuccess then
  begin
    if FModified[svMsdiaDllPath] then
      NtxSetValueKeyString(hxKey, SETTING_NAMES[svMsdiaDllPath], MsdiaDllPath);

    if FModified[svSymbolSearchPath] then
      NtxSetValueKeyString(hxKey, SETTING_NAMES[svSymbolSearchPath],
        SymbolSearchPath);

    if FModified[svEnableSorting] then
      NtxSetValueKeyUInt32(hxKey, SETTING_NAMES[svEnableSorting],
        Cardinal(EnableSorting));

    if FModified[svShowStackTraces] then
      NtxSetValueKeyUInt32(hxKey, SETTING_NAMES[svShowStackTraces],
        Cardinal(ShowStackTraces));
  end;
end;

class procedure TSettings.SetEnableSorting;
begin
  if DiaNodes.EnableSorting <> Value then
  begin
    DiaNodes.EnableSorting := Value;
    FModified[svEnableSorting] := True;
  end;
end;

class procedure TSettings.SetMsdiaDllPath;
begin
  if NtUtils.DbgHelp.Dia.MsdiaDllPath <> Value then
  begin
    NtUtils.DbgHelp.Dia.MsdiaDllPath := Value;
    FModified[svMsdiaDllPath] := True;
  end;
end;

class procedure TSettings.SetShowStackTraces;
begin
  if (NtUtils.CaptureStackTraces <> Value) or
    (NtUiLib.Errors.Dialog.DisplayStackTraces <> Value) then
  begin
    NtUtils.CaptureStackTraces := Value;
    NtUiLib.Errors.Dialog.DisplayStackTraces := Value;
    System.ReportMemoryLeaksOnShutdown := Value;
    FModified[svShowStackTraces] := True
  end;
end;

class procedure TSettings.SetSymbolSearchPath;
begin
  if FSymbolSearchPath <> Value then
  begin
    FSymbolSearchPath := Value;
    FModified[svSymbolSearchPath] := True;
  end;
end;

end.
