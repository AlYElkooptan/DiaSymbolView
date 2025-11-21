unit DiaProperties;

{
  This module provides access to most of IDiaSymbol's properties.
}

interface

uses
  Ntapi.DbgHelp, Ntapi.msdia, Ntapi.ImageHlp, NtUtils, DelphiApi.Reflection,
  DelphiUtils.LiteRTTI, DelphiUiLib.LiteReflection;

type
  DiaxProperties = class abstract
    // Iterate over a DIA enum and collect all elements
    class function CollectEnum<I>(
      const Enum: IDiaEnum<I>;
      out EnumValues: TArray<I>
    ): TNtxStatus; static;
  end;

  // A callback for collecting a simple (reflection) object property
  TDiaxReflectionPropertyCallback<I> = function (
    const Source: I;
    out Reflection: TRttixFullReflection
  ): Boolean;

  // A simple (reflection) object property
  TDiaxReflectionProperty<I> = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<I>;
  end;

  // A callback for collecting an associated instance
  TDiaxInstancePropertyCallback<ISource, IInstance> = function (
    const Source: ISource;
    out Instance: IInstance
  ): Boolean;

  // An associated instance property
  TDiaxInstanceProperty<ISource, IInstance> = record
    Name: String;
    Callback: TDiaxInstancePropertyCallback<ISource, IInstance>;
  end;

  // A callback for collecting an associated instance enumerator
  TDiaxEnumPropertyCallback<ISource, IInstance> = function (
    const Source: ISource;
    out Enum: IDiaEnum<IInstance>
  ): Boolean;

  // An associated instance enumerator property
  TDiaxEnumProperty<ISource, IInstance> = record
    Name: String;
    Callback: TDiaxEnumPropertyCallback<ISource, IInstance>;
  end;

  TSimpleProperty = record
    Name: String;
    Value: TRttixFullReflection;
  end;

  // A custom interface for exposed blocks of binary data
  IBinaryData = interface
    ['{22F1DB68-33DF-4B6C-BE1C-8192C486D040}']
    function GetTypeInfo: IRttixType;
    function GetData: IMemory;
    function FormatProperties: TArray<TSimpleProperty>;

    property TypeInfo: IRttixType read GetTypeInfo;
    property Data: IMemory read GetData;
  end;

  // A custom interface for IMAGE_SECTION_HEADER data
  IImageSectionHeader = interface (IBinaryData)
    ['{2A9909F3-182C-4B21-A88B-BF0BE3C49C90}']
    function GetSectionName: String;
    property SectionName: String read GetSectionName;
  end;

  // A custom interface for XFIXUP_DATA data
  IXFixupData = interface (IBinaryData)
    ['{A573CB90-4CFC-4156-BC62-045E23087436}']
  end;

  // A custom interface for FPO_DATA data
  IFpoData = interface (IBinaryData)
    ['{015513A3-E2CA-4A4B-9B11-780C5AB76CE3}']
  end;

  // A custom interface for FRAMEDATA data
  IFrameData = interface (IBinaryData)
    ['{41AECE30-A550-487F-860C-CBF2C415CCDF}']
  end;

  // A custom interface for OMAP_DATA data
  IOmapData = interface (IBinaryData)
    ['{80121B3E-3325-4CF8-B591-DB21D597A26B}']
  end;

{ IDiaSession }

// Remember the current session for better setting lookup
procedure DiaxSetCurrentSession(const Session: IDiaSession);

// IDiaSession to reflection properties
function DiaxSessionToReflection(
): TArray<TDiaxReflectionProperty<IDiaSession>>;

// IDiaSession to IDiaSymbol properties
function DiaxSessionToSymbol(
): TArray<TDiaxInstanceProperty<IDiaSession, IDiaSymbol>>;

// IDiaSession to IDiaEnum<IDiaSymbol> properties
function DiaxSessionToSymbolEnum(
): TArray<TDiaxEnumProperty<IDiaSession, IDiaSymbol>>;

// IDiaSession to IDiaEnum<IDiaSourceFile> properties
function DiaxSessionToSourceFileEnum(
): TArray<TDiaxEnumProperty<IDiaSession, IDiaSourceFile>>;

// IDiaSession to IDiaEnum<IDiaInputAssemblyFile> properties
function DiaxSessionToAssemblyFileEnum(
): TArray<TDiaxEnumProperty<IDiaSession, IDiaInputAssemblyFile>>;

// IDiaSession to IDiaEnum<IDiaTable> properties
function DiaxSessionToTableEnum(
): TArray<TDiaxEnumProperty<IDiaSession, IDiaTable>>;

// IDiaSession to IDiaEnum<IDiaEnumDebugStreamData> properties
function DiaxSessionToDebugStreamEnum(
): TArray<TDiaxEnumProperty<IDiaSession, IDiaEnumDebugStreamData>>;

{ IDiaSymbol }

// Sort IDiaSymbol entries
procedure DiaxSortSymbols(
  var Symbols: TArray<IDiaSymbol>;
  SortByName: Boolean
);

// IDiaSymbol to reflection properties
function DiaxSymbolToReflection(
): TArray<TDiaxReflectionProperty<IDiaSymbol>>;

// IDiaSymbol to IDiaSymbol properties
function DiaxSymbolToSymbol(
): TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaSymbol>>;

// IDiaSymbol to IDiaEnum<IDiaSourceFile> properties
function DiaxSymbolToSourceFileEnum(
): TArray<TDiaxEnumProperty<IDiaSymbol, IDiaSourceFile>>;

// IDiaSymbol to IDiaEnum<IDiaSymbol> properties
function DiaxSymbolToSymbolEnum(
): TArray<TDiaxEnumProperty<IDiaSymbol, IDiaSymbol>>;

// IDiaSymbol to IDiaLineNumber properties
function DiaxSymbolToLineNumber(
): TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaLineNumber>>;

// IDiaSymbol to IDiaEnum<IDiaLineNumber> properties
function DiaxSymbolToLineNumberEnum(
): TArray<TDiaxEnumProperty<IDiaSymbol, IDiaLineNumber>>;

// IDiaSymbol to IDiaInputAssemblyFile properties
function DiaxSymbolToAssemblyFile(
): TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaInputAssemblyFile>>;

{ IDiaSourceFile }

// Sort IDiaSourceFile entries
procedure DiaxSortSourceFiles(var SourceFiles: TArray<IDiaSourceFile>);

// IDiaSourceFile to reflection properties
function DiaxSourceFileToReflection(
): TArray<TDiaxReflectionProperty<IDiaSourceFile>>;

{ IDiaInputAssemblyFile }

// IDiaInputAssemblyFile to reflection properties
function DiaxAssemblyFileToReflection(
): TArray<TDiaxReflectionProperty<IDiaInputAssemblyFile>>;

{ IDiaLineNumber }

// Sort IDiaLineNumber entries
procedure DiaxSortLineNumbers(var Lines: TArray<IDiaLineNumber>);

// IDiaLineNumber to reflection properties
function DiaxLineNumberToReflection(
): TArray<TDiaxReflectionProperty<IDiaLineNumber>>;

// IDiaLineNumber to IDiaSymbol properties
function DiaxLineNumberToSymbol(
): TArray<TDiaxInstanceProperty<IDiaLineNumber, IDiaSymbol>>;

// IDiaLineNumber to IDiaSourceFile properties
function DiaxLineNumberToSourceFile(
): TArray<TDiaxInstanceProperty<IDiaLineNumber, IDiaSourceFile>>;

{ IDiaSectionContrib }

// Sort IDiaSectionContrib entries
procedure DiaxSortSections(var Sections: TArray<IDiaSectionContrib>);

// IDiaSectionContrib to reflection properties
function DiaxSectionToReflection(
): TArray<TDiaxReflectionProperty<IDiaSectionContrib>>;

// IDiaSectionContrib to IDiaSymbol properties
function DiaxSectionToSymbol(
): TArray<TDiaxInstanceProperty<IDiaSectionContrib, IDiaSymbol>>;

{ IDiaSegment }

// IDiaSegment to reflection properties
function DiaxSegmentToReflection(
): TArray<TDiaxReflectionProperty<IDiaSegment>>;

{ IDiaInjectedSource }

// IDiaInjectedSource to reflection properties
function DiaxInjectedSourceToReflection(
): TArray<TDiaxReflectionProperty<IDiaInjectedSource>>;

{ IDiaFrameData }

// IDiaFrameData to reflection properties
function DiaxFrameDataToReflection(
): TArray<TDiaxReflectionProperty<IDiaFrameData>>;

// IDiaFrameData to IDiaFrameData properties
function DiaxFrameDataToFrameData(
): TArray<TDiaxInstanceProperty<IDiaFrameData, IDiaFrameData>>;

{ IDiaTable }

// IDiaTable to reflection properties
function DiaxTableToReflection(
): TArray<TDiaxReflectionProperty<IDiaTable>>;

// IDiaTable to IDiaSymbol properties
function DiaxTableToSymbolEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaSymbol>>;

// IDiaTable to IDiaSourceFile properties
function DiaxTableToSourceFileEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaSourceFile>>;

// IDiaTable to IDiaInputAssemblyFile properties
function DiaxTableToAssemblyFileEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaInputAssemblyFile>>;

// IDiaTable to IDiaLineNumber properties
function DiaxTableToLineNumberEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaLineNumber>>;

// IDiaTable to IDiaSectionContrib properties
function DiaxTableToSectionEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaSectionContrib>>;

// IDiaTable to IDiaSegment properties
function DiaxTableToSegmentEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaSegment>>;

// IDiaTable to IDiaInjectedSource properties
function DiaxTableToInjectedSourceEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaInjectedSource>>;

// IDiaTable to IDiaFrameData properties
function DiaxTableToFrameDataEnum(
): TArray<TDiaxEnumProperty<IDiaTable, IDiaFrameData>>;

{ IDiaEnumDebugStreamData }

// IDiaEnumDebugStreamData to reflection properties
function DiaxDebugStreamToReflection(
): TArray<TDiaxReflectionProperty<IDiaEnumDebugStreamData>>;

// IDiaEnumDebugStreamData to IDiaImageData properties
function DiaxDebugStreamToImageData(
): TArray<TDiaxInstanceProperty<IDiaEnumDebugStreamData, IDiaImageData>>;

// IDiaEnumDebugStreamData to IDiaEnum<IBinaryData> properties
function DiaxDebugStreamToBinaryDataEnum(
): TArray<TDiaxEnumProperty<IDiaEnumDebugStreamData, IBinaryData>>;

{ IDiaImageData }

// IDiaImageData to reflection properties
function DiaxImageDataToReflection(
): TArray<TDiaxReflectionProperty<IDiaImageData>>;

{ IBinaryData }

// IBinaryData to reflection properties
function DiaxBinaryDataToReflection(
): TArray<TDiaxReflectionProperty<IBinaryData>>;

implementation

uses
  Ntapi.WinNt, NtUtils.SysUtils, DelphiUtils.Arrays, DelphiUiLib.Strings,
  DelphiUiLib.LiteReflection.Types, RegisterNames, TextHelpers;

{$BOOLEVAL OFF}
{$IFOPT R+}{$DEFINE R+}{$ENDIF}
{$IFOPT Q+}{$DEFINE Q+}{$ENDIF}

{$REGION 'Generic helpers'}

var
  CurrentDiaSession: IDiaSession;
  CurrentDiaMachineType: TImageMachine32;

procedure DiaxSetCurrentSession;
var
  Scope: IDiaSymbol;
begin
  CurrentDiaSession := Session;

  if not Assigned(Session) or (Session.get_globalScope(Scope) <> S_OK) or
    (Scope.get_machineType(CurrentDiaMachineType) <> S_OK) then
    CurrentDiaMachineType := 0;
end;

function DiaxCurrentFixupDataTypeInfo: PLiteRttiTypeInfo;
begin
  case CurrentDiaMachineType of
    IMAGE_FILE_MACHINE_I386:
      Result := TypeInfo(TXFixupDataI386);

    IMAGE_FILE_MACHINE_AMD64:
      Result := TypeInfo(TXFixupDataAmd64);

    IMAGE_FILE_MACHINE_ARM, IMAGE_FILE_MACHINE_THUMB, IMAGE_FILE_MACHINE_ARMNT:
      Result := TypeInfo(TXFixupDataArm);

    IMAGE_FILE_MACHINE_ARM64:
      Result := TypeInfo(TXFixupDataArm64);

  else
    Result := TypeInfo(TXFixupData);
  end;
end;

type
  TFakeDiaEnum<I> = class (TAutoInterfacedObject, IDiaEnum<I>)
    FElements: TArray<I>;
    FPosition: Integer;
    function get__NewEnum(out RetVal: IUnknown): HResult; stdcall;
    function get_Count(out RetVal: Integer): HResult; stdcall;
    function Item(Index: Cardinal; out Item: I): HResult; stdcall;
    function Next(Count: Integer; out Elements: I; out Fetched: Integer): HResult; stdcall;
    function Skip(Count: Cardinal): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IDiaEnum<I>): HResult; stdcall;
    constructor Create(const Elements: TArray<I>; Position: Integer = 0);
  end;

function TFakeDiaEnum<I>.Clone;
begin
  Enum := TFakeDiaEnum<I>.Create(FElements, FPosition);
  Result := S_OK;
end;

constructor TFakeDiaEnum<I>.Create;
begin
  FElements := Elements;
  FPosition := Position;
end;

function TFakeDiaEnum<I>.get_Count;
begin
  RetVal := Length(FElements);
  Result := S_OK;
end;

function TFakeDiaEnum<I>.get__NewEnum;
begin
  Result := E_NOTIMPL;
end;

function TFakeDiaEnum<I>.Item;
begin
  if Index <= High(FElements) then
  begin
    Item := FElements[Index];
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;

function TFakeDiaEnum<I>.Next;
var
  Buffer: ^I;
begin
  Buffer := @Elements;
  Fetched := 0;

  while (FPosition <= High(FElements)) and (Fetched < Count) do
  begin
    Buffer^ := FElements[FPosition];
    Inc(FPosition);
    Inc(Fetched);
    Inc(Buffer);
  end;

  if Fetched > 0 then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TFakeDiaEnum<I>.Reset;
begin
  FPosition := 0;
  Result := S_OK;
end;

function TFakeDiaEnum<I>.Skip;
begin
  Inc(FPosition, Count);
  Result := S_OK;
end;

class function DiaxProperties.CollectEnum<I>;
begin
var
  Count, Fetched: Integer;
begin
  Enum.Reset;
  Result.Location := 'IDiaEnum::get_Count';
  Result.HResult := Enum.get_Count(Count);

  if not Result.IsSuccess or (Count <= 0) then
    Exit;

  EnumValues := nil;
  SetLength(EnumValues, Count);

  // Retrieve all entries
  Result.Location := 'IDiaEnum::Next';
  Result.HResultAllowFalse := Enum.Next(Count, EnumValues[0], Fetched);

  // Truncate if necessary
  if Result.IsSuccess and (Fetched < Count) then
    SetLength(EnumValues, Fetched);
end;
end;

{$ENDREGION}
{$REGION 'IDiaSession to reflection'}

function DiaxSession_getFuncMDTokenMapSize(
  const Session: IDiaSession;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Session.getFuncMDTokenMapSize(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsBytes);
end;

function DiaxSession_getTypeMDTokenMapSize(
  const Session: IDiaSession;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Session.getTypeMDTokenMapSize(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsBytes);
end;

function DiaxSession_isFastLinkPDB(
  const Session: IDiaSession;
  out Reflection: TRttixFullReflection
): Boolean;
var
  SessionEx: IDiaSessionEx;
  Value: LongBool;
begin
  Result := (Session.QueryInterface(IDiaSession, SessionEx) = S_OK) and
    (SessionEx.isFastLinkPDB(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSession_isPortablePDB(
  const Session: IDiaSession;
  out Reflection: TRttixFullReflection
): Boolean;
var
  SessionEx: IDiaSessionEx;
  Value: LongBool;
begin
  Result := (Session.QueryInterface(IDiaSession, SessionEx) = S_OK) and
    (SessionEx.isPortablePDB(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxSessionReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaSession>;
  end;

const
  DiaxpSessionToReflectionDef: array [0..3] of TDiaxSessionReflectionProperty = (
    (Name: 'Function Metadata Token Map'; Callback: DiaxSession_getFuncMDTokenMapSize),
    (Name: 'Type Metadata Token Map'; Callback: DiaxSession_getTypeMDTokenMapSize),
    (Name: 'Fast-link PDB'; Callback: DiaxSession_isFastLinkPDB),
    (Name: 'Portable PDB'; Callback: DiaxSession_isPortablePDB)
  );

var
  DiaxpSessionToReflectionCache: TArray<TDiaxReflectionProperty<IDiaSession>>;

function DiaxSessionToReflection;
var
  i: Integer;
begin
  if Length(DiaxpSessionToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpSessionToReflectionCache,
      Length(DiaxpSessionToReflectionDef));

    for i := 0 to High(DiaxpSessionToReflectionCache) do
    begin
      DiaxpSessionToReflectionCache[i].Name :=
        DiaxpSessionToReflectionDef[i].Name;
      DiaxpSessionToReflectionCache[i].Callback :=
        DiaxpSessionToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpSessionToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaSession to IDiaSymbol'}

function DiaxSession_get_globalScope(
  const Session: IDiaSession;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Session.get_globalScope(Value) = S_OK;
end;

function DiaxSessionToSymbol;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Global Scope';
  Result[0].Callback := DiaxSession_get_globalScope;
end;

{$ENDREGION}
{$REGION 'IDiaSession to IDiaEnum<IDiaSymbol>'}

function DiaxSession_getExports(
  const Session: IDiaSession;
  out Value: IDiaEnum<IDiaSymbol>
): Boolean;
begin
  Result := Session.getExports(IDiaEnumSymbols(Value)) = S_OK;
end;

function DiaxSession_getHeapAllocationSites(
  const Session: IDiaSession;
  out Value: IDiaEnum<IDiaSymbol>
): Boolean;
begin
  Result := Session.getHeapAllocationSites(IDiaEnumSymbols(Value)) = S_OK;
end;

function DiaxSessionToSymbolEnum;
begin
  SetLength(Result, 2);
  Result[0].Name := 'Exports';
  Result[0].Callback := DiaxSession_getExports;
  Result[1].Name := 'Heap Allocation Sites';
  Result[1].Callback := DiaxSession_getHeapAllocationSites;
end;

{$ENDREGION}
{$REGION 'IDiaSession to IDiaEnum<IDiaSourceFile>'}

function DiaxSession_findFile(
  const Session: IDiaSession;
  out Value: IDiaEnum<IDiaSourceFile>
): Boolean;
begin
  Result := Session.findFile(nil, '', 0, IDiaEnumSourceFiles(Value)) = S_OK;
end;

function DiaxSessionToSourceFileEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Source Files';
  Result[0].Callback := DiaxSession_findFile;
end;

{$ENDREGION}
{$REGION 'IDiaSession to IDiaEnum<IDiaInputAssemblyFile>'}

function DiaxSession_findInputAssemblyFiles(
  const Session: IDiaSession;
  out Value: IDiaEnum<IDiaInputAssemblyFile>
): Boolean;
begin
  Result := Session.findInputAssemblyFiles(IDiaEnumInputAssemblyFiles(Value)) = S_OK;
end;

function DiaxSessionToAssemblyFileEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Input Assembly Files';
  Result[0].Callback := DiaxSession_findInputAssemblyFiles;
end;

{$ENDREGION}
{$REGION 'IDiaSession to IDiaEnum<IDiaTable>'}

function DiaxSession_getEnumTables(
  const Session: IDiaSession;
  out Value: IDiaEnum<IDiaTable>
): Boolean;
begin
  Result := Session.getEnumTables(IDiaEnumTables(Value)) = S_OK;
end;

function DiaxSessionToTableEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Tables';
  Result[0].Callback := DiaxSession_getEnumTables;
end;

{$ENDREGION}
{$REGION 'IDiaSession to IDiaEnum<IDiaEnumDebugStreamData>'}

function DiaxSession_getEnumDebugStreams(
  const Session: IDiaSession;
  out Value: IDiaEnum<IDiaEnumDebugStreamData>
): Boolean;
begin
  Result := Session.getEnumDebugStreams(IDiaEnumDebugStreams(Value)) = S_OK;
end;

function DiaxSessionToDebugStreamEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Debug Streams';
  Result[0].Callback := DiaxSession_getEnumDebugStreams;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol Sorting'}

procedure DiaxSortSymbols;
var
  Tags: TArray<TSymTagEnum>;
  Names: TArray<String>;
  WideName: WideString;
  Parent: IDiaSymbol;
  i: Integer;
begin
  // Collect all tags
  SetLength(Tags, Length(Symbols));

  for i := 0 to High(Symbols) do
    if Symbols[i].get_symTag(Tags[i]) <> S_OK then
      Tags[i] := SymTagNull;

  if SortByName then
  begin
    // Collect all names, in case we need to sort by them
    SetLength(Names, Length(Symbols));

    for i := 0 to High(Symbols) do
      if (Symbols[i].get_name(WideName) = S_OK) and (WideName <> '') then
      begin
        Names[i] := WideName;

        // For compilands, we only need the file name, not the entire path
        if Tags[i] = SymTagCompiland then
          Names[i] := RtlxExtractNamePath(Names[i]);
      end
      // For heap allocation sites, we use the name of the parent
      else if (Tags[i] = SymTagHeapAllocationSite) and
        (Symbols[i].get_lexicalParent(Parent) = S_OK) and
        (Parent.get_name(WideName) = S_OK) and (WideName <> '') then
        Names[i] := WideName;
  end
  else
    Names := nil;

  // Sort by tag and then by name
  TArray.SortIndexInline<IDiaSymbol>(Symbols,
    function (const IndexA, IndexB: Integer): Integer
    begin
      // Group by tag
      {$R-}{$Q-}
      Result := Integer(Tags[IndexA]) - Integer(Tags[IndexB]);
      {$IFDEF Q+}{$Q+}{$ENDIF}{$IFDEF R+}{$R+}{$ENDIF}

      if Result <> 0 then
        Exit;

      // Then sort by name
      if SortByName then
      begin
        Result := RtlxCompareStrings(Names[IndexA], Names[IndexB]);

        if Result <> 0 then
          Exit;
      end;

      // Then by index, to preserve order of unnamed entries of the same type
      {$R-}{$Q-}
      Result := IndexA - IndexB;
      {$IFDEF Q+}{$Q+}{$ENDIF}{$IFDEF R+}{$R+}{$ENDIF}
    end
  );
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to reflection'}

function DiaxSymbol_get_symTag(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TSymTagEnum;
begin
  Result := Symbol.get_symTag(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_name(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_name(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_dataKind(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TDataKind;
begin
  Result := Symbol.get_dataKind(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_locationType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TLocationType;
begin
  Result := Symbol.get_locationType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_addressSection(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_addressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_addressOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_addressOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_relativeVirtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_relativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_virtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Symbol.get_virtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_registerId(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_registerId(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText, rfHint];
    Reflection.Text := LookupRegisterName(Value, CurrentDiaMachineType);
    Reflection.Hint := BuildHint([
      THintSection.New('Value (decimal)', UiLibUIntToDec(Value)),
      THintSection.New('Value (hex)', UiLibUIntToHex(Value))
    ]);
  end;
end;

function DiaxSymbol_get_offset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Integer;
begin
  Result := Symbol.get_offset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_length(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Symbol.get_length(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_slot(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_slot(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_volatileType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_volatileType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_constType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_constType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_unalignedType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_unalignedType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_access(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvAccessE;
begin
  Result := Symbol.get_access(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_libraryName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_libraryName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_platform(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvCpuTypeE;
begin
  Result := Symbol.get_platform(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_language(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvCflLang;
begin
  Result := Symbol.get_language(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_editAndContinueEnabled(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_editAndContinueEnabled(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_frontEndMajor(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_frontEndMajor(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_frontEndMinor(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_frontEndMinor(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_frontEndBuild(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_frontEndBuild(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_backEndMajor(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_backEndMajor(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_backEndMinor(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_backEndMinor(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_backEndBuild(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_backEndBuild(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_sourceFileName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_sourceFileName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_thunkOrdinal(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TThunkOrdinal;
begin
  Result := Symbol.get_thunkOrdinal(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_thisAdjust(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Integer;
begin
  Result := Symbol.get_thisAdjust(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_virtualBaseOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_virtualBaseOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_virtual(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_virtual(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_intro(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_intro(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_pure(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_pure(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_callingConvention(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvCallE;
begin
  Result := Symbol.get_callingConvention(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_value(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TVarData;
begin
  Result := Symbol.get_value(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_baseType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TBasicType;
begin
  Result := Symbol.get_baseType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_token(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_token(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_timeStamp(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TUnixTime;
begin
  Result := Symbol.get_timeStamp(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_guid(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TGuid;
begin
  Result := Symbol.get_guid(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_symbolsFileName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_symbolsFileName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_reference(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_reference(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_count(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_count(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_bitPosition(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_bitPosition(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_packed(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_packed(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_constructor(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_constructor(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_overloadedOperator(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_overloadedOperator(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_nested(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_nested(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasNestedTypes(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasNestedTypes(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasAssignmentOperator(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasAssignmentOperator(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasCastOperator(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasCastOperator(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_scoped(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_scoped(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_virtualBaseClass(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_virtualBaseClass(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_indirectVirtualBaseClass(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_indirectVirtualBaseClass(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_virtualBasePointerOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Integer;
begin
  Result := Symbol.get_virtualBasePointerOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_code(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_code(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_function(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_function(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_managed(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_managed(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_msil(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_msil(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_virtualBaseDispIndex(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_virtualBaseDispIndex(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_undecoratedName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_undecoratedName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_age(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_age(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_signature(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_signature(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_compilerGenerated(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_compilerGenerated(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_addressTaken(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_addressTaken(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_rank(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_rank(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_dataBytes(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Buffer: IMemory;
  Size: Cardinal;
begin
  Result := Symbol.get_dataBytes(0, Size, nil) = S_OK;

  if not Result then
    Exit;

  Buffer := Auto.AllocateDynamic(Size);
  Result := Symbol.get_dataBytes(Buffer.Size, Size, Buffer.Data) = S_OK;

  if Result then
    Reflection := FormatBuffer(Buffer, True, True);
end;

function DiaxSymbol_get_targetSection(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_targetSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_targetOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_targetOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_targetRelativeVirtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_targetRelativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_targetVirtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Symbol.get_targetVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_machineType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TImageMachine32;
begin
  Result := Symbol.get_machineType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_oemId(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_oemId(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_oemSymbolId(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_oemSymbolId(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_udtKind(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TUdtKind;
begin
  Result := Symbol.get_udtKind(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_noReturn(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_noReturn(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_customCallingConvention(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_customCallingConvention(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_noInline(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_noInline(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_optimizedCodeDebugInfo(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_optimizedCodeDebugInfo(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_notReached(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_notReached(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_interruptReturn(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_interruptReturn(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_farReturn(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_farReturn(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isStatic(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isStatic(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasDebugInfo(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasDebugInfo(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isLTCG(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isLTCG(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isDataAligned(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isDataAligned(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasSecurityChecks(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasSecurityChecks(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_compilerName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_compilerName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_hasAlloca(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasAlloca(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasSetJump(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasSetJump(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasLongJump(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasLongJump(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasInlAsm(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasInlAsm(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasEH(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasEH(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasSEH(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasSEH(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasEHa(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasEHa(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isNaked(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isNaked(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isAggregated(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isAggregated(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isSplitted(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isSplitted(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_inlSpec(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_inlSpec(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_noStackOrdering(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_noStackOrdering(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasManagedCode(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasManagedCode(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isHotpatchable(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isHotpatchable(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isCVTCIL(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isCVTCIL(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isMSILNetmodule(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isMSILNetmodule(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isCTypes(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isCTypes(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isStripped(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isStripped(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_frontEndQFE(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_frontEndQFE(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_backEndQFE(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_backEndQFE(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_wasInlined(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_wasInlined(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_strictGSCheck(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_strictGSCheck(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isCxxReturnUdt(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isCxxReturnUdt(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isConstructorVirtualBase(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isConstructorVirtualBase(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_RValueReference(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_RValueReference(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_framePointerPresent(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_framePointerPresent(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isSafeBuffers(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isSafeBuffers(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_intrinsic(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_intrinsic(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_sealed(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_sealed(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hfaFloat(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hfaFloat(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hfaDouble(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hfaDouble(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_liveRangeStartAddressSection(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_liveRangeStartAddressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_liveRangeStartAddressOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_liveRangeStartAddressOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_liveRangeStartRelativeVirtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_liveRangeStartRelativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_countLiveRanges(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_countLiveRanges(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_liveRangeLength(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Symbol.get_liveRangeLength(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_offsetInUdt(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_offsetInUdt(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_paramBasePointerRegisterId(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_paramBasePointerRegisterId(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText, rfHint];
    Reflection.Text := LookupRegisterName(Value, CurrentDiaMachineType);
    Reflection.Hint := BuildHint([
      THintSection.New('Value (decimal)', UiLibUIntToDec(Value)),
      THintSection.New('Value (hex)', UiLibUIntToHex(Value))
    ]);
  end;
end;

function DiaxSymbol_get_localBasePointerRegisterId(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_localBasePointerRegisterId(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText, rfHint];
    Reflection.Text := LookupRegisterName(Value, CurrentDiaMachineType);
    Reflection.Hint := BuildHint([
      THintSection.New('Value (decimal)', UiLibUIntToDec(Value)),
      THintSection.New('Value (hex)', UiLibUIntToHex(Value))
    ]);
  end;
end;

function DiaxSymbol_get_isLocationControlFlowDependent(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isLocationControlFlowDependent(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_stride(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_stride(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_numberOfRows(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_numberOfRows(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_numberOfColumns(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_numberOfColumns(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isMatrixRowMajor(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isMatrixRowMajor(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isReturnValue(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isReturnValue(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isOptimizedAway(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isOptimizedAway(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_builtInKind(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_builtInKind(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_registerType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvHlslRegE;
begin
  Result := Symbol.get_registerType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_baseDataSlot(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_baseDataSlot(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_baseDataOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_baseDataOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_textureSlot(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_textureSlot(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_samplerSlot(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_samplerSlot(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_uavSlot(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_uavSlot(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_sizeInUdt(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_sizeInUdt(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_memorySpaceKind(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvHlslMemorySpaceE;
begin
  Result := Symbol.get_memorySpaceKind(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_numberOfModifiers(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_numberOfModifiers(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_numberOfRegisterIndices(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_numberOfRegisterIndices(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isHLSLData(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isHLSLData(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isPointerToDataMember(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isPointerToDataMember(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isPointerToMemberFunction(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isPointerToMemberFunction(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isSingleInheritance(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isSingleInheritance(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isMultipleInheritance(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isMultipleInheritance(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isVirtualInheritance(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isVirtualInheritance(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_restrictedType(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_restrictedType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isPointerBasedOnSymbolValue(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isPointerBasedOnSymbolValue(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_objectFileName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_objectFileName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_isAcceleratorGroupSharedLocal(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isAcceleratorGroupSharedLocal(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isAcceleratorPointerTagLiveRange(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isAcceleratorPointerTagLiveRange(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isAcceleratorStubFunction(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isAcceleratorStubFunction(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_numberOfAcceleratorPointerTags(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_numberOfAcceleratorPointerTags(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isSdl(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isSdl(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isWinRTPointer(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isWinRTPointer(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isRefUdt(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isRefUdt(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isValueUdt(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isValueUdt(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isInterfaceUdt(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isInterfaceUdt(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isPGO(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isPGO(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_hasValidPGOCounts(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasValidPGOCounts(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_isOptimizedForSpeed(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_isOptimizedForSpeed(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_PGOEntryCount(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_PGOEntryCount(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_PGOEdgeCount(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_PGOEdgeCount(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_PGODynamicInstructionCount(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Symbol.get_PGODynamicInstructionCount(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_staticSize(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_staticSize(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_finalLiveStaticSize(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_finalLiveStaticSize(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_phaseName(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Symbol.get_phaseName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSymbol_get_hasControlFlowCheck(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_hasControlFlowCheck(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_constantExport(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_constantExport(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_dataExport(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_dataExport(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_privateExport(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_privateExport(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_noNameExport(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_noNameExport(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_exportHasExplicitlyAssignedOrdinal(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_exportHasExplicitlyAssignedOrdinal(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_exportIsForwarder(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Symbol.get_exportIsForwarder(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_ordinal(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_ordinal(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_frameSize(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_frameSize(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_exceptionHandlerAddressSection(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_exceptionHandlerAddressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_exceptionHandlerAddressOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_exceptionHandlerAddressOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_exceptionHandlerRelativeVirtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_exceptionHandlerRelativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_exceptionHandlerVirtualAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Symbol.get_exceptionHandlerVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol_get_characteristics(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TImageSectionCharacteristics;
begin
  Result := Symbol.get_characteristics(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol_get_bindID(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_bindID(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_bindSpace(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_bindSpace(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol_get_bindSlot(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Symbol.get_bindSlot(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol2_get_isObjCClass(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol2: IDiaSymbol2;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol2, Symbol2) = S_OK) and
    (Symbol2.get_isObjCClass(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol2_get_isObjCCategory(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol2: IDiaSymbol2;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol2, Symbol2) = S_OK) and
    (Symbol2.get_isObjCCategory(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol2_get_isObjCProtocol(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol2: IDiaSymbol2;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol2, Symbol2) = S_OK) and
    (Symbol2.get_isObjCProtocol(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol4_get_noexcept(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol4: IDiaSymbol4;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol4, Symbol4) = S_OK) and
    (Symbol4.get_noexcept(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol5_get_hasAbsoluteAddress(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol5: IDiaSymbol5;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol5, Symbol5) = S_OK) and
    (Symbol5.get_hasAbsoluteAddress(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol6_get_isStaticMemberFunc(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol6: IDiaSymbol6;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol6, Symbol6) = S_OK) and
    (Symbol6.get_isStaticMemberFunc(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol7_get_isSignRet(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol7: IDiaSymbol7;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol7, Symbol7) = S_OK) and
    (Symbol7.get_isSignRet(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol8_get_coroutineKind(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol8: IDiaSymbol8;
  Value: TCvCoroutineKindE;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol8, Symbol8) = S_OK) and
    (Symbol8.get_coroutineKind(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSymbol8_get_associatedSymbolKind(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol8: IDiaSymbol8;
  Value: TCvAssociationKindE;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol8, Symbol8) = S_OK) and
    (Symbol8.get_associatedSymbolKind(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol8_get_associatedSymbolSection(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol8: IDiaSymbol8;
  Value: Cardinal;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol8, Symbol8) = S_OK) and
    (Symbol8.get_associatedSymbolSection(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol8_get_associatedSymbolOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol8: IDiaSymbol8;
  Value: Cardinal;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol8, Symbol8) = S_OK) and
    (Symbol8.get_associatedSymbolOffset(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol8_get_associatedSymbolRva(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol8: IDiaSymbol8;
  Value: Cardinal;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol8, Symbol8) = S_OK) and
    (Symbol8.get_associatedSymbolRva(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol8_get_associatedSymbolAddr(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol8: IDiaSymbol8;
  Value: UInt64;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol8, Symbol8) = S_OK) and
    (Symbol8.get_associatedSymbolAddr(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol9_get_framePadSize(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol9: IDiaSymbol9;
  Value: Cardinal;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol9, Symbol9) = S_OK) and
    (Symbol9.get_framePadSize(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol9_get_framePadOffset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol9: IDiaSymbol9;
  Value: Cardinal;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol9, Symbol9) = S_OK) and
    (Symbol9.get_framePadOffset(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSymbol9_get_isRTCs(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol9: IDiaSymbol9;
  Value: LongBool;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol9, Symbol9) = S_OK) and
    (Symbol9.get_isRTCs(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSymbol11_get_discriminatedUnionTag_Offset(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol11: IDiaSymbol11;
  ValueSymbol: IDiaSymbol;
  ValueOffset: Cardinal;
  ValueMask: TDiaTagValue;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol11, Symbol11) = S_OK) and
    (Symbol11.get_discriminatedUnionTag(ValueSymbol, ValueOffset,
    ValueMask) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(ValueOffset, RttixAsHex);
end;

function DiaxSymbol11_get_discriminatedUnionTag_Mask(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Symbol11: IDiaSymbol11;
  ValueSymbol: IDiaSymbol;
  ValueOffset: Cardinal;
  ValueMask: TDiaTagValue;
  Value: UInt64;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol11, Symbol11) = S_OK) and
    (Symbol11.get_discriminatedUnionTag(ValueSymbol, ValueOffset,
    ValueMask) = S_OK);

  if Result then
  begin
    case ValueMask.valueSizeBytes of
      1: Value := ValueMask.value.data8;
      2: Value := ValueMask.value.data16;
      4: Value := ValueMask.value.data32;
      8: Value := ValueMask.value.data64;
    else
      Reflection.ValidFormats := [rfText];
      Reflection.Text := RtlxFormatString('(A DiaTagValue with %u bytes)',
        [Cardinal(ValueMask.valueSizeBytes)]);
      Reflection.Hint := '';
      Exit;
    end;

    Reflection := Rttix.FormatFull(Value, RttixAsHex);
  end;
end;

function DiaxSymbol_getSourceLinkInfo(
  const Symbol: IDiaSymbol;
  out Reflection: TRttixFullReflection
): Boolean;
var
  SessionEx: IDiaSessionEx;
  Enum: IDiaEnumSourceLink;
  Value: Cardinal;
begin
  Result := Assigned(CurrentDiaSession) and
    (CurrentDiaSession.QueryInterface(IDiaSession, SessionEx) = S_OK) and
    (SessionEx.getSourceLinkInfo(Symbol, Enum) = S_OK) and
    (Enum.Count(Value) = S_OK);

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxFormatString('(%u Source Link Blobs)', [Value]);
    Reflection.Hint := ''
  end;
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxSymbolReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaSymbol>;
  end;

const
  DiaxpSymbolToReflectionDef: array [0..206] of TDiaxSymbolReflectionProperty = (
    (Name: 'Tag'; Callback: DiaxSymbol_get_symTag),
    (Name: 'Name'; Callback: DiaxSymbol_get_name),
    (Name: 'Data Kind'; Callback: DiaxSymbol_get_dataKind),
    (Name: 'Location Type'; Callback: DiaxSymbol_get_locationType),
    (Name: 'Address Section'; Callback: DiaxSymbol_get_addressSection),
    (Name: 'Address Offset'; Callback: DiaxSymbol_get_addressOffset),
    (Name: 'Relative Virtual Address'; Callback: DiaxSymbol_get_relativeVirtualAddress),
    (Name: 'Virtual Address'; Callback: DiaxSymbol_get_virtualAddress),
    (Name: 'Register Id'; Callback: DiaxSymbol_get_registerId),
    (Name: 'Offset'; Callback: DiaxSymbol_get_offset),
    (Name: 'Length'; Callback: DiaxSymbol_get_length),
    (Name: 'Slot'; Callback: DiaxSymbol_get_slot),
    (Name: 'Volatile'; Callback: DiaxSymbol_get_volatileType),
    (Name: 'Const'; Callback: DiaxSymbol_get_constType),
    (Name: 'Unaligned'; Callback: DiaxSymbol_get_unalignedType),
    (Name: 'Access'; Callback: DiaxSymbol_get_access),
    (Name: 'Library Name'; Callback: DiaxSymbol_get_libraryName),
    (Name: 'Platform'; Callback: DiaxSymbol_get_platform),
    (Name: 'Language'; Callback: DiaxSymbol_get_language),
    (Name: 'Edit And Continue Enabled'; Callback: DiaxSymbol_get_editAndContinueEnabled),
    (Name: 'Frontend Major'; Callback: DiaxSymbol_get_frontEndMajor),
    (Name: 'Frontend Minor'; Callback: DiaxSymbol_get_frontEndMinor),
    (Name: 'Frontend Build'; Callback: DiaxSymbol_get_frontEndBuild),
    (Name: 'Frontend QFE'; Callback: DiaxSymbol_get_frontEndQFE),
    (Name: 'Backend Major'; Callback: DiaxSymbol_get_backEndMajor),
    (Name: 'Backend Minor'; Callback: DiaxSymbol_get_backEndMinor),
    (Name: 'Backend Build'; Callback: DiaxSymbol_get_backEndBuild),
    (Name: 'Backend QFE'; Callback: DiaxSymbol_get_backEndQFE),
    (Name: 'Source File Name'; Callback: DiaxSymbol_get_sourceFileName),
    (Name: 'Thunk Ordinal'; Callback: DiaxSymbol_get_thunkOrdinal),
    (Name: 'This Adjust'; Callback: DiaxSymbol_get_thisAdjust),
    (Name: 'Virtual Base Offset'; Callback: DiaxSymbol_get_virtualBaseOffset),
    (Name: 'Virtual'; Callback: DiaxSymbol_get_virtual),
    (Name: 'Introducing Virtual Function'; Callback: DiaxSymbol_get_intro),
    (Name: 'Pure'; Callback: DiaxSymbol_get_pure),
    (Name: 'Calling Convention'; Callback: DiaxSymbol_get_callingConvention),
    (Name: 'Value'; Callback: DiaxSymbol_get_value),
    (Name: 'Base Type'; Callback: DiaxSymbol_get_baseType),
    (Name: 'Token'; Callback: DiaxSymbol_get_token),
    (Name: 'Timestamp'; Callback: DiaxSymbol_get_timeStamp),
    (Name: 'GUID'; Callback: DiaxSymbol_get_guid),
    (Name: 'Symbols File Name'; Callback: DiaxSymbol_get_symbolsFileName),
    (Name: 'Reference'; Callback: DiaxSymbol_get_reference),
    (Name: 'Count'; Callback: DiaxSymbol_get_count),
    (Name: 'Bit Position'; Callback: DiaxSymbol_get_bitPosition),
    (Name: 'Packed'; Callback: DiaxSymbol_get_packed),
    (Name: 'Constructor'; Callback: DiaxSymbol_get_constructor),
    (Name: 'Overloaded Operator'; Callback: DiaxSymbol_get_overloadedOperator),
    (Name: 'Nested'; Callback: DiaxSymbol_get_nested),
    (Name: 'Has Nested Types'; Callback: DiaxSymbol_get_hasNestedTypes),
    (Name: 'Has Assignment Operator'; Callback: DiaxSymbol_get_hasAssignmentOperator),
    (Name: 'Has Cast Operator'; Callback: DiaxSymbol_get_hasCastOperator),
    (Name: 'Scoped'; Callback: DiaxSymbol_get_scoped),
    (Name: 'Virtual Base Class'; Callback: DiaxSymbol_get_virtualBaseClass),
    (Name: 'Indirect Virtual Base Class'; Callback: DiaxSymbol_get_indirectVirtualBaseClass),
    (Name: 'Virtual Base Pointer Offset'; Callback: DiaxSymbol_get_virtualBasePointerOffset),
    (Name: 'Code'; Callback: DiaxSymbol_get_code),
    (Name: 'Function'; Callback: DiaxSymbol_get_function),
    (Name: 'Managed'; Callback: DiaxSymbol_get_managed),
    (Name: 'MSIL'; Callback: DiaxSymbol_get_msil),
    (Name: 'Virtual Base Disp Index'; Callback: DiaxSymbol_get_virtualBaseDispIndex),
    (Name: 'Undecorated Name'; Callback: DiaxSymbol_get_undecoratedName),
    (Name: 'Age'; Callback: DiaxSymbol_get_age),
    (Name: 'Signature'; Callback: DiaxSymbol_get_signature),
    (Name: 'Compiler-generated'; Callback: DiaxSymbol_get_compilerGenerated),
    (Name: 'Address Taken'; Callback: DiaxSymbol_get_addressTaken),
    (Name: 'Rank'; Callback: DiaxSymbol_get_rank),
    (Name: 'Data Bytes'; Callback: DiaxSymbol_get_dataBytes),
    (Name: 'Target Section'; Callback: DiaxSymbol_get_targetSection),
    (Name: 'Target Offset'; Callback: DiaxSymbol_get_targetOffset),
    (Name: 'Target RVA'; Callback: DiaxSymbol_get_targetRelativeVirtualAddress),
    (Name: 'Target VA'; Callback: DiaxSymbol_get_targetVirtualAddress),
    (Name: 'Machine Type'; Callback: DiaxSymbol_get_machineType),
    (Name: 'OEM ID'; Callback: DiaxSymbol_get_oemId),
    (Name: 'OEM Symbol ID'; Callback: DiaxSymbol_get_oemSymbolId),
    (Name: 'UDT Kind'; Callback: DiaxSymbol_get_udtKind),
    (Name: 'No-return'; Callback: DiaxSymbol_get_noReturn),
    (Name: 'Custom Calling Convention'; Callback: DiaxSymbol_get_customCallingConvention),
    (Name: 'No-inline'; Callback: DiaxSymbol_get_noInline),
    (Name: 'Optimized Code Debug Info'; Callback: DiaxSymbol_get_optimizedCodeDebugInfo),
    (Name: 'Not Reached'; Callback: DiaxSymbol_get_notReached),
    (Name: 'Interrupt Return'; Callback: DiaxSymbol_get_interruptReturn),
    (Name: 'Far Return'; Callback: DiaxSymbol_get_farReturn),
    (Name: 'Static'; Callback: DiaxSymbol_get_isStatic),
    (Name: 'Has Debug Info'; Callback: DiaxSymbol_get_hasDebugInfo),
    (Name: 'LTCG'; Callback: DiaxSymbol_get_isLTCG),
    (Name: 'Data Aligned'; Callback: DiaxSymbol_get_isDataAligned),
    (Name: 'Has Security Checks'; Callback: DiaxSymbol_get_hasSecurityChecks),
    (Name: 'Compiler Name'; Callback: DiaxSymbol_get_compilerName),
    (Name: 'Has Alloca'; Callback: DiaxSymbol_get_hasAlloca),
    (Name: 'Has Set Jump'; Callback: DiaxSymbol_get_hasSetJump),
    (Name: 'Has Long Jump'; Callback: DiaxSymbol_get_hasLongJump),
    (Name: 'Has Inline Assembly'; Callback: DiaxSymbol_get_hasInlAsm),
    (Name: 'Has EH'; Callback: DiaxSymbol_get_hasEH),
    (Name: 'Has SEH'; Callback: DiaxSymbol_get_hasSEH),
    (Name: 'Has EHa'; Callback: DiaxSymbol_get_hasEHa),
    (Name: 'Naked'; Callback: DiaxSymbol_get_isNaked),
    (Name: 'Aggregated'; Callback: DiaxSymbol_get_isAggregated),
    (Name: 'Splitted'; Callback: DiaxSymbol_get_isSplitted),
    (Name: 'Inline Specified'; Callback: DiaxSymbol_get_inlSpec),
    (Name: 'No Stack Ordering'; Callback: DiaxSymbol_get_noStackOrdering),
    (Name: 'Has Managed Code'; Callback: DiaxSymbol_get_hasManagedCode),
    (Name: 'Hotpatchable'; Callback: DiaxSymbol_get_isHotpatchable),
    (Name: 'CVTCIL'; Callback: DiaxSymbol_get_isCVTCIL),
    (Name: 'MSIL Netmodule'; Callback: DiaxSymbol_get_isMSILNetmodule),
    (Name: 'C Types'; Callback: DiaxSymbol_get_isCTypes),
    (Name: 'Stripped'; Callback: DiaxSymbol_get_isStripped),
    (Name: 'Was Inlined'; Callback: DiaxSymbol_get_wasInlined),
    (Name: 'Strict GS Check'; Callback: DiaxSymbol_get_strictGSCheck),
    (Name: 'C++ Return UDT'; Callback: DiaxSymbol_get_isCxxReturnUdt),
    (Name: 'Constructor Virtual Base'; Callback: DiaxSymbol_get_isConstructorVirtualBase),
    (Name: 'RValueReference'; Callback: DiaxSymbol_get_RValueReference),
    (Name: 'Frame Pointer Present'; Callback: DiaxSymbol_get_framePointerPresent),
    (Name: 'Safe Buffers'; Callback: DiaxSymbol_get_isSafeBuffers),
    (Name: 'Intrinsic'; Callback: DiaxSymbol_get_intrinsic),
    (Name: 'Sealed'; Callback: DiaxSymbol_get_sealed),
    (Name: 'HFA Float'; Callback: DiaxSymbol_get_hfaFloat),
    (Name: 'HFA Double'; Callback: DiaxSymbol_get_hfaDouble),
    (Name: 'Live Range Start Address Section'; Callback: DiaxSymbol_get_liveRangeStartAddressSection),
    (Name: 'Live Range Start Address Offset'; Callback: DiaxSymbol_get_liveRangeStartAddressOffset),
    (Name: 'Live Range Start RVA'; Callback: DiaxSymbol_get_liveRangeStartRelativeVirtualAddress),
    (Name: 'Count Live Ranges'; Callback: DiaxSymbol_get_countLiveRanges),
    (Name: 'Live Range Length'; Callback: DiaxSymbol_get_liveRangeLength),
    (Name: 'Offset In UDT'; Callback: DiaxSymbol_get_offsetInUdt),
    (Name: 'Param Base Pointer Register ID'; Callback: DiaxSymbol_get_paramBasePointerRegisterId),
    (Name: 'Local Base Pointer Register ID'; Callback: DiaxSymbol_get_localBasePointerRegisterId),
    (Name: 'Location Control Flow Dependent'; Callback: DiaxSymbol_get_isLocationControlFlowDependent),
    (Name: 'Stride'; Callback: DiaxSymbol_get_stride),
    (Name: 'Number Of Rows'; Callback: DiaxSymbol_get_numberOfRows),
    (Name: 'Number Of Columns'; Callback: DiaxSymbol_get_numberOfColumns),
    (Name: 'Matrix Row Major'; Callback: DiaxSymbol_get_isMatrixRowMajor),
    (Name: 'Return Value'; Callback: DiaxSymbol_get_isReturnValue),
    (Name: 'Optimized Away'; Callback: DiaxSymbol_get_isOptimizedAway),
    (Name: 'Built-in Kind'; Callback: DiaxSymbol_get_builtInKind),
    (Name: 'Register Type'; Callback: DiaxSymbol_get_registerType),
    (Name: 'Base Data Slot'; Callback: DiaxSymbol_get_baseDataSlot),
    (Name: 'Base Data Offset'; Callback: DiaxSymbol_get_baseDataOffset),
    (Name: 'Texture Slot'; Callback: DiaxSymbol_get_textureSlot),
    (Name: 'Sampler Slot'; Callback: DiaxSymbol_get_samplerSlot),
    (Name: 'UAV Slot'; Callback: DiaxSymbol_get_uavSlot),
    (Name: 'Size In UDT'; Callback: DiaxSymbol_get_sizeInUdt),
    (Name: 'Memory Space Kind'; Callback: DiaxSymbol_get_memorySpaceKind),
    (Name: 'Number Of Modifiers'; Callback: DiaxSymbol_get_numberOfModifiers),
    (Name: 'Number Of Register Indices'; Callback: DiaxSymbol_get_numberOfRegisterIndices),
    (Name: 'HLSL Data'; Callback: DiaxSymbol_get_isHLSLData),
    (Name: 'Pointer To Data Member'; Callback: DiaxSymbol_get_isPointerToDataMember),
    (Name: 'Pointer To Member Function'; Callback: DiaxSymbol_get_isPointerToMemberFunction),
    (Name: 'Single Inheritance'; Callback: DiaxSymbol_get_isSingleInheritance),
    (Name: 'Multiple Inheritance'; Callback: DiaxSymbol_get_isMultipleInheritance),
    (Name: 'Virtual Inheritance'; Callback: DiaxSymbol_get_isVirtualInheritance),
    (Name: 'Restricted Type'; Callback: DiaxSymbol_get_restrictedType),
    (Name: 'Pointer Based On Symbol Value'; Callback: DiaxSymbol_get_isPointerBasedOnSymbolValue),
    (Name: 'Object Filename'; Callback: DiaxSymbol_get_objectFileName),
    (Name: 'Accelerator Group Shared Local'; Callback: DiaxSymbol_get_isAcceleratorGroupSharedLocal),
    (Name: 'Accelerator Pointer Tag Live Range'; Callback: DiaxSymbol_get_isAcceleratorPointerTagLiveRange),
    (Name: 'Accelerator Stub Function'; Callback: DiaxSymbol_get_isAcceleratorStubFunction),
    (Name: 'Number Of Accelerator Pointer Tags'; Callback: DiaxSymbol_get_numberOfAcceleratorPointerTags),
    (Name: 'SDL'; Callback: DiaxSymbol_get_isSdl),
    (Name: 'WinRT Pointer'; Callback: DiaxSymbol_get_isWinRTPointer),
    (Name: 'Ref UDT'; Callback: DiaxSymbol_get_isRefUdt),
    (Name: 'Value UDT'; Callback: DiaxSymbol_get_isValueUdt),
    (Name: 'Interface UDT'; Callback: DiaxSymbol_get_isInterfaceUdt),
    (Name: 'PGO'; Callback: DiaxSymbol_get_isPGO),
    (Name: 'Has Valid PGO Counts'; Callback: DiaxSymbol_get_hasValidPGOCounts),
    (Name: 'Optimized For Speed'; Callback: DiaxSymbol_get_isOptimizedForSpeed),
    (Name: 'PGO Entry Count'; Callback: DiaxSymbol_get_PGOEntryCount),
    (Name: 'PGO Edge Count'; Callback: DiaxSymbol_get_PGOEdgeCount),
    (Name: 'PGO Dynamic Instruction Count'; Callback: DiaxSymbol_get_PGODynamicInstructionCount),
    (Name: 'Static Size'; Callback: DiaxSymbol_get_staticSize),
    (Name: 'Final Live Static Size'; Callback: DiaxSymbol_get_finalLiveStaticSize),
    (Name: 'Phase Name'; Callback: DiaxSymbol_get_phaseName),
    (Name: 'Has Control Flow Check'; Callback: DiaxSymbol_get_hasControlFlowCheck),
    (Name: 'Constant Export'; Callback: DiaxSymbol_get_constantExport),
    (Name: 'Data Export'; Callback: DiaxSymbol_get_dataExport),
    (Name: 'Private Export'; Callback: DiaxSymbol_get_privateExport),
    (Name: 'No-name Export'; Callback: DiaxSymbol_get_noNameExport),
    (Name: 'Export Has Explicitly Assigned Ordinal'; Callback: DiaxSymbol_get_exportHasExplicitlyAssignedOrdinal),
    (Name: 'Export Is Forwarder'; Callback: DiaxSymbol_get_exportIsForwarder),
    (Name: 'Ordinal'; Callback: DiaxSymbol_get_ordinal),
    (Name: 'Frame Size'; Callback: DiaxSymbol_get_frameSize),
    (Name: 'Exception Handler Address Section'; Callback: DiaxSymbol_get_exceptionHandlerAddressSection),
    (Name: 'Exception Handler Address Offset'; Callback: DiaxSymbol_get_exceptionHandlerAddressOffset),
    (Name: 'Exception Handler RVA'; Callback: DiaxSymbol_get_exceptionHandlerRelativeVirtualAddress),
    (Name: 'Exception Handler VA'; Callback: DiaxSymbol_get_exceptionHandlerVirtualAddress),
    (Name: 'Characteristics'; Callback: DiaxSymbol_get_characteristics),
    (Name: 'Bind ID'; Callback: DiaxSymbol_get_bindID),
    (Name: 'Bind Space'; Callback: DiaxSymbol_get_bindSpace),
    (Name: 'Bind Slot'; Callback: DiaxSymbol_get_bindSlot),
    (Name: 'Objective-C Class'; Callback: DiaxSymbol2_get_isObjCClass),
    (Name: 'Objective-C Category'; Callback: DiaxSymbol2_get_isObjCCategory),
    (Name: 'Objective-C Protocol'; Callback: DiaxSymbol2_get_isObjCProtocol),
    (Name: 'No-except'; Callback: DiaxSymbol4_get_noexcept),
    (Name: 'Has Absolute Address'; Callback: DiaxSymbol5_get_hasAbsoluteAddress),
    (Name: 'Static Member Function'; Callback: DiaxSymbol6_get_isStaticMemberFunc),
    (Name: 'Sign Return'; Callback: DiaxSymbol7_get_isSignRet),
    (Name: 'Coroutine Kind'; Callback: DiaxSymbol8_get_coroutineKind),
    (Name: 'Associated Symbol Kind'; Callback: DiaxSymbol8_get_associatedSymbolKind),
    (Name: 'Associated Symbol Section'; Callback: DiaxSymbol8_get_associatedSymbolSection),
    (Name: 'Associated Symbol Offset'; Callback: DiaxSymbol8_get_associatedSymbolOffset),
    (Name: 'Associated Symbol RVA'; Callback: DiaxSymbol8_get_associatedSymbolRva),
    (Name: 'Associated Symbol Address'; Callback: DiaxSymbol8_get_associatedSymbolAddr),
    (Name: 'Frame Pad Size'; Callback: DiaxSymbol9_get_framePadSize),
    (Name: 'Frame Pad Offset'; Callback: DiaxSymbol9_get_framePadOffset),
    (Name: 'RTCs'; Callback: DiaxSymbol9_get_isRTCs),
    (Name: 'Discriminated Union Tag Offset'; Callback: DiaxSymbol11_get_discriminatedUnionTag_Offset),
    (Name: 'Discriminated Union Tag Mask'; Callback: DiaxSymbol11_get_discriminatedUnionTag_Mask),
    (Name: 'Source Link Info'; Callback: DiaxSymbol_getSourceLinkInfo)
  );

var
  DiaxpSymbolToReflectionCache: TArray<TDiaxReflectionProperty<IDiaSymbol>>;

function DiaxSymbolToReflection;
var
  i: Integer;
begin
  if Length(DiaxpSymbolToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpSymbolToReflectionCache, Length(DiaxpSymbolToReflectionDef));

    for i := 0 to High(DiaxpSymbolToReflectionCache) do
    begin
      DiaxpSymbolToReflectionCache[i].Name :=
        DiaxpSymbolToReflectionDef[i].Name;
      DiaxpSymbolToReflectionCache[i].Callback :=
        DiaxpSymbolToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpSymbolToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to IDiaSymbol'}

function DiaxSymbol_get_lexicalParent(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_lexicalParent(Value) = S_OK;
end;

function DiaxSymbol_get_classParent(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_classParent(Value) = S_OK;
end;

function DiaxSymbol_get_type(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_type(Value) = S_OK;
end;

function DiaxSymbol_get_arrayIndexType(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_arrayIndexType(Value) = S_OK;
end;

function DiaxSymbol_get_virtualTableShape(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_virtualTableShape(Value) = S_OK;
end;

function DiaxSymbol_get_lowerBound(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_lowerBound(Value) = S_OK;
end;

function DiaxSymbol_get_upperBound(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_upperBound(Value) = S_OK;
end;

function DiaxSymbol_get_objectPointerType(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_objectPointerType(Value) = S_OK;
end;

function DiaxSymbol_get_container(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_container(Value) = S_OK;
end;

function DiaxSymbol_get_virtualBaseTableType(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_virtualBaseTableType(Value) = S_OK;
end;

function DiaxSymbol_get_unmodifiedType(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_unmodifiedType(Value) = S_OK;
end;

function DiaxSymbol_get_subType(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_subType(Value) = S_OK;
end;

function DiaxSymbol_get_baseSymbol(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_baseSymbol(Value) = S_OK;
end;

function DiaxSymbol_get_coffGroup(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
begin
  Result := Symbol.get_coffGroup(Value) = S_OK;
end;

function DiaxSymbol3_get_inlinee(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
var
  Symbol3: IDiaSymbol3;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol3, Symbol3) = S_OK) and
    (Symbol3.get_inlinee(Value) = S_OK);
end;

function DiaxSymbol11_get_discriminatedUnionTag_Type(
  const Symbol: IDiaSymbol;
  out Value: IDiaSymbol
): Boolean;
var
  Symbol11: IDiaSymbol11;
  ValueOffset: Cardinal;
  ValueMask: TDiaTagValue;
begin
  Result := (Symbol.QueryInterface(IDiaSymbol11, Symbol11) = S_OK) and
    (Symbol11.get_discriminatedUnionTag(Value, ValueOffset, ValueMask) = S_OK);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxSymbolSymbolProperty = record
    Name: String;
    Callback: TDiaxInstancePropertyCallback<IDiaSymbol, IDiaSymbol>;
  end;

const
  DiaxpSymbolToSymbolDef: array [0..15] of TDiaxSymbolSymbolProperty = (
    (Name: 'Lexical Parent'; Callback: DiaxSymbol_get_lexicalParent),
    (Name: 'Class Parent'; Callback: DiaxSymbol_get_classParent),
    (Name: 'Type'; Callback: DiaxSymbol_get_type),
    (Name: 'Array Index Type'; Callback: DiaxSymbol_get_arrayIndexType),
    (Name: 'Virtual Table Shape'; Callback: DiaxSymbol_get_virtualTableShape),
    (Name: 'Lower Bound'; Callback: DiaxSymbol_get_lowerBound),
    (Name: 'Upper Bound'; Callback: DiaxSymbol_get_upperBound),
    (Name: 'Object Pointer Type'; Callback: DiaxSymbol_get_objectPointerType),
    (Name: 'Container'; Callback: DiaxSymbol_get_container),
    (Name: 'Virtual Base Table Type'; Callback: DiaxSymbol_get_virtualBaseTableType),
    (Name: 'Unmodified Type'; Callback: DiaxSymbol_get_unmodifiedType),
    (Name: 'Subtype'; Callback: DiaxSymbol_get_subType),
    (Name: 'Base Symbol'; Callback: DiaxSymbol_get_baseSymbol),
    (Name: 'Coff Group'; Callback: DiaxSymbol_get_coffGroup),
    (Name: 'Inlinee'; Callback: DiaxSymbol3_get_inlinee),
    (Name: 'Discriminated Union Tag Type'; Callback: DiaxSymbol11_get_discriminatedUnionTag_Type)
  );

var
  DiaxpSymbolToSymbolCache: TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaSymbol>>;

function DiaxSymbolToSymbol;
var
  i: Integer;
begin
  if Length(DiaxpSymbolToSymbolCache) <= 0 then
  begin
    SetLength(DiaxpSymbolToSymbolCache, Length(DiaxpSymbolToSymbolDef));

    for i := 0 to High(DiaxpSymbolToSymbolCache) do
    begin
      DiaxpSymbolToSymbolCache[i].Name := DiaxpSymbolToSymbolDef[i].Name;
      DiaxpSymbolToSymbolCache[i].Callback := DiaxpSymbolToSymbolDef[i].Callback;
    end;
  end;

  Result := DiaxpSymbolToSymbolCache;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to IDiaEnum<IDiaSymbol>'}

function DiaxSymbol_findFile(
  const Symbol: IDiaSymbol;
  out Enum: IDiaEnum<IDiaSourceFile>
): Boolean;
begin
  Result := Assigned(CurrentDiaSession) and (CurrentDiaSession.findFile(Symbol,
    '', 0, IDiaEnumSourceFiles(Enum)) = S_OK);
end;

function DiaxSymbolToSourceFileEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Files';
  Result[0].Callback := DiaxSymbol_findFile;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to IDiaEnum<IDiaSymbol>'}

function DiaxSymbol_get_types(
  const Symbol: IDiaSymbol;
  out Enum: IDiaEnum<IDiaSymbol>
): Boolean;
var
  Required: Cardinal;
  Elements: TArray<IDiaSymbol>;
begin
  Result := Symbol.get_types(0, Required, nil) = S_OK;

  if not Result then
    Exit;

  if Required > 0 then
  begin
    SetLength(Elements, Required);
    Result := Symbol.get_types(Length(Elements), Required, Elements[0]) = S_OK;

    if not Result then
      Exit;

    SetLength(Elements, Required);
  end
  else
    Elements := nil;

  Enum := TFakeDiaEnum<IDiaSymbol>.Create(Elements);
end;

function DiaxSymbol_findChildren(
  const Symbol: IDiaSymbol;
  out Enum: IDiaEnum<IDiaSymbol>
): Boolean;
begin
  Result := Symbol.findChildren(SymTagNull, '', 0, IDiaEnumSymbols(Enum)) = S_OK;
end;

function DiaxSymbolToSymbolEnum;
begin
  SetLength(Result, 2);
  Result[0].Name := 'Types';
  Result[0].Callback := DiaxSymbol_get_types;
  Result[1].Name := 'Children';
  Result[1].Callback := DiaxSymbol_findChildren;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to IDiaLineNumber'}

function DiaxSymbol_getSrcLineOnTypeDefn(
  const Symbol: IDiaSymbol;
  out LineNumber: IDiaLineNumber
): Boolean;
begin
  Result := Symbol.getSrcLineOnTypeDefn(LineNumber) = S_OK;
end;

function DiaxSymbolToLineNumber;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Source Line On Type Definition';
  Result[0].Callback := DiaxSymbol_getSrcLineOnTypeDefn;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to IDiaEnum<IDiaLineNumber>'}

function DiaxSymbol_findInlineeLines(
  const Symbol: IDiaSymbol;
  out Enum: IDiaEnum<IDiaLineNumber>
): Boolean;
begin
  Result := Symbol.findInlineeLines(IDiaEnumLineNumbers(Enum)) = S_OK;
end;

function DiaxSymbolToLineNumberEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Inlinee Lines';
  Result[0].Callback := DiaxSymbol_findInlineeLines;
end;

{$ENDREGION}
{$REGION 'IDiaSymbol to IDiaInputAssemblyFile'}

function DiaxSymbol_findInputAssemblyFile(
  const Symbol: IDiaSymbol;
  out AssemblyFile: IDiaInputAssemblyFile
): Boolean;
begin
  Result := Symbol.findInputAssemblyFile(AssemblyFile) = S_OK;
end;

function DiaxSymbolToAssemblyFile;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Input Assembly File';
  Result[0].Callback := DiaxSymbol_findInputAssemblyFile;
end;

{$ENDREGION}
{$REGION 'IDiaSourceFile sorting'}

procedure DiaxSortSourceFiles;
var
  Names: TArray<String>;
  WideName: WideString;
  i: Integer;
begin
  // Collect names for sorting
  SetLength(Names, Length(SourceFiles));

  for i := 0 to High(SourceFiles) do
    if SourceFiles[i].get_fileName(WideName) = S_OK then
      Names[i] := RtlxExtractNamePath(WideName);

  // Sort by name
  TArray.SortIndexInline<IDiaSourceFile>(SourceFiles,
    function (const IndexA, IndexB: Integer): Integer
    begin
      Result := RtlxCompareStrings(Names[IndexA], Names[IndexB]);
    end
  );
end;

{$ENDREGION}
{$REGION 'IDiaSourceFile to reflection'}

function DiaxSourceFilee_get_fileName(
  const SourceFile: IDiaSourceFile;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := SourceFile.get_fileName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxSourceFilee_get_checksumType(
  const SourceFile: IDiaSourceFile;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TCvSourceChecksumT;
begin
  Result := SourceFile.get_checksumType(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

function DiaxSourceFilee_get_checksum(
  const SourceFile: IDiaSourceFile;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Size: Cardinal;
  Buffer: IMemory;
begin
  Result := (SourceFile.get_checksum(0, Size, nil) = S_OK) and (Size > 0);

  if not Result then
    Exit;

  Buffer := Auto.AllocateDynamic(Size);
  Result := SourceFile.get_checksum(Buffer.Size, Size, Buffer.Data) = S_OK;

  if Result then
    Reflection := FormatBuffer(Buffer, False, False);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxSourceFileReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaSourceFile>;
  end;

const
  DiaxpSourceFileToReflectionDef: array [0..2] of TDiaxSourceFileReflectionProperty = (
    (Name: 'File Name'; Callback: DiaxSourceFilee_get_fileName),
    (Name: 'Checksum Type'; Callback: DiaxSourceFilee_get_checksumType),
    (Name: 'Checksum'; Callback: DiaxSourceFilee_get_checksum)
  );

var
  DiaxpSourceFileToReflectionCache: TArray<TDiaxReflectionProperty<IDiaSourceFile>>;

function DiaxSourceFileToReflection;
var
  i: Integer;
begin
  if Length(DiaxpSourceFileToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpSourceFileToReflectionCache, Length(DiaxpSourceFileToReflectionDef));

    for i := 0 to High(DiaxpSourceFileToReflectionCache) do
    begin
      DiaxpSourceFileToReflectionCache[i].Name :=
        DiaxpSourceFileToReflectionDef[i].Name;
      DiaxpSourceFileToReflectionCache[i].Callback :=
        DiaxpSourceFileToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpSourceFileToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaInputAssemblyFile to reflection'}

function DiaxAssemblyFile_get_timestamp(
  const AssemblyFile: IDiaInputAssemblyFile;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TUnixTime;
begin
  Result := AssemblyFile.get_timestamp(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxAssemblyFile_get_pdbAvailableAtILMerge(
  const AssemblyFile: IDiaInputAssemblyFile;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := AssemblyFile.get_pdbAvailableAtILMerge(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxAssemblyFile_get_fileName(
  const AssemblyFile: IDiaInputAssemblyFile;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := AssemblyFile.get_fileName(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxAssemblyFileReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaInputAssemblyFile>;
  end;

const
  DiaxpAssemblyFileToReflectionDef: array [0..2] of TDiaxAssemblyFileReflectionProperty = (
    (Name: 'Timestamp'; Callback: DiaxAssemblyFile_get_timestamp),
    (Name: 'PDB Available At IL Merge'; Callback: DiaxAssemblyFile_get_pdbAvailableAtILMerge),
    (Name: 'File Name'; Callback: DiaxAssemblyFile_get_fileName)
  );

var
  DiaxpAssemblyFileToReflectionCache: TArray<TDiaxReflectionProperty<IDiaInputAssemblyFile>>;

function DiaxAssemblyFileToReflection;
var
  i: Integer;
begin
  if Length(DiaxpAssemblyFileToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpAssemblyFileToReflectionCache,
      Length(DiaxpAssemblyFileToReflectionDef));

    for i := 0 to High(DiaxpAssemblyFileToReflectionCache) do
    begin
      DiaxpAssemblyFileToReflectionCache[i].Name :=
        DiaxpAssemblyFileToReflectionDef[i].Name;
      DiaxpAssemblyFileToReflectionCache[i].Callback :=
        DiaxpAssemblyFileToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpAssemblyFileToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaLineNumber sorting'}

procedure DiaxSortLineNumbers;
var
  Names: TArray<String>;
  FileIDs: TArray<Cardinal>;
  Numbers: TArray<Cardinal>;
  SourceFile: IDiaSourceFile;
  WideName: WideString;
  i: Integer;
begin
  // Collect filenames and numbers for sorting
  SetLength(Names, Length(Lines));
  SetLength(FileIDs, Length(Lines));
  SetLength(Numbers, Length(Lines));

  for i := 0 to High(Lines) do
  begin
    if Lines[i].get_sourceFile(SourceFile) = S_OK then
    begin
      // Use only the name part, not the entire path
      if SourceFile.get_fileName(WideName) = S_OK then
        Names[i] := RtlxExtractNamePath(WideName);

      if SourceFile.get_uniqueId(FileIDs[i]) <> S_OK then
        FileIDs[i] := 0;
    end;

    if Lines[i].get_lineNumber(Numbers[i]) <> S_OK then
      Numbers[i] := 0;
  end;

  // Sort by name and then by number
  TArray.SortIndexInline<IDiaLineNumber>(Lines,
    function (const IndexA, IndexB: Integer): Integer
    begin
      if FileIDs[IndexA] = FileIDs[IndexB] then
        Result := 0
      else
        Result := RtlxCompareStrings(Names[IndexA], Names[IndexB]);

      if Result = 0 then
      begin
        {$R-}{$Q-}
        Result := Integer(Numbers[IndexA]) - Integer(Numbers[IndexB]);
        {$IFDEF Q+}{$Q+}{$ENDIF}{$IFDEF R+}{$R+}{$ENDIF}
      end;
    end
  );
end;

{$ENDREGION}
{$REGION 'IDiaLineNumber to reflection'}

function DiaxLine_get_lineNumber(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_lineNumber(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxLine_get_lineNumberEnd(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_lineNumberEnd(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxLine_get_columnNumber(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_columnNumber(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxLine_get_columnNumberEnd(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_columnNumberEnd(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxLine_get_addressSection(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_addressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxLine_get_addressOffset(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_addressOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxLine_get_relativeVirtualAddress(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_relativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxLine_get_virtualAddress(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Line.get_virtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxLine_get_length(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Line.get_length(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxLine_get_statement(
  const Line: IDiaLineNumber;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Line.get_statement(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxLineNumberReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaLineNumber>;
  end;

const
  DiaxpLineNumberToReflectionDef: array [0..9] of TDiaxLineNumberReflectionProperty = (
    (Name: 'Line Number'; Callback: DiaxLine_get_lineNumber),
    (Name: 'Line Number End'; Callback: DiaxLine_get_lineNumberEnd),
    (Name: 'Column Number'; Callback: DiaxLine_get_columnNumber),
    (Name: 'Column Number End'; Callback: DiaxLine_get_columnNumberEnd),
    (Name: 'Address Section'; Callback: DiaxLine_get_addressSection),
    (Name: 'Address Offset'; Callback: DiaxLine_get_addressOffset),
    (Name: 'Relative Virtual Address'; Callback: DiaxLine_get_relativeVirtualAddress),
    (Name: 'Virtual Address'; Callback: DiaxLine_get_virtualAddress),
    (Name: 'Length'; Callback: DiaxLine_get_length),
    (Name: 'Statement'; Callback: DiaxLine_get_statement)
  );

var
  DiaxpLineNumberToReflectionCache: TArray<TDiaxReflectionProperty<IDiaLineNumber>>;

function DiaxLineNumberToReflection;
var
  i: Integer;
begin
  if Length(DiaxpLineNumberToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpLineNumberToReflectionCache,
      Length(DiaxpLineNumberToReflectionDef));

    for i := 0 to High(DiaxpLineNumberToReflectionCache) do
    begin
      DiaxpLineNumberToReflectionCache[i].Name :=
        DiaxpLineNumberToReflectionDef[i].Name;
      DiaxpLineNumberToReflectionCache[i].Callback :=
        DiaxpLineNumberToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpLineNumberToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaLineNumber to IDiaSymbol'}

function DiaxLine_get_compiland(
  const LineNumber: IDiaLineNumber;
  out Symbol: IDiaSymbol
): Boolean;
begin
  Result := LineNumber.get_compiland(Symbol) = S_OK;
end;

function DiaxLineNumberToSymbol;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Compiland';
  Result[0].Callback := DiaxLine_get_compiland;
end;

{$ENDREGION}
{$REGION 'IDiaLineNumber to IDiaSourceFile'}

function DiaxLineNumber_get_sourceFile(
  const LineNumber: IDiaLineNumber;
  out SourceFile: IDiaSourceFile
): Boolean;
begin
  Result := LineNumber.get_sourceFile(SourceFile) = S_OK;
end;

function DiaxLineNumberToSourceFile;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Source File';
  Result[0].Callback := DiaxLineNumber_get_sourceFile;
end;

{$ENDREGION}
{$REGION 'IDiaSectionContrib sorting'}

procedure DiaxSortSections;
var
  RVAs: TArray<Cardinal>;
  i: Integer;
begin
  // Collect RVAs for sorting
  SetLength(RVAs, Length(Sections));

  for i := 0 to High(Sections) do
    if Sections[i].get_relativeVirtualAddress(RVAs[i]) <> S_OK then
      RVAs[i] := 0;

  // Sort by RVAs
  TArray.SortIndexInline<IDiaSectionContrib>(Sections,
    function (const IndexA, IndexB: Integer): Integer
    begin
      {$R-}{$Q-}
      Result := Integer(RVAs[IndexA]) - Integer(RVAs[IndexB]);
      {$IFDEF Q+}{$Q+}{$ENDIF}{$IFDEF R+}{$R+}{$ENDIF}
    end
  );
end;

{$ENDREGION}
{$REGION 'IDiaSectionContrib to reflection'}

function DiaxSection_get_addressSection(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Section.get_addressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_addressOffset(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Section.get_addressOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSection_get_relativeVirtualAddress(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Section.get_relativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSection_get_virtualAddress(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Section.get_virtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSection_get_length(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Section.get_length(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSection_get_notPaged(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_notPaged(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_code(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_code(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_initializedData(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_initializedData(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_uninitializedData(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_uninitializedData(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_remove(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_remove(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_comdat(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_comdat(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_discardable(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_discardable(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_notCached(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_notCached(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_share(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_share(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_execute(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_execute(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_read(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_read(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_write(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_write(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSection_get_dataCrc(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Section.get_dataCrc(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSection_get_relocationsCrc(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Section.get_relocationsCrc(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSection_get_code16bit(
  const Section: IDiaSectionContrib;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Section.get_code16bit(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxSectionReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaSectionContrib>;
  end;

const
  DiaxpSectionToReflectionDef: array [0..19] of TDiaxSectionReflectionProperty = (
    (Name: 'Address Section'; Callback: DiaxSection_get_addressSection),
    (Name: 'Address Offset'; Callback: DiaxSection_get_addressOffset),
    (Name: 'Relative Virtual Address'; Callback: DiaxSection_get_relativeVirtualAddress),
    (Name: 'Virtual Address'; Callback: DiaxSection_get_virtualAddress),
    (Name: 'Length'; Callback: DiaxSection_get_length),
    (Name: 'Not Paged'; Callback: DiaxSection_get_notPaged),
    (Name: 'Code'; Callback: DiaxSection_get_code),
    (Name: 'Initialized Data'; Callback: DiaxSection_get_initializedData),
    (Name: 'Uninitialized Data'; Callback: DiaxSection_get_uninitializedData),
    (Name: 'Remove'; Callback: DiaxSection_get_remove),
    (Name: 'COMDAT'; Callback: DiaxSection_get_comdat),
    (Name: 'Discardable'; Callback: DiaxSection_get_discardable),
    (Name: 'Not Cached'; Callback: DiaxSection_get_notCached),
    (Name: 'Share'; Callback: DiaxSection_get_share),
    (Name: 'Execute'; Callback: DiaxSection_get_execute),
    (Name: 'Read'; Callback: DiaxSection_get_read),
    (Name: 'Write'; Callback: DiaxSection_get_write),
    (Name: 'Data CRC'; Callback: DiaxSection_get_dataCrc),
    (Name: 'Relocations CRC'; Callback: DiaxSection_get_relocationsCrc),
    (Name: 'Code 16-bit'; Callback: DiaxSection_get_code16bit)
  );

var
  DiaxpSectionToReflectionCache: TArray<TDiaxReflectionProperty<IDiaSectionContrib>>;

function DiaxSectionToReflection;
var
  i: Integer;
begin
  if Length(DiaxpSectionToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpSectionToReflectionCache,
      Length(DiaxpSectionToReflectionDef));

    for i := 0 to High(DiaxpSectionToReflectionCache) do
    begin
      DiaxpSectionToReflectionCache[i].Name :=
        DiaxpSectionToReflectionDef[i].Name;
      DiaxpSectionToReflectionCache[i].Callback :=
        DiaxpSectionToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpSectionToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaSectionContrib to IDiaSymbol'}

function DiaxSection_get_sourceFile(
  const Section: IDiaSectionContrib;
  out Symbol: IDiaSymbol
): Boolean;
begin
  Result := Section.get_compiland(Symbol) = S_OK;
end;

function DiaxSectionToSymbol;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Compiland';
  Result[0].Callback := DiaxSection_get_sourceFile;
end;

{$ENDREGION}
{$REGION 'IDiaSegment to reflection'}

function DiaxSegment_get_frame(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Segment.get_frame(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSegment_get_offset(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Segment.get_offset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSegment_get_length(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Segment.get_length(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSegment_get_read(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Segment.get_read(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSegment_get_write(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Segment.get_write(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSegment_get_execute(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := Segment.get_execute(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSegment_get_addressSection(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Segment.get_addressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxSegment_get_relativeVirtualAddress(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := Segment.get_relativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxSegment_get_virtualAddress(
  const Segment: IDiaSegment;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := Segment.get_virtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxSegmentReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaSegment>;
  end;

const
  DiaxpSegmentToReflectionDef: array [0..8] of TDiaxSegmentReflectionProperty = (
    (Name: 'Frame'; Callback: DiaxSegment_get_frame),
    (Name: 'Offset'; Callback: DiaxSegment_get_offset),
    (Name: 'Length'; Callback: DiaxSegment_get_length),
    (Name: 'Read'; Callback: DiaxSegment_get_read),
    (Name: 'Write'; Callback: DiaxSegment_get_write),
    (Name: 'Execute'; Callback: DiaxSegment_get_execute),
    (Name: 'Address Section'; Callback: DiaxSegment_get_addressSection),
    (Name: 'Relative Virtual Address'; Callback: DiaxSegment_get_relativeVirtualAddress),
    (Name: 'Virtual Address'; Callback: DiaxSegment_get_virtualAddress)
  );

var
  DiaxpSegmentToReflectionCache: TArray<TDiaxReflectionProperty<IDiaSegment>>;

function DiaxSegmentToReflection;
var
  i: Integer;
begin
  if Length(DiaxpSegmentToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpSegmentToReflectionCache,
      Length(DiaxpSegmentToReflectionDef));

    for i := 0 to High(DiaxpSegmentToReflectionCache) do
    begin
      DiaxpSegmentToReflectionCache[i].Name :=
        DiaxpSegmentToReflectionDef[i].Name;
      DiaxpSegmentToReflectionCache[i].Callback :=
        DiaxpSegmentToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpSegmentToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaInjectedSource to reflection'}

function DiaxInjectedSource_get_crc(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := InjectedSource.get_crc(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxInjectedSource_get_length(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := InjectedSource.get_length(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxInjectedSource_get_filename(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := InjectedSource.get_filename(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxInjectedSource_get_objectFilename(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := InjectedSource.get_objectFilename(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxInjectedSource_get_virtualFilename(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := InjectedSource.get_virtualFilename(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxInjectedSource_get_sourceCompression(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := InjectedSource.get_sourceCompression(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxInjectedSource_get_source(
  const InjectedSource: IDiaInjectedSource;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Buffer: IMemory;
  Size: Cardinal;
begin
  Result := InjectedSource.get_source(0, Size, nil) = S_OK;

  if not Result then
    Exit;

  Buffer := Auto.AllocateDynamic(Size);
  Result := InjectedSource.get_source(Buffer.Size, Size, Buffer.Data) = S_OK;

  if Result then
    Reflection := FormatBuffer(Buffer, True, True);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxInjectedSourceReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaInjectedSource>;
  end;

const
  DiaxpInjectedSourceToReflectionDef: array [0..6] of TDiaxInjectedSourceReflectionProperty = (
    (Name: 'CRC'; Callback: DiaxInjectedSource_get_crc),
    (Name: 'Length'; Callback: DiaxInjectedSource_get_length),
    (Name: 'Filename'; Callback: DiaxInjectedSource_get_filename),
    (Name: 'Object Filename'; Callback: DiaxInjectedSource_get_objectFilename),
    (Name: 'Virtual Filename'; Callback: DiaxInjectedSource_get_virtualFilename),
    (Name: 'Source Compression Method'; Callback: DiaxInjectedSource_get_sourceCompression),
    (Name: 'Source Size'; Callback: DiaxInjectedSource_get_source)
  );

var
  DiaxpInjectedSourceToReflectionCache: TArray<TDiaxReflectionProperty<IDiaInjectedSource>>;

function DiaxInjectedSourceToReflection;
var
  i: Integer;
begin
  if Length(DiaxpInjectedSourceToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpInjectedSourceToReflectionCache,
      Length(DiaxpInjectedSourceToReflectionDef));

    for i := 0 to High(DiaxpInjectedSourceToReflectionCache) do
    begin
      DiaxpInjectedSourceToReflectionCache[i].Name :=
        DiaxpInjectedSourceToReflectionDef[i].Name;
      DiaxpInjectedSourceToReflectionCache[i].Callback :=
        DiaxpInjectedSourceToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpInjectedSourceToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaFrameData to reflection'}

function DiaxFrameData_get_addressSection(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_addressSection(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxFrameData_get_addressOffset(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_addressOffset(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_relativeVirtualAddress(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_relativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_virtualAddress(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := FrameData.get_virtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_lengthBlock(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_lengthBlock(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_lengthLocals(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_lengthLocals(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_lengthParams(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_lengthParams(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_maxStack(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_maxStack(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_lengthProlog(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_lengthProlog(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_lengthSavedRegisters(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := FrameData.get_lengthSavedRegisters(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxFrameData_get_program(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := FrameData.get_program(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxFrameData_get_systemExceptionHandling(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := FrameData.get_systemExceptionHandling(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxFrameData_get_cplusplusExceptionHandling(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := FrameData.get_cplusplusExceptionHandling(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxFrameData_get_functionStart(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := FrameData.get_functionStart(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxFrameData_get_allocatesBasePointer(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: LongBool;
begin
  Result := FrameData.get_allocatesBasePointer(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxFrameData_get_type(
  const FrameData: IDiaFrameData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: TStackFrameTypeEnum;
begin
  Result := FrameData.get_type(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixPreserveEnumCase);
end;

type
  // HACK: redefine the type without generics to make the compiler happy with
  // the constant array defined below
  TDiaxFrameDataReflectionProperty = record
    Name: String;
    Callback: TDiaxReflectionPropertyCallback<IDiaFrameData>;
  end;

const
  DiaxpFrameDataToReflectionDef: array [0..15] of TDiaxFrameDataReflectionProperty = (
    (Name: 'Address Section'; Callback: DiaxFrameData_get_addressSection),
    (Name: 'Address Offset'; Callback: DiaxFrameData_get_addressOffset),
    (Name: 'Relative Virtual Address'; Callback: DiaxFrameData_get_relativeVirtualAddress),
    (Name: 'Virtual Address'; Callback: DiaxFrameData_get_virtualAddress),
    (Name: 'Length Block'; Callback: DiaxFrameData_get_lengthBlock),
    (Name: 'Length Locals'; Callback: DiaxFrameData_get_lengthLocals),
    (Name: 'Length Params'; Callback: DiaxFrameData_get_lengthParams),
    (Name: 'Max Stack'; Callback: DiaxFrameData_get_maxStack),
    (Name: 'Length Prolog'; Callback: DiaxFrameData_get_lengthProlog),
    (Name: 'Length Saved Registers'; Callback: DiaxFrameData_get_lengthSavedRegisters),
    (Name: 'Program'; Callback: DiaxFrameData_get_program),
    (Name: 'System Exception Handling'; Callback: DiaxFrameData_get_systemExceptionHandling),
    (Name: 'C++ Exception Handling'; Callback: DiaxFrameData_get_cplusplusExceptionHandling),
    (Name: 'Function Start'; Callback: DiaxFrameData_get_functionStart),
    (Name: 'Allocates Base Pointer'; Callback: DiaxFrameData_get_allocatesBasePointer),
    (Name: 'Type'; Callback: DiaxFrameData_get_type)
  );

var
  DiaxpFrameDataToReflectionCache: TArray<TDiaxReflectionProperty<IDiaFrameData>>;

function DiaxFrameDataToReflection;
var
  i: Integer;
begin
  if Length(DiaxpFrameDataToReflectionCache) <= 0 then
  begin
    SetLength(DiaxpFrameDataToReflectionCache,
      Length(DiaxpFrameDataToReflectionDef));

    for i := 0 to High(DiaxpFrameDataToReflectionCache) do
    begin
      DiaxpFrameDataToReflectionCache[i].Name :=
        DiaxpFrameDataToReflectionDef[i].Name;
      DiaxpFrameDataToReflectionCache[i].Callback :=
        DiaxpFrameDataToReflectionDef[i].Callback;
    end;
  end;

  Result := DiaxpFrameDataToReflectionCache;
end;

{$ENDREGION}
{$REGION 'IDiaFrameData to IDiaFrameData' }

function DiaxFrameData_get_functionParent(
  const FrameData: IDiaFrameData;
  out Parent: IDiaFrameData
): Boolean;
begin
  Result := FrameData.get_functionParent(Parent) = S_OK;
end;

function DiaxFrameDataToFrameData;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Function Parent';
  Result[0].Callback := DiaxFrameData_get_functionParent;
end;

{$ENDREGION}
{$REGION 'IDiaTable to reflection'}

function DiaxTable_get_name(
  const Table: IDiaTable;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := Table.get_name(Value) = S_OK;

  if Result then
  begin
    Reflection.ValidFormats := [rfText];
    Reflection.Text := RtlxStringOrDefault(Value, '(Empty)');
    Reflection.Hint := '';
  end;
end;

function DiaxTable_get_Count(
  const Table: IDiaTable;
  out Reflection: TRttixFullReflection
): Boolean;
var
  KnownTable: IUnknown;
  Value: Integer;
begin
  // Only show the count if it's not covered by known enumerators
  Result := (Table.QueryInterface(IDiaEnumSymbols, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumLineNumbers, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumSourceFiles, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumInputAssemblyFiles, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumSectionContribs, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumSegments, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumInjectedSources, KnownTable) <> S_OK) and
    (Table.QueryInterface(IDiaEnumFrameData, KnownTable) <> S_OK) and
    (Table.get_Count(Value) = S_OK);

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxTableToReflection;
begin
  SetLength(Result, 2);
  Result[0].Name := 'Name';
  Result[0].Callback := DiaxTable_get_name;
  Result[1].Name := 'Count';
  Result[1].Callback := DiaxTable_get_Count;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaSymbol>'}

function DiaxTable_AsSymbols(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaSymbol>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumSymbols, Enum) = S_OK;
end;

function DiaxTableToSymbolEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Symbols';
  Result[0].Callback := DiaxTable_AsSymbols;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaSourceFile>'}

function DiaxTable_AsSourceFiles(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaSourceFile>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumSourceFiles, Enum) = S_OK;
end;

function DiaxTableToSourceFileEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Source Files';
  Result[0].Callback := DiaxTable_AsSourceFiles;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaInputAssemblyFile>'}

function DiaxTable_AsAssemblyFiles(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaInputAssemblyFile>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumInputAssemblyFiles, Enum) = S_OK;
end;

function DiaxTableToAssemblyFileEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Assembly Files';
  Result[0].Callback := DiaxTable_AsAssemblyFiles;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaLineNumber>'}

function DiaxTable_AsLineNumbers(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaLineNumber>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumLineNumbers, Enum) = S_OK;
end;

function DiaxTableToLineNumberEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Line Numbers';
  Result[0].Callback := DiaxTable_AsLineNumbers;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaSectionContrib>'}

function DiaxTable_AsSections(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaSectionContrib>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumSectionContribs, Enum) = S_OK;
end;

function DiaxTableToSectionEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Section Contributions';
  Result[0].Callback := DiaxTable_AsSections;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaSegment>'}

function DiaxTable_AsSegments(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaSegment>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumSegments, Enum) = S_OK;
end;

function DiaxTableToSegmentEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Segments';
  Result[0].Callback := DiaxTable_AsSegments;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaInjectedSource>'}

function DiaxTable_AsInjectedSources(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaInjectedSource>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumInjectedSources, Enum) = S_OK;
end;

function DiaxTableToInjectedSourceEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Injected Sources';
  Result[0].Callback := DiaxTable_AsInjectedSources;
end;

{$ENDREGION}
{$REGION 'IDiaTable to IDiaEnum<IDiaFrameData>'}

function DiaxTable_AsFrameDatas(
  const Table: IDiaTable;
  out Enum: IDiaEnum<IDiaFrameData>
): Boolean;
begin
  Result := Table.QueryInterface(IDiaEnumFrameData, Enum) = S_OK;
end;

function DiaxTableToFrameDataEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Frame Data';
  Result[0].Callback := DiaxTable_AsFrameDatas;
end;

{$ENDREGION}
{$REGION 'IDiaEnumDebugStreamData to reflection'}

function DiaxDebugStream_get_name(
  const DebugStream: IDiaEnumDebugStreamData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: WideString;
begin
  Result := DebugStream.get_name(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value);
end;

function DiaxDebugStreamToReflection;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Name';
  Result[0].Callback := DiaxDebugStream_get_name;
end;

{$ENDREGION}
{$REGION 'IDiaEnumDebugStreamData to IDiaImageData'}

function DiaxDebugStream_AsImageData(
  const DebugStream: IDiaEnumDebugStreamData;
  out ImageData: IDiaImageData
): Boolean;
begin
  Result := DebugStream.QueryInterface(IDiaImageData, ImageData) = S_OK;
end;

function DiaxDebugStreamToImageData;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Image Data';
  Result[0].Callback := DiaxDebugStream_AsImageData;
end;

{$ENDREGION}
{$REGION 'IDiaEnumDebugStreamData to IDiaEnum<IBinaryData>'}

type
  TBinaryStreamData = class (TAutoInterfacedObject, IBinaryData)
    FData: IMemory;
    FType: IRttixType;
    function GetTypeInfo: IRttixType;
    function GetData: IMemory;
    function FormatProperties: TArray<TSimpleProperty>; virtual;
    constructor Create(
      const AType: IRttixType;
      const Data: IMemory
    );
  end;
  TBinaryStreamDataClass = class of TBinaryStreamData;

  TImageSectionHeaderStreamData = class (TBinaryStreamData, IBinaryData,
    IImageSectionHeader)
    function GetSectionName: String;
  end;

  TXFixupDataStreamData = class (TBinaryStreamData, IBinaryData, IXFixupData)
  end;

  TFpoDataStreamData = class (TBinaryStreamData, IBinaryData, IFpoData)
    function FormatProperties: TArray<TSimpleProperty>; override;
  end;

  TFrameDataStreamData = class (TBinaryStreamData, IBinaryData, IFrameData)
    function FormatProperties: TArray<TSimpleProperty>; override;
  end;

  TOmapDataStreamData = class (TBinaryStreamData, IBinaryData, IOmapData)
  end;

constructor TBinaryStreamData.Create;
begin
  FType := AType;
  FData := Data;
end;

function TBinaryStreamData.FormatProperties;
var
  Formatters: TArray<IRttixFieldFormatter>;
  i: Integer;
begin
  if not Assigned(FType) then
    Exit(nil);

  // Ask our reflection system to format all record fields
  Formatters := RttixMakeFieldFormattersForType(FType);
  SetLength(Result, Length(Formatters));

  for i := 0 to High(Formatters) do
  begin
    Result[i].Name := Formatters[i].Field.Name;
    Result[i].Value := Formatters[i].Format(FData.Data^, [rfText, rfHint]);
  end;
end;

function TBinaryStreamData.GetData;
begin
  Result := FData;
end;

function TBinaryStreamData.GetTypeInfo;
begin
  Result := FType;
end;

function TImageSectionHeaderStreamData.GetSectionName;
begin
  Result := String(PImageSectionHeader(FData.Data).Name)
end;

function TFpoDataStreamData.FormatProperties;
var
  Buffer: PFpoData;
begin
  // FPO_DATA makes uses bit fields and our reflection system is not yet
  // flexible enough to handle them automatically, so do formatting manually
  SetLength(Result, 9);
  Buffer := FData.Data;
  Result[0].Name := 'OffsetStart';
  Result[0].Value := Rttix.FormatFull(Buffer.OffsetStart, RttixAsHex);
  Result[1].Name := 'ProcedureSize';
  Result[1].Value := Rttix.FormatFull(Buffer.ProcSize, RttixAsHex);
  Result[2].Name := 'Locals';
  Result[2].Value := Rttix.FormatFull(Buffer.Locals, RttixAsHex);
  Result[3].Name := 'Parameters';
  Result[3].Value := Rttix.FormatFull(Buffer.Params, RttixAsHex);
  Result[4].Name := 'Prolog';
  Result[4].Value := Rttix.FormatFull(Buffer.Prolog, RttixAsHex);
  Result[5].Name := 'Registers';
  Result[5].Value := Rttix.FormatFull(Buffer.BitField and $07, RttixAsHex);
  Result[6].Name := 'HasSEH';
  Result[6].Value := Rttix.FormatFull(BitTest(Buffer.BitField and $08));
  Result[7].Name := 'UseBP';
  Result[7].Value := Rttix.FormatFull(BitTest(Buffer.BitField and $10));
  Result[8].Name := 'Frame';
  Result[8].Value := Rttix.FormatFull(TFpoDataFrame(
    (Buffer.BitField and $C0) shr 6), RttixPreserveEnumCase);
end;

function TFrameDataStreamData.FormatProperties;
var
  Buffer: PFrameData;
begin
  // FPO_DATA makes uses bit fields and our reflection system is not yet
  // flexible enough to handle them automatically, so do formatting manually
  SetLength(Result, 11);
  Buffer := FData.Data;
  Result[0].Name := 'RVAStart';
  Result[0].Value := Rttix.FormatFull(Buffer.RvaStart, RttixAsHex);
  Result[1].Name := 'Block';
  Result[1].Value := Rttix.FormatFull(Buffer.Block, RttixAsHex);
  Result[2].Name := 'Locals';
  Result[2].Value := Rttix.FormatFull(Buffer.Locals, RttixAsHex);
  Result[3].Name := 'Parameters';
  Result[3].Value := Rttix.FormatFull(Buffer.Params, RttixAsHex);
  Result[4].Name := 'StackMax';
  Result[4].Value := Rttix.FormatFull(Buffer.StkMax, RttixAsHex);
  Result[5].Name := 'FrameFunc';
  Result[5].Value := Rttix.FormatFull(Buffer.FrameFunc, RttixAsHex);
  Result[6].Name := 'Prolog';
  Result[6].Value := Rttix.FormatFull(Buffer.Prolog, RttixAsHex);
  Result[7].Name := 'SavedReg';
  Result[7].Value := Rttix.FormatFull(Buffer.SavedRegs, RttixAsHex);
  Result[8].Name := 'HasSEH';
  Result[8].Value := Rttix.FormatFull(BitTest(Buffer.BitField and $01));
  Result[9].Name := 'HasEH';
  Result[9].Value := Rttix.FormatFull(BitTest(Buffer.BitField and $02));
  Result[10].Name := 'IsFunctionStart';
  Result[10].Value := Rttix.FormatFull(BitTest(Buffer.BitField and $04));
end;

function DiaxDebugStream_AsBinaryDataEnum(
  const DebugStream: IDiaEnumDebugStreamData;
  out Enum: IDiaEnum<IBinaryData>
): Boolean;
var
  i, Count: Integer;
  Entries: TArray<IBinaryData>;
  Name: WideString;
  Buffer: IMemory;
  Size, ExpectedSize, Fetched: Cardinal;
  AType: IRttixType;
  BinaryClass: TBinaryStreamDataClass;
begin
  DebugStream.Reset;
  Result := DebugStream.get_Count(Count) = S_OK;

  if not Result then
    Exit;

  SetLength(Entries, Count);
  AType := nil;
  BinaryClass := TBinaryStreamData;
  ExpectedSize := 0;

  // Some streams store elements with known structure
  if DebugStream.get_name(Name) = S_OK then
  begin
    // IMAGE_SECTION_HEADER entries
    if RtlxEqualStrings(Name, 'SECTIONHEADERS') or
      RtlxEqualStrings(Name, 'SECTIONHEADERSORIG') then
    begin
      AType := RttixTypeInfo(TypeInfo(TImageSectionHeader));
      ExpectedSize := SizeOf(TImageSectionHeader);
      BinaryClass := TImageSectionHeaderStreamData;
    end

    // XFIXUP_DATA entries
    else if RtlxEqualStrings(Name, 'FIXUP') then
    begin
      AType := RttixTypeInfo(DiaxCurrentFixupDataTypeInfo, RttixPreserveEnumCase);
      ExpectedSize := SizeOf(TXFixupData);
      BinaryClass := TXFixupDataStreamData;
    end

    // FPO_DATA entries
    else if RtlxEqualStrings(Name, 'FPO') then
    begin
      AType := RttixTypeInfo(TypeInfo(TFpoData), RttixPreserveEnumCase);
      ExpectedSize := SizeOf(TFpoData);
      BinaryClass := TFpoDataStreamData;
    end

    // FRAMEDATA entries
    else if RtlxEqualStrings(Name, 'NEWFPO') then
    begin
      AType := RttixTypeInfo(TypeInfo(TFrameData), RttixPreserveEnumCase);
      ExpectedSize := SizeOf(TFrameData);
      BinaryClass := TFrameDataStreamData;
    end

    // OMAP_DATA entries
    else if RtlxEqualStrings(Name, 'OMAPTO') or
      RtlxEqualStrings(Name, 'OMAPFROM') then
    begin
      AType := RttixTypeInfo(TypeInfo(TOmapData), RttixPreserveEnumCase);
      ExpectedSize := SizeOf(TOmapData);
      BinaryClass := TOmapDataStreamData;
    end
  end;

  for i := 0 to High(Entries) do
  begin
    // Determine the entry's size
    Result := DebugStream.Next(1, 0, Size, nil, Fetched) = S_OK;

    if not Result then
      Exit;

    // Read it
    Buffer := Auto.AllocateDynamic(Size);
    Result := DebugStream.Next(1, Buffer.Size, Size, Buffer.Data, Fetched) = S_OK;

    if not Result then
      Exit;

    // Create a wrapper object
    if Buffer.Size >= ExpectedSize then
      Entries[i] := BinaryClass.Create(AType, Buffer)
    else
      Entries[i] := TBinaryStreamData.Create(AType, Buffer)
  end;

  // Put them into an enumerator
  Enum := TFakeDiaEnum<IBinaryData>.Create(Entries);
end;

function DiaxDebugStreamToBinaryDataEnum;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Items';
  Result[0].Callback := DiaxDebugStream_AsBinaryDataEnum;
end;

{$ENDREGION}
{$REGION 'IDiaImageData to reflection'}

function DiaxImageData_get_relativeVirtualAddress(
  const ImageData: IDiaImageData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: Cardinal;
begin
  Result := ImageData.get_relativeVirtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxImageData_get_virtualAddress(
  const ImageData: IDiaImageData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := ImageData.get_virtualAddress(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxImageData_get_imageBase(
  const ImageData: IDiaImageData;
  out Reflection: TRttixFullReflection
): Boolean;
var
  Value: UInt64;
begin
  Result := ImageData.get_imageBase(Value) = S_OK;

  if Result then
    Reflection := Rttix.FormatFull(Value, RttixAsHex);
end;

function DiaxImageDataToReflection;
begin
  SetLength(Result, 3);
  Result[0].Name := 'Relative Virtual Address';
  Result[0].Callback := DiaxImageData_get_relativeVirtualAddress;
  Result[1].Name := 'Virtual Address';
  Result[1].Callback := DiaxImageData_get_virtualAddress;
  Result[2].Name := 'Image Base';
  Result[2].Callback := DiaxImageData_get_imageBase;
end;

{$ENDREGION}
{$REGION 'IBinaryData to reflection'}

function DiaxBinaryData_GetData(
  const BinaryData: IBinaryData;
  out Reflection: TRttixFullReflection
): Boolean;
begin
  // Skip data with known type info (which is handled via RTTI)
  Result := not Assigned(BinaryData.TypeInfo);

  if Result then
    Reflection := FormatBuffer(BinaryData.GetData, False, True);
end;

function DiaxBinaryDataToReflection;
begin
  SetLength(Result, 1);
  Result[0].Name := 'Raw Data (As Hex)';
  Result[0].Callback := DiaxBinaryData_GetData;
end;

{$ENDREGION}

initialization
  RttixRegisterBasicFormatters;
  RttixRegisterTimeFormatters;
end.
