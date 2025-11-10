unit DiaNodes;

{
  This module provides logic for creating tree nodes with symbol properties.
}

interface

uses
  Ntapi.msdia, NtUtils, DevirtualizedTree;

const
  colProperty = 0;
  colValue = 1;
  colMax = 2;

var
  EnableSorting: Boolean = True;

// Prepare initial nodes for a PDB
function DiaxMakeSessionNodeChildren(
  const Session: IDiaSession
): TArray<INodeProvider>;

implementation

uses
  Ntapi.DbgHelp, NtUiLib.Errors, NtUtils.SysUtils, VirtualTrees,
  DevirtualizedTree.Provider, DelphiUiLib.LiteReflection, NtUiCommon.Helpers,
  DiaProperties, TextHelpers, DelphiUtils.LiteRTTI, DelphiUiLib.Strings;

type
  // A simple leaf node
  TDiaxTextNode = class (TNodeProvider, INodeProvider)
    constructor Create(
      const PropertyName: String;
      const Value: String;
      const Hint: String = ''
    );
  end;

  // An node for an object with inspectable properties
  TDiaxInstanceNode<I : IInterface> = class (TNodeProvider, INodeProvider)
    FInstance: I;
    FAutoExpand: Boolean;
    function MakeChildNodes: TArray<INodeProvider>;
    constructor Create(const PropertyName: String; Instance: I; AutoExpand: Boolean = False);
    procedure Initialize; override;
    procedure NotifyFirstExpanding; override;
    class function NodeFromRelationTo<ISource>(
      const Source: ISource;
      const Definition: TDiaxInstanceProperty<ISource, I>;
      out Node: INodeProvider;
      AutoExpand: Boolean = False
    ): Boolean; static;
  end;

  // A node for a collection of objects
  TDiaxEnumNode<I : IInterface> = class (TNodeProvider, INodeProvider)
    FSortByName: Boolean;
    FEnum: IDiaEnum<I>;
    procedure SetCount(const Status: TNtxStatus; Count: Integer);
    procedure Initialize; override;
    procedure NotifyFirstExpanding; override;
    class function NodeFromRelationTo<ISource>(
      const Source: ISource;
      const Definition: TDiaxEnumProperty<ISource, I>;
      out Node: INodeProvider;
      SortByName: Boolean = False
    ): Boolean; static;
    constructor Create(
      const PropertyName: String;
      const Enum: IDiaEnum<I>;
      SortByName: Boolean = False
    );
  end;

constructor TDiaxTextNode.Create;
begin
  inherited Create(colMax);
  FColumnText[colProperty] := PropertyName;
  FColumnText[colValue] := Value;
  FHint := Hint;
end;

constructor TDiaxInstanceNode<I>.Create;
begin
  inherited Create(colMax);
  FColumnText[colProperty] := PropertyName;
  FInstance := Instance;
  FAutoExpand := AutoExpand;
end;

class function TDiaxInstanceNode<I>.NodeFromRelationTo<ISource>;
var
  Instance: I;
begin
  // Try to query a related instance
  Result := Definition.Callback(Source, Instance);

  if Result then
    Node := TDiaxInstanceNode<I>.Create(Definition.Name, Instance, AutoExpand);
end;

procedure TDiaxInstanceNode<I>.Initialize;
var
  Name: String;
  LocalInstance: I;
  Symbol: IDiaSymbol absolute LocalInstance;
  SourceFile: IDiaSourceFile absolute LocalInstance;
  AssemeblyFile: IDiaInputAssemblyFile absolute LocalInstance;
  Line: IDiaLineNumber absolute LocalInstance;
  Section: IDiaSectionContrib absolute LocalInstance;
  Segment: IDiaSegment absolute LocalInstance;
  InjectedSource: IDiaInjectedSource absolute LocalInstance;
  FrameData: IDiaFrameData absolute LocalInstance;
  Table: IDiaTable absolute LocalInstance;
  DebugStream: IDiaEnumDebugStreamData absolute LocalInstance;
  ImageData: IDiaImageData absolute LocalInstance;
  BinaryData: IBinaryData absolute LocalInstance;
begin
  inherited;
  LocalInstance := FInstance;

  // Determine the instance name base on its type
  if TypeInfo(I) = TypeInfo(IDiaSymbol) then
    Name := DiaxFormatSymbolName(Symbol)
  else if TypeInfo(I) = TypeInfo(IDiaSourceFile) then
    Name := DiaxFormatSourceFileName(SourceFile)
  else if TypeInfo(I) = TypeInfo(IDiaInputAssemblyFile) then
    Name := DiaxFormatInputAssemblyName(AssemeblyFile)
  else if TypeInfo(I) = TypeInfo(IDiaLineNumber) then
    Name := DiaxFormatLineNumberName(Line)
  else if TypeInfo(I) = TypeInfo(IDiaSectionContrib) then
    Name := DiaxFormatSectionName(Section)
  else if TypeInfo(I) = TypeInfo(IDiaSegment) then
    Name := DiaxFormatSegmentName(Segment)
  else if TypeInfo(I) = TypeInfo(IDiaInjectedSource) then
    Name := DiaxFormatInjectedSourceName(InjectedSource)
  else if TypeInfo(I) = TypeInfo(IDiaFrameData) then
    Name := DiaxFormatFrameDataName(FrameData)
  else if TypeInfo(I) = TypeInfo(IDiaTable) then
    Name := DiaxFormatTableName(Table)
  else if TypeInfo(I) = TypeInfo(IDiaEnumDebugStreamData) then
    Name := DiaxFormatDebugStreamName(DebugStream)
  else if TypeInfo(I) = TypeInfo(IDiaImageData) then
    Name := DiaxFormatImageData(ImageData)
  else if TypeInfo(I) = TypeInfo(IBinaryData) then
    Name := DiaxFormatBinaryData(BinaryData)
  else if Assigned(TypeInfo(I)) then
    Name := '(' + PLiteRttiTypeInfo(TypeInfo(I)).Name + ')'
  else
    Name := '';

  FColumnText[colValue] := Name;
  Include(FNode.States, vsHasChildren);

  if FAutoExpand then
    FTree.Expanded[FNode] := True;
end;

function DiaxMakeSessionNodeChildren(
  const Session: IDiaSession
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaSession>>;
  ToSymbol: TArray<TDiaxInstanceProperty<IDiaSession, IDiaSymbol>>;
  ToSymbolEnum: TArray<TDiaxEnumProperty<IDiaSession, IDiaSymbol>>;
  ToSourceFileEnum: TArray<TDiaxEnumProperty<IDiaSession, IDiaSourceFile>>;
  ToAssemblyFileEnum: TArray<TDiaxEnumProperty<IDiaSession, IDiaInputAssemblyFile>>;
  ToTableEnum: TArray<TDiaxEnumProperty<IDiaSession, IDiaTable>>;
  ToDebugStreamEnum: TArray<TDiaxEnumProperty<IDiaSession, IDiaEnumDebugStreamData>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  // Collect callbacks for session properties
  ToReflection := DiaxSessionToReflection;
  ToSymbol := DiaxSessionToSymbol;
  ToSymbolEnum := DiaxSessionToSymbolEnum;
  ToSourceFileEnum := DiaxSessionToSourceFileEnum;
  ToAssemblyFileEnum := DiaxSessionToAssemblyFileEnum;
  ToTableEnum := DiaxSessionToTableEnum;
  ToDebugStreamEnum := DiaxSessionToDebugStreamEnum;

  SetLength(Result, Length(ToReflection) + Length(ToSymbol) +
    Length(ToSymbolEnum) + Length(ToSourceFileEnum) +
    Length(ToAssemblyFileEnum) + Length(ToTableEnum) +
    Length(ToDebugStreamEnum));
  j := 0;

  // Symbol instances, auto-expanded
  for k := 0 to High(ToSymbol) do
    if TDiaxInstanceNode<IDiaSymbol>.NodeFromRelationTo<IDiaSession>(Session,
      ToSymbol[k], Result[j], True) then
      Inc(j);

  // Symbol enumerators, auto-sorted
  for k := 0 to High(ToSymbolEnum) do
    if TDiaxEnumNode<IDiaSymbol>.NodeFromRelationTo<IDiaSession>(Session,
      ToSymbolEnum[k], Result[j], True) then
      Inc(j);

  // Source file enumerators
  for k := 0 to High(ToSourceFileEnum) do
    if TDiaxEnumNode<IDiaSourceFile>.NodeFromRelationTo<IDiaSession>(Session,
      ToSourceFileEnum[k], Result[j]) then
      Inc(j);

  // Assembly file enumerators
  for k := 0 to High(ToAssemblyFileEnum) do
    if TDiaxEnumNode<IDiaInputAssemblyFile>.NodeFromRelationTo<IDiaSession>(
      Session, ToAssemblyFileEnum[k], Result[j]) then
      Inc(j);

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(Session, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Table enumerators
  for k := 0 to High(ToTableEnum) do
    if TDiaxEnumNode<IDiaTable>.NodeFromRelationTo<IDiaSession>(
      Session, ToTableEnum[k], Result[j]) then
      Inc(j);

  // Debug stream enumerators
  for k := 0 to High(ToDebugStreamEnum) do
    if TDiaxEnumNode<IDiaEnumDebugStreamData>.NodeFromRelationTo<IDiaSession>(
      Session, ToDebugStreamEnum[k], Result[j]) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeSymbolNodeChildren(
  const Symbol: IDiaSymbol
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaSymbol>>;
  ToSymbol: TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaSymbol>>;
  ToSymbolEnum: TArray<TDiaxEnumProperty<IDiaSymbol, IDiaSymbol>>;
  ToSourceEnum: TArray<TDiaxEnumProperty<IDiaSymbol, IDiaSourceFile>>;
  ToLine: TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaLineNumber>>;
  ToLineEnum: TArray<TDiaxEnumProperty<IDiaSymbol, IDiaLineNumber>>;
  ToAssembly: TArray<TDiaxInstanceProperty<IDiaSymbol, IDiaInputAssemblyFile>>;
  Reflection: TRttixFullReflection;
  IsExeTag: Boolean;
  Tag: TSymTagEnum;
  j, k: Integer;
begin
  // Collect callbacks for symbol properties
  IsExeTag := (Symbol.get_symTag(Tag) = S_OK) and (Tag = SymTagExe);
  ToReflection := DiaxSymbolToReflection;
  ToSymbol := DiaxSymbolToSymbol;
  ToSourceEnum := DiaxSymbolToSourceFileEnum;
  ToLine := DiaxSymbolToLineNumber;
  ToAssembly := DiaxSymbolToAssemblyFile;
  ToSymbolEnum := DiaxSymbolToSymbolEnum;
  ToLineEnum := DiaxSymbolToLineNumberEnum;

  SetLength(Result, Length(ToReflection) + Length(ToSymbol) +
    Length(ToSymbolEnum) + Length(ToSourceEnum) + Length(ToLine) +
    Length(ToLineEnum) + Length(ToAssembly));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(Symbol, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Symbol instances
  for k := 0 to High(ToSymbol) do
    if TDiaxInstanceNode<IDiaSymbol>.NodeFromRelationTo<IDiaSymbol>(Symbol,
      ToSymbol[k], Result[j]) then
      Inc(j);

  // Source file enumerators
  for k := 0 to High(ToSourceEnum) do
    if TDiaxEnumNode<IDiaSourceFile>.NodeFromRelationTo<IDiaSymbol>(Symbol,
      ToSourceEnum[k], Result[j]) then
      Inc(j);

  // Line number instances
  for k := 0 to High(ToLine) do
    if TDiaxInstanceNode<IDiaLineNumber>.NodeFromRelationTo<IDiaSymbol>(Symbol,
      ToLine[k], Result[j]) then
      Inc(j);

  // Input assembly instances
  for k := 0 to High(ToAssembly) do
    if TDiaxInstanceNode<IDiaInputAssemblyFile>.NodeFromRelationTo<IDiaSymbol>(
      Symbol, ToAssembly[k], Result[j]) then
      Inc(j);

  // Line number enumerators
  for k := 0 to High(ToLineEnum) do
    if TDiaxEnumNode<IDiaLineNumber>.NodeFromRelationTo<IDiaSymbol>(Symbol,
      ToLineEnum[k], Result[j]) then
      Inc(j);

  // Symbol enumerators, auto-sorted when referring to the global scope
  for k := 0 to High(ToSymbolEnum) do
    if TDiaxEnumNode<IDiaSymbol>.NodeFromRelationTo<IDiaSymbol>(Symbol,
      ToSymbolEnum[k], Result[j], IsExeTag) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeSourceFileChildren(
  const SourceFile: IDiaSourceFile
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaSourceFile>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxSourceFileToReflection;

  SetLength(Result, Length(ToReflection));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(SourceFile, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  SetLength(Result, j);
end;

function DiaxMakeAssemblyFileChildren(
  const AssemblyFile: IDiaInputAssemblyFile
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaInputAssemblyFile>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxAssemblyFileToReflection;

  SetLength(Result, Length(ToReflection));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(AssemblyFile, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  SetLength(Result, j);
end;

function DiaxMakeLineNodeChildren(
  const Line: IDiaLineNumber
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaLineNumber>>;
  ToSymbol: TArray<TDiaxInstanceProperty<IDiaLineNumber, IDiaSymbol>>;
  ToSourceFile: TArray<TDiaxInstanceProperty<IDiaLineNumber, IDiaSourceFile>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxLineNumberToReflection;
  ToSymbol := DiaxLineNumberToSymbol;
  ToSourceFile := DiaxLineNumberToSourceFile;

  SetLength(Result, Length(ToReflection) + Length(ToSymbol) +
    Length(ToSourceFile));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(Line, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Symbol instances
  for k := 0 to High(ToSymbol) do
    if TDiaxInstanceNode<IDiaSymbol>.NodeFromRelationTo<IDiaLineNumber>(Line,
      ToSymbol[k], Result[j]) then
      Inc(j);

  // Source file instances
  for k := 0 to High(ToSourceFile) do
    if TDiaxInstanceNode<IDiaSourceFile>.NodeFromRelationTo<IDiaLineNumber>(Line,
      ToSourceFile[k], Result[j]) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeSectionChildren(
  const Section: IDiaSectionContrib
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaSectionContrib>>;
  ToSymbol: TArray<TDiaxInstanceProperty<IDiaSectionContrib, IDiaSymbol>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxSectionToReflection;
  ToSymbol := DiaxSectionToSymbol;

  SetLength(Result, Length(ToReflection) + Length(ToSymbol));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(Section, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Symbol instances
  for k := 0 to High(ToSymbol) do
    if TDiaxInstanceNode<IDiaSymbol>.NodeFromRelationTo<IDiaSectionContrib>(
      Section, ToSymbol[k], Result[j]) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeSegmentChildren(
  const Segment: IDiaSegment
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaSegment>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxSegmentToReflection;

  SetLength(Result, Length(ToReflection));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(Segment, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  SetLength(Result, j);
end;

function DiaxMakeInjectedSourceChildren(
  const InjectedSource: IDiaInjectedSource
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaInjectedSource>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxInjectedSourceToReflection;

  SetLength(Result, Length(ToReflection));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(InjectedSource, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  SetLength(Result, j);
end;

function DiaxMakeFrameDataChildren(
  const FrameData: IDiaFrameData
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaFrameData>>;
  ToFrameData: TArray<TDiaxInstanceProperty<IDiaFrameData, IDiaFrameData>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxFrameDataToReflection;
  ToFrameData := DiaxFrameDataToFrameData;

  SetLength(Result, Length(ToReflection) + Length(ToFrameData));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(FrameData, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Frame data instances
  for k := 0 to High(ToFrameData) do
    if TDiaxInstanceNode<IDiaFrameData>.NodeFromRelationTo<IDiaFrameData>(
      FrameData, ToFrameData[k], Result[j]) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeTableChildren(
  const Table: IDiaTable
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaTable>>;
  ToSymbolEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaSymbol>>;
  ToSourceFileEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaSourceFile>>;
  ToAssemblyFileEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaInputAssemblyFile>>;
  ToLineEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaLineNumber>>;
  ToSectionEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaSectionContrib>>;
  ToSegmentEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaSegment>>;
  ToInjectedSourceEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaInjectedSource>>;
  ToFrameDataEnum: TArray<TDiaxEnumProperty<IDiaTable, IDiaFrameData>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxTableToReflection;
  ToSymbolEnum := DiaxTableToSymbolEnum;
  ToSourceFileEnum := DiaxTableToSourceFileEnum;
  ToAssemblyFileEnum := DiaxTableToAssemblyFileEnum;
  ToLineEnum := DiaxTableToLineNumberEnum;
  ToSectionEnum := DiaxTableToSectionEnum;
  ToSegmentEnum := DiaxTableToSegmentEnum;
  ToInjectedSourceEnum := DiaxTableToInjectedSourceEnum;
  ToFrameDataEnum := DiaxTableToFrameDataEnum;

  SetLength(Result, Length(ToReflection) + Length(ToSymbolEnum) +
    Length(ToSourceFileEnum) + Length(ToAssemblyFileEnum) + Length(ToLineEnum) +
    Length(ToSectionEnum) + Length(ToSegmentEnum) + Length(ToInjectedSourceEnum)
    + Length(ToFrameDataEnum));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(Table, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Symbol enumerators
  for k := 0 to High(ToSymbolEnum) do
    if TDiaxEnumNode<IDiaSymbol>.NodeFromRelationTo<IDiaTable>(Table,
      ToSymbolEnum[k], Result[j], True) then
      Inc(j);

  // Line number enumerators
  for k := 0 to High(ToLineEnum) do
    if TDiaxEnumNode<IDiaLineNumber>.NodeFromRelationTo<IDiaTable>(Table,
      ToLineEnum[k], Result[j], True) then
      Inc(j);

  // Source file enumerators
  for k := 0 to High(ToSourceFileEnum) do
    if TDiaxEnumNode<IDiaSourceFile>.NodeFromRelationTo<IDiaTable>(Table,
      ToSourceFileEnum[k], Result[j], True) then
      Inc(j);

  // Input assembly enumerators
  for k := 0 to High(ToAssemblyFileEnum) do
    if TDiaxEnumNode<IDiaInputAssemblyFile>.NodeFromRelationTo<IDiaTable>(Table,
      ToAssemblyFileEnum[k], Result[j], True) then
      Inc(j);

  // Section contribution enumerators
  for k := 0 to High(ToSectionEnum) do
    if TDiaxEnumNode<IDiaSectionContrib>.NodeFromRelationTo<IDiaTable>(Table,
      ToSectionEnum[k], Result[j], True) then
      Inc(j);

  // Segment enumerators
  for k := 0 to High(ToSegmentEnum) do
    if TDiaxEnumNode<IDiaSegment>.NodeFromRelationTo<IDiaTable>(Table,
      ToSegmentEnum[k], Result[j], True) then
      Inc(j);

  // Injected sources enumerators
  for k := 0 to High(ToInjectedSourceEnum) do
    if TDiaxEnumNode<IDiaInjectedSource>.NodeFromRelationTo<IDiaTable>(Table,
      ToInjectedSourceEnum[k], Result[j], True) then
      Inc(j);

  // Frame data enumerators
  for k := 0 to High(ToFrameDataEnum) do
    if TDiaxEnumNode<IDiaFrameData>.NodeFromRelationTo<IDiaTable>(Table,
      ToFrameDataEnum[k], Result[j], True) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeDebugStreamChildren(
  const DebugStream: IDiaEnumDebugStreamData
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaEnumDebugStreamData>>;
  ToImageData: TArray<TDiaxInstanceProperty<IDiaEnumDebugStreamData, IDiaImageData>>;
  ToBinaryDataEnum: TArray<TDiaxEnumProperty<IDiaEnumDebugStreamData, IBinaryData>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxDebugStreamToReflection;
  ToImageData := DiaxDebugStreamToImageData;
  ToBinaryDataEnum := DiaxDebugStreamToBinaryDataEnum;

  SetLength(Result, Length(ToReflection) + Length(ToImageData) +
    Length(ToBinaryDataEnum));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(DebugStream, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // Image data instances
  for k := 0 to High(ToImageData) do
    if TDiaxInstanceNode<IDiaImageData>.NodeFromRelationTo<IDiaEnumDebugStreamData>(
      DebugStream, ToImageData[k], Result[j]) then
      Inc(j);

  // Binary data instances
  for k := 0 to High(ToBinaryDataEnum) do
    if TDiaxEnumNode<IBinaryData>.NodeFromRelationTo<IDiaEnumDebugStreamData>(
      DebugStream, ToBinaryDataEnum[k], Result[j]) then
      Inc(j);

  SetLength(Result, j);
end;

function DiaxMakeImageDataChildren(
  const ImageData: IDiaImageData
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IDiaImageData>>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxImageDataToReflection;

  SetLength(Result, Length(ToReflection));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(ImageData, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  SetLength(Result, j);
end;

function DiaxMakeBinaryDataChildren(
  const BinaryData: IBinaryData
): TArray<INodeProvider>;
var
  ToReflection: TArray<TDiaxReflectionProperty<IBinaryData>>;
  ToFields: TArray<TSimpleProperty>;
  Reflection: TRttixFullReflection;
  j, k: Integer;
begin
  ToReflection := DiaxBinaryDataToReflection;
  ToFields := BinaryData.FormatProperties;

  SetLength(Result, Length(ToReflection) + Length(ToFields));
  j := 0;

  // Simple properties
  for k := 0 to High(ToReflection) do
    if ToReflection[k].Callback(BinaryData, Reflection) then
    begin
      Result[j] := TDiaxTextNode.Create(ToReflection[k].Name, Reflection.Text,
        Reflection.Hint);
      Inc(j);
    end;

  // RTTI fields
  for k := 0 to High(ToFields) do
  begin
    Result[j] := TDiaxTextNode.Create(PrettifyCamelCase(ToFields[k].Name),
      ToFields[k].Value.Text, ToFields[k].Value.Hint);
    Inc(j);
  end;

  SetLength(Result, j);
end;

function TDiaxInstanceNode<I>.MakeChildNodes;
var
  LocalInstance: I;
  Symbol: IDiaSymbol absolute LocalInstance;
  SourceFile: IDiaSourceFile absolute LocalInstance;
  AssemblyFile: IDiaInputAssemblyFile absolute LocalInstance;
  Line: IDiaLineNumber absolute LocalInstance;
  Section: IDiaSectionContrib absolute LocalInstance;
  Segment: IDiaSegment absolute LocalInstance;
  InjectedSource: IDiaInjectedSource absolute LocalInstance;
  FrameData: IDiaFrameData absolute LocalInstance;
  Table: IDiaTable absolute LocalInstance;
  DebugStream: IDiaEnumDebugStreamData absolute LocalInstance;
  ImageData: IDiaImageData absolute LocalInstance;
  BinaryData: IBinaryData absolute LocalInstance;
begin
  LocalInstance := FInstance;

  // Collect children based on the type
  if TypeInfo(I) = TypeInfo(IDiaSymbol) then
    Result := DiaxMakeSymbolNodeChildren(Symbol)
  else if TypeInfo(I) = TypeInfo(IDiaSourceFile) then
    Result := DiaxMakeSourceFileChildren(SourceFile)
  else if TypeInfo(I) = TypeInfo(IDiaInputAssemblyFile) then
    Result := DiaxMakeAssemblyFileChildren(AssemblyFile)
  else if TypeInfo(I) = TypeInfo(IDiaLineNumber) then
    Result := DiaxMakeLineNodeChildren(Line)
  else if TypeInfo(I) = TypeInfo(IDiaSectionContrib) then
    Result := DiaxMakeSectionChildren(Section)
  else if TypeInfo(I) = TypeInfo(IDiaSegment) then
    Result := DiaxMakeSegmentChildren(Segment)
  else if TypeInfo(I) = TypeInfo(IDiaInjectedSource) then
    Result := DiaxMakeInjectedSourceChildren(InjectedSource)
  else if TypeInfo(I) = TypeInfo(IDiaFrameData) then
    Result := DiaxMakeFrameDataChildren(FrameData)
  else if TypeInfo(I) = TypeInfo(IDiaTable) then
    Result := DiaxMakeTableChildren(Table)
  else if TypeInfo(I) = TypeInfo(IDiaEnumDebugStreamData) then
    Result := DiaxMakeDebugStreamChildren(DebugStream)
  else if TypeInfo(I) = TypeInfo(IDiaImageData) then
    Result := DiaxMakeImageDataChildren(ImageData)
  else if TypeInfo(I) = TypeInfo(IBinaryData) then
    Result := DiaxMakeBinaryDataChildren(BinaryData)
  else
    Result := nil;
end;

procedure TDiaxInstanceNode<I>.NotifyFirstExpanding;
var
  ChildNodes: TArray<INodeProvider>;
  ChildNode: INodeProvider;
begin
  FTree.BeginUpdateAuto;

  // Generate children nodes
  ChildNodes := MakeChildNodes;

  for ChildNode in ChildNodes do
    FTree.AddChildEx(FNode, ChildNode);

  if Length(ChildNodes) <= 0 then
    Exclude(FNode.States, vsHasChildren);
end;

constructor TDiaxEnumNode<I>.Create;
begin
  inherited Create(colMax);
  FColumnText[colProperty] := PropertyName;
  FEnum := Enum;
  FSortByName := SortByName;
end;

class function TDiaxEnumNode<I>.NodeFromRelationTo<ISource>;
var
  Enum: IDiaEnum<I>;
begin
  // Try to query a related enumerators
  Result := Definition.Callback(Source, Enum);

  if Result then
    Node := TDiaxEnumNode<I>.Create(Definition.Name, Enum, SortByName);
end;

procedure TDiaxEnumNode<I>.Initialize;
var
  Status: TNtxStatus;
  Count: Integer;
begin
  inherited;

  Status.Location := 'IDiaEnum::get_Count';
  Status.HResult := FEnum.get_Count(Count);
  SetCount(Status, Count);

  if Status.IsSuccess and (Count > 0) then
    Include(FNode.States, vsHasChildren);
end;

procedure TDiaxEnumNode<I>.NotifyFirstExpanding;
var
  Status: TNtxStatus;
  Children: TArray<I>;
  j: Integer;
begin
  if not Assigned(FEnum) then
    Exit;

  // Collect entries from the enumerator
  Status := DiaxProperties.CollectEnum<I>(FEnum, Children);
  SetCount(Status, Length(Children));

  // Apply sorting if supported
  if EnableSorting then
  begin
    if (TypeInfo(I) = TypeInfo(IDiaSymbol)) then
      DiaxSortSymbols(TArray<IDiaSymbol>(Children), FSortByName)
    else if TypeInfo(I) = TypeInfo(IDiaSourceFile) then
      DiaxSortSourceFiles(TArray<IDiaSourceFile>(Children))
    else if TypeInfo(I) = TypeInfo(IDiaLineNumber) then
      DiaxSortLineNumbers(TArray<IDiaLineNumber>(Children))
    else if TypeInfo(I) = TypeInfo(IDiaSectionContrib) then
      DiaxSortSections(TArray<IDiaSectionContrib>(Children));
  end;

  if Status.IsSuccess then
  begin
    FTree.BeginUpdateAuto;

    // Generate child nodes
    for j := 0 to High(Children) do
      FTree.AddChildEx(FNode, TDiaxInstanceNode<I>.Create(
        FormatArrayIndex(j), Children[j]));
  end;
end;

procedure TDiaxEnumNode<I>.SetCount;
var
  ElementName: String;
begin
  if Status.IsSuccess then
  begin
    if TypeInfo(I) = TypeInfo(IDiaSymbol) then
      ElementName := 'Symbols'
    else if TypeInfo(I) = TypeInfo(IDiaSourceFile) then
      ElementName := 'Source Files'
    else if TypeInfo(I) = TypeInfo(IDiaInputAssemblyFile) then
      ElementName := 'Input Assembly Files'
    else if TypeInfo(I) = TypeInfo(IDiaLineNumber) then
      ElementName := 'Line Numbers'
    else if TypeInfo(I) = TypeInfo(IDiaSectionContrib) then
      ElementName := 'Section Contributions'
    else if TypeInfo(I) = TypeInfo(IDiaSegment) then
      ElementName := 'Segments'
    else if TypeInfo(I) = TypeInfo(IDiaInjectedSource) then
      ElementName := 'Injected Sources'
    else if TypeInfo(I) = TypeInfo(IDiaTable) then
      ElementName := 'Tables'
    else if TypeInfo(I) = TypeInfo(IDiaEnumDebugStreamData) then
      ElementName := 'Debug Streams'
    else
      ElementName := 'Entries';

    FColumnText[colValue] := RtlxFormatString('(%u %s)', [Count, ElementName])
  end
  else
  begin
    FColumnText[colValue] := '(Error)';
    FHint := Status.ToString;
  end;
  Invalidate;
end;

end.
