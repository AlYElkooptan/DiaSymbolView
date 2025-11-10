unit TextHelpers;

{
  This module provides helper functions for faster node text manipulation.
}

interface

uses
  Ntapi.DbgHelp, Ntapi.msdia, DiaProperties, NtUtils,
  DelphiUiLib.LiteReflection;

// Prepare a string for a symbol tag
function FormatSymbolTag(
  Tag: TSymTagEnum
): String;

// Prepare a string for a basic type kind
function FormatBasicType(
  Basic: TBasicType
): String;

// Prepare a string with an array index
function FormatArrayIndex(
  Index: Integer
): String;

// Prepare a string with an binary buffer
function FormatBuffer(
  const Buffer: IMemory;
  PrefixSize: Boolean;
  SpaceBytes: Boolean
): TRttixFullReflection;

// Prepare a string with a human-readable symbol name
function DiaxFormatSymbolName(
  const Symbol: IDiaSymbol
): String;

// Prepare a string with a human-readable line number name
function DiaxFormatLineNumberName(
  const Line: IDiaLineNumber
): String;

// Prepare a string with a human-readable source file name
function DiaxFormatSourceFileName(
  const SourceFile: IDiaSourceFile
): String;

// Prepare a string with a human-readable input assembly name
function DiaxFormatInputAssemblyName(
  const AssemblyFile: IDiaInputAssemblyFile
): String;

// Prepare a string with a human-readable section name
function DiaxFormatSectionName(
  const Section: IDiaSectionContrib
): String;

// Prepare a string with a human-readable segment name
function DiaxFormatSegmentName(
  const Segment: IDiaSegment
): String;

// Prepare a string with a human-readable injected source name
function DiaxFormatInjectedSourceName(
  const InjectedSource: IDiaInjectedSource
): String;

// Prepare a string with a human-readable injected source name
function DiaxFormatFrameDataName(
  const FrameData: IDiaFrameData
): String;

// Prepare a string with a human-readable table name
function DiaxFormatTableName(
  const Table: IDiaTable
): String;

// Prepare a string with a human-readable debug stream name
function DiaxFormatDebugStreamName(
  const DebugStream: IDiaEnumDebugStreamData
): String;

// Prepare a string with a human-readable image data object name
function DiaxFormatImageData(
  const ImageData: IDiaImageData
): String;

// Prepare a string with a human-readable binary data object name
function DiaxFormatBinaryData(
  const BinaryData: IBinaryData
): String;

implementation

uses
  NtUtils.SysUtils, DelphiUtils.LiteRTTI, DelphiUiLib.Strings, Ntapi.ImageHlp;

var
  TagNamesCacheInitialized: Boolean;
  TagNamesCache: array [TSymTagEnum] of String;

procedure InitializeTagNamesCache;
var
  Tag: TSymTagEnum;
  Formatter: IRttixTypeFormatter;
begin
  Formatter := RttixMakeTypeFormatter(TypeInfo(TSymTagEnum));

  for Tag := Low(TSymTagEnum) to High(TSymTagEnum) do
    TagNamesCache[Tag] := Formatter.FormatText(Tag);

  TagNamesCacheInitialized := True;
end;

function FormatSymbolTag;
begin
  if Tag > High(TSymTagEnum) then
    Exit('Unknown Symbol Tag (' + RtlxIntToDec(Cardinal(Tag)) + ')');

  if not TagNamesCacheInitialized then
    InitializeTagNamesCache;

  Result := TagNamesCache[Tag];
end;

var
  BasicTypeNamesCacheInitialized: Boolean;
  BasicTypeNamesCache: array [TBasicType] of String;

procedure InitializeBasicTypeNamesCache;
var
  Basic: TBasicType;
  Formatter: IRttixTypeFormatter;
begin
  Formatter := RttixMakeTypeFormatter(TypeInfo(TBasicType),
    RttixPreserveEnumCase);

  for Basic := Low(TBasicType) to High(TBasicType) do
    BasicTypeNamesCache[Basic] := Formatter.FormatText(Basic);

  BasicTypeNamesCacheInitialized := True;
end;

function FormatBasicType;
begin
  if Basic > High(TBasicType) then
    Exit('Unknown Basic Type (' + RtlxIntToDec(Cardinal(Basic)) + ')');

  if not BasicTypeNamesCacheInitialized then
    InitializeBasicTypeNamesCache;

  Result := BasicTypeNamesCache[Basic];
end;

function FormatArrayIndex;
var
  Short: ShortString;
begin
  Str(Index, Short);
  Insert('[', Short, 0);
  Insert(']', Short, Length(Short) + 1);
  Result := String(Short);
end;

function FormatBuffer;
begin
  Result.ValidFormats := [rfText, rfHint];

  if Buffer.Size = 0 then
    Result.Text := '(Empty)'
  else if PrefixSize then
    Result.Text := UiLibBytesToString(Buffer.Size) + ' as Hex: '
  else
    Result.Text := '';

  Result.Text := Result.Text + RtlxBytesToHexStr(Buffer.Data, Buffer.Size,
    SpaceBytes);
  Result.Hint := BuildHint([
    THintSection.New('Size (decimal)', UiLibUIntToDec(Buffer.Size)),
    THintSection.New('Size (hex)', UiLibUIntToHex(Buffer.Size))
  ]);
end;

function DiaxFormatSymbolName;
var
  Name: String;
  WideName: WideString;
  Tag: TSymTagEnum;
  Basic: TBasicType;
  NameKnown, TagKnown: Boolean;
  Parent: IDiaSymbol;
begin
  NameKnown := (Symbol.get_name(WideName) = S_OK) and (WideName <> '');
  TagKnown := Symbol.get_symTag(Tag) = S_OK;

  if NameKnown and TagKnown then
  begin
    // Shorten compiland names by removing the path
    if Tag = SymTagCompiland then
      Name := RtlxExtractNamePath(WideName)
    else
      Name := WideName;

    if Name <> '' then
      Result := FormatSymbolTag(Tag) + ' "' + Name + '"'
    else
      Result := FormatSymbolTag(Tag);
  end
  else if TagKnown then
  begin
    Result := FormatSymbolTag(Tag);

    case Tag of
      SymTagBaseType:
        if Symbol.get_baseType(Basic) = S_OK then
          Result := Result + ' ' + FormatBasicType(Basic);

      SymTagHeapAllocationSite:
        if (Symbol.get_lexicalParent(Parent) = S_OK) and
          (Parent.get_name(WideName) = S_OK) and (WideName <> '') then
          Result := Result + ' In "' + WideName + '"';
    end;
  end
  else
    Result := 'Symbol';

  Result := '(' + Result + ')';
end;

function UIntToStr(Value: Cardinal): String;
var
  ShortResult: ShortString;
begin
  Str(Value, ShortResult);
  Result := String(ShortResult);
end;

function DiaxFormatLineNumberName;
var
  Number: Cardinal;
  SourceFile: IDiaSourceFile;
  SourceFileName: WideString;
begin
  if Line.get_lineNumber(Number) = S_OK then
  begin
    Result := '(Line Number ' + UIntToStr(Number);

    if (Line.get_sourceFile(SourceFile) = S_OK) and
      (SourceFile.get_fileName(SourceFileName) = S_OK) and
      (SourceFileName <> '') then
      Result := Result + ' in "' + RtlxExtractNamePath(SourceFileName) + '")'
    else
      Result := Result + ')';
  end
  else
    Result := '(Line Number)';
end;

function DiaxFormatSourceFileName;
var
  Name: WideString;
begin
  if (SourceFile.get_fileName(Name) = S_OK) and (Name <> '') then
    Result := '(Source File "' + RtlxExtractNamePath(Name) + '")'
  else
    Result := '(Source File)';
end;

function DiaxFormatInputAssemblyName;
var
  Name: WideString;
begin
  if (AssemblyFile.get_fileName(Name) = S_OK) and (Name <> '') then
    Result := '(Input Assembly File "' + RtlxExtractNamePath(Name) + '")'
  else
    Result := '(Input Assembly File)';
end;

function DiaxFormatSectionName;
var
  RVA: Cardinal;
begin
  if Section.get_relativeVirtualAddress(RVA) = S_OK then
    Result := '(Section Contribution at ' +
      RtlxIntToHex(RVA, NUMERIC_WIDTH_ROUND_TO_GROUP, True, npSpace) + ')'
  else
    Result := '(Section Contribution)';
end;

function DiaxFormatSegmentName;
var
  RVA: Cardinal;
  Access: LongBool;
  Permissions: String;
begin
  if Segment.get_relativeVirtualAddress(RVA) = S_OK then
  begin
    Permissions := '';

    if (Segment.get_read(Access) = S_OK) and Access then
      Permissions := Permissions + 'R';

    if (Segment.get_write(Access) = S_OK) and Access then
      Permissions := Permissions + 'W';

    if (Segment.get_execute(Access) = S_OK) and Access then
      Permissions := Permissions + 'X';

    if Permissions = '' then
      Permissions := 'NA';

    Result := '(Segment at ' + RtlxIntToHex(RVA, NUMERIC_WIDTH_ROUND_TO_GROUP,
      True, npSpace) + ' [' + Permissions + '])';
  end
  else
    Result := '(Segment)';
end;

function DiaxFormatInjectedSourceName;
var
  Name: WideString;
begin
  if (InjectedSource.get_filename(Name) = S_OK) and (Name <> '') then
    Result := '(Injected Source "' + RtlxExtractNamePath(Name) + '")'
  else
    Result := '(Injected Source)';
end;

function DiaxFormatFrameDataName;
var
  RVA: Cardinal;
begin
  if FrameData.get_relativeVirtualAddress(RVA) = S_OK then
    Result := '(Frame Data at ' + UiLibUIntToHex(RVA) + ')'
  else
    Result := '(Frame Data)';
end;

function DiaxFormatTableName;
var
  Name: WideString;
begin
  if (Table.get_name(Name) = S_OK) and (Name <> '') then
    Result := '(Table "' + RtlxExtractNamePath(Name) + '")'
  else
    Result := '(Table)';
end;

function DiaxFormatDebugStreamName;
var
  Name: WideString;
begin
  if (DebugStream.get_name(Name) = S_OK) and (Name <> '') then
    Result := '(Debug Stream "' + Name + '")'
  else
    Result := '(Debug Stream)';
end;

function DiaxFormatImageData;
begin
  Result := '(Image Data)';
end;

function DiaxFormatBinaryData;
var
  ImageSection: IImageSectionHeader;
  Dummy: IUnknown;
  Name: String;
begin
  if BinaryData.QueryInterface(IImageSectionHeader, ImageSection) = S_OK then
  begin
    Name := ImageSection.SectionName;
    if Name <> '' then
      Result := '(Image Section Header "' + Name + '")'
    else
      Result := '(Image Section Header)';
  end
  else if BinaryData.QueryInterface(IXFixupData, Dummy) = S_OK then
    Result := '(Fixup Data)'
  else if BinaryData.QueryInterface(IFpoData, Dummy) = S_OK then
    Result := '(Frame Pointer Omission Data)'
  else if BinaryData.QueryInterface(IFrameData, Dummy) = S_OK then
    Result := '(Frame Data)'
  else if BinaryData.QueryInterface(IOmapData, Dummy) = S_OK then
    Result := '(OMAP Data)'
  else
    Result := '(Binary Data of ' + UiLibBytesToString(BinaryData.GetData.Size) + ')'
end;

end.
