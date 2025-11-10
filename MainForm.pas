unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees,
  VirtualTreesEx, DevirtualizedTree, Vcl.Menus, VclEx.Form, Ntapi.msdia,
  NtUiFrame.Search, NtUiFrame;

type
  TDiaxOpenMode = (opUnknown, omFromPdb, omFromExe);

  TDiaSymbolViewMain = class(TFormEx)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuOpenPdb: TMenuItem;
    Tree: TDevirtualizedTree;
    OpenDialog: TOpenDialog;
    MenuSetMsdiaPath: TMenuItem;
    MenuOptions: TMenuItem;
    MenuSetSearchPath: TMenuItem;
    SearchBox: TSearchFrame;
    MenuSorting: TMenuItem;
    MenuNoSort: TMenuItem;
    MenuSort: TMenuItem;
    MenuStackTraces: TMenuItem;
    MenuClose: TMenuItem;
    MenuExit: TMenuItem;
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuSetMsdiaPathClick(Sender: TObject);
    procedure MenuSetSearchPathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuSortClick(Sender: TObject);
    procedure MenuStackTracesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuCloseClick(Sender: TObject);
  private
    procedure OpenFile(const FileName: String; Mode: TDiaxOpenMode = opUnknown);
    procedure WMDropFiles(var AMessage: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  DiaSymbolViewMain: TDiaSymbolViewMain;

implementation

uses
  NtUtils, NtUtils.DbgHelp.Dia, NtUiCommon.Helpers, NtUtils.SysUtils,
  NtUtils.WinUser, DiaNodes, DiaProperties, System.UITypes, Settings;

{$R *.dfm}

procedure TDiaSymbolViewMain.CreateParams;
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_ACCEPTFILES;
end;

procedure TDiaSymbolViewMain.FormClose;
begin
  TSettings.Save;
end;

procedure TDiaSymbolViewMain.FormCreate;
begin
  SearchBox.AttachToTree(Tree);
  TSettings.Load;
  MenuStackTraces.Checked := TSettings.ShowStackTraces;

  if TSettings.EnableSorting then
    MenuSort.Checked := True
  else
    MenuNoSort.Checked := True;

  if ParamCount >= 1 then
    OpenFile(ParamStr(1));
end;

procedure TDiaSymbolViewMain.MenuStackTracesClick;
begin
  TSettings.ShowStackTraces := MenuStackTraces.Checked;
end;

procedure TDiaSymbolViewMain.MenuCloseClick;
begin
  Tree.BeginUpdateAuto;
  Tree.Clear;
  SearchBox.ClearQuery;
  DiaxSetCurrentSession(nil);
  Tree.NoItemsText := 'Go to File -> Open or drop a .pdb file here';
  Caption := 'DiaSymbolView - PDB File Inspection Tool';
  Tree.SetFocus;
end;

procedure TDiaSymbolViewMain.MenuExitClick;
begin
  Close;
end;

procedure TDiaSymbolViewMain.MenuOpenClick;
var
  Mode: TDiaxOpenMode;
begin
  if not OpenDialog.Execute(Handle) then
    Exit;

  case OpenDialog.FilterIndex of
    1: Mode := omFromPdb;
    2: Mode := omFromExe;
  else
    Mode := opUnknown;
  end;

  OpenFile(OpenDialog.FileName, Mode);
end;

procedure TDiaSymbolViewMain.MenuSetMsdiaPathClick;
var
  Path: String;
begin
  Path := TSettings.MsdiaDllPath;
  InputQuery('MSDIA DLL', 'Path:', Path);
  TSettings.MsdiaDllPath := Path;
end;

procedure TDiaSymbolViewMain.MenuSetSearchPathClick;
var
  Path: String;
begin
  Path := TSettings.SymbolSearchPath;
  InputQuery('Symbol Search Path', 'Path:', Path);
  TSettings.SymbolSearchPath := Path;
end;

procedure TDiaSymbolViewMain.MenuSortClick;
begin
  if TSettings.EnableSorting = (Sender = MenuSort) then
    Exit;

  if Tree.RootNodeCount > 0 then
    TaskMessageDlg('Deferred setting effect', 'The change will take effect ' +
      'from the next symbol enumeration.',
      TMsgDlgType.mtWarning, [mbOK], 0);

  TSettings.EnableSorting := (Sender = MenuSort);
  (Sender as TMenuItem).Checked := True;
end;

procedure TDiaSymbolViewMain.OpenFile;
var
  Node: INodeProvider;
  Session: IDiaSession;
begin
  // If no mode already selected, choose it based on the extension
  case Mode of
    omFromPdb, omFromExe: ;
  else
    if RtlxEqualStrings(RtlxExtractExtensionPath(FileName), 'pdb') then
      Mode := omFromPdb
    else
      Mode := omFromExe;
  end;

  // Load the file
  case Mode of
    omFromPdb: DiaxRtlxLoadPdb(Session, FileName).RaiseOnError;
    omFromExe: DiaxRtlxLoadExe(Session, FileName,
      TSettings.SymbolSearchPath).RaiseOnError;
  else
    Exit;
  end;

  Tree.BeginUpdateAuto;
  Tree.Clear;
  SearchBox.ClearQuery;
  DiaxSetCurrentSession(Session);

  for Node in DiaxMakeSessionNodeChildren(Session) do
    Tree.AddChildEx(nil, Node);

  Tree.NoItemsText := 'Nothing to display';
  Caption := 'DiaSymbolView - ' + RtlxExtractNamePath(FileName);
end;

procedure TDiaSymbolViewMain.WMDropFiles;
begin
  ShlxDelayedFinishDrag(AMessage.Drop);
  OpenFile(ShlxQueryDragFile(AMessage.Drop, 0));
end;

end.
