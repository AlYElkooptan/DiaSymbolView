program DiaSymbolView;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {DiaSymbolViewMain},
  DiaNodes in 'DiaNodes.pas',
  DevirtualizedTree in 'NtUtilsUI\Components\DevirtualizedTree.pas',
  DevirtualizedTree.Provider in 'NtUtilsUI\Components\DevirtualizedTree.Provider.pas',
  VirtualTreesEx.DefaultMenu in 'NtUtilsUI\Components\VirtualTreesEx.DefaultMenu.pas',
  VirtualTreesEx in 'NtUtilsUI\Components\VirtualTreesEx.pas',
  NtUiCommon.Exceptions in 'NtUtilsUI\Common\NtUiCommon.Exceptions.pas',
  NtUiCommon.Helpers in 'NtUtilsUI\Common\NtUiCommon.Helpers.pas',
  DiaProperties in 'DiaProperties.pas',
  VclEx.Form in 'NtUtilsUI\VclEx\VclEx.Form.pas',
  VclEx.Edit in 'NtUtilsUI\VclEx\VclEx.Edit.pas',
  NtUiFrame in 'NtUtilsUI\Prototypes\NtUiFrame.pas' {BaseFrame: TFrame},
  NtUiFrame.Search in 'NtUtilsUI\Prototypes\NtUiFrame.Search.pas' {SearchFrame: TFrame},
  NtUiCommon.Interfaces in 'NtUtilsUI\Common\NtUiCommon.Interfaces.pas',
  TextHelpers in 'TextHelpers.pas',
  Settings in 'Settings.pas',
  RegisterNames in 'RegisterNames.pas';

{$R *.res}
{$WEAKLINKRTTI ON}

begin
  EnableNtUiLibExceptionHandling;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.HintHidePause := 20000;
  Application.CreateForm(TDiaSymbolViewMain, DiaSymbolViewMain);
  Application.Run;
end.
