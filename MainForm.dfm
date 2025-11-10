object DiaSymbolViewMain: TDiaSymbolViewMain
  Left = 0
  Top = 0
  Caption = 'DiaSymbolView - PDB File Inspection Tool'
  ClientHeight = 441
  ClientWidth = 984
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object Tree: TDevirtualizedTree
    Left = 0
    Top = 29
    Width = 984
    Height = 412
    Align = alClient
    ClipboardFormats.Strings = (
      'CSV'
      'Plain text'
      'Unicode text')
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 24
    Header.Height = 24
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoHotTrack, hoRestrictDrag, hoVisible, hoDisableAnimatedResize, hoAutoColumnPopupMenu, hoAutoResizeInclCaption]
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.ExportMode = emSelected
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    NoItemsText = 'Go to File -> Open or drop a .pdb file here'
    Columns = <
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
        Position = 0
        Text = 'Property'
        Width = 450
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAutoSpring, coSmartResize, coAllowFocus, coDisableAnimatedResize, coEditable, coStyleColor]
        Position = 1
        Text = 'Value'
        Width = 500
      end>
  end
  inline SearchBox: TSearchFrame
    AlignWithMargins = True
    Left = 0
    Top = 3
    Width = 984
    Height = 23
    Margins.Left = 0
    Margins.Right = 0
    Align = alTop
    Constraints.MinHeight = 21
    Constraints.MinWidth = 240
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    inherited Splitter: TSplitter
      Left = 818
    end
    inherited tbxSearchBox: TButtonedEditEx
      Width = 818
    end
    inherited cbxColumn: TComboBox
      Left = 824
    end
  end
  object MainMenu: TMainMenu
    Left = 48
    Top = 32
    object MenuFile: TMenuItem
      Caption = 'File'
      ShortCut = 16467
      object MenuOpenPdb: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = MenuOpenClick
      end
      object MenuClose: TMenuItem
        Caption = 'Close'
        ShortCut = 16499
        OnClick = MenuCloseClick
      end
      object MenuExit: TMenuItem
        Caption = 'Exit'
        OnClick = MenuExitClick
      end
    end
    object MenuOptions: TMenuItem
      Caption = 'Options'
      object MenuSetMsdiaPath: TMenuItem
        Caption = 'Set MSDIA DLL path...'
        OnClick = MenuSetMsdiaPathClick
      end
      object MenuSetSearchPath: TMenuItem
        Caption = 'Set symbol search path...'
        OnClick = MenuSetSearchPathClick
      end
      object MenuSorting: TMenuItem
        Caption = 'Entry sorting'
        object MenuNoSort: TMenuItem
          Caption = 'Preserve original order'
          RadioItem = True
          OnClick = MenuSortClick
        end
        object MenuSort: TMenuItem
          Caption = 'Smart sort'
          Checked = True
          RadioItem = True
          OnClick = MenuSortClick
        end
      end
      object MenuStackTraces: TMenuItem
        AutoCheck = True
        Caption = 'Display stack traces on errors'
        OnClick = MenuStackTracesClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'PDB Files|*.pdb|PE Files|*.exe;*.dll;*.sys;*.scr;*.cpl|All Suppo' +
      'rted Files|*.pdb;*.exe;*.dll;*.sys;*.scr;*.cpl|All Files|*'
    Left = 120
    Top = 32
  end
end
