object MainForm: TMainForm
  Left = 334
  Top = 148
  ClientHeight = 321
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object DisplayHeader: THeader
    Left = 0
    Top = 0
    Width = 587
    Height = 19
    Align = alTop
    AllowResize = False
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Sections.Sections = (
      #0'42'#0'Label'
      #0'43'#0'Value')
    TabOrder = 0
  end
  object DisplayListBox: TListBox
    Left = 0
    Top = 19
    Width = 587
    Height = 302
    Align = alClient
    TabOrder = 1
    OnDblClick = MECurrentClick
  end
  object MainMenu: TMainMenu
    Left = 168
    object MFile: TMenuItem
      Caption = '&File'
      OnClick = MFileClick
      object MFNew: TMenuItem
        Caption = '&New'
        ShortCut = 16462
        OnClick = MFNewClick
      end
      object MFOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = MFOpenClick
      end
      object MFSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = MFSaveClick
      end
      object MFSaveAs: TMenuItem
        Caption = 'Save &As...'
        ShortCut = 16449
        OnClick = MFSaveAsClick
      end
      object MFExport: TMenuItem
        Caption = '&Export...'
        ShortCut = 16453
        OnClick = MFExportClick
      end
      object MFSpacer0: TMenuItem
        Caption = '-'
      end
      object MFCompile: TMenuItem
        Caption = '&Compile'
        ShortCut = 120
        OnClick = MFCompileClick
      end
      object MFSpacer1: TMenuItem
        Caption = '-'
      end
      object MFPreferences: TMenuItem
        Caption = 'Save As &Preferences'
        OnClick = MFPreferencesClick
      end
      object MFViewRC: TMenuItem
        Caption = '&View RC Statements...'
        OnClick = MFViewRCClick
      end
      object MFSpacer2: TMenuItem
        Caption = '-'
      end
      object MFExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MFExitClick
      end
    end
    object MEdit: TMenuItem
      Caption = '&Edit'
      OnClick = MEditClick
      object MECurrent: TMenuItem
        Caption = 'C&urrent Item...'
        ShortCut = 113
        OnClick = MECurrentClick
      end
      object MESpacer0: TMenuItem
        Caption = '-'
      end
      object MERCIdentifier: TMenuItem
        Caption = 'RC &Identifier...'
        OnClick = MERCIdentifierClick
      end
      object MERCComments: TMenuItem
        Caption = '&RC Comments...'
        OnClick = MERCCommentsClick
      end
      object MEVIComments: TMenuItem
        Caption = '&VI Comments...'
        OnClick = MEVICommentsClick
      end
      object MECompOut: TMenuItem
        Caption = 'Compiler &Output Folder...'
        OnClick = MECompOutFolderClick
      end
      object MESpacer1: TMenuItem
        Caption = '-'
      end
      object MEAnalyse: TMenuItem
        Caption = '&Analyse...'
        OnClick = MEAnalyseClick
      end
      object MESpacer2: TMenuItem
        Caption = '-'
      end
      object MECopy: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = MECopyClick
      end
    end
    object MOptions: TMenuItem
      Caption = '&Options'
      object MOAutoValidate: TMenuItem
        Caption = 'Automatic &Validation'
        OnClick = MOAutoValidateClick
      end
      object MODescribeFileFlags: TMenuItem
        Caption = '&Describe File Flags'
        OnClick = MODescribeFileFlagsClick
      end
      object MOSpacer: TMenuItem
        Caption = '-'
      end
      object MOUserSetup: TMenuItem
        Caption = '&User Setup...'
        OnClick = MOUserSetupClick
      end
      object MOResCompiler: TMenuItem
        Caption = '&Resource Compiler...'
        OnClick = MOResCompilerClick
      end
    end
    object MHelp: TMenuItem
      Caption = '&Help'
      object MHContents: TMenuItem
        Caption = '&Contents'
        ShortCut = 16496
        OnClick = MHContentsClick
      end
      object MHOverview: TMenuItem
        Caption = '&Overview'
        OnClick = MHOverviewClick
      end
      object MHSpacer1: TMenuItem
        Caption = '-'
      end
      object MHWebsite: TMenuItem
        Caption = '&Visit DelphiDabbler.com'
        OnClick = MHWebsiteClick
      end
      object MHSpacer2: TMenuItem
        Caption = '-'
      end
      object MHAbout: TMenuItem
        Caption = '&About...'
        OnClick = MHAboutClick
      end
    end
  end
  object AboutBoxDlg: TPJAboutBoxDlg
    Title = 'About'
    ButtonPlacing = abpRight
    ButtonKind = abkDone
    DlgLeft = 60
    DlgTop = 40
    CentreDlg = False
    VersionInfo = AboutVersionInfo
    Position = abpOwner
    UseOwnerAsParent = True
    Left = 232
    Top = 32
  end
  object AboutVersionInfo: TPJVersionInfo
    Left = 200
    Top = 32
  end
  object SaveDlg: TSaveDialog
    Filter = 'Version info files (*.vi)|*.vi'
    Options = [ofHideReadOnly, ofShowHelp, ofNoReadOnlyReturn]
    Left = 232
  end
  object OpenDlg: TOpenDialog
    DefaultExt = 'VI'
    Filter = 'Version info files (*.vi)|*.vi'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist]
    Left = 200
  end
  object WdwState: TPJRegWdwState
    AutoSaveRestore = True
    OnGetRegData = WdwStateGetRegData
    Left = 200
    Top = 64
  end
  object ExportDlg: TSaveDialog
    Options = [ofHideReadOnly, ofShowHelp, ofNoReadOnlyReturn]
    Title = 'Export File'
    Left = 264
  end
  object CompilerDlg: TSaveDialog
    Filter = 'Resource binary files (*.res)|*.res'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofEnableSizing]
    Title = 'Compile To File'
    Left = 296
  end
  object FileCatcher: TPJFormDropFiles
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = FileCatcherDropFiles
    Left = 264
    Top = 32
  end
end
