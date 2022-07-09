object MainForm: TMainForm
  Left = 334
  Top = 148
  ClientHeight = 321
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  Position = poDesigned
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object DisplayListView: TListView
    Left = 0
    Top = 0
    Width = 587
    Height = 321
    Align = alClient
    Columns = <
      item
        Caption = 'Label'
        Width = 140
      end
      item
        Caption = 'Value'
        Width = 300
      end>
    Groups = <
      item
        Header = 'Fixed File Information'
        GroupID = 0
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Translation Information'
        GroupID = 1
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'String Information'
        GroupID = 2
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end>
    GroupView = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = DisplayListViewDblClick
  end
  object MainMenu: TMainMenu
    Left = 32
    Top = 32
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
      object MFClearPreferences: TMenuItem
        Caption = 'Clear Preferences'
        OnClick = MFClearPreferencesClick
      end
      object MFSpacer2: TMenuItem
        Caption = '-'
      end
      object MFViewRC: TMenuItem
        Caption = '&View RC Statements...'
        OnClick = MFViewRCClick
      end
      object MFSpacer3: TMenuItem
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
        Caption = 'Edit C&urrent Item...'
        ShortCut = 113
        OnClick = MECurrentClick
      end
      object MEClearCurrent: TMenuItem
        Caption = 'Cle&ar Current Item'
        ShortCut = 16430
        OnClick = MEClearCurrentClick
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
      object MEMacros: TMenuItem
        Caption = 'Macros...'
        OnClick = MEMacrosClick
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
        ShortCut = 112
        OnClick = MHContentsClick
      end
      object MHHowDoI: TMenuItem
        Caption = '&How Do I?'
        OnClick = MHHowDoIClick
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
      object MHLicense: TMenuItem
        Caption = 'License'
        OnClick = MHLicenseClick
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
    VersionInfo = AboutVersionInfo
    Position = abpOwner
    UseOwnerAsParent = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 32
    Top = 200
  end
  object AboutVersionInfo: TPJVersionInfo
    Left = 32
    Top = 80
  end
  object WdwState: TPJRegWdwState
    AutoSaveRestore = True
    OnGetRegData = WdwStateGetRegData
    Left = 32
    Top = 136
  end
  object FileCatcher: TPJFormDropFiles
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = FileCatcherDropFiles
    Left = 32
    Top = 264
  end
end
