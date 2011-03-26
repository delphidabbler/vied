inherited ResCompilerDlg: TResCompilerDlg
  Left = 516
  Top = 367
  Caption = 'Specify Resource Compiler'
  OnShow = FormShow
  ExplicitWidth = 504
  ExplicitHeight = 361
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 329
    Height = 143
    ExplicitWidth = 329
    ExplicitHeight = 143
    object lblCompiler: TLabel
      Left = 0
      Top = 0
      Width = 123
      Height = 13
      Caption = '&Path to resource compiler:'
      FocusControl = edCompiler
    end
    object sbBrowse: TSpeedButton
      Left = 306
      Top = 15
      Width = 23
      Height = 22
      Hint = 'Browse for compiler...'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777000000000007777700333333333077770B033333333307770FB033333333
        30770BFB0333333333070FBFB000000000000BFBFBFBFB0777770FBFBFBFBF07
        77770BFB00000007777770007777777700077777777777777007777777770777
        0707777777777000777777777777777777777777777777777777}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbBrowseClick
    end
    object lblCmdLine: TLabel
      Left = 0
      Top = 46
      Width = 212
      Height = 13
      Caption = '&Command line to use to compile a source file:'
      FocusControl = edCmdLine
    end
    object lblHelp1: TLabel
      Left = 0
      Top = 86
      Width = 276
      Height = 26
      Caption = 
        'In the above command line use the following placeholders for fil' +
        'enames:'
      WordWrap = True
    end
    object lblHelp2: TLabel
      Left = 0
      Top = 114
      Width = 224
      Height = 13
      Caption = '   '#183'   <SRC> to represent a source (.rc) input file.'
    end
    object lblHelp3: TLabel
      Left = 0
      Top = 130
      Width = 228
      Height = 13
      Caption = '   '#183'   <BIN> to represent a binary (.res) output file.'
    end
    object edCompiler: TEdit
      Left = 0
      Top = 16
      Width = 297
      Height = 21
      TabOrder = 0
      OnChange = edCompilerChange
    end
    object edCmdLine: TEdit
      Left = 0
      Top = 62
      Width = 329
      Height = 21
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
