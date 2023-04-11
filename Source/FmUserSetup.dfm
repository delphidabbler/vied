inherited UserSetupDlg: TUserSetupDlg
  Caption = 'User Setup'
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 441
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 241
    Height = 111
    ExplicitWidth = 241
    ExplicitHeight = 111
    object lblDesc: TLabel
      Left = 0
      Top = 0
      Width = 244
      Height = 13
      Caption = 'Check the options you want to be used by default:'
    end
    object chkValidate: TCheckBox
      Left = 0
      Top = 24
      Width = 281
      Height = 17
      Caption = '&Automatic validation'
      TabOrder = 0
    end
    object chkDescFileFlags: TCheckBox
      Left = 0
      Top = 55
      Width = 281
      Height = 17
      Caption = '&Describe file flags in main window'
      TabOrder = 1
    end
    object chkUseUTF8: TCheckBox
      Left = 0
      Top = 87
      Width = 281
      Height = 17
      Caption = '&Use UTF-8 as default encoding for .vi files'
      TabOrder = 2
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
