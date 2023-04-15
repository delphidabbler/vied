inherited FileEncodingDlg: TFileEncodingDlg
  Caption = 'Choose .vi File Encoding'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 239
    Height = 111
    ExplicitWidth = 239
    ExplicitHeight = 111
    object Panel1: TPanel
      Left = 10
      Top = 10
      Width = 241
      Height = 111
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      BevelOuter = bvNone
      TabOrder = 0
      object lblDesc: TLabel
        Left = 0
        Top = 0
        Width = 206
        Height = 13
        Caption = 'Choose the encoding to use for the .vi file:'
      end
      object rbANSI: TRadioButton
        Left = 0
        Top = 32
        Width = 113
        Height = 17
        Caption = '&ANSI'
        TabOrder = 0
      end
      object rbUTF8: TRadioButton
        Left = 0
        Top = 64
        Width = 113
        Height = 17
        Caption = '&UTF-8'
        TabOrder = 1
      end
    end
  end
end
