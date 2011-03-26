inherited NumEditor: TNumEditor
  Caption = ''
  OnShow = FormShow
  ExplicitWidth = 504
  ExplicitHeight = 361
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 233
    Height = 67
    ExplicitWidth = 233
    ExplicitHeight = 67
    object lblNum: TLabel
      Left = 0
      Top = 4
      Width = 75
      Height = 13
      Caption = '&Enter a number:'
      FocusControl = edNum
    end
    object lblInstruct: TLabel
      Left = 0
      Top = 36
      Width = 217
      Height = 33
      AutoSize = False
      Caption = 
        'To enter a number in hexadecimal format, preceed the number with' +
        ' a $ character.'
      WordWrap = True
    end
    object edNum: TEdit
      Left = 96
      Top = 0
      Width = 121
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 5
      TabOrder = 0
      OnKeyPress = edNumKeyPress
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
