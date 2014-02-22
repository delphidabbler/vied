inherited VerNumEditor: TVerNumEditor
  Caption = ''
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 440
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 240
    Height = 72
    ExplicitWidth = 240
    ExplicitHeight = 72
    object lblPrompt: TLabel
      Left = 0
      Top = 0
      Width = 226
      Height = 13
      Caption = 'Enter version numbers in the edit boxes below:'
    end
    object lblV: TLabel
      Left = 0
      Top = 18
      Width = 8
      Height = 20
      Caption = 'v'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblDot1: TLabel
      Left = 67
      Top = 13
      Width = 4
      Height = 20
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblDot2: TLabel
      Left = 126
      Top = 13
      Width = 4
      Height = 20
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblDot3: TLabel
      Left = 185
      Top = 13
      Width = 4
      Height = 20
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edV1: TEdit
      Left = 18
      Top = 18
      Width = 45
      Height = 21
      MaxLength = 5
      TabOrder = 0
      OnKeyPress = VerEditKeyPress
    end
    object edV2: TEdit
      Left = 77
      Top = 18
      Width = 45
      Height = 21
      MaxLength = 5
      TabOrder = 1
      OnKeyPress = VerEditKeyPress
    end
    object edV3: TEdit
      Left = 136
      Top = 18
      Width = 45
      Height = 21
      MaxLength = 5
      TabOrder = 2
      OnKeyPress = VerEditKeyPress
    end
    object edV4: TEdit
      Left = 195
      Top = 18
      Width = 45
      Height = 21
      MaxLength = 5
      TabOrder = 3
      OnKeyPress = VerEditKeyPress
    end
    object btnPlus1V1: TButton
      Left = 28
      Top = 45
      Width = 25
      Height = 25
      Caption = '+1'
      TabOrder = 4
      OnClick = btnPlus1V1Click
    end
    object btnPlus1V2: TButton
      Left = 87
      Top = 45
      Width = 25
      Height = 25
      Caption = '+1'
      TabOrder = 5
      OnClick = btnPlus1V2Click
    end
    object btnPlus1V3: TButton
      Left = 146
      Top = 45
      Width = 25
      Height = 25
      Caption = '+1'
      TabOrder = 6
      OnClick = btnPlus1V3Click
    end
    object btnPlus1V4: TButton
      Left = 205
      Top = 45
      Width = 25
      Height = 25
      Caption = '+1'
      TabOrder = 7
      OnClick = btnPlus1V4Click
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
