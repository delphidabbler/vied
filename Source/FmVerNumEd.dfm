inherited VerNumEditor: TVerNumEditor
  Caption = ''
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 441
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Left = 9
    Top = 9
    Width = 263
    Height = 280
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ExplicitLeft = 9
    ExplicitTop = 9
    ExplicitWidth = 263
    ExplicitHeight = 280
    object pnlLiteralNumbers: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 40
      Width = 263
      Height = 74
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelEdges = [beTop]
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 19
      object lblV: TLabel
        Left = 0
        Top = 21
        Width = 8
        Height = 19
        Caption = 'v'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblDot1: TLabel
        Left = 67
        Top = 16
        Width = 5
        Height = 24
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
        Top = 16
        Width = 5
        Height = 24
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
        Top = 16
        Width = 5
        Height = 24
        Caption = '.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblPrompt: TLabel
        Left = 0
        Top = 0
        Width = 226
        Height = 13
        Caption = 'Enter version numbers in the edit boxes below:'
      end
      object edV1: TEdit
        Left = 18
        Top = 21
        Width = 45
        Height = 21
        MaxLength = 5
        TabOrder = 0
        OnKeyPress = VerEditKeyPress
      end
      object edV2: TEdit
        Left = 77
        Top = 21
        Width = 45
        Height = 21
        MaxLength = 5
        TabOrder = 1
        OnKeyPress = VerEditKeyPress
      end
      object edV3: TEdit
        Left = 136
        Top = 21
        Width = 45
        Height = 21
        MaxLength = 5
        TabOrder = 2
        OnKeyPress = VerEditKeyPress
      end
      object edV4: TEdit
        Left = 195
        Top = 21
        Width = 45
        Height = 21
        MaxLength = 5
        TabOrder = 3
        OnKeyPress = VerEditKeyPress
      end
      object btnPlus1V1: TButton
        Left = 28
        Top = 48
        Width = 25
        Height = 25
        Caption = '+1'
        TabOrder = 4
        OnClick = btnPlus1V1Click
      end
      object btnPlus1V2: TButton
        Left = 87
        Top = 48
        Width = 25
        Height = 25
        Caption = '+1'
        TabOrder = 5
        OnClick = btnPlus1V2Click
      end
      object btnPlus1V3: TButton
        Left = 146
        Top = 48
        Width = 25
        Height = 25
        Caption = '+1'
        TabOrder = 6
        OnClick = btnPlus1V3Click
      end
      object btnPlus1V4: TButton
        Left = 205
        Top = 48
        Width = 25
        Height = 25
        Caption = '+1'
        TabOrder = 7
        OnClick = btnPlus1V4Click
      end
    end
    object pnlMacros: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 126
      Width = 263
      Height = 134
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 115
      object lblField: TLabel
        Left = 1
        Top = 85
        Width = 65
        Height = 13
        Caption = 'Insert Ma&cro:'
        FocusControl = cmbField
      end
      object lblMacros: TLabel
        Left = 1
        Top = 35
        Width = 38
        Height = 13
        Caption = 'M&acros:'
        FocusControl = edMacros
      end
      object lbMacroInstructions: TLabel
        Left = 1
        Top = 0
        Width = 263
        Height = 32
        AutoSize = False
        Caption = 
          'Only use macros that contain valid version number strings, such ' +
          'as 1.2.3.4 or 1,2'
        WordWrap = True
      end
      object cmbField: TComboBox
        Left = 1
        Top = 104
        Width = 160
        Height = 21
        Style = csDropDownList
        DropDownCount = 6
        TabOrder = 1
      end
      object btnInsert: TButton
        Left = 167
        Top = 100
        Width = 75
        Height = 25
        Caption = '&Insert'
        TabOrder = 2
        OnClick = btnInsertClick
      end
      object edMacros: TEdit
        Left = 1
        Top = 54
        Width = 241
        Height = 21
        TabOrder = 0
        OnChange = edMacrosChange
      end
    end
    object pnlRadios: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 263
      Height = 28
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
      object rbUseVersionNumbers: TRadioButton
        Left = 0
        Top = 0
        Width = 131
        Height = 17
        Caption = 'Enter &literal numbers'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = EntryTypeRadioBtnClick
      end
      object rbMacros: TRadioButton
        Left = 126
        Top = 0
        Width = 116
        Height = 17
        Caption = 'Enter &macros or text'
        TabOrder = 1
        OnClick = EntryTypeRadioBtnClick
      end
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
