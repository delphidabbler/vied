inherited StringEditor: TStringEditor
  BorderStyle = bsSizeable
  Caption = ''
  ClientHeight = 402
  ClientWidth = 603
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 440
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 345
    Height = 300
    ExplicitWidth = 345
    ExplicitHeight = 300
    object lblStr: TLabel
      Left = 0
      Top = 0
      Width = 345
      Height = 16
      AutoSize = False
      FocusControl = edStr
    end
    object lblField: TLabel
      Left = 0
      Top = 112
      Width = 58
      Height = 13
      Caption = 'Insert &Field:'
      FocusControl = cmbField
    end
    object edStr: TMemo
      Left = 0
      Top = 17
      Width = 345
      Height = 89
      HideSelection = False
      ScrollBars = ssVertical
      TabOrder = 0
      WantReturns = False
    end
    object cmbField: TComboBox
      Left = 0
      Top = 128
      Width = 249
      Height = 21
      Style = csDropDownList
      DropDownCount = 6
      TabOrder = 1
      Items.Strings = (
        '#F1'
        '#F2'
        '#F3'
        '#F4'
        '#P1'
        '#P2'
        '#P3'
        '#P4'
        'YEAR'
        'SHORTFNAME')
    end
    object btnInsert: TButton
      Left = 256
      Top = 127
      Width = 75
      Height = 25
      Caption = '&Insert'
      TabOrder = 2
      OnClick = btnInsertClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
