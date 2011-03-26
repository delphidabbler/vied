inherited IdEditor: TIdEditor
  Caption = 'Edit Identifier'
  OnShow = FormShow
  ExplicitWidth = 504
  ExplicitHeight = 361
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 281
    Height = 48
    ExplicitWidth = 281
    ExplicitHeight = 48
    object lblID: TLabel
      Left = 0
      Top = 0
      Width = 190
      Height = 13
      Caption = 'Enter new &Identifier (max 31 characters):'
      FocusControl = edId
    end
    object edId: TEdit
      Left = 0
      Top = 24
      Width = 281
      Height = 21
      MaxLength = 31
      TabOrder = 0
      OnKeyPress = edIdKeyPress
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
