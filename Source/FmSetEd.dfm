inherited SetEditor: TSetEditor
  Caption = ''
  OnShow = FormShow
  ExplicitWidth = 504
  ExplicitHeight = 361
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 249
    Height = 201
    ExplicitWidth = 249
    ExplicitHeight = 201
    object lblSet: TLabel
      Left = 0
      Top = 0
      Width = 216
      Height = 13
      Caption = 'Check items that are to be are included in set:'
    end
    object clbSet: TCheckListBox
      Left = 0
      Top = 16
      Width = 249
      Height = 185
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
