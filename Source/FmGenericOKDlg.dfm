inherited GenericOKDlg: TGenericOKDlg
  Caption = 'GenericOKDlg'
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnHelp: TButton
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 302
    Top = 374
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 221
    Top = 374
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
