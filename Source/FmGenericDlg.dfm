object GenericDlg: TGenericDlg
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  ClientHeight = 335
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object bvlBottom: TBevel
    Left = 8
    Top = 296
    Width = 377
    Height = 2
    Shape = bsTopLine
  end
  object pnlBody: TPanel
    Left = 8
    Top = 8
    Width = 377
    Height = 281
    BevelOuter = bvNone
    TabOrder = 0
  end
  object btnHelp: TButton
    Left = 312
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 1
    OnClick = btnHelpClick
  end
end
