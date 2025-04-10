inherited ViewMacrosDlg: TViewMacrosDlg
  Caption = 'View Macros'
  OnShow = FormShow
  ExplicitWidth = 619
  ExplicitHeight = 441
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object pcMacros: TPageControl
      Left = 0
      Top = 0
      Width = 464
      Height = 346
      ActivePage = tsMacros
      Align = alClient
      TabOrder = 0
      object tsMacros: TTabSheet
        Caption = 'Macros'
        object lvMacros: TListView
          Left = 0
          Top = 0
          Width = 456
          Height = 318
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
            end
            item
              Caption = 'Value'
            end>
          TabOrder = 0
          ViewStyle = vsReport
          ExplicitLeft = 104
          ExplicitTop = 88
          ExplicitWidth = 250
          ExplicitHeight = 150
        end
      end
      object tsWarnings: TTabSheet
        Caption = 'Warnings'
        ImageIndex = 1
        object edWarnings: TMemo
          Left = 0
          Top = 0
          Width = 456
          Height = 318
          Align = alClient
          ReadOnly = True
          TabOrder = 0
          ExplicitLeft = 136
          ExplicitTop = 112
          ExplicitWidth = 185
          ExplicitHeight = 89
        end
      end
    end
  end
end
