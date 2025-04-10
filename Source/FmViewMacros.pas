{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (www.delphidabbler.com).
 *
 * Dialogue box used to display the values of macros and any warnings associated
 * with them.
}


unit FmViewMacros;

interface

uses
  // Delphi
  ComCtrls,
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  // Project
  FmGenericViewDlg,
  UMacros;

type
  TViewMacrosDlg = class(TGenericViewDlg)
    pcMacros: TPageControl;
    tsMacros: TTabSheet;
    tsWarnings: TTabSheet;
    lvMacros: TListView;
    edWarnings: TMemo;
    procedure FormShow(Sender: TObject);
  strict private
    var
      fMacros: TMacros;
    ///  <summary>Displays the macros in the list view on Macros tab.</summary>
    procedure DisplayMacros;
    ///  <summary>Sizes the list view columns to ensure the data is visible.
    ///  </summary>
    procedure SetLVColumnWidths;
    ///  <summary>Displays any warnings in the memo control on the Warnings tab.
    ///  </summary>
    procedure DisplayWarnings;
  strict protected
  public
    ///  <summary>Displays the dialogue box showing details of the macros
    ///  specified by AMacros.</summary>
    class procedure Show(AOwner: TComponent; const AMacros: TMacros);
  end;

implementation

uses
  // Delphi
  Windows,
  Graphics;

{$R *.dfm}

///  <summary>Returns the size of the given string when rendered on a canvas in
///  the given font.</summary>
function StringExtent(const S: string; const Font: TFont): Windows.TSize;
var
  Canvas: Graphics.TCanvas; // canvas used to measure text extent
begin
  Assert(Assigned(Font));
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := CreateDC('DISPLAY', nil, nil, nil);
    try
      Canvas.Font := Font;
      Result := Canvas.TextExtent(S);
    finally
      DeleteDC(Canvas.Handle);
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Free;
  end;
end;

{ TViewMacrosDlg }

procedure TViewMacrosDlg.DisplayMacros;
var
  ResMacro: TMacros.TResolvedMacro;
  ResMacros: TArray<TMacros.TResolvedMacro>;
  LI: TListItem;
begin;
  ResMacros := fMacros.GetResolvedMacros;
  for ResMacro in ResMacros do
  begin
    LI := lvMacros.Items.Add;
    LI.Caption := ResMacro.Macro;
    LI.SubItems.Add(ResMacro.Value);
  end;
end;

procedure TViewMacrosDlg.DisplayWarnings;
var
  BadFileMacros: TArray<TMacros.TMacroDefinition>;
  BadFileMacro: TMacros.TMacroDefinition;
resourcestring
  sBadFilesPrefix = 'The externally referenced files listed below cannot be '
    + 'found:' + sLineBreak;
  sBadFilesSuffix = 'This means that the macros are either incomplete or are '
    + 'wrong. You can edit the macros using the Edit | Macros menu option.';

  procedure AddLine(const S: string);
  begin
    edWarnings.Lines.Add(S);
  end;

begin
  edWarnings.Clear;
  // Get list of invalid file references in macros
  BadFileMacros := fMacros.GetInvalidFileMacroDefinitions;
  if Length(BadFileMacros) > 0 then
  begin
    AddLine(sBadFilesPrefix);
    for BadFileMacro in BadFileMacros do
      AddLine('  • ' + BadFileMacro.Value);
    AddLine('');
    AddLine(sBadFilesSuffix);
  end;
end;

procedure TViewMacrosDlg.FormShow(Sender: TObject);
begin
  inherited;
  fMacros.Resolve;
  DisplayMacros;
  SetLVColumnWidths;
  DisplayWarnings;
  // display warnings tab iff there are warnings
  tsWarnings.TabVisible := edWarnings.Lines.Count > 0;
end;

procedure TViewMacrosDlg.SetLVColumnWidths;
var
  ResMacros: TArray<TMacros.TResolvedMacro>;
  NameColWidth: Integer;
  MacrosNameWidth: Integer;
  ValueColWidth: Integer;
  MacrosValueWidth: Integer;
  LI: TListItem;
begin;
  // Must be called after DisplayMacros
  ResMacros := fMacros.GetResolvedMacros;
  // Set column widths
  // make sure there is room for the heading text
  NameColWidth := StringExtent(lvMacros.Columns[0].Caption, lvMacros.Font).cx;
  ValueColWidth := StringExtent(lvMacros.Columns[1].Caption, lvMacros.Font).cx;;
  // expand column width for longest text in each column
  for LI in lvMacros.Items do
  begin
    MacrosNameWidth := StringExtent(LI.Caption, lvMacros.Font).cx;
    if NameColWidth <= MacrosNameWidth then
      NameColWidth := MacrosNameWidth;
    MacrosValueWidth := StringExtent(LI.SubItems[0], lvMacros.Font).cx;
    if ValueColWidth < MacrosValueWidth then
      ValueColWidth := MacrosValueWidth;
  end;
  // add a fudge factor to ensure the text is not truncated
  lvMacros.Columns[0].Width := NameColWidth + 24;
  lvMacros.Columns[1].Width := ValueColWidth + 24;
end;

class procedure TViewMacrosDlg.Show(AOwner: TComponent; const AMacros: TMacros);
var
  Dlg: TViewMacrosDlg;
begin
  Assert(Assigned(AMacros), ClassName + '.Show: AMacros is nil');
  Dlg := TViewMacrosDlg.Create(AOwner);
  try
    Dlg.HelpTopic := 'dlg-viewmacros';
    Dlg.fMacros := AMacros;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

end.
