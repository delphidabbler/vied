{
 * FmGenericDlg.pas
 *
 * Implements a generic base class from which other program dialog boxes can be
 * descended. Provides layout and functionality common to all dlg boxes.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FmGenericDlg.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmGenericDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, ExtCtrls, Classes, Forms;


type

  {
  TGenericDlg:
    Generic base class for other dialog boxes - displays and handles help button
    and permits aligning of child dlg boxes to this window
  }
  TGenericDlg = class(TForm)
    bvlBottom: TBevel;
    pnlBody: TPanel;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    procedure AlignToOwner; virtual;
      {Aligns this dialog box relative to its owner form. Called automatically
      when form is created}
    procedure ArrangeControls; virtual;
      {Arranges controls within window}
  end;


implementation


uses
  // Delphi
  Math, Windows,
  // Project
  UDlgParent;


{$R *.DFM}


resourcestring
  // Message displayed if no help context is asigned and help button is clicked
  sNoHelp = 'Sorry there is no help available.';


{ TGenericDlg }

procedure TGenericDlg.AlignToOwner;
  {Aligns this dialog box relative to its owner form. Called automatically when
  form is created}
var
  OwnerForm: TForm;     // Form that owns this dialog box
begin
  // Check that we have a owner that is a form, if not do nothing
  if not (Owner is TForm) then
    Exit;
  OwnerForm := Owner as TForm;
  // Now display according to who owns us
  if OwnerForm.BorderStyle = bsDialog then
  begin
    // We're centering over another dlg box - just offset down and left a bit
    Self.Left := Max(0, Min(OwnerForm.Left + 40, Screen.Width - Self.Width));
    Self.Top := Max(0, Min(OwnerForm.Top + 40, Screen.Height - Self.Height));
  end
  else
  begin
    // We're probably centering over a main window -
    // centre horizontally over form, while keeping on screen
    Self.Left :=
      Max(0, Min(OwnerForm.Left + (OwnerForm.Width - Self.Width) div 2,
        Screen.Width - Self.Width));
    // "centre" 1/3rd way down main window if possible}
    Self.Top :=
      Max(0, Min(OwnerForm.Top + (OwnerForm.Height - Self.Height) div 3,
        Screen.Height - Self.Height));
  end;
end;

procedure TGenericDlg.ArrangeControls;
  {Arranges controls within window}
begin
  ClientWidth := pnlBody.Width + 16;
  bvlBottom.Top := pnlBody.Height + 16;
  btnHelp.Top := bvlBottom.Top + 8;
  ClientHeight := btnHelp.Top + btnHelp.Height + 4;
  bvlBottom.Width := pnlBody.Width;
  btnHelp.Left := ClientWidth - 8 - btnHelp.Width;
end;

procedure TGenericDlg.btnHelpClick(Sender: TObject);
  {Help button click event handler - if there's a help context assigned then
  calls main help file with topic whose context is stored in HelpContext
  otherwise a warning dialog box is displayed}
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext)
  else
    MessageBox(0, PChar(sNoHelp), PChar(Application.Title), MB_OK);
end;

procedure TGenericDlg.FormCreate(Sender: TObject);
  {Form creation event handler - arranges controls, sizes form and applies a
  Delphi bug fix}
begin
  // Set dialog box parent window to its owner
  TDlgParent.SetParentToOwner(Self);
  // Position components
  ArrangeControls;
  // Align form to owner
  AlignToOwner;
end;

procedure TGenericDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Checks if user has pressed F1 and acts as if help button has been pressed if
  so}
begin
  if (Key = VK_F1) and (Shift = []) then
  begin
    // F1 pressed with no modifier
    btnHelp.Click;
    Key := 0;
  end;
end;

end.

