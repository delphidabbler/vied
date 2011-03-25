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
    ///  <summary>Form creation event handler. Arranges controls and sizes form.
    ///  </summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Help button click event handler. Attempts to display help
    ///  topic matching A-link keyword associated with the form.</summary>
    procedure btnHelpClick(Sender: TObject);
    ///  <summary>Handles key presses on form. Acts as if help button has been
    ///  pressed if user presses F1.</summary>
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  strict private
    ///  <summary>Gets help A-link keyword for this dialog.</summary>
    ///  <remarks>If HelpKeyword property is set its value is returned,
    ///  otherwise the name of the form is used.</remarks>
    function GetHelpALinkKeyword: string;
  strict protected
    ///  <summary>Arranges controls within form.</summary>
    procedure ArrangeControls; virtual;
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UDlgParent, UHelp;


{$R *.DFM}


resourcestring
  // Message displayed if no help context is asigned and help button is clicked
  sNoHelp = 'Sorry there is no help available.';


{ TGenericDlg }

procedure TGenericDlg.ArrangeControls;
begin
  ClientWidth := pnlBody.Width + 16;
  bvlBottom.Top := pnlBody.Height + 16;
  btnHelp.Top := bvlBottom.Top + 8;
  ClientHeight := btnHelp.Top + btnHelp.Height + 4;
  bvlBottom.Width := pnlBody.Width;
  btnHelp.Left := ClientWidth - 8 - btnHelp.Width;
end;

procedure TGenericDlg.btnHelpClick(Sender: TObject);
begin
  THelp.ShowALink(GetHelpALinkKeyword, THelp.DlgErrTopic);
end;

procedure TGenericDlg.FormCreate(Sender: TObject);
begin
  TDlgParent.SetParentToOwner(Self);
  ArrangeControls;
end;

procedure TGenericDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) and (Shift = []) then
  begin
    // F1 pressed with no modifier
    btnHelp.Click;
    Key := 0;
  end;
end;

function TGenericDlg.GetHelpALinkKeyword: string;
begin
  if HelpKeyword <> '' then
    Result := HelpKeyword
  else
    Result := Name;
end;

end.

