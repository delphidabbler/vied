{
 * FmIdEd.pas
 *
 * Identifier editor dialog box. Allows user to enter a valid version
 * information resource identifier.
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
 * The Original Code is FmIdEd.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1998-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmIdEd;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TIdEditor:
    Class that implements a dialog box that allows user to enter a valid version
    information resource identifier.
  }
  TIdEditor = class(TGenericOKDlg)
    lblID: TLabel;
    edId: TEdit;
    procedure edIdKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    fIdentifier: string;
      {Value of Identifier property}
  public
    property Identifier: string read fIdentifier write fIdentifier;
      {Holds default identifier provided by caller and edited identifier if user
      clicks OK}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelp, UMsgDlgs;


{$R *.DFM}

{ TIdEditor }

procedure TIdEditor.btnOKClick(Sender: TObject);
  {Click event handler for OK button: validates entered identifier and stores
  in Identifier property if valid.
    @param Sender [in] Not used.
  }
begin
  inherited;
  {Check that entered text doesn't start with digit}
  if (edId.Text <> '') and CharInSet(edId.Text[1], ['0'..'9']) then
  begin
     // Identifier did start with digit - error
    MsgInvalidIdentifier(HELP_MSGDLG_BADIDENTIFIER);
    ModalResult := 0;
    edId.SetFocus;
  end
  else
    // Identifier is OK - record it in property
    fIdentifier := edID.Text;
end;

procedure TIdEditor.edIdKeyPress(Sender: TObject; var Key: Char);
  {Key press event handler for identifier edit box: only accepts valid ID
  characters or backspace.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if not CharInSet(Key, ['a'..'z', 'A'..'Z', '_', '0'..'9', #8]) then
    Key := #0
end;

procedure TIdEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Display indentifier in edit box and move focus to it
  edID.Text := fIdentifier;
  edId.SetFocus;
end;

end.

