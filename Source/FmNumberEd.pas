{
 * FmNumberEd.pas
 *
 * Number editor dialog box. Allows user to enter or edit a whole number in
 * decimal or in hex.
 *
 * Original unit name was EdNum.pas. Changed to FmNumberEd.pas at v2.0.
 *
 * v1.0 of 25 May 1998  - Original version. Named EdNum.pas.
 * v1.1 of 13 Apr 1999  - Removed glyphs from buttons in form. Removed code from
 *                        form creation event that turns glyphs off. This was
 *                        incompatible with Delphi 2.
 * v2.0 of 18 May 2000  - Re-wrote dialog box to descend from TGenericOKDlg.
 *                        Removed all code for functionality that is now
 *                        provided in base class. This change also means that
 *                        button style and font face conforms to Windows 95+
 *                        standards.
 *                      - Removed call to utility function HexToInt since
 *                        SysUtils StrToInt function handles hex.
 * v2.1 of 17 Mar 2003  - Replaced reference to VInfoExp.inc help topic include
 *                        file with reference to UHelp unit.
 * v2.2 of 20 Apr 2008  - Changed to use renamed UMsgDlgs unit.
 *
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
 * The Original Code is FmNumberEd.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1998-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK *****
}


unit FmNumberEd;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;


type

  {
  TNumEditor:
    Class that implements a dialog box where the use can enter a number in
    decimal or hex format.
  }
  TNumEditor = class(TGenericOKDlg)
    lblNum: TLabel;
    lblInstruct: TLabel;
    edNum: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure edNumKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    fShowAsHex: Boolean;
      {Value of ShowAsHex property}
    fNumber: LongInt;
      {Value of Number property}
    fKind: string;
      {Value of Kind property}
  public
    property Kind: string write fKind;
      {Description of the kind of edit required. Will be preceeded by 'Edit'}
    property Number: LongInt read fNumber write fNumber;
      {Number entered. Caller sets the property to provide a default value}
    property ShowAsHex: Boolean write fShowAsHex;
      {Determines whether to display default value as hex or integer}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelp, UMsgDlgs;


{$R *.DFM}

{ TNumEditor }

procedure TNumEditor.btnOKClick(Sender: TObject);
  {OK button click event handler: checks validity of number entered and, if OK,
  stores value in Number property.
    @param Sender [in] Not used.
  }
begin
  inherited;
  try
    // Convert hex or decimal entry to number: throws exception or error
    fNumber := StrToInt(edNum.Text);
  except
    on E: EConvertError do
    begin
      // We've had a conversion error: report to user & keep dlg open
      MsgInvalidNumber(HELP_MSGDLG_BADNUMBER);
      ModalResult := 0;
      edNum.SetFocus;
    end;
  end;
end;

procedure TNumEditor.edNumKeyPress(Sender: TObject; var Key: Char);
  {Keypress event handler for number edit control: rejects all but hex numbers,
  '$' and backspace keys.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if not (Key in [#8, '$', '0'..'9', 'A'..'F', 'a'..'f']) then
    Key := #0;
end;

procedure TNumEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Set caption
  Caption := 'Edit ' + fKind;
  // Write default number into edit box in required format
  if fShowAsHex then
    edNum.Text := '$' + IntToHex(fNumber, 4)
  else
    edNum.Text := IntToStr(fNumber);
  // Set focus on edit box
  edNum.SetFocus;
end;

end.

