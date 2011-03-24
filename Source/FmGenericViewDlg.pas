{
 * FmGenericViewDlg.pas
 *
 * This is a generic base class for dialog boxes that simply display information
 * and have a single "Done" button. It descends from TGenericDlg and simply adds
 * a "Done" button that closes the dialogue box.
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
 * The Original Code is FmGenericViewDlg.pas.
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


unit FmGenericViewDlg;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericDlg;

type
  {
  TGenericViewDlg:
    Generic dialogue box used to view (rather then edit) information - provides
    a Done button that closes dialogue box in addition to Help button inherited
    from TGenericDlg
  }
  TGenericViewDlg = class(TGenericDlg)
    btnDone: TButton;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.DFM}

{ TGenericViewDlg }

procedure TGenericViewDlg.FormCreate(Sender: TObject);
  {Form creation event handler: aligns done button to rest of form}
begin
  inherited;
  btnDone.Top := bvlBottom.Top + 8;
  btnDone.Left := btnHelp.Left - btnDone.Width - 4;
end;

end.
