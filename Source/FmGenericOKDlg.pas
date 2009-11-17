{ ##
  @PROJECT_NAME             Version Information Editor.
  @PROJECT_DESC             Program that allows creation of resource source
                            files containining version information resources.
  @FILE                     FmGenericOKDlg.pas
  @COMMENTS                 This is a generic OK dialogue box, descended from
                            TGenericDlg, that can be used as a base class for
                            dialog boxes that permit editing - adds "OK" and
                            "Cancel" buttons to the form that close the dialog
                            box with the appropriate modal result. All
                            descendant dlg boxes should place their additional
                            components within the body panel and size it
                            accordingly. The dialog and the basic components
                            will then be sized and positioned using the body
                            panel's size.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 18/03/2002
      @COMMENTS             Original version. Based on v1.0 of similar code in
                            delphiDabbler's Help Topic Author.
    )
  )
}


{
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
 * The Original Code is FmGenericOKDlg.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 1998-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * Contributor(s):
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmGenericOKDlg;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericDlg;

type

  {
  TGenericOKDlg:
    Generic OK dialog box used as a base class for dialog boxes that permit
    editing - adds OK and cancel buttons to the form that close the dialog box
    with appropriate modal result
  }
  TGenericOKDlg = class(TGenericDlg)
    btnCancel: TButton;
    btnOK: TButton;
  protected
    procedure ArrangeControls; override;
      {Arranges controls within window}
  end;

implementation

{$R *.DFM}

procedure TGenericOKDlg.ArrangeControls;
  {Arranges controls within window}
begin
  inherited;
  btnOK.Top := bvlBottom.Top + 8;
  btnCancel.Top := btnOK.Top;
  btnCancel.Left := btnHelp.Left - btnCancel.Width - 4;
  btnOK.Left := btnCancel.Left - btnOK.Width - 4;
end;

end.
