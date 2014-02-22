{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * This is a generic OK dialogue box, descended from TGenericDlg, that can be
 * used as a base class for dialogue boxes that permit editing - adds "OK" and
 * "Cancel" buttons to the form that close the dialogue box with the appropriate
 * modal result.
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
