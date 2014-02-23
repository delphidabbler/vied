{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * This is a generic base class for dialogue boxes that simply display
 * information and have a single "Done" button. It descends from TGenericDlg and
 * simply adds "Done" button that closes the dialogue box.
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
