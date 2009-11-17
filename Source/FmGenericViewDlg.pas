{ ##
  @PROJECT_NAME             Version Information Editor.
  @PROJECT_DESC             Program that allows creation of resource source
                            files containining version information resources.
  @FILE                     FmGenericViewDlg.pas
  @COMMENTS                 This is a generic base class for dialog boxes that
                            simply display information and have a "done" button
                            rather than "OK" and "Cancel". It descends from
                            TGenericDlg and simple adds a "Done" button that
                            closes the dialogue box. All descendant dlg boxes
                            should place their additional components within the
                            body panel and size it accordingly. The dialog and
                            the basic components will then be sized and
                            positioned using the body panel's size.
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
 * The Original Code is FmGenericViewDlg.pas.
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
