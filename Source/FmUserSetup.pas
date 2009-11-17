{ ##
  @FILE                     FmUserSetup.pas
  @COMMENTS                 User set-up dialog box. Allows user to configure
                            some application options.
  @PROJECT_NAME             Version Information Editor.
  @PROJECT_DESC             Program that allows creation of resource source
                            files containining version information resources.
  @OTHER_NAMES              + Original unit name was UsrSetup.pas
                            + Changed to FmUserSetup.pas at v2.0
  @DEPENDENCIES             None.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 25/05/1998
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 13/04/1999
      @COMMENTS             Removed glyphs from buttons in form. Removed code
                            from form creation event that turns glyphs off. This
                            was incompatible with Delphi 2.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 18/03/2002
      @COMMENTS             Re-wrote dialog box to descend from TGenericOKDlg.
                            Removed all code for functionality that is now
                            provided in base class. This change also means that
                            button style and font face conforms to Window 95+
                            standards.
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
 * The Original Code is FmUserSetup.pas.
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


unit FmUserSetup;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TUserSetupDlg:
    Class implementing a dialog where user can modify application options.
  }
  TUserSetupDlg = class(TGenericOKDlg)
    lblDesc: TLabel;
    chkValidate: TCheckBox;
    chkDescFileFlags: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private // properties
    fAutoValidate: Boolean;
    fDescribeFileFlags: Boolean;
  public
    property AutoValidate : Boolean
      read fAutoValidate write fAutoValidate;
      {Whether auto-validation is switched on at start-up}
    property DescribeFileFlags : Boolean
      read fDescribeFileFlags write fDescribeFileFlags;
      {Whether file flags are fully described at start-up}
  end;

implementation

{$R *.DFM}

{ TUserSetupDlg }

procedure TUserSetupDlg.btnOKClick(Sender: TObject);
  {OK button click event handler: update properties with values entered}
begin
  inherited;
  fAutoValidate := chkValidate.Checked;
  fDescribeFileFlags := chkDescFileFlags.Checked;
end;

procedure TUserSetupDlg.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
begin
  inherited;
  // Set check boxes to state given by properties
  chkValidate.Checked := fAutoValidate;
  chkDescFileFlags.Checked := fDescribeFileFlags;
end;

end.
