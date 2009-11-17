{ ##
  @FILE                     FmVerNumEd.pas
  @COMMENTS                 Version number editor dialog box.
  @PROJECT_NAME             Version Information Editor.
  @PROJECT_DESC             Program that allows creation of resource source
                            files containining version information resources.
  @OTHER_NAMES              + Original unit name was EdVerNum.pas
                            + Changed to FmVerNumEd.pas at v2.0
  @DEPENDENCIES             + TPJVersionInfo (from PJVersionInfo unit v3.0)
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
    @REVISION(
      @VERSION              2.1
      @DATE                 06/05/2002
      @COMMENTS             + Now use TPJVersionNumber type in place of
                              TVersionNumber from old VerTypes unit.
                            + Removed usage of VerTypes unit
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
 * The Original Code is FmVerNumEd.pas.
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


unit FmVerNumEd;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // PJSoft library
  PJVersionInfo,
  // Project
  FmGenericOKDlg;

type
  {
  TVerNumEditor:
    Class implements a dialog box where version numbers can be edited.
  }
  TVerNumEditor = class(TGenericOKDlg)
    lblPrompt: TLabel;
    lblV: TLabel;
    lblDot1: TLabel;
    lblDot2: TLabel;
    lblDot3: TLabel;
    edV1: TEdit;
    edV2: TEdit;
    edV3: TEdit;
    edV4: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VerEditKeyPress(Sender: TObject; var Key: Char);
  private // properties
    fKind: string;
    fVersionNumber: TPJVersionNumber;
  public
    property Kind: string write fKind;
      {Description of kind of version we are editing - write only}
    property VersionNumber: TPJVersionNumber read fVersionNumber
        write fVersionNumber;
      {The version number to be edited, along with result}
  end;

implementation

uses
  // Delphi
  SysUtils;

{$R *.DFM}

procedure TVerNumEditor.btnOKClick(Sender: TObject);
  {OK button click event: accept changes}
begin
  inherited;
  // Write version number from edit boxes to property: '' => 0
  fVersionNumber.V1 := StrToIntDef(edV1.Text, 0);
  fVersionNumber.V2 := StrToIntDef(edV2.Text, 0);
  fVersionNumber.V3 := StrToIntDef(edV3.Text, 0);
  fVersionNumber.V4 := StrToIntDef(edV4.Text, 0);
end;

procedure TVerNumEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
begin
  inherited;
  // Put name of required kind of version number in caption
  Caption := 'Edit ' + fKind + ' Version Number';
  // Put existing version numbers in edit boxes
  edV1.Text := IntToStr(fVersionNumber.V1);
  edV2.Text := IntToStr(fVersionNumber.V2);
  edV3.Text := IntToStr(fVersionNumber.V3);
  edV4.Text := IntToStr(fVersionNumber.V4);
  // Set focus on first edit box
  edV1.SetFocus;
end;

procedure TVerNumEditor.VerEditKeyPress(Sender: TObject; var Key: Char);
  {Key press event handler for all edit controls: rejects all characters but
  digits and backspace}
begin
  inherited;
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

end.
