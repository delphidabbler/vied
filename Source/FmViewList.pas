{ ##
  @FILE                     FmViewList.pas
  @COMMENTS                 Dialog box used to display (but not edit) string
                            lists. Used to display resource file info and error
                            messages.
  @PROJECT_NAME             Version Information Expert.
  @PROJECT_DESC             Program that allows creation of resource source
                            files containining version information resources.
  @OTHER_NAMES              + Original unit name was ViewList.pas
                            + Changed to FmViewList.pas at v2.0
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
      @COMMENTS             + Re-wrote dialog box to descend from TGenericOKDlg.
                              Removed all code for functionality that is now
                              provided in base class. This change also means
                              that button style and font face conforms to Window
                              95+ standards.
                            + Removed code that prevented user from highlighting
                              and copying text in read only memo.
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
 * The Original Code is FmViewList.pas.
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


unit FmViewList;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg;

type

  {
  TViewListDlg:
    Class implementing a dialog box that permits display of string lists. Used
    to display resource file info and error messages.
  }
  TViewListDlg = class(TGenericViewDlg)
    lblList: TLabel;
    edList: TMemo;
    procedure FormShow(Sender: TObject);
  private // properties
    fDescription: string;
    fTitle: string;
    procedure SetList(const Value: TStringList);
  public
    property List: TStringList write SetList;
      {The list to display in dialog's memo control}
    property Title: string write fTitle;
      {The title of the dialog box window}
    property Description: string write fDescription;
      {The description of the contents of the memo to appear above memo}
  end;

implementation

{$R *.DFM}

{ TViewListDlg }

procedure TViewListDlg.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
begin
  inherited;
  // Set caption per Title property
  Caption := fTitle;
  // Set label text per Description property}
  lblList.Caption := fDescription;
end;

procedure TViewListDlg.SetList(const Value: TStringList);
  {Write access method for List property: copies list into memo control}
begin
  edList.Lines.Assign(Value);
end;

end.
