{
 * FmViewList.pas
 *
 * Dialog box used to display (but not edit) string lists. Used to display
 * resource file info and error messages.
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
 * The Original Code is FmViewList.pas.
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
    property HelpTopic;
      {Redeclared to make public}
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
