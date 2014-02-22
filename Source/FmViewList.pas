{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Dialogue box used to display (but not edit) string lists. Used to display
 * resource file info and error messages.
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
