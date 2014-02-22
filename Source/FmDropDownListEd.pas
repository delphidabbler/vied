{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Drop-down list editor dialogue box - allows user to select a value from a
 * drop-down list.
}


unit FmDropDownListEd;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TDropDownListEditor:
    Class that implements a dialog box containing a drop-down list control. The
    selected value is passed back to callers
  }
  TDropDownListEditor = class(TGenericOKDlg)
    lblDesc: TLabel;
    cmbDropDown: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private // properties
    fText: string;
    fKind: string;
    procedure SetList(const Value: TStringList);
  public
    property Kind: string write fKind;
      {Description of the kind of edit required - will be preceeded by 'Edit'}
    property List: TStringList write SetList;
      {The list of strings to choose from}
    property Text: string read fText write fText;
      {The chosen text (default can be provided by user)}
  end;

implementation

{$R *.DFM}

procedure TDropDownListEditor.btnOKClick(Sender: TObject);
  {Click event handler for OK button: stores combo box selection in Text
  property}
begin
  inherited;
  fText := cmbDropDown.Text;
end;

procedure TDropDownListEditor.FormCreate(Sender: TObject);
  {Form creation event handler. Sets help topic}
begin
  inherited;
  HelpTopic := 'dlg-picklist';
end;

procedure TDropDownListEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
var
  Index: Integer; // index of Text property item in combo box - if any
begin
  inherited;
  // Set up caption
  Caption := 'Edit ' + fKind;
  // Make default selection in combo box
  Index := cmbDropDown.Items.IndexOf(fText);
  if Index = -1 then
    Index := 0;
  cmbDropDown.ItemIndex := Index;
  // Set focus on edit box
  cmbDropDown.SetFocus;
end;

procedure TDropDownListEditor.SetList(const Value: TStringList);
  {Write access method for List property: copies list items into combo box}
begin
  cmbDropDown.Items := Value;
end;

end.
