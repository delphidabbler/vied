{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Set editor dialogue box. Allows user to include and exclude items in a set.
}


unit FmSetEd;

interface

uses
  // Delphi
  StdCtrls, CheckLst, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TSetEditor:
    Class that implements a dialog box that can be used to edit sets of values.
    The dialog box displays a checked list box, with checked items indicating
    that the item is part of a set.
  }
  TSetEditor = class(TGenericOKDlg)
    lblSet: TLabel;
    clbSet: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private // properties
    fKind: string;
    fIncList: TStringList;
    fExcList: TStringList;
    procedure SetExcList(const Value: TStringList);
    procedure SetIncList(const Value: TStringList);
  private
    procedure RebuildList;
      {Updates check list box from the include and exclude lists, setting check
      marks only on items from include list}
  public
    property Kind: string write fKind;
      {Description of the kind of edit required - will be preceeded by 'Edit'
      (write only)}
    property IncList: TStringList read fIncList write SetIncList;
      {List of items to be placed in Include list box. This list is updated to
      edited contents of this box when user clicks OK}
    property ExcList: TStringList read fExcList write SetExcList;
      {List of items to be placed in Exclude list box - write onle since user is
      only interested in result in Include list box}
  end;

implementation

{$R *.DFM}

{ TSetEditor }

procedure TSetEditor.btnOKClick(Sender: TObject);
  {OK button click event handler: copies checked items to IncList property and
  unchecked items into ExcList property}
var
  CLBIdx: Integer;  // index into check list box items
begin
  inherited;
  // Clear list properties
  fIncList.Clear;
  fExcList.Clear;
  // Scan thru list, adding included and excluded items to appropriate lists
  for CLBIdx := 0 to Pred(clbSet.Items.Count) do
  begin
    if clbSet.Checked[CLBIdx] then
      fIncList.Add(clbSet.Items[CLBIdx])
    else
      fExcList.Add(clbSet.Items[CLBIdx]);
  end;
end;

procedure TSetEditor.FormCreate(Sender: TObject);
  {Form creation event handler: creates owned string list objects and sets help
  topic}
begin
  inherited;
  fIncList := TStringList.Create;
  fExcList := TStringList.Create;
  HelpTopic := 'dlg-bitset';
end;

procedure TSetEditor.FormDestroy(Sender: TObject);
  {Form destruction event handler: free owned objects}
begin
  inherited;
  fExcList.Free;
  fIncList.Free;
end;

procedure TSetEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
begin
  inherited;
  // Set caption
  Caption := 'Edit ' + fKind;
  // Select check list box
  clbSet.SetFocus;
end;

procedure TSetEditor.RebuildList;
  {Updates check list box from the include and exclude lists, setting check
  marks only on items from include list}
  // ---------------------------------------------------------------------------
  procedure AddItem(const Str: string; Checked: Boolean);
    {Adds given item to checked list box, checked or unchecked according to
    Checked parameter}
  var
    CLBIdx: Integer; // index into checked list box
  begin
    CLBIdx := clbSet.Items.Add(Str);
    clbSet.Checked[CLBIdx] := Checked;
  end;
  // ---------------------------------------------------------------------------
var
  ListIdx: Integer; // index into include and exclude lists
begin
  // Clear list box
  clbSet.Clear;
  // Add items from exclude list box to check list box: unchecked
  for ListIdx := 0 to Pred(fExcList.Count) do
    AddItem(fExcList[ListIdx], False);
  // Add items from include list box to check list box: checked
  for ListIdx := 0 to Pred(fIncList.Count) do
    AddItem(fIncList[ListIdx], True);
end;

procedure TSetEditor.SetExcList(const Value: TStringList);
  {Write access method for the ExcList property: items in list are also added to
  checked list box, unchecked}
begin
  fExcList.Assign(Value);
  RebuildList;
end;

procedure TSetEditor.SetIncList(const Value: TStringList);
  {Write access method for the IncList property: items in list are also added to
  checked list box, checked}
begin
  fIncList.Assign(Value);
  RebuildList;
end;

end.
