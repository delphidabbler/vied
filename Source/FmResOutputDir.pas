{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Dialogue box used to set the default folder used when compiling .res files.
}


unit FmResOutputDir;


interface


uses
  // Delphi
  Buttons, StdCtrls, Controls, ExtCtrls, Classes,
  // delphiDabbler library
  PJShellFolders,
  // Project
  FmGenericOKDlg;


type

  {
    TResOutputDirDlg:
      Implements a dialog box used to get a directory name from the user that is
      used as default folder for compiled .res files. Provides access to "Select
      Folder" dialog box assist in choosing folder name.

      Inheritance: TResOutputDirDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TResOutputDirDlg = class(TGenericOKDlg)
    lblDirName: TLabel;
    edDirName: TEdit;
    sbDirName: TSpeedButton;
    dlgDirName: TPJBrowseDialog;
    procedure sbDirNameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private // properties
    function GetDirName: string;
    procedure SetDirName(const Value: string);
  public
    property DirName: string read GetDirName write SetDirName;
      {This is the directory name entered by the user or chosen from Select
      Folder dialog box}
  end;


implementation


uses
  // Project
  UHelp;

{$R *.DFM}


{ TResOutputDirDlg }

procedure TResOutputDirDlg.FormCreate(Sender: TObject);
  {Form construction: set dialog's help context}
begin
  inherited;
  HelpTopic := 'dlg-compilerdir';
end;

function TResOutputDirDlg.GetDirName: string;
  {Getter for DirName property: accesses edit control for directory name}
begin
  Result := edDirName.Text;
  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;

procedure TResOutputDirDlg.sbDirNameClick(Sender: TObject);
  {Click event handler for browse speed button: displays folder dialog to allow
  user to enter folder which is then copied into edit control}
begin
  inherited;
  dlgDirName.FolderName := edDirName.Text;
  if dlgDirName.Execute then
    edDirName.Text := dlgDirName.FolderName;
end;

procedure TResOutputDirDlg.SetDirName(const Value: string);
  {Setter for DirName property: stores value in directory name edit control}
begin
  edDirName.Text := Value;
end;

end.
