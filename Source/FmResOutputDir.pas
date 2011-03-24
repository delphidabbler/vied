{
 * FmResOutputDir.pas
 *
 * Dialog box used to set the default folder used when compiling .res files.
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
 * The Original Code is FmResOutputDir.pas.
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
  HelpContext := HELP_DLG_COMPILERFOLDER;
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
