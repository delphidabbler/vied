{
 * FmResCompilerCheck.pas
 *
 * Implements a dialog box where user can enter details of the resource compiler
 * to be used to create binary resource files.
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
 * The Original Code is FmResCompiler.pas.
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


unit FmResCompiler;

interface

uses
  // Delphi
  Dialogs, StdCtrls, Buttons, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmGenericOKDlg;

type

  {
  TResCompilerDlg:
    Dialog box where user can enter details of the resource compiler to be used
    to create binary resource files. This class directly updates compiler
    information in registry.

    Inheritance: TResCompilerDlg -> TGenericOKDlg -> TGenericDlg -> [TForm]
  }
  TResCompilerDlg = class(TGenericOKDlg)
    lblCompiler: TLabel;
    edCompiler: TEdit;
    sbBrowse: TSpeedButton;
    lblCmdLine: TLabel;
    edCmdLine: TEdit;
    lblHelp1: TLabel;
    lblHelp2: TLabel;
    lblHelp3: TLabel;
    dlgBrowse: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure sbBrowseClick(Sender: TObject);
    procedure edCompilerChange(Sender: TObject);
  public
    class procedure EditResCompiler(Owner: TForm);
      {Displays dialog box which updates information in registry according to
      user's input}
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UHelp, USettings;

{$R *.DFM}


{ TResCompilerDlg }

class procedure TResCompilerDlg.EditResCompiler(Owner: TForm);
  {Displays dialog box which updates information in registry according to user's
  input}
begin
  // Simply create and display dialog box
  with TResCompilerDlg.Create(Owner) do
    try
//      HelpContext := HELP_DLG_RESCOMP;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TResCompilerDlg.FormShow(Sender: TObject);
  {Form Show event handler: sets up controls to reflect current compiler
  settings and sets help context for browse dlg box}
begin
  inherited;
//  dlgBrowse.HelpContext := HELP_DLG_BROWSERESCOMP;
  edCompiler.Text := Settings.ReadStr(siCompilerPath);
  edCmdLine.Text := Settings.ReadStr(siCompilerCmdLine);
end;

procedure TResCompilerDlg.btnOKClick(Sender: TObject);
  {OK Button click event handler: validates input and updates current compiler
  settings}
resourcestring
  // Error messages
  sBadPathErr = 'Path to compiler does not exist';
  sMissingCmdLine = 'A command line must be provided';
begin
  inherited;
  // Assume failure, preventing closure of dlg
  ModalResult := mrNone;
  try
    if edCompiler.Text <> '' then
    begin
      // User has entered some text in controls: validate it
      if not FileExists(edCompiler.Text) then
        raise Exception.Create(sBadPathErr);
      if edCmdLine.Text = '' then
        raise Exception.Create(sMissingCmdLine);
      // All is valid: update compiler settings
      Settings.WriteStr(siCompilerPath, edCompiler.Text);
      Settings.WriteStr(siCompilerCmdLine, edCmdLine.Text);
    end
    else
    begin
      // No text entered for compiler path:
      // if text was deleted this time we ensure user is nagged at next start-up
      if Settings.ReadStr(siCompilerPath) <> '' then
        Settings.WriteBool(siNoCompilerCheck, False);
      // delete both path and command line
      Settings.WriteStr(siCompilerPath, '');
      Settings.WriteStr(siCompilerCmdLine, '');

    end;
    // Everything OK: allow dlg box to close
    ModalResult := mrOK;
  except
    // We have a validation error: report to user
    on E: Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TResCompilerDlg.sbBrowseClick(Sender: TObject);
  {Browse button click event handler: display file dlg box and store any name
  entered in edit control}
begin
  inherited;
  if dlgBrowse.Execute then
    edCompiler.Text := dlgBrowse.FileName;
end;

procedure TResCompilerDlg.edCompilerChange(Sender: TObject);
  {Change event handler for compiler edit box: enter suggested command line if
  none yet provided and user is going to use BRCC32}
const
  cBRCC32 = 'brcc32';                     // name of BRCC32 program
  cBRCC32CmdLine = '-fo"<BIN>" "<SRC>"';  // suggested command line for BRCC32
begin
  inherited;
  // Check if BRCC32 is entered and suggest command line if none present
  if (Pos(cBRCC32, Trim(LowerCase(edCompiler.Text))) > 0)
    and (Trim(edCmdLine.Text) = '') then
    edCmdLine.Text := cBRCC32CmdLine;
end;

end.
