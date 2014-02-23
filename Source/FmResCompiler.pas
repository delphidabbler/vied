{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Implements a dialogue box where user can enter details of the resource
 * compiler to be used to create binary resource files.
}


unit FmResCompiler;

interface

uses
  // Delphi
  StdCtrls, Buttons, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmGenericOKDlg, UCommonDlg;

type

  {
  TResCompilerDlg:
    Dialog box where user can enter details of the resource compiler to be used
    to create binary resource files. This class directly updates compiler
    information in registry.
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
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure sbBrowseClick(Sender: TObject);
    procedure edCompilerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    fBrowseDlg: TOpenDialogEx;
  public
    class procedure EditResCompiler(Owner: TForm);
      {Displays dialog box which updates information in registry according to
      user's input}
  end;

implementation

uses
  // Delphi
  SysUtils, Dialogs,
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
      HelpTopic := 'dlg-rescompiler';
      ShowModal;
    finally
      Free;
    end;
end;

procedure TResCompilerDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Creates and sets up custom file open dialog box}
resourcestring
  sBrowseDlgFilter = 'Executable files (*.exe)|*.exe';
  sBrowseDlgTitle = 'Browse for Resource Compiler';
begin
  inherited;
  fBrowseDlg := TOpenDialogEx.Create(Self);
  fBrowseDlg.Title := sBrowseDlgTitle;
  fBrowseDlg.Filter := sBrowseDlgFilter;
  fBrowseDlg.HelpTopic := 'dlg-browserescomp';
end;

procedure TResCompilerDlg.FormShow(Sender: TObject);
  {Form Show event handler: sets up controls to reflect current compiler
  settings and sets help context for browse dlg box}
begin
  inherited;
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
  if fBrowseDlg.Execute then
    edCompiler.Text := fBrowseDlg.FileName;
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
