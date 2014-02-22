{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Implements a dialogue box that gets compiler settings from user at program
 * start-up when absence of a compiler is detected.
}


unit FmResCompilerCheck;

interface

uses
  // Delphi
  StdCtrls, Dialogs, Buttons, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmResCompiler;

type

  {
  TResCompilerCheckDlg:
    Dialog box derived from dialog box that gets compiler settings from user,
    but customised to be displayed at program start-up when absence of a
    compiler is detected. Has additional check box to switch off further start-
    up checks.

    Inheritance: TResCompilerCheckDlg -> TResCompilerDlg -> TGenericOKDlg
      -> TGenericDlg -> [TForm]
  }
  TResCompilerCheckDlg = class(TResCompilerDlg)
    chkDontCheck: TCheckBox;
    lblInfo: TLabel;
    procedure chkDontCheckClick(Sender: TObject);
  public
    class procedure SetResCompiler(Owner: TForm);
      {Displays dialog box in which user can enter compiler settings}
  end;

implementation

uses
  // Project
  UHelp, USettings;

{$R *.DFM}


procedure TResCompilerCheckDlg.chkDontCheckClick(Sender: TObject);
  {Click event handler for check box used to prevent further checks: stores
  value of check box in settings}
begin
  inherited;
  Settings.WriteBool(siNoCompilerCheck, chkDontCheck.Checked);
end;

class procedure TResCompilerCheckDlg.SetResCompiler(Owner: TForm);
  {Displays dialog box in which user can enter compiler settings}
begin
  // Simply display the dlg box
  with TResCompilerCheckDlg.Create(Owner) do
    try
      HelpTopic := 'dlg-rescompiler';
      ShowModal;
    finally
      Free;
    end;
end;

end.
