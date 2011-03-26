{
 * FmResCompilerCheck.pas
 *
 * Implements a dialog box that gets compiler settings from user at program
 * start-up when absence of a compiler is detected.
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
 * The Original Code is FmResCompilerCheck.pas.
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
