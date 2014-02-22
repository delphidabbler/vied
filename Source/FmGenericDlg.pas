{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2002-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Implements a generic base class from which other program dialog boxes can be
 * descended. Provides layout and functionality common to all dlg boxes.
}


unit FmGenericDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, ExtCtrls, Classes, Forms;


type

  {
  TGenericDlg:
    Generic base class for other dialog boxes - displays and handles help button
    and permits aligning of child dlg boxes to this window
  }
  TGenericDlg = class(TForm)
    bvlBottom: TBevel;
    pnlBody: TPanel;
    btnHelp: TButton;
    ///  <summary>Form creation event handler. Arranges controls and sizes form.
    ///  </summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Help button click event handler. Displays a help topic
    ///  associated with this dialog box.</summary>
    procedure btnHelpClick(Sender: TObject);
    ///  <summary>Handles key presses on form. Displays help topic associated
    ///  with this dialog box if user presses F1.</summary>
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  strict private
    ///  <summary>Value of HelpTopic property.</summary>
    fHelpTopic: string;
    ///  <summary>Displays help topic determined by HelpTopic property.
    ///  </summary>
    ///  <remarks>See documentation of HelpTopic property for details of how
    ///  this is handled if the property is not set.</remarks>
    procedure DisplayHelp;
  strict protected
    ///  <summary>Modifies the form's window creation parameters to provide a
    ///  custom window class name for the form.</summary>
    ///  <remarks>This method is called by the VCL just before the form's
    ///  window is created.</remarks>
    procedure CreateParams(var Params: TCreateParams); override;
    ///  <summary>Arranges controls within form.</summary>
    procedure ArrangeControls; virtual;
    ///  <summary>Specifies name of help topic associated with dialog box.
    ///  </summary>
    ///  <remarks>
    ///  <para>If this property is set the specified topic is displayed when
    ///  help is accessed. If the property is not set a special "no help
    ///  available" topic is displayed.</para>
    ///  <para>Descendant classes should set the property in their constructors.
    ///  Those classes that need to enable callers to change the help topic must
    ///  redeclare the property as public.</para>
    ///  </remarks>
    property HelpTopic: string read fHelpTopic write fHelpTopic;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  UDlgParent, UHelp;


{$R *.DFM}


resourcestring
  // Message displayed if no help context is asigned and help button is clicked
  sNoHelp = 'Sorry there is no help available.';


{ TGenericDlg }

procedure TGenericDlg.ArrangeControls;
begin
  ClientWidth := pnlBody.Width + 16;
  bvlBottom.Top := pnlBody.Height + 16;
  btnHelp.Top := bvlBottom.Top + 8;
  ClientHeight := btnHelp.Top + btnHelp.Height + 4;
  bvlBottom.Width := pnlBody.Width;
  btnHelp.Left := ClientWidth - 8 - btnHelp.Width;
end;

procedure TGenericDlg.btnHelpClick(Sender: TObject);
begin
  DisplayHelp;
end;

procedure TGenericDlg.CreateParams(var Params: TCreateParams);
var
  ClsName: string;  // name of window class
begin
  inherited;
  ClsName := 'DelphiDabbler.VIEd.' + Name;
  StrLCopy(
    Params.WinClassName,
    PChar(ClsName),
    SizeOf(Params.WinClassName) div SizeOf(Char) - 1
  );
end;

procedure TGenericDlg.DisplayHelp;
begin
  if HelpTopic <> '' then
    THelp.ShowTopic(HelpTopic)
  else
    THelp.ShowTopic(THelp.DlgErrTopic);
end;

procedure TGenericDlg.FormCreate(Sender: TObject);
begin
  TDlgParent.SetParentToOwner(Self);
  ArrangeControls;
end;

procedure TGenericDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) and (Shift = []) then
  begin
    // F1 pressed with no modifier
    DisplayHelp;
    Key := 0;
  end;
end;

end.

