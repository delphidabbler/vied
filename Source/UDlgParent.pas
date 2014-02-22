{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Provides code that sets the parent window of a dialogue box to a specified
 * window handle. This code required to enable dialogues to work correctly with
 * main form that appears in the Windows task bar.
}


unit UDlgParent;


interface


uses
  // Delphi
  Controls;


type

  {
  TDlgParent:
    Static class that can set parent window handle of a dialog box.
  }
  TDlgParent = class(TObject)
  private
    class procedure SetParent(Dlg, Parent: TWinControl);
      {Sets a dialog box's parent window to window handle of a parent control.
        @param Dlg [in] Reference to dialog box to have parent set.
        @param Parent [in] Parent control that provides window handle. If Parent
          is nil either active form or main form handle is used.
      }
  public
    class procedure SetParentToOwner(Dlg: TWinControl);
      {Sets a dialog box's parent window to window handle of its owner control,
      if any. If owner is nil or has no window handle, handle of active form or
      main form is used.
        @param Dlg [in] Reference to dialog box to have parent set.
      }
  end;


implementation


uses
  // Delphi
  Forms, Windows;


{ TDlgParent }

class procedure TDlgParent.SetParent(Dlg, Parent: TWinControl);
  {Sets a dialog box's parent window to window handle of a parent control.
    @param Dlg [in] Reference to dialog box to have parent set.
    @param Parent [in] Parent control that provides window handle. If Parent is
      nil either active form or main form handle is used.
  }
var
  ParentWnd: THandle; // window handle of parent control
begin
  Assert(Assigned(Dlg),                                    // ** do not localise
    'TDlgParent.SetParent: Dlg is nil');
  if Assigned(Parent) then
    ParentWnd := Parent.Handle
  else if Assigned(Screen.ActiveCustomForm) then
    ParentWnd := Screen.ActiveCustomForm.Handle
  else if Assigned(Application.MainForm) then
    ParentWnd := Application.MainForm.Handle
  else
    ParentWnd := Application.Handle;
  Assert(ParentWnd <> 0,                                   // ** do not localise
    'TDlgParent.SetParent: Can''t get parent window');
  SetWindowLong(Dlg.Handle, GWL_HWNDPARENT, ParentWnd);
end;

class procedure TDlgParent.SetParentToOwner(Dlg: TWinControl);
  {Sets a dialog box's parent window to window handle of its owner control, if
  any. If owner is nil or has no window handle, handle of active form or main
  form is used.
    @param Dlg [in] Reference to dialog box to have parent set.
  }
begin
  if Dlg.Owner is TWinControl then
    SetParent(Dlg, Dlg.Owner as TWinControl)
  else
    SetParent(Dlg, nil);
end;

end.

