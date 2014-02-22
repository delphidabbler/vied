{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * Displays the caret position of one or more memo controls in associated label
 * controls. Labels are automatically updated whenever the caret position
 * changes.
 *
 * NOTE: simplified from UMemoCaretPosDisplayMgr.pas in CodeSnip SVN repo trunk
 * as at revision 3016.
}


unit UMemoCaretPosDisplayMgr;


interface


uses
  // Delphi
  Classes, Controls, StdCtrls, Messages;

type

  {
  TMemoCaretPosDisplayMgr:
    Class that displays the caret position of one or more memo controls in
    associated label controls. Labels are automatically updated whenever the
    caret position changes.
  }
  TMemoCaretPosDisplayMgr = class(TObject)
  strict private
    type
      {
      TMemoHook:
        Class used to hook into a memo control's message loop and detect
        selection changes, triggering an event when detected.
      }
      TMemoHook = class(TObject)
      strict private
        var
          fMemo: TMemo;               // Memo control to be hooked
          fMemoWndProc: Pointer;      // Memo's original
          fWndProcHook: Pointer;      // New hook window procedure
          fOnSelChange: TNotifyEvent; // OnSelChange event handler
        procedure WndProcHook(var Msg: TMessage);
          {Window procedure that replaces and calls into memo control's own
          window procedure. Detects selection changes and triggers OnSelChange
          event.
            @param Msg [in/out] Contains information about message. Result field
              updated with message return value.
          }
        function SetWndProc(WndProc: Pointer): Pointer;
          {Assigns a new window procedure to memo control.
            @param WndProc [in] Pointer to new window procedure.
            @return Pointer to old window procedure.
          }
      public
        constructor Create(const AMemo: TMemo);
          {Object constructor. Creates hook object for a specified memo.
            @param AMemo [in] Memo control to be hooked.
          }
        destructor Destroy; override;
          {Object destructor. Restores memo's original window procedure.
          }
        property OnSelChange: TNotifyEvent read fOnSelChange write fOnSelChange;
          {Event triggered when a selection change in memo control is detected}
      end;
    type
      // Record of values associated with memo control
      TAssociations = record
        OnKeyUp: TKeyEvent;     // Original OnKeyUp event handler
        OnMouseUp: TMouseEvent; // Original OnMouseUp event handler
        OnEnter: TNotifyEvent;  // Original OnEnter event handler
        Hook: TMemoHook;        // Object that hooks memo's window proc
        DisplayCtrl: TLabel;    // Label in which to display caret info
      end;
    var
      // memo control being managed
      fSourceCtrl: TMemo;
      // information about values associated with memo
      fAssociations: TAssociations;
    procedure OnKeyUpHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState);
      {OnKeyUp event handler for managed memo controls. Calls any original event
      handler then updates caret position display.
        @param Sender [in] Memo control that triggered event.
        @param Key [in/out] Not used. Passed to any original event handler.
        @param Shift [in] Not used. Passed to any original event handler.
      }
    procedure OnMouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      {OnMouseUp event handler for managed memo controls. Calls any original
      event handler then updates caret position display.
        @param Sender [in] Memo control that triggered event.
        @param Button [in] Not used. Passed to any original event handler.
        @param Shift [in] Not used. Passed to any original event handler.
        @param X [in] Not used. Passed to any original event handler.
        @param Y [in] Not used. Passed to any original event handler.
      }
    procedure OnEnterHandler(Sender: TObject);
      {OnEnter event handler for managed memo controls. Calls any original event
      handler then updates caret position display.
        @param Sender [in] Memo control that triggered event.
      }
    procedure OnSelChangeHandler(Sender: TObject);
      {Handles events triggered when selection changes are reported be memo
      hook. Updates caret position display.
        @param Sender [in] Memo control that triggered event.
      }
    procedure UpdateCaretPos;
      {Updates display of memo control's caret position.
        @param SourceCtrl [in] Memo whose caret position to be displayed.
      }
    procedure Init(const SourceCtrl: TMemo; const DisplayCtrl: TLabel);
      overload;
      {Initialises object to link source memo control and display control.
        @param SourceCtrl [in] Memo control whose caret position is to be
          displayed.
        @param DisplayCtrl [in] Label used to display caret position.
      }
  public
    constructor Create(const SrcCtrl: TMemo; const DisplayCtrl: TLabel);
      {Object constructor. Sets up object.
      NOTE: It is best to construct the object after the associated memo has
      been initialised: OnFormShow is a good place.
      }
    destructor Destroy; override;
      {Object destructor. Restores original event handlers to managed memo
      controls then clears up object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows;


{ TMemoCaretPosDisplayMgr }

constructor TMemoCaretPosDisplayMgr.Create(const SrcCtrl: TMemo;
  const DisplayCtrl: TLabel);
  {Object constructor. Sets up object.
  }
begin
  inherited Create;
  Init(SrcCtrl, DisplayCtrl);
end;

destructor TMemoCaretPosDisplayMgr.Destroy;
  {Object destructor. Restores original event handlers to managed memo controls
  then clears up object.
  }
begin
  // restore saved event handlers
  fSourceCtrl.OnKeyUp := fAssociations.OnKeyUp;
  fSourceCtrl.OnMouseUp := fAssociations.OnMouseUp;
  fSourceCtrl.OnEnter := fAssociations.OnEnter;
  fAssociations.Hook.Free;
  inherited;
end;

procedure TMemoCaretPosDisplayMgr.Init(const SourceCtrl: TMemo;
  const DisplayCtrl: TLabel);
  {Registers a memo control to have caret position displayed in an associated
  label.
    @param SourceCtrl [in] Memo control whose caret position is to be displayed.
    @param DisplayCtrl [in] Label used to display caret position.
  }
begin
  Assert(Assigned(SourceCtrl), ClassName + '.Manage: SourceCtrl is nil');
  Assert(Assigned(DisplayCtrl), ClassName + '.Manage: DisplayCtrl is nil');
  // record memo being handled
  fSourceCtrl := SourceCtrl;
  // save old event handlers
  fAssociations.OnKeyUp := SourceCtrl.OnKeyUp;
  fAssociations.OnMouseUp := SourceCtrl.OnMouseUp;
  fAssociations.OnEnter := SourceCtrl.OnEnter;
  // record display label
  fAssociations.DisplayCtrl := DisplayCtrl;
  // add menu hook object
  fAssociations.Hook := TMemoHook.Create(SourceCtrl);
  fAssociations.Hook.OnSelChange := OnSelChangeHandler;
  // hook required event handlers (each of these calls any saved handler)
  SourceCtrl.OnKeyUp := OnKeyUpHandler;
  SourceCtrl.OnMouseUp := OnMouseUpHandler;
  SourceCtrl.OnEnter := OnEnterHandler;
  // initialise caret position display
  UpdateCaretPos;
end;

procedure TMemoCaretPosDisplayMgr.OnEnterHandler(Sender: TObject);
  {OnEnter event handler for managed memo controls. Calls any original event
  handler then updates caret position display.
    @param Sender [in] Memo control that triggered event.
  }
begin
  Assert(Sender = fSourceCtrl, ClassName + '.OnEnterHandler: Invalid Sender');
  // call any original event hander
  if Assigned(fAssociations.OnEnter) then
    fAssociations.OnEnter(Sender);
  UpdateCaretPos;
end;

procedure TMemoCaretPosDisplayMgr.OnKeyUpHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {OnKeyUp event handler for managed memo controls. Calls any original event
  handler then updates caret position display.
    @param Sender [in] Memo control that triggered event.
    @param Key [in/out] Not used. Passed to any original event handler.
    @param Shift [in] Not used. Passed to any original event handler.
  }
begin
  Assert(Sender = fSourceCtrl, ClassName + '.OnKeyUpHandler: Invalid Sender');
  // call any original event hander
  if Assigned(fAssociations.OnKeyUp) then
    fAssociations.OnKeyUp(Sender, Key, Shift);
  UpdateCaretPos;
end;

procedure TMemoCaretPosDisplayMgr.OnMouseUpHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {OnMouseUp event handler for managed memo controls. Calls any original event
  handler then updates caret position display.
    @param Sender [in] Memo control that triggered event.
    @param Button [in] Not used. Passed to any original event handler.
    @param Shift [in] Not used. Passed to any original event handler.
    @param X [in] Not used. Passed to any original event handler.
    @param Y [in] Not used. Passed to any original event handler.
  }
begin
  Assert(Sender = fSourceCtrl, ClassName + '.OnMouseUpHandler: Invalid Sender');
  // call any original event hander
  if Assigned(fAssociations.OnMouseUp) then
    fAssociations.OnMouseUp(Sender, Button, Shift, X, Y);
  UpdateCaretPos;
end;

procedure TMemoCaretPosDisplayMgr.OnSelChangeHandler(Sender: TObject);
  {Handles events triggered when selection changes are reported be memo hook.
  Updates caret position display.
    @param Sender [in] Memo control that triggered event.
  }
begin
  UpdateCaretPos;
end;

procedure TMemoCaretPosDisplayMgr.UpdateCaretPos;
  {Updates display of a memo control's caret position.
    @param SourceCtrl [in] Memo whose caret position to be displayed.
  }
begin
  fAssociations.DisplayCtrl.Caption := Format(
    '%d: %d', [fSourceCtrl.CaretPos.Y, fSourceCtrl.CaretPos.X]
  );
end;

{ TMemoCaretPosDisplayMgr.TMemoHook }

constructor TMemoCaretPosDisplayMgr.TMemoHook.Create(const AMemo: TMemo);
  {Object constructor. Creates hook object for a specified memo.
    @param AMemo [in] Memo control to be hooked.
  }
begin
  inherited Create;
  fMemo := AMemo;
  // hook memo's window procedure
  fWndProcHook := Classes.MakeObjectInstance(WndProcHook);
  fMemoWndProc := SetWndProc(fWndProcHook);
end;

destructor TMemoCaretPosDisplayMgr.TMemoHook.Destroy;
  {Object destructor. Restores memo's original window procedure.
  }
begin
  fOnSelChange := nil;
  if Assigned(fWndProcHook) then
  begin
    // restore original window procedure
    SetWndProc(fMemoWndProc);
    Classes.FreeObjectInstance(fWndProcHook);
  end;
  inherited;
end;

function TMemoCaretPosDisplayMgr.TMemoHook.SetWndProc(
  WndProc: Pointer): Pointer;
  {Assigns a new window procedure to memo control.
    @param WndProc [in] Pointer to new window procedure.
    @return Pointer to old window procedure.
  }
begin
  Result := Pointer(
    SetWindowLongPtr(fMemo.Handle, GWL_WNDPROC, LONG_PTR(WndProc))
  );
end;

procedure TMemoCaretPosDisplayMgr.TMemoHook.WndProcHook(var Msg: TMessage);
  {Window procedure that replaces and calls into memo control's own window
  procedure. Detects selection changes and triggers OnSelChange event.
    @param Msg [in/out] Contains information about message. Result field updated
      with message return value.
  }
begin
  Msg.Result := CallWindowProc(
    fMemoWndProc, fMemo.Handle, Msg.Msg, Msg.WParam, Msg.LParam
  );
  if (Msg.Msg = EM_SETSEL) and Assigned(fOnSelChange) then
    fOnSelChange(fMemo);
end;

end.

