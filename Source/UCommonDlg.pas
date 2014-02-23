{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Implements customisations of open and save common dialogue boxes that access
 * a help topic in HTML help and align over the owner control.
}


unit UCommonDlg;


interface


uses
  // Delphi
  Messages, Dialogs, CommDlg, Classes, Windows;


type
  ///  <summary>
  ///  Type of common dialog box hook (callback) function.
  ///  </summary>
  ///  <remarks>
  ///  These functions receive messages and notifications intended for a dialog
  ///  box and can handle them or pass them on to the dialog box procedure.
  ///  </remarks>
  ///  <param name="Wnd">HWND [in] Handle of the dialog box for which message is
  ///  intended.</param>
  ///  <param name="Msg">UINT [in] Message id.</param>
  ///  <param name="WParam">Integer [in] Additional information depending on
  ///  value of Msg.</param>
  ///  <param name="LParam">Integer [in] Additional information depending on
  ///  value of Msg.</param>
  ///  <returns>UINT. 0 to pass message to dialog box procedure or non-zero to
  ///  cause the dialog box procedure to ignore the message.</returns>
  TCommonDlgHookFn = function(Wnd: HWnd; Msg: UINT; WParam: Integer;
    LParam: Integer): UINT; stdcall;

type
  ///  <summary>
  ///  Abstract base class for classes that hook and handle dialog hook messages
  ///  on behalf of an associated common dialog box.
  ///  </summary>
  TCommonDlgHook = class abstract(TObject)
  strict private
    ///  <summary>Reference to hooked dialog box.</summary>
    fDlg: TCommonDialog;
    ///  <summary>Value of OldHookFn property.</summary>
    fOldHookFn: TCommonDlgHookFn;
  strict protected
    ///  <summary>Dialog's previous (default?) hook function.</summary>
    property OldHookFn: TCommonDlgHookFn read fOldHookFn write fOldHookFn;
  public
    ///  <summary>Sets up object and associates with the given common dialog
    ///  box.</summary>
    constructor Create(const Dlg: TCommonDialog);
    ///  <summary>Applies new dialog box hook function and ensures dialog box
    ///  has reference back to this object.</summary>
    ///  <param name="DialogData">Untyped [in/out] Contains data providing info
    ///  about dialog box. This is with new hook function and reference to
    ///  associated hook object.</param>
    procedure Initialise(var DialogData); virtual; abstract;
    ///  <summary>Calls associated dialog hook function with the given
    ///  parameters and returns its result.</summary>
    ///  <remarks>
    ///  <para>A reference to the dialog function must be recorded in the
    ///  OldHookFn property.</para>
    ///  <para>Parameters are simply passed to the hook function. The parameters
    ///  and the returns value have same meaning as those of TCommonDlgHookFn.
    ///  </para>
    ///  </remarks>
    function CallHookFn(Wnd: HWnd; Msg: UINT; WParam, LParam: Integer): UINT;
    ///  <summary>Aligns hooked dialog box to its owner.</summary>
    procedure AlignDlg;
  end;

type
  ///  <summary>
  ///  Class that hooks and handles dialog hook messages on behalf of file open
  ///  and save common dailogs using a custom hook function. Aligns dialog box
  ///  over owner.
  ///  </summary>
  TFileDlgHook = class(TCommonDlgHook)
  public
    ///  <summary>Applies new dialog box hook function and ensures file dialog
    ///  box has reference back to this object.</summary>
    ///  <param name="DialogData">Untyped [in/out] Contains address of file
    ///  dialog's current hook function. Receives new hook function reference
    ///  and reference to this hook object.</param>
    procedure Initialise(var DialogData); override;
    ///  <summary>Recovers an instance an object of this class from
    ///  TOpenFileName data.</summary>
    ///  <param name="P">POpenFileName [in] Pointer to TOpenFilename structure
    ///  that contains reference to a TFileDlgHook instance in one of its
    ///  fields.</param>
    class function RecoverInstance(P: POpenFileName): TFileDlgHook;
      overload;
    ///  <summary>Recovers an instance an object of this class from TOFNotify
    ///  data.</summary>
    ///  <param name="P">POFNotify [in] Pointer to TOFNotify structure that
    ///  contains a reference to a TFileDlgHook instance in one of its fields.
    ///  </param>
    class function RecoverInstance(P: POFNotify): TFileDlgHook;
      overload;
  end;

type
  ///  <summary>
  ///  Subclasses the Save dialog box to enable the dialog to align itself over
  ///  its owner, to work correctly with the Vista task bar and to add support
  ///  for HTML help topics and a help button.
  ///  </summary>
  TSaveDialogEx = class(TSaveDialog)
  strict private
    ///  <summary>Value of HelpTopic property.</summary>
    fHelpTopic: string;
    ///  <summary>Object that wraps dialog box hook function.</summary>
    fHook: TFileDlgHook;
  strict protected
    ///  <summary>Overridden method that updates the DialogData structure to
    ///  route message processing through a custom explorer hook object.
    ///  </summary>
    ///  <param name="DialogFunc">Pointer [in] Windows function to be called to
    ///  execute dialog box (GetOpenFileName() in this case).</param>
    ///  <param name="DialogData">Untyped [in] Dialog box description to be
    ///  passed to DialogFunc. In this case data is of type TOpenFileName.
    ///  </param>
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
      override;
    ///  <summary>Intercepts messages sent to the dialog window before they
    ///  reach the dialog’s window procedure.</summary>
    ///  <remarks>This implementation changes default support for the help
    ///  button to include the new HelpTopic property and to use the program's
    ///  own help manager.</remarks>
    ///  <param name="Msg">TMessage [in/out] Intercepted message. Unchanged by
    ///  this method. May be modified by inherited implementation(s).</param>
    ///  <returns>Bool. False to pass message on to dilog's window procedure,
    ///  True to inhibit this.</returns>
    function MessageHook(var Msg: TMessage): Boolean; override;
    ///  <summary>Sets up dialog box just before it is displayed.</summary>
    procedure DoShow; override;
  public
    ///  <summary>Creates dialog box instance owned by given component.
    ///  </summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Displays dialog box.</summary>
    ///  <returns>Boolean. True if user OKs and False if cancels.</returns>
    function Execute: Boolean; override;
  published
    ///  <summary>Help topic to be displayed when help button is clicked.
    ///  </summary>
    property HelpTopic: string read fHelpTopic write fHelpTopic;
  end;

type
  ///  <summary>
  ///  Subclasses the Open dialog box to enable the dialog to align itself over
  ///  its owner, to work correctly with the Vista task bar and to add support
  ///  for HTML help topics and a help button.
  ///  </summary>
  TOpenDialogEx = class(TOpenDialog)
  strict private
    ///  <summary>Value of HelpTopic property.</summary>
    fHelpTopic: string;
    ///  <summary>Object that wraps dialog box hook function.</summary>
    fHook: TFileDlgHook;
  strict protected
    ///  <summary>Overridden method that updates the DialogData structure to
    ///  route message processing through a custom explorer hook object.
    ///  </summary>
    ///  <param name="DialogFunc">Pointer [in] Windows function to be called to
    ///  execute dialog box (GetOpenFileName() in this case).</param>
    ///  <param name="DialogData">Untyped [in] Dialog box description to be
    ///  passed to DialogFunc. In this case data is of type TOpenFileName.
    ///  </param>
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
      override;
    ///  <summary>Intercepts messages sent to the dialog window before they
    ///  reach the dialog’s window procedure.</summary>
    ///  <remarks>This implementation changes default support for the help
    ///  button to include the new HelpTopic property and to use the program's
    ///  own help manager.</remarks>
    ///  <param name="Msg">TMessage [in/out] Intercepted message. Unchanged by
    ///  this method. May be modified by inherited implementation(s).</param>
    ///  <returns>Bool. False to pass message on to dilog's window procedure,
    ///  True to inhibit this.</returns>
    function MessageHook(var Msg: TMessage): Boolean; override;
    ///  <summary>Sets up dialog box just before it is displayed.</summary>
    procedure DoShow; override;
  public
    ///  <summary>Creates dialog box instance owned by given component.
    ///  </summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Displays dialog box.</summary>
    ///  <returns>Boolean. True if user OKs and False if cancels.</returns>
    function Execute: Boolean; override;
  published
    ///  <summary>Help topic to be displayed when help button is clicked.
    ///  </summary>
    property HelpTopic: string read fHelpTopic write fHelpTopic;
  end;


implementation


uses
  // Delphi
  Controls, Forms, Types,
  // Project
  UDlgParent, UHelp;


type
  ///  <summary>A record containing only static methods to help with common
  ///  dialog tasks.</summary>
  TCommonDlgHelper = record
  public
    ///  <summary>Checks if a given message is a common dialog help message and
    ///  returns True if so.</summary>
    class function IsHelpMessage(const Msg: TMessage): Boolean; static;
    ///  <summary>Displays the given help topic if specified and returns True.
    ///  False is returned if Topic is not specified.</summary>
    class function ShowHelp(const Topic: string): Boolean; static;
    ///  <summary>Returns the true window handle of a given common dialog
    ///  component.</summary>
    ///  <remarks>For some common dialogs the true window handle is NOT that
    ///  given by the component's Handle property.</remarks>
    class function Handle(const Dlg: TCommonDialog): HWND; static;
    ///  <summary>Centres the common dialog instance Dlg over the TWinControl
    ///  specified by Owner.</summary>
    class procedure AlignToOwner(const Dlg: TCommonDialog;
      const Owner: TWinControl); static;
  end;

type
  ///  <summary>Record containing only static methods to assist in manipulating
  ///  component owners for display purposes.</summary>
  ///  <remarks>Where a component has no owner, the active or main forms are
  ///  assumed to be the owner.</remarks>
  TOwnerHelper = class(TObject)
  public
    ///  <summary>Checks the given owner and if it is not nil returns it
    ///  unchanged. Otherwise a default form is returned.</summary>
    ///  <remarks>The 1st preference for default form is the current active form
    ///  and if there is none the 2nd preference of the application's main form
    ///  is used. If that is not defined then nil is returned.</remarks>
    class function AdjustOwner(AOwner: TComponent): TComponent;
    ///  <summary>Returns the window handle of the given component. If it has
    ///  no handle the handle of the default form is used if available,
    ///  otherwise 0 is returned.</summary>
    ///  <remarks>For details of the default form see AdjustOwner.</remarks>
    class function Handle(AOwner: TComponent): HWND;
  end;

{ TCommonDlgHelper }

class procedure TCommonDlgHelper.AlignToOwner(const Dlg: TCommonDialog;
  const Owner: TWinControl);
var
  OwnerBounds: TRect; // bounding rectangle of common dialog's owner
  DlgBounds: TRect;   // bounding rectangle of common dialog box
  WorkArea: TRect;    // screen work area: keep dialog within it
  DlgHandle: HWND;    // common dialog's window handle
begin
  if not Assigned(Owner) then
    Exit;
  DlgHandle := Handle(Dlg);
  OwnerBounds := Owner.BoundsRect;
  GetWindowRect(DlgHandle, DlgBounds);
  // Centre dialog over owner
  OffsetRect(
    DlgBounds,
    OwnerBounds.Left + (RectWidth(OwnerBounds) - RectWidth(DlgBounds)) div 2,
    OwnerBounds.Top + (RectHeight(OwnerBounds) - RectHeight(DlgBounds)) div 2
  );
  // Esnure dialog is within work area of monitor containing it
  WorkArea := Screen.MonitorFromRect(DlgBounds).WorkareaRect;
  if DlgBounds.Right > WorkArea.Right then
    OffsetRect(DlgBounds, WorkArea.Right - DlgBounds.Right, 0);
  if DlgBounds.Left < WorkArea.Left then
    OffsetRect(DlgBounds, WorkArea.Left - DlgBounds.Left, 0);
  if DlgBounds.Bottom > WorkArea.Bottom then
    OffsetRect(DlgBounds, 0, WorkArea.Bottom - DlgBounds.Bottom);
  if DlgBounds.Top < WorkArea.Top then
    OffsetRect(DlgBounds, 0, WorkArea.Top - DlgBounds.Top);
  // Update position of dialog box
  SetWindowPos(
    DlgHandle,                      // window to be positioned
    0,                              // only required if setting z-order
    DlgBounds.Left, DlgBounds.Top,  // X and Y co-ords of window
    0, 0,                           // only required if setting size of window
    SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER  // flags saying what to do
  );
end;

class function TCommonDlgHelper.Handle(const Dlg: TCommonDialog): HWND;
begin
  Result := Dlg.Handle;
  if (Dlg is TOpenDialog) and NewStyleControls
    and not (ofOldStyleDialog in (Dlg as TOpenDialog).Options) then
    // For explorer style dialogs with explorer hooks and / or customisation
    // templates the main window handle is the parent of the handle returned
    // from dialog's Handle property. Delphi always provides an explorer hook
    // for new style dialog boxes, so we can assume we need the parent handle
    Result := GetParent(Result)
end;

class function TCommonDlgHelper.IsHelpMessage(const Msg: TMessage): Boolean;
begin
  Result := Msg.Msg = RegisterWindowMessage(CommDlg.HELPMSGSTRING);
end;

class function TCommonDlgHelper.ShowHelp(const Topic: string): Boolean;
begin
  Result := Topic <> '';
  if Result then
    THelp.ShowTopic(Topic);
end;

{ TOwnerHelper }

class function TOwnerHelper.AdjustOwner(AOwner: TComponent): TComponent;
begin
  if Assigned(AOwner) then
    Exit(AOwner);
  if Assigned(Screen.ActiveCustomForm) then
    Exit(Screen.ActiveCustomForm);
  Result := Application.MainForm;
end;

class function TOwnerHelper.Handle(AOwner: TComponent): HWND;
begin
  AOwner := AdjustOwner(AOwner);
  if not Assigned(AOwner) then
    Exit(0);
  if AOwner is TWinControl then
    Exit((AOwner as TWinControl).Handle);
  if AOwner is TCommonDialog then
    Exit(TCommonDlgHelper.Handle(AOwner as TCommonDialog));
  Result := 0;
end;

{ TCommonDlgHook }

function CallHookFunc(Fn: TCommonDlgHookFn; Wnd: HWnd; Msg: UINT;
  WParam, LParam: Integer): UINT;
begin
  Assert(Assigned(Fn), 'CallHookFunc: Fn is nil');
  Result := Fn(Wnd, Msg, LParam, WParam)
end;

procedure TCommonDlgHook.AlignDlg;
var
  Owner: TComponent;  // component that owns the common dialog
begin
  Owner := TOwnerHelper.AdjustOwner(fDlg.Owner);
  if Assigned(Owner) and (Owner is TWinControl) then
    TCommonDlgHelper.AlignToOwner(fDlg, Owner as TWinControl);
end;

function TCommonDlgHook.CallHookFn(Wnd: HWnd; Msg: UINT; WParam,
  LParam: Integer): UINT;
begin
  Assert(Assigned(OldHookFn), ClassName + '.CallHookFn: OldHookFn is nil');
  Result := CallHookFunc(OldHookFn, Wnd, Msg, LParam, WParam);
end;

constructor TCommonDlgHook.Create(const Dlg: TCommonDialog);
begin
  Assert(Assigned(Dlg), ClassName + '.Create: Dlg is nil');
  inherited Create;
  fDlg := Dlg;
end;

{ TFileDlgHook }

function NewExplorerHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM;
  LParam: LPARAM): UINT; stdcall;
var
  Hook: TFileDlgHook; // object that handles hook messages
begin
  // Set default result passed back to windows
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    // Dialog initialising: pass on to original hook function
    Hook := TFileDlgHook.RecoverInstance(POpenFileName(LParam))
      as TFileDlgHook;
    Result := Hook.CallHookFn(Wnd, Msg, LParam, WParam);
  end
  else if Msg = WM_NOTIFY then
  begin
    // Get reference to dialog box object from data structure
    Hook := TFileDlgHook.RecoverInstance(POFNotify(LParam))
      as TFileDlgHook;
    if POFNotify(LParam)^.hdr.code = CDN_INITDONE then
      // Dialog intialization complete: align the dialog box. We don't call old
      // hook function since all this does is centre the dialog box!
      // Windows ignores return value (we leave as default 0)
      Hook.AlignDlg
    else
      // Other notification: pass on to original hook function
      Result := Hook.CallHookFn(Wnd, Msg, WParam, LParam);
  end;
end;

procedure TFileDlgHook.Initialise(var DialogData);
begin
  OldHookFn := TOpenFilename(DialogData).lpfnHook;
  TOpenFilename(DialogData).lpfnHook := NewExplorerHook;
  TOpenFilename(DialogData).lCustData := Integer(Self);
end;

class function TFileDlgHook.RecoverInstance(P: POpenFileName): TFileDlgHook;
begin
  Result := TFileDlgHook(P^.lCustData)
end;

class function TFileDlgHook.RecoverInstance(P: POFNotify): TFileDlgHook;
begin
  Result := RecoverInstance(P^.lpOFN);
end;

{ TSaveDialogEx }

constructor TSaveDialogEx.Create(AOwner: TComponent);
begin
  inherited;
  fHook := TFileDlgHook.Create(Self);
  // Override property defaults
  Options := Options + [ofNoReadOnlyReturn] - [ofEnableSizing];
end;

destructor TSaveDialogEx.Destroy;
begin
  fHook.Free;
  inherited;
end;

procedure TSaveDialogEx.DoShow;
begin
  // Prevent task bar button press bringing owner window to foreground by
  // setting window parent
  SetWindowLong(
    TCommonDlgHelper.Handle(Self),
    GWL_HWNDPARENT,
    TOwnerHelper.Handle(Self.Owner)
  );
  inherited;
end;

function TSaveDialogEx.Execute: Boolean;
begin
  if HelpTopic <> '' then
    Options := Options + [ofShowHelp]
  else
    Options := Options - [ofShowHelp];
  Result := inherited Execute;
end;

function TSaveDialogEx.MessageHook(var Msg: TMessage): Boolean;
begin
  if TCommonDlgHelper.IsHelpMessage(Msg) then
    Result := TCommonDlgHelper.ShowHelp(HelpTopic)
  else
    Result := inherited MessageHook(Msg);
end;

function TSaveDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    fHook.Initialise(DialogData);
  // Call inherited function with (modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

{ TOpenDialogEx }

constructor TOpenDialogEx.Create(AOwner: TComponent);
begin
  inherited;
  fHook := TFileDlgHook.Create(Self);
  // Override property defaults
  Options := Options + [ofPathMustExist, ofFileMustExist] - [ofEnableSizing];
end;

destructor TOpenDialogEx.Destroy;
begin
  fHook.Free;
  inherited;
end;

procedure TOpenDialogEx.DoShow;
begin
  // Prevent task bar button press bringing owner window to foreground by
  // setting window parent
  SetWindowLong(
    TCommonDlgHelper.Handle(Self),
    GWL_HWNDPARENT,
    TOwnerHelper.Handle(Self.Owner)
  );
  inherited;
end;

function TOpenDialogEx.Execute: Boolean;
begin
  if HelpTopic <> '' then
    Options := Options + [ofShowHelp]
  else
    Options := Options - [ofShowHelp];
  Result := inherited Execute;
end;

function TOpenDialogEx.MessageHook(var Msg: TMessage): Boolean;
begin
  if TCommonDlgHelper.IsHelpMessage(Msg) then
    Result := TCommonDlgHelper.ShowHelp(HelpTopic)
  else
    Result := inherited MessageHook(Msg);
end;

function TOpenDialogEx.TaskModalDialog(DialogFunc: Pointer;
  var DialogData): Bool;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    fHook.Initialise(DialogData);
  // Call inherited function with (modified) data structure
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

initialization

// We can't use latest dialog style since they don't display a help button, and
// we need it.
Dialogs.UseLatestCommonDialogs := False;

end.

