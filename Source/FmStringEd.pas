{
 * FmStringEd.pas
 *
 * String editor dialog box. Allows user to enter multi or single line strings
 * and (optionally) to select required fields from a drop down list.
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
 * The Original Code is FmStringEd.pas.
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


unit FmStringEd;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;


type

  {
  TStringEditor:
    Class the implements a dialog box where user can enter strings of text. Size
    and style of editor cab be configured. The dialog box can also be used to
    add fields to the text if required.
  }
  TStringEditor = class(TGenericOKDlg)
    lblStr: TLabel;
    lblField: TLabel;
    edStr: TMemo;
    cmbField: TComboBox;
    btnInsert: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    fWrapLines: Boolean;
      {Value of WrapLines property}
    fCompulsory: Boolean;
      {Value of Compulsory property}
    fFixedWidthFont: Boolean;
      {Value of FixedWidth Font property}
    fMemoWidth: Integer;
      {Value of MemoWidth property}
    fMemoHeight: Integer;
      {Value of MemoHeight property}
    fText: string;
      {Value of Text property}
    fKind: string;
      {Value of Kind property}
    fDisplayFieldStuff: Boolean;
      {Flag true if field related controls are being displayed in dlg box and
      false if not}
    function GetLines: TStrings;
      {Read access method for Lines property.
        @return Reference to lines of string editor control.
      }
    function GetMaxLength: Integer;
      {Read access method for MaxLength property.
        @return Maximum length of string editor control.
      }
    procedure SetAllowEnter(const Value: Boolean);
      {Write access method for AllowEnter property. Determines if string editor
      receives return (enter) key presses.
        @param Value [in] New property value.
      }
    procedure SetFixedWidthFont(const Value: Boolean);
      {Write access method for FixedWidthFont property. Sets font in string
      editor.
        @param Value [in] New property value.
      }
    procedure SetLines(const Value: TStrings);
      {Write access method for Lines property. Updates string editor control.
        @param Value [in] New property value.
      }
    procedure SetMaxLength(const Value: Integer);
      {Write access method for MaxLength property.
        @param Value [in] New property value. Sets maximum length of string
          editor.
      }
    procedure SetText(const Value: string);
      {Write access method for Text property: stores value and displays in
      string editor.
        @param Value [in] New property value.
      }
    procedure SetValidFields(const Value: TStrings);
      {Write access method for ValidFields property.
        @param Value [in] New property value. If none nil strings are displayed
          in combo box. If nil, combo box is hidden.
      }
    procedure SetWrapLines(const Value: Boolean);
      {Write access method for SetWrapLines property. Updates strings editor
      control.
        @param Value [in] New property value.
      }
    function FieldsValid: Boolean;
      {Checks if all fields entered in memo are valid.
        @return True if fields are valid, False if not.
      }
    procedure SetFieldDisplay(State: Boolean);
      {Cause all dlg box components related to Fields to be displayed or hidden
      and update dialog box size and layout to suit.
        @param State [in] Flag indicating whether to display controls.
      }
    procedure Arrange;
      {Arrange dialog box components and size according to whether fields are to
      be displayed.
      }
  public
    property Kind: string write fKind;
      {Description of the kind of edit required - will be preceeded by 'Edit'
      (write only)}
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
      {The maximum length of string that can be entered (0=unlimited)}
    property WrapLines: Boolean read fWrapLines write SetWrapLines;
      {Property true if lines are to be wrapped, false otherwise}
    property FixedWidthFont: Boolean read fFixedWidthFont
      write SetFixedWidthFont;
      {Property true if memo component is to display text in fixed width font,
      false to use a proportional font}
    property MemoHeight: Integer read fMemoHeight write fMemoHeight;
      {The number of lines of text to display in memo box}
    property MemoWidth: Integer read fMemoWidth write fMemoWidth;
      {Width in pixels of memo box}
    property Text: string read fText write SetText;
      {The text entered by the user. The user can also set a default text to be
      used by assigning a string to the property before calling the dialogue
      box. If lenght of string is not limited by MaxLength property then strings
      returned by this property are truncated to 255 characters}
    property Lines: TStrings read GetLines write SetLines;
      {Lines entered by user - user can set default lines to be displayed by
      assigning a string list to the property before calling the dlg box}
    property Compulsory: Boolean write fCompulsory;
      {Flag set true if the user must enter some text}
    property ValidFields: TStrings write SetValidFields;
      {List of valid fields for field combo box. If this value is nil then the
      field combo box is hidden, otherwise it is displayed. The dialogue box
      size is adjusted accordingly}
    property AllowEnter: Boolean write SetAllowEnter;
      {Flag which allows return to enter a new line when true and not when
      false. Text property will usually be used when this falg is false and
      Lines property will usually be used when the falg is true}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelp, UMsgDlgs;

{$R *.DFM}


{ TStringEditor }

procedure TStringEditor.Arrange;
  {Arrange dialog box components and size according to whether fields are to be
  displayed.
  }
begin
  // Assumes that:
  //   All left side controls have left sides correctly positioned
  //   lblStr and edStr tops are correctly postioned

  // Set component widths and horizontal positions
  edStr.Width := fMemoWidth;
  pnlBody.Width := fMemoWidth;
  lblStr.Width := fMemoWidth;
  btnInsert.Left := pnlBody.Width - btnInsert.Width;
  cmbField.Width := pnlBody.Width - btnInsert.Width - 16;
  // Set component heights and vertical positions
  edStr.Height := fMemoHeight;
  if fDisplayFieldStuff then
  begin
    // We're displaying field combo box etc
    lblField.Top := edStr.Top + edStr.Height + 6;
    cmbField.Top := lblField.Top + lblField.Height + 4;
    btnInsert.Top := cmbField.Top
      + (cmbField.Height - btnInsert.height) div 2;
    pnlBody.Height := btnInsert.Top + btnInsert.Height;
  end
  else
  begin
    // We're only displaying edit control
    pnlBody.Height := fMemoHeight + edStr.Top;
  end;
  // Call inherited method to arrange remaining controls
  ArrangeControls;
end;

procedure TStringEditor.btnInsertClick(Sender: TObject);
  {Insert button click event handler: enters chosen field in text at current
  text cursor position.
    @param Sender [in] Not used.
  }
var
  TheText: string;  // text of the memo
  Start: Integer;   // current position of the text cursor in memo
begin
  inherited;
  // Record memo text and cursor position
  TheText := edStr.Text;
  Start := edStr.SelStart;
  // If some text selected delete it - field over-writes selected text
  if edStr.SelLength > 0 then
    Delete(TheText, Start+1, edStr.SelLength);
  // Add field at current position in text
  Insert(cmbField.Text, TheText, Start+1);
  // Copy revised text to memo, preserving caret position
  edStr.Text := TheText;
  edStr.SelStart := Start + Length(cmbField.Text);
  edStr.SelLength := 0;
  // Give the memo the focus for further editing
  edStr.SetFocus;
end;

procedure TStringEditor.btnOKClick(Sender: TObject);
  {OK button click event handler: validate text and any fields entered and close
  dialog box if all OK, otherwise return to editing.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if fCompulsory and (edStr.Text = '') then
    // No text entered and text is compulsory: check whether to accept anyway
    if MsgStringRequired(fKind) = mrOk then
    begin
      // Not accepting: don't close dlg
      ModalResult := mrNone;
      edStr.SetFocus;
    end
    else
      // Ignoring warning: accept empty entry
      fText := ''
  else if FieldsValid then
    // We're OK: fields are valid
    fText := edStr.Text
  else
  begin
    // Some invalid fields are present - return to memo control
    ModalResult := mrNone;
    edStr.SetFocus;
  end;
end;

function TStringEditor.FieldsValid: Boolean;
  {Checks if all fields entered in memo are valid.
    @return True if fields are valid, False if not.
  }
var
  Str: string;    // string used to hold text to be checked
  Tok: string;    // the current field being examined
  Posn: Integer;  // position of start of field in Str
  Stop: Integer;  // position of end of field in Str
begin
  // Assume all OK
  Result := True;
  // Record memo text
  Str := edStr.Text;
  // Record position of first field (if any) in Str
  Posn := Pos('<', Str);
  // Iterate for as long as there are more fields in Str
  while Posn > 0 do
  begin
    // Find end of field - it's a '>' character (or end of string)
    Stop := Posn + 1;
    while (Stop < Length(Str)) and (Str[Stop] <> '>') do
      Inc(Stop);
    // Record field in Tok
    Tok := Copy(Str, Posn, Stop - Posn + 1);
    // Find if field is in field combo box - it's valid if it is
    if cmbField.Items.IndexOf(Tok) = -1 then
    begin
      // Error - token not in combo box: highlight error and return false
      Result := False;
      edStr.SelStart := Pos(Tok, edStr.Text) - 1;
      edStr.SelLength := Length(Tok);
      MsgInvalidField(Tok, fKind);
      Exit;
    end;
    // Record part of string to be examined: i.e. all string after current field
    Str := Copy(Str, Stop + 1, $FF);
    // Find start of next field if any
    Posn := Pos('<', Str);
  end;
end;

procedure TStringEditor.FormCreate(Sender: TObject);
  {Form creation event handler: sets default property values.
    @param Sender [in] Not used.
  }
begin
  fMemoWidth := 295;
  fMemoHeight := 102;
  inherited;  // inherited last: we may want to use values to arrange controls
end;

procedure TStringEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls.
    @param Sender [in] Not used.
  }
resourcestring
  // Text that appears in dialog box
  sEdit = 'Edit %s';
  sEditTextLbl = '&Edit the required text below';
  sMaxChars = '(max %d characters):';
begin
  inherited;
  // Set caption
  Caption := Format(sEdit, [fKind]);
  // Focus memo component and select all text unless unlimited text length
  edStr.SetFocus;
  edStr.SelStart := 0;
  if MaxLength > 0 then
    edStr.SelLength := Length(edStr.Text);
  // Update memo caption to indicate any maximum text length
  lblStr.Caption := sEditTextLbl;
  if MaxLength > 0 then
    lblStr.Caption := lblStr.Caption + ' ' + Format(sMaxChars, [MaxLength])
  else
    lblStr.Caption := lblStr.Caption + ':';
  // Make 1st field in combo box current
  cmbField.ItemIndex := 0;
  // Arrange dlg box according to whether field controls are being displayed
  Arrange;
end;

function TStringEditor.GetLines: TStrings;
  {Read access method for Lines property.
    @return Reference to lines of string editor control.
  }
begin
  Result := edStr.Lines;
end;

function TStringEditor.GetMaxLength: Integer;
  {Read access method for MaxLength property.
    @return Maximum length of string editor control.
  }
begin
  Result := edStr.MaxLength;
end;

procedure TStringEditor.SetAllowEnter(const Value: Boolean);
  {Write access method for AllowEnter property. Determines if string editor
  receives return (enter) key presses.
    @param Value [in] New property value.
  }
begin
  edStr.WantReturns := Value;
end;

procedure TStringEditor.SetFieldDisplay(State: Boolean);
  {Cause all dlg box components related to Fields to be displayed or hidden
  and update dialog box size and layout to suit.
    @param State [in] Flag indicating whether to display controls.
  }
begin
  // Alter display state of components
  cmbField.Visible := State;
  lblField.Visible := State;
  btnInsert.Visible := State;
  // Record current state
  fDisplayFieldStuff := State;
  // Re-arrange dlg box controls and size accordingly
  Arrange;
end;

procedure TStringEditor.SetFixedWidthFont(const Value: Boolean);
  {Write access method for FixedWidthFont property. Sets font in string editor.
    @param Value [in] New property value.
  }
begin
  // Record new state
  fFixedWidthFont := Value;
  if fFixedWidthFont then
  begin
    // Set a fixed width font in memo
    edStr.Font.Name := 'Courier New';
    edStr.Font.Size := 9;
  end
  else
  begin
    // Not fixed width - use form's font
    edStr.ParentFont := True;
  end;
end;

procedure TStringEditor.SetLines(const Value: TStrings);
  {Write access method for Lines property. Updates string editor control.
    @param Value [in] New property value.
  }
begin
  edStr.Lines := Value;
end;

procedure TStringEditor.SetMaxLength(const Value: Integer);
  {Write access method for MaxLength property.
    @param Value [in] New property value. Sets maximum length of string editor.
  }
begin
  edStr.MaxLength := Value;
end;

procedure TStringEditor.SetText(const Value: string);
  {Write access method for Text property: stores value and displays in string
  editor.
    @param Value [in] New property value.
  }
begin
  fText := Value;
  edStr.Text := Value;
end;

procedure TStringEditor.SetValidFields(const Value: TStrings);
  {Write access method for ValidFields property.
    @param Value [in] New property value. If none nil strings are displayed in
      combo box. If nil, combo box is hidden.
  }
begin
  if Value <> nil then
  begin
    // We have strings to record in combo box
    // record strings
    cmbField.Items := Value;
    // ensure that combo box appears
    SetFieldDisplay(True);
  end
  else
    // hide combo box - the are no fields to display
    SetFieldDisplay(False);
end;

procedure TStringEditor.SetWrapLines(const Value: Boolean);
  {Write access method for SetWrapLines property. Updates strings editor
  control.
    @param Value [in] New property value.
  }
begin
  // Record new state
  fWrapLines := Value;
  if fWrapLines then
  begin
    // Wrapping lines - no scroll bars & tell memo to wrap lines
    edStr.WordWrap := True;
    edStr.ScrollBars := ssNone;
  end
  else
  begin
    // Not wrapping lines
    // vertical and horizontal scroll bars and tell memo not to wrap lines
    edStr.WordWrap := False;
    edStr.ScrollBars := ssBoth;
  end;
end;

end.

