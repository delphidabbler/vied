{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2022, Peter Johnson (www.delphidabbler.com).
 *
 * Version number editor dialogue box.
}


unit FmVerNumEd;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // PJSoft library
  PJVersionInfo,
  // Project
  FmGenericOKDlg;

type
  TVerNumRenderer = reference to function(const CodeStr: string):
    TPJVersionNumber;

type
  {
  TVerNumEditor:
    Class implements a dialog box where version numbers can be edited.
  }
  TVerNumEditor = class(TGenericOKDlg)
    pnlLiteralNumbers: TPanel;
    lblV: TLabel;
    edV1: TEdit;
    edV2: TEdit;
    lblDot1: TLabel;
    lblDot2: TLabel;
    edV3: TEdit;
    lblDot3: TLabel;
    edV4: TEdit;
    btnPlus1V1: TButton;
    btnPlus1V2: TButton;
    btnPlus1V3: TButton;
    btnPlus1V4: TButton;
    lblPrompt: TLabel;
    pnlMacros: TPanel;
    lblField: TLabel;
    cmbField: TComboBox;
    btnInsert: TButton;
    lblMacros: TLabel;
    edMacros: TEdit;
    pnlRadios: TPanel;
    rbUseVersionNumbers: TRadioButton;
    rbMacros: TRadioButton;
    lbMacroInstructions: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VerEditKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure btnPlus1V1Click(Sender: TObject);
    procedure btnPlus1V2Click(Sender: TObject);
    procedure btnPlus1V3Click(Sender: TObject);
    procedure btnPlus1V4Click(Sender: TObject);
    procedure EntryTypeRadioBtnClick(Sender: TObject);
    procedure edMacrosChange(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
  private
    fKind: string;
    fVersionNumberCode: string;
    procedure BumpNumber(const Ed: TEdit);
    procedure UpdateEntryPanes;
    procedure UpdateVerNumControls;
    function AreMacrosValid: Boolean;
    function VerNumControlsToVerNum: string;
    function IsMacroInUse(const Text: string): Boolean;
    function GetValidMacros: TStrings;
    procedure SetValidMacros(Macros: TStrings);
  strict protected
    procedure ArrangeControls; override;
  public
    property Kind: string read fKind write fKind;
      {Description of kind of version we are editing - write only}
    property VersionNumberCode: string read fVersionNumberCode
        write fVersionNumberCode;
      {The version number to be edited, along with result}
    ///  <summary>List of valid macros for use in Macros combo box.</summary>
    property ValidMacros: TStrings read GetValidMacros write SetValidMacros;
  end;

implementation

uses
  // Delphi
  SysUtils, StrUtils, Math, Dialogs,
  // Project
  UMsgDlgs, UMAcros, UVerUtils, UVInfo;

{$R *.DFM}

function TVerNumEditor.AreMacrosValid: Boolean;
var
  Str: string;    // string used to hold text to be checked
  Tok: string;    // the current field being examined
  Posn: Integer;  // position of start of field in Str
  Stop: Integer;  // position of end of field in Str
resourcestring
  sBadMacro = 'Macro "%s" is not valid';
begin
  // ** Copied and tweaked from TStringEditor.FieldsValid method
  //    There should be a cleaner way to do this - perhaps a method of TVInfo
  //    to extract fields / macros from text?
  // Assume all OK
  Result := True;
  // Record memo text
  Str := edMacros.Text;
  // Record position of first macro (if any) in Str
  Posn := Pos('<%', Str);
  // Iterate for as long as there are more fields in Str
  while Posn > 0 do
  begin
    // Find end of field - it's a '>' character (or end of string)
    Stop := Posn + 1;
    while (Stop < Length(Str)) and (Str[Stop] <> '>') do
      Inc(Stop);
    // Record field in Tok
    Tok := Copy(Str, Posn, Stop - Posn + 1);
    // Find if macro is in macro combo box - it's valid if it is
    if cmbField.Items.IndexOf(Tok) = -1 then
    begin
      // Error - token not in combo box: highlight error and return false
      Result := False;
      edMacros.SelStart := Pos(Tok, edMacros.Text) - 1;
      edMacros.SelLength := Length(Tok);
      Display(Format(sBadMacro, [Tok]), mtError, [mbOK]);
      Exit;
    end;
    // Record part of string to be examined: i.e. all string after current field
    Str := Copy(Str, Stop + 1, MaxInt);
    // Find start of next field if any
    Posn := Pos('<', Str);
  end;
end;

procedure TVerNumEditor.ArrangeControls;
begin
  // Arrange controls
  pnlBody.Height := pnlRadios.Height
    + Max(pnlMacros.Height, pnlLiteralNumbers.Height)
    + Max(pnlMacros.Margins.Top, pnlLiteralNumbers.Margins.Top);
  inherited;
end;

procedure TVerNumEditor.btnInsertClick(Sender: TObject);
var
  TheText: string;  // text of the memo
  Start: Integer;   // current position of the text cursor in memo
begin
  inherited;
  // Record memo text and cursor position
  TheText := edMacros.Text;
  Start := edMacros.SelStart;
  // If some text selected delete it - field over-writes selected text
  if edMacros.SelLength > 0 then
    Delete(TheText, Start+1, edMacros.SelLength);
  // Add field at current position in text
  Insert(cmbField.Text, TheText, Start+1);
  // Copy revised text to memo, preserving caret position
  edMacros.Text := TheText;
  // Give the memo the focus for further editing
  edMacros.SetFocus;
  edMacros.SelLength := 0;
  edMacros.SelStart := Start + Length(cmbField.Text);
end;

procedure TVerNumEditor.btnOKClick(Sender: TObject);
  {OK button click event: accept changes}
begin
  inherited;
  // Write version number from edit boxes to property: '' => 0
  if rbUseVersionNumbers.Checked and not IsMacroInUse(edMacros.Text) then
    fVersionNumberCode := UVerUtils.VersionNumberToStr(
      StrToIntDef(edV1.Text, 0), StrToIntDef(edV2.Text, 0),
      StrToIntDef(edV3.Text, 0), StrToIntDef(edV4.Text, 0),
      False
    )
  else if AreMacrosValid then
    fVersionNumberCode := Trim(edMacros.Text)
  else
  begin
    // Some invalid macros are present - return to memo control
    ModalResult := mrNone;
    rbMacros.Checked := True;
    edMacros.SetFocus;
  end;
end;

procedure TVerNumEditor.btnPlus1V1Click(Sender: TObject);
begin
  BumpNumber(edV1);
end;

procedure TVerNumEditor.btnPlus1V2Click(Sender: TObject);
begin
  BumpNumber(edV2);
end;

procedure TVerNumEditor.btnPlus1V3Click(Sender: TObject);
begin
  BumpNumber(edV3);
end;

procedure TVerNumEditor.btnPlus1V4Click(Sender: TObject);
begin
  BumpNumber(edV4);
end;

procedure TVerNumEditor.BumpNumber(const Ed: TEdit);
var
  Value: Integer;
begin
  if Ed.Text <> '' then
    Value := StrToInt(ed.Text)
  else
    Value := 0;
  Inc(Value);
  Ed.Text := IntToStr(Value);
end;

procedure TVerNumEditor.edMacrosChange(Sender: TObject);
begin
  rbUseVersionNumbers.Enabled := not IsMacroInUse(edMacros.Text);
end;

procedure TVerNumEditor.FormCreate(Sender: TObject);
  {Form creation event handler. Sets help topic}
begin
  inherited;
  HelpTopic := 'dlg-vernum';
end;

procedure TVerNumEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
begin
  inherited;
  Caption := 'Edit ' + fKind + ' Version Number';
  edMacros.Text := fVersionNumberCode;
  UpdateVerNumControls;
  // Set focus on first edit box
  if pnlLiteralNumbers.Visible then
    edV1.SetFocus
  else
  begin
    edMacros.SetFocus;
    edMacros.SelLength := 0;
  end;
  // Make required data entry panel visible
  UpdateEntryPanes;
end;

function TVerNumEditor.GetValidMacros: TStrings;
begin
  Result := cmbField.Items;
end;

function TVerNumEditor.IsMacroInUse(const Text: string): Boolean;
begin
  Result := TMacros.ContainsMacro(Text);
end;

procedure TVerNumEditor.SetValidMacros(Macros: TStrings);
begin
  cmbField.Enabled := Assigned(Macros);
  if cmbField.Enabled then
    cmbField.Items := Macros;
end;

procedure TVerNumEditor.EntryTypeRadioBtnClick(Sender: TObject);
begin
  if pnlMacros.Visible then
    UpdateVerNumControls
  else if pnlLiteralNumbers.Visible then
    edMacros.Text := VerNumControlsToVerNum;
  UpdateEntryPanes;
end;

procedure TVerNumEditor.VerEditKeyPress(Sender: TObject; var Key: Char);
  {Key press event handler for all edit controls: rejects all characters but
  digits and backspace}
begin
  inherited;
  if not CharInSet(Key, [#8, '0'..'9']) then
    Key := #0;
end;

function TVerNumEditor.VerNumControlsToVerNum: string;
begin
  Result := UVerUtils.VersionNumberToStr(
    StrToIntDef(edV1.Text, 0), StrToIntDef(edV2.Text, 0),
    StrToIntDef(edV3.Text, 0), StrToIntDef(edV4.Text, 0),
    False
  );
end;

procedure TVerNumEditor.UpdateEntryPanes;
begin
  pnlLiteralNumbers.Visible := rbUseVersionNumbers.Checked;
  pnlMacros.Visible := not rbUseVersionNumbers.Checked;
end;

procedure TVerNumEditor.UpdateVerNumControls;
var
  VerNum: TPJVersionNumber;
begin
  if not IsMacroInUse(edMacros.Text) then
  begin
    VerNum := StrToVersionNumber(Trim(edMacros.Text));
    edV1.Text := IntToStr(VerNum.V1);
    edV2.Text := IntToStr(VerNum.V2);
    edV3.Text := IntToStr(VerNum.V3);
    edV4.Text := IntToStr(VerNum.V4);
    rbUseVersionNumbers.Checked := True;
  end
  else
  begin
    edV1.Text := '';
    edV2.Text := '';
    edV3.Text := '';
    edV4.Text := '';
    rbMacros.Checked := True;
  end;
end;

end.
