{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
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
  {
  TVerNumEditor:
    Class implements a dialog box where version numbers can be edited.
  }
  TVerNumEditor = class(TGenericOKDlg)
    lblPrompt: TLabel;
    lblV: TLabel;
    lblDot1: TLabel;
    lblDot2: TLabel;
    lblDot3: TLabel;
    edV1: TEdit;
    edV2: TEdit;
    edV3: TEdit;
    edV4: TEdit;
    btnPlus1V1: TButton;
    btnPlus1V2: TButton;
    btnPlus1V3: TButton;
    btnPlus1V4: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VerEditKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure btnPlus1V1Click(Sender: TObject);
    procedure btnPlus1V2Click(Sender: TObject);
    procedure btnPlus1V3Click(Sender: TObject);
    procedure btnPlus1V4Click(Sender: TObject);
  private
    fKind: string;
    fVersionNumber: TPJVersionNumber;
    procedure BumpNumber(const Ed: TEdit);
  public
    property Kind: string write fKind;
      {Description of kind of version we are editing - write only}
    property VersionNumber: TPJVersionNumber read fVersionNumber
        write fVersionNumber;
      {The version number to be edited, along with result}
  end;

implementation

uses
  // Delphi
  SysUtils;

{$R *.DFM}

procedure TVerNumEditor.btnOKClick(Sender: TObject);
  {OK button click event: accept changes}
begin
  inherited;
  // Write version number from edit boxes to property: '' => 0
  fVersionNumber.V1 := StrToIntDef(edV1.Text, 0);
  fVersionNumber.V2 := StrToIntDef(edV2.Text, 0);
  fVersionNumber.V3 := StrToIntDef(edV3.Text, 0);
  fVersionNumber.V4 := StrToIntDef(edV4.Text, 0);
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
  // Put name of required kind of version number in caption
  Caption := 'Edit ' + fKind + ' Version Number';
  // Put existing version numbers in edit boxes
  edV1.Text := IntToStr(fVersionNumber.V1);
  edV2.Text := IntToStr(fVersionNumber.V2);
  edV3.Text := IntToStr(fVersionNumber.V3);
  edV4.Text := IntToStr(fVersionNumber.V4);
  // Set focus on first edit box
  edV1.SetFocus;
end;

procedure TVerNumEditor.VerEditKeyPress(Sender: TObject; var Key: Char);
  {Key press event handler for all edit controls: rejects all characters but
  digits and backspace}
begin
  inherited;
  if not CharInSet(Key, [#8, '0'..'9']) then
    Key := #0;
end;

end.
