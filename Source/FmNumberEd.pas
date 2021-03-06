{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Number editor dialogue box. Allows user to enter or edit a whole number in
 * decimal or in hex.
}


unit FmNumberEd;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;


type

  {
  TNumEditor:
    Class that implements a dialog box where the use can enter a number in
    decimal or hex format.
  }
  TNumEditor = class(TGenericOKDlg)
    lblNum: TLabel;
    lblInstruct: TLabel;
    edNum: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure edNumKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fShowAsHex: Boolean;
      {Value of ShowAsHex property}
    fNumber: LongInt;
      {Value of Number property}
    fKind: string;
      {Value of Kind property}
  public
    property Kind: string write fKind;
      {Description of the kind of edit required. Will be preceeded by 'Edit'}
    property Number: LongInt read fNumber write fNumber;
      {Number entered. Caller sets the property to provide a default value}
    property ShowAsHex: Boolean write fShowAsHex;
      {Determines whether to display default value as hex or integer}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelp, UMsgDlgs;


{$R *.DFM}

{ TNumEditor }

procedure TNumEditor.btnOKClick(Sender: TObject);
  {OK button click event handler: checks validity of number entered and, if OK,
  stores value in Number property.
    @param Sender [in] Not used.
  }
begin
  inherited;
  try
    // Convert hex or decimal entry to number: throws exception or error
    fNumber := StrToInt(edNum.Text);
  except
    on E: EConvertError do
    begin
      // We've had a conversion error: report to user & keep dlg open
      MsgInvalidNumber;
      ModalResult := 0;
      edNum.SetFocus;
    end;
  end;
end;

procedure TNumEditor.edNumKeyPress(Sender: TObject; var Key: Char);
  {Keypress event handler for number edit control: rejects all but hex numbers,
  '$' and backspace keys.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if not CharInSet(Key, [#8, '$', '0'..'9', 'A'..'F', 'a'..'f']) then
    Key := #0;
end;

procedure TNumEditor.FormCreate(Sender: TObject);
  {Form creation event handler. Sets help topic}
begin
  inherited;
  HelpTopic := 'dlg-numbers';
end;

procedure TNumEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Set caption
  Caption := 'Edit ' + fKind;
  // Write default number into edit box in required format
  if fShowAsHex then
    edNum.Text := '$' + IntToHex(fNumber, 4)
  else
    edNum.Text := IntToStr(fNumber);
  // Set focus on edit box
  edNum.SetFocus;
end;

end.

