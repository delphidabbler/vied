{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Identifier editor dialogue box. Allows user to enter a valid version
 * information resource identifier.
}


unit FmIdEd;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TIdEditor:
    Class that implements a dialog box that allows user to enter a valid version
    information resource identifier.
  }
  TIdEditor = class(TGenericOKDlg)
    lblID: TLabel;
    edId: TEdit;
    procedure edIdKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fIdentifier: string;
      {Value of Identifier property}
  public
    property Identifier: string read fIdentifier write fIdentifier;
      {Holds default identifier provided by caller and edited identifier if user
      clicks OK}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHelp, UMsgDlgs;


{$R *.DFM}

{ TIdEditor }

procedure TIdEditor.btnOKClick(Sender: TObject);
  {Click event handler for OK button: validates entered identifier and stores
  in Identifier property if valid.
    @param Sender [in] Not used.
  }
begin
  inherited;
  {Check that entered text doesn't start with digit}
  if (edId.Text <> '') and CharInSet(edId.Text[1], ['0'..'9']) then
  begin
     // Identifier did start with digit - error
    MsgInvalidIdentifier;
    ModalResult := 0;
    edId.SetFocus;
  end
  else
    // Identifier is OK - record it in property
    fIdentifier := edID.Text;
end;

procedure TIdEditor.edIdKeyPress(Sender: TObject; var Key: Char);
  {Key press event handler for identifier edit box: only accepts valid ID
  characters or backspace.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if not CharInSet(Key, ['a'..'z', 'A'..'Z', '_', '0'..'9', #8]) then
    Key := #0
end;

procedure TIdEditor.FormCreate(Sender: TObject);
  {Form creation event handler. Sets help topic}
begin
  inherited;
  HelpTopic := 'dlg-identifier';
end;

procedure TIdEditor.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Display indentifier in edit box and move focus to it
  edID.Text := fIdentifier;
  edId.SetFocus;
end;

end.

