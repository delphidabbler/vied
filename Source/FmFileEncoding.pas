{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2023, Peter Johnson (www.delphidabbler.com).
 *
 * Dialogue box that lets user chose the text encoding in which to save a .vi
 * file.
}


unit FmFileEncoding;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FmGenericOKDlg, StdCtrls, ExtCtrls;

type
  TFileEncodingDlg = class(TGenericOKDlg)
    Panel1: TPanel;
    lblDesc: TLabel;
    rbANSI: TRadioButton;
    rbUTF8: TRadioButton;
    procedure FormCreate(Sender: TObject);
  strict private
    ///  <summary>Returns True if the UTF-8 radio button is selected or false
    ///  if not (i.e. the ANSI radio button is selected).</summary>
    function GetUseUTF8: Boolean;
    ///  <summary>Ticks the UTF-8 radio button if <c>Value</c> is true or ticks
    ///  the ANSI radion button otherwise.</summary>
    procedure SetUseUTF8(const Value: Boolean);
  public
    ///  <summary>Displays dialogue box with default choice as specified by
    ///  <c>UseUTF8</c> when called. When the the user OKs the dialogue box
    ///  <c>UseUTF8</c> is set True if UTF-8 is selected and false otherwise.
    ///  If the user cancels <c>UseUTF8</c> is not changed.</summary>
    ///  <param name="AOwner"><c>TComponent</c> [in] Dialogue form's owner.
    ///  </param>
    ///  <param name="UseUTF8"><c>Boolean</c> [in/out] Set by the caller to
    ///  specify whether UTF-8 is the default choice to be displayed in the
    ///  dialogue box. Updated if the user OKs to reflect the chosen encoding.
    ///  </param>
    ///  <returns><c>Boolean</c>. True if user OKs the dialogue box and false
    ///  if the user cancels.</returns>
    ///  <remarks>NOTE: The dialogue box should not be constructed directly.
    ///  This method should be called instead.</remarks>
    class function GetFileEncodingFromUser(const AOwner: TComponent;
      var UseUTF8: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TFileEncodingDlg }

procedure TFileEncodingDlg.FormCreate(Sender: TObject);
begin
  inherited;
  HelpTopic := 'dlg-encoding';
end;

class function TFileEncodingDlg.GetFileEncodingFromUser(
  const AOwner: TComponent; var UseUTF8: Boolean): Boolean;
var
  Dlg: TFileEncodingDlg;
begin
  Dlg := TFileEncodingDlg.Create(AOwner);
  try
    Dlg.SetUseUTF8(UseUTF8);
    Result := Dlg.ShowModal = mrOK;
    if Result then
      UseUTF8 := Dlg.GetUseUTF8;
  finally
    Dlg.Free;
  end;
end;

function TFileEncodingDlg.GetUseUTF8: Boolean;
begin
  Result := rbUTF8.Checked;
end;

procedure TFileEncodingDlg.SetUseUTF8(const Value: Boolean);
begin
  if Value then
    rbUTF8.Checked := True
  else
    rbANSI.Checked := True;
end;

end.
