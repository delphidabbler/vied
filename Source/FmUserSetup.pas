{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2014, Peter Johnson (www.delphidabbler.com).
 *
 * User set-up dialogue box. Allows user to configure some application options.
}


unit FmUserSetup;

interface

uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;

type

  {
  TUserSetupDlg:
    Class implementing a dialog where user can modify application options.
  }
  TUserSetupDlg = class(TGenericOKDlg)
    lblDesc: TLabel;
    chkValidate: TCheckBox;
    chkDescFileFlags: TCheckBox;
    chkUseUTF8: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private // properties
    fAutoValidate: Boolean;
    fDescribeFileFlags: Boolean;
    fUTF8EncodeVIFiles: Boolean;
  public
    property AutoValidate : Boolean
      read fAutoValidate write fAutoValidate;
      {Whether auto-validation is switched on at start-up}
    property DescribeFileFlags : Boolean
      read fDescribeFileFlags write fDescribeFileFlags;
      {Whether file flags are fully described at start-up}
    ///  <summary>Whether UTF-8 is used by default when writing .vi files.
    ///  </summary>
    property UTF8EncodeVIFiles: Boolean
      read fUTF8EncodeVIFiles write fUTF8EncodeVIFiles;
  end;

implementation

{$R *.DFM}

{ TUserSetupDlg }

procedure TUserSetupDlg.btnOKClick(Sender: TObject);
  {OK button click event handler: update properties with values entered}
begin
  inherited;
  fAutoValidate := chkValidate.Checked;
  fDescribeFileFlags := chkDescFileFlags.Checked;
  fUTF8EncodeVIFiles := chkUseUTF8.Checked;
end;

procedure TUserSetupDlg.FormCreate(Sender: TObject);
  {Form creation event handler. Sets help topic}
begin
  inherited;
  HelpTopic := 'dlg-setup';
end;

procedure TUserSetupDlg.FormShow(Sender: TObject);
  {Event handler called when form is shown: intialises controls}
begin
  inherited;
  // Set check boxes to state given by properties
  chkValidate.Checked := fAutoValidate;
  chkDescFileFlags.Checked := fDescribeFileFlags;
  chkUseUTF8.Checked := fUTF8EncodeVIFiles;
end;

end.
