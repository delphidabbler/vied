{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2022, Peter Johnson (www.delphidabbler.com).
 *
 * Simple message dialogue boxes used by Version Information Editor program.
}


unit UMsgDlgs;


interface


uses
  // Delphi
  Dialogs, Classes;


///  <summary>Displays a customised message dialog box.</summary>
///  <param name="Msg">string [in] Message displayed in dialog box.</param>
///  <param name="MsgType">TMsgDlgType [in] Type of dialog box. Must not be
///  mtCustom.</param>
///  <param name="Buttons">TMsgDlgButtons [in] Set of buttons to display in
///  dialog box.</param>
///  <returns>Word. Value indicating which button was pressed to close the
///  dialog box.</returns>
function Display(const Msg: string; const MsgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons): Word;

///  <summary>Displays dialog that warns that a file has changed and gets user's
///  response.</summary>
///  <param name="FileName">string [in] Name of file that has changed.</param>
///  <returns>Word. mrYes to save file, mrNo to abandon file or mrCancel to
///  cancel the current operation.</returns>
function MsgQuerySaveFile(const FileName: string): Word;

///  <summary>Displays error message that notes a file has an invalid extension.
///  </summary>
///  <param name="FileName">string [in] Name of file with invalid extension.
///  </param>
procedure MsgInvalidExtension(const FileName: string);

///  <summary>Display dialog seeking confirmation that it is OK to overwrite a
///  file.</summary>
///  <param name="FileName">string [in] Name of file to be overwritten.</param>
///  <returns>Boolean. True if it is OK to overwrite the file.</returns>
function MsgOKToOverwrite(const FileName: string): Boolean;

///  <summary>Displays error message noting an attempt has been made to edit an
///  item when none is selected.</summary>
procedure MsgNoItemToEdit;

///  <summary>Displays an error message noting an attempt has been made to edit
///  a title.</summary>
///  <param name="Title">string [in] The title concerned.</param>
procedure MsgCantEditTitle(const Title: string);

///  <summary>Displays an error message noting an attempt has been made to edit
///  a File Sub-Type for a File Type which has no Sub-Type.</summary>
///  <param name="FType">string [in] Name of file type.</param>
procedure MsgCantEditSubType(const FType: string);

///  <summary>Displays an error message noting an invalid number was entered.
///  </summary>
procedure MsgInvalidNumber;

///  <summary>Displays a warning that an entry is required for a string item
///  and gives the user the choice of providing an entry or not.</summary>
///  <param name="StrDesc">string [in] Description of string item.</param>
///  <returns>Word. mrOK if user agrees to provide a string or mrIgnore if the
///  user chooses to ignore the warning.</returns>
function MsgStringRequired(const StrDesc: string): Word;

///  <summary>Displays an error message noting an attempt was made to edit a
///  string item which is only valid if a specific File Flag is set.</summary>
///  <param name="StrDesc">string [in] Description of string item.</param>
procedure MsgNeedFileFlag(const StrDesc: string);

///  <summary>Displays a message box that offers to delete the text of a string
///  item that can only have a value if a specific file flag is set.</summary>
///  <param name="StrDesc">string [in] Description of string item.</param>
///  <returns>Boolean. True if the user agrees.</returns>
function MsgDeleteInvalidText(const StrDesc: string): Boolean;

///  <summary>Displays an error message noting a field or macro has been found
///  in the text of a string item where the field is not valid for the string
///  item.</summary>
///  <param name="Field">string [in] Name of field or macro.</param>
///  <param name="StrDesc">string [in] Description of string item.</param>
procedure MsgInvalidField(const Field, StrDesc: string);

///  <summary>Display an error message noting an identifier that begins with a
///  digit has been entered.</summary>
procedure MsgInvalidIdentifier;

///  <summary>Displays a messages that informs the user that no errors have been
///  found following analysis of a file.</summary>
procedure MsgNoAnalysisErrorsFound;

///  <summary>Displays a message noting an error occurred while accessing a
///  file.</summary>
///  <param name="FileName">string [in] Name of file being accessed.</param>
procedure MsgFileAccessError(const FileName: string);

///  <summary>Displays an error message noting that no file name was specified.
///  </summary>
procedure MsgNoFileName;


implementation


uses
  // Delphi
  SysUtils, Controls, Forms,
  // Project
  UHelp;


resourcestring
  // Dialog box messages
  sQuerySaveFile = '"%s" has changed.'#13
      + 'Changes will be lost unless the file is saved.'#13#13
      + 'Do you wish to save the file?';
  sInvalidExtension = '"%s" doesn''t have a valid extension.';
  sOKToOverwrite = 'File "%s" already exists.'#13#13'OK to overwrite?';
  sNoItemToEdit = 'You have not selected an item to edit.';
  sCantEditTitle = 'You have selected "%s" which is a title.'#13
      + 'Titles can''t be edited.';
  sCantEditSubType = 'Sub-type for file type "%s" is not used.';
  sInvalidNumber = 'An invalid number was entered.';
  sStringRequired = 'A string is required for %s.';
  sNeedFileFlag = 'The file flag for %s must be set'#13
      + 'before a string may be entered.';
  sDeleteInvalidText = '%s should not have a value'#13
      + 'since the related file flag is not set.'#13#13
      + 'Do you wish to delete the text?';
  sInvalidField = 'Field or macro "%s" is not valid for %s.';
  sInvalidIdentifier = 'An identifier can''t start with a digit.';
  sNoAnalysisErrorsFound = 'No errors were found.';
  sFileAccessError = 'An error occurred while attempting to access file'#13
      + '"%s".';
  sNoFileName = 'No file name was specified.';
  sUntitled = 'Untitled';

function Display(const Msg: string; const MsgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons): Word;
var
  Dlg: TForm;           // dialog box instance
begin
  Assert(MsgType <> mtCustom, 'Display: MsgType is mtCustom');
  // Create a dialog box of required type
  Dlg := CreateMessageDialog(Msg, MsgType, Buttons);
  try
    // Display the dialog and return result
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function MsgQuerySaveFile(const FileName: string): Word;
var
  Str: string;     {the filename to be displayed}
begin
  {If file name is '' use [Untitled] otherwise use the file name}
  if FileName <> '' then
    Str := ExtractFileName(FileName)
  else
    Str := '[' + sUntitled + ']';
  Result := Display(
    Format(sQuerySaveFile, [Str]), mtWarning, [mbYes, mbNo, mbCancel]
  );
end;

procedure MsgInvalidExtension(const FileName: string);
begin
  Display(
    Format(sInvalidExtension, [ExtractFileName(FileName)]), mtError, [mbOK]
  );
end;

function MsgOKToOverwrite(const FileName: string): Boolean;
begin
  Result := Display(
    Format(sOKToOverwrite, [FileName]), mtConfirmation, [mbYes, mbNo]
  ) = mrYes;
end;

procedure MsgNoItemToEdit;
begin
  Display(sNoItemToEdit, mtError, [mbOK]);
end;

procedure MsgCantEditTitle(const Title: string);
begin
  Display(Format(sCantEditTitle, [Title]), mtError, [mbOK]);
end;

procedure MsgCantEditSubType(const FType: string);
begin
  Display(Format(sCantEditSubType, [FType]), mtError, [mbOK]);
end;

procedure MsgInvalidNumber;
begin
  Display(sInvalidNumber, mtError, [mbOK]);
end;

function MsgStringRequired(const StrDesc: string): Word;
begin
  Result := Display(
    Format(sStringRequired, [StrDesc]), mtWarning, [mbOk, mbIgnore]
  );
end;

procedure MsgNeedFileFlag(const StrDesc: string);
begin
  Display(Format(sNeedFileFlag, [StrDesc]), mtError, [mbOk]);
end;

function MsgDeleteInvalidText(const StrDesc: string): Boolean;
begin
  Result := Display(
    Format(sDeleteInvalidText, [StrDesc]), mtWarning, [mbYes, mbNo]
  ) = mrYes;
end;

procedure MsgInvalidField(const Field, StrDesc: string);
begin
  Display(Format(sInvalidField, [Field, StrDesc]), mtError, [mbOK]);
end;

procedure MsgInvalidIdentifier;
begin
  Display(sInvalidIdentifier, mtError, [mbOK]);
end;

procedure MsgNoAnalysisErrorsFound;
begin
  Display(sNoAnalysisErrorsFound, mtInformation, [mbOK]);
end;

procedure MsgFileAccessError(const FileName: string);
begin
  Display(Format(sFileAccessError, [FileName]), mtError, [mbOK]);
end;

procedure MsgNoFileName;
begin
  Display(sNoFileName, mtError, [mbOK]);
end;

end.

