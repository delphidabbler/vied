{
 * UMsgDlgs.pas
 *
 * Simple message dialog boxes used by Version Information Editor program.
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
 * The Original Code is UMsgDlgs.pas.
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


unit UMsgDlgs;


interface


uses
  // Delphi
  Classes;


function MsgQuerySaveFile(const FileName: string;
  const HelpContext: THelpContext): Word;
  {Warns that a file has changed and contents will be lost and gets user's
  response.
    @param FileName [in] Name of file that has changed.
    @param HelpContext [in] References help topic associated with dialog box.
    @return mrYes to save file, mrNo to abandon file or mrCancel to cancel the
      current operation.
  }

procedure MsgInvalidExtension(const FileName: string;
  const HelpContext: THelpContext);
  {Error message that notes a file has an invalid extension.
    @param FileName [in] Name of file with invalid extenstion.
    @param HelpContext [in] References help topic associated with dialog box.
  }

function MsgOKToOverwrite(const FileName: string;
  const HelpContext: THelpContext): Boolean;
  {Seeks confirmation that it's OK to overwrite a file.
    @param FileName [in] Name of file to be overwritten.
    @param HelpContext [in] References help topic associated with dialog box.
    @return True if it's OK to overwrite file, False if not.
  }

procedure MsgNoItemToEdit(const HelpContext: THelpContext);
  {Error message noting an attempt has been made to edit an item when none is
  selected.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgCantEditTitle(const Title: string;
  const HelpContext: THelpContext);
  {Error message noting an attempt has been made to edit a title.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgCantEditSubType(const FType: string;
  const HelpContext: THelpContext);
  {Error message noting an attempt has been made to edit a File Sub-Type for
  a File Type which has no Sub-Type.
    @param FType [in] File type that has no sub-type.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgInvalidNumber(const HelpContext: THelpContext);
  {Error message noting an invalid number was entered.
    @param HelpContext [in] References help topic associated with dialog box.
  }

function MsgStringRequired(const StrDesc: string;
  const HelpContext: THelpContext): Word;
  {Warns that an entry is required for a string item.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
    @return mrOK if user agrees to provide a string or mrIgnore if the user
      chooses to ignore the warning.
  }

procedure MsgNeedFileFlag(const StrDesc: string;
  const HelpContext: THelpContext);
  {Error message noting an attempt was made to edit a string item which is only
  valid if a specific File Flag is set.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
  }

function MsgDeleteInvalidText(const StrDesc: string;
  const HelpContext: THelpContext): Boolean;
  {Offers to delete the text of a string item that can only have a value if a
  specific file flag is set.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
    @return True if the user agrees and False if not.
  }

procedure MsgInvalidField(const Field, StrDesc: string;
  const HelpContext: THelpContext);
  {Error message noting a field has been found in the text of a string item
  where the field is not valid for the string item.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgInvalidIdentifier(const HelpContext: THelpContext);
  {Error message noting an identifier that begins with a digit has been entered.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgNoAnalysisErrorsFound(const HelpContext: THelpContext);
  {Informs the user that no errors have been found following analysis of a file.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgFileAccessError(const FileName: string;
  const HelpContext: THelpContext);
  {Error message noting an error occurred while accessing a file.
    @param FileName [in] Name of file being accessed.
    @param HelpContext [in] References help topic associated with dialog box.
  }

procedure MsgNoFileName;
  {Error message noting that no file name was specified.
  }


implementation


uses
  // Delphi
  SysUtils, Controls, Dialogs, Forms,
  // Project
  UAltBugFix;


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
  sInvalidField = 'Field "%s" is not valid for %s.';
  sInvalidIdentifier = 'An identifier can''t start with a digit.';
  sNoAnalysisErrorsFound = 'No errors were found.';
  sFileAccessError = 'An error occurred while attempting to access file'#13
      + '"%s".';
  sNoFileName = 'No file name was specified.';
  sUntitled = 'Untitled';


function Display(const Msg: string; const MsgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons; const HelpContext: THelpContext): Word;
  {Displays a message in a customised dialog box.
    @param Msg [in] Message displayed in dialog.
    @param MsgType [in] Type of dialog box. Must not be mtCustom.
    @param Buttons [in] Set of buttons to display in dialog box.
    @param HelpContext [in] Help context number. Help button displayed if non
      zero.
    @return Value indicating which button was pressed to close dialog box.
  }
var
  Dlg: TForm;           // dialog box instance
  Btns: TMsgDlgButtons; // set of buttons to be displayed
begin
  Assert(MsgType <> mtCustom,                              // ** do not localise
    'TMessageBox.Display: MsgType is mtCustom');
  // Create a dialog box of required type
  Btns := Buttons;
  if HelpContext <> 0 then
    Include(Btns, mbHelp);
  Dlg := CreateMessageDialog(Msg, MsgType, Btns);
  try
    // Set help context
    Dlg.HelpContext := HelpContext;
    // Register dialog box to fix Delphi's Alt bug
    AltBugFix.RegisterCtrl(Dlg);
    // Display the dialog and return result
    Result := Dlg.ShowModal;
    // Unregister bug fixer
    AltBugFix.UnRegisterCtrl(Dlg);
  finally
    Dlg.Free;
  end;
end;

function MsgQuerySaveFile(const FileName: string;
  const HelpContext: THelpContext): Word;
  {Warns that a file has changed and contents will be lost and gets user's
  response.
    @param FileName [in] Name of file that has changed.
    @param HelpContext [in] References help topic associated with dialog box.
    @return mrYes to save file, mrNo to abandon file or mrCancel to cancel the
      current operation.
  }
var
  Str: string;     {the filename to be displayed}
begin
  {If file name is '' use [Untitled] otherwise use the file name}
  if FileName <> '' then
    Str := ExtractFileName(FileName)
  else
    Str := '[' + sUntitled + ']';
  Result := Display(
    Format(sQuerySaveFile, [Str]),
    mtWarning,
    [mbYes, mbNo, mbCancel, mbHelp],
    HelpContext
  );
end;

procedure MsgInvalidExtension(const FileName: string;
  const HelpContext: THelpContext);
  {Error message that notes a file has an invalid extension.
    @param FileName [in] Name of file with invalid extenstion.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    Format(sInvalidExtension, [ExtractFileName(FileName)]),
    mtError,
    [mbOK, mbHelp],
    HelpContext
  );
end;

function MsgOKToOverwrite(const FileName: string;
  const HelpContext: THelpContext): Boolean;
  {Seeks confirmation that it's OK to overwrite a file.
    @param FileName [in] Name of file to be overwritten.
    @param HelpContext [in] References help topic associated with dialog box.
    @return True if it's OK to overwrite file, False if not.
  }
begin
  Result := Display(
    Format(sOKToOverwrite, [FileName]),
    mtConfirmation,
    [mbYes, mbNo, mbHelp],
    HelpContext
  ) = mrYes;
end;

procedure MsgNoItemToEdit(const HelpContext: THelpContext);
  {Error message noting an attempt has been made to edit an item when none is
  selected.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(sNoItemToEdit, mtError, [mbOK, mbHelp], HelpContext);
end;

procedure MsgCantEditTitle(const Title: string;
  const HelpContext: THelpContext);
  {Error message noting an attempt has been made to edit a title.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    Format(sCantEditTitle, [Title]), mtError, [mbOK, mbHelp], HelpContext
  );
end;

procedure MsgCantEditSubType(const FType: string;
  const HelpContext: THelpContext);
  {Error message noting an attempt has been made to edit a File Sub-Type for
  a File Type which has no Sub-Type.
    @param FType [in] File type that has no sub-type.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    Format(sCantEditSubType, [FType]), mtError, [mbOK, mbHelp], HelpContext
  );
end;

procedure MsgInvalidNumber(const HelpContext: THelpContext);
  {Error message noting an invalid number was entered.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(sInvalidNumber, mtError, [mbOK, mbHelp], HelpContext);
end;

function MsgStringRequired(const StrDesc: string;
  const HelpContext: THelpContext): Word;
  {Warns that an entry is required for a string item.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
    @return mrOK if user agrees to provide a string or mrIgnore if the user
      chooses to ignore the warning.
  }
begin
  Result := Display(
    Format(sStringRequired, [StrDesc]),
    mtWarning,
    [mbOk, mbIgnore, mbHelp],
    HelpContext
  );
end;

procedure MsgNeedFileFlag(const StrDesc: string;
  const HelpContext: THelpContext);
  {Error message noting an attempt was made to edit a string item which is only
  valid if a specific File Flag is set.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    Format(sNeedFileFlag, [StrDesc]), mtError, [mbOk, mbHelp], HelpContext
  );
end;

function MsgDeleteInvalidText(const StrDesc: string;
  const HelpContext: THelpContext): Boolean;
  {Offers to delete the text of a string item that can only have a value if a
  specific file flag is set.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
    @return True if the user agrees and False if not.
  }
begin
  Result := Display(
    Format(sDeleteInvalidText, [StrDesc]),
    mtWarning,
    [mbYes, mbNo, mbHelp],
    HelpContext
  ) = mrYes;
end;

procedure MsgInvalidField(const Field, StrDesc: string;
  const HelpContext: THelpContext);
  {Error message noting a field has been found in the text of a string item
  where the field is not valid for the string item.
    @param StrDesc [in] Description of string item.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    Format(sInvalidField, [Field, StrDesc]),
    mtError,
    [mbOK, mbHelp],
    HelpContext
  );
end;

procedure MsgInvalidIdentifier(const HelpContext: THelpContext);
  {Error message noting an identifier that begins with a digit has been entered.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(sInvalidIdentifier, mtError, [mbOK, mbHelp], HelpContext);
end;

procedure MsgNoAnalysisErrorsFound(const HelpContext: THelpContext);
  {Informs the user that no errors have been found following analysis of a file.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    sNoAnalysisErrorsFound, mtInformation, [mbOK, mbHelp], HelpContext
  );
end;

procedure MsgFileAccessError(const FileName: string;
  const HelpContext: THelpContext);
  {Error message noting an error occurred while accessing a file.
    @param FileName [in] Name of file being accessed.
    @param HelpContext [in] References help topic associated with dialog box.
  }
begin
  Display(
    Format(sFileAccessError, [FileName]), mtError, [mbOK, mbHelp], HelpContext
  );
end;

procedure MsgNoFileName;
  {Error message noting that no file name was specified.
  }
begin
  Display(sNoFileName, mtError, [mbOK], 0);
end;

end.

