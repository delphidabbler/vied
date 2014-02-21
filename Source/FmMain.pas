{
 * FmMain.pas
 *
 * Main user interface and program logic for Version Information Editor.
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
 * The Original Code is FmMain.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1998-2014 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmMain;


interface


uses
  // Delphi
  Forms, StdCtrls, Dialogs, Menus, Classes, Controls, ExtCtrls, Windows,
  Messages, ComCtrls,
  // DelphiDabbler components
  PJVersionInfo, PJAbout, PJDropFiles, PJWdwState,
  // Project
  UCommonDlg, UVInfo;


const
  {
    Custom message posted when -makerc switch provided: handler stores .rc file
    for .vi file passed on command line without displaying window.
  }
  MSG_SILENT = WM_USER + 1;


type
  {
    TMainForm:
      Implements main program window and main program logic.
  }
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MFNew: TMenuItem;
    MFOpen: TMenuItem;
    MFSave: TMenuItem;
    MFSaveAs: TMenuItem;
    MFExport: TMenuItem;
    MFSpacer0: TMenuItem;
    MFPreferences: TMenuItem;
    MFViewRC: TMenuItem;
    MFSpacer2: TMenuItem;
    MFExit: TMenuItem;
    MEdit: TMenuItem;
    MECurrent: TMenuItem;
    MERCIdentifier: TMenuItem;
    MERCComments: TMenuItem;
    MEVIComments: TMenuItem;
    MESpacer1: TMenuItem;
    MEAnalyse: TMenuItem;
    MESpacer2: TMenuItem;
    MECopy: TMenuItem;
    MOptions: TMenuItem;
    MOAutoValidate: TMenuItem;
    MODescribeFileFlags: TMenuItem;
    MOSpacer: TMenuItem;
    MOUserSetup: TMenuItem;
    MOResCompiler: TMenuItem;
    MHelp: TMenuItem;
    MHContents: TMenuItem;
    MHSpacer1: TMenuItem;
    MHWebsite: TMenuItem;
    MHSpacer2: TMenuItem;
    MHAbout: TMenuItem;
    AboutBoxDlg: TPJAboutBoxDlg;
    AboutVersionInfo: TPJVersionInfo;
    WdwState: TPJRegWdwState;
    MESpacer0: TMenuItem;
    MECompOut: TMenuItem;
    MFCompile: TMenuItem;
    MFSpacer1: TMenuItem;
    FileCatcher: TPJFormDropFiles;
    MHHowDoI: TMenuItem;
    MHLicense: TMenuItem;
    DisplayListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure MFNewClick(Sender: TObject);
    procedure MFOpenClick(Sender: TObject);
    procedure MFSaveClick(Sender: TObject);
    procedure MFSaveAsClick(Sender: TObject);
    procedure MFPreferencesClick(Sender: TObject);
    procedure MFExitClick(Sender: TObject);
    procedure MECurrentClick(Sender: TObject);
    procedure MECopyClick(Sender: TObject);
    procedure MHContentsClick(Sender: TObject);
    procedure MHWebsiteClick(Sender: TObject);
    procedure MHAboutClick(Sender: TObject);
    procedure MEAnalyseClick(Sender: TObject);
    procedure MOAutoValidateClick(Sender: TObject);
    procedure MODescribeFileFlagsClick(Sender: TObject);
    procedure MOUserSetupClick(Sender: TObject);
    procedure MOResCompilerClick(Sender: TObject);
    procedure MFViewRCClick(Sender: TObject);
    procedure MERCIdentifierClick(Sender: TObject);
    procedure MERCCommentsClick(Sender: TObject);
    procedure MEVICommentsClick(Sender: TObject);
    procedure FileCatcherDropFiles(Sender: TObject);
    procedure WdwStateGetRegData(var RootKey: HKEY; var SubKey: String);
    procedure MFExportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MFileClick(Sender: TObject);
    procedure MFCompileClick(Sender: TObject);
    procedure MEditClick(Sender: TObject);
    procedure MECompOutFolderClick(Sender: TObject);
    procedure MHHowDoIClick(Sender: TObject);
    procedure MHLicenseClick(Sender: TObject);
    procedure DisplayListViewDblClick(Sender: TObject);
  private
    fSaveDlg: TSaveDialogEx;
    fExportDlg: TSaveDialogEx;
    fCompilerDlg: TSaveDialogEx;
    fOpenDlg: TOpenDialogEx;
    fVerInfo: TVInfo;
      {Instance of TVInfo class which formats, reads and saves version info}
    fCurrentFile: string;
      {Name of current file}
    fChanged: Boolean;
      {Flag true if current file has been altered since last load, save etc}
    procedure CreateCommonDlgs;
      {Creates and initialises dynamic common dialog box components.
      }
    procedure MsgSilent(var Msg: TMessage); message MSG_SILENT;
      {Message handler for custom "silent" mesasge sent when user provides a
      -makerc switch as first item on command line. We generate a .rc file from
      the .vi file passed 2nd on command line. I there is a 3rd item it is used
      as the output file name. The program is closed once processing is
      complete.
        @param Msg [in/out] Not used.
      }
    function GetVersionNumber(const VKind: string;
      const Value: TPJVersionNumber): TPJVersionNumber;
      {Gets a version number from user via a dialog box
        @param VKind [in] Type of version information required.
        @param Value [in] Default version number. Used if user cancels.
        @return Edited version number or Value if users cancels.
      }
    function GetDropDownChoice(const AKind: string;
      const AList: TStringList; const DefChoice: string): string;
      {Gets a string from user from amongst a list of possible choices displayed
      in a dialog box.
        @param AKind [in] Describes item being edited.
        @param AList [in] List of options for user to choose from.
        @param DefChoice [in] Default choice to use if user cancels.
        @return User's choice or DefChoice if user cancel.
      }
    function GetHexNumber(const AKind: string;
      const DefNumber: LongInt): LongInt;
      {Gets a number from the user via a dialog box.
        @param AKind [in] Describes item being edited.
        @param DefNumber [in] Default number to return if user cancels.
        @return Number entered by user, or DefNumber if user cancels.
      }
    function GetStringList(const AKind: string;
      const IList, EList: TStringList): TStringList;
      {Get a set of selections made from a list of item made by user in a dialog
      box.
        @param AKind [in] Describes item being edited.
        @param IList [in] Default list of selections.
        @param EList [in] List of additional possibilities that may be selected.
        @return List of selected items. This is same as IList when method exits.
      }
    function GetString(const AKind, DefChoice: string;
      const MustEnter: Boolean; const ValidFields: TStringList): string;
      {Gets a string up to 128 characters entered by the user in a dialog box.
      Text may optionally contain some "fields".
        @param AKind [in] Describes item being edited.
        @param DefChoice [in] String to return if user cancels.
        @param MustEnter [in] True if user must enter some text.
        @param ValidFields [in] Fields that are valid in entered text.
        @return String enetred by user or DefChoice if user cancels.
      }
    procedure CheckCompiler;
      {Checks if a resource compiler is specified, and exists if specified.
      Allows user to set up compiler if problems encountered.
      }
    function HaveCompiler: Boolean;
      {Checks if an external resource compiler has been specified and exists.
        @return True if compiler specified and exists, False if not.
      }
    procedure SetCurrentFile(const FName: string);
      {Sets current file name and updates caption.
        @param FName [in] Current file name.
      }
    function QueryFileSave: Boolean;
      {Check whether file needs saving (if it's changed) and whether user wants
      to save it.
        @return True if file to be saved, False otherwise.
      }
    function SaveFile: Boolean;
      {Tries to save the current file.
        @return True if file saved, False if not.
      }
    function SaveFileAs: Boolean;
      {Gets file name from user and tries to save to file.
        @return True if file saved, False otherwise.
      }
    procedure SavePreferences;
      {Saves preferences.
      }
    function OpenFile(const FileName: string): Boolean;
      {Opens a file as a new document.
        @param FileName [in] Name of file to open.
      }
    procedure OpenPreferences;
      {Opens preferences file if as a new document.
      }
    procedure DoSave(const FName: string);
      {Saves the current document to file.
        @param FName [in] Name of file.
      }
    procedure DoSavePreferences(const FName: string);
      {Writes the current document out as preferences.
        @param FName [in] Name of preferences files.
      }
    procedure DoExportRC(const FName: string);
      {Exports a resource source file. Doesn't update save flag or caption.
        @param FName [in] Name of export file.
      }
    procedure DoExportRes(const FName: string);
      {Exports a binary resource file. Deosn't update save flag or caption.
        @param FName [in] Name of export file.
      }
    function DoOpenVIFile(const FName: string): Boolean;
      {Opens a version information source file
        @param FName [in] Name of file.
        @return True if file opened successfuly, False otherwise.
      }
    procedure DoOpenPreferences(const FName: string);
      {Reads preferences in file.
        @param FName [in] Name of preferences file.
      }
    procedure DisplayVI;
      {Displays version information.
      }
    procedure EditViItem(Index: Integer);
      {Edits version information item at given index in list view.
        @param Index [in] Index of list view item whose version information is
          to be edited.
      }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Updates style of window to ensure this main window appears on task bar.
        @params Params [in/out] In: current parameters. Out: adjusted
          parameters.
      }
  end;


var
  MainForm: TMainForm;


implementation


uses
  // Delphi
  SysUtils, ShellAPI,
  // Project units
  UHelp, UMsgDlgs, UVerUtils, UUtils, USettings, UResCompiler,
  FmDropDownListEd, FmIdEd, FmNumberEd, FmResCompiler, FmResCompilerCheck,
  FmSetEd, FmStringEd, FmUserSetup, FmViewList, FmVerNumEd, FmResOutputDir;

{$R *.DFM}


const
  // File extensions
  cRCExt = '.rc';
  cResExt = '.res';
  cVIExt = '.vi';

resourcestring
  // Message dialogue box messages
  sBadCompilerPath = 'The resource compiler specified for use with Version'#13
      + 'Information Expert does not exist. Do you wish to edit'#13
      + 'the compiler''s details?'
      + #13#13'Click:'#13
      + '   ·   Yes to edit the compiler details.'#13
      + '   ·   No to make no changes.';
  sSuccess = 'File compiled successfully.';
  sCantRun = 'Can''t run resource compiler.'#13#13'Check compiler settings?';
  sCantCompile = 'Can''t compile this resource.'#13#13
    + 'The resource compiler returned error code %0:X hex, (%0:d decimal).'
    + #13'Please see your resource compiler documentation for details of this '
    + 'error.';
  sCompilePermission = 'Compiling will overwrite %0:s.';
  sAnalysisErrTitle = 'Analysis Errors';
  sAnalysisErrDesc = 'List of errors found during analysis:';

  // Dialogue box titles
  sFile = 'File';
  sProduct = 'Product';
  sFileOS = 'File OS';
  sFileType = 'File Type';
  sDrvSubType = 'Driver File Sub-type';
  sFontSubType = 'Font File Sub-type';
  sVXDSubType = 'Virtual Device Sub-type';
  sFileFlagsMask = 'File Flags Mask';
  sFileFlags = 'File Flags';
  sLanguage = 'Language';
  sCharSet = 'Character Set';
  sRCCommentsTitle = 'RC File Comments';
  sVICommentsTitle = 'VI File Comments';
  sViewRCTitle = 'View RC Statements';
  // Other
  sViewRCDesc = 'The resource file is:';  // dlg box descriptive text
  sUntitled = '[Untitled]'; // caption text when file is un-named

const
  // List View Group IDs of the different sections of the display
  lvgiFixedFileInfo = 0;
  lvgiTranslationInfo = 1;
  lvgiStringInfo = 2;

{ Helper routines }

function AppDataFolder: string;
  {Gets fully specified name of application data folder.
    @return Required folder name.
  }
begin
  Result := IncludeTrailingPathDelimiter(UserAppDataFolder)
    + 'DelphiDabbler\VIEd';
end;

function PreferencesFileName: string;
  {Gets fully specified name of preferences file.
    @return Required file name.
  }
begin
  Result := AppDataFolder + '\Preferences' + cVIExt;
end;

{ TMainForm }

procedure TMainForm.CheckCompiler;
  {Checks if a resource compiler is specified, and exists if specified. Allows
  user to set up compiler if problems encountered.
  }
var
  CompilerPath: string; // path to compiler
begin
  // Check if compiler specified
  CompilerPath := Settings.ReadStr(siCompilerPath);
  if CompilerPath = '' then
  begin
    // No compiler specified: ask user unless prohibited
    if not Settings.ReadBool(siNoCompilerCheck) then
      TResCompilerCheckDlg.SetResCompiler(Self);
  end
  else if not FileExists(CompilerPath) then
  begin
    // Specified compiler doesn't exist: ask user if wishes to change details
    if MessageDlg(sBadCompilerPath, mtWarning, [mbYes, mbNo], 0) = mrYes then
      // edit the compiler settings
      TResCompilerDlg.EditResCompiler(Self);
  end;
end;

procedure TMainForm.CreateCommonDlgs;
resourcestring
  sVIFileFilter = 'Version info files (*.vi)|*.vi';
  sExportDlgTitle = 'Export File';
  sCompilerDlgTitle = 'Compile To File';
  sCompilerDlgFilter = 'Resource binary files (*.res)|*.res';
begin
  fSaveDlg := TSaveDialogEx.Create(Self);
  fSaveDlg.Filter := sVIFileFilter;
  fSaveDlg.HelpTopic := 'dlg-filesave';

  fExportDlg := TSaveDialogEx.Create(Self);
  fExportDlg.Title := sExportDlgTitle;
  fExportDlg.HelpTopic := 'dlg-fileexport';

  fCompilerDlg := TSaveDialogEx.Create(Self);
  fCompilerDlg.Title := sCompilerDlgTitle;
  fCompilerDlg.Filter := sCompilerDlgFilter;
  fCompilerDlg.HelpTopic := 'dlg-compile';

  fOpenDlg := TOpenDialogEx.Create(Self);
  fOpenDlg.DefaultExt := 'vi';
  fOpenDlg.Filter := sVIFileFilter;
  fOpenDlg.HelpTopic := 'dlg-fileopen';
end;

procedure TMainForm.CreateParams(var Params: TCreateParams);
  {Updates style of window to ensure this main window appears on task bar.
    @params Params [in/out] In: current parameters. Out: adjusted parameters.
  }
begin
  inherited;
  Params.WinClassName := 'DelphiDabbler.VIEd.Main';
end;

procedure TMainForm.DisplayListViewDblClick(Sender: TObject);
var
  MousePos: TPoint;
  HitTestInfo: THitTests;
begin
  MousePos := DisplayListView.ScreenToClient(Mouse.CursorPos);
  HitTestInfo := DisplayListView.GetHitTestInfoAt(MousePos.X, MousePos.Y);
  if htNowhere in HitTestInfo then
    Exit;
  EditViItem(DisplayListView.ItemIndex);
end;

procedure TMainForm.DisplayVI;
  {Displays version information.
  }

  // Adds a list view item in given group with given caption and text
  procedure AddItem(AGroupID: Integer; const ACaption, AText: string);
  var
    LI: TListItem;
  begin
    LI := DisplayListView.Items.Add;
    LI.Caption := ACaption;
    LI.SubItems.Add(AText);
    LI.GroupID := AGroupID;
  end;

resourcestring
  sFileVersion = 'File Version #';
  sProductVersion = 'Product Version #';
  sFileOS = 'File OS';
  sFileType = 'File Type';
  sFileSubType = 'File Sub-type';
  sFileFlagsMask = 'File Flags Mask';
  sFileFlags = 'File Flags';
  sLanguage = 'Language';
  sCharSet = 'Character Set';
var
  I: TStrInfo;  // loop control for string info
begin
  // Ensure the VerUtils routines use Pascal Hex symbol for output
  UsePasHexSymbol(True);
  DisplayListView.Items.BeginUpdate;
  try
    DisplayListView.Clear;
    // Add Fixed File Info items to list
    AddItem(
      lvgiFixedFileInfo,
      sFileVersion,
      VersionNumberToStr(fVerInfo.FileVersionNumber)
    );
    AddItem(
      lvgiFixedFileInfo,
      sProductVersion,
      VersionNumberToStr(fVerInfo.ProductVersionNumber)
    );
    AddItem(
      lvgiFixedFileInfo,
      sFileOS,
      FileOSToStr(fVerInfo.FileOS)
    );
    AddItem(
      lvgiFixedFileInfo,
      sFileType, FileTypeToStr(fVerInfo.FileType)
    );
    AddItem(
      lvgiFixedFileInfo,
      sFileSubType,
      FileSubTypeToStr(fVerInfo.FileType, fVerInfo.FileSubType)
    );
    if fVerInfo.DescribeFileFlags then
    begin
      AddItem(
        lvgiFixedFileInfo,
        sFileFlagsMask,
        FileFlagSetToStr(fVerInfo.FileFlagsMask)
      );
      AddItem(
        lvgiFixedFileInfo,
        sFileFlags,
        FileFlagSetToStr(fVerInfo.FileFlags)
      );
    end
    else
    begin
      AddItem(
        lvgiFixedFileInfo,
        sFileFlagsMask,
        HexSymbol + IntToHex(fVerInfo.FileFlagsMask, 2)
      );
      AddItem(
        lvgiFixedFileInfo,
        sFileFlags,
        HexSymbol + IntToHex(fVerInfo.FileFlags, 2)
      );
    end;
    // Add Variable Info to list
    AddItem(
      lvgiTranslationInfo, sLanguage, LangCodeToStr(fVerInfo.LanguageCode)
    );
    AddItem(
      lvgiTranslationInfo, sCharSet, CharCodeToStr(fVerInfo.CharSetCode)
    );
    // Add String info to list
    for I := Low(TStrInfo) to High(TStrInfo) do
      AddItem(lvgiStringInfo, fVerInfo.StrDesc[I], fVerInfo.StrInfo[I]);
  finally
    DisplayListView.Items.EndUpdate;
  end;
end;

procedure TMainForm.DoExportRC(const FName: string);
  {Exports a resource source file. Doesn't update save flag or caption.
    @param FName [in] Name of export file.
  }
var
  PrevMode: Word; // Windows error mode flag to be restored
begin
  // Check that the drive is available for writing
  if FName <> '' then
  begin
    try
      // Record previous Windows error mode
      PrevMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        // Save the resource file
        fVerInfo.SaveToResourceFile(FName);
      finally
        // Restore the Windows error mode
        SetErrorMode(PrevMode);
      end;
    except
      // Trap file errors and display message
      on E: EInOutError do
        MsgFileAccessError(FName);
    end;
  end
  else
    // Report error due to drive not being available for access
    MsgNoFileName;
end;

procedure TMainForm.DoExportRes(const FName: string);
  {Exports a binary resource file. Deosn't update save flag or caption.
    @param FName [in] Name of export file.
  }
var
  ErrCode: Integer; // Error code returned by resource compiler (0 = OK)
begin
  // Execute resource compiler using path and command line per settings to
  // compile the required file
  ErrCode := ExecuteCompiler(
    fVerInfo,
    Settings.ReadStr(siCompilerPath),
    Settings.ReadStr(siCompilerCmdLine),
    FName
  );
  // Act according to compiler "error" code
  case ErrCode of
    0:
      // no error: say all is well
      MessageDlg(sSuccess, mtInformation, [mbOK], 0);
    -MaxInt:
      // can't run compiler
      if MessageDlg(sCantRun, mtError, [mbYes, mbNo], 0) = mrYes then
        MOResCompilerClick(Self);
    else
      // compiler failed to compile the source file
      MessageDlg(Format(sCantCompile, [ErrCode]), mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.DoOpenPreferences(const FName: string);
  {Reads preferences in file.
    @param FName [in] Name of preferences file.
  }
begin
  // Read preferences into fVerInfo, or clear VerInfo if can't read them
  fVerInfo.Clear;
  try
    if FileExists(FName) then
      fVerInfo.LoadFromFile(FName);
  except
    // Swallow any exception
  end;
  // Display result
  DisplayVI;
  // New file is untitled and unchanged
  SetCurrentFile('');
  fChanged := False;
end;

function TMainForm.DoOpenVIFile(const FName: string): Boolean;
  {Opens a version information source file
    @param FName [in] Name of file.
    @return True if file opened successfuly, False otherwise.
  }
var
  PrevMode: Word; // Windows error mode flag to be restored
begin
  // Assume failue
  Result := False;
  if FName <> '' then
  begin
    try
      // Switch off Windows file error reporting
      PrevMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        // Read file into fVerInfo and display details
        fVerInfo.LoadFromFile(FName);
        DisplayVI;
        // Set current file name to given name
        SetCurrentFile(FName);
        // Record that file hasn't been changed and that opened successfuly
        fChanged := False;
        Result := True;
      finally
        {Restore the Windows error mode}
        SetErrorMode(PrevMode);
      end;
    except
      // Catch and report any file errors with a generic message
      on E: EInOutError do
        MsgFileAccessError(FName);
    end;
  end
  else
    MsgNoFileName;
end;

procedure TMainForm.DoSave(const FName: string);
  {Saves the current document to file.
    @param FName [in] Name of file.
  }
begin
  // Check if drive letter can be written to
  if FName <> '' then
  begin
    // Get fVerInfo to write current settings to given file
    fVerInfo.SaveToFile(FName);
    // Set current file name to given name and note it's unchanged
    SetCurrentFile(FName);
    fChanged := False;
  end
  else
    // No file name
    MsgNoFileName;
end;

procedure TMainForm.DoSavePreferences(const FName: string);
  {Writes the current document out as preferences.
    @param FName [in] Name of preferences files.
  }
begin
  // Check if we can write to given drive
  if FName <> '' then
    // Get fVerInfo to write current settings to given file
    fVerInfo.SaveToFile(FName)
  else
    // No file name
    MsgNoFileName
end;

procedure TMainForm.EditViItem(Index: Integer);
  {Edits version information item at given index in list view.
    @param Index [in] Index of list view item whose version information is to be
      edited.
  }
const
  // Map of list view item index to pre-defined string information items
  cStrInfo: array[9..20] of TStrInfo =
    (siComments, siCompanyName, siFileDescription, siFileVersion,
    siInternalName, siLegalCopyright, siLegalTrademarks, siOriginalFileName,
    siPrivateBuild, siProductName, siProductVersion, siSpecialBuild);
var
  StrList: TStringList;    // for holding drop down lists / list box items
  StrList2: TStringList;   // for holding list box items
  StrInfoId: TStrInfo;     // for holding string info id
begin
  // Create the string lists
  StrList2 := nil;
  StrList := TStringList.Create;
  try
    StrList2 := TStringList.Create;
    // Act according to selected item in list box
    case Index of
      0: // File version number
        fVerInfo.FileVersionNumber := GetVersionNumber(
          sFile, fVerInfo.FileVersionNumber
        );
      1: // Product version number
        fVerInfo.ProductVersionNumber := GetVersionNumber(
          sProduct, fVerInfo.ProductVersionNumber
        );
      2: // File OS
        try
          fVerInfo.FileOS := StrToFileOS(
            GetDropDownChoice(
              sFileOS, FileOSCodeList(StrList), FileOSToStr(fVerInfo.FileOS)
            )
          );
        except
          on E: EVersionError do
            {nothing};
        end;
      3: // File Type
        try
          fVerInfo.FileType := StrToFileType(
            GetDropDownChoice(
              sFileType,
              FileTypeCodeList(StrList),
              FileTypeToStr(fVerInfo.FileType)
            )
          );
        except
          on E: EVersionError do
            {nothing};
        end;
      4: // File sub-type
        try
          case fVerInfo.FileType of
            VFT_DRV:   // Driver type
              fVerInfo.FileSubType := StrToFileSubType(
                fVerInfo.FileType,
                GetDropDownChoice(
                  sDrvSubType,
                  DriverSubTypeCodeList(StrList),
                  FileSubTypeToStr(fVerInfo.FileType, fVerInfo.FileSubType)
                )
              );
            VFT_FONT:  // Font type
              fVerInfo.FileSubType := StrToFileSubType(
                fVerInfo.FileType,
                GetDropDownChoice(
                  sFontSubType,
                  FontSubTypeCodeList(StrList),
                  FileSubTypeToStr(fVerInfo.FileType, fVerInfo.FileSubType)
                )
              );
            VFT_VXD:   // Virtual device driver type
              fVerInfo.FileSubType := GetHexNumber(
                sVXDSubType, fVerInfo.FileSubType
              );
            else       // All other file types - don't have sub-types
              MsgCantEditSubType(FileTypeToStr(fVerInfo.FileType));
          end;
        except
          on E: EVersionError do
            {nothing};
        end;
      5:   // File flags mask
        fVerInfo.FileFlagsMask := StrListToFileFlagSet(
          GetStringList(
            sFileFlagsMask,
            FileFlagSetToStrList(fVerInfo.FileFlagsMask, StrList),
            FileFlagSetToStrList(not fVerInfo.FileFlagsMask, StrList2)
          )
        );
      6:   // File flags
        fVerInfo.FileFlags := StrListToFileFlagSet(
          GetStringList(
            sFileFlags,
            FileFlagSetToStrList(
              fVerInfo.FileFlags and fVerInfo.FileFlagsMask,
              StrList
            ),
            FileFlagSetToStrList(
              (not fVerInfo.FileFlags) and fVerInfo.FileFlagsMask,
              StrList2
            )
          )
        );
      7:   // Language
        try
          fVerInfo.LanguageCode := StrToLangCode(
            GetDropDownChoice(
              sLanguage,
              LanguageStrList(StrList),
              LangCodeToStr(fVerInfo.LanguageCode)
            )
          );
        except
          on E: EVersionError do
            {nothing};
        end;
      8:  // Character set
        try
          fVerInfo.CharSetCode := StrToCharCode(
            GetDropDownChoice(
              sCharSet,
              CharSetStrList(StrList),
              CharCodeToStr(fVerInfo.CharSetCode)
            )
          );
        except
          on E: EVersionError do
            {nothing};
        end;
      9..20: // String Info items
      begin
        // find Id of string-info item
        StrInfoId := cStrInfo[Index];
        // build list of valid fields for the item
        fVerInfo.ValidFields(StrInfoId, StrList);
        // decide if we can enter a string
        //   only if we're not validating *or*
        //   we are validating and string is permitted
        if (not fVerInfo.Validating) or fVerInfo.StrPermitted[StrInfoId] then
          fVerInfo.StrInfo[StrInfoId] := GetString(
            fVerInfo.StrDesc[StrInfoId],
            fVerInfo.StrInfo[StrInfoId],
            fVerInfo.StrRequired[StrInfoId] and fVerInfo.Validating,
            StrList
          )
        else if fVerInfo.StrInfo[StrInfoId] = '' then
          // string not permitted and there is no string - prevent edit
          MsgNeedFileFlag(fVerInfo.StrDesc[StrInfoId])
        else if MsgDeleteInvalidText(fVerInfo.StrDesc[StrInfoId]) then
          // string not permitted, there is a value, user accepts deletion
          fVerInfo.StrInfo[StrInfoId] := '';
      end;
      -1: // No item selected - error
        MsgNoItemToEdit;
    end;
    // Re-display and select current item agian
    DisplayVI;
    DisplayListView.ItemIndex := Index;
  finally
    StrList2.Free;
    StrList.Free;
  end;
end;

procedure TMainForm.FileCatcherDropFiles(Sender: TObject);
  {Event handler for drop files component: opens the first file caught by the
  component.
    @param Sender [in] Not used.
  }
begin
  // Check if file can be loaded and succeeds and set open dlg to use path
  // next time it is opened
  if QueryFileSave and OpenFile(FileCatcher.FileName) then
    fOpenDlg.InitialDir := ExtractFilePath(FileCatcher.FileName);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  {Form closure query event handler: can close if file is saved or user doesn't
  want to save.
    @param Sender [in] Not used.
  }
begin
  CanClose := QueryFileSave;
end;

procedure TMainForm.FormCreate(Sender: TObject);
  {Form creation event handler}

  // ---------------------------------------------------------------------------
  function PixelsToDlgBaseUnits(Pixels: Integer): Integer;
    {Converts pixels to equivalent dialog base unit as used by TListBox TabWidth
    property. This calculation assumes that the system font is being used}
  begin
    Result := (Pixels * 4) div LoWord(GetDialogBaseUnits);
  end;
  // ---------------------------------------------------------------------------

begin
  // Create dynamic components
  CreateCommonDlgs;

  // Deselect list box item
  DisplayListView.Selected := nil;
  // Create Version info-processor object
  fVerInfo := TVInfo.Create;
  // Set user options in version info processor
  fVerInfo.Validating := Settings.ReadBool(siAutoValidate);
  fVerInfo.DescribeFileFlags := Settings.ReadBool(siDescribeFileFlags);
  // Update menu items
  MOAutoValidate.Checked := Settings.ReadBool(siAutoValidate);
  MODescribeFileFlags.Checked := Settings.ReadBool(siDescribeFileFlags);
  // Emulate File | New menu click to get new file using preferences
  MFNewClick(Self);
  // Process command line
  // Set initial dir of Open, Export and Save As dialog boxes to first parameter
  // unless param is -silent
  if ParamCount > 0 then
  begin
    // If first param is -makerc we need to create an .rc file from .vi file
    // name passed as second parameter. We do this without displaying main
    // window and we close program immediately
    if ParamStr(1) = '-makerc' then
    begin
      // prevent window from showing
      Application.ShowMainForm := False;
      // post message that processes file and closes window (can't close in
      // FormCreate)
      PostMessage(Handle, MSG_SILENT, 0, 0);
    end
    else
    begin
      // First param is considered to be default path for various file dialogs
      fOpenDlg.InitialDir := ParamStr(1);
      fSaveDlg.InitialDir := ParamStr(1);
      fExportDlg.InitialDir := ParamStr(1);
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
  {Form destruction event handler
    @param Sender [in] Not used.
  }
begin
  fVerInfo.Free;
  THelp.Quit;
end;

procedure TMainForm.FormShow(Sender: TObject);
  {Form show event handler: checks for presence of resource compiler.
    @param Sender [in] Not used.
  }

  // Returns which if any scroll bars are displayed by the given window.
  function WindowScrollbars(const Wnd: Windows.HWND): StdCtrls.TScrollStyle;
  var
    StyleFlags: Windows.DWORD;
  begin
    StyleFlags:= Windows.GetWindowLong(Wnd, Windows.GWL_STYLE) and
      (Windows.WS_VSCROLL or Windows.WS_HSCROLL);
    case StyleFlags of
      0: Result := StdCtrls.ssNone;
      Windows.WS_VSCROLL: Result := StdCtrls.ssVertical;
      Windows.WS_HSCROLL: Result := StdCtrls.ssHorizontal;
      else Result := StdCtrls.ssBoth;
    end;
  end;

  // Returns the width of the given control's vertical scrollbar if displayed or
  // 0 if not displayed.
  function VScrollbarWidth(const Ctrl: TWinControl): Integer;
  begin
    if WindowScrollbars(Ctrl.Handle)
      in [StdCtrls.ssVertical, StdCtrls.ssBoth] then
      Result := Windows.GetSystemMetrics(Windows.SM_CYVSCROLL)
    else
      Result := 0;
  end;

begin
  // Size the list box columns, allowing for scrollbar
  DisplayListView.Column[1].Width := 140;
  DisplayListView.Column[1].Width := DisplayListView.Width
    - DisplayListView.Column[0].Width - 4 - VScrollbarWidth(DisplayListView);
  CheckCompiler;
end;

function TMainForm.GetDropDownChoice(const AKind: string;
  const AList: TStringList; const DefChoice: string): string;
  {Gets a string from user from amongst a list of possible choices displayed in
  a dialog box.
    @param AKind [in] Describes item being edited.
    @param AList [in] List of options for user to choose from.
    @param DefChoice [in] Default choice to use if user cancels.
    @return User's choice or DefChoice if user cancel.
  }
var
  Ed: TDropDownListEditor; // instance of drop down list editor dlg box
begin
  // Create instance of dlg box
  Ed := TDropDownListEditor.Create(Self);
  try
    // Set required properties
    Ed.Kind := AKind;       // info for title
    Ed.Text := DefChoice;   // default choice from drop-down list
    Ed.List := AList;       // the options for drop down list
    // Display dlg box and act on user response (order of test is significant)
    fChanged := (Ed.ShowModal = mrOK) or fChanged;
    // Return selected text (will be unchanged if user pressed cancel)
    Result := Ed.Text;
  finally
    Ed.Free;
  end;
end;

function TMainForm.GetHexNumber(const AKind: string;
  const DefNumber: LongInt): LongInt;
  {Gets a number from the user via a dialog box.
    @param AKind [in] Describes item being edited.
    @param DefNumber [in] Default number to return if user cancels.
    @return Number entered by user, or DefNumber if user cancels.
  }
var
  Ed: TNumEditor; // instance of number editor dlg box
begin
  // Create the dlg box
  Ed := TNumEditor.Create(Self);
  try
    // Set required properties
    Ed.Kind := AKind;       // info for title
    Ed.Number := DefNumber; // the default number for editing
    Ed.ShowAsHex := True;   // tell dlg box to display default number as hex
    // Display dlg box and act on input: NB order of test is significant
    fChanged := (Ed.ShowModal = mrOK) or fChanged;
    // Return number entered (will be unchanged if user pressed cancel)
    Result := Ed.Number;
  finally
    Ed.Free;
  end;
end;

function TMainForm.GetString(const AKind, DefChoice: string;
  const MustEnter: Boolean; const ValidFields: TStringList): string;
  {Gets a string up to 128 characters entered by the user in a dialog box. Text
  may optionally contain some "fields".
    @param AKind [in] Describes item being edited.
    @param DefChoice [in] String to return if user cancels.
    @param MustEnter [in] True if user must enter some text.
    @param ValidFields [in] Fields that are valid in entered text.
    @return String enetred by user or DefChoice if user cancels.
  }
var
  Ed: TStringEditor;  // instance of dlg box
begin
  // Create dlg box
  Ed := TStringEditor.Create(Self);
  try
    // Set required properties
    Ed.Kind := AKind;               // info for title
    Ed.MaxLength := 128;            // limit length of text to 128
    Ed.WrapLines := True;           // word-wrap lines
    Ed.FixedWidthFont := False;     // use a proportional font to display text
    Ed.MemoHeight := 102;           // set height of memo component
    Ed.MemoWidth := 295;            // set width of memo component
    Ed.Text := DefChoice;           // default text for editing
    Ed.Compulsory := MustEnter;     // record whether user must enter some text
    Ed.ValidFields := ValidFields;  // record list of valid fields
    Ed.AllowEnter := False;         // pressing Enter closes dlg box
    Ed.HelpTopic := 'dlg-string';
    // Display dlg box and act on input: order of tests is significant
    fChanged := (Ed.ShowModal = mrOK) or fChanged;
    // Record resulting text
    Result := Ed.Text;
  finally
    Ed.Free;
  end;
end;

function TMainForm.GetStringList(const AKind: string;
  const IList, EList: TStringList): TStringList;
  {Get a set of selections made from a list of item made by user in a dialog
  box.
    @param AKind [in] Describes item being edited.
    @param IList [in] Default list of selections.
    @param EList [in] List of additional possibilities that may be selected.
    @return List of selected items. This is same as IList when method exits.
  }
var
  Ed: TSetEditor; // instance of dlg box
begin
  // Create dlg box
  Ed := TSetEditor.Create(Self);
  try
    // Set required properties
    Ed.Kind := AKind;     // info for title
    Ed.IncList := IList;  // list of items for "include" list
    Ed.ExcList := EList;  // list of items for "exclude" list
    // Display dlg and act on user entry: order of test is significant
    fChanged := (Ed.ShowModal = mrOK) or fChanged;
    // Record items now on include list
    IList.Assign(Ed.IncList);
    // Return a reference to the include list
    Result := IList;
  finally
    Ed.Free;
  end;
end;

function TMainForm.GetVersionNumber(const VKind: string;
  const Value: TPJVersionNumber): TPJVersionNumber;
  {Gets a version number from user via a dialog box
    @param VKind [in] Type of version information required.
    @param Value [in] Default version number. Used if user cancels.
    @return Edited version number or Value if users cancels.
  }
var
  Ed: TVerNumEditor;  // instance of version number editor dlg box
begin
  // Create instance of dlg box
  Ed := TVerNumEditor.Create(Self);
  try
    // Set required properties
    Ed.Kind := VKind;               // info for title
    Ed.VersionNumber := Value;      // default version number for editing
    // Display dlg and respond to user input: order of tests is significant
    fChanged := (Ed.ShowModal = mrOK) or fChanged;
    // Return new version number (will be unchanged if user pressed cancel)
    Result := Ed.VersionNumber;
  finally
    Ed.Free;
  end;
end;

function TMainForm.HaveCompiler: Boolean;
  {Checks if an external resource compiler has been specified and exists.
    @return True if compiler specified and exists, False if not.
  }
var
  CompilerPath: string; // full path to resource compiler
begin
  // Get path to compiler and check the executable file exists
  CompilerPath := Settings.ReadStr(siCompilerPath);
  if CompilerPath <> '' then
    Result := FileExists(Settings.ReadStr(siCompilerPath))
  else
    Result := False;
end;

procedure TMainForm.MEAnalyseClick(Sender: TObject);
  {Edit | Analyse menu click event handler.
    @param Sender [in] Not used.
  }
var
  EList: TStringList;    {string list to contain errors}
  DBox: TViewListDlg;    {instance of analysis dlg box}
begin
  // Create the list and the dialogue box
  EList := TStringList.Create;
  try
    DBox := TViewListDlg.Create(Self);
    try
      // Perform analysis and get list of errors
      if fVerInfo.Analyse(EList) then
        // No error - report the fact
        MsgNoAnalysisErrorsFound
      else
      begin
        // There were errors - display them
        // pass error list to dlg box
        DBox.List := EList;
        // set the dlg box's caption and description
        DBox.Title := sAnalysisErrTitle;
        DBox.Description := sAnalysisErrDesc;
        DBox.HelpTopic := 'dlg-analysis';
        // display the dlg box
        DBox.ShowModal;
      end;
    finally
      DBox.Free;
    end;
  finally
    EList.Free;
  end;
end;

procedure TMainForm.MECompOutFolderClick(Sender: TObject);
  {Edit | Compiler Output Folder menu clik event handler: displays a dialog box
  to allow user to enter the default output folder to use when compiling to a
  binary resource file.
    @param Sender [in] Not used.
  }
begin
  with TResOutputDirDlg.Create(Self) do
    try
      DirName := fVerInfo.ResOutputDir;
      if ShowModal = mrOK then
      begin
        fVerInfo.ResOutputDir := DirName;
        fChanged := True;
      end;
    finally
      Free;
    end;
end;

procedure TMainForm.MECopyClick(Sender: TObject);
  {Edit | Copy menu click event handler.
    @param Sender [in] Not used.
  }
begin
  fVerInfo.CopyToClipBoard;
end;

procedure TMainForm.MECurrentClick(Sender: TObject);
  {Edit | Current Item menu click event handler.
    @param Sender [in] Not used.
  }
begin
  EditViItem(DisplayListView.ItemIndex);
end;

procedure TMainForm.MEditClick(Sender: TObject);
  {Edit menu click event handler. Enables / disables relevant menu options just
  before Edit menu appears.
    @param Sender [in] Not used.
  }
begin
  // Compiler out folder item only enabled if an external compiler is specified
  MECompOut.Enabled := HaveCompiler;
end;

procedure TMainForm.MERCCommentsClick(Sender: TObject);
  {Edit | RC File Comments menu click event handler.
    @param Sender [in] Not used.
  }
var
  Ed: TStringEditor;  // instance of string editor dialogue box
begin
  // Create the dialogue box
  Ed := TStringEditor.Create(Self);
  try
    // Set properties of dlg box
    Ed.Kind := sRCCommentsTitle;      // goes in title
    Ed.MaxLength := 0;                // no limit on length of text
    Ed.WrapLines := False;            // don't wrap lines
    Ed.FixedWidthFont := True;        // use a fixed width font to display text
    Ed.MemoHeight := 215;             // set height of memo component
    Ed.MemoWidth := 409;              // set width of memo component
    Ed.Lines := fVerInfo.RCComments;  // the lines of text to edit
    Ed.Compulsory := False;           // user doesn't have to enter anything
    Ed.ValidFields := nil;            // no fields can be used
    Ed.AllowEnter := True;            // pressing enter starts a new line
    Ed.HelpTopic := 'dlg-comments';
    // Call dialogue box and record user response
    if Ed.ShowModal = mrOK then
    begin
      // User OK'd - record lines entered and that file has been changed
      fVerInfo.RCComments := TStringList(Ed.Lines);
      fChanged := True;
    end;
  finally
    Ed.Free;
  end;
end;

procedure TMainForm.MERCIdentifierClick(Sender: TObject);
  {Edit | RC File Identifier menu click event handler.
    @param Sender [in] Not used.
  }
var
  Ed: TIdEditor;  // instance of Identifier editor dlg box
begin
  // Create the dg box
  Ed := TIdEditor.Create(Self);
  try
    // Set default identifier & help context
    Ed.Identifier := fVerInfo.Identifier;
    // Display dlg box & get user's input and record if user made changes
    fChanged := (Ed.ShowModal = mrOK) or fChanged;
    // Record new identifier - this will be unchanged if user clicked cancel
    fVerInfo.Identifier := Ed.Identifier;
  finally
    Ed.Free;
  end;
end;

procedure TMainForm.MEVICommentsClick(Sender: TObject);
  {Edit | VI File Comments menu click event.
    @param Sender [in] Not used.
  }
var
  Ed: TStringEditor;  // instance of string editor dialogue box
begin
  // Create the dialogue box
  Ed := TStringEditor.Create(Self);
  try
    // Set properties of dlg box
    Ed.Kind := sVICommentsTitle;      // goes in title
    Ed.MaxLength := 0;                // no limit on length of text
    Ed.WrapLines := False;            // don't wrap lines
    Ed.FixedWidthFont := True;        // use a fixed width font to display text
    Ed.MemoHeight := 215;             // set height of memo component
    Ed.MemoWidth := 409;              // set width of memo component
    Ed.Lines := fVerInfo.VIComments;  // the lines of text to edit
    Ed.Compulsory := False;           // user doesn't have to enter anything
    Ed.ValidFields := nil;            // no fields can be used
    Ed.AllowEnter := True;            // pressing enter starts a new line
    Ed.HelpTopic := 'dlg-comments';
    // Display dlg and record whether user made changes and clicked OK
    if Ed.ShowModal = mrOK then
    begin
      // User clicked OK - record lines entered and that file has been changed
      fVerInfo.VIComments := TStringList(Ed.Lines);
      fChanged := True;
    end;
  finally
    Ed.Free;
  end;
end;

procedure TMainForm.MFCompileClick(Sender: TObject);
  {File | Compiler menu click event handler. Attempts to compile current
  document to a resource file.
    @param Sender [in] Not used.
  }
var
  FileName: string; // file name to use for .res file
begin
  // Don't do anything is there is no external compiler
  if not HaveCompiler then Exit;
  // Check if there is a user-specified .res file output folder
  if (fVerInfo.ResOutputDir <> '') and (fCurrentFile <> '') then
  begin
    // We have default output folder: uses it
    FileName := fVerInfo.ResOutputDir
      + ChangeFileExt(ExtractFileName(fCurrentFile), '.res');
    if not FileExists(FileName) or MsgOKToOverwrite(FileName) then
      DoExportRes(FileName);
  end
  else
  begin
    // No default output folder: get output file from user
    // display file dialog box
    if fCompilerDlg.InitialDir = '' then
      fCompilerDlg.InitialDir := fExportDlg.InitialDir;
    if fCompilerDlg.Execute then
    begin
      fCompilerDlg.FileName := EnsureExtension(
        fCompilerDlg.FileName, fCompilerDlg.Filter, fCompilerDlg.FilterIndex
      );
      // check if file exists and OK to overwrite if so
      if not FileExists(fCompilerDlg.FileName)
        or MsgOKToOverwrite(fCompilerDlg.FileName) then
      begin
        // compile the file
        DoExportRes(fCompilerDlg.FileName);
        // record file's folder for future reference
        fCompilerDlg.InitialDir := ExtractFilePath(fCompilerDlg.FileName);
      end;
    end;
  end;
end;

procedure TMainForm.MFExitClick(Sender: TObject);
  {File | Exit menu click event handler.
    @param Sender [in] Not used.
  }
begin
  Close;
end;

procedure TMainForm.MFExportClick(Sender: TObject);
  {File | Export menu click event handler. Exports document as either a source
  or binary resource file.
    @param Sender [in] Not used.
  }

  // ---------------------------------------------------------------------------
  function CanWrite: Boolean;
    {Checks if we can write a file.
      @return True if file can be written to, False otherwise.
    }
  begin
    Result := not FileExists(fExportDlg.FileName)
      or MsgOKToOverwrite(fExportDlg.FileName);
  end;
  // ---------------------------------------------------------------------------

const
  // Possible filters used by export dialog box
  cRCFilter = 'Resource source files (*.rc)|*.rc';
  cResFilter = 'Resource binary files (*.res)|*.res';
var
  Ext: string;  // extension
begin
  // Set up acceptable file filters / extensions
  // we can always write RC files
  fExportDlg.Filter := cRCFilter;
  if HaveCompiler then
    // we can write RES files if we have an associated compiler
    fExportDlg.Filter := fExportDlg.Filter + '|' + cResFilter;
  // Display Export dialog box and process selected file if user OKs
  if fExportDlg.Execute then
  begin
    // Ensure that file names without extension gets appropriate one
    fExportDlg.FileName := EnsureExtension(
      fExportDlg.FileName, fExportDlg.Filter, fExportDlg.FilterIndex
    );
    // Record file extension and use it to select how to export file
    Ext := ExtractFileExt(fExportDlg.FileName);
    if CompareText(Ext, cRCExt) = 0 then
    begin
      // Saving resource source file if possible
      if CanWrite then
      begin
        // Export a .rc file + remember dir used for next use of Save As
        DoExportRC(fExportDlg.FileName);
        fExportDlg.InitialDir := ExtractFilePath(fExportDlg.FileName);
      end;
    end
    else if CompareText(Ext, cResExt) = 0 then
    begin
      // Saving resource binary file if possible
      if CanWrite then
      begin
        // Export a .rc file + remember dir used for next use of Save As
        DoExportRes(fExportDlg.FileName);
        fExportDlg.InitialDir := ExtractFilePath(fExportDlg.FileName);
      end;
    end
    else
      // Can't cope with other extensions - refuse
      MsgInvalidExtension(fExportDlg.FileName);
  end;
end;

procedure TMainForm.MFileClick(Sender: TObject);
  {File menu click event handler. Enables / disables relevant menu options just
  before File menu appears.
    @param Sender [in] Not used.
  }
begin
  // only enable Compile menu option is an external compiler is specified
  MFCompile.Enabled := HaveCompiler;
end;

procedure TMainForm.MFNewClick(Sender: TObject);
  {File | New menu click even handler.
    @param Sender [in] Not used.
  }
begin
  OpenPreferences
end;

procedure TMainForm.MFOpenClick(Sender: TObject);
  {File | Open menu click event handler.
    @param Sender [in] Not used.
  }
begin
  if QueryFileSave and fOpenDlg.Execute and OpenFile(fOpenDlg.FileName) then
    // File open succeeded: record path so open dlg uses it next time
    fOpenDlg.InitialDir := ExtractFilePath(fOpenDlg.FileName);
end;

procedure TMainForm.MFPreferencesClick(Sender: TObject);
  {File | Save Preferences menu click event handler.
    @param Sender [in] Not used.
  }
begin
  SavePreferences;
end;

procedure TMainForm.MFSaveAsClick(Sender: TObject);
  {File | Save As menu click event handler.
    @param Sender [in] Not used.
  }
begin
  SaveFileAs;
end;

procedure TMainForm.MFSaveClick(Sender: TObject);
  {File | Save menu click event handler.
    @param Sender [in] Not used.
  }
begin
  SaveFile;
end;

procedure TMainForm.MFViewRCClick(Sender: TObject);
  {File | View RC Statements menu click event handler.
    @param Sender [in] Not used.
  }
var
  DBox: TViewListDlg; // the instance of the dialogue box
  List: TStringList;  // list of lines of resource statements to be viewed
begin
  // Create the list and the dialogue box
  List := TStringList.Create;
  try
    DBox := TViewListDlg.Create(Self);
    try
      // Get the list of recource file statements and copy to dialogue box
      fVerInfo.WriteASRC(List);
      DBox.List := List;
      // Set the dlg box's caption and description
      DBox.Title := sViewRCTitle;
      DBox.Description := sViewRCDesc;
      DBox.HelpTopic := 'dlg-viewrc';
      // Display the dlg
      DBox.ShowModal;
    finally
      DBox.Free;
    end;
  finally
    List.Free;
  end;
end;

procedure TMainForm.MHAboutClick(Sender: TObject);
  {Help | About menu click event handler.
    @param Sender [in] Not used.
  }
begin
  AboutBoxDlg.Execute;
end;

procedure TMainForm.MHContentsClick(Sender: TObject);
  {Help | Contents menu click event handler.
    @param Sender [in] Not used.
  }
begin
  THelp.Contents;
end;

procedure TMainForm.MHHowDoIClick(Sender: TObject);
  {Help | How Do I? menu click event handler: displays "How Do I?" topic in
  help file.
    @param Sender [in] Not used.
  }
begin
  THelp.ShowTopic('howto');
end;

procedure TMainForm.MHLicenseClick(Sender: TObject);
  {Help | License menu click event handler: displays program's license in help
  file.
    @param Sender [in] Not used.
  }
begin
  THelp.ShowTopic('license');
end;

procedure TMainForm.MHWebsiteClick(Sender: TObject);
  {Help | Visit DelphiDabbler.com menu click event handler: accesses
  DelphiDabbler website.
    @param Sender [in] Not used.
  }
begin
  if ShellAPI.ShellExecute(Handle, nil, 'http://www.delphidabbler.com', nil,
    nil, SW_NORMAL) <= 32 then
    RaiseLastOSError();
end;

procedure TMainForm.MOAutoValidateClick(Sender: TObject);
  {Options | Automatic Validation menu click event handler.
    @param Sender [in] Not used.
  }
begin
  // Toggle check mark on menu option
  MOAutoValidate.Checked := not MOAutoValidate.Checked;
  // Use menu option check mark to determine whether to validate entries
  fVerInfo.Validating := MOAutoValidate.Checked;
end;

procedure TMainForm.MODescribeFileFlagsClick(Sender: TObject);
  {Options | Describe File Flags menu click event handler.
    @param Sender [in] Not used.
  }
begin
  // Toggle check mark on menu option
  MODescribeFileFlags.Checked := not MODescribeFileFlags.Checked;
  // Use menu option check mark to determine whether to describe file flags
  fVerInfo.DescribeFileFlags := MODescribeFileFlags.Checked;
  // Update display to reflect new choice
  DisplayVI;
end;

procedure TMainForm.MOResCompilerClick(Sender: TObject);
  {Options | Resource Compiler menu click event handler: displays dialog box
  used to configure resource compiler.
    @param Sender [in] Not used.
  }
begin
  TResCompilerDlg.EditResCompiler(Self);
end;

procedure TMainForm.MOUserSetupClick(Sender: TObject);
  {Options | User Setup menu click event handler.
    @param Sender [in] Not used.
  }
var
  DBox: TUserSetupDlg;  // instance of uer setup dialogue box
begin
  // Create the dialogue box
  DBox := TUserSetupDlg.Create(Self);
  try
    // Set check boxes to default values
    DBox.AutoValidate := Settings.ReadBool(siAutoValidate);
    DBox.DescribeFileFlags := Settings.ReadBool(siDescribeFileFlags);
    // Display the dlg box and act on result
    if DBox.ShowModal = mrOK then
    begin
      // User clicked OK - update set up file
      Settings.WriteBool(siAutoValidate, DBox.AutoValidate);
      Settings.WriteBool(siDescribeFileFlags, DBox.DescribeFileFlags);
    end;
  finally
    DBox.Free;
  end;
end;

procedure TMainForm.MsgSilent(var Msg: TMessage);
  {Message handler for custom "silent" mesasge sent when user provides a
  -makerc switch as first item on command line. We generate a .rc file from the
  .vi file passed 2nd on command line. I there is a 3rd item it is used as
  the output file name. The program is closed once processing is complete.
    @param Msg [in/out] Not used.
  }
var
  InFile: string;   // name of input file (2nd on command line)
  OutFile: string;  // name of output file (3rd on command line)
begin
  // Assume success
  ExitCode := 0;
  try
    // Record input file: error if not present or doesn't exist
    InFile := ParamStr(2);
    if (InFile = '') or not FileExists(InFile) then
    begin
      ExitCode := 1;  // error 1 => missing in file
      Exit;
    end;
    // Record output file: either 3rd on command line or same as InFile with
    // .vi extension
    OutFile := ParamStr(3);
    if OutFile = '' then
      OutFile := ChangeFileExt(InFile, cRCExt);
    // Load input file
    try
      fVerInfo.LoadFromFile(InFile);
    except
      ExitCode := 2;  // error 2 => exception on loading file
      Exit;
    end;
    // Save file as .rc
    try
      fVerInfo.SaveToResourceFile(OutFile);
    except
      ExitCode := 3;  // error 3 => exception on saving file
      Exit;
    end;
  finally
    // Close program without displaying window
    Close;
  end;
end;

function TMainForm.OpenFile(const FileName: string): Boolean;
  {Opens a file as a new document.
    @param FileName [in] Name of file to open.
  }
begin
  // check if extension is supported
  if AnsiCompareText(ExtractFileExt(FileName), cVIExt) = 0 then
  begin
    // .vi suported: try to open file
    Result := DoOpenVIFile(FileName);
  end
  else
  begin
    // in-valid extension - tell user
    MsgInvalidExtension(FileName);
    Result := False;
  end;
end;

procedure TMainForm.OpenPreferences;
  {Opens preferences file if as a new document.
  }
begin
  //Check if we can dispose of old file
  if QueryFileSave then
    // We can - read new file from applications ini file
    DoOpenPreferences(PreferencesFileName);
end;

function TMainForm.QueryFileSave: Boolean;
  {Check whether file needs saving (if it's changed) and whether user wants to
  save it.
    @return True if file to be saved, False otherwise.
  }
begin
  // Check if file has changed
  if fChanged then
    // File has changed - ask user if they want to save it
    case MsgQuerySaveFile(fCurrentFile) of
      mrYes:
        // user want to save: result depends on whether saved OK
        //   it's OK to throw away if file was saved OK
        Result := SaveFile;
      mrNo:
        // user doesn't want to save - it's OK to loose changes
        Result := True;
      mrCancel:
        // user cancelled - don't throw away changes
        Result := False;
      else
        raise Exception.Create(
          'TMainForm.QueryFileSave: Unknown result from MsgQuerySaveFile()'
        );
    end
  else
    // File wasn't changed so can be thrown away safely
    Result := True;
end;

function TMainForm.SaveFile: Boolean;
  {Tries to save the current file.
    @return True if file saved, False if not.
  }
begin
  if fCurrentFile = '' then
    // Current file un-named - use SaveFileAs
    Result := SaveFileAs
  else
  begin
    // Current file is named - save it and return true - no error trapping
    DoSave(fCurrentFile);
    Result := True;
  end;
end;

function TMainForm.SaveFileAs: Boolean;
  {Gets file name from user and tries to save to file.
    @return True if file saved, False otherwise.
  }
var
  Ext: string;  // extension
begin
  // Assume we didn't save file
  Result := False;
  fSaveDlg.FileName := fCurrentFile;
  if fSaveDlg.Execute then
  begin
    // Ensure that file names without extension gets appropriate one
    fSaveDlg.FileName := EnsureExtension(fSaveDlg.FileName,
      fSaveDlg.Filter, fSaveDlg.FilterIndex);
    // Test extension and act on result
    Ext := ExtractFileExt(fSaveDlg.FileName);
    if (AnsiCompareText(Ext, cVIExt) = 0) then
    begin
      // Saving version information file
      if not FileExists(fSaveDlg.FileName)
        or MsgOKToOverwrite(fSaveDlg.FileName) then
      begin
        // Save a .vi file and set result to true: remember dir used for next
        // use of Save As
        DoSave(fSaveDlg.FileName);
        fSaveDlg.InitialDir := ExtractFilePath(fSaveDlg.FileName);
        Result := True;
      end
    end
    else
      // Can't cope with other extensions - refuse
      MsgInvalidExtension(fSaveDlg.FileName);
  end;
end;

procedure TMainForm.SavePreferences;
  {Saves preferences.
  }
begin
  // Write current file details to preferences file
  ForceDirectories(AppDataFolder);
  DoSavePreferences(PreferencesFileName);
end;

procedure TMainForm.SetCurrentFile(const FName: string);
  {Sets current file name and updates caption.
    @param FName [in] Current file name.
  }
begin
  // Record name
  fCurrentFile := FName;
  // Update caption with either name of file or [Untitled]
  if FName = '' then
    Caption := Application.Title + ' - ' + sUntitled
  else
    Caption := Application.Title + ' - ' + UpperCase(ExtractFileName(FName));
end;

procedure TMainForm.WdwStateGetRegData(var RootKey: HKEY;
  var SubKey: String);
  {Handler for event triggered by window state component when it needs to know
  where to read/write data in registry.
    @param RootKey [in/out] Required registry root key. Left as default.
    @param SubKey [in/out] Set to registry sub key where window state
      information is stored.
  }
begin
  SubKey := Settings.MainWindowKey;
end;

end.

