{
 * FmMain.pas
 *
 * Main user interface and program logic for Version Information Editor.
 *
 * Original unit name was Main.pas. Changed to FmMain.pas at v1.6.
 *
 * Required the following DelphiDabbler components:
 *   - TPJAboutBox (PJAbout unit v3.4 or later)
 *   - TPJVersionInfo (PJVersionInfo unit v3.1.1 or later)
 *   - TPJFormDropFiles (PJDropFiles unit v5.0.1 or later)
 *   - TPJRegWdwState (PJWdwState unit v5.3 or later)
 *
 * v1.0 of 25 May 1998  - Original version, named Main.pas.
 * v1.1 of 07 Jul 1998  - The Open and Save As dialogs now select the path used
 *                        when the dialog box was last opened. If the program is
 *                        started with a command line parameter the path of the
 *                        parameter is used when the dialogs are first
 *                        displayed.
 * v1.2 of 13 Apr 1999  - Made inclusion of Delphi 1's Ver unit conditional on
 *                        Windows version.
 * v1.3 of 24 Apr 1999  - Made changes to work with revised string editor dialog
 *                        box that is used both for editing comments and
 *                        resource strings:
 *                        - Sets up comments editor so it has no maximum string
 *                          length and performs no word-wrapping.
 *                        - Sets up resource string editor it continues to wrap
 *                          text and has a new 128 character limit on string
 *                          lengths.
 * v1.4 of 25 Apr 1999  - Made changes to work with further new features in
 *                        string editor dialog box that is used both for editing
 *                        comments and resource strings:
 *                        - Comments editor now gets a larger editing area and
 *                          displays comments in mono-spaced font.
 *                        - Resource string editor continues to use main window
 *                          font and a smaller editing area.
 * v1.5 of 17 Mar 2002  - Updated to enable compilation with Delphi 4 as 32 bit
 *                        only program.
 *                        - Removed tabbed list box - component no longer
 *                          available. Replaced with conventional list box with
 *                          TabWidth property used to store single tab stop
 *                          required.
 *                        - Replaced version info component with newer (v3.0)
 *                          version and changed unit name from VerInfo to
 *                          PJVersionInfo.
 *                        - Replaced about box component with newer (v3.2)
 *                          version and changed unit name from AboutBox to
 *                          PJAbout.
 *                        - Removed all conditional compilation directives and
 *                          16 bit-only code.
 * v1.6 of 17 Mar 2002  - Added facility to open files by dropping them onto
 *                        main window.
 *                      - Renamed unit from Main.pas to FmMain.pas.
 * v1.7 of 18 Mar 2002  - Changed uses clause to use renamed dialog box units.
 *                      - Modified header height and font to conform to 32 bit
 *                        Windows standards.
 *                      - Added extra try..finally blocks to some methods that
 *                        create and free objects.
 * v1.8 of 18 Mar 2002  - Added component to animate window minimising.
 *                      - Added component to store / restore window state using
 *                        registry.
 *                      - Revised code that stores user options to use new
 *                        settings object that uses registry instead of old ini
 *                        file based object.
 *                      - Made about box appear offset over main not at screen
 *                        centre.
 * v1.9 of 05 May 2002  - Added support for exporting binary resource files in
 *                        addition to source files. In particular:
 *                        - Added new File | Export menu option for export to
 *                          either rc or res files.
 *                        - Removed export facility from File | Save As menu
 *                          option.
 *                        - Added new menu option to enable user to configure
 *                          external resource compiler used to actually create
 *                          the binary .res files.
 *                        - Added start-up check for a configured external
 *                          compiler which trigeers prompt to provide details
 *                          if no compiler is registered.
 *                      - Also added new help constants for revised help file
 *                        (including help topics for each main menu item).
 * v1.10 of 06 May 2002 - Now uses TPJVersionNumber type in place of
 *                        TVersionNumber from old VerTypes unit.
 *                      - Removed usage of VerTypes unit.
 * v1.11 of 17 Mar 2003 - Added new Help menu option and code to visit
 *                        DelphiDabbler.com website.
 *                      - Replaced reference to VInfoExp.inc help topic include
 *                        file with reference to UHelp unit.
 * v1.12 of 18 Nov 2003 - Added new File|Compile menu option to directly compile
 *                        the current document file using the external compiler.
 *                      - Added Ctrl+E shortcut to File|Export menu option.
 *                      - Added Edit|Compile Output Folder menu option to dispay
 *                        a dialog that Lets user enter default folder to be
 *                        used by the File |Compile menu option.
 *                      - Moved local resource string declarations from methods
 *                        into global resource string list.
 * v1.13 of 04 Dec 2003 - Altered File | Save As code so that a .vi extension
 *                        is appended to any file name where the user has
 *                        provided no extension.
 * v1.14 of 20 Mar 2005 - Added facility to call program with -makerc switch and
 *                        a .vi file on command line. Program creates .rc file
 *                        from it and then terminates without displaying a
 *                        window.
 * v1.15 of 30 Apr 2008 - About box now aligns itself over main form rather
 *                        than being explictly aligned. This uses a feature of
 *                        about box component v3.3.1.
 *                      - Changed to write preferences to Preferences.ini in
 *                        user application data folder rather to VIEd.ini in
 *                        program's folder.
 *                      - Removed code that checks a drive by writing test file:
 *                        can't write test file to C:\ on Vista.
 *                      - Changed some file error messages to display dialog
 *                        when no file name is provided.
 *                      - Changed to use renamed UMsgDlgs, UUtils, UVerUtils and
 *                        UVInfo units.
 *                      - Capitalised various menu options.
 *                      - Added code to detect Vista and WinHelp, putting up a
 *                        one-time message if WinHelp not available.
 * v1.16 of 16 Jun 2008 - Modified to change task bar handling. Main form window
 *                        is now used by task bar instead of Application
 *                        object's hidden window. This change was required for
 *                        compatibility with Vista.
 *                      - Now uses v3.4 of About Box Component to enable about
 *                        box to work correctly with main form and task bar.
 *                      - Removed code that checks for Vista OS to new UOSInfo
 *                        unit.
 *
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
 * Portions created by the Initial Developer are Copyright (C) 1998-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK *****
}


unit FmMain;


interface


uses
  // Delphi
  Forms, StdCtrls, Dialogs, Menus, Classes, Controls, ExtCtrls, Windows,
  Messages,
  // DelphiDabbler components
  PJVersionInfo, PJAbout, PJDropFiles, PJWdwState,
  // Project
  UVInfo;


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
    MHOverview: TMenuItem;
    MHSpacer1: TMenuItem;
    MHWebsite: TMenuItem;
    MHSpacer2: TMenuItem;
    MHAbout: TMenuItem;
    DisplayHeader: THeader;
    DisplayListBox: TListBox;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    ExportDlg: TSaveDialog;
    AboutBoxDlg: TPJAboutBoxDlg;
    AboutVersionInfo: TPJVersionInfo;
    WdwState: TPJRegWdwState;
    MESpacer0: TMenuItem;
    MECompOut: TMenuItem;
    MFCompile: TMenuItem;
    MFSpacer1: TMenuItem;
    CompilerDlg: TSaveDialog;
    FileCatcher: TPJFormDropFiles;
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
    procedure MHOverviewClick(Sender: TObject);
    procedure MEVICommentsClick(Sender: TObject);
    procedure FileCatcherDropFiles(Sender: TObject);
    procedure WdwStateGetRegData(var RootKey: HKEY; var SubKey: String);
    procedure MFExportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MFileClick(Sender: TObject);
    procedure MFCompileClick(Sender: TObject);
    procedure MEditClick(Sender: TObject);
    procedure MECompOutFolderClick(Sender: TObject);
  private
    fVerInfo: TVInfo;
      {Instance of TVInfo class which formats, reads and saves version info}
    fCurrentFile: string;
      {Name of current file}
    fChanged: Boolean;
      {Flag true if current file has been altered since last load, save etc}
    procedure WMSyscommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
      {Handles system command messages. Overrides default processing of
      minimizing and restoration of main window. This is required now we have
      inhibited application object's default processing of these messages.
        @param Msg [in/out] Details of system command. Result field set to 0 if
          we handle message to prevent default processing.
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
  protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Updates style of window to ensure this main window appears on task bar.
        @params Params [in/out] In: current parameters. Out: adjusted
          parameters: we update ExStyle field with new window styles.
      }
  end;


var
  MainForm: TMainForm;


implementation


uses
  // Delphi
  SysUtils, ShellAPI,
  // Project units
  UHelp, UMsgDlgs, UOSInfo, UVerUtils, UUtils, USettings, UResCompiler,
  FmDropDownListEd, FmIdEd, FmNumberEd, FmResCompiler, FmResCompilerCheck,
  FmSetEd, FmStringEd, FmUserSetup, FmViewList, FmVerNumEd, FmResOutputDir;

{$R *.DFM}


const
  // File extensions
  cRCExt = '.rc';
  cResExt = '.res';
  cVIExt = '.vi';

resourcestring
  // Message dialog box messages
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
  sNoWinHelp = 'You are running on Windows Vista or later and WinHelp is not '
      + 'installed.'#10#10
      + 'This means that you will not be able to use the program''s help.'#10#10
      + 'Search for update KB917607 on MSDN or the Microsoft website to obtain '
      + 'a copy of WinHelp that works on Vista or Windows 2008 Server.'#10#10
      + 'This message will not be shown again.';

  // Dialog box titles
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

procedure TMainForm.CreateParams(var Params: TCreateParams);
  {Updates style of window to ensure this main window appears on task bar.
    @params Params [in/out] In: current parameters. Out: adjusted parameters:
      we update ExStyle field with new window styles.
  }
begin
  inherited;
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
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
        MsgFileAccessError(FName, HELP_MSGDLG_FILEACCESSERROR);
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
  fVerInfo.WriteToDisplay(DisplayListBox.Items);
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
        fVerInfo.WriteToDisplay(DisplayListBox.Items);
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
        MsgFileAccessError(FName, HELP_MSGDLG_FILEACCESSERROR);
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

procedure TMainForm.FileCatcherDropFiles(Sender: TObject);
  {Event handler for drop files component: opens the first file caught by the
  component.
    @param Sender [in] Not used.
  }
begin
  // Check if file can be loaded and succeeds and set open dlg to use path
  // next time it is opened
  if QueryFileSave and OpenFile(FileCatcher.FileName) then
    OpenDlg.InitialDir := ExtractFilePath(FileCatcher.FileName);
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

var
  I: Integer; // loop control
begin
  // Remove hidden application window from task bar: this form is now use on
  // task bar. This required so task bar button conforms to Vista requirements.
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(
    Application.Handle,
    GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE)
      and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW
  );
  ShowWindow(Application.Handle, SW_SHOW);
  // Ensure that application help file specifies full path
  // this makes sure file can be found from standard dialogs
  Application.HelpFile := ExtractFilePath(ParamStr(0))
    + ExtractFileName(Application.HelpFile);
  // Set size of header sections and set list box tab stop
  DisplayHeader.SectionWidth[0] := 140;
  DisplayListBox.TabWidth :=
    PixelsToDlgBaseUnits(DisplayHeader.SectionWidth[0]);
  // Deselect list box item
  DisplayListBox.ItemIndex := -1;
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
  // Set up help contexts
  // system dialogs
  SaveDlg.HelpContext :=              HELP_DLG_SAVEAS;
  ExportDlg.HelpContext :=            HELP_DLG_EXPORT;
  OpenDlg.HelpContext :=              HELP_DLG_OPEN;
  CompilerDlg.HelpContext :=          HELP_DLG_COMPILEFILE;
  AboutBoxDlg.HelpContext :=          HELP_DLG_ABOUT;
  // menu options
  MFile.HelpContext :=                HELP_MENU_FILE;
  MFNew.HelpContext :=                HELP_MENU_FILENEW;
  MFOpen.HelpContext :=               HELP_MENU_FILEOPEN;
  MFSave.HelpContext :=               HELP_MENU_FILESAVE;
  MFSaveAs.HelpContext :=             HELP_MENU_FILESAVEAS;
  MFExport.HelpContext :=             HELP_MENU_FILEEXPORT;
  MFPreferences.HelpContext :=        HELP_MENU_FILEPREFERENCES;
  MFViewRC.HelpContext :=             HELP_MENU_FILEVIEWRC;
  MFExit.HelpContext :=               HELP_MENU_FILEEXIT;
  MEdit.HelpContext :=                HELP_MENU_EDIT;
  MECurrent.HelpContext :=            HELP_MENU_EDITCURRENT;
  MERCIdentifier.HelpContext :=       HELP_MENU_EDITIDENTIFIER;
  MERCComments.HelpContext :=         HELP_MENU_EDITRCCOMMENTS;
  MEVIComments.HelpContext :=         HELP_MENU_EDITVICOMMENTS;
  MEAnalyse.HelpContext :=            HELP_MENU_EDITANALYSE;
  MECopy.HelpContext :=               HELP_MENU_EDITCOPY;
  MOptions.HelpContext :=             HELP_MENU_OPTIONS;
  MOAutoValidate.HelpContext :=       HELP_MENU_OPTIONSVALIDATION;
  MODescribeFileFlags.HelpContext :=  HELP_MENU_OPTIONSDESCRIBE;
  MOUserSetup.HelpContext :=          HELP_MENU_OPTIONSSETUP;
  MOResCompiler.HelpContext :=        HELP_MENU_OPTIONSRESCOMP;
  MHelp.HelpContext :=                HELP_MENU_HELP;
  MHContents.HelpContext :=           HELP_MENU_HELPCONTENTS;
  MHOverview.HelpContext :=           HELP_MENU_HELPOVERVIEW;
  MHWebsite.HelpContext :=            HELP_MENU_WEBSITE;
  MHAbout.HelpContext :=              HELP_MENU_HELPABOUT;
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
      OpenDlg.InitialDir := ParamStr(1);
      SaveDlg.InitialDir := ParamStr(1);
      ExportDlg.InitialDir := ParamStr(1);
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
  {Form destruction event handler
    @param Sender [in] Not used.
  }
begin
  // Dispose of owned objects
  fVerInfo.Free;
  // Free help system
  Application.HelpCommand(HELP_QUIT, 0);
end;

procedure TMainForm.FormShow(Sender: TObject);
  {Form show event handler: checks for presence of compiler and WinHelp.
    @param Sender [in] Not used.
  }

  function WindowsFolder: string;
    {Gets Windows folder.
      @return Full path to Windows folder.
    }
  begin
    SetLength(Result, MAX_PATH);
    SetLength(
      Result, GetWindowsDirectory(PChar(Result), MAX_PATH)
    );
  end;

  function IsWinHelpInstalled: Boolean;
    {Checks if WinHelp is installed.
      @return True if OS is earlier than Vista or if Vista or later and WinHelp
        program exists in Windows folder. False otherwise.
    }
  var
    WinHelpPath: string;  // full path to WinHelp
  begin
    if IsVistaOrLater then
    begin
      WinHelpPath := IncludeTrailingPathDelimiter(WindowsFolder)
        + 'winhlp32.exe';
      Result := FileExists(WinHelpPath);
    end
    else
      Result := True;
  end;

  function Is1stRun: Boolean;
    {Checks if this is first run of program.
      @return True if this is first run of program.
    }
  begin
    Result := not Settings.ReadBool(siHasRun);
  end;

  procedure RecordHasRun;
    {Record in registry that the program has been run.
    }
  begin
    Settings.WriteBool(siHasRun, True);
  end;

begin
  // Check if a resource compiler has been set up
  CheckCompiler;
  // If 1st run, Vista and WinHelp is not installed display a message to say so
  if Is1stRun and not IsWinHelpInstalled then
  begin
    MessageBeep(0);
    MessageDlg(sNoWinHelp, mtInformation, [mbOK], 0);
  end;
  RecordHasRun;
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
    Ed.HelpContext := HELP_DLG_EDITCONSTANT;
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
    Ed.HelpContext := HELP_DLG_EDITNUMBER;
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
    Ed.HelpContext := HELP_DLG_EDITSTRING;
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
    Ed.HelpContext := HELP_DLG_EDITBITSET;
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
    Ed.HelpContext := HELP_DLG_EDITVNUMBER;
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
        MsgNoAnalysisErrorsFound(HELP_MSGDLG_NOANALYSISERRORS)
      else
      begin
        // There were errors - display them
        // pass error list to dlg box
        DBox.List := EList;
        // set the dlg box's caption and description
        DBox.Title := sAnalysisErrTitle;
        DBox.Description := sAnalysisErrDesc;
        DBox.HelpContext := HELP_DLG_ANALYSIS;
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
const
  cStrInfo: array[12..23] of TStrInfo =
    (siComments, siCompanyName, siFileDescription, siFileVersion,
    siInternalName, siLegalCopyright, siLegalTrademarks, siOriginalFileName,
    siPrivateBuild, siProductName, siProductVersion, siSpecialBuild);
var
  StrList: TStringList;    // for holding drop down lists / list box items
  StrList2: TStringList;   // for holding list box items
  Index: Integer;          // for holding current item index of display list
  StrInfoId: TStrInfo;     // for holding string info id
begin
  // Create the string lists
  StrList2 := nil;
  StrList := TStringList.Create;
  try
    StrList2 := TStringList.Create;
    // Act according to selected item in list box
    Index := DisplayListBox.ItemIndex;
    case Index of
      0, 8, 11:  // Titles - error - can't be edited
        MsgCantEditTitle(
          DisplayListBox.Items[DisplayListBox.ItemIndex],
          HELP_MSGDLG_CANTEDITTITLE
        );
      1: // File version number
        fVerInfo.FileVersionNumber := GetVersionNumber(
          sFile, fVerInfo.FileVersionNumber
        );
      2: // Product version number
        fVerInfo.ProductVersionNumber := GetVersionNumber(
          sProduct, fVerInfo.ProductVersionNumber
        );
      3: // File OS
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
      4: // File Type
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
      5: // File sub-type
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
              MsgCantEditSubType(
                FileTypeToStr(fVerInfo.FileType), HELP_MSGDLG_CANTEDITSUBTYPE
              );
          end;
        except
          on E: EVersionError do
            {nothing};
        end;
      6:   // File flags mask
        fVerInfo.FileFlagsMask := StrListToFileFlagSet(
          GetStringList(
            sFileFlagsMask,
            FileFlagSetToStrList(fVerInfo.FileFlagsMask, StrList),
            FileFlagSetToStrList(not fVerInfo.FileFlagsMask, StrList2)
          )
        );
      7:   // File flags
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
      9:   // Language
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
      10:  // Character set
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
      12..23: // String Info items
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
          MsgNeedFileFlag(
            fVerInfo.StrDesc[StrInfoId], HELP_MSGDLG_FILEFLAGREQUIRED
          )
        else if MsgDeleteInvalidText(
          fVerInfo.StrDesc[StrInfoId], HELP_MSGDLG_NOTEXTALLOWED
        ) then
          // string not permitted, there is a value, user accepts deletion
          fVerInfo.StrInfo[StrInfoId] := '';
      end;
      -1: // No item selected - error
        MsgNoItemToEdit(HELP_MSGDLG_NOITEMTOEDIT);
    end;
    // Re-display and select current item agian
    fVerInfo.WriteToDisplay(DisplayListBox.Items);
    DisplayListBox.ItemIndex := Index;
  finally
    StrList2.Free;
    StrList.Free;
  end;
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
    Ed.HelpContext := HELP_DLG_EDITCOMMENTS;
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
    Ed.HelpContext := HELP_DLG_EDITIDENTIFIER;
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
    Ed.HelpContext := HELP_DLG_EDITCOMMENTS;
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
    if not FileExists(FileName)
        or MsgOKToOverwrite(FileName, HELP_MSGDLG_OVERWRITEFILE
      ) then
      DoExportRes(FileName);
  end
  else
  begin
    // No default output folder: get output file from user
    // display file dialog box
    if CompilerDlg.InitialDir = '' then
      CompilerDlg.InitialDir := ExportDlg.InitialDir;
    if CompilerDlg.Execute then
    begin
      CompilerDlg.FileName := EnsureExtension(
        CompilerDlg.FileName, CompilerDlg.Filter, CompilerDlg.FilterIndex
      );
      // check if file exists and OK to overwrite if so
      if not FileExists(CompilerDlg.FileName)
        or MsgOKToOverwrite(CompilerDlg.FileName, HELP_MSGDLG_OVERWRITEFILE
      ) then
      begin
        // compile the file
        DoExportRes(CompilerDlg.FileName);
        // record file's folder for future reference
        CompilerDlg.InitialDir := ExtractFilePath(CompilerDlg.FileName);
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
    Result := not FileExists(ExportDlg.FileName)
      or MsgOKToOverwrite(ExportDlg.FileName, HELP_MSGDLG_OVERWRITEFILE);
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
  ExportDlg.Filter := cRCFilter;
  if HaveCompiler then
    // we can write RES files if we have an associated compiler
    ExportDlg.Filter := ExportDlg.Filter + '|' + cResFilter;
  // Display Export dialog box and process selected file if user OKs
  if ExportDlg.Execute then
  begin
    // Ensure that file names without extension gets appropriate one
    ExportDlg.FileName := EnsureExtension(ExportDlg.FileName,
      ExportDlg.Filter, ExportDlg.FilterIndex);
    // Record file extension and use it to select how to export file
    Ext := ExtractFileExt(ExportDlg.FileName);
    if CompareText(Ext, cRCExt) = 0 then
    begin
      // Saving resource source file if possible
      if CanWrite then
      begin
        // Export a .rc file + remember dir used for next use of Save As
        DoExportRC(ExportDlg.FileName);
        ExportDlg.InitialDir := ExtractFilePath(ExportDlg.FileName);
      end;
    end
    else if CompareText(Ext, cResExt) = 0 then
    begin
      // Saving resource binary file if possible
      if CanWrite then
      begin
        // Export a .rc file + remember dir used for next use of Save As
        DoExportRes(ExportDlg.FileName);
        ExportDlg.InitialDir := ExtractFilePath(ExportDlg.FileName);
      end;
    end
    else
      // Can't cope with other extensions - refuse
      MsgInvalidExtension(ExportDlg.FileName, HELP_MSGDLG_BADEXTENSION);
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
  // Attempt to open file
  if QueryFileSave                        // check if we need to save prev file
    and OpenDlg.Execute                   // open file dlg: did user OK?
    and OpenFile(OpenDlg.FileName) then   // try to open file
      // File open succeeded: record path so open dlg uses it next time
      OpenDlg.InitialDir := ExtractFilePath(OpenDlg.FileName);
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
      DBox.HelpContext := HELP_DLG_VIEWRC;
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
  // Call WinHelp for contents topic in application's help file
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TMainForm.MHOverviewClick(Sender: TObject);
  {Help | Overview menu click event handler.
    @param Sender [in] Not used.
  }
begin
  // Call WinHelp for overview topic in application's help file
  Application.HelpContext(HELP_OVERVIEW);
end;

procedure TMainForm.MHWebsiteClick(Sender: TObject);
  {Help | Visit DelphiDabbler.com menu click event handler: accesses
  DelphiDabbler website.
    @param Sender [in] Not used.
  }
begin
  if ShellAPI.ShellExecute(Handle, nil, 'http://www.delphidabbler.com', nil,
    nil, SW_NORMAL) <= 32 then
    RaiseLastWin32Error();
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
  // {Update display to reflect new choice
  fVerInfo.WriteToDisplay(DisplayListBox.Items);
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
    // Set help context
    DBox.HelpContext := HELP_DLG_SETUP;
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
    MsgInvalidExtension(FileName, HELP_MSGDLG_BADEXTENSION);
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
    case MsgQuerySaveFile(fCurrentFile, HELP_MSGDLG_FILECHANGED) of
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
  SaveDlg.FileName := fCurrentFile;
  if SaveDlg.Execute then
  begin
    // Ensure that file names without extension gets appropriate one
    SaveDlg.FileName := EnsureExtension(SaveDlg.FileName,
      SaveDlg.Filter, SaveDlg.FilterIndex);
    // Test extension and act on result
    Ext := ExtractFileExt(SaveDlg.FileName);
    if (AnsiCompareText(Ext, cVIExt) = 0) then
    begin
      // Saving version information file
      if not FileExists(SaveDlg.FileName) or
        MsgOKToOverwrite(SaveDlg.FileName, HELP_MSGDLG_OVERWRITEFILE) then
      begin
        // Save a .vi file and set result to true: remember dir used for next
        // use of Save As
        DoSave(SaveDlg.FileName);
        SaveDlg.InitialDir := ExtractFilePath(SaveDlg.FileName);
        Result := True;
      end
    end
    else
      // Can't cope with other extensions - refuse
      MsgInvalidExtension(SaveDlg.FileName, HELP_MSGDLG_BADEXTENSION);
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

procedure TMainForm.WMSyscommand(var Msg: TWMSysCommand);
  {Handles system command messages. Overrides default processing of minimizing
  and restoration of main window. This is required now we have inhibited
  application object's default processing of these messages.
    @param Msg [in/out] Details of system command. Result field set to 0 if we
      handle message to prevent default processing.
  }
begin
  // Note: according to Win API low order four bits of Msg.CmdType are reserved
  // for use by windows. We therefore mask out those bytes before processing.
  case (Msg.CmdType and $FFF0) of
    SC_MINIMIZE:
    begin
      ShowWindow(Handle, SW_MINIMIZE);
      Msg.Result := 0;
    end;
    SC_RESTORE:
    begin
      ShowWindow(Handle, SW_RESTORE);
      Msg.Result := 0;
    end;
    else
      inherited;
  end;
end;

end.

