Change Log for Version Information Editor
-----------------------------------------

v2.13.1 of 14 October 2014

+ Fixed bug [GitHub issue 21] where symbol used to indicate a hex value in main window was inconsistent: the Pascal '$' symbol is now always used in preference to the C style '0x'.
+ Documentation changes:
  - Added source code section to read-me file [GitHub issue 19]
  - Corrected error in change log [GitHub issue 20].

v2.13.0 of 23 February 2014

+ Gave GUI, especially main window, a refresh with a new font and a more attractive way of displaying version information [GitHub issue 12].
+ New commands:
  - User preferences can now be deleted via the new "File | Clear Preferences" menu option [GitHub issue 2].
  - The selected version information item can now be deleted or cleared to its default value by pressing Ctrl+Del or choosing the new "Edit | Clear Current Item" menu item [GitHub issue 3].
+ Made various improvements to dialogue boxes:
  - Increased size of dialogues used to display generated RC source and analysis errors [GitHub issue 7].
  - Changed comments editor and string editor dialogues to be resizeable and to display the position of the edit control's caret [GitHub issues 5 & 6].
  - Version number edit dialogue now has "+1" buttons to quickly increment each part of the version number [GitHub issue 4].
+ File Flag Masks now defaults to zero instead of $3F for new documents. Attempting to edit File Flags when File Flag Mask is zero now results in an error message being displayed.
+ Fix Bug [GitHub issue 14]: Insufficient memory allocated for window class names in dialogue boxes.
+ Program now requires Windows XP as a minimum operating system.
+ The executable program and original source code are now licensed under the Mozilla Public Licence v2.0. The EULA distributed with the program has been re-written as a result.
+ Updated help topics re changes [GitHub issue 17].
+ Updated documentation re changes.

v2.12.0 of 28 March 2011

+ Completely rewrote help system:
  - Changed from WinHelp to HTML Help.
  - Removed context sensitive help for menu options.
  - Help menu revised to access relevant topics in new help file.
  - F1 rather than Ctrl+F1 now displays help contents.
+ Modified to compile cleanly with Delphi 2010 compiler.
+ Handling of common dialogue boxes changed to centre them over main window and to work with HTML help.
+ Stripped out Alt key bug fix and work around for Vista UI problems that were added in v2.11.2 as these are handled correctly by Delphi 2010.
+ Changed window class names of main window and some dialogue boxes.
+ Refactored some code.
+ All new installer. This is now a GUI based install wizard generated by Inno Setup.
+ Created new makefile to perform all compilation functions and removed old build scripts.
+ Program now requires Windows 2000 as the minimum operating system and will no longer run on the Windows 9x platform.
+ Updated documentation.

v2.11.2 of 16 June 2008

+ Changed to make application minimisation, task bar preview window, and appearance in "Flip 3D" task switching display correctly on Windows Vista. This required an update to DelphiDabbler About Box Component v3.4.
+ Provided work-around for Delphi's Alt key bug on XP and Vista (CodeGear Quality Central bug report #374030).

v2.11.1 of 30 April 2008

+ Added manifest that enables XP and Vista themes and informs Vista that program can be run with user privileges.
+ Changed to use user's application data folder for preferences instead of program installation folder.
+ Changed text of some dialogue boxes.
+ Prevented program from writing test files to check if a drive is available. This code fails in protected folders on Vista.
+ Added one-time message that informs Vista users if WinHelp is not available.
+ Moved some string literals to resources to make them easier to translate.
+ Some refactoring and unit renaming.
+ Changed installer to display license and to permit installation on Vista.

v2.11.0 of 20 March 2005

+ Added facility to call program with -makerc switch and a .vi file on command line. Program creates .rc file from the .vi file and then terminates without displaying a window. This was added to enable the program to be used in an automated build process.
+ Added new topic to help file covering command line options.
+ Changed to Mozilla Public License.

v2.10.1 of 4 December 2003

+ Save dialogue box now appends default file extension when none is specified by the user.
+ Made default command line for BRCC32 compiler enclose file parameters in quotes to prevent compiler from failing when file names contain spaces.
+ Revised help file to advise users to enclose compiler file parameters in quotes when specifying resource compiler.

v2.10.0 of 18 November 2003

+ Added new facility to directly compile a .vi file to a predetermined output folder, details of which are stored in the .vi file, providing that an external compiler is configured. This was done by:
  - adding a new File | Compile menu option with hot key F9;
  - adding a new Edit | Compile Output Folder menu option to enable compiler output folder to be edited.
+ The help file was updated to cover the new features.

v2.9.1 of 25 August 2003

+ Fixed bug that was causing a prompt for details of a resource compiler to be displayed at start-up when user had requested the prompt should not appear.

v2.9.0 of 17 March 2003

+ Renamed from "Version Info Expert" to "Version Information Editor".
+ Added new Help menu option to access delphiDabbler website.
+ Changed to use registry key used for persistent settings from HKLM\Software\DelphiDabbler\VIEd to HKLM\Software\PJSoft\VInfoExp.

v2.8.2 of 06 May 2002 (Unreleased)

+ Revised dialogue box that is used to edit file flags. It now uses a checked list box rather than twin include/exclude lists.

v2.8.1 of 06 May 2002 (Unreleased)

+ Replaced TVersionNumber type with one from PJVersionInfo unit.
+ Deleted some redundant code.
+ Deleted redundant VerTypes unit.

v2.8.0 of 05 May 2002 (Unreleased)

+ Added ability to export binary resource files by using an external resource compiler.

v2.7.0 of 18 March 2002 (Unreleased)

+ Main window is now animated on minimisation.
+ Position, size and state of main window is now saved in registry between executions.
+ User defined settings are now stored in registry.
+ About box is now displayed relative to main window.

v2.6.0 of 18 March 2002 (Unreleased)

+ Updated appearance of main window and dialogue boxes to have standard 32 bit windows look and feel. Dialogue boxes now descend from common classes that handle positioning and manage help file access.

v2.5.0 of 17 March 2002 (Unreleased)

+ Added facility to open .vi files by dragging and dropping from Explorer.

v2.4.0 of 17 March 2002 (Unreleased)

+ Revised to compile with Delphi 4 and current component library.
+ Program is now 32 bit only - all 16 bit specific conditionally compiled code has been removed.
+ WinVer.inc was removed since relevant constants are in Delphi 4 Windows unit.
+ 16 bit version information resource has been removed.

v2.3.0 of 25 April 1999 (Unreleased)

+ Modified way string editor displays depending on what it's editing.
+ Removed bug which was indenting all VI file comments by one additional space.

v2.2.0 of 24 April 1999 (Unreleased)

+ Comments for .rc and .vi files can now exceed 255 characters.
+ Indentation in comments is now preserved when re-loaded.

v2.1.0 of 15 April 1999 (Unreleased)

+ Fixed bug where 32bit version of program was treating ProductVersion resource string as not being permitted whenever it was supplied.

v2.0.0 of 13 April 1999 (Unreleased)

+ Converted program to allow compilation as 16 bit and 32 bit programs, using Delphi 1 and Delphi 2 respectively.
+ Moved code previously called from StrProcs.pas library unit into main project.
+ Added an include file to provide information from 32bit Windows WinVer.h header that is not provided with Delphi 2.

v1.1.0 of 07 July 1998 (Unreleased)

+ Made Open and Save As dialogue boxes remember last used directory and to use any directory passed as first parameter on command line as the current default directory on start-up.

v1.0.0 of 25 May 1998 (Unreleased)

+ Original version for Windows 3.1 compiled using Delphi 1.