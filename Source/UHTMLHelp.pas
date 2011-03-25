{
 * UHTMLHelp.pas
 *
 * Provides a partial translation of HTMLHelp header files. Dynamically imports
 * HTMLHelp function from hhctrl.ocx.
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
 * The Original Code is UHTMLHelp.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLHelp;


interface


uses
  // Delphi
  SysUtils, Windows;


///  <summary>
///  Single function used to control HTML Help. Action varies according to the
///  specified command.
///  </summary>
///  <param name="hwndCaller">HWND [in] Handle of window calling HtmlHelp.
///  HtmlHelp returns focus to this window when closed, unless window is
///  desktop. If notifcations are enabled HtmlHelp sends notification messages
///  to this window.</param>
///  <param name="pszFile">LPCTSTR [in] Depending on uCommand specifies either a
///  .chm file or a topic file within a help file. May also specify a window.
///  </param>
///  <param name="uCommand">UINT [in] Specifies command to be executed.</param>
///  <param name="dwData">DWORD [in] Specifies any data that may be required,
///  based on the value of the uCommand parameter.</param>
///  <returns>HWND. Depending on uCommand and the result, either the handle of
///  the help window or 0 are returned. Zero can indicate either failure or that
///  the help window has not yet been created.</returns>
///  <remarks>An EHtmlHelp exception is raised if the hhctrl.ocx library is not
///  available.</remarks>
function HtmlHelp(hwndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT;
  dwData: DWORD): HWND; stdcall;


const
  // Commands for HtmlHelp API function

  ///  Opens a help topic in a specified help window
  HH_DISPLAY_TOPIC           = $0000;
  ///  Alias for HH_DISPLAY_TOPIC
  HH_HELP_FINDER             = HH_DISPLAY_TOPIC;
  ///  Selects the Contents tab in the Navigation pane of the HTML Help Viewer
  HH_DISPLAY_TOC             = $0001;
  ///  Selects the Index tab in the Navigation pane of the HTML Help Viewer and
  ///  searches for the keyword specified in the dwData parameter
  HH_DISPLAY_INDEX           = $0002;
  ///  Selects the Search tab in the Navigation pane of the HTML Help Viewer
  HH_DISPLAY_SEARCH          = $0003;
  ///  Creates a new help window or modifies an existing help window at run time
  HH_SET_WIN_TYPE            = $0004;
  ///  Retrieves a pointer to the HH_WINTYPE structure associated with a
  ///  specified window type
  HH_GET_WIN_TYPE            = $0005;
  ///  Returns the handle of a specified window type
  HH_GET_WIN_HANDLE          = $0006;
  ///  No documentation in htmlhelp.chm
  HH_ENUM_INFO_TYPE          = $0007;
  ///  No documentation in htmlhelp.chm
  HH_SET_INFO_TYPE           = $0008;
  ///  Locates and selects the contents entry for the help topic that is open in
  ///  the Topic pane of the HTML Help Viewer
  HH_SYNC                    = $0009;
  ///  Reserved for future use
  HH_RESERVED1               = $000A;
  ///  Reserved for future use
  HH_RESERVED2               = $000B;
  ///  Reserved for future use
  HH_RESERVED3               = $000C;
  ///  Looks up one or more keywords in a compiled help file
  HH_KEYWORD_LOOKUP          = $000D;
  ///  Opens a pop-up window that displays the contents of one of the following:
  ///  an explicit text string; a text string based on a resource ID; a text
  ///  string ID based on a text file contained in a compiled help file
  HH_DISPLAY_TEXT_POPUP      = $000E;
  ///  Displays a help topic based on a mapped topic ID
  HH_HELP_CONTEXT            = $000F;
  ///  Opens a pop-up context menu. Generally used in response to the Windows
  ///  WM_CONTEXTMENU message. For example, this message is sent when a user
  ///  right-clicks a dialog box control
  HH_TP_HELP_CONTEXTMENU     = $0010;
  ///  Opens a pop-up help topic. Generally used in response to the Windows
  ///  WM_HELP message. For example, this message is sent when a user presses F1
  HH_TP_HELP_WM_HELP         = $0011;
  ///  Closes all windows opened directly or indirectly by the calling program
  HH_CLOSE_ALL               = $0012;
  ///  Looks up one or more Associative Link (ALink) names in compiled help file
  HH_ALINK_LOOKUP            = $0013;
  ///  Returns information about the last error that occurred in hhctrl.ocx
  HH_GET_LAST_ERROR          = $0014;
  ///  No documentation in htmlhelp.chm
  HH_ENUM_CATEGORY           = $0015;
  ///  No documentation in htmlhelp.chm
  HH_ENUM_CATEGORY_IT        = $0016;
  ///  No documentation in htmlhelp.chm
  HH_RESET_IT_FILTER         = $0017;
  ///  No documentation in htmlhelp.chm
  HH_SET_INCLUSIVE_FILTER    = $0018;
  ///  No documentation in htmlhelp.chm
  HH_SET_EXCLUSIVE_FILTER    = $0019;
  ///  Initializes the help system for use and must be the first HTML Help
  ///  command called. It returns a cookie which must be used in the
  ///  HH_UNINITIALIZE call
  HH_INITIALIZE              = $001C;
  ///  Called to properly shut down HTML Help. This function should be the last
  ///  help command the application calls. HH_UNINITIALIZE should not be called
  ///  during DLL process detach, but during the normal application shutdown
  ///  process
  HH_UNINITIALIZE            = $001D;
  ///  Called in a Window application's message loop to ensure proper handling
  ///  of Windows messages, especially keyboard messages when running HTML Help
  ///  single thread
  HH_PRETRANSLATEMESSAGE     = $00FD;
  ///  No documentation in htmlhelp.chm
  HH_SET_GLOBAL_PROPERTY     = $00FC;


type
  ///  <summary>
  ///  Structure used to specify one or more ALink names or KLink keywords to be
  ///  searched for.
  ///  </summary>
  THHAKLink = packed record
    cbStruct: Integer;    // sizeof this structure
    fReserved: BOOL;      // must be FALSE (really!)
    pszKeywords: LPCTSTR; // semi-colon separated keywords
    pszUrl: LPCTSTR;      // URL to jump to if no keywords found (may be nil)
    pszMsgText: LPCTSTR;  // MessageBox text on failure (used if pszUrl nil)
    pszMsgTitle: LPCTSTR; // Title of any failure MessageBox
    pszWindow: LPCTSTR;   // Window to display pszURL in
    fIndexOnFail: BOOL;   // Displays index if keyword lookup fails.
  end;
  PHHAKLink = ^THHAKLink;

type
  ///  <summary>
  ///  Exception raised if the HTML help control can't be loaded.
  ///  </summary>
  EHtmlHelp = class(Exception);


implementation


type
  ///  <summary>
  ///  Prototype for HtmlHelpA API function.
  ///  </summary>
  THtmlHelp = function (hwndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT;
    dwData: DWORD): HWND; stdcall;


var
  ///  Handle to hhctrl.ocx library
  pvtHHCtrl: THandle = 0;
  ///  Pointer to HtmlHelpA API function
  pvtHtmlHelp: THtmlHelp = nil;


resourcestring
  // Error message used for exception raised when hhctrl.ocx not found
  sNoHelp = 'Sorry, help is not available. '#13#10#13#10
    + 'This is because the HTML Help library (hhctrl.ocx) cannot be found.';


function HtmlHelp(hwndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT;
  dwData: DWORD): HWND; stdcall;
begin
  if not Assigned(pvtHtmlHelp) then
    raise EHtmlHelp.Create(sNoHelp);
  Result := pvtHtmlHelp(hwndCaller, pszFile, uCommand, dwData);
end;

///  <summary>Attempt to load HtmlHelpW function from hhctrl.ocx.</summary>
procedure LoadHtmlHelp;
begin
  pvtHHCtrl := LoadLibrary('hhctrl.ocx');
  if pvtHHCtrl <> 0 then
    pvtHtmlHelp := GetProcAddress(pvtHHCtrl, 'HtmlHelpW');
end;

initialization

LoadHtmlHelp;

finalization

if pvtHHCtrl <> 0 then
  FreeLibrary(pvtHHCtrl);

end.

