{
 * UHTMLHelp.pas
 *
 * Provides a minimal translation of HTMLHelp header files sufficient for the
 * needs of Version Information Editor. Dynamically imports HTMLHelp function
 * from hhctrl.ocx.
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
  ///  Selects the Contents tab in the Navigation pane of the HTML Help Viewer
  HH_DISPLAY_TOC             = $0001;
  ///  Closes all windows opened directly or indirectly by the calling program
  HH_CLOSE_ALL               = $0012;

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

