{
 * VIEd.dpr
 *
 * Version Information Editor project file.
 *
 * v1.0 of 25 May 1998  - Original version. Named VInfoExp.dpr.
 * v1.1 of 13 Apr 1999  - Amended copyright notice to prevent modification of
 *                        source and to update copyright date to 1998-1999.
 *                      - Made declaration of MsgDlgGlyphs "constant" only occur
 *                        in 16 bit compilations - "constant" not defined or
 *                        used in Delphi 2.
 *                      - Ensured appropriate resource file was included
 *                        depending on whether compiling for 16bit or 32bit
 *                        Windows. Referenced new resource files in new
 *                        locations.
 * v1.2 of 17 Mar 2002  - Removed all conditional compilation directives and 16
 *                        bit-only code. Program is now 32 bit only.
 *                        Removed path from included resource file.
 * v1.3 of 17 Mar 2002  - Name of Main unit changed to FmMain.
 * v1.4 of 18 Mar 2002  - Added new units: FmGenericDlg, FmGenericOKDlg and
 *                        FmGenericViewDlg.
 *                      - Renamed the following units: EdDropDn as
 *                        FmDropDownListEd, EdId as FmIdEd, EdNum as FmNumberEd,
 *                        EdSet as FmSetEd, EdString as FmStringEd, EdVerNum as
 *                        FmVerNumEd, UsrSetup as FmUserSetup and ViewList as
 *                        FmViewList.
 * v1.5 of 18 Mar 2002  - Renamed Setup unit as USettings.
 * v1.6 of 05 May 2002  - Added UResCompiler, FmResCompiler and
 *                        FmResCompilerCheck units.
 * v1.7 of 06 May 2002  - Removed VerTypes unit.
 * v1.8 of 17 Mar 2003  - Changed file name from VInfoExp.dpr to VIEd.dpr.
 *                      - Added UHelp unit.
 *                      - Changed program title from "Version Info Expert" to
 *                        "Version Information Editor".
 *                      - Changed help file name from VInfoExp.hlp to VIEd.hlp.
 * v1.9 of 18 Nov 2003  - Added new FmResOutputDlg form and unit.
 *                      - Renamed from VInfoExp.dpr to VIEd.dpr.
 * v1.10 of 20 Apr 2008 - Removed inclusion on VIEd.res, changed Resource.res to
 *                        Version.res and added Resources.res.
 * v1.11 of 16 Jun 2008 - Added new UAltBugFix, UDlgParent and UOSInfo units.
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
 * The Original Code is VIEd.dpr.
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


program VIEd;


uses
  Forms,
  FmMain in 'FmMain.pas' {MainForm},
  FmGenericDlg in 'FmGenericDlg.pas' {GenericDlg},
  FmGenericOKDlg in 'FmGenericOKDlg.pas' {GenericOKDlg},
  FmGenericViewDlg in 'FmGenericViewDlg.pas' {GenericViewDlg},
  FmDropDownListEd in 'FmDropDownListEd.pas' {DropDownListEditor},
  FmIdEd in 'FmIdEd.pas' {IdEditor},
  FmNumberEd in 'FmNumberEd.pas' {NumEditor},
  FmResCompiler in 'FmResCompiler.pas' {ResCompilerDlg},
  FmResCompilerCheck in 'FmResCompilerCheck.pas' {ResCompilerCheckDlg},
  FmSetEd in 'FmSetEd.pas' {SetEditor},
  FmStringEd in 'FmStringEd.pas' {StringEditor},
  FmUserSetup in 'FmUserSetup.pas' {UserSetupDlg},
  FmViewList in 'FmViewList.pas' {ViewListDlg},
  FmVerNumEd in 'FmVerNumEd.pas' {VerNumEditor},
  FmResOutputDir in 'FmResOutputDir.pas' {ResOutputDirDlg},
  UAltBugFix in 'UAltBugFix.pas',
  UDlgParent in 'UDlgParent.pas',
  UHelp in 'UHelp.pas',
  UMsgDlgs in 'UMsgDlgs.pas',
  UOSInfo in 'UOSInfo.pas',
  UResCompiler in 'UResCompiler.pas',
  USettings in 'USettings.pas',
  UUtils in 'UUtils.pas',
  UVerUtils in 'UVerUtils.pas',
  UVInfo in 'UVInfo.pas';

{$Resource Resources.res}
{$Resource Version.res}


begin
  Application.Title := 'Version Information Editor';
  Application.HelpFile := 'VIEd.hlp';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

