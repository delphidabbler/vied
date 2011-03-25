{
 * VIEd.dpr
 *
 * Version Information Editor project file.
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
 * The Original Code is VIEd.dpr.
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
  UDlgParent in 'UDlgParent.pas',
  UHelp in 'UHelp.pas',
  UHTMLHelp in 'UHTMLHelp.pas',
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
  Application.MainFormOnTaskBar := True;
  Application.ModalPopupMode := pmAuto;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

