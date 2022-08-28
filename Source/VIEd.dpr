{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2022, Peter Johnson (www.delphidabbler.com).
 *
 * Version Information Editor project file.
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
  FmResOutputDir in 'FmResOutputDir.pas' {ResOutputDirDlg},
  FmSetEd in 'FmSetEd.pas' {SetEditor},
  FmStringEd in 'FmStringEd.pas' {StringEditor},
  FmUserSetup in 'FmUserSetup.pas' {UserSetupDlg},
  FmViewList in 'FmViewList.pas' {ViewListDlg},
  FmVerNumEd in 'FmVerNumEd.pas' {VerNumEditor},
  UCommonDlg in 'UCommonDlg.pas',
  UDlgParent in 'UDlgParent.pas',
  UHelp in 'UHelp.pas',
  UMemoCaretPosDisplayMgr in 'UMemoCaretPosDisplayMgr.pas',
  UMsgDlgs in 'UMsgDlgs.pas',
  UResCompiler in 'UResCompiler.pas',
  USettings in 'USettings.pas',
  UUtils in 'UUtils.pas',
  UVerUtils in 'UVerUtils.pas',
  UVInfo in 'UVInfo.pas',
  FmMacroEd in 'FmMacroEd.pas' {MacroEditor};

{$Resource Resources.res}
{$Resource Version.res}


begin
  Application.Title := 'Version Information Editor';
  Application.MainFormOnTaskBar := True;
  Application.ModalPopupMode := pmAuto;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

