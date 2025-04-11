{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022-2024, Peter Johnson (https://delphidabbler.com).
 *
 * Macro editor dialogue box.
}


unit FmMacroEd;

interface

uses
  // VCL
  FmGenericOKDlg, ImgList, Controls, ExtCtrls, StdCtrls, ComCtrls,
  Classes, Buttons,
  // Project
  UMacros,
  UVInfo;

type
  TMacroEditor = class(TGenericOKDlg)
    lvMacros: TListView;
    cbCmd: TComboBox;
    edName: TEdit;
    ilMacroEditor: TImageList;
    btnAddUpdate: TButton;
    btnDelete: TButton;
    lblMacros: TLabel;
    lblCmd: TLabel;
    lblName: TLabel;
    lblValue: TLabel;
    lblInstructions: TLabel;
    btnValue: TBitBtn;
    edValue: TEdit;
    btnCheckFile: TButton;
    procedure cbCmdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure lvMacrosSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnAddUpdateClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnValueClick(Sender: TObject);
    procedure btnCheckFileClick(Sender: TObject);
    procedure edValueChange(Sender: TObject);
  strict private
    var
      fMacroDefinitions: TArray<TMacros.TMacroDefinition>;
      fRelativeFilePath: string;
      fCurrentName: string;
    function GetMacros: TArray<TMacros.TMacroDefinition>;
    procedure SetMacros(const AValue: TArray<TMacros.TMacroDefinition>);
    procedure ClearEditCtrls;
    procedure UpdateButtons;
    procedure DisplayMacros;
    procedure UpdateMacros(SelIdx: Integer);
    procedure GetMacroDetailsFromCtrls(out Cmd, Name, Value: string);
    function GetMacroNameFromCtrl: string;
    procedure AddMacroFromCtrls;
    procedure UpdateMacroFromCtrls;
    procedure DeleteMacroFromCtrls;
    procedure SelectMacroCmd(const CmdStr: string);
    function IndexOfMacroName(const Name: string): Integer;
    procedure CopySelectedMacroToEditCtrls;
    function NormaliseFileName(const FileName: string): string;
    procedure DeSelectItem;
  public
    property Macros: TArray<TMacros.TMacroDefinition>
      read GetMacros write SetMacros;
    property RelativeFilePath: string
      read fRelativeFilePath write fRelativeFilePath;
  end;

implementation

// ** NOTE **
// Macro names have to be globally unique, not just within their command type

uses
  // VCL
  SysUtils, IOUtils, Dialogs,
  // Project
  FmViewList, UCommonDlg, UMsgDlgs, UUtils;

{$R *.dfm}

resourcestring
  sAddBtnCaption = '&Add';
  sUpdateBtnCaption = '&Update';
  sFileEditCaption = '&File:';
  sValueEditCaption = '&Value:';
  sEnvEditCaption = 'Al&ias for:';
  sBadFileBtnCaption = 'No such file';
  sGoodFileBtnCaption = 'Vie&w File...';

const
  // ** Do not localise
  BugInvalidCmd = 'Bug: Invalid command "%s"';
  BugDelete = 'Bug: can''t delete macro name "%s" - can''t find it';
  BugBadCmdType = 'Bug: Unknown macro command type: "%s"';
  BugUpdate = 'Bug: can''t update macro name "%s" - can''t find it';

procedure TMacroEditor.AddMacroFromCtrls;
var
  Cmd, Name, Value: string;
  Idx: Integer;
begin
  GetMacroDetailsFromCtrls(Cmd, Name, Value);
  SetLength(fMacroDefinitions, Length(fMacroDefinitions) + 1);
  Idx := High(fMacroDefinitions);
  if not TMacros.TryLookupMacroCmd(Cmd, fMacroDefinitions[Idx].Cmd) then
    raise Exception.CreateFmt(BugInvalidCmd, [Cmd]);
  fMacroDefinitions[Idx].Name := Name;
  fMacroDefinitions[Idx].Value := Value;
  UpdateMacros(Idx);
end;

procedure TMacroEditor.btnAddUpdateClick(Sender: TObject);
begin
  if btnAddUpdate.Caption = sAddBtnCaption then
    AddMacroFromCtrls
  else
    UpdateMacroFromCtrls;
  DeSelectItem;
end;

procedure TMacroEditor.btnCheckFileClick(Sender: TObject);
var
  FileName: string;
  Bytes: TBytes;
  Encoding: TEncoding;
  Lines: TStringList;
  Dlg: TViewListDlg;
resourcestring
  sBadFile = 'File "%s" does not exist';
  sViewDlgCaption = 'Macro File Viewer';
begin
  begin
    FileName := NormaliseFileName(Trim(edValue.Text));
    if TFile.Exists(FileName) then
    begin
      Bytes := TFile.ReadAllBytes(FileName);
      Encoding := nil;
      TEncoding.GetBufferEncoding(Bytes, Encoding, TEncoding.UTF8);
      Lines := TStringList.Create;
      try
        Lines.Text := Encoding.GetString(Bytes);
        Dlg := TViewListDlg.Create(Self);
        try
          Dlg.List := Lines;
          Dlg.Title := sViewDlgCaption;
          Dlg.ShowModal;
        finally
          Dlg.Free;
        end;
      finally
        Lines.Free;
      end;
    end
    else
      Display(Format(sBadFile, [FileName]), mtError, [mbOK]);
  end;
end;

procedure TMacroEditor.btnDeleteClick(Sender: TObject);
begin
  DeleteMacroFromCtrls;
  DeSelectItem;
end;

procedure TMacroEditor.btnValueClick(Sender: TObject);
var
  OpenDlg: TOpenDialogEx;
begin
  OpenDlg := TOpenDialogEx.Create(Self);
  try
    OpenDlg.DefaultExt := '';
    OpenDlg.Filter := '*';
    OpenDlg.HelpTopic := '';  // No help topic
    if OpenDlg.Execute then
    begin
      edValue.Text := OpenDlg.FileName;
    end;
  finally
    OpenDlg.Free;
  end;
end;

procedure TMacroEditor.cbCmdChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMacroEditor.ClearEditCtrls;
begin
  edName.Text := '';
  edValue.Text := '';
  cbCmd.ItemIndex := -1;
end;

procedure TMacroEditor.CopySelectedMacroToEditCtrls;
var
  LI: TListItem;
begin
  LI := lvMacros.Selected;
  Assert(Assigned(LI),
    'TMacroEditor.CopySelectedMacroToEditCtrls: lvMacros.Selected = nil');
  if not Assigned(LI) then
    Exit;
  // Copy macro details into edit controls
  SelectMacroCmd(LI.Caption);
  edName.Text := fMacroDefinitions[LI.Index].Name;
  edValue.Text := fMacroDefinitions[LI.Index].Value;
end;

procedure TMacroEditor.DeleteMacroFromCtrls;
var
  Name: string;
  Idx: Integer;
  DelIdx: Integer;
begin
  Name := Trim(edName.Text);
  DelIdx := IndexOfMacroName(Name);
  if DelIdx = -1 then
    raise Exception.CreateFmt(BugDelete, [Name]);
  for Idx := DelIdx to Pred(High(fMacroDefinitions)) do
    fMacroDefinitions[Idx] := fMacroDefinitions[Idx + 1];
  SetLength(fMacroDefinitions, Length(fMacroDefinitions) - 1);
  if DelIdx > High(fMacroDefinitions) then
    Dec(DelIdx);
  UpdateMacros(DelIdx);
end;

procedure TMacroEditor.DeSelectItem;
begin
  lvMacros.ItemIndex := -1;
  ClearEditCtrls;
end;

procedure TMacroEditor.DisplayMacros;
var
  Macro: TMacros.TMacroDefinition;
  LI: TListItem;
begin
  lvMacros.Items.BeginUpdate;
  try
    lvMacros.Clear;
    for Macro in fMacroDefinitions do
    begin
      LI := lvMacros.Items.Add;
      LI.Caption := TMacros.MacroCmds[Macro.Cmd];
      LI.SubItems.Add(Macro.Name);
      LI.SubItems.Add(Macro.Value);
    end;
    if lvMacros.Items.Count > 0 then
    begin
      lvMacros.ItemIndex := 0;
    end
    else
      DeSelectItem;
  finally
    lvMacros.Items.EndUpdate;
  end;
  UpdateButtons;
end;

procedure TMacroEditor.edNameChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMacroEditor.edValueChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMacroEditor.FormCreate(Sender: TObject);
var
  CmdName: string;
begin
  inherited;
  HelpTopic := 'dlg-macros';
  for CmdName in TMacros.MacroCmds do
    cbCmd.Items.Add(CmdName);
end;

procedure TMacroEditor.lvMacrosSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Assigned(lvMacros.Selected) then
  begin
    ClearEditCtrls;
    Exit;
  end;
  CopySelectedMacroToEditCtrls;
  UpdateButtons;
end;

function TMacroEditor.NormaliseFileName(const FileName: string): string;
begin
  Result := FileName;
  if Result = '' then
    Exit('');
  if not TPath.IsPathRooted(Result) then
    // File not rooted: must be relative to ini file being read
    Result := TPath.Combine(fRelativeFilePath, Result);
end;

procedure TMacroEditor.GetMacroDetailsFromCtrls(out Cmd, Name, Value: string);
begin
  // Get current values in from editing controls
  if cbCmd.ItemIndex >= 0 then
    Cmd := cbCmd.Items[cbCmd.ItemIndex]
  else
    Cmd := '';
  Name := GetMacroNameFromCtrl;
  Value := Trim(edValue.Text);
  fCurrentName := Name;
end;

function TMacroEditor.GetMacroNameFromCtrl: string;
begin
  Result := Trim(edName.Text);
end;

function TMacroEditor.GetMacros: TArray<TMacros.TMacroDefinition>;
begin
  Result := fMacroDefinitions;
end;

function TMacroEditor.IndexOfMacroName(const Name: string): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := Low(fMacroDefinitions) to High(fMacroDefinitions) do
    if SameText(Name, fMacroDefinitions[Idx].Name) then
      Exit(Idx);
end;

procedure TMacroEditor.SelectMacroCmd(const CmdStr: string);
var
  Idx: Integer;
begin
  cbCmd.ItemIndex := -1;
  for Idx := 0 to Pred(cbCmd.Items.Count) do
  begin
    if SameText(CmdStr, cbCmd.Items[Idx]) then
    begin
      cbCmd.ItemIndex := Idx;
      Exit;
    end;
  end;
end;

procedure TMacroEditor.SetMacros(
  const AValue: TArray<TMacros.TMacroDefinition>);
var
  Idx: Integer;
begin
  SetLength(fMacroDefinitions, Length(AValue));
  for Idx := Low(AValue) to High(AValue) do
    fMacroDefinitions[Idx] := AValue[Idx];
  DisplayMacros;
  UpdateButtons;
end;

procedure TMacroEditor.UpdateButtons;
var
  EditCmd: string;
  EditCmdCode: TMacros.TMacroCmd;
  EditName: string;
  EditValue: string;
  EditNameLVIdx: Integer;
  SelLI: TListItem;
  SelCmd: string;
  SelName: string;
  SelValue: string;
  CanAdd: Boolean;
  CanUpdate: Boolean;
  CanDelete: Boolean;
begin
  // Get current values in from editing controls
  GetMacroDetailsFromCtrls(EditCmd, EditName, EditValue);
  EditNameLVIdx := IndexOfMacroName(EditName);

  // Get value for edited name from list view
  if EditNameLVIdx >= 0 then
  begin
    SelLI := lvMacros.Items[EditNameLVIdx];
    SelCmd := SelLI.Caption;
    SelName := SelLI.SubItems[0];
    SelValue := SelLI.SubItems[1];
  end
  else
  begin
    SelCmd := '';
    SelName := '';
    SelValue := '';
  end;

  CanUpdate := (EditNameLVIdx >= 0) and TMacros.IsValidMacroName(EditName)
    and ((EditValue <> SelValue) or (EditCmd <> SelCmd));
  CanAdd := (EditNameLVIdx = -1) and TMacros.IsValidMacroName(EditName);
  CanDelete := (EditNameLVIdx >= 0);
  if TMacros.TryLookupMacroCmd(EditCmd, EditCmdCode)
    and (EditCmdCode in [mcExternal, mcImport])
    and (EditValue = '') then
  begin
    CanAdd := False;
    CanUpdate := False;
  end;

  // Add / Update button
  btnAddUpdate.Enabled := CanAdd or CanUpdate;
  if CanAdd then
    btnAddUpdate.Caption := sAddBtnCaption
  else
    btnAddUpdate.Caption := sUpdateBtnCaption;

  // Delete Button
  btnDelete.Enabled := CanDelete;

  // Validate file name button
  if not TMacros.TryLookupMacroCmd(EditCmd, EditCmdCode) then
  begin
    btnCheckFile.Visible := False;
    btnValue.Enabled := False;
    lblValue.Caption := sValueEditCaption;
  end
  else
  begin
    // Show check file button if macro references a file
    btnCheckFile.Visible := EditCmdCode in [mcExternal, mcImport];
    btnValue.Enabled := btnCheckFile.Visible;
    if btnCheckFile.Visible then
      btnCheckFile.Enabled := TFile.Exists(NormaliseFileName(EditValue));
    if btnCheckFile.Enabled then
      btnCheckFile.Caption := sGoodFileBtnCaption
    else
      btnCheckFile.Caption := sBadFileBtnCaption;
    case EditCmdCode of
      mcDefine:
        lblValue.Caption := sValueEditCaption;
      mcExternal, mcImport:
        lblValue.Caption := sFileEditCaption;
      mcEnv:
        lblValue.Caption := sEnvEditCaption;
    end;
  end;
end;

procedure TMacroEditor.UpdateMacroFromCtrls;
var
  Cmd, Name, Value: string;
  EditIdx: Integer;
begin
  GetMacroDetailsFromCtrls(Cmd, Name, Value);
  EditIdx := IndexOfMacroName(Name);
  if EditIdx = -1 then
    raise Exception.CreateFmt(BugUpdate, [Name]);
  if not TMacros.TryLookupMacroCmd(Cmd, fMacroDefinitions[EditIdx].Cmd) then
    raise Exception.CreateFmt(BugInvalidCmd, [Cmd]);
  fMacroDefinitions[EditIdx].Value := Value;
  UpdateMacros(EditIdx);
  lvMacros.ItemIndex := -1;
end;

procedure TMacroEditor.UpdateMacros(SelIdx: Integer);
begin
  DisplayMacros;
  while SelIdx > Pred(lvMacros.Items.Count) do
    Dec(SelIdx);
  if SelIdx >= 0 then
    lvMacros.ItemIndex := SelIdx;
end;

end.
