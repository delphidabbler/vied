{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (www.delphidabbler.com).
 *
 * Manages macros that are defined within the .vi or within include files
 * referenced from the .vi file.
}

unit UMacros;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,

  UVIFile;

type
  TMacros = class(TObject)
  strict private
    var
      fMacros: TStrings;
      fResolved: TStrings;
      fVIFile: TVIFile;
    procedure SetMacros(const Value: TStrings);
    function AdjustFilePath(const FilePath: string): string;
  public
    type
      TMacroCmd = (mcDefine, mcExternal, mcImport);
      TMacro = record
        Cmd: TMacroCmd;
        Name: string;
        Value: string;
      end;
      TResolvedMacro = record
        Macro: string;
        Value: string;
      end;
    const
      { TODO: Once fields have been extracted from UVInfo, MacroOpener &
              MacroCloser could possibly be redefined in terms of field opener &
              closer say
                MacroOpener = TFields.FieldOpener + '%'
                MacroCloser = TFields.FieldCloser
      }
      MacroOpener = '<%';
      MacroCloser = '>';
      MacroCmdSep = ':';
      MacroValueSep = '=';
      DefineMacroCmd = 'Define';
      ExternalMacroCmd = 'External';
      ImportMacroCmd = 'Import';
      ImportMacroSeparator = '.';
      MacroCmds: array[TMacroCmd] of string = (
        DefineMacroCmd, ExternalMacroCmd, ImportMacroCmd
      );

  strict private
    class function IsFileReferenceCommand(const ACmd: TMacroCmd): Boolean;
      inline;
    function EnumBadMacroFileRefs(ACallback: TFunc<TMacro,Boolean>): Boolean;

  public

    constructor Create(VIFile: TVIFile);
    destructor Destroy; override;

    class function IsValidMacroName(const N: string): Boolean;
    class function CrackMacros(const Macros: TStrings): TArray<TMacro>;
    class function EncodeMacro(const Macro: TMacros.TMacro): string;
    class function TryLookupMacroCmd(const CmdStr: string;
      out Cmd: TMacroCmd): Boolean;
    class function ContainsMacro(const Code: string): Boolean;

    function RelativeMacroFilePath: string;

    property Macros: TStrings read fMacros write SetMacros;
    property Resolved: TStrings read fResolved;

    procedure Clear;
    procedure Add(const Macro: string);
    function Count: Integer;
    { TODO: Rename following method it gets macro details at position Idx in
            fMacros[], split into Cmd:Name (Key) and Value parts.
    }
    function GetNameAndValue(Idx: Integer): TPair<string,string>;

    ///  <summary>Processes the macro definitions, reads in external and
    ///  imported files and creates a list of all macros and their actual
    ///  values. Macro command type is removed from the macro name.</summary>
    ///  <remarks>Any invalid macro names are ignored and there values are lost.
    ///  No error is reported in this case.</remarks>
    procedure Resolve;

    ///  <summary>Append names of all resolved macros to a given string
    ///  list.</summary>
    procedure ListResolvedNames(const AList: TStrings);

    ///  <summary>Get list of all resolved macros and return as array of TMacro
    ///  records.</summary>
    function GetAllResolved: TArray<TResolvedMacro>;

    function EvalResolvedMacros(const ACodeStr: string): string;

    function GetInvalidFileMacros: TArray<TMacro>;
    function HasBadMacroFileReferences: Boolean;
    function Validate(const AErrorList: TStrings): Boolean;
  end;

implementation

uses
  Types,
  Character,
  StrUtils,
  IOUtils,

  UUtils;

{ TMacros }

procedure TMacros.Add(const Macro: string);
  // TODO: change to name, value pair in Add and insert separator
begin
  fMacros.Add(Macro);
end;

function TMacros.AdjustFilePath(const FilePath: string): string;
begin
  Result := FilePath;
  if not TPath.IsPathRooted(Result) then
    // File not rooted: must be relative to ini file being read
    Result := TPath.Combine(RelativeMacroFilePath, Result);
end;

procedure TMacros.Clear;
begin
  fMacros.Clear;
end;

class function TMacros.ContainsMacro(const Code: string): Boolean;
begin
  Result := ContainsStr(Code, MacroOpener);
end;

function TMacros.Count: Integer;
begin
  Result := fMacros.Count;
end;

class function TMacros.CrackMacros(const Macros: TStrings): TArray<TMacro>;
var
  Idx: Integer;
  FullName: string;
  CmdName: string;
  Cmd: TMacroCmd;
resourcestring
  sBadMacroFmtErr = 'Malformed macro command: "%s"';
  sNoMacroCmdErr = 'Missing macro command in "%s"';
  sNoMacroNameErr = 'Missing macro name in "%s"';
  sBadMacroCmdErr = 'Invalid macro command: "%s"';
  sNoMacroValueErr = 'Macro has no value: "%s"';
begin
  SetLength(Result, Macros.Count);
  for Idx := 0 to Pred(Macros.Count) do
  begin
    FullName := Macros.Names[Idx];
    // Parse macro and check for errors
    if not SplitStr(FullName, MacroCmdSep, CmdName, Result[Idx].Name) then
      raise Exception.CreateFmt(sBadMacroFmtErr, [FullName]);
    if CmdName = '' then
      raise Exception.CreateFmt(sNoMacroCmdErr, [FullName]);
    if Result[Idx].Name = '' then
      raise Exception.CreateFmt(sNoMacroNameErr, [FullName]);
    if not TryLookupMacroCmd(CmdName, Cmd) then
      raise Exception.CreateFmt(sBadMacroCmdErr, [CmdName]);
    Result[Idx].Cmd := Cmd;
    Result[Idx].Value := Macros.ValueFromIndex[Idx];
    if IsFileReferenceCommand(Cmd) and (Result[Idx].Value = '') then
      raise Exception.CreateFmt(sNoMacroCmdErr, [FullName]);
  end;
end;

constructor TMacros.Create(VIFile: TVIFile);
begin
  inherited Create;
  fVIFile := VIFile;
  fMacros := TStringList.Create;
  fResolved := TStringList.Create;
end;

destructor TMacros.Destroy;
begin
  fResolved.Free;
  fMacros.Free;
end;

class function TMacros.EncodeMacro(const Macro: TMacros.TMacro): string;
begin
  Result := MacroCmds[Macro.Cmd] + MacroCmdSep
    + Macro.Name + MacroValueSep + Macro.Value;
end;

function TMacros.EnumBadMacroFileRefs(ACallback: TFunc<TMacro,Boolean>):
  Boolean;
var
  CrackedMacros: TArray<TMacro>;
  Macro: TMacro;
begin
  CrackedMacros := TMacros.CrackMacros(fMacros);
  for Macro in CrackedMacros do
  begin
    if IsFileReferenceCommand(Macro.Cmd) and
      not TFile.Exists(AdjustFilePath(Macro.Value)) then
      if not ACallback(Macro) then
        Exit(False);
  end;
  Result := True;
end;

function TMacros.EvalResolvedMacros(const ACodeStr: string): string;
var
  MacroIdx: Integer;
  ResMacros: TArray<TResolvedMacro>;
  ResMacro: TResolvedMacro;
begin
  Result := ACodeStr;
  ResMacros := GetAllResolved;
  for ResMacro in ResMacros do
  begin
    repeat
      // Check if macro is in result string
      MacroIdx := Pos(ResMacro.Macro, Result);
      // There is a macro, replace it by its value
      if MacroIdx > 0 then
        Replace(ResMacro.Macro, ResMacro.Value, Result);
    until MacroIdx = 0;
  end;
end;

function TMacros.GetAllResolved: TArray<TResolvedMacro>;
{ TODO: Add a boolean parameter to force the macros to be (re-)resolved before
        getting them. If paramter is ForceResolve then add following at top of
        method:
          if ForceResolve then
            Resolve;
}
var
  Idx: Integer;
  ResolvedMacro: TResolvedMacro;
begin
  SetLength(Result, fResolved.Count);
  for Idx := 0 to Pred(fResolved.Count) do
  begin
    ResolvedMacro.Macro := fResolved.Names[Idx];
    ResolvedMacro.Value := fResolved.ValueFromIndex[Idx];
    Result[Idx] := ResolvedMacro;
  end;
end;

function TMacros.GetInvalidFileMacros: TArray<TMacro>;
var
  BadFileList: TList<TMacro>;
begin
  BadFileList := TList<TMacro>.Create;
  try
    EnumBadMacroFileRefs(
      function (AMacro: TMacro): Boolean
      begin
        Result := True;
        BadFileList.Add(AMacro);
      end
    );
    Result := BadFileList.ToArray;
  finally
    BadFileList.Free;
  end;
end;

function TMacros.GetNameAndValue(Idx: Integer): TPair<string, string>;
begin
  Result.Key := fMacros.Names[Idx];
  Result.Value := fMacros.ValueFromIndex[Idx];
end;

function TMacros.HasBadMacroFileReferences: Boolean;
var
  BadRefFound: Boolean;
begin
  Assert(fVIFile.IsSaved, 'TVInfo.HasBadMacroFileReferences: file never saved');
  BadRefFound := False;
  EnumBadMacroFileRefs(
    function (AMacro: TMacro): Boolean
    begin
      BadRefFound := True;
      Result := False;
    end
  );
  Result := BadRefFound;
end;

class function TMacros.IsFileReferenceCommand(const ACmd: TMacroCmd): Boolean;
begin
  Result := ACmd in [mcExternal, mcImport];
end;

class function TMacros.IsValidMacroName(const N: string): Boolean;
var
  Idx: Integer;
begin
  Result := True;
  if N = '' then
    Exit(False); //! fix part of issue #52
  if not TCharacter.IsLetterOrDigit(N[1]) then
    Exit(False); //! fix remainder of issue #52
  //! permit '-' and '_' in name per issue #51
  for Idx := 2 to Length(N) do
    if not TCharacter.IsLetterOrDigit(N[Idx])
      and (N[Idx] <> '-') and (N[Idx] <> '_')  then
      Exit(False);
end;

procedure TMacros.ListResolvedNames(const AList: TStrings);
var
  ResMacro: TResolvedMacro;
  ResMacros: TArray<TResolvedMacro>;
begin
  Assert(Assigned(AList));
  ResMacros := GetAllResolved;
  for ResMacro in ResMacros do
    AList.Add(ResMacro.Macro);
end;

function TMacros.RelativeMacroFilePath: string;
begin
  Result := fVIFile.FileDir;
end;

procedure TMacros.Resolve;

  procedure StoreResolvedMacro(const Name, Value: string);
  begin
    fResolved.Add(
      TMacros.MacroOpener + Name + MacroCloser + MacroValueSep + Value
    );
  end;

  function FirstNonEmptyLine(const S: string): string;
  var
    SA: TStringDynArray;
    Str: string;
  begin
    SA := SplitString(S, sLineBreak);
    for Str in SA do
      if Str <> '' then
        Exit(Str);
    Result := '';
  end;

  function ReadUTF8TextFile(FileName: string): string;
  var
    Bytes: TBytes;
    Encoding: TEncoding;
  begin
    FileName := AdjustFilePath(FileName);
    try
      Bytes := TFile.ReadAllBytes(FileName);
      Encoding := nil;
      TEncoding.GetBufferEncoding(Bytes, Encoding, TEncoding.UTF8);
      Result := Encoding.GetString(Bytes);
    except
      // On error return empty string
      Result := '';
    end;
  end;

var
  FileLines: TStringList;
  FileIdx: Integer;
  Macro: TMacros.TMacro;
begin
  {
    Macros have three forms in the ini file:

      1) Define - Define:Name=Value
         Resolved by stripping "Def:" from the name and storing Name & Value.

      2) External - External:Name=FileName
         Resolved by stripping "Inc:" from the name, and storing Name & contents
         of first non blank line of FileName.

      3) Import - Import:Name=FileName
         Resolved by reading contents of the file FileName. Name is sored as a
         macro name in the .ini file, but doesn't itself resolve to a resolved
         macro name). The file FileName must have lines in the format Name=Value
         and a new resolved macro is created for each Name/Value pair.

    All macro names:

      * MUST be unique after stripping the prefix
      * MUST begin with a letter or a digit
      * MAY contain other letter, digits, hyphens or underscores

    For Define & External macros the macro name in the ini file is that used in
    the program.

    For Import macros the macro names available to the program have the form
    xxx.yyy where xxx is the macro name from the ini file and yyy is one of
    those imported from the specified file. This is done to avoid name clashes
    for macro names defined in the .vi file and those defined in other Import
    files. Macro names in imported files MUST be valid names as defined above.

    Invalid macro command type and macro names are ignored. No error is
    reported.
  }

  // Clear any existing resolved macros
  fResolved.Clear;

  // Do nothing
  if not fVIFile.IsSaved then
    Exit;

  // Process each macro, split into its Cmd, Name & Value fields
  for Macro in TMacros.CrackMacros(fMacros) do
  begin
    // Ignore any macros with invalid names
    if TMacros.IsValidMacroName(Macro.Name) then
    begin
      case Macro.Cmd of

        mcDefine:
          // Create a macro from value stored in [Macros] section of .vi file
          StoreResolvedMacro(Macro.Name, Macro.Value);

        mcExternal:
          // Create a macro with value from 1st non-empty line of file
          StoreResolvedMacro(
            Macro.Name, FirstNonEmptyLine(ReadUTF8TextFile(Macro.Value))
          );

        mcImport:
        begin
          FileLines := TStringList.Create;
          try
            // Split file into lines
            FileLines.Text := ReadUTF8TextFile(Macro.Value);
            // Delete any lines that are not in Name=Value format
            // or are comments lines (start with #)
            for FileIdx := Pred(FileLines.Count) downto 0 do
              if not ContainsStr(FileLines[FileIdx], MacroValueSep)
                or StartsStr('#', TrimLeft(FileLines[FileIdx])) then
                FileLines.Delete(FileIdx);
            // Create a macro for each Name=Value line
            for FileIdx := 0 to Pred(FileLines.Count) do
            begin
              // Ignore any invalid macro name from file
              if TMacros.IsValidMacroName(FileLines.Names[FileIdx]) then
                StoreResolvedMacro(
                  Macro.Name + ImportMacroSeparator + FileLines.Names[FileIdx],
                  FileLines.ValueFromIndex[FileIdx]
                );
            end;
          finally
            FileLines.Free;
          end;
        end;

        else ; // ignore any unrecognised commands

      end;
    end;
  end;

end;

procedure TMacros.SetMacros(const Value: TStrings);
begin
  if Assigned(Value) then
    fMacros.Assign(Value)
  else
    fMacros.Clear;
end;

class function TMacros.TryLookupMacroCmd(const CmdStr: string;
  out Cmd: TMacroCmd): Boolean;
var
  MC: TMacroCmd;
begin
  Result := False;
  for MC := Low(TMacroCmd) to High(TMacroCmd) do
  begin
    if SameText(MacroCmds[MC], CmdStr) then
    begin
      Result := True;
      Cmd := MC;
      Exit;
    end;
  end;
end;

function TMacros.Validate(const AErrorList: TStrings): Boolean;
var
  CrackedMacros: TArray<TMacro>;
  Macro: TMacro;
  BadRelPath: Boolean;
  BadFileMacro: TMacro;
  BadFileMacros: TArray<TMacro>;
resourcestring
  sMacroFileNoFound = 'Macro "%0:s": file "%1:s" not found';
  sFileNotSaved = 'Any relative file names referenced by macros are ambiguous:'
    + #13#10'  relative file names are resolved relative to the directory'
    + #13#10'  containing the .vi file, but the .vi file has not been saved';
begin
  Result := True;

  BadFileMacros := Self.GetInvalidFileMacros;
  for BadFileMacro in BadFileMacros do
  begin
    AErrorList.Add(
      Format(sMacroFileNoFound, [BadFileMacro.Name, BadFileMacro.Value])
    );
    Result := False;
  end;

  if not fVIFile.IsSaved then
  begin
    CrackedMacros := TMacros.CrackMacros(fMacros);
    BadRelPath := False;
    for Macro in CrackedMacros do
    begin
      if IsFileReferenceCommand(Macro.Cmd)
        and not TPath.IsPathRooted(Macro.Value) then
        BadRelPath := True;
    end;
    if BadRelPath then
    begin
      AErrorList.Add(sFileNotSaved);
      Result := False;
    end;
  end;
end;

end.
