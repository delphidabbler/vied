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
  // VCL
  SysUtils,
  Classes,
  Generics.Collections,
  // Project
  UVIFile;

type
  ///  <summary>Class that encapsulates all macros.</summary>
  TMacros = class(TObject)
  strict private
    var
      ///  <summary>Value of <c>MacroDefinitions</c> property.</summary>
      fMacroDefinitions: TStrings;
      ///  <summary>String list of resolved macros in Name=Value format.
      ///  </summary>
      fResolvedMacros: TStrings;
        // TODO: Change to fResolvedMacros to TList<TResolvedMacro> or similar
      ///  <summary>Reference to object encapsulating a .vi file.</summary>
      fVIFile: TVIFile;

    ///  <summary>Write accessor for <c>MacroDefinitions</c> property.</summary>
    procedure SetMacroDefinitions(const Value: TStrings);
    ///  <summary>Adjusts the given file path to make rooted.</summary>
    ///  <remarks>File paths that are already rooted are unchanged. Relative
    ///  file paths adjusted relative to the directory of the .vi file, if
    ///  saved, other wise they remain relative.</remarks>
    function AdjustFilePath(const FilePath: string): string;
  public
    type
      ///  <summary>Enumeration of the identifiers of valid commands.</summary>
      TMacroCmd = (mcDefine, mcExternal, mcImport, mcEnv);
      ///  <summary>Record containing constituent parts of macro definition.
      ///  </summary>
      TMacroDefinition = record
        ///  <summary>Macro command type.</summary>
        Cmd: TMacroCmd;
        ///  <summary>Unresolved macro name.</summary>
        Name: string;
        ///  <summary>Unresolved macro value</summary>
        ///  <remarks>May be an actual value or a file reference.</remarks>
        Value: string;
      end;
      ///  <summary>Record containing constituent parts of a resolved macro.
      ///  </summary>
      TResolvedMacro = record
        ///  <summary>Resolved macro name.</summary>
        Macro: string;
        ///  <summary>Resolved macro value.</summary>
        Value: string;
      end;
    const
      { TODO: Once fields have been extracted from UVInfo, MacroOpener &
              MacroCloser could possibly be redefined in terms of field opener &
              closer say
                MacroOpener = TFields.FieldOpener + '%'
                MacroCloser = TFields.FieldCloser
      }
      ///  <summary>Character sequence that begins a macro reference in text.
      ///  </summary>
      MacroOpener = '<%';
      ///  <summary>Character that ends a macro reference in text.</summary>
      MacroCloser = '>';
      ///  <summary>Character that separates a macro command from a macro name
      ///  in text.</summary>
      MacroCmdSep = ':';
      ///  <summary>Character that separates a macro name from its value in
      ///  text.</summary>
      MacroValueSep = '=';
      ///  <summary>Text that introduces a Define macro.</summary>
      DefineMacroCmd = 'Define';
      ///  <summary>Text that introduces an External macro.</summary>
      ExternalMacroCmd = 'External';
      ///  <summary>Text that introduces an Import macro.</summary>
      ImportMacroCmd = 'Import';
      ///  <summary>Text that introduces an Env macro.</summary>
      EnvMacroCmd = 'Env';
      ///  <summary>Character that separates the two parts of a resolved macro
      ///  name defined by an Import macro.</summary>
      ImportMacroSeparator = '.';
      {TODO: replace const names in MacroCmds with literal text then delete
             the consts above: they are only used in MacroCmds and just clutter
             the class definition.
      }
      ///  <summary>Array of recognised macro command types as they appear in
      ///  text.</summary>
      MacroCmds: array[TMacroCmd] of string = (
        DefineMacroCmd, ExternalMacroCmd, ImportMacroCmd, EnvMacroCmd
      );

  strict private

    ///  <summary>Checks is a given macro command type is one that references a
    ///  file.</summary>
    class function IsFileReferenceCommand(const ACmd: TMacroCmd): Boolean;
      inline;

    ///  <summary>Enumerates all macro definitions and calls a callback function
    ///  for each one.</summary>
    ///  <remarks>Iff the callback function returns <c>False</c> then the
    ///  enumeration is aborted.</remarks>
    function EnumBadMacroFileRefs(ACallback: TFunc<TMacroDefinition,Boolean>):
      Boolean;

  public

    ///  <summary>Object constructor.</summary>
    ///  <param name="VIFIle">[in] Object representing the .vi file containing
    ///  the macros.</param>
    constructor Create(VIFile: TVIFile);

    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;

    ///  <summary>Checks if the name <c>N</c> is a valid macro name.</summary>
    class function IsValidMacroName(const N: string): Boolean;

    ///  <summary>Iterates through each macro definition string in the given
    ///  list, parses each one into its component parts and returns an array of
    ///  the parsed macro definitions.</summary>
    ///  <param name="Macros"><c>TStrings</c> [in] List of macro definition
    ///  strings.</param>
    ///  <returns><c>TArray&lt;TMacroDefinition&gt;</c>. Array of macro
    ///  definitions decomposed into their constituent parts.</returns>
    ///  <exception><c>Exception</c> is raised if any of the macro definition
    ///  string are malformed.</exception>
    class function ParseMacroDefinitions(const AMacroDefStrs: TStrings):
      TArray<TMacroDefinition>;

    ///  <summary>Creates and returns a string composed of the given macro
    ///  definitions's command type, name and value, using the correct
    ///  separators.</summary>
    class function EncodeMacroDefinition(const Macro: TMacroDefinition): string;

    ///  <summary>Looks up a macro command in the list of valid commands.
    ///  </summary>
    ///  <param name="CmdStr">[in] String representation of the macro command.
    ///  </param>
    ///  <param name="Cmd">[out] Set to the identifier of the command if found.
    ///  Undefined if not found.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if the command is valid.
    ///  <c>False</c> if not.</returns>
    class function TryLookupMacroCmd(const CmdStr: string;
      out Cmd: TMacroCmd): Boolean;

    ///  <summary>Checks if <c>Code</c> contains one or more macro references.
    ///  </summary>
    class function ContainsMacro(const Code: string): Boolean;

    // TODO: Make RelativeMacroFilePath inline or refactor out
    // TODO: Rename RelativeMacroFilePath as RelativeMacroFileDir
    ///  <summary>Returns the directory of the .vi file to which all relative
    ///  macro file references relate.</summary>
    ///  <remarks>Returns the empty string if the .vi file has not been saved.
    ///  </remarks>
    function RelativeMacroFilePath: string;

    ///  <summary>List of all macro definition strings.</summary>
    property MacroDefinitions: TStrings
      read fMacroDefinitions write SetMacroDefinitions;

    ///  <summary>Clears the list of macro definitions.</summary>
    procedure Clear;

    ///  <summary>Adds the given macro definition to the <c>MacroDefinitions</c>
    ///  list.</summary>
    procedure AddDefinition(const AMacroDefinition: string);

    ///  <summary>Returns the number of macro definitions.</summary>
    function DefinitionCount: Integer;

    ///  <summary>Gets the macro definition at the given index in the
    ///  <c>MacroDefinitions</c> list, splits the command/name part from the
    //   value, then returns them as a tuple.</summary>
    function GetMacroDefinitionKVPair(Idx: Integer): TPair<string,string>;

    ///  <summary>Processes the macro definitions, reads in external and
    ///  imported files and creates a list of all resolved macros and their
    ///  actual values.</summary>
    ///  <remarks>Any invalid macro names are ignored and there values are lost.
    ///  No error is reported in this case.</remarks>
    procedure Resolve;

    ///  <summary>Appends the names of all resolved macros to a given string
    ///  list.</summary>
    procedure ListResolvedMacroNames(const AList: TStrings);

    ///  <summary>Get list of all resolved macros and returns as an array of
    ///  <c>TMacro</c> records.</summary>
    function GetResolvedMacros: TArray<TResolvedMacro>;

    ///  <summary>Evaluates any and all the resolved macros in the given string
    ///  and replaces them with their values.</summary>
    function EvalResolvedMacros(const ACodeStr: string): string;

    ///  <summary>Returns a list of all macro definitions that reference
    ///  non-existent files.</summary>
    function GetInvalidFileMacroDefinitions: TArray<TMacroDefinition>;

    ///  <summary>Checks if any macro definitions reference non-existent files.
    ///  </summary>
    function HasBadMacroFileReferences: Boolean;
      // TODO: Rename HasBadMacroFileReferences as HasBadMacroDefinitionFileRefs

    ///  <summary>Validates macros.</summary>
    ///  <param name="AErrorList">[in] String list that receives any error
    ///  information.</param>
    ///  <returns><c>True</c> if no errors are found, <c>False</c> otherwise.
    ///  </returns>
    function Validate(const AErrorList: TStrings): Boolean;
  end;

implementation

uses
  // VCL
  Types,
  Character,
  StrUtils,
  IOUtils,
  Generics.Defaults,
  // Project
  UMutableEnvVars,
  UParams,
  UUtils;

{ TMacros }

procedure TMacros.AddDefinition(const AMacroDefinition: string);
  // TODO: change to name, value pair in AddDefinition and insert separator
begin
  fMacroDefinitions.Add(AMacroDefinition);
end;

function TMacros.AdjustFilePath(const FilePath: string): string;
begin
  Result := FilePath;
  if not TPath.IsPathRooted(Result) then
    // File not rooted: must be relative to ini file being read
    Result := TPath.Combine(RelativeMacroFilePath, Result);
end;

procedure TMacros.Clear;
  // TODO: Change to clear resolved macros too
begin
  fMacroDefinitions.Clear;
  fResolvedMacros.Clear;
end;

class function TMacros.ContainsMacro(const Code: string): Boolean;
begin
  Result := ContainsStr(Code, MacroOpener);
end;

constructor TMacros.Create(VIFile: TVIFile);
begin
  inherited Create;
  fVIFile := VIFile;
  fMacroDefinitions := TStringList.Create;
  fResolvedMacros := TStringList.Create;
end;

function TMacros.DefinitionCount: Integer;
begin
  Result := fMacroDefinitions.Count;
end;

destructor TMacros.Destroy;
begin
  fResolvedMacros.Free;
  fMacroDefinitions.Free;
end;

class function TMacros.EncodeMacroDefinition(
  const Macro: TMacroDefinition): string;
begin
  Result := MacroCmds[Macro.Cmd] + MacroCmdSep
    + Macro.Name + MacroValueSep + Macro.Value;
end;

function TMacros.EnumBadMacroFileRefs(ACallback:
  TFunc<TMacroDefinition,Boolean>): Boolean;
var
  MacroDefinitions: TArray<TMacroDefinition>;
  MacroDefinition: TMacroDefinition;
begin
  MacroDefinitions := TMacros.ParseMacroDefinitions(fMacroDefinitions);
  for MacroDefinition in MacroDefinitions do
  begin
    if IsFileReferenceCommand(MacroDefinition.Cmd) and
      not TFile.Exists(AdjustFilePath(MacroDefinition.Value)) then
      if not ACallback(MacroDefinition) then
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
  ResMacros := GetResolvedMacros;
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

function TMacros.GetInvalidFileMacroDefinitions: TArray<TMacroDefinition>;
var
  BadFileList: TList<TMacroDefinition>;
begin
  BadFileList := TList<TMacroDefinition>.Create;
  try
    EnumBadMacroFileRefs(
      function (AMacro: TMacroDefinition): Boolean
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

function TMacros.GetMacroDefinitionKVPair(Idx: Integer): TPair<string, string>;
begin
  Result.Key := fMacroDefinitions.Names[Idx];
  Result.Value := fMacroDefinitions.ValueFromIndex[Idx];
end;

function TMacros.GetResolvedMacros: TArray<TResolvedMacro>;
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
  SetLength(Result, fResolvedMacros.Count);
  for Idx := 0 to Pred(fResolvedMacros.Count) do
  begin
    ResolvedMacro.Macro := fResolvedMacros.Names[Idx];
    ResolvedMacro.Value := fResolvedMacros.ValueFromIndex[Idx];
    Result[Idx] := ResolvedMacro;
  end;
end;

function TMacros.HasBadMacroFileReferences: Boolean;
var
  BadRefFound: Boolean;
begin
  Assert(fVIFile.IsSaved, 'TVInfo.HasBadMacroFileReferences: file never saved');
  BadRefFound := False;
  EnumBadMacroFileRefs(
    function (AMacro: TMacroDefinition): Boolean
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

procedure TMacros.ListResolvedMacroNames(const AList: TStrings);
var
  ResMacro: TResolvedMacro;
  ResMacros: TArray<TResolvedMacro>;
begin
  Assert(Assigned(AList));
  ResMacros := GetResolvedMacros;
  for ResMacro in ResMacros do
    AList.Add(ResMacro.Macro);
end;

class function TMacros.ParseMacroDefinitions(
  const AMacroDefStrs: TStrings): TArray<TMacroDefinition>;
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
  SetLength(Result, AMacroDefStrs.Count);
  for Idx := 0 to Pred(AMacroDefStrs.Count) do
  begin
    FullName := AMacroDefStrs.Names[Idx];
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
    Result[Idx].Value := AMacroDefStrs.ValueFromIndex[Idx];
    if IsFileReferenceCommand(Cmd) and (Result[Idx].Value = '') then
      raise Exception.CreateFmt(sNoMacroCmdErr, [FullName]);
  end;
end;

function TMacros.RelativeMacroFilePath: string;
begin
  Result := fVIFile.FileDir;
end;

procedure TMacros.Resolve;

  procedure StoreResolvedMacro(const Name, Value: string);
  begin
    fResolvedMacros.Add(
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
  MacroDefinition: TMacros.TMacroDefinition;
  EnvVarName: string;
  EnvVars: TMutableEnvVars;

begin
  {
    Macro definitions have four forms in the ini file:

      1) Define - Define:Name=Value
         Resolved by stripping "Def:" from the name and storing Name & Value.

      2) External - External:Name=FileName
         Resolved by stripping "Inc:" from the name, and storing Name & contents
         of first non blank line of FileName.

      3) Import - Import:Name=FileName
         Resolved by reading contents of the file FileName. Name is sored as a
         macro name in the .ini file, but doesn't itself resolve to a resolved
         macro name. The file FileName must have lines in the format Name=Value
         and a new resolved macro is created for each Name/Value pair.

      4) Env - (1) Env:Name (2) Env:Name=Value
         Resolved by stripping "Env:" from the name. In case (1) the environment
         variable name is the same as Name. In case (2) Name is an alias for
         the environment variable whose name is stored in Value. The macro's
         value is resolved by getting environment variable's value. If the
         environment variable does not exist then the macro is given an empty
         value.

    All macro names:

      * MUST be unique after stripping the prefix
      * MUST begin with a letter or a digit
      * MAY contain other letter, digits, hyphens or underscores

    For Define, Env & External macros the macro name in the ini file is used as
    the resolved macro name.

    For Import macros the macro names available to the program have the form
    xxx.yyy where xxx is the macro name from the ini file and yyy is one of
    those imported from the specified file. This is done to avoid name clashes
    for macro names defined in the .vi file and those defined in other Import
    files. Macro names in imported files MUST be valid names as defined above.

    Invalid macro command type and macro names are ignored. No error is
    reported.
  }

  // Clear any existing resolved macros and associated data structures
  fResolvedMacros.Clear;
  EnvVars := TParams.EnvVars;

  // Do nothing
  if not fVIFile.IsSaved then
    Exit;

  // Process each macro, split into its Cmd, Name & Value fields
  for MacroDefinition in TMacros.ParseMacroDefinitions(fMacroDefinitions) do
  begin
    // Ignore any macros with invalid names
    if TMacros.IsValidMacroName(MacroDefinition.Name) then
    begin
      case MacroDefinition.Cmd of

        mcDefine:
          // Create a macro from value stored in [Macros] section of .vi file
          StoreResolvedMacro(MacroDefinition.Name, MacroDefinition.Value);

        mcExternal:
          // Create a macro with value from 1st non-empty line of file
          StoreResolvedMacro(
            MacroDefinition.Name,
            FirstNonEmptyLine(ReadUTF8TextFile(MacroDefinition.Value))
          );

        mcImport:
        begin
          FileLines := TStringList.Create;
          try
            // Split file into lines
            FileLines.Text := ReadUTF8TextFile(MacroDefinition.Value);
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
                  MacroDefinition.Name + ImportMacroSeparator
                    + FileLines.Names[FileIdx],
                  FileLines.ValueFromIndex[FileIdx]
                );
            end;
          finally
            FileLines.Free;
          end;
        end;

        mcEnv:
        begin
          if MacroDefinition.Value = '' then
            // macro name = env var name
            EnvVarName := MacroDefinition.Name
          else
            // macro name is alias for env var name
            EnvVarName := MacroDefinition.Value;
          if EnvVars.Contains(EnvVarName) then
            StoreResolvedMacro(
              MacroDefinition.Name, EnvVars.GetValue(EnvVarName)
            )
          else
            StoreResolvedMacro(MacroDefinition.Name, '');
        end;

        else ; // ignore any unrecognised commands

      end;
    end;
  end;

end;

procedure TMacros.SetMacroDefinitions(const Value: TStrings);
begin
  if Assigned(Value) then
    fMacroDefinitions.Assign(Value)
  else
    fMacroDefinitions.Clear;
  Resolve;
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
  MacroDefinitions: TArray<TMacroDefinition>;
  Macro: TMacroDefinition;
  BadRelPath: Boolean;
  BadFileMacro: TMacroDefinition;
  BadFileMacros: TArray<TMacroDefinition>;
resourcestring
  sMacroFileNoFound = 'Macro "%0:s": file "%1:s" not found';
  sFileNotSaved = 'Any relative file names referenced by macros are ambiguous:'
    + #13#10'  relative file names are resolved relative to the directory'
    + #13#10'  containing the .vi file, but the .vi file has not been saved';
begin
  Result := True;

  BadFileMacros := Self.GetInvalidFileMacroDefinitions;
  for BadFileMacro in BadFileMacros do
  begin
    AErrorList.Add(
      Format(sMacroFileNoFound, [BadFileMacro.Name, BadFileMacro.Value])
    );
    Result := False;
  end;

  if not fVIFile.IsSaved then
  begin
    MacroDefinitions := TMacros.ParseMacroDefinitions(fMacroDefinitions);
    BadRelPath := False;
    for Macro in MacroDefinitions do
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
