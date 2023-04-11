{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2023, Peter Johnson (www.delphidabbler.com).
 *
 * Encapsulates the version information data file format.
}


unit UVIData;

interface

uses
  // Delphi
  Generics.Defaults,
  Generics.Collections,
  SysUtils,
  Classes;

type
  ///  <summary>Enumeration of valid sections in the .vi file format.</summary>
  TVIDataSectionId = (dsMacros, dsFFI, dsVar, dsString, dsConfig);
    // ** If this enumeration is re-ordered then TVIDataIO.SectionNames needs to
    //    be re-ordered to match.

  ///  <summary>Encapsulates a section in the .vi file format.</summary>
  TVIDataSection = class(TObject)
  strict private
    var
      ///  <summary>Dictionary storing section's key/value pair entries.
      ///  </summary>
      fEntries: TDictionary<string,string>;
  public
    ///  <summary>Object constructor.</summary>
    constructor Create;
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Clears all key/value entries from the section.</summary>
    procedure Clear;
    ///  <summary>Returns enumerator for the section's key/value pairs</summary>
    function GetEnumerator: TEnumerator<TPair<string,string>>;
    ///  <summary>Adds to or updates the section with the key and string value
    ///  recorded in the given <c>TPair</c> record.</summary>
    ///  <remarks>If <c>AKey</c> already exists in the section its value is
    ///  updated, otherwise a new value is added.</remarks>
    procedure AddOrSetValue(const AKVPair: TPair<string,string>); overload;
    ///  <summary>Adds to or updates the section with a given key and its given
    ///  string value.</summary>
    ///  <remarks>If <c>AKey</c> already exists in the section its value is
    ///  updated, otherwise a new value is added.</remarks>
    procedure AddOrSetValue(const AKey, AValue: string); overload;
    ///  <summary>Adds to or updates the section with a given key and its given
    ///  integer value.</summary>
    ///  <remarks>If <c>AKey</c> already exists in the section its value is
    ///  updated, otherwise a new value is added.</remarks>
    procedure AddOrSetValue(const AKey: string; AValue: Integer); overload;
    ///  <summary>Retrieves and returns a string value from the section.
    ///  </summary>
    ///  <param name="AKey"><c>string</c> [in] Key identifying required value.
    ///  </param>
    ///  <param name="DefValue"><c>string</c> [in] Default value used if
    ///  <c>AKey</c> doesn't exist.</param>
    function GetValue(const AKey: string; const DefValue: string = ''): string;
    ///  <summary>Retrieves and returns an integer value from the section.
    ///  </summary>
    ///  <param name="AKey"><c>string</c> [in] Key identifying required value.
    ///  </param>
    ///  <param name="DefValue"><c>Integer</c> [in] Default value used if
    ///  <c>AKey</c> doesn't exist or if the value is not an integer.</param>
    function GetValueAsInt(const AKey: string; const DefValue: Integer):
      Integer;
    ///  <summary>Checks if given section is empty.</summary>
    function IsEmpty: Boolean;
  end;

  /// <summary>Encapsulates the .vi file format.</summary>
  TVIData = class(TObject)
  strict private
    var
      ///  <summary>List of .vi comments.</summary>
      fComments: TStringList;
      ///  <summary>Array of section objects, one for each supported section.
      ///  </summary>
      fSections: array[TVIDataSectionId] of TVIDataSection;
  public
    ///  <summary>Object contructor. Creates and initialises object.</summary>
    constructor Create;
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Clears all data from the object.</summary>
    procedure Clear;
    ///  <summary>Returns an array .vi containing comments.</summary>
    function GetComments: TArray<string>; overload;
    ///  <summary>Copies .vi comments to <c>AStrings</c>.</summary>
    ///  <remarks>Comments overwrite any current content in <c>AStrings</c>.
    ///  </remarks>
    procedure GetComments(const AStrings: TStrings); overload;
    ///  <summary>Sets .vi comments to those contained in <c>AComments</c>.
    ///  </summary>
    procedure SetComments(const AComments: TStrings);
    ///  <summary>Adds a line to .vi comments.</summary>
    procedure AddComment(const AComment: string);
    ///  <summary>Returns reference to section object for given section ID.
    ///  </summary>
    function GetSection(const ASection: TVIDataSectionId): TVIDataSection;
  end;

  ///  <summary>Base class for .vi file format readers and writers.</summary>
  TVIDataIO = class abstract(TObject)
  strict protected
    const
      ///  <summary>Section names used in .vi file format.</summary>
      SectionNames: array[TVIDataSectionId] of string = (
        'Macros',                 // dsMacros
        'Fixed File Info',        // dsFFI,
        'Variable File Info',     // dsVar,
        'String File Info',       // dsString,
        'Configuration Details'   // dsConfig,
      );
  public
    const
      // Key names used in version information (.vi) files
      FileVersionNumberName = 'File Version #';
      ProductVersionNumberName = 'Product Version #';
      FileOSName = 'File OS';
      FileTypeName = 'File Type';
      FileSubTypeName = 'File Sub-Type';
      FileFlagsMaskName = 'File Flags Mask';
      FileFlagsName = 'File Flags';
      LanguageName = 'Language';
      CharacterSetName = 'Character Set';
      IdentifierName = 'Identifier';
      NumRCCommentsName = 'NumRCComments';
      RCCommentLineNameFmt = 'RC Comment Line %d';
      ResOutputDirName = 'ResOutputDir';
      VIFileVersionName = 'FileVersion';
      ///  <summary>Token used to open a comment in .vi file format.</summary>
      CommentToken = ';';
      ///  <summary>Token used to open a section in .vi file format.</summary>
      SectionOpenToken = '[';
      ///  <summary>Token used to close a section in .vi file format.</summary>
      SectionCloseToken = ']';
      ///  <summary>Token used to separate keys from values in .vi file format.
      ///  </summary>
      KeyValueSeparatorToken = '=';
  strict protected
    ///  <summary>Constructor only to be called by inherited classes.</summary>
    ///  <remarks>Simply causes inherited <c>Create</c> constructor.</remarks>
    constructor InternalCreate;
  public
    ///  <summary>Constructor that prevents this and descendants classes from
    ///  being instantiated.</summary>
    ///  <exception><c>ENoConstructException</c> always raised.</exception>
    constructor Create;
  end;

  ///  <summary>Class that reads and parses .vi file formatted text into a
  ///  <c>TVIData</c> object.</summary>
  TVIDataReader = class sealed(TVIDataIO)
  strict private
    var
      fData: TVIData;
      fLines: TList<string>;
    ///  <summary>Split given text into <c>fLines</c> list after trimming lines
    ///  and skipping empty lines.</summary>
    procedure PreProcess(const AContent: string);
    ///  <summary>Checks if given line is a comment.</summary>
    function IsCommentLine(const ALine: string): Boolean;
    ///  <summary>Updates <c>Idx</c> to index the next line with content, i.e.
    ///  the next line that isn't a comment.</summary>
    procedure NextContentLine(var Idx: Integer);
    ///  <summary>Checks if the given section name is valid and passes out the
    ///  section id, if found, in <c>ASection</c>. Returns true if section is
    ///  found, false otherwise.</summary>
    function TryLookupSectionName(const ASectionName: string;
      out ASection: TVIDataSectionId): Boolean;
    ///  <summary>Strips leading comment token from given comment line and
    ///  returns the result.</summary>
    function ParseComment(const ALine: string): string;
    ///  <summary>Extracts and returns a key/value pair from the given line.
    ///  </summary>
    ///  <exception><c>EVIDataReader</c> raised if the key is empty.
    ///  </exception>
    function ParseKeyValuePair(const ALine: string): TPair<string,string>;
    ///  <summary>Extracts a section header from the given line and returns its
    ///  its ID.</summary>
    ///  <exception><c>EVIDataReader</c> raised if line is not a valid section
    ///  name.</exception>
    function ParseSectionHeader(const ALine: string): TVIDataSectionId;
    ///  <summary>Parses header comment starting at line index <c>Idx</c> and
    ///  ending when <c>Idx</c> addresses the first non-comment line.</summary>
    procedure ParseHeaderComments(var Idx: Integer);
    ///  <summary>Parses all sections in .vi file formatted text, starting at
    ///  <c>Idx</c>.</summary>
    procedure ParseSections(var Idx: Integer);
  strict protected
    ///  <summary>Private contructor called from <c>Parse</c> class function.
    ///  </summary>
    constructor InternalCreate(const AData: TVIData);
  public
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Parses given content, which must be in .vi file format, and
    ///  stores the parsed information in <c>AData</c>.</summary>
    class procedure Parse(const AContent: string; const AData: TVIData);
  end;

  ///  <summary>Class that writes data from a <c>TVIData</c> object into a
  ///  text in .vi file format.</summary>
  TVIDataWriter = class sealed(TVIDataIO)
  public
    ///  <summary>Writes data from <c>AData</c> to a string in .vi file format.
    ///  </summary>
    class function Write(const AData: TVIData): string;
  end;

  ///  <summary>Class of exceptions raised when <c>TVIDataReader</c> encounters
  ///  invalid .vi format.</summary>
  EVIDataReader = class(Exception);

implementation

uses
  // Delphi
  RTLConsts,
  StrUtils,
  // Project
  UUtils;

///  <summary>String has function.</summary>
///  <remarks>Sourced from https://www.scalabium.com/faq/dct0136.htm.</summary>
function ElfHash(const Value: string): Integer;
var
  I: Integer; // loops thru string
  X: Integer; // stores interim results
begin
  Result := 0;
  for I := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[I]);
    X := Result and $F0000000;
    if (X <> 0) then
      Result := Result xor (X shr 24);
    Result := Result and (not X);
  end;
end;

{ TVIData }

procedure TVIData.AddComment(const AComment: string);
begin
  fComments.Add(AComment);
end;

procedure TVIData.Clear;
var
  Section: TVIDataSectionId;
begin
  fComments.Clear;
  for Section := Low(TVIDataSectionId) to High(TVIDataSectionId) do
    fSections[Section].Clear;
end;

constructor TVIData.Create;
var
  Section: TVIDataSectionId;
begin
  inherited;
  fComments := TStringList.Create;
  for Section := Low(TVIDataSectionId) to High(TVIDataSectionId) do
    fSections[Section] := TVIDataSection.Create;
end;

destructor TVIData.Destroy;
begin
  fComments.Free;
  inherited;
end;

function TVIData.GetComments: TArray<string>;
begin
  Result := fComments.ToStringArray;
end;

procedure TVIData.GetComments(const AStrings: TStrings);
begin
  AStrings.Assign(fComments);
end;

function TVIData.GetSection(const ASection: TVIDataSectionId): TVIDataSection;
begin
  Result := fSections[ASection];
end;

procedure TVIData.SetComments(const AComments: TStrings);
begin
  fComments.Assign(AComments);
end;

{ TVIDataReader }

destructor TVIDataReader.Destroy;
begin
  fLines.Free;
  inherited;
end;

constructor TVIDataReader.InternalCreate(const AData: TVIData);
begin
  inherited InternalCreate;
  fLines := TList<string>.Create;
  fData := AData;
  fData.Clear;
end;

function TVIDataReader.IsCommentLine(const ALine: string): Boolean;
begin
  Result := StartsStr(CommentToken, ALine);
end;

procedure TVIDataReader.NextContentLine(var Idx: Integer);
begin
  while (Idx < fLines.Count) and IsCommentLine(fLines[Idx]) do
    Inc(Idx);
  Inc(Idx);
end;

class procedure TVIDataReader.Parse(const AContent: string;
  const AData: TVIData);
var
  Idx: Integer;
  Reader: TVIDataReader;
begin
  Reader := InternalCreate(AData);
  try
    // Preprocess AContent, stripping blank lines and copying to fLines
    Reader.PreProcess(AContent);
    if Reader.fLines.Count = 0 then
      Exit;
    Idx := 0;
    Reader.ParseHeaderComments(Idx);
    Reader.ParseSections(Idx);
  finally
    Reader.Free;
  end;
end;

function TVIDataReader.ParseComment(const ALine: string): string;
begin
  // Assumes ALine is trimmed and starts with comment token
  Assert(StartsStr(CommentToken, ALine));
  // Strip comment token, preserving any following white space
  Result := RightStr(ALine, Length(ALine) - Length(CommentToken));
end;

procedure TVIDataReader.ParseHeaderComments(var Idx: Integer);
begin
  while (Idx < fLines.Count) and IsCommentLine(fLines[Idx]) do
  begin
    fData.AddComment(ParseComment(fLines[Idx]));
    Inc(Idx);
  end;
end;

function TVIDataReader.ParseKeyValuePair(
  const ALine: string): TPair<string, string>;
var
  Key, Value: string;
begin
  SplitStr(ALine, KeyValueSeparatorToken, Key, Value);
  Result.Key := Trim(Key);
  Result.Value := Trim(Value);
  if Result.Key = '' then
    raise EVIDataReader.Create('Empty key');
end;

function TVIDataReader.ParseSectionHeader(const ALine: string):
  TVIDataSectionId;
var
  SectionName: string;
begin
  if not StartsStr(SectionOpenToken, ALine)
    or not EndsStr(SectionCloseToken, ALine) then
    raise EVIDataReader.CreateFmt(
      'Expected section header in line "%s"', [ALine]
    );
  SectionName := MidStr(
    ALine,
    Length(SectionOpenToken) + 1,
    Length(ALine) - Length(SectionOpenToken) - Length(SectionCloseToken)
  );
  if not TryLookupSectionName(SectionName, Result) then
    raise EVIDataReader.CreateFmt('Unknown section name: "%s"', [SectionName]);
end;

procedure TVIDataReader.ParseSections(var Idx: Integer);
var
  CurrentSection: TVIDataSectionId;
  KVPair: TPair<string,string>;
begin
  while (Idx < fLines.Count) and StartsStr(SectionOpenToken, fLines[Idx]) do
  begin
    // Parse section header
    CurrentSection := ParseSectionHeader(fLines[Idx]);
    NextContentLine(Idx);
    // Parse section content
    while (Idx < fLines.Count)
      and not StartsStr(SectionOpenToken, fLines[Idx]) do
    begin
      KVPair := ParseKeyValuePair(fLines[Idx]);
      fData.GetSection(CurrentSection).AddOrSetValue(KVPair);
      NextContentLine(Idx);
    end;
  end;
end;

procedure TVIDataReader.PreProcess(const AContent: string);
var
  ContentLines: TStringList;
  Line: string;
  Idx: Integer;
begin
  // Record lines, trimming leading and trailing space and ignoring empty lines
  fLines.Clear;
  ContentLines := TStringList.Create;
  try
    ContentLines.Text := AContent;
    for Idx := 0 to Pred(ContentLines.Count) do
    begin
      Line := Trim(ContentLines[Idx]);
      if Line <> '' then
        fLines.Add(Line);
    end;
  finally
    ContentLines.Free;
  end;
end;

function TVIDataReader.TryLookupSectionName(const ASectionName: string;
  out ASection: TVIDataSectionId): Boolean;
var
  Section: TVIDataSectionId;
begin
  Result := True;
  for Section := Low(TVIDataSectionId) to High(TVIDataSectionId) do
    if ASectionName = SectionNames[Section] then
    begin
      ASection := Section;
      Exit;
    end;
  Result := False;
end;

{ TVIDataWriter }

class function TVIDataWriter.Write(const AData: TVIData): string;
var
  Lines: TStringList;
  Comment: string;
  Section: TVIDataSectionId;
  Entry: TPair<string,string>;
begin
  Lines := TStringList.Create;
  try
    if Length(AData.GetComments) > 0 then
    begin
      for Comment in AData.GetComments do
        Lines.Add(CommentToken + Comment);
      Lines.Add('');
    end;
    for Section := Low(TVIDataSectionId) to High(TVIDataSectionId) do
    begin
      if AData.GetSection(Section).IsEmpty then
        Continue;
      Lines.Add(SectionOpenToken + SectionNames[Section] + SectionCloseToken);
      for Entry in AData.GetSection(Section) do
        Lines.Add(Entry.Key + KeyValueSeparatorToken + Entry.Value);
      Lines.Add('');
    end;
    Result := TrimRight(Lines.Text) + #13#10;
  finally
    Lines.Free;
  end;
end;

{ TVIDataSection }

procedure TVIDataSection.AddOrSetValue(const AKVPair: TPair<string,string>);
begin
  AddOrSetValue(AKVPair.Key, AKVPair.Value);
end;

procedure TVIDataSection.AddOrSetValue(const AKey, AValue: string);
begin
  fEntries.AddOrSetValue(AKey, AValue);
end;

procedure TVIDataSection.AddOrSetValue(const AKey: string; AValue: Integer);
begin
  AddOrSetValue(AKey, IntToStr(AValue));
end;

procedure TVIDataSection.Clear;
begin
  fEntries.Clear;
end;

constructor TVIDataSection.Create;
begin
  inherited Create;
  fEntries := TDictionary<string,string>.Create(
    TDelegatedEqualityComparer<string>.Create(
      function(const Left, Right: string): Boolean
      begin
        Result := SameText(Left, Right, loInvariantLocale);
      end,
      function(const Value: string): Integer
      begin
        Result := ElfHash(Value);
      end
    )
  );
end;

destructor TVIDataSection.Destroy;
begin
  fEntries.Free;
  inherited;
end;

function TVIDataSection.GetEnumerator: TEnumerator<TPair<string,string>>;
begin
  Result := fEntries.GetEnumerator;
end;

function TVIDataSection.GetValue(const AKey: string; const DefValue: string):
  string;
begin
  if not fEntries.TryGetValue(AKey, Result) then
    Result := DefValue;
end;

function TVIDataSection.GetValueAsInt(const AKey: string;
  const DefValue: Integer): Integer;
var
  V: string;
begin
  if not fEntries.TryGetValue(AKey, V) then
    Exit(DefValue);
  Result := StrToIntDef(V, DefValue);
end;

function TVIDataSection.IsEmpty: Boolean;
begin
  Result := fEntries.Count = 0;
end;

{ TVIDataIO }

constructor TVIDataIO.Create;
begin
  raise ENoConstructException.CreateResFmt(@sNoConstruct, [ClassName]);
end;

constructor TVIDataIO.InternalCreate;
begin
  inherited Create;
end;

end.

