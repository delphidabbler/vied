{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2023, Peter Johnson (www.delphidabbler.com).
 *
 * Engine of Version Information Editor program. Encapsulates version
 * information functionality in a class.
}


{
  *** THIS CODE IS VERY "SMELLY"!

  - "Smelly" according to Fowler's definition of bad code smells in his book
    "Refactoring"

  The macro handling functionality was added in to the TVInfo class when it
  really should be in its own class. *** NOW EXTRACTED TO UMacros ***

  Further, the implementation just grew like topsy and there's a lot of
  duplication and confusing code.

  Data structures are badly chosen too.

  This code really needs a major overhaul.
}

unit UVInfo;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  Windows,
  // DelphiDabbler library
  PJVersionInfo,
  // Project
  UMacros,
  UVIFile;


type

  ///  <summary>Enumerated type of all standard string information items.
  ///  </summary>
  TStrInfo = (siComments, siCompanyName, siFileDescription, siFileVersion,
    siInternalName, siLegalCopyright, siLegalTrademarks, siOriginalFileName,
    siPrivateBuild, siProductName, siProductVersion, siSpecialBuild);

  ///  <summary>Enumerated type of tokens for all fields.</summary>
  TTokens = (tkF1, tkF2, tkF3, tkF4, tkP1, tkP2, tkP3, tkP4, tkYEAR,
    tkSHORTFNAME, tkPRODUCTNAME, tkSPECIALBUILD, tkDELIMITER, tkCOMMENTS,
    tkCOMPANYNAME, tkFILEDESCRIPTION, tkFILEVERSION, tkINTERNALNAME,
    tkLEGALCOPYRIGHT, tkLEGALTRADEMARK, tkORIGINALFILENAME, tkPRIVATEBUILD,
    tkPRODUCTVERSION, tkMONTH, tkMONTH0, tkDAY, tkDAY0, thHOUR, tkHOUR0,
    tkMINUTE, tkMINUTE0, tkSECOND, tkSECOND0, tkMILLISECOND, tkMILLISECOND0,
    tkYEAR2
  );

  ///  <summary>Set of field tokens.</summary>
  TTokenSet = set of TTokens;

  ///  <summary>Class that encapsulates version information, inputs it from a
  ///  .vi file and outputs it in various formats.</summary>
  TVInfo = class(TObject)
  strict private
    var

      // Property values

      fFileVersionNumberCode: string;
      fProductVersionNumberCode: string;
      fFileOS: LongInt;
      fFileType: LongInt;
      fFileSubType: LongInt;
      fFileFlagsMask: LongInt;
      fFileFlags: LongInt;
      fLanguageCode: Word;
      fCharSetCode: Word;
      fValidating: Boolean;
      fDescribeFileFlags: Boolean;
      fStrInfo: TStringList;
      fIdentifier: string;
      fRCComments: TStringList;
      fVIComments: TStringList;
      fResOutputDir: string;
      fIsUTF8EncodedFile: Boolean;

      // Other fields

      ///  <summary>Object encapsulating macros.</summary>
      fMacros: TMacros;
      ///  <summary>Object encapsulating a .vi file.</summary>
      fVIFile: TVIFile;

    // Property access methods

    ///  <summary>Write accessor for the <c>FileOS</c> property.</summary>
    procedure SetFileOS(AValue: LongInt);

    ///  <summary>Write accessor for the <c>FileType</c> property.</summary>
    procedure SetFileType(AValue: LongInt);

    ///  <summary>Write accessor for the <c>FileSubType</c> property.</summary>
    procedure SetFileSubType(AValue: LongInt);

    ///  <summary>Write accessor for the <c>FileFlagsMask</c> property.
    ///  </summary>
    procedure SetFileFlagsMask(AValue: LongInt);

    ///  <summary>Write accessor for the <c>FileFlags</c> property.</summary>
    procedure SetFileFlags(AValue: LongInt);

    ///  <summary>Write accessor for the <c>LanguageCode</c> property.</summary>
    procedure SetLanguageCode(AValue: Word);

    ///  <summary>Write accessor for the <c>CharSetCode</c> property.</summary>
    procedure SetCharSetCode(AValue: Word);

    ///  <summary>Read accessor for the <c>StrName</c> property.</summary>
    function GetStrName(AnId: TStrInfo): string;

    ///  <summary>Read accessor for the <c>StrDesc</c> property.</summary>
    function GetStrDesc(AnId: TStrInfo): string;

    ///  <summary>Read accessor for the <c>StrRequired</c> property.</summary>
    function GetStrRequired(AnId: TStrInfo): Boolean;

    ///  <summary>Read accessor for the <c>StrPermitted</c> property.</summary>
    function GetStrPermitted(AnId: TStrInfo): Boolean;

    ///  <summary>Read accessor for the <c>StrRequired</c> property.</summary>
    function GetStrInfo(AnId: TStrInfo): string;

    ///  <summary>Write accessor for the <c>StrInfo</c> property.</summary>
    procedure SetStrInfo(AnId: TStrInfo; AStr: string);

    ///  <summary>Write accessor for the <c>RCComments</c> property.</summary>
    procedure SetRCComments(SL: TStringList);

    ///  <summary>Write accessor for the <c>VIComments</c> property.</summary>
    procedure SetVIComments(SL: TStringList);

    ///  <summary>Converts the given <c>Code</c> string into a version number,
    ///  after evaluating any macros contained in <c>Code</c>.</summary>
    ///  <remarks>If any parts of the version number can't be converted to a
    ///  number then that part is set to zero. No error is reported.</remarks>
    function RenderVersionNumberFromCode(const Code: string): TPJVersionNumber;

    ///  <summary>Parses string info item <c>StrInfoId</c>, recursively finding,
    ///  evaluating and replacing any fields and macros.</summary>
    function EvaluateFields(StrInfoId: TStrInfo): string;

    ///  <summary>Parses string info item <c>StrInfoId</c>, recursively finding,
    ///  evaluating and replacing any fields and macros. If any fields are
    ///  contained in <c>AExclusions</c> then an exception is raised.</summary>
    function DoEvaluateFields(StrInfoId: TStrInfo; AExclusions: TTokenSet):
      string;

    ///  <summary>Evaluates the given field <c>I</c>, recursively replacing all
    ///  field and macro references, raising an exception if any embedded field
    ///  containing is one of the excluded fields per <c>AExclusions</c>.
    ///  </summary>
    function FieldValue(I: TTokens; AExclusions: TTokenSet): string;

    ///  <summary>Checks if any undefined macros are referenced anywhere in the
    ///  version information.</summary>
    ///  <remarks>Since this check uses information not available in the TMacros
    ///  the check must be done in TVinfo.</remarks>
    function HasUndefinedMacros: Boolean;

  public

    const

      // Default version information values

      ///  <summary>Default version number used for file and product versions.
      ///  </summary>
      DefVersionNumber: TPJVersionNumber = (V1: 0; V2: 0; V3: 0; V4: 0);

      ///  <summary>Default value for <c>FileOS</c> property.</summary>
      DefFileOS = VOS__WINDOWS32;

      ///  <summary>Default value for <c>FileType</c> property.</summary>
      DefFileType = VFT_APP;

      ///  <summary>Default value for <c>FileFlagsMask</c> property.</summary>
      DefFileFlagsMask = 0;

      ///  <summary>Default value for <c>FileFlags</c> property.</summary>
      DefFileFlags = 0;

      ///  <summary>Default value for <c>LanguageCode</c> property.</summary>
      DefLanguageCode = $0809;    // UK English

      ///  <summary>Default value for <c>CharSetCode</c> property.</summary>
      DefCharSetCode = 1200;      // Unicode

      ///  <summary>Default value for all string information items.</summary>
      DefString = '';

      ///  <summary>Default identifier used in the VERSIONINFO resource
      ///  statement.</summary>
      DefIdentifier = 'VERINFO';

  public

    ///  <summary>Object constructor.</summary>
    constructor Create;

    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;

    ///  <summary>Clears version information in memory and resets to default
    ///  values.</summary>
    procedure Clear;

    ///  <summary>Writes version information in resource file (.rc) format to a
    ///  string list.</summary>
    procedure WriteAsRC(const SL: TStringList);

    ///  <summary>Reads version information from a given .vi format file.
    ///  </summary>
    procedure LoadFromFile(const FileName: string);

    ///  <summary>Saves version information to a .vi format file.</summary>
    procedure SaveToFile(const FileName: string);

    ///  <summary>Copies version information in resource file (.rc) format to
    ///  the clipboard.</summary>
    procedure CopyToClipBoard;

    ///  <summary>Saves version information in resource file (.rc) in default
    ///  ANSI encoding.</summary>
    procedure SaveToResourceFile(const FileName: string);

    ///  <summary>Analyses current version information in memory and checks its
    ///  validity.</summary>
    ///  <param name="ErrList">[in] String list receives any error messages
    ///  unless nil.</param>
    ///  <returns><c>Boolean</c>. True if the information is valid and
    ///  consistent, False otherwise.</returns>
    function Analyse(const ErrList: TStringList): Boolean;

    ///  <summary>Gets a list of valid fields for a string info item.</summary>
    ///  <param name="Id">Id of the string info item.</param>
    ///  <param name="SL">Receives list of permitted fields for the string info
    ///  item.</param>
    procedure ValidFields(const Id: TStrInfo; const SL: TStringList);

    ///  <summary>Checks whether version information has been saved to file.
    ///  </summary>
    ///  <remarks>Whether the object has been changed since a save is not
    ///  relevant. The only test is whether the object has been saved at all.
    ///  </remarks>
    function HasBeenSaved: Boolean;

    // Fixed File Info properties

    ///  <summary>File version number code, including any macros.</summary>
    property FileVersionNumberCode: string read fFileVersionNumberCode
      write fFileVersionNumberCode;

    ///  <summary>Product version number code, includes any macros.</summary>
    property ProductVersionNumberCode: string read fProductVersionNumberCode
      write fProductVersionNumberCode;

    ///  <summary>Identifies the operating system.</summary>
    property FileOS: LongInt read fFileOS write SetFileOS;

    ///  <summary>Identifies the file type.</summary>
    property FileType: LongInt read fFileType write SetFileType;

    ///  <summary>Identifies the file sub-type. The meaning or validity depends
    ///  on the <c>FileType</c> property.</summary>
    property FileSubType: LongInt read fFileSubType write SetFileSubType;

    ///  <summary>Mask that defines valid values of the <c>FileFlags</c>
    ///  property.</summary>
    property FileFlagsMask: LongInt read fFileFlagsMask write SetFileFlagsMask;

    ///  <summary>Bitmask of file flags.</summary>
    property FileFlags: LongInt read fFileFlags write SetFileFlags;

    // Variable File Info properties

    ///  <summary>Identifies the version information language.</summary>
    property LanguageCode: Word read fLanguageCode write SetLanguageCode;

    ///  <summary>Identifies the version information character set.</summary>
    property CharSetCode: Word read fCharSetCode write SetCharSetCode;

    // String Info properties

    ///  <summary>Names of the standard string information items.</summary>
    property StrName[AnId: TStrInfo]: string read GetStrName;

    ///  <summary>Descriptions of the standard string information items.
    ///  </summary>
    property StrDesc[AnId: TStrInfo]: string read GetStrDesc;

    ///  <summary>Flags showing if a string information item is required under
    ///  the Microsoft guidelines.</summary>
    property StrRequired[AnId: TStrInfo]: Boolean read GetStrRequired;

    ///  <summary>Flags showing if a string information item is permitted (by
    ///  File Flags etc).</summary>
    property StrPermitted[AnId: TStrInfo]: Boolean read GetStrPermitted;

    ///  <summary>Values of the string information items.</summary>
    property StrInfo[AnId: TStrInfo]: string read GetStrInfo write SetStrInfo;

    // Other properties

    ///  <summary>Identifier used in the VERSIONINFO resource statement.
    ///  </summary>
    property Identifier: string read fIdentifier write fIdentifier;

    ///  <summary>Lines of comments which should be written to a resource file.
    ///  </summary>
    ///  <remarks>The opening <c>/*</c> and <c>*/</c> are not included in the
    ///  lines.</remarks>
    property RCComments: TStringList read fRCComments write SetRCComments;

    ///  <summary>Lines of comments (excluding the leading <c>;</c> character)
    ///  which should be to a version information file.</summary>
    property VIComments: TStringList read fVIComments write SetVIComments;

    ///  <summary>Object that manages macros.</summary>
    property Macros: TMacros read fMacros;

    ///  <summary>Flag that indicates whether assignment to version information
    ///  properties should be validated according to the Microsoft rules.
    ///  </summary>
    property Validating: Boolean read fValidating write fValidating;

    ///  <summary>Flag that determines whether file flags should be output as a
    ///  string of symbolic descriptive constants (<c>True</c>) or as hex
    ///  numnbers (<c>False</c>).</summary>
    property DescribeFileFlags: Boolean read fDescribeFileFlags
      write fDescribeFileFlags;

    ///  <summary>Default output directory of binary resource (.res) files.
    ///  </summary>
    ///  <remarks>If the directory is not a rooted path it is taken as relative
    ///  to the directory containing the .vi file.</remarks>
    property ResOutputDir: string read fResOutputDir write fResOutputDir;

    ///  <summary>Flag that indicates wether the .vi file was read from, or
    ///  should be written to, a UTF-8 encoded file (<c>True</c>) or a file in
    ///  default ANSI encoding (<c>False</c>).</summary>
    ///  <remarks>For a new document, the encoding depends on the preferred
    ///  encoding format per settings.</remarks>
    property IsUTF8EncodedFile: Boolean read fIsUTF8EncodedFile
      write fIsUTF8EncodedFile;

  end;


implementation


uses
  // Delphi
  ClipBrd,
  IniFiles,
  StrUtils,
  IOUtils,
  Types,
  Character,
  Generics.Collections,
  // Project
  UFileIO,
  UVerUtils,
  UVIData,
  UUtils;


const

  ///  <summary>Names of the standard string information items.</summary>
  ///  <remarks>Do not localise.</remarks>
  StringInfoNames: array[TStrInfo] of string = (
    'Comments', 'CompanyName', 'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFileName',
    'PrivateBuild', 'ProductName', 'ProductVersion', 'SpecialBuild'
  );

  ///  <summary>Descriptions of the standard string information items.</summary>
  ///  <remarks>Do not localise.</remarks>
  StringInfoDescs: array[TStrInfo] of string = (
    'Comments', 'Company Name', 'File Description', 'File Version',
    'Internal Name','Legal Copyright', 'Legal Trademark', 'Original File Name',
    'Private Build', 'Product Name', 'Product Version', 'Special Build'
  );

  ///  <summary>Those fields not permitted in each string information item.
  ///  </summary>
  ExcludeFields: array[TStrInfo] of set of TTokens = (
    [tkCOMMENTS],                         // Comments
    [tkCOMPANYNAME],                      // CompanyName
    [tkFILEDESCRIPTION],                  // FileDescription
    [tkFILEVERSION],                      // FileVersion
    [tkINTERNALNAME],                     // InternalName
    [tkLEGALCOPYRIGHT],                   // LegalCopyright
    [tkLEGALTRADEMARK],                   // LegalTrademarks
    [tkORIGINALFILENAME, tkSHORTFNAME],   // OriginalFileName
    [tkPRIVATEBUILD],                     // PrivateBuild
    [tkPRODUCTNAME],                      // ProductName
    [tkPRODUCTVERSION],                   // ProductVersion
    [tkSPECIALBUILD]                      // SpecialBuild
  );

  ///  <summary>Tokens that represent standard string information items.
  ///  </summary>
  StrInfoFieldTokens: TTokenSet = [
    tkCOMMENTS, tkCOMPANYNAME, tkFILEDESCRIPTION, tkFILEVERSION, tkINTERNALNAME,
    tkLEGALCOPYRIGHT, tkLEGALTRADEMARK, tkORIGINALFILENAME, tkPRIVATEBUILD,
    tkPRODUCTNAME, tkPRODUCTVERSION, tkSPECIALBUILD, tkSHORTFNAME
  ];

  ///  <summary>Character that begins a field reference in text.</summary>
  FieldOpener = '<';
  ///  <summary>Character that ends a field reference in text.</summary>
  FieldCloser = '>';

  ///  <summary>List of supported field names.</summary>
  Fields: array[TTokens] of string = (
    FieldOpener + '#F1' + FieldCloser,
    FieldOpener + '#F2' + FieldCloser,
    FieldOpener + '#F3' + FieldCloser,
    FieldOpener + '#F4' + FieldCloser,
    FieldOpener + '#P1' + FieldCloser,
    FieldOpener + '#P2' + FieldCloser,
    FieldOpener + '#P3' + FieldCloser,
    FieldOpener + '#P4' + FieldCloser,
    FieldOpener + 'YEAR' + FieldCloser,
    FieldOpener + 'SHORTFNAME' + FieldCloser,
    FieldOpener + 'PRODUCTNAME' + FieldCloser,
    FieldOpener + 'SPECIALBUILD' + FieldCloser,
    FieldOpener + FieldOpener + FieldCloser,
    FieldOpener + 'COMMENTS' + FieldCloser,
    FieldOpener + 'COMPANYNAME' + FieldCloser,
    FieldOpener + 'FILEDESCRIPTION' + FieldCloser,
    FieldOpener + 'FILEVERSION' + FieldCloser,
    FieldOpener + 'INTERNALNAME' + FieldCloser,
    FieldOpener + 'LEGALCOPYRIGHT' + FieldCloser,
    FieldOpener + 'LEGALTRADEMARKS' + FieldCloser,
    FieldOpener + 'ORIGINALFILENAME' + FieldCloser,
    FieldOpener + 'PRIVATEBUILD' + FieldCloser,
    FieldOpener + 'PRODUCTVERSION' + FieldCloser,
    FieldOpener + 'MONTH' + FieldCloser,
    FieldOpener + 'MONTH0' + FieldCloser,
    FieldOpener + 'DAY' + FieldCloser,
    FieldOpener + 'DAY0' + FieldCloser,
    FieldOpener + 'HOUR' + FieldCloser,
    FieldOpener + 'HOUR0' + FieldCloser,
    FieldOpener + 'MINUTE' + FieldCloser,
    FieldOpener + 'MINUTE0' + FieldCloser,
    FieldOpener + 'SECOND' + FieldCloser,
    FieldOpener + 'SECOND0' + FieldCloser,
    FieldOpener + 'MILLISECOND' + FieldCloser,
    FieldOpener + 'MILLISECOND0' + FieldCloser,
    FieldOpener + 'YEAR2' + FieldCloser
  );

  ///  <summary>Current file version.</summary>
  ///  <remarks>
  ///  <para>Bump this each time file format changes in such a way that it can't
  ///  safely be read by an earlier version of the program.</para>
  ///  <para>This versioning was effectively introduced when the macros were
  ///  added to the file format: before that the file version is assumed to have
  ///  been zero, although the program didn't check the version then.</para>
  ///  <para>History:</para>
  ///  <para>* v0 - everything up to v2.13.1</para>
  ///  <para>* v1 - from v2.14.0: added Macros</para>
  ///  <para>* v2 - from v2.15.0: allowed dash &amp; underscore in macro names;
  ///  added numerous new fields; permitted UTF-8 formatted .vi files.</para>
  ///  <para>* v3 - from v2.16.0: added support for Env type macros.</para>
  ///  </remarks>
  VIFileVersion = 3;

resourcestring
  // Default VI and RC comments
  sVIComment1 = 'Version information file';
  sVIComment2 = 'Produced by Version Information Editor from DelphiDabbler';
  sRCComment1 = 'Resource file containing VERSIONINFO resource statement';
  sRCComment2 = 'Produced by Version Information Editor from DelphiDabbler';

const
  ///  <summary>Comments written to .vi files by default.</summary>
  DefaultVIComments: array[0..1] of string = (sVIComment1, sVIComment2);
  ///  <summary>Comments written to .rc files by default.</summary>
  DefaultRCComments: array[0..1] of string = (sRCComment1, sRCComment2);


{ TVInfo }

function TVInfo.Analyse(const ErrList: TStringList): Boolean;

  // Records that an error has been found and adds decription to the error list.
  procedure ErrorFound(Str: string);
  begin
    Result := False;
    if ErrList <> nil then ErrList.Add(Str);
  end;

var
  I: TStrInfo;  // loop control
resourcestring
  // Error messages
  sInvalidOS = 'Invalid OS';
  sInvalidFileType = 'Invalid file type';
  sInvalidFileSubType = 'Invalid file sub-type for file type %s';
  sInvalidFileFlagsMask = 'Invalid file flags mask';
  sInvalidFileFlags = 'Invalid file flags for mask %x';
  sInvalidLanguageCode = 'Invalid language code';
  sInvalidCharSetCode = 'Invalid character set code';
  sStrValueRequired = 'A value for %s is required';
  sStrValueNotAllowed = 'No value for %s is permitted';
  sUndefinedMacros = 'One or more macros are undefined';
begin
  // Set default OK result
  Result := True;
  // Version numbers are always OK
  // Check File OS
  if not ValidFileOS(fFileOS) then
    ErrorFound(sInvalidOS);
  // Check File Type and File Sub-Type - only check Sub-Type if File Type OK
  if not ValidFileType(fFileType) then
    ErrorFound(sInvalidFileType)
  else if not ValidFileSubType(fFileType, fFileSubType) then
    ErrorFound(Format(sInvalidFileSubType, [FileTypeToStr(fFileType)]));
  // Check File Flags Mask and File Flags
  if not ValidFileFlagsMask(fFileFlagsMask) then
    ErrorFound(sInvalidFileFlagsMask)
  else if not ValidFileFlags(fFileFlags, fFileFlagsMask) then
    ErrorFound(Format(sInvalidFileFlags, [fFileFlagsMask]));
  // Check Language Code
  if not ValidLangCode(fLanguageCode) then
    ErrorFound(sInvalidLanguageCode);
  // Check Character Set code
  if not ValidCharCode(fCharSetCode) then
    ErrorFound(sInvalidCharSetCode);
  // Check all string info items
  for I := Low(TStrInfo) to High(TStrInfo) do
  begin
    // check if string is required and isn't there
    if (StrInfo[I] = '') and StrRequired[I] then
      ErrorFound(Format(sStrValueRequired, [StrDesc[I]]));
    // check if string isn't permitted and is there
    if (StrInfo[I] <> '') and not StrPermitted[I] then
      ErrorFound(Format(sStrValueNotAllowed, [StrDesc[I]]));
  end;
  if not fMacros.Validate(ErrList) then
    Result := False;
  if HasUndefinedMacros then
    ErrorFound(sUndefinedMacros);
end;

procedure TVInfo.Clear;
var
  I: TStrInfo;  // loop control for string info
  J: Integer;   // loop control for default comments
begin
  // Reset Fixed File Info to default values
  fFileVersionNumberCode := DefVersionNumber;
  fProductVersionNumberCode := DefVersionNumber;
  fFileOS := DefFileOS;
  fFileType := DefFileType;
  fFileSubType := DefaultFileSubType(DefFileType);
  fFileFlagsMask := DefFileFlagsMask;
  fFileFlags := DefFileFlags;
  // Reset variable info to default values
  fLanguageCode := DefLanguageCode;
  fCharSetCode := DefCharSetCode;
  // Reset string info to default values
  for I := Low(TStrInfo) to High(TStrInfo) do
    fStrInfo.Values[StringInfoNames[I]] := DefString;
  // Reset identifier
  fIdentifier := DefIdentifier;
  // Reset comment string lists to default values
  // vi comments
  fVIComments.Clear;
  for J := Low(DefaultVIComments) to High(DefaultVIComments) do
    fVIComments.Add(DefaultVIComments[J]);
  // rc comments
  fRCComments.Clear;
  for J := Low(DefaultRCComments) to High(DefaultRCComments) do
    fRCComments.Add(DefaultRCComments[J]);
  // default .res file folder
  fResOutputDir := '';
  fVIFile.Clear;
  fMacros.Clear;      // clears defined macros
  fMacros.Resolve;    // clears resolved macros
end;

procedure TVInfo.CopyToClipBoard;
var
  RCList: TStringList;  // string list to hold resource file code
begin
  RCList := TStringList.Create;
  try
    WriteAsRC(RCList);
    Clipboard.AsText := TrimRight(RCList.Text) + #13#10;
  finally
    RCList.Free;
  end;
end;

constructor TVInfo.Create;
begin
  inherited Create;
  fVIFile := TVIFile.Create;
  fStrInfo := TStringList.Create;
  fRCComments := TStringList.Create;
  fVIComments := TStringList.Create;
  fMacros := TMacros.Create(fVIFile);
  // Set defaults
  Clear;
end;

destructor TVInfo.Destroy;
begin
  fMacros.Free;
  fVIComments.Free;
  fRCComments.Free;
  fStrInfo.Free;
  fVIFile.Free;
  inherited Destroy;
end;

function TVInfo.DoEvaluateFields(StrInfoId: TStrInfo;
  AExclusions: TTokenSet): string;
var
  TokenIdx: Integer;
  Token: TTokens;
begin
  AExclusions := AExclusions + ExcludeFields[StrInfoId];
  // Process macros in string info item
  Result := fMacros.EvalResolvedMacros(StrInfo[StrInfoId]);
  // Scan through all tokens, searching for each one in string turn
  for Token := Low(TTokens) to High(TTokens) do
  begin
    // Repeatedly check output string for presence of a field's token until
    // all instances have been replaced by value
    repeat
      // Check if field token is in result string
      TokenIdx := Pos(Fields[Token], Result);
      // There is a field token, replace it by its value
      if TokenIdx > 0 then
        Replace(Fields[Token], FieldValue(Token, AExclusions), Result);
    until TokenIdx = 0;
  end;
  Result := TrimRight(Result);
end;

function TVInfo.EvaluateFields(StrInfoId: TStrInfo): string;
begin
  Result := DoEvaluateFields(StrInfoId, []);
end;

function TVInfo.FieldValue(I: TTokens; AExclusions: TTokenSet): string;

  // Parses a version information field
  function ParseVersionField(const Code: string; FieldNum: Byte): string;
  var
    VerNum: TPJVersionNumber;
    FieldValue: Word;
  begin
    Assert(FieldNum in [1..4]);
    // convert to version number
    // TODO: replace by call to RenderVersionNumberFromCode ?
    VerNum := StrToVersionNumber(fMacros.EvalResolvedMacros(Code));
    // pick required field
    case FieldNum of
      1: FieldValue := VerNum.V1;
      2: FieldValue := VerNum.V2;
      3: FieldValue := VerNum.V3;
      4: FieldValue := VerNum.V4;
      else FieldValue := 0;   // avoid compiler hint
    end;
    // convert to string
    Result := IntToStr(FieldValue);
  end;

var
  DateTimeNow: TDateTime;
  Locale: TFormatSettings;

  // Format the current time
  function DateFmtNow(const AFmtStr: string): string;
  begin
    Result := FormatDateTime(AFmtStr, DateTimeNow, Locale);
  end;

resourcestring
  sCircularRef = 'Circular reference while resolving %s field';
begin
  if I in AExclusions then
    raise Exception.CreateFmt(sCircularRef, [Fields[I]]);
  DateTimeNow := Now;
  Locale := TFormatSettings.Create;
  // Return result dependant on token
  case I of
    tkF1: Result := ParseVersionField(fFileVersionNumberCode, 1);
    tkF2: Result := ParseVersionField(fFileVersionNumberCode, 2);
    tkF3: Result := ParseVersionField(fFileVersionNumberCode, 3);
    tkF4: Result := ParseVersionField(fFileVersionNumberCode, 4);
    tkP1: Result := ParseVersionField(fProductVersionNumberCode, 1);
    tkP2: Result := ParseVersionField(fProductVersionNumberCode, 2);
    tkP3: Result := ParseVersionField(fProductVersionNumberCode, 3);
    tkP4: Result := ParseVersionField(fProductVersionNumberCode, 4);
    tkYEAR: Result := DateFmtNow('yyyy');
    tkYEAR2: Result := DateFmtNow('yy');
    tkMONTH: Result := DateFmtNow('m');
    tkMONTH0: Result := DateFmtNow('mm');
    tkDAY: Result := DateFmtNow('d');
    tkDAY0: Result := DateFmtNow('dd');
    thHOUR: Result := DateFmtNow('h');
    tkHOUR0: Result := DateFmtNow('hh');
    tkMINUTE: Result := DateFmtNow('n');
    tkMINUTE0: Result := DateFmtNow('nn');
    tkSECOND: Result := DateFmtNow('s');
    tkSECOND0: Result := DateFmtNow('ss');
    tkMILLISECOND: Result := DateFmtNow('z');
    tkMILLISECOND0: Result := DateFmtNow('zzz');
    tkSHORTFNAME:
      Result := RemoveExtension(
        DoEvaluateFields(siOriginalFileName, AExclusions)
      );
    tkPRODUCTNAME:
      Result := DoEvaluateFields(siProductName, AExclusions);
    tkSPECIALBUILD: Result :=
      DoEvaluateFields(siSpecialBuild, AExclusions);
    tkDELIMITER:
      Result := FieldOpener;
    tkCOMMENTS:
      Result := DoEvaluateFields(siComments, AExclusions);
    tkCOMPANYNAME:
      Result := DoEvaluateFields(siCompanyName, AExclusions);
    tkFILEDESCRIPTION:
      Result := DoEvaluateFields(siFileDescription, AExclusions);
    tkFILEVERSION:
      Result := DoEvaluateFields(siFileVersion, AExclusions);
    tkINTERNALNAME:
      Result := DoEvaluateFields(siInternalName, AExclusions);
    tkLEGALCOPYRIGHT:
      Result := DoEvaluateFields(siLegalCopyright, AExclusions);
    tkLEGALTRADEMARK:
      Result := DoEvaluateFields(siLegalTrademarks, AExclusions);
    tkORIGINALFILENAME:
      Result := DoEvaluateFields(siOriginalFileName, AExclusions);
    tkPRIVATEBUILD:
      Result := DoEvaluateFields(siPrivateBuild, AExclusions);
    tkPRODUCTVERSION:
      Result := DoEvaluateFields(siProductVersion, AExclusions);
  end;
end;

function TVInfo.GetStrDesc(AnId: TStrInfo): string;
begin
  Result := StringInfoDescs[AnId];
end;

function TVInfo.GetStrInfo(AnId: TStrInfo): string;
begin
  Result := fStrInfo.Values[StringInfoNames[AnId]];
end;

function TVInfo.GetStrName(AnId: TStrInfo): string;
begin
  Result := StringInfoNames[AnId];
end;

function TVInfo.GetStrPermitted(AnId: TStrInfo): Boolean;
begin
  case AnId of
    siComments, siCompanyName, siFileDescription, siFileVersion, siInternalName,
    siLegalCopyright, siLegalTrademarks, siOriginalFileName, siProductName,
    siProductVersion:
      // These strings are always permitted
      Result := True;
    siPrivateBuild:
      // This string is permiited only if appropriate flag in FileFlags is set
      Result := (VS_FF_PRIVATEBUILD and FileFlags) = VS_FF_PRIVATEBUILD;
    siSpecialBuild:
      // This string is permiited only if appropriate flag in FileFlags is set
      Result := (VS_FF_SPECIALBUILD and FileFlags) = VS_FF_SPECIALBUILD;
    else
      raise Exception.Create('TVInfo.GetStrPermitted: Unknown TStrInfo value');
  end;
end;

function TVInfo.GetStrRequired(AnId: TStrInfo): Boolean;
begin
  case AnId of
    siComments, siLegalCopyright, siLegalTrademarks:
      // These strings are never required
      Result := False;
    siCompanyName, siFileDescription, siFileVersion, siInternalName,
    siOriginalFileName, siProductName, siProductVersion:
      // These strings are always required
      Result := True;
    siPrivateBuild :
      // This string is required if appropriate flag in FileFlags is set
      Result := (VS_FF_PRIVATEBUILD and FileFlags) = VS_FF_PRIVATEBUILD;
    siSpecialBuild :
      // This string is required if appropriate flag in FileFlags is set
      Result := (VS_FF_SPECIALBUILD and FileFlags) = VS_FF_SPECIALBUILD;
    else
      raise Exception.Create('TVInfo.GetStrRequired: Unknown TStrInfo value');
  end;
end;

function TVInfo.HasBeenSaved: Boolean;
begin
  Result := fVIFile.IsSaved;
end;

function TVInfo.HasUndefinedMacros: Boolean;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    WriteAsRC(SL);
    Result := TMacros.ContainsMacro(SL.Text);
  finally
    SL.Free;
  end;
end;

procedure TVInfo.LoadFromFile(const FileName: string);
var
  I: TStrInfo;
  J: Integer;
  Line: string;
  FileContent: TFileContent;
  VIData: TVIData;
  ConfigDataSection: TVIDataSection;
  FFIDataSection: TVIDataSection;
  VarDataSection: TVIDataSection;
  StrDataSection: TVIDataSection;
  Entry: TPair<string,string>;
resourcestring
  sUnknownFileVer = 'This .vi file''s format can''t be read by this version of '
    + 'VIEd - a later version is required.';
begin
  // Read file & record format
  FileContent := fVIFile.Load(FileName);
  fIsUTF8EncodedFile := FileContent.IsUTF8;

  VIData := TVIData.Create;
  try
    // Parse the file content
    TVIDataReader.Parse(FileContent.Content, VIData);

    // Get VI comments
    VIData.GetComments(fVIComments);

    // Check if file version supported
    ConfigDataSection := VIData.GetSection(dsConfig);
    if StrToIntDef(
      ConfigDataSection.GetValue(TVIDataIO.VIFileVersionName), 0
    ) > VIFileVersion then
    begin
      Clear;
      raise Exception.Create(sUnknownFileVer);
    end;

    // Read macros
    fMacros.Clear;
    for Entry in VIData.GetSection(dsMacros) do
      fMacros.Add(Entry.Key + TMacros.MacroValueSep + Entry.Value);
    fMacros.Resolve;

    // Read FFI
    FFIDataSection := VIData.GetSection(dsFFI);
    // read in fixed file info
    FileVersionNumberCode := FFIDataSection.GetValue(
      TVIDataIO.FileVersionNumberName, VersionNumberToStr(DefVersionNumber)
    );
    ProductVersionNumberCode := FFIDataSection.GetValue(
      TVIDataIO.ProductVersionNumberName,
      VersionNumberToStr(DefVersionNumber)
    );
    FileOS := FFIDataSection.GetValueAsInt(TVIDataIO.FileOSName, DefFileOS);
    FileType := FFIDataSection.GetValueAsInt(
      TVIDataIO.FileTypeName, DefFileType
    );
    FileSubType := FFIDataSection.GetValueAsInt(
      TVIDataIO.FileSubTypeName, DefaultFileSubType(FileType)
    );
    FileFlagsMask := FFIDataSection.GetValueAsInt(
      TVIDataIO.FileFlagsMaskName, DefFileFlagsMask
    );
    FileFlags := FFIDataSection.GetValueAsInt(
      TVIDataIO.FileFlagsName, DefFileFlags
    );

    // read in variable file info
    VarDataSection := VIData.GetSection(dsVar);
    LanguageCode := VarDataSection.GetValueAsInt(
      TVIDataIO.LanguageName, DefLanguageCode
    );
    CharSetCode := VarDataSection.GetValueAsInt(
      TVIDataIO.CharacterSetName, DefCharSetCode
    );

    // read in string info
    StrDataSection := VIData.GetSection(dsString);
    for I := Low(TStrInfo) to High(TStrInfo) do
      StrInfo[I] := StrDataSection.GetValue(StrDesc[I], DefString);

    // Read config section
    // read identifier
    Identifier := ConfigDataSection.GetValue(
      TVIDataIO.IdentifierName, DefIdentifier
    );
    // read RC comments - stripping | characters (used to preserve indentation)
    fRCComments.Clear;
    for J := 0 to Pred(
      ConfigDataSection.GetValueAsInt(TVIDataIO.NumRCCommentsName, 0)
    ) do
    begin
      Line := ConfigDataSection.GetValue(
        Format(TVIDataIO.RCCommentLineNameFmt, [J]), ''
      );
      if (Length(Line) > 0) and (Line[1] = '|') then
        Line := Copy(Line, 2, Length(Line) -1);
      fRCComments.Add(Line);
    end;
    // read default .res file output folder
    fResOutputDir := ConfigDataSection.GetValue(TVIDataIO.ResOutputDirName, '');
  finally
    VIData.Free;
  end;
end;

function TVInfo.RenderVersionNumberFromCode(const Code: string):
  TPJVersionNumber;
begin
  Result := StrToVersionNumber(fMacros.EvalResolvedMacros(Code));
end;

procedure TVInfo.SaveToFile(const FileName: string);
var
  I: TStrInfo;
  J: Integer;
  M: Integer;
  FileNameChanged: Boolean;
  VIData: TVIData;
  FFIData: TVIDataSection;
  VarData: TVIDataSection;
  CfgData: TVIDataSection;
begin
  FileNameChanged := not SameText(fVIFile.Name, FileName);

  VIData := TVIData.Create;
  try
    VIData.SetComments(fVIComments);

    // Write macros
    for M := 0 to Pred(fMacros.Macros.Count) do
      VIData.GetSection(dsMacros).AddOrSetValue(
        fMacros.GetNameAndValue(M)
      );

    // write fixed file info
    FFIData := VIData.GetSection(dsFFI);
    FFIData.AddOrSetValue(
      TVIDataIO.FileVersionNumberName, FileVersionNumberCode
    );
    FFIData.AddOrSetValue(
      TVIDataIO.ProductVersionNumberName, ProductVersionNumberCode
    );
    FFIData.AddOrSetValue(TVIDataIO.FileOSName, FileOS);
    FFIData.AddOrSetValue(TVIDataIO.FileTypeName, FileType);
    FFIData.AddOrSetValue(TVIDataIO.FileSubTypeName, FileSubType);
    FFIData.AddOrSetValue(TVIDataIO.FileFlagsMaskName, FileFlagsMask);
    FFIData.AddOrSetValue(TVIDataIO.FileFlagsName, FileFlags);
    // write variable file info
    VarData := VIData.GetSection(dsVar);
    VarData.AddOrSetValue(TVIDataIO.LanguageName, LanguageCode);
    VarData.AddOrSetValue(TVIDataIO.CharacterSetName, CharSetCode);
    // write string info
    for I := Low(TStrInfo) to High(TStrInfo) do
      VIData.GetSection(dsString).AddOrSetValue(StrDesc[I], StrInfo[I]);
    // Write config section
    CfgData := VIData.GetSection(dsConfig);
    // write identifier
    CfgData.AddOrSetValue(TVIDataIO.IdentifierName, Identifier);
    // write RC comments: first # of lines then write lines preceded by |
    CfgData.AddOrSetValue(TVIDataIO.NumRCCommentsName, fRCComments.Count);
    for J := 0 to fRCComments.Count - 1 do
    begin
      CfgData.AddOrSetValue(
        Format(TVIDataIO.RCCommentLineNameFmt, [J]), '|' + fRCComments[J]
      );
    end;
    // write .res file default output folder
    CfgData.AddOrSetValue(TVIDataIO.ResOutputDirName, fResOutputDir);
    // write .vi file version
    CfgData.AddOrSetValue(TVIDataIO.VIFileVersionName, VIFileVersion);

    fVIFile.Save(
      FileName,
      TFileContent.Create(fIsUTF8EncodedFile, TVIDataWriter.Write(VIData))
    );
  finally
    VIData.Free;
  end;
  if FileNameChanged then
    fMacros.Resolve;
end;

procedure TVInfo.SaveToResourceFile(const FileName: string);
var
  RCList: TStringList;  // .rc source code lines
begin
  RCList := TStringList.Create;
  try
    WriteAsRC(RCList);
    RCList.SaveToFile(FileName, TEncoding.Default);
  finally
    RCList.Free;
  end;
end;

procedure TVInfo.SetCharSetCode(AValue: Word);
begin
  if (not Validating) or ValidCharCode(AValue) then
    fCharSetCode := AValue
  else
    fCharSetCode := DefCharSetCode;
end;

procedure TVInfo.SetFileFlags(AValue: LongInt);
begin
  if Validating then
  begin
    // We're validating
    // ensure that new value is sub-set of FileFlagsMask
    fFileFlags := AValue and fFileFlagsMask;
    // set PrivateBuild and SpecialBuild to '' if their file flag not set
    if not StrPermitted[siPrivateBuild] then
      StrInfo[siPrivateBuild] := '';
    if not StrPermitted[siSpecialBuild] then
      StrInfo[siSpecialBuild] := '';
  end
  else
    // Not validating - simply store new value
    fFileFlags := AValue;
end;

procedure TVInfo.SetFileFlagsMask(AValue: LongInt);
begin
  if (not Validating) or ValidFileFlagsMask(AValue) then
    // Either we're not validating or we are and the value is OK - record value
    fFileFlagsMask := AValue
  else
    // We are validating and value isn't OK - use default
    fFileFlagsMask := DefFileFlagsMask;
  // If validating ensure that FileFlags is a sub-set of new FileFlags mask
  if Validating then
    SetFileFlags(fFileFlags and fFileFlagsMask);
end;

procedure TVInfo.SetFileOS(AValue: LongInt);
begin
  if (not Validating) or ValidFileOS(AValue) then
    fFileOS := AValue
  else
    fFileOS := DefFileOS;
end;

procedure TVInfo.SetFileSubType(AValue: LongInt);
begin
  if (not Validating) or ValidFileSubType(fFileType, AValue) then
    fFileSubType := AValue
  else
    fFileSubType := DefaultFileSubType(fFileType);
end;

procedure TVInfo.SetFileType(AValue: LongInt);
begin
  if (not Validating) or ValidFileType(AValue) then
    // Either we're not validating or we are and the value is OK - record value
    fFileType := AValue
  else
    // We are validating and value isn't OK - use default
    fFileType := DefFileType;
  // If we're validating replace invalid sub types with default
  if Validating and not ValidFileSubType(fFileType, fFileSubType) then
    fFileSubType := DefaultFileSubType(fFileType);
end;

procedure TVInfo.SetLanguageCode(AValue: Word);
begin
  if (not Validating) or ValidLangCode(AValue) then
    fLanguageCode := AValue
  else
    fLanguageCode := DefLanguageCode;
end;

procedure TVInfo.SetRCComments(SL: TStringList);
begin
  fRCComments.Assign(SL);
end;

procedure TVInfo.SetStrInfo(AnId: TStrInfo; AStr: string);
begin
  if (not Validating) or StrPermitted[AnId] then
    fStrInfo.Values[StringInfoNames[AnId]] := AStr
  else
    fStrInfo.Values[StringInfoNames[AnId]] := '';
end;

procedure TVInfo.SetVIComments(SL: TStringList);
begin
  fVIComments.Assign(SL);
end;

procedure TVInfo.ValidFields(const Id: TStrInfo; const SL: TStringList);
var
  I: TTokens;
begin
  // Clear the list
  SL.Clear;
  // Add non-excluded field tokens to list
  for I := Low(TTokens) to High(TTokens) do
    if not (I in ExcludeFields[Id]) then
      SL.Add(Fields[I]);
  SL.Sort;
  // Add macros
  fMacros.ListResolvedNames(SL);
end;

procedure TVInfo.WriteAsRC(const SL: TStringList);
var
  I: TStrInfo;
  J: Integer;
begin
  // Ensure macro External & Import macros re-read data in case changed
  fMacros.Resolve;
  // ** Do not localise any string literals in this method
  // Clear the list
  SL.Clear;
  // Write out C-style comments if required
  if fRCComments.Count > 0 then
  begin
    SL.Add('/*');
    for J := 0 to fRCComments.Count - 1 do
      SL.Add('  ' + fRCComments[J]);
    SL.Add('*/');
    SL.Add('');
  end;
  // Write any required definitions for symbols unknown to BRCC32
  if (fFileType = VFT_DRV)
    and (fFileSubType = VFT2_DRV_VERSIONED_PRINTER) then
  begin
    SL.Add('/* Define symbols not recognised by BRCC32 by default */');
    SL.Add('#define VFT2_DRV_VERSIONED_PRINTER 0x0000000C');
    SL.Add('');
  end;
  // Write VERSIONINFO statement, with define for identifier if required
  if Identifier <> '' then
  begin
    SL.Add(Format('#define %s 1', [Identifier]));
    SL.Add('');
    SL.Add(Format('%s VERSIONINFO', [Identifier]));
  end
  else
    SL.Add('1 VERSIONINFO');
  // Write Fixed File Info
  SL.Add(
    'FILEVERSION ' +
    VersionNumberToStr(RenderVersionNumberFromCode(FileVersionNumberCode))
  );
  SL.Add(
    'PRODUCTVERSION ' +
    VersionNumberToStr(RenderVersionNumberFromCode(ProductVersionNumberCode))
  );
  // write File Flags Mask in hex format
  SL.Add('FILEFLAGSMASK ' + CHexSymbol + IntToHex(FileFlagsMask, 2));
  if FileFlagSetToStr(FileFlags) <> '' then
    // there is a non-empty File Flags bit set: write using symbolic constants
    SL.Add('FILEFLAGS ' + FileFlagSetToStr(FileFlags));
  SL.Add('FILEOS ' + FileOSToStr(FileOS));
  SL.Add('FILETYPE ' + FileTypeToStr(FileType));
  if FileTypeHasSubType(FileType) then
    // file type has a sub-type - write it out
    SL.Add(
      'FILESUBTYPE ' + FileSubTypeToStr(FileType, FileSubType, CHexSymbol)
    );
  // Write out string file info block
  SL.Add('{');
  SL.Add(' BLOCK "StringFileInfo"');
  SL.Add(' {');
  SL.Add(Format('  BLOCK "%4.4x%4.4x"', [fLanguageCode, fCharSetCode]));
  SL.Add('  {');
  // write out each string with fields converted to values
  for I := Low(TStrInfo) to High(TStrInfo) do
  begin
    if StrInfo[I] <> '' then
      SL.Add(
        Format(
          '   VALUE "%s", "%s\000"',
          [StrName[I], BackslashEscape(EvaluateFields(I), '"\', '"\')]
        )
      );
  end;
  SL.Add('  }');
  SL.Add(' }');
  // Write out variable file info block
  SL.Add(' BLOCK "VarFileInfo"');
  SL.Add(' {');
  SL.Add(Format('  VALUE "Translation", %s%4.4x, %d',
    [CHexSymbol, fLanguageCode, fCharSetCode]));
  SL.Add(' }');
  SL.Add('}');
end;

end.

