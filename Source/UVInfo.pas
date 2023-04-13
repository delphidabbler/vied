{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2022, Peter Johnson (www.delphidabbler.com).
 *
 * Engine of Version Information Editor program. Encapsulates version
 * information functionality in a class.
}


{
  *** THIS CODE IS VERY "SMELLY"!

  - "Smelly" according to Fowler's definition of bad code smells in his book
    "Refactoring"

  The macro handling functionality was added in to the TVInfo class when it
  really should be in its own class.

  Further, the implementation just grew like topsy and there's a lot of
  duplication and confusing code.

  Data structures are badly chosen too.

  This code really needs a major overhaul.
}

unit UVInfo;


interface


uses
  // Delphi
  SysUtils, Classes, Windows,
  // PJSoft library
  PJVersionInfo;


type

  {
  TStrInfo:
    Enumerated type listing all string-info items.
  }
  TStrInfo = (siComments, siCompanyName, siFileDescription, siFileVersion,
    siInternalName, siLegalCopyright, siLegalTrademarks, siOriginalFileName,
    siPrivateBuild, siProductName, siProductVersion, siSpecialBuild);


  {
  TTokens:
    Enumerated type listing tokens for all fields.
  }
  TTokens = (tkF1, tkF2, tkF3, tkF4, tkP1, tkP2, tkP3, tkP4, tkYEAR,
    tkSHORTFNAME, tkPRODUCTNAME, tkSPECIALBUILD, tkDELIMITER, tkCOMMENTS,
    tkCOMPANYNAME, tkFILEDESCRIPTION, tkFILEVERSION, tkINTERNALNAME,
    tkLEGALCOPYRIGHT, tkLEGALTRADEMARK, tkORIGINALFILENAME, tkPRIVATEBUILD,
    tkPRODUCTVERSION, tkMONTH, tkMONTH0, tkDAY, tkDAY0, thHOUR, tkHOUR0,
    tkMINUTE, tkMINUTE0, tkSECOND, tkSECOND0, tkMILLISECOND, tkMILLISECOND0,
    tkYEAR2
  );

  TTokenSet = set of TTokens;

  {
  TVInfo:
    Engine for version info processing.
  }
  TVInfo = class(TObject)
  private
    // Property values
    fFileVersionNumberCode: string;
    fProductVersionNumberCode: string;
    fFileOS: LongInt;
      {Value of FileOS property}
    fFileType: LongInt;
      {Value of FileType property}
    fFileSubType: LongInt;
      {Value of FileSubType property}
    fFileFlagsMask: LongInt;
      {Value of FileFlagsMask property}
    fFileFlags: LongInt;
      {Value of FileFlags property}
    fLanguageCode: Word;
      {Value of LanguageCode property}
    fCharSetCode: Word;
      {Value of CharSetCode property}
    fValidating: Boolean;
      {Value of Validating property}
    fDescribeFileFlags: Boolean;
      {Value of DescribeFileFlags property}
    fStrInfo: TStringList;
      {Value of StrInfo property}
    fIdentifier: string;
      {Value of Identifier property}
    fRCComments: TStringList;
      {Value of RCComments property}
    fVIComments: TStringList;
      {Value of VIComments property}
    // Value of ResolvedMacros
    fResolvedMacros: TStringList;
    // Value of Macros property (macros are unresolved)
    fMacros: TStrings;
    // Value of ResOutputDir property
    fResOutputDir: string;
    // Value of IsUTF8EncodedFile property
    fIsUTF8EncodedFile: Boolean;
    fVIFile: string;
    procedure SetFileOS(AValue: LongInt);
      {Write access method for FileOS property.
        @param AValue [in] New property value.
      }
    procedure SetFileType(AValue: LongInt);
      {Write access method for FileType property.
        @param AValue [in] New property value.
      }
    procedure SetFileSubType(AValue: LongInt);
      {Write access method for FileSubType property.
        @param AValue [in] New property value.
      }
    procedure SetFileFlagsMask(AValue: LongInt);
      {Write access method for FileFlagsMask property.
        @param AValue [in] New property value.
      }
    procedure SetFileFlags(AValue: LongInt);
      {Write access method for FileFlags property.
        @param AValue [in] New property value.
      }
    procedure SetLanguageCode(AValue: Word);
      {Write access method for LanguageCode property.
        @param AValue [in] New property value.
      }
    procedure SetCharSetCode(AValue: Word);
      {Write access method for CharSetCode property.
        @param AValue [in] New property value.
      }
    function GetStrName(AnId: TStrInfo): string;
      {Read accessor for StrName property.
        @param AnId [in] String info item id.
        @return Value of property for AnId.
      }
    function GetStrDesc(AnId: TStrInfo): string;
      {Read accessor for StrDesc property.
        @param AnId [in] String info item id.
        @return Value of property for AnId.
      }
    function GetStrRequired(AnId: TStrInfo): Boolean;
      {Read accessor for StrRequired property. Finds if a string is required to
      have a value.
        @param AnId [in] String info item id.
        @return Property value for AnId.
      }
    function GetStrPermitted(AnId: TStrInfo): Boolean;
      {Read access  method for StrPermitted property. Finds if a string is
      permitted to have a value.
        @param AnId [in] String info item id.
        @return Value of property for AnId.
      }
    function GetStrInfo(AnId: TStrInfo): string;
      {Read accessor for StrInfo property.
        @param AnId [in] String info item id.
        @return Value of property for AnId.
      }
    procedure SetStrInfo(AnId: TStrInfo; AStr: string);
      {Write access method for StrInfo property.
        @param AnId [in] String info item id.
        @param AStr [in] New property value for AnId.
      }
    procedure SetRCComments(SL: TStringList);
      {Write access method for RCComments property.
        @param SL [in] String list containing new property value.
      }
    procedure SetVIComments(SL: TStringList);
      {Write access method for VIComments property.
        @param SL [in] String list containing new property value.
      }
    procedure SetMacros(SL: TStrings);
    function ProcessMacros(const S: string): string;
    procedure ResolveMacros;
    function RenderVersionNumberFromCode(const Code: string): TPJVersionNumber;

    ///  <summary>Replaces all fields and macros in a given string info item by
    ///  their values then returns result with trailing spaces trimmed.
    ///  </summary>
    function EvaluateFields(StrInfoId: TStrInfo): string;

    function DoEvaluateFields(StrInfoId: TStrInfo; AExclusions: TTokenSet):
      string;

    function FieldValue(I: TTokens; AExclusions: TTokenSet): string;
      {Gets value of a field.
        @param I [in] Field identifier.
        @return Value of field.
      }
  public
    const
      // Default version information values
      DefVersionNumber: TPJVersionNumber = (V1: 0; V2: 0; V3: 0; V4: 0);
      DefFileOS = VOS__WINDOWS32;
      DefFileType = VFT_APP;
      DefFileFlagsMask = 0;
      DefFileFlags = 0;
      DefLanguageCode = $0809;    // UK English
      DefCharSetCode = 1200;      // Unicode
      DefString = '';
      DefIdentifier = 'VERINFO';
  public
    type
      TMacroCmd = (mcDefine, mcExternal, mcImport);
      TMacro = record
        Cmd: TMacroCmd;
        Name: string;
        Value: string;
      end;
    const
      MacroCmdSep = ':';
      MacroValueSep = '=';
      DefineMacroCmd = 'Define';
      ExternalMacroCmd = 'External';
      ImportMacroCmd = 'Import';
      ImportMacroSeparator = '.';
      MacroCmds: array[TMacroCmd] of string = (
        DefineMacroCmd, ExternalMacroCmd, ImportMacroCmd
      );
    class function IsValidMacroName(const N: string): Boolean;
    class function CrackMacros(const Macros: TStrings): TArray<TMacro>;
    class function EncodeMacro(const Macro: TMacro): string;
    class function TryLookupMacroCmd(const CmdStr: string;
      out Cmd: TMacroCmd): Boolean;
    class function ContainsMacro(const Code: string): Boolean;
    function RelativeMacroFilePath: string;
    function AdjustFilePath(const FilePath: string): string;
    function HasUndefinedMacros: Boolean;
    function HasBadMacroFileReferences: Boolean;
  strict private
    ///  <summary>Removes macros with invalid names from Macros list.</summary>
    procedure FixupMacros;
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Clear;
      {Clears version info in memory. Resets to default values.
      }
    procedure WriteAsRC(const SL: TStringList);
      {Writes resource file (.rc) format to a stringlist.
        @param SL [in] Receives source code.
      }
    procedure LoadFromFile(const FileName: string);
      {Reads version information from a (.vi) file.
        @param FileName [in] Name of file to be loaded.
      }
    procedure SaveToFile(const FileName: string);
      {Saves version information to a version information (.vi) file.
        @param FileName [in] Name of saved file.
      }
    procedure CopyToClipBoard;
      {Copies version information in a resource file (.rc) format to clipboard.
      }
    procedure SaveToResourceFile(const FileName: string);
      {Saves version information to a resource (.rc) file in default ANSI
      encoding.
        @param FileName [in] Name of output file.
      }
    function Analyse(const ErrList: TStringList): Boolean;
      {Analyses current version information in memory and checks its validity.
        @param ErrList [in] Receives any error messages unless nil.
        @return True if the information is valid and consistent, False
          otherwise.
      }
    procedure ValidFields(const Id: TStrInfo; const SL: TStringList);
      {Gets a list of valid fields for a string info item.
        @param Id [in] Id of string info item.
        @param SL [in] Receives list of permitted fields for string info item.
      }
    ///  <summary>Appends any macros to the given string list.</summary>
    procedure AddMacros(const SL: TStringList);

    ///  <summary>Checks whether version information has been saved to file.
    ///  </summary>
    ///  <remarks>Whether the object has been changed since a save is not
    ///  relevant. The only test is whether the object has been saved at all.
    ///  </remarks>
    function HasBeenSaved: Boolean;

    // Fixed File Info properties

    // file version number code (includes any macros)
    property FileVersionNumberCode: string read fFileVersionNumberCode
      write fFileVersionNumberCode;
    // product version number code (includes any macros)
    property ProductVersionNumberCode: string read fProductVersionNumberCode
      write fProductVersionNumberCode;

    property FileOS: LongInt read fFileOS write SetFileOS;
      {Identifies operating system}
    property FileType: LongInt read fFileType write SetFileType;
      {Identifies file type}
    property FileSubType: LongInt read fFileSubType write SetFileSubType;
      {Identifies and file sub-type. Meaning or validity depends on FileType
      property}
    property FileFlagsMask: LongInt read fFileFlagsMask write SetFileFlagsMask;
      {Mask that defines valid values of FileFlags property}
    property FileFlags: LongInt read fFileFlags write SetFileFlags;
      {Bitmask of file flags}

    // Variable File Info properties

    property LanguageCode: Word read fLanguageCode write SetLanguageCode;
      {Identifies language of version information}
    property CharSetCode: Word read fCharSetCode write SetCharSetCode;
      {Identifies character set of version information}

    // String Info properties

    property StrName[AnId: TStrInfo]: string read GetStrName;
      {Names of string info items}
    property StrDesc[AnId: TStrInfo]: string read GetStrDesc;
      {Descriptions of string info items}
    property StrRequired[AnId: TStrInfo]: Boolean read GetStrRequired;
      {Flag showing if a string info item is required}
    property StrPermitted[AnId: TStrInfo]: Boolean read GetStrPermitted;
      {Flag showing if a string info item is permitted (by File Flags etc)}
    property StrInfo[AnId: TStrInfo]: string read GetStrInfo write SetStrInfo;
      {Value of string info items}

    // Other properties

    property Identifier: string read fIdentifier write fIdentifier;
      {Identifier used in the VERSIONINFO resource statement}
    property RCComments: TStringList read fRCComments write SetRCComments;
      {Lines of comments (excluding /* and */) which should be written to a
      resource file}
    property VIComments: TStringList read fVIComments write SetVIComments;
      {Lines of comments (excluding leading ';' characters) which should be
      written to a version information file}
    ///  <summary>List of macro field definitions.</summary>
    ///  <remarks>Macros are custom fields referenced using &lt;%xxx&gt; fields
    ///  where xxx is the name of the macro. Macros maps a macro name to a
    ///  value. The Macros string list stores macros in Key=Value format. Macros
    ///  are evaluated before other fields.</remarks>
    property Macros: TStrings read fMacros write SetMacros;
    ///  <summary>List of resolved macros.</summary>
    ///  <remarks>This are the macros available for use in the program.
    ///  </remarks>
    property ResolvedMacros: TStringList read fResolvedMacros;
    property Validating: Boolean read fValidating write fValidating;
      {Flag true when assignments to version information properties should be
      validated and False if not}
    property DescribeFileFlags: Boolean read fDescribeFileFlags
      write fDescribeFileFlags;
      {Flag true when file flags should be output as a string of symbolic
      constants and false when File Flags are described by hex numbers}
    property ResOutputDir: string read fResOutputDir write fResOutputDir;
      {Default output directory of .res files: can be relative to .vi file
      folder}

    ///  <summary>Flag true if current vi file was read from a UTF-8 encoded
    ///  file or False if file was ANSI.</summary>
    ///  <remarks>For a new document, the encoding depends on the preferred
    ///  encoding format in settings.</remarks>
    property IsUTF8EncodedFile: Boolean read fIsUTF8EncodedFile
      write fIsUTF8EncodedFile;
  end;


implementation


uses
  // Delphi
  ClipBrd, IniFiles, StrUtils, IOUtils, Types, Character, Generics.Collections,
  // Project
  UFileIO, UVerUtils, UVIData, UUtils;


const

  StringInfoNames: array[TStrInfo] of string = (
    // Names of all string file info strings   ** do not localise
    'Comments', 'CompanyName', 'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFileName',
    'PrivateBuild', 'ProductName', 'ProductVersion', 'SpecialBuild'
  );

  StringInfoDescs: array[TStrInfo] of string = (
    // Description of all string info strings  ** do not localise
    'Comments', 'Company Name', 'File Description', 'File Version',
    'Internal Name','Legal Copyright', 'Legal Trademark', 'Original File Name',
    'Private Build', 'Product Name', 'Product Version', 'Special Build'
  );

  ExcludeFields: array[TStrInfo] of set of TTokens = (
    // Those fields not permitted in info strings
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

  StrInfoFieldTokens: TTokenSet = [
    tkCOMMENTS, tkCOMPANYNAME, tkFILEDESCRIPTION, tkFILEVERSION, tkINTERNALNAME,
    tkLEGALCOPYRIGHT, tkLEGALTRADEMARK, tkORIGINALFILENAME, tkPRIVATEBUILD,
    tkPRODUCTNAME, tkPRODUCTVERSION, tkSPECIALBUILD, tkSHORTFNAME
  ];

  FieldOpener = '<';
  FieldCloser = '>';

  Fields: array[TTokens] of string = (
    // List of field names
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

  MacroOpener = FieldOpener + '%';

  // Current file version
  // - bump this each time file format changes in such a way that it can't
  //   safely be read by an earlier version of the program
  // - this versioning was effectively introduced when the macros were added to
  //   the file format: before that the file version is assumed to have been
  //   zero, although the program didn't check the version then
  VIFileVersion = 1;

resourcestring
  // Default VI and RC comments
  sVIComment1 = 'Version information file';
  sVIComment2 = 'Produced by Version Information Editor from DelphiDabbler';
  sRCComment1 = 'Resource file containing VERSIONINFO resource statement';
  sRCComment2 = 'Produced by Version Information Editor from DelphiDabbler';

const
  // Record default VI and RC comments
  DefaultVIComments: array[0..1] of string = (sVIComment1, sVIComment2);
  DefaultRCComments: array[0..1] of string = (sRCComment1, sRCComment2);


{ TVInfo }

procedure TVInfo.AddMacros(const SL: TStringList);
var
  MacroIdx: Integer;
begin
  for MacroIdx := 0 to Pred(fResolvedMacros.Count) do
    SL.Add(fResolvedMacros.Names[MacroIdx]);
end;

function TVInfo.AdjustFilePath(const FilePath: string): string;
begin
  Result := FilePath;
  if not TPath.IsPathRooted(Result) then
    // File not rooted: must be relative to ini file being read
    Result := TPath.Combine(RelativeMacroFilePath, Result);
end;

function TVInfo.Analyse(const ErrList: TStringList): Boolean;
  {Analyses current version information in memory and checks its validity.
    @param ErrList [in] Receives any error messages unless nil.
    @return True if the information is valid and consistent, False otherwise.
  }

  // ---------------------------------------------------------------------------
  procedure ErrorFound(Str: string);
    {Records that an error has been found and adds decription to the error list.
      @param Str [in] Description of error.
    }
  begin
    Result := False;
    if ErrList <> nil then ErrList.Add(Str);
  end;
  // ---------------------------------------------------------------------------

var
  I: TStrInfo;  // loop control
  Macro: TMacro;
  Macros: TArray<TMacro>;
  BadRelPath: Boolean;
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
  sFileNotSaved = 'Any relative file names referenced by macro are ambiguous:'
    + #13#10'  relative file names are resolved relative to the directory'
    + #13#10'  containg the .vi file, but the .vi file has not been saved';
  sMacroFileNoFound = 'Macro "%0:s": file "%1:s" not found';
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

  // Check macros
  Macros := CrackMacros(fMacros);
  BadRelPath := False;
  for Macro in Macros do
  begin
    if Macro.Cmd in [mcExternal, mcImport] then
    begin
      if (fVIFile = '') and not TPath.IsPathRooted(Macro.Value) then
        BadRelPath := True;
      if not TFile.Exists(AdjustFilePath(Macro.Value)) then
        ErrorFound(Format(sMacroFileNoFound, [Macro.Name, Macro.Value]));
    end;
  end;
  if BadRelPath then
    ErrorFound(sFileNotSaved);
  if HasUndefinedMacros then
    ErrorFound(sUndefinedMacros);
end;

procedure TVInfo.Clear;
  {Clears version info in memory. Resets to default values.
  }
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
  fVIFile := '';
  fMacros.Clear;
  ResolveMacros;
end;

class function TVInfo.ContainsMacro(const Code: string): Boolean;
begin
  Result := ContainsStr(Code, MacroOpener);
end;

procedure TVInfo.CopyToClipBoard;
  {Copies version information in a resource file (.rc) format to clipboard.
  }
var
  RCList: TStringList;          // string list to hold resource file code
begin
  RCList := TStringList.Create;
  try
    WriteAsRC(RCList);
    Clipboard.AsText := TrimRight(RCList.Text) + #13#10;
  finally
    RCList.Free;
  end;
end;

class function TVInfo.CrackMacros(const Macros: TStrings): TArray<TMacro>;
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
    if not SplitStr(
      FullName, MacroCmdSep, CmdName, Result[Idx].Name
    ) then
      raise Exception.CreateFmt(sBadMacroFmtErr, [FullName]);
    if CmdName = '' then
      raise Exception.CreateFmt(sNoMacroCmdErr, [FullName]);
    if Result[Idx].Name = '' then
      raise Exception.CreateFmt(sNoMacroNameErr, [FullName]);
    if not TryLookupMacroCmd(CmdName, Cmd) then
      raise Exception.CreateFmt(sBadMacroCmdErr, [CmdName]);
    Result[Idx].Cmd := Cmd;
    Result[Idx].Value := Macros.ValueFromIndex[Idx];
    if (Cmd in [mcExternal, mcImport]) and (Result[Idx].Value = '') then
      raise Exception.CreateFmt(sNoMacroCmdErr, [FullName]);
  end;
end;

constructor TVInfo.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited Create;
  // Create string lists for string info and comment lines for RC & VI
  fStrInfo := TStringList.Create;
  fRCComments := TStringList.Create;
  fVIComments := TStringList.Create;
  fMacros := TStringList.Create;
  fResolvedMacros := TStringList.Create;
  // Set defaults
  Clear;
end;

destructor TVInfo.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fResolvedMacros.Free;
  fMacros.Free;
  fVIComments.Free;
  fRCComments.Free;
  fStrInfo.Free;
  inherited Destroy;
end;

function TVInfo.DoEvaluateFields(StrInfoId: TStrInfo;
  AExclusions: TTokenSet): string;
var
  TokenIdx: Integer;  // index of field token in string
  Token: TTokens;     // loop control
begin
  AExclusions := AExclusions + ExcludeFields[StrInfoId];
  // Process macros in strining info item
  Result := ProcessMacros(StrInfo[StrInfoId]);
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

class function TVInfo.EncodeMacro(const Macro: TMacro): string;
begin
  Result := MacroCmds[Macro.Cmd] + MacroCmdSep
    + Macro.Name + MacroValueSep + Macro.Value;
end;

function TVInfo.EvaluateFields(StrInfoId: TStrInfo): string;
begin
  Result := DoEvaluateFields(StrInfoId, []);
end;

function TVInfo.FieldValue(I: TTokens; AExclusions: TTokenSet): string;

  function ParseVersionField(const Code: string; FieldNum: Byte): string;
  var
    VerNum: TPJVersionNumber;
    FieldValue: Word;
  begin
    Assert(FieldNum in [1..4]);
    // convert to version number
    VerNum := StrToVersionNumber(ProcessMacros(Code));
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
      Result := RemoveExtension(EvaluateFields(siOriginalFileName));
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

procedure TVInfo.FixupMacros;
var
  Macro: TMacro;
  FixedMacros: TStrings;
begin
  FixedMacros := TStringList.Create;
  try
    for Macro in CrackMacros(fMacros) do
      if IsValidMacroName(Macro.Name) then
        FixedMacros.Add(EncodeMacro(Macro));
    fMacros.Assign(FixedMacros);
  finally
    FixedMacros.Free;
  end;
end;

function TVInfo.GetStrDesc(AnId: TStrInfo): string;
  {Read accessor for StrDesc property.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
begin
  Result := StringInfoDescs[AnId];
end;

function TVInfo.GetStrInfo(AnId: TStrInfo): string;
  {Read accessor for StrInfo property.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
begin
  Result := fStrInfo.Values[StringInfoNames[AnId]];
end;

function TVInfo.GetStrName(AnId: TStrInfo): string;
  {Read accessor for StrName property.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
begin
  Result := StringInfoNames[AnId];
end;

function TVInfo.GetStrPermitted(AnId: TStrInfo): Boolean;
  {Read access method for StrPermitted property. Finds if a string is permitted
  to have a value.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
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
  {Read accessor for StrRequired property. Finds if a string is required to have
  a value.
    @param AnId [in] String info item id.
    @return Property value for AnId.
  }
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

function TVInfo.HasBadMacroFileReferences: Boolean;
var
  Macro: TMacro;
  Macros: TArray<TMacro>;
begin
  Assert(HasBeenSaved, 'TVInfo.HasBadMacroFileReferences: file never saved');
  Macros := CrackMacros(fMacros);
  for Macro in Macros do
  begin
    if Macro.Cmd in [mcExternal, mcImport] then
    begin
      if not TFile.Exists(AdjustFilePath(Macro.Value)) then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TVInfo.HasBeenSaved: Boolean;
begin
  Result := fVIFile <> '';
end;

function TVInfo.HasUndefinedMacros: Boolean;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    WriteAsRC(SL);
    Result := StrUtils.ContainsStr(SL.Text, MacroOpener);
  finally
    SL.Free;
  end;
end;

class function TVInfo.IsValidMacroName(const N: string): Boolean;
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

procedure TVInfo.LoadFromFile(const FileName: string);
  {Reads version information from a (.vi) file.
    @param FileName [in] Name of file to be loaded.
  }
var
  I: TStrInfo;    // loop control for string info
  J: Integer;     // loop control for comments
  Line: string;   // line to receive comments
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
  fVIFile := FileName;

  // Read file & record format
  FileContent := TFileIO.ReadFile(FileName);
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
      fMacros.Add(Entry.Key + MacroValueSep + Entry.Value);
    FixupMacros;
    ResolveMacros;

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

function TVInfo.ProcessMacros(const S: string): string;
var
  MValue: string;
  MField: string;
  NameIdx: Integer;
  MacroIdx: Integer;
begin
  Result := S;
  for NameIdx := 0 to Pred(fResolvedMacros.Count) do
  begin
    MField := fResolvedMacros.Names[NameIdx];
    MValue := fResolvedMacros.ValueFromIndex[NameIdx];
    repeat
      // Check if macro is in result string
      MacroIdx := Pos(MField, Result);
      // There is a macro, replace it by its value
      if MacroIdx > 0 then
        Replace(MField, MValue, Result);
    until MacroIdx = 0;
  end;
end;

function TVInfo.RelativeMacroFilePath: string;
begin
  if HasBeenSaved then
    Result := TPath.GetDirectoryName(fVIFile)
  else
    Result := '';
end;

function TVInfo.RenderVersionNumberFromCode(const Code: string):
  TPJVersionNumber;
begin
  Result := StrToVersionNumber(ProcessMacros(Code));
end;

procedure TVInfo.ResolveMacros;

  procedure StoreResolvedMacro(const Name, Value: string);
  begin
    fResolvedMacros.Add(
      MacroOpener + Name + FieldCloser + MacroValueSep + Value
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
  Macro: TMacro;
begin
  {
    Macros have three forms in the ini file:

    1) Define - Define:Name=Value
       resolve by stripping "Def:" from the name and storing Name & Value.

    2) External - External:Name=FileName
       resolve by stripping "Inc:" from the name, and storing Name & contents of
       first non blank line of FileName.

    3) Import - Import:Name=FileName
       resolve by reading contents of FileName (Name is sored as a macro name,
       but doesn't itself resolve to a resolved macro name). FileName must have
       lines in format Name=Value) and a new resolved macro is created for
       each Name/Value pair.

    All macro names must be unique after stripping the prefix AND must comprise
    only letters and digits.

    For Define & External macros the macro name in the ini file is that used in
    the program.

    For Import macros the macro names available to the program have the form
    xxx.yyy where xxx is the macro name from the ini file and yyy is one of
    those imported from the specified file. This is done to avoid name clashes
    for macro names defined in the .vi file and those defined in other Import
    files. Macro names in imported files must also comprise only letters and
    digits.

    Macro names that do not conform to the letter/digit only specification are
    ignored.
  }

  fResolvedMacros.Clear;
  if fVIFile = '' then
    Exit;
  for Macro in CrackMacros(fMacros) do
  begin
    // Ignore any macros with invalid names
    // ** this check is duplicated providing FixupMacros has been called to
    //    clean up fMacros
    if IsValidMacroName(Macro.Name) then
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
            // Delete any lines that are in Name=Value format
            // or are comments lines (start with #)
            for FileIdx := Pred(FileLines.Count) downto 0 do
              if not ContainsStr(FileLines[FileIdx], MacroValueSep)
                or StartsStr('#', TrimLeft(FileLines[FileIdx])) then
                FileLines.Delete(FileIdx);
            // Create a macro for each Name=Value line
            for FileIdx := 0 to Pred(FileLines.Count) do
            begin
              // Ignore any invalid macro name from file
              if IsValidMacroName(FileLines.Names[FileIdx]) then
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

procedure TVInfo.SaveToFile(const FileName: string);
  {Saves version information to a version information (.vi) file.
    @param FileName [in] Name of saved file.
  }
var
  I: TStrInfo;    // loop control for string info
  J: Integer;     // loop control for comments
  M: Integer;     // loop control for macros
  FileNameChanged: Boolean;
  VIData: TVIData;
  FFIData: TVIDataSection;
  VarData: TVIDataSection;
  CfgData: TVIDataSection;
begin
  FileNameChanged := not SameText(fVIFile, FileName);
  fVIFile := FileName;

  VIData := TVIData.Create;
  try
    VIData.SetComments(fVIComments);

    // Write macros
    for M := 0 to Pred(fMacros.Count) do
      VIData.GetSection(dsMacros).AddOrSetValue(
        fMacros.Names[M], fMacros.ValueFromIndex[M]
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

    TFileIO.WriteFile(
      FileName,
      TFileContent.Create(fIsUTF8EncodedFile, TVIDataWriter.Write(VIData))
    );
  finally
    VIData.Free;
  end;
  if FileNameChanged then
    ResolveMacros;
end;

procedure TVInfo.SaveToResourceFile(const FileName: string);
  {Saves version information to a resource (.rc) file in default ANSI encoding.
    @param FileName [in] Name of output file.
  }
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
  {Write access method for CharSetCode property.
    @param AValue [in] New property value.
  }
begin
  if (not Validating) or ValidCharCode(AValue) then
    fCharSetCode := AValue
  else
    fCharSetCode := DefCharSetCode;
end;

procedure TVInfo.SetFileFlags(AValue: LongInt);
  {Write access method for FileFlags property.
    @param AValue [in] New property value.
  }
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
  {Write access method for FileFlagsMask property.
    @param AValue [in] New property value.
  }
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
  {Write access method for FileOS property.
    @param AValue [in] New property value.
  }
begin
  if (not Validating) or ValidFileOS(AValue) then
    fFileOS := AValue
  else
    fFileOS := DefFileOS;
end;

procedure TVInfo.SetFileSubType(AValue: LongInt);
  {Write access method for FileSubType property.
    @param AValue [in] New property value.
  }
begin
  if (not Validating) or ValidFileSubType(fFileType, AValue) then
    fFileSubType := AValue
  else
    fFileSubType := DefaultFileSubType(fFileType);
end;

procedure TVInfo.SetFileType(AValue: LongInt);
  {Write access method for FileType property.
    @param AValue [in] New property value.
  }
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
  {Write access method for LanguageCode property.
    @param AValue [in] New property value.
  }
begin
  if (not Validating) or ValidLangCode(AValue) then
    fLanguageCode := AValue
  else
    fLanguageCode := DefLanguageCode;
end;

procedure TVInfo.SetMacros(SL: TStrings);
begin
  if Assigned(SL) then
    fMacros.Assign(SL)
  else
    fMacros.Clear;
  ResolveMacros;
end;

procedure TVInfo.SetRCComments(SL: TStringList);
  {Write access method for RCComments property.
    @param SL [in] String list containing new property value.
  }
begin
  fRCComments.Assign(SL);
end;

procedure TVInfo.SetStrInfo(AnId: TStrInfo; AStr: string);
  {Write access method for StrInfo property.
    @param AnId [in] String info item id.
    @param AStr [in] New property value for AnId.
  }
begin
  if (not Validating) or StrPermitted[AnId] then
    fStrInfo.Values[StringInfoNames[AnId]] := AStr
  else
    fStrInfo.Values[StringInfoNames[AnId]] := '';
end;

procedure TVInfo.SetVIComments(SL: TStringList);
  {Write access method for VIComments property.
    @param SL [in] String list containing new property value.
  }
begin
  fVIComments.Assign(SL);
end;

class function TVInfo.TryLookupMacroCmd(const CmdStr: string;
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

procedure TVInfo.ValidFields(const Id: TStrInfo; const SL: TStringList);
  {Gets a list of valid fields for a string info item.
    @param Id [in] Id of string info item.
    @param SL [in] Receives list of permitted fields for string info item.
  }
var
  I: TTokens; // loop control
begin
  // Clear the list
  SL.Clear;
  // Add non-excluded field tokens to list
  for I := Low(TTokens) to High(TTokens) do
    if not (I in ExcludeFields[Id]) then
      SL.Add(Fields[I]);
  SL.Sort;
  // Add macros
  AddMacros(SL);
end;

procedure TVInfo.WriteAsRC(const SL: TStringList);
  {Writes resource file (.rc) format to a stringlist.
    @param SL [in] Receives source code.
  }
var
  I: TStrInfo;  // loop control for string info
  J: Integer;   // loop control for comments
begin
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

