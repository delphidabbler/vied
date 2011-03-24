{
 * UVInfo.pas
 *
 * Engine of Version Information Editor program. Encapsulates version
 * information functionality in a class.
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
 * The Original Code is UVInfo.pas.
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


unit UVInfo;


interface


uses
  // Delphi
  SysUtils, Classes,
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
      tkSHORTFNAME, tkPRODUCTNAME, tkSPECIALBUILD, tkDELIMITER);


  {
  TVInfo:
    Engine for version info processing.
  }
  TVInfo = class(TObject)
  private
    fFileVersionNumber: TPJVersionNumber;
      {Value of FileVersionNumber property}
    fProductVersionNumber: TPJVersionNumber;
      {Value of ProductVersionNumber property}
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
    fResOutputDir: string;
      {Value of ResOutputDir property}
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
      {Read access method for StrPermitted property. Finds if a string is
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
    function FileFlagsAsString(const FFlag: LongInt): string;
      {Builds string describing file flags bit-set depending on whether file
      flags are being fully described or are being shown as Hex values.
        @param FFlag [in] File flags bit set.
        @return Required description string.
      }
    function EvaluateFields(StrInfoId: TStrInfo): string;
      {Replaces all fields in a string info item by their values.
        @param StrInfoId [in] String info item to be processed.
        @return Given string info item with fields substuted by values.
      }
    function FieldValue(I: TTokens): string;
      {Gets value of a field.
        @param I [in] Field identifier.
        @return Value of field.
      }
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
    procedure WriteToDisplay(const S: TStrings);
      {Writes version information in format ready for display.
        @param S [in] String list that receives version information in display
          format.
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
      {Saves version information to a resource (.rc) file.
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

    {Fixed File Info properties}
    property FileVersionNumber: TPJVersionNumber read fFileVersionNumber
      write fFileVersionNumber;
      {File version number}
    property ProductVersionNumber: TPJVersionNumber read fProductVersionNumber
      write fProductVersionNumber;
      {Product version number}
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

    {Variable File Info properties}
    property LanguageCode: Word read fLanguageCode write SetLanguageCode;
      {Identifies language of version information}
    property CharSetCode: Word read fCharSetCode write SetCharSetCode;
      {Identifies character set of version information}

    {String Info properties}
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

    {Other properties}
    property Identifier: string read fIdentifier write fIdentifier;
      {Identifier used in the VERSIONINFO resource statement}
    property RCComments: TStringList read fRCComments write SetRCComments;
      {Lines of comments (excluding /* and */) which should be written to a
      resource file}
    property VIComments: TStringList read fVIComments write SetVIComments;
      {Lines of comments (excluding leading ';' characters) which should be
      written to a version information file}
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
  end;


implementation


uses
  // Delphi
  IniFiles, ClipBrd, Windows,
  // Project
  UVerUtils, UUtils;


const

  cStrNames: array[TStrInfo] of string[20] = (
    // Names of all string file info strings   ** do not localise
    'Comments', 'CompanyName', 'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFileName',
    'PrivateBuild', 'ProductName', 'ProductVersion', 'SpecialBuild'
  );

  cStrDesc: array[TStrInfo] of string[40] = (
    // Description of all string info strings  ** do not localise
    'Comments', 'Company Name', 'File Description', 'File Version',
    'Internal Name','Legal Copyright', 'Legal Trademark', 'Original File Name',
    'Private Build', 'Product Name', 'Product Version', 'Special Build'
  );

  cExcludeFields: array[TStrInfo] of set of TTokens = (
    // Those fields not permitted in info strings
    [],               // Comments
    [],               // CompanyName
    [],               // FileDescription
    [],               // FileVersion
    [],               // InternalName
    [],               // LegalCopyright
    [],               // LegalTrademarks
    [tkSHORTFNAME],   // OriginalFileName
    [],               // PrivateBuild
    [tkPRODUCTNAME],  // ProductName
    [],               // ProductVersion
    [tkSPECIALBUILD]  // SpecialBuild
  );

  cFields: array[TTokens] of string[15] = (
    // List of field names
    '<#F1>', '<#F2>', '<#F3>', '<#F4>', '<#P1>', '<#P2>', '<#P3>', '<#P4>',
    '<YEAR>', '<SHORTFNAME>', '<PRODUCTNAME>', '<SPECIALBUILD>', '<<>'
  );

  // Section names for use in version information (.vi) files
  cFixedFile = 'Fixed File Info';
  cVarFile = 'Variable File Info';
  cStringFile = 'String File Info';
  cConfig = 'Configuration Details';

  // Fixed file info default values
  cDefVersionNumber: TPJVersionNumber = (V1: 0; V2: 0; V3: 0; V4: 0);
  cDefFileOS = VOS__WINDOWS32;
  cDefFileType = VFT_APP;
  cDefFileFlagsMask = $3F;
  cDefFileFlags = 0;
  cDefLanguageCode = $0809;
  cDefCharSetCode = 1252;
  cDefString = '';
  cDefIdentifier = 'VERINFO';



resourcestring
  // Default VI and RC comments
  sVIComment1 = 'Version information file';
  sVIComment2 = 'Produced by Version Information Editor from DelphiDabbler';
  sRCComment1 = 'Resource file containing VERSIONINFO resource statement';
  sRCComment2 = 'Produced by Version Information Editor from DelphiDabbler';

const
  // Record default VI and RC comments
  cDefVIComments: array[0..1] of string = (sVIComment1, sVIComment2);
  cDefRCComments: array[0..1] of string = (sRCComment1, sRCComment2);


{ TVInfo }

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
end;

procedure TVInfo.Clear;
  {Clears version info in memory. Resets to default values.
  }
var
  I: TStrInfo;  // loop control for string info
  J: Integer;   // loop control for default comments
begin
  // Reset Fixed File Info to default values
  fFileVersionNumber := cDefVersionNumber;
  fProductVersionNumber := cDefVersionNumber;
  fFileOS := cDefFileOS;
  fFileType := cDefFileType;
  fFileSubType := DefaultFileSubType(cDefFileType);
  fFileFlagsMask := cDefFileFlagsMask;
  fFileFlags := cDefFileFlags;
  // Reset variable info to default values
  fLanguageCode := cDefLanguageCode;
  fCharSetCode := cDefCharSetCode;
  // Reset string info to default values
  for I := Low(TStrInfo) to High(TStrInfo) do
    fStrInfo.Values[cStrNames[I]] := cDefString;
  // Reset identifier
  fIdentifier := cDefIdentifier;
  // Reset comment string lists to default values
  // vi comments
  fVIComments.Clear;
  for J := Low(cDefVIComments) to High(cDefVIComments) do
    fVIComments.Add(cDefVIComments[J]);
  // rc comments
  fRCComments.Clear;
  for J := Low(cDefRCComments) to High(cDefRCComments) do
    fRCComments.Add(cDefRCComments[J]);
  // default .res file folder
  fResOutputDir := '';
end;

procedure TVInfo.CopyToClipBoard;
  {Copies version information in a resource file (.rc) format to clipboard.
  }
var
  RCList: TStringList;          // string list to hold resource file code
  I: Integer;                   // loop control
  PBuf: PChar;                  // buffer to hold whole  text to copy
  BufSize: Integer;             // size of above buffer
  Line: array[0..257] of char;  // text of each line of resource file
begin
  // Give PBuf a nil value so that StrDispose will always work
  PBuf := nil;
  // Create the string list
  RCList := TStringList.Create;
  try
    // Write text of "file" to string list
    WriteAsRC(RCList);
    // Calcualte size required for buffer to hold all text
    // size 1 for terminal \0 character
    BufSize := 1;
    // itereate through lines incrementing by length of line (+ 2 for EOL)
    for I := 0 to RCList.Count - 1 do
      Inc(BufSize, Length(RCList[I]) + 2);
    // Allocate buffer of required size and set to empty string
    PBuf := StrAlloc(BufSize);
    StrCopy(PBuf, '');
    // Iterate through list of lines, appending each line to the buffer
    for I := 0 to RCList.Count - 1 do
    begin
      // copy line + CR + LF to C format string
      StrPCopy(Line, RCList[I] + #13#10);
      // append line to end of buffer
      StrCat(PBuf, Line);
    end;
    // Copy whole buffer to clipboard
    ClipBoard.SetTextBuf(PBuf);
  finally
    // Free buffer and string list
    StrDispose(PBuf);
    RCList.Free;
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
  // Set defaults
  Clear;
end;

destructor TVInfo.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fVIComments.Free;
  fRCComments.Free;
  fStrInfo.Free;
  inherited Destroy;
end;

function TVInfo.EvaluateFields(StrInfoId: TStrInfo): string;
  {Replaces all fields in a string info item by their values.
    @param StrInfoId [in] String info item to be processed.
    @return Given string info item with fields substuted by values.
  }
var
  TokenIdx: Integer;  // index of field token in string
  I: TTokens;         // loop control
begin
  // Copy string info item value to result
  Result := StrInfo[StrInfoId];
  // Scan through all tokens, searching for each one in string turn
  for I := Low(TTokens) to High(TTokens) do
    // Repeatedly check output string for presence of a field's token until all
    // instances have been replaced by value
    repeat
      // Check if field token is in result string
      TokenIdx := Pos(cFields[I], Result);
      // There is a field token, replace it by its value
      if TokenIdx > 0 then
        Replace(cFields[I], FieldValue(I), Result);
    until TokenIdx = 0;
end;

function TVInfo.FieldValue(I: TTokens): string;
  {Gets value of a field.
    @param I [in] Field identifier.
    @return Value of field.
  }
begin
  // Return result dependant on token
  case I of
    tkF1: Result := IntToStr(FileVersionNumber.V1);
    tkF2: Result := IntToStr(FileVersionNumber.V2);
    tkF3: Result := IntToStr(FileVersionNumber.V3);
    tkF4: Result := IntToStr(FileVersionNumber.V4);
    tkP1: Result := IntToStr(ProductVersionNumber.V1);
    tkP2: Result := IntToStr(ProductVersionNumber.V2);
    tkP3: Result := IntToStr(ProductVersionNumber.V3);
    tkP4: Result := IntToStr(ProductVersionNumber.V4);
    tkYEAR: Result := YearToStr(Date, True);
    tkSHORTFNAME: Result := RemoveExtension(StrInfo[siOriginalFileName]);
    tkPRODUCTNAME: Result := EvaluateFields(siProductName);
    tkSPECIALBUILD: Result := EvaluateFields(siSpecialBuild);
    tkDELIMITER: Result := '<';
  end;
end;

function TVInfo.FileFlagsAsString(const FFlag: LongInt): string;
  {Builds string describing file flags bit-set depending on whether file flags
  are being fully described or are being shown as Hex values.
    @param FFlag [in] File flags bit set.
    @return Required description string.
  }
begin
  if fDescribeFileFlags then
    // Fully describe file flags as line of symbolic constants
    Result := FileFlagSetToStr(FFlag)
  else
    // Describe file flags usingnhex notation
    Result := HexSymbol + IntToHex(FFlag, 2);
end;

function TVInfo.GetStrDesc(AnId: TStrInfo): string;
  {Read accessor for StrDesc property.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
begin
  Result := cStrDesc[AnId];
end;

function TVInfo.GetStrInfo(AnId: TStrInfo): string;
  {Read accessor for StrInfo property.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
begin
  Result := fStrInfo.Values[cStrNames[AnId]];
end;

function TVInfo.GetStrName(AnId: TStrInfo): string;
  {Read accessor for StrName property.
    @param AnId [in] String info item id.
    @return Value of property for AnId.
  }
begin
  Result := cStrNames[AnId];
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
  end;
end;

procedure TVInfo.LoadFromFile(const FileName: string);
  {Reads version information from a (.vi) file.
    @param FileName [in] Name of file to be loaded.
  }
var
  Ini: TIniFile;  // ini file instance - .vi files use ini file format
  F: TextFile;    // file identifier used to read VI comments
  I: TStrInfo;    // loop control for string info
  J: Integer;     // loop control for comments
  Line: string;   // line to receive comments
  Done: Boolean;  // flag set true when done reading comments
begin
  // ** Do not localise any string literals in this message
  // Read VI comments from file
  // open file
  AssignFile(F, FileName);
  Reset(F);
  try
    // read in comments
    fVIComments.Clear;
    Done := False;
    while not Eof(F) and not Done do
    begin
      ReadLn(F, Line);
      // if line is comment, record it, else gone past end of comments
      if Pos(';', Line) = 1 then
        fVIComments.Add(Copy(Line, 2, $FF))
      else
        Done := Pos('[', Line) = 1;
    end;
  finally
    // Close the file
    CloseFile(F);
  end;
  // Create instance of ini file to use to write ini file style data
  Ini := TIniFile.Create(FileName);
  try
    // Read version info stuff into properties: this automatically verifies data
    // read in fixed file info
    FileVersionNumber := StrToVersionNumber(
        Ini.ReadString(cFixedFile, 'File Version #',
        VersionNumberToStr(cDefVersionNumber)));
    ProductVersionNumber := StrToVersionNumber(
        Ini.ReadString(cFixedFile, 'Product Version #',
        VersionNumberToStr(cDefVersionNumber)));
    FileOS := Ini.ReadInteger(cFixedFile, 'File OS', cDefFileOS);
    FileType := Ini.ReadInteger(cFixedFile, 'File Type', cDefFileType);
    FileSubType := Ini.ReadInteger(cFixedFile, 'File Sub-Type',
        DefaultFileSubType(FileType));
    FileFlagsMask := Ini.ReadInteger(cFixedFile, 'File Flags Mask',
        cDefFileFlagsMask);
    FileFlags := Ini.ReadInteger(cFixedFile, 'File Flags', cDefFileFlags);
    // read in variable file info
    LanguageCode := Ini.ReadInteger(cVarFile, 'Language', cDefLanguageCode);
    CharSetCode := Ini.ReadInteger(cVarFile, 'Character Set', cDefCharSetCode);
    // read in string info
    for I := Low(TStrInfo) to High(TStrInfo) do
      StrInfo[I] := Ini.ReadString(cStringFile, StrDesc[I], cDefString);
    // Read config section
    // read identifier
    Identifier := Ini.ReadString(cConfig, 'Identifier', cDefIdentifier);
    // read RC comments - stripping | characters (used to preserve indentation)
    fRCComments.Clear;
    for J := 0 to Ini.ReadInteger(cConfig, 'NumRCComments', 0) - 1 do
    begin
      Line := Ini.ReadString(cConfig, Format('RC Comment Line %d', [J]), '');
      if (Length(Line) > 0) and (Line[1] = '|') then
        Line := Copy(Line, 2, Length(Line) -1);
      fRCComments.Add(Line);
    end;
    // read default .res file output folder
    fResOutputDir := Ini.ReadString(cConfig, 'ResOutputDir', '');
  finally
    // Free the instance of the ini file
    Ini.Free;
  end;
end;

procedure TVInfo.SaveToFile(const FileName: string);
  {Saves version information to a version information (.vi) file.
    @param FileName [in] Name of saved file.
  }
var
  Ini: TIniFile;  // ini file instance - .vi files use ini file format
  F: TextFile;    // file identifier used to read VI comments
  I: TStrInfo;    // loop control for string info
  J: Integer;     // loop control for comments
begin
  // ** Do not localise any string literals in this message
  // Read VI comments from file if there are any
  if fVIComments.Count > 0 then
  begin
    // associate name of required file with file identifier
    AssignFile(F, FileName);
    // open file for writing
    Rewrite(F);
    try
      // write out all comments preceeded by ';' char
      for J := 0 to fVIComments.Count - 1 do
        WriteLn(F, Format(';%s', [fVIComments[J]]));
    finally
      // close the file
      CloseFile(F);
    end;
  end;
  // Create an instance of the ini file class using the required file name
  Ini := TIniFile.Create(FileName);
  try
    // Write version information
    // write fixed file info
    Ini.WriteString(cFixedFile, 'File Version #',
        VersionNumberToStr(FileVersionNumber));
    Ini.WriteString(cFixedFile, 'Product Version #',
        VersionNumberToStr(ProductVersionNumber));
    Ini.WriteInteger(cFixedFile, 'File OS', FileOS);
    Ini.WriteInteger(cFixedFile, 'File Type', FileType);
    Ini.WriteInteger(cFixedFile, 'File Sub-Type', FileSubType);
    Ini.WriteInteger(cFixedFile, 'File Flags Mask', FileFlagsMask);
    Ini.WriteInteger(cFixedFile, 'File Flags', FileFlags);
    // write variable file info
    Ini.WriteInteger(cVarFile, 'Language', LanguageCode);
    Ini.WriteInteger(cVarFile, 'Character Set', CharSetCode);
    // write string info
    for I := Low(TStrInfo) to High(TStrInfo) do
      Ini.WriteString(cStringFile, StrDesc[I], StrInfo[I]);
    // Write config section
    // write identifier
    Ini.WriteString(cConfig, 'Identifier', Identifier);
    // write RC comments: first # of lines then write lines preceded by |
    Ini.WriteInteger(cConfig, 'NumRCComments', fRCComments.Count);
    for J := 0 to fRCComments.Count - 1 do
    begin
      Ini.WriteString(cConfig, Format('RC Comment Line %d', [J]),
          '|' + fRCComments[J]);
    end;
    // write .res file default output folder
    Ini.WriteString(cConfig, 'ResOutputDir', fResOutputDir);
  finally
    // Free the ini file instance
    Ini.Free;
  end;
end;

procedure TVInfo.SaveToResourceFile(const FileName: string);
  {Saves version information to a resource (.rc) file.
    @param FileName [in] Name of output file.
  }
var
  I: Integer;           // loop control
  RCList: TStringList;  // string list containing lines of code for file
  F: TextFile;          // identifiers the text file
begin
  // Create string list to hold resource text and write contents of file to it
  RCList := TStringList.Create;
  WriteAsRC(RCList);
  try
    // Open file for writing
    AssignFile(F, FileName);
    try
      Rewrite(F);
      // Write resource file lines to file
      for I := 0 to RCList.Count - 1 do
        WriteLn(F, RCList[I]);
    finally
      // Close the file
      CloseFile(F);
    end;
  finally
    // Free the string list
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
    fCharSetCode := cDefCharSetCode;
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
    fFileFlagsMask := cDefFileFlagsMask;
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
    fFileOS := cDefFileOS;
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
    fFileType := cDefFileType;
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
    fLanguageCode := cDefLanguageCode;
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
    fStrInfo.Values[cStrNames[AnId]] := AStr
  else
    fStrInfo.Values[cStrNames[AnId]] := '';
end;

procedure TVInfo.SetVIComments(SL: TStringList);
  {Write access method for VIComments property.
    @param SL [in] String list containing new property value.
  }
begin
  fVIComments.Assign(SL);
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
    if not (I in cExcludeFields[Id]) then
      SL.Add(cFields[I]);
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
  // Ensure the VerInfo routines use C format hex symbol
  UsePasHexSymbol(False);
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
  // Write VERSIONINFO statement with #define for identifier if required
  if Identifier <> '' then
  begin
    SL.Add(Format('#define %s 1', [Identifier]));
    SL.Add('');
    SL.Add(Format('%s VERSIONINFO', [Identifier]));
  end
  else
    SL.Add('1 VERSIONINFO');
  // Write Fixed File Info
  SL.Add('FILEVERSION ' + VersionNumberToStr(FileVersionNumber));
  SL.Add('PRODUCTVERSION ' + VersionNumberToStr(ProductVersionNumber));
  // write File Flags Mask in hex format
  SL.Add('FILEFLAGSMASK ' + HexSymbol + IntToHex(FileFlagsMask, 2));
  if FileFlagSetToStr(FileFlags) <> '' then
    // there is a non-empty File Flags bit set: write using symbolic constants
    SL.Add('FILEFLAGS ' + FileFlagSetToStr(FileFlags));
  SL.Add('FILEOS ' + FileOSToStr(FileOS));
  SL.Add('FILETYPE ' + FileTypeToStr(FileType));
  if FileTypeHasSubType(FileType) then
    // file type has a sub-type - write it out
    SL.Add('FILESUBTYPE ' + FileSubTypeToStr(FileType, FileSubType));
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
      SL.Add(Format('   VALUE "%s", "%s\000"',
        [StrName[I], EvaluateFields(I)]));
  end;
  SL.Add('  }');
  SL.Add(' }');
  // Write out variable file info block
  SL.Add(' BLOCK "VarFileInfo"');
  SL.Add(' {');
  SL.Add(Format('  VALUE "Translation", %s%4.4x, %d',
    [HexSymbol, fLanguageCode, fCharSetCode]));
  SL.Add(' }');
  SL.Add('}');
end;

procedure TVInfo.WriteToDisplay(const S: TStrings);
  {Writes version information in format ready for display.
    @param S [in] String list that receives version information in display
      format.
  }
var
  I: TStrInfo;  // loop control for string info
resourcestring
  sFFI = 'FIXED FILE INFO';
  sFileVersion = 'File Version #';
  sProductVersion = 'Product Version #';
  sFileOS = 'File OS';
  sFileType = 'File Type';
  sFileSubType = 'File Sub-type';
  sFileFlagsMask = 'File Flags Mask';
  sFileFlags = 'File Flags';
  sLanguage = 'Language';
  sCharSet = 'Character Set';
  sTransInfo = 'TRANSLATION INFO';
  sStringInfo = 'STRING INFO';
begin
  // Ensure the VerUtils routines use Pascal Hex symbol for output
  UsePasHexSymbol(True);
  // Clear the given list
  S.Clear;
  // Add Fixed File Info items to list
  S.Add(sFFI);
  S.Add('   ' + sFileVersion + #9 + VersionNumberToStr(fFileVersionNumber));
  S.Add(
    '   ' + sProductVersion + #9 + VersionNumberToStr(fProductVersionNumber)
  );
  S.Add('   ' + sFileOS + #9 + FileOSToStr(fFileOS));
  S.Add('   ' + sFileType + #9 + FileTypeToStr(fFileType));
  S.Add('   ' + sFileSubType + #9 + FileSubTypeToStr(fFileType, fFileSubType));
  S.Add('   ' + sFileFlagsMask + #9 + FileFlagsAsString(fFileFlagsMask));
  S.Add('   ' + sFileFlags + #9 + FileFlagsAsString(fFileFlags));
  // Add Variable Info to list
  S.Add(sTransInfo);
  S.Add('   ' + sLanguage + #9 + LangCodeToStr(fLanguageCode));
  S.Add('   ' + sCharSet + #9 + CharCodeToStr(fCharSetCode));
  // Add String info to list
  S.Add(sStringInfo);
  for I := Low(TStrInfo) to High(TStrInfo) do
    S.Add('   ' + StrDesc[I] + #9 + StrInfo[I]);
end;

end.

