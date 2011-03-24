{
 * UVerUtils.pas
 *
 * Version information utility routines and look-up tables. Provides conversions
 * between version information codes and text descriptions etc.
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
 * The Original Code is UVerUtils.pas.
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


unit UVerUtils;


interface


uses
  // Delphi
  Classes, SysUtils,
  // PJSoft library
  PJVersionInfo;


{ --- Output format routines --- }

procedure UsePasHexSymbol(AValue: Boolean);
  {Sets current hex symbol.
    @param AValue [in] True to use Pascal style symbol or false to use C style
      symbol.
  }
function HexSymbol: string;
  {Gets hex symbol currently in use.
    @return Current hex symbol.
  }


{ --- Version number routines --- }

function VersionNumberToStr(const VN: TPJVersionNumber): string;
  {Converts a version number into a string that can be used in a VERSIONINFO
  resource statement.
    @param VN [in] Version number to convert.
    @return Required string representation of version number.
  }
function StrToVersionNumber(const Str: string): TPJVersionNumber;
  {Converts string in format used in VERSIONINFO resource statement to
  TPJVersionNumber type.
    @param Str [in] String representation of version number.
    @return Required TPJVersion information structure.
  }


{ --- File OS routines --- }

function FileOSToStr(const OS: LongInt): string;
  {Gets symbolic constant representing an OS code.
    @param OS [in] OS code.
    @return Matching symbolic constant or '' if OS is not recognised.
  }
function StrToFileOS(const Str: string): LongInt;
  {Converts an OS symbolic constant into the equivalent code.
    @param Str [in] Symbolic constant.
    @return OS code of symbolic constant.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
function FileOSCodeList(const SL: TStringList): TStringList;
  {Builds a list of OS symbolic constants.
    @param SL [in] Receives symbolic constant list.
    @return Reference to SL.
  }
function ValidFileOS(const OS: LongInt): Boolean;
  {Checks if an OS code is valid.
    @param OS [in] OS code to check.
    @return True if OS is valid, False if not.
  }


{ --- File Type routines --- }

function FileTypeToStr(const FType: LongInt): string;
  {Gets the symbolic constant for a file type.
    @param FType [in] File type code.
    @return Symbolic constant or '' if FType not recognised.
  }
function StrToFileType(const Str: string): LongInt;
  {Converts a file type symbolic constant into its value.
    @param Str [in] Symbolic constant.
    @return Code associated with symbolic constant.
    @except EVersionError raised if symbolic constant not recognised.
  }
function FileTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of file type symbolic constants.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
function ValidFileType(const FType: LongInt): Boolean;
  {Checks if a file type is valid.
    @param FType [in] File type to check.
    @return True if the FType is valid, False otherwise.
  }
function FileTypeHasSubType(const FType: LongInt): Boolean;
  {Checks if a file type has sub-types.
    @param FType [in] File type to check.
    @return True if given FType has sub-types, False if not.
  }


{ --- File Sub-Type routines --- }

function FileSubTypeToStr(const FType, FSType: LongInt): string;
  {Get the symbolic constant representing a file sub type within a file type.
    @param FType [in] File type.
    @param FSType [in] File sub-type for which symbolic constant required.
    @return Required symbolic constant, or hex string.
  }
function ValidFileSubType(const FType, FSubType: LongInt): Boolean;
  {Checks if a sub-type is valid for a file type.
    @param FType [in] File type.
    @param FSubType [in] File sub-type.
    @return True if FSubType is valid sub-type of FType or if FType has no
      sub-types and FSubType is zero.
  }
function DefaultFileSubType(const FType: LongInt): LongInt;
  {Gets the default sub type code for a file type.
    @param FType [in] File type for which default value is required.
    @return Default value.
  }
function StrToFileSubType(const FType: LongInt; const Str: string): LongInt;
  {Converts a sub-type symbolic constant to its equivalent code.
    @param FType [in] Specifies type that sub-type belongs to.
    @param Str [in] Symbolic constant or hex digits depending on type.
    @return Required code.
  }
function DriverSubTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of drv sub-type symbolic constants.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
function FontSubTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of all font sub-types symbolic constants.
    @param SL [in] Receives list of sub-types.
    @return Reference to SL.
  }


{ --- File Flags routines --- }

function FileFlagSetToStr(const FFlags: LongInt): string;
  {Builds a string containing a '+' delimited string of symbolic constants of
  file flags contained in a bitmask.
    @param FFlags [in] File flags bitmask.
    @return Required list of symbolic constants.
  }
function StrToFileFlag(const Str: string): LongInt;
  {Gets the file flag code associated with a symbolic constant.
    @param Str [in] Symbolic constant.
    @return Associated file flag.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
function FileFlagSetToStrList(const FFlags: LongInt;
  const SL: TStringList): TStringList;
  {Converts a file flags bitmask into a list of the symbolic constants included
  in the mask.
    @param FFlags [in] Bitmask of file flags.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
function StrListToFileFlagSet(const SL: TStringList): LongInt;
  {Converts a list of file flag symbolic constants to a bit mask.
    @param SL [in] List of symbolic constants.
    @return Equivalent bitmask.
  }
function ValidFileFlagsMask(const Mask: LongInt): Boolean;
  {Checks if a file flags mask contains valid File Flag codes.
    @param Mask [in] Mask to be checked.
    @return True if Mask is valid, False if not.
  }
function ValidFileFlags(const FFlags, Mask: LongInt): Boolean;
  {Checks if a bit mask of file flags is a subset of a file flags mask.
    @param FFlags [in] Flags to test against mask.
    @param Mask [in] Bitmask that must include all flags.
    @return True if FFlags is a sub-set of Mask, False otherwise.
  }


{ --- Language routines --- }

function LangCodeToStr(const LangCode: Word): string;
  {Gets description of language specified by a code.
    @param LangCode [in] Language code.
    @return Description of LangCode or '' if no matching description.
  }
function StrToLangCode(const Str: string): Word;
  {Gets the language code that correlates with a description.
    @param Str [in] Language description.
    @return Code of language described by Str.
    @except EVersionError raised if language description doesn't exist.
  }
function LanguageStrList(const SL: TStringList): TStringList;
  {Puts list of language descriptions into a string list.
    @param SL [in] Receives language descriptions.
    @return Reference to SL.
  }
function ValidLangCode(const LangCode: Word): Boolean;
  {Checks if a language code is valid.
    @param LangCode [in] Language code to be checked.
    @return True if LangCode is valid, False if not.
  }


{ --- Character Set routines --- }

function CharCodeToStr(const CharCode: Word): string;
  {Gets the description of a character code.
    @param CharCode [in] Character code for which description required.
    @return Required description or '' if no description found.
  }
function StrToCharCode(const Str: string): Word;
  {Gets character code associated with a description.
    @param Str [in] Character set description.
    @return Related character code.
    @except EVersionError raised if description doesn't exist.
  }
function CharSetStrList(const SL: TStringList): TStringList;
  {Puts list of character set descriptions into a string list.
    @param SL [in] Receives character set description.
    @return Reference to SL.
  }
function ValidCharCode(const CharCode: Word): Boolean;
  {Checks if a character code is valid.
    @param CharCode [in] Character code to check.
    @return True if CharCode is valid, False if not.
  }


type

  {
  EVersionError:
    Exception class for version information errors.
  }
  EVersionError = class(Exception);


implementation


uses
  // Delphi
  Windows,
  // Project
  UUtils;


const
  cHexSymbols: array[Boolean] of string[2] = ('0x', '$');
    {Possible symbols to use to indicate hex strings - 'C' and 'Pascal' styles}
var
  pvtUsePasHexSymbol: Boolean = True;
    {Flag true if Pascal style hex symbols being used - false if C style}


{ --- Error handling ---- }

resourcestring
  { Error messages }
  sErrOSCode = 'OS code "%s" not known';
  sErrFileType = 'File type code "%s" not known';
  sErrDriverSubType = 'Driver sub-type code "%s" not known';
  sErrFileFlag = 'File flag "%s" not known';
  sErrLanguage = 'Language description "%s" not known';
  sErrCharSet = 'Character set description "%s" not known';

procedure Error(const Msg: string); overload;
  {Raises exception with a message.
    @param Msg [in] Exception message.
    @except EVersionError raised.
  }
begin
  raise EVersionError.Create(Msg);
end;

procedure Error(const Msg: string; const Args: array of const); overload;
  {Raises exception with a formatted message.
    @param Msg [in] Format string for message.
    @param Args [in] Arguments to be included in message.
    @except EVersionError raised.
  }
begin
  Error(Format(Msg, Args));
end;


{ --- Output format routines --- }

procedure UsePasHexSymbol(AValue: Boolean);
  {Sets current hex symbol.
    @param AValue [in] True to use Pascal style symbol or false to use C style
      symbol.
  }
begin
  // Update symbol flag
  pvtUsePasHexSymbol := AValue;
end;

function HexSymbol: string;
  {Gets hex symbol currently in use.
    @return Current hex symbol.
  }
begin
  Result := cHexSymbols[pvtUsePasHexSymbol];
end;


{ --- Version numbers --- }

function VersionNumberToStr(const VN: TPJVersionNumber): string;
  {Converts a version number into a string that can be used in a VERSIONINFO
  resource statement.
    @param VN [in] Version number to convert.
    @return Required string representation of version number.
  }
begin
  Result := Format('%d, %d, %d, %d', [VN.V1, VN.V2, VN.V3, VN.V4]);
end;

function StrToVersionNumber(const Str: string): TPJVersionNumber;
  {Converts string in format used in VERSIONINFO resource statement to
  TPJVersionNumber type.
    @param Str [in] String representation of version number.
    @return Required TPJVersion information structure.
  }
var
  Start: Integer;               // cursor to walk along string}
  NumStr: string;               // current number in string}
  Nums: array[1..4] of LongInt; // array to hold numbers from string}
  I: Integer;                   // loop control variable}
begin
  // Start at first location in string
  Start := 1;
  // Itereate for each version number there are four in string
  for I := 1 to 4 do
  begin
    // Copy first "field" in comma delimted string to NumStr and trim spaces
    NextField(Str, Start, NumStr, ',');
    NumStr := TrimSpaces(NumStr);
    // Convert NumStr into the required number - deafult 0 if no valid value
    if NumStr <> '' then
      Nums[I] := StrToIntDef(NumStr, 0)
    else
      Nums[I] := 0;
  end;
  // Copy resulting numbers into TPJVersionInfo structure
  Result.V1 := Nums[1];
  Result.V2 := Nums[2];
  Result.V3 := Nums[3];
  Result.V4 := Nums[4];
end;


{ --- File OS --- }

const
  // Array of OS codes to symbolic constants
  cFileOS: array[0..6] of record
    Code: LongInt;        // the FileOS code number
    CodeStr: string[20];  // the symbolic constant as a string
  end =
  (
    (Code: VOS_DOS;           CodeStr: 'VOS_DOS';           ),
    (Code: VOS_NT;            CodeStr: 'VOS_NT';            ),
    (Code: VOS__WINDOWS16;    CodeStr: 'VOS__WINDOWS16';    ),
    (Code: VOS__WINDOWS32;    CodeStr: 'VOS__WINDOWS32';    ),
    (Code: VOS_DOS_WINDOWS16; CodeStr: 'VOS_DOS_WINDOWS16'; ),
    (Code: VOS_DOS_WINDOWS32; CodeStr: 'VOS_DOS_WINDOWS32'; ),
    (Code: VOS_NT_WINDOWS32;  CodeStr: 'VOS_NT_WINDOWS32';  )
  );

function FileOSToStr(const OS: LongInt): string;
  {Gets symbolic constant representing an OS code.
    @param OS [in] OS code.
    @return Matching symbolic constant or '' if OS is not recognised.
  }
var
  I: Integer; // loop control
begin
  // Set default '' result
  Result := '';
  // Scan array looking for given code
  for I := 0 to 6 do
    if cFileOS[I].Code = OS then
    begin
      // Found the code, record the symbolic constant name and stop looking
      Result := cFileOS[I].CodeStr;
      Break;
    end;
end;

function StrToFileOS(const Str: string): LongInt;
  {Converts an OS symbolic constant into the equivalent code.
    @param Str [in] Symbolic constant.
    @return OS code of symbolic constant.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
var
  I: Integer;     // loop control
  Found: Boolean; // flag true if symbolic constant found
begin
  // Set default "not found" result
  Found := False;
  // Scan array looking for given symbolic constant string (case sensitive)
  for I := 0 to 6 do
    if cFileOS[I].CodeStr = Str then
    begin
      // Found symbolic constant - record FileOS code & leave loop
      Result := cFileOS[I].Code;
      Found := True;
      Break;
    end;
  // Check if constant found and raise exception if not
  if not Found then
    Error(sErrOSCode, [Str]);
end;

function FileOSCodeList(const SL: TStringList): TStringList;
  {Builds a list of OS symbolic constants.
    @param SL [in] Receives symbolic constant list.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the string list
  SL.Clear;
  // Iterate across all FileOS codes, adding symbolic constants to list
  for I := 0 to 6 do
    Sl.Add(cFileOS[I].CodeStr);
  // Return a reference to the given list
  Result := SL;
end;

function ValidFileOS(const OS: LongInt): Boolean;
  {Checks if an OS code is valid.
    @param OS [in] OS code to check.
    @return True if OS is valid, False if not.
  }
begin
  // Try to convert code to symbolic constant - this returns '' if not found
  Result := (FileOSToStr(OS) <> '');
end;


{ --- File type --- }

const
  // Map of file type code to symbolic constants and descriptions.
  cFileType: array[0..6] of record
    Code: LongInt;        // FileType code
    CodeStr: string[15];  // symbolic constant as a string
  end =
  (
    (Code: VFT_UNKNOWN;     CodeStr: 'VFT_UNKNOWN';     ),
    (Code: VFT_APP;         CodeStr: 'VFT_APP';         ),
    (Code: VFT_DLL;         CodeStr: 'VFT_DLL';         ),
    (Code: VFT_DRV;         CodeStr: 'VFT_DRV';         ),
    (Code: VFT_FONT;        CodeStr: 'VFT_FONT';        ),
    (Code: VFT_VXD;         CodeStr: 'VFT_VXD';         ),
    (Code: VFT_STATIC_LIB;  CodeStr: 'VFT_STATIC_LIB';  )
  );

function FileTypeToStr(const FType: LongInt): string;
  {Gets the symbolic constant for a file type.
    @param FType [in] File type code.
    @return Symbolic constant or '' if FType not recognised.
  }
var
  I: Integer; // loop control
begin
  // Set default result to empty string
  Result := '';
  // Iterate across all table of FileType codes
  for I := 0 to 6 do
    if cFileType[I].Code = FType then
    begin
      // Found code - record symbolic constant string and leave loop
      Result := cFileType[I].CodeStr;
      Break;
    end;
end;

function StrToFileType(const Str: string): LongInt;
  {Converts a file type symbolic constant into its value.
    @param Str [in] Symbolic constant.
    @return Code associated with symbolic constant.
    @except EVersionError raised if symbolic constant not recognised.
  }
var
  I: Integer;     // loop control
  Found: Boolean; // flag true if symbolic constant found
begin
  // Set default "not found" result
  Found := False;
  // Scan through FileType table doing case sensitve search
  for I := 0 to 6 do
    if cFileType[I].CodeStr = Str then
    begin
      // Found the symbolic constant - record and leave loop
      Result := cFileType[I].Code;
      Found := True;
      Break;
    end;
  if not Found then
    // Raise execption since constant not found
    Error(sErrFileType, [Str]);
end;

function FileTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of file type symbolic constants.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the list
  SL.Clear;
  // Iterate across table putting all symbolic constant names in list
  for I := 0 to 6 do
    Sl.Add(cFileType[I].CodeStr);
  // Return reference to list
  Result := SL;
end;

function FileTypeHasSubType(const FType: LongInt): Boolean;
  {Checks if a file type has sub-types.
    @param FType [in] File type to check.
    @return True if given FType has sub-types, False if not.
  }
begin
  // Check to see if file type is in list of file types with sub-types
  Result := FType in [VFT_DRV, VFT_FONT, VFT_VXD];
end;

function ValidFileType(const FType: LongInt): Boolean;
  {Checks if a file type is valid.
    @param FType [in] File type to check.
    @return True if the FType is valid, False otherwise.
  }
begin
  // Check to see if code converts to a symbolic constant - result is '' if not
  Result := (FileTypeToStr(FType) <> '');
end;


{ --- File sub type --- }

function PvtDriveSubTypeToStr(const FSType: LongInt): string; forward;
  {Converts a driver sub-type code to a symbolic constant.
    @param FSType [in] Driver sub-type code.
    @return Related symbolic constant or '' if FSType is not recognised.
  }
function PvtFontSubTypeToStr(const FSType: LongInt): string; forward;
  {Converts a font sub-type code to its symbolic constant.
    @param FSType [in] Font sub-type code.
    @return Associated symbolic constant or '' if FSType is not recognised.
  }

function FileSubTypeToStr(const FType, FSType: LongInt): string;
  {Get the symbolic constant representing a file sub type within a file type.
    @param FType [in] File type.
    @param FSType [in] File sub-type for which symbolic constant required.
    @return Required symbolic constant, or hex string.
  }
begin
  case FType of
    VFT_DRV: Result := PvtDriveSubTypeToStr(FSType);
    VFT_FONT: Result := PvtFontSubTypeToStr(FSType);
    else Result := HexSymbol + IntToHex(FSType, 4);
  end;
end;

function ValidFileSubType(const FType, FSubType: LongInt): Boolean;
  {Checks if a sub-type is valid for a file type.
    @param FType [in] File type.
    @param FSubType [in] File sub-type.
    @return True if FSubType is valid sub-type of FType or if FType has no
      sub-types and FSubType is zero.
  }
begin
  case FType of
    VFT_DRV:
      // Driver symbolic constant is '' when sub-type code is not valid
      Result := (PvtDriveSubTypeToStr(FSubType) <> '');
    VFT_FONT:
      // Font symbolic constant is '' when sub-type code is not valid
      Result := (PvtFontSubTypeToStr(FSubType) <> '');
    VFT_VXD:
      // All values can be valid for this File Type
      Result := True;
    else
      // All other File Types don't have sub-types so only valid value is 0
      Result := (FSubType = 0);
  end;
end;

function DefaultFileSubType(const FType: LongInt): LongInt;
  {Gets the default sub type code for a file type.
    @param FType [in] File type for which default value is required.
    @return Default value.
  }
begin
  case FType of
    VFT_DRV, VFT_FONT: Result := VFT2_UNKNOWN;
    else Result := 0;
  end;
end;

function PvtStrToDriveSubType(const Str: string): LongInt; forward;
  {Converts a symbolic constant to the equivalent driver sub-type code.
    @param Str [in] Symbolic constant to convert.
    @return Equivalent driver sub-type code.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
function PvtStrToFontSubType(const Str: string): LongInt; forward;
  {Converts a symbolic constant to font sub-type code.
    @param Str [in] Symbolic constant.
    @return Code matching symbolic constant.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }

function StrToFileSubType(const FType: LongInt; const Str: string): LongInt;
  {Converts a sub-type symbolic constant to its equivalent code.
    @param FType [in] Specifies type that sub-type belongs to.
    @param Str [in] Symbolic constant or hex digits depending on type.
    @return Required code.
  }
begin
  case FType of
    VFT_DRV: Result := PvtStrToDriveSubType(Str);
    VFT_FONT: Result := PvtStrToFontSubType(Str);
    else Result := StrToInt('$' + Str);
  end;
end;


{ Driver sub type }

const
  // Maps driver sub-type code to symbolic constants.
  cDrvSubType: array[0..10] of record
    Code: LongInt;        // driver sub-type code
    CodeStr: string[22];  // symbolic constant for code
  end =
  (
    (Code: VFT2_UNKNOWN;          CodeStr: 'VFT2_UNKNOWN';          ),
    (Code: VFT2_DRV_COMM;         CodeStr: 'VFT2_DRV_COMM';         ),
    (Code: VFT2_DRV_PRINTER;      CodeStr: 'VFT2_DRV_PRINTER';      ),
    (Code: VFT2_DRV_KEYBOARD;     CodeStr: 'VFT2_DRV_KEYBOARD';     ),
    (Code: VFT2_DRV_LANGUAGE;     CodeStr: 'VFT2_DRV_LANGUAGE';     ),
    (Code: VFT2_DRV_DISPLAY;      CodeStr: 'VFT2_DRV_DISPLAY';      ),
    (Code: VFT2_DRV_MOUSE;        CodeStr: 'VFT2_DRV_MOUSE';        ),
    (Code: VFT2_DRV_NETWORK;      CodeStr: 'VFT2_DRV_NETWORK';      ),
    (Code: VFT2_DRV_SYSTEM;       CodeStr: 'VFT2_DRV_SYSTEM';       ),
    (Code: VFT2_DRV_INSTALLABLE;  CodeStr: 'VFT2_DRV_INSTALLABLE';  ),
    (Code: VFT2_DRV_SOUND;        CodeStr: 'VFT2_DRV_SOUND';        )
  );

function PvtDriveSubTypeToStr(const FSType: LongInt): string;
  {Converts a driver sub-type code to a symbolic constant.
    @param FSType [in] Driver sub-type code.
    @return Related symbolic constant or '' if FSType is not recognised.
  }
var
  I: Integer; // loop control
begin
  // Set default result
  Result := '';
  // Iterate over table of driver sub-types
  for I := 0 to 10 do
    if cDrvSubType[I].Code = FSType then
    begin
      // Found the code record symbolic constant and exit loop
      Result := cDrvSubType[I].CodeStr;
      Break;
    end;
end;

function PvtStrToDriveSubType(const Str: string): LongInt;
  {Converts a symbolic constant to the equivalent driver sub-type code.
    @param Str [in] Symbolic constant to convert.
    @return Equivalent driver sub-type code.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
var
  I: Integer;     // loop control
  Found: Boolean; // flag showing if symbolic constant is found is table
begin
  // Set default "not found" result
  Found := False;
  // Iterate over table of driver codes doing case sensitive search
  for I := 0 to 10 do
    if cDrvSubType[I].CodeStr = Str then
    begin
      // Found symbolic constant - resorde it and that found and leave loop
      Result := cDrvSubType[I].Code;
      Found := True;
      Break;
    end;
  if not Found then
    // Raise execption because symbolic constant not in table
    Error(sErrDriverSubType, [Str]);
end;

function DriverSubTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of drv sub-type symbolic constants.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the list
  SL.Clear;
  // Add all symbolic constant names to the list
  for I := 0 to 10 do
    SL.Add(cDrvSubType[I].CodeStr);
  // Return a reference to the list
  Result := SL;
end;


{ Font sub type }

const
  // Map of Font Sub-Type codes to their symbolic constants
  cFontSubType: array[0..3] of record
    Code: LongInt;        // the font sub-type code
    CodeStr: string[20];  // symbolic constants for the sub-type code
    Desc: string[20];     // (UNUSED v1.0) description of the sub-type code
  end =
  (
    (Code: VFT2_UNKNOWN;        CodeStr: 'VFT2_UNKNOWN';        ),
    (Code: VFT2_FONT_RASTER;    CodeStr: 'VFT2_FONT_RASTER';    ),
    (Code: VFT2_FONT_VECTOR;    CodeStr: 'VFT2_FONT_VECTOR';    ),
    (Code: VFT2_FONT_TRUETYPE;  CodeStr: 'VFT2_FONT_TRUETYPE';  )
  );

function PvtFontSubTypeToStr(const FSType: LongInt): string;
  {Converts a font sub-type code to its symbolic constant.
    @param FSType [in] Font sub-type code.
    @return Associated symbolic constant or '' if FSType is not recognised.
  }
var
  I: Integer; // loop control
begin
  // Record default '' result
  Result := '';
  // Itereate across table searching for code
  for I := 0 to 3 do
    if cFontSubType[I].Code = FSType then
    begin
      // Found code - record symbolic constant end exit loop
      Result := cFontSubType[I].CodeStr;
      Break;
    end;
end;

function PvtStrToFontSubType(const Str: string): LongInt;
  {Converts a symbolic constant to font sub-type code.
    @param Str [in] Symbolic constant.
    @return Code matching symbolic constant.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
var
  I: Integer;     // loop contol
  Found: Boolean; // flag recording if symbolic constant found
begin
  // Set default "not found" result
  Found := False;
  // Iterate across table doing case-sensitive search for symbolic constant
  for I := 0 to 3 do
    if cFontSubType[I].CodeStr = Str then
    begin
      // Found symbolic constant - record code and that found and leave loop
      Result := cFontSubType[I].Code;
      Found := True;
      Break;
    end;
  if not Found then
    // Raise exception because symbolic constnat not found
    Error(sErrDriverSubType, [Str]);
end;

function FontSubTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of all font sub-types symbolic constants.
    @param SL [in] Receives list of sub-types.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the list
  SL.Clear;
  // Scan table adding symbolic constant names to list
  for I := 0 to 3 do
    Sl.Add(cFontSubType[I].CodeStr);
  // Return reference to the list
  Result := SL;
end;


{ --- File flags --- }

const
  // Map of file flag codes to the equivalent symbolic constants
  cFileFlags: array[0..5] of record
    Code: LongInt;        // File Flags codes
    CodeStr: string[20];  // Symbolic constants for File Flags codes
  end =
  (
    (Code: vs_FF_Debug;         CodeStr: 'VS_FF_DEBUG'        ),
    (Code: vs_FF_Prerelease;    CodeStr: 'VS_FF_PRERELEASE'   ),
    (Code: vs_FF_Patched;       CodeStr: 'VS_FF_PATCHED'      ),
    (Code: vs_FF_PrivateBuild;  CodeStr: 'VS_FF_PRIVATEBUILD' ),
    (Code: vs_FF_InfoInferred;  CodeStr: 'VS_FF_INFOINFERRED' ),
    (Code: vs_FF_SpecialBuild;  CodeStr: 'VS_FF_SPECIALBUILD' )
  );

function FileFlagSetToStr(const FFlags: LongInt): string;
  {Builds a string containing a '+' delimited string of symbolic constants of
  file flags contained in a bitmask.
    @param FFlags [in] File flags bitmask.
    @return Required list of symbolic constants.
  }
var
  I: Integer;     // loop control
  Count: Integer; // count of number of flags in set to date
begin
  // Set default "empty set" result
  Result := '';
  // Record that no file flags inset to date
  Count := 0;
  // Iterate across file flags table looking for codes
  for I := 0 to 5 do
    if (cFileFlags[I].Code and FFlags) = cFileFlags[i].Code then
    begin
      // Found a code
      // increment counter and add symbolic constant to string
      // preceed code by delimiter if code not the first
      Inc(Count);
      if Count = 1 then
        Result := cFileFlags[I].CodeStr
      else
        Result := Result + ' + ' + cFileFlags[I].CodeStr;
    end;
end;

function StrToFileFlag(const Str: string): LongInt;
  {Gets the file flag code associated with a symbolic constant.
    @param Str [in] Symbolic constant.
    @return Associated file flag.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
var
  I: Integer;     // loop control
  Found: Boolean; // flag true if symbolic constant found
begin
  // Set default "not found" result
  Found := False;
  // Iterate across all table doing case sensitive search for symbolic constant
  for I := 0 to 5 do
    if cFileFlags[I].CodeStr = Str then
    begin
      // Found it - record code and that found and skip from loop
      Result := cFileFlags[I].Code;
      Found := True;
      Break;
    end;
  if not Found then
    // Not found so raise exception
    Error(sErrFileFlag, [Str]);
end;

function FileFlagSetToStrList(const FFlags: LongInt;
  const SL: TStringList): TStringList;
  {Converts a file flags bitmask into a list of the symbolic constants included
  in the mask.
    @param FFlags [in] Bitmask of file flags.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the list
  SL.Clear;
  // Itereate over all table looking for codes in bit-set
  for I := 0 to 5 do
    if (cFileFlags[I].Code and FFlags) = cFileFlags[i].Code then
      // The code's in the bit-set - add symbolic constant to list
      SL.Add(cFileFlags[I].CodeStr);
  // Return reference to list
  Result := SL;
end;

function StrListToFileFlagSet(const SL: TStringList): LongInt;
  {Converts a list of file flag symbolic constants to a bit mask.
    @param SL [in] List of symbolic constants.
    @return Equivalent bitmask.
  }
var
  I: Integer; // loop control
begin
  // Set default result - empty set
  Result := 0;
  // Itereate across list including code for each symbolic constant in bit-set
  for I := 0 to SL.Count - 1 do
    Result := Result or StrToFileFlag(SL[I]);
end;

function ValidFileFlagsMask(const Mask: LongInt): Boolean;
  {Checks if a file flags mask contains valid File Flag codes.
    @param Mask [in] Mask to be checked.
    @return True if Mask is valid, False if not.
  }
const
  // Bit-set of all valid codes
  AllFlags: LongInt = vs_FF_Debug or vs_FF_Prerelease or vs_FF_Patched or
    vs_FF_PrivateBuild or vs_FF_InfoInferred or vs_FF_SpecialBuild;
begin
  // Given Mask anded with complement of set comprising all valid flags should
  // be zero - if it's not then Mask must contain an invalid file flag code
  Result := ((Mask and not AllFlags) = 0);
end;

function ValidFileFlags(const FFlags, Mask: LongInt): Boolean;
  {Checks if a bit mask of file flags is a subset of a file flags mask.
    @param FFlags [in] Flags to test against mask.
    @param Mask [in] Bitmask that must include all flags.
    @return True if FFlags is a sub-set of Mask, False otherwise.
  }
begin
  // Given FFlags anded with complement of Mask should be zero - if it's not
  // then FFlags is not a subset of Mask and must contain file flag code(s) not
  // in the mask
  Result := ((FFlags and not Mask) = 0);
end;


{ --- Language --- }

const
  // Map of language codes and their descriptions
  cLanguages: array[0..44] of record
    Code: Word;       // the language code
    Desc: string[26]; // the description
  end = (
    (Code: $0401; Desc: 'Arabic'                    ),
    (Code: $0402; Desc: 'Bulgarian'                 ),
    (Code: $0403; Desc: 'Catalan'                   ),
    (Code: $0404; Desc: 'Traditional Chinese'       ),
    (Code: $0405; Desc: 'Czech'                     ),
    (Code: $0406; Desc: 'Danish'                    ),
    (Code: $0407; Desc: 'German'                    ),
    (Code: $0408; Desc: 'Greek'                     ),
    (Code: $0409; Desc: 'U.S. English'              ),
    (Code: $040A; Desc: 'Castilian Spanish'         ),
    (Code: $040B; Desc: 'Finnish'                   ),
    (Code: $040C; Desc: 'French'                    ),
    (Code: $040D; Desc: 'Hebrew'                    ),
    (Code: $040E; Desc: 'Hungarian'                 ),
    (Code: $040F; Desc: 'Icelandic'                 ),
    (Code: $0410; Desc: 'Italian'                   ),
    (Code: $0411; Desc: 'Japanese'                  ),
    (Code: $0412; Desc: 'Korean'                    ),
    (Code: $0413; Desc: 'Dutch'                     ),
    (Code: $0414; Desc: 'Norwegian - Bokmål'        ),
    (Code: $0415; Desc: 'Polish'                    ),
    (Code: $0416; Desc: 'Brazilian Portuguese'      ),
    (Code: $0417; Desc: 'Rhaeto-Romanic'            ),
    (Code: $0418; Desc: 'Romanian'                  ),
    (Code: $0419; Desc: 'Russian'                   ),
    (Code: $041A; Desc: 'Croato-Serbian (Latin)'    ),
    (Code: $041B; Desc: 'Slovak'                    ),
    (Code: $041C; Desc: 'Albanian'                  ),
    (Code: $041D; Desc: 'Swedish'                   ),
    (Code: $041E; Desc: 'Thai'                      ),
    (Code: $041F; Desc: 'Turkish'                   ),
    (Code: $0420; Desc: 'Urdu'                      ),
    (Code: $0421; Desc: 'Bahasa'                    ),
    (Code: $0804; Desc: 'Simplified Chinese'        ),
    (Code: $0807; Desc: 'Swiss German'              ),
    (Code: $0809; Desc: 'U.K. English'              ),
    (Code: $080A; Desc: 'Mexican Spanish'           ),
    (Code: $080C; Desc: 'Belgian French'            ),
    (Code: $0810; Desc: 'Swiss Italian'             ),
    (Code: $0813; Desc: 'Belgian Dutch'             ),
    (Code: $0814; Desc: 'Norwegian - Nynorsk'       ),
    (Code: $0816; Desc: 'Portuguese'                ),
    (Code: $081A; Desc: 'Serbo-Croatian (Cyrillic)' ),
    (Code: $0C0C; Desc: 'Canadian French'           ),
    (Code: $100C; Desc: 'Swiss French'              )
  );

function LangCodeToStr(const LangCode: Word): string;
  {Gets description of language specified by a code.
    @param LangCode [in] Language code.
    @return Description of LangCode or '' if no matching description.
  }
var
  I: Integer; // loop control
begin
  // Set default empty string result
  Result := '';
  // Iterate across entire table searching for language code
  for I := 0 to 44 do
    if cLanguages[I].Code = LangCode then
    begin
      // Found the code - record description and break from loop
      Result := cLanguages[I].Desc;
      Break;
    end;
end;

function StrToLangCode(const Str: string): Word;
  {Gets the language code that correlates with a description.
    @param Str [in] Language description.
    @return Code of language described by Str.
    @except EVersionError raised if language description doesn't exist.
  }
var
  I: Integer;     // loop control
  Found: Boolean; // flag true if description found in table
begin
  // Set default "not found" result
  Found := False;
  // Iterate across table looking for description in non-case sensitive search
  for I := 0 to 44 do
    if CompareText(cLanguages[I].Desc, Str) = 0 then
    begin
      // We've found description - record related code
      Result := cLanguages[I].Code;
      Found := True;
      Break;
    end;
  if not Found then
    // We didn't find the description - raise exception
    Error(sErrLanguage, [Str]);
end;

function LanguageStrList(const SL: TStringList): TStringList;
  {Puts list of language descriptions into a string list.
    @param SL [in] Receives language descriptions.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the list
  SL.Clear;
  // Iterate through table adding descriptions to list
  for I := 0 to 44 do
    Sl.Add(cLanguages[I].Desc);
  // Return a reference to the list
  Result := SL;
end;

function ValidLangCode(const LangCode: Word): Boolean;
  {Checks if a language code is valid.
    @param LangCode [in] Language code to be checked.
    @return True if LangCode is valid, False if not.
  }
begin
  // Result depends on whether a description of the language code is found - it
  // is empty string if there is no such code
  Result := (LangCodeToStr(LangCode) <> '');
end;


{ --- Character sets --- }

const
  // Map of character set codes to the descriptions of the codes
  cCharSets: array[0..11] of record
    Code: Word;       // the character set code
    Desc: string[40]; // the description of the code
  end = (
    (Code: 0;     Desc: '7-bit ASCII'                         ),
    (Code: 932;   Desc: 'Windows, Japan (Shift - JIS X-0208)' ),
    (Code: 949;   Desc: 'Windows, Korea (Shift - KSC 5601)'   ),
    (Code: 950;	  Desc: 'Windows, Taiwan (GB5)'               ),
    (Code: 1200;	Desc: 'Unicode'                             ),
    (Code: 1250;	Desc: 'Windows, Latin-2 (Eastern European)' ),
    (Code: 1251;	Desc: 'Windows, Cyrillic'                   ),
    (Code: 1252;	Desc: 'Windows, Multilingual'               ),
    (Code: 1253;	Desc: 'Windows, Greek'                      ),
    (Code: 1254;	Desc: 'Windows, Turkish'                    ),
    (Code: 1255;	Desc: 'Windows, Hebrew'                     ),
    (Code: 1256;  Desc: 'Windows, Arabic'                     )
  );

function CharCodeToStr(const CharCode: Word): string;
  {Gets the description of a character code.
    @param CharCode [in] Character code for which description required.
    @return Required description or '' if no description found.
  }
var
  I: Integer; // loop control
begin
  // Set default '' result
  Result := '';
  // Iterate across table looking for code
  for I := 0 to 11 do
    if cCharSets[I].Code = CharCode then
    begin
      // Found it - record code and break from loop
      Result := cCharSets[I].Desc;
      Break;
    end;
end;

function StrToCharCode(const Str: string): Word;
  {Gets character code associated with a description.
    @param Str [in] Character set description.
    @return Related character code.
    @except EVersionError raised if description doesn't exist.
  }
var
  I: Integer;     // loop control
  Found: Boolean; // flag true when description found in table
begin
  // Set default "not found" flag
  Found := False;
  // Itereate across table doing non-case sensitive serch for description
  for I := 0 to 11 do
    if CompareText(cCharSets[I].Desc, Str) = 0 then
    begin
      // Found description - record code and that found and break from loop
      Result := cCharSets[I].Code;
      Found := True;
      Break;
    end;
  if not Found then
    // We didn't find description so raise exception
    Error(sErrCharSet, [Str]);
end;

function CharSetStrList(const SL: TStringList): TStringList;
  {Puts list of character set descriptions into a string list.
    @param SL [in] Receives character set descriptions.
    @return Reference to SL.
  }
var
  I: Integer; // loop control
begin
  // Clear the list
  SL.Clear;
  // Itereate across table adding each description to the list
  for I := 0 to 11 do
    Sl.Add(cCharSets[I].Desc);
  // Return a reference to the list
  Result := SL;
end;

function ValidCharCode(const CharCode: Word): Boolean;
  {Checks if a character code is valid.
    @param CharCode [in] Character code to check.
    @return True if CharCode is valid, False if not.
  }
begin
  // Check whether description for given code is empty string: no valid if so
  Result := (CharCodeToStr(CharCode) <> '');
end;

end.

