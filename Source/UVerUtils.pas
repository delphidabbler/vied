{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2022, Peter Johnson (www.delphidabbler.com).
 *
 * Version information utility routines and look-up tables. Provides conversions
 * between version information codes and text descriptions etc.
}


unit UVerUtils;


interface


uses
  // Delphi
  Classes, SysUtils,
  // PJSoft library
  PJVersionInfo;


{ --- Styles of hex symbols that can be used --- }

const
  PascalHexSymbol = '$';
  CHexSymbol = '0x';


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

function FileSubTypeToStr(const FType, FSType: LongInt;
  const AHexSymbol: string): string;
  {Get the symbolic constant representing a file sub type within a file type.
    @param FType [in] File type.
    @param FSType [in] File sub-type for which symbolic constant required.
    @param AHexSymbol [in] Hex symbol to be used when a hex value is returned.
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

{ --- Table lookup helpers --- }

type
  // Map of a code number to its string representation
  TCodeStrMap = record
    Code: LongInt;  // code number
    Str: string;    // string representation of code
  end;

// Returns the string representation of the given code number in the given table
// of code / string map records.
function CodeToStr(const ACode: LongInt; const ATable: array of TCodeStrMap):
  string;
var
  Row: TCodeStrMap; // each row in table
begin
  for Row in ATable do
    if Row.Code = ACode then
      Exit(Row.Str);
  Result := '';
end;

// Adds the string representation of each row in the given table to the given
// string list, which is first cleared. A reference to the string list is
// returned.
function BuildCodeList(const ATable: array of TCodeStrMap;
  const SL: TStringList): TStringList;
var
  Row: TCodeStrMap; // each row in table
begin
  SL.Clear;
  for Row in ATable do
    SL.Add(Row.Str);
  Result := SL;
end;

// Attempts to find the code associated with the given string in the given
// table. If the code is found it is passed out in the ACode parameter and True
// is returned. If the code is not found ACode is undefined and False is
// returned.
function TryStrToCode(const AStr: string; const ATable: array of TCodeStrMap;
  out ACode: LongInt): Boolean;
var
  Row: TCodeStrMap; // each row in table
begin
  for Row in ATable do
    if AnsiSameText(Row.Str, AStr) then
    begin
      ACode := Row.Code;
      Exit(True);
    end;
  Result := False;
end;

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
const
  Separator = ',';
  AltSeparator = '.';
var
  Start: Integer;               // cursor to walk along string}
  NumStr: string;               // current number in string}
  Nums: array[1..4] of LongInt; // array to hold numbers from string}
  I: Integer;                   // loop control variable}
  Sep: Char;
begin
  // Decide whether to use VI style version number separator (',') or
  // alternatate separator ('.')
  Sep := Separator;
  if Pos(Sep, Str) = 0 then
    Sep := AltSeparator;

  Start := 1;
  // Iterate for each version number (there are up to four in string)
  for I := Low(Nums) to High(Nums) do
  begin
    NextField(Str, Start, NumStr, Sep);
    NumStr := TrimSpaces(NumStr);
    // Convert NumStr into the required number - default 0 if no valid value
    if NumStr <> '' then
      Nums[I] := StrToIntDef(NumStr, 0)
    else
      Nums[I] := 0;
  end;
  Result.V1 := Nums[1];
  Result.V2 := Nums[2];
  Result.V3 := Nums[3];
  Result.V4 := Nums[4];
end;


{ --- File OS --- }

const
  // Array of OS codes to symbolic constants
  FileOSTable: array[0..6] of TCodeStrMap = (
    (Code: VOS_DOS;           Str: 'VOS_DOS';           ),
    (Code: VOS_NT;            Str: 'VOS_NT';            ),
    (Code: VOS__WINDOWS16;    Str: 'VOS__WINDOWS16';    ),
    (Code: VOS__WINDOWS32;    Str: 'VOS__WINDOWS32';    ),
    (Code: VOS_DOS_WINDOWS16; Str: 'VOS_DOS_WINDOWS16'; ),
    (Code: VOS_DOS_WINDOWS32; Str: 'VOS_DOS_WINDOWS32'; ),
    (Code: VOS_NT_WINDOWS32;  Str: 'VOS_NT_WINDOWS32';  )
  );

function FileOSToStr(const OS: LongInt): string;
  {Gets symbolic constant representing an OS code.
    @param OS [in] OS code.
    @return Matching symbolic constant or '' if OS is not recognised.
  }
begin
  Result := CodeToStr(OS, FileOSTable);
end;

function StrToFileOS(const Str: string): LongInt;
  {Converts an OS symbolic constant into the equivalent code.
    @param Str [in] Symbolic constant.
    @return OS code of symbolic constant.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
begin
  if not TryStrToCode(Str, FileOSTable, Result) then
    Error(sErrOSCode, [Str]);
end;

function FileOSCodeList(const SL: TStringList): TStringList;
  {Builds a list of OS symbolic constants.
    @param SL [in] Receives symbolic constant list.
    @return Reference to SL.
  }
begin
  Result := BuildCodeList(FileOSTable, SL);
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
  FileTypeTable: array[0..6] of TCodeStrMap = (
    (Code: VFT_UNKNOWN;     Str: 'VFT_UNKNOWN';     ),
    (Code: VFT_APP;         Str: 'VFT_APP';         ),
    (Code: VFT_DLL;         Str: 'VFT_DLL';         ),
    (Code: VFT_DRV;         Str: 'VFT_DRV';         ),
    (Code: VFT_FONT;        Str: 'VFT_FONT';        ),
    (Code: VFT_VXD;         Str: 'VFT_VXD';         ),
    (Code: VFT_STATIC_LIB;  Str: 'VFT_STATIC_LIB';  )
  );

function FileTypeToStr(const FType: LongInt): string;
  {Gets the symbolic constant for a file type.
    @param FType [in] File type code.
    @return Symbolic constant or '' if FType not recognised.
  }
begin
  Result := CodeToStr(FType, FileTypeTable);
end;

function StrToFileType(const Str: string): LongInt;
  {Converts a file type symbolic constant into its value.
    @param Str [in] Symbolic constant.
    @return Code associated with symbolic constant.
    @except EVersionError raised if symbolic constant not recognised.
  }
begin
  if not TryStrToCode(Str, FileTypeTable, Result) then
    Error(sErrFileType, [Str]);
end;

function FileTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of file type symbolic constants.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
begin
  Result := BuildCodeList(FileTypeTable, SL);
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

function FileSubTypeToStr(const FType, FSType: LongInt;
  const AHexSymbol: string): string;
  {Get the symbolic constant representing a file sub type within a file type.
    @param FType [in] File type.
    @param FSType [in] File sub-type for which symbolic constant required.
    @param AHexSymbol [in] Hex symbol to be used when a hex value is returned.
    @return Required symbolic constant, or hex string.
  }
begin
  case FType of
    VFT_DRV: Result := PvtDriveSubTypeToStr(FSType);
    VFT_FONT: Result := PvtFontSubTypeToStr(FSType);
    else Result := AHexSymbol + IntToHex(FSType, 4);
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
  DriverSubTypeTable: array[0..10] of TCodeStrMap = (
    (Code: VFT2_UNKNOWN;          Str: 'VFT2_UNKNOWN';          ),
    (Code: VFT2_DRV_COMM;         Str: 'VFT2_DRV_COMM';         ),
    (Code: VFT2_DRV_PRINTER;      Str: 'VFT2_DRV_PRINTER';      ),
    (Code: VFT2_DRV_KEYBOARD;     Str: 'VFT2_DRV_KEYBOARD';     ),
    (Code: VFT2_DRV_LANGUAGE;     Str: 'VFT2_DRV_LANGUAGE';     ),
    (Code: VFT2_DRV_DISPLAY;      Str: 'VFT2_DRV_DISPLAY';      ),
    (Code: VFT2_DRV_MOUSE;        Str: 'VFT2_DRV_MOUSE';        ),
    (Code: VFT2_DRV_NETWORK;      Str: 'VFT2_DRV_NETWORK';      ),
    (Code: VFT2_DRV_SYSTEM;       Str: 'VFT2_DRV_SYSTEM';       ),
    (Code: VFT2_DRV_INSTALLABLE;  Str: 'VFT2_DRV_INSTALLABLE';  ),
    (Code: VFT2_DRV_SOUND;        Str: 'VFT2_DRV_SOUND';        )
  );

function PvtDriveSubTypeToStr(const FSType: LongInt): string;
  {Converts a driver sub-type code to a symbolic constant.
    @param FSType [in] Driver sub-type code.
    @return Related symbolic constant or '' if FSType is not recognised.
  }
begin
  Result := CodeToStr(FSType, DriverSubTypeTable);
end;

function PvtStrToDriveSubType(const Str: string): LongInt;
  {Converts a symbolic constant to the equivalent driver sub-type code.
    @param Str [in] Symbolic constant to convert.
    @return Equivalent driver sub-type code.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
begin
  if not TryStrToCode(Str, DriverSubTypeTable, Result) then
    Error(sErrDriverSubType, [Str]);
end;

function DriverSubTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of drv sub-type symbolic constants.
    @param SL [in] Receives list of symbolic constants.
    @return Reference to SL.
  }
begin
  Result := BuildCodeList(DriverSubTypeTable, SL);
end;


{ Font sub type }

const
  // Map of Font Sub-Type codes to their symbolic constants
  FontSubTypeTable: array[0..3] of TCodeStrMap = (
    (Code: VFT2_UNKNOWN;        Str: 'VFT2_UNKNOWN';        ),
    (Code: VFT2_FONT_RASTER;    Str: 'VFT2_FONT_RASTER';    ),
    (Code: VFT2_FONT_VECTOR;    Str: 'VFT2_FONT_VECTOR';    ),
    (Code: VFT2_FONT_TRUETYPE;  Str: 'VFT2_FONT_TRUETYPE';  )
  );

function PvtFontSubTypeToStr(const FSType: LongInt): string;
  {Converts a font sub-type code to its symbolic constant.
    @param FSType [in] Font sub-type code.
    @return Associated symbolic constant or '' if FSType is not recognised.
  }
begin
  Result := CodeToStr(FSType, FontSubTypeTable);
end;

function PvtStrToFontSubType(const Str: string): LongInt;
  {Converts a symbolic constant to font sub-type code.
    @param Str [in] Symbolic constant.
    @return Code matching symbolic constant.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
begin
  if not TryStrToCode(Str, FontSubTypeTable, Result) then
    Error(sErrDriverSubType, [Str]);
end;

function FontSubTypeCodeList(const SL: TStringList): TStringList;
  {Builds a list of all font sub-types symbolic constants.
    @param SL [in] Receives list of sub-types.
    @return Reference to SL.
  }
begin
  Result := BuildCodeList(FontSubTypeTable, SL);
end;


{ --- File flags --- }

const
  // Map of file flag codes to the equivalent symbolic constants
  FileFlagTable: array[0..5] of TCodeStrMap = (
    (Code: VS_FF_DEBUG;         Str: 'VS_FF_DEBUG'        ),
    (Code: VS_FF_PRERELEASE;    Str: 'VS_FF_PRERELEASE'   ),
    (Code: VS_FF_PATCHED;       Str: 'VS_FF_PATCHED'      ),
    (Code: VS_FF_PRIVATEBUILD;  Str: 'VS_FF_PRIVATEBUILD' ),
    (Code: VS_FF_INFOINFERRED;  Str: 'VS_FF_INFOINFERRED' ),
    (Code: VS_FF_SPECIALBUILD;  Str: 'VS_FF_SPECIALBUILD' )
  );

function FileFlagSetToStr(const FFlags: LongInt): string;
  {Builds a string containing a '+' delimited string of symbolic constants of
  file flags contained in a bitmask.
    @param FFlags [in] File flags bitmask.
    @return Required list of symbolic constants.
  }
var
  Row: TCodeStrMap; // each row in table
  Count: Integer;   // count of number of flags in set to date
begin
  Result := '';
  Count := 0;
  for Row in FileFlagTable do
    if (Row.Code and FFlags) = Row.Code then
    begin
      Inc(Count);
      if Count = 1 then
        Result := Row.Str
      else
        Result := Result + ' + ' + Row.Str;
    end;
end;

function StrToFileFlag(const Str: string): LongInt;
  {Gets the file flag code associated with a symbolic constant.
    @param Str [in] Symbolic constant.
    @return Associated file flag.
    @except EVersionError raised if Str is not a valid symbolic constant.
  }
begin
  if not TryStrToCode(Str, FileFlagTable, Result) then
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
  Row: TCodeStrMap; // each row in table
begin
  SL.Clear;
  for Row in FileFlagTable do
    if (Row.Code and FFlags) = Row.Code then
      SL.Add(Row.Str);
  Result := SL;
end;

function StrListToFileFlagSet(const SL: TStringList): LongInt;
  {Converts a list of file flag symbolic constants to a bit mask.
    @param SL [in] List of symbolic constants.
    @return Equivalent bitmask.
  }
var
  Str: string;  // each string constant in list
begin
  Result := 0;
  for Str in SL do
    Result := Result or StrToFileFlag(Str);
end;

function ValidFileFlagsMask(const Mask: LongInt): Boolean;
  {Checks if a file flags mask contains valid File Flag codes.
    @param Mask [in] Mask to be checked.
    @return True if Mask is valid, False if not.
  }
const
  // Bit-set of all valid codes
  AllFlags: LongInt = VS_FF_DEBUG or VS_FF_PRERELEASE or VS_FF_PATCHED or
    VS_FF_PRIVATEBUILD or VS_FF_INFOINFERRED or VS_FF_SPECIALBUILD;
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
  LanguageTable: array[0..44] of TCodeStrMap = (
    (Code: $0401; Str: 'Arabic'                    ),
    (Code: $0402; Str: 'Bulgarian'                 ),
    (Code: $0403; Str: 'Catalan'                   ),
    (Code: $0404; Str: 'Traditional Chinese'       ),
    (Code: $0405; Str: 'Czech'                     ),
    (Code: $0406; Str: 'Danish'                    ),
    (Code: $0407; Str: 'German'                    ),
    (Code: $0408; Str: 'Greek'                     ),
    (Code: $0409; Str: 'U.S. English'              ),
    (Code: $040A; Str: 'Castilian Spanish'         ),
    (Code: $040B; Str: 'Finnish'                   ),
    (Code: $040C; Str: 'French'                    ),
    (Code: $040D; Str: 'Hebrew'                    ),
    (Code: $040E; Str: 'Hungarian'                 ),
    (Code: $040F; Str: 'Icelandic'                 ),
    (Code: $0410; Str: 'Italian'                   ),
    (Code: $0411; Str: 'Japanese'                  ),
    (Code: $0412; Str: 'Korean'                    ),
    (Code: $0413; Str: 'Dutch'                     ),
    (Code: $0414; Str: 'Norwegian - Bokmål'        ),
    (Code: $0415; Str: 'Polish'                    ),
    (Code: $0416; Str: 'Brazilian Portuguese'      ),
    (Code: $0417; Str: 'Rhaeto-Romanic'            ),
    (Code: $0418; Str: 'Romanian'                  ),
    (Code: $0419; Str: 'Russian'                   ),
    (Code: $041A; Str: 'Croato-Serbian (Latin)'    ),
    (Code: $041B; Str: 'Slovak'                    ),
    (Code: $041C; Str: 'Albanian'                  ),
    (Code: $041D; Str: 'Swedish'                   ),
    (Code: $041E; Str: 'Thai'                      ),
    (Code: $041F; Str: 'Turkish'                   ),
    (Code: $0420; Str: 'Urdu'                      ),
    (Code: $0421; Str: 'Bahasa'                    ),
    (Code: $0804; Str: 'Simplified Chinese'        ),
    (Code: $0807; Str: 'Swiss German'              ),
    (Code: $0809; Str: 'U.K. English'              ),
    (Code: $080A; Str: 'Mexican Spanish'           ),
    (Code: $080C; Str: 'Belgian French'            ),
    (Code: $0810; Str: 'Swiss Italian'             ),
    (Code: $0813; Str: 'Belgian Dutch'             ),
    (Code: $0814; Str: 'Norwegian - Nynorsk'       ),
    (Code: $0816; Str: 'Portuguese'                ),
    (Code: $081A; Str: 'Serbo-Croatian (Cyrillic)' ),
    (Code: $0C0C; Str: 'Canadian French'           ),
    (Code: $100C; Str: 'Swiss French'              )
  );

function LangCodeToStr(const LangCode: Word): string;
  {Gets description of language specified by a code.
    @param LangCode [in] Language code.
    @return Description of LangCode or '' if no matching description.
  }
begin
  Result := CodeToStr(LangCode, LanguageTable);
end;

function StrToLangCode(const Str: string): Word;
  {Gets the language code that correlates with a description.
    @param Str [in] Language description.
    @return Code of language described by Str.
    @except EVersionError raised if language description doesn't exist.
  }
var
  ResInt: LongInt;  // result code as long int
begin
  if not TryStrToCode(Str, LanguageTable, ResInt) then
    Error(sErrLanguage, [Str]);
  Result := Word(ResInt);
end;

function LanguageStrList(const SL: TStringList): TStringList;
  {Puts list of language descriptions into a string list.
    @param SL [in] Receives language descriptions.
    @return Reference to SL.
  }
begin
  Result := BuildCodeList(LanguageTable, SL);
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
  CharSetTable: array[0..11] of TCodeStrMap = (
    (Code: 0;     Str: '7-bit ASCII'                         ),
    (Code: 932;   Str: 'Windows, Japan (Shift - JIS X-0208)' ),
    (Code: 949;   Str: 'Windows, Korea (Shift - KSC 5601)'   ),
    (Code: 950;	  Str: 'Windows, Taiwan (GB5)'               ),
    (Code: 1200;	Str: 'Unicode'                             ),
    (Code: 1250;	Str: 'Windows, Latin-2 (Eastern European)' ),
    (Code: 1251;	Str: 'Windows, Cyrillic'                   ),
    (Code: 1252;	Str: 'Windows, Multilingual'               ),
    (Code: 1253;	Str: 'Windows, Greek'                      ),
    (Code: 1254;	Str: 'Windows, Turkish'                    ),
    (Code: 1255;	Str: 'Windows, Hebrew'                     ),
    (Code: 1256;  Str: 'Windows, Arabic'                     )
  );

function CharCodeToStr(const CharCode: Word): string;
  {Gets the description of a character code.
    @param CharCode [in] Character code for which description required.
    @return Required description or '' if no description found.
  }
begin
  Result := CodeToStr(CharCode, CharSetTable);
end;

function StrToCharCode(const Str: string): Word;
  {Gets character code associated with a description.
    @param Str [in] Character set description.
    @return Related character code.
    @except EVersionError raised if description doesn't exist.
  }
var
  ResInt: LongInt;  // result code as long int
begin
  if not TryStrToCode(Str, CharSetTable, ResInt) then
    Error(sErrCharSet, [Str]);
  Result := Word(ResInt);
end;

function CharSetStrList(const SL: TStringList): TStringList;
  {Puts list of character set descriptions into a string list.
    @param SL [in] Receives character set descriptions.
    @return Reference to SL.
  }
begin
  Result := BuildCodeList(CharSetTable, SL);
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

