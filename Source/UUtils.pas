{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2022, Peter Johnson (www.delphidabbler.com).
 *
 * Miscellaneous support routines for Version Information Editor.
}


unit UUtils;


interface


uses
  // Delphi
  SysUtils;


function NextField(TextLine: string; var StringStart: Integer;
  var Field: string; Separator: Char): Boolean;
  {Finds next text field in a delimited string.
    @param TextLine [in] String from which field is to be extracted.
    @param StringStart [in/out] In: index of start of field. Out: Set to start
      of following field.
    @param Field [in/out] Set to content of required field.
    @param Separator [in] Character used to separate fields.
    @return True if a field was found, False if no more fields.
  }

///  <summary>Splits string S at the first occurence of delimiter string Delim.
///  S1 is set to to the sub-string of S before Delim and S2 to the remainder of
///  S after Delim. If Delim is not found then S1 is set to S and S2 to the
///  empty string. Returns True if S contains Delim or False if not.</summary>
function SplitStr(const S, Delim: string; out S1, S2: string): Boolean;

procedure Replace(DelStr, InsStr: string; var S: string);
  {Replaces first occurence of a substring in a string with another string.
    @param DelStr [in] String to be replaced.
    @param InsStr [in] String to replace DelStr.
    @param S [in/out] In: String to be modified. Out: Modified string. S is
      not changed if DelStr is not a substring of S.
  }

function TrimSpaces(const Str: string): string;
  {Trims leading and trailing spaces from a string.
    @param Str [in] String to be trimmed.
    @return Trimmed string.
  }

function RemoveExtension(const FileName: string): string;
  {Removes any extension from a file name.
    @param FileName [in] File name to be processed.
    @return File name without extension..
  }

function EnsureExtension(const FileName, Filters: string;
  const FilterIndex: Integer): string;
  {Ensures a file name has an extension based on a file filter.
    @param FileName [in] Name of file requiring extension.
    @param Filters [in] Pipe delimited file filters, in format used by file
      dialog boxes.
    @param FilterIndex [in] Index of required filter in Filters.
    @return If FileName has extension it is returned unchanged, otherwise the
      extension from Filters specified by FiltErIndex is added to FileName.
  }

function UserAppDataFolder: string;
  {Gets current user's application data folder.
    @return Required folder.
  }

///  <summary>Checks if two byte arrays are equal.</summary>
///  <param name="BA1">TBytes [in] First byte array to be compared.</param>
///  <param name="BA2">TBytes [in] Second byte array to be compared.</param>
///  <returns>True if the two arrays are equal, False if not.</returns>
///  <remarks>If both arrays are empty they are considered equal.</remarks>
function IsEqualBytes(const BA1, BA2: TBytes): Boolean; overload;

///  <summary>Escapes all characters from string S that are included in
///  Escapable with the backslash character followed by the matching character
///  in Escapes.</summary>
///  <remarks>Escapable and Escapes must be the same length.</remarks>
function BackslashEscape(const S, Escapable, Escapes: string): string;


implementation


uses
  // Delphi
  Classes, Windows, ShlObj, ActiveX, StrUtils;


function NextField(TextLine: string; var StringStart: Integer;
  var Field: string; Separator: Char): Boolean;
  {Finds next text field in a delimited string.
    @param TextLine [in] String from which field is to be extracted.
    @param StringStart [in/out] In: index of start of field. Out: Set to start
      of following field.
    @param Field [in/out] Set to content of required field.
    @param Separator [in] Character used to separate fields.
    @return True if a field was found, False if no more fields.
  }
var
  StringEnd: Integer;  // end of string
  L: Integer;          // length of string
  Done: Boolean;       // loop termination flag
begin
  // Find length of given line
  L := Length(TextLine);
  // Check if StringStart is beyond length of line
  if StringStart > L then
  begin
    // StringStart is beyond line end - return nul string & false - no field
    Field := '';
    Result := False;
  end
  else
  begin
    // StringStart is within line
    // set string end to string start & initialise loop control flag
    StringEnd := StringStart;
    Done := False;
    // loop while string end is within string and separator not found
    while (StringEnd <= L) and not Done do
    begin
      if TextLine[StringEnd] = Separator then
        // we have found separator - halt loop
        Done := True
      else
        // haven't yet found separator - try next string position
        StringEnd := StringEnd + 1;
    end;
    // check if we found separator
    if Done then
      // separator found: return line from StartString to just before separator
      Field := Copy(TextLine, StringStart, StringEnd - StringStart)
    else
      // no separator found - return line from StringStart to end of line
      Field := Copy(TextLine, StringStart, StringEnd - StringStart);
    // Set StringStart for next time to just after StringEnd
    StringStart := StringEnd + 1;
    // Succesful result
    Result := True;
  end;
end;

function SplitStr(const S, Delim: string; out S1, S2: string): Boolean;
var
  DelimPos: Integer;  // position of delimiter in source string
begin
  // Find position of first occurence of delimiter in string
  DelimPos := SysUtils.AnsiPos(Delim, S);
  if DelimPos > 0 then
  begin
    // Delimiter found: split and return True
    S1 := Copy(S, 1, DelimPos - 1);
    S2 := Copy(S, DelimPos + Length(Delim), MaxInt);
    Result := True;
  end
  else
  begin
    // Delimiter not found: return false and set S1 to whole string
    S1 := S;
    S2 := '';
    Result := False;
  end;
end;

procedure Replace(DelStr, InsStr: string; var S: string);
  {Replaces first occurence of a substring in a string with another string.
    @param DelStr [in] String to be replaced.
    @param InsStr [in] String to replace DelStr.
    @param S [in/out] In: String to be modified. Out: Modified string. S is
      not changed if DelStr is not a substring of S.
  }
begin
  S := StringReplace(S, DelStr, InsStr, []);
end;

function TrimSpaces(const Str: string): string;
  {Trims leading and trailing spaces from a string.
    @param Str [in] String to be trimmed.
    @return Trimmed string.
  }
begin
  Result := Trim(Str);
end;

function RemoveExtension(const FileName: string): string;
  {Removes any extension from a file name.
    @param FileName [in] File name to be processed.
    @return File name without extension.
  }
var
  P: Byte;  // position of start of extension in FileName
begin
  // Find position of start of extension, if any
  P := Pos('.', FileName);
  if P > 0 then
    // There is an extension - remove it and return result
    Result := Copy(FileName, 1, P - 1)
  else
    // There is no extension - return whole file name
    Result := FileName;
end;

function EnsureExtension(const FileName, Filters: string;
  const FilterIndex: Integer): string;
  {Ensures a file name has an extension based on a file filter.
    @param FileName [in] Name of file requiring extension.
    @param Filters [in] Pipe delimited file filters, in format used by file
      dialog boxes.
    @param FilterIndex [in] Index of required filter in Filters.
    @return If FileName has extension it is returned unchanged, otherwise the
      extension from Filters specified by FiltErIndex is added to FileName.
  }

  // ---------------------------------------------------------------------------
  function SelectedFilter(Filter: string; Index: Integer): string;
    {Gets file extension from a filter string.
     @param Filter [in] Pipe delimited file filters, in format used by file
        dialog boxes.
      @param FilterIndex [in] Index of required filter in Filters.
      @return Required extension.
    }
  var
    List: TStringList;  // list to hold extensions represented by filters
    BarPos: Integer;    // position of a bar char (|) in a string
  begin
    // Create string to hold extensions
    List := TStringList.Create;
    try
      // Scan thru filter string pulling out extensions
      // Each filter is in form <description>|<extension> and multiple filters
      // are separated by another '|' character
      BarPos := AnsiPos('|', Filter);
      while BarPos > 0 do
      begin
        // strip away description (up to first bar)
        Filter := Copy(Filter, BarPos + 1, MaxInt);
        // find any bar following extension
        BarPos := AnsiPos('|', Filter);
        if BarPos > 0 then
        begin
          // there is a bar => more filters: extension occurs before bar
          // .. copy out extension and add to list
          List.Add(Copy(Filter, 1, BarPos - 1));
          // .. delete extension and move to start of next filter
          Filter := Copy(Filter, BarPos + 1, MaxInt);
          BarPos := AnsiPos('|', Filter);
        end
        else if Filter <> '' then
          // no bar => last filter: store extension if it exists
          List.Add(Filter);
      end;
      // Decrease index: filters have 1 based indices, string lists are 0 based
      Dec(Index);
      // Check index is valid (in range)
      if (Index >= 0) and (Index < List.Count) then
      begin
        // Get hold of selected extension and strip off any leading '*.'
        Result := List[Index];
        if AnsiPos('*.', Result) > 0 then
          Result := Copy(Result, 2, MaxInt);
      end
      else
        // Index out of range: return empty string
        Result := '';
    finally
      List.Free;
    end;
  end;
  // ---------------------------------------------------------------------------
var
  Ext: string;  // file's extension
begin
  // Get extension from file
  Ext := ExtractFileExt(FileName);
  if Ext = '' then
  begin
    // File name has no extension: add required one from filter
    Ext := SelectedFilter(Filters, FilterIndex);
    Result := FileName + Ext;
  end
  else
    // File name already has etension: return unchanged
    Result := FileName;
end;

procedure FreePIDL(PIDL: PItemIDList);
  {Uses to shell allocator to free the memory used by a PIDL.
    @param PIDL [in] PIDL to be freed.
  }
var
  Malloc: IMalloc;  // shell's allocator
begin
  // Try to get shell allocator
  if Succeeded(SHGetMalloc(Malloc)) then
    // Use allocator to free PIDL: Malloc is freed by Delphi
    Malloc.Free(PIDL);
end;

function PIDLToFolderPath(PIDL: PItemIDList): string;
  {Gets full path to a file system folder from a PIDL.
    @param PIDL [in] PIDL describing folder.
    @return Required folder name or '' if PIDL refers to a virtual folder.
  }
begin
  // Set max length of return string
  SetLength(Result, MAX_PATH);
  // Get the path
  if SHGetPathFromIDList(PIDL, PChar(Result)) then
    Result := PChar(Result)
  else
    Result := '';
end;

function SpecialFolderPath(CSIDL: Integer): string;
  {Gets full path to a special file system folder.
    @param CSIDL [in] Identifier of required folder.
    @return Required folder or '' if the special folder is virtual or CSIDL not
      supported.
  }
var
  PIDL: PItemIDList;  // PIDL of the special folder
begin
  Result := '';
  // Get PIDL for required folder
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, PIDL)) then
  begin
    try
      // Get path from PIDL
      Result := PIDLToFolderPath(PIDL);
    finally
      // Free the PIDL using shell allocator
      FreePIDL(PIDL);
    end;
  end
end;

function UserAppDataFolder: string;
  {Gets current user's application data folder.
    @return Required folder.
  }
begin
  Result := SpecialFolderPath(CSIDL_APPDATA);
end;

function IsEqualBytes(const BA1, BA2: TBytes): Boolean; overload;
var
  I: Integer;
begin
  if Length(BA1) <> Length(BA2) then
    Exit(False);
  for I := 0 to Pred(Length(BA1)) do
    if BA1[I] <> BA2[I] then
      Exit(False);
  Result := True;
end;

function BackslashEscape(const S, Escapable, Escapes: string): string;
const
  EscChar = '\';        // the C escape character
var
  EscCount: Integer;
  Ch: Char;
  PRes: PChar;
  EscCharPos: Integer;
begin
  Assert(Length(Escapable) = Length(Escapes));
  // Check for empty string and treat specially (empty string crashes main code)
  if S = '' then
  begin
    Result := '';
    Exit;
  end;
  // Count escapable characters in string
  EscCount := 0;
  for Ch in S do
  begin
    if ContainsStr(Escapable, Ch) then
      Inc(EscCount);
  end;
  // Set size of result string and get pointer to it
  SetLength(Result, Length(S) + EscCount);
  PRes := PChar(Result);
  // Replace escapable chars with the escaped version
  for Ch in S do
  begin
    EscCharPos := AnsiPos(Ch, Escapable);
    if EscCharPos > 0 then
    begin
      PRes^ := EscChar;
      Inc(PRes);
      PRes^ := Escapes[EscCharPos];
    end
    else
      PRes^ := Ch;
    Inc(PRes);
  end;
end;

end.

