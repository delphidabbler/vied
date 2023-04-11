{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2023, Peter Johnson (www.delphidabbler.com).
 *
 * Code that reads and writes files in either ANSI or UTF-8 format.
}


unit UFileIO;

interface

uses
  // Delphi
  SysUtils,
  Classes;

type
  ///  <summary>Record that encapsulates a text file's content along with
  ///  information about whether the file's encoding.</summary>
  ///  <remarks>VIEd only supports ANSI (default code page) and UTF-8 encoding.
  ///  </remarks>
  TFileContent = record
    ///  <summary>Indicates whether the file uses UTF-8 format (true) or ANSI
    ///  (false).</summary>
    IsUTF8: Boolean;
    ///  <summary>The file's content.</summary>
    Content: string;
    ///  <summary>Record constructor: sets fields to given values.</summary>
    constructor Create(const AIsUTF8: Boolean; const AContent: string);
    ///  <summary>Returns the file content as an array of lines.</summary>
    function ContentLines: TArray<string>;
    ///  <summary>Returns the file content as an array of bytes, produced using
    ///  the encoding specified by the <c>IsUTF8</c> field.</summary>
    function ContentBytes: TBytes;
    ///  <summary>Returns either an ANSI or UTF-8 encoding depending on the
    ///  value of the <c>IsUTF8</c> field.</summary>
    function Encoding: TEncoding; overload;
    ///  <summary>Returns either an ANSI or UTF-8 encoding, as specified the
    ///  parameter.</summary>
    class function Encoding(const AIsUTF8: Boolean): TEncoding; overload;
      static;
  end;

  ///  <summary>Container for static methods that read and write text files.
  ///  </summary>
  TFileIO = record
  strict private
    ///  <summary>Checks if the given stream starts with the preamble of the
    ///  given encoding.</summary>
    class function CheckBOM(const Stream: TStream;
      const Encoding: TEncoding): Boolean; static;
    ///  <summary>Checks if the given file has the UTF-8 preamble.</summary>
    class function IsUTF8(const AFileName: string): Boolean; static;
    ///  <summary>Appends whole contents of a byte array to a stream.</summary>
    class procedure BytesToStream(const Bytes: TBytes; const Stream: TStream);
      static;
  public
    ///  <summary>Reads the text file specified by the given file name and
    ///  returns its content and encoding information, encapsulated in a
    ///  <c>TFileContent</c> record.</summary>
    class function ReadFile(const AFileName: string): TFileContent; static;
    ///  <summary>Writes a text file with the given file name in the required
    ///  format.</summary>
    ///  <param name="AFileName"><c>string</c> [in] Name of file.</param>
    ///  <param name="AFileContent"><c>TFileContent</c> [in] Record that
    ///  contains file content and indicates encoding to be used.</param>
    class procedure WriteFile(const AFileName: string;
      const AFileContent: TFileContent); static;
  end;

implementation

uses
  // Delphi
  IOUtils,
  // Project
  UUtils;

{ TFileIO }

class procedure TFileIO.BytesToStream(const Bytes: TBytes;
  const Stream: TStream);
begin
  if Length(Bytes) > 0 then
    Stream.WriteBuffer(Pointer(Bytes)^, Length(Bytes));
end;

class function TFileIO.CheckBOM(const Stream: TStream;
  const Encoding: TEncoding): Boolean;
var
  Bytes: TBytes;
  Preamble: TBytes;
  OldPos: Int64;
begin
  Assert(Assigned(Stream), 'TFileIO.CheckBOM: Stream is nil');
  Assert(Assigned(Encoding), 'TFileIO.CheckBOM: Encoding is nil');
  Preamble := Encoding.GetPreamble;
  if Stream.Size < Length(Preamble) then
    Exit(False);
  OldPos := Stream.Position;
  SetLength(Bytes, Length(Preamble));
  Stream.Position := 0;
  Stream.ReadBuffer(Pointer(Bytes)^, Length(Preamble));
  Stream.Position := OldPos;
  Result := IsEqualBytes(Bytes, Preamble);
end;

class function TFileIO.IsUTF8(const AFileName: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CheckBOM(Stream, TEncoding.UTF8);
  finally
    Stream.Free;
  end;
end;

class function TFileIO.ReadFile(const AFileName: string): TFileContent;
var
  IsUTF8Format: Boolean;
begin
  IsUTF8Format := IsUTF8(AFileName);
  Result := TFileContent.Create(
    IsUTF8Format,
    // Following TFile.ReadAllText expects a UTF-8 preamble is UTF-8 file
    TFile.ReadAllText(AFileName, TFileContent.Encoding(IsUTF8Format))
  );
end;

class procedure TFileIO.WriteFile(const AFileName: string;
  const AFileContent: TFileContent);
var
  Stream: TStream;
begin
  // There are no suitable TFile methods to write BOM, so we have to implement
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    BytesToStream(AFileContent.Encoding.GetPreamble, Stream);
    BytesToStream(AFileContent.ContentBytes, Stream);
  finally
    Stream.Free;
  end;
end;

{ TFileContent }

function TFileContent.ContentBytes: TBytes;
begin
  Result := Encoding.GetBytes(Content);
end;

function TFileContent.ContentLines: TArray<string>;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Content;
    Result := SL.ToStringArray;
  finally
    SL.Free;
  end;
end;

constructor TFileContent.Create(const AIsUTF8: Boolean; const AContent: string);
begin
  IsUTF8 := AIsUTF8;
  Content := AContent;
end;

function TFileContent.Encoding: TEncoding;
begin
  Result := Encoding(IsUTF8);
end;

class function TFileContent.Encoding(const AIsUTF8: Boolean): TEncoding;
begin
  if AIsUTF8 then
    Result := TEncoding.UTF8
  else
    Result := TEncoding.Default;
end;

end.

