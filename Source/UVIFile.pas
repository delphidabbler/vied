{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (www.delphidabbler.com).
 *
 * Loads, saves and provides information about a .vi file.
}

unit UVIFile;

interface

uses
  UFileIO;

type
  ///  <summary>Encapsulates a physical .vi file.</summary>
  TVIFile = class(TObject)
  strict private
    var
      fName: string;
  public
    ///  <summary>Name of the .vi file, including path, or empty string the file
    ///  has not been saved or loaded.</summary>
    property Name: string read fName;
    ///  <summary>Saves the .vi file.</summary>
    ///  <param name="AFileName">[in] the name of the file. Stored in the
    ///  <c>Name</c> property.</param>
    ///  <param name="AContent">[in] The content to be written to the file.
    ///  </param>
    procedure Save(const AFileName: string; const AContent: TFileContent);
    ///  <summary>Loads the .vi file.</summary>
    ///  <param name="AFileName">[in] the name of the file. Stored in the
    ///  <c>Name</c> property.</param>
    ///  <returns><c>TFileContent</c>. The content of the .vi file.</returns>
    function Load(const AFileName: string): TFileContent;
    ///  <summary>Checks whether the file has been saved yet.</summary>
    function IsSaved: Boolean;
    ///  <summary>Clears file name, indicating that the file has not yet been
    ///  saved.</summary>
    procedure Clear;
    ///  <summary>Directory of the .vi file name or empty string if not set.
    ///  </summary>
    function FileDir: string;
  end;

implementation

uses
  IOUtils;

{ TVIFile }

procedure TVIFile.Clear;
begin
  fName := '';
end;

function TVIFile.FileDir: string;
begin
  if IsSaved then
    Result := TPath.GetDirectoryName(fName)
  else
    Result := '';
end;

function TVIFile.IsSaved: Boolean;
begin
  Result := fName <> '';
end;

function TVIFile.Load(const AFileName: string): TFileContent;
begin
  fName := AFileName;
  Result := TFileIO.ReadFile(AFileName);
end;

procedure TVIFile.Save(const AFileName: string; const AContent: TFileContent);
begin
  fName := AFileName;
  TFileIO.WriteFile(AFileName, AContent);
end;

end.
