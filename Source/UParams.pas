{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024-2025, Peter Johnson (www.delphidabbler.com).
 *
 * Implements a static class that processes the program's command line.
}


unit UParams;

interface

uses
  Generics.Collections,

  UMutableEnvVars;

(*
  Possible command lines:

    1) deffilepath {-E:EnvName=EnvValue}
    2) -makerc vifilename [rcfilename] {-E:EnvName=EnvValue}
    3) -open vifilename {-E:EnvName=EnvValue}
    4) {-E:EnvName=EnvValue}

  where {-E:EnvName=EnvValue} is zero or more iterations of -E:EnvName=EnvValue

  Command line can also be empty

*)

type
  TParams = class(TObject)
  public
    type
      TMode = (Normal, MakeRC, OpenVI);
  strict private
    class var
      fMode: TMode;
      fVIFileName: string;
      fRCFileName: string;
      fFileDlgInitialDir: string;
      fEnvVars: TMutableEnvVars;
    class function IsCmdParam(const AParamIdx: Integer): Boolean;
    class procedure ParseEnvironmentCommand(const Cmd: string);
    class procedure ParseMakeRCCommandParams(var CmdIdx: Integer);
    class procedure ParseOpenCommandParams(var CmdIdx: Integer);
  public
    class constructor Create;
    class destructor Destroy;
    class procedure ParseCommandLine;
    class property Mode: TMode read fMode;
    class property VIFileName: string read fVIFileName;
    class property RCFileName: string read fRCFileName;
    class property FileDlgInitialDir: string read fFileDlgInitialDir;
    class property EnvVars: TMutableEnvVars read fEnvVars;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Classes,
  Generics.Defaults,

  UUtils;

{ TParams }

class constructor TParams.Create;
begin
  fMode := TMode.Normal;
  fEnvVars := TMutableEnvVars.Create;
end;

class destructor TParams.Destroy;
begin
  fEnvVars.Free;
  inherited;
end;

class function TParams.IsCmdParam(const AParamIdx: Integer): Boolean;
begin
  Assert(Length(ParamStr(AParamIdx)) >= 1);
  Result := ParamStr(AParamIdx)[1] = '-';
end;

class procedure TParams.ParseCommandLine;
var
  I: Integer;
  CurrentCmdIdx: Integer;
begin
  if ParamCount = 0 then
    Exit;
  CurrentCmdIdx := 1;
  if not IsCmdParam(CurrentCmdIdx) then
  begin
    // 1st param not a command: it MUST be intial file dlg directory
    fFileDlgInitialDir := ParamStr(CurrentCmdIdx);
    Inc(CurrentCmdIdx);;
  end
  else if ParamStr(CurrentCmdIdx) = '-makerc' then
  begin
    // 1st param is the -makerc command
    fMode := TMode.MakeRC;
    ParseMakeRCCommandParams(CurrentCmdIdx);
  end
  else if ParamStr(CurrentCmdIdx) = '-open' then
  begin
    // 1st param is the -opem command
    fMode := TMode.OpenVI;
    ParseOpenCommandParams(CurrentCmdIdx);
  end;
  // Remainder of command line MUST be commands
  for I := CurrentCmdIdx to ParamCount do
  begin
    if not IsCmdParam(I) then
      raise Exception.Create('Command beginning with "-" expected');
    if Length(ParamStr(I)) < 2 then
      raise Exception.CreateFmt('Unrecognised command: %s', [ParamStr(I)]);
    if StartsStr('-E', ParamStr(I)) then
      ParseEnvironmentCommand(ParamStr(I))
    else
      raise Exception.CreateFmt('Invalid command: %s', [ParamStr(I)]);
  end;
end;

class procedure TParams.ParseEnvironmentCommand(const Cmd: string);
var
  CmdName: string;
  EnvVar: string;
  EnvVarName: string;
  EnvVarValue: string;
begin
  // Command must have form -E:name=value, where name MUST NOT be empty and
  // value MAY be empty
  if not SplitStr(Cmd, ':', CmdName, EnvVar)
    or not SplitStr(EnvVar, '=', EnvVarName, EnvVarValue)
    or (EnvVarName = '') then
    raise Exception.CreateFmt('Malformed -E command: %s', [Cmd]);
  // Add defined variable to dictionary, overwriting any existing environment
  // variable with the same name
  fEnvVars.AddOrSet(TEnvironmentVar.Create(EnvVarName, EnvVarValue));
end;

class procedure TParams.ParseMakeRCCommandParams(var CmdIdx: Integer);
begin
  // jump to next parameter after the command, which MUST be VI file name
  Inc(CmdIdx);
  if IsCmdParam(CmdIdx) then
    raise Exception.Create('.vi file name expected after -makerc command');
  fVIFileName := ParamStr(CmdIdx);
  // next parameter MAY be RC file name
  Inc(CmdIdx);
  if (ParamStr(CmdIdx) <> '') and not IsCmdParam(CmdIdx) then
  begin
    fRCFileName := ParamStr(CmdIdx);
    Inc(CmdIdx);
  end;
end;

class procedure TParams.ParseOpenCommandParams(var CmdIdx: Integer);
begin
  // jump to next parameter after the command, which MUST be VI file name
  Inc(CmdIdx);
  if IsCmdParam(CmdIdx) then
    raise Exception.Create('.vi file name expected after -open command');
  fVIFileName := ParamStr(CmdIdx);
  Inc(CmdIdx);
end;

end.
