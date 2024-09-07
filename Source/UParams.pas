unit UParams;

interface

uses
  Generics.Collections,

  UMutableEnvVars;

(*
  Possible command lines:

    1) deffilepath {-env:EnvName=EnvValue}
    2) -makerc vifilename [rcfilename] {-env:EnvName=EnvValue}
    3) {-env:EnvName=EnvValue}

  where {-env:EnvName=EnvValue} is zero or more iterations of
  -env:EnvName=EnvValue

  Command line can also be empty

  Logic:

  if param count = 0 then exit

  if 1st param is -makerc then
    store that we're
    if 2nd second param is a file name then
      store
    2nd parameter must be a file name: store as vifilename
    if 3rd parameter is a file name then store as rcfilename
*)

type
  TParams = class(TObject)
  public
    type
      TMode = (Normal, MakeRC);
  strict private
    class var
      fMode: TMode;
      fVIFileName: string;
      fRCFileName: string;
      fFileDlgInitialDir: string;
      fEnvVars: TMutableEnvVars;
    class procedure ParseEnvironmentCommand(const Cmd: string);
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

class procedure TParams.ParseCommandLine;

  function IsCmdParam(const AParamIdx: Integer): Boolean;
  begin
    Assert(Length(ParamStr(AParamIdx)) >= 1);
    Result := ParamStr(AParamIdx)[1] = '-';
  end;

var
  I: Integer;
  CurrentCmdIdx: Integer;
begin
  if ParamCount = 0 then
    Exit;
  CurrentCmdIdx := 1;
  if not IsCmdParam(CurrentCmdIdx) then
  begin
    // 1st param not a command: intial file dlg directory is 1st parameter
    fFileDlgInitialDir := ParamStr(CurrentCmdIdx);
    Inc(CurrentCmdIdx);;
  end
  else if ParamStr(CurrentCmdIdx) = '-makerc' then
  begin
    // 1st param is the -makerc command
    fMode := TMode.MakeRC;
    Inc(CurrentCmdIdx);
    // 2nd command MUST be VI file name
    if IsCmdParam(CurrentCmdIdx) then
      raise Exception.Create('.vi file name expected after -makerc command');
    fVIFileName := ParamStr(CurrentCmdIdx);
    Inc(CurrentCmdIdx);
    // 3rd command MAY be RC file name
    if (ParamStr(CurrentCmdIdx) <> '') and not IsCmdParam(CurrentCmdIdx) then
    begin
      fRCFileName := ParamStr(CurrentCmdIdx);
      Inc(CurrentCmdIdx);
    end;
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

end.
