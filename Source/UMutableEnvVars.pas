unit UMutableEnvVars;

interface

uses
  Generics.Collections,
  Classes;


type
  TEnvironmentVar = TPair<string,string>;

  TMutableEnvVars = class(TObject)
  strict private
    var
      fEnvVars: TDictionary<string,string>;

    class procedure GetAllSystemEnvVars(const EnvVars: TStrings);
    procedure RecordSystemEnvVars;
  public
    { TODO: Remove any unused methods }
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AddOrSet(const E: TEnvironmentVar);
    function Contains(const Name: string): Boolean;
    function GetValue(const Name: string): string;
    function TryGetValue(const Name: string; out Value: string): Boolean;
  end;

implementation

uses
  SysUtils,
  Generics.Defaults,
  Windows,

  UUtils;

{ TMutableEnvVars }

procedure TMutableEnvVars.AddOrSet(const E: TEnvironmentVar);
begin
  fEnvVars.AddOrSetValue(E.Key, E.Value);
end;

procedure TMutableEnvVars.AfterConstruction;
begin
  inherited;
  RecordSystemEnvVars;
end;

function TMutableEnvVars.Contains(const Name: string): Boolean;
begin
  Result := fEnvVars.ContainsKey(Name);
end;

constructor TMutableEnvVars.Create;
begin
  inherited Create;
  fEnvVars := TDictionary<string,string>.Create(
    { TODO: Create new TTextComparer object to replace following comparer.
            Put in its own unit along with a similar TStringComparer for use in
            UVIData.}
    TDelegatedEqualityComparer<string>.Create(
      function (const Left, Right: string): Boolean
      begin
        Result := SameText(Left, Right, loInvariantLocale);
      end,
      function (const S: string): Integer
      begin
        Result := ElfHash(LowerCase(S));
      end
    )
  );
end;

destructor TMutableEnvVars.Destroy;
begin
  fEnvVars.Free;
  inherited;
end;

class procedure TMutableEnvVars.GetAllSystemEnvVars(const EnvVars: TStrings);
var
  PEnvVars: PChar;    // pointer to start of environment block
  PEnvEntry: PChar;   // pointer to an environment string in block
begin
  Assert(Assigned(EnvVars));
  EnvVars.Clear;
  // Get reference to environment block for this process
  PEnvVars := GetEnvironmentStrings;
  if PEnvVars <> nil then
  begin
    // We have a block: extract strings from it
    // Env strings are #0 terminated and list ends an additional #0, e.g.:
    // Foo=Lorem#0Bar=Ipsum#0Raboof=Dolore#0#0
    PEnvEntry := PEnvVars;
    try
      while PEnvEntry^ <> #0 do
      begin
        EnvVars.Add(PEnvEntry);
        Inc(PEnvEntry, StrLen(PEnvEntry) + 1);  // +1 to skip terminating #0
      end;
    finally
      FreeEnvironmentStrings(PEnvVars);
    end;
  end;
end;

function TMutableEnvVars.GetValue(const Name: string): string;
begin
  Result := fEnvVars[Name];
end;

procedure TMutableEnvVars.RecordSystemEnvVars;
var
  AllEnvVars: TStringList;
  Idx: Integer;
  Name, Value: string;
begin
  AllEnvVars := TStringList.Create;
  try
    GetAllSystemEnvVars(AllEnvVars);
    for Idx := 0 to Pred(AllEnvVars.Count) do
    begin
      Name := Trim(AllEnvVars.Names[Idx]);
      // ignore system env vars with empty name (they can exist)
      if Name <> '' then
      begin
        Value := AllEnvVars.ValueFromIndex[Idx];
        // add env var to dictionary: overwrite any duplicates
        // duplicates shouldn't happen but might because we trimmed the names
        fEnvVars.AddOrSetValue(Name, Value);
      end;
    end;
  finally
    AllEnvVars.Free;
  end;
end;

function TMutableEnvVars.TryGetValue(const Name: string;
  out Value: string): Boolean;
begin
  if not Contains(Name) then
    Exit(False);
  Value := GetValue(Name);
  Result := True;
end;

end.
