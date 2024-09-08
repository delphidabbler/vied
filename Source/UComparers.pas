unit UComparers;

interface

uses
  // Delphi
  Generics.Defaults;

type
  ///  <summary>
  ///  Case insenstive string equality comparer.
  ///  </summary>
  TTextEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
  public
    ///  <summary>Checks if two strings are equal, ignoring case.</summary>
    function Equals(const Left, Right: string): Boolean; override;
    ///  <summary>Gets hash of lower case version of given string.</summary>
    function GetHashCode(const Value: string): Integer; override;
  end;

type
  ///  <summary>
  ///  Case senstive string equality comparer.
  ///  </summary>
  TStringEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
  public
    ///  <summary>Checks if two strings are equal, taking account of case.
    ///  </summary>
    function Equals(const Left, Right: string): Boolean; override;
    ///  <summary>Gets hash of given string.</summary>
    function GetHashCode(const Value: string): Integer; override;
  end;

implementation

uses
  // VCL
  SysUtils,
  // Project
  UUtils;

{ TTextEqualityComparer }

function TTextEqualityComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := SameText(Left, Right, loInvariantLocale);
end;

function TTextEqualityComparer.GetHashCode(const Value: string): Integer;
begin
  // Comparison takes place (i.e. Equals gets called) only if hashes are same.
  // So we must ignore case in hash if two strings that differ only in case are
  // to be considered same.
  Result := ElfHash(LowerCase(Value));
end;

{ TStringEqualityComparer }

function TStringEqualityComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := SameStr(Left, Right, loInvariantLocale);
end;

function TStringEqualityComparer.GetHashCode(const Value: string): Integer;
begin
  Result := ElfHash(Value);
end;

end.
