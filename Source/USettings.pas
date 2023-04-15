{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1998-2023, Peter Johnson (www.delphidabbler.com).
 *
 * Handles configuration information for Version Information Editor.
}


unit USettings;

interface


type

  {
  TVISettingID:
    Enumeration identifying all value names that are managed by the Settings
    object. These identifiers are used by code calling Settigns object to
    identify a data value.
  }
  TVISettingID = (
    siAutoValidate,       // whether program auto-validates entries
    siDescribeFileFlags,  // whether file flags are described or numeric values
    siCompilerPath,       // path to external compiler
    siCompilerCmdLine,    // command line for external compiler
    siNoCompilerCheck,    // whether to check for res compiler at start up
    siHasRun,             // whether program has run before
    siUTF8Encoding        // whether .vi files are UTF-8 (true) or ANSI (false)
  );

  {
  IVISettings:
    Interface to persistent setting manager object: permits persistent setting
    to be stored and retrieved.
  }
  IVISettings = interface(IUnknown)
    ['{E80DCFE5-04AE-4FAA-A4A0-80BAC3E1BBC1}']
    function ReadInt(const ID: TVISettingID): Integer;
      {Reads an integer valued setting.
        @param ID [in] ID of setting.
        @return Required value.
      }
    procedure WriteInt(const ID: TVISettingID; Value: Integer);
      {Writes an integer valued setting item.
        @param ID [in] ID of setting.
        @param Value [in] Required value.
      }
    function ReadStr(const ID: TVISettingID): WideString;
      {Reads a string valued setting.
        @param ID [in] ID of setting.
        @return Required value.
      }
    procedure WriteStr(const ID: TVISettingID; const Value: WideString);
      {Writes a string valued setting item.
        @param ID [in] ID of setting.
        @param Value [in] Required value.
      }
    function ReadBool(const ID: TVISettingID): Boolean;
      {Reads a Boolean valued setting.
        @param ID [in] ID of setting.
        @return Required value.
      }
    procedure WriteBool(const ID: TVISettingID; Value: Boolean);
      {Writes a Boolean valued setting item.
        @param ID [in] ID of setting.
        @param Value [in] Required value.
      }
    function MainWindowKey: WideString;
      {Gets key used to store dimensions of main window.
        @return Required key.
      }
  end;

function Settings: IVISettings;
  {Gets reference to singleton settings object. Creates singleton on first use.
    @return Settings object reference.
  }

implementation

uses
  // Delphi
  SysUtils, Registry;

const

  {
  Registry key organisation below base sub key. Plain text represents a sub key
  and %xx% represents a data value name:

    GUI                       persistent GUI properties
      MainWdw                 position and state of main window
        %Height% *              height of window
        %Left% *                position of left side of window
        %State% *               window state maximised, minimised, normal
        %Top% *                 position of window top
        %Width% *               width of window
    Options                   user options
      %AutoValidate%            whether entries are validated automatically
      %DescribeFileFlags%       whether file flags are described or are numbers
      %UTF8Encoding%            whether .vi is written in UTF-8 encoding
    Compiler                  information about external resource compiler
      %Path%                    path to external compiler
      %CmdLine%                 command line to pass to external compiler
      %NoCheck%                 whether to check for compiler at start up
    %HasRun%                  true if program has run before

  Key:
    *   these values set by the TPJRegWdwState component, not via Settings
        object.
  }

  // Registry keys
  cBaseRegKey = '\Software\DelphiDabbler\VIEd\2.0';
  cGUIKey = cBaseRegKey + '\GUI';
  cMainWdwKey = cGUIKey + '\MainWdw';
  cOptionsKey = cBaseRegKey + '\Options';
  cCompilerKey = cBaseRegKey + '\Compiler';

  // Value names and default values
  cTable: array[TVISettingID] of record
    // Table of registry sub keys, value names and default values for properties
    Key, ValName, DefVal: string
  end =
  (
    ( // siAutoValidate
      Key:        cOptionsKey;
      ValName:    'AutoValidate';
      DefVal:     '0'
    ),
    ( // siDescribeFileFlags
      Key:        cOptionsKey;
      ValName:    'DescribeFileFlags';
      DefVal:     '0'
    ),
    ( // siCompilerPath
      Key:        cCompilerKey;
      ValName:    'Path';
      DefVal:     ''
    ),
    ( // siCompilerCmdLine
      Key:        cCompilerKey;
      ValName:    'CmdLine';
      DefVal:     ''
    ),
    ( // siNoCompilerCheck
      Key:        cCompilerKey;
      ValName:    'NoCheck';
      DefVal:     '0'
    ),
    ( // siHasRun
      Key:        cBaseRegKey;
      ValName:    'HasRun';
      DefVal:     '0'
    ),
    ( // siUTF8Encoding
      Key:        cOptionsKey;
      ValName:    'UTF8Encoding';
      DefVal:     '0';
    )
  );

type

  {
  TVISettings:
    Class that implements the IVISettings interface and retrieves and stores
    persistent data using registry.
  }
  TVISettings = class(TInterfacedObject, IVISettings)
  protected
    { ISettings methods }
    function ReadInt(const ID: TVISettingID): Integer;
      {Reads an integer valued setting.
        @param ID [in] ID of setting.
        @return Required value.
      }
    procedure WriteInt(const ID: TVISettingID; Value: Integer);
      {Writes an integer valued setting item.
        @param ID [in] ID of setting.
        @param Value [in] Required value.
      }
    function ReadStr(const ID: TVISettingID): WideString;
      {Reads a string valued setting.
        @param ID [in] ID of setting.
        @return Required value.
      }
    procedure WriteStr(const ID: TVISettingID; const Value: WideString);
      {Writes a string valued setting item.
        @param ID [in] ID of setting.
        @param Value [in] Required value.
      }
    function ReadBool(const ID: TVISettingID): Boolean;
      {Reads a Boolean valued setting.
        @param ID [in] ID of setting.
        @return Required value.
      }
    procedure WriteBool(const ID: TVISettingID; Value: Boolean);
      {Writes a Boolean valued setting item.
        @param ID [in] ID of setting.
        @param Value [in] Required value.
      }
    function MainWindowKey: WideString;
      {Gets key used to store dimensions of main window.
        @return Required key.
      }
  end;


{ TVISettings }

function TVISettings.MainWindowKey: WideString;
  {Gets key used to store dimensions of main window.
    @return Required key.
  }
begin
  Result := cMainWdwKey;
end;

function TVISettings.ReadBool(const ID: TVISettingID): Boolean;
  {Reads a Boolean valued setting.
    @param ID [in] ID of setting.
    @return Required value.
  }
var
  Reg: TRegistry; // registry object
begin
  // Set default result in case we can't read from registy
  Result := Boolean(StrToInt(cTable[ID].DefVal));
  Reg := TRegistry.Create;
  try
    // Attempt to get required value from appropriate sub-key in registry
    if Reg.OpenKeyReadOnly(cTable[ID].Key)
      and Reg.ValueExists(cTable[ID].ValName) then
      Result := Reg.ReadBool(cTable[ID].ValName);
  finally
    Reg.Free;
  end;
end;

function TVISettings.ReadInt(const ID: TVISettingID): Integer;
  {Reads an integer valued setting.
    @param ID [in] ID of setting.
    @return Required value.
  }
var
  Reg: TRegistry; // registry object
begin
  // Set default result in case we can't read from registy
  Result := StrToInt(cTable[ID].DefVal);
  Reg := TRegistry.Create;
  try
    // Attempt to get required value from appropriate sub-key in registry
    if Reg.OpenKeyReadOnly(cTable[ID].Key)
      and Reg.ValueExists(cTable[ID].ValName) then
      Result := Reg.ReadInteger(cTable[ID].ValName)
  finally
    Reg.Free;
  end;
end;

function TVISettings.ReadStr(const ID: TVISettingID): WideString;
  {Reads a string valued setting.
    @param ID [in] ID of setting.
    @return Required value.
  }
var
  Reg: TRegistry; // registry object
begin
  // Set default result in case we can't read from registy
  Result := cTable[ID].DefVal;
  Reg := TRegistry.Create;
  try
    // Attempt to get required value from appropriate sub-key in registry
    if Reg.OpenKeyReadOnly(cTable[ID].Key)
      and Reg.ValueExists(cTable[ID].ValName) then
      Result := Reg.ReadString(cTable[ID].ValName)
  finally
    Reg.Free;
  end;
end;

procedure TVISettings.WriteBool(const ID: TVISettingID; Value: Boolean);
  {Writes a Boolean valued setting item.
    @param ID [in] ID of setting.
    @param Value [in] Required value.
  }
var
  Reg: TRegistry;   // the registry object
begin
  Reg := TRegistry.Create;
  try
    // Open required key and write value with appropriate name
    if Reg.OpenKey(cTable[ID].Key, True) then
      Reg.WriteBool(cTable[ID].ValName, Value);
  finally
    Reg.Free;
  end;
end;

procedure TVISettings.WriteInt(const ID: TVISettingID; Value: Integer);
  {Writes an integer valued setting item.
    @param ID [in] ID of setting.
    @param Value [in] Required value.
  }
var
  Reg: TRegistry;   // the registry object
begin
  Reg := TRegistry.Create;
  try
    // Open required key and write value with appropriate name
    if Reg.OpenKey(cTable[ID].Key, True) then
      Reg.WriteInteger(cTable[ID].ValName, Value);
  finally
    Reg.Free;
  end;
end;

procedure TVISettings.WriteStr(const ID: TVISettingID; const Value: WideString);
  {Writes a string valued setting item.
    @param ID [in] ID of setting.
    @param Value [in] Required value.
  }
var
  Reg: TRegistry;   // the registry object
begin
  Reg := TRegistry.Create;
  try
    // Open required key and write value with appropriate name
    if Reg.OpenKey(cTable[ID].Key, True) then
      Reg.WriteString(cTable[ID].ValName, Value);
  finally
    Reg.Free;
  end;
end;

var
  // Stores reference to singleton setting object
  PvtSettings: IVISettings;

function Settings: IVISettings;
  {Gets reference to singleton settings object. Creates singleton on first use.
    @return Settings object reference.
  }
begin
  if not Assigned(PvtSettings) then
    PvtSettings := TVISettings.Create;
  Result := PvtSettings;
end;

initialization

finalization

PvtSettings := nil;

end.
