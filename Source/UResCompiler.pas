{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2002-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Unit exposing a function that executes an external resource compiler to
 * compile a resource file.
}


unit UResCompiler;


interface


uses
  // Delphi
  Classes,
  // Project
  UVInfo;


function ExecuteCompiler(const Project: TVInfo;
  const CompilerPath, CmdTemplate, OutFile: string): Integer;
  {Excutes resource compiler.
    @param Project [in] Version information project.
    @param CompilerPath [in] Fully specified path to compiler.
    @param CmdTemplate [in] Template for command line passed to compiler.
    @param OutFile [in] Output file.
    @return 0 on success, -MaxInt if compiler can't be executed or compiler
      error code if source can't be compiled.
  }


implementation


uses
  // Delphi
  Windows, SysUtils;


function TempFolder: string;
  {Gets system's temporary folder.
    @return Required folder.
  }
var
  PathBuf: array[0..MAX_PATH] of Char;  // holds temp folder as #0 term str
begin
  // Get temporary folder
  GetTempPath(MAX_PATH, PathBuf);
  Result := StrPas(PathBuf);
end;

function ExecAndWait(const CmdLine: string; const ShowWindow: Word): Integer;
  {Runs an application command line.
    @param CmdLine [in] Command line of application to be run.
    @param ShowWindow [in] Windows flag that determines how application is
      displayed.
    @return Program exit code or -MaxInt if program could not be started.
  }
var
  StartupInfo: TStartupInfo;        // start-up information passed to process
  ProcessInfo: TProcessInformation; // information about the process
  ProcessExitCode: DWord;           // the process's exit code
begin
  // Assume failure
  Result := -MaxInt;
  // Set up StartupInfo structure
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);        // size of structure
    wShowWindow := ShowWindow;        // how window is displayed
    dwFlags := STARTF_USESHOWWINDOW;  // use the wShowWindow field
  end;
  // Now execute the application
  if CreateProcess(
    nil,                // application name is passed as part of command line
    PChar(CmdLine),     // the command line, including application name
    nil, nil,           // security and thread handles: ignored
    False,              // application inherits handles if redirecting
    0,                  // no special creation flags
    nil,                // no environment passed
    nil,                // don't specify a current directory
    StartupInfo,        // startup information (see above)
    ProcessInfo         // holds information about process once started
  ) then
  begin
    try
      // Wait for application to complete
      if WaitForSingleObject(ProcessInfo.hProcess, INFINITE)
        = WAIT_OBJECT_0 then
        // It's completed - get its exit code and return it
        if GetExitCodeProcess(ProcessInfo.hProcess, ProcessExitCode) then
          Result := Integer(ProcessExitCode)
    finally
      // Tidy up
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end;
end;

function ExecuteCompiler(const Project: TVInfo;
  const CompilerPath, CmdTemplate, OutFile: string): Integer;
  {Excutes resource compiler.
    @param Project [in] Version information project.
    @param CompilerPath [in] Fully specified path to compiler.
    @param CmdTemplate [in] Template for command line passed to compiler.
    @param OutFile [in] Output file.
    @return 0 on success, -MaxInt if compiler can't be executed or compiler
      error code if source can't be compiled.
  }
const
  cTempRCFileName = 'VIXTmp.rc';  // name of temporary resource source file
var
  RCFile: string;       // full path to temp resource file
  CompilerCmd: string;  // command line passed to compiler

  // ---------------------------------------------------------------------------
  function ExpandCmdTemplate(CmdTemplate, RCFile, ResFile: string): string;
    {Replaces <SRC> and <BIN> placeholders in CmdTemplate with names of files
    given by RCFile and ResFile respectively}
  begin
    Result := StringReplace(CmdTemplate, '<SRC>', RCFile, [rfReplaceAll]);
    Result := StringReplace(Result, '<BIN>', OutFile, [rfReplaceAll]);
  end;
  // ---------------------------------------------------------------------------

begin
  // Write project to temporary .rc file
  RCFile := TempFolder + cTempRCFileName;
  Project.SaveToResourceFile(RCFile);
  // Compile temp source .rc file to given output binary file
  CompilerCmd := ExpandCmdTemplate(CmdTemplate, RCFile, OutFile);
  Result := ExecAndWait(CompilerPath + ' ' + CompilerCmd, SW_HIDE);
  // Delete the temporary .rc file
  if FileExists(RCFile) then
    SysUtils.DeleteFile(RCFile);
end;

end.

