@rem ---------------------------------------------------------------------------
@rem Script used to build Version Information Editor's resource files from
@rem source.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 29 Apr 2008 - Original version.
@rem ---------------------------------------------------------------------------

@echo off

@rem ---------------------------------------------------------------------------
@rem Script used to build the DelphiDabbler Version Information Editor.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 16 Apr 2008 - Original version.
@rem
@rem Requires the following compilers and build tools:
@rem   Borland Delphi7
@rem   Borland BRCC32 from Delphi 7 installation
@rem   DelphiDabbler Version Information Editor from www.delphidabbler.com
@rem   Microsoft HTML Help compiler v1.1
@rem
@rem Also requires the following environment variables:
@rem   DELPHI7 to be set to the install directory of Delphi 7
@rem   DELPHIDABLIBD7 to be set to the install directory of the required
@rem     DelphiDabbler components on Delphi 7.
@rem
@rem Switches: exactly one of the following must be provided
@rem   all - build everything
@rem   res - build binary resource files only
@rem   pas - build Delphi Pascal project only
@rem   exe - build Pascal and resource files
@rem   help - build help file
@rem
@rem ---------------------------------------------------------------------------

@echo off

setlocal


rem ----------------------------------------------------------------------------
rem Check that required environment variables exist
rem ----------------------------------------------------------------------------

:CheckEnvVars

echo Checking predefined environment environment variables
if not defined DELPHI7 goto BadDELPHI7Env
echo Done.
echo.
goto SetEnvVars

rem we have at least one undefined env variable

:BadDELPHI7Env
set ErrorMsg=DELPHI7 Environment variable not defined
goto Error


rem ----------------------------------------------------------------------------
rem Set up required environment variables
rem ----------------------------------------------------------------------------

:SetEnvVars
echo Setting Up Environment

rem directories

rem source directory
set SrcDir=..\Source\
rem binary files directory
set BinDir=..\Bin\

rem Borland Resource Compiler - use full path since maybe multple installations
set BRCC32Exe="%DELPHI7%\Bin\BRCC32.exe"
rem DelphiDabbler Version Information Editor - assumed to be on the path
set VIEdExe=VIEd.exe

rem ----------------------------------------------------------------------------
rem Build resource files
rem ----------------------------------------------------------------------------

echo Building Resources
echo.

rem set required env vars

rem Ver info resource
set VerInfoBase=Version
set VerInfoSrc=%SrcDir%%VerInfoBase%.vi
set VerInfoTmp=%SrcDir%%VerInfoBase%.rc
set VerInfoRes=%BinDir%%VerInfoBase%.res
rem Main resource
set MainBase=Resources
set MainSrc=%SrcDir%%MainBase%.rc
set MainRes=%BinDir%%MainBase%.res

rem Compile version information resource

echo Compiling %VerInfoSrc% to %VerInfoRes%
rem VIedExe creates temp resource .rc file from .vi file
set ErrorMsg=
%VIEdExe% -makerc %VerInfoSrc%
if errorlevel 1 set ErrorMsg=Failed to compile %VerInfoSrc%
if not "%ErrorMsg%"=="" goto VerInfoRes_End
rem BRCC32Exe compiles temp resource .rc file to required .res
%BRCC32Exe% %VerInfoTmp% -fo%VerInfoRes%
if errorlevel 1 set ErrorMsg=Failed to compile %VerInfoTmp%
if not "%ErrorMsg%"=="" goto VerInfoRes_End
echo Done
echo.

:VerInfoRes_End
if exist %VerInfoTmp% del %VerInfoTmp%
if not "%ErrorMsg%"=="" goto Error

rem Compile Main resource

echo Compiling %MainSrc% to %MainRes%
%BRCC32Exe% %MainSrc% -fo%MainRes%
if errorlevel 1 goto MainRes_Error
echo Done
echo.
goto MainRes_End

:MainRes_Error
set ErrorMsg=Failed to compile %MainSrc%
goto Error

:MainRes_End
goto End

rem ----------------------------------------------------------------------------
rem Handle errors
rem ----------------------------------------------------------------------------

:Error
echo.
echo *** ERROR: %ErrorMsg%
echo.


rem ----------------------------------------------------------------------------
rem Finished
rem ----------------------------------------------------------------------------

:End
echo.
echo DONE.

endlocal
