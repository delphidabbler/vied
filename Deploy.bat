:: Deploy script for Version Information Editor (VIEd).
::
:: This script compiles a release version of VIEd, creates a setup file for the
:: program then stores it in a zip file, along with a read-me file. Both the
:: setup file and zip file are labelled with the release version number.
::
:: This script has the following dependencies:
::
::   1) MSBuild & Delphi.
::   2) InfoZip's zip.exe. See https://delphidabbler.com/extras/info-zip
::   3) Inno Setup v5.6.1 or later Unicode version (not v6). See
::      https://www.innosetup.com/
::   4) DelphiDabbler Version Information Editor v2.15.1 or later. See
::      https://delphidabbler.com/software/vied
::   5) PowerShell.
::   6) HHC, the command line compiler supplied with Microsoft HTML Help
::      Workshop.
::
:: To use the script:

::   1) Start the Embarcadero RAD Studio Command Prompt to set the required
::      environment variables for MSBuild.
::   2) Set the ZipRoot environment variable to the directory where zip.exe is
::      installed.
::   3) Set the VIEdRoot environment variable to the directory where VIEd.exe is
::      installed.
::   4) Set the InnoSetupRoot environment variable to the directoty where Inno
::      Setup is installed.
::   5) Set the HHCRoot environment variable to the directory where the
::      Microsoft Help Compiler is installed.
::   6) Change directory to that where this script is located.
::   7) Run the script by entering Deploy with no parameters.
::
:: The script does the following:
::
::   1) Builds the VIEd executable using MSBuild, Delphi and a previously
::      released copy of VIEd.
::   2) Uses PowerShell to extract the release version from version information
::      embedded in the VIEd executable. This is used to set the version of
::      the setup program and to name the setup program and zip file.
::   3) Compiles the HTML Help file using HCC.
::   4) Builds the setup file using Inno Setup.
::   5) Creates the release zip file using Zip.exe.


@echo off

echo ======================
echo Deploying VIEd Release
echo ======================

:: Check for required environment variables

if "%ZipRoot%"=="" goto envvarerror
if "%VIEdRoot%"=="" goto envvarerror
if "%InnoSetupRoot%"=="" goto envvarerror

:: Set other variables that don't depend on version

set ProjectName=VIEd
set BuildRoot=.\_build
set Win32ExeDir=%BuildRoot%\Win32\Release\exe
set Win32ExeStub=%ProjectName%
set HelpExeDir=%BuildRoot%\help
set ReleaseDir=%BuildRoot%\release
set TempDir=%ReleaseDir%\~temp
set SrcDir=Source  
set DocsDir=Docs
set ReadMeFile=%DocsDir%\README.txt
set ISCC="%InnoSetupRoot%\ISCC.exe"
set HHC="%HHCRoot%\HHC.exe"

:: Make a clean directory structure

if exist %BuildRoot% rmdir /S /Q %BuildRoot%
mkdir %ReleaseDir%
mkdir %HelpExeDir%
mkdir %TempDir%

:: Build Pascal

echo.
echo -------
echo --- Building executable program
echo -------
echo.

setlocal

cd %SrcDir%

msbuild %ProjectName%.dproj /p:config=Release /p:platform=Win32

endlocal

:: Get version and set variables that depend on version

echo.
echo -------
echo --- Extracting version information
echo -------
echo.

PowerShell(Get-Command %Win32ExeDir%\%Win32ExeStub%.exe).FileVersionInfo.ProductVersion > "%TempDir%\~version"
set /p Version= < "%TempDir%\~version"

set SetupFileName=VIEd-Setup-%Version%
set ZipFile=%ReleaseDir%\vied-exe-%Version%.zip

:: Split Version into prefix and suffix with "-" delimiter: suffix may be empty

for /f "tokens=1,2 delims=-" %%a in ("%Version%") do (
  :: prefix and suffix required for Inno Setup
  set VersionPrefix=%%a
  set VersionSuffix=%%b
)
if not [%VersionSuffix%] == [] (
  :: prefix "-" when suffix not empty
  set VersionSuffix=-%VersionSuffix%
)

echo Release version number: %Version%

:: Build Help File

echo.
echo -------
echo --- Building help file
echo -------
echo.

setlocal

cd %SrcDir%

%HHC% .\Help\VIEd.chm

endlocal

echo Copying %HelpExeDir%\%ProjectName%.chm to %Win32ExeDir%

copy %HelpExeDir%\%ProjectName%.chm %Win32ExeDir%

:: Build Setup

echo.
echo -------
echo --- Building setup program
echo -------
echo.

setlocal

cd %SrcDir%

%ISCC% /DAppShortName=%ProjectName% /DAppVersion=%VersionPrefix% /DAppVersionSuffix=%VersionSuffix% /DSetupOutDir=%TempDir% /DSetupFileName=%SetupFileName% /DExeInDir=%Win32ExeDir% /DDocsInDir=%DocsDir% /DHelpInDir=%HelpExeDir% Install.iss

endlocal

:: Create zip files

echo.
echo -------
echo --- Creating zip files
echo -------
echo.

%ZipRoot%\zip.exe -j -9 %ZipFile% %TempDir%\%SetupFileName%.exe
%ZipRoot%\zip.exe -j -9 %ZipFile% %ReadMeFile%

:: Tidy up

echo.
echo -------
echo --- Tidying up
echo -------
echo.


echo Deleting %TempDir%

if exist %TempDir% rmdir /S /Q %TempDir%

:: Done

echo.
echo -------
echo --- Build completed
echo -------

goto end

:: Error messages

:envvarerror
echo.
echo ***ERROR: ZipRoot, VIEdRoot or InnoSteup environment variable not set
echo.
goto end

:: End

:end
