; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/
;
; Copyright (C) 2011-2024, Peter Johnson (www.delphidabbler.com).
;
; Version Information Editor install file generation script for use with Inno
; Setup.
;
; The Unicode version of Inno Setup 5.4.0 or later is required.
;
; The following defines use these macros that are predefined by ISPP:
;   SourcePath - path where this script is located
;   GetStringFileInfo - gets requested version info string from an executable
; 
; The following define, which must be defined on the command line using the
; /D option:
;   AppShortName - short name of project. Exe file will be named by appending
;     ".exe"
;   AppVersion - version number of the application (must contain only digits,
;     e.g. 1.2.3.4).
;   AppVersionSuffix - any suffix to the application version number (must start)
;     with "-" , e.g. "-beta.1"
;   SetupOutDir - setup program output directory relative to project root
;   SetupFileName - name of setup file to be created (without path)
;   ExeInDir - directory containing compiled .exe file relative to project root
;   DocsInDir - directory containing documentation relative to project root
;   HelpInDir - directory containing compiled help file relative to project root


#define ExeFile AppShortName + ".exe"
#define HelpFile "VIEd.chm"
#define LicenseFile "License.rtf"
#define ReadmeFile "ReadMe.txt"
#define ChangeLogFile "CHANGELOG.md"
#define InstDocsDir "Docs"
#define InstUninstDir "Uninstall"
#define OutDir SourcePath + "..\" + SetupOutDir
#define SrcExePath SourcePath + "..\" + ExeInDir + "\"
#define SrcDocsPath SourcePath + "..\" + DocsInDir + "\"
#define SrcHelpPath SourcePath + "..\" + HelpInDir + "\"
#define ExeProg SrcExePath + ExeFile
#define Company "DelphiDabbler.com"
#define AppPublisher "DelphiDabbler"
#define AppName "Version Information Editor"
#define Copyright GetStringFileInfo(ExeProg, LEGAL_COPYRIGHT)
#define WebAddress "delphidabbler.com"
#define WebURL "https://" + WebAddress + "/"
#define AppURL WebURL + "vied"
#define FullAppVersion AppVersion + AppVersionSuffix
#define RootPath SourcePath + "..\"

[Setup]
AppID={{C968D909-86D7-44CA-8ED1-7052DD327C09}
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppPublisher} {#AppName} {#FullAppVersion}
AppPublisher={#AppPublisher}
AppPublisherURL={#WebURL}
AppSupportURL={#AppURL}
AppUpdatesURL={#AppURL}
AppReadmeFile={app}\{#InstDocsDir}\{#ReadmeFile}
AppCopyright={#Copyright} ({#WebAddress})
AppComments=
AppContact=
DefaultDirName={pf}\{#AppPublisher}\VIEd
DefaultGroupName={#AppName}
AllowNoIcons=false
LicenseFile={#SrcDocsPath}{#LicenseFile}
Compression=lzma/ultra
SolidCompression=true
OutputDir={#OutDir}
OutputBaseFilename={#SetupFileName}
MinVersion=6.1sp1
RestartIfNeededByRun=false
PrivilegesRequired=poweruser
UsePreviousAppDir=true
UsePreviousGroup=true
UsePreviousSetupType=false
UsePreviousTasks=false
ShowLanguageDialog=no
LanguageDetectionMethod=none
InternalCompressLevel=ultra
InfoAfterFile=
InfoBeforeFile=
VersionInfoVersion={#AppVersion}
VersionInfoTextVersion={#AppVersion}
VersionInfoProductVersion={#AppVersion}
VersionInfoProductTextVersion={#FullAppVersion}
VersionInfoCompany={#Company}
VersionInfoDescription=Installer for {#AppName}
VersionInfoCopyright={#Copyright}
UninstallFilesDir={app}\{#InstUninstDir}
UpdateUninstallLogAppName=true
UninstallDisplayIcon={app}\{#ExeFile}
UserInfoPage=false

[Files]
; Executable files
Source: {#SrcExePath}{#ExeFile}; DestDir: {app}; Flags: uninsrestartdelete
Source: {#SrcHelpPath}{#HelpFile}; DestDir: {app}; Flags: ignoreversion
; Documentation
Source: {#SrcDocsPath}{#LicenseFile}; DestDir: {app}\{#InstDocsDir}; Flags: ignoreversion
Source: {#SrcDocsPath}{#ReadmeFile}; DestDir: {app}\{#InstDocsDir}; Flags: isreadme ignoreversion
Source: {#RootPath}{#ChangeLogFile}; DestDir: {app}\{#InstDocsDir}; Flags: ignoreversion

[Icons]
Name: {group}\{#AppName}; Filename: {app}\{#ExeFile}
Name: {group}\{cm:UninstallProgram,{#AppName}}; Filename: {uninstallexe}

[Run]
Filename: {app}\{#ExeFile}; Description: {cm:LaunchProgram,{#AppName}}; Flags: nowait postinstall skipifsilent

[Dirs]
Name: {app}\{#InstDocsDir}; Flags: uninsalwaysuninstall
Name: {app}\{#InstUninstDir}; Flags: uninsalwaysuninstall

[Registry]
; Register application and its path
Root: HKLM; Subkey: SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\{#ExeFile}; ValueType: string; ValueData: {app}\{#ExeFile}; Flags: uninsdeletekey
Root: HKLM; Subkey: SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\{#ExeFile}; ValueType: string; ValueName: Path; ValueData: {app}\; Flags: uninsdeletekey

[Messages]
; Brand installer
BeveledLabel={#Company}
