; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/
;
; Copyright (C) 2011-2022, Peter Johnson (www.delphidabbler.com).
;
; Version Information Editor install file generation script for use with Inno
; Setup.
;
; The Unicode version of Inno Setup 5.4.0 or later is required.


; Deletes "Release " from beginning of S
#define DeleteToVerStart(str S) \
  /* assumes S begins with "Release " followed by version as x.x.x */ \
  Local[0] = Copy(S, Len("Release ") + 1, 99), \
  Local[0]

; The following defines use these macros that are predefined by ISPP:
;   SourcePath - path where this script is located
;   GetStringFileInfo - gets requested version info string from an executable
;   GetFileProductVersion - gets product version info string from an executable

#define ExeFile "VIEd.exe"
#define HelpFile "VIEd.chm"
#define LicenseFile "License.rtf"
#define ReadmeFile "ReadMe.txt"
#define ChangeLogFile "..\CHANGELOG.md"
#define InstDocsDir "Docs"
#define InstUninstDir "Uninstall"
#define OutDir SourcePath + "..\Exe"          /* SourcePath is predefined */
#define SrcExePath SourcePath + "..\Exe\"
#define SrcDocsPath SourcePath + "..\Docs\"
#define ExeProg SrcExePath + ExeFile
#define Company "DelphiDabbler.com"
#define AppPublisher "DelphiDabbler"
#define AppName "Version Information Editor"
#define AppVersion DeleteToVerStart(GetFileProductVersion(ExeProg))
#define Copyright GetStringFileInfo(ExeProg, LEGAL_COPYRIGHT)
#define WebAddress "www.delphidabbler.com"
#define WebURL "http://" + WebAddress + "/"
#define AppURL WebURL + "vied"

[Setup]
AppID={{C968D909-86D7-44CA-8ED1-7052DD327C09}
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppPublisher} {#AppName} {#AppVersion}
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
OutputBaseFilename=VIEd-Setup-{#AppVersion}
MinVersion=5.1
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
VersionInfoCompany={#Company}
VersionInfoDescription=Installer for {#AppName}
VersionInfoTextVersion={#AppVersion}
VersionInfoCopyright={#Copyright}
UninstallFilesDir={app}\{#InstUninstDir}
UpdateUninstallLogAppName=true
UninstallDisplayIcon={app}\{#ExeFile}
UserInfoPage=false

[Files]
; Executable files
Source: {#SrcExePath}{#ExeFile}; DestDir: {app}; Flags: uninsrestartdelete
Source: {#SrcExePath}{#HelpFile}; DestDir: {app}; Flags: ignoreversion
; Documentation
Source: {#SrcDocsPath}{#LicenseFile}; DestDir: {app}\{#InstDocsDir}; Flags: ignoreversion
Source: {#SrcDocsPath}{#ReadmeFile}; DestDir: {app}\{#InstDocsDir}; Flags: isreadme ignoreversion
Source: {#ChangeLogFile}; DestDir: {app}\{#InstDocsDir}; Flags: ignoreversion

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
