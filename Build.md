# Version Information Editor Build Instructions

## Introduction

_Version Information Editor (VIEd)_ is written in Object Pascal and is targeted at Delphi XE.

Development, debugging and testing of the executable program can take place entirely within the IDE providing some prerequisites are met. However releases must be built using the `Deploy.bat` script.

## Prerequisites

### Libraries & Components

Several DelphiDabbler components are required to compile _VIEd_. They are:

*   [Version Information Component](https://delphidabbler.com/software/verinfo) v3.3 or later.
*   [About Box Component](https://delphidabbler.com/software/aboutbox) v3.5.1 or later.
*   [Drop Files Components](https://delphidabbler.com/software/dropfiles) v5.0.2 or later.
*   [Window State Components](https://delphidabbler.com/software/wdwstate) v5.3.1 or later.

These must all be installed into the Delphi component palette since they are required at design time. The easiest way to do this is to install everything into the Delphi user design time package. [See here](http://www.delphidabbler.com/url/install-comp) if you need help doing this.

In addition, the following DelphiDabbler library unit must either be added to a suitable package or placed somewhere on Delphi's include path:

*   [Shell Folders Unit](https://delphidabbler.com/software/shellfolders) v2.3 or later.

### Tools

The following tools are required to build _VIEd_.

Tools marked with an asterisk are required when compiling from the IDE: compiles will fail if they are not installed and configured.

| Tools | Notes |
|-------|--------|
| Delphi XE | Later Delphi compilers may be suitable, but none have been tested. |
| MSBuild | This tool is installed with Delphi. Used directly by `Deploy.bat` to build _VIEd_. |
| BRCC32 * | This tool is installed with Delphi. Used in pre-build events to create `.res` files from custom `.rc` files. |
| Version Information Editor * † | v2.15.1 or later is required. Used in pre-build events to create an resource source file containing version information from `.vi` files. [Download here](https://github.com/delphidabbler/vied/releases). |
| HHC | This tool is the command line compiler supplied with Microsoft HTML Help Workshop. Used in `Deploy.bat` to compile the help file. |
| Inno Setup | v5.6.1 or later Unicode version (not v6). Used by `Deploy.bat` to create the installer. [Download here](https://www.innosetup.com/). |
| InfoZip's Zip tool | Used by `Deploy.bat` to create the release zip file. [Download here](https://delphidabbler.com/extras/info-zip). |
| PowerShell | Used by `Deploy.bat` to grab version information from the compiled `.exe` file. |

> † Yes, you do need an executable version of the tool you are going to build!

### Environment Variables

The following environment variables must be set to build _VIEd_

Environment variables marked with an asterisk are required when compiling from the IDE: compiles will fail if they are not set correctly. Such variables can be set using Delphi's _Tools | Options_ menu, going to the _Environment Variables_ page then creating the variable in _User System Overrides_ section.

All environment variables are required when creating releases using `Deploy.bat` or when compiling the help file by hand.

| Environment Variables | Notes |
|-----------------------|-------|
| MSBuild specific variables | The `rsvars.bat` script in the `Bin` sub-directory of the Delphi installation directory sets these variables to the required values. |
| `ZipRoot` | Set this to the directory where the InfoZip Zip tool is installed. |
| `VIEdRoot` * | Set this to the directory where Version Information Editor is installed (the `DelphiDabbler\VIEd` subdirectory of the 32 bit program files directory, by default). |
| `HHCRoot` | Set this to the directory where the HTML Help Workshop HHC command line compiler is installed. |
| `InnoSetupRoot` | Set this to the directory where the Unicode version of Inno Setup 5 is installed. |

You can configure the environment using a batch file similar to the following:

```batch
:: set path to Delphi XE installation (change directory if not using Delphi XE)
set DELPHIROOT=C:\Program Files (x86)\Embarcadero\RAD Studio\8.0

:: set environment variables required by MSBuild
call "%DELPHIROOT%\Bin\rsvars.bat"

:: set install path of tools (change as required)
set ZipRoot=C:\Tools
set VIEdRoot=C:\Program Files (x86)\DelphiDabbler\VIEd
set InnoSetup=C:\Program Files (x86)\Inno Setup 5
set HHCRoot=C:\Program Files (x86)\HTML Help Workshop
```

## Source Code

Download the _VIEd_ source code from the [`delphidabbler/vied`](https://github.com/delphidabbler/vied) GitHub repository. You can either clone it using Git or download a zip file containing the source.

After obtaining the source code you should have the following directory structure:
</p>

```text
/-+
  |
  +-- .git                  - present only if using git
  |
  +-- Docs                  - documentation
  |
  +-- Source                - source code
  |   |
  |   +-- Assets            - various files to include in resources
  |
  +-- Help                  - help file source code
      |
      +-- CSS               - style sheet for help HTML files
      |
      +-- HTML              - HTML help topic files
```

Before compiling remember to install the required [libraries & components](#libraries--components).

## Compiling

### From The Delphi IDE

Simply open the `.dproj` file in Delphi and compile. Providing the [prerequisites](#prerequisites) have been met, the program should compile without problems.

All compiler output is placed in a `_build` directory. This directory is ignored by Git. You can build either the Debug or Release build configurations, or both. Compiling both configurations results in the following directory tree being created:

```text
/-+
  |
  +-- .git                  - present only if using git
  |
  +-- _build                - top level build directory
  |   |
  |   +-- Win32             - directory for Win 32 builds (the only target)
  |       |
  |       +-- Debug         - directory containing debug binaries
  |       |   |
  |       |   +-- bin       - contains intermediate binary debug files
  |       |   |
  |       |   +-- exe       - contains the debug executable file
  |       |
  |       +-- Release       - directory containing release binaries
  |           |
  |           +-- bin       - contains intermediate binary release files
  |           |
  |           +-- exe       - contains the release executable file
  |
  +-- Docs                  - documentation
  ⁞
```

You can now hack away as you wish.

### Test Compiling The Help File

You cannot currently compile the help file from within the IDE. This must be done from the command line. To do this open a terminal on the project root directory. Make sure the `HHCRoot` environment variable is set, as described [above](#environment-variables). Now enter the following commands to  compile the help file:

```batch
md _build\help
cd Source
"%HHCRoot%\HHC" .\Help\VIEd.chm
cd ..
```

> You only need the `md` command if the `_build\help` directory does not exist.

To test the help file simply enter the following command in the same terminal:

```batch
_build\help\VIEd.chm
```

or just double click the `.chm` file in the `_build\help` directory in Explorer.

If you wish to test the help file from within the program, first compile the required build configuration then copy the help file from `_build\help` into the relevant `exe` directory. E.g. if you have done a Debug build then, using the same terminal as before, enter the following command:

```batch
copy _build\help\VIEd.chm _build\Win32\Debug\exe
```

### Creating A Release

To create a release ensure that all the [tools](#tools) have been installed. Then:

1. Open a terminal.
2. Run any configuration script you have created, or set the [environment variables](#environment-variables) manually.
3. Change directory into the root of the _VIEd_ source code.
4. Run `Deploy.bat` without any parameters.

`Deploy.bat` will:

1. Build the _VIEd_ executable.
2. Extract the release version information from the executable.
3. Compile the help file.
4. Compile the setup program.
5. Create a zip file containing the setup program and some documentation.

The release version information extracted in step 2 is used as the setup program's version number and is embedded in the file names of the setup program and zip file.

The release zip file is placed in the `release` sub-directory of `_build`:

```text
/-+
  ⁞
  +-- _build                - top level build directory
  |   |
  |   +-- help              - contains compiled help file
  |   |
  |   +-- release           - contains the release zip file
  |   |
  |   +-- Win32             - directory for Win 32 builds (the only target)
  |       |
  |       +-- Release       - directory containing release binaries
  |           |
  |           +-- bin       - contains intermediate binary release files
  |           |
  |           +-- exe       - contains the release executable file
  ⁞
```

> Notice that any pre-existing `_build\Win32\Debug` directory will have been deleted.

### Tidying Up

If you are using Git you can run

```test
git clean -fxd
```

to remove all unwanted files.

> ⚠️ Running the above command will remove the `_build` directory and all its contents, so ensure you copy any wanted files from there beforehand. The command will also remove Delphi's `__history` directory.

## License

If you are planning to re-use or modify any of the code, please see `SourceCodeLicenses.md` in the `Docs` directory for an overview of the various open source licenses that apply to the _Version Information Editor_ source code.
