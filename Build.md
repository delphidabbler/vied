# Version Information Editor Build Instructions

## Introduction

_Version Information Editor_ is written in Object Pascal and is targeted at Delphi XE. The Delphi IDE can be used to modify the source and to perform test builds. Final builds should be done using the provided makefile, but you can get away with using the IDE if you don't change any resources.

## Dependencies

Several DelphiDabbler libraries and components are required to compile _Version Information Editor_. They are:

*   [Version Information Component](https://delphidabbler.com/software/verinfo) v3.3 or later.
*   [About Box Component](https://delphidabbler.com/software/aboutbox) v3.5.1 or later.
*   [Drop Files Components](https://delphidabbler.com/software/dropfiles) v5.0.2 or later.
*   [Window State Components](https://delphidabbler.com/software/wdwstate) v5.3.1 or later.
*   [Shell Folders Unit](https://delphidabbler.com/software/shellfolders) v2.3 or later.

These components must be installed into the Delphi component palette since they are required at design time.

All the components and classes must be installed in the same directory, so the easiest thing to do is to install everything into the Delphi user design time package. [See here](http://www.delphidabbler.com/url/install-comp) if you need help doing this.

## Build Tools

The following tools are required to build _VIEd_.

### Delphi

A copy of the Delphi XE command line compiler is required to build the object Pascal code from the provided makefile.

You can use the Delphi IDE to edit the code and test compile it, but final builds should be created using the makefile, which requires the following tools that are supplied with Delphi:

The following command line tools are required to compile the whole project using the build scripts.

* `DCC32` - The Delphi command line compiler.
* `BRCC32` - The Borland resource compiler. This is used to compile resource source (`.rc`) files.
* `MAKE` - The Borland Make tool. This is used to build the project from the makefile.

The `DELPHIROOT` environment variable must be set to the install directory of the version of Delphi being used. All the above tools are expected to be located in the `Bin` sub-directory of `DELPHIROOT`.

### DelphiDabbler Version Information Editor

Yes, you need an executable version of the tool you are going to build! v2.14.0 or later is required. This tool is used to compile the program's version information (`.vi`) files into intermediate resource source (`.rc`) files.

You can get the latest stable version from [https://delphidabbler.com/software/vied](https://delphidabbler.com/software/vied).

Version Information Editor is expected to be on the system path unless its install directory is specified by the `VIEDROOT` environment variable.

### Inno Setup

The Unicode version of the Inno setup command line compiler is needed to create _CodeSnip_'s install program. Any v5 release from v5.4.0 (u) is required as is a compatible version of the ISPP pre-processor. v6 is not suitable.

You can get Inno Setup with ISPP at [http://www.jrsoftware.org/isinfo.php](https://www.jrsoftware.org/isinfo.php). Choose the Unicode version. If you already have the ANSI version the Unicode version can be installed alongside it - just use a different install directory and program group name.

The setup compiler is expected to be on the path unless its install directory is specified by the `INNOSETUP` environment variable.

### Microsoft HTML Help Compiler (HHC)

This command line compiler is supplied with Microsoft HTML Help Workshop. It is used to compile the _VIEd_ help file.

The program is expected to be on the path unless its install directory is specified by the `HHCROOT` environment variable.

### Zip

This program is used to create _VIEd_'s release file.

You can get a Windows command line version at [http://stahlforce.com/dev/index.php?tool=zipunzip](http://stahlforce.com/dev/index.php?tool=zipunzip).

The program is expected to be on the path unless its install directory is specified by the `ZIPROOT` environment variable.

## Preparation

### Get the Source Code

First you need to get the source code of _Version Information Editor_. This is maintained in the **[delphidabbler/vied](https://github.com/delphidabbler/vied)** Git repository on GitHub.

All releases back to v2.11.2 are available from the [GitHub releases page](https://github.com/delphidabbler/vied/releases). Choose the release you want from those listed and download an archive containing the required source code.

Alternatively you can fork and clone the repo using Git. The `master` branch contains the source code of the latest release while the `develop` branch contains any changes made since the last release.

### Configure the Environment

The makefile makes use of the following environment:

* `DELPHIROOT` (required)

  Must be set to the install directory of the version of Delphi being used.

* `DELPHIDABLIB_U` (required)

  Must be set to the directory where the required DelphiDabbler components' .dcu files are installed.

* `DELPHIDABLIB_R` (required)

  Must be set to the directory where the required DelphiDabbler components' .res & .dfm files are installed.

* `INNOSETUP`

  Set to the install directory of Inno Setup 5. If not set then Inno Setup must be on the system path.

* `VIEDROOT`

  Set to the path where the DelphiDabbler Version Information Editor (VIEd) is installed. If not set then VIEd must be on the system path.

* `HHCROOT`

  Set to the path where the Microsoft HTML Help Compiler is installed. If not set then the compiler must be on the system path.

* `ZIPROOT`

  Set to the path where Zip is installed. If not set then Zip must be on the system path.

* `RELEASEFILENAME`

  Set to the name of the zip file to be used to store a release. This file name should have no path or extension. If not set then `dd-vied` is used.

You may find it convenient to create a batch file that sets up the environment variables. Such a batch file should be run before running `MAKE`.

### Configure the Source Tree

After obtaining the source code you should have the following directory structure:

```
./
  |
  +-- Docs                   - documentation
  |
  +-- Source                 - main source code
      |
      +-- Assets             - assets to be included in resources
      |
      +-- Help               - source for help file: contains project files
          |
          +-- CSS            - style sheet used for help file
          |
          +-- HTML           - HTML help topic files
```

The first thing to do before attempting to hack the code is to configure the source tree. Open a console window and navigate to the `Source` directory. Run any script you have created to set the environment variables then do:

```
> make config
```

This command does two things:

1.  It creates the `Bin`, `Exe` and `Release` directories, along with various sub-directories of `Bin` that are required to receive the binary code. If any of these directories already existed they will be emptied.
2.  It creates the `.cfg` file from a template file. This file is required to configure the Delphi command line compiler.

The above directories and the `.cfg` file will be ignored by Git.

If you are intending to edit and compile the code using the Delphi IDE you must also run the following command from the `Source` directory:

```
> make resources
```

This creates the resource files that the IDE needs to link into compiled executables.

You are now ready to modify the code if you wish and to build the source.

## Building Version Information Editor

The code is built using the makefile, which must be run from a command line that has `Source` as its working directory.

The makefile offers several options. They are:

* `make config`

  As we have already seen this command configures the source tree. It should be run before using any other command. The command creates the required Delphi `.cfg` file from its template and creates empty `Bin`, `Exe` and `Release` directories.

* `make resources`

  Compiles the resource files required to build the program. The resulting `.res` files are placed in the `Bin` directory, which must exist.

* `make pascal`

  Compiles the pascal source code. Requires that resource files are present in the `Bin` directory. The compiled executable file is placed in the `Exe` directory and `.dcu` files are written to the `Bin` directory.

* `make vied`

  Compiles _VIEd_ from source. This command is equivalent to `make resources` followed by `make pascal`.

* `make help`

  Compiles the HTML help file from source. The compiled file is placed in the `Exe` directory.

* `make setup`

  Builds the setup program. The command requires that the _VIEd_ executable and the help file have been compiled and are present in the `Exe` directory. It also requires that certain documents are present in the `Docs` directory. The compiled setup program is placed in the `Exe` directory.

* `make release`

  Creates a zip file containing the necessary release files. By default the release file is named `dd-vied.zip` but this can be changed by setting the `RELEASEFILENAME` environment variable (see above). The `.zip` file is written to the `Release` directory.

* `make everything` or `make`

  This is the default option used when make is run with no targets. It configures the source tree, builds the _VIEd_ executable and the help file, then builds the install program and finally creates the release zip file.

* `make clean`

  Deletes unwanted and temporary files and directories from the source tree. The command does not delete the `Bin`, `Exe` and `Release` directories and contents and neither does it delete the `.cfg` file from the `Source` directory.

* `make deepclean`

  Like `make clean` this command deletes unwanted and temporary files and directories. It also removes the directories and files created by `make config`, i.e. the `Bin`, `Exe` and `Release` directories along with the `.cfg` file in the `Source` directory.

**Note:** If the `make` command fails to run you may need to use `%DELPHIROOT%\Bin\Make`.

## Copyright

If you are planning to re-use or modify any of the code, please see `SourceCodeLicenses.txt` in the `Docs` directory for an overview of the various open source licenses that apply to the _Version Information Editor_ source code.
