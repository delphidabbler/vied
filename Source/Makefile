# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at http://mozilla.org/MPL/2.0/
#
# Copyright (C) 2011-2014, Peter Johnson (www.delphidabbler.com).
#
# Project makefile.


# Required macros
# ---------------
#
# Make will fail if any of the following macros is not defined.
#
# BIN
#   Must be defined to address the directory that is to receive .res and .dcu
#   output files. This macro is defined in this makefile.
#
# DELPHIROOT (or DELPHI2010)
#   DELPHIROOT must address the installation directory of the required Delphi
#   compiler (Delphi 2010 as a minimum). If Delphi 2010 is to be used the
#   DELPHI2010 macro can be defined to address the Delphi 2010 installation
#   directory. When DELPHI2010 is defined any existing value of DELPHIROOT is
#   ignored and the value of DELPHI2010 is used instead.
#
# DELPHIDABLIB (or DELPHIDABLIBD2010)
#    DELPHIDABLIB address the directory where the DelphiDabbler library
#    components are installed for the required Delphi compiler. If Delphi 2010
#    is to be used the DELPHIDABLIBD2010 macro can be defined to address the
#    directory where the required components are installed for Delphi 2010. When
#    DELPHIDABLIBD2010 is defined and existing value of DELPHIDABLIB is ignored
#    and the value of DELPHIDABLIBD2010 is used instead.
#
# Optional macros
# ---------------
#
# These macros may be defined, but make will probably be successful without them
# providing certain programs are available on the path.
#
# VIEDROOT
#   Path where the DelphiDabbler Version Information Editor (VIEd) is installed.
#   If not set VIEd must be on the system path.
#
# INNOSETUP
#   Path where Inno Setup is installed. If not set Inno Setup must be on the
#   system path.
#
# HHCROOT
#   Path where the Microsoft HTML Help Compiler is installed. If not set the
#   compiler must be on the system path.
#
# ZIPROOT
#   Path where Zip.exe is installed. If not set Zip.exe must be on the system
#   path.
#
# RELEASEFILENAME
#   Name of zip file to be used to store a release. File name should have no
#   path or extension. If not set then "dd-vied" is used.
#
# Supported Commands
# ------------------
#
# make config
#   Configures source tree ready for building program. Creates Delphi .cfg file
#   from template and creates empty Bin, Exe and Release directories.
#
# make resources
#   Compiles the resource files required to build the program. Files are placed
#   in the Bin directory, which must exist.
#
# make pascal
#   Compiles the pascal source using the required version of Delphi. Requires
#   that required resource files are present in Bin directory. Exe file is
#   placed in Exe directory.
#
# make vied
#   Compiles VIEd from source. Equivalent to "make resources" followed by "make
#   pascal".
#
# make help
#   Compiles help file from source. Compiled file is placed in Exe directory.
#
# make setup
#   Builds the setup program. Requires that the VIEd executable and the help
#   file have been compiled. Setup program is placed in Exe directory.
#
# make release
#   Creates a zip file containing the required release files.
#
# make everything
#   This is the default option used when make is run with no targets. It
#   configures the source tree, builds the VIEd executable and the help file
#   then creates the install program and finally creates the release zip file.
#
# make clean
#   Deletes unwanted and temporary files and directories from source tree. Does
#   not delete the Bin, Exe and Release directories and contents.
#
# make deepclean
#   Deletes unwanted and temporary files and directories as "make clean" and
#   then removes the directories and files created by "make config".


# Set bin path
BIN = ..\Bin

# Check for DELPHI2010 macro and use it for DELPHIROOT if set.

!ifdef DELPHI2010
DELPHIROOT = $(DELPHI2010)
!endif

!ifdef DELPHIDABLIBD2010
DELPHIDABLIB = $(DELPHIDABLIBD2010)
!endif

# Check for required macros

!ifndef DELPHIROOT
!error DELPHIROOT environment variable required.
!endif
!ifndef DELPHIDABLIB
!error DELPHIDABLIB environment variable required.
!endif
!ifndef BIN
!error BIN macro must be defined in calling script.
!endif

# Define common macros that access required build tools

MAKE = "$(MAKEDIR)\Make.exe" -$(MAKEFLAGS)

DCC32 = "$(DELPHIROOT)\Bin\DCC32.exe"

BRCC32 = "$(DELPHIROOT)\Bin\BRCC32.exe"

!ifdef VIEDROOT
VIED = "$(VIEDROOT)\VIEd.exe" -makerc
!else
VIED = VIEd.exe -makerc
!endif

!ifdef INNOSETUP
ISCC = "$(INNOSETUP)\ISCC.exe"
!else
ISCC = ISCC.exe
!endif

!ifdef HHCROOT
HHC = "$(HHCROOT)\HHC.exe"
!else
HHC = HHC.exe
!endif

!ifdef ZIPROOT
ZIP = "$(ZIPROOT)\Zip.exe"
!else
ZIP = Zip.exe
!endif

!ifndef RELEASEFILENAME
RELEASEFILENAME = dd-vied
!endif

# Implicit rules

# Delphi projects are assumed to contain required output and search path
# locations in the project options .cfg file.
.dpr.exe:
  @echo +++ Compiling Delphi Project $< +++
  @$(DCC32) $< -B -U"$(DELPHIDABLIB)"

# Resource files are compiled to the directory specified by BIN macro.
.rc.res:
  @echo +++ Compiling Resource file $< +++
  @$(BRCC32) $< -fo$(BIN)\$(@F)

# Version info files are compiled by VIEd. A temporary .rc file is left behind
.vi.rc:
  @echo +++ Compiling Version Info file $< +++
  @$(VIED) .\$<

# Targets

# Default is to build everything and release
everything: config vied help setup release

# Build VIEd executable
vied: resources pascal

# Configure source folders
config:
  @echo Configuring VIEd source tree
  # Create .cfg file from template
  @copy /Y VIEd.cfg.tplt VIEd.cfg
  # Create empty build folders
  @cd ..
  @if exist Bin rmdir /S /Q Bin
  @if exist Exe rmdir /S /Q Exe
  @if exist Release rmdir /S /Q Release
  @mkdir Bin
  @mkdir Exe
  @mkdir Release
  @cd Source

# Builds VIEd pascal files and links program
pascal: VIEd.exe

# Builds help file
help:
  -1 @$(HHC) .\Help\VIEd.chm

# Compiles the resources and deletes intermediate file created by VIED
resources: Resources.res Version.res
  -@del Version.rc

# Builds setup program
setup:
  @del ..\Exe\VIEd-Setup-*
  @$(ISCC) Install.iss

# Create release files (.zip)
OUTFILE = Release\$(RELEASEFILENAME).zip
release:
  @echo +++ Creating Release File $(OUTFILE) +++
  @cd ..
  -@if exist $(OUTFILE) del $(OUTFILE)
  @$(ZIP) -j -9 $(OUTFILE) Exe\VIEd-Setup-*.exe Docs\ReadMe.txt
  @cd Source

# Clean up unwanted files
clean:
  @cd ..
  # remove unwanted files: temps (~ or .~*), .dsk, .local, .identcache
  -@del /S *.~* 2>nul
  -@del /S ~* 2>nul
  -@del /S *.dsk 2>nul
  -@del /S *.local 2>nul
  -@del /S *.identcache 2>nul
  -@del /S *.orig 2>nul
  # remove __history folders
  -@for /F "usebackq" %i in (`dir /S /B /A:D ..\__history`) do @rmdir /S /Q %i
  @cd Source

deepclean: clean
  @cd ..
  # remove directories created by config
  @if exist Bin rmdir /S /Q Bin
  @if exist Exe rmdir /S /Q Exe
  @if exist Release rmdir /S /Q Release
  # remove .cfg file created by config
  @cd Source
  @if exist VIEd.cfg del VIEd.cfg
