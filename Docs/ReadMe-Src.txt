This file discusses how to build the Version Information Editor from source
code.

The source is provided in a zip file that contains the source code in the Source
folder and a required binary file is provided in the Bin folder.

To build the program you need the following directory structure within a root
folder:

Bin           - receives *.dcu and (provided) .res file
Exe           - receives .exe file [and is where help file is placed]
Source        - the source code
Source\Help   - source code for help file (.hpj, .map and .rtf files)
Source\Assets - assets used in the program's resources

In addition the the provided files, the following DelphiDabbler components are
required (not provided in this zip file):

+ Version information component
+ About box component
+ Drop files components
+ Window state components

They are all available from www.delphidabbler.com.

To compile:

+ Install the source code and create the folders described above.
+ Install the required DelphiDabbler components.
+ Load VIEd.dpr into Delphi 7 and build.

The .dcu files should be placed in the Bin folder and VIEd.exe in the Exe
folder. If this does not happen set the required paths in the Delphi project
options.

The Bin\Version.res file contains the program's version information. The source
for this is in the file is in Source\Version.vi. This is a source file that can
be edited by this program! Once you have built Version Information Editor you
can load Resource.vi into it, edit the file and save it. Export the file as an
.rc file and compile it with BRCC32 (or register BRCC32 with VIEd and compile
directly from the program).

The Bin\Resources.res file contains the program's other resources. The file can
be built from Source\Resources.rc using BRCC32, provided with Delphi.

The help file is defined in Source\Help\VIEd.rtf which can be edited with MS
Word and compiled with MS Help Workshop 4 (provided with Delphi). 

If there are any problems please contact the author at
http://www.delphidabbler.com/contact.

The source code is released under the Mozilla Public License. See the file
MPL.txt for details of this license.
