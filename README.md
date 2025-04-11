# Version Information Editor (VIEd)

## Overview

Version Information Editor enables version information to be edited. It also creates version information resource source (`.rc`) files. The program stores details of the version information in its own project (`.vi`) files. Simple macros can be used to help automate the updating of version information files.

The program can also create binary resource (`.res`) files containing version information. To do this it needs to be configured to use a 3rd party resource compiler such as Embarcadero's BRCC32.

The program supports some command line options are supported. For details see the help file's [_Command Line Options_](https://htmlpreview.github.io/?https://github.com/delphidabbler/vied/blob/master/Source/Help/HTML/cmdline.htm) topic.  

## System Requirements

VIEd requires Windows XP and later.

## Source Code

VIEd is written in Object Pascal and is compiled with Delphi XE, although any later Delphi compiler may suffice.

See `Build.md` in the repo root for information about how to build the program from source code.

## Contributing

Contributions are welcome.

The [GitFlow](http://nvie.com/posts/a-successful-git-branching-model/) branching model is used.

To contribute please fork the project and create a new branch off `develop` that is named `feature/your-feature-name`. Create a pull request for your feature branch when you're ready. Pull requests must be configured to merge to the `develop` branch, not `master`.

Contributions should be licensed under either the Mozilla Public License v2.0 (without a "Incompatible with Secondary Licenses" notice) or under some other compatible license, preferably one in common use. Note that the GPL and its cousins are not compatible.

## Executable Code

VIEd is distributed in a zip file containing a standard Windows installer. All releases from the latest back to v2.13.0 are available from the project's [releases page](https://github.com/delphidabbler/vied/releases).

## Documentation

For help using the program the first port of call should be the program's comprehensive HTML help file.

Information on how to install the program is in `Docs/ReadMe.txt`.

The change log is in `CHANGELOG.md` in the repository root.

The program's .vi file format is documented in `Docs/vi-file-format.md`.

Finally, the program has a Web page at <https://delphidabbler.com/software/vied>.

## Bugs and Feature Requests

Any bug reports and feature requests can be made via the project's [issue tracker](https://github.com/delphidabbler/vied/issues).

## License

Most of the original source code is licensed under the terms of the Mozilla Public License 2.0. For full details of source code licensing, including that for 3rd party code and the program's images, see `Docs/SourceCodeLicenses.md`.

The license for the executable code can be found in `Docs/License.rtf`.

> Note that versions prior to v2.13.0 had different licensing arrangements, so please check before using those versions.
