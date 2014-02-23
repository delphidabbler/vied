Version  Information Editor (vied)
==================================


Description
-----------

Version  Information Editor enables version information to be edited. It also creates version information resource source (.rc) files. The program stores details of the version information in its own project (.vi) files. Simple macros can be used to help automate the updating of version information files.

The program can also create binary resource (.res) files containing version information. To do this it needs to use a 3rd party resource compiler such as Borland's BRCC32.

From version 2.11 the program supports a command line switch - `-makerc` - that silently creates a .rc file from a native .vi file passed on the command line. The program simply creates the .rc file and exits, without displaying a window. This option has been made available to enable Version Information Editor to be used in automated build processes.


### System Requirements

VIEd requires Windows XP and later.


Source Code
-----------

VIEd is written in Object Pascal and targeted at Delphi 2010, although any later non-Unicode compiler should suffice.

See `Build.html` in the repo root for information about how to build the program from code.

Each release from v2.11.2 is has a tag in the form vX.X.X where X.X.X is the release version number.


Contributing
------------

Contributions are welcome.

I use the [GitFlow](http://nvie.com/posts/a-successful-git-branching-model/) branching model. All development is done either directly on the `develop` branch, or on feature branches off `develop`.

To contribute please fork the project and create a new branch off `develop` that is named `feature/your-feature-name`. Create a pull request when you're ready.

You can make suggestions for features using the GitHub [issue tracker](https://github.com/delphidabbler/vied/issues) for this project.

Contributions should be licensed under either the Mozilla Public License v2.0 (without a "Incompatible with Secondary Licenses" notice) or under some other compatible license, preferably one in common use. Note that the GPL and its cousins are not acceptable.

Executable Code
---------------

You can get a zip archive containing VIEd's installer and read-me file from http://delphidabbler.com/download?id=vied&type=exe. At present only the latest version is available.


Documentation
-------------

For help using the program the first port of call should be the program's comprehensive HTML help file.

Information on how to install the program is in `Docs/ReadMe.txt`.

The change log is in `Docs/ChangeLog.txt`.

Finally, the program has its home at http://delphidabbler.com/software/vied.


Bugs and Feature Requests
-------------------------

An bug reports and feature requests via the project's issue tracker please on GitHub.


License
-------

The executable program an the original source code are licensed under the terms of the Mozilla Public License 2.0.

For full details of licensing, including that for 3rd party code and the program's images, see `Docs/SourceCodeLicenses.txt` for details.

Note that versions prior to v2.13.0 had different licensing arrangements, so please check before using those versions.
