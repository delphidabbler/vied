# Source Code Licensing

This document discusses the relevant licenses governing the Version Information Editor source code.

The code available from the project's [Git repository](https://github.com/delphidabbler/vied) comprises:

* [Original source code](#original-source-code)
* [Image files](#image-files)

The download does not include **all** the source code required to build Version Information Editor. Explicitly it doesn't include:

* Source files from the Delphi VCL. You must have Delphi in order to access these files.

* The required DelphiDabbler [DDabLib library](https://github.com/ddablib) components and units. The latest versions are all covered by the Mozilla Public License v2.0.

## Original Source Code

Original source files are stored the `Source` directory and its sub-directories.

Any original source code file that is governed by a license has a comment to that effect in the source. The exception is that `.dfm` files are considered to be governed by the same license as the associated `.pas` file.

Where files are auto-generated (such as the `.cfg` file), and there are no copyright or license comments in the files, the licensing of the file depends on how it was generated. If the file was generated from another source file then the generated file is covered by the same license as the originating source file, if any. Otherwise the file is placed in the public domain.

Most, if not all, files are licensed under the Mozilla Public License v2.0. A full copy of that license can be found in `MPL.txt`.

## Image Files

The images used in the program are stored in the `Source` directory and its sub-directories.

Images may also be embedded in forms by various VCL controls. Separate image files are not provided in this case.

All images are either original or have been copied or adapted from various sources. With the exception of the images listed under [Exceptions](#exceptions), all images are believed to be in the public domain and can be re-used without restriction.

### Exceptions

The file `Source\Assets\VIEd.ico` is copyright (C) 2009 Peter Johnson (<https://delphidabbler.com>).

This file may not be altered in any way and may not be used in any other programs, including programs derived from the Version Information Editor code base, unless explicit permission is given by the copyright holder.

This means that if you modify an aspect of the Version Information Editor code base and publish your changes then you must remove the icon or replace it with your own. (You must also remove any "DelphiDabbler" branding).
