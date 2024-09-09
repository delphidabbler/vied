# Version Information Editor .vi File Format

## Contents

* [Introduction](#introduction)

* [File Encoding](#file-encoding)

* [Comments & Blank Lines](#comments--blank-lines)

* [File Data Structure](#file-data-structure)

    * [`Macros` Section](#macros)
    * [`Fixed File Info` Section](#fixed-file-info)
    * [`Variable File Info` Section](#variable-file-info)
    * [`String File Info` Section](#string-file-info)
    * [`Configuration Details` Section](#configuration-details)

* [Footnotes](#footnotes)

## Introduction

**_This document applies to .vi file format version 3, introduced with VIEd v2.16.0._**

The .vi file format is the native file format used by Version Information Editor (VIEd) to record version information along with all built in fields and user defined macros that apply to the version information.

It is best to allow VIEd to write .vi files, although they can be hand written providing all the information in this document is applied carefully.

## File Encoding

.vi files can be written in either UTF-8 format (with preamble) or in the default ANSI encoding of the computer on which the file is saved.

> UTF-8 is recommended.

## Comments & Blank Lines

Comments are permitted in .vi files. They must occupy a line on their own and must be introduced by a `;` (semicolon) as the 1st non-whitespace character on the line.

Blank lines can appear anywhere in a .vi file.

> Note that VIEd remembers any comments it finds at the start of the file and will write those comments again when it save the file. However all other comments will be lost. VIEd outputs a blank line before each section.

## File Data Structure

The .vi format is similar to the Windows .ini file format in that the file is in plain text and comprises various key/value pairs divided into named sections.

The following rules apply:

* Section names are enclosed between `[` and `]` characters.
* Key/value pairs are stored in the format `key=value`. Keys with no values are written `key=`.
* Each section name and key/value pair must be appear on a line on its own.
* Section and key names are case sensitive.
* Sections may appear in any order

There are five pre-defined sections:

* `[Macros]`
* `[Fixed File Info]`
* `[Variable File Info]`
* `[String File Info]`
* `[Configuration Details]`

Each section is explained below.

All sections, except `[Macros]` have values that are text representations of one of the following types:

| Data Type | Description |
|-----------|-------------|
| _string_  | A string of characters, including spaces. All characters in the chosen encoding are accepted. Strings are not quoted, so any quotes form part of the string. |
| _digits_  | A string of digits representing a non-negative base 10 integer. |
| _version_ | A sequence of characters in the form `a.b.c.d`* where `a`, `b`, `c` and `d` are _digits_ that represent parts of a version number, with `a` being the most significant and `d` being the least significant. Partial version numbers such a `a` or `a.b.c` are acceptable. In this case the least significant version number is set to zero. Each part has a maximum value of `65535`. |
| _identifier_ | A sequence of characters beginning with a letter or underscore, followed by zero or more letters, digits or underscores. Letters must be in the range `A`..`Z` or `a`..`z`. |
| _empty_ | A empty value. |

A key/value pair is considered to have an _empty_ value if either the key is missing or it has the form `key=`. In such cases, for all sections except `[Macros]`, a default value may be applied. Default values are noted in the section descriptions. A default value can be _empty_.

In all sections except `[Macros]` keys are predefined.

### `[Macros]`

This section defines the macros that can be used within the .vi file.

Each macro is defined on its own line as follows:

```
<Type>:<Name>=<Parameter>
```

where:

* `<Type>` is the type of macro. Valid types are `Define`, `Env`, `External` and `Import`.
* `<Name>` is the name of the macro.
* `<Value>` is a parameter associated with the macro: the meaning depends on the macro type. Some types permit the value to be _empty_.

Entries for each macro **MUST** be provided, regardless of whether the value is _empty_.

This section _may_ be omitted if no macros are defined.

> VIEd _will not_ write this section if there are no macros.

### `[Fixed File Info]`

This section defines the values of the fixed file information component of the version information.

Valid keys and their values are described in the following table:

| Key | Value Type | Description | Default Value | Accepts Fields? | Accepts Macros? | Notes |
|-----|------------|-------------|---------------|-----------------|-----------------|-------|
| `File Version #` | _version_ | The executable file's version number. | `0.0.0.0` | No | Yes | Macros can be used to specify all or part of the version number, but should evaluate to a valid version number. |
| `Product Version #` | _version_ | The version number of the product with which the executable file is distributed. | `0.0.0.0` | No | Yes | Macros can be used to specify all or part of the version number, but should evaluate to a valid version number. |
| `File OS` | _digits_ | The operating system at which the program is targetted. | `VOS__WINDOWS32` (`4`) | No | No | See the description of the `dwFileOS` field in the Microsoft documentation for the [`VS_FIXEDFILEINFO` structure ](https://learn.microsoft.com/en-us/windows/win32/api/verrsrc/ns-verrsrc-vs_fixedfileinfo#members) for details of valid values. |
| `File Type` | _digits_ | The general type of the executable file. | `VFT_APP` (`1`) | No | No | See the description of the `dwFileType` field in the Microsoft documentation for the [`VS_FIXEDFILEINFO` structure ](https://learn.microsoft.com/en-us/windows/win32/api/verrsrc/ns-verrsrc-vs_fixedfileinfo#members) for details of valid values. |
| `File Sub-Type` | _digits_ | The sub-type of the executable file. | `0` | No | No |  Valid values depend on the general file type. See the description of the `dwFileSubtype` field in the MS documentation for the [`VS_FIXEDFILEINFO` structure ](https://learn.microsoft.com/en-us/windows/win32/api/verrsrc/ns-verrsrc-vs_fixedfileinfo#members) for details of valid values. |
| `File Flags Mask` | _digits_ | A bitmask that specifies the valid bits in the `File Flags` bitmask. | `0` | No | No | See the description of the `dwFileFlagsMask` field in the Microsoft documentation for the [`VS_FIXEDFILEINFO` structure ](https://learn.microsoft.com/en-us/windows/win32/api/verrsrc/ns-verrsrc-vs_fixedfileinfo#members) for details of valid values. |
| `File Flags` | _digits_ | A bitmask that describes attributes of the executable file. | `0` | No | No | See the description of the `dwFileFlags` field in the Microsoft documentation for the [`VS_FIXEDFILEINFO` structure ](https://learn.microsoft.com/en-us/windows/win32/api/verrsrc/ns-verrsrc-vs_fixedfileinfo#members) for details of valid values. |

This section _may_ be omitted if all the keys have their default values. 

> VIEd will _always_ write this section.

### `[Variable File Info]`

This section defines the language and character set that the executable file uses. The string table is assumed to use the same language and character set. Only one character set and language can be specified.

Valid keys and their values are described in the following table:

| Key | Value Type | Description | Default Value | Accepts Fields? | Accepts Macros? | Notes |
|-----|------------|-------------|---------------|-----------------|-----------------|-------|
| `Character Set` | _digits_ | Character set identifier. | `1200` (Unicode)| No | No | See the description of the _charsetID_ parameter in the Microsoft documentation of the [`VarFileInfo` BLOCK statement](https://learn.microsoft.com/en-us/windows/win32/menurc/varfileinfo-block) for a list of valid character set identifiers. |
| `Language` | _digits_ | Language identifier. | `2057` (UK English) | No | No | See the description of the _langID_ parameter in the Microsoft documentation of the [`VarFileInfo` BLOCK statement](https://learn.microsoft.com/en-us/windows/win32/menurc/varfileinfo-block) for a list of valid language identifiers. |

This section _may_ be omitted if all the keys have their default values. 

> VIEd will _always_ write this section.

### `[String File Info]`

This section specifies the values of string information items. The language and character set used should correspond to that specified in the `[Variable File Info]` section.

Valid keys and their values are described in the following table:

| Key | Value Type | Description | Default Value |Accepts Fields? | Accepts Macros? | Notes |
|-----|------------|-------------|---------------|----------------|-----------------|-------|
| `Comments` | _string_ | Information for diagnostic purposes. | _empty_ | Yes, except for `<COMMENTS>`† | Yes |  |
| `Company Name` | _string_ | Company that produced the executable file or product. | _empty_ | Yes, except for `<COMPANYNAME>`† | Yes | Required ‡. |
| `File Description` | _string_ | A description of the program to be presented to users. | _empty_ | Yes, except for `<FILEDESCRIPTION>`† | Yes | Required ‡. |
| `File Version` | _string_ | Version number of the executable file. | _empty_ | Yes, except for `<FILEVERSION>`† | Yes | Required ‡. Does not _have_ to be related to the version number specified in the `File Version #` value from the `[Fixed File Info]` section, but this is advised.  |
| `Internal Name` | _string_ | Internal name of the executable file. | _empty_ | Yes, except for `<INTERNALNAME>`† | Yes | Required ‡. This should be, for e.g. a module name if the file is a DLL. If the file has no internal name then this should be the same as `Original File Name`, but without the extension. |
| `Legal Copyright` | _string_ | Copyright notices that apply to the executable file or product. | _empty_ | Yes, except for `<LEGALCOPYRIGHT>`† | Yes | |
| `Legal Trademark` | _string_ | Trademarks and registered trademarks that apply to the file. | _empty_ | Yes, except for `<LEGALTRADEMARK>`† | Yes | |
| `Original File Name` | _string_ | Original name of the executable file, not including a path. | _empty_ | Yes, except for `<ORIGINALFILENAME>` & `<SHORTFNAME>`† | Yes | Required ‡. |
| `Private Build` | _string_ | Information about a private build of the executable file differs from the standard version. | _empty_ | Yes, except for `<PRIVATEBUILD>`† | Yes | This value should be present only if the value of the `VS_FF_PRIVATEBUILD` constant (`8`) is included in the `File Flags` bitmask in the `[Fixed File Info]` section. |
| `Product Name` | _string_ | Name of the product with which the executable file is distributed. | _empty_ | Yes, except for `<PRODUCTNAME>`† | Yes | Required ‡. |
| `Product Version` | _string_ | Version of the product with which the executable file is distributed. | _empty_ | Yes, except for `<PRODUCTVERSION>`† | Yes | Required ‡. Does not _have_ to be related to the version number specified in the `Product Version #` value from the `[Fixed File Info]` section, but this is advised.  |
| `Special Build` | _string_ | Information about a special build of the executable file differs from the standard version. | empty | Yes, except for `<SPECIALBUILD>`† | Yes | This value should be present only if the value of the `VS_FF_SPECIALBUILD` constant (`32`) is included in the `File Flags` bitmask in the `[Fixed File Info]` section. |

For further details of the purpose and example use of the keys in this section see the _string-name_ parameter description in Microsoft's [StringFileInfo BLOCK](https://learn.microsoft.com/en-us/windows/win32/menurc/stringfileinfo-block) documentation.

This section _may_ be omitted if all the keys have _empty_ values. 

> VIEd will _always_ write this section.

### `[Configuration Details]`

This section contains details of various configuration options.

Valid keys and their values are described in the following table:

| Key | Value Type | Description | Default Value | Accepts Fields? | Accepts Macros? | Notes |
|-----|------------|-------------|---------------|-----------------|-----------------|-------|
| `FileVersion` | _digits_ | .vi file version. | `0` | No | No | This value should _always_ be set to `3`. |
| `Identifier` | _identifier_ | Version information resource identifier. | `VERINFO` | No | No |  |
| `ResOutputDir` | _string_ | Path to default output directory used when compiling binary .res files. | _empty_ | No § | No § | Must be a valid file path. |
| `NumRCComments` | _digits_ | The number of comments to be written to the generated .rc source file. | `0` | No | No | Each comment must begin with a pipe character. Spaces after the pipe determine the level of indenting of the comment line. The value must be equal to the total number of `RC Comment Line X` keys (see below). |
| `RC Comment Line <X>` | _string_ | The `X`th line of comments to be written to the generated .rc source file. | _empty_ | No | No | There are zero or more such keys, where `<X>` is an index number in the range `0` to `NumRCComments - 1`. Keys with indices that are out of this range are ignored. When `NumRCComments` = `0` these keys should be omitted. |

This section _may_ be omitted if all the keys have their default values. But, since `FileVersion` should always be set to a non-default value it is rare for all keys to have default values.

> VIEd will _always_ write this section.

## Footnotes

|   |   |
|---|---|
| * | In older file formats version numbers had to be written as `a, b, c, d` instead of `a.b.c.d`. This form can still be used for backwards compatibility. |
| † | Fields excluded from specific string information item values are excluded because those fields reference the related value (directly or indirectly) and so cause infitine loops when evaluated. |
| ‡ | According to Microsoft, string information items marked as "required" must be present in all version information resources. VIEd does not enforce this rule. |
| § | Field and macro names _can_, but _shouldn't_ be entered into the `ResOutputDir` value: they will not be evaluated and their references will be considered part of the file path. |
