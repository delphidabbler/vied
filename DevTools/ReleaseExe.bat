@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing binary release of Version
@rem Information Editor.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 30 Apr 2008 - Original version.
@rem ---------------------------------------------------------------------------

@echo off
setlocal

set OutFile=..\Release\dd-vied.zip

if exist %OutFile% del %OutFile%

zip -j -9 %OutFile% ..\Install\VIEdInst.exe ..\Docs\ReadMe.txt

endlocal
