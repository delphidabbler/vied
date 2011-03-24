@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing binary release of Version
@rem Information Editor.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2011
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off
setlocal

set OutFile=..\Release\dd-vied.zip

if exist %OutFile% del %OutFile%

zip -j -9 %OutFile% ..\Install\VIEdInst.exe ..\Docs\ReadMe.txt

endlocal
