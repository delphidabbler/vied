@rem ---------------------------------------------------------------------------
@rem Script used to delete Version Information Editor's temp and backup source
@rem files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2011
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off
setlocal

echo Tidying
echo ~~~~~~~
echo.

set SrcDir=..\Source

echo Deleting *.~* from "%SrcDir%" and subfolders
del /S %SrcDir%\*.~* 
echo.

echo Deleting *.dpp from "%SrcDir%" and subfolders
del /S %SrcDir%\*.ddp 
echo.

echo Done.

endlocal
