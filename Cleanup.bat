@ECHO OFF


REM ****************************************************************************


SET CleanRootPath=%CD%
SET CleanPublishPath=%CleanRootPath%\Publish
SET CleanSourcePath=%CleanRootPath%\Source
SET CleanTempPath=%CleanRootPath%\Temp


REM ****************************************************************************


CD "%CleanRootPath%"

if exist "%CleanPublishPath%" RMDIR /S /Q "%CleanPublishPath%"
if exist "%CleanTempPath%" RMDIR /S /Q "%CleanTempPath%"
mkdir "%CleanTempPath%"
copy "%CleanSourcePath%\FastMM\FastMM_FullDebugMode.dll" "%CleanTempPath%" > nul

for /D /R %%I in (*) do (
  if exist "%%I\__history" RMDIR /S /Q %%I\__history"
  if exist "%%I\*.dproj.local" DEL %%I\*.dproj.local"
  if exist "%%I\*.identcache" DEL "%%I\*.identcache"
  if exist "%%I\*.~dsk" DEL "%%I\*.~dsk"
)

CHOICE /M "Clean all?"
if Errorlevel 2 goto End
if Errorlevel 1 goto CleanAll
goto End

:CleanAll
for /D /R %%I in (*) do (
  if exist "%%I\Bin\*" DEL /Q "%%I\Bin\*"
)

:End
