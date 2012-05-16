@ECHO OFF


REM ****************************************************************************


SET BuildRootPath=%CD%\..
SET BuildPublishPath=%BuildRootPath%\Publish


REM ****************************************************************************


CD "%BuildRootPath%"

call git commit -a -m "Version {BuildVerStrFull}"
if Errorlevel 1 goto Error
call git push
if Errorlevel 1 goto Error

CD "%BuildPublishPath%"

if exist "%BuildPublishPath%\Publish.ftp" ftp.exe -s:"%BuildPublishPath%\Publish.ftp"
if Errorlevel 1 goto Error

goto End


REM ****************************************************************************


:Error
@ECHO off
ECHO _
ECHO ***************
ECHO *             *
ECHO *  ERROR !!!  *
ECHO *             *
ECHO ***************
ECHO _
PAUSE

:End
