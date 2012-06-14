@ECHO OFF


REM ****************************************************************************


SET BuildGit=%ProgramFiles(x86)%\Git\bin\git.exe
SET BuildFTP=ftp.exe
SET BuildMOVEit=%ProgramFiles(x86)%\MOVEit\FTPS.exe

SET BuildRootPath=%CD%\..
SET BuildPublishPath=%BuildRootPath%\Publish


REM ****************************************************************************


CD "%BuildRootPath%"

"%BuildGit%" commit -a -m "Version {BuildVerStrFull}"
if Errorlevel 1 goto Error
"%BuildGit%" push
if Errorlevel 1 goto Error

CD "%BuildPublishPath%"

if exist "%BuildMOVEit%" SET BuildFTP=%BuildMOVEit%

if exist "%BuildPublishPath%\Publish.ftp" "%BuildFTP%" -v -n -s:"%BuildPublishPath%\Publish.ftp"
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
