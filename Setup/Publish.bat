@ECHO OFF


REM ****************************************************************************


SET PublishGit=%ProgramFiles(x86)%\Git\bin\git.exe
SET PublishFTP=ftp.exe
SET PublishMOVEit=%ProgramFiles(x86)%\MOVEit\FTPS.exe -quiterror

SET BuildRootPath=%CD%\..
SET BuildPublishPath=%BuildRootPath%\Publish


REM ****************************************************************************


CD "%BuildRootPath%"

"%PublishGit%" commit -a -m "Version {BuildVerStrFull}"
if Errorlevel 1 goto Error
"%PublishGit%" push
if Errorlevel 1 goto Error


CD "%BuildPublishPath%"

if exist "%PublishMOVEit%" SET PublishFTP=%PublishMOVEit%

if exist "%BuildPublishPath%\Publish.ftp" "%PublishFTP%" -n -s:"%BuildPublishPath%\Publish.ftp"
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
