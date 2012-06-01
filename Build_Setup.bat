@ECHO OFF


REM ****************************************************************************


SET BuildName=MySQL-Front

SET BuildAWK=%ProgramFiles(x86)%\gawk\bin\gawk.exe
SET BuildBRCC=%ProgramFiles(x86)%\Embarcadero\RAD Studio\9.0\bin\brcc32.exe
SET BuildCompiler=%ProgramFiles(x86)%\Embarcadero\RAD Studio\9.0\bin\dcc32.exe
SET BuildEurekaLog=%ProgramFiles(x86)%\Embarcadero\RAD Studio\9.0\bin\ecc32.exe
SET BuildUPX=%ProgramFiles(x86)%\UPX\UPX.exe
SET BuildHelpMan=%ProgramFiles(x86)%\HelpandManual4\HelpMan.exe
SET BuildInnoSetup=%ProgramFiles(x86)%\Inno Setup 5\iscc.exe

SET BuildRootPath=%CD%
SET BuildImagesPath=%BuildRootPath%\Images
SET BuildLanguagesPath=%BuildRootPath%\Languages
SET BuildManualPath=%BuildRootPath%\Manual
SET BuildPublishPath=%BuildRootPath%\Publish
SET BuildSetupPath=%BuildRootPath%\Setup
SET BuildSkinsPath=%BuildRootPath%\Skins
SET BuildSourcePath=%BuildRootPath%\Source
SET BuildTempPath=%BuildRootPath%\Temp


REM ****************************************************************************


if not exist "%BuildTempPath%" MKDIR "%BuildTempPath%"

if exist "%BuildTempPath%\Build_Setup.exe" goto Build
CD "%BuildSetupPath%"
"%BuildCompiler%" /B Build_Setup.dpr
if Errorlevel 1 goto Error

:Build
"%BuildTempPath%\Build_Setup.exe"
if Errorlevel 1 goto End

"%BuildAWK%" -f "%BuildSetupPath%\Resource.awk" "%BuildTempPath%\Build_Setup.awk" > "%BuildTempPath%\Resource.awk"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Resource.awk" "%BuildSetupPath%\MySQLFront.rc" > "%BuildTempPath%\MySQLFront.rc"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildSetupPath%\Language.awk" "%BuildLanguagesPath%\English.ini" >> "%BuildTempPath%\MySQLFront.rc"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Resource.awk" "%BuildSetupPath%\Skin.awk" > "%BuildTempPath%\Skin.awk"
if Errorlevel 1 goto Error
"%BuildAWK%" -f "%BuildTempPath%\Skin.awk" "%BuildSkinsPath%\Gio_medium.ini" >> "%BuildTempPath%\MySQLFront.rc"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildSetupPath%\MySQLFront.manifest" > "%BuildTempPath%\MySQLFront.manifest"
if Errorlevel 1 goto Error

"%BuildBRCC%" /fo"%BuildSourcePath%\MySQLFront.res" -32 "%BuildTempPath%\MySQLFront.rc"
if Errorlevel 1 goto Error

CD %BuildSourcePath%
if exist "%BuildEurekaLog%" (
  "%BuildEurekaLog%" /B --el_config"MySQLFront.eof" "MySQLFront.dpr"
) else (
  "%BuildCompiler%" /B "MySQLFront.dpr"
)
if Errorlevel 1 goto Error
DEL "%BuildTempPath%\*.dcu"
DEL "%BuildSourcePath%\*.drc"

if exist "%BuildTempPath%\%BuildName%.exe" DEL "%BuildTempPath%\%BuildName%.exe"
"%BuildUPX%" "%BuildTempPath%\MySQLFront.exe" -o "%BuildTempPath%\%BuildName%.exe"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildManualPath%\MySQLFront.hmv" > "%BuildTempPath%\MySQLFront.hmv"
if Errorlevel 1 goto Error

"%BuildHelpMan%" "%BuildManualPath%\MySQLFront.hmx" /chm="%BuildTempPath%\%BuildName%.chm" /V="%BuildTempPath%\MySQLFront.hmv" /L="%BuildTempPath%\MySQLFront.log"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildSetupPath%\pad_file.xml" > "%BuildTempPath%\pad_file.xml"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildSourcePath%\MySQL-DataSet\Source\libMySQL.php" > "%BuildTempPath%\libMySQL.php"
if Errorlevel 1 goto Error

"%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildSetupPath%\MySQLFront.iss" > "%BuildTempPath%\MySQLFront.iss"
if Errorlevel 1 goto Error

"%BuildInnoSetup%" /O"%BuildTempPath%" "%BuildTempPath%\MySQLFront.iss" /F"%BuildName%_Setup"
if Errorlevel 1 goto Error

"%BuildTempPath%\Build_Setup.exe" /PAD_FILE
if Errorlevel 1 goto Error

"%BuildInnoSetup%" /O"%BuildTempPath%" /F"%BuildName%_Setup" "%BuildTempPath%\MySQLFront.iss"
if Errorlevel 1 goto Error


if not exist "%BuildPublishPath%" MKDIR "%BuildPublishPath%"

MOVE "%BuildTempPath%\%BuildName%_Setup.exe" "%BuildPublishPath%"

MOVE "%BuildTempPath%\pad_file.xml" "%BuildPublishPath%"

if exist "%BuildSetupPath%\Publish.bat" "%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildSetupPath%\Publish.bat" > "%BuildPublishPath%\Publish.bat"
if Errorlevel 1 goto Error

if exist "%BuildSetupPath%\Publish.ftp" "%BuildAWK%" -f "%BuildTempPath%\Build_Setup.awk" "%BuildSetupPath%\Publish.ftp" > "%BuildPublishPath%\Publish.ftp"
if Errorlevel 1 goto Error


DEL "%BuildTempPath%\MySQLFront.*"
DEL "%BuildTempPath%\%BuildName%.*"
DEL "%BuildTempPath%\*.awk"
DEL "%BuildTempPath%\libMySQL.php"
DEL "%BuildTempPath%\MySQLFront.log"


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
