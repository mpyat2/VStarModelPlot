@ECHO OFF

REM === BEGIN OF CONFIGURATION SECTION

REM Set path to your R interpreter
SET RPATH="C:\Program Files\R\R-3.5.1\bin\x64\"

REM === END OF CONFIGURATION SECTION

REM Startup options
SET ROPTIONS=--no-save --no-environ --no-site-file --no-restore --quiet

REM CD to BATCH file path
CD /D %~dp0

ECHO Current directory: %CD%

IF NOT EXIST plot_model2.R GOTO :ERROR2

IF NOT EXIST .Rprofile ECHO .First = function() { source("plot_model2.R") } > .Rprofile
IF NOT EXIST .Rprofile GOTO :ERROR1
%RPATH%\R %ROPTIONS%

GOTO :EOF

:ERROR1
ECHO **** ERROR: Cannot create .Rprofile
PAUSE

:ERROR2
ECHO **** ERROR: Cannot find script: plot_model2.R
PAUSE
