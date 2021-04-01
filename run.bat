@ECHO OFF

REM === BEGIN OF CONFIGURATION SECTION

REM Set path to your R interpreter
SET RPATH="C:\Program Files\R\R-4.0.3\bin\x64\"

REM Set script name
SET SCRIPT=plot_model2.R

REM === END OF CONFIGURATION SECTION

REM Startup options
SET ROPTIONS=--vanilla --quiet

REM CD to BATCH file path
CD /D %~dp0

ECHO Current directory: %CD%

IF NOT EXIST %SCRIPT% GOTO :ERROR_SCRIPT_NOT_FOUND

%RPATH%\Rscript %ROPTIONS% -e "source('%SCRIPT%')"

GOTO :EOF

:ERROR_SCRIPT_NOT_FOUND
ECHO **** ERROR: Cannot find script: %SCRIPT%
PAUSE
