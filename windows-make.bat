@ECHO OFF
@echo 1st:%1 2nd:%2 3rd:%3 4th:%4

SET PWD=%cd%
SET APPNAME=%3
for %%* in (.) do set PARENTDIR=%%~n*

IF [%1] EQ [] CALL :all
IF /I "%1"=="all" CALL :all
IF /I "%1"=="boss" CALL :boss
IF /I "%1"=="clean" CALL :clean
IF /I "%1"=="get-deps" CALL :get-deps
IF /I "%1"=="deps" CALL :deps
IF /I "%1"=="test" CALL :test
IF /I "%1"=="app" IF /I "%2"=="project" IF [%3] NEQ [] CALL :app

:: End of main program
GOTO End


:all
CALL rebar.bat get-deps
CALL rebar.bat compile
GOTO :EOF

:boss
CALL rebar.bat compile skip_deps=true
GOTO :EOF

:clean
CALL rebar.bat clean
GOTO :EOF

:get-deps
CALL rebar.bat get-deps
GOTO :EOF

:deps
CALL rebar.bat compile
GOTO :EOF

:test
CALL rebar.bat skip_deps=true eunit
GOTO :EOF

:: example how to invoke: windows-make.bat app PROJECT=awesomename
:app
CALL rebar.bat create template=skel dest=../%APPNAME% src=../%PARENTDIR% appid=%APPNAME% skip_deps=true
GOTO :EOF

:End
:: End of batch file
