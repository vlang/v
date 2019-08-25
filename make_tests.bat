@echo off

echo Cleanup
del v.exe
del v_win.c
del v2.exe

echo fetch v_win.c
curl -O https://raw.githubusercontent.com/vlang/vc/master/v_win.c
if %ERRORLEVEL% NEQ 0 goto :fail

echo build vc using gcc
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o vc.exe v_win.c
del v_win.c
if %ERRORLEVEL% NEQ 0 goto :fail

echo build v using vc
vc.exe -o v.exe compiler
if %ERRORLEVEL% NEQ 0 goto :fail


setlocal EnableDelayedExpansion
echo testing v
v test v
if %ERRORLEVEL% NEQ 0 goto :fail

echo skipping build vc.msvc using vc
REM vc.exe -os msvc -o v.msvc.exe compiler
REM if %ERRORLEVEL% NEQ 0 goto :fail

echo skipping testing v.msvc -os msvc
REM v.msvc.exe -os msvc test v
REM if %ERRORLEVEL% NEQ 0 goto :fail

goto :done

:fail
echo fail
exit /b 1

:done
echo pass
