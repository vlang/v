set exiterror=0
@echo off

echo Building V for Windows...

if exist "vc" (
rd /s /q vc
)

git version
git clone --depth 1 --quiet https://github.com/vlang/vc

gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o v2.exe vc/v_win.c
if %ERRORLEVEL% GEQ 1 (
   goto :compileerror
)

v2.exe -o v.exe compiler
del v2.exe
rd /s /q vc


:compileerror
echo Failed to compile - Create an issue at 'https://github.com/vlang'
goto :error


:error
echo fail
exit /b 1