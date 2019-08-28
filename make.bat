set exiterror=0
@echo off

echo Building V for Windows...

if exist "vc" (
	rd /s /q vc
)

git version
git clone --depth 1 --quiet https://github.com/vlang/vc

echo Building v.c...
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o v2.exe vc/v_win.c 2>&1
if %ERRORLEVEL% GEQ 1 (
	echo gcc failed to compile - Create an issue at 'https://github.com/vlang'
   exit /b 1
)

echo Building v.v...
v2.exe -o v.exe compiler
if %ERRORLEVEL% GEQ 1 (
	echo v.exe failed to compile itself - Create an issue at 'https://github.com/vlang'
   exit /b 1
)

echo Cleaning up...
rem del v2.exe
rd /s /q vc

if exist "v.exe" (
	echo V has been successfully built
) else (
	echo v.exe was not generated - Create an issue at 'https://github.com/vlang'
	exit /b 1
)