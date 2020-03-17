@echo off

echo Building V

if exist "vc" (
	rd /s /q vc
)

git version

echo Downloading v.c...
git clone --depth 1 --quiet https://github.com/vlang/vc

REM option to force msvc or gcc
if "%~1"=="-gcc" goto :gcc_strap
if "%~1"=="-msvc" goto :msvc_strap

:gcc_strap
echo Attempting to build v.c with GCC...

for /f "usebackq tokens=*" %%i in (`where gcc`) do (
	set gccpath=%%i
)

if not exist "%gccpath%" (
	goto :msvc_strap
)

gcc -std=c99 -municode -w -o v.exe vc\v_win.c
if %ERRORLEVEL% NEQ 0 (
	echo gcc failed to compile - Create an issue at 'https://github.com/vlang'
	rd /s /q vc
	goto :error
)

echo Now using V to build V...
v self -prod
if %ERRORLEVEL% NEQ 0 (
	echo v.exe failed to compile itself - Create an issue at 'https://github.com/vlang'
	rd /s /q vc
	goto :error
)

rd /s /q vc
goto :success

:msvc_strap
echo Attempting to build v.c  with MSVC...
set VsWhereDir=%ProgramFiles(x86)%
set HostArch=x64
if "%PROCESSOR_ARCHITECTURE%" == "x86" (
	echo Using x86 Build Tools...
	set VsWhereDir=%ProgramFiles%
	set HostArch=x86
)

if exist "%VsWhereDir%"\"Microsoft Visual Studio 14.0"\Common7\Tools\vsdevcmd.bat (
	echo Microsoft Visual Studio 14.0
	call "%VsWhereDir%"\"Microsoft Visual Studio 14.0"\Common7\Tools\vsdevcmd.bat -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%"\"Microsoft Visual Studio 15.0"\Common7\Tools\vsdevcmd.bat (
	echo Microsoft Visual Studio 15.0
	call "%VsWhereDir%"\"Microsoft Visual Studio 15.0"\Common7\Tools\vsdevcmd.bat -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%"\"Microsoft Visual Studio 17.0"\Common7\Tools\vsdevcmd.bat (
	echo Microsoft Visual Studio 17.0
	call "%VsWhereDir%"\"Microsoft Visual Studio 17.0"\Common7\Tools\vsdevcmd.bat -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%"\"Microsoft Visual Studio 19.0"\Common7\Tools\vsdevcmd.bat (
	echo Microsoft Visual Studio 19.0
	call "%VsWhereDir%"\"Microsoft Visual Studio 19.0"\Common7\Tools\vsdevcmd.bat -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%"\"Microsoft Visual Studio 12.0"\Common7\Tools\vsdevcmd.bat (
	echo Microsoft Visual Studio 12.0
	call "%VsWhereDir%"\"Microsoft Visual Studio 12.0"\Common7\Tools\vsdevcmd.bat -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%"\"Microsoft Visual Studio 11.0"\Common7\Tools\vsdevcmd.bat (
	echo Microsoft Visual Studio 11.0
	call "%VsWhereDir%"\"Microsoft Visual Studio 11.0"\Common7\Tools\vsdevcmd.bat -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else (
	goto :no_compiler
)

set ObjFile=.v.c.obj

cl.exe /nologo /w /volatile:ms /Fo%ObjFile% /O2 /MD /D_VBOOTSTRAP vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /NOLOGO /OUT:v.exe /INCREMENTAL:NO
if %ERRORLEVEL% NEQ 0 (
	echo cl.exe failed to build V
	goto :compile_error
)

echo rebuild from source (twice, in case of C definitions changes)
v self -prod
if %ERRORLEVEL% NEQ 0 (
	echo V failed to build itself with error %ERRORLEVEL%
	goto :compile_error
)

rd /s /q vc
del %ObjFile%

goto :success

:no_compiler
echo You do not appear to have a GCC installation on your PATH and also do not have an MSVC installation
echo  - this means that you cannot bootstrap a V installation at this time...
echo.
echo Head to 'https://github.com/vlang/v/releases/download/v0.1.10/mingw-w64-install.exe' to download and install GCC
echo or head to 'https://visualstudio.microsoft.com/downloads/' to download and install MSVC
echo   (look for the Build Tools if you don't want to install the Visual Studio IDE)
echo.
goto :error

:compile_error
echo Failed to compile - Create an issue at 'https://github.com/vlang' and tag '@emily33901'!
rd /s /q vc
del %ObjFile%
goto :error

:error
echo Exiting from error
exit /b 1

:success
echo V build OK!
v -version
