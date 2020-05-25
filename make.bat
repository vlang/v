@echo off

echo Building V

pushd %~dp0

if exist "vc" (
	echo Updating vc...
	cd vc
	git pull --quiet
	cd ..
) else (
	echo Cloning vc...
	git clone --depth 1 --quiet https://github.com/vlang/vc
)

REM option to force msvc or gcc
if "%~1"=="-gcc" goto :gcc_strap
if "%~1"=="-msvc" goto :msvc_strap


:gcc_strap
echo Attempting to build v.c with GCC...

for /f "usebackq tokens=*" %%i in (`where gcc`) do (
	set gcc_path=%%i
)

if not exist "%gcc_path%" (
	goto :msvc_strap
)

gcc -std=c99 -municode -w -o v.exe vc\v_win.c
if %ERRORLEVEL% NEQ 0 (
	echo gcc failed to compile - Create an issue at 'https://github.com/vlang'
	rd /s /q vc
	goto :error
)

REM remove the -prod parameter to shorten compilation time,
REM and it will be restored when v is a stable version.
v.exe self
if %ERRORLEVEL% NEQ 0 (
	echo v.exe failed to compile itself - Create an issue at 'https://github.com/vlang'
	goto :error
)

del v_old.exe
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
for /f "usebackq tokens=*" %%i in (`"%VsWhereDir%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -prerelease -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
	set InstallDir=%%i
)

if exist "%InstallDir%\Common7\Tools\vsdevcmd.bat" (
	call "%InstallDir%\Common7\Tools\vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%\Microsoft Visual Studio 14.0\Common7\Tools\vsdevcmd.bat" (
	call "%VsWhereDir%\Microsoft Visual Studio 14.0\Common7\Tools\vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else (
	goto :no_compiler
)

set ObjFile=.v.c.obj

cl.exe /nologo /w /volatile:ms /Fo%ObjFile% /O2 /MD /D_VBOOTSTRAP vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /NOLOGO /OUT:v.exe /INCREMENTAL:NO
if %ERRORLEVEL% NEQ 0 (
	echo cl.exe failed to build V
	goto :compile_error
)

REM remove the -prod parameter to shorten compilation time,
REM and it will be restored when v is a stable version.
v.exe -cc msvc self
if %ERRORLEVEL% NEQ 0 (
	echo V failed to build itself with error %ERRORLEVEL%
	del %ObjFile%
	goto :compile_error
)

del v_old.exe
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
goto :error

:error
echo Exiting from error
popd
exit /b 1

:success
echo V build OK!
v.exe version
popd
