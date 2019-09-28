@echo off

echo Building V

if exist "vc" (
    rd /s /q vc
)

git version

echo Downloading v.c...
git clone --depth 1 --quiet https://github.com/vlang/vc

REM option to force msvc or gcc
if "%~1"=="-gcc" goto :gccstrap
if "%~1"=="-msvc" goto :msvcstrap


:gccstrap

echo Attempting to build v.c with GCC...

for /f "usebackq tokens=*" %%i in (`where gcc`) do (
  set gccpath=%%i
)

if not exist "%gccpath%" (
    goto:msvcstrap
)

gcc -std=gnu11 -w -o v2.exe vc\v_win.c
if %ERRORLEVEL% NEQ 0 (
    echo gcc failed to compile - Create an issue at 'https://github.com/vlang'
    exit /b 1
)

echo rebuild from source (twice, in case of C definitions changes)
v2.exe -o v3.exe compiler
v3.exe -o v.exe -prod compiler
if %ERRORLEVEL% NEQ 0 (
    echo v.exe failed to compile itself - Create an issue at 'https://github.com/vlang'
    exit /b 1
)

del v2.exe
del v3.exe
rd /s /q vc

goto :success


:msvcstrap
echo Attempting to build v.c  with MSVC...

for /f "usebackq tokens=*" %%i in (`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
  set InstallDir=%%i
)

if exist "%InstallDir%\Common7\Tools\vsdevcmd.bat" (
    call "%InstallDir%\Common7\Tools\vsdevcmd.bat" -arch=x64 -host_arch=x64 -no_logo
) else (
    goto :nocompiler
)

cl.exe /nologo /w /volatile:ms /Fo.v.c.obj /O2 /MD vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /NOLOGO /OUT:v2.exe /INCREMENTAL:NO
if %ERRORLEVEL% NEQ 0 (
    echo cl.exe failed to build V
    goto :compileerror
)

echo rebuild from source (twice, in case of C definitions changes)
v2.exe -os msvc -o v3.exe compiler
v3.exe -os msvc -o v.exe -prod compiler
if %ERRORLEVEL% NEQ 0 (
    echo V failed to build itself
    goto :compileerror
)

del v2.exe
del v3.exe
rd /s /q vc

goto :success

:nocompiler
echo You do not appear to have a GCC installation on your PATH and also do not have an MSVC installation
echo  - this means that you cannot bootstrap a V installation at this time...
echo.
echo Head to 'https://github.com/vlang/v/releases/download/v0.1.10/mingw-w64-install.exe' to download and install GCC
echo or head to 'https://visualstudio.microsoft.com/downloads/' to download and install MSVC
echo   (look for the Build Tools if you don't want to install the Visual Studio IDE)
echo.
goto :error

:compileerror
echo Failed to compile - Create an issue at 'https://github.com/vlang' and tag '@emily33901'!
goto :error

:error
echo Exiting from error
exit /b 1

:success
