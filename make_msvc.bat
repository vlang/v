@echo off

set exiterror=0

echo finding an MSVC installation

for /f "usebackq tokens=*" %%i in (`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
  set InstallDir=%%i
)

REM set up a devcmd

if exist "%InstallDir%\Common7\Tools\vsdevcmd.bat" (
  call "%InstallDir%\Common7\Tools\vsdevcmd.bat" -arch=x64 -host_arch=x64
) else (
  goto :nomsvc
)

echo fetch v_win.c
curl -O https://raw.githubusercontent.com/vlang/vc/master/v_win.c

echo build v_win.c with msvc
cl.exe /w /volatile:ms /D_UNICODE /DUNICODE /D_BOOTSTRAP_NO_UNICODE_STREAM /Fo.v_win.c.obj /O2 /MD v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /DEBUG:NONE /NOLOGO /OUT:v2.exe /INCREMENTAL:NO

if %ERRORLEVEL% GEQ 1 (
   goto :compileerror
)

echo rebuild from source
v2.exe -os msvc -o v.exe compiler

if %ERRORLEVEL% GEQ 1 (
   goto :compileerror
)

del .v_win.c.obj
del v_win.c
del v2.exe

exit

:nomsvc
echo Cannot find an msvc installation
goto :error

:compileerror
echo Failed to compile - Create an issue at 'https://github.com/vlang' and tag '@emily33901'!
goto :error

:error
echo Exiting from error
exit /b 1
