@setlocal EnableDelayedExpansion EnableExtensions

IF NOT DEFINED VERBOSE_MAKE @echo off

REM Option flags
set /a shift_counter=0
set /a flag_local=0

REM Option variables
set compiler=
set subcmd=
set target=build

REM TCC variables
set "tcc_url=https://github.com/vlang/tccbin"
set "tcc_dir=thirdparty\tcc"
set "tcc_exe=thirdparty\tcc\tcc.exe"
if "%PROCESSOR_ARCHITECTURE%" == "x86" ( set "tcc_branch=thirdparty-windows-i386" ) else ( set "tcc_branch=thirdparty-windows-amd64" )
if "%~1" == "-tcc32" set "tcc_branch=thirdparty-windows-i386"

REM VC settings
set "vc_url=https://github.com/vlang/vc"
set "vc_dir=%~dp0vc"

REM Let a particular environment specify their own TCC and VC repos (to help mirrors)
if /I not ["%TCC_GIT%"] == [""] set "tcc_url=%TCC_GIT%"
if /I not ["%TCC_BRANCH%"] == [""] set "tcc_branch=%TCC_BRANCH%"

if /I not ["%VC_GIT%"] == [""] set "vc_url=%VC_GIT%"

pushd %~dp0

:verifyopt
REM Read stdin EOF
if ["%~1"] == [""] goto :init

REM Target options
if !shift_counter! LSS 1 (
    if "%~1" == "help" (
        if not ["%~2"] == [""] set "subcmd=%~2"& shift& set /a shift_counter+=1
    )
    for %%z in (build clean cleanall check help) do (
        if "%~1" == "%%z" set target=%1& shift& set /a shift_counter+=1& goto :verifyopt
    )
)

REM Compiler option
for %%g in (-gcc -msvc -tcc -tcc32 -clang) do (
    if "%~1" == "%%g" set compiler=%~1& set compiler=!compiler:~1!& shift& set /a shift_counter+=1& goto :verifyopt
)

REM Standard options
if "%~1" == "--local" (
    if !flag_local! NEQ 0 (
        echo The flag %~1 has already been specified. 1>&2
        exit /b 2
    )
    set /a flag_local=1
    set /a shift_counter+=1
    shift
    goto :verifyopt
)

echo Undefined option: %~1
exit /b 2

:init
goto :!target!

:check
echo.
echo Check everything
v.exe test-all
exit /b 0

:cleanall
call :clean
if %ERRORLEVEL% NEQ 0 exit /b %ERRORLEVEL%
echo.
echo Cleanup vc
echo  ^> Purge TCC binaries
rmdir /s /q "%tcc_dir%"
echo  ^> Purge vc repository
rmdir /s /q "%vc_dir%"
exit /b 0

:clean
echo Cleanup build artifacts
echo  ^> Purge debug symbols
del *.pdb *.lib *.bak *.out *.ilk *.exp *.obj *.o *.a *.so

echo  ^> Delete old V executable
del v_old.exe v*.exe
exit /b 0

:help
if [!subcmd!] == [] (
    call :usage
) else (
    call :help_!subcmd!
)
if %ERRORLEVEL% NEQ 0 echo Invalid subcommand: !subcmd!
exit /b %ERRORLEVEL%

:build
if !flag_local! NEQ 1 (
    call :download_tcc
    if %ERRORLEVEL% NEQ 0 goto :error
    pushd "%vc_dir%" && (
        echo Updating vc...
        echo  ^> Sync with remote !vc_url!
        cd "%vc_dir%"
        git pull --quiet
        cd ..
        popd
    ) || call :cloning_vc
    echo.
)

echo Building V...
if not [!compiler!] == [] goto :!compiler!_strap


REM By default, use tcc, since we have it prebuilt:
:tcc_strap
:tcc32_strap
echo  ^> Attempting to build v_win.c with "!tcc_exe!"
"!tcc_exe!" -Bthirdparty/tcc -bt10 -g -w -o v.exe vc\v_win.c -ladvapi32
if %ERRORLEVEL% NEQ 0 goto :compile_error
echo  ^> Compiling .\v.exe with itself
v.exe -keepc -g -showcc -cc "!tcc_exe!" -cflags -Bthirdparty/tcc -o v2.exe cmd/v
if %ERRORLEVEL% NEQ 0 goto :clang_strap
call :move_v2_to_v
goto :success

:clang_strap
where /q clang
if %ERRORLEVEL% NEQ 0 (
	echo  ^> Clang not found
	if not [!compiler!] == [] goto :error
	goto :gcc_strap
)

echo  ^> Attempting to build v_win.c with Clang
clang -std=c99 -municode -g -w -o v.exe .\vc\v_win.c -ladvapi32
if %ERRORLEVEL% NEQ 0 (
	echo In most cases, compile errors happen because the version of Clang installed is too old
	clang --version
	goto :compile_error
)

echo  ^> Compiling .\v.exe with itself
v.exe -keepc -g -showcc -cc clang -o v2.exe cmd/v
if %ERRORLEVEL% NEQ 0 goto :compile_error
call :move_v2_to_v
goto :success

:gcc_strap
where /q gcc
if %ERRORLEVEL% NEQ 0 (
	echo  ^> GCC not found
	if not [!compiler!] == [] goto :error
	goto :msvc_strap
)

echo  ^> Attempting to build v_win.c with GCC
gcc -std=c99 -municode -g -w -o v.exe .\vc\v_win.c -ladvapi32
if %ERRORLEVEL% NEQ 0 (
	echo In most cases, compile errors happen because the version of GCC installed is too old
	gcc --version
	goto :compile_error
)

echo  ^> Compiling .\v.exe with itself
v.exe -keepc -g -showcc -cc gcc -o v2.exe cmd/v
if %ERRORLEVEL% NEQ 0 goto :compile_error
call :move_v2_to_v
goto :success

:msvc_strap
set VsWhereDir=%ProgramFiles(x86)%
set HostArch=x64
if "%PROCESSOR_ARCHITECTURE%" == "x86" (
	echo Using x86 Build Tools...
	set VsWhereDir=%ProgramFiles%
	set HostArch=x86
)

if not exist "%VsWhereDir%\Microsoft Visual Studio\Installer\vswhere.exe" (
	echo  ^> MSVC not found
	if not [!compiler!] == [] goto :error
	goto :compile_error
)

for /f "usebackq tokens=*" %%i in (`"%VsWhereDir%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
	set InstallDir=%%i
)

if exist "%InstallDir%\Common7\Tools\vsdevcmd.bat" (
	call "%InstallDir%\Common7\Tools\vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%\Microsoft Visual Studio 14.0\Common7\Tools\vsdevcmd.bat" (
	call "%VsWhereDir%\Microsoft Visual Studio 14.0\Common7\Tools\vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo
)

set ObjFile=.v.c.obj

echo  ^> Attempting to build v_win.c with MSVC
cl.exe /volatile:ms /Fo%ObjFile% /O2 /MD /D_VBOOTSTRAP vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /nologo /out:v.exe /incremental:no
if %ERRORLEVEL% NEQ 0 (
    echo In some cases, compile errors happen because of the MSVC compiler version
    cl.exe
    goto :compile_error
)

echo  ^> Compiling .\v.exe with itself
v.exe -keepc -g -showcc -cc msvc -o v2.exe cmd/v
del %ObjFile%
if %ERRORLEVEL% NEQ 0 goto :compile_error
call :move_v2_to_v
goto :success

:download_tcc
pushd %tcc_dir% && (
    echo Updating TCC
    echo  ^> Syncing TCC from !tcc_url!
    git pull --quiet
    popd
) || call :bootstrap_tcc

if [!tcc_exe!] == [] echo  ^> TCC not found, even after cloning& goto :error
echo.
exit /b 0

:compile_error
echo.
echo Backend compiler error
goto :error

:error
echo.
echo Exiting from error
echo ERROR: please follow the instructions in https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows
exit /b 1

:success
.\v.exe run cmd\tools\detect_tcc.v
echo  ^> V built successfully!
echo  ^> To add V to your PATH, run `.\v.exe symlink`.

:version
echo.
echo | set /p="V version: "
.\v.exe version
goto :eof

:usage
echo Usage:
echo     make.bat [target] [compiler] [options]
echo.
echo Compiler:
echo     -msvc ^| -gcc ^| -tcc ^| -tcc32 ^| -clang    Set C compiler
echo.
echo Target:
echo     build[default]    Compiles V using the given C compiler
echo     clean             Clean build artifacts and debugging symbols
echo     cleanall          Cleanup entire ALL build artifacts and vc repository
echo     check             Check that tests pass, and the repository is in a good shape for Pull Requests
echo     help              Display help for the given target
echo.
echo Examples:
echo     make.bat -msvc
echo     make.bat -gcc --local
echo     make.bat build -tcc --local
echo     make.bat -tcc32
echo     make.bat help clean
echo.
echo Use "make help <target>" for more information about a target, for instance: "make help clean"
echo.
echo Note: Any undefined/unsupported options will be ignored
exit /b 0

:help_help
echo Usage:
echo     make.bat help [target]
echo.
echo Target:
echo     build ^| clean ^| cleanall ^| help    Query given target
exit /b 0

:help_clean
echo Usage:
echo     make.bat clean
echo.
exit /b 0

:help_cleanall
echo Usage:
echo     make.bat cleanall
echo.
exit /b 0

:help_build
echo Usage:
echo     make.bat build [compiler] [options]
echo.
echo Compiler:
echo     -msvc ^| -gcc ^| -tcc ^| -tcc32 ^| -clang    Set C compiler
echo.
echo Options:
echo    --local     Use the local vc repository without
echo                syncing with remote
exit /b 0

:bootstrap_tcc
echo Bootstraping TCC...
echo  ^> TCC not found
if "!tcc_branch!" == "thirdparty-windows-i386" ( echo  ^> Downloading TCC32 from !tcc_url! , branch !tcc_branch! ) else ( echo  ^> Downloading TCC64 from !tcc_url! , branch !tcc_branch! )
git clone --depth 1 --quiet --single-branch --branch !tcc_branch! !tcc_url! "%tcc_dir%"
git -C "%tcc_dir%" log -n3
exit /b 0

:cloning_vc
echo Cloning vc...
echo  ^> Cloning from remote !vc_url!
git clone --depth 1 --quiet "%vc_url%"
exit /b 0

:eof
popd
endlocal
exit /b 0

:move_v2_to_v
del v.exe
REM sleep for at most 100ms
ping 192.0.2.1 -n 1 -w 100 >nul
move v2.exe v.exe
exit /b 0
