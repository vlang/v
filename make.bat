@echo off
setlocal EnableDelayedExpansion EnableExtensions

REM Option flags
set /a invalid_cc=0
set /a shift_counter=0
set /a flag_local=0
set /a flag_verbose=0

REM Option variables
set "log_file=%TEMP%\v_make.log"
set compiler=
set subcmd=
set target=build

REM TCC variables
set "tcc_url=https://github.com/vlang/tccbin_win.git"
set "tcc_dir=%~dp0thirdparty\tcc"
set "vc_url=https://github.com/vlang/vc.git"
set "vc_dir=%~dp0vc"

REM Let a particular environment specify their own TCC repo
if /I not ["%TCC_GIT%"] == [""] set "tcc_url=%TCC_GIT%"

pushd %~dp0

:verifyopt
REM Read stdin EOF
if ["%~1"] == [""] goto :init

REM Target options
if !shift_counter! LSS 1 (
    if "%~1" == "help" (
        if not ["%~2"] == [""] set "subcmd=%~2"& shift& set /a shift_counter+=1
    )
    for %%z in (build clean cleanall help) do (
        if "%~1" == "%%z" set target=%1& shift& set /a shift_counter+=1& goto :verifyopt
    )
)

REM Compiler option
for %%g in (-gcc -msvc -tcc -fresh-tcc -clang) do (
    if "%~1" == "%%g" set compiler=%~1& set compiler=!compiler:~1!& shift& set /a shift_counter+=1& goto :verifyopt
)

REM Standard options
if "%~1" == "--verbose" (
    if !flag_verbose! NEQ 0 (
        echo The flag %~1 has already been specified. 1>&2
        exit /b 2
    )
    set /a flag_verbose=1
    set /a shift_counter+=1
    shift
    goto :verifyopt
)
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
if "%~1" == "--logfile" (
    if ["%~2"] == [""] (
        echo Log file is not specified for -logfile parameter. 1>&2
        exit /b 2
    )
    pushd "%~dp2" 2>NUL || (
        echo The log file specified for -logfile parameter does not exist. 1>&2
        exit /b 2
    )
    popd
    set "log_file=%~sf2"
    set /a shift_counter+=2
    shift
    shift
    goto :verifyopt
)

echo Undefined option: %~1
exit /b 2

:init
goto :!target!

:cleanall
call :clean
echo.
echo Cleanup vc
echo  ^> Purge TCC binaries
call :buildcmd "rmdir /s /q "%tcc_dir%"" "  "
echo  ^> Purge vc repository
call :buildcmd "rmdir /s /q "%vc_dir%"" "  "
exit /b 0

:clean
echo Cleanup build artifacts
echo  ^> Purge debug symbols
call :buildcmd "del *.pdb *.lib *.bak *.out *.ilk *.exp *.obj *.o *.a *.so" "  "
echo  ^> Delete old V executable
call :buildcmd "del v_old.exe v*.exe" "  "
exit /b 0

:help
if [!subcmd!] == [] (
    call :usage 2>NUL
) else (
    call :help_!subcmd! 2>NUL
)
if %ERRORLEVEL% NEQ 0 echo Invalid subcommand: !subcmd!
exit /b %ERRORLEVEL%

:build
del "!log_file!">NUL 2>&1
if !flag_local! NEQ 1 (
    pushd "%vc_dir%" 2>NUL && (
        echo Updating vc...
        echo  ^> Sync with remote !vc_url!
        call :buildcmd "cd "%vc_dir%"" "  "
        call :buildcmd "git pull --quiet" "  "
        call :buildcmd "cd .." "  "
    ) || (
        echo Cloning vc...
        echo  ^> Cloning from remote !vc_url!
        call :buildcmd "git clone --depth 1 --quiet "%vc_url%"" "  "
    )
    popd
    echo.
)

echo Building V...
if not [!compiler!] == [] goto :!compiler!_strap

:clang_strap
where /q clang
if %ERRORLEVEL% NEQ 0 (
	echo  ^> Clang not found
	if not [!compiler!] == [] goto :error
	goto :gcc_strap
)

echo  ^> Attempting to build v.c with Clang
call :buildcmd "clang -std=c99 -municode -w -o v.exe .\vc\v_win.c" "  "
if %ERRORLEVEL% NEQ 0 (
	REM In most cases, compile errors happen because the version of Clang installed is too old
	call :buildcmd "clang --version" "  "
	goto :compile_error
)

echo  ^> Compiling with .\v.exe self
call :buildcmd "v.exe -cc clang self" "  "
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:gcc_strap
where /q gcc
if %ERRORLEVEL% NEQ 0 (
	echo  ^> GCC not found
	if not [!compiler!] == [] goto :error
	goto :msvc_strap
)

echo  ^> Attempting to build v.c with GCC
call :buildcmd "gcc -std=c99 -municode -w -o v.exe .\vc\v_win.c" "  "
if %ERRORLEVEL% NEQ 0 (
	REM In most cases, compile errors happen because the version of GCC installed is too old
	call :buildcmd "gcc --version" "  "
	goto :compile_error
)

echo  ^> Compiling with .\v.exe self
call :buildcmd "v.exe self" "  "
if %ERRORLEVEL% NEQ 0 goto :compile_error
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
	goto :tcc_strap
)

for /f "usebackq tokens=*" %%i in (`"%VsWhereDir%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -prerelease -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
	set InstallDir=%%i
)

if exist "%InstallDir%\Common7\Tools\vsdevcmd.bat" (
	call "%InstallDir%\Common7\Tools\vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo > NUL
) else if exist "%VsWhereDir%\Microsoft Visual Studio 14.0\Common7\Tools\vsdevcmd.bat" (
	call "%VsWhereDir%\Microsoft Visual Studio 14.0\Common7\Tools\vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo > NUL
)

set ObjFile=.v.c.obj

echo  ^> Attempting to build v.c with MSVC
call :buildcmd "cl.exe /volatile:ms /Fo%ObjFile% /O2 /MD /D_VBOOTSTRAP vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /nologo /out:v.exe /incremental:no" "  "
if %ERRORLEVEL% NEQ 0 goto :compile_error

echo  ^> Compiling with .\v.exe self
call :buildcmd "v.exe -cc msvc self" "  "
del %ObjFile%>>"!log_file!" 2>>&1
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:fresh-tcc_strap
echo  ^> Clean TCC directory
call :buildcmd "rmdir /s /q "%tcc_dir%"" "  "
call :tcc_strap
exit /b %ERRORLEVEL%

:tcc_strap
where /q tcc
if %ERRORLEVEL% NEQ 0 (
    if [!compiler!] == [] set /a invalid_cc=1
    if not exist %tcc_dir% (
        echo  ^> TCC not found
        echo  ^> Downloading TCC from %tcc_url%
        call :buildcmd "git clone --depth 1 --quiet "!tcc_url!" "%tcc_dir%"" "  "
    )
    pushd %tcc_dir% || (
        echo  ^> TCC not found, even after cloning
        goto :error
    )
    popd
    set "tcc_exe=%tcc_dir%\tcc.exe"
) else (
	for /f "usebackq delims=" %%i in (`where tcc`) do set "tcc_exe=%%i"
)

echo  ^> Updating prebuilt TCC...
pushd "%tcc_dir%\"
call :buildcmd "git pull -q" "  "
popd

echo  ^> Attempting to build v.c with TCC
call :buildcmd ""!tcc_exe!" -std=c99 -municode -lws2_32 -lshell32 -ladvapi32 -bt10 -w -o v.exe vc\v_win.c" "  "
if %ERRORLEVEL% NEQ 0 goto :compile_error

echo  ^> Compiling with .\v.exe self
call :buildcmd "v.exe -cc "!tcc_exe!" self" "  "
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:compile_error
echo.
type "!log_file!">NUL 2>&1
goto :error

:error
echo.
echo Exiting from error
exit /b 1

:success
echo  ^> V built successfully!
echo  ^> To add V to your PATH, run `.\v.exe symlink`.
if !invalid_cc! EQU 1 (
    echo.
    echo WARNING:  No C compiler was detected in your PATH. `tcc` was used temporarily
    echo           to build V, but it may have some bugs and may not work in all cases.
    echo           A more advanced C compiler like GCC or MSVC is recommended.
    echo           https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows
    echo.
)

del v_old.exe>>"!log_file!" 2>NUL
del "!log_file!" 2>NUL

:version
echo.
echo | set /p="V version: "
.\v.exe version
goto :eof

:buildcmd
if !flag_verbose! EQU 1 (
    echo [Debug] %~1>>"!log_file!"
    echo %~2 %~1
)
%~1>>"!log_file!" 2>>&1
exit /b %ERRORLEVEL%

:usage
echo Usage:
echo     make.bat [target] [compiler] [options]
echo.
echo Compiler:
echo     -msvc ^| -gcc ^| -[fresh-]tcc ^| -clang    Set C compiler
echo.
echo Target:
echo    build[default]                    Compiles V using the given C compiler
echo    clean                             Clean build artifacts and debugging symbols
echo    clean-all                         Cleanup entire ALL build artifacts and vc repository
echo    help                              Display usage help for the given target
echo.
echo Examples:
echo     make.bat -msvc
echo     make.bat -gcc --local --logpath output.log
echo     make.bat build -fresh-tcc --local
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
echo     build ^| clean ^| clean-all ^| help    Query given target
exit /b 0

:help_clean
echo Usage:
echo     make.bat clean
echo.
echo Options:
echo    --logfile PATH                    Use the specified PATH as the log
echo    --verbose                         Output compilation commands to stdout
exit /b 0

:help_cleanall
echo Usage:
echo     make.bat clean-all
echo.
echo Options:
echo    --logfile PATH                    Use the specified PATH as the log
echo    --verbose                         Output compilation commands to stdout
exit /b 0

:help_build
echo Usage:
echo     make.bat build [compiler] [options]
echo.
echo Compiler:
echo     -msvc ^| -gcc ^| -[fresh-]tcc ^| -clang    Set C compiler
echo.
echo Options:
echo    --local                           Use the local vc repository without
echo                                      syncing with remote
echo    --logfile PATH                    Use the specified PATH as the log
echo                                      file
echo    --verbose                         Output compilation commands to stdout
exit /b 0

:eof
popd
endlocal
exit /b 0