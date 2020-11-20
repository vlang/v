@echo off
setlocal EnableDelayedExpansion

REM option flags
set /a valid_cc=0
set /a use_local=0
set /a verbose_log=0

REM option variables
set log_file="%TEMP%\v_make.log"
set compiler_opt=""

REM tcc variables
set tcc_url="https://github.com/vlang/tccbin_win.git"
set tcc_dir="%~dp0thirdparty\tcc"

REM let a particular environment specify their own tcc
if /I not "%TCC_GIT%" == "" (
    set tcc_url="%TCC_GIT%"
)

pushd %~dp0

:verifyopt
REM end at EOL
if ["%~1"] == [""] goto :init

REM help option
if "%~1" == "-h" call :usage& exit /b %ERRORLEVEL%
if "%~1" == "--help" call :usage& exit /b %ERRORLEVEL%

REM compiler option
if "%~1" == "-gcc" set compiler_opt="gcc"& shift& goto :verifyopt
if "%~1" == "-msvc" set compiler_opt="msvc"& shift& goto :verifyopt
if "%~1" == "-tcc" set compiler_opt="tcc"& shift& goto :verifyopt
if "%~1" == "-fresh-tcc" set compiler_opt="fresh-tcc"& shift& goto :verifyopt
if "%~1" == "-clang" set compiler_opt="clang"& shift& goto :verifyopt

REM standard options
if "%~1" == "-local" (
    if !use_local! EQU 0 ( set /a use_local=1 )
    shift
    goto :verifyopt
)
if "%~1" == "-v" (
    if !verbose_log! EQU 0 ( set /a verbose_log=1 )
    shift
    goto :verifyopt
)
if "%~1" == "--verbose" (
    if !verbose_log! EQU 0 ( set /a verbose_log=1 )
    shift
    goto :verifyopt
)
if "%~1" == "-logfile" (
    if /I "%~2" == "" (
        echo Log file is not specified for -logfile parameter. 1>&2
        exit /b 2
    )
    pushd "%~dp2" 2>NUL || (
        echo The log file specified for -logfile parameter does not exist. 1>&2
        exit /b 2
    )
    popd
    set log_file="%~sf2"
    shift
    shift
    goto :verifyopt
)
echo Ignoring unidentified option: %~1 & shift
goto :verifyopt

:init
echo.
del !log_file!>NUL 2>&1
if !use_local! NEQ 1 (
    if exist "vc" (
        echo Updating vc...
        echo  ^> Sync with remote https://github.com/vlang/vc.git
        call :buildcmd "cd vc" "  "
        call :buildcmd "git pull --quiet" "  "
        call :buildcmd "cd .." "  "
    ) else (
        echo Cloning vc...
        echo  ^> Cloning from remote https://github.com/vlang/vc.git
        call :buildcmd "git clone --depth 1 --quiet https://github.com/vlang/vc.git" "  "
    )
)

echo.
echo Building V...

if !compiler_opt! == "clang" goto :clang_strap
if !compiler_opt! == "gcc" goto :gcc_strap
if !compiler_opt! == "msvc" goto :msvc_strap
if !compiler_opt! == "tcc" goto :tcc_strap
if !compiler_opt! == "fresh-tcc" goto :tcc_strap
if !compiler_opt! == "" goto :gcc_strap

:clang_strap
where /q clang
if %ERRORLEVEL% NEQ 0 (
	echo  ^> Clang not found
	if not !compiler_opt! == "" goto :error
	goto :gcc_strap
)

set /a valid_cc=1

echo  ^> Attempting to build v.c with Clang
call :buildcmd "clang -std=c99 -municode -pedantic -w -o v.exe .\vc\v_win.c" "  "
if %ERRORLEVEL% NEQ 0 (
	REM In most cases, compile errors happen because the version of Clang installed is too old
	clang --version>>!log_file! 2>>&1
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
	if not !compiler_opt! == "" goto :error
	goto :msvc_strap
)

set /a valid_cc=1

echo  ^> Attempting to build v.c with GCC
call :buildcmd "gcc -std=c99 -municode -w -o v.exe .\vc\v_win.c" "  "
if %ERRORLEVEL% NEQ 0 (
	rem In most cases, compile errors happen because the version of GCC installed is too old
	gcc --version>>!log_file! 2>>&1
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
	if not !compiler_opt! == "" goto :error
	goto :tcc_strap
)

set /a valid_cc=1

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
del %ObjFile%>>!log_file! 2>>&1
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:tcc_strap
where /q tcc
if %ERRORLEVEL% NEQ 0 (
	if !compiler_opt! == "fresh-tcc" (
        echo  ^> Clean TCC directory
        call :buildcmd "rd /s /q "%tcc_dir%">NUL 2>&1" "  "
        set /a valid_cc=1
    ) else if !compiler_opt! == "tcc" ( set /a valid_cc=1 )
    if not exist "%tcc_dir%" (
        echo  ^> TCC not found
        echo  ^> Downloading TCC from %tcc_url%
        call :buildcmd "git clone --depth 1 --quiet "%tcc_url%" "%tcc_dir%"" "  "
    )
    pushd %tcc_dir% || (
        echo  ^> TCC not found, even after cloning
        goto :error
    )
    popd
    set tcc_exe="%tcc_dir%\tcc.exe"
) else (
	for /f "delims=" %%i in ('where tcc') do set "tcc_exe=%%i"
    set /a valid_cc=1
)

echo  ^> Updating prebuilt TCC...
pushd "%tcc_dir%"\
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
type !log_file!>NUL 2>&1
goto :error

:error
echo.
echo Exiting from error
popd
exit /b 1

:success
echo  ^> V built successfully!
echo  ^> To add V to your PATH, run `.\v.exe symlink`.
if !valid_cc! EQU 0 (
    echo.
    echo WARNING:  No C compiler was detected in your PATH. `tcc` was used temporarily
    echo           to build V, but it may have some bugs and may not work in all cases.
    echo           A more advanced C compiler like GCC or MSVC is recommended.
    echo           https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows
    echo.
)

del v_old.exe>>!log_file! 2>>&1
del !log_file!

:version
echo.
echo | set /p="V version: "
.\v.exe version
popd
exit /b 0

:buildcmd
if !verbose_log! EQU 1 (
    echo [Debug] %~1>>!log_file!
    echo %~2 %~1
)
%~1>>!log_file! 2>>&1
exit /b %ERRORLEVEL%

:usage
echo.
echo Usage:
echo     make.bat [compiler] [options]
echo.
echo Compiler :
echo     -msvc ^| -gcc ^| -[fresh-]tcc ^| -clang    Set C compiler
echo.
echo Options:
echo     -local                        Use the local vc repository without
echo                                   syncing with remote
echo     -logfile PATH                 Use the specified PATH as the log
echo                                   file
echo     -v ^| --verbose                Output compilation commands to stdout
echo     -h ^| --help             	  Display this help message and exit
echo.
echo Examples:
echo     make.bat -msvc
echo     make.bat -gcc --local --logpath output.log
echo     make.bat -fresh-tcc --local
echo     make.bat --help
echo.
echo Note: Any undefined options will be ignored
exit /b 0