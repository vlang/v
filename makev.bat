@setlocal EnableDelayedExpansion EnableExtensions

@IF NOT DEFINED VERBOSE_MAKE @echo off

REM Option flags
set /a shift_counter=0
set /a flag_local=0

REM Option variables
set compiler=
set subcmd=
set target=build

set V_EXE=./v.exe
set V_BOOTSTRAP=./v_win_bootstrap.exe
set V_OLD=./v_old.exe
set V_UPDATED=./v_up.exe
set V_C_FILE=./vc/v_win.c
set where_exe=where.exe
if not ["%SystemRoot%"] == [""] if exist "%SystemRoot%\System32\where.exe" set where_exe=%SystemRoot%\System32\where.exe

REM TCC variables
set tcc_url=https://github.com/vlang/tccbin
set tcc_dir=%~dp0thirdparty\tcc
set tcc_exe=%tcc_dir%\tcc.exe
if "%PROCESSOR_ARCHITECTURE%" == "x86" ( set tcc_branch="thirdparty-windows-i386" ) else ( set tcc_branch="thirdparty-windows-amd64" )
if "%~1" == "-tcc32" set tcc_branch="thirdparty-windows-i386"

REM VC settings
set vc_url=https://github.com/vlang/vc
set vc_dir=%~dp0vc

REM Let a particular environment specify their own TCC and VC repos (to help mirrors)
if /I not ["%TCC_GIT%"] == [""] set tcc_url=%TCC_GIT%
if /I not ["%TCC_BRANCH%"] == [""] set tcc_branch=%TCC_BRANCH%

if /I not ["%VC_GIT%"] == [""] set vc_url=%VC_GIT%

pushd "%~dp0"

:verifyopt
REM Read stdin EOF
if ["%~1"] == [""] goto :init

REM Target options
if !shift_counter! LSS 1 (
	if "%~1" == "help" (
		if not ["%~2"] == [""] set subcmd=%~2& shift& set /a shift_counter+=1
	)
	for %%z in (build clean cleanall check help rebuild) do (
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
"%V_EXE%" test-all
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

echo  ^> Delete old V executable(s)
del v*.exe
exit /b 0

:rebuild
call :cleanall
goto :build

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
	if exist "%vc_dir%" (
		pushd "%vc_dir%" && (
			echo Updating vc...
			echo  ^> Sync with remote !vc_url!
			cd %vc_dir%
			git pull --quiet
			cd ..
			popd
		)
	) else (
		call :cloning_vc
	)
	echo.
)

echo Building V...
if not [!compiler!] == [] goto :!compiler!_strap


REM By default, use tcc, since we have it prebuilt:
:tcc_strap
:tcc32_strap
echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with "!tcc_exe!"
"!tcc_exe!" -B"%tcc_dir%" -bt10 -g -w -o "%V_BOOTSTRAP%" "%V_C_FILE%" -ladvapi32 -lws2_32 -Wl,-stack=33554432
if %ERRORLEVEL% NEQ 0 goto :compile_error
echo  ^> Compiling "%V_EXE%" with "%V_BOOTSTRAP%"
REM Keep the TCC root relative here; V forwards -cflags through a response file.
REM An absolute -B path breaks there when the checkout path contains spaces.
"%V_BOOTSTRAP%" -keepc -g -showcc -cc "!tcc_exe!" -cflags -Bthirdparty/tcc -o "%V_UPDATED%" cmd/v
if %ERRORLEVEL% NEQ 0 goto :clang_strap
call :move_updated_to_v
goto :success

:clang_strap
"%where_exe%" /q clang
if %ERRORLEVEL% NEQ 0 (
	echo  ^> Clang not found
	if not [!compiler!] == [] goto :error
	goto :gcc_strap
)

echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with Clang
clang -std=c99 -municode -g -w -o "%V_BOOTSTRAP%" "%V_C_FILE%" -ladvapi32 -lws2_32 -Wl,-stack=33554432
if %ERRORLEVEL% NEQ 0 (
	echo In most cases, compile errors happen because the version of Clang installed is too old
	clang --version
	if [!compiler!] == [] goto :gcc_strap
	goto :compile_error
)

echo  ^> Compiling "%V_EXE%" with "%V_BOOTSTRAP%"
"%V_BOOTSTRAP%" -keepc -g -showcc -cc clang -o "%V_UPDATED%" cmd/v
if %ERRORLEVEL% NEQ 0 goto :compile_error
call :move_updated_to_v
goto :success

:gcc_strap
call :find_gcc_exe
if %ERRORLEVEL% NEQ 0 (
	echo  ^> GCC not found
	if not [!compiler!] == [] goto :error
	goto :msvc_strap
)

echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with GCC "!gcc_exe!"
"!gcc_exe!" -std=c99 -municode -g -w -o "%V_BOOTSTRAP%" "%V_C_FILE%" -ladvapi32 -lws2_32 -Wl,-stack=33554432
if %ERRORLEVEL% NEQ 0 (
	echo In most cases, compile errors happen because the version of GCC installed is too old
	"!gcc_exe!" --version
	goto :compile_error
)

echo  ^> Compiling "%V_EXE%" with "%V_BOOTSTRAP%"
"%V_BOOTSTRAP%" -keepc -g -showcc -cc "!gcc_exe!" -o "%V_UPDATED%" cmd/v
if %ERRORLEVEL% NEQ 0 goto :compile_error
call :move_updated_to_v
goto :success

:msvc_strap
set VsWhereDir=%ProgramFiles(x86)%
set HostArch=x64
if "%PROCESSOR_ARCHITECTURE%" == "x86" (
	echo Using x86 Build Tools...
	set VsWhereDir=%ProgramFiles%
	set HostArch=x86
)

if not exist "%VsWhereDir%/Microsoft Visual Studio/Installer/vswhere.exe" (
	echo  ^> MSVC not found
	if not [!compiler!] == [] goto :error
	goto :compile_error
)

for /f "usebackq tokens=*" %%i in (`"%VsWhereDir%/Microsoft Visual Studio/Installer/vswhere.exe" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do (
	set InstallDir=%%i
)

if exist "%InstallDir%/Common7/Tools/vsdevcmd.bat" (
	call "%InstallDir%/Common7/Tools/vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo
) else if exist "%VsWhereDir%/Microsoft Visual Studio 14.0/Common7/Tools/vsdevcmd.bat" (
	call "%VsWhereDir%/Microsoft Visual Studio 14.0/Common7/Tools/vsdevcmd.bat" -arch=%HostArch% -host_arch=%HostArch% -no_logo
)

set ObjFile=.v.c.obj

echo  ^> Attempting to build "%V_BOOTSTRAP%" from %V_C_FILE% with MSVC
cl.exe /volatile:ms /Fo%ObjFile% /W0 /MD /D_VBOOTSTRAP /F33554432 "%V_C_FILE%" user32.lib kernel32.lib advapi32.lib shell32.lib ws2_32.lib /link /nologo /out:"%V_BOOTSTRAP%" /incremental:no
if %ERRORLEVEL% NEQ 0 (
	echo In some cases, compile errors happen because of the MSVC compiler version
	cl.exe
	if exist %ObjFile% del %ObjFile%
	goto :compile_error
)

echo  ^> Compiling "%V_EXE%" with "%V_BOOTSTRAP%"
"%V_BOOTSTRAP%" -keepc -g -showcc -cc msvc -o "%V_UPDATED%" cmd/v
if exist %ObjFile% del %ObjFile%
if %ERRORLEVEL% NEQ 0 goto :compile_error
call :move_updated_to_v
goto :success

:download_tcc
if exist "%tcc_dir%" (
	pushd "%tcc_dir%" && (
		echo Updating TCC
		echo  ^> Syncing TCC from !tcc_url!
		if exist "lib\advapi32.def" git checkout -- lib\advapi32.def >nul 2>nul
		git pull --quiet
		popd
	)
) else (
	call :bootstrap_tcc
)

call :patch_tcc_defs
if %ERRORLEVEL% NEQ 0 goto :error

if not exist "%tcc_exe%" echo  ^> TCC not found, even after cloning& goto :error
echo.
exit /b 0

:patch_tcc_defs
set "advapi32_def=%tcc_dir%\lib\advapi32.def"
if not exist "%advapi32_def%" exit /b 0
for %%G in (RegEnumKeyExW RegEnumValueW RegQueryInfoKeyW) do (
	findstr /x /c:"%%G" "%advapi32_def%" >nul || >>"%advapi32_def%" echo %%G
)
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
"%V_EXE%" run cmd/tools/detect_tcc.v
echo  ^> V built successfully!
echo  ^> To add V to your PATH, run `%V_EXE% symlink`.
echo  ^> Note: Antivirus programs may sometimes tell you there is a virus in V (there aren't any).  They can also slow compilation by a considerable amount.  Consider adding exemptions for the V install directory as well as your V project folders.

:version
echo.
echo | set /p="V version: "
"%V_EXE%" version
"%V_EXE%" run .github/problem-matchers/register_all.vsh
goto :eof

:usage
echo Usage:
echo     makev.bat [target] [compiler] [options]
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
echo     rebuild           Fully clean/reset repository and rebuild V
echo.
echo Examples:
echo     makev.bat -msvc
echo     makev.bat -gcc --local
echo     makev.bat build -tcc --local
echo     makev.bat -tcc32
echo     makev.bat help clean
echo.
echo Use "make help <target>" for more information about a target, for instance: "make help clean"
echo.
echo Note: Any undefined/unsupported options will be ignored
exit /b 0

:help_help
echo Usage:
echo     makev.bat help [target]
echo.
echo Target:
echo     build ^| clean ^| cleanall ^| help    Query given target
exit /b 0

:help_clean
echo Usage:
echo     makev.bat clean
echo.
exit /b 0

:help_cleanall
echo Usage:
echo     makev.bat cleanall
echo.
exit /b 0

:help_build
echo Usage:
echo     makev.bat build [compiler] [options]
echo.
echo Compiler:
echo     -msvc ^| -gcc ^| -tcc ^| -tcc32 ^| -clang    Set C compiler
echo.
echo Options:
echo    --local     Use the local vc repository without
echo                syncing with remote
exit /b 0

:help_rebuild
echo Usage:
echo     makev.bat rebuild [compiler] [options]
echo.
echo Compiler:
echo     -msvc ^| -gcc ^| -tcc ^| -tcc32 ^| -clang    Set C compiler
echo.
echo Options:
echo    --local     Use the local vc repository without
echo                syncing with remote
exit /b 0

:bootstrap_tcc
echo Bootstrapping TCC...
echo  ^> TCC not found
if "!tcc_branch!" == "thirdparty-windows-i386" ( echo  ^> Downloading TCC32 from !tcc_url! , branch !tcc_branch! ) else ( echo  ^> Downloading TCC64 from !tcc_url! , branch !tcc_branch! )
git clone --filter=blob:none --quiet --branch !tcc_branch! !tcc_url! "%tcc_dir%"
git --no-pager -C "%tcc_dir%" log -n3
exit /b 0

:cloning_vc
echo Cloning vc...
echo  ^> Cloning from remote !vc_url!
git clone --filter=blob:none --quiet "%vc_url%"
exit /b 0

:find_gcc_exe
REM Prefer MinGW target-prefixed drivers when present, so PATH conflicts do not pick an unrelated gcc.exe.
set "gcc_exe="
if "%PROCESSOR_ARCHITECTURE%" == "x86" (
	call :resolve_executable i686-w64-mingw32-gcc
	if not [!resolved_exe!] == [] (
		set "gcc_exe=!resolved_exe!"
		exit /b 0
	)
) else (
	call :resolve_executable x86_64-w64-mingw32-gcc
	if not [!resolved_exe!] == [] (
		set "gcc_exe=!resolved_exe!"
		exit /b 0
	)
)
call :resolve_executable gcc
if not [!resolved_exe!] == [] (
	set "gcc_exe=!resolved_exe!"
	exit /b 0
)
exit /b 1

:resolve_executable
set "resolved_exe="
for %%i in ("%~1") do if exist "%%~fi" (
	set "resolved_exe=%%~fi"
	exit /b 0
)
for /f "usebackq delims=" %%i in (`where "%~1" 2^>nul`) do (
	set "resolved_exe=%%~fi"
	exit /b 0
)
exit /b 1

:eof
popd
endlocal
exit /b 0

:move_updated_to_v
@REM del "%V_EXE%" &:: breaks if `makev.bat` is run from `v up` b/c of held file handle on `%V_EXE%`
if exist "%V_EXE%" move "%V_EXE%" "%V_OLD%" >nul
REM sleep for at most 100ms
ping 192.0.2.1 -n 1 -w 100 >nul
move "%V_UPDATED%" "%V_EXE%" >nul
exit /b 0
