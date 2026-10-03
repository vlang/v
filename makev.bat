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
set V_STAGE=./v_stage.exe
set V_STAGE_C=./v_stage.c
set V_C_FILE=./vc/v_win.c
REM Portable vc snapshots need the full V1 compiler until the new driver is built.
REM Keep this aligned with VC_BOOTSTRAP_DEFINE in the Unix makefiles.
set VC_BOOTSTRAP_DEFINE=-DCUSTOM_DEFINE_v1_fallback
REM Existing vc bootstraps may predate the TCC Win64 CRT prelude fix, so keep
REM their cgen single-threaded while they build a fresh compiler from sources.
REM TODO: remove this after vc/v_win.c is regenerated with the fixed CRT prelude.
set V_BOOTSTRAP_VFLAGS=-no-parallel
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
	for %%z in (build clean cleanall check help latest_tcc rebuild) do (
		if "%~1" == "%%z" set target=%~1& shift& set /a shift_counter+=1& goto :verifyopt
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
exit /b !ERRORLEVEL!

:cleanall
call :clean
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
echo.
echo Cleanup vc
echo  ^> Purge TCC binaries
if exist "%tcc_dir%" (
	rmdir /s /q "%tcc_dir%"
	if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
)
echo  ^> Purge vc repository
if exist "%vc_dir%" (
	rmdir /s /q "%vc_dir%"
	if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
)
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
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
goto :build

:latest_tcc
call :download_tcc
if !ERRORLEVEL! NEQ 0 goto :error
echo  ^> TCC is up to date.
exit /b 0

:help
if [!subcmd!] == [] (
	call :usage
) else (
	call :help_!subcmd!
)
if !ERRORLEVEL! NEQ 0 echo Invalid subcommand: !subcmd!
exit /b !ERRORLEVEL!

:build
if !flag_local! NEQ 1 (
	call :download_tcc
	if !ERRORLEVEL! NEQ 0 goto :error
	if exist "%vc_dir%" (
		pushd "%vc_dir%"
		if !ERRORLEVEL! NEQ 0 goto :error
		echo Updating vc...
		echo  ^> Sync with remote !vc_url!
		git pull --rebase --quiet
		if !ERRORLEVEL! NEQ 0 (
			popd
			goto :error
		)
		popd
	) else (
		call :cloning_vc
		if !ERRORLEVEL! NEQ 0 goto :error
	)
	echo.
)

echo Building V...
if not [!compiler!] == [] goto :!compiler!_strap


REM By default, use tcc, since we have it prebuilt:
:tcc_strap
:tcc32_strap
call :build_bootstrap_with_tcc
if !ERRORLEVEL! NEQ 0 goto :compile_error
call :build_fresh_v_with_tcc
if !ERRORLEVEL! NEQ 0 goto :tcc_retry_with_host_bootstrap
call :move_updated_to_v
if !ERRORLEVEL! NEQ 0 goto :compile_error
goto :success

:tcc_retry_with_host_bootstrap
echo  ^> TCC-built bootstrap failed; retrying bootstrap with Clang/GCC before compiling "%V_EXE%" with TCC
call :build_bootstrap_with_clang
if !ERRORLEVEL! NEQ 0 call :build_bootstrap_with_gcc
if !ERRORLEVEL! NEQ 0 (
	if [!compiler!] == [] goto :clang_strap
	goto :compile_error
)
call :build_fresh_v_with_tcc
if !ERRORLEVEL! NEQ 0 (
	if [!compiler!] == [] goto :clang_strap
	goto :compile_error
)
call :move_updated_to_v
if !ERRORLEVEL! NEQ 0 goto :compile_error
goto :success

:build_fresh_v_with_tcc
call :build_stage_with_tcc
set stage_error=!ERRORLEVEL!
if !stage_error! NEQ 0 (
	call :try_delete "%V_STAGE%"
	exit /b !stage_error!
)
echo  ^> Compiling "%V_EXE%" with "%V_STAGE%"
REM V3 supplies the absolute bundled-TCC root itself. A relative -B here would
REM override it after V3 changes into its isolated link directory.
"%V_STAGE%" %V_BOOTSTRAP_VFLAGS% -keepc -g -showcc -cc "!tcc_exe!" -o "%V_UPDATED%" cmd/v
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE%"
exit /b !stage_error!

:clang_strap
call :build_bootstrap_with_clang
if !ERRORLEVEL! NEQ 0 (
	if not [!compiler!] == [] goto :error
	goto :gcc_strap
)

call :build_stage_with_clang
if !ERRORLEVEL! NEQ 0 goto :compile_error
echo  ^> Compiling "%V_EXE%" with "%V_STAGE%"
"%V_STAGE%" %V_BOOTSTRAP_VFLAGS% -keepc -g -showcc -cc "!clang_exe!" -cflags "--target=!clang_target!" -o "%V_UPDATED%" cmd/v
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE%"
if !stage_error! NEQ 0 goto :compile_error
call :move_updated_to_v
if !ERRORLEVEL! NEQ 0 goto :compile_error
goto :success

:gcc_strap
call :build_bootstrap_with_gcc
if !ERRORLEVEL! NEQ 0 (
	if not [!compiler!] == [] goto :error
	goto :msvc_strap
)

call :build_stage_with_gcc
if !ERRORLEVEL! NEQ 0 goto :compile_error
echo  ^> Compiling "%V_EXE%" with "%V_STAGE%"
"%V_STAGE%" %V_BOOTSTRAP_VFLAGS% -keepc -g -showcc -cc "!gcc_exe!" -o "%V_UPDATED%" cmd/v
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE%"
if !stage_error! NEQ 0 goto :compile_error
call :move_updated_to_v
if !ERRORLEVEL! NEQ 0 goto :compile_error
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

echo  ^> Bootstrapping "%V_BOOTSTRAP%" before compiling "%V_EXE%" with MSVC
REM Bootstrap order for -msvc: bundled TCC first (fastest, no external
REM toolchain needed), then MSVC itself (already confirmed present -
REM vsdevcmd.bat has already run above - and explicitly what -msvc asked
REM for), then Clang, then GCC as the remaining fallbacks. Any of these can
REM fail to compile vc/v_win.c on its own (e.g. a stale snapshot only some
REM toolchains can parse - see vlang/v#29025 and vlang/tccbin#96), which is
REM why every option here is still tried in order rather than stopping at
REM the first choice.
REM A bootstrap can also compile successfully but fail at runtime, for example
REM while setting up process pipes. Accept it only after it builds the stage.
call :build_msvc_stage tcc
if !ERRORLEVEL! NEQ 0 call :build_msvc_stage msvc
if !ERRORLEVEL! NEQ 0 call :build_msvc_stage clang
if !ERRORLEVEL! NEQ 0 call :build_msvc_stage gcc
if !ERRORLEVEL! NEQ 0 (
	echo Could not build a working bootstrap compiler before compiling with MSVC
	call :try_delete "%ObjFile%"
	call :try_delete "%V_STAGE%"
	goto :compile_error
)

echo  ^> Compiling "%V_EXE%" with "%V_STAGE%"
"%V_STAGE%" %V_BOOTSTRAP_VFLAGS% -keepc -g -showcc -cc msvc -o "%V_UPDATED%" cmd/v
set msvc_error=!ERRORLEVEL!
call :try_delete "%ObjFile%"
call :try_delete "%V_STAGE%"
if %msvc_error% NEQ 0 goto :compile_error
call :move_updated_to_v
if !ERRORLEVEL! NEQ 0 goto :compile_error
goto :success

:download_tcc
if exist "%tcc_dir%" (
	pushd "%tcc_dir%"
	if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
	echo Updating TCC
	echo  ^> Syncing TCC from !tcc_url!
	if exist "lib\advapi32.def" git checkout -- lib\advapi32.def >nul 2>nul
	git pull --rebase --quiet
	if !ERRORLEVEL! NEQ 0 (
		set tcc_update_error=!ERRORLEVEL!
		popd
		exit /b !tcc_update_error!
	)
	popd
) else (
	call :bootstrap_tcc
	if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
)

call :patch_tcc_defs
if !ERRORLEVEL! NEQ 0 goto :error

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
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
echo  ^> V built successfully!
echo  ^> To add V to your PATH, run `%V_EXE% symlink`.
echo  ^> Note: Antivirus programs may sometimes tell you there is a virus in V (there aren't any).  They can also slow compilation by a considerable amount.  Consider adding exemptions for the V install directory as well as your V project folders.

:version
echo.
echo | set /p="V version: "
"%V_EXE%" version
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
"%V_EXE%" run .github/problem-matchers/register_all.vsh
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
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
echo     latest_tcc        Update the bundled TCC without rebuilding V
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

:help_latest_tcc
echo Usage:
echo     makev.bat latest_tcc
echo.
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
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
git --no-pager -C "%tcc_dir%" log -n3
exit /b !ERRORLEVEL!

:cloning_vc
echo Cloning vc...
echo  ^> Cloning from remote !vc_url!
git clone --filter=blob:none --quiet "%vc_url%"
exit /b !ERRORLEVEL!

:build_msvc_stage
call :build_bootstrap_with_%~1
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
call :build_stage_with_%~1
set stage_error=!ERRORLEVEL!
if !stage_error! NEQ 0 call :try_delete "%V_STAGE%"
exit /b !stage_error!

:build_bootstrap_with_tcc
if not exist "!tcc_exe!" (
	echo  ^> TCC not found
	exit /b 1
)
echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with "!tcc_exe!"
"!tcc_exe!" %VC_BOOTSTRAP_DEFINE% -B"%tcc_dir%" -bt10 -g -w -o "%V_BOOTSTRAP%" "%V_C_FILE%" -ladvapi32 -lws2_32 -lbcrypt -Wl,-stack=33554432
exit /b !ERRORLEVEL!

:build_bootstrap_with_msvc
REM Only reachable from msvc_strap, where vsdevcmd.bat has already put cl.exe
REM on PATH and MSVC's presence is already confirmed. Tried right after the
REM bundled TCC there, since -msvc means MSVC was explicitly requested.
REM Clang/GCC are the remaining fallbacks for when neither TCC nor cl.exe can
REM compile %V_C_FILE% (e.g. a stale vc/v_win.c snapshot that only some
REM toolchains can parse - see vlang/v#29025 and vlang/tccbin#96 for two
REM concrete cases).
where cl >nul 2>&1
if !ERRORLEVEL! NEQ 0 (
	echo  ^> MSVC's cl.exe not found on PATH
	exit /b 1
)
echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with MSVC
cl /nologo %VC_BOOTSTRAP_DEFINE% /volatile:ms /bigobj /MD /we4013 /utf-8 /w /std:c11 /D_CRT_DECLARE_NONSTDC_NAMES=1 /Fe"%V_BOOTSTRAP%" "%V_C_FILE%" kernel32.lib user32.lib dbghelp.lib ws2_32.lib bcrypt.lib advapi32.lib /link /STACK:33554432
set msvc_bootstrap_error=!ERRORLEVEL!
call :try_delete "%ObjFile%"
exit /b !msvc_bootstrap_error!

:build_bootstrap_with_clang
call :resolve_executable clang
if [!resolved_exe!] == [] (
	echo  ^> Clang not found
	exit /b 1
)
set "clang_exe=!resolved_exe!"
if "%PROCESSOR_ARCHITECTURE%" == "x86" ( set clang_target=i686-w64-mingw32 ) else ( set clang_target=x86_64-w64-mingw32 )
echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with Clang
"!clang_exe!" %VC_BOOTSTRAP_DEFINE% --target=!clang_target! -std=c99 -municode -g -w -Wno-error=implicit-function-declaration -Wno-error=incompatible-function-pointer-types -o "%V_BOOTSTRAP%" "%V_C_FILE%" -ladvapi32 -lws2_32 -lbcrypt -Wl,-stack=33554432
if !ERRORLEVEL! NEQ 0 (
	echo In most cases, compile errors happen because the version of Clang installed is too old
	"!clang_exe!" --version
	exit /b 1
)
exit /b 0

:generate_stage_c
echo  ^> Generating "%V_STAGE_C%" with "%V_BOOTSTRAP%"
REM Older vc snapshots have broken process-spawn output pointers. Emit C without
REM spawning a compiler, then let this batch script link the fresh stage instead.
REM Match the stage C dialect and avoid requiring an unlinked GC library.
"%V_BOOTSTRAP%" %V_BOOTSTRAP_VFLAGS% -gc none -g !stage_vflags! -o "%V_STAGE_C%" cmd/v
set stage_error=!ERRORLEVEL!
if !stage_error! NEQ 0 call :try_delete "%V_STAGE_C%"
exit /b !stage_error!

:build_stage_with_tcc
set stage_vflags=-cc "!tcc_exe!"
call :generate_stage_c
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
echo  ^> Compiling "%V_STAGE%" from "%V_STAGE_C%" with TCC
"!tcc_exe!" -B"%tcc_dir%" -bt10 -g -w -o "%V_STAGE%" "%V_STAGE_C%" -ldbghelp -lws2_32 -L"%~dp0vlib\crypto\rand\internal\libraries\bcrypt" -lbcrypt -I "%~dp0thirdparty\stdatomic\win" -ladvapi32 -Wl,-stack=33554432
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE_C%"
exit /b !stage_error!

:build_stage_with_gcc
set stage_vflags=-cc "!gcc_exe!"
call :generate_stage_c
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
echo  ^> Compiling "%V_STAGE%" from "%V_STAGE_C%" with GCC
"!gcc_exe!" -std=gnu11 -municode -g -w -fwrapv -o "%V_STAGE%" "%V_STAGE_C%" -ldbghelp -lws2_32 -L"%~dp0vlib\crypto\rand\internal\libraries\bcrypt" -lbcrypt -I "%~dp0thirdparty\stdatomic\win" -ladvapi32 -Wl,--stack=33554432
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE_C%"
exit /b !stage_error!

:build_stage_with_msvc
set stage_vflags=-cc msvc
call :generate_stage_c
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
echo  ^> Compiling "%V_STAGE%" from "%V_STAGE_C%" with MSVC
cl /nologo /volatile:ms /bigobj /MD /utf-8 /w /std:c11 /D_CRT_DECLARE_NONSTDC_NAMES=1 /Fe"%V_STAGE%" "%V_STAGE_C%" kernel32.lib user32.lib dbghelp.lib ws2_32.lib bcrypt.lib advapi32.lib /link /STACK:33554432
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE_C%"
call :try_delete "v_stage.obj"
exit /b !stage_error!

:build_stage_with_clang
set stage_vflags=-cc "!clang_exe!" -cflags "--target=!clang_target!"
call :generate_stage_c
if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
echo  ^> Compiling "%V_STAGE%" from "%V_STAGE_C%" with Clang
"!clang_exe!" --target=!clang_target! -std=gnu11 -municode -g -w -fwrapv -Wno-int-conversion -o "%V_STAGE%" "%V_STAGE_C%" -ldbghelp -lws2_32 -L"%~dp0vlib\crypto\rand\internal\libraries\bcrypt" -lbcrypt -I "%~dp0thirdparty\stdatomic\win" -ladvapi32 -Wl,--stack=33554432
set stage_error=!ERRORLEVEL!
call :try_delete "%V_STAGE_C%"
exit /b !stage_error!

:build_bootstrap_with_gcc
call :find_gcc_exe
if !ERRORLEVEL! NEQ 0 (
	echo  ^> GCC not found
	exit /b 1
)
echo  ^> Attempting to build "%V_BOOTSTRAP%" (from %V_C_FILE%) with GCC "!gcc_exe!"
"!gcc_exe!" %VC_BOOTSTRAP_DEFINE% -std=c99 -municode -g -w -o "%V_BOOTSTRAP%" "%V_C_FILE%" -ladvapi32 -lws2_32 -lbcrypt -Wl,-stack=33554432
if !ERRORLEVEL! NEQ 0 (
	echo In most cases, compile errors happen because the version of GCC installed is too old
	"!gcc_exe!" --version
	exit /b 1
)
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

:try_delete
REM Best-effort delete of a just-executed intermediate compiler binary.
REM Windows (or a real-time antivirus scanner) can briefly hold a lock on an
REM exe right after the process running it exits, so retry a couple of times
REM with short waits before giving up silently - a leftover intermediate
REM binary here does not affect build success either way, and the noisy
REM native "Access is denied." message is not worth surfacing to the user.
REM
REM CONTRACT for callers: this always `exit /b 0`, regardless of whether the
REM delete actually succeeded. Every call site MUST capture the real
REM compile/link step's !ERRORLEVEL! into a named variable BEFORE calling
REM this (see the "set msvc_error=!ERRORLEVEL!" / "set stage_error=!ERRORLEVEL!"
REM pattern at its call sites) and check that saved variable afterward, never
REM !ERRORLEVEL! directly after a `call :try_delete` - this always succeeds
REM and would silently swallow a real failure otherwise.
REM
REM cmd/tools/makev_test.py exercises stage generation/link failures and cleanup
REM through cmd.exe on Windows. Compiler discovery and MSVC installation setup
REM still require validation with real Windows toolchains.
if not exist "%~1" exit /b 0
del "%~1" >nul 2>&1
if not exist "%~1" exit /b 0
ping 192.0.2.1 -n 1 -w 100 >nul
del "%~1" >nul 2>&1
if not exist "%~1" exit /b 0
ping 192.0.2.1 -n 1 -w 250 >nul
del "%~1" >nul 2>&1
exit /b 0

:move_updated_to_v
REM The V1 compatibility compiler is installed on demand by `make v1`.
REM Compiling cmd/v again would only produce another copy of the new driver.
if not exist "%V_UPDATED%" exit /b 1
@REM del "%V_EXE%" &:: breaks if `makev.bat` is run from `v up` b/c of held file handle on `%V_EXE%`
if exist "%V_EXE%" (
	move /Y "%V_EXE%" "%V_OLD%" >nul
	if !ERRORLEVEL! NEQ 0 exit /b !ERRORLEVEL!
)
REM sleep for at most 100ms
ping 192.0.2.1 -n 1 -w 100 >nul
move /Y "%V_UPDATED%" "%V_EXE%" >nul
exit /b !ERRORLEVEL!
