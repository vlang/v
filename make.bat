@echo off

echo Building V

REM default tcc
set tcc_url=https://github.com/vlang/tccbin_win
set tcc_dir="%~dp0thirdparty\tcc"

REM let a particular environment specify their own tcc
if "%TCC_GIT%" =="" goto :init
set tcc_url="%TCC_GIT%"

:init
REM initialize the log file with the failure message
set log_file=%TEMP%\v_make.bat.log
echo Failed to compile - Create an issue at 'https://github.com/vlang/v' with the following info:>%log_file%
echo.>>%log_file%

REM alleviate weird issues with this var
set cloned_tcc=
set ERRORLEVEL=

pushd %~dp0

if "%~1"=="-local" goto :compile
if "%~2"=="-local" goto :compile

if exist "vc" (
	echo Updating vc...
	cd vc
	git pull --quiet
	cd ..
) else (
	echo Cloning vc...
	git clone --depth 1 --quiet https://github.com/vlang/vc
)

:compile
REM option to force msvc, gcc or tcc
if "%~1"=="-gcc"        set force_gcc=1  & goto :gcc_strap
if "%~2"=="-gcc"        set force_gcc=1  & goto :gcc_strap
if "%~1"=="-msvc"       set force_msvc=1 & goto :msvc_strap
if "%~2"=="-msvc"       set force_msvc=1 & goto :msvc_strap
if "%~1"=="-tcc"        set force_tcc=1  & goto :tcc_strap
if "%~2"=="-tcc"        set force_tcc=1  & goto :tcc_strap
if "%~1"=="-fresh_tcc"  set force_tcc=1  & goto :fresh_tcc
if "%~2"=="-fresh_tcc"  set force_tcc=1  & goto :fresh_tcc

:gcc_strap
echo.
echo Attempting to build v.c with GCC...

where /q gcc
if %ERRORLEVEL% NEQ 0 (
	echo  ^> GCC not found
	if "%force_gcc%" NEQ "" goto :error
	goto :msvc_strap
)

gcc -std=c99 -municode -w -o v.exe .\vc\v_win.c>>%log_file% 2>>&1
if %ERRORLEVEL% NEQ 0 (
	rem In most cases, compile errors happen because the version of GCC installed is too old
	gcc --version>>%log_file% 2>>&1
	goto :compile_error
)

echo  ^> Compiling with .\v.exe self
v.exe self>>%log_file% 2>>&1
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:msvc_strap
echo.
echo Attempting to build v.c with MSVC...
set VsWhereDir=%ProgramFiles(x86)%
set HostArch=x64
if "%PROCESSOR_ARCHITECTURE%" == "x86" (
	echo Using x86 Build Tools...
	set VsWhereDir=%ProgramFiles%
	set HostArch=x86
)

if not exist "%VsWhereDir%\Microsoft Visual Studio\Installer\vswhere.exe" (
	echo  ^> MSVC not found
	if "%force_msvc%" NEQ "" goto :error
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

cl.exe /volatile:ms /Fo%ObjFile% /O2 /MD /D_VBOOTSTRAP vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /nologo /out:v.exe /incremental:no>>%log_file% 2>>&1
if %ERRORLEVEL% NEQ 0 goto :compile_error

echo  ^> Compiling with .\v.exe self
v.exe -cc msvc self>>%log_file% 2>>&1
del %ObjFile%>>%log_file% 2>>&1
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:fresh_tcc
rd /s /q "%tcc_dir%"

:clone_tcc
git clone --depth 1 --quiet "%tcc_url%" "%tcc_dir%"
set cloned_tcc=1
goto :tcc_strap

:tcc_strap
echo.
echo Attempting to build v.c with TCC...

where /q tcc
if %ERRORLEVEL% NEQ 0 (
	if exist "%tcc_dir%" (
		set tcc_exe="%tcc_dir%\tcc.exe"
	) else if "%cloned_tcc%"=="" (
		echo  ^> TCC not found
		echo  ^> Downloading TCC from %tcc_url%
		goto :clone_tcc
	) else (
		echo  ^> TCC not found, even after cloning %cloned_tcc%
		goto :error
	)
) else (
	for /f "delims=" %%i in ('where tcc') do set tcc_exe=%%i
)

if exist "%tcc_dir%" (
	if "%cloned_tcc%"=="" (
		echo  ^> Updating prebuilt TCC...
		pushd "%tcc_dir%"\
		git pull -q
		popd
	)
)
"%tcc_exe%" -std=c99 -municode -lws2_32 -lshell32 -ladvapi32 -bt10 -w -o v.exe vc\v_win.c
if %ERRORLEVEL% NEQ 0 goto :compile_error

echo  ^> Compiling with .\v.exe self
v.exe -cc "%tcc_exe%" self>>%log_file% 2>>&1
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:compile_error
echo.
echo.
type %log_file%
del %log_file%
goto :error

:error
echo.
echo Exiting from error
popd
exit /b 1

:success
echo  ^> V built successfully!
echo  ^> To add V to your PATH, run `.\v.exe symlink`.
del v_old.exe >>%log_file% 2>>&1
del %log_file%

:version
echo.
echo | set /p="V version: "
.\v.exe version
if "%cloned_tcc%" NEQ "" (
	if "%force_tcc%" == "" (
		echo.
		echo WARNING:  No C compiler was detected in your PATH. `tcc` was used temporarily
		echo           to build V, but it may have some bugs and may not work in all cases.
		echo           A more advanced C compiler like GCC or MSVC is recommended.
		echo           https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows
		echo.
	)
)

popd
