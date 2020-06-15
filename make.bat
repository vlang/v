@echo off

echo Building V

set tcc_path=%~dp0thirdparty\tcc\
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
REM option to disable adding V to PATH
if "%~1"=="-skip-path" set skip_path=1
if "%~2"=="-skip-path" set skip_path=1

REM option to force msvc, gcc or tcc
if "%~1"=="-gcc"  set force_gcc=1  & goto :gcc_strap
if "%~2"=="-gcc"  set force_gcc=1  & goto :gcc_strap
if "%~1"=="-msvc" set force_msvc=1 & goto :msvc_strap
if "%~2"=="-msvc" set force_msvc=1 & goto :msvc_strap
if "%~1"=="-tcc"  set force_tcc=1  & goto :tcc_strap
if "%~2"=="-tcc"  set force_tcc=1  & goto :tcc_strap

:gcc_strap
echo.
echo Attempting to build v.c with GCC...

where /q gcc
if %ERRORLEVEL% NEQ 0 (
	echo  ^> GCC not found
	if "%force_gcc%" NEQ "" goto :error
	goto :msvc_strap
)

gcc -std=c99 -municode -w -o v.exe vc\v_win.c
if %ERRORLEVEL% NEQ 0 goto :compile_error

v.exe self > NUL
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

cl.exe /nologo /w /volatile:ms /Fo%ObjFile% /O2 /MD /D_VBOOTSTRAP vc\v_win.c user32.lib kernel32.lib advapi32.lib shell32.lib /link /NOLOGO /OUT:v.exe /INCREMENTAL:NO > NUL
if %ERRORLEVEL% NEQ 0 goto :compile_error

v.exe -cc msvc self
del %ObjFile%
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:clone_tcc
git clone --depth 1 --quiet https://github.com/vlang/tccbin_win %tcc_path%
set cloned_tcc=1
goto :tcc_strap

:tcc_strap
echo.
echo Attempting to build v.c with TCC...

where /q tcc
if %ERRORLEVEL% NEQ 0 (
	if exist "%tcc_path%" (
		set tcc_exe=%tcc_path%tcc.exe
	) else if "%cloned_tcc%"=="" (
		echo  ^> TCC not found
		echo  ^> Downloading TCC from https://github.com/vlang/tccbin_win
		goto :clone_tcc
	) else (
		echo  ^> TCC not found, even after cloning
		goto :error
	)
) else (
	for /f "delims=" %%i in ('where tcc') do set tcc_exe=%%i
)

if exist "%tcc_path%" (
	if "%cloned_tcc%"=="" (
		echo  ^> Updating prebuilt TCC...
		pushd "%tcc_path%"
		git pull -q > NUL
		popd
	)
)
call "%tcc_exe%" -std=c99 -municode -lws2_32 -lshell32 -ladvapi32 -bt10 -w -o v.exe vc\v_win.c
if %ERRORLEVEL% NEQ 0 goto :compile_error

v.exe -cc "%tcc_exe%" self > NUL
if %ERRORLEVEL% NEQ 0 goto :compile_error
goto :success

:compile_error
echo Failed to compile - Create an issue at 'https://github.com/vlang'
goto :error

:error
echo.
echo Exiting from error
popd
exit /b 1

:success
echo  ^> V built successfully!
del v_old.exe

:path
if "%skip_path%" NEQ "" goto :version
echo.
echo Adding V to PATH...
v.exe symlink > NUL
if %ERRORLEVEL% NEQ 0 (
	echo  ^> Could not add V to %%PATH%%, try rebuilding as admin.
	goto :error
)
echo  ^> V added to %%PATH%%

if "%cloned_tcc%" NEQ "" (
	echo @echo off> "%~dp0.bin\tcc.bat"
	echo %tcc_path%tcc %%^*>> "%~dp0.bin\tcc.bat"
	echo  ^> TCC added to %%PATH%%
)

echo  ^> Restart your shell/IDE to reload it

:version
echo.
echo | set /p="V version: "
v.exe version
if "%cloned_tcc%" NEQ "" (
	echo.
	echo WARNING:  No C compiler was detected in your PATH. `tcc` was used temporarily
	echo           to build V, but it may have some bugs and may not work in all cases.
	echo           A more advanced C compiler like GCC or MSVC is recommended.
	echo           https://github.com/vlang/v/wiki/Installing-a-C-compiler-on-Windows
	echo.
)

popd
