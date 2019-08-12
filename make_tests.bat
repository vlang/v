@echo off

echo Cleanup
del v.exe
del v.c
del v2.exe

echo fetch v.c
curl -O https://raw.githubusercontent.com/vlang/vc/master/v.c
if %ERRORLEVEL% NEQ 0 goto :fail

echo build vc using gcc
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o vc.exe v.c
del v.c
if %ERRORLEVEL% NEQ 0 goto :fail

echo build v using vc
vc.exe -o v.exe compiler
if %ERRORLEVEL% NEQ 0 goto :fail

echo build vc.msvc using vc
vc.exe -o -os msvc v.msvc.exe compiler
if %ERRORLEVEL% NEQ 0 goto :fail

echo build v.msvc.3 using v
v.exe -os msvc -o v.msvc.2.exe compiler
if %ERRORLEVEL% NEQ 0 goto :fail

echo build v.msvc.3 using v.msvc
v.msvc.exe -os msvc -o v.msvc.3.exe compiler
if %ERRORLEVEL% NEQ 0 goto :fail

echo build v.gcc using v.msvc
v.msvc.exe -o v.gcc.exe compiler
if %ERRORLEVEL% NEQ 0 goto :fail

setlocal EnableDelayedExpansion
echo testing v
for /r . %%x in (*_test.v) do (
  v -o test.exe -debug %%x
  if !ERRORLEVEL! NEQ 0 goto :fail
)
echo testing v.msvc
for /r . %%x in (*_test.v) do (
  v.msvc.exe -o test.exe -debug %%x
  if !ERRORLEVEL! NEQ 0 goto :fail
)

echo testing v -os msvc
for /r . %%x in (*_test.v) do (
  v -os msvc -o test.exe -debug %%x
  if !ERRORLEVEL! NEQ 0 goto :fail
)

echo testing v.msvc -os msvc
for /r . %%x in (*_test.v) do (
  v.msvc.exe -os msvc -o test.exe -debug %%x
  if !ERRORLEVEL! NEQ 0 goto :fail
)

goto :done

:fail
echo fail
exit /b 1

:done
echo pass
