@echo off
curl -O https://raw.githubusercontent.com/vlang/vc/master/v.c
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o vc.exe v.c
del v.c
vc.exe -o v.exe compiler
setlocal EnableDelayedExpansion
for /r . %%x in (*_test.v) do (
  v -o test.exe -debug %%x
  if !ERRORLEVEL! NEQ 0 goto :fail
)
goto :done

:fail
exit /b 1

:done
