REM This file should be run from the top folder of the V compiler itself.

python .github\workflows\windows-install-sqlite.py
exit /b %ERRORLEVEL%
