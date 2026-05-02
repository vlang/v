REM This file should be run from the top folder of the V compiler itself.
REM
REM Historically this invoked `v vlib/db/sqlite/install_thirdparty_sqlite.vsh`,
REM which compiled and ran a V script to download the SQLite amalgamation. On
REM tcc-windows CI that .vsh compile reliably crashes with EXCEPTION_STACK_OVERFLOW
REM (0xc00000fd) before the download starts, blocking the entire job. The native
REM PowerShell implementation does the same work without invoking the V compiler,
REM and is faster on every Windows CI variant since it avoids a full V compile.

pwsh.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0windows-install-sqlite.ps1"
