@echo off
setlocal EnableDelayedExpansion
REM Find every openssl.exe reachable through PATH:
for /f "delims=" %%F in ('where openssl 2^>nul') do (
    set "OPENSSL_DIR=%%~dpF"
    echo OpenSSL lives in: "!OPENSSL_DIR!"
    goto :end
)
echo OpenSSL not found in: "!PATH!"
:end
