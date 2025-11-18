@echo off

curl -L https://sqlite.org/2025/sqlite-amalgamation-3510000.zip -o sqlite-amalgamation-3510000.zip

unzip sqlite-amalgamation-3510000.zip -d thirdparty\

del thirdparty\sqlite-amalgamation-3510000\shell.c

move /y thirdparty\sqlite-amalgamation-3510000 thirdparty\sqlite

dir thirdparty\sqlite
