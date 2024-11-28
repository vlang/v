@echo off

curl -L https://www.sqlite.org/2024/sqlite-amalgamation-3450300.zip -o sqlite-amalgamation-3450300.zip

unzip sqlite-amalgamation-3450300.zip -d thirdparty\

del thirdparty\sqlite-amalgamation-3450300\shell.c

move /y thirdparty\sqlite-amalgamation-3450300 thirdparty\sqlite

dir thirdparty\sqlite
