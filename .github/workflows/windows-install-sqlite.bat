@echo off

curl -L https://www.sqlite.org/2022/sqlite-amalgamation-3380200.zip -o sqlite-amalgamation-3380200.zip

unzip sqlite-amalgamation-3380200.zip -d thirdparty\

del thirdparty\sqlite-amalgamation-3380200\shell.c

move /y thirdparty\sqlite-amalgamation-3380200 thirdparty\sqlite

dir thirdparty\sqlite
