git clone --depth 1 --quiet https://github.com/vlang/vc
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o v1.exe vc/v_win.c
v1.exe -o v2.exe compiler
v2.exe -prod -o v.exe compiler
del v1.exe
del v2.exe
rd /s /q vc
