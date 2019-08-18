git clone --depth 1 --quiet https://github.com/vlang/vc
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o v2.exe vc/v_win.c
v2.exe -o v.exe compiler
del v2.exe
rd /s /q vc
