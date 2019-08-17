curl -O https://raw.githubusercontent.com/vlang/vc/master/v_win.c
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o v2.exe v_win.c
v2.exe -o v.exe compiler
del v2.exe
del v_win.c
