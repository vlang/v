curl -O https://raw.githubusercontent.com/vlang/vc/master/v.c
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o v.exe v.c
del v.c
