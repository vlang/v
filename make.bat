curl -O https://raw.githubusercontent.com/vlang/vc/master/v.c
gcc -std=gnu11 -w -o v.exe v.c -lws2_32
del v.c
