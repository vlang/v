curl -O https://raw.githubusercontent.com/vlang/vc/master/v.c
gcc -std=gnu11 -DUNICODE -D_UNICODE -w -o vc.exe v.c
del v.c
vc.exe -o v.exe compiler
for /r . %%x in (*_test.v) do @v -o test.exe -debug %%x
