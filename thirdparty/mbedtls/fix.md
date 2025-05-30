1. Get `mbedtls` from https://github.com/Mbed-TLS/mbedtls/releases
2. Download and extract `mbedtls`, and copy files to `thirdparty/mbedtls/`:
```
rsync -a --delete mbedtls-3.6.3.1/include/  thirdparty/mbedtls/include/
rsync -a --delete mbedtls-3.6.3.1/library/  thirdparty/mbedtls/library/
rsync -a --delete mbedtls-3.6.3.1/3rdparty/ thirdparty/mbedtls/3rdparty/
rsync -a          mbedtls-3.6.3.1/LICENSE   thirdparty/mbedtls/LICENSE
rsync -a          mbedtls-3.6.3.1/README.md thirdparty/mbedtls/README.md
find thirdparty/mbedtls/ -name '*.txt' -or -name '*.inc' -or -name '.gitignore' -or -name 'Makefile' | xargs rm -f
```
3. In `thirdparty/mbedtls/`, apply patch by `patch -p1 < ./mbedtls.patch`

BTW, you can generate a new patch by `diff -Naur mbedtls.orig mbedtls > mbedtls.patch`
