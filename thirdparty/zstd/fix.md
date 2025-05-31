1. Get `zstd` from https://github.com/facebook/zstd/releases
2. Download and extract `zstd`, and goto `zstd-1.5.7/build/single_file_libs/`, run `create_single_file_library.sh`
3. Copy generated `zstd.c` to v's `thirdparty/zstd/zstd-1.5.7.c`
4. In `thirdparty/zstd/`, apply patch by `patch -p0 -i zstd_v.patch -o zstd.c`
5. Remove `zstd-1.5.7.c`

You can generate a new patch by `diff -u zstd-1.5.7.c zstd_modified.c > zstd_custom.patch`

BTW, patch is between `/* >> v_patch start */` and `/* << v_patch end */` mostly.
