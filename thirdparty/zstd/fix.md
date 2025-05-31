The changes here are produced by modifying `thirdparty/zstd/zstd_v.patch`,
and then running `v thirdparty/zstd/update.vsh` .

You can generate a new patch by:
`diff -u zstd-1.5.7/build/single_file_libs/zstd.c thirdparty/zstd/zstd.c > zstd_custom.patch`

BTW, patch is between `/* >> v_patch start */` and `/* << v_patch end */` mostly.
