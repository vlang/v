1. replace all `abs64` with `zstd_abs64`.

2. add following at header of the file:
#if defined(__TINYC__) && defined(_WIN32)
#undef ZSTD_MULTITHREAD
#define ZSTD_NO_INTRINSICS
#endif

3. add !defined(__MUSL__) , it will use `qsort` instead of `qsort_r`
