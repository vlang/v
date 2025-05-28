1. replace all `abs64` with `zstd_abs64`.
2. add following at header of the file:
#if defined(__TINYC__) && defined(_WIN32)
#undef ZSTD_MULTITHREAD
#define ZSTD_NO_INTRINSICS
#endif

