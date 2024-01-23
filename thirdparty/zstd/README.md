Need patch zstd.c:
#if defined(__TINYC__) && defined(_WIN32)
#undef ZSTD_MULTITHREAD
#define ZSTD_NO_INTRINSICS
#endif
