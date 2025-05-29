1. replace all `abs64` with `zstd_abs64`.

2. add following at header of the file:
```c
#if defined(__TINYC__) && defined(_WIN32)
#undef ZSTD_MULTITHREAD
#define ZSTD_NO_INTRINSICS
#endif

#if defined(__arm__) || defined(__aarch64__)
#define NO_PREFETCH
#endif
```

3. replace `qsort_r` with `qsort`, as there is no way detect `__MUSL__` macro. add following at header of the file:
```c
#ifndef ZDICT_QSORT
# if defined(__APPLE__)
#   define ZDICT_QSORT ZDICT_QSORT_APPLE /* uses qsort_r() with a different order for parameters */
# elif defined(__GLIBC__)
#   define ZDICT_QSORT ZDICT_QSORT_GNU /* uses qsort_r() */
# elif defined(_WIN32) && defined(_MSC_VER)
#   define ZDICT_QSORT ZDICT_QSORT_MSVC /* uses qsort_s() with a different order for parameters */
# elif defined(STDC_LIB_EXT1) && (STDC_LIB_EXT1 > 0) /* C11 Annex K */
#   define ZDICT_QSORT ZDICT_QSORT_C11 /* uses qsort_s() */
# else
#   define ZDICT_QSORT ZDICT_QSORT_C90 /* uses standard qsort() which is not re-entrant (requires global variable) */
# endif
#endif
```
