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

4. fix compilation with tcc on OpenBSD
  * disable prefetch (define `NO_PREFTECH`)
  * fix for `DONT_VECTORIZE` define
  * don't use ASM in `ZSTD_compressBlock_lazy_generic` and `ZSTD_decompressSequences_body` functions.
```c
/* Disable prefetch on OpenBSD with tcc */
#if defined(__OpenBSD__) && defined(__TINYC__)
#define NO_PREFETCH
#endif
(...)
/* vectorization
 * older GCC (pre gcc-4.3 picked as the cutoff) uses a different syntax,
 * and some compilers, like Intel ICC and MCST LCC, do not support it at all.
 */
#if !(defined(__OpenBSD__) && defined(__TINYC__)) && !defined(__INTEL_COMPILER) && !defined(__clang__) && defined(__GNUC__) && !defined(__LCC__)
#  if (__GNUC__ == 4 && __GNUC_MINOR__ > 3) || (__GNUC__ >= 5)
#    define DONT_VECTORIZE __attribute__((optimize("no-tree-vectorize")))
#  else
#    define DONT_VECTORIZE _Pragma("GCC optimize(\"no-tree-vectorize\")")
#  endif
#else
#  define DONT_VECTORIZE
#endif
(...)
/* Match Loop */
#if !(defined(__OpenBSD__) && defined(__TINYC__)) && defined(__GNUC__) && defined(__x86_64__)
      /* I've measured random a 5% speed loss on levels 5 & 6 (greedy) when the
       * code alignment is perturbed. To fix the instability align the loop on 32-bytes.
       */
      __asm__(".p2align 5");
#endif
(...)
#if !(defined(__OpenBSD__) && defined(__TINYC__)) && defined(__GNUC__) && defined(__x86_64__)
              __asm__(".p2align 6");
              __asm__("nop");
#  if __GNUC__ >= 7
(...)
```
