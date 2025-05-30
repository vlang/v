module zstd

// As MUSL has no `qsort_r()` as GLIBC, so fallback to `qsort()`
#flag -DZSTD_USE_C90_QSORT
