module builtin

// Just define the C functions, so that V does not error because of the missing definitions.

// NB: they will NOT be used, since calls to them are wrapped with `$if gcboehm ? { }`

fn C.GC_MALLOC(n size_t) voidptr

fn C.GC_REALLOC(ptr voidptr, n size_t) voidptr

fn C.GC_FREE(ptr voidptr)
