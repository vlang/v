module builtin

// <execinfo.h>
fn C.backtrace(a &voidptr, size int) int
fn C.backtrace_symbols(a &voidptr, size int) &&char
fn C.backtrace_symbols_fd(a &voidptr, size int, fd int)
