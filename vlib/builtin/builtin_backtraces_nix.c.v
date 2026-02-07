module builtin

// <execinfo.h>
fn C.backtrace(a &voidptr, size i32) i32
fn C.backtrace_symbols(a &voidptr, size i32) &&char
fn C.backtrace_symbols_fd(a &voidptr, size i32, fd i32)
