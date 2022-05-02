module builtin

// <execinfo.h>
fn C.backtrace(a &voidptr, size int) int

fn C.backtrace_symbols(a &voidptr, size int) &&char

fn C.backtrace_symbols_fd(a &voidptr, size int, fd int)

// These are just dummy implementations to appease gcc on musl/alpine							

[export: 'backtrace_symbols']
[weak]
fn vbacktrace_symbols(const_buffer &voidptr, size int) &&char {
	return 0
}

[export: 'backtrace']
[weak]
fn vbacktrace(buffer &voidptr, size int) int {
	return 0
}

[export: 'backtrace_symbols_fd']
[weak]
fn vbacktrace_symbols_fd(const_buffer &voidptr, size int, fd int) {
}
