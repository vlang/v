module builtin

// These are just dummy implementations to appease linking on musl/alpine

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
