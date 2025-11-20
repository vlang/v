// vtest build: !sanitize-memory-gcc && !sanitize-address-gcc && !sanitize-address-clang
// vtest vflags: -autofree
import os

fn test_os_args_no_double_free() {
	args := os.args
	assert args.len > 0
}

fn test_os_args_clone() {
	a1 := os.args
	a2 := os.args
	assert a1 == a2
}
