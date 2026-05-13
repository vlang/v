module main

#flag @VMODROOT/return_u64.c

fn C.bug_return_u64() u64

fn test_c_function_u64_return_keeps_high_bits() {
	bugval := C.bug_return_u64()
	assert bugval == u64(0xA5A5A5A5A5A5A5A5)
}
