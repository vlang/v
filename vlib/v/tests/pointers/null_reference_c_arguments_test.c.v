// C function parameters accept null pointer constants for C interoperability.
fn test_c_function_arguments() {
	// `fflush(NULL)` flushes every open output stream.
	assert C.fflush(voidptr(0)) == 0
	assert C.fflush(0) == 0
	assert C.fflush(unsafe { nil }) == 0
}
