module arm64

// zeroed_table_alloc returns zeroed memory for the code generation tables.
fn zeroed_table_alloc(size int) voidptr {
	data := C.calloc(1, size)
	if isnil(data) {
		panic('arm64: could not allocate ${size} bytes for code generation tables')
	}
	return data
}

// free_zeroed_table releases a table from zeroed_table_alloc.
fn free_zeroed_table(data voidptr, size int) {
	if size > 0 {
		C.free(data)
	}
}
