module builtin

[typedef]
struct C.IError {
	_object voidptr
}

[unsafe]
pub fn (ie &IError) free() {
	unsafe {
		cie := &C.IError(ie)
		free(cie._object)
	}
}
