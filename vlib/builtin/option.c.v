module builtin

[typedef]
struct C.IError {
	_object voidptr
}

[unsafe]
pub fn (ie &IError) free() {
	unsafe {
		ie.msg.free()
		cie := &C.IError(ie)
		free(cie._object)
	}
}
