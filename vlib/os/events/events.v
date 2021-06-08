module events

// TODO: this function was copied from os module to prevent cyclical
// imports. refactor to a utils module
// posix_get_error_msg return error code representation in string.
fn posix_get_error_msg(code int) string {
	ptr_text := C.strerror(code) // voidptr?
	if ptr_text == 0 {
		return ''
	}
	return unsafe { tos3(ptr_text) }
}
