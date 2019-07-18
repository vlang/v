module os 

#include <dirent.h>

const (
	PathSeparator = '/' 
)

// get_error_msg return error code representation in string.
pub fn get_error_msg(code int) string {
	_ptr_text := C.strerror(code) // voidptr?
	if _ptr_text == 0 {
		return ''
	}
	return tos(_ptr_text, C.strlen(_ptr_text))
}