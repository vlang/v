module os

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types
// A handle to an object.
/* 
type HANDLE voidptr // C.HANDLE

pub fn get_file_handle(path string) HANDLE {
    mode := 'rb'
    _fh := C.fopen(path.cstr(), mode.cstr())
    if isnil(_fh) {
	      return HANDLE(INVALID_HANDLE_VALUE)
    }
    _handle := C._get_osfhandle(C._fileno(_fh)) // CreateFile? - hah, no -_-
    return _handle
}
*/ 
