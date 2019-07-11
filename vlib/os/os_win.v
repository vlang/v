module os

#flag -lws2_32
#include <winsock2.h>

const (
	PathSeparator = '\\' 
) 

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types
// A handle to an object.
type HANDLE voidptr

// Ref - https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/get-osfhandle?view=vs-2019
// get_file_handle retrieves the operating-system file handle that is associated with the specified file descriptor.
pub fn get_file_handle(path string) HANDLE {
    mode := 'rb'
    _fd := C._wfopen(path.to_wide(), mode.to_wide())
    if _fd == 0 {
	    return HANDLE(INVALID_HANDLE_VALUE)
    }
    _handle := C._get_osfhandle(C._fileno(_fd)) // CreateFile? - hah, no -_-
    return _handle
}

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-getmodulefilenamea
// get_module_filename retrieves the fully qualified path for the file that contains the specified module. 
// The module must have been loaded by the current process.
pub fn get_module_filename(handle HANDLE) ?string {
    mut sz := int(4096) // Optimized length 
    mut buf := &u16(malloc(4096))
    for {
        status := C.GetModuleFileName(handle, &buf, sz)
        switch status {
        case SUCCESS:
            _filename := string_from_wide2(buf, sz)
            return _filename
        default:
            // Must handled with GetLastError and converted by FormatMessage
            return error('Cannot get file name from handle.')
        }
    }
}

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-formatmessagea#parameters
const (
    FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100
    FORMAT_MESSAGE_ARGUMENT_ARRAY  = 0x00002000
    FORMAT_MESSAGE_FROM_HMODULE    = 0x00000800
    FORMAT_MESSAGE_FROM_STRING     = 0x00000400
    FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000
    FORMAT_MESSAGE_IGNORE_INSERTS  = 0x00000200
)

// Ref - winnt.h
const (
    SUBLANG_NEUTRAL = 0x00
    SUBLANG_DEFAULT = 0x01
    LANG_NEUTRAL    = (SUBLANG_NEUTRAL)
)   

// Ref - https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes--12000-15999-
const (
    MAX_ERROR_CODE  = 15841 // ERROR_API_UNAVAILABLE
)

// ptr_win_get_error_msg return string (voidptr) 
// representation of error, only for windows. 
fn ptr_win_get_error_msg(code u32) voidptr {
    mut buf := voidptr(0)
    // Check for code overflow
    if code > u32(MAX_ERROR_CODE) {
        return buf
    }
    C.FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER
		| FORMAT_MESSAGE_FROM_SYSTEM
		| FORMAT_MESSAGE_IGNORE_INSERTS,
        0, code, C.MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), &buf, 0, 0)
    return buf
}

// get_error_msg return error code representation in string.
pub fn get_error_msg(code int) string {
    if code < 0 { // skip negative
        return ''
    }
    _ptr_text := ptr_win_get_error_msg(u32(code))
    if _ptr_text == 0 { // compare with null
        return ''
    }
    return tos(_ptr_text, C.strlen(_ptr_text))
}