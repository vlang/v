module os

#flag -lws2_32
#include <winsock2.h>

const (
	PathSeparator = '\\'
)

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types
// A handle to an object.
type HANDLE voidptr

// win: FILETIME
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-filetime
struct Filetime {
  dwLowDateTime u32
  dwHighDateTime u32
}

// win: WIN32_FIND_DATA
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-_win32_find_dataw
struct Win32finddata {
mut:
    dwFileAttributes u32
    ftCreationTime Filetime
  	ftLastAccessTime Filetime
  	ftLastWriteTime Filetime
	nFileSizeHigh u32
	nFileSizeLow u32
	dwReserved0 u32
	dwReserved1 u32
	cFileName [260]u16 // MAX_PATH = 260
	cAlternateFileName [14]u16 // 14
  	dwFileType u32
  	dwCreatorType u32
  	wFinderFlags u16
}


fn init_os_args(argc int, argv &byteptr) []string {
	mut args := []string
	mut args_list := &voidptr(0)
	mut args_count := 0
	args_list = C.CommandLineToArgvW(C.GetCommandLine(), &args_count)
	for i := 0; i < args_count; i++ {
		args << string_from_wide(&u16(args_list[i]))
	}
	C.LocalFree(args_list)
	return args
}


pub fn ls(path string) []string {
	mut find_file_data := Win32finddata{}
	mut dir_files := []string
	// We can also check if the handle is valid. but using dir_exists instead
	// h_find_dir := C.FindFirstFile(path.str, &find_file_data)
	// if (INVALID_HANDLE_VALUE == h_find_dir) {
	//     return dir_files
	// }
	// C.FindClose(h_find_dir)
	if !dir_exists(path) {
		println('ls() couldnt open dir "$path" (does not exist).')
		return dir_files
	}
	// NOTE: Should eventually have path struct & os dependant path seperator (eg os.PATH_SEPERATOR)
	// we need to add files to path eg. c:\windows\*.dll or :\windows\*
	path_files := '$path\\*'
	// NOTE:TODO: once we have a way to convert utf16 wide character to utf8
	// we should use FindFirstFileW and FindNextFileW
	h_find_files := C.FindFirstFile(path_files.to_wide(), &find_file_data)
	first_filename := string_from_wide(&u16(find_file_data.cFileName))
	if first_filename != '.' && first_filename != '..' {
		dir_files << first_filename
	}
	for C.FindNextFile(h_find_files, &find_file_data) {
		filename := string_from_wide(&u16(find_file_data.cFileName))
		if filename != '.' && filename != '..' {
			dir_files << filename.clone()
		}
	}
	C.FindClose(h_find_files)
	return dir_files
}

pub fn dir_exists(path string) bool {
	_path := path.replace('/', '\\')
	attr := int(C.GetFileAttributes(_path.to_wide()))
	if attr == C.INVALID_FILE_ATTRIBUTES {
		return false
	}
	if (attr & C.FILE_ATTRIBUTE_DIRECTORY) != 0 {
		return true
	}
	return false
}

// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) {
	_path := path.replace('/', '\\')
	// Windows doesnt recursively create the folders
	// so we need to help it out here
	if _path.last_index('\\') != -1 {
		mkdir(_path.all_before_last('\\'))
	}
	C.CreateDirectory(_path.to_wide(), 0)
}

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
            return error('Cannot get file name from handle')
        }
    }
    panic('this should be unreachable') // TODO remove unreachable after loop
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
    return tos(_ptr_text, vstrlen(_ptr_text))
}
