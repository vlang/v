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

const(
	INVALID_HANDLE_VALUE = -1
)

// FILETIME
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-filetime
struct filetime {
  dwLowDateTime u32
  dwHighDateTime u32
}

// WIN32_FIND_DATA
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-_win32_find_dataa
struct win32finddata {
mut:
    dwFileAttributes u32
    ftCreationTime filetime
  	ftLastAccessTime filetime
  	ftLastWriteTime filetime
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

pub fn ls(path string) []string {
    mut find_file_data := win32finddata{}
    mut dir_files := []string

    // We can also check if the handle is valid. but using dir_exists instead
    // h_find_dir := C.FindFirstFile(path.cstr(), &find_file_data)
    // if (INVALID_HANDLE_VALUE == h_find_dir) {
    //     return dir_files
    // }
    // C.FindClose(h_find_dir)
    if !dir_exists(path) {
        println('ls() couldnt open dir "$path" (does not exist).')
        return dir_files
    }
    // NOTE: Should eventually have path struct & os dependant path seperator (eg os.PATH_SEPERATOR)
    // we need to add files (*) to path eg. c:\windows\*.dll or c:\windows\*
    path_files := '$path\\*' 
    // NOTE:TODO: once we have a way to convert utf16 wide character to utf8
    // we should use FindFirstFileW and FindNextFileW
    h_find_files := C.FindFirstFile(path_files.cstr(), &find_file_data)
    first_filename := tos(&find_file_data.cFileName, strlen(find_file_data.cFileName))
    if first_filename != '.' && first_filename != '..' {
        dir_files << first_filename
    }
    for C.FindNextFile(h_find_files, &find_file_data) {
        filename := tos(&find_file_data.cFileName, strlen(find_file_data.cFileName))
        if filename != '.' && filename != '..' {
            dir_files << filename.clone()
        }
    }
    C.FindClose(h_find_files)
    return dir_files
}