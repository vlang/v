module os

import strings

#flag -lws2_32
#include <winsock2.h>

pub const (
	/**
	 * This constant is deprecated. Use `filepath.separator` instead.
	 * FIXME Remove this separator, as it a part of `filepath` module.
	 */
	path_separator = '\\'
)

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types
// A handle to an object.
pub type HANDLE voidptr

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

struct ProcessInformation {
mut:
	hProcess voidptr
	hThread voidptr
	dwProcessId u32
	dwThreadId u32
}

struct StartupInfo {
mut:
	cb u32
	lpReserved &u16
	lpDesktop &u16
	lpTitle &u16
	dwX u32
	dwY u32
	dwXSize u32
	dwYSize u32
	dwXCountChars u32
	dwYCountChars u32
	dwFillAttribute u32
	dwFlags u32
	wShowWindow u16
	cbReserved2 u16
	lpReserved2 byteptr
	hStdInput voidptr
	hStdOutput voidptr
	hStdError voidptr
}

struct SecurityAttributes {
mut:
	nLength u32
	lpSecurityDescriptor voidptr
	bInheritHandle bool
}

fn init_os_args_wide(argc int, argv &byteptr) []string {
	mut args := []string
	for i in 0..argc {
		args << string_from_wide(&u16(argv[i]))
	}
	return args
}

pub fn ls(path string) ?[]string {
	mut find_file_data := Win32finddata{}
	mut dir_files := []string
	// We can also check if the handle is valid. but using is_dir instead
	// h_find_dir := C.FindFirstFile(path.str, &find_file_data)
	// if (INVALID_HANDLE_VALUE == h_find_dir) {
	//     return dir_files
	// }
	// C.FindClose(h_find_dir)
	if !is_dir(path) {
		return error('ls() couldnt open dir "$path": directory does not exist')
	}
	// NOTE: Should eventually have path struct & os dependant path seperator (eg os.PATH_SEPERATOR)
	// we need to add files to path eg. c:\windows\*.dll or :\windows\*
	path_files := '$path\\*'
	// NOTE:TODO: once we have a way to convert utf16 wide character to utf8
	// we should use FindFirstFileW and FindNextFileW
	h_find_files := C.FindFirstFile(path_files.to_wide(), voidptr(&find_file_data))
	first_filename := string_from_wide(&u16(find_file_data.cFileName))
	if first_filename != '.' && first_filename != '..' {
		dir_files << first_filename
	}
	for C.FindNextFile(h_find_files, voidptr(&find_file_data)) {
		filename := string_from_wide(&u16(find_file_data.cFileName))
		if filename != '.' && filename != '..' {
			dir_files << filename.clone()
		}
	}
	C.FindClose(h_find_files)
	return dir_files
}

/*
pub fn is_dir(path string) bool {
	_path := path.replace('/', '\\')
	attr := C.GetFileAttributesW(_path.to_wide())
	if int(attr) == int(C.INVALID_FILE_ATTRIBUTES) {
		return false
	}
	if (int(attr) & C.FILE_ATTRIBUTE_DIRECTORY) != 0 {
		return true
	}
	return false
}
*/

pub fn open(path string) ?File {
	file := File {
		cfile: C._wfopen(path.to_wide(), 'rb'.to_wide())
		opened: true
	}
	if isnil(file.cfile) {
		return error('failed to open file "$path"')
	}
	return file
}

// create creates a file at a specified location and returns a writable `File` object.
pub fn create(path string) ?File {
	file := File {
		cfile: C._wfopen(path.to_wide(), 'wb'.to_wide())
		opened: true
	}
	if isnil(file.cfile) {
		return error('failed to create file "$path"')
	}
	return file
}

pub fn (f mut File) write(s string) {
	if !f.opened {
		return
	}
	C.fputs(s.str, f.cfile)
}

pub fn (f mut File) writeln(s string) {
	if !f.opened {
		return
	}
	// TODO perf
	C.fputs(s.str, f.cfile)
	C.fputs('\n', f.cfile)
}


// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) ?bool {
	if path == '.' { return true }
	apath := os.realpath( path )
	if !C.CreateDirectory(apath.to_wide(), 0) {
		return error('mkdir failed for "$apath", because CreateDirectory returned ' + get_error_msg(int(C.GetLastError())))
	}
	return true
}

// Ref - https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/get-osfhandle?view=vs-2019
// get_file_handle retrieves the operating-system file handle that is associated with the specified file descriptor.
pub fn get_file_handle(path string) HANDLE {
    mode := 'rb'
    _fd := C._wfopen(path.to_wide(), mode.to_wide())
    if _fd == 0 {
	    return HANDLE(INVALID_HANDLE_VALUE)
    }
    _handle := HANDLE(C._get_osfhandle(C._fileno(_fd))) // CreateFile? - hah, no -_-
    return _handle
}

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-getmodulefilenamea
// get_module_filename retrieves the fully qualified path for the file that contains the specified module.
// The module must have been loaded by the current process.
pub fn get_module_filename(handle HANDLE) ?string {
	unsafe {
		mut sz := 4096 // Optimized length
		mut buf := &u16(malloc(4096))
		for {
			status := int(C.GetModuleFileNameW(handle, voidptr(&buf), sz))
			match status {
				SUCCESS {
					_filename := string_from_wide2(buf, sz)
					return _filename
				}
				else {
					// Must handled with GetLastError and converted by FormatMessage
					return error('Cannot get file name from handle')
				}
			}
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
        0, code, C.MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), voidptr(&buf), 0, 0)
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
    return string_from_wide(_ptr_text)
}

// exec starts the specified command, waits for it to complete, and returns its output.
pub fn exec(cmd string) ?Result {
	if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
		return error(';, &&, || and \\n are not allowed in shell commands')
	}
	mut child_stdin := &u32(0)
	mut child_stdout_read := &u32(0)
	mut child_stdout_write := &u32(0)
	mut sa := SecurityAttributes {}
	sa.nLength = sizeof(C.SECURITY_ATTRIBUTES)
	sa.bInheritHandle = true

	create_pipe_ok := C.CreatePipe(voidptr(&child_stdout_read),
		voidptr(&child_stdout_write), voidptr(&sa), 0)
	if !create_pipe_ok {
		error_msg := get_error_msg(int(C.GetLastError()))
		return error('exec failed (CreatePipe): $error_msg')
	}
	set_handle_info_ok := C.SetHandleInformation(child_stdout_read, C.HANDLE_FLAG_INHERIT, 0)
	if !set_handle_info_ok {
		error_msg := get_error_msg(int(C.GetLastError()))
		panic('exec failed (SetHandleInformation): $error_msg')
	}

	proc_info := ProcessInformation{}
	mut start_info := StartupInfo{}
	start_info.cb = sizeof(C.PROCESS_INFORMATION)
	start_info.hStdInput = child_stdin
	start_info.hStdOutput = child_stdout_write
	start_info.hStdError = child_stdout_write
	start_info.dwFlags = u32(C.STARTF_USESTDHANDLES)
	command_line := [32768]u16
	C.ExpandEnvironmentStringsW(cmd.to_wide(), voidptr(&command_line), 32768)
	create_process_ok := C.CreateProcessW(0, command_line, 0, 0, C.TRUE, 0, 0, 0, voidptr(&start_info), voidptr(&proc_info))
	if !create_process_ok {
		error_msg := get_error_msg(int(C.GetLastError()))
		return error('exec failed (CreateProcess): $error_msg')
	}
	C.CloseHandle(child_stdin)
	C.CloseHandle(child_stdout_write)
	buf := [4096]byte
	mut bytes_read := u32(0)
	mut read_data := strings.new_builder(1024)
	for {
		readfile_result := C.ReadFile(child_stdout_read, buf, 1000, voidptr(&bytes_read), 0)
		read_data.write_bytes(buf, int(bytes_read))
		if readfile_result == false || int(bytes_read) == 0 {
			break
		}
	}
	soutput := read_data.str().trim_space()
	read_data.free()
	exit_code := u32(0)
	C.WaitForSingleObject(proc_info.hProcess, C.INFINITE)
	C.GetExitCodeProcess(proc_info.hProcess, voidptr(&exit_code))
	C.CloseHandle(proc_info.hProcess)
	C.CloseHandle(proc_info.hThread)
	return Result {
		output: soutput
		exit_code: int(exit_code)
	}
}

fn C.CreateSymbolicLinkW(&u16, &u16, u32) int

pub fn symlink(origin, target string) ?bool {
	flags := if os.is_dir(origin) { 1 } else { 0 }
	if C.CreateSymbolicLinkW(origin.to_wide(), target.to_wide(), u32(flags)) != 0 {
		return true
	}
	return error(get_error_msg(int(C.GetLastError())))
}

pub fn (f mut File) write_bytes(data voidptr, size int) {
	C.fwrite(data, 1, size, f.cfile)
}

pub fn (f mut File) close() {
	if !f.opened {
		return
	}
	f.opened = false
	C.fflush(f.cfile)
	C.fclose(f.cfile)
}
