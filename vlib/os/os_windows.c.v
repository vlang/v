module os

import strings

#include <process.h>

pub const (
	path_separator = '\\'
	path_delimiter = ';'
)

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types
// A handle to an object.
pub type HANDLE = voidptr

// win: FILETIME
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-filetime
struct Filetime {
	dw_low_date_time  u32
	dw_high_date_time u32
}

// win: WIN32_FIND_DATA
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-win32_find_dataw
struct Win32finddata {
mut:
	dw_file_attributes    u32
	ft_creation_time      Filetime
	ft_last_access_time   Filetime
	ft_last_write_time    Filetime
	n_file_size_high      u32
	n_file_size_low       u32
	dw_reserved0          u32
	dw_reserved1          u32
	c_file_name           [260]u16 // max_path_len = 260
	c_alternate_file_name [14]u16  // 14
	dw_file_type          u32
	dw_creator_type       u32
	w_finder_flags        u16
}

struct ProcessInformation {
mut:
	h_process     voidptr
	h_thread      voidptr
	dw_process_id u32
	dw_thread_id  u32
}

struct StartupInfo {
mut:
	cb                 u32
	lp_reserved        &u16
	lp_desktop         &u16
	lp_title           &u16
	dw_x               u32
	dw_y               u32
	dw_x_size          u32
	dw_y_size          u32
	dw_x_count_chars   u32
	dw_y_count_chars   u32
	dw_fill_attributes u32
	dw_flags           u32
	w_show_window      u16
	cb_reserved2       u16
	lp_reserved2       byteptr
	h_std_input        voidptr
	h_std_output       voidptr
	h_std_error        voidptr
}

struct SecurityAttributes {
mut:
	n_length               u32
	lp_security_descriptor voidptr
	b_inherit_handle       bool
}

fn init_os_args_wide(argc int, argv &byteptr) []string {
	mut args_ := []string{}
	for i in 0 .. argc {
		args_ << unsafe { string_from_wide(&u16(argv[i])) }
	}
	return args_
}

pub fn ls(path string) ?[]string {
	mut find_file_data := Win32finddata{}
	mut dir_files := []string{}
	// We can also check if the handle is valid. but using is_dir instead
	// h_find_dir := C.FindFirstFile(path.str, &find_file_data)
	// if (invalid_handle_value == h_find_dir) {
	// return dir_files
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
	first_filename := unsafe { string_from_wide(&find_file_data.c_file_name[0]) }
	if first_filename != '.' && first_filename != '..' {
		dir_files << first_filename
	}
	for C.FindNextFile(h_find_files, voidptr(&find_file_data)) > 0 {
		filename := unsafe { string_from_wide(&find_file_data.c_file_name[0]) }
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
// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) ?bool {
	if path == '.' {
		return true
	}
	apath := real_path(path)
	if !C.CreateDirectory(apath.to_wide(), 0) {
		return error('mkdir failed for "$apath", because CreateDirectory returned ' +
			get_error_msg(int(C.GetLastError())))
	}
	return true
}

// Ref - https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/get-osfhandle?view=vs-2019
// get_file_handle retrieves the operating-system file handle that is associated with the specified file descriptor.
pub fn get_file_handle(path string) HANDLE {
	cfile := vfopen(path, 'rb') or { return HANDLE(invalid_handle_value) }
	handle := HANDLE(C._get_osfhandle(fileno(cfile))) // CreateFile? - hah, no -_-
	return handle
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
				success {
					return string_from_wide2(buf, sz)
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
	format_message_allocate_buffer = 0x00000100
	format_message_argument_array  = 0x00002000
	format_message_from_hmodule    = 0x00000800
	format_message_from_string     = 0x00000400
	format_message_from_system     = 0x00001000
	format_message_ignore_inserts  = 0x00000200
)

// Ref - winnt.h
const (
	sublang_neutral = 0x00
	sublang_default = 0x01
	lang_neutral    = (sublang_neutral)
)

// Ref - https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes--12000-15999-
const (
	max_error_code = 15841 // ERROR_API_UNAVAILABLE
)

// ptr_win_get_error_msg return string (voidptr)
// representation of error, only for windows.
fn ptr_win_get_error_msg(code u32) voidptr {
	mut buf := voidptr(0)
	// Check for code overflow
	if code > u32(os.max_error_code) {
		return buf
	}
	C.FormatMessage(os.format_message_allocate_buffer | os.format_message_from_system | os.format_message_ignore_inserts,
		0, code, C.MAKELANGID(os.lang_neutral, os.sublang_default), voidptr(&buf), 0,
		0)
	return buf
}

// get_error_msg return error code representation in string.
pub fn get_error_msg(code int) string {
	if code < 0 { // skip negative
		return ''
	}
	ptr_text := ptr_win_get_error_msg(u32(code))
	if ptr_text == 0 { // compare with null
		return ''
	}
	return unsafe { string_from_wide(ptr_text) }
}

// execute starts the specified command, waits for it to complete, and returns its output.
pub fn execute(cmd string) Result {
	if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
		return Result{
			exit_code: -1
			output: ';, &&, || and \\n are not allowed in shell commands'
		}
	}
	mut child_stdin := &u32(0)
	mut child_stdout_read := &u32(0)
	mut child_stdout_write := &u32(0)
	mut sa := SecurityAttributes{}
	sa.n_length = sizeof(C.SECURITY_ATTRIBUTES)
	sa.b_inherit_handle = true
	create_pipe_ok := C.CreatePipe(voidptr(&child_stdout_read), voidptr(&child_stdout_write),
		voidptr(&sa), 0)
	if !create_pipe_ok {
		error_num := int(C.GetLastError())
		error_msg := get_error_msg(error_num)
		return Result{
			exit_code: error_num
			output: 'exec failed (CreatePipe): $error_msg'
		}
	}
	set_handle_info_ok := C.SetHandleInformation(child_stdout_read, C.HANDLE_FLAG_INHERIT,
		0)
	if !set_handle_info_ok {
		error_num := int(C.GetLastError())
		error_msg := get_error_msg(error_num)
		return Result{
			exit_code: error_num
			output: 'exec failed (SetHandleInformation): $error_msg'
		}
	}
	proc_info := ProcessInformation{}
	start_info := StartupInfo{
		lp_reserved: 0
		lp_desktop: 0
		lp_title: 0
		cb: sizeof(C.PROCESS_INFORMATION)
		h_std_input: child_stdin
		h_std_output: child_stdout_write
		h_std_error: child_stdout_write
		dw_flags: u32(C.STARTF_USESTDHANDLES)
	}
	command_line := [32768]u16{}
	C.ExpandEnvironmentStringsW(cmd.to_wide(), voidptr(&command_line), 32768)
	create_process_ok := C.CreateProcessW(0, command_line, 0, 0, C.TRUE, 0, 0, 0, voidptr(&start_info),
		voidptr(&proc_info))
	if !create_process_ok {
		error_num := int(C.GetLastError())
		error_msg := get_error_msg(error_num)
		return Result{
			exit_code: error_num
			output: 'exec failed (CreateProcess) with code $error_num: $error_msg cmd: $cmd'
		}
	}
	C.CloseHandle(child_stdin)
	C.CloseHandle(child_stdout_write)
	buf := [4096]byte{}
	mut bytes_read := u32(0)
	mut read_data := strings.new_builder(1024)
	for {
		mut result := false
		unsafe {
			result = C.ReadFile(child_stdout_read, buf, 1000, voidptr(&bytes_read), 0)
			read_data.write_bytes(&buf[0], int(bytes_read))
		}
		if result == false || int(bytes_read) == 0 {
			break
		}
	}
	soutput := read_data.str().trim_space()
	unsafe { read_data.free() }
	exit_code := u32(0)
	C.WaitForSingleObject(proc_info.h_process, C.INFINITE)
	C.GetExitCodeProcess(proc_info.h_process, voidptr(&exit_code))
	C.CloseHandle(proc_info.h_process)
	C.CloseHandle(proc_info.h_thread)
	return Result{
		output: soutput
		exit_code: int(exit_code)
	}
}

// See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createsymboliclinkw
fn C.CreateSymbolicLinkW(&u16, &u16, u32) int

pub fn symlink(symlink_path string, target_path string) ?bool {
	mut flags := 0
	if is_dir(symlink_path) {
		flags |= 1
	}
	flags |= 2 // SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
	res := C.CreateSymbolicLinkW(symlink_path.to_wide(), target_path.to_wide(), flags)
	if res == 0 {
		return error(get_error_msg(int(C.GetLastError())))
	}
	if !exists(symlink_path) {
		return error('C.CreateSymbolicLinkW reported success, but symlink still does not exist')
	}
	return true
}

pub fn (mut f File) close() {
	if !f.is_opened {
		return
	}
	f.is_opened = false
	C.fflush(f.cfile)
	C.fclose(f.cfile)
}

pub struct ExceptionRecord {
pub:
	// status_ constants
	code        u32
	flags       u32
	record      &ExceptionRecord
	address     voidptr
	param_count u32
	// params []voidptr
}

pub struct ContextRecord {
	// TODO
}

pub struct ExceptionPointers {
pub:
	exception_record &ExceptionRecord
	context_record   &ContextRecord
}

pub type VectoredExceptionHandler = fn (&ExceptionPointers) u32

// This is defined in builtin because we use vectored exception handling
// for our unhandled exception handler on windows
// As a result this definition is commented out to prevent
// duplicate definitions from displeasing the compiler
// fn C.AddVectoredExceptionHandler(u32, VectoredExceptionHandler)
pub fn add_vectored_exception_handler(first bool, handler VectoredExceptionHandler) {
	C.AddVectoredExceptionHandler(u32(first), C.PVECTORED_EXCEPTION_HANDLER(handler))
}

// this is defined in builtin_windows.c.v in builtin
// fn C.IsDebuggerPresent() bool
pub fn debugger_present() bool {
	return C.IsDebuggerPresent()
}

pub fn uname() Uname {
	// TODO: implement `os.uname()` for windows
	unknown := 'unknown'
	return Uname{
		sysname: unknown
		nodename: unknown
		release: unknown
		version: unknown
		machine: unknown
	}
}

// `is_writable_folder` - `folder` exists and is writable to the process
pub fn is_writable_folder(folder string) ?bool {
	if !exists(folder) {
		return error('`$folder` does not exist')
	}
	if !is_dir(folder) {
		return error('`folder` is not a folder')
	}
	tmp_perm_check := join_path(folder, 'tmp_perm_check_pid_' + getpid().str())
	mut f := open_file(tmp_perm_check, 'w+', 0o700) or {
		return error('cannot write to folder $folder: $err')
	}
	f.close()
	rm(tmp_perm_check) ?
	return true
}

fn C._getpid() int

[inline]
pub fn getpid() int {
	return C._getpid()
}

pub fn posix_set_permission_bit(path_s string, mode u32, enable bool) {
	// windows has no concept of a permission mask, so do nothing
}
