module os

import strings

#flag windows -l advapi32
#include <process.h>
#include <sys/utime.h>

// See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createsymboliclinkw
fn C.CreateSymbolicLinkW(&u16, &u16, u32) int

// See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createhardlinkw
fn C.CreateHardLinkW(&u16, &u16, C.SECURITY_ATTRIBUTES) int

fn C._getpid() int

const executable_suffixes = ['.exe', '.bat', '.cmd', '']

pub const (
	path_separator = '\\'
	path_delimiter = ';'
)

// Ref - https://docs.microsoft.com/en-us/windows/desktop/winprog/windows-data-types
// A handle to an object.
pub type HANDLE = voidptr
pub type HMODULE = voidptr

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
	lp_reserved        &u16 = unsafe { nil }
	lp_desktop         &u16 = unsafe { nil }
	lp_title           &u16 = unsafe { nil }
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
	lp_reserved2       &u8 = unsafe { nil }
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

struct C._utimbuf {
	actime  int
	modtime int
}

fn C._utime(&char, voidptr) int

fn init_os_args_wide(argc int, argv &&u8) []string {
	mut args_ := []string{len: argc}
	for i in 0 .. argc {
		args_[i] = unsafe { string_from_wide(&u16(argv[i])) }
	}
	return args_
}

fn native_glob_pattern(pattern string, mut matches []string) ! {
	$if debug {
		// FindFirstFile() and FindNextFile() both have a globbing function.
		// Unfortunately this is not as pronounced as under Unix, but should provide some functionality
		eprintln('os.glob() does not have all the features on Windows as it has on Unix operating systems')
	}
	mut find_file_data := Win32finddata{}
	wpattern := pattern.replace('/', '\\').to_wide()
	h_find_files := C.FindFirstFile(wpattern, voidptr(&find_file_data))

	defer {
		C.FindClose(h_find_files)
	}

	if h_find_files == C.INVALID_HANDLE_VALUE {
		return error('os.glob(): Could not get a file handle: ' +
			get_error_msg(int(C.GetLastError())))
	}

	// save first finding
	fname := unsafe { string_from_wide(&find_file_data.c_file_name[0]) }
	if fname !in ['.', '..'] {
		mut fp := fname.replace('\\', '/')
		if find_file_data.dw_file_attributes & u32(C.FILE_ATTRIBUTE_DIRECTORY) > 0 {
			fp += '/'
		}
		matches << fp
	}

	// check and save next findings
	for i := 0; C.FindNextFile(h_find_files, voidptr(&find_file_data)) > 0; i++ {
		filename := unsafe { string_from_wide(&find_file_data.c_file_name[0]) }
		if filename in ['.', '..'] {
			continue
		}
		mut fpath := filename.replace('\\', '/')
		if find_file_data.dw_file_attributes & u32(C.FILE_ATTRIBUTE_DIRECTORY) > 0 {
			fpath += '/'
		}
		matches << fpath
	}
}

pub fn utime(path string, actime int, modtime int) ! {
	mut u := C._utimbuf{actime, modtime}
	if C._utime(&char(path.str), voidptr(&u)) != 0 {
		return error_with_code(posix_get_error_msg(C.errno), C.errno)
	}
}

pub fn ls(path string) ![]string {
	if path.len == 0 {
		return error('ls() expects a folder, not an empty string')
	}
	mut find_file_data := Win32finddata{}
	mut dir_files := []string{}
	// We can also check if the handle is valid. but using is_dir instead
	// h_find_dir := C.FindFirstFile(path.str, &find_file_data)
	// if (invalid_handle_value == h_find_dir) {
	// return dir_files
	// }
	// C.FindClose(h_find_dir)
	if !is_dir(path) {
		return error('ls() couldnt open dir "${path}": directory does not exist')
	}
	// NOTE: Should eventually have path struct & os dependant path seperator (eg os.PATH_SEPERATOR)
	// we need to add files to path eg. c:\windows\*.dll or :\windows\*
	path_files := '${path}\\*'
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

// mkdir creates a new directory with the specified path.
pub fn mkdir(path string, params MkdirParams) ! {
	if path == '.' {
		return
	}
	apath := real_path(path)
	if !C.CreateDirectory(apath.to_wide(), 0) {
		return error('mkdir failed for "${apath}", because CreateDirectory returned: ' +
			get_error_msg(int(C.GetLastError())))
	}
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
pub fn get_module_filename(handle HANDLE) !string {
	unsafe {
		mut sz := 4096 // Optimized length
		mut buf := &u16(malloc_noscan(4096))
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
	lang_neutral    = sublang_neutral
)

// Ref - https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes--12000-15999-
const (
	max_error_code = 15841 // ERROR_API_UNAVAILABLE
)

// ptr_win_get_error_msg return string (voidptr)
// representation of error, only for windows.
fn ptr_win_get_error_msg(code u32) voidptr {
	mut buf := unsafe { nil }
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
// In opposition to `raw_execute` this function will safeguard against content that is known to cause
// a lot of problems when executing shell commands on Windows.
pub fn execute(cmd string) Result {
	if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
		return Result{
			exit_code: -1
			output: ';, &&, || and \\n are not allowed in shell commands'
		}
	}
	return unsafe { raw_execute(cmd) }
}

// raw_execute starts the specified command, waits for it to complete, and returns its output.
// It's marked as `unsafe` to help emphasize the problems that may arise by allowing, for example,
// user provided escape sequences.
[unsafe]
pub fn raw_execute(cmd string) Result {
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
			output: 'exec failed (CreatePipe): ${error_msg}'
		}
	}
	set_handle_info_ok := C.SetHandleInformation(child_stdout_read, C.HANDLE_FLAG_INHERIT,
		0)
	if !set_handle_info_ok {
		error_num := int(C.GetLastError())
		error_msg := get_error_msg(error_num)
		return Result{
			exit_code: error_num
			output: 'exec failed (SetHandleInformation): ${error_msg}'
		}
	}
	proc_info := ProcessInformation{}
	start_info := StartupInfo{
		lp_reserved2: 0
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
	create_process_ok := C.CreateProcessW(0, &command_line[0], 0, 0, C.TRUE, 0, 0, 0,
		voidptr(&start_info), voidptr(&proc_info))
	if !create_process_ok {
		error_num := int(C.GetLastError())
		error_msg := get_error_msg(error_num)
		return Result{
			exit_code: error_num
			output: 'exec failed (CreateProcess) with code ${error_num}: ${error_msg} cmd: ${cmd}'
		}
	}
	C.CloseHandle(child_stdin)
	C.CloseHandle(child_stdout_write)
	buf := [4096]u8{}
	mut bytes_read := u32(0)
	mut read_data := strings.new_builder(1024)
	for {
		mut result := false
		unsafe {
			result = C.ReadFile(child_stdout_read, &buf[0], 1000, voidptr(&bytes_read),
				0)
			read_data.write_ptr(&buf[0], int(bytes_read))
		}
		if result == false || int(bytes_read) == 0 {
			break
		}
	}
	soutput := read_data.str()
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

pub fn symlink(origin string, target string) ! {
	// this is a temporary fix for TCC32 due to runtime error
	// TODO: find the cause why TCC32 for Windows does not work without the compiletime option
	$if x64 || x32 {
		mut flags := 0
		if is_dir(origin) {
			flags ^= 1
		}

		flags ^= 2 // SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
		res := C.CreateSymbolicLinkW(target.to_wide(), origin.to_wide(), flags)

		// 1 = success, != 1 failure => https://stackoverflow.com/questions/33010440/createsymboliclink-on-windows-10
		if res != 1 {
			return error(get_error_msg(int(C.GetLastError())))
		}
		if !exists(target) {
			return error('C.CreateSymbolicLinkW reported success, but symlink still does not exist')
		}
		return
	}
	return error('could not symlink')
}

pub fn link(origin string, target string) ! {
	res := C.CreateHardLinkW(target.to_wide(), origin.to_wide(), C.NULL)
	// 1 = success, != 1 failure => https://stackoverflow.com/questions/33010440/createsymboliclink-on-windows-10
	if res != 1 {
		return error(get_error_msg(int(C.GetLastError())))
	}
	if !exists(target) {
		return error('C.CreateHardLinkW reported success, but link still does not exist')
	}
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
	record      &ExceptionRecord = unsafe { nil }
	address     voidptr
	param_count u32
	// params []voidptr
}

pub struct ContextRecord {
	// TODO
}

pub struct ExceptionPointers {
pub:
	exception_record &ExceptionRecord = unsafe { nil }
	context_record   &ContextRecord   = unsafe { nil }
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
	sys_and_ver := execute('cmd /c ver').output.split('[')
	nodename := hostname()
	machine := getenv('PROCESSOR_ARCHITECTURE')
	return Uname{
		sysname: sys_and_ver[0].trim_space()
		nodename: nodename
		release: sys_and_ver[1].replace(']', '')
		version: sys_and_ver[0] + '[' + sys_and_ver[1]
		machine: machine
	}
}

pub fn hostname() string {
	hostname := [255]u16{}
	size := u32(255)
	res := C.GetComputerNameW(&hostname[0], &size)
	if !res {
		return get_error_msg(int(C.GetLastError()))
	}
	return unsafe { string_from_wide(&hostname[0]) }
}

pub fn loginname() string {
	loginname := [255]u16{}
	size := u32(255)
	res := C.GetUserNameW(&loginname[0], &size)
	if !res {
		return get_error_msg(int(C.GetLastError()))
	}
	return unsafe { string_from_wide(&loginname[0]) }
}

// ensure_folder_is_writable checks that `folder` exists, and is writable to the process
// by creating an empty file in it, then deleting it.
pub fn ensure_folder_is_writable(folder string) ! {
	if !exists(folder) {
		return error_with_code('`${folder}` does not exist', 1)
	}
	if !is_dir(folder) {
		return error_with_code('`folder` is not a folder', 2)
	}
	tmp_folder_name := 'tmp_perm_check_pid_' + getpid().str()
	tmp_perm_check := join_path_single(folder, tmp_folder_name)
	write_file(tmp_perm_check, 'test') or {
		return error_with_code('cannot write to folder "${folder}": ${err}', 3)
	}
	rm(tmp_perm_check)!
}

[inline]
pub fn getpid() int {
	return C._getpid()
}

[inline]
pub fn getppid() int {
	return 0
}

[inline]
pub fn getuid() int {
	return 0
}

[inline]
pub fn geteuid() int {
	return 0
}

[inline]
pub fn getgid() int {
	return 0
}

[inline]
pub fn getegid() int {
	return 0
}

pub fn posix_set_permission_bit(path_s string, mode u32, enable bool) {
	// windows has no concept of a permission mask, so do nothing
}

//

pub fn (mut c Command) start() ! {
	panic('not implemented')
}

pub fn (mut c Command) read_line() string {
	panic('not implemented')
}

pub fn (mut c Command) close() ! {
	panic('not implemented')
}
