module os

import strings

fn C.GenerateConsoleCtrlEvent(event u32, pgid u32) bool
fn C.GetModuleHandleA(name &char) HMODULE
fn C.GetProcAddress(handle voidptr, procname &u8) voidptr
fn C.TerminateProcess(process HANDLE, exit_code u32) bool
fn C.ResumeThread(thread HANDLE) u32
fn C.PeekNamedPipe(hNamedPipe voidptr, lpBuffer voidptr, nBufferSize i32, lpBytesRead voidptr, lpTotalBytesAvail voidptr,
	lpBytesLeftThisMessage voidptr) bool
fn C.CreateFileW(lpFileName &u16, dwDesiredAccess u32, dwShareMode u32, lpSecurityAttributes voidptr, dwCreationDisposition u32, dwFlagsAndAttributes u32, hTemplateFile voidptr) voidptr
fn C.GetSystemDirectoryW(lpBuffer &u16, uSize u32) u32
fn C.GetWindowsDirectoryW(lpBuffer &u16, uSize u32) u32

type FN_NTSuspendResume = fn (voidptr) u64

// The job object functions are looked up in kernel32 when they are used, as the ntdll
// ones are: the import lists of V's bundled tcc do not carry all of them, and a program
// that links without one must not stop linking because of a function that only
// `use_pgroup` children need.
type FN_CreateJobObjectW = fn (voidptr, voidptr) voidptr
type FN_SetInformationJobObject = fn (voidptr, i32, voidptr, u32) i32
type FN_AssignProcessToJobObject = fn (voidptr, voidptr) i32
type FN_TerminateJobObject = fn (voidptr, u32) i32

// JobObjectExtendedLimitInformation, the information class of SetInformationJobObject
// that takes JOBOBJECT_EXTENDED_LIMIT_INFORMATION.
const win_job_object_extended_limit_information = 9
// JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE: closing the last handle of the job ends every
// process in it, so the tree does not outlive a V that dies without waiting for it.
const win_job_object_limit_kill_on_job_close = u32(0x00002000)
// CREATE_SUSPENDED: the child starts with its primary thread suspended, so that it can
// be put in its job before it runs a single instruction, and cannot start a process of
// its own outside of it.
const win_create_suspended = 0x00000004

fn kernel32_fn(name &char) voidptr {
	kernel32 := C.GetModuleHandleA(c'KERNEL32')
	if kernel32 == 0 {
		return unsafe { nil }
	}
	return C.GetProcAddress(kernel32, voidptr(name))
}

fn ntdll_fn(name &char) FN_NTSuspendResume {
	ntdll := C.GetModuleHandleA(c'NTDLL')
	if ntdll == 0 {
		return unsafe { FN_NTSuspendResume(0) }
	}
	the_fn := FN_NTSuspendResume(C.GetProcAddress(ntdll, voidptr(name)))
	return the_fn
}

fn failed_cfn_report_error(ok bool, label string) {
	if ok {
		return
	}
	error_num := int(C.GetLastError())
	error_msg := get_error_msg(error_num)
	eprintln('failed ${label}: ${error_msg}')
	exit(1)
}

// win_env_value_from_entries returns the value of the environment variable
// `name` from a list of `key=value` entries. Windows environment variable
// names are case insensitive, and the system spells the search path `Path`,
// so the comparison must be too.
fn win_env_value_from_entries(env []string, name string) ?string {
	prefix := '${name}='.to_lower_ascii()
	for entry in env {
		if entry.len > prefix.len && entry[..prefix.len].to_lower_ascii() == prefix {
			return entry[prefix.len..]
		}
	}
	return none
}

// win_bare_command_search_dirs returns the directories that `CreateProcessW`
// itself searches for a bare module name before it consults `PATH`, in its
// documented order: the directory the running application was loaded from,
// the current directory, the 32-bit system directory, the 16-bit system
// directory (`<windir>\System`; there is no function that returns it), and
// the Windows directory. A directory that cannot be determined is skipped.
fn win_bare_command_search_dirs() []string {
	mut dirs := []string{cap: 5}
	dirs << dir(executable())
	dirs << getwd()
	mut buf := [max_path_buffer_size]u16{}
	sys_len := C.GetSystemDirectoryW(&buf[0], max_path_buffer_size)
	if sys_len > 0 && sys_len < u32(max_path_buffer_size) {
		dirs << unsafe { string_from_wide2(&buf[0], int(sys_len)) }
	}
	win_len := C.GetWindowsDirectoryW(&buf[0], max_path_buffer_size)
	if win_len > 0 && win_len < u32(max_path_buffer_size) {
		windir := unsafe { string_from_wide2(&buf[0], int(win_len)) }
		dirs << join_path_single(windir, 'System')
		dirs << windir
	}
	return dirs
}

// win_resolve_filename turns `p.filename` into the path handed to
// `CreateProcessW`, mirroring `unix_resolve_filename`: an absolute path is used
// as is, a relative path containing a directory separator is anchored to the
// current directory (the child's work folder may differ), and a bare command
// name (`gcc`, `cl`, `node`) is looked up in the same order `CreateProcessW`
// uses for a bare module name - the application directory, the current
// directory and the system directories (`win_bare_command_search_dirs`), and
// only then the `PATH` the child will start with - trying the Windows
// executable suffixes, as `find_abs_path_of_executable` does. Each directory
// is searched on its own, on purpose: the helper tries every suffix across
// all of its directories before moving to the next suffix, so a single
// combined search would let a `tool.exe` on PATH shadow a `tool.cmd` in the
// current directory, or a same-named `cmd.exe` on PATH shadow the one in the
// system directory. `CreateProcessW` performs no lookup at all when given an
// application name, so without this every bare-name spawn failed with "The
// system cannot find the file specified".
fn (p &Process) win_resolve_filename() !string {
	// A `\\?\` path is already absolute and must reach CreateProcessW unchanged:
	// abs_path would normalize it back into a form that exceeds MAX_PATH.
	if p.filename.starts_with('\\\\?\\') {
		return p.filename
	}
	if is_abs_path(p.filename) {
		return win_exec_application_name(p.filename)
	}
	if p.filename.contains('\\') || p.filename.contains('/') {
		return abs_path(p.filename)
	}
	for d in win_bare_command_search_dirs() {
		if found := find_abs_path_of_executable_in_path_env(p.filename, d) {
			return found
		}
	}
	path := win_env_value_from_entries(p.env, 'PATH') or { return error_failed_to_find_executable() }
	return find_abs_path_of_executable_in_path_env(p.filename, path)
}

fn close_valid_handle(p voidptr) {
	h := &&u32(p)
	if *h != &u32(unsafe { nil }) {
		C.CloseHandle(*h)
		unsafe {
			*h = &u32(nil)
		}
	}
}

pub struct WProcess {
pub mut:
	proc_info         ProcessInformation
	command_line      [65536]u8
	child_stdin_read  &u32 = unsafe { nil }
	child_stdin_write &u32 = unsafe { nil }

	child_stdout_read  &u32 = unsafe { nil }
	child_stdout_write &u32 = unsafe { nil }

	child_stderr_read  &u32 = unsafe { nil }
	child_stderr_write &u32 = unsafe { nil }

	// job is the job object of a `use_pgroup` child (nil for every other child, and when the
	// child could not be put in one): win_kill_pgroup ends it, and with it the whole tree.
	job &u32 = unsafe { nil }
}

// win_assign_job puts the child of `wdata` in a new job object that kills what is in it when
// its last handle closes and does not let a process break away from it. It reports whether
// that worked; a job that was made but could not take the child is closed again. A process
// that is already in a job (a child of a CI runner is) can be put in a nested job on Windows
// 8 and later; on an older system the assignment is refused and the caller falls back.
fn win_assign_job(mut wdata WProcess) bool {
	create := FN_CreateJobObjectW(kernel32_fn(c'CreateJobObjectW'))
	set_information := FN_SetInformationJobObject(kernel32_fn(c'SetInformationJobObject'))
	assign := FN_AssignProcessToJobObject(kernel32_fn(c'AssignProcessToJobObject'))
	if voidptr(create) == 0 || voidptr(set_information) == 0 || voidptr(assign) == 0 {
		return false
	}
	job := create(unsafe { nil }, unsafe { nil })
	if job == 0 {
		return false
	}
	// JOBOBJECT_EXTENDED_LIMIT_INFORMATION is 144 bytes on x64 and 112 on x86; its
	// LimitFlags sit at offset 16 (the basic information starts with two 8 byte
	// LARGE_INTEGERs) in both. Zero everything else: no limit but the flag.
	mut info := [256]u8{}
	unsafe {
		mut flags := &u32(&info[16])
		*flags = win_job_object_limit_kill_on_job_close
	}
	mut size := u32(112)
	$if x64 {
		size = u32(144)
	}
	if set_information(job, win_job_object_extended_limit_information, voidptr(&info[0]), size) == 0
		|| assign(job, wdata.proc_info.h_process) == 0 {
		C.CloseHandle(job)
		return false
	}
	unsafe {
		wdata.job = &u32(job)
	}
	return true
}

@[manualfree]
fn (mut p Process) win_spawn_process() int {
	mut to_be_freed := []voidptr{cap: 5}
	defer {
		for idx := to_be_freed.len - 1; idx >= 0; idx-- {
			unsafe { free(to_be_freed[idx]) }
		}
		unsafe { to_be_freed.free() }
	}
	// Expand the path to an absolute one, in case we later change the working
	// folder. A bare command name that none of the searched directories or PATH
	// contains is left as is, so that CreateProcessW can still run its own
	// lookup (it appends `.exe` to an extension-less name and searches the
	// same directories, using the parent's PATH) before the spawn is reported
	// as failed.
	p.filename = p.win_resolve_filename() or { p.filename }
	mut wdata := &WProcess{
		child_stdin_read:   unsafe { nil }
		child_stdin_write:  unsafe { nil }
		child_stdout_read:  unsafe { nil }
		child_stdout_write: unsafe { nil }
		child_stderr_read:  unsafe { nil }
		child_stderr_write: unsafe { nil }
	}
	p.wdata = voidptr(wdata)
	mut start_info := StartupInfo{
		lp_reserved2: unsafe { nil }
		lp_reserved:  unsafe { nil }
		lp_desktop:   unsafe { nil }
		lp_title:     unsafe { nil }
		cb:           sizeof(StartupInfo)
	}
	mut sa := SecurityAttributes{}
	if p.use_stdio_ctl || p.has_stdin_path {
		sa.n_length = sizeof(C.SECURITY_ATTRIBUTES)
		sa.b_inherit_handle = true
	}
	if p.has_stdin_path {
		stdin_path_wide := p.stdin_path.to_wide()
		to_be_freed << stdin_path_wide
		stdin_handle := C.CreateFileW(stdin_path_wide, C.GENERIC_READ,
			C.FILE_SHARE_READ | C.FILE_SHARE_WRITE | C.FILE_SHARE_DELETE, voidptr(&sa),
			C.OPEN_EXISTING, C.FILE_ATTRIBUTE_NORMAL, 0)
		if stdin_handle == invalid_handle_value {
			failed_cfn_report_error(false, 'CreateFileW stdin')
		}
		wdata.child_stdin_read = &u32(stdin_handle)
	}
	if p.use_stdio_ctl {
		if !p.has_stdin_path {
			create_pipe_ok0 := C.CreatePipe(voidptr(&wdata.child_stdin_read),
				voidptr(&wdata.child_stdin_write), voidptr(&sa), 65536)
			failed_cfn_report_error(create_pipe_ok0, 'CreatePipe stdin')
			set_handle_info_ok0 := C.SetHandleInformation(wdata.child_stdin_write,
				C.HANDLE_FLAG_INHERIT, 0)
			failed_cfn_report_error(set_handle_info_ok0, 'SetHandleInformation')
		}
		create_pipe_ok1 := C.CreatePipe(voidptr(&wdata.child_stdout_read),
			voidptr(&wdata.child_stdout_write), voidptr(&sa), 65536)
		failed_cfn_report_error(create_pipe_ok1, 'CreatePipe stdout')
		set_handle_info_ok1 := C.SetHandleInformation(wdata.child_stdout_read,
			C.HANDLE_FLAG_INHERIT, 0)
		failed_cfn_report_error(set_handle_info_ok1, 'SetHandleInformation')
		if !p.merge_stdio {
			create_pipe_ok2 := C.CreatePipe(voidptr(&wdata.child_stderr_read),
				voidptr(&wdata.child_stderr_write), voidptr(&sa), 65536)
			failed_cfn_report_error(create_pipe_ok2, 'CreatePipe stderr')
			set_handle_info_ok2 := C.SetHandleInformation(wdata.child_stderr_read,
				C.HANDLE_FLAG_INHERIT, 0)
			failed_cfn_report_error(set_handle_info_ok2, 'SetHandleInformation stderr')
		}
		start_info.h_std_input = wdata.child_stdin_read
		start_info.h_std_output = wdata.child_stdout_write
		start_info.h_std_error = if p.merge_stdio {
			wdata.child_stdout_write
		} else {
			wdata.child_stderr_write
		}
		start_info.dw_flags = u32(C.STARTF_USESTDHANDLES)
	} else if p.has_stdin_path {
		start_info.h_std_input = wdata.child_stdin_read
		start_info.h_std_output = C.GetStdHandle(C.STD_OUTPUT_HANDLE)
		start_info.h_std_error = C.GetStdHandle(C.STD_ERROR_HANDLE)
		start_info.dw_flags = u32(C.STARTF_USESTDHANDLES)
	}
	mut cmd := requote_arg(p.filename)
	if p.args.len > 0 {
		cmd += ' ' + requote_args(p.args)
	}
	cmd_wide_ptr := cmd.to_wide()
	to_be_freed << cmd_wide_ptr
	if p.expand_environment {
		C.ExpandEnvironmentStringsW(cmd_wide_ptr, voidptr(&wdata.command_line[0]), 32768)
	}
	command_line_ptr := if p.expand_environment {
		voidptr(&wdata.command_line[0])
	} else {
		voidptr(cmd_wide_ptr)
	}

	mut creation_flags := if p.create_no_window {
		int(C.CREATE_NO_WINDOW)
	} else {
		int(C.NORMAL_PRIORITY_CLASS)
	}
	if p.use_pgroup {
		// A new group, so that the console event of win_kill_pgroup reaches the child, and
		// a suspended start, so that the child is in its job before it runs.
		creation_flags |= C.CREATE_NEW_PROCESS_GROUP | win_create_suspended
	}

	mut application_name_ptr := &u16(unsafe { nil })
	filename_lc := p.filename.to_lower_ascii()
	if is_abs_path(p.filename) && !filename_lc.ends_with('.bat') && !filename_lc.ends_with('.cmd') {
		// Bind CreateProcessW to the exact executable path, instead of relying only on
		// command-line parsing. That avoids accidental prefix matches for spaced paths like
		// `C:\work\testing v\program.exe`, where Windows may otherwise resolve a stale
		// `C:\work\testing.exe`.
		application_name_ptr = p.filename.to_wide()
		to_be_freed << application_name_ptr
	}

	mut work_folder_ptr := voidptr(unsafe { nil })
	if p.work_folder != '' {
		work_folder_ptr = p.work_folder.to_wide()
		to_be_freed << work_folder_ptr
	}

	windows_avoid_long_temp_env()
	mut env_block := []u16{}
	if p.env.len > 0 {
		mut env_ptr := &u16(unsafe { nil })

		for e in windows_temp_env_entries_fix(p.env, windows_temp_env_candidates) {
			// e should in `ABC=123` format
			env_ptr = e.to_wide()
			if isnil(env_ptr) {
				continue
			}
			mut i := 0
			for {
				character := unsafe { env_ptr[i] }
				if character == 0 {
					break
				}
				env_block << character
				i++
			}
			env_block << u16(0)
			to_be_freed << env_ptr
		}
		env_block << u16(0)
		creation_flags |= C.CREATE_UNICODE_ENVIRONMENT
		defer(fn) {
			unsafe { env_block.free() }
		}
	}

	create_process_ok := C.CreateProcessW(application_name_ptr, command_line_ptr, 0,
		0, C.TRUE, creation_flags, if env_block.len > 0 {
			env_block.data
		} else {
			voidptr(0)
		}, work_folder_ptr, voidptr(&start_info), voidptr(&wdata.proc_info))
	failed_cfn_report_error(create_process_ok, 'CreateProcess `${p.filename}`')
	if p.use_pgroup {
		if !win_assign_job(mut wdata) {
			// Nothing is lost compared with before, and the caller can tell: signal_pgkill
			// then only reaches the direct child and what shares its console group.
			p.pgroup_fallback = true
		}
		C.ResumeThread(wdata.proc_info.h_thread)
	}
	if p.use_stdio_ctl {
		close_valid_handle(&wdata.child_stdin_read)
		close_valid_handle(&wdata.child_stdout_write)
		close_valid_handle(&wdata.child_stderr_write)
	} else if p.has_stdin_path {
		close_valid_handle(&wdata.child_stdin_read)
	}
	p.pid = int(wdata.proc_info.dw_process_id)
	return p.pid
}

fn (mut p Process) win_stop_process() {
	the_fn := ntdll_fn(c'NtSuspendProcess')
	if voidptr(the_fn) == 0 {
		return
	}
	wdata := unsafe { &WProcess(p.wdata) }
	the_fn(wdata.proc_info.h_process)
}

fn (mut p Process) win_resume_process() {
	the_fn := ntdll_fn(c'NtResumeProcess')
	if voidptr(the_fn) == 0 {
		return
	}
	wdata := unsafe { &WProcess(p.wdata) }
	the_fn(wdata.proc_info.h_process)
}

fn (mut p Process) win_kill_process() {
	wdata := unsafe { &WProcess(p.wdata) }
	C.TerminateProcess(wdata.proc_info.h_process, 3)
}

fn (mut p Process) win_term_process() {
	p.win_kill_process()
}

fn (mut p Process) win_kill_pgroup() {
	wdata := unsafe { &WProcess(p.wdata) }
	// A child that handles CTRL_BREAK gets 20 ms to end itself, as before.
	C.GenerateConsoleCtrlEvent(C.CTRL_BREAK_EVENT, wdata.proc_info.dw_process_id)
	C.Sleep(20)
	if !isnil(wdata.job) {
		// Every process of the job, whatever console it has or whether it has a window.
		terminate := FN_TerminateJobObject(kernel32_fn(c'TerminateJobObject'))
		if voidptr(terminate) != 0 {
			terminate(voidptr(wdata.job), 3)
		}
	}
	C.TerminateProcess(wdata.proc_info.h_process, 3)
}

// win_release_job closes the job handle of a child that has been waited for. What is still
// running in the job is left running, as on Linux (a `use_pgroup` child that exits normally
// does not take its descendants with it): the kill-on-close flag is cleared first. It stays
// set for as long as the job is open, which is the net for a V that dies first.
fn win_release_job(mut wdata WProcess) {
	if isnil(wdata.job) {
		return
	}
	set_information := FN_SetInformationJobObject(kernel32_fn(c'SetInformationJobObject'))
	if voidptr(set_information) != 0 {
		mut info := [256]u8{}
		mut size := u32(112)
		$if x64 {
			size = u32(144)
		}
		set_information(voidptr(wdata.job), win_job_object_extended_limit_information,
			voidptr(&info[0]), size)
	}
	close_valid_handle(&wdata.job)
}

fn (mut p Process) win_wait() {
	mut exit_code := u32(1)
	mut wdata := unsafe { &WProcess(p.wdata) }
	if p.wdata != 0 {
		C.WaitForSingleObject(wdata.proc_info.h_process, C.INFINITE)
		// The SDK DWORD output is a 32-bit value; use its exact pointer type.
		unsafe {
			C.GetExitCodeProcess(wdata.proc_info.h_process, &C.DWORD(&exit_code))
		}
		close_valid_handle(&wdata.child_stdin_read)
		close_valid_handle(&wdata.child_stdout_write)
		close_valid_handle(&wdata.child_stderr_write)
		win_release_job(mut wdata)
		close_valid_handle(&wdata.proc_info.h_process)
		close_valid_handle(&wdata.proc_info.h_thread)
	}
	p.status = .exited
	p.code = int(i32(exit_code))
}

fn (mut p Process) win_is_alive() bool {
	mut exit_code := u32(0)
	wdata := unsafe { &WProcess(p.wdata) }
	// The SDK DWORD output is a 32-bit value; use its exact pointer type.
	unsafe {
		C.GetExitCodeProcess(wdata.proc_info.h_process, &C.DWORD(&exit_code))
	}
	if exit_code == C.STILL_ACTIVE {
		return true
	}
	return false
}

///////////////

fn (mut p Process) win_write_string(idx int, _s string) {
	mut wdata := unsafe { &WProcess(p.wdata) }
	if unsafe { wdata == 0 } || idx != 0 {
		return
	}
	mut rhandle := wdata.child_stdin_write
	if rhandle == 0 {
		return
	}
	mut bytes_write := C.DWORD(0)
	unsafe {
		C.WriteFile(rhandle, _s.str, u32(_s.len), voidptr(&bytes_write), 0)
	}
}

fn (mut p Process) win_read_string(idx int, _maxbytes int) (string, int) {
	mut wdata := unsafe { &WProcess(p.wdata) }
	if unsafe { wdata == 0 } {
		return '', 0
	}
	mut rhandle := &u32(unsafe { nil })
	if idx == 1 {
		rhandle = wdata.child_stdout_read
	}
	if idx == 2 {
		rhandle = wdata.child_stderr_read
	}
	if rhandle == 0 {
		return '', 0
	}
	mut bytes_avail := int(0)
	if !C.PeekNamedPipe(rhandle, unsafe { nil }, int(0), unsafe { nil }, voidptr(&bytes_avail),
		unsafe { nil }) {
		return '', 0
	}
	if bytes_avail == 0 {
		return '', 0
	}

	mut bytes_read := int(0)
	buf := []u8{len: bytes_avail + 300}
	unsafe {
		C.ReadFile(rhandle, &buf[0], buf.cap, voidptr(&bytes_read), 0)
	}
	return decode_windows_captured_output(buf[..bytes_read].bytestr()), bytes_read
}

fn (mut p Process) win_is_pending(idx int) bool {
	mut wdata := unsafe { &WProcess(p.wdata) }
	if unsafe { wdata == 0 } {
		return false
	}
	mut rhandle := &u32(unsafe { nil })
	if idx == 1 {
		rhandle = wdata.child_stdout_read
	}
	if idx == 2 {
		rhandle = wdata.child_stderr_read
	}
	if rhandle == 0 {
		return false
	}
	mut bytes_avail := int(0)
	if C.PeekNamedPipe(rhandle, unsafe { nil }, int(0), unsafe { nil }, voidptr(&bytes_avail),
		unsafe { nil }) {
		return bytes_avail > 0
	}
	return false
}

fn (mut p Process) win_slurp(idx int) string {
	mut wdata := unsafe { &WProcess(p.wdata) }
	if unsafe { wdata == 0 } {
		return ''
	}
	mut rhandle := &u32(unsafe { nil })
	if idx == 1 {
		rhandle = wdata.child_stdout_read
	}
	if idx == 2 {
		rhandle = wdata.child_stderr_read
	}
	if rhandle == 0 {
		return ''
	}
	mut bytes_read := u32(0)
	buf := [4096]u8{}
	mut read_data := strings.new_builder(1024)
	for {
		mut result := false
		unsafe {
			result = C.ReadFile(rhandle, &buf[0], 1000, voidptr(&bytes_read), 0)
			read_data.write_ptr(&buf[0], int(bytes_read))
		}
		if result == false || int(bytes_read) == 0 {
			break
		}
	}
	soutput := decode_windows_captured_output(read_data.str())
	unsafe { read_data.free() }
	//	if idx == 1 {
	//		close_valid_handle(&wdata.child_stdout_read)
	//	}
	//	if idx == 2 {
	//		close_valid_handle(&wdata.child_stderr_read)
	//	}
	return soutput
}

//
// these are here to make v_win.c/v.c generation work in all cases:
fn (mut p Process) unix_spawn_process() int {
	return 0
}

fn (mut p Process) unix_stop_process() {
}

fn (mut p Process) unix_resume_process() {
}

fn (mut p Process) unix_term_process() {
}

fn (mut p Process) unix_kill_process() {
}

fn (mut p Process) unix_kill_pgroup() {
}

fn (mut p Process) unix_wait() {
}

fn (mut p Process) unix_is_alive() bool {
	return false
}
