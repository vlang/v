module os

import strings

fn C.GenerateConsoleCtrlEvent(event u32, pgid u32) bool
fn C.GetModuleHandleA(name &char) HMODULE
fn C.GetProcAddress(handle voidptr, procname &u8) voidptr
fn C.TerminateProcess(process HANDLE, exit_code u32) bool
fn C.PeekNamedPipe(hNamedPipe voidptr, lpBuffer voidptr, nBufferSize int, lpBytesRead voidptr, lpTotalBytesAvail voidptr,
	lpBytesLeftThisMessage voidptr) bool

type FN_NTSuspendResume = fn (voidptr) u64

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
	proc_info    ProcessInformation
	command_line [65536]u8
	child_stdin  &u32 = unsafe { nil }

	child_stdout_read  &u32 = unsafe { nil }
	child_stdout_write &u32 = unsafe { nil }

	child_stderr_read  &u32 = unsafe { nil }
	child_stderr_write &u32 = unsafe { nil }
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
	p.filename = abs_path(p.filename) // expand the path to an absolute one, in case we later change the working folder
	mut wdata := &WProcess{
		child_stdin:        unsafe { nil }
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
	if p.use_stdio_ctl {
		mut sa := SecurityAttributes{}
		sa.n_length = sizeof(C.SECURITY_ATTRIBUTES)
		sa.b_inherit_handle = true
		create_pipe_ok1 := C.CreatePipe(voidptr(&wdata.child_stdout_read), voidptr(&wdata.child_stdout_write),
			voidptr(&sa), 65536)
		failed_cfn_report_error(create_pipe_ok1, 'CreatePipe stdout')
		set_handle_info_ok1 := C.SetHandleInformation(wdata.child_stdout_read, C.HANDLE_FLAG_INHERIT,
			0)
		failed_cfn_report_error(set_handle_info_ok1, 'SetHandleInformation')
		create_pipe_ok2 := C.CreatePipe(voidptr(&wdata.child_stderr_read), voidptr(&wdata.child_stderr_write),
			voidptr(&sa), 65536)
		failed_cfn_report_error(create_pipe_ok2, 'CreatePipe stderr')
		set_handle_info_ok2 := C.SetHandleInformation(wdata.child_stderr_read, C.HANDLE_FLAG_INHERIT,
			0)
		failed_cfn_report_error(set_handle_info_ok2, 'SetHandleInformation stderr')
		start_info.h_std_input = wdata.child_stdin
		start_info.h_std_output = wdata.child_stdout_write
		start_info.h_std_error = wdata.child_stderr_write
		start_info.dw_flags = u32(C.STARTF_USESTDHANDLES)
	}
	cmd := '${p.filename} ' + p.args.join(' ')
	cmd_wide_ptr := cmd.to_wide()
	to_be_freed << cmd_wide_ptr
	C.ExpandEnvironmentStringsW(cmd_wide_ptr, voidptr(&wdata.command_line[0]), 32768)

	mut creation_flags := if p.create_no_window {
		int(C.CREATE_NO_WINDOW)
	} else {
		int(C.NORMAL_PRIORITY_CLASS)
	}
	if p.use_pgroup {
		creation_flags |= C.CREATE_NEW_PROCESS_GROUP
	}

	mut work_folder_ptr := voidptr(unsafe { nil })
	if p.work_folder != '' {
		work_folder_ptr = p.work_folder.to_wide()
		to_be_freed << work_folder_ptr
	}

	mut env_block := []u16{}
	if p.env.len > 0 {
		mut env_ptr := &u16(unsafe { nil })

		for e in p.env {
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
		defer {
			unsafe { env_block.free() }
		}
	}

	create_process_ok := C.CreateProcessW(0, voidptr(&wdata.command_line[0]), 0, 0, C.TRUE,
		creation_flags, if env_block.len > 0 { env_block.data } else { 0 }, work_folder_ptr,
		voidptr(&start_info), voidptr(&wdata.proc_info))
	failed_cfn_report_error(create_process_ok, 'CreateProcess')
	if p.use_stdio_ctl {
		close_valid_handle(&wdata.child_stdout_write)
		close_valid_handle(&wdata.child_stderr_write)
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
	C.GenerateConsoleCtrlEvent(C.CTRL_BREAK_EVENT, wdata.proc_info.dw_process_id)
	C.Sleep(20)
	C.TerminateProcess(wdata.proc_info.h_process, 3)
}

fn (mut p Process) win_wait() {
	exit_code := u32(1)
	mut wdata := unsafe { &WProcess(p.wdata) }
	if p.wdata != 0 {
		C.WaitForSingleObject(wdata.proc_info.h_process, C.INFINITE)
		C.GetExitCodeProcess(wdata.proc_info.h_process, voidptr(&exit_code))
		close_valid_handle(&wdata.child_stdin)
		close_valid_handle(&wdata.child_stdout_write)
		close_valid_handle(&wdata.child_stderr_write)
		close_valid_handle(&wdata.proc_info.h_process)
		close_valid_handle(&wdata.proc_info.h_thread)
	}
	p.status = .exited
	p.code = int(exit_code)
}

fn (mut p Process) win_is_alive() bool {
	exit_code := u32(0)
	wdata := unsafe { &WProcess(p.wdata) }
	C.GetExitCodeProcess(wdata.proc_info.h_process, voidptr(&exit_code))
	if exit_code == C.STILL_ACTIVE {
		return true
	}
	return false
}

///////////////

fn (mut p Process) win_write_string(idx int, _s string) {
	panic_n('Process.write_string is not implemented yet, idx:', idx)
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
	return buf[..bytes_read].bytestr(), bytes_read
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
	if C.PeekNamedPipe(rhandle, 0, 0, 0, &bytes_avail, 0) {
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
	soutput := read_data.str()
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
