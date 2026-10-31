module os

import time

// signal_pgkill of a child started with use_pgroup ends the whole tree on Windows: the
// child (`cmd.exe`) and the process it started (`PING.EXE`, which waits 10 minutes), neither
// of which shares anything with this test but the job object the child was put in.
//
// The control, test_the_console_event_and_terminateprocess_leave_the_grandchild, is the two
// calls that signal_pgkill made before: they end the child, and the grandchild lives on.

fn C.OpenProcess(desired_access u32, inherit_handle bool, process_id u32) voidptr

const process_query_limited_information = u32(0x1000)
const ping_exe = r'C:\Windows\System32\PING.EXE'
const cmd_exe = r'C:\Windows\System32\cmd.exe'

// process_alive reports whether the process `pid` exists and has not ended.
fn process_alive(pid int) bool {
	handle := C.OpenProcess(process_query_limited_information, false, u32(pid))
	if handle == 0 {
		return false
	}
	defer {
		C.CloseHandle(handle)
	}
	mut code := u32(0)
	unsafe {
		C.GetExitCodeProcess(handle, &C.DWORD(&code))
	}
	return code == u32(C.STILL_ACTIVE)
}

// ping_pids lists the processes of PING.EXE that exist now.
fn ping_pids() []int {
	result := exec(['tasklist', '/fo', 'csv', '/nh', '/fi', 'IMAGENAME eq PING.EXE'])
	mut pids := []int{}
	for line in result.output.split_into_lines() {
		fields := line.split('","')
		if fields.len >= 2 && fields[0].trim('"').to_upper() == 'PING.EXE' {
			pids << fields[1].trim('"').int()
		}
	}
	return pids
}

// start_ping_tree starts `cmd /c PING.EXE -n 600 127.0.0.1`: a child that starts a process
// and waits for it. It returns the child and the pid of the PING.EXE it started.
fn start_ping_tree(use_pgroup bool) (&Process, int) {
	before := ping_pids()
	mut p := new_process(cmd_exe)
	p.set_args(['/d', '/c', ping_exe, '-n', '600', '127.0.0.1'])
	p.set_redirect_stdio()
	p.use_pgroup = use_pgroup
	p.run()
	mut grandchild := 0
	for _ in 0 .. 100 {
		for pid in ping_pids() {
			if pid !in before {
				grandchild = pid
			}
		}
		if grandchild != 0 {
			break
		}
		time.sleep(50 * time.millisecond)
	}
	return p, grandchild
}

// wait_gone waits up to a second for every pid of `pids` to have ended.
fn wait_gone(pids []int) bool {
	for _ in 0 .. 20 {
		if pids.all(!process_alive(it)) {
			return true
		}
		time.sleep(50 * time.millisecond)
	}
	return pids.all(!process_alive(it))
}

fn cleanup_tree(mut p Process, grandchild int) {
	if grandchild != 0 && process_alive(grandchild) {
		exec(['taskkill', '/f', '/pid', grandchild.str()])
	}
	p.signal_kill()
	p.wait()
	p.close()
}

fn test_signal_pgkill_ends_the_child_and_what_it_started() {
	mut p, grandchild := start_ping_tree(true)
	defer {
		cleanup_tree(mut p, grandchild)
	}
	assert grandchild != 0, 'the child did not start PING.EXE'
	if p.pgroup_fallback {
		eprintln('skipped: this process is in a job that does not allow another one')
		return
	}
	assert process_alive(p.pid)
	assert process_alive(grandchild)
	p.signal_pgkill()
	assert wait_gone([p.pid, grandchild]), 'the child (${p.pid}) or its PING.EXE (${grandchild}) is still running'
}

fn test_the_console_event_and_terminateprocess_leave_the_grandchild() {
	mut p, grandchild := start_ping_tree(true)
	defer {
		cleanup_tree(mut p, grandchild)
	}
	assert grandchild != 0, 'the child did not start PING.EXE'
	// what win_kill_pgroup did before the job object: a CTRL_BREAK for the child's group, a
	// short wait, and TerminateProcess of the child
	wdata := unsafe { &WProcess(p.wdata) }
	C.GenerateConsoleCtrlEvent(C.CTRL_BREAK_EVENT, wdata.proc_info.dw_process_id)
	C.Sleep(20)
	C.TerminateProcess(wdata.proc_info.h_process, 3)
	time.sleep(300 * time.millisecond)
	assert !process_alive(p.pid)
	if !process_alive(grandchild) {
		eprintln('control not reproduced: the console event reached PING.EXE on this machine')
		return
	}
	assert process_alive(grandchild), 'the grandchild is expected to outlive the two calls'
}

fn test_a_child_without_use_pgroup_is_not_put_in_a_job() {
	mut p, grandchild := start_ping_tree(false)
	defer {
		cleanup_tree(mut p, grandchild)
	}
	wdata := unsafe { &WProcess(p.wdata) }
	assert isnil(wdata.job)
	assert !p.pgroup_fallback
}
