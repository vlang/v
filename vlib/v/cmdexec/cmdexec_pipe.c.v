module cmdexec

import os

$if windows {
	#include <windows.h>

	fn C.PeekNamedPipe(handle voidptr, buffer voidptr, size i32, bytes_read voidptr, bytes_available voidptr, bytes_left voidptr) bool
} $else {
	#include "@VMODROOT/vlib/v/cmdexec/cmdexec_poll.h"
	#flag @VMODROOT/vlib/v/cmdexec/cmdexec_wait.o

	fn C.v_cmdexec_poll_revents(fd int, events i16) i16

	@[c_extern]
	fn C.v_cmdexec_child_has_exited(pid i32) i32
}

// leader_has_ended tells whether the process of a run has ended, without reaping it: 1 when
// it has, 0 when it is running, -1 when that cannot be told without reaping (see
// cmdexec_wait.c). On Windows a process is not reaped by being asked: its handle keeps it.
fn leader_has_ended(mut process os.Process) int {
	$if windows {
		return if process.is_alive() { 0 } else { 1 }
	} $else {
		return C.v_cmdexec_child_has_exited(process.pid)
	}
}

// read_process_pipe reads without waiting and distinguishes EOF from an empty
// pipe that still has writers. Process.stdout_read alone conflates those cases.
fn read_process_pipe(mut process os.Process, kind os.ChildProcessPipeKind) (string, bool) {
	$if windows {
		if process.wdata == unsafe { nil } {
			return '', true
		}
		wdata := unsafe { &os.WProcess(process.wdata) }
		handle := if kind == .stdout { wdata.child_stdout_read } else { wdata.child_stderr_read }
		if handle == unsafe { nil } {
			return '', true
		}
		mut available := u32(0)
		if !C.PeekNamedPipe(handle, 0, 0, 0, voidptr(&available), 0) {
			return '', true
		}
		if available == 0 {
			return '', false
		}
		text := if kind == .stdout { process.stdout_read() } else { process.stderr_read() }
		return text, false
	} $else {
		fd := process.stdio_fd[kind]
		if fd < 0 {
			return '', true
		}
		// FIONREAD (used by os.fd_is_pending) reports zero for both an empty
		// open pipe and EOF. poll also reports the last writer's hangup.
		revents := C.v_cmdexec_poll_revents(fd, i16(1))
		if revents == 0 {
			return '', false
		}
		if revents & i16(0x20) != 0 {
			return '', true
		}
		text, count := os.fd_read(fd, 4096)
		return text, count <= 0
	}
}
