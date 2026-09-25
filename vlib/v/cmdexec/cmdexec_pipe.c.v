module cmdexec

import os

$if windows {
	#include <windows.h>

	fn C.PeekNamedPipe(handle voidptr, buffer voidptr, size i32, bytes_read voidptr, bytes_available voidptr, bytes_left voidptr) bool
} $else {
	// Picks <sys/poll.h> on glibc and <poll.h> elsewhere, including musl.
	#insert "@VEXEROOT/vlib/v/cmdexec/cmdexec_poll.h"

	struct C.pollfd {
		fd      int
		events  i16
		revents i16
	}

	fn C.poll(fds &C.pollfd, nfds u64, timeout int) int
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
		mut descriptor := C.pollfd{
			fd:     fd
			events: i16(C.POLLIN)
		}
		if C.poll(&descriptor, 1, 0) <= 0 {
			return '', false
		}
		if descriptor.revents & i16(C.POLLNVAL) != 0 {
			return '', true
		}
		text, count := os.fd_read(fd, 4096)
		return text, count <= 0
	}
}
