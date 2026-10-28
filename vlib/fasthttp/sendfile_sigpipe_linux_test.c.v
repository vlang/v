// vtest build: linux
module fasthttp

import net
import os
import time

fn C.pthread_kill(thread uint, sig int) int

struct SendfileSignalState {
	sent            int
	mask_preserved  bool
	pending_sigpipe bool
}

fn sendfile_signal_state(socket_fd int, file_fd int, keep_pending bool) SendfileSignalState {
	mut sigpipe_set := C.sigset_t{}
	mut previous_mask := C.sigset_t{}
	C.sigemptyset(&sigpipe_set)
	assert C.sigaddset(&sigpipe_set, C.SIGPIPE) == 0
	assert C.pthread_sigmask(C.SIG_BLOCK, &sigpipe_set, &previous_mask) == 0
	defer {
		// Consume a signal retained by the test before restoring an unblocked mask.
		timeout := C.timespec{}
		C.sigtimedwait(&sigpipe_set, unsafe { nil }, &timeout)
		C.pthread_sigmask(C.SIG_SETMASK, &previous_mask, unsafe { nil })
	}
	if keep_pending {
		// Queue a thread-local application signal before entering the helper.
		assert C.pthread_kill(C.pthread_self(), C.SIGPIPE) == 0
	}
	mut offset := i64(0)
	sent := sendfile_without_sigpipe(socket_fd, file_fd, &offset, 1024 * 1024)
	mut current_mask := C.sigset_t{}
	assert C.pthread_sigmask(C.SIG_SETMASK, unsafe { nil }, &current_mask) == 0
	mut pending := C.sigset_t{}
	assert C.sigpending(&pending) == 0
	return SendfileSignalState{
		sent:            sent
		mask_preserved:  C.sigismember(&current_mask, C.SIGPIPE) == 1
		pending_sigpipe: C.sigismember(&pending, C.SIGPIPE) == 1
	}
}

// sendfile can return partial success and leave SIGPIPE pending after a TCP FIN.
// Checking EPIPE or a nonpositive return misses that signal (#31228).
fn test_partial_sendfile_drains_sigpipe_and_preserves_existing_pending_signal() ! {
	path := os.join_path(os.vtmp_dir(), 'fasthttp_sigpipe_partial.bin')
	os.write_file(path, 'x'.repeat(1024 * 1024))!
	defer { os.rm(path) or {} }
	mut file := os.open(path)!
	defer { file.close() }
	for keep_pending in [false, true] {
		mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
		defer { listener.close() or {} }
		address := listener.addr()!
		mut client := net.dial_tcp(address.str())!
		mut server := listener.accept()!
		defer { server.close() or {} }
		// A small queue ensures the entire file cannot be buffered before the reset.
		send_buffer := i32(4096)
		assert C.setsockopt(server.sock.handle, C.SOL_SOCKET, C.SO_SNDBUF, &send_buffer,
			sizeof(i32)) == 0
		client.close()!
		time.sleep(50 * time.millisecond)
		worker := spawn sendfile_signal_state(server.sock.handle, file.fd, keep_pending)
		state := worker.wait()
		assert state.sent > 0 && state.sent < 1024 * 1024
		assert state.mask_preserved
		assert state.pending_sigpipe == keep_pending
	}
}
