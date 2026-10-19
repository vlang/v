import os

#include <signal.h>
#include <sys/stat.h>

fn C.mkfifo(&char, u32) i32

fn test_write_file_array_closes_file_after_broken_pipe() {
	root := os.join_path(os.vtmp_dir(), 'write_array_fifo_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// A closed FIFO reader makes the write fail; ignore and later restore SIGPIPE.
	previous := unsafe { C.signal(C.SIGPIPE, voidptr(C.SIG_IGN)) }
	assert previous != unsafe { voidptr(-1) }
	defer { unsafe { C.signal(C.SIGPIPE, previous) } }
	arr := []u8{len: 4 * 1024 * 1024, init: 0x41}
	before := os.ls('/dev/fd')!.len
	for i in 0 .. 8 {
		fifo := os.join_path(root, 'closed_reader_${i}')
		assert C.mkfifo(&char(fifo.str), u32(0o600)) == 0
		// The child uses libc only after fork, then exits without running V cleanup.
		pid := unsafe { C.fork() }
		assert pid >= 0
		if pid == 0 {
			fd := unsafe { C.open(&char(fifo.str), C.O_RDONLY) }
			if fd < 0 { unsafe { C._exit(1) } }
			unsafe {
				C.close(fd)
				C._exit(0)
			}
		}
		mut failed := false
		os.write_file_array(fifo, arr) or { failed = true }
		mut status := 0
		assert unsafe { C.waitpid(pid, &status, 0) } == pid
		assert status == 0
		assert failed
	}
	assert os.ls('/dev/fd')!.len == before
}
