module os

fn C.fcntl(fd i32, cmd i32, arg ...voidptr) i32

fn test_pipe_descriptors_are_independent_and_reach_eof() {
	mut first := pipe()!
	mut second := pipe()!
	defer {
		first.close()
		second.close()
	}
	fds := [first.read_fd, first.write_fd, second.read_fd, second.write_fd]
	for i, fd in fds {
		assert fd > 2
		assert C.fcntl(fd, C.F_GETFD) >= 0
		assert fd !in fds[..i]
	}
	assert first.write_string('one')! == 3
	assert second.write_string('two')! == 3
	mut data := []u8{len: 8}
	assert first.read(mut data)! == 3
	assert data[..3].bytestr() == 'one'
	assert second.read(mut data)! == 3
	assert data[..3].bytestr() == 'two'
	assert fd_close(first.write_fd) == 0
	first.write_fd = -1
	flags := C.fcntl(first.read_fd, C.F_GETFL)
	assert C.fcntl(first.read_fd, C.F_SETFL, flags | C.O_NONBLOCK) == 0
	// A leaked writer returns EAGAIN here instead of EOF, without hanging the test.
	assert first.read(mut data)! == 0
}

fn test_process_pipe_descriptors_reach_eof_without_close() {
	for merged in [false, true] {
		mut child := new_process(find_abs_path_of_executable('sh')!)
		child.set_args(['-c', 'read value; printf "out:%s" "$value"; printf "err:%s" "$value" >&2'])
		if merged {
			child.set_redirect_stdio_merged()
		} else {
			child.set_redirect_stdio()
		}
		child.run()
		defer { child.close() }
		fds := if merged { child.stdio_fd[..2] } else { child.stdio_fd[..] }
		for i, fd in fds {
			assert fd > 2
			assert C.fcntl(fd, C.F_GETFD) >= 0
			assert fd !in fds[..i]
		}
		child.stdin_write('hello\n')
		child.wait()
		assert child.code == 0
		for fd in fds[1..] {
			flags := C.fcntl(fd, C.F_GETFL)
			assert C.fcntl(fd, C.F_SETFL, flags | C.O_NONBLOCK) == 0
		}
		expected := if merged { 'out:helloerr:hello' } else { 'out:hello' }
		assert child.stdout_slurp() == expected
		if !merged {
			assert child.stderr_slurp() == 'err:hello'
		}
		for fd in fds[1..] {
			_, count := fd_read(fd, 1)
			assert count == 0, 'expected EOF after child exit, got ${count}'
		}
	}
}
