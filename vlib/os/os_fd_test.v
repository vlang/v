import os

// A temp root per test function, named with the pid so parallel runs on the
// same host cannot collide over the same directory.
fn fd_test_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'os_fd_tests_${os.getpid()}', name)
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn test_vfopen_fileno_fd_write_fd_slurp_roundtrip() {
	root := fd_test_root('roundtrip')
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'payload.bin')

	// NOTE: the mode is binary on purpose. `vfopen(path, 'w+')` opens in *text*
	// mode on Windows, so `\n` is written as `\r\n` and the byte count is off by
	// the number of newlines in the payload.
	fw := os.vfopen(path, 'wb+')!
	wfd := os.fileno(fw)
	assert wfd > 0
	os.fd_write(wfd, 'the quick brown fox\n')
	os.fd_write(wfd, 'and one more line')
	assert os.file_size(path) == 'the quick brown fox\nand one more line'.len

	fr := os.vfopen(path, 'rb')!
	rfd := os.fileno(fr)
	assert rfd > 0
	assert os.fd_slurp(rfd).join('') == 'the quick brown fox\nand one more line'
	// The descriptor is exhausted, so a second slurp reads nothing.
	assert os.fd_slurp(rfd) == []string{}
	assert os.fd_close(rfd) == 0
	assert os.fd_close(wfd) == 0
	assert os.is_file(path)
}

fn test_vfopen_rejects_empty_path_and_missing_file() {
	root := fd_test_root('vfopen_errors')
	defer {
		os.rmdir_all(root) or {}
	}
	os.vfopen('', 'r') or {
		assert err.msg() == 'vfopen called with ""'
		return
	}
	assert false, 'vfopen("") should have failed'

	missing := os.join_path(root, 'missing.bin')
	os.vfopen(missing, 'r') or {
		assert err.msg() == 'failed to open file "${missing}"'
		assert err.code() == 2 // ENOENT is 2 on every supported platform
		return
	}
	assert false, 'vfopen() on a missing file should have failed'
}

fn test_fd_is_pending_follows_bytes_waiting_in_a_pipe() {
	mut p := os.pipe()!
	defer {
		p.close()
	}
	assert !os.fd_is_pending(p.read_fd)
	os.fd_write(p.write_fd, 'abc')
	assert os.fd_is_pending(p.read_fd)
	s, n := os.fd_read(p.read_fd, 2)
	assert s == 'ab'
	assert n == 2
	assert os.fd_is_pending(p.read_fd)
	// A short read returns fewer bytes than requested, and then the pipe is empty.
	t, tn := os.fd_read(p.read_fd, 10)
	assert t[..tn] == 'c'
	assert !os.fd_is_pending(p.read_fd)
}

fn test_fd_dup_writes_through_a_duplicated_descriptor() {
	mut p := os.pipe()!
	defer {
		p.close()
	}
	os.fd_write(p.write_fd, 'abc')
	dupfd := os.fd_dup(p.write_fd)
	assert dupfd > 0
	// A duplicate refers to the same underlying pipe, so it continues the stream.
	os.fd_write(dupfd, 'de')
	assert os.fd_close(dupfd) == 0

	s, n := os.fd_read(p.read_fd, 10)
	assert s[..n] == 'abcde'
}

fn test_fd_helpers_treat_invalid_descriptors_as_no_ops() {
	// Invalid descriptors are recorded, not reported, so none of these panic.
	os.fd_write(-1, 'ignored')
	assert os.fd_slurp(-1) == []string{}
	assert os.fd_close(-1) == 0
	assert !os.fd_is_pending(-1)
}
