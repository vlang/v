import os

const freestanding_descriptor_program = '
$if vlibc ? {
fn control(fd int, command int, value int) int {
    $if macos {
        result, err := vlibc_sys_fcntl(i64(fd), command, voidptr(u64(value)))
        assert err == .enoerror
        return int(result)
    } $else {
        result, err := vlibc_sys_fcntl(i64(fd), command, i64(value))
        assert err == .enoerror
        return int(result)
    }
}
}

fn main() {
    mut bytes := [8]u8{}
    fd, opened := bf_openat(c"/dev/null", 0o2000000, 0)
    assert opened == 0
    n, ended := bf_read_ready(fd, &bytes[0], bytes.len)
    assert n == 0 && ended == 0
    closed := bf_close(fd)
    assert closed == 0
    invalid_n, invalid := bf_read_ready(fd, &bytes[0], bytes.len)
    assert invalid_n == -1 && invalid == 9
    negative_n, negative := bf_read_ready(-1, &bytes[0], -1)
    assert negative_n == -1 && negative == 22
    seconds, nanos, clock_error := bf_tai_now()
    if clock_error == 0 {
        assert nanos >= 0 && nanos < 1000000000
        _ = seconds
    } else {
        assert clock_error in [22, 38]
    }
    $if vlibc ? {
        pipe, pipe_error := vlibc_pipe()
        assert pipe_error == .enoerror
        read_fd := pipe[0]
        write_fd := pipe[1]
        original := control(read_fd, 3, 0)
        unavailable_n, unavailable := bf_read_ready(read_fd, &bytes[0], bytes.len)
        assert unavailable_n == -1 && unavailable == 11
        restored := control(read_fd, 3, 0)
        assert restored == original
        written, write_error := bf_write(write_fd, c"hi", 2)
        assert written == 2 && write_error == 0
        available_n, available := bf_read_ready(read_fd, &bytes[0], bytes.len)
        assert available_n == 2 && available == 0
        assert bytes[0] == 104 && bytes[1] == 105
        restored_after_read := control(read_fd, 3, 0)
        assert restored_after_read == original
        write_flags := control(write_fd, 3, 0)
        bad_n, bad_read := bf_read_ready(write_fd, &bytes[0], bytes.len)
        assert bad_n == -1 && bad_read == 9
        restored_after_error := control(write_fd, 3, 0)
        assert restored_after_error == write_flags
        nonblock := $if linux { 0x800 } $else { 0x4 }
        _ = control(read_fd, 4, original | nonblock)
        already_n, already_error := bf_read_ready(read_fd, &bytes[0], bytes.len)
        assert already_n == -1 && already_error == 11
        retained_nonblock := control(read_fd, 3, 0)
        assert retained_nonblock == (original | nonblock)
        write_closed := bf_close(write_fd)
        assert write_closed == 0
        eof_n, eof_error := bf_read_ready(read_fd, &bytes[0], bytes.len)
        assert eof_n == 0 && eof_error == 0
        read_closed := bf_close(read_fd)
        assert read_closed == 0
        cloexec_fd, cloexec_error := bf_openat(c"/dev/null", 0o2000000, 0)
        assert cloexec_error == 0
        descriptor_flags := control(cloexec_fd, 1, 0)
        assert descriptor_flags & 1 != 0
        cloexec_closed := bf_close(cloexec_fd)
        assert cloexec_closed == 0
    }
    println("descriptors ok")
}
'

fn test_freestanding_descriptor_readiness_and_tai() ! {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_descriptor_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'descriptor.v')
	os.write_file(source, freestanding_descriptor_program)!
	for with_vlibc in [false, true] {
		output := os.join_path(root, if with_vlibc { 'vlibc' } else { 'plain' })
		mut command := [@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
			'-o', output]
		if with_vlibc { command << ['-d', 'vlibc'] }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'descriptors ok', ran.output
	}
}
