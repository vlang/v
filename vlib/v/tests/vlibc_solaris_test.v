// Solaris amd64 regressions for the raw vlibc syscall ABI.
import os

const vlibc_solaris_program = "fn check(e VlibcErrno) {
	if e != .enoerror { exit(80) }
}

fn main() {
	assert sizeof(SolStatRaw) == 128
	assert __offsetof(SolStatRaw, blksize) == 96
	assert __offsetof(SolStatRaw, blocks) == 104
	assert __offsetof(SolStatRaw, fstype) == 112
	path := arguments()[1]
	check(vlibc_write_file(path, 'longer'.bytes()))
	check(vlibc_write_file(path, 'ok'.bytes()))
	data, read_error := vlibc_read_file(path)
	check(read_error)
	assert data.bytestr() == 'ok'
	stat, stat_error := vlibc_stat(path)
	check(stat_error)
	assert stat.size == 2
	fd, open_error := vlibc_open(path, 0, 0)
	check(open_error)
	offset, seek_error := vlibc_sys_lseek(fd, 1, 0)
	check(seek_error)
	assert offset == 1
	mut marker := u8(0)
	n, read_marker_error := vlibc_sys_read(fd, &marker, 1)
	check(read_marker_error)
	assert n == 1 && marker == 107
	check(vlibc_close(fd))

	pipe, pipe_error := vlibc_pipe()
	check(pipe_error)
	assert pipe[0] != pipe[1] && pipe[0] > 2 && pipe[1] > 2
	copied, copy_error := vlibc_dup2(pipe[0], 100)
	check(copy_error)
	assert copied == 100
	same, same_error := vlibc_dup2(100, 100)
	check(same_error)
	assert same == 100
	check(vlibc_close(100))
	_, invalid := vlibc_dup2(100, 100)
	assert invalid == .ebadf
	check(vlibc_close(i64(pipe[0])))
	check(vlibc_close(i64(pipe[1])))

	// Solaris native AF_UNIX=1 and SOCK_STREAM=2.
	pair, pair_error := vlibc_socketpair(1, 2, 0)
	check(pair_error)
	assert pair[0] != pair[1] && pair[0] > 2 && pair[1] > 2
	written, write_error := vlibc_sys_write(i64(pair[0]), c'pair', 4)
	check(write_error)
	assert written == 4
	mut bytes := [4]u8{}
	received, recv_error := vlibc_sys_read(i64(pair[1]), &bytes[0], 4)
	check(recv_error)
	assert received == 4 && bytes[0] == 112 && bytes[3] == 114
	check(vlibc_close(i64(pair[0])))
	check(vlibc_close(i64(pair[1])))

	mapping, map_error := vlibc_sys_mmap(unsafe { nil }, 4096, 3, 0x102, -1, 0)
	check(map_error)
	unsafe { (&u8(mapping))[0] = 123 }
	check(vlibc_sys_madvise(mapping, 4096, 0))
	assert unsafe { (&u8(mapping))[0] } == 123
	check(vlibc_sys_munmap(mapping, 4096))
	missing := arguments()[2]
	assert vlibc_execve(missing, [missing], []) == -int(VlibcErrno.enoent)
	println('Solaris files, descriptors, sockets, and mappings ok')
}
"

fn test_vlibc_freestanding_solaris() {
	$if !solaris || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vlibc_solaris_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, vlibc_solaris_program)!
	path := os.join_path(root, 'fixture.txt')
	for optimized in [false, true] {
		exe := os.join_path(root, 'main_${optimized}')
		mut args := [@VEXE, '-nocache', '-freestanding', '-d', 'vlibc']
		if optimized { args << '-prod' }
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, 'prod=${optimized}: ${build.output}'
		run := os.exec([exe, path, os.join_path(root, 'absent')])
		assert run.exit_code == 0, 'prod=${optimized}: ${run.output}'
		assert run.output == 'Solaris files, descriptors, sockets, and mappings ok\n', run.output
		os.rm(path)!
	}
}
