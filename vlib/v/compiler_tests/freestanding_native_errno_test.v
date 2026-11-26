import os

const freestanding_native_errno_source = '
#include <errno.h>
#include <unistd.h>

fn main() {
    C.errno = 123
    // A child path of /dev/null cannot name an executable. This wrapper records
    // a normalized BF error without touching the native POSIX error cell.
    failed := bf_execve(c"/dev/null/vile", unsafe { nil }, unsafe { nil })
    assert failed == -1
    normalized := bf_last_errno()
    assert normalized > 0
    assert C.errno == 123
    written := C.write(-1, c"", 0)
    assert written == -1
    assert C.errno == C.EBADF
    assert bf_last_errno() == normalized
    fd, opened := bf_openat(c"/dev/null", 1, 0)
    assert opened == 0
    empty := C.write(i32(fd), c"", 0)
    assert empty == 0
    assert C.errno == C.EBADF
    closed := bf_close(fd)
    assert closed == 0
    println("native errno ok")
}
'

fn test_freestanding_native_errno_is_independent_of_normalized_bf_errors() ! {
	$if (!linux && !macos) || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_native_errno_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, freestanding_native_errno_source)!
	for with_vlibc in [false, true] {
		output := os.join_path(root, if with_vlibc { 'vlibc' } else { 'plain' })
		mut command := [@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
			'-cstrict', '-o', output]
		if with_vlibc { command << ['-d', 'vlibc'] }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'native errno ok', ran.output
	}
}
