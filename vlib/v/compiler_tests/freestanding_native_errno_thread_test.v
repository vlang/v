import os

const freestanding_native_errno_thread_source = '
#include <errno.h>
#include <unistd.h>

struct ErrnoProbe {
mut:
    ready u32
    release u32
    marker i32
}

fn errno_worker(address u64) {
    probe := unsafe { &ErrnoProbe(voidptr(address)) }
    C.errno = probe.marker
    unavailable := bf_execve(c"/dev/null/vile", unsafe { nil }, unsafe { nil })
    assert unavailable == -1 && bf_last_errno() == 20
    assert C.errno == probe.marker
    vlibc_store_u32(&probe.ready, 1)
    for vlibc_load_u32(&probe.release) == 0 {}
    assert C.errno == probe.marker
    assert bf_last_errno() == 20
    failed := C.write(-1, c"", 0)
    assert failed == -1 && C.errno == C.EBADF
    assert bf_last_errno() == 20
}

fn main() {
    mut first := ErrnoProbe{marker: 101}
    mut second := ErrnoProbe{marker: 202}
    mut a, a_error := vlibc_thread_spawn(errno_worker, u64(voidptr(&first)))
    assert a_error == .enoerror
    mut b, b_error := vlibc_thread_spawn(errno_worker, u64(voidptr(&second)))
    assert b_error == .enoerror
    C.errno = 77
    for vlibc_load_u32(&first.ready) == 0 || vlibc_load_u32(&second.ready) == 0 {}
    assert C.errno == 77
    close_error := bf_close(-1)
    assert close_error == 9 && bf_last_errno() == 9
    assert C.errno == 77
    vlibc_store_u32(&first.release, 1)
    vlibc_store_u32(&second.release, 1)
    a_joined := vlibc_thread_join(mut a)
    b_joined := vlibc_thread_join(mut b)
    assert a_joined == .enoerror && b_joined == .enoerror
    assert bf_last_errno() == 9 && C.errno == 77
    println("thread errno ok")
}
'

fn test_freestanding_native_errno_is_separate_for_concurrent_threads() ! {
	$if (!linux && !macos) || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_thread_errno_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, freestanding_native_errno_thread_source)!
	output := os.join_path(root, 'threads')
	built := os.exec([@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
		'-d', 'vlibc', '-cstrict', '-o', output, source])
	assert built.exit_code == 0, built.output
	ran := os.exec([output])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'thread errno ok', ran.output
}
