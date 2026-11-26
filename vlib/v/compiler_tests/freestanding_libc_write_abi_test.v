import os

const freestanding_write_main = '
#include <unistd.h>
#include <errno.h>
#include "@DIR/write_check.h"
#flag @DIR/write_check.c

fn C.v_freestanding_write_check(i32) i32

fn main() {
    fd, opened := bf_openat(c"/dev/null", 1, 0)
    assert opened == 0
    checked := C.v_freestanding_write_check(i32(fd))
    assert checked == 0
    $if linux {
        direct_count, direct_error := sys_write(i64(fd), c"", 0)
        assert direct_count == 0 && direct_error == .enoerror
    }
    bf_count, bf_error := bf_write(fd, c"", 0)
    assert bf_count == 0 && bf_error == 0
    closed := bf_close(fd)
    assert closed == 0
    println("write ABI ok")
}
'

const freestanding_write_c = '
#include <errno.h>
#include <unistd.h>
#include "write_check.h"

int v_freestanding_write_check(int fd) {
    static const unsigned char data[] = { 65, 0, 66 };
    ssize_t (*const writer)(int, const void *, size_t) = write;
    errno = 123;
    if (writer(fd, data, sizeof data) != (ssize_t)sizeof data) return 1;
    if (errno != 123) return 2;
    if (writer(fd, data, 0) != 0) return 3;
    if (errno != 123) return 4;
    if (writer(-1, data, sizeof data) != -1) return 5;
    if (errno != EBADF) return 6;
    return 0;
}
'

fn test_freestanding_write_export_matches_headers_and_external_c_abi() ! {
	$if (!linux && !macos) || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_write_abi_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, freestanding_write_main)!
	os.write_file(os.join_path(root, 'write_check.h'), 'int v_freestanding_write_check(int fd);\n')!
	os.write_file(os.join_path(root, 'write_check.c'), freestanding_write_c)!
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
		assert ran.output.trim_space() == 'write ABI ok', ran.output
	}
}
