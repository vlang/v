import os

// V's C for clang and MinGW on Windows doesn't include <stdio.h>, so the
// buffer modes for setvbuf come from manual_stdlib_c_headers.h. They must be the
// Microsoft C runtime's values: the UCRT rejects any others as an invalid
// parameter, which ends a V DLL's host program while builtin_init runs
// (vlang/v#32223).
fn test_windows_stdio_buffer_modes_match_the_microsoft_c_runtime() ! {
	clang := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_stdio_buffer_modes_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	header := os.join_path(@VEXEROOT, 'vlib/v/gen/c/manual_stdlib_c_headers.h')
	source := os.join_path(dir, 'modes.c')
	os.write_file(source, '#include <stdarg.h>
typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ isize;
typedef long long i64;
#include "${header}"
_Static_assert(_IOFBF == 0x0000, "_IOFBF");
_Static_assert(_IOLBF == 0x0040, "_IOLBF");
_Static_assert(_IONBF == 0x0004, "_IONBF");
')!
	for target in ['aarch64-w64-mingw32', 'x86_64-w64-mingw32'] {
		checked := os.exec([clang, '-target', target, '-std=gnu11', '-fsyntax-only', source])
		assert checked.exit_code == 0, '${target}: ${checked.output}'
	}
}
