import os
import v.cmdexec

fn test_manual_crt_declarations_compile_after_c23_string_macros() {
	$if !linux && !macos {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'cgen_c23_string_macros_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	header := os.join_path(test_dir, 'string_macros.h')
	// Reproduce the C23 function-like _Generic spelling even on older libc.
	// Parenthesized names inside each macro still refer to the libc functions.
	os.write_file(header, '#include <string.h>
#undef memchr
#undef strchr
#undef strrchr
#undef strstr
#define memchr(s, c, n) _Generic((s), default: (memchr))((s), (c), (n))
#define strchr(s, c) _Generic((s), default: (strchr))((s), (c))
#define strrchr(s, c) _Generic((s), default: (strrchr))((s), (c))
#define strstr(s, n) _Generic((s), default: (strstr))((s), (n))
static inline int v_c23_string_macros_work(void) {
    char text[] = "hello";
    return memchr(text, \'l\', 5) == text + 2 &&
        strchr(text, \'l\') == text + 2 &&
        strrchr(text, \'l\') == text + 3 &&
        strstr(text, "ell") == text + 1;
}
')!
	v_file := os.join_path(test_dir, 'main.c.v')
	// A preinclude places the macros before the manual CRT declarations, matching
	// the glibc 2.43 build failure in https://github.com/vlang/v/issues/28384.
	os.write_file(v_file, '#preinclude "${header.replace('\\', '/')}"
fn C.v_c23_string_macros_work() int
fn main() {
	assert C.v_c23_string_macros_work() == 1
}
')!
	for name in ['gcc', 'clang'] {
		compiler := os.find_abs_path_of_executable(name) or { continue }
		c_file := os.join_path(test_dir, '${name}.c')
		generated := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none', '-cc', compiler,
			'-o', c_file, v_file])
		assert generated.exit_code == 0, generated.output
		compiled := cmdexec.run(compiler, ['-std=gnu11', '-fsyntax-only', c_file])
		assert compiled.exit_code == 0, '${name}: ${compiled.output}'
	}
}
