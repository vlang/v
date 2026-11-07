module c

import os
import v.cmdexec

fn test_manual_stdio_declarations_preserve_function_like_macros() ! {
	$if !linux && !openbsd && !freebsd {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'stdio_macro_declarations_' + os.getpid().str())
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'stdio.c')
	// BSD stdio.h defines these macros after declaring the real functions.
	// User headers can include stdio.h before the manual libc preamble is emitted.
	os.write_file(source, '
#include <stddef.h>
#include <stdint.h>
typedef intptr_t isize;
typedef unsigned short u16;
extern int v_test_stdio_isthreaded;
#define getc(p) (!v_test_stdio_isthreaded ? v_test_sgetc(p) : (getc)(p))
#define feof(p) (!v_test_stdio_isthreaded ? v_test_sfeof(p) : (feof)(p))
#define ferror(p) (!v_test_stdio_isthreaded ? v_test_sferror(p) : (ferror)(p))
#define clearerr(p) (!v_test_stdio_isthreaded ? v_test_sclearerr(p) : (clearerr)(p))
#define getchar() getc(stdin)
#define putchar(c) putc(c, stdout)
' + manual_stdlib_c_headers() + '
int putc(int, FILE *);
int v_test_sgetc(FILE *);
int v_test_sfeof(FILE *);
int v_test_sferror(FILE *);
void v_test_sclearerr(FILE *);
int (*read_character)(void) = getchar;
int (*write_character)(int) = putchar;
int (*read_stream)(FILE *) = getc;
int (*stream_eof)(FILE *) = feof;
int (*stream_error)(FILE *) = ferror;
void (*clear_stream_error)(FILE *) = clearerr;
int read_macro(void) { return getchar(); }
int write_macro(int ch) { return putchar(ch); }
int read_stream_macro(FILE *stream) { return getc(stream); }
int stream_eof_macro(FILE *stream) { return feof(stream); }
int stream_error_macro(FILE *stream) { return ferror(stream); }
void clear_stream_error_macro(FILE *stream) { clearerr(stream); }
#ifndef getc
#error getc macro was removed
#endif
#ifndef feof
#error feof macro was removed
#endif
#ifndef ferror
#error ferror macro was removed
#endif
#ifndef clearerr
#error clearerr macro was removed
#endif
#ifndef getchar
#error getchar macro was removed
#endif
#ifndef putchar
#error putchar macro was removed
#endif
')!
	result := cmdexec.run(cc, ['-Werror', '-c', source, '-o', os.join_path(dir, 'stdio.o')])
	assert result.exit_code == 0, result.output
}

fn test_openbsd_tcc_fence_with_and_without_a_stdatomic_declaration() ! {
	$if !amd64 {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'openbsd_fence_declarations_' + os.getpid().str())
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut g := FlatGen.new()
	g.atomic_thread_fence_compat_decls()
	code := g.sb.str()
	for has_header in [false, true] {
		prefix := if has_header {
			'typedef enum { relaxed = 0, sequential = 5 } memory_order;
extern void __atomic_thread_fence(memory_order order);
#define atomic_thread_fence(order) __atomic_thread_fence(order)
'
		} else {
			''
		}
		source := os.join_path(dir, 'fence_' + has_header.str() + '.c')
		os.write_file(source, prefix + code + '
void probe_fences(void) {
	atomic_thread_fence(memory_order_seq_cst);
	__atomic_thread_fence(memory_order_relaxed);
}
')!
		result := cmdexec.run(tcc, ['-U__linux__', '-U__APPLE__', '-U_WIN32', '-U__FreeBSD__',
			'-D__OpenBSD__', '-Werror', '-c', source, '-o', os.join_path(dir, 'fence.o')])
		assert result.exit_code == 0, result.output
	}
}
