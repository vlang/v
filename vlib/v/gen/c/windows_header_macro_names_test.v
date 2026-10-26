module c

import os
import v.cmdexec

// windows_header_macro_program returns a program whose identifiers collide with the
// macros of windef.h. `include` is the C directive that brings those macros in.
fn windows_header_macro_program(include string) string {
	return '${include}

struct Values {
	near int
	far int
	pascal int
	cdecl int
	min int
	max int
}

fn near(value int) int { return value + 1 }
fn far(value int) int { return value + 2 }
fn pascal(value int) int { return value + 3 }
fn cdecl(value int) int { return value + 4 }
fn v_near(value int) int { return value + 5 }
fn v_far(value int) int { return value + 6 }
fn v_pascal(value int) int { return value + 7 }
fn v_cdecl(value int) int { return value + 8 }
// The function-like macros reject one parameter here and two in `max`.
fn min(value int) int { return value + 9 }
fn max(value int, other int) int { return value + other + 10 }
fn v_min(value int) int { return value + 11 }
fn v_max(value int) int { return value + 12 }

fn local_values() int {
	near := 1
	far := 2
	pascal := 3
	cdecl := 4
	min := 5
	max := 6
	values := Values{near: near, far: far, pascal: pascal, cdecl: cdecl, min: min, max: max}
	return values.near + values.far + values.pascal + values.cdecl + values.min + values.max
}

fn main() {
	assert near(0) == 1
	assert far(0) == 2
	assert pascal(0) == 3
	assert cdecl(0) == 4
	assert min(0) == 9
	assert max(0, 0) == 10
	assert v_min(0) == 11
	assert v_max(0) == 12
	callbacks := [near, far, pascal, cdecl, v_near, v_far, v_pascal, v_cdecl]
	for i, callback in callbacks { assert callback(0) == i + 1 }
	min_callback := min
	max_callback := max
	assert min_callback(1) == 10
	assert max_callback(1, 2) == 13
	assert local_values() == 21
	println("ok")
}
'
}

// compile_and_run_windows_header_macro_program builds the program with the v3 backend
// only: the legacy fallback compiler prefixes main module functions with `main__` and
// would hide a bare `min(` in the v3 output.
fn compile_and_run_windows_header_macro_program(root string, include string) ! {
	source := os.join_path(root, 'main.v')
	os.write_file(source, windows_header_macro_program(include))!
	executable := os.join_path(root, 'program' + $if windows { '.exe' } $else { '' })
	compiled := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-no-retry-compilation', '-gc',
		'none', '-o', executable, source])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(executable, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output.trim_space() == 'ok', executed.output
}

fn test_windows_header_macro_names_compile_and_run() ! {
	root := os.join_path(os.vtmp_dir(), 'windows_header_macro_names_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// Reproduce windef.h's macros on every host so this regression runs off Windows too.
	header := os.join_path(root, 'windows_macros.h')
	os.write_file(header, '#ifndef near
#define near
#endif
#ifndef far
#define far
#endif
#ifndef pascal
#define pascal
#endif
#ifndef cdecl
#define cdecl
#endif
#ifndef min
#define min(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef max
#define max(a, b) (((a) > (b)) ? (a) : (b))
#endif
')!
	compile_and_run_windows_header_macro_program(root, '#include "${header.replace('\\', '/')}"')!
}

fn test_windows_header_macro_names_compile_and_run_with_the_windows_sdk() ! {
	$if !windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'windows_sdk_macro_names_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// The real headers: `fn min(tree Tree)` used to fail with `macro 'min' used with too few args`.
	compile_and_run_windows_header_macro_program(root, '#include <windows.h>')!
}

fn test_windows_tcc_alloca_cannot_be_shadowed_by_v_locals() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_tcc_alloca_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[aligned: 512]
struct Cell { value int }
fn ordinary(seed int) int {
	alloca := seed
	v_alloca := 8
	__v3_internal_symbol_libc_alloca := 9
	cell := Cell{value: alloca}
	return cell.value + alloca + v_alloca + __v3_internal_symbol_libc_alloca + int(uint(voidptr(&cell)) & 511)
}
fn staging(seed int) int {
	alloca := seed
	v_alloca := 8
	__v3_internal_symbol_libc_alloca := 9
	cell := if alloca == 7 { Cell{value: alloca} } else { Cell{value: 0} }
	return cell.value + alloca + v_alloca + __v3_internal_symbol_libc_alloca + int(uint(voidptr(&cell)) & 511)
}
fn main() { _ := ordinary(7); _ := staging(7) }
')!
	c_source := os.join_path(root, 'generated.c')
	// Pin the arch: on amd64 hosts the driver replaces `-cc tcc` with MinGW gcc for Windows targets,
	// which never takes the TinyCC alloca path. No MinGW exists for arm64, so tcc is kept.
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-no-retry-compilation',
		'-os', 'windows', '-arch', 'arm64', '-cc', 'tcc', '-gc', 'none', '-o', c_source, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(c_source)!
	mut bodies := []string{}
	for name in ['ordinary', 'staging'] {
		signature := 'i64 ${name}(i64 seed) {'
		assert generated.contains(signature), signature
		body := signature + generated.all_after(signature).all_before('\n}') + '\n}\n'
		assert body.contains('alloca(sizeof(main__Cell)'), body
		bodies << body
	}
	// Run the actual TinyCC-mode bodies without requiring a Windows SDK. The object-like
	// macro renames both declarations and calls, preserving an alloca-local collision.
	host_source := os.join_path(root, 'host.c')
	os.write_file(host_source, '#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#undef alloca
#define alloca __builtin_alloca
typedef int64_t i64;
typedef struct __attribute__((aligned(512))) { i64 value; } main__Cell;
${bodies.join('\n')}
int main(void) {
	assert(ordinary(7) == 31);
	assert(staging(7) == 31);
	assert(staging(5) == 22);
	return 0;
}
')!
	executable := os.join_path(root, 'program' + $if windows { '.exe' } $else { '' })
	compiled := cmdexec.run(cc, ['-std=gnu11', host_source, '-o', executable])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(executable, [])
	assert executed.exit_code == 0, executed.output
}
