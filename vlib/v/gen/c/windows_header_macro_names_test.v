module c

import os
import v.cmdexec

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
')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, '#include "${header.replace('\\', '/')}"

struct Values {
	near int
	far int
	pascal int
	cdecl int
}

fn near(value int) int { return value + 1 }
fn far(value int) int { return value + 2 }
fn pascal(value int) int { return value + 3 }
fn cdecl(value int) int { return value + 4 }
fn v_near(value int) int { return value + 5 }
fn v_far(value int) int { return value + 6 }
fn v_pascal(value int) int { return value + 7 }
fn v_cdecl(value int) int { return value + 8 }

fn local_values() int {
	near := 1
	far := 2
	pascal := 3
	cdecl := 4
	values := Values{near: near, far: far, pascal: pascal, cdecl: cdecl}
	return values.near + values.far + values.pascal + values.cdecl
}

fn main() {
	assert near(0) == 1
	assert far(0) == 2
	assert pascal(0) == 3
	assert cdecl(0) == 4
	callbacks := [near, far, pascal, cdecl, v_near, v_far, v_pascal, v_cdecl]
	for i, callback in callbacks { assert callback(0) == i + 1 }
	assert local_values() == 10
	println("ok")
}
')!
	executable := os.join_path(root, 'program' + $if windows { '.exe' } $else { '' })
	compiled := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-gc', 'none', '-o',
		executable, source])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(executable, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output.trim_space() == 'ok', executed.output
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
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-no-retry-compilation',
		'-os', 'windows', '-cc', 'tcc', '-gc', 'none', '-o', c_source, source])
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
