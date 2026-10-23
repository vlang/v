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
