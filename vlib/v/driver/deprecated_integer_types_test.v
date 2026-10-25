module driver

import os
import v.cmdexec

fn test_platform_signed_c_binding_preserves_values_above_32_bits() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'platform_signed_c_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'count.h'), 'static inline __PTRDIFF_TYPE__ platform_signed_count(__PTRDIFF_TYPE__ value) { return value; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main
#insert "count.h"
fn C.platform_signed_count(value C.ptrdiff_t) C.ptrdiff_t
fn main() {
	value := if sizeof(int) == 8 { int(i64(1) << 40) } else { int(1234567) }
	assert int(C.platform_signed_count(C.ptrdiff_t(value))) == value
	assert int(C.platform_signed_count(C.ptrdiff_t(-value))) == -value
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, backend)
		built := cmdexec.run(@VEXE, ['-b', backend, '-o', program, root])
		assert built.exit_code == 0, built.output
		ran := cmdexec.run(program, [])
		assert ran.exit_code == 0, ran.output
	}
}

fn test_deprecated_integer_types_warn_and_respect_warnings_as_errors() {
	root := os.join_path(os.vtmp_dir(), 'deprecated_integer_types_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main
fn main() {
	x := isize(-1)
	y := usize(1)
	assert x < 0
	assert y > 0
	assert x.str() == "-1"
	assert y.str() == "1"
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		output := os.join_path(root, backend + '.c')
		args := ['-b', backend, '-o', output, root]
		built := cmdexec.run(@VEXE, args)
		assert built.exit_code == 0, built.output
		assert built.output.contains('`isize` is deprecated; use `int` instead'), built.output
		assert built.output.contains('`usize` is deprecated; use `uint` instead'), built.output
		assert built.output.contains('main.v:3:'), built.output
		assert built.output.contains('main.v:4:'), built.output
		$if macos || linux {
			program := os.join_path(root, backend)
			compiled := cmdexec.run(@VEXE, ['-b', backend, '-o', program, root])
			assert compiled.exit_code == 0, compiled.output
			ran := cmdexec.run(program, [])
			assert ran.exit_code == 0, ran.output
		}
		strict := cmdexec.run(@VEXE, ['-W', ...args])
		assert strict.exit_code != 0, strict.output
		assert strict.output.contains('error: `isize` is deprecated; use `int` instead'), strict.output
		assert strict.output.contains('error: `usize` is deprecated; use `uint` instead'), strict.output
	}
}
