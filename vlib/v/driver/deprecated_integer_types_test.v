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

fn test_nested_explicit_generic_arguments_use_canonical_integer_names() {
	root := os.join_path(os.vtmp_dir(), 'nested_legacy_integer_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct Box[T] { value T }
fn identity[T](value T) T { return value }
fn main() {
	box := identity[Box[usize]](Box[usize]{value: 123})
	assert box.value == 123
	nested := identity[Box[Box[usize]]](Box[Box[usize]]{value: Box[usize]{value: 456}})
	assert nested.value.value == 456
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-o', program, root])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}

fn test_modern_integer_library_calls_do_not_emit_legacy_type_warnings() {
	root := os.join_path(os.vtmp_dir(), 'modern_integer_libraries_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import strconv
import sync
import encoding.binary
fn main() {
	assert strconv.format_thousands(1234567, ",") == "1,234,567"
	mut local := sync.new_tls(int(-42))!
	assert local.get()! == -42
	local.destroy()!
	assert binary.encode_binary(int(-42))!.len == 8
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-nocache', '-o', program, root])
	assert built.exit_code == 0, built.output
	assert !built.output.contains('is deprecated; use'), built.output
	ran := cmdexec.run(program, [])
	assert ran.exit_code == 0, ran.output
}
