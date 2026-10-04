module types

import os

fn voidptr_slot_check(name string, source string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'voidptr_slot_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	return os.exec([@VEXE, '-check', root])
}

fn voidptr_slot_source(field_param string, fn_param string) string {
	return 'module main
struct Hooks {
	hook fn (${field_param}) int
}
fn impl(slot ${fn_param}) int {
	return 0
}
fn main() {
	h := Hooks{
		hook: impl
	}
	_ = h
}
'
}

// C translated by c2v declares a `void (**pxFunc)(...)` parameter as `&voidptr` in a
// function definition and as `&fn (...)` in a struct field: a `&voidptr` parameter
// stands for a pointer to any pointer-sized slot, at the same depth.
fn test_voidptr_slot_parameters_match_pointer_slots() {
	for i, params in [['&fn (int) int', '&voidptr'], ['&voidptr', '&fn (int) int'],
		['&&i32', '&voidptr'], ['&&&i32', '&&voidptr'], ['&&fn (int) int', '&&voidptr']] {
		result := voidptr_slot_check('ok${i}', voidptr_slot_source(params[0], params[1]))
		assert result.exit_code == 0, '${params}: ${result.output}'
	}
}

// A `&voidptr` parameter does not stand for a pointer to a value: the callee would store
// a pointer in an `i32` or a struct.
fn test_voidptr_slot_parameters_reject_value_slots() {
	for i, params in [['&i32', '&voidptr'], ['&voidptr', '&i32'], ['&&i32', '&&voidptr'],
		['&os.Result', '&voidptr'], ['&&&i32', '&voidptr'], ['&voidptr', '&&&i32'],
		['&&&&i32', '&&voidptr'], ['&&voidptr', '&&&&i32'], ['&&fn (int) int', '&voidptr'],
		['&voidptr', '&&fn (int) int']] {
		result := voidptr_slot_check('bad${i}', voidptr_slot_source(params[0], params[1]).replace('module main\n',
			'module main\nimport os\n'))
		assert result.exit_code != 0, '${params}: ${result.output}'
		assert result.output.contains('cannot assign to field `hook`'), '${params}: ${result.output}'
	}
}
