module types

import os

fn test_pointer_borrows_keep_local_storage_checks() {
	root := os.join_path(os.vtmp_dir(), 'pointer_borrow_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
struct State { value int }
fn escaped(state &State) { alias := state; _ = alias }
fn main() {
	values := [3, 5]!
	pointer := &values[0]
	_ = pointer
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot reference fixed array'), result.output
		assert result.output.contains('cannot be assigned outside'), result.output
	}
}

fn test_translated_fixed_array_borrow_cannot_be_stored() {
	root := os.join_path(os.vtmp_dir(), 'translated_borrow_store_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
fn main() {
	values := [3, 5]!
	pointer := &values[0] + 1
	_ = pointer
}
')!
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot reference fixed array'), result.output
}

fn test_local_fixed_array_shadowing_global_cannot_escape() {
	root := os.join_path(os.vtmp_dir(), 'global_borrow_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
__global values = [3, 5]!
fn main() {
	values := [7, 9]!
	pointer := &values[0]
	_ = pointer
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-enable-globals',
			'-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot reference fixed array'), result.output
	}
}

fn test_cast_fixed_array_borrow_can_be_passed_to_a_call() {
	root := os.join_path(os.vtmp_dir(), 'cast_borrow_call_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn use_ptr(p voidptr) bool { return p != unsafe { nil } }
fn use_addr(a u64) bool { return a != 0 }
fn main() {
	mut raw := [4]u8{}
	_ = use_ptr(voidptr(&raw[0]))
	_ = use_addr(u64(&raw[1]) + 2)
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code == 0, result.output
	}
}
