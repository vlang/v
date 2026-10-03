module types

import os

fn test_translated_alias_casts_preserve_argument_checks() {
	root := os.join_path(os.vtmp_dir(), 'translated_alias_casts_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
type uintptr_t = usize
fn main() {
	_ = uintptr_t("invalid")
	_ = usize("invalid")
	_ = missing_typedef(42)
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot cast'), result.output
		assert result.output.contains('to `uintptr_t`'), result.output
		assert result.output.contains('to `usize`'), result.output
		assert result.output.contains('unknown function: missing_typedef'), result.output
		assert !result.output.contains('unknown function: uintptr_t'), result.output
	}
}

fn test_translated_cast_rules_do_not_leak_into_regular_files() {
	root := os.join_path(os.vtmp_dir(), 'translated_cast_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
enum Token { zero one }
fn main() {
	value := 1
	_ = Token(value)
	_ = bool(value)
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('casting numbers to enums'), result.output
		assert result.output.contains('cannot cast to bool'), result.output
	}
}

fn test_translated_pointer_cast_does_not_override_function_bindings() {
	root := os.join_path(os.vtmp_dir(), 'translated_pointer_cast_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
struct debug_info { value int }
fn use_shadow(debug_info fn (int) int) {
	_ = &debug_info(0)
}
fn main() {
	use_shadow(fn (value int) int { return value })
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot take the address of debug_info(0)'), result.output
	}
}
