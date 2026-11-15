module types

import os

const unsafe_flag_declarations = '@[flag]
enum Mode {
	a
	b
}

struct Holder {
mut:
	flags Mode
}

const constant_flags = Mode.a
const optional_flags = ?Mode(Mode.a)

fn make_flags() Mode {
	return .a
}
'

fn test_flag_enum_mutators_allow_unsafe_reference_fields() {
	root := os.join_path(os.vtmp_dir(), 'unsafe_flag_mutators_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, unsafe_flag_declarations + '
fn (holder &Holder) change() {
	unsafe { holder.flags.set(.b) }
	assert holder.flags == Mode.a | Mode.b
	unsafe { holder.flags.clear(.a) }
	assert holder.flags == Mode.b
	unsafe { holder.flags.toggle(.a) }
	assert holder.flags == Mode.a | Mode.b
	unsafe { holder.flags.clear_all() }
	assert holder.flags == Mode.zero()
	unsafe { holder.flags.set_all() }
	assert holder.flags == Mode.a | Mode.b
}

fn change_optional() ? {
	optional := ?Mode(Mode.a)
	unsafe { optional?.set(.b) }
	assert optional? == Mode.a | Mode.b
}

fn main() {
	holder := Holder{flags: .a}
	holder.change()
	assert holder.flags == Mode.a | Mode.b
	flags := Mode.a
	unsafe { flags.set(.b) }
	assert flags == Mode.a | Mode.b
	change_optional() or { panic("unexpected none") }
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		source])
	assert result.exit_code == 0, result.output
}

fn test_flag_enum_mutators_keep_safe_immutability_and_reject_rvalues_and_constants() {
	root := os.join_path(os.vtmp_dir(), 'unsafe_flag_mutators_bad_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for method in ['set(.b)', 'clear(.a)', 'toggle(.b)', 'set_all()', 'clear_all()'] {
		for body in ['flags.${method}', 'holder.flags.${method}', 'unsafe { make_flags().${method} }',
			'unsafe { constant_flags.${method} }'] {
			os.write_file(source, unsafe_flag_declarations + '
fn main() {
	flags := Mode.a
	holder := Holder{flags: .a}
	${body}
}
')!
			result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', source])
			assert result.exit_code != 0, result.output
			assert result.output.contains('flag enum method `${method.all_before('(')}` requires a mutable receiver'), result.output
		}
	}
	os.write_file(source, unsafe_flag_declarations + '
fn change() ? {
	unsafe { optional_flags?.set(.b) }
}
fn main() { change() or {} }
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', source])
	assert result.exit_code != 0, result.output
	assert result.output.contains('flag enum method `set` requires a mutable receiver'), result.output
}

fn test_unsafe_flag_mutators_reject_imported_constants_and_allow_local_shadows() {
	root := os.join_path(os.vtmp_dir(), 'unsafe_flag_imports_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'flags'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'unsafe_flag_imports' }\n")!
	os.write_file(os.join_path(root, 'flags', 'flags.v'), 'module flags
@[flag]
pub enum Mode {
	a
	b
}
pub const constant_flags = Mode.a
pub const optional_flags = ?Mode(Mode.a)
')!
	source := os.join_path(root, 'main.v')
	for method in ['set(.b)', 'clear(.a)', 'toggle(.b)', 'set_all()', 'clear_all()'] {
		for receiver in ['flags.constant_flags', '(flags.constant_flags)'] {
			os.write_file(source, 'import flags
fn main() {
	unsafe { ${receiver}.${method} }
}
')!
			result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', root])
			assert result.exit_code != 0, result.output
			assert result.output.contains('flag enum method `${method.all_before('(')}` requires a mutable receiver'), result.output
		}
	}
	os.write_file(source, 'import flags
fn change() ? {
	unsafe { flags.optional_flags?.set(.b) }
}
fn main() { change() or {} }
')!
	optional_result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', root])
	assert optional_result.exit_code != 0, optional_result.output
	assert optional_result.output.contains('flag enum method `set` requires a mutable receiver'), optional_result.output
	os.write_file(source, 'import flags as flaglib
struct Holder {
mut:
	constant_flags flaglib.Mode
}
fn main() {
	flags := Holder{constant_flags: .a}
	unsafe { flags.constant_flags.set(.b) }
	assert flags.constant_flags.has(.b)
}
')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		root])
	assert result.exit_code == 0, result.output
}
