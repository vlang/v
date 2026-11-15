module types

import os

fn test_static_enum_initializers_keep_values_and_storage() {
	root := os.join_path(os.vtmp_dir(), 'static_enum_initializers_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'values'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'static_enum_initializers' }")!
	os.write_file(os.join_path(root, 'values', 'values.v'), 'module values
pub enum State { waiting ready }
pub type StateAlias = State
')!
	for translated in [false, true] {
		file_attribute := if translated { '@[translated]\n' } else { '' }
		fn_attribute := if translated { '' } else { '@[unsafe]\n' }
		os.write_file(os.join_path(root, 'main.v'), '${file_attribute}module main
import values as data

enum Phase { waiting ready }
type PhaseAlias = Phase

${fn_attribute}fn next_phase() Phase {
	mut static current := Phase.waiting
	old := current
	current = Phase.ready
	return old
}

${fn_attribute}fn alias_phase() i32 {
	static current := PhaseAlias.ready
	return i32(current)
}

${fn_attribute}fn imported_phase() i32 {
	static current := data.State.ready
	return i32(current)
}

${fn_attribute}fn imported_alias_phase() i32 {
	static current := data.StateAlias.waiting
	return i32(current)
}

fn main() {
	unsafe {
		assert next_phase() == Phase.waiting
		assert next_phase() == Phase.ready
		assert alias_phase() == 1
		assert imported_phase() == 1
		assert imported_alias_phase() == 0
	}
}
')!
		result := os.exec([@VEXE, '-new-compiler', '-gc', 'none', 'run', root])
		assert result.exit_code == 0, 'translated=${translated}: ${result.output}'
	}
}

fn test_static_enum_initializers_reject_runtime_selectors_and_missing_members() {
	root := os.join_path(os.vtmp_dir(), 'static_enum_invalid_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for translated in [false, true] {
		file_attribute := if translated { '@[translated]\n' } else { '' }
		fn_attribute := if translated { '' } else { '@[unsafe]\n' }
		for expression in ['holder.phase', 'get_holder().phase', 'Phase.missing'] {
			os.write_file(os.join_path(root, 'main.v'), '${file_attribute}module main
enum Phase { waiting ready }
struct Holder { phase Phase }
fn get_holder() Holder { return Holder{phase: .ready} }
${fn_attribute}fn check() {
	holder := get_holder()
	static current := ${expression}
	_ = holder
	_ = current
}
fn main() { unsafe { check() } }
')!
			result := os.exec([@VEXE, '-new-compiler', '-check', root])
			assert result.exit_code != 0, 'translated=${translated}, ${expression}: ${result.output}'
			if expression == 'Phase.missing' {
				assert result.output.contains('unknown enum field `missing`'), result.output
			} else {
				assert result.output.contains('static variable with non-constant value'), result.output
			}
		}
	}
}
