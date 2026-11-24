module main

import os
import v.cmdexec

fn test_arm64_enum_from_integer_preserves_result_and_loop_control() {
	$if !(macos && arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arm64_enum_from_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main')
	os.write_file(source, 'enum Choice {
	negative = -7
	positive = 12
}

@[flag]
enum Flags as u8 {
	read
	write
}

fn convert(value i64) !Choice {
	return Choice.from(value)
}

fn main() {
	mut accepted := []Choice{}
	for value in [i64(-7), 8, 12, 4294967289] {
		accepted << (Choice.from(value) or { continue })
	}
	assert accepted == [Choice.negative, Choice.positive]
	assert convert(-7)! == .negative
	if _ := convert(8) {
		assert false
	} else {
		assert err.msg() == "invalid value"
	}
	assert Flags.from(3)! == Flags.read | Flags.write
	assert Flags.from(0)! == Flags.zero()
	if _ := Flags.from(257) {
		assert false
	} else {
		assert err.msg() == "invalid value"
	}
}
')!
	compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run_with_timeout(output, []string{}, 5000)
	assert run.exit_code == 0, run.output
}
