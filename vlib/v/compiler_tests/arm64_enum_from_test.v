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

fn test_enum_from_ownership_only_drops_source_scopes() {
	root := os.join_path(os.vtmp_dir(), 'enum_from_ownership_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, r"enum Choice {
	first
	second
}

interface Drop {
mut:
	drop()
}

struct Owned implements Drop { value int }

fn (mut x Owned) drop() { println('drop ${x.value}') }

fn choose(n int) Choice {
	return Choice.from(n) or {
		owned := Owned{value: 7}
		println(owned.value)
		Choice.first
	}
}

fn as_result(n int) !Choice {
	if n >= 0 {
		return Choice.from(n)
	}
	later := Owned{value: 8}
	println(later.value)
	return .first
}

fn main() {
	assert choose(1) == .second
	assert choose(9) == .first
	assert as_result(1)! == .second
	if _ := as_result(9) { panic('unexpected success') }
	assert as_result(-1)! == .first
}
")!
	result := cmdexec.run(@VEXE, ['-ownership', '-gc', 'none', '-o', os.join_path(root, 'program'),
		'run', source])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '7\ndrop 7\n8\ndrop 8', result.output
}
