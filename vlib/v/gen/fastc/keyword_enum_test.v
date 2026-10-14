module fastc

import os
import v.cmdexec

fn test_fastc_keyword_enum_members_in_positional_struct_literals() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_keyword_enum_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

enum Kind {
	or
	if
	match
	fn
}

struct Token {
	kind Kind
	value int
}

fn main() {
	mut tokens := []Token{}
	tokens << Token{.or, 10}
	tokens << Token{.if, 20}
	tokens << Token{.match, 30}
	tokens << Token{.fn, 40}
	assert tokens[0].kind == .or
	assert tokens[1].kind == .if
	assert tokens[2].kind == .match
	assert tokens[3].kind == .fn
	println(tokens[0].value + tokens[1].value + tokens[2].value + tokens[3].value)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '100', run.output
}
