module fastc

import os
import v.cmdexec

fn test_fastc_call_head_rewriting_ignores_c_literals() {
	assert fastc_replace_c_call_at_head(r'"m->get(1)" + m->get(id(2))', 'm->get(',
		'replacement()') == r'"m->get(1)" + replacement()'
	assert fastc_replace_c_call_at_head(r'"escaped \" m->get(1)" + m->get(2)', 'm->get(',
		'replacement()') == r'"escaped \" m->get(1)" + replacement()'
	assert fastc_replace_c_name(r'"m->get(2)" + m->get(2)', 'm->get(2)',
		'replacement()', true) == r'"m->get(2)" + replacement()'
}

fn test_fastc_callback_call_rewriting_preserves_matching_string_text() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_callback_text_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Mapper {
	get fn (int) int
}
fn (m Mapper) id(x int) int {
	return x + 1
}
fn main() {
	m := Mapper{get: fn (x int) int { return x * 2 }}
	println("m.get(2) ${m.get(2)}")
	println("m.get(m.id(2)) ${m.get(m.id(2))}")
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == 'm.get(2) 4\nm.get(m.id(2)) 6', '${backend}: ${run.output}'
	}
}
