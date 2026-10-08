module parser

import os
import v.pref

fn test_for_in_accepts_newline_before_body_in_compiler_and_formatter() {
	path := os.join_path(os.vtmp_dir(), 'for_in_newline_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for header in ['value in [1, 2, 3]', 'value in [1]', 'value in [1, 2, 3]!', 'value in values',
		'index, value in values', 'mut value in values', 'index, mut value in values', 'value in 0 .. 3',
		"key, value in {'a': 1}"] {
		for separator in ['\n', '\n\n', ' // loop body follows\n'] {
			os.write_file(path, 'fn loops(mut values []int) {\n\tfor ${header}${separator}\t{\n\t\t_ = value\n\t}\n\tprintln("after")\n}\n')!
			for is_fmt in [false, true] {
				mut prefs := pref.new_preferences()
				prefs.is_fmt = is_fmt
				mut p := Parser.new(prefs)
				a := p.parse_file(path)
				assert p.diagnostics.len == 0, '${header}: ${p.diagnostics}'
				loops := a.nodes.filter(it.kind == .for_in_stmt)
				assert loops.len == 1
				loop := loops[0]
				header_count := loop.value.int()
				assert loop.children_count == header_count + 1
				assert a.node(a.child(&loop, header_count)).kind == .assign
			}
		}
	}
}

fn test_for_in_newline_body_compiles_and_runs() {
	path := os.join_path(os.vtmp_dir(), 'for_in_newline_run_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn main() {\n\tmut total := 0\n\tfor value in [1, 2, 3]\n\t{\n\t\ttotal += value\n\t}\n\tassert total == 6\n}\n')!
	result := os.exec([@VEXE, 'run', path])
	assert result.exit_code == 0, result.output
}
