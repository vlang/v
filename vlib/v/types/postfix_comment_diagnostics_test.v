module types

import os

fn test_postfix_value_diagnostics_skip_complete_nested_comments() {
	root := os.join_path(os.vtmp_dir(), 'postfix_nested_trivia_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	trivia := ['/* outer /* nested */ end */', '/* outer /* one */ /* two */ end */',
		'/* outer /* nested */ ) ] */']
	mut program := 'module main\nfn take(value int) int { return value }\nfn main() {\n mut x := 0\n values := [1, 2]\n'
	for gap in trivia {
		program += ' _ = take(x++${gap})\n _ = values[x--${gap}]\n'
	}
	program += '}\n'
	os.write_file(source, program)!
	for flags in ['', '-no-parallel -nocache'] {
		for mode in ['', '-prod', '-W'] {
			result := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
				...(os.split_args(mode) or { panic(err) }), '-check', source])
			assert (result.exit_code != 0) == (mode == '-W'), result.output
			for op in ['++', '--'] {
				message := '`${op}` operator can only be used as a statement'
				assert result.output.count(message) == trivia.len, result.output
			}
		}
	}
}

fn test_delimiters_inside_nested_comments_do_not_warn_on_postfix_values() {
	root := os.join_path(os.vtmp_dir(), 'postfix_nested_false_warning_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main
fn main() {
 mut x := 0
 first := x++ /* outer /* inner */ ) */ + 1
 second := x-- /* outer /* inner */ ] */ + 1
 _ = first
 _ = second
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, '-new-compiler', ...(os.split_args(flags) or { panic(err) }),
			'-W', '-check', source])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('operator can only be used as a statement'), result.output
	}
}
