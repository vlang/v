module parser

import os
import v.pref

fn test_array_initializer_rejects_positional_elements_in_compiler_and_formatter() {
	root := os.join_path(os.vtmp_dir(), 'array_init_positional_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for i, expression in ['[]int{1, 2, 3}', "[]string{'a', 'b'}", '[]Any{1, "s"}', '&[]int{1, 2}',
		'[3]int{1, 2, 3}', '[]int{len: 2, 1, 2}', '[]Point{Point{x: 1, y: 2}, Point{x: 3, y: 4}}',
		'[]int{1, 2} == []int{}'] {
		path := os.join_path(root, '${i}.v')
		source := 'type Any = int | string\nstruct Point { x int y int }\nfn main() {\n\tx := ${expression}\n\t_ = x\n}\n'
		os.write_file(path, source)!
		for is_fmt in [false, true] {
			mut prefs := pref.new_preferences()
			prefs.is_fmt = is_fmt
			mut p := Parser.new(prefs)
			p.parse_file(path)
			assert p.diagnostics.len == 1, p.diagnostics.str()
			assert p.diagnostics[0].message == 'array initializer elements must use square brackets'
		}
		result := os.exec([@VEXE, 'fmt', '-w', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('array initializer elements must use square brackets'), result.output
		assert os.read_file(path)! == source
	}
}

fn test_array_initializer_named_fields_and_square_bracket_elements_remain_valid() {
	path := os.join_path(os.vtmp_dir(), 'array_init_valid_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expression in ['[]int{}', '[]int{len: 3, cap: 4, init: 7}', '[3]int{init: 7}', '[1, 2, 3]',
		'[3]int[1, 2, 3]'] {
		os.write_file(path, 'fn main() { x := ${expression}; _ = x }\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
	}
}

fn test_vml_array_literals_preserve_nonempty_elements_and_typed_empty_arrays() {
	assert vml_array_literal('string', []) == '[]string{}'
	assert vml_array_literal('string', ["'Home'", "'Work'"]) == "['Home', 'Work']"
	assert vml_array_literal('ui2.MenuEntry', ['ui2.MenuEntry{id: "home", title: "Home"}']) == '[ui2.MenuEntry{id: "home", title: "Home"}]'
	assert vml_array_literal('ui2.MessageBoxAction', []) == '[]ui2.MessageBoxAction{}'
}
