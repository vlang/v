module errors

import encoding.utf8.east_asian
import os
import v3.flat
import v3.token

fn test_diagnostic_display_width_matches_east_asian_boundaries() {
	assert diagnostic_display_width('ascii') == 5
	assert diagnostic_display_width('\xffx') == 2
	for entry in diagnostic_wide_ranges {
		for codepoint in [entry.start - 1, entry.start, entry.end, entry.end + 1] {
			text := rune(codepoint).str()
			assert diagnostic_display_width(text) == east_asian.display_width(text, 1)
		}
	}
}

fn test_relative_error_path_honors_absolute_path_requests() {
	old_value := os.getenv_opt('VERROR_PATHS')
	defer {
		if value := old_value {
			os.setenv('VERROR_PATHS', value, true)
		} else {
			os.unsetenv('VERROR_PATHS')
		}
	}
	path := os.join_path(os.getwd(), 'vlib', 'v3', 'errors', 'format.v')
	absolute_path := os.real_path(path).replace('\\', '/')
	os.setenv('VERROR_PATHS', 'absolute', true)
	assert relative_error_path(path) == absolute_path
	os.unsetenv('VERROR_PATHS')
	assert relative_error_path(path) == 'vlib/v3/errors/format.v'
}

fn test_template_call_stack_includes_every_parent() {
	root := os.join_path(os.temp_dir(), 'v3_template_call_stack_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := os.join_path(root, 'main.v')
	outer_path := os.join_path(root, 'outer.txt')
	inner_path := os.join_path(root, 'inner.txt')
	main_source := "fn main() {\n\t\$tmpl('outer.txt')\n}\n"
	outer_source := "@{\$tmpl('inner.txt')}\n"
	inner_source := '@missing\n'
	os.write_file(main_path, main_source) or { panic(err) }
	os.write_file(outer_path, outer_source) or { panic(err) }
	os.write_file(inner_path, inner_source) or { panic(err) }
	mut file_set := token.FileSet.new()
	mut main_file := file_set.add_file(main_path, main_source.len)
	main_file.index_lines(main_source)
	mut outer_file := file_set.add_file(outer_path, outer_source.len)
	outer_file.index_lines(outer_source)
	mut inner_file := file_set.add_file(inner_path, inner_source.len)
	inner_file.index_lines(inner_source)
	mut a := &flat.FlatAst{
		source_files:        {
			1: main_file
			2: outer_file
			3: inner_file
		}
		template_call_sites: {
			3: token.new_pos(2, outer_source.index('\$tmpl') or { 0 })
			2: token.new_pos(1, main_source.index('\$tmpl') or { 0 })
		}
	}
	output := formatted_parser_error('undefined ident: `missing`', a, token.new_pos(3, inner_source.index('missing') or {
		0
	}))
	assert output.count('called from ') == 2, output
	assert output.contains('called from ${relative_error_path(outer_path)}:1:'), output
	assert output.contains('called from ${relative_error_path(main_path)}:2:'), output
}
