module errors

import encoding.utf8.east_asian
import os
import v.flat
import v.token

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
	path := os.join_path(os.getwd(), 'vlib', 'v', 'errors', 'format.v')
	absolute_path := os.real_path(path).replace('\\', '/')
	os.setenv('VERROR_PATHS', 'absolute', true)
	assert relative_error_path(path) == absolute_path
	os.unsetenv('VERROR_PATHS')
	assert relative_error_path(path) == 'vlib/v/errors/format.v'
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

fn test_source_lines_through_matches_split_into_lines() {
	sources := ['', 'one', 'one\n', 'one\ntwo', 'one\r\ntwo\r\n', '\n\nthree\n\nfive', 'a\rb\r\rc',
		'last line\n\n']
	for source in sources {
		all := source.split_into_lines()
		for first in -1 .. all.len + 3 {
			for last in first .. all.len + 3 {
				lines := source_lines_through(source, first, last)
				limit := if last < 1 { 1 } else { last }
				assert lines.len == if all.len < limit { all.len } else { limit }
				for i, line in lines {
					assert line == if i + 1 >= first { all[i] } else { '' }
				}
			}
		}
	}
}

fn test_diagnostic_run_finds_lines_in_any_order() {
	sources := ['', 'one', 'one\n', 'one\ntwo', 'one\r\ntwo\r\n', '\n\nthree\n\nfive', 'a\rb\r\rc',
		'last line\n\n', 'a\nb\nc\nd\ne\nf\ng\nh']
	for source in sources {
		count := source.split_into_lines().len
		// forwards, backwards, and back and forth around a line found before
		mut requests := [][]int{}
		for first in -1 .. count + 3 {
			requests << [first, first + 2]
		}
		for first := count + 2; first >= -1; first-- {
			requests << [first, first]
		}
		for first in 0 .. count + 1 {
			requests << [count - first, count + 4]
			requests << [first, first + 1]
			requests << [first + 2, first]
		}
		mut run := DiagnosticRun{
			source_known: true
			source_read:  true
			source_name:  'memory'
			source:       source
		}
		for request in requests {
			assert run.source_lines_through(source, request[0], request[1]) == source_lines_through(source,
				request[0], request[1]), '${source.bytes()} ${request}'
		}
		// another text of the same length is not the source the run has read
		other := source.clone()
		assert run.source_lines_through(other, 2, 3) == source_lines_through(other, 2, 3)
	}
}

fn test_diagnostic_run_formats_like_separate_diagnostics() {
	root := os.join_path(os.temp_dir(), 'v3_diagnostic_run_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	first_path := os.join_path(root, 'first.v')
	second_path := os.join_path(root, 'second.v')
	first_source := 'fn main() {\n\tone := 1\n\ttwo := 2\n\tprintln(one)\n}\n\nfn unused() {}\n'
	second_source := 'module main\n\nfn other(value int) int {\r\n\treturn value\r\n}\n'
	os.write_file(first_path, first_source) or { panic(err) }
	os.write_file(second_path, second_source) or { panic(err) }
	mut file_set := token.FileSet.new()
	mut first_file := file_set.add_file(first_path, first_source.len)
	first_file.index_lines(first_source)
	mut second_file := file_set.add_file(second_path, second_source.len)
	second_file.index_lines(second_source)
	a := &flat.FlatAst{
		source_files: {
			1: first_file
			2: second_file
		}
	}
	reports := [
		token.new_pos(1, first_source.index('two') or { 0 }),
		token.new_pos(1, first_source.index('unused') or { 0 }),
		token.new_pos(2, second_source.index('value') or { 0 }),
		token.new_pos(1, first_source.index('one') or { 0 }),
		token.new_pos(2, second_source.index('return') or { 0 }),
		token.new_pos(9, 0),
	]
	mut run := DiagnosticRun{}
	for i, pos in reports {
		message := 'diagnostic ${i}'
		expected := formatted_error('notice:', message, a, flat.NodeId(-1), pos)
		assert run.formatted_error('notice:', message, a, flat.NodeId(-1), pos) == expected
		if pos.id != 9 {
			assert expected.contains(' | '), expected
		}
	}
	// A file that cannot be read keeps reporting just its location.
	os.rm(second_path) or { panic(err) }
	mut missing_run := DiagnosticRun{}
	for _ in 0 .. 2 {
		output := missing_run.formatted_error('error:', 'gone', a, flat.NodeId(-1), reports[2])
		assert output == formatted_error('error:', 'gone', a, flat.NodeId(-1), reports[2])
		assert !output.contains(' | '), output
	}
}
