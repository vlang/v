module c

import os
import v3.flat
import v3.token

fn test_coverage_json_escape_handles_every_control_character() {
	for code in 0 .. 32 {
		raw := [u8(code)].bytestr()
		expected := match code {
			8 { '\\b' }
			9 { '\\t' }
			10 { '\\n' }
			12 { '\\f' }
			13 { '\\r' }
			else { '\\u00${code:02x}' }
		}
		assert coverage_json_escape(raw) == expected
	}
	assert coverage_json_escape('quote" slash\\') == 'quote\\" slash\\\\'
}

fn test_coverage_points_keep_one_based_source_lines() {
	source := 'first\nsecond\nthird\n'
	path := os.join_path(os.temp_dir(), 'v3_coverage_source_${os.getpid()}.v')
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut ast := flat.FlatAst.new()
	// The AST owns this File pointer for the lifetime of the test fixture.
	unsafe {
		ast.source_files[1] = file
	}
	mut g := FlatGen.new()
	g.a = &ast
	g.cur_fn_name = 'main.main'
	g.coverage_dir = os.join_path(os.temp_dir(), 'v3_coverage_${os.getpid()}')
	second_offset := source.index('second') or { panic('missing second line') }
	third_offset := source.index('third') or { panic('missing third line') }
	g.write_coverage_point(flat.Node{
		kind: .expr_stmt
		pos:  token.new_pos(1, 0)
	})
	g.write_coverage_point(flat.Node{
		kind: .expr_stmt
		pos:  token.new_pos(1, second_offset)
	})
	g.write_coverage_point(flat.Node{
		kind: .expr_stmt
		pos:  token.new_pos(1, second_offset + 1)
	})
	g.write_coverage_point(flat.Node{
		kind: .return_stmt
		pos:  token.new_pos(1, third_offset)
	})
	info := g.coverage_files[os.real_path(path)] or { panic('missing coverage metadata') }
	assert info.points == [1, 2, 3]
	assert info.counters == [0, 1, 2]
	assert g.coverage_counter_count == 3
	generated := g.sb.str()
	assert generated.count('_v3_cov[0]++;') == 1
	assert generated.count('_v3_cov[1]++;') == 2
	assert generated.count('_v3_cov[2]++;') == 1
}

fn test_coverage_user_text_is_not_embedded_in_c_format_strings() {
	dir := os.join_path(os.temp_dir(), 'v3_coverage_%s_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	mut g := FlatGen.new()
	g.coverage_dir = dir
	g.coverage_build_options = '-d percent=%d'
	g.emit_coverage_support()
	generated := g.sb.str()
	escaped_dir := c_escape(dir)
	assert generated.contains('snprintf(cov_filename, sizeof(cov_filename), "%s/vcounters_v3_')
	assert generated.contains('.csv", "${escaped_dir}", cov_secs, cov_nsecs);')
	assert !generated.contains('"${escaped_dir}/vcounters_v3_')
	assert generated.contains('fprintf(cov_file, "# path: %s\\n", "${escaped_dir}");')
	assert generated.contains('fprintf(cov_file, "# build_options: %s\\n", "-d percent=%d");')
}
