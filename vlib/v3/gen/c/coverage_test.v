module c

import os
import v3.flat
import v3.token

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
}
