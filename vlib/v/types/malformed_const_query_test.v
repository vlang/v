module types

import v.flat
import v.token

struct MalformedConstQueryCase {
	source         string
	offset         int
	keyword_offset int = -1
}

fn malformed_const_query_checker(source string, file_id int, offset int) (&TypeChecker, flat.NodeId) {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[file_id] = files.add_file('typo.v', source.len)
	a.source_texts[file_id] = source
	id := a.add_node(flat.Node{
		kind: .ident
		pos:  token.new_pos(file_id, offset)
	})
	mut tc := TypeChecker.new(&a)
	tc.source_texts_by_file['typo.v'] = source
	return tc, id
}

fn test_malformed_const_query_preserves_line_and_keyword_boundaries() {
	cases := [
		MalformedConstQueryCase{ source: '\t cosnt amount = 1\n', offset: 10, keyword_offset: 2 },
		MalformedConstQueryCase{ source: 'module main\n\tcosnt\tamount = 1', offset: 20, keyword_offset: 13 },
		MalformedConstQueryCase{ source: 'cosnt', offset: 0, keyword_offset: 0 },
		MalformedConstQueryCase{ source: 'cosnt\n', offset: 0, keyword_offset: 0 },
		MalformedConstQueryCase{ source: 'cosntamount = 1\n', offset: 0 },
		MalformedConstQueryCase{ source: '// cosnt amount = 1\n', offset: 10 },
		MalformedConstQueryCase{ source: 'const amount = "cosnt"\n', offset: 10 },
		MalformedConstQueryCase{ source: 'const amount = 1\n', offset: 10 },
		MalformedConstQueryCase{ source: 'cosnt amount = 1\namount = 2\n', offset: 18 },
		MalformedConstQueryCase{ source: 'cosnt amount = 1', offset: -1 },
		MalformedConstQueryCase{ source: 'cosnt amount = 1', offset: 100 },
	]
	for indexed in [false, true] {
		for item in cases {
			mut tc, id := malformed_const_query_checker(item.source, 1, item.offset)
			if indexed {
				tc.index_multiple_module_import_lines(tc.a)
			}
			if pos := tc.malformed_const_keyword_pos(id) {
				assert item.keyword_offset >= 0
				assert pos == token.new_span(1, item.keyword_offset, item.keyword_offset + 5)
			} else {
				assert item.keyword_offset == -1
			}
		}
	}
}

fn test_malformed_const_query_reindexes_changed_sources_without_mutating_forks() {
	mut tc, id := malformed_const_query_checker('const amount = 1', 1, 10)
	tc.index_multiple_module_import_lines(tc.a)
	forked := tc.fork_for_parallel_check()
	assert tc.malformed_const_keyword_pos(id) == none
	assert forked.malformed_const_keyword_pos(id) == none
	tc.a.source_texts[1] = 'cosnt amount = 1'
	tc.index_multiple_module_import_lines(tc.a)
	assert (tc.malformed_const_keyword_pos(id) or { panic('missing typo after source reset') }).offset == 0
	assert forked.malformed_const_keyword_pos(id) == none
}

fn test_malformed_const_query_indexes_added_files_and_retains_sparse_fallback() {
	mut tc, _ := malformed_const_query_checker('const amount = 1', 1, 10)
	tc.index_multiple_module_import_lines(tc.a)
	mut files := token.FileSet.new()
	for file_id in [2, 1000000000] {
		source := '\tcosnt amount = 1'
		tc.a.source_files[file_id] = files.add_file('typo_${file_id}.v', source.len)
		tc.a.source_texts[file_id] = source
		id := tc.a.add_node(flat.Node{
			kind: .ident
			pos:  token.new_pos(file_id, 10)
		})
		tc.index_module_import_lines_of_new_files(tc.a)
		pos := tc.malformed_const_keyword_pos(id) or { panic('missing added-file typo') }
		assert pos == token.new_span(file_id, 1, 6)
	}
	assert tc.malformed_const_source_states.len < 128
}

fn test_malformed_const_query_retains_filename_fallback_for_duplicate_ids() {
	for incremental in [false, true] {
		mut tc, first := malformed_const_query_checker('const amount = 1', 1, 10)
		if incremental {
			tc.index_multiple_module_import_lines(tc.a)
		} else {
			tc.source_texts_by_file.clear()
		}
		mut files := token.FileSet.new()
		tc.a.source_files[2] = files.add_file('typo.v', 16)
		tc.a.source_texts[2] = 'cosnt amount = 1'
		second := tc.a.add_node(flat.Node{
			kind: .ident
			pos:  token.new_pos(2, 10)
		})
		tc.index_module_import_lines_of_new_files(tc.a)
		assert tc.malformed_const_source_states[1] == 0
		assert tc.malformed_const_source_states[2] == 0
		has_typo := tc.source_texts_by_file['typo.v'].starts_with('cosnt')
		if incremental {
			assert has_typo
		}
		for id in [first, second] {
			if pos := tc.malformed_const_keyword_pos(id) {
				assert has_typo
				assert pos == token.new_span(tc.a.nodes[int(id)].pos.id, 0, 5)
			} else {
				assert !has_typo
			}
		}
	}
}
