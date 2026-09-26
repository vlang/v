module types

import os
import v.parser
import v.pref

// source_file_declares_bare_fn answers from the per-file index that
// build_fn_declaration_indexes fills, instead of walking the top-level index
// per call. Two files declaring the same bare name must each see only their
// own declaration, and a forked parallel worker must share the index.
fn test_bare_fn_names_are_indexed_per_file_and_shared_with_workers() {
	root := os.join_path(os.vtmp_dir(), 'v3_file_bare_fn_names_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	os.write_file(first, 'module main\n\nfn shared() int {\n\treturn 1\n}\n\nfn only_first() {}\n\nfn main() {\n\tprintln(shared())\n}\n')!
	os.write_file(second, 'module main\n\nfn shared() int {\n\treturn 2\n}\n\nfn (x int) method() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(first)
	mut a := p.parse_file(second)
	assert p.diagnostics.len == 0, p.diagnostics.str()

	mut file_ids := map[string]int{}
	for id, file in a.source_files {
		file_ids[os.file_name(file.name)] = id
	}
	first_id := file_ids['first.v']
	second_id := file_ids['second.v']
	assert first_id != second_id

	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.source_file_declares_bare_fn('shared', first_id)
	assert tc.source_file_declares_bare_fn('shared', second_id)
	assert tc.source_file_declares_bare_fn('only_first', first_id)
	assert !tc.source_file_declares_bare_fn('only_first', second_id)
	// A method's bare name is not a bare fn, and unknown names or files never match.
	assert !tc.source_file_declares_bare_fn('method', second_id)
	assert !tc.source_file_declares_bare_fn('missing', first_id)
	assert !tc.source_file_declares_bare_fn('shared', first_id + second_id + 1)

	worker := tc.fork_for_parallel_check()
	assert worker.source_file_declares_bare_fn('shared', second_id)
	assert worker.source_file_declares_bare_fn('only_first', first_id)
	assert !worker.source_file_declares_bare_fn('only_first', second_id)
}

fn test_bare_fn_index_matches_a_top_level_walk() {
	root := os.join_path(os.vtmp_dir(), 'v3_file_bare_fn_walk_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'walk.v')
	os.write_file(path, 'module main\n\nstruct S {}\n\nfn (s S) m() {}\n\nfn a() {}\n\nfn b() {}\n\nfn main() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	// Every top-level fn_decl, and nothing else, is in the index.
	mut expected := map[string]bool{}
	for index in tc.top_level_idx {
		node := a.nodes[index]
		if node.kind == .fn_decl {
			expected['${node.pos.id}\x00${node.value}'] = true
		}
	}
	assert expected.len > 0
	assert tc.file_bare_fn_names.len == expected.len
	for key, _ in expected {
		assert key in tc.file_bare_fn_names, key
	}
}
