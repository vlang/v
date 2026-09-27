module c

import os
import v.flat
import v.token

// cache_program_file_matches must accept a program file under any spelling that
// resolves to it, and resolve each written path at most once per memo.
fn test_cache_program_file_matches_resolves_each_written_path_once() {
	root := os.join_path(os.temp_dir(), 'v_cache_program_file_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	other := os.join_path(root, 'other.v')
	os.write_file(program, 'module main\n')!
	os.write_file(other, 'module main\n')!
	program_files := {
		os.real_path(program): true
	}
	// This spelling only matches after resolution (join_path would drop the `.`).
	dotted := root + os.path_separator + '.' + os.path_separator + 'main.v'
	assert !program_files[dotted]
	a := flat.FlatAst.new()
	mut memo := map[string]bool{}
	assert cache_program_file_matches(&a, program_files, dotted, mut memo)
	assert !cache_program_file_matches(&a, program_files, other, mut memo)
	assert memo == {
		dotted: true
		other:  false
	}
	// A memoized answer is used as is; the path is not resolved again.
	mut seeded := {
		other: true
	}
	assert cache_program_file_matches(&a, program_files, other, mut seeded)
	// Without program files nothing matches, and nothing is resolved or memoized.
	mut empty_memo := map[string]bool{}
	assert !cache_program_file_matches(&a, map[string]bool{}, other, mut empty_memo)
	assert empty_memo.len == 0
}

// The table of resolved source paths must not change any answer.
fn test_cache_program_file_matches_answers_the_same_from_the_source_path_table() {
	root := os.join_path(os.temp_dir(), 'v_cache_program_file_table_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	other := os.join_path(root, 'other.v')
	os.write_file(program, 'module main\n')!
	os.write_file(other, 'module main\n')!
	program_files := {
		os.real_path(program): true
	}
	dotted := root + os.path_separator + '.' + os.path_separator + 'main.v'
	mut file_set := token.FileSet.new()
	mut a := flat.FlatAst.new()
	a.source_files[1] = file_set.add_file(dotted, 12)
	a.source_files[2] = file_set.add_file(other, 12)
	a.resolve_source_paths()
	mut memo := map[string]bool{}
	assert cache_program_file_matches(&a, program_files, dotted, mut memo)
	assert !cache_program_file_matches(&a, program_files, other, mut memo)
}

// Every program-file check in cgen resolves through the AST's table of resolved
// source paths instead of resolving the path again.
fn test_program_file_checks_resolve_through_the_source_path_table() {
	mut a := flat.FlatAst.new()
	// An answer only the table can give, so a direct os.real_path would show.
	a.resolved_source_paths['written.v'] = 'recorded.v'
	a.resolve_source_paths()
	mut memo := map[string]bool{}
	assert cache_program_file_matches(&a, {
		'recorded.v': true
	}, 'written.v', mut memo)

	mut g := FlatGen.new()
	g.a = &a
	g.set_cache_program_files(&a, ['written.v'])
	assert g.cache_program_files['recorded.v']

	g.cache_program_files = {
		'recorded.v': true
	}
	for helper in ['__v3_sum_eq_reflection__Type', '__v3_autostr_reflection__Type',
		'__v3_default_clone_reflection__Type'] {
		node_id := a.add_node(flat.Node{
			kind:  .fn_decl
			value: helper
		})
		node := a.nodes[int(node_id)]
		assert g.is_program_specialization_fn_node_with_qfn(node, int(node_id), helper,
			'written.v')
	}
}
