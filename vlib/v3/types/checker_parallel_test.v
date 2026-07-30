module types

import os
import strings
import v3.flat
import v3.parser
import v3.pref

fn test_fast_file_index_collects_translated_module_attribute() {
	old_no_file_idx := os.getenv_opt('V3_NO_FILE_IDX')
	os.unsetenv('V3_NO_FILE_IDX')
	defer {
		if value := old_no_file_idx {
			os.setenv('V3_NO_FILE_IDX', value, true)
		}
	}
	path := os.join_path(os.vtmp_dir(), 'v3_translated_file_index_${os.getpid()}.v')
	os.write_file(path, '@[translated]\nmodule main\n\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert file_index_usable(a)

	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.translated_files[path]
}

fn test_parallel_checker_dependencies_are_private_and_merged() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	master_dependency, _ := tc.intern_symbol('main.master_dependency')
	worker_dependency, _ := tc.intern_symbol('main.worker_dependency')
	tc.direct_dependencies_by_fn[10] = [master_dependency]

	mut worker := tc.fork_for_parallel_check()
	assert voidptr(worker.visible_mutation_cache) != voidptr(tc.visible_mutation_cache)
	assert worker.direct_dependencies_by_fn.len == 0
	worker.direct_dependencies_by_fn[10] = [worker_dependency]
	worker.direct_dependencies_by_fn[20] = [master_dependency]
	assert tc.direct_dependencies_by_fn[10] == [master_dependency]
	assert 20 !in tc.direct_dependencies_by_fn

	tc.merge_parallel_check_worker(worker)
	assert tc.direct_dependencies_by_fn[10] == [master_dependency, worker_dependency]
	assert tc.direct_dependencies_by_fn[20] == [master_dependency]
	worker.free_parallel_check_worker_cache()

	mut transform_worker := tc.fork_for_parallel_transform(&a)
	assert isnil(transform_worker.visible_mutation_cache)
	assert transform_worker.direct_dependencies_by_fn.len == 0
	transform_worker.free_parallel_transform_caches()
}

fn test_direct_parent_index_preserves_first_parent_and_falls_back_for_new_nodes() {
	mut a := flat.FlatAst.new()
	child := a.add_val(.ident, 'child')
	first_children := a.begin_children()
	a.add_child(child)
	first_parent := a.add_node(flat.Node{
		kind:           .paren
		children_start: first_children
		children_count: 1
	})
	second_children := a.begin_children()
	a.add_child(child)
	a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: second_children
		children_count: 1
	})

	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	assert tc.direct_parent_id(child) == first_parent
	assert tc.direct_parent_id(first_parent) == flat.empty_node

	appended_child := a.add_val(.ident, 'appended')
	appended_children := a.begin_children()
	a.add_child(appended_child)
	appended_parent := a.add_node(flat.Node{
		kind:           .paren
		children_start: appended_children
		children_count: 1
	})
	assert tc.direct_parent_id(appended_child) == appended_parent
}

fn test_enclosing_generic_param_uses_the_owning_top_level_declaration() {
	mut a := flat.FlatAst.new()
	generic_child := a.add_val(.ident, 'T')
	generic_children := a.begin_children()
	a.add_child(generic_child)
	mut generic_fn := flat.Node{
		kind:           .fn_decl
		children_start: generic_children
		children_count: 1
	}
	generic_fn.set_generic_params(['T'])
	generic_fn_id := a.add_node(generic_fn)

	unrelated_child := a.add_val(.ident, 'T')
	unrelated_children := a.begin_children()
	a.add_child(unrelated_child)
	unrelated_fn_id := a.add_node(flat.Node{
		kind:           .fn_decl
		children_start: unrelated_children
		children_count: 1
	})

	mut tc := TypeChecker.new(&a)
	tc.top_level_idx = [int(generic_fn_id), int(unrelated_fn_id)]
	tc.build_enclosing_generic_param_index(&a)
	assert tc.node_has_enclosing_generic_param(generic_child, 'T')
	assert !tc.node_has_enclosing_generic_param(unrelated_child, 'T')
}

fn test_parallel_checker_preserves_all_dependency_edges() {
	$if !windows {
		old_vjobs := os.getenv_opt('VJOBS')
		os.setenv('VJOBS', '4', true)
		defer {
			if value := old_vjobs {
				os.setenv('VJOBS', value, true)
			} else {
				os.unsetenv('VJOBS')
			}
		}
		mut source := strings.new_builder(64_000)
		source.writeln('module main')
		for i in 0 .. 320 {
			source.writeln('fn dependency_${i}() int { return ${i} }')
			source.writeln('fn caller_${i}(flag bool) int { value := if flag { dependency_${i}() } else { ${i} }; return value }')
		}
		source.writeln('fn main() { println(caller_0(true)) }')
		path := os.join_path(os.vtmp_dir(), 'v3_parallel_checker_dependencies_${os.getpid()}.v')
		os.write_file(path, source.str()) or { panic(err) }
		defer {
			os.rm(path) or {}
		}
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		assert tc.check_semantics_opt(true)
		assert tc.errors.len == 0, tc.errors.str()

		mut caller_ids := map[string]int{}
		for i, node in a.nodes {
			if node.kind == .fn_decl && node.value.starts_with('caller_') {
				caller_ids[node.value] = i
			}
		}
		for i in 0 .. 320 {
			dependencies := tc.direct_dependencies(caller_ids['caller_${i}'])
			assert dependencies.any(it == 'dependency_${i}' || it.ends_with('.dependency_${i}')), 'caller_${i}: ${dependencies}'
		}
	}
}

fn assert_preflight_error_keeps_function_semantics(name string, source string, initial_error string, collection_error bool) {
	for want_parallel in [false, true] {
		path := os.join_path(os.vtmp_dir(),
			'v3_preflight_continuation_${name}_${want_parallel}_${os.getpid()}.v')
		os.write_file(path, source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		if collection_error {
			assert tc.errors.any(it.msg.contains(initial_error)), tc.errors.str()
		}
		tc.diagnose_unknown_calls = true
		tc.check_semantics_opt(want_parallel)
		assert tc.errors.any(it.msg.contains(initial_error)), tc.errors.str()
		assert tc.errors.filter(it.msg.contains(initial_error)).len == 1, tc.errors.str()
		assert tc.errors.any(it.msg.contains('unknown function') && it.msg.contains('unknown_call')), tc.errors.str()

		os.rm(path) or {}
	}
}

fn test_preflight_errors_do_not_skip_function_semantics() {
	assert_preflight_error_keeps_function_semantics('collection_error',
		'type Recursive = []Recursive\n\nfn main() {\n\tunknown_call()\n}\n',
		'recursive declarations of aliases', true)
	assert_preflight_error_keeps_function_semantics('for_in_const_conflict',
		'const item = 1\n\nfn report_other_error() {\n\tunknown_call()\n}\n\nfn main() {\n\tfor item in [1, 2] {}\n}\n',
		'duplicate of a const name `item`', false)
}
