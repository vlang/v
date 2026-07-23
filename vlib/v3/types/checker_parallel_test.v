module types

import os
import strings
import v3.flat
import v3.parser
import v3.pref

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
