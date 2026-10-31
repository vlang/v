module types

import os
import v.parser
import v.pref

fn dependency_checker(call string, parallel bool) !TypeChecker {
	return dependency_checker_source('module main\nimport dep\nfn main() { println(dep.${call}(dep.Item{})) }\n', parallel)
}

fn dependency_checker_source(source string, parallel bool) !TypeChecker {
	root := os.join_path(os.vtmp_dir(), 'dependency_errors_${os.getpid()}_${parallel}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep\npub struct Item {\npub:\n\tname string\n}\npub fn describe(item Item) string { return item.missing }\npub fn rename(item Item) Item { item.name = "renamed"; return item }\npub fn (item Item) renamed() Item { item.name = "renamed"; return item }\npub fn wrapper(item Item) Item { return rename(item) }\npub fn unused() { unused_value := 1 }\npub fn unused_callback() int { return "invalid" }\npub struct FieldItem {\npub:\n\trenamed int\n}\npub fn (item FieldItem) renamed() int { return "invalid" }\n')!
	os.write_file(main, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dep, main])
	mut tc := TypeChecker.new(a)
	tc.diagnostic_files[main] = true
	tc.collect(a)
	tc.check_semantics_opt(parallel)
	return tc
}

fn test_dependency_function_values_keep_their_errors() {
	for source in [
		'module main\nimport dep\nfn main() { f := dep.rename; println(f(dep.Item{})) }\n',
		'module main\nimport dep\nfn apply(f fn (dep.Item) dep.Item) dep.Item { return f(dep.Item{}) }\nfn main() { println(apply(dep.rename)) }\n',
		'module main\nimport dep\nstruct Holder { cb fn (dep.Item) dep.Item = unsafe { nil } }\nfn main() { holder := Holder{ cb: dep.rename }; println(holder.cb(dep.Item{})) }\n',
		'module main\nimport dep\nfn main() { f := dep.wrapper; println(f(dep.Item{})) }\n',
		'module main\nimport dep\nfn main() { item := dep.Item{}; f := item.renamed; println(f()) }\n',
	] {
		for parallel in [false, true] {
			tc := dependency_checker_source(source, parallel)!
			assert tc.errors.any(it.file.ends_with('dep.v') && it.msg.contains('immutable')), tc.errors.str()
			assert !tc.errors.any(it.msg.contains('has no field named')), tc.errors.str()
			assert tc.notices.len == 0, tc.notices.str()
		}
	}
}

fn test_shadowed_names_do_not_keep_unused_dependency_functions() {
	for source in [
		'module main\nimport dep as _\nstruct Holder { rename int }\nfn main() { dep := Holder{ rename: 42 }; println(dep.rename) }\n',
		'module main\nimport dep { unused_callback }\nfn main() { unused_callback := 42; println(unused_callback) }\n',
		'module main\nimport dep { unused_callback }\nfn main() { f := fn (unused_callback int) int { return unused_callback }; println(f(42)) }\n',
		'module main\nimport dep { unused_callback }\nfn main() { x := 1; f := fn [x] (unused_callback int) int { return unused_callback + x }; println(f(42)) }\n',
		'module main\nimport dep { unused_callback }\nfn main() { unused_callback := 1; f := fn [unused_callback] (value int) int { return unused_callback + value }; println(f(42)) }\n',
		'module main\nimport dep { unused_callback }\nfn apply(cb fn (int) int) int { return cb(42) }\nfn main() { println(apply(|unused_callback| unused_callback)) }\n',
		'module main\nimport dep\nfn main() { item := dep.FieldItem{renamed: 42}; println(item.renamed) }\n',
		'module main\nimport dep { unused_callback }\nfn apply(unused_callback fn () int) int { return unused_callback() }\nfn good() int { return 42 }\nfn main() { println(apply(good)) }\n',
		'module main\nimport dep { unused_callback }\nfn good() int { return 42 }\nfn main() { unused_callback := good; println(unused_callback()) }\n',
		'module main\nimport dep { unused_callback }\nfn main() { f := fn (unused_callback fn () int) int { return unused_callback() }; println(f(fn () int { return 42 })) }\n',
	] {
		for parallel in [false, true] {
			tc := dependency_checker_source(source, parallel)!
			assert tc.errors.len == 0, tc.errors.str()
		}
	}
}

fn test_vmodules_callback_errors_are_reported_before_codegen() {
	root := os.join_path(os.vtmp_dir(), 'vmodules_callback_errors_${os.getpid()}')
	modules := os.join_path(root, 'modules')
	app := os.join_path(root, 'app')
	os.mkdir_all(os.join_path(modules, 'dep'))!
	os.mkdir_all(app)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(modules, 'dep', 'dep.v'), 'module dep\npub struct Item {\npub:\n\tname string\n}\npub fn rename(item Item) Item { item.name = "renamed"; return item }\n')!
	main := os.join_path(app, 'main.v')
	os.write_file(main, 'import dep\nfn apply(f fn (dep.Item) dep.Item) dep.Item { return f(dep.Item{}) }\nfn main() { println(apply(dep.rename)) }\n')!
	mut process := os.new_process(@VEXE)
	process.set_args(['-new-compiler', '-nocache', '-check', main])
	mut environment := os.environ()
	environment['VMODULES'] = modules
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	assert process.code != 0, output
	assert output.contains('dep.v:') && output.contains('immutable'), output
	assert !output.contains('C compilation error'), output
	process.close()
}

fn test_reachable_dependency_field_errors_are_reported() {
	for parallel in [false, true] {
		tc := dependency_checker('describe', parallel)!
		assert tc.errors.any(it.file.ends_with('dep.v') && it.msg.contains('has no field named')
			&& it.msg.contains('missing')), tc.errors.str()
		assert !tc.errors.any(it.msg.contains('immutable')), tc.errors.str()
		assert tc.notices.len == 0, tc.notices.str()
	}
}

fn test_transitively_reachable_dependency_mutability_errors_are_reported() {
	for parallel in [false, true] {
		tc := dependency_checker('wrapper', parallel)!
		assert tc.errors.any(it.file.ends_with('dep.v') && it.msg.contains('immutable')), tc.errors.str()
		assert !tc.errors.any(it.msg.contains('has no field named')), tc.errors.str()
		assert tc.notices.len == 0, tc.notices.str()
	}
}
