module types

import os
import v.parser
import v.pref

fn dependency_checker(call string, parallel bool) !TypeChecker {
	root := os.join_path(os.vtmp_dir(), 'dependency_errors_${os.getpid()}_${parallel}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep\npub struct Item {\npub:\n\tname string\n}\npub fn describe(item Item) string { return item.missing }\npub fn rename(item Item) Item { item.name = "renamed"; return item }\npub fn wrapper(item Item) Item { return rename(item) }\npub fn unused() { unused_value := 1 }\n')!
	os.write_file(main, 'module main\nimport dep\nfn main() { println(dep.${call}(dep.Item{})) }\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dep, main])
	mut tc := TypeChecker.new(a)
	tc.diagnostic_files[main] = true
	tc.collect(a)
	tc.check_semantics_opt(parallel)
	return tc
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
