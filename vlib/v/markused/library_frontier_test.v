module markused

import os
import v.flat
import v.parser
import v.pref
import v.types

fn library_frontier_source(tail_body string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.vtmp_dir(), 'v3_library_frontier_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or { panic(err) } }
	main_path := os.join_path(root, 'main.v')
	library_path := os.join_path(root, 'dependency.v')
	os.write_file(main_path, 'module main
import dependency
fn main() {
	item := dependency.Item{}
	_ := item.str()
}
') or { panic(err) }
	os.write_file(library_path, 'module dependency
pub struct Item {}
struct Unused {}
pub fn (item Item) str() string { return middle() }
fn middle() string { return tail() }
fn tail() string { ${tail_body} }
fn (item Unused) str() string {
	\$compile_error("unused formatter")
	return "unused"
}
') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, library_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		library_path: true
	}, []string{}, true)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn test_library_frontier_checks_transitive_bodies_before_collecting_their_calls() {
	a, checker := library_frontier_source('return "value"')
	mut tc := checker
	assert can_check_library_body_frontiers(a, tc)
	assert !tc.reachable_library_fns['dependency.Item.str']
	assert !tc.reachable_library_fns['dependency.middle']
	assert !tc.reachable_library_fns['dependency.tail']
	assert tc.library_bodies_checked_late() == 0
	used, generic := mark_used_checking_library_bodies(a, mut tc, false)
	assert tc.errors.len == 0, tc.errors.str()
	for name in ['Item.str', 'middle', 'tail'] {
		assert tc.reachable_library_fns['dependency.${name}'], name
		assert used['dependency.${name}'], name
	}
	assert !tc.reachable_library_fns['dependency.Unused.str']
	assert !used['dependency.Unused.str']
	assert tc.library_bodies_checked_late() == 3
	fresh, fresh_generic := mark_used_with_generic_usage(a, tc)
	assert used == fresh
	assert generic == fresh_generic
	assert tc.check_reached_library_bodies(used, false) == 0
	assert tc.library_bodies_checked_late() == 3
}

fn test_library_frontier_preserves_diagnostics_in_a_later_frontier() {
	a, checker := library_frontier_source('\$compile_error("reached formatter dependency") return "value"')
	mut tc := checker
	_, _ := mark_used_checking_library_bodies(a, mut tc, false)
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == 'reached formatter dependency', tc.errors.str()
	assert !tc.reachable_library_fns['dependency.Unused.str']
}

fn library_frontier_same_named_source(methods bool, first_module string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.vtmp_dir(), 'v3_library_frontier_same_name_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or { panic(err) } }
	main_path := os.join_path(root, 'main.v')
	alpha_path := os.join_path(root, '${first_module}.v')
	beta_path := os.join_path(root, 'beta.v')
	main_call := if methods {
		'_ := ${first_module}.Item{}.str()'
	} else if first_module == 'builtin' {
		'entry()'
	} else {
		'${first_module}.entry()'
	}
	main_import := if first_module == 'builtin' { '' } else { 'import ${first_module}' }
	alpha_body := if methods {
		'pub struct Item {}\npub fn (item Item) str() string { return beta.Item{}.str() }'
	} else {
		'pub fn entry() { beta.entry() }'
	}
	beta_body := if methods {
		'pub struct Item {}\npub fn (item Item) str() string { \$compile_error("reached beta body") return "beta" }'
	} else {
		'pub fn entry() { \$compile_error("reached beta body") }'
	}
	os.write_file(main_path, 'module main\n${main_import}\nfn main() { ${main_call} }\n') or {
		panic(err)
	}
	os.write_file(alpha_path, 'module ${first_module}\nimport beta\n${alpha_body}\n') or { panic(err) }
	os.write_file(beta_path, 'module beta\n${beta_body}\n') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, alpha_path, beta_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		alpha_path: true
		beta_path:  true
	}, []string{}, false)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn test_library_frontier_checks_same_named_function_in_a_later_module() {
	for methods in [false, true] {
		for parallel in [false, true] {
			a, checker := library_frontier_same_named_source(methods, 'alpha')
			mut tc := checker
			_, _ := mark_used_checking_library_bodies(a, mut tc, parallel)
			assert tc.errors.len == 1, tc.errors.str()
			assert tc.errors[0].msg == 'reached beta body', tc.errors.str()
			assert tc.library_bodies_checked_late() == 2
			name := if methods { 'Item.str' } else { 'entry' }
			assert tc.reachable_library_fns['alpha.${name}']
			assert tc.reachable_library_fns['beta.${name}']
		}
	}
}

fn test_library_frontier_checks_same_named_function_after_a_builtin_body() {
	for parallel in [false, true] {
		a, checker := library_frontier_same_named_source(false, 'builtin')
		mut tc := checker
		_, _ := mark_used_checking_library_bodies(a, mut tc, parallel)
		assert tc.errors.len == 1, tc.errors.str()
		assert tc.errors[0].msg == 'reached beta body', tc.errors.str()
		assert tc.library_bodies_checked_late() == 2
		assert tc.reachable_library_fns['builtin.entry']
		assert tc.reachable_library_fns['beta.entry']
	}
}

fn library_frontier_multiple_errors_source(type_errors bool) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.vtmp_dir(), 'v3_library_frontier_multiple_errors_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or { panic(err) } }
	main_path := os.join_path(root, 'main.v')
	alpha_path := os.join_path(root, 'alpha.v')
	beta_path := os.join_path(root, 'beta.v')
	alpha_error := if type_errors {
		'return "alpha"'
	} else {
		'\$compile_error("alpha later error")'
	}
	beta_error := if type_errors {
		'return "beta"'
	} else {
		'\$compile_error("beta first error")'
	}
	alpha_signature := if type_errors { 'fn later() int' } else { 'fn later()' }
	beta_signature := if type_errors { 'pub fn fail() int' } else { 'pub fn fail()' }
	os.write_file(main_path, 'module main
import alpha
import beta
fn main() { alpha.entry() beta.fail() }
') or { panic(err) }
	os.write_file(alpha_path, 'module alpha
pub fn entry() { later() }
${alpha_signature} { ${alpha_error} }
') or { panic(err) }
	os.write_file(beta_path, 'module beta
${beta_signature} { ${beta_error} }
') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, alpha_path, beta_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		alpha_path: true
		beta_path:  true
	}, []string{}, false)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn test_library_frontier_reports_later_errors_in_legacy_diagnostic_order() {
	for type_errors in [false, true] {
		for parallel in [false, true] {
			a, checker := library_frontier_multiple_errors_source(type_errors)
			mut tc := checker
			used, generic := mark_used_checking_library_bodies(a, mut tc, parallel)
			assert tc.errors.len == 2, tc.errors.str()
			assert tc.library_bodies_checked_late() == 3
			legacy_ast, legacy_checker := library_frontier_multiple_errors_source(type_errors)
			mut legacy := legacy_checker
			legacy_used, legacy_generic := mark_used_with_generic_usage(legacy_ast, legacy)
			assert legacy.check_reached_library_bodies(legacy_used, parallel) == 3
			assert tc.errors.map(it.msg) == legacy.errors.map(it.msg)
			assert tc.errors.map(it.node_pos) == legacy.errors.map(it.node_pos)
			assert tc.errors.map(it.kind) == legacy.errors.map(it.kind)
			assert used == legacy_used
			assert generic == legacy_generic
			assert tc.reachable_library_fns['alpha.later']
		}
	}
}
