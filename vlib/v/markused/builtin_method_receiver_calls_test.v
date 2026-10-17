module markused

import os
import v.flat
import v.parser
import v.pref
import v.types

fn check_builtin_method_receiver_source(name string, source string) (&flat.FlatAst, &types.TypeChecker, &PreparedMarkusedDecls) {
	path := os.join_path(os.vtmp_dir(), 'v3_builtin_method_receiver_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files[path] = true
	// Like a build of the compiler itself: the declarations are prepared while the
	// checker runs, and markused then follows the call edges that the checker records.
	prepared_thread := spawn prepare_markused_declarations(a, &tc, true)
	tc.check_semantics()
	prepared := prepared_thread.wait()
	return a, &tc, prepared
}

fn main_fn_dependencies(a &flat.FlatAst, tc &types.TypeChecker) []string {
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value == 'main' {
			return tc.direct_dependencies(i)
		}
	}
	return []string{}
}

// A function called only inside the receiver of a builtin array or map method, like
// `[a(), b()].filter(it != '')`, must stay in a build that trusts the checker's
// call edges; it used to be dropped, and the C called an undefined function.
fn test_calls_in_builtin_method_receivers_are_kept() {
	a, tc, mut prepared := check_builtin_method_receiver_source('kept', "module main

fn filtered() string {
	return 'a'
}

fn first_one() string {
	return 'b'
}

fn contained() string {
	return 'c'
}

fn mapped() string {
	return 'd'
}

fn map_value() int {
	return 1
}

fn main() {
	println(['x', filtered()].filter(it != ''))
	println([first_one(), 'y'].first())
	println(['z', contained()].contains('z'))
	println([mapped()].map(it.len))
	println({
		'k': map_value()
	}.keys())
}
")
	assert tc.errors.len == 0, tc.errors.str()
	names := ['filtered', 'first_one', 'contained', 'mapped', 'map_value']
	used := mark_used_without_generic_detection_prepared(a, tc, mut prepared)
	prepared.release()
	for name in names {
		assert used[name], name
	}
	dependencies := main_fn_dependencies(a, tc)
	for name in names {
		assert name in dependencies, name
	}
}

// The receiver of a builtin array or map method is checked like any other receiver.
fn test_errors_in_builtin_method_receivers_are_reported() {
	_, tc, mut prepared := check_builtin_method_receiver_source('errors', "module main

fn takes_int(x int) string {
	return x.str()
}

fn main() {
	println([takes_int('a')].filter(it != ''))
	println({
		'k': takes_int('b')
	}.keys())
}
")
	prepared.release()
	messages := tc.errors.map(it.msg)
	assert messages.len == 2, messages.str()
	for message in messages {
		assert message == 'cannot use `string` as `int` in argument 1 to `takes_int`', message
	}
}

fn test_calls_in_sort_receivers_are_kept() {
	a, tc, mut prepared := check_builtin_method_receiver_source('sort_kept', 'module main

fn default_index() int {
	return 0
}

fn comparator_index() int {
	return 1
}

fn main() {
	mut groups := [[3, 1], [4, 2]]
	groups[default_index()].sort()
	groups[comparator_index()].sort(a > b)
}
')
	assert tc.errors.len == 0, tc.errors.str()
	used := mark_used_without_generic_detection_prepared(a, tc, mut prepared)
	prepared.release()
	dependencies := main_fn_dependencies(a, tc)
	for name in ['default_index', 'comparator_index'] {
		assert used[name], name
		assert name in dependencies, name
	}
}

fn test_errors_in_sort_receivers_are_reported() {
	_, tc, mut prepared := check_builtin_method_receiver_source('sort_errors', "module main

fn pick(index int) int {
	return index
}

fn main() {
	mut groups := [[3, 1], [4, 2]]
	groups[pick('default')].sort()
	groups[pick('comparator')].sort(a > b)
}
")
	prepared.release()
	messages := tc.errors.map(it.msg)
	assert messages.len == 2, messages.str()
	for message in messages {
		assert message == 'cannot use `string` as `int` in argument 1 to `pick`', message
	}
}
