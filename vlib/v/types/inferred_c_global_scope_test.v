// vtest vflags: -prealloc -gc none
module types

import os
import v.flat
import v.parser
import v.pref

fn test_header_only_c_global_is_inferred_and_kept_across_scoped_batches() {
	header_name := 'v3_inferred_c_global_${os.getpid()}.h'
	header := os.join_path(os.vtmp_dir(), header_name)
	path := os.join_path(os.vtmp_dir(), 'v3_inferred_c_global_${os.getpid()}.c.v')
	os.write_file(header, 'static double v_inferred_assignment_value = 0.0;\n')!
	defer {
		os.rm(path) or {}
		os.rm(header) or {}
	}
	os.write_file(path, 'module main
#include "@DIR/${header_name}"
fn infer_value() f64 { return C.v_inferred_assignment_value }
fn update_value() { C.v_inferred_assignment_value = f64(2.5) }
fn read_value() f64 {
	observed := C.v_inferred_assignment_value
	return observed
}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.diagnostic_files[path] = true
	tc.collect(a)
	assert tc.c_identifier_is_undeclared('v_inferred_assignment_value')
	assert 'C.v_inferred_assignment_value' !in tc.c_globals
	assert tc.inferred_c_globals.len == 0
	items := tc.collect_parallel_check_items()
	assert items.len == 3
	assert a.nodes[items[0].fn_idx].value == 'infer_value'
	assert a.nodes[items[1].fn_idx].value == 'update_value'
	assert a.nodes[items[2].fn_idx].value == 'read_value'
	for item in items {
		// Separate calls force the previous batch's arena to be freed, even for this small fixture.
		tc.check_scoped_batches([item], 1)
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.inferred_c_globals.len == 1
		assert tc.inferred_c_globals[0].name == 'C.v_inferred_assignment_value'
		assert tc.inferred_c_globals[0].typ == builtin_f64_type
		assert !tc.c_identifier_is_undeclared('v_inferred_assignment_value')
		remembered := tc.c_globals['C.v_inferred_assignment_value'] or {
			panic('lost inferred global')
		}
		assert remembered == builtin_f64_type
	}
	mut saw_untyped_read := false
	for node in a.nodes {
		if node.kind == .decl_assign {
			lhs_id := a.child(&node, 0)
			if a.nodes[int(lhs_id)].value == 'observed' {
				local_type := tc.expr_type(lhs_id) or { panic('missing inferred local type') }
				assert local_type == builtin_f64_type
				saw_untyped_read = true
			}
		}
	}
	assert saw_untyped_read
}

fn test_inferred_c_global_names_and_types_outlive_checker_batch() {
	$if prealloc {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		scope := unsafe { prealloc_scope_begin() }
		mut worker := tc.fork_for_parallel_check()
		worker.remember_inferred_c_global('C.errno'.clone(), builtin_int_type)
		worker.remember_inferred_c_global('C.batch_value'.clone(), Type(ArrayFixed{
			elem_type: Type(Struct{ name_id: flat.source_type_id('C.BatchItem'.clone()) })
			len:       4
			len_expr:  'batch_count'.clone()
		}))
		// Inferred globals must remain private until their owning checker merges them.
		assert 'C.errno' !in tc.c_globals
		assert 'C.batch_value' !in tc.c_globals
		unsafe { prealloc_scope_leave(scope) }
		tc.merge_parallel_check_worker_scoped(worker, true)
		assert tc.inferred_c_globals.len == 2
		for inferred in tc.inferred_c_globals {
			assert !unsafe { prealloc_scope_owns(scope, inferred.name.str) }
		}
		promoted := tc.c_globals['C.batch_value'] or { panic('inferred array was not promoted') }
		assert promoted is ArrayFixed
		assert !unsafe { prealloc_scope_owns(scope, promoted.len_expr.str) }
		elem := promoted.elem_type
		assert elem is Struct
		assert !unsafe { prealloc_scope_owns(scope, elem.name().str) }
		unsafe { prealloc_scope_free_after(scope) }

		errno_type := tc.c_globals['C.errno'] or { panic('errno was not promoted') }
		assert errno_type == builtin_int_type
		assert promoted.len == 4
		assert promoted.len_expr == 'batch_count'
		assert elem.name() == 'C.BatchItem'
		assert !tc.c_identifier_is_undeclared('errno')
		mut next_batch := tc.fork_for_parallel_check()
		next_errno_type := next_batch.c_globals['C.errno'] or { panic('errno was not inherited') }
		assert next_errno_type == builtin_int_type
		assert !next_batch.c_identifier_is_undeclared('batch_value')
		next_batch.free_parallel_check_worker_cache()
	}
}
