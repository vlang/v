// vtest vflags: -prealloc -gc none
module types

import v.flat

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
