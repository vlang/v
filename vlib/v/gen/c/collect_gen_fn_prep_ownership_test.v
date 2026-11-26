// vtest vflags: -prealloc -gc none
module c

import v.types

// A FastC-built compiler allocates type references outside V's arenas, while
// their lists still come from the current arena. A prep task's return type
// must be copied out of its scratch arena whenever its lists live there, even
// when the reference itself does not.
fn test_own_collect_gen_fn_preps_copies_scratch_lists_of_an_outside_return_type() {
	$if prealloc {
		keep := cgen_worker_scope_begin(true)
		list_scope := cgen_worker_scope_begin(true)
		scratch := cgen_worker_scope_begin(true)
		results := [types.Type(types.int_), types.Type(types.string_)]
		// The layout of a FastC-built compiler: the reference outside the
		// scratch arena, the list of the multi-return inside it.
		state := cgen_worker_scope_suspend(scratch)
		return_type := types.new_type_reference(types.Type(types.MultiReturn{
			types: results
		}))
		cgen_worker_scope_resume(scratch, state)
		preps := [CollectGenFnPrep{
			prepared:    true
			return_type: return_type
		}]
		regs := [
			FnSignatureRegistration{
				module_key:  'main'
				short_name:  'pair'
				return_type: return_type
			},
		]
		cgen_worker_scope_leave(scratch)
		assert !cgen_worker_scope_owns(scratch, return_type)
		assert cgen_worker_scope_owns(scratch, results.data)
		assert cgen_type_refers_to_scope(scratch, return_type)
		owned_preps, owned_regs := own_collect_gen_fn_preps(scratch, list_scope, preps,
			regs)
		assert owned_preps.len == 1
		assert owned_regs.len == 1
		assert !cgen_type_refers_to_scope(scratch, owned_preps[0].return_type)
		assert !cgen_type_refers_to_scope(scratch, owned_regs[0].return_type)
		cgen_worker_scope_free(scratch)
		for owned in [*owned_preps[0].return_type, *owned_regs[0].return_type] {
			if owned is types.MultiReturn {
				assert owned.types.len == 2
				assert owned.types[0] is types.Primitive
				assert owned.types[1] is types.String
			} else {
				assert false, 'owned return type: ${owned.type_name()}'
			}
		}
		cgen_worker_scope_leave(list_scope)
		cgen_worker_scope_free(list_scope)
		cgen_worker_scope_leave(keep)
		cgen_worker_scope_free(keep)
	}
}
