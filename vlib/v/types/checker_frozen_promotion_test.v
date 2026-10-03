module types

import v.flat

fn frozen_promotion_same_payload(first Type, second Type) bool {
	first_word, first_tail, _ := type_value_words(&first)
	second_word, second_tail, _ := type_value_words(&second)
	return first_word == second_word && first_tail == second_tail
}

fn test_frozen_parallel_promotion_reuses_canonical_payloads_and_defers_misses() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	mut interner := tc.type_interner
	other_id, other := interner.canonicalize(Type(ArrayFixed{
		elem_type: Type(int_)
		len:       18
	}))
	target_id, target := interner.canonicalize(Type(ArrayFixed{
		elem_type: Type(int_)
		len:       17
	}))
	// Place a different semantic type at the first hash bucket, then the
	// matching type at the collision bucket. Both probes must walk the chain.
	target_key := semantic_type_hash(target)
	interner.buckets[target_key] = other_id
	interner.buckets[type_hash_tag(target_key, 0x5bd1e995)] = target_id
	worker_target := Type(ArrayFixed{ elem_type: Type(int_), len: 17 })
	worker_other := Type(ArrayFixed{ elem_type: Type(int_), len: 18 })
	missing_first := Type(Struct{ name: 'FrozenFirstMissing' })
	missing_second := Type(Struct{ name: 'FrozenSecondMissing' })
	assert !frozen_promotion_same_payload(worker_target, target)
	assert frozen_promotion_same_payload(interner.probe(worker_target)?, target)
	tc.expr_type_values = [worker_target, worker_other, missing_first, worker_target, missing_second,
		worker_other]
	tc.expr_type_set = []bool{len: tc.expr_type_values.len, init: true}
	tc.resolved_call_set = []bool{len: tc.expr_type_values.len}
	mut chunks := [
		[CheckWorkItem{ range_lo: 0, fn_idx: 2 }],
		[CheckWorkItem{ range_lo: 3, fn_idx: 5 }],
	]
	mut args := []CheckCloneChunkArgs{cap: chunks.len}
	for ci in 0 .. chunks.len {
		args << CheckCloneChunkArgs{
			tc:        voidptr(&tc)
			items_ptr: unsafe { voidptr(&chunks[ci]) }
			miss:      []int{cap: 16}
		}
	}
	initial_count := tc.type_count()
	// These task arguments and semantic payloads stay alive until both lanes
	// join. Each lane writes only its own node range and preallocated miss list.
	first := spawn check_clone_chunk_thread(unsafe { voidptr(&args[0]) })
	second := spawn check_clone_chunk_thread(unsafe { voidptr(&args[1]) })
	first.wait()
	second.wait()
	assert args[0].miss == [2]
	assert args[1].miss == [4]
	assert tc.type_count() == initial_count
	assert frozen_promotion_same_payload(tc.expr_type_values[0], target)
	assert frozen_promotion_same_payload(tc.expr_type_values[3], target)
	assert frozen_promotion_same_payload(tc.expr_type_values[1], other)
	assert frozen_promotion_same_payload(tc.expr_type_values[5], other)
	assert frozen_promotion_same_payload(tc.expr_type_values[2], missing_first)
	assert frozen_promotion_same_payload(tc.expr_type_values[4], missing_second)
	assert interner.probe_frozen(missing_first) == none
	assert interner.probe_frozen(missing_second) == none

	// Only the serial replay can grow the table, in the original chunk order.
	for arg in args {
		tc.intern_expr_type_misses(arg.miss)
	}
	assert tc.type_count() == initial_count + 2
	first_id, first_type := interner.canonicalize(missing_first)
	second_id, second_type := interner.canonicalize(missing_second)
	assert first_id == TypeId(initial_count)
	assert second_id == TypeId(initial_count + 1)
	assert frozen_promotion_same_payload(tc.expr_type_values[2], first_type)
	assert frozen_promotion_same_payload(tc.expr_type_values[4], second_type)
}

fn test_frozen_probe_preserves_empty_and_invalid_bucket_misses() {
	mut interner := new_type_interner()
	typ := Type(Struct{ name: 'FrozenMissing' })
	assert interner.probe_frozen(typ) == none
	assert interner.probe(typ) == none
	interner.buckets[semantic_type_hash(typ)] = TypeId(123)
	assert interner.probe_frozen(typ) == none
	assert interner.probe(typ) == none
	assert interner.types.len == 0

	tc := TypeChecker{}
	mut cache := CheckTypePromotionCache{}
	assert tc.cached_check_type_promotion(typ, mut cache, false, true) == none
}
