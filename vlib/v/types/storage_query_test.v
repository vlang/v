module types

fn test_storage_query_result_equality_ignores_traversal_order() {
	result := {
		'.first':   [1, 2]
		'.second':  [3]
		'.cleared': []int{}
	}
	mut permuted := map[string][]int{}
	permuted['.cleared'] = []int{}
	permuted['.second'] = [3]
	permuted['.first'] = [2, 1]
	assert result.keys() != permuted.keys()
	assert result['.first'] != permuted['.first']
	assert storage_query_results_equal(result.keys(), result, permuted)
	assert storage_query_results_equal(permuted.keys(), permuted, result)
}

fn test_storage_query_result_equality_preserves_paths_and_sources() {
	result := {
		'.first':   [1, 2]
		'.cleared': []int{}
	}
	assert !storage_query_results_equal(result.keys(), result, {
		'.first': [1, 2]
		'.other': []int{}
	})
	assert !storage_query_results_equal(result.keys(), result, {
		'.first':   [1, 3]
		'.cleared': []int{}
	})
	assert !storage_query_results_equal(result.keys(), result, {
		'.first':   [1]
		'.cleared': []int{}
	})
	assert !storage_query_results_equal(result.keys(), result, {
		'.first': [1, 2]
	})
}

fn test_storage_query_certificates_reuse_permuted_results() {
	result := {
		'.first':  [1, 2]
		'.second': [3]
	}
	cached := {
		'.second': [3]
		'.first':  [2, 1]
	}
	mut cache := VisibleMutationCache{}
	cache.storage_query_results['query'] = [StorageQueryResult{
		writes: cached
		guards: StorageQueryGuards{ present: [u64(11)] }
	}]
	incoming := {
		u64(11): false
	}
	proof := cache.storage_query_union('query', result, incoming) or {
		panic('equal summaries should combine complementary guards')
	}
	assert proof.guard_id == 11
	assert proof.incoming && proof.existing
	mut forwarded := incoming.clone()
	assert cache.storage_query_forward_union('query', result, mut forwarded) == 0
	assert forwarded.len == 0
	assert cache.storage_query_results['query'][0].guards.present == [u64(11)]
	shared := cache.storage_query_shared_result('query', result) or {
		panic('equal summaries should share their retained payload')
	}
	assert shared.keys() == cached.keys()
}
