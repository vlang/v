struct CollectionValues {
	items  []i32
	counts map[string]i32
}

fn collection_values() !CollectionValues {
	return CollectionValues{
		items:  [i32(1), 2]
		counts: {
			'a': i32(1)
			'b': 2
		}
	}
}

fn optional_collection_values() ?CollectionValues {
	return collection_values() or { return none }
}

fn test_propagated_collection_comparison_keeps_literal_element_width() {
	assert collection_values()!.items == [1, 2]
	assert [1, 2] == collection_values()!.items
	assert collection_values()!.items != [1, 3]
	assert [1, 3] != collection_values()!.items
	assert optional_collection_values()?.items == [1, 2]
	assert [1, 2] == optional_collection_values()?.items
	assert collection_values()!.counts == {
		'a': 1
		'b': 2
	}
	assert {
		'a': 1
		'b': 2
	} == collection_values()!.counts
	back := collection_values()!
	assert back.counts == {
		'a': 1
		'b': 2
	}
	assert {
		'a': 1
		'b': 2
	} == back.counts
}

fn record_element(mut order []int, marker int) i32 {
	order << marker
	return 1
}

fn record_collection(mut order []int) ![]i32 {
	order << 2
	return [i32(1)]
}

fn test_collection_literal_comparison_keeps_evaluation_order() {
	mut order := []int{}
	assert [record_element(mut order, 1)] == record_collection(mut order)!
	assert order == [1, 2]
	order.clear()
	assert record_collection(mut order)! == [record_element(mut order, 3)]
	assert order == [2, 3]
}
