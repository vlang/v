fn test_shared_map_delete() {
	shared store := map[string]int{}
	lock store {
		store['abc'] = 5
		store['xyz'] = 10
	}

	lock store {
		assert store.len == 2
		store.delete('abc')
		assert store.len == 1
		assert store['xyz'] == 10
	}
}
