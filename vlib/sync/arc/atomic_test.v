// vtest ownership
// vtest vflags: -ownership
module arc

fn test_atomic_clones_share_storage_across_workers() {
	count := new_atomic(1)
	first := spawn increment_atomic(count.clone())
	second := spawn increment_atomic(count.clone())
	first.wait()
	second.wait()
	assert count.load() == 201
	assert count.sub(1) == 201
	assert count.swap(5) == 200
	assert count.compare_and_swap(5, 6)
	assert count.load() == 6
}

fn increment_atomic(count Atomic[int]) {
	for _ in 0 .. 100 {
		count.add(1)
	}
}

fn test_atomic_clone_outlives_its_original_owner() {
	value := make_atomic_clone()
	assert value.load()
	value.store(false)
	assert !value.load()
}

fn make_atomic_clone() Atomic[bool] {
	value := new_atomic(true)
	return value.clone()
}
