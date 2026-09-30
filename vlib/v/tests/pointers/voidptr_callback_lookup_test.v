// A container that hands out components through a type-erased callback lets the
// caller mutate separately stored components at an explicit unsafe boundary.
struct Container {
mut:
	on_query fn (&Container, usize) voidptr = unsafe { nil }
}

fn (c &Container) query(idx usize) voidptr {
	return c.on_query(c, idx)
}

@[heap]
struct Counter {
mut:
	hits int
}

fn Counter.from_container(c &Container) &Counter {
	return unsafe { &Counter(c.query(usize(typeof(Counter{}).idx))) }
}

fn bump(c &Container) {
	mut counter := Counter.from_container(c)
	counter.hits++
	counter.hits = counter.hits * 10
}

fn test_component_from_immutable_container_is_mutable() {
	counter := &Counter{}
	c := Container{
		on_query: fn [counter] (_ &Container, _ usize) voidptr {
			return voidptr(counter)
		}
	}
	bump(&c)
	assert counter.hits == 10
}

struct StoredContainer {
	counter &Counter
}

fn erase_stored_counter(counter &Counter) voidptr {
	return counter
}

fn stored_counter(c &StoredContainer) &Counter {
	return erase_stored_counter(c.counter)
}

fn test_visible_stored_pointer_lookup_does_not_borrow_the_container() {
	counter := &Counter{}
	c := StoredContainer{
		counter: counter
	}
	mut found := stored_counter(&c)
	found.hits = 13
	assert counter.hits == 13
}

fn counter_by_key(key usize, get fn (usize) voidptr) &Counter {
	return get(key)
}

fn test_type_erased_lookup_with_copied_key_does_not_borrow_the_key() {
	key := usize(7)
	counter := &Counter{}
	get := fn [counter] (idx usize) voidptr {
		assert idx == 7
		return counter
	}
	mut found := counter_by_key(key, get)
	found.hits = 17
	assert counter.hits == 17
	assert key == 7
}
