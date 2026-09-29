// A container that hands out components through a type-erased callback lets the
// caller mutate a component without the container itself being mutable.
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
	return c.query(usize(typeof(Counter{}).idx))
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
