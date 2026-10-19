module maps

// CounterEntry is one counted item: key observed count times.
pub struct CounterEntry[T] {
pub:
	key   T
	count int
}

// Counter counts occurrences of values. Missing keys count 0. It is the
// V-idiomatic equivalent of Python's `collections.Counter`.
pub struct Counter[T] {
mut:
	counts map[T]int
}

// new_counter creates an empty Counter.
pub fn new_counter[T]() Counter[T] {
	return Counter[T]{
		counts: map[T]int{}
	}
}

// inc adds one occurrence of key.
pub fn (mut c Counter[T]) inc(key T) {
	c.counts[key] = c.counts[key] + 1
}

// dec removes one occurrence of key.
pub fn (mut c Counter[T]) dec(key T) {
	c.counts[key] = c.counts[key] - 1
}

// add_many counts every item once.
pub fn (mut c Counter[T]) add_many(items []T) {
	for item in items {
		c.inc(item)
	}
}

// get returns the count of key (0 if never seen).
pub fn (c &Counter[T]) get(key T) int {
	return c.counts[key]
}

// total returns the sum of all counts.
pub fn (c &Counter[T]) total() int {
	mut sum := 0
	for _, n in c.counts {
		sum += n
	}
	return sum
}

// most_common returns up to n entries ordered by descending count.
pub fn (c &Counter[T]) most_common(n int) []CounterEntry[T] {
	mut rest := c.counts.clone()
	mut out := []CounterEntry[T]{}
	for out.len < n && rest.len > 0 {
		mut best_key := rest.keys()[0]
		mut best_n := rest[best_key]
		for k, v in rest {
			if v > best_n {
				best_key = k
				best_n = v
			}
		}
		out << CounterEntry[T]{best_key, best_n}
		rest.delete(best_key)
	}
	return out
}

// DefaultMap is a map that creates missing entries with a factory closure on
// first access. It is the V-idiomatic equivalent of Python's
// `collections.defaultdict`.
pub struct DefaultMap[K, V] {
mut:
	m      map[K]V
	create fn () V @[required]
}

// new_default_map creates an empty DefaultMap using factory for new keys.
pub fn new_default_map[K, V](factory fn () V) DefaultMap[K, V] {
	return DefaultMap[K, V]{
		m:      map[K]V{}
		create: factory
	}
}

// get returns the value for key, creating it with the factory on first access.
pub fn (mut d DefaultMap[K, V]) get(key K) V {
	if key !in d.m {
		d.m[key] = d.create()
	}
	return d.m[key]
}

// get_opt returns the value for key, or none if absent (no creation).
pub fn (d &DefaultMap[K, V]) get_opt(key K) ?V {
	if key !in d.m {
		return none
	}
	return d.m[key]
}

// set stores value under key.
pub fn (mut d DefaultMap[K, V]) set(key K, value V) {
	d.m[key] = value
}

// len returns the number of stored keys.
pub fn (d &DefaultMap[K, V]) len() int {
	return d.m.len
}
