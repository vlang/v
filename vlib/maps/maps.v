module maps

// filter filters map entries by the given predicate function
pub fn filter<K, V>(m map[K]V, f fn (key K, val V) bool) map[K]V {
	mut mp := map[K]V{}

	for k, v in m {
		if f(k, v) {
			mp[k] = v
		}
	}

	return mp
}

// to_array maps map entries into one-dimensional array
pub fn to_array<K, V, I>(m map[K]V, f fn (key K, val V) I) []I {
	mut a := []I{cap: m.len}

	for k, v in m {
		a << f(k, v)
	}

	return a
}

// flat_map maps map entries into arrays and flattens into a one-dimensional array
pub fn flat_map<K, V, I>(m map[K]V, f fn (key K, val V) []I) []I {
	mut a := []I{cap: m.len}

	for k, v in m {
		a << f(k, v)
	}

	return a
}

// to_map maps map entries into new entries and constructs a new map
pub fn to_map<K, V, X, Y>(m map[K]V, f fn (key K, val V) (X, Y)) map[X]Y {
	mut mp := map[X]Y{}

	for k, v in m {
		x, y := f(k, v)
		mp[x] = y
	}

	return mp
}

// invert returns a new map, created by swapping key to value and vice versa for each entry.
pub fn invert<K, V>(m map[K]V) map[V]K {
	mut mp := map[V]K{}

	for k, v in m {
		mp[v] = k
	}

	return mp
}

// from_array maps array into map with index to element per entry
pub fn from_array<T>(array []T) map[int]T {
	mut mp := map[int]T{}

	for i, e in array {
		mp[i] = e
	}

	return mp
}
