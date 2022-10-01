module maps

pub fn filter<K, V>(m map[K]V, f fn (K, V) bool) map[K]V {
	mut mp := map[K]V{}

	for k, v in m {
		if f(k, v) {
			mp[k] = v
		}
	}

	return mp
}

pub fn flat_map<K, V, I>(m map[K]V, f fn (K, V) []I) []I {
	mut a := []I{cap: m.len}

	for k, v in m {
		a << f(k, v)
	}

	return a
}

pub fn maps<K, V, I>(m map[K]V, f fn (K, V) I) []I {
	mut a := []I{cap: m.len}

	for k, v in m {
		a << f(k, v)
	}

	return a
}
