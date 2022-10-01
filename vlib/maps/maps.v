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

pub fn maps<K, V, R, S>(m map[K]V, f fn (K, V) (R, S)) map[R]S {
	mut mp := map[R]S{}

	for k, v in m {
		r, s := f(k, v)
		mp[r] = s
	}

	return mp
}
