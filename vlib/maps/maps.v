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
