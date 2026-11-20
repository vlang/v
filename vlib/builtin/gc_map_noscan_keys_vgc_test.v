// vtest vflags: -gc vgc
// DenseArray sides that hold pointers must never land on noscan spans, even
// when the map was created through a noscan constructor for the other side
// (here: pointer-free int values). A collection must not sweep live key
// strings whose span was wrongly filed as noscan.
module builtin

fn test_vgc_map_noscan_value_keeps_string_keys() {
	mut m := map[string]int{}
	for i in 0 .. 2000 {
		m['key_${i}'] = i
		if i % 500 == 499 {
			gc_collect()
		}
	}
	assert m.len == 2000
	mut sum := 0
	for k, v in m {
		assert v == k[4..].int()
		sum += v
	}
	assert sum == 2000 * 1999 / 2
	gc_collect()
	assert m.len == 2000
}
