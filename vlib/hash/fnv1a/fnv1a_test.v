import hash.fnv1a

fn test_fnv1a_sum32() {
	$if windows {
		return
	}
	ahash := '10bc2abf'
	a := 'apple'
	b := fnv1a.sum32_string(a)
	c := fnv1a.sum32(a.bytes())
	d := unsafe { fnv1a.sum32_bytes(a.str, a.len) }
	assert b.hex() == ahash
	assert c.hex() == ahash
	assert d.hex() == ahash
}

fn test_fnv1a_sum64() {
	$if windows {
		return
	}
	a := 'apple'
	ahash := 'f74a62a458befdbf'
	b := fnv1a.sum64_string(a)
	c := fnv1a.sum64(a.bytes())
	d := unsafe { fnv1a.sum64_bytes(a.str, a.len) }
	assert b.hex() == ahash
	assert c.hex() == ahash
	assert d.hex() == ahash
}
