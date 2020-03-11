import hash.fnv1a

fn test_fnv1a() {
	$if windows {
		return
	}
	a := 'apple'
	b := fnv1a.sum64_string(a)
	c := fnv1a.sum64(a.bytes())
	assert b.hex() == 'f74a62a458befdbf'
	assert c.hex() == 'f74a62a458befdbf'
}
