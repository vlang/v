import os

[direct_array_access]
fn test_big_int_array() {
	dump(sizeof(isize))
	mut maxn := 500_000_000 // try allocating ~2GB worth of integers on 32bit platforms
	if sizeof(isize) > 4 {
		maxn = 1_000_000_000 // 1 billion integers, when each is 4 bytes => require ~4GB
	}
	// NB: this test requires RAM that many people do not have, so only run it in full, when VTEST_BIGMEM is 1
	vtest_bigmem := os.getenv('VTEST_BIGMEM').int()
	if vtest_bigmem == 0 {
		maxn = 10_000_000
	}
	dump(maxn)
	mut data := []int{len: maxn}

	// ensure that all of the elements are written at least once, to prevent the OS from cheating:
	for i in 0 .. maxn {
		data[i] = i
	}
	assert data[0] == 0
	assert data[maxn - 1] == maxn - 1
	dump(data#[0..10])
	dump(data#[-10..])
}
