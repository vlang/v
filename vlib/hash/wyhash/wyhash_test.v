import hash.wyhash

struct WyHashTest {
	s        string
	seed     u64
	expected u64
}

fn test_wyhash() {
	tests := [WyHashTest{
		'',0,0x0},
	WyHashTest{
		'v',1,0xc72a8f8bdfdd82},
	WyHashTest{
		'is',2,0xa1099c1c58fc13e},
	WyHashTest{
		'the best',3,0x1b1215ef0b0b94c},
	WyHashTest{
		'abcdefghijklmnopqrstuvwxyz',4,0x6db0e773d1503fac},
	WyHashTest{
		'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',5,0xe062dfda99413626},
	]
	for test in tests {
		got := wyhash.sum64(test.s.bytes(), test.seed)
		// println(' #      GOT: $got | $got.hex()')
		// println(' # EXPECTED: $test.expected | $test.expected.hex()')
		assert got == test.expected
	}
}

fn test_rand_u64() {
	seed := u64(111)
	mut rand_nos := []u64{}
	for _ in 0..40 {
		rand_no := wyhash.rand_u64(&seed)
		for r in rand_nos {
			assert rand_no != r
		}
		rand_nos << rand_no
	}
	assert true
}
