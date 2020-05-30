import rand

fn mt19937_test() {
	rng := rand.MT19937Rng{}
	rng.seed([u32(0xdeadbeef)])
	target := [956529277, 3842322136, 3319553134, 1843186657, 2704993644, 595827513, 938518626, 1676224337, 3221315650, 1819026461]
	for i := 0; i < 10; i++ {
		assert target[i] == rng.u32()
	}
}