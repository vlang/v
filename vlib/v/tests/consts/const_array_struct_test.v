struct Uint128 {
mut:
	lo u64
	hi u64
}

const zzz_test = [Uint128{1, 2}, Uint128{3, 4}]!

fn test_main() {
	assert dump(zzz_test) == zzz_test
}
