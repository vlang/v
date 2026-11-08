module quick

fn reverse_str(s string) string {
	r := s.runes()
	mut out := []rune{len: r.len}
	for i, c in r {
		out[r.len - 1 - i] = c
	}
	return out.string()
}

fn test_check_passing_property() {
	mut g := StringGenerator{}
	n := check(mut g, fn (s string) bool {
		return reverse_str(reverse_str(s)) == s
	})!
	assert n == 100
}

fn test_check_finds_counterexample() {
	mut g := IntGenerator{ lo: 0, hi: 100 }
	if _ := check(mut g, fn (x int) bool {
		return x < 50
	}) {
		assert false, 'expected a counterexample'
	} else {
		assert err.msg().contains('counterexample')
	}
}

fn test_check_bool_and_f64() {
	mut b := BoolGenerator{}
	assert check(mut b, fn (v bool) bool {
		return v == true || v == false
	})! == 100
	mut f := F64Generator{ lo: -1.0, hi: 1.0 }
	assert check(mut f, fn (v f64) bool {
		return v >= -1.0 && v <= 1.0
	})! == 100
}

fn test_rng_deterministic() {
	mut a := new_rng(42)
	mut b := new_rng(42)
	for _ in 0 .. 10 {
		assert a.next_u64() == b.next_u64()
	}
}
