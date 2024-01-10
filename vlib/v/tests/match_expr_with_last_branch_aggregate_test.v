type MyT = f32 | f64 | i64 | int | rune | string | u32 | u64
type FnMyT = fn (MyT)

fn (any []MyT) each(fun FnMyT) {
	for x in any {
		fun(x)
	}
}

fn (myt MyT) to_s() string {
	return match myt {
		string { myt }
		rune { rune(myt).str() }
		int, u32, i64, u64, f32, f64 { myt.str() } // FAIL compilation
	}
}

fn test_match_expr_with_last_branch_aggregate() {
	mut arr := [MyT(1), `😝`, 'we are mamamoo', f32(u64(-1)), f64(u64(-1)), i64(-1), u64(-1)]
	arr.each(fn (my MyT) {
		println(my.to_s())
	})
	assert true
}
