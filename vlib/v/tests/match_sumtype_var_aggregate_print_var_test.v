type Bug = i64 | u64

fn test_match_sumtype_var_aggregate_print_var() {
	f := Bug(i64(-17))
	ret := match f {
		u64, i64 {
			println(f)
			println(f.str())
			f.str()
		}
	}
	assert ret == '-17'
}
