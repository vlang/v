type Bug = i64 | u64

fn test_match_sumtype_var_aggregate_print_var() {
	f1 := Bug(i64(-17))
	ret1 := match f1 {
		u64, i64 {
			println(f1)
			println(f1.str())
			f1.str()
		}
	}
	assert ret1 == '-17'

	f2 := Bug(i64(-18))
	ret2 := match f2 {
		u64, i64 {
			println(f2)
			println('${f2}')
			'${f2}'
		}
	}
	assert ret2 == '-18'
}
