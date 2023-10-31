pub fn foo1() bool {
	return false
}

pub fn bar1(i int) ?int {
	if i < 0 {
		return none
	}
	return if i == 0 {
		if foo1() {
			1
		} else {
			2
		}
	} else {
		3
	}
}

// result
pub fn foo2() bool {
	return false
}

pub fn bar2(i int) !int {
	if i < 0 {
		return error('')
	}
	return if i == 0 {
		if foo2() {
			1
		} else {
			2
		}
	} else {
		3
	}
}

pub fn bar3(ok bool) ?fn () string {
	return if ok {
		fn () string {
			return 'yes'
		}
	} else {
		none
	}
}

pub fn bar4(ok bool) !fn () string {
	return if ok {
		fn () string {
			return 'yes'
		}
	} else {
		error('no:error')
	}
}

fn test_if_expr_nested_with_option_result() {
	ret11 := bar1(0) or { 0 }
	println(ret11)
	assert ret11 == 2
	ret12 := bar1(1) or { 0 }
	println(ret12)
	assert ret12 == 3

	ret21 := bar2(0) or { 0 }
	println(ret21)
	assert ret21 == 2
	ret22 := bar2(1) or { 0 }
	println(ret22)
	assert ret22 == 3

	ret31 := bar3(true) or {
		fn () string {
			return 'no:default'
		}
	}
	println(ret31())
	assert ret31() == 'yes'
	ret32 := bar3(false) or {
		fn () string {
			return 'no:default'
		}
	}
	println(ret32())
	assert ret32() == 'no:default'

	ret41 := bar4(true) or {
		fn [err] () string {
			return err.msg()
		}
	}
	println(ret41())
	assert ret41() == 'yes'
	ret42 := bar4(false) or {
		fn [err] () string {
			return err.msg()
		}
	}
	println(ret42())
	assert ret42() == 'no:error'
}
