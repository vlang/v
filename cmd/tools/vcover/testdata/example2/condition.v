module example2

pub fn condition() int {
	mut res := 0
	$if condition1 ? {
		res += 1
	}
	$if condition2 ? {
		res += 2
	}
	$if condition3 ? {
		res += 4
	}
	$if condition4 ? {
		res += 8
	}
	$if condition5 ? {
		res += 16
	}
	$if condition6 ? {
		res += 32
	}
	$if condition7 ? {
		res += 64
	}
	$if condition8 ? {
		return 128
	}
	return res
}
