fn test_arr_rval() {
	a := [1, 2]
	s := (*&a)[..]
	assert s == a

	b := unsafe { *&[]int(&a) }[..]
	assert b == a
}

fn geta(mut n []int) []int {
	n[0]++
	return [1]
}

fn gets(mut n []int) string {
	n[0]++
	return "1"
}

fn test_double_eval() {
	mut n := [0]
	r := geta(mut n)[..]
	assert n[0] == 1
	
	s := gets(mut n)[..]
	assert s.len == 1 // use s
	assert n[0] == 2
}
