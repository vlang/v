// should allow `a:=arr.sorted(a < b)` use `a` as var name
fn test_skip_check_sort_arg_a_b() {
	arr := [4, 2, 1, 3]
	a := arr.sorted(a < b)
	assert a == [1, 2, 3, 4]
	b := arr.sorted(a > b)
	assert b == [4, 3, 2, 1]
}

// should allow `it:=arr.map(it*2)` use `it` as var name
fn test_skip_check_map_arg_it() {
	it := [4, 2, 1, 3].map(it * 2)
	assert it == [8, 4, 2, 6]
}

// should allow `it:=arr.filter(it%2==0)` use `it` as var name
fn test_skip_check_filter_arg_it() {
	it := [4, 2, 1, 3].filter(it % 2 == 0)
	assert it == [4, 2]
}

// should allow `it:=arr.any(it%2==0)` use `it` as var name
fn test_skip_check_any_arg_it() {
	it := [4, 2, 1, 3].any(it % 2 == 0)
	assert it
}

// should allow `it:=arr.all(it%2==0)` use `it` as var name
fn test_skip_check_all_arg_it() {
	it := [4, 2, 1, 3].all(it % 2 == 0)
	assert !it
}

// should allow `it:=arr.count(it%2==0)` use `it` as var name
fn test_skip_check_count_arg_it() {
	it := [4, 2, 1, 3].count(it % 2 == 0)
	assert it == 2
}
