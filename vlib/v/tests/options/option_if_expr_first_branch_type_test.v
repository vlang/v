// An if-expression whose first branch is `?T` is typed `?T`; later `T` and
// `none` branches of the same `if`/`else if` chain are wrapped into it.
@[heap]
struct IfFirstBranchObject {
	n int
}

fn if_first_branch_height(anonymous bool, definite bool, established bool, containing ?f64, content ?f64) ?f64 {
	height := if anonymous {
		containing
	} else if definite {
		f64(2.5)
	} else if established {
		content
	} else {
		none
	}
	return height
}

fn if_first_branch_object(undefined bool) ?&IfFirstBranchObject {
	object := &IfFirstBranchObject{
		n: 3
	}
	compare := if undefined { ?&IfFirstBranchObject(none) } else { object }
	return compare
}

fn if_first_branch_value(use_option bool, value ?int) string {
	result := if use_option { value } else { 7 }
	return '${typeof(result).name} ${result or { -1 }}'
}

fn test_if_expr_first_option_branch_wraps_later_payloads() {
	assert if_first_branch_height(true, false, false, 1.0, 2.0) or { 0 } == 1.0
	assert if_first_branch_height(true, false, false, none, 2.0) == none
	assert if_first_branch_height(false, true, false, 1.0, 2.0) or { 0 } == 2.5
	assert if_first_branch_height(false, false, true, 1.0, 2.0) or { 0 } == 2.0
	assert if_first_branch_height(false, false, false, 1.0, 2.0) == none
}

fn test_if_expr_first_option_pointer_branch() {
	assert if_first_branch_object(true) == none
	object := if_first_branch_object(false) or { panic('missing object') }
	assert object.n == 3
}

fn test_if_expr_first_option_scalar_branch() {
	assert if_first_branch_value(true, 5) == '?int 5'
	assert if_first_branch_value(true, none) == '?int -1'
	assert if_first_branch_value(false, none) == '?int 7'
}
